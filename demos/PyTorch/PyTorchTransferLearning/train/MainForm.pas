unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  PythonEngine, FMX.PythonGUIInputOutput, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, TorchVision, PyTorch, PyCommon, PyModule, PyPackage,
  NumPy, FMX.StdCtrls, Winapi.Windows;

type
  TTrainModelMainForm = class(TForm)
    PythonEngine1: TPythonEngine;
    mmOutput: TMemo;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    NumPy1: TNumPy;
    PyTorch1: TPyTorch;
    TorchVision1: TTorchVision;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function CreateDataSet(const AImageFolder: string): variant;
    procedure SplitDataSetTrainAndTest(const ADataSet: variant;
      out ATrainDataSet, ATestDataSet: variant);
    procedure CreateDataLoaders(const ATrainDataSet, ATestDataSet: variant;
      out ATrainLoader, ATestLoader: variant);
    procedure DefineNeuralNetwork(out ADevice, AModel: variant);
    procedure TrainNeuralNetwork(const ATrainLoader, ATestLoader, 
      ATrainDataSet, ATestDataSet, ADevice, AModel: variant);
  end;

var
  TrainModelMainForm: TTrainModelMainForm;

implementation

uses
  System.StrUtils, PyUtils, VarPyth;

{$R *.fmx}

{ TForm2 }

function TTrainModelMainForm.CreateDataSet(const AImageFolder: string): variant;
begin
  var datasets := TorchVision1.torchvision.datasets;
  var transforms := TorchVision1.torchvision.transforms;

  Result := datasets.ImageFolder(
    AImageFolder,
    transforms.Compose(TPyEx.List([
      transforms.ColorJitter(0.1, 0.1, 0.1, 0.1),
      transforms.Resize(TPyEx.Tuple([224, 224])),
      transforms.ToTensor(),
      transforms.Normalize(TPyEx.List([0.485, 0.456, 0.406]), TPyEx.List([0.229, 0.224, 0.225]))
    ]))
  );
end;

procedure TTrainModelMainForm.SplitDataSetTrainAndTest(const ADataSet: variant;
  out ATrainDataSet, ATestDataSet: variant);
begin
  var tuple := PyTorch1.torch.utils.data.random_split(ADataSet,
    TPyEx.List([ADataSet.length - 50, 50]));
  ATrainDataSet := tuple.GetItem(0);
  ATestDataSet := tuple.GetItem(1);
end;

procedure TTrainModelMainForm.CreateDataLoaders(const ATrainDataSet, ATestDataSet: variant;
  out ATrainLoader, ATestLoader: variant);
begin
  ATrainLoader := PyTorch1.torch.utils.data.DataLoader(
    ATrainDataSet,
    batch_size := 16,
    shuffle := true,
    num_workers := 0
  );

  ATestLoader := PyTorch1.torch.utils.data.DataLoader(
    ATestDataSet,
    batch_size := 16,
    shuffle := true,
    num_workers := 0
  );
end;


procedure TTrainModelMainForm.DefineNeuralNetwork(out ADevice, AModel: variant);
begin
  with PyTorch1, TorchVision1 do begin
    var models := torchvision.models;
    var model := models.alexnet(pretrained := true);
    model.classifier.SetItem(6, torch.nn.Linear(model.classifier.GetItem(6).in_features, 2));
    var device_string := IfThen(torch.cuda.is_available(), 'cuda', 'cpu');
    ADevice := torch.device(device_string);      
    AModel := model.to(ADevice);
  end;
end;

procedure TTrainModelMainForm.TrainNeuralNetwork(const ATrainLoader, ATestLoader,
  ATrainDataSet, ATestDataSet, ADevice, AModel: variant);
const
  NUM_EPOCHS = 30;
  BEST_MODEL_PATH = 'best_model.pth';
begin
  var best_accuracy := 0.0;
  with PyTorch1 do begin
    var op := VarPyth.Import('operator');
    var F := torch.nn.functional;
    var optimizer := torch.optim.SGD(AModel.parameters(), lr := 0.001, momentum := 0.9);

    var bm := BuiltinModule();
    for var epoch in bm.range(NUM_EPOCHS).GetEnumerator() do begin
                       
      for var tl_tuple in ATrainLoader.GetEnumerator() do begin
        var images := tl_tuple.GetItem(0);
        var labels := tl_tuple.GetItem(1);

        images := images.to(ADevice);
        labels := labels.to(ADevice);

        optimizer.zero_grad();
        var outputs := AModel.__call__(images);
        var loss: variant;
        TPyEx.ExecuteMasked(procedure() begin 
          loss := F.cross_entropy(outputs, labels);
        end);
        loss.backward();
        optimizer.step();
      end;

      var test_error_count := 0.0;
      for var tl_tuple in ATestLoader.GetEnumerator() do begin
        var images := tl_tuple.GetItem(0);
        var labels := tl_tuple.GetItem(1);

        images := images.to(ADevice);
        labels := labels.to(ADevice);  

        var outputs := AModel.__call__(images);
        test_error_count := 1.0 - bm.float((torch.sum(torch.abs(labels - outputs.argmax(1)))));
      end;

      var test_accuracy := 1.0 - (bm.float(test_error_count) / bm.float(ATestDataSet.length));
      bm.print(op.mod('%d: %f', TPyEx.Tuple([epoch, test_accuracy])));
      if (test_accuracy > best_accuracy) then begin
        torch.save(AModel.state_dict(), BEST_MODEL_PATH); 
        best_accuracy := test_accuracy;         
      end;
    end;
  end;                
end;

procedure TTrainModelMainForm.FormCreate(Sender: TObject);
begin
  for var I := 0 to ParamCount - 1 do
    mmOutput.Lines.Add(ParamStr(I));

  if ParamCount = 3 then begin
    //Check if this is a spawned process from a multiprocessing DataLoader (workers)
    if ParamStr(1) = '-c' then begin
      try
        PythonEngine1.ExecString(ParamStr(2));
      except
        on E: Exception do begin
          mmOutput.Lines.Add(E.Message);
          Halt(1);
        end;
      end;
    end;
  end;
end;

procedure TTrainModelMainForm.Button1Click(Sender: TObject);
begin  
  var train_dataset: variant;
  var test_dataset: variant;
  var train_loader: variant;
  var test_loader: variant;
  var device: variant;
  var model: variant;

  var dataset := CreateDataSet('C:\Users\lucas\Documents\Embarcadero\Studio\Projects\P4D_AI_ML\demos\PyTorch\PyTorchTransferLearning\server\Win32\Debug\training_data');  
  SplitDataSetTrainAndTest(dataset, train_dataset, test_dataset);  
  CreateDataLoaders(train_dataset, test_dataset, train_loader, test_loader);  
  DefineNeuralNetwork(device, model);
  TrainNeuralNetwork(train_loader, test_loader, train_dataset, test_dataset, 
    device, model);
end;


end.
