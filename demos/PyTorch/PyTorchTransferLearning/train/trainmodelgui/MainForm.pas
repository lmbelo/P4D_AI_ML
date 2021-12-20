unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  PythonEngine, FMX.PythonGUIInputOutput, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, TorchVision, PyTorch, PyCommon, PyModule, PyPackage,
  NumPy, FMX.StdCtrls, Winapi.Windows, CompModule;

type
  TTrainModelMainForm = class(TForm)
    mmOutput: TMemo;
    btnTrain: TButton;
    procedure btnTrainClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  TrainModelMainForm: TTrainModelMainForm;

implementation

uses
  TrainModel;

{$R *.fmx}

procedure TTrainModelMainForm.btnTrainClick(Sender: TObject);
begin
  var dataset_path: string;
  var trained_model_path := 'best_model.pth';
  if SelectDirectory('Select the thumbs up/down images directory', '', dataset_path) then begin
    var LTrainModel := TTrainModel.Create();
    try
      LTrainModel.Train(dataset_path, trained_model_path);
    finally
      LTrainModel.Free();
    end;
  end;
end;

procedure TTrainModelMainForm.FormCreate(Sender: TObject);
begin
  PyComps := TPyComps.Create(Self,
      procedure(AText: string) begin
        mmOutput.Lines.Add('In: ' + AText);
      end,
      procedure(AText: string) begin
        mmOutput.Lines.Add('Out: ' + AText);
      end);
end;

end.
