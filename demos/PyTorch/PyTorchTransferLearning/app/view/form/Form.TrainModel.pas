unit Form.TrainModel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, Datasnap.DSClientRest,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, System.JSON;

type
  TTrainModelForm = class(TForm)
    ToolBar1: TToolBar;
    lbProfile: TLabel;
    mmPipe: TMemo;
    btnTrain: TButton;
    procedure btnTrainClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProfile: string;
    FChannel: TDSRestClientChannel;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; const AProfile: string); reintroduce;
    property Profile: string read FProfile;
  end;

var
  TrainModelForm: TTrainModelForm;

implementation

uses
  Remote.ClientModule;

{$R *.fmx}

procedure TTrainModelForm.btnTrainClick(Sender: TObject);
begin
  var LMsg: string;
  var LResult := ClientModule.TrainingClassClient.TrainModel(FProfile, LMsg);
  var LResultStr: string;
  if LResult.TryGetValue<string>('callback_id', LResultStr) then begin
    var LCallback := TDSRestClientCallback.Create(FChannel, LResultStr,
      function(AValue: TJSONValue; ADataType: string): boolean begin
        TThread.Synchronize(nil, procedure() begin
          var LValue: string;
          var LStatus: boolean;
          if AValue is TJSONObject then begin
            if AValue.TryGetValue<string>('pipe', LValue) then
              mmPipe.Lines.Text := mmPipe.Lines.Text + LValue
            else if AValue.TryGetValue<boolean>('done', LStatus) then begin
              if LStatus then
                mmPipe.Lines.Add('Model successfuly trained.')
              else
                mmPipe.Lines.Add('Model not trained. Check the pipe lines for errors.');
            end else if AValue.TryGetValue<string>('error', LValue) then
              mmPipe.Lines.Add('Error: ' + LValue);
            mmPipe.GoToTextEnd();
          end;
        end);
        Result := true;
      end);

    if FChannel.Connected then
      FChannel.RegisterCallback(LCallback)
    else
      FChannel.Connect(LCallback);
  end else if LResult.TryGetValue<string>('error', LResultStr) then begin
    raise Exception.Create(LResultStr);
  end;
end;

constructor TTrainModelForm.Create(AOwner: TComponent; const AProfile: string);
const
  CHANNEL_SUFIX = '_TRAIN_MODEL_CALLBACK';
begin
  inherited Create(AOwner);
  FProfile := AProfile;
  lbProfile.Text := AProfile;
  FChannel := TDSRestClientChannel.Create('', FProfile + CHANNEL_SUFIX, ClientModule.DSRestConnection1);
end;

procedure TTrainModelForm.FormDestroy(Sender: TObject);
begin
  FChannel.Free();
end;

end.
