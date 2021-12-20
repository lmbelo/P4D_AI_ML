program ThumbsUpDownTrainModel;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {TrainModelMainForm},
  CompModule in '..\CompModule.pas' {PyComps: TDataModule},
  TrainModel in '..\TrainModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTrainModelMainForm, TrainModelMainForm);
  Application.Run;
end.
