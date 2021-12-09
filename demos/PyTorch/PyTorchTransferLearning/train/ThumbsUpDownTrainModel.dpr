program ThumbsUpDownTrainModel;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {TrainModelMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTrainModelMainForm, TrainModelMainForm);
  Application.Run;
end.
