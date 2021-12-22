program ThumbsUpDownApp;









uses
  System.StartUpCopy,
  FMX.Forms,
  Frame.CameraLayout in 'view\frame\Frame.CameraLayout.pas' {CameraLayoutFrame: TFrame},
  Frame.Menu in 'view\frame\Frame.Menu.pas' {MenuFrame: TFrame},
  Form.DataCollection in 'view\form\Form.DataCollection.pas' {DataCollectionForm},
  Form.Main in 'view\form\Form.Main.pas' {MainForm},
  Form.DataCollection.ThumbsUp in 'view\form\Form.DataCollection.ThumbsUp.pas' {ThumbsUpForm},
  Form.DataCollection.ThumbsDown in 'view\form\Form.DataCollection.ThumbsDown.pas' {ThumbsDownForm},
  Frame.TrainingClassSelection in 'view\frame\Frame.TrainingClassSelection.pas' {TrainingClassSelectionFrame: TFrame},
  Remote.ClientClasses in 'remote\Remote.ClientClasses.pas',
  Remote.ClientModule in 'remote\Remote.ClientModule.pas' {ClientModule: TDataModule},
  Form.TrainModel in 'view\form\Form.TrainModel.pas' {TrainModelForm},
  Form.Classify in 'view\form\Form.Classify.pas' {ClassifyForm},
  BitmapHelper in 'utils\BitmapHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TClientModule, ClientModule);
  Application.Run;
end.
