unit Form.DataCollection;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Media, FMX.Layouts,
  System.Messaging, System.Permissions, System.Actions, FMX.ActnList,
  FMX.StdActns, FMX.MediaLibrary.Actions, System.ImageList, FMX.ImgList,
  Frame.CameraLayout;

type
  TTrainingClass = (ThumbsUp, ThumbsDown);

  TDataCollectionForm = class(TForm)
    camLayout: TCameraLayoutFrame;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FProfile: string;
    procedure SaveImageAsync(Sender: TObject; AImageName: string; AImage: TBitmap);
  public
    function GetTrainingClass(): TTrainingClass; virtual; abstract;
  public
    property Profile: string read FProfile write FProfile;
  end;

  TTrainingClassHelper = record helper for TTrainingClass
  public
    function ToString(): string;
  end;

var
  DataCollectionForm: TDataCollectionForm;

implementation

uses
  Remote.ClientModule;

{$R *.fmx}

procedure TDataCollectionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TDataCollectionForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := true;
  if camLayout.IsSaving() then begin
    ShowMessage('Saving in progress.');
    CanClose := false;
  end;
end;

procedure TDataCollectionForm.FormCreate(Sender: TObject);
begin
  camLayout.OnSaveAsync := SaveImageAsync;
  var LCount := ClientModule.TrainingClassClient.CountClass(Profile,
      GetTrainingClass().ToString());
  camLayout.UpdateCounter(LCount);
end;

procedure TDataCollectionForm.SaveImageAsync(Sender: TObject; AImageName: string;
  AImage: TBitmap);
begin
  var LCount := 0;
  var LStream := TMemoryStream.Create();
  try
    AImage.SaveToStream(LStream);
    LCount := ClientModule.TrainingClassClient.SendImage(Profile,
      GetTrainingClass().ToString(), AImageName, LStream);
  finally
    LStream.Free();
  end;

  TThread.Queue(nil, procedure() begin
    camLayout.UpdateCounter(LCount);
  end);
end;

{ TTrainingClassHelper }

function TTrainingClassHelper.ToString: string;
begin
  Result := String.Empty;
  case Self of
    ThumbsUp: Result := 'ThumbsUp';
    ThumbsDown: Result := 'ThumbsDown';
  end;
end;

end.
