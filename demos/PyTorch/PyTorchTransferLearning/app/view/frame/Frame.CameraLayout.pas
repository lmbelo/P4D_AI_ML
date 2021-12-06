unit Frame.CameraLayout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, FMX.Media,
  System.Actions, FMX.ActnList, System.Generics.Collections;

type
  TOnSaveAsync = procedure(Sender: TObject; AImageName: string; AImage: TBitmap) of object;
  TBitmapQueue = TObjectQueue<TBitmap>;
  TCameraLayoutFrame = class(TFrame)
    actList: TActionList;
    actSwitch: TAction;
    actTakePhoto: TAction;
    actAccept: TAction;
    actDecline: TAction;
    ccCameraComp: TCameraComponent;
    imgCamera: TImage;
    loTakePhotoActions: TLayout;
    loTakePhotoActionsCenter: TLayout;
    btnAccept: TSpeedButton;
    imgAccept: TImage;
    btnDecline: TSpeedButton;
    imgDecline: TImage;
    loBottom: TLayout;
    btnSwitch: TSpeedButton;
    imgSwitch: TImage;
    btnTakePhoto: TSpeedButton;
    crTakePhotoExternal: TCircle;
    crTakePhotoInternal: TCircle;
    loTop: TLayout;
    imgStopwatch: TImage;
    imgMenu: TImage;
    recBackgroud: TRectangle;
    imgThumbs: TImage;
    loThumbs: TLayout;
    lbCounter: TLabel;
    imgLast: TImage;
    Layout1: TLayout;
    imgSave: TImage;
    aiSave: TAniIndicator;
    procedure actSwitchExecute(Sender: TObject);
    procedure actAcceptExecute(Sender: TObject);
    procedure actDeclineExecute(Sender: TObject);
    procedure actListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actTakePhotoExecute(Sender: TObject);
    procedure ccCameraCompSampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
  private
    FSavingQueue: TObjectQueue<TBitmap>;
    FSaving: boolean;
    FOnSaveAsync: TOnSaveAsync;
    function GenerateImageName(): string;
    procedure SaveImage(const AImage: TBitmap);
    procedure TryRunSaveQueueAsync();
    procedure InitSaveAsync(const AImage: TBitmap);
    procedure EndSaveAsync();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function IsStreaming(): boolean;
    procedure StartStreaming;
    procedure StopStreaming;
    function IsSaving(): boolean;
    procedure UpdateCounter(const ACounter: integer);
  public
    property OnSaveAsync: TOnSaveAsync read FOnSaveAsync write FOnSaveAsync;
  end;

implementation

uses
  System.Messaging, FMX.Platform;

type
  TBitmapHelper = class helper for TBitmap
  public
    function Clone(): TBitmap;
  end;

  TSafeBitmapQueueHelper = class helper for TBitmapQueue
  public
    procedure Enqueue(const ABitmap: TBitmap);
    procedure Dequeue();
    function Peek: TBitmap;
    procedure Clear();
    function Count: integer;
    procedure Safe(AProc: TProc);
  end;

{$R *.fmx}

procedure TCameraLayoutFrame.InitSaveAsync(const AImage: TBitmap);
begin
  TThread.Synchronize(nil, procedure() begin
    aiSave.Visible := true;
    aiSave.Enabled := true;
    imgSave.Bitmap.Assign(AImage);
    imgSave.Visible := true;
  end);
end;

function TCameraLayoutFrame.IsSaving: boolean;
begin
  Result := FSaving;
end;

function TCameraLayoutFrame.IsStreaming: boolean;
begin
  Result := ccCameraComp.Active;
end;

procedure TCameraLayoutFrame.SaveImage(const AImage: TBitmap);
begin
  imgLast.Bitmap.Assign(AImage);
  if Assigned(FOnSaveAsync) then begin
    FSavingQueue.Enqueue(AImage.Clone());
    TryRunSaveQueueAsync();
  end;
end;

procedure TCameraLayoutFrame.TryRunSaveQueueAsync;
begin
  if not FSaving then begin
    FSaving := true;
    TThread.CreateAnonymousThread(procedure() begin
      var LCount := FSavingQueue.Count;
      while (LCount > 0) do begin
        var LImage := FSavingQueue.Peek();
        try
          InitSaveAsync(LImage);
          try
            FOnSaveAsync(Self, GenerateImageName(), LImage);
            //If something went wrong, keep currnt syncinc img on the screen
            EndSaveAsync();
          except
            on E: Exception do begin
              TThread.Synchronize(nil, procedure() begin
                FSavingQueue.Enqueue(LImage.Clone());
                ShowMessage('Failed to save file.'
                  + sLineBreak
                  + E.Message);
              end);
              FSaving := false;
              Break;
            end;
          end;
        finally
          FSavingQueue.Dequeue();
        end;

        FSavingQueue.Safe(procedure() begin
          LCount := FSavingQueue.Count;
          if (LCount = 0) then
            FSaving := false;
        end);
      end;
    end).Start();
  end;
end;

procedure TCameraLayoutFrame.StartStreaming;
begin
  ccCameraComp.Active := true;
end;

procedure TCameraLayoutFrame.StopStreaming;
begin
  ccCameraComp.Active := false;
end;

procedure TCameraLayoutFrame.UpdateCounter(const ACounter: integer);
begin
  lbCounter.Text := String.Format('%.3d', [ACounter]);
end;

procedure TCameraLayoutFrame.actAcceptExecute(Sender: TObject);
begin
  SaveImage(imgCamera.Bitmap);
  StartStreaming();
end;

procedure TCameraLayoutFrame.actDeclineExecute(Sender: TObject);
begin
  StartStreaming();
end;

procedure TCameraLayoutFrame.actListUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  loTakePhotoActionsCenter.Visible := not IsStreaming();
  actTakePhoto.Enabled := IsStreaming();
  actSwitch.Enabled := IsStreaming();
end;

procedure TCameraLayoutFrame.actSwitchExecute(Sender: TObject);
begin
  case ccCameraComp.Kind of
    TCameraKind.FrontCamera: ccCameraComp.Kind := TCameraKind.BackCamera;
    else
      ccCameraComp.Kind := TCameraKind.FrontCamera
  end;
end;

procedure TCameraLayoutFrame.actTakePhotoExecute(Sender: TObject);
begin
  ccCameraComp.SampleBufferToBitmap(imgCamera.Bitmap, true);
  StopStreaming();
end;

procedure TCameraLayoutFrame.ccCameraCompSampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  TThread.Synchronize(TThread.CurrentThread, procedure() begin
    ccCameraComp.SampleBufferToBitmap(imgCamera.Bitmap, true);
  end);
end;

constructor TCameraLayoutFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSavingQueue := TBitmapQueue.Create(True);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage,
    procedure(const Sender: TObject; const M: TMessage) begin
      case TApplicationEventMessage(M).Value.Event of
        TApplicationEvent.BecameActive: StartStreaming();
        TApplicationEvent.WillBecomeInactive: StopStreaming();
      end;
    end);
  StartStreaming();
end;

destructor TCameraLayoutFrame.Destroy;
begin
  StopStreaming();
  FSavingQueue.Clear();
  while FSaving do
    Sleep(100);
  FSavingQueue.Free();
  inherited;
end;

procedure TCameraLayoutFrame.EndSaveAsync;
begin
  TThread.Synchronize(nil, procedure() begin
    imgSave.Visible := false;
    aiSave.Visible := false;
    aiSave.Enabled := false;
  end);
end;

function TCameraLayoutFrame.GenerateImageName: string;
begin
  Result := Format('img_%s.bmp', [lbCounter.Text]);
end;

{ TSafeBitmapQueueHelper }

procedure TSafeBitmapQueueHelper.Clear;
begin
  Safe(procedure begin
    inherited Clear();
  end);
end;

function TSafeBitmapQueueHelper.Count: integer;
begin
  var LCount: integer;
  Safe(procedure begin
    LCount := inherited Count;
  end);
  Result := LCount;
end;

procedure TSafeBitmapQueueHelper.Dequeue;
begin
  Safe(procedure begin
    inherited Dequeue();
  end);
end;

procedure TSafeBitmapQueueHelper.Enqueue(const ABitmap: TBitmap);
begin
  Safe(procedure begin
    inherited Enqueue(ABitmap);
  end);
end;

function TSafeBitmapQueueHelper.Peek: TBitmap;
begin
  var LBitmap: TBitmap;
  Safe(procedure begin
    LBitmap := inherited Peek();
  end);
  Result := LBitmap;
end;

procedure TSafeBitmapQueueHelper.Safe(AProc: TProc);
begin
  TMonitor.Enter(Self);
  try
    AProc();
  finally
    TMonitor.Exit(Self);
  end;
end;

{ TBitmapHelper }

function TBitmapHelper.Clone: TBitmap;
begin
 Result := TBitmap.Create(Self.Width, Self.Height);
 Result.CopyFromBitmap(Self);
end;

end.
