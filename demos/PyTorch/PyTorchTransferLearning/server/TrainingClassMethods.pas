unit TrainingClassMethods;

interface

uses System.SysUtils, System.Classes, System.Json,
    DataSnap.DSProviderDataModuleAdapter,
    Datasnap.DSServer, Datasnap.DSAuth, FMX.Forms;

type
  TTrainingClass = class(TDSServerModule)
  private
    procedure BuildDir(const ADir: string);
    function BuildImagesFolder(): string;
    function BuildProfileFolder(const ABaseDir, AProfile: string): string;
    function BuildClassFolder(const ABaseDir, ATrainingClass: string): string;
    function BuildImagePath(const ABaseDir, AImageName: string): string;
    function GetCount(const ABaseDir: string): integer;
  public
    const IMAGES_FOLDER = 'training_data';
  public
    { Public declarations }
    function CountClass(const AProfile, ATrainingClass: string): integer;
    function SendImage(const AProfile, ATrainingClass, AImageName: string;
      const AImage: TStream): integer;
    procedure Clear(const AProfile, ATrainingClass: string);

    function TrainModel(const AProfile: string): TJSONValue;
    function Recognize(const AProfile: string; const AImage: TStream): TJSONValue;
  end;

implementation

uses
  System.IOUtils, ServerContainerUnit1, DSSession,
  ExecProc.Win;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TTrainingClass }

function TTrainingClass.BuildClassFolder(const ABaseDir,
  ATrainingClass: string): string;
begin
  Result := TPath.Combine(ABaseDir, ATrainingClass);
  BuildDir(Result);
end;

procedure TTrainingClass.BuildDir(const ADir: string);
begin
  if not TDirectory.Exists(ADir) then
    TDirectory.CreateDirectory(ADir);
end;

function TTrainingClass.BuildImagePath(const ABaseDir, AImageName: string): string;
begin
  Result := TPath.Combine(ABaseDir, AImageName);
end;

function TTrainingClass.BuildProfileFolder(const ABaseDir,
  AProfile: string): string;
begin
  Result := TPath.Combine(ABaseDir, AProfile);
  BuildDir(Result);
end;

procedure TTrainingClass.Clear(const AProfile, ATrainingClass: string);
begin
  TDirectory.Delete(BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass), true);
end;

function TTrainingClass.CountClass(const AProfile,
  ATrainingClass: string): integer;
begin
  Result := GetCount(BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass));
end;

function TTrainingClass.GetCount(const ABaseDir: string): integer;
begin
  Result := Length(TDirectory.GetFiles(ABaseDir));
end;

function TTrainingClass.BuildImagesFolder(): string;
begin
  var LBinDir := TPath.GetDirectoryName(ParamStr(0));
  var LRootDir := TDirectory.GetParent(LBinDir);
  Result := TPath.Combine(LRootDir, IMAGES_FOLDER, false);
  BuildDir(Result);
end;

function TTrainingClass.SendImage(const AProfile, ATrainingClass, AImageName: string;
  const AImage: TStream): integer;
begin
  var LDir := BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass);
  var LStream := TFileStream.Create(BuildImagePath(LDir, AImageName), fmCreate or fmOpenWrite);
  try
    LStream.Seek(0, soFromEnd);
    LStream.CopyFrom(AImage, AImage.Size)
  finally
    LStream.Free();
  end;

  Result := GetCount(LDir);
end;

function TTrainingClass.TrainModel(const AProfile: string): TJSONValue;
const
  CHANNEL_SUFIX = '_TRAIN_MODEL_CALLBACK';
  PROC = 'ThumbsUpDownTrainModelProc.exe';
  DEBUG_FLAG = 'd';
begin
  if not TFile.Exists(PROC) then
    raise Exception.Create('Train model application not found.');

  var LChannel := AProfile + CHANNEL_SUFIX;
  var LCallBackId := LChannel + '_' + TDSSessionManager.GetThreadSession().SessionName;

  //Only one training execution per profile session
  if TDSSessionManager.GetThreadSession().HasData(LChannel) then begin
    Result := TJSONObject.Create();
    TJSONObject(Result).AddPair(TJSONPair.Create(
      'error',
      'A training set is already running to the current session.'));
    TJSONObject(Result).AddPair(TJSONPair.Create(
      'callback_id',
      LCallBackId));
    Exit;
  end;

  TDSSessionManager.GetThreadSession().PutData(LChannel, DateTimeToStr(Now));

  var LModel := TPath.Combine(BuildProfileFolder(
    BuildImagesFolder(), AProfile), 'best_model.pth');

  if TFile.Exists(LModel) then
    TFile.Delete(LModel);

  var LCmd := PROC
    + ' ' + AProfile
    + ' ' + BuildProfileFolder(BuildImagesFolder(), AProfile)
    + ' ' + LModel;
//    + ' ' + DEBUG_FLAG; //The debug flag to hang on the proc. until you attach the debugger...

  var LSessionPredicate: TPredicate<string> := function(Data: string): boolean begin
    Result := Assigned(TDSSessionManager.Instance.Session[Data])
      and TDSSessionManager.Instance.Session[Data].IsValid;
  end;

  var LStdOutLogPath := TPath.Combine(BuildProfileFolder(BuildImagesFolder(), AProfile), 'stdout.log');
  if TFile.Exists(LStdOutLogPath) then
    TFile.Delete(LStdOutLogPath);

  var LSessionId := TDSSessionManager.GetThreadSession().SessionName;
  var LSessionActive := true;
  TThread.CreateAnonymousThread(procedure() begin

    var LResultCode := TExecCmdWin.ExecCmd(LCmd,
      procedure(AText: string) begin
        DSServer.BroadcastMessage(LChannel, LCallBackId,
          TJSONObject.Create(TJSONPair.Create('pipe', AText)));
        TFile.AppendAllText(LStdOutLogPath, AText);
      end,
      function(): boolean begin
        Result := (not LSessionActive)
          or not LSessionPredicate(LSessionId)
          or Application.Terminated;
      end);

    if (LResultCode = 0) and TFile.Exists(LModel) then begin
      DSServer.BroadcastMessage(LChannel, LCallBackId,
        TJSONObject.Create(TJSONPair.Create('done', true)));
    end else begin
      DSServer.BroadcastMessage(LChannel, LCallBackId,
        TJSONObject.Create(TJSONPair.Create('done', false)));
    end;

    if LSessionPredicate(LSessionId) then
      TDSSessionManager.Instance.Session[LSessionId].RemoveData(LChannel);

  end).Start();

  TDSSessionManager.Instance.AddSessionEvent(
    procedure(Sender: TObject; const EventType: TDSSessionEventType; const Session: TDSSession) begin
      case EventType of
        SessionClose: begin
          LSessionActive := false; //Is it a good idea? And if the training set takes more time than the session expire time? hmm...
        end;
      end;
    end);

  Result := TJSONObject.Create(TJSONPair.Create('callback_id', LCallBackId));
end;

function TTrainingClass.Recognize(const AProfile: string;
  const AImage: TStream): TJSONValue;
const
  PROC = 'ThumbsUpDownTrainedModelProc.exe';
  DEBUG_FLAG = 'DEBUG';
begin
  if not TFile.Exists(PROC) then
    raise Exception.Create('Trained model application not found.');

  var LImagePath := TPath.GetTempFileName();
  var LStream := TFileStream.Create(LImagePath, fmCreate or fmOpenWrite);
  try
    LStream.Seek(0, soFromEnd);
    LStream.CopyFrom(AImage, AImage.Size)
  finally
    LStream.Free();
  end;

  var LCmd := PROC
    + ' -mCHILD_PROC';
//    + ' ' + DEBUG_FLAG; //The debug flag to hang on the proc. until you attach the debugger...

  var LProc := TExecCmdWin.Create(LCmd, [TRedirect.stdout, TRedirect.stdin]);
  try
    var LWriter: TWriter;
    var LReader: TReader;
    var LProb: extended;
    LProc.Redirect(LReader, LWriter);
    try
      LWriter('RUN');
      var LStdOut := LReader();
      if (LStdOut = 'WAITING') then begin
        LWriter(LImagePath);
        LStdOut := LReader();
        if (LStdOut.StartsWith('ERROR')) then begin
          Exit(TJSONNumber.Create(-1));
        end else if TryStrToFloat(LStdOut, LProb) then begin
          LWriter('EXIT');
          Exit(TJSONNumber.Create(LProb));
        end;
      end;
    finally
      LProc.Kill();
    end;
  finally
    LProc.Free();
  end;
  Result := TJSONNumber.Create(-1);
end;

end.

