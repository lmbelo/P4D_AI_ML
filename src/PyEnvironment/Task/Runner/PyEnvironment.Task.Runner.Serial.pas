unit PyEnvironment.Task.Runner.Serial;

interface

uses
  System.Classes, System.SysUtils, System.Threading, System.Generics.Collections,
  PyEnvironment.Task,
  PyEnvironment.Task.Runner,
  PyEnvironment.Notification,
  PyEnvironment.Task.Model;

type
  TPyEnvironmentExecTask = procedure(const ASender: TObject; const AModel: TPyEnvironmentTaskModel) of object;
  TPyEnvironmentCancelTask = procedure(const ASender: TObject; const AModel: TPyEnvironmentTaskModel) of object;
  TPyEnvironmentWaitTask = procedure(const ASender: TObject; const AModel: TPyEnvironmentTaskModel) of object;
  TPyEnvironmentTaskCompleted = procedure(const ASender: TObject; const ATask: TPyEnvironmentTaskModel) of object;
  TPyEnvironmentTasksCompleted = procedure(const ASender: TObject; const ATasks: TArray<TPyEnvironmentTaskModel>) of object;
  TPyEnvironmentTasksTerminate = procedure(const ASender: TObject; const ACanceled: boolean) of object;

  TPyEnvironmentTaskRunnerSerial = class(TComponent, IPyEnvironmentTaskRunner)
  private
    FSerialRunner: ITask;
    FTaskCollection: IPyEnvironmentTaskCollection;
    FNotificationQueue: TThreadedQueue<byte>;
    FRunning: TPyEnvironmentTaskModel;
    FTasksCompleted: TObjectList<TPyEnvironmentTaskModel>;
    FCanceled: boolean;
    //Events
    FOnExecute: TPyEnvironmentExecTask;
    FOnComplete: TPyEnvironmentTaskCompleted;
    FOnWait: TPyEnvironmentWaitTask;
    FOnCancel: TPyEnvironmentCancelTask;
    FOnCompleteAll: TPyEnvironmentTasksCompleted;
    FOnTerminate: TPyEnvironmentTasksTerminate;
    //Execution loop
    procedure Start();
    procedure Stop(const AWait: boolean = false);
    //Runner operations
    procedure RunNotificationTasks(const ANotification: TEnvironmentNotification);
    //Operations
    function MoveNext(const ANotification: TEnvironmentNotification): boolean;
    procedure CancelCurrent();
    procedure WaitCurrent();
    procedure DoCurrentTaskCompleted;
    procedure DoOnTasksCompleted(const ANotification: TEnvironmentNotification);
    //Predicates
    function CanRunNext(const AModel: TPyEnvironmentTaskModel): boolean;
  public
    constructor Create(AOwner: TComponent; const ATaskCollection: IPyEnvironmentTaskCollection); reintroduce;
    destructor Destroy(); override;
    //Task operations
    procedure Run(const ANotification: TEnvironmentNotification);
    procedure Cancel(const ACancelAll: boolean = false);
    procedure Wait();
    //Query status
    function IsRunning(): boolean;
    function IsCanceled(): boolean;
  published
    property OnExecute: TPyEnvironmentExecTask read FOnExecute write FOnExecute;
    property OnComplete: TPyEnvironmentTaskCompleted read FOnComplete write FOnComplete;
    property OnCancel: TPyEnvironmentCancelTask read FOnCancel write FOnCancel;
    property OnWait: TPyEnvironmentWaitTask read FOnWait write FOnWait;
    property OnCompleteAll: TPyEnvironmentTasksCompleted read FOnCompleteAll write FOnCompleteAll;
    property OnTerminate: TPyEnvironmentTasksTerminate read FOnTerminate write FOnTerminate;
  end;

implementation

{ TPyEnvironmentTaskRunner }

constructor TPyEnvironmentTaskRunnerSerial.Create(AOwner: TComponent;
  const ATaskCollection: IPyEnvironmentTaskCollection);
begin
  inherited Create(AOwner);
  Name := 'TaskRunner';
  SetSubComponent(true);

  FRunning := nil;
  FNotificationQueue := TThreadedQueue<byte>.Create();
  FTasksCompleted := TObjectList<TPyEnvironmentTaskModel>.Create();
  FTaskCollection := ATaskCollection;

  Start();
end;

destructor TPyEnvironmentTaskRunnerSerial.Destroy;
begin
  Stop(true);
  FTasksCompleted.Free();
  FNotificationQueue.Free();
  inherited;
end;

procedure TPyEnvironmentTaskRunnerSerial.Start;
begin
  FCanceled := false;
  FSerialRunner := TTask.Run(procedure()
    begin
      try
        while not FCanceled do begin
          if (FNotificationQueue.QueueSize > 0) then
            RunNotificationTasks(FNotificationQueue.PopItem());
          Sleep(100);
        end;
      finally
        if Assigned(FOnTerminate) then
          FOnTerminate(Self, FCanceled);
      end;
    end, FTaskCollection.GetThreadPool());
end;

procedure TPyEnvironmentTaskRunnerSerial.Stop(const AWait: boolean);
begin
  if not FCanceled then begin
    FCanceled := true;
    //FSerialRunner.Cancel();
  end;
  if AWait then
    try
      FSerialRunner.Wait();
    except
      //
    end;
end;

procedure TPyEnvironmentTaskRunnerSerial.DoCurrentTaskCompleted;
begin
  FTasksCompleted.Add(FRunning);
  TMonitor.Enter(Self);
  try
    if Assigned(FOnComplete) then
      FOnComplete(Self, FRunning);
    FRunning := nil;
    TMonitor.Pulse(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TPyEnvironmentTaskRunnerSerial.DoOnTasksCompleted(const ANotification: TEnvironmentNotification);
begin
  if Assigned(FOnCompleteAll) then
    FOnCompleteAll(Self, FTasksCompleted.ToArray);

  FTasksCompleted.Clear();
end;

procedure TPyEnvironmentTaskRunnerSerial.Run(const ANotification: TEnvironmentNotification);
begin
  FNotificationQueue.PushItem(ANotification);
end;

function TPyEnvironmentTaskRunnerSerial.CanRunNext(
  const AModel: TPyEnvironmentTaskModel): boolean;
var
  LTask: IFuture<boolean>;
begin
  LTask := AModel.Task;
  Result := not (
    ((LTask.Status = TTaskStatus.Exception) or not LTask.Value)
    and AModel.AbortOnFailure);
end;

function TPyEnvironmentTaskRunnerSerial.MoveNext(
  const ANotification: TEnvironmentNotification): boolean;
begin
  TMonitor.Enter(Self);
  try
    Result := FTaskCollection.PickTask(ANotification, FRunning);
    TMonitor.Pulse(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TPyEnvironmentTaskRunnerSerial.CancelCurrent;
begin
  TMonitor.Enter(Self);
  try
    if Assigned(FRunning) then
      FRunning.Task.Cancel();
    if Assigned(FOnCancel) then
      FOnCancel(Self, FRunning);
    TMonitor.Pulse(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TPyEnvironmentTaskRunnerSerial.WaitCurrent;
begin
  TMonitor.Enter(Self);
  try
    if Assigned(FOnWait) then
      FOnWait(Self, FRunning);
    if Assigned(FRunning) then
      FRunning.Task.Wait();
    TMonitor.Pulse(Self);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TPyEnvironmentTaskRunnerSerial.RunNotificationTasks(
  const ANotification: TEnvironmentNotification);
var
  LTask: IFuture<boolean>;
begin
  while not FCanceled and MoveNext(ANotification) do begin
    LTask := FRunning.Task.Start();
    try
      if Assigned(FOnExecute) then
        FOnExecute(Self, FRunning);

      try
        LTask.Wait();
      except
        //
      end;

      if not CanRunNext(FRunning) then
        Break;
    finally
      DoCurrentTaskCompleted();
    end;
  end;

  if (ANotification = INTERNAL_READY_NOTIFICATION) then
    DoOnTasksCompleted(ANotification);
end;

procedure TPyEnvironmentTaskRunnerSerial.Cancel(const ACancelAll: boolean);
begin
  if ACancelAll then
    Stop();
  CancelCurrent();
end;

procedure TPyEnvironmentTaskRunnerSerial.Wait;
begin
  WaitCurrent();
end;

function TPyEnvironmentTaskRunnerSerial.IsCanceled: boolean;
begin
  Result := FCanceled;
end;

function TPyEnvironmentTaskRunnerSerial.IsRunning: boolean;
begin
  Result := Assigned(FRunning);
end;

initialization
  RegisterClass(TPyEnvironmentTaskRunnerSerial);

finalization
  UnRegisterClass(TPyEnvironmentTaskRunnerSerial);

end.
