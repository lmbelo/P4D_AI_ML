unit PyEnvironment.Task.List;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Threading,
  PyEnvironment.Task,
  PyEnvironment.Task.Model,
  PyEnvironment.Notification;

type
  TPyEnvironmentTaskList = class(TInterfacedObject, IPyEnvironmentTaskCollection)
  private
    FThreadPool: TThreadPool;
    FDictionary: TObjectDictionary<TEnvironmentNotification, TObjectList<TPyEnvironmentTaskModel>>;
    function SafeExec<T>(const AFunc: TFunc<T>): T;
  public
    constructor Create(); overload;
    constructor Create(const AThreadPool: TThreadPool); overload;
    destructor Destroy(); override;

    function GetThreadPool(): TThreadPool;
    function CreateTask(const AFunc: TFunc<boolean>): TFuture<boolean>;

    function AddTask(const ATrigger: TEnvironmentNotification;
      const AModel: TPyEnvironmentTaskModel): boolean;
    function RemoveTask(const ATrigger: TEnvironmentNotification;
      const AId: integer): boolean;
    function PickTask(const ATrigger: TEnvironmentNotification;
      out AModel: TPyEnvironmentTaskModel): boolean;
    function HasTasks(const ATrigger: TEnvironmentNotification): boolean;
  end;

implementation

uses
  System.Types;

{ TPyEnvironmentTaskQueue }

constructor TPyEnvironmentTaskList.Create;
begin
  Create(TThreadPool.Default);
end;

constructor TPyEnvironmentTaskList.Create(const AThreadPool: TThreadPool);
var
  LEnvironmentNotification: TEnvironmentNotification;
begin
  FThreadPool := AThreadPool;
  FDictionary := TObjectDictionary<TEnvironmentNotification, TObjectList<TPyEnvironmentTaskModel>>.Create([doOwnsValues]);
  for LEnvironmentNotification := Low(TPyEnvironmentNotifications) to High(TPyEnvironmentNotifications) do
    FDictionary.Add(LEnvironmentNotification, TObjectList<TPyEnvironmentTaskModel>.Create());
end;

function TPyEnvironmentTaskList.CreateTask(
  const AFunc: TFunc<boolean>): TFuture<boolean>;
begin
  Result := TFuture<boolean>.Create(TObject(nil), TFunctionEvent<boolean>(nil),
    AFunc, GetThreadPool());
end;

destructor TPyEnvironmentTaskList.Destroy;
begin
  FDictionary.Free();
  inherited;
end;

function TPyEnvironmentTaskList.GetThreadPool: TThreadPool;
begin
  Result := FThreadPool;
end;

function TPyEnvironmentTaskList.HasTasks(
  const ATrigger: TEnvironmentNotification): boolean;
begin
  Result := SafeExec<boolean>(function(): boolean begin
    Result := FDictionary.Items[ATrigger].Count > 0;
  end);
end;

function TPyEnvironmentTaskList.AddTask(
  const ATrigger: TEnvironmentNotification;
  const AModel: TPyEnvironmentTaskModel): boolean;
begin
  Result := SafeExec<boolean>(function(): boolean begin
    Result := FDictionary.Items[ATrigger].Add(AModel) >= 0;
  end);
end;

function TPyEnvironmentTaskList.PickTask(
  const ATrigger: TEnvironmentNotification;
  out AModel: TPyEnvironmentTaskModel): boolean;
var
  LModel: TPyEnvironmentTaskModel;
begin
  LModel := nil;
  Result := SafeExec<boolean>(function(): boolean
  var
    LNotificationList: TList<TPyEnvironmentTaskModel>;
  begin
    LNotificationList := FDictionary.Items[ATrigger];
    Result := LNotificationList.Count > 0;
    if not Result then
      Exit(false);
    LModel := LNotificationList.First();
    Result := Assigned(LNotificationList.Extract(LModel));
  end);
  AModel := LModel;
end;

function TPyEnvironmentTaskList.RemoveTask(
  const ATrigger: TEnvironmentNotification; const AId: integer): boolean;
begin
  Result := SafeExec<boolean>(
    function(): boolean
    var
      I: integer;
    begin
      for I := 0 to FDictionary.Items[ATrigger].Count - 1 do
        if FDictionary.Items[ATrigger][I].Id = AId then begin
          FDictionary.Items[ATrigger].Remove(FDictionary.Items[ATrigger][I]);
          Exit(true);
        end;
      Result := false;
    end);
end;

function TPyEnvironmentTaskList.SafeExec<T>(const AFunc: TFunc<T>): T;
begin
  TMonitor.Enter(FDictionary);
  try
    Result := AFunc();
    TMonitor.Pulse(FDictionary);
  finally
    TMonitor.Exit(FDictionary);
  end;
end;

end.
