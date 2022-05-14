(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment'   Copyright (c) 2021                     *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironment  layer                                  *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
unit PyEnvironment;

interface

uses
  System.Classes, System.SysUtils, System.Threading, System.Generics.Collections,
  PythonEngine,
  PyEnvironment.Distribution,
  PyEnvironment.Notification,
  PyEnvironment.Task,
  PyEnvironment.Task.Runner,
  PyEnvironment.Task.Runner.Serial;

type
  TPyEnvironmentBeforeSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentBeforeActivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterActivate = procedure(Sender: TObject; const APythonVersion: string; const AActivated: boolean) of object;
  TPyEnvironmentBeforeDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentReady = procedure(Sender: TObject; const APythonVersion: string) of object;

  TPyCustomEnvironment = class(TComponent, IEnvironmentNotifier<TPyCustomEnvironment>, IPyEnvironmentTaskCollection)
  private
    FDistributions: TPyDistributionCollection;
    FAutoLoad: boolean;
    FPythonEngine: TPythonEngine;
    FPythonVersion: string;
    FAsync: boolean;
    //Events
    FBeforeSetup: TPyEnvironmentBeforeSetup;
    FAfterSetup: TPyEnvironmentAfterSetup;
    FBeforeActivate: TPyEnvironmentBeforeActivate;
    FAfterActivate: TPyEnvironmentAfterActivate;
    FBeforeDeactivate: TPyEnvironmentBeforeDeactivate;
    FAfterDeactivate: TPyEnvironmentAfterDeactivate;
    //Delegators
    FEnvironmentNotifier: IEnvironmentNotifier<TPyCustomEnvironment>;
    FEnvironmentTaskList: IPyEnvironmentTaskCollection;
    FEnvironmentTaskRunner: TPyEnvironmentTaskRunnerSerial;
    FOnReady: TPyEnvironmentReady;
    procedure SetEnvironments(const ADistributions: TPyDistributionCollection);
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
    procedure DoAutoLoad;
    //Hooked routines
    procedure InternalSetup(APythonVersion: string);
    function InternalActivate(APythonVersion: string): boolean;
    procedure InternalDeactivate();
    //Install and Import task
    function CreateSetupAndActivateTasks(): IFuture<boolean>;
    procedure ScheduleSetupAndActivateTasks();
    //Events handlers
    procedure DoInit;
    procedure DoBeforeSetup(APythonVersion: string);
    procedure DoAfterSetup(APythonVersion: string; const ADistribution: TPyDistribution);
    procedure DoBeforeActivate(APythonVersion: string);
    procedure DoAfterActivate(APythonVersion: string; const ADistribution: TPyDistribution; var Result: Boolean);
    procedure DoBeforeDeactivate;
    procedure DoAfterDeactivate;
    procedure DoInternalReady;
    procedure DoFinalize();
  protected
    property EnvironmentNotifier: IEnvironmentNotifier<TPyCustomEnvironment>
      read FEnvironmentNotifier
      implements IEnvironmentNotifier<TPyCustomEnvironment>;
    property EnvironmentTaskList: IPyEnvironmentTaskCollection
      read FEnvironmentTaskList
      implements IPyEnvironmentTaskCollection;
  protected
    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    procedure SetPythonVersion(const Value: string); virtual;
    function CreateCollection(): TPyDistributionCollection; virtual; abstract;
    procedure Prepare(); virtual;
    //IEnvironmentNotifier<TPyCustomEnvironment> delegation
    procedure InternalNotifyAll(ANotification: TEnvironmentNotification;
      ADistribution: TPyDistribution); dynamic;
    //IPyEnvironmentTask delegation
    procedure InternalRunTasks(ANotification: TEnvironmentNotification); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Setup(APythonVersion: string);
    function SetupAsync(APythonVersion: string): ITask;

    function Activate(APythonVersion: string): boolean;
    procedure Deactivate();
  public
    property Distributions: TPyDistributionCollection read FDistributions write SetEnvironments;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonVersion: string read FPythonVersion write SetPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
    property Async: boolean read FAsync write FAsync;
    property TaskRunner: TPyEnvironmentTaskRunnerSerial read FEnvironmentTaskRunner;
  published
    property BeforeSetup: TPyEnvironmentBeforeSetup read FBeforeSetup write FBeforeSetup;
    property AfterSetup: TPyEnvironmentAfterSetup read FAfterSetup write FAfterSetup;
    property BeforeActivate: TPyEnvironmentBeforeActivate read FBeforeActivate write FBeforeActivate;
    property AfterActivate: TPyEnvironmentAfterActivate read FAfterActivate write FAfterActivate;
    property BeforeDeactivate: TPyEnvironmentBeforeDeactivate read FBeforeDeactivate write FBeforeDeactivate;
    property AfterDeactivate: TPyEnvironmentAfterDeactivate read FAfterDeactivate write FAfterDeactivate;
    property OnReady: TPyEnvironmentReady read FOnReady write FOnReady;
  end;

  TPyEnvironment = class(TPyCustomEnvironment)
  private
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  published
    property AutoLoad;
    property PythonVersion;
    property PythonEngine;
    property Async;
    property TaskRunner;
  end;

implementation

uses
  TypInfo,
  System.IOUtils,
  PyCommon.Path,
  PyEnvironment.Task.Model,
  PyEnvironment.Task.List;

type
  __TReader = class(TReader);
  __TWriter = class(TWriter);

{ TPyCustomEnvironment }

constructor TPyCustomEnvironment.Create(AOwner: TComponent);
begin
  FDistributions := CreateCollection();
  FEnvironmentNotifier := TEnvironmentBroadcaster<TPyCustomEnvironment>.Create(Self);
  FEnvironmentTaskList := TPyEnvironmentTaskList.Create();
  FEnvironmentTaskRunner := TPyEnvironmentTaskRunnerSerial.Create(Self, FEnvironmentTaskList);
  with (FEnvironmentTaskRunner as IInterfaceComponentReference) do begin
    GetComponent().FreeNotification(Self);
  end;
  inherited;
end;

destructor TPyCustomEnvironment.Destroy;
begin
  DoFinalize();
  FEnvironmentTaskRunner.Cancel(true);
  if not (csDesigning in ComponentState) then
    FEnvironmentTaskRunner.Wait();
  FDistributions.Free();
  inherited;
end;

procedure TPyCustomEnvironment.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState)
    and FAutoLoad
      and Assigned(FPythonEngine)
        and not (PythonVersion.IsEmpty) then
          DoAutoLoad();
  DoInit();
end;

procedure TPyCustomEnvironment.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) then begin
    if (AComponent = FPythonEngine) then
      FPythonEngine := nil;

    if Assigned(FEnvironmentTaskRunner)
      and (AComponent = (FEnvironmentTaskRunner as IInterfaceComponentReference).GetComponent()) then
        FEnvironmentTaskRunner := nil;
  end;
end;

function TPyCustomEnvironment.CreateSetupAndActivateTasks: IFuture<boolean>;
var
  LTask: TFuture<boolean>;
begin
  LTask := FEnvironmentTaskList.CreateTask(function(): boolean
    begin
      try
        Setup(FPythonVersion);

        LTask.CheckCanceled();

        TThread.Queue(nil, procedure() begin
          Activate(FPythonVersion);
        end);
      finally
        LTask := nil; //Avoid memory leak of the nested LTask var
      end;
      Result := true;
    end);
  Result := LTask;
end;

procedure TPyCustomEnvironment.ScheduleSetupAndActivateTasks;
var
  LTask: IFuture<boolean>;
begin
  LTask := CreateSetupAndActivateTasks();

  FEnvironmentTaskList.AddTask(
    INIT_NOTIFICATION,
    TPyEnvironmentTaskModel.Create(
      LTask.Id, LTask, INIT_NOTIFICATION, false));
end;

procedure TPyCustomEnvironment.Setup(APythonVersion: string);
begin
  InternalSetup(APythonVersion);
end;

function TPyCustomEnvironment.SetupAsync(APythonVersion: string): ITask;
begin
  Result := TTask.Run(procedure() begin
    InternalSetup(APythonVersion);
  end);
end;

function TPyCustomEnvironment.Activate(APythonVersion: string): boolean;
begin
  Result := InternalActivate(APythonVersion);
end;

procedure TPyCustomEnvironment.Deactivate;
begin
  InternalDeactivate();
end;

procedure TPyCustomEnvironment.DoInternalReady;
begin
  InternalNotifyAll(INTERNAL_READY_NOTIFICATION, nil);
  InternalRunTasks(INTERNAL_READY_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoAfterDeactivate;
begin
  if Assigned(FAfterDeactivate) then
    FAfterDeactivate(Self, FPythonEngine.RegVersion);

  InternalNotifyAll(AFTER_DEACTIVATE_NOTIFICATION, nil);
  InternalRunTasks(AFTER_DEACTIVATE_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoBeforeDeactivate;
begin
  if Assigned(FBeforeDeactivate) then
    FBeforeDeactivate(Self, FPythonEngine.RegVersion);

  InternalNotifyAll(BEFORE_DEACTIVATE_NOTIFICATION, nil);
  InternalRunTasks(BEFORE_DEACTIVATE_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoAfterActivate(APythonVersion: string; const ADistribution: TPyDistribution; var Result: Boolean);
begin
  if Assigned(FAfterActivate) then
    FAfterActivate(Self, APythonVersion, Result);

  InternalNotifyAll(AFTER_ACTIVATE_NOTIFICATION, ADistribution);
  InternalRunTasks(AFTER_ACTIVATE_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoBeforeActivate(APythonVersion: string);
begin
  if Assigned(FBeforeDeactivate) then
    FBeforeDeactivate(Self, APythonVersion);

  InternalNotifyAll(BEFORE_ACTIVATE_NOTIFICATION, nil);
  InternalRunTasks(BEFORE_ACTIVATE_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoAfterSetup(APythonVersion: string; const ADistribution: TPyDistribution);
begin
  if Assigned(FAfterSetup) then
    FAfterSetup(Self, APythonVersion);

  InternalNotifyAll(AFTER_SETUP_NOTIFICATION, ADistribution);
  InternalRunTasks(AFTER_SETUP_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoBeforeSetup(APythonVersion: string);
begin
  if Assigned(FBeforeSetup) then
    FBeforeSetup(Self, APythonVersion);

  InternalNotifyAll(BEFORE_SETUP_NOTIFICATION, nil);
  InternalRunTasks(BEFORE_SETUP_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoFinalize;
begin
  InternalNotifyAll(FINALIZE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoInit;
begin
  InternalNotifyAll(INIT_NOTIFICATION, nil);
  InternalRunTasks(INIT_NOTIFICATION);
end;

procedure TPyCustomEnvironment.DoAutoLoad;
begin
  if FAsync then
    ScheduleSetupAndActivateTasks()
  else begin
    Setup(FPythonVersion);
    Activate(FPythonVersion);
  end;
end;

function TPyCustomEnvironment.InternalActivate(APythonVersion: string): boolean;
var
  LDistribution: TPyDistribution;
begin
  Result := false;

  DoBeforeActivate(APythonVersion);

  if not Assigned(FPythonEngine) then
    Exit();

  LDistribution := FDistributions.LocateEnvironment(APythonVersion);
  if not Assigned(LDistribution) then
    Exit();

  FPythonEngine.UseLastKnownVersion := false;
  FPythonEngine.PythonHome := TPyPathHandler.ResolvePath(LDistribution.Home);
  FPythonEngine.ProgramName := TPyPathHandler.ResolvePath(LDistribution.Executable);
  FPythonEngine.DllPath := TPath.GetDirectoryName(TPyPathHandler.ResolvePath(LDistribution.SharedLibrary));
  FPythonEngine.DllName := TPath.GetFileName(LDistribution.SharedLibrary);
  FPythonEngine.LoadDll();

  Result := FPythonEngine.IsHandleValid();

  DoAfterActivate(APythonVersion, LDistribution, Result);

  DoInternalReady;
end;

procedure TPyCustomEnvironment.InternalDeactivate;
begin
  DoBeforeDeactivate;

  if not Assigned(FPythonEngine) then
    Exit();

  FPythonEngine.UnloadDll();
  FPythonEngine.PythonHome := String.Empty;
  FPythonEngine.ProgramName := String.Empty;
  FPythonEngine.DllPath := String.Empty;
  FPythonEngine.DllName := String.Empty;

  DoAfterDeactivate;
end;

procedure TPyCustomEnvironment.InternalNotifyAll(ANotification: TEnvironmentNotification;
  ADistribution: TPyDistribution);
begin
  FEnvironmentNotifier.NotifyAll(ANotification, ADistribution);
end;

procedure TPyCustomEnvironment.InternalRunTasks(
  ANotification: TEnvironmentNotification);
begin
  if Assigned(FEnvironmentTaskRunner) then
    FEnvironmentTaskRunner.Run(ANotification);
end;

procedure TPyCustomEnvironment.InternalSetup(APythonVersion: string);
var
  LDistribution: TPyDistribution;
begin
  if (TThread.CurrentThread.ThreadID <> MainThreadID) then
    TThread.Synchronize(nil, procedure() begin
      DoBeforeSetup(APythonVersion);
    end)
  else begin
    DoBeforeSetup(APythonVersion);
  end;

  Prepare();

  LDistribution := FDistributions.LocateEnvironment(APythonVersion);
  if not Assigned(LDistribution) then
    Exit();

  LDistribution.Setup();

  if (TThread.CurrentThread.ThreadID <> MainThreadID) then
    TThread.Synchronize(nil, procedure() begin
      DoAfterSetup(APythonVersion, LDistribution);
    end)
  else begin
    DoAfterSetup(APythonVersion, LDistribution);
  end;
end;

procedure TPyCustomEnvironment.Prepare;
begin
  //
end;

procedure TPyCustomEnvironment.SetEnvironments(
  const ADistributions: TPyDistributionCollection);
begin
  FDistributions.Assign(ADistributions);
end;

procedure TPyCustomEnvironment.SetPythonEngine(const APythonEngine: TPythonEngine);
begin
  if (APythonEngine <> FPythonEngine) then begin
    if Assigned(FPythonEngine) then
      FPythonEngine.RemoveFreeNotification(Self);
    FPythonEngine := APythonEngine;
    if Assigned(FPythonEngine) then begin
      FPythonEngine.FreeNotification(Self);
      if (csDesigning in ComponentState) then
        FPythonEngine.AutoLoad := false;
    end;
  end;
end;

procedure TPyCustomEnvironment.SetPythonVersion(const Value: string);
begin
  FPythonVersion := Value;
end;

{ TPyEnvironment }

procedure TPyEnvironment.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

function TPyEnvironment.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TPyEnvironment.GetChildren(Proc: TGetChildProc; Root: TComponent);
//var
//  I: Integer;
begin
  inherited GetChildren(Proc, Root);
//  for I := 0 to ComponentCount - 1 do
//    Proc(Components[I]);
end;

initialization
  RegisterClass(TPyEnvironment);

finalization
  UnRegisterClass(TPyEnvironment);

end.
