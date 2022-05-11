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
  PyEnvironment.Notification;

type
  TPyEnvironmentBeforeSetup = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterSetup = procedure(Sender: TObject; const APythonVersion: string) of object;

  TPyEnvironmentBeforeActivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterActivate = procedure(Sender: TObject; const APythonVersion: string; const AActivated: boolean) of object;

  TPyEnvironmentBeforeDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;
  TPyEnvironmentAfterDeactivate = procedure(Sender: TObject; const APythonVersion: string) of object;

  TPyCustomEnvironment = class(TComponent, IEnvironmentNotifier<TPyCustomEnvironment>)
  private
    FDistributions: TPyDistributionCollection;
    FAutoLoad: boolean;
    FPythonEngine: TPythonEngine;
    FPythonVersion: string;
    FEnvironmentNotifier: IEnvironmentNotifier<TPyCustomEnvironment>;
    FAsync: boolean;
    FBeforeSetup: TPyEnvironmentBeforeSetup;
    FAfterSetup: TPyEnvironmentAfterSetup;
    FBeforeActivate: TPyEnvironmentBeforeActivate;
    FAfterActivate: TPyEnvironmentAfterActivate;
    FBeforeDeactivate: TPyEnvironmentBeforeDeactivate;
    FAfterDeactivate: TPyEnvironmentAfterDeactivate;
    procedure SetEnvironments(const ADistributions: TPyDistributionCollection);
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
    procedure DoAutoLoad;
    procedure InternalSetup(APythonVersion: string);
    //Exception handlers
    procedure HandleAutoLoadException(const AException: Exception;
      const AExceptionAddr: Pointer);
    //Events handlers
    procedure DoBeforeSetup(APythonVersion: string);
    procedure DoAfterSetup(APythonVersion: string; const ADistribution: TPyDistribution);
    procedure DoBeforeActivate(APythonVersion: string);
    procedure DoAfterActivate(APythonVersion: string; const ADistribution: TPyDistribution; var Result: Boolean);
    procedure DoBeforeDeactivate;
    procedure DoAfterDeactivate;
  protected
    property EnvironmentNotifier: IEnvironmentNotifier<TPyCustomEnvironment>
      read FEnvironmentNotifier
      implements IEnvironmentNotifier<TPyCustomEnvironment>;
  protected
    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    procedure SetPythonVersion(const Value: string); virtual;
    function CreateCollection(): TPyDistributionCollection; virtual; abstract;
    procedure Prepare(); virtual;
    procedure InternalNotifyAll(ANotification: TEnvironmentNotification;
      ADistribution: TPyDistribution); virtual;
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
  published
    //Events
    property BeforeSetup: TPyEnvironmentBeforeSetup read FBeforeSetup write FBeforeSetup;
    property AfterSetup: TPyEnvironmentAfterSetup read FAfterSetup write FAfterSetup;
    property BeforeActivate: TPyEnvironmentBeforeActivate read FBeforeActivate write FBeforeActivate;
    property AfterActivate: TPyEnvironmentAfterActivate read FAfterActivate write FAfterActivate;
    property BeforeDeactivate: TPyEnvironmentBeforeDeactivate read FBeforeDeactivate write FBeforeDeactivate;
    property AfterDeactivate: TPyEnvironmentAfterDeactivate read FAfterDeactivate write FAfterDeactivate;
  end;

  TPyEnvironment = class(TPyCustomEnvironment)
  published
    property AutoLoad;
    property PythonVersion;
    property PythonEngine;
  end;

implementation

{ TPyCustomEnvironment }

constructor TPyCustomEnvironment.Create(AOwner: TComponent);
begin
  FDistributions := CreateCollection();
  FEnvironmentNotifier := TEnvironmentBroadcaster<TPyCustomEnvironment>.Create(Self);
  inherited;
end;

destructor TPyCustomEnvironment.Destroy;
begin
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
end;

procedure TPyCustomEnvironment.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FPythonEngine) then begin
    FPythonEngine := nil;
  end;
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
  FPythonEngine.PythonHome := ExpandFileName(LDistribution.Home);
  FPythonEngine.ProgramName := ExpandFileName(LDistribution.Executable);
  FPythonEngine.DllPath := ExpandFileName(ExtractFilePath(LDistribution.SharedLibrary));
  FPythonEngine.DllName := ExtractFileName(LDistribution.SharedLibrary);
  FPythonEngine.LoadDll();

  Result := FPythonEngine.IsHandleValid();

  DoAfterActivate(APythonVersion, LDistribution, Result);
end;

procedure TPyCustomEnvironment.Deactivate;
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

procedure TPyCustomEnvironment.DoAfterDeactivate;
begin
  if Assigned(FAfterDeactivate) then
    FAfterDeactivate(Self, FPythonEngine.RegVersion);

  InternalNotifyAll(AFTER_DEACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoBeforeDeactivate;
begin
  if Assigned(FBeforeDeactivate) then
    FBeforeDeactivate(Self, FPythonEngine.RegVersion);

  InternalNotifyAll(BEFORE_DEACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoAfterActivate(APythonVersion: string; const ADistribution: TPyDistribution; var Result: Boolean);
begin
  if Assigned(FAfterActivate) then
    FAfterActivate(Self, APythonVersion, Result);

  InternalNotifyAll(AFTER_ACTIVATE_NOTIFICATION, ADistribution);
end;

procedure TPyCustomEnvironment.DoBeforeActivate(APythonVersion: string);
begin
  if Assigned(FBeforeDeactivate) then
    FBeforeDeactivate(Self, APythonVersion);

  InternalNotifyAll(BEFORE_ACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoAfterSetup(APythonVersion: string; const ADistribution: TPyDistribution);
begin
  if Assigned(FAfterSetup) then
    FAfterSetup(Self, APythonVersion);

  InternalNotifyAll(AFTER_SETUP_NOTIFICATION, ADistribution);
end;

procedure TPyCustomEnvironment.DoBeforeSetup(APythonVersion: string);
begin
  if Assigned(FBeforeSetup) then
    FBeforeSetup(Self, APythonVersion);

  InternalNotifyAll(BEFORE_SETUP_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.DoAutoLoad;
begin
  if FAsync then begin
    TTask.Run(procedure()
    var
      LTask: ITask;
    begin
      LTask := SetupAsync(FPythonVersion);
      try
        LTask.Wait();
        if (LTask.Status in [TTaskStatus.Completed]) then
          TThread.Queue(nil, procedure() begin
            Activate(FPythonVersion);
          end);
      except
        on E: Exception do
          HandleAutoLoadException(E, ExceptAddr);
      end;
    end);
  end else begin
    Setup(FPythonVersion);
    Activate(FPythonVersion);
  end;
end;

procedure TPyCustomEnvironment.HandleAutoLoadException(
  const AException: Exception; const AExceptionAddr: Pointer);
var
  LExcept: Exception;
begin
  if (TThread.Current.ThreadID <> MainThreadID) then
    TThread.Synchronize(nil, procedure() begin
      ShowException(AException, AExceptionAddr);
    end)
  else begin
    LExcept := AcquireExceptionObject() as Exception;
    raise LExcept;
  end;
end;

procedure TPyCustomEnvironment.InternalNotifyAll(ANotification: TEnvironmentNotification;
  ADistribution: TPyDistribution);
begin
  FEnvironmentNotifier.NotifyAll(ANotification, ADistribution);
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

end.
