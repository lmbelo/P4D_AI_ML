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
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  PythonEngine,
  PyEnvironment.Distribution,
  PyEnvironment.Notification;

type
  TPyCustomEnvironment = class(TComponent, IEnvironmentNotifier<TPyCustomEnvironment>)
  private
    FDistributions: TPyDistributionCollection;
    FAutoLoad: boolean;
    FPythonEngine: TPythonEngine;
    FPythonVersion: string;
    FEnvironmentNotifier: IEnvironmentNotifier<TPyCustomEnvironment>;
    procedure SetEnvironments(const ADistributions: TPyDistributionCollection);
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
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
    function Activate(APythonVersion: string): boolean;
    procedure Deactivate();
  public
    property Distributions: TPyDistributionCollection read FDistributions write SetEnvironments;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonVersion: string read FPythonVersion write SetPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
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
  begin
    Setup(PythonVersion);
    Activate(PythonVersion);
  end;
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
var
  LDistribution: TPyDistribution;
begin
  InternalNotifyAll(BEFORE_SETUP_NOTIFICATION, nil);

  Prepare();

  LDistribution := FDistributions.LocateEnvironment(APythonVersion);
  if not Assigned(LDistribution) then
    Exit();

  LDistribution.Setup();

  InternalNotifyAll(AFTER_SETUP_NOTIFICATION, LDistribution);
end;

function TPyCustomEnvironment.Activate(APythonVersion: string): boolean;
var
  LDistribution: TPyDistribution;
begin
  Result := false;

  if not Assigned(FPythonEngine) then
    Exit();

  LDistribution := FDistributions.LocateEnvironment(APythonVersion);
  if not Assigned(LDistribution) then
    Exit();

  InternalNotifyAll(BEFORE_ACTIVATE_NOTIFICATION, LDistribution);

  FPythonEngine.UseLastKnownVersion := false;
  FPythonEngine.PythonHome := ExpandFileName(LDistribution.Home);
  FPythonEngine.ProgramName := ExpandFileName(LDistribution.Executable);
  FPythonEngine.DllPath := ExpandFileName(ExtractFilePath(LDistribution.SharedLibrary));
  FPythonEngine.DllName := ExtractFileName(LDistribution.SharedLibrary);
  FPythonEngine.LoadDll();

  InternalNotifyAll(AFTER_ACTIVATE_NOTIFICATION, LDistribution);

  Result := FPythonEngine.IsHandleValid();
end;

procedure TPyCustomEnvironment.Deactivate;
begin
  InternalNotifyAll(BEFORE_DEACTIVATE_NOTIFICATION, nil);

  if not Assigned(FPythonEngine) then
    Exit();

  FPythonEngine.UnloadDll();
  FPythonEngine.PythonHome := String.Empty;
  FPythonEngine.ProgramName := String.Empty;
  FPythonEngine.DllPath := String.Empty;
  FPythonEngine.DllName := String.Empty;

  InternalNotifyAll(AFTER_DEACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.InternalNotifyAll(ANotification: TEnvironmentNotification;
  ADistribution: TPyDistribution);
begin
  FEnvironmentNotifier.NotifyAll(ANotification, ADistribution);
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
