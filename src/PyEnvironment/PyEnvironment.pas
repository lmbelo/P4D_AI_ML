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
  PyEnvironment.Distribution, PyEnvironment.AddOn, PyEnvironment.Notification, PythonEngine;

type
  TPyCustomEnvironment = class(TComponent, IEnvironmentNotified)
  private
    FDistributions: TPyDistributionCollection;
    FAutoLoad: boolean;
    FPythonEngine: TPythonEngine;
    FPythonVersion: string;
    FAddOns: TPyEnvironmentAddOns;
    FOnSendNotification: TOnSendNotification;
    FOnReceiveNotification: TOnReceiveNotification;
    procedure SetEnvironments(const ADistributions: TPyDistributionCollection);
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
    procedure SetAddOns(const Value: TPyEnvironmentAddOns);
  protected 
    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    function CreateCollection(): TPyDistributionCollection; virtual; abstract;
    procedure Prepare(); virtual;
    procedure NotifyAll(ANotification: TEnvironmentNotification;
      ADistribution: TPyDistribution); virtual;
    //IEnvironmentNotified implementation
    procedure NotifyUpdate(ANotifier: TObject; ANotification: TEnvironmentNotification;
      const AArgs: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Setup(APythonVersion: string);
    procedure Activate(APythonVersion: string);
    procedure Deactivate();
  public
    property Distributions: TPyDistributionCollection read FDistributions write SetEnvironments;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
    property AddOns: TPyEnvironmentAddOns read FAddOns write SetAddOns;
    //Broacast events
    property OnSendNotification: TOnSendNotification read FOnSendNotification write FOnSendNotification;
    property OnReceiveNotification: TOnReceiveNotification read FOnReceiveNotification write FOnReceiveNotification;
  end;

  TPyEnvironment = class(TPyCustomEnvironment)
  published
    property Distributions;
    property AutoLoad;
    property PythonVersion;
    property PythonEngine;
    property AddOns;
    property OnSendNotification;
    property OnReceiveNotification;
  end;

implementation

{ TPyCustomEnvironment }

constructor TPyCustomEnvironment.Create(AOwner: TComponent);
begin
  FDistributions := CreateCollection();
  inherited;
  TEnvironmentBroadcaster.Instance.AddListener(Self);
end;

destructor TPyCustomEnvironment.Destroy;
begin
  TEnvironmentBroadcaster.Instance.RemoveListener(Self);
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
  end else if (AOperation = opRemove) and (AComponent = FAddOns) then begin
    SetAddOns(nil);
  end;
end;

procedure TPyCustomEnvironment.Setup(APythonVersion: string);
var
  LDistribution: TPyDistribution;
begin
  NotifyAll(BEFORE_SETUP_NOTIFICATION, nil);

  Prepare();

  LDistribution := FDistributions.LocateEnvironment(APythonVersion);
  if not Assigned(LDistribution) then
    Exit();

  LDistribution.Setup();

  NotifyAll(AFTER_SETUP_NOTIFICATION, LDistribution);
end;

procedure TPyCustomEnvironment.Activate(APythonVersion: string);
var
  LDistribution: TPyDistribution;
begin
  if not Assigned(FPythonEngine) then
    Exit();

  LDistribution := FDistributions.LocateEnvironment(APythonVersion);
  if not Assigned(LDistribution) then
    Exit();

  NotifyAll(BEFORE_ACTIVATE_NOTIFICATION, LDistribution);

  FPythonEngine.UseLastKnownVersion := false;
  FPythonEngine.PythonHome := ExpandFileName(LDistribution.Home);
  FPythonEngine.ProgramName := ExpandFileName(LDistribution.Executable);
  FPythonEngine.DllPath := ExpandFileName(ExtractFilePath(LDistribution.SharedLibrary));
  FPythonEngine.DllName := ExtractFileName(LDistribution.SharedLibrary);
  FPythonEngine.LoadDll();

  NotifyAll(AFTER_ACTIVATE_NOTIFICATION, LDistribution);
end;

procedure TPyCustomEnvironment.Deactivate;
begin
  NotifyAll(BEFORE_DEACTIVATE_NOTIFICATION, nil);

  if not Assigned(FPythonEngine) then
    Exit();

  FPythonEngine.UnloadDll();
  FPythonEngine.PythonHome := String.Empty;
  FPythonEngine.ProgramName := String.Empty;
  FPythonEngine.DllPath := String.Empty;
  FPythonEngine.DllName := String.Empty;

  NotifyAll(AFTER_DEACTIVATE_NOTIFICATION, nil);
end;

procedure TPyCustomEnvironment.NotifyUpdate(ANotifier: TObject;
  ANotification: TEnvironmentNotification;
  const AArgs: TObject);
begin
  if (ANotifier <> Self) and Assigned(FOnReceiveNotification) then
    FOnReceiveNotification(ANotifier, ANotification, AArgs);
end;

procedure TPyCustomEnvironment.NotifyAll(ANotification: TEnvironmentNotification;
  ADistribution: TPyDistribution);
var
  LBroadcast: Boolean;
begin
  LBroadcast := true;

  if Assigned(FOnSendNotification) then
    FOnSendNotification(ANotification, LBroadcast, ADistribution);

  if Assigned(FAddOns) then
    FAddOns.Apply(Self, ANotification, ADistribution);

  if LBroadcast then
    TEnvironmentBroadcaster.Instance.NotifyAll(Self, ANotification, ADistribution);
end;

procedure TPyCustomEnvironment.Prepare;
begin
  //
end;

procedure TPyCustomEnvironment.SetAddOns(const Value: TPyEnvironmentAddOns);
begin
  if (Value <> FAddOns) then begin
    if Assigned(FAddOns) then begin
      FAddOns.RemoveFreeNotification(Self);
    end;
    FAddOns := Value;
    if Assigned(FAddOns) then begin
      FAddOns.FreeNotification(Self);
    end;
  end;
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

end.
