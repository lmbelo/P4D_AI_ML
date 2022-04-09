(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironments'   Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironments layer                                  *)
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
unit PyEnvironments;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  PythonEngine, System.Generics.Collections;

type
  TEnvironmentNotification = byte;
  TPyEnvironmentItem = class;
  TPyEnvironmentCustomAddOn = class;
  TPyEnvironmentAddOns = class;
  TPyEnvironment = class;

  TPyEnvironmentAddOnExecute = procedure(ANotification: TEnvironmentNotification;
    AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem) of object;
  TPyEnvironmentAddOnExecuteError = procedure(AException: Exception;
    const AAddOn: TPyEnvironmentCustomAddOn; AEnvironment: TPyEnvironmentItem) of object;

  TPyEnvironmentCustomAddOn = class(TComponent)
  private
    FAddOns: TPyEnvironmentAddOns;
    FOnExecute: TPyEnvironmentAddOnExecute;
    procedure SetAddOns(const Value: TPyEnvironmentAddOns);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure Execute(ANotification: TEnvironmentNotification;
      AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem); virtual;
  published
    property AddOns: TPyEnvironmentAddOns read FAddOns write SetAddOns;
    property OnExecute: TPyEnvironmentAddOnExecute read FOnExecute write FOnExecute;
  end;

  TPyEnvironmentAddOnUser = class(TPyEnvironmentCustomAddOn);

  TPyEnvironmentAddOns = class(TComponent)
  private
    FList: TList<TPyEnvironmentCustomAddOn>;
    FOnExecuteError: TPyEnvironmentAddOnExecuteError;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Add(AAddOn: TPyEnvironmentCustomAddOn);
    procedure Remove(AAddOn: TPyEnvironmentCustomAddOn);

    procedure Execute(ANotification: TEnvironmentNotification;
      AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem);
  published
    property OnExecuteError: TPyEnvironmentAddOnExecuteError
      read FOnExecuteError write FOnExecuteError;
  end;

  TPyEnvironmentItem = class abstract(TCollectionItem)
  private
    FPythonVersion: string;
    FHome: string;
    FProgramName: string;
    FSharedLibrary: string;
    FExecutable: string;
  public
    procedure Setup(); virtual; abstract;
  published
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property Home: string read FHome write FHome;
    property ProgramName: string read FProgramName write FProgramName;
    property SharedLibrary: string read FSharedLibrary write FSharedLibrary;
    property Executable: string read FExecutable write FExecutable;
  end;

  TPyEnvironmentCollection = class abstract(TOwnedCollection)
  public
    function LocateEnvironment(APythonVersion: string): TPyEnvironmentItem; virtual;
  end;

  TPyEnvironment = class(TComponent)
  private
    FEnvironments: TPyEnvironmentCollection;
    FAutoLoad: boolean;
    FPythonEngine: TPythonEngine;
    FPythonVersion: string;
    FAddOns: TPyEnvironmentAddOns;
    procedure SetEnvironments(const Value: TPyEnvironmentCollection);
    procedure SetPythonEngine(const Value: TPythonEngine);
  protected 
    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    function CreateCollection(): TPyEnvironmentCollection; virtual; abstract;
    procedure Prepare(); virtual;
    procedure NotifyAction(AAction: TEnvironmentNotification;
      AEnvironment: TPyEnvironmentItem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    
    procedure Activate(APythonVersion: string);
  published
    property Environments: TPyEnvironmentCollection read FEnvironments write SetEnvironments;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
    property AddOns: TPyEnvironmentAddOns read FAddOns write FAddOns;
  end;

  EEmbeddableNotFound = class(Exception);
  EInvalidFileStructure = class(Exception);

const
  BEFORE_SETUP_NOTIFICATION = $0;
  AFTER_SETUP_NOTIFICATION = $1;

implementation

uses
  System.Character, System.IOUtils, System.Zip;

{ TPyEnvironmentCollection }

function TPyEnvironmentCollection.LocateEnvironment(
  APythonVersion: string): TPyEnvironmentItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    if (TPyEnvironmentItem(Items[I]).PythonVersion = APythonVersion) then
      Exit(TPyEnvironmentItem(Items[I]));
  end;
  Result := nil;
end;

{ TPyEnvironment }

procedure TPyEnvironment.Activate(APythonVersion: string);
var
  LItem: TPyEnvironmentItem;
begin
  if not Assigned(FPythonEngine) then
    Exit();

  LItem := FEnvironments.LocateEnvironment(APythonVersion);
  if not Assigned(LItem) then
    Exit();

  NotifyAction(BEFORE_SETUP_NOTIFICATION, LItem);

  LItem.Setup();

  FPythonEngine.UnloadDll();
  FPythonEngine.UseLastKnownVersion := false;
  FPythonEngine.PythonHome := LItem.Home;
  FPythonEngine.ProgramName := LItem.ProgramName;
  FPythonEngine.DllPath := ExtractFilePath(LItem.SharedLibrary);
  FPythonEngine.DllName := ExtractFileName(LItem.SharedLibrary);
  FPythonEngine.LoadDll();

  FPythonEngine.ExecString('import sys');
  FPythonEngine.ExecString(AnsiString(Format('sys.executable = r"%s"', [LItem.Executable])));

  NotifyAction(AFTER_SETUP_NOTIFICATION, LItem);
end;

constructor TPyEnvironment.Create(AOwner: TComponent);
begin
  FEnvironments := CreateCollection();
  inherited;  
end;

destructor TPyEnvironment.Destroy;
begin
  FEnvironments.Free();
  inherited;
end;

procedure TPyEnvironment.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then  
    Prepare();
end;

procedure TPyEnvironment.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FPythonEngine) then begin
    FPythonEngine := nil;
  end;
end;

procedure TPyEnvironment.NotifyAction(AAction: TEnvironmentNotification;
  AEnvironment: TPyEnvironmentItem);
begin
  if not Assigned(FAddOns) then
    Exit();

  FAddOns.Execute(AAction, Self, AEnvironment);
end;

procedure TPyEnvironment.Prepare;
begin
  if FAutoLoad and Assigned(FPythonEngine) then
    Activate(PythonVersion);
end;

procedure TPyEnvironment.SetEnvironments(
  const Value: TPyEnvironmentCollection);
begin
  FEnvironments.Assign(Value);
end;

procedure TPyEnvironment.SetPythonEngine(const Value: TPythonEngine);
begin
  if (Value <> FPythonEngine) then begin
    if Assigned(FPythonEngine) then
      FPythonEngine.RemoveFreeNotification(Self);
    FPythonEngine := Value;
    if Assigned(FPythonEngine) then begin
      FPythonEngine.FreeNotification(Self);
      FPythonEngine.AutoLoad := false;
    end;
  end;
end;

{ TPyEnvironmentCustomAddOn }

procedure TPyEnvironmentCustomAddOn.Execute(ANotification: TEnvironmentNotification;
  AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem);
begin
  if Assigned(FOnExecute) then
    FOnExecute(ANotification, AManager, AEnvironment);
end;

procedure TPyEnvironmentCustomAddOn.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FAddOns) then begin
    SetAddOns(nil);
  end;
end;

procedure TPyEnvironmentCustomAddOn.SetAddOns(
  const Value: TPyEnvironmentAddOns);
begin
  if Assigned(FAddOns) then begin
    FAddOns.RemoveFreeNotification(Self);
    FAddOns.Remove(Self);
  end;

  FAddOns := Value;
  if Assigned(FAddOns) then begin
    FAddOns.FreeNotification(Self);
    FAddOns.Add(Self);
  end;
end;

{ TPyEnvironmentAddOns }

constructor TPyEnvironmentAddOns.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList<TPyEnvironmentCustomAddOn>.Create();
end;

destructor TPyEnvironmentAddOns.Destroy;
begin
  FList.Free();
  inherited;
end;

procedure TPyEnvironmentAddOns.Add(AAddOn: TPyEnvironmentCustomAddOn);
begin
  if not FList.Contains(AAddOn) then
    FList.Add(AAddOn);
end;

procedure TPyEnvironmentAddOns.Remove(AAddOn: TPyEnvironmentCustomAddOn);
begin
  if FList.Contains(AAddOn) then
    FList.Remove(AAddOn);
end;

procedure TPyEnvironmentAddOns.Execute(ANotification: TEnvironmentNotification;
  AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem);
var
  LItem: TPyEnvironmentCustomAddOn;
begin
  for LItem in FList do begin
    try
      LItem.Execute(ANotification, AManager, AEnvironment);
    except
      on E: Exception do
        if Assigned(FOnExecuteError) then begin
          FOnExecuteError(E, LItem, AEnvironment);
        end else
          raise;
    end;
  end;
end;

end.
