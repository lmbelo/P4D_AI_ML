(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Collection'                               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                https://github.com/lmbelo/P4D_AI_ML      *)
(**************************************************************************)
(*  Functionality:  PyEnvironment layer                                   *)
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
unit PyEnvironment.Collection;

interface

uses
  System.Classes, System.Generics.Collections,
  PythonEngine,
  PyEnvironment,
  PyEnvironment.Intf;

type
  TPyEnvironmentCollection = class
  private
    class var FInstance: TPyEnvironmentCollection;
  private
    FDictionary: TDictionary<string, IEnvironmentSettings>;
    function FormatKey(const APlatform: TPyEnvironment.TPlatform;
      const AArchitecture: TPyEnvironment.TArchitecture;
      APythonVersion: string): string;
  private
    class constructor Create();
    class destructor Destroy();
  public
    constructor Create();
    destructor Destroy(); override;

    {***** Registration methods *****}
    procedure RegisterEnvironment(const APlatform: TPyEnvironment.TPlatform;
      const AArchitecture: TPyEnvironment.TArchitecture;
      APythonVersion: string; ASettings: IEnvironmentSettings);
    procedure UnregisterEnvironment(const APlatform: TPyEnvironment.TPlatform;
      const AArchitecture: TPyEnvironment.TArchitecture;
      APythonVersion: string; ASettings: IEnvironmentSettings);

    function UseEnvironment(const APlatform: TPyEnvironment.TPlatform;
      const AArchitecture: TPyEnvironment.TArchitecture;
      APythonVersion: string; APythonEngine: TPythonEngine): boolean; overload;
    function UseEnvironment(APythonVersion: string; APythonEngine: TPythonEngine): boolean; overload;

    class procedure Patch(APythonEngine: TPythonEngine; ASettings: IEnvironmentSettings);

    class property Instance: TPyEnvironmentCollection read FInstance;
  end;

implementation

uses
  System.SysUtils;

{ TPyManager }

class constructor TPyEnvironmentCollection.Create;
begin
  FInstance := TPyEnvironmentCollection.Create();
end;

class destructor TPyEnvironmentCollection.Destroy;
begin
  FInstance.Free();
end;

constructor TPyEnvironmentCollection.Create;
begin
  inherited;
  FDictionary := TDictionary<string, IEnvironmentSettings>.Create();
end;

destructor TPyEnvironmentCollection.Destroy;
begin
  FDictionary.Free();
  inherited;
end;

procedure TPyEnvironmentCollection.RegisterEnvironment(
  const APlatform: TPyEnvironment.TPlatform;
  const AArchitecture: TPyEnvironment.TArchitecture; APythonVersion: string;
  ASettings: IEnvironmentSettings);
begin
  FDictionary.Add(FormatKey(APlatform, AArchitecture, APythonVersion), ASettings);
end;

procedure TPyEnvironmentCollection.UnregisterEnvironment(
  const APlatform: TPyEnvironment.TPlatform;
  const AArchitecture: TPyEnvironment.TArchitecture;
  APythonVersion: string; ASettings: IEnvironmentSettings);
var
  LKey: string;
  LSettings: IEnvironmentSettings;
begin
  LKey := FormatKey(APlatform, AArchitecture, APythonVersion);
  if FDictionary.TryGetValue(LKey, LSettings) and (LSettings = ASettings) then
    FDictionary.Remove(LKey);
end;

function TPyEnvironmentCollection.FormatKey(const APlatform: TPyEnvironment.TPlatform;
  const AArchitecture: TPyEnvironment.TArchitecture;
  APythonVersion: string): string;
begin
  Result := Format('%s.%s.%s', [APlatform.ToString(), AArchitecture.ToString(), APythonVersion]);
end;

class procedure TPyEnvironmentCollection.Patch(APythonEngine: TPythonEngine;
  ASettings: IEnvironmentSettings);
var
  LSharedLibrary: string;
begin
  APythonEngine.UseLastKnownVersion := false;
  APythonEngine.PythonHome := ASettings.GetHome();
  APythonEngine.ProgramName := ASettings.GetProgramName();
  LSharedLibrary := ASettings.GetSharedLibrary();
  APythonEngine.DllPath := ExtractFilePath(LSharedLibrary);
  APythonEngine.DllName := ExtractFileName(LSharedLibrary);
  APythonEngine.InitScript.Add('import sys');
  APythonEngine.InitScript.Add(Format('sys.executable = r"%s"', [ASettings.GetExecutable()]));
end;

function TPyEnvironmentCollection.UseEnvironment(APythonVersion: string;
  APythonEngine: TPythonEngine): boolean;
begin
  if UseEnvironment(
    //Current platform
    TPyEnvironment.TPlatform.FromSysPlatform(TOSVersion.Platform),
    //Current architecture
    TPyEnvironment.TArchitecture.FromSysArchitecture(TOSVersion.Architecture),
    APythonVersion, APythonEngine)
  then
    Result := true
  else
    Result := UseEnvironment(
      TPyEnvironment.TPlatform.pfAny,
      TPyEnvironment.TArchitecture.arAny,
      APythonVersion, APythonEngine)
end;

function TPyEnvironmentCollection.UseEnvironment(const APlatform: TPyEnvironment.TPlatform;
  const AArchitecture: TPyEnvironment.TArchitecture; APythonVersion: string;
  APythonEngine: TPythonEngine): boolean;
var
  LSettings: IEnvironmentSettings;
begin
  Result := FDictionary.TryGetValue(FormatKey(APlatform, AArchitecture, APythonVersion), LSettings);
  if Result then
    TPyEnvironmentCollection.Patch(APythonEngine, LSettings);
end;

end.
