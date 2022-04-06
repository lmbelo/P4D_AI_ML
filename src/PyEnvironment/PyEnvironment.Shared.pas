(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyShared'         Copyright (c) 2021                    *)
(*                                                                        *)
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
unit PyEnvironment.Shared;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  PyEnvironment,
  PyEnvironment.Intf;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                         JSON structure example                        *)
  (*                                                                       *)
  (*                                                                       *)
  (*                                                                       *)
  (*  JSON                                                                 *)
  (*    [{"platform": [                                                    *)
  (*      {"arch": [                                                       *)
  (*          {"python_version":                                           *)
  (*            {"home": "",                                               *)
  (*             "program_name": "",                                       *)
  (*             "shared_library": "",                                     *)
  (*             "executable": ""}}]}]}]                                   *)
  (*-----------------------------------------------------------------------*)
  [ComponentPlatforms(pidAllPlatforms)]
  /// <summary>
  ///   Provide access a shared Python environment based on a JSON file.
  ///   Use "any" for global configuration.
  /// </summary>
  TPyEnvironmentShared = class(TPyEnvironment)
  private
    FFilePath: string;
    FHome: string;
    FProgramName: string;
    FSharedLibrary: string;
    FExecutable: string;
  private
    procedure LoadInfo(const AProc: TProc<TJSONObject>);
  protected
    /// <summary>
    ///   Having the FilePath empty, the app path is used as root.
    /// </summary>
    function ResolveFilePath(): string;
  protected
    function GetEnvironmentPath(): string; override;
    procedure InternalSetup(); override;
  public
    function GetHome(): string; override;
    function GetProgramName(): string; override;
    function GetSharedLibrary(): string; override;
    function GetExecutable(): string; override;
  published
    property Platform;
    property Architecture;
    property PythonVersion;
    /// <summary>
    ///   Specifies the JSON file path. Let it empty for default settings.
    /// </summary>
    property FilePath: string read FFilePath write FFilePath;
  end;

  EInvalidFileStructure = class(Exception);

const
  JSON_FILE_NAME = 'pyenv.json';

implementation

uses
  System.IOUtils;

{ TPyUserFile }

procedure TPyEnvironmentShared.LoadInfo(const AProc: TProc<TJSONObject>);
var
  LFile: string;
  LPlatforms: TJSONValue;
  LPlatform: TJSONValue;
  LArchitectures: TJSONValue;
  LArchitecture: TJSONValue;
  LPythonVersions: TJSONValue;
  LPythonVersion: TJSONValue;
  LPythonInfo: TJSONValue;

  procedure CheckIsArray(const AValue: TJSONValue);
  begin
    if not (AValue is TJSONArray) then
      raise EInvalidFileStructure.Create('Invalid file structure.');
  end;

begin
  LFile := ResolveFilePath();

  if not TFile.Exists(LFile) then
    raise EFileNotFoundException.CreateFmt('File not found.' + #13#10 + '%s', [LFile]);

  LPlatforms := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFile));
  try
    CheckIsArray(LPlatforms);

    for LPlatform in TJSONArray(LPlatforms) do begin
      LArchitectures := LPlatform.FindValue(GetPlatformName(true));
      if not Assigned(LArchitectures) then
        LArchitectures := LPlatform.FindValue(GetPlatformName(false));

      CheckIsArray(LArchitectures);

      for LArchitecture in TJSONArray(LArchitectures) do begin
        LPythonVersions := LArchitecture.FindValue(GetArchitectureName(true));
        if not Assigned(LPythonVersions) then
          LPythonVersions := LArchitecture.FindValue(GetArchitectureName(false));

        CheckIsArray(LPythonVersions);

        for LPythonVersion in TJSONArray(LPythonVersions) do begin
          LPythonInfo := TJSONObject(LPythonVersion).Values[PythonVersion];

          if not Assigned(LPythonInfo) then
            Continue;

          if not (LPythonInfo is TJSONObject) then
            raise EInvalidFileStructure.Create('Invalid file structure.');

          AProc(TJSONObject(LPythonInfo));
          Exit;
        end;
      end;
    end;
  finally
    LPlatforms.Free();
  end;
end;

function TPyEnvironmentShared.ResolveFilePath: string;
begin
  if not FilePath.IsEmpty() then
    Result := FilePath
  else
    Result := TPath.Combine(ExtractFilePath(ParamStr(0)), JSON_FILE_NAME);
end;

function TPyEnvironmentShared.GetEnvironmentPath: string;
begin
  Result := GetHome();
end;

procedure TPyEnvironmentShared.InternalSetup;
begin
  inherited;
  LoadInfo(procedure(AJSONObj: TJSONObject)
    begin
      AJSONObj.TryGetValue<string>('home', FHome);
      AJSONObj.TryGetValue<string>('program_name', FProgramName);
      AJSONObj.TryGetValue<string>('shared_library', FSharedLibrary);
      AJSONObj.TryGetValue<string>('executable', FExecutable);
    end);
end;

function TPyEnvironmentShared.GetHome: string;
begin
  Result := FHome;
end;

function TPyEnvironmentShared.GetProgramName: string;
begin
  Result := FProgramName;
end;

function TPyEnvironmentShared.GetSharedLibrary: string;
begin
  Result := FSharedLibrary;
end;

function TPyEnvironmentShared.GetExecutable: string;
begin
  Result := FExecutable;
end;

end.
