(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEmbedded'       Copyright (c) 2021                    *)
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
unit PyEnvironment.Embedded;

interface

uses
  System.Classes, System.SysUtils, System.Zip,
  PyEnvironment,
  PyEnvironment.Intf;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                      Embeddables structure example                    *)
  (*                                                                       *)
  (*                                                                       *)
  (*                                                                       *)
  (* EmbeddablesPath                                                       *)
  (*  +-- python/                                                          *)
  (*       +-- environments/                                               *)
  (*            +-- os/                                                    *)
  (*                 +-- arch/                                             *)
  (*                      +-- python version/                              *)
  (*                           +-- python zip                              *)
  (*-----------------------------------------------------------------------*)
  [ComponentPlatforms(pidAllPlatforms)]
  /// <summary>
  ///   Provide access to a Python environment based on embeddables.
  /// </summary>
  TPyEnvironmentEmbedded = class(TPyEnvironment)
  private
    FEmbeddablesPath: string;
    FEnvironmentsPath: string;
    //Events
    FOnUnzipProgress: TZipProgressEvent;
    FBeforeCreate: TNotifyEvent;
    FAfterCreate: TNotifyEvent;
  protected
    /// <summary>
    ///   Having the EmbeddablesPath empty, the app path is used as root.
    /// </summary>
    function ResolveEmbeddablesPath(): string;
    /// <summary>
    ///   Having the EnvironmentsPath empty, the app path is used as root.
    /// </summary>
    function ResolveEnvironmentsPath(): string;
    /// <summary>
    ///   Return the embeddable path based on the current settings.
    /// </summary>
    function GetEmbeddablePath(): string;
    /// <summary>
    ///   Return the first zip file contained in the embeddables path.
    /// </summary>
    function GetEmbeddablePackage(): string;
    /// <summary>
    ///   Navigates through the embeddables searching for a compatible distribution.
    /// </summary>
    function EmbeddableExists(): boolean;
    /// <summary>
    ///   Creates a new environment based on the current settings.
    ///   An embeddable distribution will be used as an "image".
    /// </summary>
    procedure CreateEnvironment(); virtual;
  protected
    function GetEnvironmentPath(): string; override;
  public
    procedure InternalSetup(); override;
  published
    property Architecture;
    property Platform;
    property PythonVersion;

    //Environment settings

    /// <summary>
    ///   Specifies the Pyton embeddables folder.
    /// </summary>
    property EmbeddablesPath: string read FEmbeddablesPath write FEmbeddablesPath;
    /// <summary>
    ///   Specifies the Python environment folder.
    /// </summary>
    property EnvironmentsPath: string read FEnvironmentsPath write FEnvironmentsPath;

    //Events
    property BeforeCreate: TNotifyEvent read FBeforeCreate write FBeforeCreate;
    property OnUnzipProgress: TZipProgressEvent read FOnUnzipProgress write FOnUnzipProgress;
    property AfterCreate: TNotifyEvent read FAfterCreate write FAfterCreate;
  end;

  EEmbeddableNotFound = class(Exception);

const
  PYTHON_ROOT_DIR_NAME = 'python';
  PYTHON_EMBEDDABLES_DIR_NAME = 'embeddables';
  PYTHON_ENVIRONMENT_DIR_NAME = 'environments';

implementation

uses
  System.IOUtils;

{ TPyEmbedded }

function TPyEnvironmentEmbedded.GetEmbeddablePackage: string;
var
  LFiles: TArray<string>;
begin
  LFiles := TDirectory.GetFiles(GetEmbeddablePath(), '*.zip', TSearchOption.soTopDirectoryOnly);
  if Length(LFiles) > 0 then
    Result := LFiles[0]
  else
    Result := String.Empty;
end;

function TPyEnvironmentEmbedded.GetEmbeddablePath: string;
begin
  Result := TPath.Combine(
    TPath.Combine(ResolveEmbeddablesPath(), PYTHON_ROOT_DIR_NAME),
      TPath.Combine(PYTHON_EMBEDDABLES_DIR_NAME,
        TPath.Combine(GetPlatformName(),
          TPath.Combine(GetArchitectureName(), PythonVersion))));
end;

function TPyEnvironmentEmbedded.ResolveEmbeddablesPath: string;
begin
  if not FEmbeddablesPath.IsEmpty() then
    Result := FEmbeddablesPath
  else
    Result := ExtractFilePath(ParamStr(0));
end;

function TPyEnvironmentEmbedded.ResolveEnvironmentsPath: string;
begin
  if not FEnvironmentsPath.IsEmpty() then
    Result := FEnvironmentsPath
  else
    Result := ExtractFilePath(ParamStr(0));
end;

procedure TPyEnvironmentEmbedded.InternalSetup;
begin
  if not Exists() then begin
    if not EmbeddableExists() then
      raise EEmbeddableNotFound.CreateFmt(
        'Embeddable package not found.' + #13#10 + '%s', [GetEmbeddablePath()]);
    CreateEnvironment();
  end;
end;

function TPyEnvironmentEmbedded.GetEnvironmentPath: string;
begin
  inherited;
  Result := TPath.Combine(
    TPath.Combine(ResolveEmbeddablesPath(), PYTHON_ROOT_DIR_NAME),
      TPath.Combine(PYTHON_ENVIRONMENT_DIR_NAME,
        TPath.Combine(GetPlatformName(),
          TPath.Combine(GetArchitectureName(), PythonVersion))));
end;

function TPyEnvironmentEmbedded.EmbeddableExists: boolean;
begin
  Result := TFile.Exists(GetEmbeddablePackage());
end;

procedure TPyEnvironmentEmbedded.CreateEnvironment;
begin
  if Assigned(FBeforeCreate) then
    FBeforeCreate(Self);

  //Unzip the embeddable package into the target directory.
  TZipFile.ExtractZipFile(GetEmbeddablePackage(), GetEnvironmentPath(), FOnUnzipProgress);

  if Assigned(FAfterCreate) then
    FAfterCreate(Self);
end;

end.
