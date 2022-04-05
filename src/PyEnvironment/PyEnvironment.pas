(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment'    Copyright (c) 2021                    *)
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
unit PyEnvironment;

interface

uses
  System.Classes, System.SysUtils,
  PythonEngine,
  PyEnvironment.Intf;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                      Environment structure example                    *)
  (*                                                                       *)
  (*                                                                       *)
  (*                                                                       *)
  (* EnvironmentsPath                                                      *)
  (*  +-- python/                                                          *)
  (*       +-- environments/                                               *)
  (*            +-- os/                                                    *)
  (*                 +-- arch/                                             *)
  (*                      +-- python version/                              *)
  (*                           +-- python directory                        *)
  (*-----------------------------------------------------------------------*)
  TPyEnvironment = class abstract(TComponent, IEnvironmentSettings)
  public type
    TArchitecture = (arAny, arIntelX86, arIntelX64, arARM32, arARM64);
    TPlatform = (pfAny, pfWindows, pfMacOS, pfLinux);
  private
    FArchitecture: TArchitecture;
    FPlatform: TPlatform;    
    FPythonVersion: string;
  protected
    procedure Loaded; override;
    /// <summary>
    ///   Having the architecture set to "any", we consider the current architecture.
    /// </summary>
    function ResolveArchitecture(): TArchitecture;
    /// <summary>
    ///   Having the platform set to "any", we consider the current platform.
    /// </summary>
    function ResolvePlatform(): TPlatform;
    /// <summary>
    ///   Return the platform folder name.
    /// </summary>
    ///  /// <param name="AResolve">
    ///   Resolves 'any' to the current platform.
    /// </param>
    function GetPlatformName(const AResolve: boolean = true): string;
    /// <summary>
    ///   Return the architecure folder name.
    /// </summary>
    /// <param name="AResolve">
    ///   Resolves 'any' to the current architecture.
    /// </param>
    function GetArchitectureName(const AResolve: boolean = true): string;
    /// <summary>
    ///   Return the Python environment based on the current settings.
    /// </summary>
    function GetEnvironmentPath(): string; virtual; abstract;
    /// <summary>
    ///   Internally setup the environment.
    /// </summary>
    procedure InternalSetup(); virtual;
  public
    {***** IEnvironmentPaths implementation *****}

    //Python environment paths
    function GetHome(): string; virtual;
    function GetProgramName(): string; virtual;

    //Python file paths
    function GetSharedLibrary(): string; virtual;
    function GetExecutable(): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    /// <summary>
    ///   Check if the current platform and architecture is compatible with the evnrionment definitions.
    /// </summary>
    function IsSupported(): boolean;
    /// <summary>
    ///   Check if a compatible environment exists.
    /// </summary>
    function Exists(): boolean; virtual;
    /// <summary>
    ///   Setup the environment.
    /// </summary>
    procedure Setup();
    /// <summary>
    ///   Apply the current settings in a PythonEngine instance.
    /// </summary>
    procedure Patch(const APythonEngine: TPythonEngine);
  public
    //Platform and achitecture

    /// <summary>
    ///   Specific architecture configuration. Use "any" for global settings.
    /// </summary>
    property Architecture: TArchitecture read FArchitecture write FArchitecture;
    /// <summary>
    ///   Specific platform configuration. Use "any" for global settings.
    /// </summary>
    property Platform: TPlatform read FPlatform write FPlatform;    

    //Python settings

    /// <summary>
    ///   Specifies the Python version.
    /// </summary>
    property PythonVersion: string read FPythonVersion write FPythonVersion;
  end;

  TPyPlatformHelper = record helper for TPyEnvironment.TPlatform
  public
    function AsSysPlatform(): TOSVersion.TPlatform;
    function ToString(): string;

    class function FromSysPlatform(
      const APlatform: TOSVersion.TPlatform): TPyEnvironment.TPlatform; static;
  end;

  TPyArchitectureHelper = record helper for TPyEnvironment.TArchitecture
  public
    function AsSysArchitecture(): TOSVersion.TArchitecture;
    function ToString(): string;

    class function FromSysArchitecture(
      const AArchitecture: TOSVersion.TArchitecture): TPyEnvironment.TArchitecture; static;
  end;

  EEnvironmentNotAvailable = class(Exception);

  EEmbeddableNotAvailable = class(Exception);

  EUnsupportedArchitecture = class(Exception);
  
  EUnsupportedPlatform = class(Exception);

implementation

uses
  System.IOUtils, System.Character,
  PyEnvironment.Manager;

{ TPyEnvironment }

function TPyEnvironment.ResolveArchitecture: TArchitecture;
begin
  if (FArchitecture = TArchitecture.arAny) then
    Result := TPyEnvironment.TArchitecture.FromSysArchitecture(TOSVersion.Architecture)
  else
    Result := FArchitecture;
end;

function TPyEnvironment.ResolvePlatform: TPlatform;
begin
  if (FPlatform = pfAny) then
    Result := TPyEnvironment.TPlatform.FromSysPlatform(TOSVersion.Platform)
  else
    Result := FPlatform;
end;

procedure TPyEnvironment.Setup;
begin
  InternalSetup();
end;

constructor TPyEnvironment.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TPyEnvironment.Destroy;
begin
  inherited;
  TPyManager.Instance.UnregisterEnvironment(FPlatform, FArchitecture, FPythonVersion, Self);
end;

function TPyEnvironment.Exists: boolean;
begin
  Result := TDirectory.Exists(GetEnvironmentPath());
end;

function TPyEnvironment.GetArchitectureName(const AResolve: boolean): string;
var
  LArchitecture: TArchitecture;
begin
  if AResolve then
    LArchitecture := ResolveArchitecture()
  else
    LArchitecture := FArchitecture;

  Result := LArchitecture.ToString().ToLower();
end;

function TPyEnvironment.GetPlatformName(const AResolve: boolean): string;
var
  LPlatform: TPlatform;
begin
  if AResolve then
    LPlatform := ResolvePlatform()
  else
    LPlatform := FPlatform;

  Result := LPlatform.ToString().ToLower();
end;

function TPyEnvironment.IsSupported: boolean;
begin
  if (FPlatform = pfAny) and (FArchitecture = arAny) then
    Result := true
  else
    Result := (FPlatform.AsSysPlatform() = TOSVersion.Platform)
      and (FArchitecture.AsSysArchitecture() = TOSVersion.Architecture);
end;

procedure TPyEnvironment.Loaded;
begin
  inherited;
  TPyManager.Instance.RegisterEnvironment(FPlatform, FArchitecture, FPythonVersion, Self);
end;

procedure TPyEnvironment.Patch(const APythonEngine: TPythonEngine);
begin
  TPyManager.Patch(APythonEngine, Self);
end;

procedure TPyEnvironment.InternalSetup;
begin
  //
end;

function TPyEnvironment.GetHome: string;
begin
  Result := GetEnvironmentPath();
end;

function TPyEnvironment.GetProgramName: string;
begin
  Result := GetEnvironmentPath();
end;

function TPyEnvironment.GetSharedLibrary: string;
var
  LVerNum: string;
  I: integer;
  LLibName: string;
begin
  { TODO : (BETA) Improve localizer }
  LVerNum := String.Empty;
  for I := Low(PythonVersion) to High(PythonVersion) do
    if Char.IsDigit(PythonVersion, I) then
      LVerNum := LVerNum + PythonVersion[I];

  case ResolvePlatform() of
    TPlatform.pfWindows: begin
      if LVerNum.IsEmpty() then
        Exit(String.Empty);

      for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do
        if LVerNum.StartsWith(PYTHON_KNOWN_VERSIONS[I].RegVersion) then begin
          LLibName := PYTHON_KNOWN_VERSIONS[I].DllName;
          Break;
        end;

      Result := TPath.Combine(GetEnvironmentPath(), LLibName);
      if not TFile.Exists(Result) then
        Result := String.Empty;
    end
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

function TPyEnvironment.GetExecutable: string;
begin
  { TODO : (BETA) Improve localizer }
  case ResolvePlatform() of
    TPlatform.pfWindows: begin
      Result := TPath.Combine(GetEnvironmentPath(), 'python.exe');
      if not TFile.Exists(Result) then
        Result := String.Empty;
    end
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

{ TPyPlatformHelper }

class function TPyPlatformHelper.FromSysPlatform(
  const APlatform: TOSVersion.TPlatform): TPyEnvironment.TPlatform;
begin
  case APlatform of
    TOSVersion.TPlatform.pfWindows: Result := TPyEnvironment.TPlatform.pfWindows;
    TOSVersion.TPlatform.pfMacOS  : Result := TPyEnvironment.TPlatform.pfWindows;
    TOSVersion.TPlatform.pfLinux  : Result := TPyEnvironment.TPlatform.pfWindows;
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

function TPyPlatformHelper.AsSysPlatform: TOSVersion.TPlatform;
begin
  case Self of
    pfAny     : Result := TOSVersion.Platform;
    pfWindows : Result := TOSVersion.TPlatform.pfWindows;
    pfMacOS   : Result := TOSVersion.TPlatform.pfMacOS;
    pfLinux   : Result := TOSVersion.TPlatform.pfLinux;
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

function TPyPlatformHelper.ToString: string;
begin
  case Self of
    pfAny     : Result := 'Any';
    pfWindows : Result := 'Windows';
    pfMacOS   : Result := 'MacOS';
    pfLinux   : Result := 'Linux';
  end;
end;

{ TPyArchitectureHelper }

class function TPyArchitectureHelper.FromSysArchitecture(
  const AArchitecture: TOSVersion.TArchitecture): TPyEnvironment.TArchitecture;
begin
  case AArchitecture of
    TOSVersion.TArchitecture.arIntelX86: Result := TPyEnvironment.TArchitecture.arIntelX86;
    TOSVersion.TArchitecture.arIntelX64: Result := TPyEnvironment.TArchitecture.arIntelX64;
    TOSVersion.TArchitecture.arARM32   : Result := TPyEnvironment.TArchitecture.arARM32;
    TOSVersion.TArchitecture.arARM64   : Result := TPyEnvironment.TArchitecture.arARM64;
    else raise EUnsupportedArchitecture.Create('Unsupported architecture.');
  end;
end;

function TPyArchitectureHelper.AsSysArchitecture: TOSVersion.TArchitecture;
begin
  case Self of
    arAny      : Result := TOSVersion.Architecture;
    arIntelX86 : Result := TOSVersion.TArchitecture.arIntelX86;
    arIntelX64 : Result := TOSVersion.TArchitecture.arIntelX64;
    arARM32    : Result := TOSVersion.TArchitecture.arARM32;
    arARM64    : Result := TOSVersion.TArchitecture.arARM64;
    else raise EUnsupportedArchitecture.Create('Unsupported architecture.');
  end;
end;

function TPyArchitectureHelper.ToString: string;
begin
  case Self of
    arAny      : Result := 'Any';
    arIntelX86 : Result := 'IntelX86';
    arIntelX64 : Result := 'IntelX64';
    arARM32    : Result := 'ARM32';
    arARM64    : Result := 'ARM64';
  end;
end;

end.
