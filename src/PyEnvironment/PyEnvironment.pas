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
  TPyEnvironment = class abstract(TComponent, IEnvironmentPaths)
  public type
    TArchitecture = (arAny, arIntelX86, arIntelX64, arARM32, arARM64);
    TPlatform = (pfAny, pfWindows, pfMacOS, pfLinux);
  private
    FArchitecture: TArchitecture;
    FPlatform: TPlatform;    
    FPythonVersion: string;
  protected    
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
  public
    {***** IEnvironmentPaths implementation *****}

    //Python environment paths
    function GetHome(): string; virtual;
    function GetProgramName(): string; virtual;

    //Python file paths
    function GetSharedLibrary(): string; virtual;
    function GetExecutable(): string; virtual;
  public
    /// <summary>
    ///   Check if the current platform and architecture is compatible with the evnrionment definitions.
    /// </summary>
    function IsSupported(): boolean;
    /// <summary>
    ///   Check if a compatible environment exists.
    /// </summary>
    function Exists(): boolean; virtual;
    /// <summary>
    ///   Prepares the component.
    /// </summary>
    procedure Prepare(); virtual;
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

  EEnvironmentNotAvailable = class(Exception);

  EEmbeddableNotAvailable = class(Exception);

  EUnsupportedArchitecture = class(Exception);
  
  EUnsupportedPlatform = class(Exception);

implementation

uses
  System.IOUtils;

{ TPyEnvironment }

function TPyEnvironment.ResolveArchitecture: TArchitecture;
begin
  if (FArchitecture = TArchitecture.arAny) then
    case TOSVersion.Architecture of
      TOSVersion.TArchitecture.arIntelX86: Result := arIntelX86;
      TOSVersion.TArchitecture.arIntelX64: Result := arIntelX64;
      TOSVersion.TArchitecture.arARM32   : Result := arARM32; 
      TOSVersion.TArchitecture.arARM64   : Result := arARM64;
      else raise EUnsupportedArchitecture.Create('Unsupported architecture.');     
    end
  else
    Result := FArchitecture;
end;

function TPyEnvironment.ResolvePlatform: TPlatform;
begin
  if (FPlatform = pfAny) then
    case TOSVersion.Platform of
      TOSVersion.TPlatform.pfWindows: Result := pfWindows; 
      TOSVersion.TPlatform.pfMacOS  : Result := pfMacOS;
      TOSVersion.TPlatform.pfLinux  : Result := pfLinux;
      else raise EUnsupportedPlatform.Create('Unsupported platform.');
    end
  else
    Result := FPlatform;
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

  case LArchitecture of
    arAny     : Result := 'any';
    arIntelX86: Result := 'intelx86';
    arIntelX64: Result := 'intelx64';
    arARM32   : Result := 'arm32';
    arARM64   : Result := 'arm64';
    else Result := String.Empty;
  end;
end;

function TPyEnvironment.GetPlatformName(const AResolve: boolean): string;
var
  LPlatform: TPlatform;
begin
  if AResolve then
    LPlatform := ResolvePlatform()
  else
    LPlatform := FPlatform;

  case LPlatform of
    pfAny    : Result := 'any';
    pfWindows: Result := 'windows';
    pfMacOS  : Result := 'macos';
    pfLinux  : Result := 'linux';
  end;
end;

function TPyEnvironment.IsSupported: boolean;
begin
  if (FPlatform = pfAny) and (FArchitecture = arAny) then
    Exit(true);

  case ResolvePlatform() of
    pfWindows: Result := TOSVersion.Platform = TOSVersion.TPlatform.pfWindows;
    pfMacOS  : Result := TOSVersion.Platform = TOSVersion.TPlatform.pfMacOS;
    pfLinux  : Result := TOSVersion.Platform = TOSVersion.TPlatform.pfLinux;
    else Result := false;
  end;

  if not Result then
    Exit(false);

  case ResolveArchitecture() of  
    arIntelX86: Result := TOSVersion.Architecture = TOSVersion.TArchitecture.arIntelX86;
    arIntelX64: Result := TOSVersion.Architecture = TOSVersion.TArchitecture.arIntelX64;
    arARM32   : Result := TOSVersion.Architecture = TOSVersion.TArchitecture.arARM32;
    arARM64   : Result := TOSVersion.Architecture = TOSVersion.TArchitecture.arARM64;
    else Result := false;
  end;
end;

procedure TPyEnvironment.Prepare;
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
  LLibName: string;
  LFiles: TArray<string>;
begin
  { TODO : (BETA) Improve localizer }
  case ResolvePlatform() of
    TPlatform.pfWindows: begin
      if PythonVersion.StartsWith('3.5') then
        LLibName := 'python35.dll'
      else if PythonVersion.StartsWith('3.6') then
        LLibName := 'python36.dll'
      else if PythonVersion.StartsWith('3.7') then
        LLibName := 'python37.dll'
      else if PythonVersion.StartsWith('3.8') then
        LLibName := 'python38.dll'
      else if PythonVersion.StartsWith('3.9') then
        LLibName := 'python39.dll'
      else if PythonVersion.StartsWith('3.10') then
        LLibName := 'python310.dll'
      else Result := String.Empty;

      LFiles := TDirectory.GetFiles(GetEnvironmentPath(), LLibName, TSearchOption.soTopDirectoryOnly);
      if Length(LFiles) > 0 then
        Result := LFiles[0]
      else
        Result := String.Empty;
    end
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

function TPyEnvironment.GetExecutable: string;
var
  LFiles: TArray<string>;
begin
  { TODO : (BETA) Improve localizer }
  case ResolvePlatform() of
    TPlatform.pfWindows: begin
      LFiles := TDirectory.GetFiles(GetEnvironmentPath(), 'python.exe', TSearchOption.soTopDirectoryOnly);
      if Length(LFiles) > 0 then
        Result := LFiles[0]
      else
        Result := String.Empty;
    end
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

end.
