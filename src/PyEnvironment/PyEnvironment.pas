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
  System.Classes, System.SysUtils;

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
  TPyEnvironment = class abstract(TComponent)
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
    ///   Return the architecure folder name.
    /// </summary>
    function GetArchitectureName(): string;
    /// <summary>
    ///   Return the platform folder name.
    /// </summary>
    function GetPlatformName(): string;
    /// <summary>
    ///   Return the Python environment based on the current settings.
    /// </summary>
    function GetEnvironmentPath(): string; virtual; abstract;
  public
    /// <summary>
    ///   Check if the current platform and architecture is compatible with the evnrionment definitions.
    /// </summary>
    function IsSupported(): boolean;
    /// <summary>
    ///   Check if a compatible environment exists.
    /// </summary>
    function Exists(): boolean; virtual;
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

function TPyEnvironment.GetArchitectureName: string;
begin
  case ResolveArchitecture() of
    arIntelX86: Result := 'intelx86';
    arIntelX64: Result := 'intelx64';
    arARM32   : Result := 'arm32';
    arARM64   : Result := 'arm64';
    else Result := String.Empty;
  end;
end;

function TPyEnvironment.GetPlatformName: string;
begin
  case ResolvePlatform() of
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

end.