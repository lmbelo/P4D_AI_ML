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
  PythonEngine;

type
  (*----------------------------------------------------------------------------*)
  (*                                                                            *)
  (*                      Environment structure example                         *)
  (*                                                                            *)
  (*                                                                            *)
  (*                                                                            *)
  (* EnvironmentsPath                                                           *)
  (*  +-- python/                                                               *)
  (*       +-- environments/                                                    *)
  (*            +-- os/                                                         *)
  (*                 +-- arch/                                                  *)
  (*                      +-- python version/                                   *)
  (*                           +-- python folder                                *)
  (*----------------------------------------------------------------------------*)
  TPyEnvironment = class(TComponent)
  public type
    TArchitecture = (arAny, arIntelX86, arIntelX64, arARM32, arARM64);
    TPlatform = (pfAny, pfWindows, pfMacOS, pfLinux);
  private
    FArchitecture: TArchitecture;
    FPlatform: TPlatform;
    FEnvironmentsPath: string;
    FPythonVersion: string;
    FPythonEngine: TPythonEngine;
  protected
    /// <summary>
    ///   Navigates through the environments searching for a compatible installation.
    /// </summary>
    function EnvironmentExists(): boolean; virtual;
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

    //Environment settings

    /// <summary>
    ///   Specifies the Python environment folder.
    /// </summary>
    property EnvironmentsPath: string read FEnvironmentsPath write FEnvironmentsPath;

    //Python settings

    /// <summary>
    ///   Specifies the Python version.
    /// </summary>
    property PythonVersion: string read FPythonVersion write FPythonVersion;

    //P4D Python Engine settings

    /// <summary>
    ///   Reference to the P4D Python Engine
    /// </summary>
    property PythonEngine: TPythonEngine read FPythonEngine write FPythonEngine;
  end;

  (*----------------------------------------------------------------------------*)
  (*                                                                            *)
  (*                      Embeddables structure example                         *)
  (*                                                                            *)
  (*                                                                            *)
  (*                                                                            *)
  (* EmbeddablesPath                                                            *)
  (*  +-- python/                                                               *)
  (*       +-- environments/                                                    *)
  (*            +-- os/                                                         *)
  (*                 +-- arch/                                                  *)
  (*                      +-- python version/                                   *)
  (*                           +-- python zip                                   *)
  (*----------------------------------------------------------------------------*)
  TPyEmbeddedEnv = class(TPyEnvironment)
  private
    FEmbeddablesPath: string;
  protected
    /// <summary>
    ///   Navigates through the embeddables searching for a compatible distribution.
    /// </summary>
    function EmbeddableExists(): boolean; virtual;
    /// <summary>
    ///   Creates a new environment based on the current settings.
    ///   An embeddable distribution will be used as an "image".
    /// </summary>
    procedure CreateEnvironment(); virtual;
  published
    property Architecture;
    property Platform;
    /// <summary>
    ///   Specifies the Python version. It must match with a Python version localized in the embeddables folder.
    /// </summary>
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    /// <summary>
    ///   Specifies where the embedded Python will be placed. Let it empty for default settings.
    /// </summary>
    property EnvironmentsPath;

    //Environment settings

    /// <summary>
    ///   Specifies the Pyton embeddables folder.
    /// </summary>
    property EmbeddablesPath: string read FEmbeddablesPath write FEmbeddablesPath;
  end;

  EEnvironmentNotAvailable = class(Exception)
  end;

  EEmbeddableNotAvailable = class(Exception)
  end;

implementation

{ TPyEnvironment }

function TPyEnvironment.EnvironmentExists: boolean;
begin

end;

end.
