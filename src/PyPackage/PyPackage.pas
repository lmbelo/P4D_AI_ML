(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyPackage'        Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                   https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyPackage layer                                       *)
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
unit PyPackage;

interface

uses
  System.SysUtils, System.Rtti, System.Classes, System.Generics.Collections,
  PyCore, PyCommon, PyModule;

type
  TPyPip = class;
  (*----------------------------------------------------------------------------*)
  (*                                                                            *)
  (*                      Packages structure example                            *)
  (*                                                                            *)
  (* ref: https://docs.python.org/3/tutorial/modules.html                       *)
  (*                                                                            *)
  (*                                                                            *)
  (* dir                                                                        *)
  (*  +-- sound/                         Top-level package                      *)
  (*       +-- __init__.py               Initialize the sound package           *)
  (*       +-- formats/                  Subpackage for file format conversions *)
  (*            +-- __init__.py                                                 *)
  (*            +-- wavread.py           wavread module                         *)
  (*            +-- wavwrite.py          wavwrite module                        *)
  (*            +-- ...                                                         *)
  (*       +-- effects/                  Subpackage for sound effects           *)
  (*            +-- __init__.py                                                 *)
  (*            +-- echo.py              echo module                            *)
  (*            +-- surround.py          surround module                        *)
  (*            +-- ...                                                         *)
  (*       +-- filters/                  Subpackage for filters                 *)
  (*            +-- __init__.py                                                 *)
  (*            +-- equalizer.py         qualizer module                        *)
  (*            +-- vocoder.py           vocoder module                         *)
  (*            +-- ...                                                         *)
  (*----------------------------------------------------------------------------*)
  TPyPackageBase = class(TPyModuleBase)
  private
    FSubPackages: TDictionary<TPyPackageName, TPyPackageBase>;
    FModules: TDictionary<TPyModuleName, TPyModuleBase>;
    //Get methods
    function GetModule(const AName: TPyModuleName): TPyModuleBase;
    function GetSubPackage(const AName: TPyPackageName): TPyPackageBase;
  protected
    //Virtual methods
    function CreateSubPackage(const AName: TPyPackageName): TPyPackageBase; virtual;
    function CreateModule(const AName: TPyModuleName): TPyModuleBase; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function IsSubPackage(): boolean;

    property PySubPackage[const AName: TPyPackageName]: TPyPackageBase read GetSubPackage;
    property PyModule[const AName: TPyModuleName]: TPyModuleBase read GetModule;
  end;

  //Non PIP package
  TPyPackage = class(TPyPackageBase)
  published
    property PythonEngine;
  end;

  //PyPI package base
  TPyPyPIPackageBase = class(TPyPackageBase)
  protected
    function GetPyPIPackageName(): string; virtual;
    function GetPyPIPackageVer(): string; virtual;
  published
    property PyPIPackageName: string read GetPyPIPackageName;
    property PyPIPackageVersion: string read GetPyPIPackageVer;
  end;

  //PyPI package
  TPyPyPIPackage = class(TPyPyPIPackageBase)
  private
    FAutoInstall: boolean;
    FPyPIP: TPyPip;
  protected
    procedure ImportModule(); override;
    //PIP commands
    procedure CheckInstalled();
    procedure InstallPackage(); virtual;
    procedure UnInstallPackage(); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Install();
    procedure UnInstall();
  published
    property PythonEngine;
    property AutoInstall: boolean read FAutoInstall write FAutoInstall default true;
  end;

  //http://dcjtech.info/wp-content/uploads/2015/10/Pip-Cheatsheet.pdf
  TPyPip = class
  private
    FPyModule: TPyPyPIPackage;
    function IsReady(): boolean;
    function GetPackageName(): string;
    function GetPackageVer(): string;
    function FmtPkgInstallCmd(): string;
  public
    constructor Create(const APyModule: TPyPyPIPackage);
    function IsInstalled(): boolean;
    procedure Install();
    procedure UnInstall();
  end;

implementation

uses
  PyContext, PyExceptions, VarPyth, PythonEngine, System.Variants;

type
  TPyAnonymousPackage = class(TPyPackageBase)
  private
    FPyPackageName: TPyPackageName;
    FPyParent: TPyModuleBase;
  protected
    function GetPyModuleName(): string; override;
    function GetPyParent(): TPyModuleBase; override;
  public
    constructor Create(const AName: TPyPackageName; const AParent: TPyModuleBase); reintroduce;
  end;

  TPyAnonymousModule = class(TPyModuleBase)
  private
    FPyModuleName: TPyModuleName;
    FPyParent: TPyModuleBase;
  protected
    function GetPyModuleName(): string; override;
    function GetPyParent(): TPyModuleBase; override;
  public
    constructor Create(const AName: TPyModuleName; const AParent: TPyModuleBase); reintroduce;
  end;

{ TPyPyPIPackageBase }

function TPyPyPIPackageBase.GetPyPIPackageName: string;
begin
  var LInfo := TPyContext.Instance.FindInfo(ClassType);
  if Assigned(LInfo) then
    Result := LInfo.PyPipInfo.PackageName
  else
    Result := EmptyStr;
end;

function TPyPyPIPackageBase.GetPyPIPackageVer: string;
begin
  var LInfo := TPyContext.Instance.FindInfo(ClassType);
  if Assigned(LInfo) then
    Result := LInfo.PyPipInfo.PackageVer
  else
    Result := EmptyStr;
end;

{ TPyPIPPackage }

procedure TPyPyPIPackage.CheckInstalled;
begin
  if not FPyPIP.IsInstalled() then
    raise EPyPackageNotInstalled.CreateFmt(ErrPackageNotInstalled, [
      GetPyModuleName()]);
end;

constructor TPyPyPIPackage.Create(AOwner: TComponent);
begin
  inherited;
  FAutoInstall := true;
  FPyPIP := TPyPip.Create(Self);
end;

destructor TPyPyPIPackage.Destroy;
begin
  FPyPIP.Free();
  inherited;
end;

procedure TPyPyPIPackage.ImportModule;
begin
  if FAutoInstall then
    InstallPackage();
  CheckInstalled();
  inherited;
end;

procedure TPyPyPIPackage.InstallPackage;
begin
  if not FPyPIP.IsInstalled() then begin
    FPyPIP.Install();
  end;
end;

procedure TPyPyPIPackage.UnInstallPackage;
begin
  if FPyPIP.IsInstalled() then begin
    FPyPIP.UnInstall();
  end else
    raise EPyPackageNotInstalled.CreateFmt(ErrPackageNotInstalled, [
      GetPyModuleName()]);
end;

procedure TPyPyPIPackage.Install;
begin
  InstallPackage();
end;

procedure TPyPyPIPackage.UnInstall;
begin
  UnInstallPackage();
end;

{ TPyPip }

function TPyPip.IsReady(): boolean;
begin
  Result := Assigned(FPyModule)
        and Assigned(FPyModule.PythonEngine)
        and FPyModule.PythonEngine.Initialized;
end;

function TPyPip.IsInstalled(): boolean;
begin
  Result := false;
  var package := GetPackageName();
  if IsReady() then begin
    var sp := Import('subprocess');
    var cmd := 'pip show ' + package;
    var resp := sp.run(cmd, stdout := sp.PIPE, stderr := sp.STDOUT, shell := true, text := true);
    var stdout := resp.stdout;
    if VarIsPythonString(stdout) then begin
      Result := not (String(stdout).StartsWith('WARNING:') and String(stdout).Contains('Package(s) not found'));
    end;
  end else raise EModuleNotReady.CreateFmt('Module %s not ready', [package]);
end;

constructor TPyPip.Create(const APyModule: TPyPyPIPackage);
begin
  FPyModule := APyModule;
end;

function TPyPip.FmtPkgInstallCmd: string;
begin
  var LVer := GetPackageVer();
  if not LVer.IsEmpty then
    Result := GetPackageName() + '==' + LVer
  else
    Result := GetPackageName();
end;

function TPyPip.GetPackageName(): string;
begin
  Result := FPyModule.PyPIPackageName;
end;

function TPyPip.GetPackageVer: string;
begin
  Result := FPyModule.PyPIPackageVersion;
end;

procedure TPyPip.Install();
begin
  if IsReady() then begin
    var sp := Import('subprocess');
    var cmd := 'pip install ' + FmtPkgInstallCmd();
    var exec := sp.run(cmd, stdout := sp.PIPE, stderr := sp.STDOUT, shell := true, text := true);
  end else raise EModuleNotReady.CreateFmt('Module %s not ready', [GetPackageName()]);
end;

procedure TPyPip.UnInstall();
begin
  if IsReady() then begin
    var sp := Import('subprocess');
    var cmd := 'pip uninstall ' + GetPackageName() + ' -y';
    sp.run(cmd, stdout := sp.PIPE, stderr := sp.STDOUT, shell := true, text := true);
  end else raise EModuleNotReady.CreateFmt('Module %s not ready', [GetPackageName()]);
end;

{ TPyPackageBase }

constructor TPyPackageBase.Create(AOwner: TComponent);
begin
  inherited;
  FSubPackages := TDictionary<TPyPackageName, TPyPackageBase>.Create();
  FModules := TDictionary<TPyModuleName, TPyModuleBase>.Create();
end;

function TPyPackageBase.CreateModule(const AName: TPyModuleName): TPyModuleBase;
begin
  Result := TPyAnonymousModule.Create(AName, Self);
  Result.PythonEngine := PythonEngine;
end;

function TPyPackageBase.CreateSubPackage(
  const AName: TPyPackageName): TPyPackageBase;
begin
  Result := TPyAnonymousPackage.Create(AName, Self);
  Result.PythonEngine := PythonEngine;
end;

destructor TPyPackageBase.Destroy;
begin
  FModules.Free();
  FSubPackages.Free();
  inherited;
end;

function TPyPackageBase.GetModule(const AName: TPyModuleName): TPyModuleBase;
begin
  FModules.TryGetValue(AName, Result);
  if not Assigned(Result) then begin
    Result := CreateModule(AName);
    FModules.Add(AName, Result);
  end;
end;

function TPyPackageBase.GetSubPackage(
  const AName: TPyPackageName): TPyPackageBase;
begin
  FSubPackages.TryGetValue(AName, Result);
  if not Assigned(Result) then begin
    Result := CreateSubPackage(AName);
    FSubPackages.Add(AName, Result);
  end;
end;

function TPyPackageBase.IsSubPackage: boolean;
begin
  Result := Assigned(PyParent);
end;

{ TPyAnonymousPackage }

constructor TPyAnonymousPackage.Create(const AName: TPyPackageName;
  const AParent: TPyModuleBase);
begin
  inherited Create(AParent);
  FPyParent := AParent;
  FPyPackageName := AName;
end;

function TPyAnonymousPackage.GetPyModuleName: string;
begin
  Result := FPyPackageName;
end;

function TPyAnonymousPackage.GetPyParent: TPyModuleBase;
begin
  Result := FPyParent;
end;

{ TPyAnonymousModule }

constructor TPyAnonymousModule.Create(const AName: TPyModuleName;
  const AParent: TPyModuleBase);
begin
  inherited Create(AParent);
  FPyParent := AParent;
  FPyModuleName := AName;
end;

function TPyAnonymousModule.GetPyModuleName: string;
begin
  Result := FPyModuleName;
end;

function TPyAnonymousModule.GetPyParent: TPyModuleBase;
begin
  Result := FPyParent;
end;

end.
