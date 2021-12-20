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
  PyCore, PyCommon, PyModule,
  PyPackage.Model, PyPackage.Manager.ManagerKind,
  PyPackage.Manager.Intf, PyPackage.Manager.Defs;

type
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

  //Non-managed package
  TPyPackage = class(TPyPackageBase)
  published
    property PythonEngine;
  end;

  //Managed package
  TPyManagedPackage = class abstract(TPyPackageBase)
  private type
    TPyManagers = class(TPersistent)
    private
      FModel: TPyPackageModel;
      function GetConda: TPyPackageManagerDefs;
      function GetPip: TPyPackageManagerDefs;
      procedure SetConda(const Value: TPyPackageManagerDefs);
      procedure SetPip(const Value: TPyPackageManagerDefs);
    public
      constructor Create(const AModel: TPyPackageModel);
    published
      property Pip: TPyPackageManagerDefs read GetPip write SetPip;
      property Conda: TPyPackageManagerDefs read GetConda write SetConda;
    end;
  private
    FModel: TPyPackageModel;
    FManagerKind: TPyPackageManagerKind;
    FManagers: TPyManagers;
    FAutoInstall: boolean;
    procedure SetManagers(const Value: TPyManagers);
  protected
    function GetPyModuleName(): string; override;
    procedure ImportModule(); override;
  protected
    procedure Prepare(const AModel: TPyPackageModel); virtual; abstract;
  protected
    procedure CheckInstalled();
    procedure InstallPackage(); virtual;
    procedure UnInstallPackage(); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Install();
    procedure Uninstall();
    function IsInstalled(): boolean;
  published
    property PythonEngine;
    property ManagerKind: TPyPackageManagerKind read FManagerKind write FManagerKind;
    property Managers: TPyManagers read FManagers write SetManagers;
    property AutoInstall: boolean read FAutoInstall write FAutoInstall default true;
  end;

implementation

uses
  PyExceptions, VarPyth, PythonEngine, System.Variants;

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

{ TPyManagedPackage }

constructor TPyManagedPackage.Create(AOwner: TComponent);
begin
  inherited;
  FAutoInstall := true;
  FModel := TPyPackageModel.Create();
  FManagers := TPyManagers.Create(FModel);
  Prepare(FModel);
end;

destructor TPyManagedPackage.Destroy;
begin
  FManagers.Free();
  FModel.Free();
  inherited;
end;

procedure TPyManagedPackage.SetManagers(const Value: TPyManagers);
begin
  FManagers.Assign(Value);
end;

function TPyManagedPackage.GetPyModuleName: string;
begin
  Result := FModel.PackageName;
end;

procedure TPyManagedPackage.ImportModule;
begin
  if FAutoInstall then
    InstallPackage();
  CheckInstalled();
  inherited;
end;

procedure TPyManagedPackage.Install;
begin
  InstallPackage();
end;

procedure TPyManagedPackage.Uninstall;
begin
  UnInstallPackage();
end;

function TPyManagedPackage.IsInstalled: boolean;
begin
  Result := FModel.PackageManagers.Items[ManagerKind].IsInstalled();
end;

procedure TPyManagedPackage.CheckInstalled;
begin
  if IsReady() then
    if not IsInstalled() then
      raise EPyPackageNotInstalled.CreateFmt(ErrPackageNotInstalled, [
        GetPyModuleName()]);
end;

procedure TPyManagedPackage.InstallPackage;
begin
  if IsReady() then
    if not IsInstalled() then begin
      FModel.PackageManagers.Items[ManagerKind].Install();
    end;
end;

procedure TPyManagedPackage.UnInstallPackage;
begin
  if IsReady() then
    if IsInstalled() then begin
      FModel.PackageManagers.Items[ManagerKind].Uninstall();
    end else
      raise EPyPackageNotInstalled.CreateFmt(ErrPackageNotInstalled, [
        GetPyModuleName()]);
end;

{ TPyManagedPackage.TPyManagers }

constructor TPyManagedPackage.TPyManagers.Create(const AModel: TPyPackageModel);
begin
  FModel := AModel;
end;

function TPyManagedPackage.TPyManagers.GetConda: TPyPackageManagerDefs;
begin
  if FModel.PackageManagers.ContainsKey(TPyPackageManagerKind.conda) then
    Result := FModel.PackageManagers.Items[TPyPackageManagerKind.conda].Defs
  else Result := nil;
end;

function TPyManagedPackage.TPyManagers.GetPip: TPyPackageManagerDefs;
begin
  if FModel.PackageManagers.ContainsKey(TPyPackageManagerKind.pip) then
    Result := FModel.PackageManagers.Items[TPyPackageManagerKind.pip].Defs
  else Result := nil;
end;

procedure TPyManagedPackage.TPyManagers.SetConda(
  const Value: TPyPackageManagerDefs);
begin
  if FModel.PackageManagers.ContainsKey(TPyPackageManagerKind.conda) then
    FModel.PackageManagers.Items[TPyPackageManagerKind.conda].Defs.Assign(Value);
end;

procedure TPyManagedPackage.TPyManagers.SetPip(
  const Value: TPyPackageManagerDefs);
begin
  if FModel.PackageManagers.ContainsKey(TPyPackageManagerKind.pip) then
    FModel.PackageManagers.Items[TPyPackageManagerKind.pip].Defs.Assign(Value);
end;

end.
