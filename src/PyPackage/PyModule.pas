(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'    Copyright (c) 2021                         *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                https://github.com/lmbelo/P4D_AI_ML      *)
(**************************************************************************)
(*  Functionality:  PyModule layer                                        *)
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
unit PyModule;

interface

uses
  System.Classes, System.Generics.Collections, PythonEngine, PyCommon, System.SysUtils;

type
  TPyModuleBaseClass = class of TPyModuleBase;
  TPyModuleBase = class(TPyCommonCustomModule)
  private
    FSubModules: TList<TPyModuleBase>;
    FPyParentModule: TPyModuleBase; //if this module is a submodule
    FAutoImport: boolean;
    FAutoImportSubModules: boolean;
    FAutoInstall: boolean;
    function CanImport(): boolean;
    //Set methods
    procedure SetPyParentModule(const AParentModule: TPyModuleBase);
    //Get methods
    function GetPyModuleName(): string;
  protected
    procedure Loaded; override;
    procedure EngineLoaded(); override;
    procedure ImportModule(); reintroduce; virtual;
    procedure InstallPackage();
    procedure CheckImported();
    //Submodules rotines
    procedure CreateSubModules(); virtual;
    procedure ImportSubModules(); virtual;
    procedure CheckSubModule(const AClass: TPyModuleBaseClass);
    function GetSubModule(const AClass: TPyModuleBaseClass): TPyModuleBase;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Import();
    function IsImported(): boolean;
    function IsSubModule: boolean;

    property PyParentModule: TPyModuleBase read FPyParentModule write SetPyParentModule;
  published
    property PyModuleName: string read GetPyModuleName;
    property AutoImport: boolean read FAutoImport write FAutoImport default true;
    property AutoImportSubModules: boolean read FAutoImportSubModules write FAutoImportSubModules default true;
    property AutoInstall: boolean read FAutoInstall write FAutoInstall default true;
  end;

  TPyModule = class(TPyModuleBase)
  published
    property PythonEngine;
  end;

  PyModuleNameAttribute = class(TCustomAttribute)
  private
    FPyModuleName: string;
  public
    constructor Create(const APyModuleName: string);
    property PyModuleName: string read FPyModuleName write FPyModuleName;
  end;

  EPyPackageNotInstalled = class(EPyCommonException)
  end;

  EPyParentModuleCircularReference = class(EPyCommonException)
  end;

  EPyModuleNotImported = class(EPyCommonException)
  end;

  EPySubModuleNotFound = class(EPyCommonException)
  end;

resourcestring
  ErrPackageNotInstalled = 'Package %s not installed.';
  ErrModuleNotImported = 'Module not imported.';
  ErrSubModuleNotFound = 'Submodule not found.';
  ErrCircularRefNotAllowed = 'Circular reference not allowed.';

implementation

uses
  PyContext, PyPIP, System.Rtti;

{ TPyModuleBase }

procedure TPyModuleBase.CheckImported;
begin
  if not IsImported() then
    raise EPyModuleNotImported.Create(ErrModuleNotImported);
end;

procedure TPyModuleBase.CheckSubModule(const AClass: TPyModuleBaseClass);
begin
  var LSubModule := GetSubModule(AClass);
  if not Assigned(LSubModule) then
    raise EPySubModuleNotFound.Create(ErrSubModuleNotFound);
end;

constructor TPyModuleBase.Create(AOwner: TComponent);
begin
  inherited;
  FAutoImport := true;
  FAutoImportSubModules := true;
  FAutoInstall := true;
  FSubModules := TList<TPyModuleBase>.Create();
  CreateSubModules();
end;

procedure TPyModuleBase.CreateSubModules;
var
  LSubModuleInfo: TPySubModuleInfo;
begin
  var LModuleInfo := TPyContext.Instance.FindModuleInfo(Self.ClassType, true);
  if Assigned(LModuleInfo) then begin
    for LSubModuleInfo in LModuleInfo.SubModules do begin
      var LSubModule := TPyModuleBaseClass(LSubModuleInfo.Clazz).Create(Self);
      LSubModule.PyParentModule := Self;
      FSubModules.Add(LSubModule);
    end;
  end;
end;

destructor TPyModuleBase.Destroy;
begin
  FSubModules.Free();
  inherited;
end;

procedure TPyModuleBase.EngineLoaded;
begin
  inherited;
  if FAutoImport and CanImport() then
    Import();
end;

function TPyModuleBase.GetPyModuleName: string;
begin
  var LModuleInfo := TPyContext.Instance.FindModuleInfo(Self.ClassType, true);
  if Assigned(LModuleInfo) then
    Result := LModuleInfo.ModuleName
  else
    Result := String.Empty;
end;

function TPyModuleBase.GetSubModule(const AClass: TPyModuleBaseClass): TPyModuleBase;
var
  LSubModule: TPyModuleBase;
begin
  for LSubModule in FSubModules do begin
    if (LSubModule.ClassType = AClass) then
      Exit(LSubModule);
  end;
  Result := nil;
end;

function TPyModuleBase.CanImport: boolean;
begin
  Result := not (csDesigning in ComponentState)
    and Assigned(PythonEngine)
    and PythonEngine.Initialized
    and not IsImported();
end;

procedure TPyModuleBase.Import;
begin
  if not IsSubModule() then
    InstallPackage();
  ImportModule();
  if FAutoImportSubModules then
    ImportSubModules();
end;

function TPyModuleBase.IsImported: boolean;
begin
  Result := Assigned(PyModule);
end;

function TPyModuleBase.IsSubModule: boolean;
begin
  Result := Assigned(FPyParentModule);
end;

procedure TPyModuleBase.ImportModule;
begin
  if IsSubModule then
    inherited ImportModule(FPyParentModule.PyModuleName, PyModuleName)
  else
    inherited ImportModule(PyModuleName);
end;

procedure TPyModuleBase.ImportSubModules;
var
  LSubModule: TPyModuleBase;
begin
  for LSubModule in FSubModules do begin
    LSubModule.PythonEngine := Self.PythonEngine;
    LSubModule.Import();
  end;
end;

procedure TPyModuleBase.InstallPackage;
begin
  var LPyPIP := TPyPip.Create(Self);
  try
    if not LPyPIP.IsInstalled() then begin
      if FAutoInstall then begin
        LPyPIP.Install();
      end else
        raise EPyPackageNotInstalled.CreateFmt(ErrPackageNotInstalled, [
          GetPyModuleName()]);
    end;
  finally
    LPyPIP.Free();
  end;
end;

procedure TPyModuleBase.Loaded;
begin
  inherited;
  if FAutoImport and CanImport() then
    Import();
end;

procedure TPyModuleBase.SetPyParentModule(const AParentModule: TPyModuleBase);
begin
  if AParentModule = Self then
    raise EPyParentModuleCircularReference.Create(ErrCircularRefNotAllowed);
  FPyParentModule := AParentModule;
end;

{ ModuleNameAttribute }

constructor PyModuleNameAttribute.Create(const APyModuleName: string);
begin
  FPyModuleName := APyModuleName;
end;

end.
