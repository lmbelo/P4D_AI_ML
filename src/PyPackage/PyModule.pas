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
  System.Classes, PythonEngine, PyCommon, System.SysUtils;

type
  TPyModuleBase = class(TPyCommonCustomModule)
  private
    FPyParentModule: TPyModuleBase; //if this module is a submodule
    FAutoImport: boolean;
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
  public
    constructor Create(AOwner: TComponent); override;

    procedure Import();
    function IsImported(): boolean;
    function IsSubModule: boolean;

    property PyParentModule: TPyModuleBase read FPyParentModule write SetPyParentModule;
  published
    property PyModuleName: string read GetPyModuleName;
    property AutoImport: boolean read FAutoImport write FAutoImport default true;
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

  EPyPackageNotInstalled = class(Exception)
  end;

  EPyParentModuleCircularReference = class(Exception)
  end;

  EPyModuleNotImported = class(Exception)
  end;

implementation

uses
  PyContext, PyPIP, System.Rtti;

{ TPyModuleBase }

procedure TPyModuleBase.CheckImported;
begin
  if not Assigned(PyModule) then
    raise EPyModuleNotImported.Create('Module not imported.');
end;

constructor TPyModuleBase.Create(AOwner: TComponent);
begin
  inherited;
  FAutoImport := true;
  FAutoInstall := true;
end;

procedure TPyModuleBase.EngineLoaded;
begin
  inherited;
  if FAutoImport and CanImport() then
    Import();
end;

function TPyModuleBase.GetPyModuleName: string;
begin
  var LInfo := TPyContext.Instance.FindInfo(Self.ClassType);
  if Assigned(LInfo) then
    Result := LInfo.PyModuleInfo.ModuleName
  else
    Result := EmptyStr;
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

procedure TPyModuleBase.InstallPackage;
begin
  var LPyPIP := TPyPip.Create(Self);
  try
    if not LPyPIP.IsInstalled() then begin
      if FAutoInstall then begin
        LPyPIP.Install();
      end else
        raise EPyPackageNotInstalled.CreateFmt('Package %s not installed.', [
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
    raise EPyParentModuleCircularReference.Create('Circular reference not allowed.');
  FPyParentModule := AParentModule;
end;

{ ModuleNameAttribute }

constructor PyModuleNameAttribute.Create(const APyModuleName: string);
begin
  FPyModuleName := APyModuleName;
end;

end.
