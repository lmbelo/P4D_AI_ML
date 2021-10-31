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
  protected
    procedure Loaded; override;
    procedure EngineLoaded(); override;
    procedure ImportModule; reintroduce; virtual;
    //Get methods
    function GetPyModuleName(): string; virtual; abstract;
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
  protected
    function GetPyModuleName(): string; override;
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

implementation

uses
  PyPIP, System.Rtti;

{ TPyModuleBase }

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

function TPyModuleBase.CanImport: boolean;
begin
  Result := not (csDesigning in ComponentState)
    and Assigned(PythonEngine)
    and PythonEngine.Initialized
    and not IsImported();
end;

procedure TPyModuleBase.Import;
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
  ImportModule;
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

{ TPyModule }

function TPyModule.GetPyModuleName: string;
var
  LAttr: TCustomAttribute;
begin
  var LCtx := TRttiContext.Create();
  try
    var LType := LCtx.GetType(ClassType);
    for LAttr in LType.GetAttributes() do begin
      if LAttr is PyModuleNameAttribute then begin
        Exit(PyModuleNameAttribute(LAttr).PyModuleName);
      end;
    end;
  finally
    LCtx.Free();
  end;
end;

{ ModuleNameAttribute }

constructor PyModuleNameAttribute.Create(const APyModuleName: string);
begin
  FPyModuleName := APyModuleName;
end;

end.
