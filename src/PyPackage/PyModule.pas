unit PyModule;

interface

uses
  System.Classes, PyCommon, PythonEngine;

type
  TPyCustomModule = class(TPyCommonCustomModule)
  private
    FPyParentModule: TPyCustomModule; //if this module is a submodule
    FAutoImport: boolean;
    function CanImport(): boolean;
    //Set methods
    procedure SetPyParentModule(const AParentModule: TPyCustomModule);
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

    property PyParentModule: TPyCustomModule read FPyParentModule write SetPyParentModule;
  published
    property PyModuleName: string read GetPyModuleName;
    property AutoImport: boolean read FAutoImport write FAutoImport default true;
  end;

  TPyModule = class(TPyCustomModule)
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

implementation

uses
  System.Rtti, System.SysUtils;

{ TPyCustomModule }

constructor TPyCustomModule.Create(AOwner: TComponent);
begin
  inherited;
  FAutoImport := true;
end;

procedure TPyCustomModule.EngineLoaded;
begin
  inherited;
  if CanImport() then
    Import();
end;

function TPyCustomModule.CanImport: boolean;
begin
  Result := not (csDesigning in ComponentState)
    and not IsImported()
    and Assigned(PythonEngine)
    and PythonEngine.Initialized;
end;

procedure TPyCustomModule.Import;
begin
  ImportModule;
end;

function TPyCustomModule.IsImported: boolean;
begin
  Result := Assigned(PyModule);
end;

function TPyCustomModule.IsSubModule: boolean;
begin
  Result := Assigned(FPyParentModule);
end;

procedure TPyCustomModule.ImportModule;
begin
  if IsSubModule then
    inherited ImportModule(FPyParentModule.PyModuleName, PyModuleName)
  else
    inherited ImportModule(PyModuleName);
end;

procedure TPyCustomModule.Loaded;
begin
  inherited;
  if FAutoImport and CanImport() then
    Import();
end;

procedure TPyCustomModule.SetPyParentModule(const AParentModule: TPyCustomModule);
begin
  if AParentModule = Self then
    raise Exception.Create('Circular reference not allowed.');
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

{ PyModuleNameAttribute }

constructor PyModuleNameAttribute.Create(const APyModuleName: string);
begin
  FPyModuleName := APyModuleName;
end;

end.
