unit PyCommon;

interface

uses
  System.Classes, PythonEngine;

type
  //P4D AI&ML extension
  TPyCommon = class(TComponent)
  private
    FPythonEngine: TPythonEngine;
  protected
    procedure SetPythonEngine(const APythonEngine: TPythonEngine);
    procedure EngineLoaded(); virtual;
  public
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
  end;

  //P4D AI&ML module extension
  TPyCommonCustomModule = class(TPyCommon)
  private
    FPyModule: PPyObject;
  protected
    //https://docs.python.org/3/c-api/import.html?highlight=importmodule#c.PyImport_ImportModule
    procedure ImportModule(const AModuleName: string); overload;
    //https://docs.python.org/3/c-api/import.html?highlight=importmodule#c.PyImport_ImportModule
    procedure ImportModule(const AModuleName, ASubModuleName: string); overload;

    property PyModule: PPyObject read FPyModule;
  end;

implementation

{ TPyCommon }

procedure TPyCommon.EngineLoaded;
begin
  //
end;

procedure TPyCommon.SetPythonEngine(const APythonEngine: TPythonEngine);
begin
  if APythonEngine <> FPythonEngine then begin
    FPythonEngine := APythonEngine;
    EngineLoaded();
  end;
end;

{ TPyCommonCustomModule }

procedure TPyCommonCustomModule.ImportModule(const AModuleName,
  ASubModuleName: string);
begin
  FPyModule := PythonEngine.PyImport_ImportModule(PAnsiChar(AnsiString(
    AModuleName
    + '.'
    + ASubModuleName)));
end;

procedure TPyCommonCustomModule.ImportModule(const AModuleName: string);
begin
  FPyModule := PythonEngine.PyImport_ImportModule(PAnsiChar(AnsiString(AModuleName)));
end;

end.
