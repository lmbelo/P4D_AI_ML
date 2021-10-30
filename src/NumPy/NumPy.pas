unit NumPy;

interface

uses
  System.Classes, PyModule, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  [PyModuleName('numpy')]
  TNumPy = class(TPyModule)
  private
    Fnp: variant;
    function Getnp: variant;
  protected
    procedure ImportModule; override;
  public
    property np: variant read Getnp;
  end;

implementation

uses
  VarPyth, System.Variants;

{ TNumPy }

function TNumPy.Getnp: variant;
begin
  if VarIsNull(Fnp) or VarIsEmpty(Fnp) then
    Fnp := VarPythonCreate(PyModule);
  Result := Fnp;
end;

procedure TNumPy.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

end.
