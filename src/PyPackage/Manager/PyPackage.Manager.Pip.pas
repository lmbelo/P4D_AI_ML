unit PyPackage.Manager.Pip;

interface

uses
  PyPackage.Manager.Intf, PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf;

type
  TPyPackageManagerPip = class(TInterfacedObject, IPyPackageManager)
  private
    FDefs: TPyPackageManagerDefs;
    FCmd: IPyPackageManagerCmdIntf;

    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
  public
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  PyPackage.Manager.Defs.Pip, PyPackage.Manager.Cmd.Pip;

{ TPyPackageManagerPip }

constructor TPyPackageManagerPip.Create;
begin
  FDefs := TPyPackageManagerDefsPip.Create();
  FCmd := TPyPackageManagerCmdPip.Create();
  inherited Create();
end;

destructor TPyPackageManagerPip.Destroy;
begin
  FDefs.Free();
  inherited;
end;

function TPyPackageManagerPip.GetCmd: IPyPackageManagerCmdIntf;
begin
  Result := FCmd;
end;

function TPyPackageManagerPip.GetDefs: TPyPackageManagerDefs;
begin
  Result := FDefs;
end;

end.
