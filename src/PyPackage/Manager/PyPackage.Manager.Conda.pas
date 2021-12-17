unit PyPackage.Manager.Conda;

interface

uses
  PyPackage.Manager.Intf, PyPackage.Manager.Defs, PyPackage.Manager.Cmd.Intf;

type
  TPyPackageManagerConda = class(TInterfacedObject, IPyPackageManager)
  private
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
  end;

implementation

uses
  System.SysUtils;

{ TPyPackageManagerConda }

function TPyPackageManagerConda.GetCmd: IPyPackageManagerCmdIntf;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TPyPackageManagerConda.GetDefs: TPyPackageManagerDefs;
begin
  raise ENotImplemented.Create('Not implemented');
end;

end.
