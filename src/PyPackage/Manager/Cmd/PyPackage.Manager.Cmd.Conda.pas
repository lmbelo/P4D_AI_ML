unit PyPackage.Manager.Cmd.Conda;

interface

uses
  PyPackage.Manager.Cmd.Intf,
  PyPackage.Manager.Defs;

type
  TPyPackageManagerCmdConda = class(TInterfacedObject, IPyPackageManagerCmdIntf)
  public
    function BuildInstallCmd(const ADefs: TPyPackageManagerDefs): TArray<string>;
    function BuildUninstallCmd(const ADefs: TPyPackageManagerDefs): TArray<string>;
  end;

implementation

uses
  System.SysUtils;

{ TPyPackageManagerCmdConda }

function TPyPackageManagerCmdConda.BuildInstallCmd(
  const ADefs: TPyPackageManagerDefs): TArray<string>;
begin
  raise ENotImplemented.Create('Not Implemented.');
end;

function TPyPackageManagerCmdConda.BuildUninstallCmd(
  const ADefs: TPyPackageManagerDefs): TArray<string>;
begin
  raise ENotImplemented.Create('Not Implemented.');
end;

end.
