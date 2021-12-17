unit PyPackage.Manager.Cmd.Conda;

interface

uses
  PyPackage.Manager.Cmd.Intf,
  PyPackage.Manager.Defs;

type
  TPyPackageManagerCmdConda = class(TInterfacedObject, IPyPackageManagerCmdIntf)
  public
    function BuildInstallCmd(const ADefs: TPyPackageManagerDefs): string;
    function BuildUninstallCmd(const ADefs: TPyPackageManagerDefs): string;
    function BuildIsInstalledCmd(const ADefs: TPyPackageManagerDefs): string;
  end;

implementation

{ TPyPackageManagerCmdConda }

function TPyPackageManagerCmdConda.BuildInstallCmd(
  const ADefs: TPyPackageManagerDefs): string;
begin

end;

function TPyPackageManagerCmdConda.BuildIsInstalledCmd(
  const ADefs: TPyPackageManagerDefs): string;
begin

end;

function TPyPackageManagerCmdConda.BuildUninstallCmd(
  const ADefs: TPyPackageManagerDefs): string;
begin

end;

end.
