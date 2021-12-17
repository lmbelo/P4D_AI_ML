unit PyPackage.Manager.Cmd.Intf;

interface

uses
  PyPackage.Manager.Defs;

type
  IPyPackageManagerCmdIntf = interface
    ['{B6FADED7-BBA9-4E55-B1AD-A0C0CE535B96}']
    function BuildInstallCmd(const ADefs: TPyPackageManagerDefs): string;
    function BuildUninstallCmd(const ADefs: TPyPackageManagerDefs): string;
    function BuildIsInstalledCmd(const ADefs: TPyPackageManagerDefs): string;
  end;

implementation

end.
