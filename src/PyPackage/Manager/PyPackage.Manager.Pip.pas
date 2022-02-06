unit PyPackage.Manager.Pip;

interface

uses
  PyCore, PyPackage,
  PyPackage.Manager,
  PyPackage.Manager.Intf,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf;

type
  TPyPackageManagerPip = class(TPyPackageManager, IPyPackageManager)
  private
    FDefs: TPyPackageManagerDefs;
    FCmd: IPyPackageManagerCmdIntf;

    //IPyPackageManager implementation
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
    function IsInstalled(): boolean; reintroduce;
    procedure Install();
    procedure Uninstall();
  public
    constructor Create(const APackageName: TPyPackageName); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Variants, System.SysUtils,
  VarPyth, PyUtils, PyExceptions,
  PyPackage.Manager.Defs.Pip,
  PyPackage.Manager.Cmd.Pip, PythonEngine;

{ TPyPackageManagerPip }

constructor TPyPackageManagerPip.Create(const APackageName: TPyPackageName);
begin
  inherited;
  FDefs := TPyPackageManagerDefsPip.Create(APackageName);
  FCmd := TPyPackageManagerCmdPip.Create();
end;

destructor TPyPackageManagerPip.Destroy;
begin
  FCmd := nil;
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

function TPyPackageManagerPip.IsInstalled: boolean;
begin
  //Reloading the module garantees we're considering the latest installed packages
  var LPkgRes := Reload(Import('pkg_resources'));
  for var LPkg in VarPyIterate(LPkgRes.working_set) do begin
    if (LPkg.key = FDefs.PackageName) then
      Exit(true);
  end;
  Result := false;
end;

procedure TPyPackageManagerPip.Install;
begin
  //Using pip programmatically guarantees we're using the same Python interpreter
  //loaded by the application
  var LIn := FCmd.BuildInstallCmd(FDefs);
  var LPip := Import('pip');
  var LResult := LPip.main(TPyEx.List<String>(LIn));
  if LResult <> 0 then
    raise EPyModuleInstallError.CreateFmt(
      'An error occurred while installing the package %s.', [FDefs.PackageName]);
end;

procedure TPyPackageManagerPip.Uninstall;
begin
  //Using pip programmatically guarantees we're using the same Python interpreter
  //loaded by the application
  var LIn := FCmd.BuildUninstallCmd(FDefs);
  var LPip := Import('pip');
  var LResult := LPip.main(TPyEx.List<String>(LIn));
  if LResult <> 0 then
    raise EPyModuleInstallError.CreateFmt(
      'An error occurred while uninstalling the package %s.', [FDefs.PackageName]);
end;

end.
