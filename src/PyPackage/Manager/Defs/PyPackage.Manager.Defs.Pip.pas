unit PyPackage.Manager.Defs.Pip;

interface

uses
  System.Classes,
  PyCore,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Defs.InstallOpts.Pip,
  PyPackage.Manager.Defs.UninstallOpts.Pip;

type
  TPyPackageManagerDefsPip = class(TPyPackageManagerDefs)
  private
    FInstallOptions: TPyPackageManagerDefsInstallOptsPip;
    FUninstallOptions: TPyPackageManagerDefsUninstallOptsPip;
    procedure SetInstallOptions(const AOpts: TPyPackageManagerDefsInstallOptsPip);
    procedure SetUninstallOptions(const AOpts: TPyPackageManagerDefsUninstallOptsPip);
  public
    constructor Create(const APackageName: TPyPackageName); override;
    destructor Destroy(); override;
  published
    property InstallOptions: TPyPackageManagerDefsInstallOptsPip read FInstallOptions write SetInstallOptions;
    property UninstallOptions: TPyPackageManagerDefsUninstallOptsPip read FUninstallOptions write SetUninstallOptions;
  end;

implementation

{ TPyPackageManagerDefsPip }

constructor TPyPackageManagerDefsPip.Create(const APackageName: TPyPackageName);
begin
  inherited;
  FInstallOptions := TPyPackageManagerDefsInstallOptsPip.Create();
  FUninstallOptions := TPyPackageManagerDefsUninstallOptsPip.Create();
end;

destructor TPyPackageManagerDefsPip.Destroy;
begin
  FUninstallOptions.Free();
  FInstallOptions.Free();
  inherited;
end;

procedure TPyPackageManagerDefsPip.SetInstallOptions(
  const AOpts: TPyPackageManagerDefsInstallOptsPip);
begin
  FInstallOptions.Assign(AOpts);
end;

procedure TPyPackageManagerDefsPip.SetUninstallOptions(
  const AOpts: TPyPackageManagerDefsUninstallOptsPip);
begin
  FUninstallOptions.Assign(AOpts);
end;

end.
