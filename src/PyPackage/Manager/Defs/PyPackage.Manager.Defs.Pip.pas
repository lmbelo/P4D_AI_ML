unit PyPackage.Manager.Defs.Pip;

interface

uses
  System.Classes,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Defs.InstallOpts.Pip,
  PyPackage.Manager.Defs.UninstallOpts.Pip;

type
  TPyPackageManagerDefsPip = class(TPyPackageManagerDefs)
  private
    FPackageName: string;
    FPackageVersion: string;
    FInstallOptions: TPyPackageManagerDefsInstallOptsPip;
    FUninstallOptions: TPyPackageManagerDefsUninstallOptsPip;
    procedure SetInstallOptions(const AOpts: TPyPackageManagerDefsInstallOptsPip);
    procedure SetUninstallOptions(const AOpts: TPyPackageManagerDefsUninstallOptsPip);
  public
    constructor Create();
    destructor Destroy(); override;
  published
    property PackageName: string read FPackageName write FPackageName;
    property PackageVersion: string read FPackageVersion write FPackageVersion;
    property InstallOptions: TPyPackageManagerDefsInstallOptsPip read FInstallOptions write SetInstallOptions;
    property UninstallOptions: TPyPackageManagerDefsUninstallOptsPip read FUninstallOptions write SetUninstallOptions;
  end;

implementation

{ TPyPackageManagerDefsPip }

constructor TPyPackageManagerDefsPip.Create;
begin
  FInstallOptions := TPyPackageManagerDefsInstallOptsPip.Create();
  FUninstallOptions := TPyPackageManagerDefsUninstallOptsPip.Create();
  inherited Create();
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
