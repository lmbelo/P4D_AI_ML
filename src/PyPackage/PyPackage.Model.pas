unit PyPackage.Model;

interface

uses
  PyCore,
  PyPackage.Manager.Managers;

type
  TPyPackageModel = class
  private
    FPackageName: TPyPackageName;
    FPackageManagers: TPyPackageManagers;
  public
    constructor Create();
    destructor Destroy(); override;

    property PackageName: TPyPackageName read FPackageName write FPackageName;
    property PackageManagers: TPyPackageManagers read FPackageManagers;
  end;

implementation

{ TPyPackageModel }

constructor TPyPackageModel.Create;
begin
  inherited;
  FPackageManagers := TPyPackageManagers.Create()
end;

destructor TPyPackageModel.Destroy;
begin
  FPackageManagers.Free();
  inherited;
end;

end.
