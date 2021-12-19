unit PyPackage.Manager.Defs;

interface

uses
  System.Classes, PyCore;

type
  TPyPackageManagerDefs = class(TPersistent)
  private
    FPackageName: TPyPackageName;
    FPackageVersion: string;
  public
     /// <param name="APackageName">
     ///   The package name on the repository
     /// </param>
    constructor Create(const APackageName: TPyPackageName); virtual;
  published
    //Package name may differ between PIP and CONDA
    property PackageName: TPyPackageName read FPackageName;
    property PackageVersion: string read FPackageVersion write FPackageVersion;
  end;

implementation

{ TPyPackageManagerDefs }

constructor TPyPackageManagerDefs.Create(const APackageName: TPyPackageName);
begin
  FPackageName := APackageName;
end;

end.
