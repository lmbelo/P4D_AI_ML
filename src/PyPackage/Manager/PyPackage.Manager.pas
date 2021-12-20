unit PyPackage.Manager;

interface

uses
  PyCore, PyPackage;

type
  TPyPackageManager = class abstract(TInterfacedObject)
  private
    FPackageName: TPyPackageName;
  public
    constructor Create(const APackageName: TPyPackageName); virtual;

    property PackageName: TPyPackageName read FPackageName;
  end;

implementation

{ TPyPackageManager }

constructor TPyPackageManager.Create(const APackageName: TPyPackageName);
begin
  FPackageName := APackageName;
end;

end.
