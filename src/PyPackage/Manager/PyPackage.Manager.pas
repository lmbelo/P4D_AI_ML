unit PyPackage.Manager;

interface

uses
  PyCore;

type
  TPyPackageManager = class abstract(TInterfacedObject)
  public
    constructor Create(const APackageName: TPyPackageName); virtual; abstract;
  end;

implementation

end.
