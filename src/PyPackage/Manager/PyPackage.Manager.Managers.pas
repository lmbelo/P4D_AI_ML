unit PyPackage.Manager.Managers;

interface

uses
  System.Generics.Collections,
  PyPackage.Manager.ManagerKind,
  PyPackage.Manager.Intf;

type
  TPyPackageManagers = class(TDictionary<TPyPackageManagerKind, IPyPackageManager>)
  end;

implementation

end.
