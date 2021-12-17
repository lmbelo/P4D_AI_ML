unit PyPackage.Manager.Managers;

interface

uses
  System.Generics.Collections, PyPackage.Manager.Manager, PyPackage.Manager.Intf;

type
  TPyPackageManagers = class(TDictionary<TPyPackageManagerType, IPyPackageManager>)
  public
    class function CreateManagers(): TPyPackageManagers; static;
  end;

implementation

uses
  PyPackage.Manager.Pip,
  PyPackage.Manager.Conda;

{ TManagers }

class function TPyPackageManagers.CreateManagers: TPyPackageManagers;
begin
  Result := TPyPackageManagers.Create();
  Result.Add(TPyPackageManagerType.pip, TPyPackageManagerPip.Create());
  Result.Add(TPyPackageManagerType.conda, TPyPackageManagerConda.Create());
end;

end.
