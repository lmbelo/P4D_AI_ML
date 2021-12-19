unit PyPackage.Manager.Conda;

interface

uses
  PyCore,
  PyPackage.Manager,
  PyPackage.Manager.Intf,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf;

type
  TPyPackageManagerConda = class(TPyPackageManager, IPyPackageManager)
  private
    //IPyPackageManager implementation
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
    procedure Install();
    procedure Uninstall();
    function IsInstalled(): boolean;
  public
    constructor Create(const APackageName: TPyPackageName); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TPyPackageManagerConda }

constructor TPyPackageManagerConda.Create(const APackageName: TPyPackageName);
begin
  inherited;
end;

destructor TPyPackageManagerConda.Destroy;
begin
  inherited;
end;

function TPyPackageManagerConda.GetCmd: IPyPackageManagerCmdIntf;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TPyPackageManagerConda.GetDefs: TPyPackageManagerDefs;
begin
  raise ENotImplemented.Create('Not implemented');
end;

procedure TPyPackageManagerConda.Install;
begin

end;

function TPyPackageManagerConda.IsInstalled: boolean;
begin
  Result := false;
end;

procedure TPyPackageManagerConda.Uninstall;
begin

end;

end.
