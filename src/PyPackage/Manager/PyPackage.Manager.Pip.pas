unit PyPackage.Manager.Pip;

interface

uses
  PyCore, PyPackage,
  PyPackage.Manager,
  PyPackage.Manager.Intf,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf,
  PyPackage.Manager.Exec.Intf;

type
  TPyPackageManagerPip = class(TPyPackageManager, IPyPackageManager)
  private
    FDefs: TPyPackageManagerDefs;
    FCmd: IPyPackageManagerCmdIntf;
    FExec: IPyPackageManagerCmdExec;

    //IPyPackageManager implementation
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
    function GetExec(): IPyPackageManagerCmdExec;
    procedure Install();
    procedure Uninstall();
    function IsInstalled(): boolean;
  public
    constructor Create(const APackageName: TPyPackageName); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Variants, System.SysUtils,
  PyPackage.Manager.Defs.Pip,
  PyPackage.Manager.Cmd.Pip,
  PyPackage.Manager.Exec.SubProcess.Pip;

{ TPyPackageManagerPip }

constructor TPyPackageManagerPip.Create(const APackageName: TPyPackageName);
begin
  inherited;
  FDefs := TPyPackageManagerDefsPip.Create(APackageName);
  FCmd := TPyPackageManagerCmdPip.Create();
  FExec := TPyPackageManagerCmdExecSubProcessPip.Create();
end;

destructor TPyPackageManagerPip.Destroy;
begin
  FExec := nil;
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

function TPyPackageManagerPip.GetExec: IPyPackageManagerCmdExec;
begin
  Result := FExec;
end;

procedure TPyPackageManagerPip.Install;
begin
  var LIn := FCmd.BuildInstallCmd(FDefs);
  FExec.Exec(LIn, procedure(AOut: string) begin
  end);
end;

procedure TPyPackageManagerPip.Uninstall;
begin
  var LIn := FCmd.BuildUninstallCmd(FDefs);
  FExec.Exec(LIn, procedure(AOut: string) begin
  end);
end;

function TPyPackageManagerPip.IsInstalled: boolean;
begin
  var LIn := FCmd.BuildIsInstalledCmd(FDefs);
  var LOut: string;
  FExec.Exec(LIn, procedure(AOut: string) begin
    LOut := LOut + AOut;
  end);
  Result := LOut.Contains(FDefs.PackageName);
end;

end.
