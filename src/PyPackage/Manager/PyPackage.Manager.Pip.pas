unit PyPackage.Manager.Pip;

interface

uses
  PyCore,
  PyPackage.Manager,
  PyPackage.Manager.Intf,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf;

type
  TPyPackageManagerPip = class(TPyPackageManager, IPyPackageManager)
  private
    FDefs: TPyPackageManagerDefs;
    FCmd: IPyPackageManagerCmdIntf;

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
  System.Variants, System.SysUtils,
  PyPackage.Manager.Defs.Pip, PyPackage.Manager.Cmd.Pip,
  VarPyth;

type
  TCmdExecStrategyClass = class of TCmdExecStrategy;

  TCmdExecStrategy = class abstract
  public
    class procedure Exec(const AIn: string; out AOut: string); virtual; abstract;
  end;

  TCmdExecPySubprocessStrategy = class(TCmdExecStrategy)
  public
    class procedure Exec(const AIn: string; out AOut: string); override;
  end;

{ TPyPackageManagerPip }

constructor TPyPackageManagerPip.Create(const APackageName: TPyPackageName);
begin
  FDefs := TPyPackageManagerDefsPip.Create(APackageName);
  FCmd := TPyPackageManagerCmdPip.Create();
end;

destructor TPyPackageManagerPip.Destroy;
begin
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

procedure TPyPackageManagerPip.Install;
begin
  var LIn := FCmd.BuildInstallCmd(FDefs);
  var LOut: string;
  TCmdExecPySubprocessStrategy.Exec(LIn, LOut);
end;

procedure TPyPackageManagerPip.Uninstall;
begin
  var LIn := FCmd.BuildUninstallCmd(FDefs);
  var LOut: string;
  TCmdExecPySubprocessStrategy.Exec(LIn, LOut);
end;

function TPyPackageManagerPip.IsInstalled: boolean;
begin
  var LIn := FCmd.BuildIsInstalledCmd(FDefs);
  var LOut: string;
  TCmdExecPySubprocessStrategy.Exec(LIn, LOut);
  Result := LOut.Contains(FDefs.PackageName);
end;

{ TCmdExecPySubprocessStrategy }

class procedure TCmdExecPySubprocessStrategy.Exec(const AIn: string;
  out AOut: string);
begin
  var sp := Import('subprocess');
  var exec := sp.run(AIn, stdout := sp.PIPE, stderr := sp.STDOUT,
    shell := true, text := true);
  var stdout := exec.stdout;
  if VarIsPythonString(stdout) then begin
    AOut := VarToStr(stdout);
  end;
end;

end.
