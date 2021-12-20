unit PyPackage.Manager.Exec.SubProcess.Pip;

interface

uses
  System.SysUtils,
  PyCore,
  PyPackage.Manager.Exec.Intf;

type
  TPyPackageManagerCmdExecSubProcessPip = class(TInterfacedObject, IPyPackageManagerCmdExec)
  public
    function Exec(const AIn: string; const AYield: TProc<string>): TPyExecCmdResultCode;
  end;

implementation

uses
  System.Variants, VarPyth;

{ TPyPackageManagerCmdExecPip }

function TPyPackageManagerCmdExecSubProcessPip.Exec(const AIn: string;
  const AYield: TProc<string>): TPyExecCmdResultCode;
begin
  var sp := Import('subprocess');
//  var exec := sp.run(AIn, stdout := sp.PIPE, stderr := sp.STDOUT,
//    shell := true, text := true);
//  var stdout := exec.stdout;
//  if VarIsPythonString(stdout) then begin
//    AOut := VarToStr(stdout);
//  end;

  var LPopen := sp.Popen(AIn, stdout := sp.PIPE, universal_newlines := true,
    shell := true);
  var LStdOut := LPopen.stdout.readline();
  while VarIsPythonString(LStdOut) and (VarToStr(LStdOut) <> String.Empty) do begin
    if Assigned(AYield) then
      AYield(LStdOut);
    LStdOut := LPopen.stdout.readline();
  end;

  LPopen.stdout.close();
  Result := LPopen.wait();
end;

end.
