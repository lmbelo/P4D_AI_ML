unit PyPackage.Manager.Exec.Intf;

interface

uses
  System.SysUtils, PyCore;

type
  IPyPackageManagerCmdExec = interface
    ['{FE9FC748-4317-4B88-9266-0157DADA01A7}']
    function Exec(const AIn: string; const AYield: TProc<string>): TPyExecCmdResultCode;
  end;

implementation

end.
