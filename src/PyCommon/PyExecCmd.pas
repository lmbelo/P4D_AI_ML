unit PyExecCmd;

interface

uses
  System.SysUtils;

type
  TRedirect = (stdin, stdout);
  TRedirections = set of TRedirect;
  TReader = TFunc<string>;
  TWriter = TProc<string>;

  IExecCmd = interface
    ['{FDCA9BAA-D412-4B48-96C2-0F08057FD6ED}']
    function GetExitCode: Integer;
    function GetIsAlive: boolean;

    function Run(): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(out AReader: TReader; out AWriter: TWriter; const ARedirections: TRedirections): IExecCmd; overload;
    procedure Kill();
    function Wait(): Integer;

    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

  TPyExecCmdService = class
  public
    class function Cmd(const ACmd: string; const AArg, AEnv: TArray<string>): IExecCmd; overload;
    class function Cmd(const ACmd: string; const AArg: TArray<string>): IExecCmd; overload;
  end;

  EExecCmd = class(Exception);

const
  EXIT_SUCCESS  = 0;
  EXIT_FAILURE = 1;

implementation

uses
  {$IFDEF MSWINDOWS}
  PyExecCmd.Win;
  {$ELSE}
  PyExecCmd.Posix;
  {$ENDIF}

{ TExecCmdService }

class function TPyExecCmdService.Cmd(const ACmd: string; const AArg, AEnv: TArray<string>): IExecCmd;
begin
  {$IFDEF MSWINDOWS}
  Result := TExecCmdWin.Create(ACmd, AArg, AEnv);
  {$ELSE}
  Result := TExecCmdPosix.Create(ACmd, AArg, AEnv);
  {$ENDIF}
end;

class function TPyExecCmdService.Cmd(const ACmd: string;
  const AArg: TArray<string>): IExecCmd;
begin
  Result := Cmd(ACmd, AArg, []);
end;

end.
