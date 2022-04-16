unit PyExecCmd.Posix;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.Base, Posix.Fcntl, Posix.Unistd, Posix.SysWait, Posix.Stdlib,
  Posix.Stdio, Posix.SysTypes, Posix.Signal, Posix.Errno, Posix.SysStat,
  Posix.String_,
  PyExecCmd;

type
  TStreamHandle = pointer;

  TExecCmdPosix = class(TInterfacedObject, IExecCmd)
  private  
    FCmd: string;
    FArgs: string;
    FPid: Integer;
    FRead: TPipeDescriptors;
    FWrite: TPipeDescriptors;
    FExitCode: integer;
    procedure Redirect(out AReader: TReader; out AWriter: TWriter);
    function PeekMessage(): string;
  protected
    function GetIsAlive: boolean;
    function GetExitCode: Integer;
  public
    constructor Create(const ACmd, AArgs: string);
    destructor Destroy(); override;

    function Run(): IExecCmd; overload;
    function Run(out AOutput: string): IExecCmd; overload;
    function Run(out AReader: TReader; out AWriter: TWriter; const ARedirections: TRedirections): IExecCmd; overload;
    procedure Kill();
    function Wait(): Integer;

    property IsAlive: boolean read GetIsAlive;
    property ExitCode: Integer read GetExitCode;
  end;

  EForkFailed = class(EExecCmd);
  EPipeFailed = class(EExecCmd);
  EInvalidArgument = class(EExecCmd);
  EOperationNotPermitted = class(EExecCmd);
  ENoSuchProcess = class(EExecCmd);
  EWaitFailed = class(EExecCmd);

implementation

uses
  System.SysUtils, System.IOUtils;

const
  INITIAL_EXIT_CODE = -999;

{ TExecCmdPosix }

constructor TExecCmdPosix.Create(const ACmd, AArgs: string);
begin
  FCmd := ACmd;
  FArgs := AArgs;
  FExitCode := INITIAL_EXIT_CODE;
end;

destructor TExecCmdPosix.Destroy;
begin
  if IsAlive then
    Kill();

  __close(FRead.ReadDes);
  __close(FWrite.WriteDes);     
  inherited;
end;

function TExecCmdPosix.GetExitCode: Integer;
begin
  Result := Wait();
end;

function TExecCmdPosix.GetIsAlive: boolean;
var
  LWaitedPid: integer;
  LStatus: integer;
begin
  if (FExitCode <> INITIAL_EXIT_CODE) then
    Exit(false);
    
  LWaitedPid := waitpid(FPid, @LStatus, WNOHANG);
  if (LWaitedPid = -1) then
    raise EWaitFailed.Create('Failed waiting for proess.')
  else if (LWaitedPid = 0) then
    Exit(true)
  else begin    
    if WIFEXITED(LStatus) then begin
      FExitCode := WEXITSTATUS(LStatus);      
    end else begin
      FExitCode := EXIT_FAILURE;
    end;
  end;
  Result := false;
end;

procedure TExecCmdPosix.Kill;
begin
  if (Posix.Signal.kill(FPid, Posix.Signal.SIGKILL) <> 0) then
    if (errno = EINVAL) then //Invalid signal
      raise EInvalidArgument.Create('Invalid argument.')
    else if (errno = EPERM) then //The process does not have permission to send the signal to any of the target processes.
      raise EOperationNotPermitted.Create('Operation not permitted.') 
    else if (errno = ESRCH) then //The pid or process group does not exist. Note that an existing process might be a zombie, a process which already committed termination, but has not yet been wait(2)ed for.
      raise ENoSuchProcess.Create('No such process.') 
end;

function TExecCmdPosix.PeekMessage: string;
var
  LBuffer: array[0..511] of UInt8;
  LCount: integer;
begin   
  while True do begin
    LCount := __read(FRead.ReadDes, @LBuffer[0], SizeOf(LBuffer));
    if (LCount = -1) then begin     
      if (errno = EINTR) then
        Continue
      else
        Exit(String.Empty);
    end else if (LCount = 0) then
      Exit(String.Empty)
    else begin
      Exit(Copy(UTF8ToString(@LBuffer[0]), 1, UTF8ToString(@LBuffer[0]).Length -1));
    end;      
  end;
end;

procedure TExecCmdPosix.Redirect(out AReader: PyExecCmd.TReader; out AWriter: PyExecCmd.TWriter);
var
  LBuffer: string;
  M: TMarshaller;
begin
  AReader := function(): string
  begin
    Result := String.Empty;

    while GetIsAlive() and Result.IsEmpty() do begin
      Result := Result + PeekMessage();
    end;

    if not Result.IsEmpty() then
      Exit;

    //Preventing race condition...
    repeat
      LBuffer := PeekMessage();
      if not LBuffer.IsEmpty() then
        Result := Result + LBuffer;
    until (LBuffer.IsEmpty());
  end;

  AWriter := procedure(AIn: string) begin
    __write(FWrite.WriteDes, M.AsUtf8(PWideChar(AIn)).ToPointer(), AIn.Length);
  end;
end;

function TExecCmdPosix.Run(out AOutput: string): IExecCmd;
var
  LReader: TReader;
  LWriter: TWriter;
  LOutput: string;
begin
  AOutput := String.Empty;
  Result := Run(LReader, LWriter, [TRedirect.stdout]);
  repeat
    LOutput := LReader();
    if not LOutput.IsEmpty() then
      AOutput := AOutput + LOutput;
  until LOutput.IsEmpty();
end;

function TExecCmdPosix.Run(out AReader: PyExecCmd.TReader; out AWriter: PyExecCmd.TWriter;
  const ARedirections: TRedirections): IExecCmd;
var
  M: TMarshaller;
  LArgs: array of PAnsiChar;
begin
  //#define PARENT_READ read_pipe[0]
  //#define PARENT_WRITE write_pipe[1]
  //#define CHILD_WRITE read_pipe[1]
  //#define CHILD_READ  write_pipe[0]
  
  if (pipe(FRead) = -1) or (pipe(FWrite) = -1) then
    raise EPipeFailed.Create('Failed to create pipe.');

  FPid := fork();
  if (FPid < 0) then
    raise EForkFailed.Create('Failed to fork process.')
  else if (FPid = 0) then begin
    while ((dup2(FRead.WriteDes, STDOUT_FILENO) = -1) and (errno = EINTR)) do begin end;
    while ((dup2(FRead.WriteDes, STDERR_FILENO) = -1) and (errno = EINTR)) do begin end;
    while ((dup2(FWrite.ReadDes, STDIN_FILENO) = -1) and (errno = EINTR)) do begin end;
    __close(FRead.WriteDes); 
    __close(FRead.ReadDes);
    __close(FWrite.ReadDes);

    SetLength(LArgs, 2);
    LArgs[0] := M.AsAnsi(PWideChar(FArgs)).ToPointer();
    LArgs[1] := PAnsiChar(nil);
    if execvp(M.AsAnsi(PWideChar(FCmd)).ToPointer(), PPAnsiChar(LArgs)) = -1 then begin 
      //Halt(errno);
    end else
      //Halt(EXIT_FAILURE);
  end else if (FPid > 0) then begin    
    __close(FRead.WriteDes);
    __close(FWrite.ReadDes); 
    Redirect(AReader, AWriter);
  end;      
  Result := Self;
end;

function TExecCmdPosix.Run: IExecCmd;
var
  LReader: TReader;
  LWriter: TWriter;
begin
  Run(LReader, LWriter, []);
end;

function TExecCmdPosix.Wait: Integer;
var
  LWaitedPid: integer;
  LStatus: integer;
begin
  if (FExitCode <> INITIAL_EXIT_CODE) then
    Exit(FExitCode);
    
  LWaitedPid := waitpid(FPid, @LStatus, WNOHANG);
  repeat
    if (LWaitedPid = -1) then
      raise EWaitFailed.Create('Failed waiting for process.')
    else if (LWaitedPid = 0) then
      Sleep(100)
    else begin
      if WIFEXITED(LStatus) then
        FExitCode := WEXITSTATUS(LStatus)
      else
        FExitCode := EXIT_FAILURE;
      Exit(FExitCode);
    end;
  until (LWaitedPid <> 0);
  
  Result := FExitCode;
end;

end.
