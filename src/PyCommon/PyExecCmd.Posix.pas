unit PyExecCmd.Posix;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.Base, Posix.Fcntl, Posix.Unistd, Posix.SysWait, Posix.Stdlib, Posix.SysTypes, Posix.Signal, Posix.Errno, PyExecCmd;

type
  TStreamHandle = pointer;

  TExecCmdPosix = class(TInterfacedObject, IExecCmd)
  private  
    FCmd: string;
    FArgs: string;
    FPid: Integer;
    FParent: TPipeDescriptors;
    FChild: TPipeDescriptors;
    procedure Redirect(out AReader: TReader; out AWriter: TWriter);
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
  EInvalidArgument = class(EExecCmd);
  EOperationNotPermitted = class(EExecCmd);
  ENoSuchProcess = class(EExecCmd);

implementation

uses
  System.SysUtils;

///  <summary>
///    Utility function to return a buffer of ASCII-Z data as a string.
///  </summary>
function BufferToString(ABuffer: Pointer; AMaxSize: UInt32): string;
var
  LCursor: ^UInt8;
  LEndOfBuffer: NativeUInt;
begin
  Result := '';
  if not Assigned(ABuffer) then
    Exit;
    
  LCursor := ABuffer;
  LEndOfBuffer := NativeUint(LCursor) + AMaxSize;
  while (NativeUint(LCursor) < LEndOfBuffer) and (LCursor^ <> 0) do begin
    Result := Result + Chr(LCursor^);
    LCursor := Pointer(Succ(NativeUInt(LCursor)));
  end;
end;

{ TExecCmdPosix }

constructor TExecCmdPosix.Create(const ACmd, AArgs: string);
begin
  FCmd := ACmd;
  FArgs := AArgs;
end;

destructor TExecCmdPosix.Destroy;
begin
  if IsAlive then
    Kill();
  inherited;
end;

function TExecCmdPosix.GetExitCode: Integer;
begin
  Result := Wait();
end;

function TExecCmdPosix.GetIsAlive: boolean;
begin             
  Result := (getpgid(FPid) >= 0);       
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

procedure TExecCmdPosix.Redirect(out AReader: TReader; out AWriter: TWriter);
var
  LBuffer: array[0..511] of UInt8;
begin
  AReader := function(): string 
  var
    LCount: Int64;
  begin
    LCount := __read(FParent.ReadDes, @LBuffer[0], SizeOf(LBuffer));     
    Result := BufferToString(@LBuffer[0], LCount);  
  end;

  AWriter := procedure(AIn: string) begin
    //__write()
  end;
end;

function TExecCmdPosix.Run(out AOutput: string): IExecCmd;
var
  LReader: TReader;
  LWriter: TWriter;
begin
  Result := Run(LReader, LWriter, []);

  AOutput := LReader();
  while GetIsAlive() do
    AOutput := AOutput + LReader();
end;

function TExecCmdPosix.Run(out AReader: TReader; out AWriter: TWriter;
  const ARedirections: TRedirections): IExecCmd;
begin
  pipe(FParent);
  
  FPid := fork();
  if (FPid < 0) then
    raise EForkFailed.Create('Failed to fork process.')
  else if (FPid = 0) then begin
    dup2(FChild.ReadDes, STDIN_FILENO);
    dup2(FChild.WriteDes, STDOUT_FILENO);

    __close(FChild.ReadDes);
    __close(FChild.WriteDes);
    __close(FParent.ReadDes);
    __close(FParent.WriteDes);

    execv(PAnsiChar(AnsiString(FCmd)), PPAnsiChar(AnsiString(FArgs)));    
  end else if (FPid > 0) then begin
    __close(FChild.ReadDes);
    __close(FChild.WriteDes);
  end;

  Redirect(AReader, AWriter);
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
  LWaitedPid := waitpid(FPid, @LStatus, 0);
  if (LWaitedPid < 0) then
    Exit(EXIT_FAILURE)
  else if (LWaitedPid = FPid) then
    if WIFEXITED(LStatus) then
      Exit(WEXITSTATUS(LStatus));

  Result := EXIT_SUCCESS;
end;

end.
