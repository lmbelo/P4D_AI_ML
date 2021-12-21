unit ExecProc.Win;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows;

function ExecCmd(
  const ACmd: string;
  const AStdOutPipeReader: TProc<string>;
  const ATerminate: TFunc<boolean>): integer;

const
  cCANCELATION_SIGNAL_EXIT_CODE = $001A;

implementation

type
  TExecCmdWin = class
  private
    FSecurityAttributes: TSecurityAttributes;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
    FStdOutPipeRead: THandle;
    FStdOutPipeWrite: THandle;
    FJob: THandle;
    FStatus: cardinal;
  private
    procedure AsyncReadStdOut(const AStdOutPipeReader: TProc<string>);
  public
    constructor Create(const ACmd: string);
    destructor Destroy(); override;

    function Wait(
      const AStdOutPipeReader: TProc<string>;
      const ATerminate: TFunc<boolean>): integer;

    property Status: cardinal read FStatus;
  end;

function ExecCmd(
  const ACmd: string;
  const AStdOutPipeReader: TProc<string>;
  const ATerminate: TFunc<boolean>): integer;
begin
  var LProc := TExecCmdWin.Create(ACmd);
  try
    Result := LProc.Wait(AStdOutPipeReader, ATerminate);
  finally
    LProc.Free();
  end;
end;

{ TExecCmdWin }

constructor TExecCmdWin.Create(const ACmd: string);
begin
  FStatus := 0;

  with FSecurityAttributes do begin
    nLength := SizeOf(TSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  //Preparing the pipes
  if not CreatePipe(FStdOutPipeRead, FStdOutPipeWrite, @FSecurityAttributes, 0) then
    RaiseLastOSError();

  with FStartupInfo do
  begin
    FillChar(FStartupInfo, SizeOf(TStartupInfo), 0);
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    wShowWindow := SW_SHOW;
    hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    hStdOutput := FStdOutPipeWrite;
    hStdError := FStdOutPipeWrite;
  end;

  //Kill the subprocess when parent dies
  FJob := CreateJobObject(nil, nil);
  if (FJob <> 0) then begin
    var LExInfo: TJobObjectExtendedLimitInformation;
    LExInfo.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if not SetInformationJobObject(FJob, JobObjectExtendedLimitInformation,
      @LExInfo, SizeOf(TJobObjectExtendedLimitInformation)) then
        RaiseLastOSError();
  end;

  //Create the process
  if not CreateProcess(nil, PChar(ACmd), nil, nil, True, 0, nil, nil,
    FStartupInfo, FProcessInfo) then
      RaiseLastOSError();

  CloseHandle(FStdOutPipeWrite);

  //Assign the process to the job. It takes the proc. down when parent is killed.
  AssignProcessToJobObject(FJob, FProcessInfo.hProcess);
end;

destructor TExecCmdWin.Destroy;
begin
  CloseHandle(FProcessInfo.hThread);
  CloseHandle(FProcessInfo.hProcess);
  CloseHandle(FStdOutPipeRead);
  inherited;
end;

procedure TExecCmdWin.AsyncReadStdOut(const AStdOutPipeReader: TProc<string>);
var
  LBuffer: array[0..4095] of AnsiChar;
begin
  TThread.CreateAnonymousThread(procedure()
    begin
      repeat
        var LWasOK: boolean;
        var LBytesRead: cardinal;
        repeat
          LWasOK := ReadFile(FStdOutPipeRead, LBuffer, SizeOf(LBuffer), LBytesRead, nil);
          if (LBytesRead > 0) then
          begin
            LBuffer[LBytesRead] := #0;
            if Assigned(AStdOutPipeReader) then
              AStdOutPipeReader(String(LBuffer));
          end;
          Sleep(100);
        until not LWasOK or (LBytesRead = 0);
      until (FStatus <> WAIT_TIMEOUT);
    end).Start();
end;

function TExecCmdWin.Wait(const AStdOutPipeReader: TProc<string>;
  const ATerminate: TFunc<boolean>): integer;
begin
  //Terminate before starts waiting to the process
  if ATerminate() then
    Exit(cCANCELATION_SIGNAL_EXIT_CODE);

  //This thread keeps calling the bloking readfile method
  FStatus := WaitForSingleObject(FProcessInfo.hProcess, 0);
  if (FStatus = WAIT_TIMEOUT) then begin
    AsyncReadStdOut(AStdOutPipeReader);
    repeat
      if ATerminate() then
        TerminateProcess(FProcessInfo.hProcess, cCANCELATION_SIGNAL_EXIT_CODE);

      FStatus := WaitForSingleObject(FProcessInfo.hProcess, 100);
    until (FStatus <> WAIT_TIMEOUT);
  end;
  //The ReadFile thread is automatically finalized after process finishes
  GetExitCodeProcess(FProcessInfo.hProcess, Cardinal(Result));
end;

end.
