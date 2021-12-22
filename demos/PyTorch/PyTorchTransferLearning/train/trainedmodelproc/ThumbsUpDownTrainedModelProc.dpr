program ThumbsUpDownTrainedModelProc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  LoadModel in 'LoadModel.pas';

const
  BUFFSIZE = 4096;
type
  TBuffArr = array[0..BUFFSIZE - 1] of AnsiChar;

var LStdOut: THandle;
var LStdIn: THandle;

function Read(): string;
begin
  var LBuffer: TBuffArr;
  var LBytesRead: cardinal;
  ReadFile(LStdIn, LBuffer, BUFFSIZE, LBytesRead, nil);
  if (LBytesRead > 0) then begin
    LBuffer[LBytesRead] := #0;
    Result := string(LBuffer);
  end;
end;

procedure Write(const AText: string);
begin
  var LText := AnsiString(AText);
  var LBytesWritten: cardinal;
  WriteFile(LStdOut, LText[1], Length(LText), LBytesWritten, nil);
end;

function ProcessImage(const APath: string): integer;
begin
  Result := 1;
end;

begin
  try
    var LMode := 0; //interactive mode
    var LDebug := false;
    for var I := 0 to ParamCount do begin
      if ParamStr(I).Trim().StartsWith('-m')
        and ParamStr(I).Trim().EndsWith('CHILD_PROC') then begin
          LMode := 1;
      end else if (ParamStr(I).Trim().Equals('DEBUG')) then
        LDebug := true;
    end;

    if LDebug then begin
      {$IFDEF MSWINDOWS}
      //If you want to attach this process to a debuger
      if ParamCount = 2 then begin
        var LTimer := 30;
        while (DebugHook = 0) and (LTimer > 0) do begin
          //You have 30 seconds to attach the debugger
          Sleep(1000);
          Dec(LTimer);
        end;
        if (DebugHook <> 0) then
          DebugBreak();
      end;
      {$ENDIF}
    end;

    if (LMode = 1) then begin
      LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
      LStdIn := GetStdHandle(STD_INPUT_HANDLE);
      if (LStdOut <> INVALID_HANDLE_VALUE) and (LStdIn <> INVALID_HANDLE_VALUE) then begin
        var LCmd := Read();
        while (LCmd = 'RUN') do begin
          Write('WAITING');
          var LFilePath := Read();
          if not TFile.Exists(LFilePath) then begin
            Write('ERROR: File not found.');
          end else begin
            Write(ProcessImage(LFilePath).ToString());
          end;
          LCmd := Read();
        end;
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
