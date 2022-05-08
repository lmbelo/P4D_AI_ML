unit PyExecCmd.Common;

interface

uses
  System.SysUtils;

type
  TPyExecCmdCommon = class
  public
    class function BuildArgv(const AExecutable: string;
      const AInput: TArray<string>): TArray<string>;
    class function BuildEnvp(const AHome, AExecutable,
      ASharedLibrary: string): TArray<string>;
  end;

implementation

{ TPyExecCmdCommon }

class function TPyExecCmdCommon.BuildArgv(const AExecutable: string;
  const AInput: TArray<string>): TArray<string>;
begin
  {$IFDEF MSWINDOWS}
  Result := AInput;
  {$ELSE}
  Result := [AExecutable] + AInput;
  {$ENDIF MSWINDOWS}
end;

class function TPyExecCmdCommon.BuildEnvp(const AHome, AExecutable,
  ASharedLibrary: string): TArray<string>;
begin
  {$IFDEF MSWINDOWS}
  Result := [];
  {$ELSEIF DEFINED(OSX)}
  Result := ['DYLD_LIBRARY_PATH='
           + ExtractFileDir(ASharedLibrary)
           + ':'
           + ExtractFileDir(AExecutable),
             'LD_LIBRARY_PATH=' + ExtractFileDir(ASharedLibrary),
             'PATH=' + ExtractFileDir(AExecutable)];
  {$ELSEIF DEFINED(POSIX)}
  Result := ['LD_LIBRARY_PATH=' + ExtractFileDir(ASharedLibrary),
             'PYTHONHOME=' + AHome,
             'PATH=' + ExtractFileDir(AExecutable)];
  {$ENDIF MSWINDOWS}
end;

end.
