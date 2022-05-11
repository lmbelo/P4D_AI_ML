{$IFDEF MSWINDOWS}
  {$IFDEF WIN32}
  {$R ..\..\..\resources\python3-windows-3.10-win32.res}
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  {$R ..\..\..\resources\python3-windows-3.10-amd64.res}
  {$ENDIF WIN64}
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
  {$IFDEF LINUX64}
  {$R ..\..\..\resources\python3-linux-3.10-x86_64.res}
  {$ENDIF LINUX64}
{$ENDIF LINUX}

{$IFDEF MACOS}
  {$IFDEF MACOS64}
    {$IFDEF CPU386}
    {$R ..\..\..\resources\python3-macos-3.10-x86_64.res}
    {$ENDIF CPU386}
    {$IFDEF CPUARM}
    {$R ..\..\..\resources\python3-macos-3.10-universal2.res}
    {$ENDIF CPUARM}
  {$ENDIF MACOS64}
{$ENDIF MACOS}

{$IFDEF ANDROID}
  {$IFDEF CPUARM32}
    {$IFDEF ANDROID32}
    {$R ..\..\..\resources\python3-android-3.10-arm.res}
    {$ENDIF ANDROID32}
  {$ENDIF CPUARM32}
  {$IFDEF CPUARM64}
    {$IFDEF ANDROID64}
    {$R ..\..\..\resources\python3-android-3.10-arm64.res}
    {$ENDIF ANDROID64}
  {$ENDIF CPUARM64}
{$ENDIF ANDROID}