{$IFDEF MSWINDOWS}
  {$IFDEF WIN32}
  {$R ..\..\..\resources\python3-windows-3.8-win32.res}
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  {$R ..\..\..\resources\python3-windows-3.8-amd64.res}
  {$ENDIF WIN64}
{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
  {$IFDEF LINUX64}
  {$R ..\..\..\resources\python3-linux-3.8-x86_64.res}
  {$ENDIF LINUX64}
{$ENDIF LINUX}

{$IFDEF MACOS}
  {$IFDEF MACOS64}
    {$IFDEF CPU386}
    {$R ..\..\..\resources\python3-macos-3.8-x86_64.res}
    {$ENDIF CPU386}
    {$IFDEF CPUARM}
    {$R ..\..\..\resources\python3-macos-3.8-universal2.res}
    {$ENDIF CPUARM}
  {$ENDIF MACOS64}
{$ENDIF MACOS}

{$IFDEF ANDROID}
  {$IFDEF CPUARM32}
    {$IFDEF ANDROID32}
    {$R ..\..\..\resources\python3-android-3.8-arm.res}
    {$ENDIF ANDROID32}
  {$ENDIF CPUARM32}
  {$IFDEF CPUARM64}
    {$IFDEF ANDROID64}
    {$R ..\..\..\resources\python3-android-3.8-arm64.res}
    {$ENDIF ANDROID64}
  {$ENDIF CPUARM64}
{$ENDIF ANDROID}