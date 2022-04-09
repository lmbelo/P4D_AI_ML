unit PyEnvironment.AddOnGetPip;

interface

uses
  System.SysUtils, PyEnvironments;

type
  TPyEnvironmentAddOnGetPip = class(TPyEnvironmentCustomAddOn)
  public
    procedure Execute(AAction: TEnvironmentNotification; AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem); override;
  end;

  EPipSetupFailed = class(Exception);

implementation

uses
  System.Classes, System.Types, System.IOUtils, System.Variants,
  VarPyth;

 {$R ..\..\..\resources\getpipscript.res}

{ TPyEnvinonmentAddOnGetPip }

procedure TPyEnvironmentAddOnGetPip.Execute(AAction: TEnvironmentNotification;
  AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem);
var
  LResStream: TResourceStream;
  LFileName: string;
  LPths: TArray<string>;
  LStrings: TStringList;
  I: Integer;
  LSubproc: variant;
  LOut: variant;
begin
  inherited;
  if (AAction <> AFTER_SETUP_NOTIFICATION) then
    Exit;

  LSubproc := Import('subprocess');

  //Identify if PIP is available
  LOut := LSubproc.run(
    VarPythonCreate([
      AEnvironment.Executable, '-m', 'pip', '--version'], stTuple),
    capture_output:=true, text:=true, shell:=true);

  if (LOut.returncode = 0) then
    Exit;

  //Patch the _pth file to work with site packages
  LPths := TDirectory.GetFiles(
    AEnvironment.Home, 'python*._pth', TSearchOption.soTopDirectoryOnly);
  if (Length(LPths) > 0) then begin
    LStrings := TStringList.Create();
    try
      LStrings.LoadFromFile(LPths[0]);
      for I := 0 to LStrings.Count -1 do
        if LStrings[I].Trim().StartsWith('#import site') then
          LStrings[I] := 'import site';
     LStrings.SaveToFile(LPths[0]);
    finally
      LStrings.Free();
    end;
  end;

  //Run the get-pip.py script to enabled PIP
  LFileName := TPath.GetTempFileName();
  LResStream := TResourceStream.Create(HInstance, 'getpippy', RT_RCDATA);
  try
    LResStream.SaveToFile(LFileName);

    LOut := LSubproc.run(
      VarPythonCreate([AEnvironment.Executable, LFileName], stTuple),
      capture_output:=true, text:=true, shell:=true);

    if (LOut.returncode <> 0) then
      raise EPipSetupFailed.Create(
        'Failed to setup PIP. ' + #13#10 + VarToStr(LOut.stderr));
  finally
    LResStream.Free();
  end;
end;

end.
