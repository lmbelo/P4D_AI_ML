unit PyEnvironment.Transient;

interface

uses
  System.Classes, System.SysUtils, System.JSON, PyEnvironments;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                         JSON structure example                        *)
  (*                                                                       *)
  (* JSON                                                                  *)
  (*    [{"python_version":                                                *)
  (*         {"home": "",                                                  *)
  (*          "program_name": "",                                          *)
  (*          "shared_library": "",                                        *)
  (*          "executable": ""}}]                                          *)
  (*-----------------------------------------------------------------------*)
  TPyTransientItem = class(TPyEnvironmentItem);

  TPyTransientCollection = class(TPyEnvironmentCollection);

  [ComponentPlatforms(pidAllPlatforms)]
  TPyTransientEnvironment = class(TPyEnvironment)
  private
    FFilePath: string;
    procedure EnumerateEnvironments(const AProc: TProc<string, TJSONObject>);
  protected
    function CreateCollection(): TPyEnvironmentCollection; override;
    procedure Prepare(); override;
  published
    property FilePath: string read FFilePath write FFilePath;
  end;

implementation

uses
  System.IOUtils, PythonEngine;

{ TPyTransientEnvironment }

function TPyTransientEnvironment.CreateCollection: TPyEnvironmentCollection;
begin
  Result := TPyTransientCollection.Create(Self, TPyTransientItem);
end;

procedure TPyTransientEnvironment.EnumerateEnvironments(const AProc: TProc<string, TJSONObject>);
var
  LPythonVersions: TJSONValue;
  I: Integer;
  LPythonVersion: TJSONValue;
  LEnviromentInfo: TJSONValue;
begin
  if not TFile.Exists(FFilePath) then
    raise EFileNotFoundException.CreateFmt('File not found.' + #13#10 + '%s', [FFilePath]);

  LPythonVersions := TJSONObject.ParseJSONValue(TFile.ReadAllText(FFilePath));
  try
    if not (Assigned(LPythonVersions) and (LPythonVersions is TJSONArray)) then
      raise EInvalidFileStructure.Create('Invalid file structure.');

    for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
      for LPythonVersion in TJSONArray(LPythonVersions) do begin
        if not (LPythonVersion is TJSONObject) then
          raise EInvalidFileStructure.Create('Invalid file structure.');

        LEnviromentInfo := TJSONObject(LPythonVersion).Values[PYTHON_KNOWN_VERSIONS[I].RegVersion];

        if not Assigned(LEnviromentInfo) then
          Continue;

        if not (LEnviromentInfo is TJSONObject) then
          raise EInvalidFileStructure.Create('Invalid file structure.');

        AProc(PYTHON_KNOWN_VERSIONS[I].RegVersion, TJSONObject(LEnviromentInfo));
      end;
    end;
  finally
    LPythonVersions.Free();
  end;
end;

procedure TPyTransientEnvironment.Prepare;
begin
  if not TFile.Exists(FFilePath) then
    raise Exception.Create('File not found.');

  EnumerateEnvironments(
    procedure(APythonVersion: string; AEnvironmentInfo: TJSONObject)
    var
      LItem: TPyTransientItem;
    begin
      LItem := TPyTransientItem(Environments.Add());
      LItem.PythonVersion := APythonVersion;
      LItem.Home := AEnvironmentInfo.GetValue<string>('home');
      LItem.ProgramName := AEnvironmentInfo.GetValue<string>('program_name');
      LItem.SharedLibrary := AEnvironmentInfo.GetValue<string>('shared_library');
      LItem.Executable := AEnvironmentInfo.GetValue<string>('executable');
    end);

  inherited;
end;

end.
