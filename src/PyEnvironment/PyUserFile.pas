unit PyUserFile;

interface

uses
  System.SysUtils, System.JSON,
  PyEnvironment,
  PyEnvironment.Intf;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                         JSON structure example                        *)
  (*                                                                       *)
  (*                                                                       *)
  (*                                                                       *)
  (*  JSON                                                                 *)
  (*    [{"platform": [                                                    *)
  (*      {"arch": [                                                       *)
  (*          {"python_version":                                           *)
  (*            {"home": "",                                               *)
  (*             "program_name": "",                                       *)
  (*             "shared_library": "",                                     *)
  (*             "executable": ""}}]}]}]                                   *)
  (*-----------------------------------------------------------------------*)
  /// <summary>
  ///   Provide access to the Python environment based on a JSON file.
  ///   Use "any" for global configuration.
  /// </summary>
  TPyUserFile = class(TPyEnvironment)
  private
    FFilePath: string;
  private
    procedure LoadInfo(const AProc: TProc<TJSONObject>);
  protected
    /// <summary>
    ///   Having the FilePath empty, the app path is used as root.
    /// </summary>
    function ResolveFilePath(): string;
  protected
    function GetEnvironmentPath(): string; override;
  public
    function GetHome(): string; override;
    function GetProgramName(): string; override;
    function GetSharedLibrary(): string; override;
    function GetExecutable(): string; override;
  published
    property Platform;
    property Architecture;
    property PythonVersion;
    /// <summary>
    ///   Specifies the JSON file path. Let it empty for default settings.
    /// </summary>
    property FilePath: string read FFilePath write FFilePath;
  end;

  EInvalidFileStructure = class(Exception);

const
  JSON_FILE_NAME = 'pyenv.json';

implementation

uses
  System.IOUtils, System.Classes;

{ TPyUserFile }

procedure TPyUserFile.LoadInfo(const AProc: TProc<TJSONObject>);
var
  LFile: string;
  LPlatforms: TJSONValue;
  LPlatform: TJSONValue;
  LArchitectures: TJSONValue;
  LArchitecture: TJSONValue;
  LPythonVersions: TJSONValue;
  LPythonVersion: TJSONValue;
  LPythonInfo: TJSONValue;

  procedure CheckIsArray(const AValue: TJSONValue);
  begin
    if not (AValue is TJSONArray) then
      raise EInvalidFileStructure.Create('Invalid file structure.');
  end;

begin
  LFile := ResolveFilePath();

  if not TFile.Exists(LFile) then
    raise EFileNotFoundException.CreateFmt('File not found.' + #13#10 + '%s', [LFile]);

  LPlatforms := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFile));
  try
    CheckIsArray(LPlatforms);

    for LPlatform in TJSONArray(LPlatforms) do begin
      LArchitectures := LPlatform.FindValue(GetPlatformName());

      CheckIsArray(LArchitectures);

      for LArchitecture in TJSONArray(LArchitectures) do begin
        LPythonVersions := LArchitecture.FindValue(GetArchitectureName());

        CheckIsArray(LPythonVersions);

        for LPythonVersion in TJSONArray(LPythonVersions) do begin
          LPythonInfo := TJSONObject(LPythonVersion).Values[PythonVersion];

          if not Assigned(LPythonInfo) then
            Continue;

          if not (LPythonInfo is TJSONObject) then
            raise EInvalidFileStructure.Create('Invalid file structure.');

          AProc(TJSONObject(LPythonInfo));
          Exit;
        end;
      end;
    end;
  finally
    LPlatforms.Free();
  end;
end;

function TPyUserFile.ResolveFilePath: string;
begin
  if not FilePath.IsEmpty() then
    Result := FilePath
  else
    Result := TPath.Combine(ExtractFilePath(ParamStr(0)), JSON_FILE_NAME);
end;

function TPyUserFile.GetEnvironmentPath: string;
begin
  Result := GetHome();
end;

function TPyUserFile.GetHome: string;
var
  LResult: string;
begin
  LoadInfo(procedure(AJSONObj: TJSONObject)
    begin
      AJSONObj.TryGetValue<string>('home', LResult);
    end);
  Result := LResult;
end;

function TPyUserFile.GetProgramName: string;
var
  LResult: string;
begin
  LoadInfo(procedure(AJSONObj: TJSONObject)
    begin
      AJSONObj.TryGetValue<string>('program_name', LResult);
    end);
  Result := LResult;
end;

function TPyUserFile.GetSharedLibrary: string;
var
  LResult: string;
begin
  LoadInfo(procedure(AJSONObj: TJSONObject)
    begin
      AJSONObj.TryGetValue<string>('shared_library', LResult);
    end);
  Result := LResult;
end;

function TPyUserFile.GetExecutable: string;
var
  LResult: string;
begin
  LoadInfo(procedure(AJSONObj: TJSONObject)
    begin
      AJSONObj.TryGetValue<string>('executable', LResult);
    end);
  Result := LResult;
end;

end.
