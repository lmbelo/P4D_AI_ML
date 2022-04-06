unit PyEnvironments;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  PythonEngine;

type
  TPyEnvironmentItem = class abstract(TCollectionItem)
  private
    FPythonVersion: string;
    FHome: string;
    FProgramName: string;
    FSharedLibrary: string;
    FExecutable: string;
  public
    procedure Setup(); virtual; abstract;
  published
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property Home: string read FHome write FHome;
    property ProgramName: string read FProgramName write FProgramName;
    property SharedLibrary: string read FSharedLibrary write FSharedLibrary;
    property Executable: string read FExecutable write FExecutable;
  end;

  TPyEnvironmentCollection = class abstract(TOwnedCollection)
  public
    function LocateEnvironment(APythonVersion: string): TPyEnvironmentItem; virtual;
  end;

  TPyEnvironment = class(TComponent)
  private
    FEnvironments: TPyEnvironmentCollection;
    FAutoLoad: boolean;
    FPythonEngine: TPythonEngine;
    procedure SetEnvironments(const Value: TPyEnvironmentCollection);
  protected 
    procedure Loaded(); override;
  protected
    function CreateCollection(): TPyEnvironmentCollection; virtual; abstract;
    procedure Prepare(); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    
    procedure Activate(APythonVersion: string);
  published
    property Environments: TPyEnvironmentCollection read FEnvironments write SetEnvironments;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonEngine: TPythonEngine read FPythonEngine write FPythonEngine;
  end;



  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                      Embeddables structure example                    *)
  (*                                                                       *)
  (* [Root] Directory                                                      *)
  (*  +-- python version/                                                  *)
  (*       +-- python zip                                                  *)
  (*-----------------------------------------------------------------------*)
  TPyEmbeddableBaseItem = class(TPyEnvironmentItem)
  private
    FEnvironmentPath: string;
  published
    property EnvironmentPath: string read FEnvironmentPath write FEnvironmentPath;
  end;

  TPyEmbeddableItem = class(TPyEmbeddableBaseItem)
  private
    FEmbeddablePackage: string;
    function FindSharedLibrary(): string;
    function FindExecutable(): string; 
  protected
    function EnvironmentExists(): boolean;
    /// <summary>
    ///   Navigates through the embeddables searching for a compatible distribution.
    /// </summary>
    function EmbeddableExists(): boolean;
    /// <summary>
    ///   Creates a new environment based on the current settings.
    ///   An embeddable distribution will be used as an "image".
    /// </summary>
    procedure CreateEnvironment(); virtual;
    procedure LoadSettings();
  protected
    function GetEnvironmentPath(): string; 
  public
    procedure Setup(); override;
  published
    property EmbeddablePackage: string read FEmbeddablePackage write FEmbeddablePackage;
  end;

  TPyEmbeddableResourceItem = class(TPyEmbeddableBaseItem);

  TPyEmbeddableCollection = class(TPyEnvironmentCollection);

  TPyEmbeddedEnvironment = class(TPyEnvironment)
  private
    FDirectory: string;
    FScan: boolean;
    FEnvironmentPath: string;
  protected
    function CreateCollection(): TPyEnvironmentCollection; override;
    procedure Prepare(); override;
  published
    property Directory: string read FDirectory write FDirectory;
    property Scan: boolean read FScan write FScan default false;
    /// <summary>
    ///   Default environment path.
    /// </summary>
    property EnvironmentPath: string read FEnvironmentPath write FEnvironmentPath;
  end;

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
  TPyTransientItem = class(TPyEnvironmentItem)
  public
    procedure Setup(); override;
  end;

  TPyTransientCollection = class(TPyEnvironmentCollection);

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

  EEmbeddableNotFound = class(Exception);
  EInvalidFileStructure = class(Exception);

implementation

uses
  System.Character, System.IOUtils, System.Zip;

{ TPyEnvironmentCollection }

function TPyEnvironmentCollection.LocateEnvironment(
  APythonVersion: string): TPyEnvironmentItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    if (TPyEnvironmentItem(Items[I]).PythonVersion = APythonVersion) then
      Exit(TPyEnvironmentItem(Items[I]));
  end;
  Result := nil;
end;

{ TPyEnvironment }

procedure TPyEnvironment.Activate(APythonVersion: string);
var
  LItem: TPyEnvironmentItem;
begin
  if not Assigned(FPythonEngine) then
    Exit();

  LItem := FEnvironments.LocateEnvironment(APythonVersion);
  if not Assigned(LItem) then
    Exit();

  FPythonEngine.UnloadDll();
  FPythonEngine.UseLastKnownVersion := false;
  FPythonEngine.PythonHome := LItem.Home;
  FPythonEngine.ProgramName := LItem.ProgramName;
  FPythonEngine.DllPath := ExtractFilePath(LItem.SharedLibrary);
  FPythonEngine.DllName := ExtractFileName(LItem.SharedLibrary);
  FPythonEngine.LoadDll();

  FPythonEngine.ExecString('import sys');
  FPythonEngine.ExecString(AnsiString(Format('sys.executable = r"%s"', [LItem.Executable])));
end;

constructor TPyEnvironment.Create(AOwner: TComponent);
begin
  FEnvironments := CreateCollection();
  inherited;  
end;

destructor TPyEnvironment.Destroy;
begin
  FEnvironments.Free();
  inherited;
end;

procedure TPyEnvironment.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then  
    Prepare();
end;

procedure TPyEnvironment.SetEnvironments(
  const Value: TPyEnvironmentCollection);
begin
  FEnvironments.Assign(Value);
end;

{ TPyEmbeddableItem }

procedure TPyEmbeddableItem.CreateEnvironment;
begin
  //Unzip the embeddable package into the target directory.
  TZipFile.ExtractZipFile(FEmbeddablePackage, GetEnvironmentPath());
end;

function TPyEmbeddableItem.EmbeddableExists: boolean;
begin
  Result := TFile.Exists(FEmbeddablePackage);
end;

function TPyEmbeddableItem.EnvironmentExists: boolean;
begin
  Result := TDirectory.Exists(GetEnvironmentPath());
end;

function TPyEmbeddableItem.FindExecutable: string;
begin
  {$IFDEF MSWINDOWS}
  Result := TPath.Combine(GetEnvironmentPath(), 'python.exe');
  if not TFile.Exists(Result) then
    Result := String.Empty;
  {$ELSE}
    Result := String.Empty;
  {$IFEND}
end;

function TPyEmbeddableItem.FindSharedLibrary: string;
var
  LVerNum: string;
  I: integer;
  LLibName: string;
begin
  { TODO : (BETA) Improve localizer }
  LVerNum := String.Empty;
  for I := Low(PythonVersion) to High(PythonVersion) do
    if Char.IsDigit(PythonVersion, I) then
      LVerNum := LVerNum + PythonVersion[I];

  if LVerNum.IsEmpty() then
    Exit(String.Empty);

  for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do
    if LVerNum.StartsWith(PYTHON_KNOWN_VERSIONS[I].RegVersion) then begin
      LLibName := PYTHON_KNOWN_VERSIONS[I].DllName;
      Break;
    end;

  Result := TPath.Combine(GetEnvironmentPath(), LLibName);
  if not TFile.Exists(Result) then
    Result := String.Empty;
end;

procedure TPyEmbeddableItem.LoadSettings;
begin
  Home := GetEnvironmentPath();
  ProgramName := GetEnvironmentPath();
  SharedLibrary := FindSharedLibrary();
  Executable := FindExecutable();
end;

function TPyEmbeddableItem.GetEnvironmentPath: string;
begin
  Result := TPath.Combine(EnvironmentPath, PythonVersion);
end;

procedure TPyEmbeddableItem.Setup;
begin
  inherited;
  if not EnvironmentExists() then begin
    if not EmbeddableExists() then
      raise EEmbeddableNotFound.CreateFmt(
        'Embeddable package not found.' + #13#10 + '%s', [FEmbeddablePackage]);
    CreateEnvironment();
    LoadSettings();
  end;
end;

{ TPyEmbeddedEnvironment }

function TPyEmbeddedEnvironment.CreateCollection: TPyEnvironmentCollection;
begin
  Result := TPyEmbeddableCollection.Create(Self, TPyEmbeddableItem);
end;

procedure TPyEmbeddedEnvironment.Prepare;
var
  LItem: TPyEmbeddableItem;
  I: Integer;
  LPath: string;
  LFiles: TArray<string>;
begin
  if not FScan then
    Exit();

  if not TDirectory.Exists(FDirectory) then
    raise Exception.Create('Directory not found.');

  for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
    LPath := TPath.Combine(FDirectory, PYTHON_KNOWN_VERSIONS[I].RegVersion);
    LFiles := TDirectory.GetFiles(LPath, '*.zip', TSearchOption.soTopDirectoryOnly);
    if (Length(LFiles) = 0) then
      Continue;

    LItem := TPyEmbeddableItem(Environments.Add());
    LItem.PythonVersion := PYTHON_KNOWN_VERSIONS[I].RegVersion;
    LItem.EnvironmentPath := FEnvironmentPath;
    LItem.EmbeddablePackage := LFiles[0];  
  end;
end;

{ TPyTransientItem }

procedure TPyTransientItem.Setup;
begin
  inherited;
end;

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
        LEnviromentInfo := LPythonVersion.FindValue(PYTHON_KNOWN_VERSIONS[I].RegVersion);

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

  EnumerateEnvironments(procedure(APythonVersion: string; AEnvironmentInfo: TJSONObject) 
  var
    LItem: TPyTransientItem;
  begin 
    LItem := TPyTransientItem(Environments.Add());
    LItem.PythonVersion := APythonVersion;
    AEnvironmentInfo.TryGetValue<string>('home', LItem.FHome);
    AEnvironmentInfo.TryGetValue<string>('program_name', LItem.FProgramName);
    AEnvironmentInfo.TryGetValue<string>('shared_library', LItem.FSharedLibrary);
    AEnvironmentInfo.TryGetValue<string>('executable', LItem.FExecutable);
  end);
end;

end.
