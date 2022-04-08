unit PyEnvironments;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  PythonEngine, System.Generics.Collections;

type
  TExecuteAction = byte;
  TPyEnvironmentItem = class;
  TPyEnvironmentCustomAddOn = class;
  TPyEnvironmentAddOns = class;
  TPyEnvironment = class;

  TPyEnvironmentAddOnExecute = procedure(AAction: TExecuteAction; AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem) of object;
  TPyEnvironmentAddOnExecuteError = procedure(AException: Exception; const AAddOn: TPyEnvironmentCustomAddOn; AEnvironment: TPyEnvironmentItem) of object;

  TPyEnvironmentCustomAddOn = class(TComponent)
  private
    FAddOns: TPyEnvironmentAddOns;
    FOnExecute: TPyEnvironmentAddOnExecute;
    procedure SetAddOns(const Value: TPyEnvironmentAddOns);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure Execute(AAction: TExecuteAction; AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem); virtual;
  published
    property AddOns: TPyEnvironmentAddOns read FAddOns write SetAddOns;
    property OnExecute: TPyEnvironmentAddOnExecute read FOnExecute write FOnExecute;
  end;

  TPyEnvironmentAddOnUser = class(TPyEnvironmentCustomAddOn);

  TPyEnvironmentAddOns = class(TComponent)
  private
    FList: TList<TPyEnvironmentCustomAddOn>;
    FOnExecuteError: TPyEnvironmentAddOnExecuteError;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Add(AAddOn: TPyEnvironmentCustomAddOn);
    procedure Remove(AAddOn: TPyEnvironmentCustomAddOn);

    procedure Execute(AAction: TExecuteAction; AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem);
  published
    property OnExecuteError: TPyEnvironmentAddOnExecuteError read FOnExecuteError write FOnExecuteError;
  end;

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
    FPythonVersion: string;
    FAddOns: TPyEnvironmentAddOns;
    procedure SetEnvironments(const Value: TPyEnvironmentCollection);
    procedure SetPythonEngine(const Value: TPythonEngine);
  protected 
    procedure Loaded(); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  protected
    function CreateCollection(): TPyEnvironmentCollection; virtual; abstract;
    procedure Prepare(); virtual;
    procedure NotifyAction(AAction: TExecuteAction; AEnvironment: TPyEnvironmentItem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    
    procedure Activate(APythonVersion: string);
  published
    property Environments: TPyEnvironmentCollection read FEnvironments write SetEnvironments;
    property AutoLoad: boolean read FAutoLoad write FAutoLoad;
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
    property AddOns: TPyEnvironmentAddOns read FAddOns write FAddOns;
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
    FScanned: boolean;
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
    property Scanned: boolean read FScanned write FScanned;
  published
    property EmbeddablePackage: string read FEmbeddablePackage write FEmbeddablePackage;
  end;

  TPyEmbeddableResourceItem = class(TPyEmbeddableBaseItem);

  TPyEmbeddableCollection = class(TPyEnvironmentCollection);

  [ComponentPlatforms(pidAllPlatforms)]
  TPyEmbeddedEnvironment = class(TPyEnvironment)
  private type
    TScanner = class(TPersistent)
    private
      FAutoScan: boolean;
      FEmbeddablesPath: string;
      FEnvironmentPath: string;
    public
      procedure Scan(ACallback: TProc<TPythonVersionProp, string>);
    published
      property AutoScan: boolean read FAutoScan write FAutoScan default false;
      property EmbeddablesPath: string read FEmbeddablesPath write FEmbeddablesPath;
      /// <summary>
      ///   Default environment path.
      /// </summary>
      property EnvironmentPath: string read FEnvironmentPath write FEnvironmentPath;
    end;
  private
    FScanner: TScanner;
    procedure SetScanner(const Value: TScanner);
  protected
    function CreateCollection(): TPyEnvironmentCollection; override;
    procedure Prepare(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property Scanner: TScanner read FScanner write SetScanner;
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

  EEmbeddableNotFound = class(Exception);
  EInvalidFileStructure = class(Exception);

const
  BEFORE_SETUP_ACTION = $0;
  AFTER_SETUP_ACTION = $1;

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

  NotifyAction(BEFORE_SETUP_ACTION, LItem);

  LItem.Setup();

  FPythonEngine.UnloadDll();
  FPythonEngine.UseLastKnownVersion := false;
  FPythonEngine.PythonHome := LItem.Home;
  FPythonEngine.ProgramName := LItem.ProgramName;
  FPythonEngine.DllPath := ExtractFilePath(LItem.SharedLibrary);
  FPythonEngine.DllName := ExtractFileName(LItem.SharedLibrary);
  FPythonEngine.LoadDll();

  FPythonEngine.ExecString('import sys');
  FPythonEngine.ExecString(AnsiString(Format('sys.executable = r"%s"', [LItem.Executable])));

  NotifyAction(AFTER_SETUP_ACTION, LItem);
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

procedure TPyEnvironment.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FPythonEngine) then begin
    FPythonEngine := nil;
  end;
end;

procedure TPyEnvironment.NotifyAction(AAction: TExecuteAction;
  AEnvironment: TPyEnvironmentItem);
begin
  if not Assigned(FAddOns) then
    Exit();

  FAddOns.Execute(AAction, Self, AEnvironment);
end;

procedure TPyEnvironment.Prepare;
begin
  if FAutoLoad and Assigned(FPythonEngine) then
    Activate(PythonVersion);
end;

procedure TPyEnvironment.SetEnvironments(
  const Value: TPyEnvironmentCollection);
begin
  FEnvironments.Assign(Value);
end;

procedure TPyEnvironment.SetPythonEngine(const Value: TPythonEngine);
begin
  if (Value <> FPythonEngine) then begin
    if Assigned(FPythonEngine) then
      FPythonEngine.RemoveFreeNotification(Self);
    FPythonEngine := Value;
    if Assigned(FPythonEngine) then begin
      FPythonEngine.FreeNotification(Self);
      FPythonEngine.AutoLoad := false;
    end;
  end;
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
  I: integer;
  LLibName: string;
begin
  { TODO : (BETA) Improve localizer }
  for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do
    if PythonVersion.StartsWith(PYTHON_KNOWN_VERSIONS[I].RegVersion) then begin
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
  Result := EnvironmentPath; //TPath.Combine(EnvironmentPath, PythonVersion);
end;

procedure TPyEmbeddableItem.Setup;
begin
  inherited;
  if not EnvironmentExists() then begin
    if not EmbeddableExists() then
      raise EEmbeddableNotFound.CreateFmt(
        'Embeddable package not found.' + #13#10 + '%s', [FEmbeddablePackage]);
    CreateEnvironment();
  end;

  if FScanned then
    LoadSettings();
end;

{ TPyEmbeddedEnvironment }

constructor TPyEmbeddedEnvironment.Create(AOwner: TComponent);
begin
  inherited;
  FScanner := TScanner.Create();
end;

destructor TPyEmbeddedEnvironment.Destroy;
begin
  FScanner.Free();
  inherited;
end;

function TPyEmbeddedEnvironment.CreateCollection: TPyEnvironmentCollection;
begin
  Result := TPyEmbeddableCollection.Create(Self, TPyEmbeddableItem);
end;

procedure TPyEmbeddedEnvironment.Prepare;
var
  LItem: TPyEmbeddableItem;
begin
  if FScanner.AutoScan then begin
    FScanner.Scan(
      procedure(APyVersionInfo: TPythonVersionProp; AEmbeddablePackage: string) begin
        if Assigned(Environments.LocateEnvironment(APyVersionInfo.RegVersion)) then
          Exit;

        LItem := TPyEmbeddableItem(Environments.Add());
        LItem.Scanned := true;
        LItem.PythonVersion := APyVersionInfo.RegVersion;
        LItem.EnvironmentPath := TPath.Combine(FScanner.EnvironmentPath, APyVersionInfo.RegVersion);
        LItem.EmbeddablePackage := AEmbeddablePackage;
      end);
  end;
  inherited;
end;

procedure TPyEmbeddedEnvironment.SetScanner(const Value: TScanner);
begin
  FScanner.Assign(Value);
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
      AEnvironmentInfo.TryGetValue<string>('home', LItem.FHome);
      AEnvironmentInfo.TryGetValue<string>('program_name', LItem.FProgramName);
      AEnvironmentInfo.TryGetValue<string>('shared_library', LItem.FSharedLibrary);
      AEnvironmentInfo.TryGetValue<string>('executable', LItem.FExecutable);
    end);

  inherited;
end;

{ TPyEmbeddedEnvironment.TScanner }

procedure TPyEmbeddedEnvironment.TScanner.Scan(
  ACallback: TProc<TPythonVersionProp, string>);
var
  I: Integer;
  LPath: string;
  LFiles: TArray<string>;
begin
  if not Assigned(ACallback) then
    Exit;

  if not TDirectory.Exists(FEmbeddablesPath) then
    raise Exception.Create('Directory not found.');

  for I := Low(PYTHON_KNOWN_VERSIONS) to High(PYTHON_KNOWN_VERSIONS) do begin
    LPath := TPath.Combine(FEmbeddablesPath, PYTHON_KNOWN_VERSIONS[I].RegVersion);
    if not TDirectory.Exists(LPath) then
      Continue;

    LFiles := TDirectory.GetFiles(LPath, '*.zip', TSearchOption.soTopDirectoryOnly);
    if (Length(LFiles) = 0) then
      Continue;

    ACallback(PYTHON_KNOWN_VERSIONS[I], LFiles[0]);
  end;
end;

{ TPyEnvironmentCustomAddOn }

procedure TPyEnvironmentCustomAddOn.Execute(AAction: TExecuteAction;
  AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem);
begin
  if Assigned(FOnExecute) then
    FOnExecute(AAction, AManager, AEnvironment);
end;

procedure TPyEnvironmentCustomAddOn.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FAddOns) then begin
    SetAddOns(nil);
  end;
end;

procedure TPyEnvironmentCustomAddOn.SetAddOns(
  const Value: TPyEnvironmentAddOns);
begin
  if Assigned(FAddOns) then begin
    FAddOns.RemoveFreeNotification(Self);
    FAddOns.Remove(Self);
  end;

  FAddOns := Value;
  if Assigned(FAddOns) then begin
    FAddOns.FreeNotification(Self);
    FAddOns.Add(Self);
  end;
end;

{ TPyEnvironmentAddOns }

constructor TPyEnvironmentAddOns.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList<TPyEnvironmentCustomAddOn>.Create();
end;

destructor TPyEnvironmentAddOns.Destroy;
begin
  FList.Free();
  inherited;
end;

procedure TPyEnvironmentAddOns.Add(AAddOn: TPyEnvironmentCustomAddOn);
begin
  if not FList.Contains(AAddOn) then
    FList.Add(AAddOn);
end;

procedure TPyEnvironmentAddOns.Remove(AAddOn: TPyEnvironmentCustomAddOn);
begin
  if FList.Contains(AAddOn) then
    FList.Remove(AAddOn);
end;

procedure TPyEnvironmentAddOns.Execute(AAction: TExecuteAction;
  AManager: TPyEnvironment; AEnvironment: TPyEnvironmentItem);
var
  LItem: TPyEnvironmentCustomAddOn;
begin
  for LItem in FList do begin
    try
      LItem.Execute(AAction, AManager, AEnvironment);
    except
      on E: Exception do
        if Assigned(FOnExecuteError) then begin
          FOnExecuteError(E, LItem, AEnvironment);
        end else
          raise;
    end;
  end;
end;

end.
