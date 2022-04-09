unit PyEnvironment.Embeddable;

interface

uses
  System.Classes, System.SysUtils, PyEnvironments, PythonEngine;

type
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

implementation

uses
  System.Zip, System.IOUtils;

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

end.
