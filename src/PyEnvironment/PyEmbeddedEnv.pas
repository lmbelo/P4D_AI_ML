unit PyEmbeddedEnv;

interface

uses
  System.Classes, System.SysUtils, System.Zip,
  PyEnvironment,
  PyEnvironment.Intf;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                      Embeddables structure example                    *)
  (*                                                                       *)
  (*                                                                       *)
  (*                                                                       *)
  (* EmbeddablesPath                                                       *)
  (*  +-- python/                                                          *)
  (*       +-- environments/                                               *)
  (*            +-- os/                                                    *)
  (*                 +-- arch/                                             *)
  (*                      +-- python version/                              *)
  (*                           +-- python zip                              *)
  (*-----------------------------------------------------------------------*)
  /// <summary>
  ///   Provide access to a Python environment based on embeddables.
  /// </summary>
  TPyEmbeddedEnv = class(TPyEnvironment, IEnvironmentPaths)
  private
    FEmbeddablesPath: string;
    FEnvironmentsPath: string;
    //Events
    FOnUnzipProgress: TZipProgressEvent;
    FBeforeCreate: TNotifyEvent;
    FAfterCreate: TNotifyEvent;
  private
    {***** IEnvironmentPaths implementation *****}

    //Python environment paths
    function GetHome(): string;
    function GetProgramName(): string;

    //Python file paths
    function GetSharedLibrary(): string;
    function GetExecutable(): string;
  protected
    /// <summary>
    ///   Having the EmbeddablesPath empty, the app path is used as root.
    /// </summary>
    function ResolveEmbeddablesPath(): string;
    /// <summary>
    ///   Having the EnvironmentsPath empty, the app path is used as root.
    /// </summary>
    function ResolveEnvironmentsPath(): string;
    /// <summary>
    ///   Return the embeddable path based on the current settings.
    /// </summary>
    function GetEmbeddablePath(): string;
    /// <summary>
    ///   Return the first zip file contained in the embeddables path.
    /// </summary>
    function GetEmbeddablePackage(): string;
    /// <summary>
    ///   Navigates through the embeddables searching for a compatible distribution.
    /// </summary>
    function EmbeddableExists(): boolean; virtual;
    /// <summary>
    ///   Creates a new environment based on the current settings.
    ///   An embeddable distribution will be used as an "image".
    /// </summary>
    procedure CreateEnvironment(); virtual;
  protected
    function GetEnvironmentPath(): string; override;
  public
    procedure Run();
  published
    property Architecture;
    property Platform;
    property PythonVersion;

    //Environment settings

    /// <summary>
    ///   Specifies the Pyton embeddables folder.
    /// </summary>
    property EmbeddablesPath: string read FEmbeddablesPath write FEmbeddablesPath;
    /// <summary>
    ///   Specifies the Python environment folder.
    /// </summary>
    property EnvironmentsPath: string read FEnvironmentsPath write FEnvironmentsPath;

    //Events
    property BeforeCreate: TNotifyEvent read FBeforeCreate write FBeforeCreate;
    property OnUnzipProgress: TZipProgressEvent read FOnUnzipProgress write FOnUnzipProgress;
    property AfterCreate: TNotifyEvent read FAfterCreate write FAfterCreate;
  end;

  EEmbeddableNotAvailable = class(Exception);

implementation

uses
  System.IOUtils;

{ TPyEmbeddedEnv }

function TPyEmbeddedEnv.GetEmbeddablePackage: string;
var
  LFiles: TArray<string>;
begin
  LFiles := TDirectory.GetFiles(GetEmbeddablePath(), '*.zip', TSearchOption.soTopDirectoryOnly);
  if Length(LFiles) > 0 then
    Result := LFiles[0]
  else
    Result := String.Empty;
end;

function TPyEmbeddedEnv.GetEmbeddablePath: string;
begin
  Result :=
    TPath.Combine(PythonVersion,
      TPath.Combine(GetArchitectureName(),
        TPath.Combine(GetPlatformName(),
          TPath.Combine(
            TPath.Combine(ResolveEmbeddablesPath(), 'python'),
            'embeddables'))));
end;

function TPyEmbeddedEnv.ResolveEmbeddablesPath: string;
begin
  if not FEmbeddablesPath.IsEmpty() then
    Result := FEmbeddablesPath
  else
    Result := ExtractFilePath(ParamStr(0));
end;

function TPyEmbeddedEnv.ResolveEnvironmentsPath: string;
begin
  if not FEnvironmentsPath.IsEmpty() then
    Result := FEnvironmentsPath
  else
    Result := ExtractFilePath(ParamStr(0));
end;

procedure TPyEmbeddedEnv.Run;
var
  LPackage: string;
begin
  LPackage := GetEmbeddablePackage();
  if not Exists() then begin
    if not TFile.Exists(LPackage) then
      raise EEmbeddableNotAvailable.CreateFmt('Embeddable not available. %s', [LPackage]);
    CreateEnvironment();
  end;
end;

function TPyEmbeddedEnv.GetEnvironmentPath: string;
begin
  Result :=
    TPath.Combine(PythonVersion,
      TPath.Combine(GetArchitectureName(),
        TPath.Combine(GetPlatformName(),
          TPath.Combine(
            TPath.Combine(ResolveEnvironmentsPath(), 'python'),
            'environments'))));
end;

function TPyEmbeddedEnv.EmbeddableExists: boolean;
begin
  Result := TFile.Exists(GetEmbeddablePackage());
end;

procedure TPyEmbeddedEnv.CreateEnvironment;
begin
  if Assigned(FBeforeCreate) then
    FBeforeCreate(Self);

  //Unzip the embeddable package into the target directory.
  TZipFile.ExtractZipFile(GetEmbeddablePackage(), GetEnvironmentPath(), FOnUnzipProgress);

  if Assigned(FAfterCreate) then
    FAfterCreate(Self);
end;

function TPyEmbeddedEnv.GetHome: string;
begin
  Result := GetEnvironmentPath();
end;

function TPyEmbeddedEnv.GetProgramName: string;
begin
  Result := GetEnvironmentPath();
end;

function TPyEmbeddedEnv.GetSharedLibrary: string;
var
  LFiles: TArray<string>;
begin
  { TODO : (BETA) Improve localizer }
  case ResolvePlatform() of
    TPlatform.pfWindows: begin
      LFiles := TDirectory.GetFiles(GetEnvironmentPath(), 'python*.dll', TSearchOption.soTopDirectoryOnly);
      if Length(LFiles) > 0 then
        Result := LFiles[0]
      else
        Result := String.Empty;
    end
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

function TPyEmbeddedEnv.GetExecutable: string;
var
  LFiles: TArray<string>;
begin
  { TODO : (BETA) Improve localizer }
  case ResolvePlatform() of
    TPlatform.pfWindows: begin
      LFiles := TDirectory.GetFiles(GetEnvironmentPath(), 'python.exe', TSearchOption.soTopDirectoryOnly);
      if Length(LFiles) > 0 then
        Result := LFiles[0]
      else
        Result := String.Empty;
    end
    else raise EUnsupportedPlatform.Create('Unsupported platform.');
  end;
end;

end.
