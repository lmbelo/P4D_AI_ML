unit PyEmbedded;

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
  TPyEmbedded = class(TPyEnvironment)
  private
    FEmbeddablesPath: string;
    FEnvironmentsPath: string;
    //Events
    FOnUnzipProgress: TZipProgressEvent;
    FBeforeCreate: TNotifyEvent;
    FAfterCreate: TNotifyEvent;
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
    function EmbeddableExists(): boolean;
    /// <summary>
    ///   Creates a new environment based on the current settings.
    ///   An embeddable distribution will be used as an "image".
    /// </summary>
    procedure CreateEnvironment(); virtual;
  protected
    function GetEnvironmentPath(): string; override;
  public
    procedure Prepare(); override;
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

  EEmbeddableNotFound = class(Exception);

const
  PYTHON_ROOT_DIR_NAME = 'python';
  PYTHON_EMBEDDABLES_DIR_NAME = 'embeddables';
  PYTHON_ENVIRONMENT_DIR_NAME = 'environments';

implementation

uses
  System.IOUtils;

{ TPyEmbedded }

function TPyEmbedded.GetEmbeddablePackage: string;
var
  LFiles: TArray<string>;
begin
  LFiles := TDirectory.GetFiles(GetEmbeddablePath(), '*.zip', TSearchOption.soTopDirectoryOnly);
  if Length(LFiles) > 0 then
    Result := LFiles[0]
  else
    Result := String.Empty;
end;

function TPyEmbedded.GetEmbeddablePath: string;
begin
  Result := TPath.Combine(
    TPath.Combine(ResolveEmbeddablesPath(), PYTHON_ROOT_DIR_NAME),
      TPath.Combine(PYTHON_EMBEDDABLES_DIR_NAME,
        TPath.Combine(GetPlatformName(),
          TPath.Combine(GetArchitectureName(), PythonVersion))));
end;

function TPyEmbedded.ResolveEmbeddablesPath: string;
begin
  if not FEmbeddablesPath.IsEmpty() then
    Result := FEmbeddablesPath
  else
    Result := ExtractFilePath(ParamStr(0));
end;

function TPyEmbedded.ResolveEnvironmentsPath: string;
begin
  if not FEnvironmentsPath.IsEmpty() then
    Result := FEnvironmentsPath
  else
    Result := ExtractFilePath(ParamStr(0));
end;

procedure TPyEmbedded.Prepare;
begin
  if not Exists() then begin
    if not EmbeddableExists() then
      raise EEmbeddableNotFound.CreateFmt(
        'Embeddable package not found.' + #13#10 + '%s', [GetEmbeddablePath()]);
    CreateEnvironment();
  end;
end;

function TPyEmbedded.GetEnvironmentPath: string;
begin
  Result := TPath.Combine(
    TPath.Combine(ResolveEmbeddablesPath(), PYTHON_ROOT_DIR_NAME),
      TPath.Combine(PYTHON_ENVIRONMENT_DIR_NAME,
        TPath.Combine(GetPlatformName(),
          TPath.Combine(GetArchitectureName(), PythonVersion))));
end;

function TPyEmbedded.EmbeddableExists: boolean;
begin
  Result := TFile.Exists(GetEmbeddablePackage());
end;

procedure TPyEmbedded.CreateEnvironment;
begin
  if Assigned(FBeforeCreate) then
    FBeforeCreate(Self);

  //Unzip the embeddable package into the target directory.
  TZipFile.ExtractZipFile(GetEmbeddablePackage(), GetEnvironmentPath(), FOnUnzipProgress);

  if Assigned(FAfterCreate) then
    FAfterCreate(Self);
end;

end.
