(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Embeddable'                               *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironment Embeddable                              *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
unit PyEnvironment.Embeddable;

interface

uses
  System.Classes, System.SysUtils, System.Zip,
  PyEnvironment, PyEnvironment.Info, PythonEngine;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                      Embeddables structure example                    *)
  (*                                                                       *)
  (* [Root] Directory                                                      *)
  (*  +-- python version/                                                  *)
  (*       +-- python zip                                                  *)
  (*-----------------------------------------------------------------------*)

  TPyEmbeddableInfo = class;
  TZipProgress = procedure(Sender: TObject; AInfo: TPyEmbeddableInfo; FileName: string; Header: TZipHeader; Position: Int64) of object;

  TPyEmbeddableBaseInfo = class(TPyEnvironmentInfo)
  private
    FEnvironmentPath: string;
  published
    property EnvironmentPath: string read FEnvironmentPath write FEnvironmentPath;
  end;

  TPyEmbeddableInfo = class(TPyEmbeddableBaseInfo)
  private
    FEmbeddablePackage: string;
    FScanned: boolean;
    FOnZipProgress: TZipProgress;
    function FindSharedLibrary(): string;
    function FindExecutable(): string;
  private
    procedure DoZipProgressEvt(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
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
    property OnZipProgress: TZipProgress read FOnZipProgress write FOnZipProgress;
  end;

  TPyEmbeddableCollection = class(TPyEnvironmentCollection);

  [ComponentPlatforms(pidAllPlatforms)]
  TPyEmbeddedEnvironment = class(TPyCustomEnvironment)
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
    FOnZipProgress: TZipProgress;
    procedure SetScanner(const Value: TScanner);
  protected
    function CreateCollection(): TPyEnvironmentCollection; override;
    procedure Prepare(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property Scanner: TScanner read FScanner write SetScanner;
    property OnZipProgress: TZipProgress read FOnZipProgress write FOnZipProgress;
  end;

  EEmbeddableNotFound = class(Exception);

implementation

uses
  System.IOUtils, PyEnvironment.Notification;

{ TPyEmbeddableInfo }

procedure TPyEmbeddableInfo.CreateEnvironment;
begin
  //Unzip the embeddable package into the target directory.
  TZipFile.ExtractZipFile(FEmbeddablePackage, GetEnvironmentPath(), DoZipProgressEvt);
end;

procedure TPyEmbeddableInfo.DoZipProgressEvt(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  if Assigned(FOnZipProgress) then
    FOnZipProgress(Sender, Self, FileName, Header, Position);
end;

function TPyEmbeddableInfo.EmbeddableExists: boolean;
begin
  Result := TFile.Exists(FEmbeddablePackage);
end;

function TPyEmbeddableInfo.EnvironmentExists: boolean;
begin
  Result := TDirectory.Exists(GetEnvironmentPath());
end;

function TPyEmbeddableInfo.FindExecutable: string;
begin
  {$IFDEF MSWINDOWS}
  Result := TPath.Combine(GetEnvironmentPath(), 'python.exe');
  if not TFile.Exists(Result) then
    Result := String.Empty;
  {$ELSE}
    Result := String.Empty;
  {$IFEND}
end;

function TPyEmbeddableInfo.FindSharedLibrary: string;
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

procedure TPyEmbeddableInfo.LoadSettings;
begin
  Home := GetEnvironmentPath();
  ProgramName := GetEnvironmentPath();
  SharedLibrary := FindSharedLibrary();
  Executable := FindExecutable();
end;

function TPyEmbeddableInfo.GetEnvironmentPath: string;
begin
  Result := EnvironmentPath;
end;

procedure TPyEmbeddableInfo.Setup;
begin
  inherited;
  if not EnvironmentExists() then begin
    if not EmbeddableExists() then
      raise EEmbeddableNotFound.CreateFmt(
        'Embeddable package not found.' + #13#10 + '%s', [FEmbeddablePackage]);

    TEnvironmentBroadcaster.Instance.NotifyAll(Self, BEFORE_CREATE_ENVIRONMENT, Self);
    CreateEnvironment();
    TEnvironmentBroadcaster.Instance.NotifyAll(Self, AFTER_CREATE_ENVIRONMENT, Self);
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
  Result := TPyEmbeddableCollection.Create(Self, TPyEmbeddableInfo);
end;

procedure TPyEmbeddedEnvironment.Prepare;
var
  LItem: TPyEmbeddableInfo;
begin
  if FScanner.AutoScan then begin
    FScanner.Scan(
      procedure(APyVersionInfo: TPythonVersionProp; AEmbeddablePackage: string) begin
        if Assigned(Environments.LocateEnvironment(APyVersionInfo.RegVersion)) then
          Exit;

        LItem := TPyEmbeddableInfo(Environments.Add());
        LItem.Scanned := true;
        LItem.PythonVersion := APyVersionInfo.RegVersion;
        LItem.EnvironmentPath := TPath.Combine(FScanner.EnvironmentPath, APyVersionInfo.RegVersion);
        LItem.EmbeddablePackage := AEmbeddablePackage;
        LItem.OnZipProgress := FOnZipProgress;
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
