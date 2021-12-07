unit TrainingClassMethods;

interface

uses System.SysUtils, System.Classes, System.Json,
    DataSnap.DSProviderDataModuleAdapter,
    Datasnap.DSServer, Datasnap.DSAuth;

type
  TTrainingClass = class(TDSServerModule)
  private
    procedure BuildDir(const ADir: string);
    function BuildImagesFolder(): string;
    function BuildProfileFolder(const ABaseDir, AProfile: string): string;
    function BuildClassFolder(const ABaseDir, ATrainingClass: string): string;
    function BuildImagePath(const ABaseDir, AImageName: string): string;
    function GetCount(const ABaseDir: string): integer;
  public
    const IMAGES_FOLDER = 'training_data';
  public
    { Public declarations }
    function CountClass(const AProfile, ATrainingClass: string): integer;
    function SendImage(const AProfile, ATrainingClass, AImageName: string;
      const AImage: TStream): integer;
    procedure Clear(const AProfile, ATrainingClass: string);
  end;

implementation

uses
  System.IOUtils;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TTrainingClass }

function TTrainingClass.BuildClassFolder(const ABaseDir,
  ATrainingClass: string): string;
begin
  Result := TPath.Combine(ABaseDir, ATrainingClass);
  BuildDir(Result);
end;

procedure TTrainingClass.BuildDir(const ADir: string);
begin
  if not TDirectory.Exists(ADir) then
    TDirectory.CreateDirectory(ADir);
end;

function TTrainingClass.BuildImagePath(const ABaseDir, AImageName: string): string;
begin
  Result := TPath.Combine(ABaseDir, AImageName);
end;

function TTrainingClass.BuildProfileFolder(const ABaseDir,
  AProfile: string): string;
begin
  Result := TPath.Combine(ABaseDir, AProfile);
  BuildDir(Result);
end;

procedure TTrainingClass.Clear(const AProfile, ATrainingClass: string);
begin
  TDirectory.Delete(BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass), true);
end;

function TTrainingClass.CountClass(const AProfile,
  ATrainingClass: string): integer;
begin
  Result := GetCount(BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass));
end;

function TTrainingClass.GetCount(const ABaseDir: string): integer;
begin
  Result := Length(TDirectory.GetFiles(ABaseDir));
end;

function TTrainingClass.BuildImagesFolder(): string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), IMAGES_FOLDER, false);
  BuildDir(Result);
end;

function TTrainingClass.SendImage(const AProfile, ATrainingClass, AImageName: string;
  const AImage: TStream): integer;
begin
  var LDir := BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass);
  var LStream := TFileStream.Create(BuildImagePath(LDir, AImageName), fmCreate or fmOpenWrite);
  try
    LStream.Seek(0, soFromEnd);
    LStream.CopyFrom(AImage, AImage.Size)
  finally
    LStream.Free();
  end;

  Result := GetCount(LDir);
end;

end.

