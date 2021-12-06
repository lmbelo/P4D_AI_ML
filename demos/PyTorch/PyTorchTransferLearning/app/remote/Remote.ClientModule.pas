unit Remote.ClientModule;

interface

uses
  System.SysUtils, System.Classes, Remote.ClientClasses, Datasnap.DSClientRest;

type
  TClientModule = class(TDataModule)
    DSRestConnection1: TDSRestConnection;
  private
    FInstanceOwner: Boolean;
    FTrainingClassClient: TTrainingClassClient;
    function GetTrainingClassClient: TTrainingClassClient;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property TrainingClassClient: TTrainingClassClient read GetTrainingClassClient write FTrainingClassClient;
end;

var
  ClientModule: TClientModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

constructor TClientModule.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := False;
end;

destructor TClientModule.Destroy;
begin
  FTrainingClassClient.Free;
  inherited;
end;

function TClientModule.GetTrainingClassClient: TTrainingClassClient;
begin
  if FTrainingClassClient = nil then
    FTrainingClassClient:= TTrainingClassClient.Create(DSRestConnection1, FInstanceOwner);
  Result := FTrainingClassClient;
end;

end.
