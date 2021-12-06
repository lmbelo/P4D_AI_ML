// 
// Created by the DataSnap proxy generator.
// 12/6/2021 7:52:19 AM
// 

unit Remote.ClientClasses;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.DBXJSONReflect;

type
  TTrainingClassClient = class(TDSAdminRestClient)
  private
    FCountClassCommand: TDSRestCommand;
    FSendImageCommand: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function CountClass(AProfile: string; ATrainingClass: string; const ARequestFilter: string = ''): Integer;
    function SendImage(AProfile: string; ATrainingClass: string; AImageName: string; AImage: TStream; const ARequestFilter: string = ''): Integer;
  end;

const
  TTrainingClass_CountClass: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'ATrainingClass'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 6; TypeName: 'Integer')
  );

  TTrainingClass_SendImage: array [0..4] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'ATrainingClass'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'AImageName'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'AImage'; Direction: 1; DBXType: 33; TypeName: 'TStream'),
    (Name: ''; Direction: 4; DBXType: 6; TypeName: 'Integer')
  );

implementation

function TTrainingClassClient.CountClass(AProfile: string; ATrainingClass: string; const ARequestFilter: string): Integer;
begin
  if FCountClassCommand = nil then
  begin
    FCountClassCommand := FConnection.CreateCommand;
    FCountClassCommand.RequestType := 'GET';
    FCountClassCommand.Text := 'TTrainingClass.CountClass';
    FCountClassCommand.Prepare(TTrainingClass_CountClass);
  end;
  FCountClassCommand.Parameters[0].Value.SetWideString(AProfile);
  FCountClassCommand.Parameters[1].Value.SetWideString(ATrainingClass);
  FCountClassCommand.Execute(ARequestFilter);
  Result := FCountClassCommand.Parameters[2].Value.GetInt32;
end;

function TTrainingClassClient.SendImage(AProfile: string; ATrainingClass: string; AImageName: string; AImage: TStream; const ARequestFilter: string): Integer;
begin
  if FSendImageCommand = nil then
  begin
    FSendImageCommand := FConnection.CreateCommand;
    FSendImageCommand.RequestType := 'POST';
    FSendImageCommand.Text := 'TTrainingClass."SendImage"';
    FSendImageCommand.Prepare(TTrainingClass_SendImage);
  end;
  FSendImageCommand.Parameters[0].Value.SetWideString(AProfile);
  FSendImageCommand.Parameters[1].Value.SetWideString(ATrainingClass);
  FSendImageCommand.Parameters[2].Value.SetWideString(AImageName);
  FSendImageCommand.Parameters[3].Value.SetStream(AImage, FInstanceOwner);
  FSendImageCommand.Execute(ARequestFilter);
  Result := FSendImageCommand.Parameters[4].Value.GetInt32;
end;

constructor TTrainingClassClient.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TTrainingClassClient.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TTrainingClassClient.Destroy;
begin
  FCountClassCommand.DisposeOf;
  FSendImageCommand.DisposeOf;
  inherited;
end;

end.

