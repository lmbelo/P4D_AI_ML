(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.AddOn'                                    *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(* Functionality: PyEnvironment AddOn                                     *)
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
unit PyEnvironment.AddOn;

interface

uses
  System.Classes, System.Generics.Collections, System.SysUtils,
  PyEnvironment.Distribution, PyEnvironment.Notification;

type
  TPyEnvironmentCustomAddOn = class;
  TPyEnvironmentAddOns = class;

  TPyEnvironmentAddOnExecute = procedure(ASender: TObject;
    ANotification: TEnvironmentNotification;
    ADistribution: TPyDistribution) of object;

  TPyEnvironmentAddOnExecuteError = procedure(AException: Exception;
    const AAddOn: TPyEnvironmentCustomAddOn;
    ADistribution: TPyDistribution) of object;

  TPyEnvironmentCustomAddOn = class(TComponent)
  private
    FAddOns: TPyEnvironmentAddOns;
    FOnExecute: TPyEnvironmentAddOnExecute;
    procedure SetAddOns(const Value: TPyEnvironmentAddOns);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure Execute(ASender: TObject; ANotification: TEnvironmentNotification;
      ADistribution: TPyDistribution); virtual;
  published
    property AddOns: TPyEnvironmentAddOns read FAddOns write SetAddOns;
    property OnExecute: TPyEnvironmentAddOnExecute read FOnExecute write FOnExecute;
  end;

  [ComponentPlatforms(pidAllPlatforms)]
  TPyEnvironmentAddOnUser = class(TPyEnvironmentCustomAddOn);

  [ComponentPlatforms(pidAllPlatforms)]
  TPyEnvironmentAddOns = class(TComponent)
  private
    FList: TList<TPyEnvironmentCustomAddOn>;
    FOnExecuteError: TPyEnvironmentAddOnExecuteError;
  private
    FEnabled: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Add(AAddOn: TPyEnvironmentCustomAddOn);
    procedure Remove(AAddOn: TPyEnvironmentCustomAddOn);

    procedure Execute(ASender: TObject; ANotification: TEnvironmentNotification;
      ADistribution: TPyDistribution);
  published
    property Enabled: boolean read FEnabled write FEnabled default true;
    property OnExecuteError: TPyEnvironmentAddOnExecuteError
      read FOnExecuteError write FOnExecuteError;
  end;

implementation

{ TPyEnvironmentCustomAddOn }

procedure TPyEnvironmentCustomAddOn.Execute(ASender: TObject;
  ANotification: TEnvironmentNotification; ADistribution: TPyDistribution);
begin
  if Assigned(FOnExecute) then
    FOnExecute(ASender, ANotification, ADistribution);
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
  FEnabled := true;
  FList := TList<TPyEnvironmentCustomAddOn>.Create();
end;

destructor TPyEnvironmentAddOns.Destroy;
begin
  FList.Free();
  inherited;
end;

procedure TPyEnvironmentAddOns.Execute(ASender: TObject;
  ANotification: TEnvironmentNotification; ADistribution: TPyDistribution);
var
  LItem: TPyEnvironmentCustomAddOn;
begin
  if not FEnabled then
    Exit;

  for LItem in FList do begin
    try
      LItem.Execute(ASender, ANotification, ADistribution);
    except
      on E: Exception do
        if Assigned(FOnExecuteError) then begin
          FOnExecuteError(E, LItem, ADistribution);
        end else
          raise;
    end;
  end;
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

end.
