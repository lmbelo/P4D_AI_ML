(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironments.Notification'                            *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironments Notification layer                     *)
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
unit PyEnvironment.Notification;

interface

uses
  System.Classes, System.Generics.Collections,
  PyEnvironment.Info;

type
  TEnvironmentNotification = byte;

  TOnReceiveNotification = procedure(ANotifier: TObject;
    ANotification: TEnvironmentNotification; AInfo: TPyEnvironmentInfo) of object;

  IEnvironmentNotified = interface
    ['{D528B694-C4C4-4A96-BF40-2153CCC80243}']
    procedure NotifyUpadte(ANotifier: TObject; ANotification: TEnvironmentNotification;
      AInfo: TPyEnvironmentInfo);
  end;

  IEnvironmentNotifier = interface
    ['{6214AB2E-4BD6-472E-B3A1-B6FCB46B1798}']
    procedure AddListener(const AListener: IEnvironmentNotified);
    procedure RemoveListener(const AListener: IEnvironmentNotified);

    procedure NotifyAll(ANotifier: TObject; ANotification: TEnvironmentNotification;
      AInfo: TPyEnvironmentInfo);
  end;

  TEnvironmentBroadcaster = class(TInterfacedPersistent, IEnvironmentNotifier)
  private
    class var FInstance: TEnvironmentBroadcaster;
  private
    class constructor Create();
    class destructor Destroy();
  private
    FListeners: TList<IEnvironmentNotified>;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure AddListener(const AListener: IEnvironmentNotified);
    procedure RemoveListener(const AListener: IEnvironmentNotified);

    procedure NotifyAll(ANotifier: TObject; ANotification: TEnvironmentNotification;
      AInfo: TPyEnvironmentInfo);

    class property Instance: TEnvironmentBroadcaster read FInstance write FInstance;
  end;

const
  BEFORE_ACTIVATE_NOTIFICATION = $0;
  AFTER_ACTIVATE_NOTIFICATION = $1;
  BEFORE_DEACTIVATE_NOTIFICATION = $02;
  AFTER_DEACTIVATE_NOTIFICATION = $03;
  BEFORE_CREATE_ENVIRONMENT = $04;
  AFTER_CREATE_ENVIRONMENT = $05;

implementation

{ TEnvironmentBroadcaster }

class constructor TEnvironmentBroadcaster.Create;
begin
  FInstance := TEnvironmentBroadcaster.Create();
end;

class destructor TEnvironmentBroadcaster.Destroy;
begin
  FInstance.Free();
end;

constructor TEnvironmentBroadcaster.Create;
begin
  FListeners := TList<IEnvironmentNotified>.Create();
end;

destructor TEnvironmentBroadcaster.Destroy;
begin
  FListeners.Free();
end;

procedure TEnvironmentBroadcaster.AddListener(
  const AListener: IEnvironmentNotified);
begin
  FListeners.Add(AListener);
end;

procedure TEnvironmentBroadcaster.RemoveListener(
  const AListener: IEnvironmentNotified);
begin
  FListeners.Remove(AListener);
end;

procedure TEnvironmentBroadcaster.NotifyAll(ANotifier: TObject;
  ANotification: TEnvironmentNotification; AInfo: TPyEnvironmentInfo);
var
  LListener: IEnvironmentNotified;
begin
  for LListener in FListeners do begin
    LListener.NotifyUpadte(ANotifier, ANotification, AInfo);
  end;
end;

end.