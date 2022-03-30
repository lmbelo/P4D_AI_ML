(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyPackage.Manager.Conda'                                *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                   https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyPackage Manager layer                               *)
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
unit PyPackage.Manager.Conda;

interface

uses
  PyCore,
  PyPackage.Manager,
  PyPackage.Manager.Intf,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf;

type
  TPyPackageManagerConda = class(TPyPackageManager, IPyPackageManager)
  private
    //IPyPackageManager implementation
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
    function IsInstalled(): boolean; reintroduce;
    procedure Install();
    procedure Uninstall();
  public
    constructor Create(const APackageName: TPyPackageName); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TPyPackageManagerConda }

constructor TPyPackageManagerConda.Create(const APackageName: TPyPackageName);
begin
  inherited;
end;

destructor TPyPackageManagerConda.Destroy;
begin
  inherited;
end;

function TPyPackageManagerConda.GetCmd: IPyPackageManagerCmdIntf;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TPyPackageManagerConda.GetDefs: TPyPackageManagerDefs;
begin
  raise ENotImplemented.Create('Not implemented');
end;

procedure TPyPackageManagerConda.Install;
begin

end;

function TPyPackageManagerConda.IsInstalled: boolean;
begin
  Result := false;
end;

procedure TPyPackageManagerConda.Uninstall;
begin

end;

end.
