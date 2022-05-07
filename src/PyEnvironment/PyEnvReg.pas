(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvReg'         Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironments Components registration                *)
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
unit PyEnvReg;

interface

procedure Register();

implementation

uses
  Classes,
  PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Local,
  PyEnvironment.AddOn,
  PyEnvironment.AddOn.GetPip,
  PyEnvironment.AddOn.EnsurePip;

procedure Register();
begin
  RegisterComponents('Python - Environments', [
    //Environments
    TPyEmbeddedEnvironment,
    TPyLocalEnvironment,
    //Add-ons
    TPyEnvironmentAddOns,
    TPyEnvironmentAddOn,
    TPyEnvironmentAddOnGetPip,
    TPyEnvironmentAddOnEnsurePip]);
end;

end.
