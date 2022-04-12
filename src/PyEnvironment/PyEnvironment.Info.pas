(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironments.PyEnvironment'                           *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyEnvironments Enfironment Info                       *)
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
unit PyEnvironment.Info;

interface

uses
  System.Classes;

type
  TPyEnvironmentInfo = class abstract(TCollectionItem)
  private
    FPythonVersion: string;
    FHome: string;
    FSharedLibrary: string;
    FExecutable: string;
  public
    procedure Setup(); virtual; abstract;
  published
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property Home: string read FHome write FHome;
    property SharedLibrary: string read FSharedLibrary write FSharedLibrary;
    property Executable: string read FExecutable write FExecutable;
  end;

  TPyEnvironmentCollection = class abstract(TOwnedCollection)
  public
    function LocateEnvironment(APythonVersion: string): TPyEnvironmentInfo; virtual;
  end;

implementation

{ TPyEnvironmentCollection }

function TPyEnvironmentCollection.LocateEnvironment(
  APythonVersion: string): TPyEnvironmentInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    if (TPyEnvironmentInfo(Items[I]).PythonVersion = APythonVersion) then
      Exit(TPyEnvironmentInfo(Items[I]));
  end;
  Result := nil;
end;

end.
