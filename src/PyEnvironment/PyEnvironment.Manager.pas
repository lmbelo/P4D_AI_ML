(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyEnvironment.Manager'                                  *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                https://github.com/lmbelo/P4D_AI_ML      *)
(**************************************************************************)
(*  Functionality:  PyEnvironment layer                                   *)
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
unit PyEnvironment.Manager;

interface

uses
  System.Classes,
  PythonEngine;

type
  TPyManager = class(TComponent)
  private
    FEnabled: boolean;
    FPythonVersion: string;
    FPythonEngine: TPythonEngine;
    procedure SetPythonEngine(const Value: TPythonEngine);
  protected
    procedure Loaded(); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled: boolean read FEnabled write FEnabled default true;
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property PythonEngine: TPythonEngine read FPythonEngine write SetPythonEngine;
  end;

implementation

uses
  PyEnvironment.Collection;

{ TPyManager }

constructor TPyManager.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
end;

procedure TPyManager.Loaded;
begin
  inherited;
  if Assigned(FPythonEngine) and FEnabled then
    TPyEnvironmentCollection.Instance.UseEnvironment(FPythonVersion, FPythonEngine);
end;

procedure TPyManager.SetPythonEngine(const Value: TPythonEngine);
begin
  if (Value <> FPythonEngine) then begin
    FPythonEngine := Value;
    if Assigned(FPythonEngine) then
      FPythonEngine.AutoLoad := false;
  end;
end;

end.
