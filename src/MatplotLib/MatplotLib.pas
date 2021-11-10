(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MatplotLib'       Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(* Project page:                    https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  MatplotLib Components                                 *)
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
unit MatplotLib;

interface

uses
  System.Classes, PyModule, PythonEngine;

type
  TPyPlot = class(TPyModule)
  private
    FPyPlot: variant;
    function GetPyPlot: variant;
  public
    property plt: variant read GetPyPlot;
  end;

  [ComponentPlatforms(pidAllPlatforms)]
//  [PyPIPPackage('matplotlib')]
//  [PyModuleName('matplotlib')]
  TMatplotLib = class(TPyModule)
  private
    FMatplotLib: variant;
    function GetMatplotLib: variant;
    function GetPyPlot: variant;
  public
    property matplot: variant read GetMatplotLib;
    property plt: variant read GetPyPlot;
  end;

implementation

uses
  PyContext, VarPyth, System.Variants;

{ TMatplotLib }

function TMatplotLib.GetMatplotLib: variant;
begin
  CheckImported();
  if VarIsNull(FMatplotLib) or VarIsEmpty(FMatplotLib) then
    FMatplotLib := VarPythonCreate(PyModule);
  Result := FMatplotLib;
end;

function TMatplotLib.GetPyPlot: variant;
begin
  CheckSubModule(TPyPlot);
  var LSubModule := TPyPlot(GetSubModule(TPyPlot));
  if Assigned(LSubModule) then
    Result := LSubModule.plt;
end;

{ TPyPlot }

function TPyPlot.GetPyPlot: variant;
begin
  CheckImported();
  if VarIsNull(FPyPlot) or VarIsEmpty(FPyPlot) then
    FPyPlot := VarPythonCreate(PyModule);
  Result := FPyPlot;
end;

initialization
  TPyContext.Instance
    .RegisterInfo(TMatplotLib)
      .RegisterPIPPackage('matplotlib')
        .RegisterModule('matplotlib')
          .RegisterSubModule(TPyPlot, 'pyplot');

finalization
  TPyContext.Instance
    .UnRegisterInfo(TMatplotLib);

end.
