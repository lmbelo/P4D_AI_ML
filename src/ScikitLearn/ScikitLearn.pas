(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'    Copyright (c) 2021                         *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                https://github.com/lmbelo/P4D_AI_ML      *)
(**************************************************************************)
(*  Functionality:  NumPy Components                                      *)
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
unit ScikitLearn;

interface

uses
  System.Classes, PyModule, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  //[PyPIPPackage('scikit-learn')]
  //[PyModuleName('sklearn')]
  TScikitLearn = class(TPyModule)
  private
    Fsklearn: variant;
    function Getsklean: variant;
  protected
    procedure ImportModule; override;
  public
    property sklearn: variant read Getsklean;
  end;

implementation

uses
  PyContext, VarPyth, System.Variants;

{ TScikitLearn }

function TScikitLearn.Getsklean: variant;
begin
  if VarIsNull(Fsklearn) or VarIsEmpty(Fsklearn) then
    Fsklearn := VarPythonCreate(PyModule);
  Result := Fsklearn;
end;

procedure TScikitLearn.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

initialization
  TPyContext.Instance
    .RegisterInfo(TScikitLearn)
      .RegisterPIPPackage('scikit-learn')
        .RegisterModule('sklearn');

finalization
  TPyContext.Instance
    .UnRegisterInfo(TScikitLearn);

end.
