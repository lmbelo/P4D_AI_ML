(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyUtils'          Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                   https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  Provide interop functionalities                       *)
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
unit PyUtils;

interface

uses
  System.Variants, System.Generics.Collections, VarPyth;

type
  TVarRecArray = array of TVarRec;
  
  TVariantHelper = record helper for variant
  private
    procedure CheckVarPython();
    procedure CheckVarIsArray();
  public       
    function AsList(): variant; //Created by a VarArrayOf 
    function AsTuple(): variant; //Created by a VarArrayOf 
    function GetEnumerator(): TVarPyEnumerateHelper;
  end;

  TVarRecArrayHelper = record helper for TVarRecArray
  public    
    function AsList(): variant; inline; //Created by the Collection function
    function AsTuple(): variant; inline; //Created by the Collection function
    function ToVarArray(): variant;
  end;

  TPyEx = record
  public
    class function Collection(const AArgs: array of const): TVarRecArray; static;
    class function Tuple(const AArgs: array of const): variant; static;
    class function List(const AArgs: array of const): variant; static;
    class function Dictionary(const AArgs: array of TPair<variant, variant>): variant; static;
  end;

implementation

uses
  System.Rtti, PyExceptions;

{ TVariantHelper }

function TVariantHelper.GetEnumerator: TVarPyEnumerateHelper;
begin
  CheckVarPython();
  Result := VarPyIterate(Self);
end;

function TVariantHelper.AsList: variant;
var
  LArr: TVarRecArray;
begin
  CheckVarIsArray();
  var LElem: PVariant := VarArrayLock(Self);
  try
    var LLen := VarArrayHighBound(Self, 1) - VarArrayLowBound(Self, 1) + 1;
    if LLen = 0 then
      Exit(LArr.AsList());
    SetLength(LArr, LLen);
    for var I := 0 to LLen - 1 do begin
      LArr[I] := TValue.FromVariant(LElem^).AsVarRec;
      Inc(LElem);
    end;
  finally
    VarArrayUnlock(Self);
  end;
  Result := LArr.AsList();
end;

function TVariantHelper.AsTuple: variant;
var      
  LArr: TVarRecArray;
begin
  CheckVarIsArray();
  var LElem: PVariant := VarArrayLock(Self);
  try
    var LLen := VarArrayHighBound(Self, 1) - VarArrayLowBound(Self, 1) + 1;
    if LLen = 0 then
      Exit(LArr.AsTuple());
    SetLength(LArr, LLen);
    for var I := 0 to LLen - 1 do begin
      LArr[I] := TValue.FromVariant(LElem^).AsVarRec;
      Inc(LElem);
    end;
  finally
    VarArrayUnlock(Self);
  end;
  Result := LArr.AsTuple();
end;

procedure TVariantHelper.CheckVarIsArray;
begin
  if not VarIsArray(Self) then
    raise EVariantNotAnArrayError.Create('Variant is not an array.');
end;

procedure TVariantHelper.CheckVarPython;
begin
  if not VarIsPython(Self) then
    raise EPyVarIsNotPython.Create(ErrVarIsNotPython);
end;

{ TConstArrayHelper }

function TVarRecArrayHelper.AsList: variant;
begin
  Result := VarPythonCreate(Self, stList);  
end;

function TVarRecArrayHelper.AsTuple: variant;
begin
  Result := VarPythonCreate(Self, stTuple);
end;

function TVarRecArrayHelper.ToVarArray: variant;
begin
  Result := VarArrayCreate([0, Length(Self) - 1], varVariant);
  for var I := Low(Self) to High(Self) do begin
    Result[I] := TValue.FromVarRec(Self[I]).AsVariant();
  end;
end;

class function TPyEx.Collection(const AArgs: array of const): TVarRecArray;
begin                                                        
  SetLength(Result, Length(AArgs));
  for var I := Low(AArgs) to High(AArgs) do
    Result[I] := AArgs[I];
end;

class function TPyEx.Tuple(const AArgs: array of const): variant;
begin
  Result := VarPythonCreate(AArgs, stTuple);
end;

class function TPyEx.List(const AArgs: array of const): variant;
begin
  Result := VarPythonCreate(AArgs, stList);
end;

class function TPyEx.Dictionary(const AArgs: array of TPair<variant, variant>): variant;
begin
  Result := NewPythonDict();
  for var LItem in AArgs do begin
    Result.SetItem(LItem.Key, LItem.Value);
  end;
end;

end.
