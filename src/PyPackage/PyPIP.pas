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
(*  Functionality:  PyPIP layer                                           *)
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
unit PyPIP;

interface

uses
  PyModule, System.SysUtils, System.Rtti;

type
  //http://dcjtech.info/wp-content/uploads/2015/10/Pip-Cheatsheet.pdf
  TPyPip = class
  private
    FPyModule: TPyModuleBase;
    function IsReady(): boolean;
    function GetPackageName(): string;
    function GetPackageVer(): string;
    function FmtPkgInstallCmd(): string;
  public
    constructor Create(const APyModule: TPyModuleBase);
    function IsInstalled(): boolean;
    procedure Install();
    procedure UnInstall();
  end;

  PyPIPPackageAttribute = class(TCustomAttribute)
  private
    FPyPackageName: string;
    FPyPackageVer: string;
  public
    constructor Create(const APyPackageName: string); overload;
    constructor Create(const APyPackageName, APyPackageVer: string); overload;

    property PyPackageName: string read FPyPackageName write FPyPackageName;
    property PyPackageVer: string read FPyPackageVer write FPyPackageVer;
  end;

  EModuleNotReady = class(Exception)
  end;

implementation

uses
  VarPyth, PythonEngine, System.Variants;

{ TPyPip }

function TPyPip.IsReady(): boolean;
begin
  Result := Assigned(FPyModule)
        and Assigned(FPyModule.PythonEngine)
        and FPyModule.PythonEngine.Initialized;
end;

function TPyPip.IsInstalled(): boolean;
begin
  Result := false;
  var package := GetPackageName();
  if IsReady() then begin
    var sp := Import('subprocess');
    var cmd := 'pip show ' + package;
    var resp := sp.run(cmd, stdout := sp.PIPE, stderr := sp.STDOUT, shell := true, text := true);
    var stdout := resp.stdout;
    if VarIsPythonString(stdout) then begin
      Result := not (String(stdout).StartsWith('WARNING:') and String(stdout).Contains('Package(s) not found'));
    end;
  end else raise EModuleNotReady.CreateFmt('Module %s not ready', [package]);
end;

constructor TPyPip.Create(const APyModule: TPyModuleBase);
begin
  FPyModule := APyModule;
end;

function TPyPip.FmtPkgInstallCmd: string;
begin
  var LVer := GetPackageVer();
  if not LVer.IsEmpty then
    Result := GetPackageName() + '==' + LVer
  else
    Result := GetPackageName();
end;

function TPyPip.GetPackageName(): string;
var
  LAttr: TCustomAttribute;
begin
//  var LCtx := TRttiContext.Create();
//  try
//    var LType := LCtx.GetType(ClassType);
//    for LAttr in LType.GetAttributes() do begin
//      if LAttr is PyPIPAttribute then begin
//        Exit(PyPIPAttribute(LAttr).PyPackageName);
//      end;
//    end;
//  finally
//    LCtx.Free();
//  end;

  Result := FPyModule.PyModuleName;
end;

function TPyPip.GetPackageVer: string;
var
  LAttr: TCustomAttribute;
begin
//  var LCtx := TRttiContext.Create();
//  try
//    var LType := LCtx.GetType(FPyModule.ClassType);
//    for LAttr in LType.GetAttributes() do begin
//      if LAttr is PyPIPAttribute then begin
//        Exit(PyPIPAttribute(LAttr).PyPackageVer);
//      end;
//    end;
//  finally
//    LCtx.Free();
//  end;
  Result := String.Empty;
end;

procedure TPyPip.Install();
begin
  if IsReady() then begin
    var sp := Import('subprocess');
    var cmd := 'pip install ' + FmtPkgInstallCmd();
    var exec := sp.run(cmd, stdout := sp.PIPE, stderr := sp.STDOUT, shell := true, text := true);
  end else raise EModuleNotReady.CreateFmt('Module %s not ready', [GetPackageName()]);
end;

procedure TPyPip.UnInstall();
begin
  if IsReady() then begin
    var sp := Import('subprocess');
    var cmd := 'pip uninstall ' + GetPackageName() + ' -y';
    sp.run(cmd, stdout := sp.PIPE, stderr := sp.STDOUT, shell := true, text := true);
  end else raise EModuleNotReady.CreateFmt('Module %s not ready', [GetPackageName()]);
end;

{ PyPIPAttribute }

constructor PyPIPPackageAttribute.Create(const APyPackageName: string);
begin
  FPyPackageName := APyPackageName;
end;

constructor PyPIPPackageAttribute.Create(const APyPackageName, APyPackageVer: string);
begin
  Create(APyPackageName);
  FPyPackageVer := APyPackageVer;
end;

end.
