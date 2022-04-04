(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyPackage.Manager.Pip'                                  *)
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
unit PyPackage.Manager.Pip;

interface

uses
  PyCore, PyPackage,
  PyPackage.Manager,
  PyPackage.Manager.Intf,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf,
  PyPackage.Manager.Defs.Opts.Pip.List;

type
  TPyPackageManagerPip = class(TPyPackageManager, IPyPackageManager)
  private
    FDefs: TPyPackageManagerDefs;
    FCmd: IPyPackageManagerCmdIntf;
    //Builders
    function BuildOptsList(): TPyPackageManagerDefsOptsPipList;
    //IPyPackageManager implementation
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
    function IsInstalled(): boolean; reintroduce;
    function Install(out AOutput: string): boolean;
    function Uninstall(out AOutput: string): boolean;
  public
    constructor Create(const APackageName: TPyPackageName); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Variants, System.SysUtils,
  PythonEngine, VarPyth,
  PyUtils, PyExceptions,
  PyPackage.Manager.Defs.Pip,
  PyPackage.Manager.Cmd.Pip;

{ TPyPackageManagerPip }

function TPyPackageManagerPip.BuildOptsList: TPyPackageManagerDefsOptsPipList;
begin
  Result := TPyPackageManagerDefsOptsPipList.Create();
  try
    Result.User := (FDefs as TPyPackageManagerDefsPip).InstallOptions.User;
  except
    on E: Exception do begin
      Result.Free();
      raise;
    end;
  end;
end;

constructor TPyPackageManagerPip.Create(const APackageName: TPyPackageName);
begin
  inherited;
  FDefs := TPyPackageManagerDefsPip.Create(APackageName);
  FCmd := TPyPackageManagerCmdPip.Create(FDefs);
end;

destructor TPyPackageManagerPip.Destroy;
begin
  FCmd := nil;
  FDefs.Free();
  inherited;
end;

function TPyPackageManagerPip.GetCmd: IPyPackageManagerCmdIntf;
begin
  Result := FCmd;
end;

function TPyPackageManagerPip.GetDefs: TPyPackageManagerDefs;
begin
  Result := FDefs;
end;

function TPyPackageManagerPip.IsInstalled(): boolean;
var
  LOpts: TPyPackageManagerDefsOptsPipList;
  LSubproc: variant;
  LIn: TArray<string>;
  LOut: string;
begin
  LOpts := BuildOptsList();
  try
    LSubproc := Import('subprocess');
    LIn := FCmd.BuildListCmd(LOpts);
    LOut := LSubproc.check_output(TPyEx.List<String>(
      [GetPythonExe(), '-m', 'pip'] + LIn), shell:=true);
    Result := LOut.Contains(FDefs.PackageName);
  finally
    LOpts.Free();
  end;
end;

function TPyPackageManagerPip.Install(out AOutput: string): boolean;
var
  LSubproc: variant;
  LIn: TArray<string>;
  LOut: variant;
begin
  LSubproc := Import('subprocess');
  LIn := FCmd.BuildInstallCmd((FDefs as TPyPackageManagerDefsPip).InstallOptions);
  LOut := LSubproc.run(TPyEx.List<String>(
    [GetPythonExe(), '-m', 'pip'] + LIn + [FDefs.PackageName]),
    capture_output:=true, text:=true, shell:=true);
  Result := LOut.returncode = 0;
  if not Result then
    AOutput := LOut.stderr
  else
    AOutput := LOut.stdout;
end;

function TPyPackageManagerPip.Uninstall(out AOutput: string): boolean;
var
  LSubproc: variant;
  LIn: TArray<string>;
  LOut: variant;
begin
  LSubproc := Import('subprocess');
  LIn := FCmd.BuildUninstallCmd((FDefs as TPyPackageManagerDefsPip).UninstallOptions);
  LOut := LSubproc.run(TPyEx.List<String>(
    [GetPythonExe(), '-m', 'pip'] + LIn + [FDefs.PackageName]),
    capture_output:=true, text:=true, shell:=true);
  Result := LOut.returncode = 0;
  if not Result then
    AOutput := LOut.stderr
  else
    AOutput := LOut.stdout;
end;

end.
