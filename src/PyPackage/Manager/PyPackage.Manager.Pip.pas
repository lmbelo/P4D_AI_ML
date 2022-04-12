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
  PythonEngine,
  PyExecCmd, PyUtils, PyExceptions,
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
  LIn: TArray<string>;
  LOut: string;
begin
  LOpts := BuildOptsList();
  try
    LIn := ['-m', 'pip'] + FCmd.BuildListCmd(LOpts);
    if TPyExecCmdService
      .Cmd(GetPythonEngine().ProgramName, String.Join(' ', LIn))
        .Run(LOut)
          .Wait() = EXIT_SUCCESS then
            Result := LOut.Contains(FDefs.PackageName)
    else
      raise Exception.CreateFmt('Failed to validate %s installation.', [FDefs.PackageName]);
  finally
    LOpts.Free();
  end;
end;

function TPyPackageManagerPip.Install(out AOutput: string): boolean;
var
  LIn: TArray<string>;
begin
  LIn := ['-m', 'pip']
    + FCmd.BuildInstallCmd((FDefs as TPyPackageManagerDefsPip).InstallOptions);

   Result := TPyExecCmdService
    .Cmd(GetPythonEngine().ProgramName, String.Join(' ', LIn) + ' ' + FDefs.PackageName)
      .Run(AOutput)
        .Wait() = EXIT_SUCCESS;
end;

function TPyPackageManagerPip.Uninstall(out AOutput: string): boolean;
var
  LIn: TArray<string>;
begin
  LIn := ['-m', 'pip']
    + FCmd.BuildInstallCmd((FDefs as TPyPackageManagerDefsPip).UninstallOptions);

  Result := TPyExecCmdService
    .Cmd(GetPythonEngine().ProgramName, String.Join(' ', LIn) + ' ' + FDefs.PackageName)
      .Run(AOutput)
        .Wait() = EXIT_SUCCESS;
end;

end.
