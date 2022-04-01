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
  PyPackage.Manager.Cmd.Intf;

type
  TPyPackageManagerPip = class(TPyPackageManager, IPyPackageManager)
  private
    FDefs: TPyPackageManagerDefs;
    FCmd: IPyPackageManagerCmdIntf;

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
  System.Variants, System.SysUtils,
  PythonEngine,
  VarPyth, PyUtils, PyExceptions,
  PyPackage.Manager.Defs.Pip,
  PyPackage.Manager.Cmd.Pip;

{ TPyPackageManagerPip }

constructor TPyPackageManagerPip.Create(const APackageName: TPyPackageName);
begin
  inherited;
  FDefs := TPyPackageManagerDefsPip.Create(APackageName);
  FCmd := TPyPackageManagerCmdPip.Create();
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

function TPyPackageManagerPip.IsInstalled: boolean;
begin
  //Reloading the module garantees we're considering the latest installed packages
  var LPkgRes := Reload(Import('pkg_resources'));
  for var LPkg in VarPyIterate(LPkgRes.working_set) do begin
    if (LPkg.key = FDefs.PackageName) then
      Exit(true);
  end;
  Result := false;
end;

procedure TPyPackageManagerPip.Install;
begin
  //Using pip programmatically guarantees we're using the same Python interpreter
  //loaded by the application
  var LIn := FCmd.BuildInstallCmd(FDefs);
  var LPip := Import('pip');
  var LResult := LPip.main(TPyEx.List<String>(LIn));
  if LResult <> 0 then
    raise EPyModuleInstallError.CreateFmt(
      'An error occurred while installing the package %s.', [FDefs.PackageName]);
end;

procedure TPyPackageManagerPip.Uninstall;
begin
  //Using pip programmatically guarantees we're using the same Python interpreter
  //loaded by the application
  var LIn := FCmd.BuildUninstallCmd(FDefs);
  var LPip := Import('pip');
  var LResult := LPip.main(TPyEx.List<String>(LIn));
  if LResult <> 0 then
    raise EPyModuleInstallError.CreateFmt(
      'An error occurred while uninstalling the package %s.', [FDefs.PackageName]);
end;

end.
