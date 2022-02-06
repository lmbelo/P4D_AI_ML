unit PyPackage.Manager.Cmd.Pip;

interface

uses
  System.SysUtils,
  PyPackage.Manager.Cmd.Intf,
  PyPackage.Manager.Defs,
  PyPackage.Manager.Defs.Pip,
  PyPackage.Manager.Defs.InstallOpts.Pip;

type
  TPyPackageManagerCmdPip = class(TInterfacedObject, IPyPackageManagerCmdIntf)
  private
    FDefs: TPyPackageManagerDefsPip;
  private
    //Defs cmds
    function MakePackageCmd: TArray<string>; inline;
    //Install opts cmds
    function MakeInstallRequirementCmd: TArray<string>; inline;
    function MakeInstallConstraintCmd: TArray<string>; inline;
    function MakeInstallNoDepsCmd: TArray<string>; inline;
    function MakeInstallPreCmd: TArray<string>; inline;
    function MakeInstallEditableCmd: TArray<string>; inline;
    function MakeInstallTargetCmd: TArray<string>; inline;
    function MakeInstallPlatformCmd: TArray<string>; inline;
    function MakeInstallPythonVersionCmd: TArray<string>; inline;
    function MakeIntallPythonImplementationCmd: TArray<string>; inline;
    function MakeInstallAbiCmd: TArray<string>; inline;
    function MakeInstallUserCmd: TArray<string>; inline;
    function MakeInstallRootCmd: TArray<string>; inline;
    function MakeInstallPrefixCmd: TArray<string>; inline;
    function MakeInstallSrcCmd: TArray<string>; inline;
    function MakeInstallUpgradeCmd: TArray<string>; inline;
    function MakeInstallUpgradeStrategyCmd: TArray<string>; inline;
    function MakeInstallForceReinstallCmd: TArray<string>; inline;
    function MakeInstallIgnoreInstalledCmd: TArray<string>; inline;
    function MakeInstallIgnoreRequiresPythonCmd: TArray<string>; inline;
    function MakeInstallNoBuildIsolationCmd: TArray<string>; inline;
    function MakeInstallUsePe517Cmd: TArray<string>; inline;
    function MakeInstallOptionCmd: TArray<string>; inline;
    function MakeInstallGlobalOptionCmd: TArray<string>; inline;
    function MakeInstallCompileCmd: TArray<string>; inline;
    function MakeInstallNoCompileCmd: TArray<string>; inline;
    function MakeInstallNoWarnScriptLocationCmd: TArray<string>; inline;
    function MakeInstallNoWarnConflictsCmd: TArray<string>; inline;
    function MakeInstallNoBinaryCmd: TArray<string>; inline;
    function MakeInstallOnlyBinaryCmd: TArray<string>; inline;
    function MakeInstallPreferBinaryCmd: TArray<string>; inline;
    function MakeInstallRequireHashesCmd: TArray<string>; inline;
    function MakeInstallProgressBarCmd: TArray<string>; inline;
    function MakeInstallNoCleanCmd: TArray<string>; inline;
    function MakeInstallIndexUrlCmd: TArray<string>; inline;
    function MakeInstallExtraIndexUrlCmd: TArray<string>; inline;
    function MakeInstallNoIndexCmd: TArray<string>; inline;
    function MakeInstallFindLinksCmd: TArray<string>; inline;
    //Uninstall cmds
    function MakeUninstallRequirementCmd: TArray<string>; inline;
    function MakeUninstallConfirmationFlagCmd: TArray<string>; inline;
  public
    function BuildInstallCmd(const ADefs: TPyPackageManagerDefs): TArray<string>;
    function BuildUninstallCmd(const ADefs: TPyPackageManagerDefs): TArray<string>;
  end;

implementation

{ TPyPackageManagerCmdPip }

function TPyPackageManagerCmdPip.BuildInstallCmd(
  const ADefs: TPyPackageManagerDefs): TArray<string>;
begin
  FDefs := ADefs as TPyPackageManagerDefsPip;
  try
    Result := TArray<string>.Create('install')
      + MakePackageCmd()
      + MakeInstallRequirementCmd()
      + MakeInstallConstraintCmd()
      + MakeInstallNoDepsCmd()
      + MakeInstallPreCmd()
      + MakeInstallEditableCmd()
      + MakeInstallTargetCmd()
      + MakeInstallPlatformCmd()
      + MakeInstallPythonVersionCmd()
      + MakeIntallPythonImplementationCmd()
      + MakeInstallAbiCmd()
      + MakeInstallUserCmd()
      + MakeInstallRootCmd()
      + MakeInstallPrefixCmd()
      + MakeInstallSrcCmd()
      + MakeInstallUpgradeCmd()
      + MakeInstallUpgradeStrategyCmd()
      + MakeInstallForceReinstallCmd()
      + MakeInstallIgnoreInstalledCmd()
      + MakeInstallIgnoreRequiresPythonCmd()
      + MakeInstallNoBuildIsolationCmd()
      + MakeInstallUsePe517Cmd()
      + MakeInstallOptionCmd()
      + MakeInstallGlobalOptionCmd()
      + MakeInstallCompileCmd()
      + MakeInstallNoCompileCmd()
      + MakeInstallNoWarnScriptLocationCmd()
      + MakeInstallNoWarnConflictsCmd()
      + MakeInstallNoBinaryCmd()
      + MakeInstallOnlyBinaryCmd()
      + MakeInstallPreferBinaryCmd()
      + MakeInstallRequireHashesCmd()
      + MakeInstallProgressBarCmd()
      + MakeInstallNoCleanCmd()
      + MakeInstallIndexUrlCmd()
      + MakeInstallExtraIndexUrlCmd()
      + MakeInstallNoIndexCmd()
      + MakeInstallFindLinksCmd();
  finally
    FDefs := nil;
  end;
end;

function TPyPackageManagerCmdPip.BuildUninstallCmd(
  const ADefs: TPyPackageManagerDefs): TArray<string>;
begin
  FDefs := ADefs as TPyPackageManagerDefsPip;
  try
    Result := TArray<string>.Create('uninstall')
      + MakePackageCmd()
      + MakeUninstallRequirementCmd()
      + MakeUninstallConfirmationFlagCmd();
  finally
    FDefs := nil;
  end;
end;

function TPyPackageManagerCmdPip.MakePackageCmd: TArray<string>;
begin
  var LVersion: string;
  if not FDefs.PackageVersion.IsEmpty() then
    LVersion := FDefs.PackageVersion
  else
    LVersion := String.Empty;

  Result := TArray<string>.Create(FDefs.PackageName + LVersion);
end;

//Install
function TPyPackageManagerCmdPip.MakeInstallAbiCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Abi.IsEmpty() then
    Result := TArray<string>.Create('--abi', FDefs.InstallOptions.Abi);
end;

function TPyPackageManagerCmdPip.MakeInstallCompileCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Compile then
    Result := TArray<string>.Create('--compile');
end;

function TPyPackageManagerCmdPip.MakeInstallConstraintCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Constraint.IsEmpty() then
    Result := TArray<string>.Create('-c', FDefs.InstallOptions.Constraint);
end;

function TPyPackageManagerCmdPip.MakeInstallEditableCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Editable.IsEmpty() then
    Result := TArray<string>.Create('-e', FDefs.InstallOptions.Editable);
end;

function TPyPackageManagerCmdPip.MakeInstallExtraIndexUrlCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.ExtraIndexUrl.IsEmpty() then
    Result := TArray<string>.Create('--extra-index-url', FDefs.InstallOptions.ExtraIndexUrl);
end;

function TPyPackageManagerCmdPip.MakeInstallFindLinksCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.FindLinks.IsEmpty() then
    Result := TArray<string>.Create('-f', FDefs.InstallOptions.FindLinks);
end;

function TPyPackageManagerCmdPip.MakeInstallForceReinstallCmd: TArray<string>;
begin
  if FDefs.InstallOptions.ForceReinstall then
    Result := TArray<string>.Create('--force-reinstall');
end;

function TPyPackageManagerCmdPip.MakeInstallGlobalOptionCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.GlobalOption.IsEmpty() then
    Result := TArray<string>.Create('--global-option', FDefs.InstallOptions.GlobalOption);
end;

function TPyPackageManagerCmdPip.MakeInstallIgnoreInstalledCmd: TArray<string>;
begin
  if FDefs.InstallOptions.IgnoreInstalled then
    Result := TArray<string>.Create('-I');
end;

function TPyPackageManagerCmdPip.MakeInstallIgnoreRequiresPythonCmd: TArray<string>;
begin
  if FDefs.InstallOptions.IgnoreRequiresPython then
    Result := TArray<string>.Create('--ignore-requires-python');
end;

function TPyPackageManagerCmdPip.MakeInstallIndexUrlCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.IndexUrl.IsEmpty() then
    Result := TArray<string>.Create('-i', FDefs.InstallOptions.IndexUrl);
end;

function TPyPackageManagerCmdPip.MakeInstallOptionCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.InstallOption.IsEmpty() then
    Result := TArray<string>.Create('--install-option', FDefs.InstallOptions.InstallOption);
end;

function TPyPackageManagerCmdPip.MakeInstallNoBinaryCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoBinary then
    Result := TArray<string>.Create('--no-binary');
end;

function TPyPackageManagerCmdPip.MakeInstallNoBuildIsolationCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoBuildIsolation then
    Result := TArray<string>.Create('--no-build-isolation');
end;

function TPyPackageManagerCmdPip.MakeInstallNoCleanCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoClean then
    Result := TArray<string>.Create('--no-clean');
end;

function TPyPackageManagerCmdPip.MakeInstallNoCompileCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoCompile then
    Result := TArray<string>.Create('--no-compile');
end;

function TPyPackageManagerCmdPip.MakeInstallNoDepsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoDeps then
    Result := TArray<string>.Create('--no-deps');
end;

function TPyPackageManagerCmdPip.MakeInstallNoIndexCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoIndex then
    Result := TArray<string>.Create('--no-index');
end;

function TPyPackageManagerCmdPip.MakeInstallNoWarnConflictsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoWarnConflicts then
    Result := TArray<string>.Create('--no-warn-conflicts');
end;

function TPyPackageManagerCmdPip.MakeInstallNoWarnScriptLocationCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoWarnScriptLocation then
    Result := TArray<string>.Create('--no-warn-script-location');
end;

function TPyPackageManagerCmdPip.MakeInstallOnlyBinaryCmd: TArray<string>;
begin
  if FDefs.InstallOptions.OnlyBinary then
    Result := TArray<string>.Create('--only-binary');
end;

function TPyPackageManagerCmdPip.MakeInstallPlatformCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Platform.IsEmpty() then
    Result := TArray<string>.Create('--platform ', FDefs.InstallOptions.Platform);
end;

function TPyPackageManagerCmdPip.MakeInstallPreCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Pre then
    Result := TArray<string>.Create('--pre');
end;

function TPyPackageManagerCmdPip.MakeInstallPreferBinaryCmd: TArray<string>;
begin
  if FDefs.InstallOptions.PreferBinary then
    Result := TArray<string>.Create('--prefer-binary');
end;

function TPyPackageManagerCmdPip.MakeInstallPrefixCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Prefix.IsEmpty() then
    Result := TArray<string>.Create('--prefix', FDefs.InstallOptions.Prefix);
end;

function TPyPackageManagerCmdPip.MakeInstallProgressBarCmd: TArray<string>;
begin
  if FDefs.InstallOptions.ProgressBar then
    Result := TArray<string>.Create('--progress-bar');
end;

function TPyPackageManagerCmdPip.MakeIntallPythonImplementationCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.PythonImplementation.IsEmpty() then
    Result := TArray<string>.Create('--implementation', FDefs.InstallOptions.PythonImplementation);
end;

function TPyPackageManagerCmdPip.MakeInstallPythonVersionCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.PythonVersion.IsEmpty() then
    Result := TArray<string>.Create('--python-version', FDefs.InstallOptions.PythonVersion);
end;

function TPyPackageManagerCmdPip.MakeInstallRequireHashesCmd: TArray<string>;
begin
  if FDefs.InstallOptions.RequireHashes then
    Result := TArray<string>.Create('--require-hashes');
end;

function TPyPackageManagerCmdPip.MakeInstallRequirementCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Requirement.IsEmpty() then
    Result := TArray<string>.Create('-r', FDefs.InstallOptions.Requirement);
end;

function TPyPackageManagerCmdPip.MakeInstallRootCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Root.IsEmpty() then
    Result := TArray<string>.Create('--root', FDefs.InstallOptions.Root);
end;

function TPyPackageManagerCmdPip.MakeInstallSrcCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Source.IsEmpty() then
    Result := TArray<string>.Create('--src', FDefs.InstallOptions.Source);
end;

function TPyPackageManagerCmdPip.MakeInstallTargetCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Target.IsEmpty() then
    Result := TArray<string>.Create('-t ', FDefs.InstallOptions.Target);
end;

function TPyPackageManagerCmdPip.MakeInstallUpgradeCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Upgrade then
    Result := TArray<string>.Create('-U');
end;

function TPyPackageManagerCmdPip.MakeInstallUpgradeStrategyCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.UpgradeStrategy.IsEmpty() then
    Result := TArray<string>.Create('--upgrade-strategy', FDefs.InstallOptions.UpgradeStrategy);
end;

function TPyPackageManagerCmdPip.MakeInstallUsePe517Cmd: TArray<string>;
begin
  if FDefs.InstallOptions.UsePep517 then
    Result := TArray<string>.Create('--use-pep517');
end;

function TPyPackageManagerCmdPip.MakeInstallUserCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.User.IsEmpty() then
    Result := TArray<string>.Create('--user', FDefs.InstallOptions.User);
end;

//Uninstall
function TPyPackageManagerCmdPip.MakeUninstallRequirementCmd: TArray<string>;
begin
  if not FDefs.UninstallOptions.Requirement.IsEmpty() then
    Result := TArray<string>.Create('-r', FDefs.UninstallOptions.Requirement);
end;

function TPyPackageManagerCmdPip.MakeUninstallConfirmationFlagCmd: TArray<string>;
begin
  if not FDefs.UninstallOptions.AskForConfirmation then
    Result := TArray<string>.Create('-y');
end;

end.
