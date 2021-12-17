unit PyPackage.Manager.Cmd.Pip;

interface

uses
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
    function MakePackageCmd: string; inline;
    function MakePackageVersionCmd: string; inline;
    //Install opts cmds
    function MakeInstallRequirementCmd: string; inline;
    function MakeInstallConstraintCmd: string; inline;
    function MakeInstallNoDepsCmd: string; inline;
    function MakeInstallPreCmd: string; inline;
    function MakeInstallEditableCmd: string; inline;
    function MakeInstallTargetCmd: string; inline;
    function MakeInstallPlatformCmd: string; inline;
    function MakeInstallPythonVersionCmd: string; inline;
    function MakeIntallPythonImplementationCmd: string; inline;
    function MakeInstallAbiCmd: string; inline;
    function MakeInstallUserCmd: string; inline;
    function MakeInstallRootCmd: string; inline;
    function MakeInstallPrefixCmd: string; inline;
    function MakeInstallSrcCmd: string; inline;
    function MakeInstallUpgradeCmd: string; inline;
    function MakeInstallUpgradeStrategyCmd: string; inline;
    function MakeInstallForceReinstallCmd: string; inline;
    function MakeInstallIgnoreInstalledCmd: string; inline;
    function MakeInstallIgnoreRequiresPythonCmd: string; inline;
    function MakeInstallNoBuildIsolationCmd: string; inline;
    function MakeInstallUsePe517Cmd: string; inline;
    function MakeInstallOptionCmd: string; inline;
    function MakeInstallGlobalOptionCmd: string; inline;
    function MakeInstallCompileCmd: string; inline;
    function MakeInstallNoCompileCmd: string; inline;
    function MakeInstallNoWarnScriptLocationCmd: string; inline;
    function MakeInstallNoWarnConflictsCmd: string; inline;
    function MakeInstallNoBinaryCmd: string; inline;
    function MakeInstallOnlyBinaryCmd: string; inline;
    function MakeInstallPreferBinaryCmd: string; inline;
    function MakeInstallRequireHashesCmd: string; inline;
    function MakeInstallProgressBarCmd: string; inline;
    function MakeInstallNoCleanCmd: string; inline;
    function MakeInstallIndexUrlCmd: string; inline;
    function MakeInstallExtraIndexUrlCmd: string; inline;
    function MakeInstallNoIndexCmd: string; inline;
    function MakeInstallFindLinksCmd: string; inline;
    //Uninstall cmds
    function MakeUninstallRequirementCmd: string; inline;
    function MakeUninstallConfirmationFlagCmd: string; inline;
  public
    function BuildInstallCmd(const ADefs: TPyPackageManagerDefs): string;
    function BuildUninstallCmd(const ADefs: TPyPackageManagerDefs): string;
    function BuildIsInstalledCmd(const ADefs: TPyPackageManagerDefs): string;
  end;

implementation

uses
  System.SysUtils;

{ TPyPackageManagerCmdPip }

function TPyPackageManagerCmdPip.BuildInstallCmd(
  const ADefs: TPyPackageManagerDefs): string;
begin
  FDefs := ADefs as TPyPackageManagerDefsPip;
  try
    Result := ' pip install'
      + MakePackageCmd()
      + MakePackageVersionCmd()
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

function TPyPackageManagerCmdPip.BuildIsInstalledCmd(
  const ADefs: TPyPackageManagerDefs): string;
begin
  FDefs := ADefs as TPyPackageManagerDefsPip;
  try
    Result := ' pip list';
  finally
    FDefs := nil;
  end;
end;

function TPyPackageManagerCmdPip.BuildUninstallCmd(
  const ADefs: TPyPackageManagerDefs): string;
begin
  FDefs := ADefs as TPyPackageManagerDefsPip;
  try
    Result := ' pip uninstall'
      + MakePackageCmd()
      + MakeUninstallRequirementCmd()
      + MakeUninstallConfirmationFlagCmd();
  finally
    FDefs := nil;
  end;
end;

function TPyPackageManagerCmdPip.MakeInstallAbiCmd: string;
begin
  if not FDefs.InstallOptions.Abi.IsEmpty() then
    Result := ' --abi ' + FDefs.InstallOptions.Abi;
end;

function TPyPackageManagerCmdPip.MakeInstallCompileCmd: string;
begin
  if FDefs.InstallOptions.Compile then
    Result := ' --compile';
end;

function TPyPackageManagerCmdPip.MakeUninstallConfirmationFlagCmd: string;
begin
  if not FDefs.UninstallOptions.AskForConfirmation then
    Result := ' -y';
end;

function TPyPackageManagerCmdPip.MakeUninstallRequirementCmd: string;
begin
  if not FDefs.UninstallOptions.Requirement.IsEmpty() then
    Result := ' -r ' + FDefs.UninstallOptions.Requirement;
end;

function TPyPackageManagerCmdPip.MakeInstallConstraintCmd: string;
begin
  if not FDefs.InstallOptions.Constraint.IsEmpty() then
    Result := ' -c ' + FDefs.InstallOptions.Constraint;
end;

function TPyPackageManagerCmdPip.MakeInstallEditableCmd: string;
begin
  if not FDefs.InstallOptions.Editable.IsEmpty() then
    Result := ' -e ' + FDefs.InstallOptions.Editable;
end;

function TPyPackageManagerCmdPip.MakeInstallExtraIndexUrlCmd: string;
begin
  if not FDefs.InstallOptions.ExtraIndexUrl.IsEmpty() then
    Result := ' --extra-index-url ' + FDefs.InstallOptions.ExtraIndexUrl;
end;

function TPyPackageManagerCmdPip.MakeInstallFindLinksCmd: string;
begin
  if not FDefs.InstallOptions.FindLinks.IsEmpty() then
    Result := ' -f ' + FDefs.InstallOptions.FindLinks;
end;

function TPyPackageManagerCmdPip.MakeInstallForceReinstallCmd: string;
begin
  if FDefs.InstallOptions.ForceReinstall then
    Result := ' --force-reinstall';
end;

function TPyPackageManagerCmdPip.MakeInstallGlobalOptionCmd: string;
begin
  if not FDefs.InstallOptions.GlobalOption.IsEmpty() then
    Result := ' --global-option ' + FDefs.InstallOptions.GlobalOption;
end;

function TPyPackageManagerCmdPip.MakeInstallIgnoreInstalledCmd: string;
begin
  if FDefs.InstallOptions.IgnoreInstalled then
    Result := ' -I';
end;

function TPyPackageManagerCmdPip.MakeInstallIgnoreRequiresPythonCmd: string;
begin
  if FDefs.InstallOptions.IgnoreRequiresPython then
    Result := ' --ignore-requires-python';
end;

function TPyPackageManagerCmdPip.MakeInstallIndexUrlCmd: string;
begin
  if not FDefs.InstallOptions.IndexUrl.IsEmpty() then
    Result := ' -i ' + FDefs.InstallOptions.IndexUrl;
end;

function TPyPackageManagerCmdPip.MakeInstallOptionCmd: string;
begin
  if not FDefs.InstallOptions.InstallOption.IsEmpty() then
    Result := ' --install-option ' + FDefs.InstallOptions.InstallOption;
end;

function TPyPackageManagerCmdPip.MakeInstallNoBinaryCmd: string;
begin
  if FDefs.InstallOptions.NoBinary then
    Result := ' --no-binary';
end;

function TPyPackageManagerCmdPip.MakeInstallNoBuildIsolationCmd: string;
begin
  if FDefs.InstallOptions.NoBuildIsolation then
    Result := ' --no-build-isolation';
end;

function TPyPackageManagerCmdPip.MakeInstallNoCleanCmd: string;
begin
  if FDefs.InstallOptions.NoClean then
    Result := ' --no-clean';
end;

function TPyPackageManagerCmdPip.MakeInstallNoCompileCmd: string;
begin
  if FDefs.InstallOptions.NoCompile then
    Result := ' --no-compile';
end;

function TPyPackageManagerCmdPip.MakeInstallNoDepsCmd: string;
begin
  if FDefs.InstallOptions.NoDeps then
    Result := ' --no-deps';
end;

function TPyPackageManagerCmdPip.MakeInstallNoIndexCmd: string;
begin
  if FDefs.InstallOptions.NoIndex then
    Result := ' --no-index';
end;

function TPyPackageManagerCmdPip.MakeInstallNoWarnConflictsCmd: string;
begin
  if FDefs.InstallOptions.NoWarnConflicts then
    Result := ' --no-warn-conflicts';
end;

function TPyPackageManagerCmdPip.MakeInstallNoWarnScriptLocationCmd: string;
begin
  if FDefs.InstallOptions.NoWarnScriptLocation then
    Result := ' --no-warn-script-location';
end;

function TPyPackageManagerCmdPip.MakeInstallOnlyBinaryCmd: string;
begin
  if FDefs.InstallOptions.OnlyBinary then
    Result := ' --only-binary';
end;

function TPyPackageManagerCmdPip.MakePackageCmd: string;
begin
  Result := ' ' + FDefs.PackageName;
end;

function TPyPackageManagerCmdPip.MakePackageVersionCmd: string;
begin
  if not FDefs.PackageVersion.IsEmpty() then
    Result := ' ' + FDefs.PackageVersion;
end;

function TPyPackageManagerCmdPip.MakeInstallPlatformCmd: string;
begin
  if not FDefs.InstallOptions.Platform.IsEmpty() then
    Result := ' --platform ' + FDefs.InstallOptions.Platform;
end;

function TPyPackageManagerCmdPip.MakeInstallPreCmd: string;
begin
  if FDefs.InstallOptions.Pre then
    Result := ' --pre';
end;

function TPyPackageManagerCmdPip.MakeInstallPreferBinaryCmd: string;
begin
  if FDefs.InstallOptions.PreferBinary then
    Result := ' --prefer-binary';
end;

function TPyPackageManagerCmdPip.MakeInstallPrefixCmd: string;
begin
  if not FDefs.InstallOptions.Prefix.IsEmpty() then
    Result := ' --prefix ' + FDefs.InstallOptions.Prefix;
end;

function TPyPackageManagerCmdPip.MakeInstallProgressBarCmd: string;
begin
  if FDefs.InstallOptions.ProgressBar then
    Result := ' --progress-bar';
end;

function TPyPackageManagerCmdPip.MakeIntallPythonImplementationCmd: string;
begin
  if not FDefs.InstallOptions.PythonImplementation.IsEmpty() then
    Result := ' --implementation ' + FDefs.InstallOptions.PythonImplementation;
end;

function TPyPackageManagerCmdPip.MakeInstallPythonVersionCmd: string;
begin
  if not FDefs.InstallOptions.PythonVersion.IsEmpty() then
    Result := ' --python-version ' + FDefs.InstallOptions.PythonVersion;
end;

function TPyPackageManagerCmdPip.MakeInstallRequireHashesCmd: string;
begin
  if FDefs.InstallOptions.RequireHashes then
    Result := ' --require-hashes';
end;

function TPyPackageManagerCmdPip.MakeInstallRequirementCmd: string;
begin
  if not FDefs.InstallOptions.Requirement.IsEmpty() then
    Result := ' -r ' + FDefs.InstallOptions.Requirement;
end;

function TPyPackageManagerCmdPip.MakeInstallRootCmd: string;
begin
  if not FDefs.InstallOptions.Root.IsEmpty() then
    Result := ' --root ' + FDefs.InstallOptions.Root;
end;

function TPyPackageManagerCmdPip.MakeInstallSrcCmd: string;
begin
  if not FDefs.InstallOptions.Source.IsEmpty() then
    Result := ' --src ' + FDefs.InstallOptions.Source;
end;

function TPyPackageManagerCmdPip.MakeInstallTargetCmd: string;
begin
  if not FDefs.InstallOptions.Target.IsEmpty() then
    Result := ' -t ' + FDefs.InstallOptions.Target;
end;

function TPyPackageManagerCmdPip.MakeInstallUpgradeCmd: string;
begin
  if FDefs.InstallOptions.Upgrade then
    Result := ' -U';
end;

function TPyPackageManagerCmdPip.MakeInstallUpgradeStrategyCmd: string;
begin
  if not FDefs.InstallOptions.UpgradeStrategy.IsEmpty() then
    Result := ' --upgrade-strategy ' + FDefs.InstallOptions.UpgradeStrategy;
end;

function TPyPackageManagerCmdPip.MakeInstallUsePe517Cmd: string;
begin
  if FDefs.InstallOptions.UsePep517 then
    Result := ' --use-pep517';
end;

function TPyPackageManagerCmdPip.MakeInstallUserCmd: string;
begin
  if not FDefs.InstallOptions.User.IsEmpty() then
    Result := ' --user ' + FDefs.InstallOptions.User;
end;

end.
