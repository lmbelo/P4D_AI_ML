(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PyPackage.Manager.Cmd.Conda'                            *)
(*                                                                        *)
(*                                  Copyright (c) 2021                    *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                   https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyPackage Cmd layer                                   *)
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
unit PyPackage.Manager.Cmd.Conda;

interface

uses
  PyPackage.Manager.Cmd.Intf,
  PyPackage.Manager.Defs, PyPackage.Manager.Defs.Conda;

type
  TPyPackageManagerCmdConda = class(TInterfacedObject, IPyPackageManagerCmdIntf)
  private
    FDefs: TPyPackageManagerDefsConda;
  private
    //Defs cmds
    function MakePackageCmd: TArray<string>; inline;
    //Install opts cmds
    function MakeInstallRevisionCmd: TArray<string>; inline;
    function MakeInstallFileCmd: TArray<string>; inline;
    function MakeInstallNameCmd: TArray<string>; inline;
    function MakeInstallPrefixCmd: TArray<string>; inline;
    function MakeInstallChannelCmd: TArray<string>; inline;
    function MakeInstallUseLocalCmd: TArray<string>; inline;
    function MakeInstallOverrideChannelsCmd: TArray<string>; inline;
    function MakeInstallRepoDataCmd: TArray<string>; inline;
    function MakeInstallStrictChannelPriorityCmd: TArray<string>; inline;
    function MakeInstallNoChannelPriorityCmd: TArray<string>; inline;
    function MakeInstallNoDepsCmd: TArray<string>; inline;
    function MakeInstallOnlyDepsCmd: TArray<string>; inline;
    function MakeInstallNoPinCmd: TArray<string>; inline;
    function MakeInstallExperimentalSolverCmd: TArray<string>; inline;
    function MakeInstallForceReinstallCmd: TArray<string>; inline;
    function MakeInstallNoUpdateDepsCmd: TArray<string>; inline;
    function MakeInstallUpdateDepsCmd: TArray<string>; inline;
    function MakeInstallSatisfiedSkipSolveCmd: TArray<string>; inline;
    function MakeInstallUpdateAllCmd: TArray<string>; inline;
    function MakeInstallUpdateSpecsCmd: TArray<string>; inline;
    function MakeInstallCopyCmd: TArray<string>; inline;
    function MakeInstallMkDirCmd: TArray<string>; inline;
    function MakeInstallClobberCmd: TArray<string>; inline;
    function MakeInstallUseIndexCacheCmd: TArray<string>; inline;
    function MakeInstallInsecureCmd: TArray<string>; inline;
    function MakeInstallOfflineCmd: TArray<string>; inline;
    function MakeInstallDryRunCmd: TArray<string>; inline;
    function MakeInstallJsonCmd: TArray<string>; inline;
    function MakeInstallQuietCmd: TArray<string>; inline;
    function MakeInstallVerboseCmd: TArray<string>; inline;
    function MakeInstallDoNotAskForConfirmationCmd: TArray<string>; inline;
    function MakeInstallDownloadOnlyCmd: TArray<string>; inline;
    function MakeInstallShowChannelUrlsCmd: TArray<string>; inline;
    //Uninstall cmds
    function MakeUninstallNameCmd: TArray<string>; inline;
    function MakeUninstallPrefixCmd: TArray<string>; inline;
    function MakeUninstallChannelCmd: TArray<string>; inline;
    function MakeUninstallUseLocalCmd: TArray<string>; inline;
    function MakeUninstallOverrideChannelsCmd: TArray<string>; inline;
    function MakeUninstallRepodataCmd: TArray<string>; inline;
    function MakeUninstallAllCmd: TArray<string>; inline;
    function MakeUninstallFeaturesCmd: TArray<string>; inline;
    function MakeUninstallForceCmd: TArray<string>; inline;
    function MakeUninstallNoPinCmd: TArray<string>; inline;
    function MakeUninstallExperimentalSolverCmd: TArray<string>; inline;
    function MakeUninstallUseIndexCacheCmd: TArray<string>; inline;
    function MakeUninstallInsecureCmd: TArray<string>; inline;
    function MakeUninstallOfflineCmd: TArray<string>; inline;
    function MakeUninstallDryRunCmd: TArray<string>; inline;
    function MakeUninstallJsonCmd: TArray<string>; inline;
    function MakeUninstallQuietCmd: TArray<string>; inline;
    function MakeUninstallVerboseCmd: TArray<string>; inline;
    function MakeUninstallYesCmd: TArray<string>; inline;
  public
    function BuildInstallCmd(const ADefs: TPyPackageManagerDefs): TArray<string>;
    function BuildUninstallCmd(const ADefs: TPyPackageManagerDefs): TArray<string>;
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections;

{ TPyPackageManagerCmdConda }

function TPyPackageManagerCmdConda.BuildInstallCmd(
  const ADefs: TPyPackageManagerDefs): TArray<string>;
begin
  FDefs := ADefs as TPyPackageManagerDefsConda;
  try
    Result := TArray<string>.Create('install')
      + MakeInstallRevisionCmd()
      + MakeInstallFileCmd()
      + MakeInstallNameCmd()
      + MakeInstallPrefixCmd()
      + MakeInstallChannelCmd()
      + MakeInstallUseLocalCmd()
      + MakeInstallOverrideChannelsCmd()
      + MakeInstallRepoDataCmd()
      + MakeInstallStrictChannelPriorityCmd()
      + MakeInstallNoChannelPriorityCmd()
      + MakeInstallNoDepsCmd()
      + MakeInstallOnlyDepsCmd()
      + MakeInstallNoPinCmd()
      + MakeInstallExperimentalSolverCmd()
      + MakeInstallForceReinstallCmd()
      + MakeInstallNoUpdateDepsCmd()
      + MakeInstallUpdateDepsCmd()
      + MakeInstallSatisfiedSkipSolveCmd()
      + MakeInstallUpdateAllCmd()
      + MakeInstallUpdateSpecsCmd()
      + MakeInstallCopyCmd()
      + MakeInstallMkDirCmd()
      + MakeInstallClobberCmd()
      + MakeInstallUseIndexCacheCmd()
      + MakeInstallInsecureCmd()
      + MakeInstallOfflineCmd()
      + MakeInstallDryRunCmd()
      + MakeInstallJsonCmd()
      + MakeInstallQuietCmd()
      + MakeInstallVerboseCmd()
      + MakeInstallDoNotAskForConfirmationCmd()
      + MakeInstallDownloadOnlyCmd()
      + MakeInstallShowChannelUrlsCmd()
      + MakePackageCmd()
  finally
    FDefs := nil;
  end;
end;

function TPyPackageManagerCmdConda.BuildUninstallCmd(
  const ADefs: TPyPackageManagerDefs): TArray<string>;
begin
  FDefs := ADefs as TPyPackageManagerDefsConda;
  try
    Result := TArray<string>.Create('remove')
      + MakeUninstallNameCmd()
      + MakeUninstallPrefixCmd()
      + MakeUninstallChannelCmd()
      + MakeUninstallUseLocalCmd()
      + MakeUninstallOverrideChannelsCmd()
      + MakeUninstallRepodataCmd()
      + MakeUninstallAllCmd()
      + MakeUninstallFeaturesCmd()
      + MakeUninstallForceCmd()
      + MakeUninstallNoPinCmd()
      + MakeUninstallExperimentalSolverCmd()
      + MakeUninstallUseIndexCacheCmd()
      + MakeUninstallInsecureCmd()
      + MakeUninstallOfflineCmd()
      + MakeUninstallDryRunCmd()
      + MakeUninstallJsonCmd()
      + MakeUninstallQuietCmd()
      + MakeUninstallVerboseCmd()
      + MakeUninstallYesCmd()
      + MakePackageCmd()
  finally
    FDefs := nil;
  end;
end;

function TPyPackageManagerCmdConda.MakePackageCmd: TArray<string>;
begin
  var LVersion: string;
  if not FDefs.PackageVersion.IsEmpty() then
    LVersion := FDefs.PackageVersion
  else
    LVersion := String.Empty;

  Result := TArray<string>.Create(FDefs.PackageName + LVersion);
end;

function TPyPackageManagerCmdConda.MakeUninstallAllCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.All then
    Result := TArray<string>.Create('--all');
end;

function TPyPackageManagerCmdConda.MakeUninstallChannelCmd: TArray<string>;
begin
  if not FDefs.UninstallOptions.Channel.IsEmpty() then
    Result := TArray<string>.Create('--channel', FDefs.UninstallOptions.Channel);
end;

function TPyPackageManagerCmdConda.MakeUninstallDryRunCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.DryRun then
    Result := TArray<string>.Create('--dry-run');
end;

function TPyPackageManagerCmdConda.MakeUninstallExperimentalSolverCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.ExperimentalSolver then
    Result := TArray<string>.Create('--experimental-solver');
end;

function TPyPackageManagerCmdConda.MakeUninstallFeaturesCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.Features then
    Result := TArray<string>.Create('--features');
end;

function TPyPackageManagerCmdConda.MakeUninstallForceCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.Force then
    Result := TArray<string>.Create('--force');
end;

function TPyPackageManagerCmdConda.MakeUninstallInsecureCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.Insecure then
    Result := TArray<string>.Create('--insecure');
end;

function TPyPackageManagerCmdConda.MakeUninstallJsonCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.Json then
    Result := TArray<string>.Create('--json');
end;

function TPyPackageManagerCmdConda.MakeUninstallNameCmd: TArray<string>;
begin
  if not FDefs.UninstallOptions.Name.IsEmpty() then
    Result := TArray<string>.Create('--name', FDefs.UninstallOptions.Name);
end;

function TPyPackageManagerCmdConda.MakeUninstallNoPinCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.NoPin then
    Result := TArray<string>.Create('--no-pin');
end;

function TPyPackageManagerCmdConda.MakeUninstallOfflineCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.Offline then
    Result := TArray<string>.Create('--offline');
end;

function TPyPackageManagerCmdConda.MakeUninstallOverrideChannelsCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.OverrideChannels then
    Result := TArray<string>.Create('--override-channels');
end;

function TPyPackageManagerCmdConda.MakeUninstallPrefixCmd: TArray<string>;
begin
  if not FDefs.UninstallOptions.Prefix.IsEmpty() then
    Result := TArray<string>.Create('--prefix', FDefs.UninstallOptions.Prefix);
end;

function TPyPackageManagerCmdConda.MakeUninstallQuietCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.Quiet then
    Result := TArray<string>.Create('--quiet');
end;

function TPyPackageManagerCmdConda.MakeUninstallRepodataCmd: TArray<string>;
var
  LList: TList<string>;
  LStr: string;
begin
  LList := TList<string>.Create();
  try
    for LStr in FDefs.UninstallOptions.RepoData do begin
      LList.Add('--repodata-fn=' + LStr);
    end;
    Result := LList.ToArray();
  finally
    LList.Free();
  end;
end;

function TPyPackageManagerCmdConda.MakeUninstallUseIndexCacheCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.UseIndexCache then
    Result := TArray<string>.Create('--use-index-cache');
end;

function TPyPackageManagerCmdConda.MakeUninstallUseLocalCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.UseLocal then
    Result := TArray<string>.Create('--use-local');
end;

function TPyPackageManagerCmdConda.MakeUninstallVerboseCmd: TArray<string>;
var
  LList: TList<string>;
  LStr: string;
begin
  LList := TList<string>.Create();
  try
    for LStr in FDefs.UninstallOptions.Verbose do begin
      LList.Add(LStr);
    end;
  finally
    LList.Free();
  end;
end;

function TPyPackageManagerCmdConda.MakeUninstallYesCmd: TArray<string>;
begin
  if FDefs.UninstallOptions.DoNotAskForConfirmation then
    Result := TArray<string>.Create('--yes');
end;

function TPyPackageManagerCmdConda.MakeInstallChannelCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Channel.IsEmpty() then
    Result := TArray<string>.Create('--channel', FDefs.InstallOptions.Channel);
end;

function TPyPackageManagerCmdConda.MakeInstallClobberCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Clobber then
    Result := TArray<string>.Create('--clobber');
end;

function TPyPackageManagerCmdConda.MakeInstallCopyCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Copy then
    Result := TArray<string>.Create('--copy');
end;

function TPyPackageManagerCmdConda.MakeInstallDoNotAskForConfirmationCmd: TArray<string>;
begin
  if FDefs.InstallOptions.DoNotAskForConfirmation then
    Result := TArray<string>.Create('--yes');
end;

function TPyPackageManagerCmdConda.MakeInstallDownloadOnlyCmd: TArray<string>;
begin
  if FDefs.InstallOptions.DownloadOnly then
    Result := TArray<string>.Create('--download-only');
end;

function TPyPackageManagerCmdConda.MakeInstallDryRunCmd: TArray<string>;
begin
  if FDefs.InstallOptions.DryRun then
    Result := TArray<string>.Create('--dry-run');
end;

function TPyPackageManagerCmdConda.MakeInstallExperimentalSolverCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.ExperimentalSolver.IsEmpty() then
    Result := TArray<string>.Create('--experimental-solver', FDefs.InstallOptions.ExperimentalSolver);
end;

function TPyPackageManagerCmdConda.MakeInstallFileCmd: TArray<string>;
var
  LList: TList<string>;
  LStr: string;
begin
  LList := TList<string>.Create();
  try
    for LStr in FDefs.InstallOptions.Files do
      LList.Add('--file=' + LStr);
    Result := LList.ToArray();
  finally
    LList.Free();
  end;
end;

function TPyPackageManagerCmdConda.MakeInstallForceReinstallCmd: TArray<string>;
begin
  if FDefs.InstallOptions.ForceReinstall then
    Result := TArray<string>.Create('--force-reinstall');
end;

function TPyPackageManagerCmdConda.MakeInstallNoUpdateDepsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoUpdateDeps then
    Result := TArray<string>.Create('--no-update-deps');
end;

function TPyPackageManagerCmdConda.MakeInstallInsecureCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Insecure then
    Result := TArray<string>.Create('--insecure');
end;

function TPyPackageManagerCmdConda.MakeInstallJsonCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Json then
    Result := TArray<string>.Create('--json');
end;

function TPyPackageManagerCmdConda.MakeInstallMkDirCmd: TArray<string>;
begin
  if FDefs.InstallOptions.MkDir then
    Result := TArray<string>.Create('--mkdir');
end;

function TPyPackageManagerCmdConda.MakeInstallNameCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Name.IsEmpty() then
    Result := TArray<string>.Create('--name', FDefs.InstallOptions.Name);
end;

function TPyPackageManagerCmdConda.MakeInstallNoChannelPriorityCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoChannelPriority then
    Result := TArray<string>.Create('--no-channel-priority');
end;

function TPyPackageManagerCmdConda.MakeInstallNoDepsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.NoDeps then
    Result := TArray<string>.Create('--no-deps');
end;

function TPyPackageManagerCmdConda.MakeInstallNoPinCmd: TArray<string>;
begin
  if FDefs.InstallOptions.OnlyDeps then
    Result := TArray<string>.Create('--no-pin');
end;

function TPyPackageManagerCmdConda.MakeInstallOfflineCmd: TArray<string>;
begin
  if FDefs.InstallOptions.OffLine then
    Result := TArray<string>.Create('--offline');
end;

function TPyPackageManagerCmdConda.MakeInstallOnlyDepsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.OnlyDeps then
    Result := TArray<string>.Create('--only-deps');
end;

function TPyPackageManagerCmdConda.MakeInstallOverrideChannelsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.OverrideChannels then
    Result := TArray<string>.Create('--override-channels');
end;

function TPyPackageManagerCmdConda.MakeInstallPrefixCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Prefix.IsEmpty() then
    Result := TArray<string>.Create('--prefix', FDefs.InstallOptions.Prefix);
end;

function TPyPackageManagerCmdConda.MakeInstallQuietCmd: TArray<string>;
begin
  if FDefs.InstallOptions.Quiet then
    Result := TArray<string>.Create('--quiet');
end;

function TPyPackageManagerCmdConda.MakeInstallRepoDataCmd: TArray<string>;
var
  LList: TList<string>;
  LStr: string;
begin
  LList := TList<string>.Create();
  try
    for LStr in FDefs.InstallOptions.RepoData do begin
      LList.Add('--repodata-fn=' + LStr);
    end;
    Result := LList.ToArray();
  finally
    LList.Free();
  end;
end;

function TPyPackageManagerCmdConda.MakeInstallRevisionCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.Revision.IsEmpty() then
    Result := TArray<string>.Create('--revision', FDefs.InstallOptions.Revision);
end;

function TPyPackageManagerCmdConda.MakeInstallSatisfiedSkipSolveCmd: TArray<string>;
begin
  if FDefs.InstallOptions.SatisfiedSkipSolve then
    Result := TArray<string>.Create('--satisfied-skip-solve');
end;

function TPyPackageManagerCmdConda.MakeInstallShowChannelUrlsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.ShowChannelUrls then
    Result := TArray<string>.Create('--show-channel-urls');
end;

function TPyPackageManagerCmdConda.MakeInstallStrictChannelPriorityCmd: TArray<string>;
begin
  if FDefs.InstallOptions.StrictChannelPriority then
    Result := TArray<string>.Create('--strict-channel-priority');
end;

function TPyPackageManagerCmdConda.MakeInstallUpdateAllCmd: TArray<string>;
begin
  if FDefs.InstallOptions.UpdateAll then
    Result := TArray<string>.Create('--all');
end;

function TPyPackageManagerCmdConda.MakeInstallUpdateDepsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.UpdateDeps then
    Result := TArray<string>.Create('--update-deps');
end;

function TPyPackageManagerCmdConda.MakeInstallUpdateSpecsCmd: TArray<string>;
begin
  if FDefs.InstallOptions.UpdateSpecs then
    Result := TArray<string>.Create('--update-specs');
end;

function TPyPackageManagerCmdConda.MakeInstallUseIndexCacheCmd: TArray<string>;
begin
  if FDefs.InstallOptions.UseIndexCache then
    Result := TArray<string>.Create('--use-index-cache');
end;

function TPyPackageManagerCmdConda.MakeInstallUseLocalCmd: TArray<string>;
begin
  if not FDefs.InstallOptions.UseLocal then
    Result := TArray<string>.Create('--use-local');
end;

function TPyPackageManagerCmdConda.MakeInstallVerboseCmd: TArray<string>;
var
  LList: TList<string>;
  LStr: string;
begin
  LList := TList<string>.Create();
  try
    for LStr in FDefs.InstallOptions.Verbose do begin
      LList.Add(LStr);
    end;
  finally
    LList.Free();
  end;
end;

end.
