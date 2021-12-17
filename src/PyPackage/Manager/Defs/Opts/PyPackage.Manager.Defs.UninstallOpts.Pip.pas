unit PyPackage.Manager.Defs.UninstallOpts.Pip;

interface

uses
  System.Classes;

type
  TPyPackageManagerDefsUninstallOptsPip = class(TPersistent)
  private
    FRequirement: string;
    FAskForConfirmation: boolean;
  published
    property Requirement: string read FRequirement write FRequirement;
    property AskForConfirmation: boolean read FAskForConfirmation write FAskForConfirmation;
  end;

implementation

end.
