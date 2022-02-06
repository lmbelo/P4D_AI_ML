unit PyPackage.Manager.Defs.InstallOpts.Pip;

interface

uses
  System.Classes;

type
  TPyPackageManagerDefsInstallOptsPip = class(TPersistent)
  private
    FRequirement: string;
    FConstraint: string;
    FNoDeps: boolean;
    FPre: boolean;
    FEditable: string;
    FTarget: string;
    FPlatform: string;
    FPythonVersion: string;
    FPythonImplementation: string;
    FAbi: string;
    FUser: string;
    FRoot: string;
    FPrefix: string;
    FSource: string;
    FUpgrade: boolean;
    FUpgradeStrategy: string;
    FForceReinstall: boolean;
    FIgnoreInstalled: boolean;
    FIgnoreRequiresPython: boolean;
    FNoBuildIsolation: boolean;
    FUsePep517: boolean;
    FInstallOption: string;
    FGlobalOption: string;
    FCompile: boolean;
    FNoWarnScriptLocation: boolean;
    FNoWarnConflicts: boolean;
    FNoBinary: boolean;
    FNoCompile: boolean;
    FOnlyBinary: boolean;
    FPreferBinary: boolean;
    FRequireHashes: boolean;
    FProgressBar: boolean;
    FNoClean: boolean;
    FIndexUrl: string;
    FExtraIndexUrl: string;
    FNoIndex: boolean;
    FFindLinks: string;
  published
    property Requirement: string read FRequirement write FRequirement;
    property Constraint: string read FConstraint write FConstraint;
    property NoDeps: boolean read FNoDeps write FNoDeps default false;
    property Pre: boolean read FPre write FPre default false;
    property Editable: string read FEditable write FEditable;
    property Target: string read FTarget write FTarget;
    property Platform: string read FPlatform write FPlatform;
    property PythonVersion: string read FPythonVersion write FPythonVersion;
    property PythonImplementation: string read FPythonImplementation write FPythonImplementation;
    property Abi: string  read FAbi write FAbi;
    property User: string read FUser write FUser;
    property Root: string read FRoot write FRoot;
    property Prefix: string read FPrefix write FPrefix;
    property Source: string read FSource write FSource;
    property Upgrade: boolean read FUpgrade write FUpgrade default false;
    property UpgradeStrategy: string read FUpgradeStrategy write FUpgradeStrategy;
    property ForceReinstall: boolean read FForceReinstall write FForceReinstall default false;
    property IgnoreInstalled: boolean read FIgnoreInstalled write FIgnoreInstalled default false;
    property IgnoreRequiresPython: boolean read FIgnoreRequiresPython write FIgnoreRequiresPython default false;
    property NoBuildIsolation: boolean read FNoBuildIsolation write FNoBuildIsolation default false;
    property UsePep517: boolean read FUsePep517 write FUsePep517 default false;
    property InstallOption: string read FInstallOption write FInstallOption;
    property GlobalOption: string read FGlobalOption write FGlobalOption;
    property Compile: boolean read FCompile write FCompile default false;
    property NoCompile: boolean read FNoCompile write FNoCompile default false;
    property NoWarnScriptLocation: boolean read FNoWarnScriptLocation write FNoWarnScriptLocation default false;
    property NoWarnConflicts: boolean read FNoWarnConflicts write FNoWarnConflicts default false;
    property NoBinary: boolean read FNoBinary write FNoBinary default false;
    property OnlyBinary: boolean read FOnlyBinary write FOnlyBinary default false;
    property PreferBinary: boolean read FPreferBinary write FPreferBinary default false;
    property RequireHashes: boolean read FRequireHashes write FRequireHashes default false;
    property ProgressBar: boolean read FProgressBar write FProgressBar default false;
    property NoClean: boolean read FNoClean write FNoClean default false;
    property IndexUrl: string read FIndexUrl write FIndexUrl;
    property ExtraIndexUrl: string read FExtraIndexUrl write FExtraIndexUrl;
    property NoIndex: boolean read FNoIndex write FNoIndex default false;
    property FindLinks: string read FFindLinks write FFindLinks;
  end;

implementation

end.
