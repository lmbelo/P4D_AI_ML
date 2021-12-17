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
    property NoDeps: boolean read FNoDeps write FNoDeps;
    property Pre: boolean read FPre write FPre;
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
    property Upgrade: boolean read FUpgrade write FUpgrade;
    property UpgradeStrategy: string read FUpgradeStrategy write FUpgradeStrategy;
    property ForceReinstall: boolean read FForceReinstall write FForceReinstall;
    property IgnoreInstalled: boolean read FIgnoreInstalled write FIgnoreInstalled;
    property IgnoreRequiresPython: boolean read FIgnoreRequiresPython write FIgnoreRequiresPython;
    property NoBuildIsolation: boolean read FNoBuildIsolation write FNoBuildIsolation;
    property UsePep517: boolean read FUsePep517 write FUsePep517;
    property InstallOption: string read FInstallOption write FInstallOption;
    property GlobalOption: string read FGlobalOption write FGlobalOption;
    property Compile: boolean read FCompile write FCompile;
    property NoCompile: boolean read FNoCompile write FNoCompile;
    property NoWarnScriptLocation: boolean read FNoWarnScriptLocation write FNoWarnScriptLocation;
    property NoWarnConflicts: boolean read FNoWarnConflicts write FNoWarnConflicts;
    property NoBinary: boolean read FNoBinary write FNoBinary;
    property OnlyBinary: boolean read FOnlyBinary write FOnlyBinary;
    property PreferBinary: boolean read FPreferBinary write FPreferBinary;
    property RequireHashes: boolean read FRequireHashes write FRequireHashes;
    property ProgressBar: boolean read FProgressBar write FProgressBar;
    property NoClean: boolean read FNoClean write FNoClean;
    property IndexUrl: string read FIndexUrl write FIndexUrl;
    property ExtraIndexUrl: string read FExtraIndexUrl write FExtraIndexUrl;
    property NoIndex: boolean read FNoIndex write FNoIndex;
    property FindLinks: string read FFindLinks write FFindLinks;
  end;

implementation

end.
