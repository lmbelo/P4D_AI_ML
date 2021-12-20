unit PyPackage.Manager.Intf;

interface

uses
  PyPackage.Manager.Defs,
  PyPackage.Manager.Cmd.Intf,
  PyPackage.Manager.Exec.Intf;

type
  IPyPackageManager = interface
    ['{5ACA3C89-1BCF-442A-A77E-9A8B0DF4823A}']
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;
    function GetExec(): IPyPackageManagerCmdExec;

    /// <summary>
    ///   PIP package definitions
    /// </summary>
    property Defs: TPyPackageManagerDefs read GetDefs;
    /// <summary>
    ///   PIP package cmd builder
    /// </summary>
    property Cmd: IPyPackageManagerCmdIntf read GetCmd;
    /// <summary>
    ///   PIP package exec cmd strategy
    /// </summary>
    property Exec: IPyPackageManagerCmdExec read GetExec;

    procedure Install();
    procedure Uninstall();
    function IsInstalled(): boolean;
  end;

implementation

end.
