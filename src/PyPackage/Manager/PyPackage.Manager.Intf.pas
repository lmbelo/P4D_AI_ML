unit PyPackage.Manager.Intf;

interface

uses
  PyPackage.Manager.Defs, PyPackage.Manager.Cmd.Intf;

type
  IPyPackageManager = interface
    ['{5ACA3C89-1BCF-442A-A77E-9A8B0DF4823A}']
    function GetDefs(): TPyPackageManagerDefs;
    function GetCmd(): IPyPackageManagerCmdIntf;

    /// <summary>
    ///   PIP package definitions
    /// </summary>
    property Defs: TPyPackageManagerDefs read GetDefs;
    /// <summary>
    ///   PIP package cmd builder
    /// </summary>
    property Cmd: IPyPackageManagerCmdIntf read GetCmd;
  end;

implementation

end.
