unit PyEnvironment.Intf;

interface

type
  IEnvironmentPaths = interface
    ['{D53389EA-11C7-457E-851B-417AA43032C0}']

    {***** Python environment paths *****}

    /// <summary>
    ///   Return the location of the standard Python libraries.
    /// </summary>
    function GetHome(): string;
    /// <summary>
    ///   Return the location of the binaries.
    /// </summary>
    function GetProgramName(): string;

    {***** Python file paths *****}

    /// <summary>
    ///   Return the Python interpreter shared library.
    /// </summary>
    function GetSharedLibrary(): string;
    /// <summary>
    ///   Return the Python executable.
    /// </summary>
    function GetExecutable(): string;
  end;

implementation

end.
