unit PyExceptions;

interface

uses
  System.SysUtils;

type
  EPyCommonException = class(Exception)
  end;

  EModuleNotReady = class(EPyCommonException)
  end;

  EPyPackageNotInstalled = class(EPyCommonException)
  end;

  EPyParentModuleCircularReference = class(EPyCommonException)
  end;

  EPyModuleNotImported = class(EPyCommonException)
  end;

  EPySubModuleNotFound = class(EPyCommonException)
  end;

resourcestring
  ErrPackageNotInstalled = 'Package %s not installed.';
  ErrModuleNotImported = 'Module not imported.';
  ErrSubModuleNotFound = 'Submodule not found.';
  ErrCircularRefNotAllowed = 'Circular reference not allowed.';

implementation

end.
