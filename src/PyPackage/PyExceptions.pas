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

  EPyModuleInstallError = class(EPyCommonException)
  end;

  EPyModuleUnInstallError = class(EPyCommonException)
  end;

  EPyParentModuleCircularReference = class(EPyCommonException)
  end;

  EPyModuleNotImported = class(EPyCommonException)
  end;

  EPySubModuleNotFound = class(EPyCommonException)
  end;

  EPyVarException = class(EPyCommonException)
  end;

  EPyVarIsNotPython = class(EPyVarException)
  end;

resourcestring
  ErrPackageNotInstalled = 'Package %s not installed.';
  ErrModuleNotImported = 'Module not imported.';
  ErrSubModuleNotFound = 'Submodule not found.';
  ErrCircularRefNotAllowed = 'Circular reference not allowed.';
  ErrVarIsNotPython = 'Variant is not a Python variant.';

implementation

end.
