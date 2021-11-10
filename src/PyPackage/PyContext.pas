unit PyContext;

interface

uses
  System.SysUtils, System.Generics.Collections, PyCore;

type
  TPyPyPIInfo = class;
  TPyModuleInfo = class;

  TPyPIPInfoReg = procedure(const APyPIPInfo: TPyPyPIInfo);
  TPyModuleInfoReg = procedure(const APyModuleInfo: TPyModuleInfo);

  TPyPyPIInfo = class
  strict private
    FPackageName: string;
    FPackageVer: string;
  public
    procedure RegisterPIPPackage(const APackageName: string); overload;
    procedure RegisterPIPPackage(const APackageName, APackageVer: string); overload;

    property PackageName: string read FPackageName write FPackageName;
    property PackageVer: string read FPackageVer write FPackageVer;
  end;

  TPyModuleInfo = class
  strict private
    FClass: TClass;
    FModuleName: string;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy(); override;

    procedure RegisterModule(const AModuleName: string); overload;

    property Clazz: TClass read FClass;
    property ModuleName: string read FModuleName write FModuleName;
  end;

  TPyInfo = class
  strict  private
    FPyPIPInfo: TPyPyPIInfo;
    FPyModuleInfo: TPyModuleInfo;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy(); override;

    //PIP routines
    function RegisterPIPPackage(const APackageName: string): TPyInfo; overload;
    function RegisterPIPPackage(const APackageName, APackageVer: string): TPyInfo; overload;
    function RegisterPIPPackage(const AInfo: TPyPIPInfoReg): TPyInfo; overload;
    //Module routines
    function RegisterModule(const AModuleName: string): TPyInfo; overload;
    function RegisterModule(const AInfo: TPyModuleInfoReg): TPyInfo; overload;

    property PyPipInfo: TPyPyPIInfo read FPyPIPInfo;
    property PyModuleInfo: TPyModuleInfo read FPyModuleInfo;
  end;

  TPyContext = class
  private
    class var FInstance: TPyContext;
    class function GetInstance: TPyContext; static;
  private
    FInfo: TObjectDictionary<TClass, TPyInfo>;
  public
    constructor Create();
    destructor Destroy(); override;

    function RegisterInfo(const AComp: TClass): TPyInfo;
    procedure UnRegisterInfo(const AComp: TClass);
    function FindInfo(const AComp: TClass): TPyInfo;
    function FindModuleInfo(const AComp: TClass): TPyModuleInfo;
  public
    class property Instance: TPyContext read GetInstance;
  end;

implementation

{ TPyPipInfo }

procedure TPyPyPIInfo.RegisterPIPPackage(const APackageName,
  APackageVer: string);
begin
  FPackageName := APackageName;
  FPackageVer := APackageVer;
end;

procedure TPyPyPIInfo.RegisterPIPPackage(const APackageName: string);
begin
  RegisterPIPPackage(APackageName, String.Empty);
end;

{ TPyModuleInfo }

constructor TPyModuleInfo.Create(const AClass: TClass);
begin
  FClass := AClass;
end;

destructor TPyModuleInfo.Destroy;
begin
  inherited;
end;

procedure TPyModuleInfo.RegisterModule(const AModuleName: string);
begin
  FModuleName := AModuleName;
end;

{ TPyInfo }

constructor TPyInfo.Create(const AClass: TClass);
begin
  FPyPIPInfo := TPyPyPIInfo.Create();
  FPyModuleInfo := TPyModuleInfo.Create(AClass);
end;

destructor TPyInfo.Destroy;
begin
  FPyModuleInfo.Free();
  FPyPIPInfo.Free();
  inherited;
end;

function TPyInfo.RegisterPIPPackage(const APackageName: string): TPyInfo;
begin
  FPyPIPInfo.RegisterPIPPackage(APackageName);
  Result := Self;
end;

function TPyInfo.RegisterPIPPackage(const AInfo: TPyPIPInfoReg): TPyInfo;
begin
  AInfo(Self.FPyPIPInfo);
  Result := Self;
end;

function TPyInfo.RegisterPIPPackage(const APackageName,
  APackageVer: string): TPyInfo;
begin
  FPyPIPInfo.RegisterPIPPackage(APackageName, APackageVer);
  Result := Self;
end;

function TPyInfo.RegisterModule(const AModuleName: string): TPyInfo;
begin
  FPyModuleInfo.RegisterModule(AModuleName);
  Result := Self;
end;

function TPyInfo.RegisterModule(const AInfo: TPyModuleInfoReg): TPyInfo;
begin
  AInfo(Self.FPyModuleInfo);
  Result := Self;
end;

{ TPyContext }

constructor TPyContext.Create;
begin
  FInfo := TObjectDictionary<TClass, TPyInfo>.Create([doOwnsValues]);
end;

destructor TPyContext.Destroy;
begin
  FInfo.Free();
  inherited;
end;

function TPyContext.FindInfo(const AComp: TClass): TPyInfo;
begin
  FInfo.TryGetValue(AComp, Result);
end;

function TPyContext.FindModuleInfo(const AComp: TClass): TPyModuleInfo;
var
  LInfo: TPyInfo;
begin
  Result := nil;
  LInfo := FIndInfo(AComp);
  if Assigned(LInfo) then
    Result := LInfo.PyModuleInfo;
end;

class function TPyContext.GetInstance: TPyContext;
begin
  if not Assigned(FInstance) then
    FInstance := TPyContext.Create();
  Result := FInstance;
end;

function TPyContext.RegisterInfo(const AComp: TClass): TPyInfo;
begin
  FInfo.TryGetValue(AComp, Result);
  if Assigned(Result) then
    Exit();
  Result := TPyInfo.Create(AComp);
  FInfo.Add(AComp, Result);
end;

procedure TPyContext.UnRegisterInfo(const AComp: TClass);
begin
  FInfo.Remove(AComp);
end;

initialization
finalization
  TPyContext.FInstance.Free();

end.
