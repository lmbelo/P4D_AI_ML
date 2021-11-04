unit PyContext;

interface

uses
  System.Generics.Collections;

type
  TPyPipInfo = class
  strict private
    FPackageName: string;
    FPackageVer: string;
  public
    property PackageName: string read FPackageName write FPackageName;
    property PackageVer: string read FPackageVer write FPackageVer;
  end;
  TPyPIPInfoReg = procedure(const APyPIPInfo: TPyPipInfo);

  TPyModuleInfo = class
  strict private
    FModuleName: string;
  public
    property ModuleName: string read FModuleName write FModuleName;
  end;
  TPyModuleInfoReg = procedure(const APyModuleInfo: TPyModuleInfo);

  TPyInfo = class
  strict  private
    FClass: TClass;
    FPyPIPInfo: TPyPipInfo;
    FPyModuleInfo: TPyModuleInfo;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy(); override;

    function RegisterPIPPackage(const APackageName: string): TPyInfo; overload;
    function RegisterPIPPackage(const AReg: TPyPIPInfoReg): TPyInfo; overload;
    function RegisterModule(const AModuleName: string): TPyInfo; overload;
    function RegisterModule(const AReg: TPyModuleInfoReg): TPyInfo; overload;

    property PyPipInfo: TPyPipInfo read FPyPIPInfo;
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
  public
    class property Instance: TPyContext read GetInstance;
  end;

implementation

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
  { TODO : If not found, try to load it by attributes }
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

{ TPyInfo }

constructor TPyInfo.Create(const AClass: TClass);
begin
  FClass := AClass;
  FPyPIPInfo := TPyPipInfo.Create();
  FPyModuleInfo := TPyModuleInfo.Create();
end;

destructor TPyInfo.Destroy;
begin
  FPyModuleInfo.Free();
  FPyPIPInfo.Free();
  inherited;
end;

function TPyInfo.RegisterModule(const AModuleName: string): TPyInfo;
begin
  FPyModuleInfo.ModuleName := AModuleName;
  Result := Self;
end;

function TPyInfo.RegisterPIPPackage(const APackageName: string): TPyInfo;
begin
  FPyPIPInfo.PackageName := APackageName;
  Result := Self;
end;

function TPyInfo.RegisterModule(const AReg: TPyModuleInfoReg): TPyInfo;
begin
  AReg(Self.FPyModuleInfo);
  Result := Self;
end;

function TPyInfo.RegisterPIPPackage(const AReg: TPyPIPInfoReg): TPyInfo;
begin
  AReg(Self.FPyPIPInfo);
  Result := Self;
end;

initialization
finalization
  TPyContext.FInstance.Free();

end.
