unit PyContext;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  TPyPipInfo = class;
  TPyModuleInfo = class;
  TPySubModuleInfo = class;

  TPyPIPInfoReg = procedure(const APyPIPInfo: TPyPipInfo);
  TPyModuleInfoReg = procedure(const APyModuleInfo: TPyModuleInfo);
  TPySubModuleInfoReg = procedure(const APySubModuleInfo: TPySubModuleInfo);

  TPyPipInfo = class
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
    FSubModules: TObjectList<TPySubModuleInfo>;
  public
    constructor Create(const AClass: TClass);
    destructor Destroy(); override;

    procedure RegisterModule(const AModuleName: string); overload;
    function RegisterSubModule(const AClass: TClass; const ASubModuleName: string): TPySubModuleInfo;

    property Clazz: TClass read FClass;
    property ModuleName: string read FModuleName write FModuleName;
    property SubModules: TObjectList<TPySubModuleInfo> read FSubModules;
  end;

  TPySubModuleInfo = class(TPyModuleInfo)
  public
    procedure RegisterSubModule(const ASubModuleName: string) overload;
  end;

  TPyInfo = class
  strict  private
    FPyPIPInfo: TPyPipInfo;
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
    //SubModule routines
    function RegisterSubModule(const AClass: TClass; const ASubModuleName: string): TPyInfo; overload;
    function RegisterSubModule(const AClass: TClass; const ASubModuleName: string;
      const AInfo: TPySubModuleInfoReg): TPyInfo; overload;

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
    function FindModuleInfo(const AComp: TClass; const ALookupSubModules: boolean = false): TPyModuleInfo;
  public
    class property Instance: TPyContext read GetInstance;
  end;

implementation

{ TPyPipInfo }

procedure TPyPipInfo.RegisterPIPPackage(const APackageName,
  APackageVer: string);
begin
  FPackageName := APackageName;
  FPackageVer := APackageVer;
end;

procedure TPyPipInfo.RegisterPIPPackage(const APackageName: string);
begin
  RegisterPIPPackage(APackageName, String.Empty);
end;

{ TPyModuleInfo }

constructor TPyModuleInfo.Create(const AClass: TClass);
begin
  FClass := AClass;
  FSubModules := TObjectList<TPySubModuleInfo>.Create();
end;

destructor TPyModuleInfo.Destroy;
begin
  FSubModules.Free();
  inherited;
end;

procedure TPyModuleInfo.RegisterModule(const AModuleName: string);
begin
  FModuleName := AModuleName;
end;

function TPyModuleInfo.RegisterSubModule(const AClass: TClass;
  const ASubModuleName: string): TPySubModuleInfo;
begin
  Result := TPySubModuleInfo.Create(AClass);
  Result.RegisterSubModule(ASubModuleName);
  FSubModules.Add(Result);
end;

{ TPyInfo }

constructor TPyInfo.Create(const AClass: TClass);
begin
  FPyPIPInfo := TPyPipInfo.Create();
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

function TPyInfo.RegisterSubModule(const AClass: TClass;
  const ASubModuleName: string; const AInfo: TPySubModuleInfoReg): TPyInfo;
begin
  AInfo(FPyModuleInfo.RegisterSubModule(AClass, ASubModuleName));
  Result := Self;
end;

function TPyInfo.RegisterSubModule(const AClass: TClass;
  const ASubModuleName: string): TPyInfo;
begin
  FPyModuleInfo.RegisterSubModule(AClass, ASubModuleName);
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
  { TODO : If not found, try to load it by attributes }
end;

function TPyContext.FindModuleInfo(const AComp: TClass;
  const ALookupSubModules: boolean): TPyModuleInfo;
var
  LInfo: TPyInfo;
  LSubModuleInfo: TPySubModuleInfo;
begin
  Result := nil;
  LInfo := FIndInfo(AComp);
  if Assigned(LInfo) then
    Result := LInfo.PyModuleInfo
  else if ALookupSubModules then begin
    for LInfo in FInfo.Values do begin
      for LSubModuleInfo in LInfo.PyModuleInfo.SubModules do begin
        if LSubModuleInfo.Clazz = AComp then
          Exit(LSubModuleInfo);
      end;
    end;
  end;
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

{ TPySubModuleInfo }

procedure TPySubModuleInfo.RegisterSubModule(const ASubModuleName: string);
begin
  RegisterModule(ASubModuleName);
end;

initialization
finalization
  TPyContext.FInstance.Free();

end.
