unit PyEnvironment.Task.Model;

interface

uses
  System.Threading,
  PyEnvironment.Notification;

type
  TPyEnvironmentTaskModel = class
  private
    FId: integer;
    FRunWhen: TEnvironmentNotification;
    FTask: IFuture<boolean>;
    FAbortOnFailure: boolean;
  public
    constructor Create(const AId: integer; const ATask: IFuture<boolean>;
      const ARunWhen: TEnvironmentNotification; const AAbortOnFailure: boolean);
    destructor Destroy(); override;

    property Id: integer read FId write FId;
    property Task: IFuture<boolean> read FTask write FTask;
    property RunWhen: TEnvironmentNotification read FRunWhen write FRunWhen;
    property AbortOnFailure: boolean read FAbortOnFailure write FAbortOnFailure;
  end;

implementation

{ TPyEnvironmentTaskModel }

constructor TPyEnvironmentTaskModel.Create(const AId: integer;
  const ATask: IFuture<boolean>; const ARunWhen: TEnvironmentNotification;
  const AAbortOnFailure: boolean);
begin
  FId := AId;
  FTask := ATask;
  FRunWhen := ARunWhen;
  FAbortOnFailure := AAbortOnFailure;
end;

destructor TPyEnvironmentTaskModel.Destroy;
begin
  inherited;
end;

end.
