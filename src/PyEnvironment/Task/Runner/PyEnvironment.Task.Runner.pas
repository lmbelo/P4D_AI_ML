unit PyEnvironment.Task.Runner;

interface

uses
  PyEnvironment.Notification,
  PyEnvironment.Task.Model;

type
  IPyEnvironmentTaskRunner = interface
    ['{8DBA21C9-F16B-4D4A-84CF-F7A22976D687}']
    procedure Run(const ANotification: TEnvironmentNotification);
    procedure Cancel(const ACancelAll: boolean = false);
    procedure Wait();

    function IsRunning(): boolean;
    function IsCanceled(): boolean;
  end;

implementation

end.
