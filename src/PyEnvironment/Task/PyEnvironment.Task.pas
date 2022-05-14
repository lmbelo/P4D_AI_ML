unit PyEnvironment.Task;

interface

uses
  System.SysUtils, System.Threading,
  PyEnvironment.Notification,
  PyEnvironment.Task.Model;

type
  /// <summary>
  ///   A serialized execution list of tasks
  /// </summary>
  IPyEnvironmentTaskCollection = interface
    ['{9492D6FE-AE38-44BC-8886-B4A7A534FC30}']
    //TThread pool reference
    function GetThreadPool(): TThreadPool;
    //Task builder
    function CreateTask(const AFunc: TFunc<boolean>): TFuture<boolean>;
    //List IO operation
    function AddTask(const ATrigger: TEnvironmentNotification;
      const AModel: TPyEnvironmentTaskModel): boolean;
    function PickTask(const ATrigger: TEnvironmentNotification;
      out AModel: TPyEnvironmentTaskModel): boolean;
    function RemoveTask(const ATrigger: TEnvironmentNotification;
      const AId: integer): boolean;
    function HasTasks(const ATrigger: TEnvironmentNotification): boolean;
  end;

implementation

end.
