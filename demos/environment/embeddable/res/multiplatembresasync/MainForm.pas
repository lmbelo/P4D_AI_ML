unit MainForm;

interface

uses
  System.SysUtils, System.Classes, System.Zip, FMX.Forms, FMX.Memo.Types, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Memo, FMX.Types, FMX.Controls, FMX.Controls.Presentation,
  PythonEngine, FMX.PythonGUIInputOutput, PyEnvironment, PyEnvironment.Task.Model,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python310;

type
  TForm2 = class(TForm)
    PythonEngine1: TPythonEngine;
    StatusBar1: TStatusBar;
    lbMsg: TLabel;
    lbDesc: TLabel;
    mmScriptInput: TMemo;
    ToolBar1: TToolBar;
    mmOutput: TMemo;
    btnExecute: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Splitter1: TSplitter;
    PyEmbeddedResEnvironment3101: TPyEmbeddedResEnvironment310;
    lblOSVersion: TLabel;
    lblInformation: TLabel;
    procedure PyEmbeddedResEnvironment3101BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedResEnvironment3101ZipProgress(Sender: TObject;
      ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
      Header: TZipHeader; Position: Int64);
    procedure btnExecuteClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PyEmbeddedResEnvironment3101TaskRunnerTerminate(
      const ASender: TObject; const ACanceled: Boolean);
    procedure PyEmbeddedResEnvironment3101TaskRunnerCompleteAll(
      const ASender: TObject;
      const ATasks: TArray<PyEnvironment.Task.Model.TPyEnvironmentTaskModel>);
    procedure FormCreate(Sender: TObject);
  private
    FClose: Boolean;
    procedure UpdateInstallationStatus(const AStatus, ADescription: string);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btnExecuteClick(Sender: TObject);
begin
  mmOutput.Lines.Clear();
  PythonEngine1.ExecString(AnsiString(mmScriptInput.Text));
end;

procedure TForm2.PyEmbeddedResEnvironment3101AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  if AActivated then
    UpdateInstallationStatus('Activate:', 'Python has been activated.')
  else
    UpdateInstallationStatus('Activate:', 'Failed to activate Python.');
end;

procedure TForm2.PyEmbeddedResEnvironment3101AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdateInstallationStatus('Setup done.', String.Empty);
end;

procedure TForm2.PyEmbeddedResEnvironment3101BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  UpdateInstallationStatus(Format('Python %s', [APythonVersion]), 'Activating');
end;

procedure TForm2.PyEmbeddedResEnvironment3101BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdateInstallationStatus(Format('Python %s', [APythonVersion]), 'Setting up...');
end;

procedure TForm2.PyEmbeddedResEnvironment3101TaskRunnerCompleteAll(
  const ASender: TObject;
  const ATasks: TArray<PyEnvironment.Task.Model.TPyEnvironmentTaskModel>);
begin
  btnExecute.Enabled := true;
end;

procedure TForm2.PyEmbeddedResEnvironment3101TaskRunnerTerminate(
  const ASender: TObject; const ACanceled: Boolean);
begin
  if FClose then
    Close();
end;

procedure TForm2.PyEmbeddedResEnvironment3101ZipProgress(Sender: TObject;
  ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  UpdateInstallationStatus('Extracting:', FileName);
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  UpdateInstallationStatus('Closing', 'Waiting for tasks');
  PyEmbeddedResEnvironment3101.TaskRunner.Cancel(true);
  CanClose := not PyEmbeddedResEnvironment3101.TaskRunner.IsRunning();
  FClose := not CanClose;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  lblOSVersion.Text := TOSVersion.ToString;
  lblInformation.Text := Format('%d-bit | RTL: %f | Compiler: %f',
    [SizeOf(Integer)*16, RTLVersion, CompilerVersion]);
end;

procedure TForm2.UpdateInstallationStatus(const AStatus, ADescription: string);
begin
  if not Assigned(PyEmbeddedResEnvironment3101) or PyEmbeddedResEnvironment3101.TaskRunner.IsCanceled() then
    Exit;
  lbMsg.Text := AStatus;
  lbMsg.Repaint;
  lbDesc.Text := ADescription;
  lbDesc.Repaint;
end;

end.
