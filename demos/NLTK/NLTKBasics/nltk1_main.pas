unit nltk1_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Zip,
  PythonEngine, PyEnvironment.AddOn, PyEnvironment.AddOn.EnsurePip,
  PyEnvironment, PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python310, PyCommon, PyModule, PyPackage,
  NLTK, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  FMX.PythonGUIInputOutput, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Layouts, FMX.ListBox, VarPyth;

type
  TForm16 = class(TForm)
    NLTK1: TNLTK;
    PyEmbeddedResEnvironment3101: TPyEmbeddedResEnvironment310;
    PyEnvironmentAddOnEnsurePip1: TPyEnvironmentAddOnEnsurePip;
    PythonEngine1: TPythonEngine;
    Button1: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    StatusBar1: TStatusBar;
    lbMsg: TLabel;
    lbDesc: TLabel;
    Memo1: TMemo;
    ListBox1: TListBox;
    PythonType1: TPythonType;
    procedure Button1Click(Sender: TObject);
    procedure PyEmbeddedResEnvironment3101AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedResEnvironment3101AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101ZipProgress(Sender: TObject;
      ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
      Header: TZipHeader; Position: Int64);
  private
    { Private declarations }
    procedure UpdateInstallationStatus(const AStatus, ADescription: string);
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

{$R *.fmx}

procedure TForm16.Button1Click(Sender: TObject);
begin
  var tokens := NLTK1.nltk.word_tokenize(memo1.lines.Text);

  if VarIsPythonList(Tokens) then
    PythonEngine1.PyListToStrings(ExtractPythonObjectFrom(Tokens), ListBox1.Items);

  var tagged := NLTK1.nltk.pos_tag(tokens);
  var entities := NLTK1.nltk.chunk.ne_chunk(tagged);

end;

procedure TForm16.PyEmbeddedResEnvironment3101AfterActivate(
  Sender: TObject; const APythonVersion: string;
  const AActivated: Boolean);
begin
  if AActivated then
    UpdateInstallationStatus('Activate:', 'Python has been activated.')
  else
    UpdateInstallationStatus('Activate:', 'Failed to activate Python.');
end;

procedure TForm16.PyEmbeddedResEnvironment3101AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdateInstallationStatus('Setup done.', String.Empty);
end;

procedure TForm16.PyEmbeddedResEnvironment3101BeforeActivate(
  Sender: TObject; const APythonVersion: string);
begin
  UpdateInstallationStatus(Format('Python %s', [APythonVersion]), 'Activating');
end;

procedure TForm16.PyEmbeddedResEnvironment3101BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdateInstallationStatus(Format('Python %s', [APythonVersion]), 'Setting up...');
end;

procedure TForm16.PyEmbeddedResEnvironment3101ZipProgress(Sender: TObject;
  ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  UpdateInstallationStatus('Extracting:', FileName);
end;

procedure TForm16.UpdateInstallationStatus(const AStatus,
  ADescription: string);
begin
  if not Assigned(PyEmbeddedResEnvironment3101) or PyEmbeddedResEnvironment3101.TaskRunner.IsCanceled() then
    Exit;
  lbMsg.Text := AStatus;
  lbMsg.Repaint;
  lbDesc.Text := ADescription;
  lbDesc.Repaint;
end;

end.
