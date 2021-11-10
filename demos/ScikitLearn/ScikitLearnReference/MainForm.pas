unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PythonEngine,
  FMX.PythonGUIInputOutput, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, PyCommon, PyModule, ScikitLearn, FMX.StdCtrls,
  FMX.Layouts, MatplotLib, NumPy, PyPackage;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo1: TMemo;
    ScikitLearn1: TScikitLearn;
    Layout1: TLayout;
    Button1: TButton;
    GroupBox1: TGroupBox;
    rbDecisionTreeRegression: TRadioButton;
    NumPy1: TNumPy;
    MatplotLib1: TMatplotLib;
    procedure Button1Click(Sender: TObject);
  private
    procedure DoDecisionTreeRegression();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if rbDecisionTreeRegression.IsChecked then begin
    DoDecisionTreeRegression();
  end;
end;

procedure TForm1.DoDecisionTreeRegression;
begin
  with ScikitLearn1, NumPy1, MatplotLib1 do begin
    sklearn.tree;
  end;
end;

end.
