(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'    Copyright (c) 2021                         *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                https://github.com/lmbelo/P4D_AI_ML      *)
(**************************************************************************)
(*  Functionality:  NumPy Data Analysis with P4D                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, 
  FMX.Dialogs, PyCommon, PyModule, NumPy, PythonEngine, FMX.PythonGUIInputOutput,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PyPackage;

type
  (*https://www.machinelearningplus.com/python/101-numpy-exercises-python/*)
  //NumPy Data Analysis exercises resolved in the P4D style.
  TForm2 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    NumPy1: TNumPy;
    Memo1: TMemo;
    PythonModule1: TPythonModule;
    procedure FormCreate(Sender: TObject);
  private
    FBm: Variant;
    FMm: Variant;
    //Interop methods
    function maxx(ASelf, AArgs: PPyObject): PPyObject; cdecl;
    //Helper methods
    procedure RestorePrintOpts(const APrintOpts: variant);
  private
    /// <summary>
    ///  1. Import numpy as np and see the version
    /// </summary>
    procedure Exercise1();
    /// <summary>
    ///  2. How to create a 1D array?
    /// </summary>
    procedure Exercise2();
    /// <summary>
    ///   3. How to create a boolean array?
    /// </summary>
    procedure Exercise3();
    /// <summary>
    ///   4. How to extract items that satisfy a given condition from 1D array?
    /// </summary>
    procedure Exercise4();
    /// <summary>
    ///   5. How to replace items that satisfy a condition with another value in numpy array?
    /// </summary>
    procedure Exercise5();
    /// <summary>
    ///   6. How to replace items that satisfy a condition without affecting the original array?
    /// </summary>
    procedure Exercise6();
    /// <summary>
    ///   7. How to reshape an array?
    /// </summary>
    procedure Exercise7();
    /// <summary>
    ///   8. How to stack two arrays vertically?
    /// </summary>
    procedure Exercise8();
    /// <summary>
    ///   9. How to stack two arrays horizontally?
    /// </summary>
    procedure Exercise9();
    /// <summary>
    ///   10. How to generate custom sequences in numpy without hardcoding?
    /// </summary>
    procedure Exercise10();
    /// <summary>
    ///   11. How to get the common items between two python numpy arrays?
    /// </summary>
    procedure Exercise11();
    /// <summary>
    ///   12. How to remove from one array those items that exist in another?
    /// </summary>
    procedure Exercise12();
    /// <summary>
    ///   13. How to get the positions where elements of two arrays match?
    /// </summary>
    procedure Exercise13();
    /// <summary>
    ///   14. How to extract all numbers between a given range from a numpy array?
    /// </summary>
    procedure Exercise14();
    /// <summary>
    ///   15. How to make a python function that handles scalars to work on numpy arrays?
    /// </summary>
    procedure Exercise15();
    /// <summary>
    ///   16. How to swap two columns in a 2d numpy array?
    /// </summary>
    procedure Exercise16();
    /// <summary>
    ///   17. How to swap two rows in a 2d numpy array?
    /// </summary>
    procedure Exercise17();
    /// <summary>
    ///   18. How to reverse the rows of a 2D array?
    /// </summary>
    procedure Exercise18();
    /// <summary>
    ///   19. How to reverse the columns of a 2D array?
    /// </summary>
    procedure Exercise19();
    /// <summary>
    ///   20. How to create a 2D array containing random floats between 5 and 10?
    /// </summary>
    procedure Exercise20();
    /// <summary>
    ///   21. How to print only 3 decimal places in python numpy array?
    /// </summary>
    procedure Exercise21();
    /// <summary>
    ///   22. How to pretty print a numpy array by suppressing the scientific notation (like 1e10)?
    /// </summary>
    procedure Exercise22();
    /// <summary>
    ///   23. How to limit the number of items printed in output of numpy array?
    /// </summary>
    procedure Exercise23();
    /// <summary>
    ///   24. How to print the full numpy array without truncating
    /// </summary>
    procedure Exercise24();
    /// <summary>
    ///   25. How to import a dataset with numbers and texts keeping the text intact in python numpy?
    /// </summary>
    procedure Exercise25();
    /// <summary>
    ///   26. How to extract a particular column from 1D array of tuples?
    /// </summary>
    procedure Exercise26();
    /// <summary>
    ///   27. How to convert a 1d array of tuples to a 2d numpy array?
    /// </summary>
    procedure Exercise27();
    /// <summary>
    ///   28. How to compute the mean, median, standard deviation of a numpy array?
    /// </summary>
    procedure Exercise28();
    /// <summary>
    ///   29. How to normalize an array so the values range exactly between 0 and 1?
    /// </summary>
    procedure Exercise29();
    /// <summary>
    ///   30. How to compute the softmax score?
    /// </summary>
    procedure Exercise30();
    /// <summary>
    ///   31. How to find the percentile scores of a numpy array?
    /// </summary>
    procedure Exercise31();
    /// <summary>
    ///   32. How to insert values at random positions in an array?
    /// </summary>
    procedure Exercise32();
    /// <summary>
    ///   33. How to find the position of missing values in numpy array?
    /// </summary>
    procedure Exercise33();
    /// <summary>
    ///   34. How to filter a numpy array based on two or more conditions?
    /// </summary>
    procedure Exercise34();
  end;

var
  Form2: TForm2;

implementation

uses
  WrapDelphi, VarPyth;

{$R *.fmx}

const
  Eval: function(const APythonExpression: AnsiString): Variant = VarPythonEval;

// Helper methods

function T(const AValues: array of const): variant;
begin
  Result := VarPythonCreate(AValues, TSequenceType.stTuple);
end;

function L(const AValues: array of const): variant;
begin
  Result := VarPythonCreate(AValues, TSequenceType.stList);
end;

procedure TForm2.Exercise1;
begin
  FBm.print('Q. Import numpy as np and print the version number.');
  with NumPy1 do begin
    FBm.print(np.__version__);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise2;
begin
  FBm.print('Q. Create a 1D array of numbers from 0 to 9');
  with NumPy1 do begin
    FBm.print(np.arange(10));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise3;
begin
  FBm.print('Q. Create a 3×3 numpy array of all True’s');
  with NumPy1 do begin
    FBm.print(np.full(VarArrayOf([3, 3]), true, dtype := FBm.bool));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise4;
var
  LVal: variant;
begin
  FBm.print('Q. Extract all odd numbers from arr');
  with NumPy1 do begin
    var LArr := np.array(VarArrayOf([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    FMm.arr := LArr;
    FBm.print('by Python eval');
    FBm.print(Eval('arr[arr %2 == 1]'));
    FBm.print('by Delphi'); 
    var LList := NewPythonList();
    for LVal in VarPyIterate(LArr) do begin
      if Odd(VarToStr(LVal).ToInteger()) then
        LList.append(VarToStr(LVal).ToInteger());           
    end;  
    FBm.print(LList);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise5;
begin
  FBm.print('Q. Replace all odd numbers in arr with -1');
  with NumPy1, PythonEngine do begin
    var LArr := np.array(VarArrayOf([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    FMm.arr := LArr;
    ExecString(AnsiString('arr[arr % 2 == 1] = -1'));
    FBm.print(Fmm.arr);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise6;
begin
  FBm.print('Q. Replace all odd numbers in arr with -1 without changing arr');
  with NumPy1 do begin
    var LArr := np.array(VarArrayOf([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));        
    LArr := np.arange(10);   
    FMm.arr := LArr;
    var LOut := np.where(Eval('arr % 2 == 1'), -1, LArr);
    FBm.print(LArr);
    FBm.print(LOut);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise7;
begin
  FBm.print('Q. Convert a 1D array to a 2D array with 2 rows');
  with NumPy1 do begin
    var LArr := np.arange(10);    
    FBm.print(LArr.reshape(2, -1));       
  end;
  FBm.print('');
end;

procedure TForm2.Exercise8;
begin
  FBm.print('Q. Stack arrays a and b vertically');
  with NumPy1 do begin
    var LA := np.arange(10).reshape(2, -1);
    var LB := np.repeat(1, 10).reshape(2, -1);
    FBm.print('# Method 1:');
    FBm.print(np.concatenate(T([LA, LB]), axis := 0));
    FBm.print('# Method 2:');
    FBm.print(np.vstack(T([LA, LB])));
    FBm.print('# Method 3:');
    FBm.print(np.r_[(T([LA, LB]))]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise9;
begin
  FBm.print('Q. Stack the arrays a and b horizontally.');
  with NumPy1 do begin
    var LA := np.arange(10).reshape(2, -1);
    var LB := np.repeat(1, 10).reshape(2, -1);
    FBm.print('# Method 1:');
    FBm.print(np.concatenate(T([LA, LB]), axis := 1));
    FBm.print('# Method 2:');
    FBm.print(np.hstack(T([LA, LB])));
    FBm.print('# Method 3:');
    FBm.print(np.c_[(T([LA, LB]))]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise10;
begin
  FBm.print('Q. Create the following pattern without hardcoding. Use only numpy functions and the below input array a.');
  with NumPy1 do begin
    var LA := np.array(VarArrayOf([1, 2, 3]));
    FBm.print(np.r_[(T([np.repeat(LA, 3), np.tile(LA, 3)]))]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise11;
begin
  FBm.print('Q. Get the common items between a and b.');
  with NumPy1 do begin
    var LA := np.array(L([1, 2, 3, 4, 5, 6]));
    var LB := np.array(L([7, 2, 10, 2, 7, 4, 9, 4, 9, 8]));
    FBm.print(np.intersect1d(LA, LB));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise12;
begin
  FBm.print('Q. From array a remove all items present in array b.');
  with NumPy1 do begin
    var LA := np.array(L([1, 2, 3, 4, 5]));
    var LB := np.array(L([5, 6, 7, 8, 9]));
    FBm.print(np.setdiff1d(LA, LB));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise13;
begin
  FBm.print('Q. Get the positions where elements of a and b match.');
  with NumPy1 do begin
    FMm.a := np.array(L([1, 2, 3, 2, 3, 4, 3, 4, 5, 6]));
    FMm.b := np.array(L([7, 2, 10, 2, 7, 4, 9, 4, 9, 8]));
    FBm.print(np.where(Eval('a == b')));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise14;
begin
  FBm.print('Q. Get all items between 5 and 10 from a.');
  with NumPy1 do begin
    FMm.a := np.arange(15);
    FBm.print('# Method 1:');
    var LIx := np.where(Eval('(a >= 5) & (a <= 10)'));
    FBm.print(FMm.a[LIx]);
    FBm.print('# Method 2:');
    LIx := np.where(np.logical_and(Eval('a >= 5'), Eval('a <= 10')));
    FBm.print(FMm.a[LIx]);
    FBm.print('# Method 3:');
    FBm.print(FMm.a[Eval('(a >= 5) & (a <= 10)')]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise15;
begin
  FBm.print('Q. Convert the function maxx that works on two scalars, to work on two arrays.');
  with NumPy1, PythonModule1 do begin
    AddDelphiMethod('maxx', maxx, String.Empty);
    Initialize();
    try
      var LEm := VarPyth.Import('exercises');
      FMm.pair_max := np.vectorize(LEm.maxx, otypes := L([FBm.float]));
      var LA := np.array(L([5, 7, 9, 8, 6, 4, 5]));
      var LB := np.array(L([6, 3, 4, 8, 9, 7, 1]));
      FBm.print(FMm.pair_max(LA, LB)); //advice: add a break point at maxx method.
    finally
      Finalize();
    end;
  end;
  FBm.print('');
end;

procedure TForm2.Exercise16;
begin
  FBm.print('Q. Swap columns 1 and 2 in the array arr.');
  with NumPy1 do begin
    FMm.arr := np.arange(9).reshape(3, 3);
    var LArr :=  FMm.arr[(T([Ellipsis(), L([1, 0, 2])]))]; //arr[:, [1, 0, 2]]
    FBm.print(LArr);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise17;
begin
  FBm.print('Q. Swap rows 1 and 2 in the array arr:');
  with NumPy1 do begin
    FMm.arr := np.arange(9).reshape(3, 3);
    var LArr :=  FMm.arr[(T([L([1, 0, 2]), Ellipsis()]))]; //arr[[1, 0, 2], :]
    FBm.print(LArr);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise18;
begin
  FBm.print('Q. Reverse the rows of a 2D array arr.');
  with NumPy1 do begin
    FMm.arr := np.arange(9).reshape(3, 3);
    var LArr :=  FMm.arr[(T([FBm.slice(None(), None(), -1)]))]; //arr[::-1]
    FBm.print(LArr);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise19;
begin
  FBm.print('Q. Reverse the columns of a 2D array arr.');
  with NumPy1 do begin
    FMm.arr := np.arange(9).reshape(3, 3);
    var LArr :=  FMm.arr[(T([Ellipsis(), FBm.slice(None(), None(), -1)]))]; //arr[:, ::-1]
    FBm.print(LArr);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise20;
begin
  FBm.print('Q. Create a 2D array of shape 5x3 to contain random decimal numbers between 5 and 10.');
  with NumPy1 do begin
    FBm.print('# Method 1:');
    var LRand_Arr := np.random.randint(low := 5, high := 10, size := T([5, 3])) + np.random.random(T([5, 3]));
    FBm.print(LRand_Arr);
    FBm.print('# Method 2:');
    LRand_Arr := np.random.uniform(5, 10, size := T([5, 3]));
    FBm.print(LRand_Arr);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise21;
begin
  FBm.print('Q. Print or show only 3 decimal places of the numpy array rand_arr.');
  with NumPy1 do begin
    FMm.rand_arr := np.random.random(T([5, 3]));
    //Limit to 3 decimal places
    var LPrintOpts := np.get_printoptions();
    try
      np.set_printoptions(precision := 3);
      Fbm.print(FMm.rand_arr[T([FBm.slice(None(), 4)])]); //rand_arr[:4]
    finally
      RestorePrintOpts(LPrintOpts);
    end;
  end;
  FBm.print('');
end;

procedure TForm2.Exercise22;
begin
  FBm.print('Q. Pretty print rand_arr by suppressing the scientific notation (like 1e10)');
  with NumPy1 do begin
    //Reset printoptions to default
    var LPrintOpts := np.get_printoptions();
    try
      np.set_printoptions(suppress := false);
      np.random.seed(100);
      var LRand_Arr := np.random.random(L([3, 3])); //1e3
      FBm.print(LRand_Arr);
      np.set_printoptions(suppress := true, precision := 6);
      FBm.print(LRand_Arr);
    finally
      RestorePrintOpts(LPrintOpts);
    end;
  end;
  FBm.print('');
end;

procedure TForm2.Exercise23;
begin
  FBm.print('Q. Limit the number of items printed in python numpy array a to a maximum of 6 elements.');
  with NumPy1 do begin
    var LPrintOpts := np.get_printoptions();
    try
      np.set_printoptions(threshold := 6);
      FBm.print(np.arange(15));
    finally
      RestorePrintOpts(LPrintOpts);
    end;
  end;
  FBm.print('');
end;

procedure TForm2.Exercise24;
begin
  FBm.print('Q. Print the full numpy array a without truncating.');
  with NumPy1 do begin
    var LPrintOpts := np.get_printoptions();
    try
      np.set_printoptions(threshold := 6);
      var LA := np.arange(15);
      np.set_printoptions(threshold := SysModule.maxsize);
      FBm.print(LA);
    finally
      RestorePrintOpts(LPrintOpts);
    end;
  end;
  FBm.print('');
end;

procedure TForm2.Exercise25;
begin
  FBm.print('Q. Import the iris dataset keeping the text intact.');
  with NumPy1 do begin
    np.set_printoptions(precision := 3);
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris := np.genfromtxt(LUrl, delimiter := ',', dtype := 'object');
    var LNames := T(['sepallength', 'sepalwidth', 'petallength', 'petalwidth', 'species']);
    //Print first 3 rows
    FBm.print(FMm.iris[T([FBm.slice(None(), 3)])]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise26;
begin
  FBm.print('Q. Extract the text column species from the 1D iris imported in previous question.');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris_1d := np.genfromtxt(LUrl, delimiter := ',', dtype := None());
    FBm.print(FMm.iris_1d.shape);

    FMm.species := np.array(Eval('[row[4] for row in iris_1d]'));
    FBm.print(FMm.species[T([FBm.slice(None(), 5)])]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise27;
begin
  FBm.print('Q. Convert the 1D iris to 2D array iris_2d by omitting the species text field.');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris_1d := np.genfromtxt(LUrl, delimiter := ',', dtype := None());
    FBm.print('# Method 1: Convert each row to a list and get the first 4 items');
    FMm.iris_2d := np.array(Eval('[row.tolist()[:4] for row in iris_1d]'));
    FBm.print(FMm.iris_2d[T([FBm.slice(None(), 4)])]);
    FBm.print('# Method 2: Import only the first 4 columns from source url');
    FMm.iris_2d := np.genfromtxt(LUrl, delimiter := ',', dtype := 'float', usecols := L([0, 1, 2, 3]));
    FBm.print(FMm.iris_2d[T([FBm.slice(None(), 4)])]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise28;
begin
  FBm.print('Q. Find the mean, median, standard deviation of iris''s sepallength (1st column)');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris := np.genfromtxt(LUrl, delimiter := ',', dtype := 'object');
    var LSepallength := np.genfromtxt(LUrl, delimiter := ',', dtype := 'float', usecols := L([0]));
    var LMu := np.mean(LSepallength);
    var LMed := np.median(LSepallength);
    var LSd := np.std(LSepallength);
    FBm.print(LMu, LMed, LSd);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise29;
begin
  FBm.print('Q. Create a normalized form of iris''s sepallength whose values range exactly between 0 and 1 so that the minimum has value 0 and maximum has value 1.');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    var LSepallength := np.genfromtxt(LUrl, delimiter := ',', dtype := 'float', usecols := L([0]));
    var LSmax := LSepallength.max();
    var LSmin := LSepallength.min();
    var LS := (LSepallength - LSmin) / (LSmax - LSmin);
    //or
    LS := (LSepallength - LSmin) / LSepallength.ptp();
    FBm.print(LS);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise30;
begin
  FBm.print('Q. Compute the softmax score of sepallength.');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris := np.genfromtxt(LUrl, delimiter := ',', dtype := 'object');
    var LSepallength := np.array(Eval('[float(row[0]) for row in iris]'));
    var softmax := function(const AValue: variant): variant
    begin
      var LE_x := np.exp(AValue - np.max(AValue));
      Result := LE_x / LE_x.sum(axis := 0);
    end;
    FBm.print(softmax(LSepallength));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise31;
begin
  FBm.print('Q. Find the 5th and 95th percentile of iris''s sepallength');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    var LSepallength := np.genfromtxt(LUrl, delimiter := ',', dtype := 'float', usecols := L([0]));
    FBm.print(np.percentile(LSepallength, q := L([5, 95])));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise32;
begin
  FBm.print('Q. Insert np.nan values at 20 random positions in iris_2d dataset');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris_2d := np.genfromtxt(LUrl, delimiter := ',', dtype := 'object');
    FMm.npNAN := np.NAN;
    FBm.print('# Method 1:');
    var LResp := np.where(FMm.iris_2d); //Response with many results (tuple) e.g. i, j = np.where(iris_2d)
    var LI := LResp.GetItem(0);
    var LJ := LResp.GetItem(1);
    np.random.seed(100);
    FMm.c1 := np.random.choice(LI, 20);
    FMm.c2 := np.random.choice(LJ, 20);
    PythonEngine.ExecString('iris_2d[c1, c2] = npNAN');
    FBm.print('# Method 2:');
    np.random.seed(100);
    FMm.c1 := np.random.randint(150, size := 20);
    FMm.c2 := np.random.randint(4, size := 20);
    PythonEngine.ExecString('iris_2d[c1, c2] = npNAN');
    FBm.print(FMm.iris_2d[T([FBm.slice(None(), 10)])]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise33;
begin
  FBm.print('Q. Find the number and position of missing values in iris_2d''s sepallength (1st column)');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris_2d := np.genfromtxt(LUrl, delimiter := ',', dtype := 'float', usecols := L([0,1,2,3]));
    FMm.c1 := np.random.randint(150, size := 20);
    FMm.c2 := np.random.randint(4, size := 20);
    PythonEngine.ExecString('iris_2d[c1, c2] = npNAN');
    FBm.print('Number of missing values: \n', np.isnan(FMm.iris_2d[T([Ellipsis(), 0])]).sum());
    FBm.print('Position of missing values: \n', np.where(np.isnan(FMm.iris_2d[T([Ellipsis(), 0])])));
  end;
  FBm.print('');
end;

procedure TForm2.Exercise34;
begin
  FBm.print('Q. Filter the rows of iris_2d that has petallength (3rd column) > 1.5 and sepallength (1st column) < 5.0');
  with NumPy1 do begin
    var LUrl := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    FMm.iris_2d := np.genfromtxt(LUrl, delimiter := ',', dtype := 'float', usecols := L([0,1,2,3]));
    FMm.c1 := FMm.iris_2d[T([Ellipsis(), 2])];
    FMm.c1 := Eval('c1 > 1.5');
    FMm.c2 := FMm.iris_2d[T([Ellipsis(), 0])];
    FMm.c2 := Eval('c2 < 5.0');
    FBm.print(FMm.iris_2d[Eval('c1 & c2')]);
  end;
  FBm.print('');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FBm := BuiltinModule;
  FMm := MainModule;
  Exercise1();
  Exercise2();
  Exercise3();
  Exercise4();
  Exercise5();
  Exercise6();
  Exercise7();
  Exercise8();
  Exercise9();
  Exercise10();
  Exercise11();
  Exercise12();
  Exercise13();
  Exercise14();
  Exercise15();
  Exercise16();
  Exercise17();
  Exercise18();
  Exercise19();
  Exercise20();
  Exercise21();
  Exercise22();
  Exercise23();
  Exercise24();
  Exercise25();
  Exercise26();
  Exercise27();
  Exercise28();
  Exercise29();
  Exercise30();
  Exercise31();
  Exercise32();
  Exercise33();
  Exercise34();
end;

function TForm2.maxx(ASelf, AArgs: PPyObject): PPyObject;
var
  LX, LY: integer;
begin
  with NumPy1.PythonEngine do begin
    if PyArg_ParseTuple(AArgs, 'ii:maxx', @LX, @LY) <> 0 then
      begin
        if (LX >= LY) then
          Result := PyLong_FromLong(LX)
        else
          Result := PyLong_FromLong(LY);
      end
    else
      Result := nil;
  end;
end;

procedure TForm2.RestorePrintOpts(const APrintOpts: variant);
begin
  with PythonEngine1 do begin
    var LArgs := PyTuple_New(0);
    try
      //simulates a keyword argument
      //np.set_printoptions(**LPrintOpts)
      PyEval_CallObjectWithKeywords(
        ExtractPythonObjectFrom(NumPy1.np.set_printoptions),
        LArgs,
        ExtractPythonObjectFrom(APrintOpts));
    finally
      Py_XDecRef(LArgs)
    end;
  end;
end;

end.
