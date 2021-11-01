unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, 
  FMX.Dialogs, PyCommon, PyModule, NumPy, PythonEngine, FMX.PythonGUIInputOutput, 
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    NumPy1: TNumPy;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    FBm: Variant;
    FMm: Variant;
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
  VarPyth;

{$R *.fmx}

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
  I: integer;
begin
  FBm.print('Q. Extract all odd numbers from arr');
  with NumPy1 do begin
    var LArr := np.array(VarArrayOf([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    FMm.arr := LArr;
    FBm.print('by Python eval');
    FBm.print(VarPythonEval('arr[arr %2 == 1]'));
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
    var LOut := np.where(VarPythonEval('arr % 2 == 1'), -1, LArr);
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
    FBm.print(np.concatenate(VarPythonCreate([LA, LB], TSequenceType.stTuple), axis := 0));
    FBm.print('# Method 2:');
    FBm.print(np.vstack(VarPythonCreate([LA, LB], TSequenceType.stTuple)));
    FBm.print('# Method 3:');
    FBm.print(np.r_[(VarPythonCreate([LA, LB], TSequenceType.stTuple))]);
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
    FBm.print(np.concatenate(VarPythonCreate([LA, LB], TSequenceType.stTuple), axis := 1));
    FBm.print('# Method 2:');
    FBm.print(np.hstack(VarPythonCreate([LA, LB], TSequenceType.stTuple)));
    FBm.print('# Method 3:');
    FBm.print(np.c_[(VarPythonCreate([LA, LB], TSequenceType.stTuple))]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise10;
begin
  FBm.print('Q. Create the following pattern without hardcoding. Use only numpy functions and the below input array a.');
  with NumPy1 do begin
    var LA := np.array(VarArrayOf([1, 2, 3]));
    FBm.print(np.r_[(VarPythonCreate([np.repeat(LA, 3), np.tile(LA, 3)], TSequenceType.stTuple))]);
  end;
  FBm.print('');
end;

procedure TForm2.Exercise11;
begin
  FBm.print('Q. Get the common items between a and b');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise12;
begin
  FBm.print('Q. From array a remove all items present in array b');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise13;
begin
  FBm.print('Q. Get the positions where elements of a and b match');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise14;
begin
  FBm.print('Q. Get all items between 5 and 10 from a.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise15;
begin
  FBm.print('Q. Convert the function maxx that works on two scalars, to work on two arrays.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise16;
begin
  FBm.print('Q. Swap columns 1 and 2 in the array arr.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise17;
begin
  FBm.print('Q. Swap rows 1 and 2 in the array arr:');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise18;
begin
  FBm.print('Q. Reverse the rows of a 2D array arr.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise19;
begin
  FBm.print('Q. Reverse the columns of a 2D array arr.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise20;
begin
  FBm.print('Q. Create a 2D array of shape 5x3 to contain random decimal numbers between 5 and 10.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise21;
begin
  FBm.print('Q. Print or show only 3 decimal places of the numpy array rand_arr.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise22;
begin
  FBm.print('Q. Pretty print rand_arr by suppressing the scientific notation (like 1e10)');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise23;
begin
  FBm.print('Q. Limit the number of items printed in python numpy array a to a maximum of 6 elements.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise24;
begin
  FBm.print('Q. Print the full numpy array a without truncating.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise25;
begin
  FBm.print('Q. Import the iris dataset keeping the text intact.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise26;
begin
  FBm.print('Q. Extract the text column species from the 1D iris imported in previous question.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise27;
begin
  FBm.print('Q. Convert the 1D iris to 2D array iris_2d by omitting the species text field.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise28;
begin
  FBm.print('Q. Find the mean, median, standard deviation of iris''s sepallength (1st column)');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise29;
begin
  FBm.print('Q. Create a normalized form of iris''s sepallength whose values range exactly between 0 and 1 so that the minimum has value 0 and maximum has value 1.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise30;
begin
  FBm.print('Q. Compute the softmax score of sepallength.');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise31;
begin
  FBm.print('Q. Find the 5th and 95th percentile of iris''s sepallength');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise32;
begin
  FBm.print('Q. Insert np.nan values at 20 random positions in iris_2d dataset');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise33;
begin
  FBm.print('Q. Find the number and position of missing values in iris_2d''s sepallength (1st column)');
  with NumPy1 do begin

  end;
  FBm.print('');
end;

procedure TForm2.Exercise34;
begin
  FBm.print('Q. Filter the rows of iris_2d that has petallength (3rd column) > 1.5 and sepallength (1st column) < 5.0');
  with NumPy1 do begin

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
end;

end.
