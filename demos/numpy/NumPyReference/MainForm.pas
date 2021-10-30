unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PythonEngine, PyProxy, PyModule, NumPy,
  PyClass, NumPy.NDArray, System.Generics.Collections, System. Rtti,
  Vcl.StdCtrls, Vcl.PythonGUIInputOutput, PyCommon;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    NumPy1: TNumPy;
    Memo1: TMemo;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  VarPyth;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  var bm := BuiltinModule;
  var mm := MainModule;
  //https://numpy.org/doc/stable/reference/index.html
  with NumPy1 do begin
    //https://numpy.org/doc/stable/reference/arrays.html
    //1D array
    bm.print('1D array');
    var x := np.array(VarArrayOf([1, 2]));
    var y := np.array(VarArrayOf([1, 2]));
    bm.print(x);
    bm.print(y);

    //2D array
    bm.print('');
    bm.print('2D array');
    x := np.array(VarArrayOf([VarArrayOf([1, 2, 3]), VarArrayOf([4, 5, 6])]));
    y := np.array(VarArrayOf([VarArrayOf([7, 8, 9]), VarArrayOf([10, 11, 12])]));
    bm.print(x);
    bm.print(y);

    //2D array of size 2x3, composed of 4-byte integer elements
    bm.print('');
    bm.print('2D array of size 2x3, composed of 4-byte integer elements');
    x := np.array(VarArrayOf([VarArrayOf([1, 2, 3]), VarArrayOf([4, 5, 6])]), np.int32);
    bm.print(x);

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.html
    bm.print('');
    bm.print('Creating ndarray giving buffer as None');
    var ndarray := np.ndarray(shape := VarArrayOf([2, 2]),
                              dtype := bm.float,
                              order := 'F');
    bm.print(ndarray);

    bm.print('');
    bm.print('Creating ndarray giving buffer as a array');
    ndarray := np.ndarray(VarArrayOf([2]),
                          buffer := np.array(VarArrayOf([1, 2, 3])),
                          offset := np.int_().itemsize,
                          dtype := bm.int);
    bm.print(ndarray);

    bm.print('');
    bm.print('Constants');
    bm.print('is finite');
    bm.print(np.isfinite(np.array(VarArrayOf([np.NZERO]), dtype := bm.float)));
    bm.print('is nan');
    bm.print(np.isnan(np.array(VarArrayOf([np.NZERO]), dtype := bm.float)));
    bm.print('is inf');
    bm.print(np.isinf(np.array(VarArrayOf([np.NZERO]), dtype := bm.float)));
    bm.print('positive zero');
    bm.print(np.PZERO);
    bm.print('negative zero');
    bm.print(np.NZERO);

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.shape.html
    bm.print('');
    bm.print('Shape array');
    x := np.array(VarArrayOf([1, 2, 3, 4]));
    bm.print(x.shape);
    bm.print('Shape zeros');        
    y := np.zeros(VarArrayOf([2, 3, 4]));
    bm.print(y.shape);
    bm.print('Shape zeros custom tuple');        
    y.shape := VarArrayOf([3, 8]);
    bm.print(y);
    //y.shape := VarArrayOf([3, 6]); //As expected, it runs into an error. Use reshape instead.

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.strides.html
    bm.print('');
    bm.print('ndarray reshape');
    y := np.reshape(np.arange(2*3*4), VarArrayOf([2, 3, 4]));
    bm.print(y);
    bm.print('ndarray strides');
    bm.print(y.strides);
    
    //https://numpy.org/doc/stable/reference/arrays.indexing.html
    var indexing := NewPythonTuple(3);
    indexing.SetItem(0, 1);
    indexing.SetItem(1, 1);
    indexing.SetItem(2, 1);
    mm.y := y;
    bm.print(mm.y[indexing]); //y[1,1,1]
        
    var offset := bm.sum(y.strides * np.array(VarArrayOf([1,1,1])));
    bm.print(offset / y.itemsize);
    bm.print('ndarray transpose');
    x := np.reshape(np.arange(5*6*7*8), VarArrayOf([5,6,7,8])).transpose(2,3,1,0);
    bm.print(x.strides);
    var i := np.array(VarArrayOf([3,5,2,2]));

    //https://numpy.org/doc/stable/reference/arrays.indexing.html
    indexing := NewPythonTuple(4);
    indexing.SetItem(0, 3);
    indexing.SetItem(1, 5);
    indexing.SetItem(2, 2);
    indexing.SetItem(3, 2);
    mm.x := x;
    bm.print(mm.x[indexing]); //x[3,5,2,2]
    
    offset := bm.sum(i * x.strides);   
    bm.print(offset / x.itemsize);

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.ndim.html
    bm.print('');
    bm.print('ndarray ndim');
    x := np.array(VarArrayOf([1,2,3]));
    bm.print(x.ndim);
    y := np.zeros(VarArrayOf([2,3,4]));
    bm.print(y.ndim);

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.size.html
    bm.print('');
    bm.print('ndarray size');
    x := np.zeros(VarArrayOf([3, 5, 2]), dtype := np.complex128);
    bm.print(x.size);
    bm.print(np.prod(x.shape));

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.itemsize.html
    bm.print('');
    bm.print('ndarray itemsize');
    x := np.array(VarArrayOf([1, 2, 3]), dtype := np.float64);
    bm.print(x.itemsize);
    x := np.array(VarArrayOf([1, 2, 3]), dtype := np.complex128);
    bm.print(x.itemsize);

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.nbytes.html
    bm.print('');
    bm.print('ndarray nbytes');
    x := np.zeros(VarArrayOf([3, 5, 2]), dtype := np.complex128);
    bm.print(x.nbytes);
    bm.print(np.prod(x.shape) * x.itemsize);

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.base.html 
    bm.print('');
    bm.print('ndarray base');
    x := np.array(VarArrayOf([1, 2, 3, 4]));
    bm.print(VarIsNone(x.base));
    y := x.GetSlice(1, Ellipsis);
    bm.print(VarIsSame(y.base, x));    

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.dtype.html
    bm.print('');
    bm.print('ndarray dtype');
    x := np.array(VarPythonCreate([0,1,2,3], stTuple)); //Same effect of VarArrayOf
    bm.print(x.dtype);
    bm.print(bm.type(x.dtype));

    //https://numpy.org/doc/stable/reference/generated/numpy.ndarray.T.html
    bm.print('');
    bm.print('ndarray T'); 
    x := np.array(VarArrayOf([VarArrayOf([1., 2.]), VarArrayOf([3., 4.])]));
    bm.print(x);
    bm.print(x.T);
    x := np.array(VarArrayOf([1., 2., 3., 4.]));
    bm.print(x);
    bm.print(x.T);

    //https://numpy.org/doc/stable/reference/generated/numpy.transpose.html
    bm.print('');
    bm.print('ndarray transpose');
    x := np.arange(4).reshape(VarArrayOf([2,2]));
    bm.print(x);
    bm.print(np.transpose(x));
    x := np.ones(VarArrayOf([1, 2, 3]));
    bm.print(np.transpose(x, VarArrayOf([1, 0, 2])).shape);
    x := np.ones(VarArrayOf([2, 3, 4, 5]));
    bm.print(np.transpose(x).shape);

    //https://numpy.org/doc/stable/reference/generated/numpy.atleast_1d.html
    bm.print('');
    bm.print('ndarray atleast_1d'); 
    bm.print(np.atleast_1d(1.0));
    x := np.arange(9.0).reshape(3,3);
    bm.print(np.atleast_1d(x));
    bm.print(VarIsSame(np.atleast_1d(x), x));
    bm.print(np.atleast_1d(1, VarArrayOf([3, 4])));

    //https://numpy.org/doc/stable/reference/generated/numpy.atleast_2d.html
    bm.print('');
    bm.print('ndarray atleast_2d'); 
    bm.print(np.atleast_2d(3.0));
    x := np.arange(3.0);
    bm.print(np.atleast_2d(x));
    bm.print(VarIsSame(np.atleast_2d(x).base, x));
    bm.print(np.atleast_2d(1, VarArrayOf([1, 2]), VarArrayOf([VarArrayOf([1, 2])])));
    
    //https://numpy.org/doc/stable/reference/generated/numpy.atleast_3d.html
    bm.print('');
    bm.print('ndarray atleast_3d'); 
    bm.print(np.atleast_3d(3.0));
    x := np.arange(3.0);
    bm.print(np.atleast_3d(x).shape);
    x := np.arange(12.0).reshape(4,3);
    bm.print(np.atleast_3d(x).shape);
    bm.print(VarIsSame(np.atleast_3d(x).base, x.base));
    var iter := VarPyth.iter(np.atleast_3d(VarArrayOf([1, 2]), VarArrayOf([VarArrayOf([1, 2])]), VarArrayOf([VarArrayOf([VarArrayOf([1, 2])])])));
    while true do begin    
      try
        var arr := iter.__next__();
        bm.print(arr, arr.shape);
      except
        on E: EPyStopIteration do begin
          Break;
        end else
          raise;
      end;
    end;

    //https://numpy.org/doc/stable/reference/generated/numpy.broadcast.html
    bm.print('');
    bm.print('ndarray broadcast (Python list comprehension)');
    x := np.array(VarArrayOf([VarArrayOf([1]), VarArrayOf([2]), VarArrayOf([3])]));
    y := np.array(VarArrayOf([4, 5, 6]));
    var b := np.broadcast(x, y);
    //Using a Python list comprehension
    MainModule.b := b;
    var &out := np.empty(MainModule.b.shape);
    &out.flat := VarPythonEval('[u + v for (u, v) in b]');
    bm.print(&out);

    //Translated to Delphi
    bm.print('ndarray broadcast (Delphi iterate)');
    b := np.broadcast(x, y);
    &out := np.empty(MainModule.b.shape); 
    var list := NewPythonList(0);
    for iter in VarPyIterate(b) do begin
      var iter2 := Null;
      var sum := 0;
      for iter2 in VarPyIterate(iter) do begin
        Inc(sum, StrToInt(iter2));  
      end;
      list.append(sum);
    end;
    &out.flat := list;
    bm.print(&out);

    //https://numpy.org/doc/stable/reference/routines.bitwise.html
    bm.print('');
    bm.print('binary operations');
    //https://numpy.org/doc/stable/reference/generated/numpy.bitwise_and.html
    bm.print('bitwise and');
    bm.print(np.bitwise_and(13, 17));
    bm.print(np.bitwise_and(np.array(VarArrayOf([2,5,255])), np.array(VarArrayOf([3,14,16]))));    
    bm.print(np.binary_repr(12));
    bm.print(np.binary_repr(np.invert(np.array(13, dtype := np.uint8)), width=16));
    //https://numpy.org/doc/stable/reference/generated/numpy.invert.html
    bm.print('invert');
    bm.print(np.invert(np.array(13, dtype := np.uint8)));
    //https://numpy.org/doc/stable/reference/generated/numpy.packbits.html
    bm.print('packbits');
    var a := np.array(VarArrayOf([
      VarArrayOf([VarArrayOf([1,0,1]), VarArrayOf([0,1,0])]), 
      VarArrayOf([VarArrayOf([1,1,0]), VarArrayOf([0,0,1])])]));
    bm.print(np.packbits(a, axis := -1));
    //https://numpy.org/doc/stable/reference/generated/numpy.binary_repr.html
    bm.print('binary repr');    
    bm.print(np.binary_repr(3));
  end;
end;

end.
