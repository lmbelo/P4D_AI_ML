unit RegNumPy;

interface

procedure Register();

implementation

uses
  Classes, NumPy;

procedure Register();
begin
  RegisterComponents('NumPy', [TNumPy]);
end;

end.
