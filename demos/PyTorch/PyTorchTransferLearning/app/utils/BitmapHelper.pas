unit BitmapHelper;

interface

uses
  FMX.Graphics, System.Classes;

type
  TBitmapHelper = class helper for TBitmap
  public
    procedure ToJpg(const AStream: TStream);
  end;

implementation

uses
  FMX.Surfaces, FMX.Types, FMX.Consts;

{ TBitmapHelper }

procedure TBitmapHelper.ToJpg(const AStream: TStream);
begin
  var LSurface := TBitmapSurface.Create();
  try
    LSurface.Assign(Self);
    if not TBitmapCodecManager.SaveToStream(AStream, LSurface, '.jpg') then
      raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
  finally
    LSurface.Free();
  end;
end;

end.
