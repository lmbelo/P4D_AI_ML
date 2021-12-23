unit Form.Classify;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Media, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, FMX.Layouts, System.JSON;

type
  TClassifyForm = class(TForm)
    imgCamera: TImage;
    ccCameraComp: TCameraComponent;
    BindingsList1: TBindingsList;
    Layout1: TLayout;
    Switch1: TSwitch;
    StyleBook1: TStyleBook;
    LinkControlToPropertyActive: TLinkControlToProperty;
    recBackground: TRectangle;
    Layout2: TLayout;
    Layout3: TLayout;
    tbThumbs: TTrackBar;
    Image1: TImage;
    Image2: TImage;
    SpeedButton1: TSpeedButton;
    Image3: TImage;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    procedure ccCameraCompSampleBufferReady(Sender: TObject;
      const ATime: TMediaTime);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FProfile: string;
    FRecognizing: boolean;
  public
    property Profile: string read FProfile write FProfile;
  end;

var
  ClassifyForm: TClassifyForm;

implementation

uses
  DateUtils,
  BitmapHelper, Remote.ClientModule;

{$R *.fmx}

procedure TClassifyForm.ccCameraCompSampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  if not FRecognizing then
    TThread.Synchronize(TThread.CurrentThread, procedure() begin
      ccCameraComp.SampleBufferToBitmap(imgCamera.Bitmap, true);
    end);
end;

procedure TClassifyForm.FormCreate(Sender: TObject);
begin
  FRecognizing := false;
end;

procedure TClassifyForm.SpeedButton1Click(Sender: TObject);
begin
  FRecognizing := true;
  try
    var LStream := TMemoryStream.Create();
    try
      imgCamera.Bitmap.ToJpg(LStream);
      var LResult := ClientModule.TrainingClassClient.Recognize(Profile, LStream);
      if LResult is TJSONObject then begin
        var LError: string;
        if LResult.TryGetValue<string>('error', LError) then
          ShowMessage(LError)
        else begin
          var LValue: Extended;
          if LResult.TryGetValue<Extended>('value', LValue) then begin
            tbThumbs.Value := Trunc(((1 - LValue) / 1 * 100) * 100 / 100);
          end;
        end;
      end;
    finally
      LStream.Free();
    end;
 finally
    FRecognizing := false;
 end;
end;

end.
