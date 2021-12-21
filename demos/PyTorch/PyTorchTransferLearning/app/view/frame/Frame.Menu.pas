unit Frame.Menu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.ComboEdit, FMX.Layouts;

type
  TMenuFrame = class(TFrame)
    loContainer: TLayout;
    loCollapse: TLayout;
    loProfile: TLayout;
    ceProfile: TComboEdit;
    lbProfile: TLabel;
    loActions: TLayout;
    btnCollectData: TButton;
    btnLiveRecognition: TButton;
    btnQuit: TButton;
    btnTrainModel: TButton;
    btnContinue: TSpeedButton;
  private
    { Private declarations }
  public
    procedure EnableActions();
    procedure DisableActions();
  end;

implementation

{$R *.fmx}

procedure TMenuFrame.DisableActions;
begin
  btnCollectData.Enabled := false;
  btnLiveRecognition.Enabled := false;
end;

procedure TMenuFrame.EnableActions;
begin
  btnCollectData.Enabled := true;
  btnLiveRecognition.Enabled := true;
end;

end.
