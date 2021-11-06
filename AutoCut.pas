unit AutoCut;
// WM_HSCROLL
interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls;

type
  TAutoCutDlg = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    eLevel: TEdit;
    udLevel: TUpDown;
    eTime: TEdit;
    udTime: TUpDown;
    Label3: TLabel;
    lPercent: TLabel;
    Label4: TLabel;
    btnHelp: TBitBtn;
    btnApply: TBitBtn;
    Label5: TLabel;
    tbRelPos: TTrackBar;
    lRelPos: TLabel;
    procedure eLevelChange(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure tbRelPosChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}
uses Main, Registry;

procedure TAutoCutDlg.eLevelChange(Sender: TObject);
var s: String;
begin
  Str(udLevel.Position/327.67 :3:2, s);
  lPercent.Caption := s + '%';
end;

procedure TAutoCutDlg.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(2001);
end;


procedure TAutoCutDlg.btnApplyClick(Sender: TObject);
begin
  TMainForm(Owner).AutoSplit(
    -udLevel.Position,
    udLevel.Position,
    udTime.Position,
    tbRelPos.Position);
end;

procedure TAutoCutDlg.tbRelPosChange(Sender: TObject);
begin
  lRelPos.Caption := IntToStr(tbRelPos.Position) + '%';
end;

procedure TAutoCutDlg.FormDestroy(Sender: TObject);
var Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  with Reg do
  try
    if OpenKey('SOFTWARE', True) and
       OpenKey('MiLo', True) and
       OpenKey('CDWAV', True) then
    with Reg do begin
        WriteInteger('AutoSplitLevel', udLevel.Position);
        WriteInteger('AutoSplitTime', udTime.Position);
        WriteInteger('AutoSplitPos', tbRelPos.Position);
    end;
  finally
    Reg.Free;
    inherited;
  end;
end;

procedure TAutoCutDlg.FormCreate(Sender: TObject);
var Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  with Reg do
  try
    if OpenKey('SOFTWARE', False) and
       OpenKey('MiLo', False) and
       OpenKey('CDWAV', False)
    then
    try
        udLevel.Position := ReadInteger('AutoSplitLevel');
        udTime.Position := ReadInteger('AutoSplitTime');
        tbRelPos.Position := ReadInteger('AutoSplitPos');
        tbRelPosChange(nil);
    except
    end;
  finally
    Reg.Free;
    inherited;
  end;
end;

end.
