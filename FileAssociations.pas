unit FileAssociations;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellUtils;

type
  TFileAssociationDlg = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    GroupBox1: TGroupBox;
    cbAsDefault: TCheckBox;
    btnHelp: TBitBtn;
    cbRegister: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure rbRegisterClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FileAssociationDlg: TFileAssociationDlg;

implementation

{$R *.DFM}

procedure TFileAssociationDlg.btnOkClick(Sender: TObject);
var
  namebuf: array[0..MAX_PATH] of char;
  ModuleName: string;
procedure AddIt(AsDefault: Boolean);
begin
end;
begin
  if GetModuleFilename(GetModuleHandle(nil), namebuf, sizeof(namebuf)) = 0 then
     raise Exception.CreateFmt('Error (%d) cannot determine exe name', [GetLastError()]);
  ModuleName := namebuf;
  if cbRegister.Checked then
    AddAssociate('.wav', 'wavfile', 'Wave audio', 'Open with CDWav',
                 'Open with &CDWav', ModuleName + ' "%L"', cbAsDefault.Checked)
end;

procedure TFileAssociationDlg.rbRegisterClick(Sender: TObject);
begin
  cbAsDefault.Enabled := cbRegister.Checked;
end;

procedure TFileAssociationDlg.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(300);
end;

end.
