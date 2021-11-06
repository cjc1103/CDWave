unit DirDlg;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, FileCtrl, Buttons, Utils;

type
  TSelectDirDlg = class(TForm)
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    lDirectory: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    lRequired: TLabel;
    lAvailable: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure DriveComboBox1Change(Sender: TObject);
  protected
    FRequired, FAvailable: Integer;
    function GetDirectory: string;
    procedure SetDirectory(const Value: string);
    procedure SetRequired(Value: Integer);
    procedure SetAvailable(Value: Integer);
  public
    property Required: Integer read FRequired write SetRequired;
    property Available: Integer read FAvailable write SetAvailable;
    property Directory: string read GetDirectory write SetDirectory;
  end;

var
  SelectDirDlg: TSelectDirDlg;

implementation

{$R *.DFM}

procedure TSelectDirDlg.SetDirectory;
begin
  if Value[2] = ':' then DriveCombobox1.Drive := Value[1];
  DirectoryListbox1.Directory := Value;
end;

function TSelectDirDlg.GetDirectory;
begin
  Result := DirectoryListbox1.Directory;
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
end;

procedure TSelectDirDlg.SetRequired;
begin
  FRequired := Value;
  lRequired.Caption := IntToStr(Value shr 10);
  if Available < Value then
    lAvailable.Font.Color := clMaroon
  else
    lAvailable.Font.Color := clWindowText;
end;

procedure TSelectDirDlg.SetAvailable;
begin
  FAVailable := Value;
  if Value = MaxInt then
    lAvailable.Caption := 'Too much'
  else
    lAvailable.Caption := IntToStr(Value shr 10);
  if Value < Required then
    lAvailable.Font.Color := clMaroon
  else
    lAvailable.Font.Color := clWindowText;
end;

procedure TSelectDirDlg.DriveComboBox1Change(Sender: TObject);
var s: Comp;
begin
  s := CalcDiskFreeSpace(DriveCombobox1.Drive + ':\');
  if s > MaxInt then Available := MaxInt else Available := Round(s);
end;

end.
