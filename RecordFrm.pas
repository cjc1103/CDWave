unit RecordFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormatFrm, ComCtrls, StdCtrls, Buttons, MMSystem, Registry;



type
  TRecordMode = (rmTest, rmFile, rmAppend);

  TRecSettings = record
    Driver:  Integer;
    Silence: SmallInt;
    Loud:    SmallInt;
    StopTime: Cardinal;
    StopSilence: Cardinal;
    RecordMode: TRecordMode;
    ScheduleTime: TDateTime;
    StopOnTime,
    StopOnSilence,
    StartOnVoice,
    StartSchedule: Boolean;
    FileName: string;
  end;

type
  TDlgRecord = class(TFormatForm)
    BtnStart: TBitBtn;
    BtnCancel: TBitBtn;
    GrpDestination: TGroupBox;
    BtnBrowse: TSpeedButton;
    lFreeLabel: TLabel;
    lFreeSpace: TLabel;
    rbTestMode: TRadioButton;
    rbToFile: TRadioButton;
    eFilename: TEdit;
    SaveDialog: TSaveDialog;
    Label10: TLabel;
    cbDriver: TComboBox;
    BtnHelp: TBitBtn;
    lFileExists: TLabel;
    rbAppendMode: TRadioButton;
    procedure BtnBrowseClick(Sender: TObject);
    procedure eFilenameChange(Sender: TObject);
    procedure eFilenameExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure cbDriverChange(Sender: TObject);
    procedure rbModeClick(Sender: TObject);
  private
    { Private declarations }
    //function GetFileName: string;
    //procedure SetFileName(value: string);
    procedure SetAllowAppend(value: Boolean);
  protected
    function FormatValid: Boolean; override;
    procedure FormatChanged; override;
    procedure StoreInRegistry(Reg: TRegistry); virtual;
    procedure RetrieveFromRegistry(Reg: TRegistry); virtual;
  public
    { Public declarations }
    procedure GetSettings(var Settings: TRecSettings); virtual;
    //property FileName: string read GetFilename write SetFileName;
    property AllowAppend: boolean write SetAllowAppend;
  end;

implementation

uses MMUtil, Utils, WaveUtil;

{$R *.DFM}
resourcestring
  MinStr = ' minutes';
  UnknownStr = '<Unknown>';

function TDlgRecord.FormatValid: Boolean;
begin
  Result :=
    WaveInOpen(nil, cbDriver.ItemIndex-1, WaveFormat, 0, 0, WAVE_FORMAT_DIRECT_QUERY) =
    MMSYSERR_NOERROR;
end;

procedure TDlgRecord.GetSettings(var Settings: TRecSettings);
begin
  with Settings do
  begin
    eFileNameExit(eFilename);
    Driver := cbDriver.ItemIndex - 1;
    FileName := eFilename.Text;
    if rbToFile.Checked then
      RecordMode := rmFile
    else if rbTestMode.Checked then
    begin
      RecordMode := rmTest;
      FileName := '';
    end else
      RecordMode := rmAppend;
    StopOnTime := False;
    StopOnSilence := False;
    StartOnVoice := False;
    StopTime := high(DWORD)-1048576; // 4GB - 1MB
    StopSilence := StopTime;
  end;
end;

procedure TDlgRecord.BtnBrowseClick(Sender: TObject);
begin
  inherited;
  if SaveDialog.Execute then
  begin
    eFilename.Text := SaveDialog.FileName;
    lFileExists.Visible := FileExists(eFileName.Text);
    rbToFile.Checked := True;
  end;
end;

procedure TDlgRecord.eFilenameChange(Sender: TObject);
var s: Int64;
    f, m: string;
begin
  f := eFileName.Text;
  s := CalcDiskFreeSpace(ExtractFileDrive(f) + '\');
  if (s >= 0) and (WaveFormat.nAvgBytesPerSec <> 0) then
  begin
    str(s/(WaveFormat.nAvgBytesPerSec*60):3:0, m);
    lFreeSpace.Caption := IntToStr(s shr 20) + ' MB - ' + m + MinStr;
    lFileExists.Visible := FileExists(f);
    if Sender = eFileName then
    begin
      rbToFile.Checked := True;
      rbModeClick(nil);
    end;
  end else
    lFreeSpace.Caption := UnknownStr;
end;

procedure TDlgRecord.eFilenameExit(Sender: TObject);
var t: string;
begin
  inherited;
  with Sender as TEdit do
  begin
    t := Text;
    if (t <> '') then
      if (ExtractFileExt(t) = '') then
        t := t + '.wav';
      Text := ExpandFilename(t);
  end;
end;

procedure TDlgRecord.StoreInRegistry(Reg: TRegistry);
begin
  with Reg do
  begin
      WriteBool('FileMode', rbToFile.Checked);
      WriteString('RecDir', ExtractFileDir(eFileName.Text));
      WriteInteger('SampleFreq', StrToInt(cbFrequency.Text));
      WriteInteger('Channels', cbChannels.ItemIndex + 1);
      WriteInteger('Resolution', cbResolution.ItemIndex);
      WriteString('InputDevice', cbDriver.Text);
  end;
end;

procedure TDlgRecord.RetrieveFromRegistry(Reg: TRegistry);
var
  DriverName: string;
  i: Integer;
begin
   with Reg do
   begin
     rbToFile.Checked := ReadBool('FileMode');
     rbTestMode.Checked := not rbToFile.Checked;
     SaveDialog.InitialDir := ReadString('RecDir');
     i := ReadInteger('SampleFreq');
     if (i > 0) then
       cbFrequency.Text := IntToStr(i);
     i := ReadInteger('Channels') - 1;
     if (i >= 0) then
       cbChannels.ItemIndex := i;
     i := ReadInteger('Resolution');
     if (i >= 0) then
       cbResolution.ItemIndex := i;
     DriverName := ReadString('InputDevice');
     i := cbDriver.Items.IndexOf(DriverName);
     if i >= 0 then
       cbDriver.ItemIndex := i;
  end;
end;


procedure TDlgRecord.FormCreate(Sender: TObject);
var Reg: TRegistry;
procedure GetDrivers;
var i: Integer;
begin
  cbDriver.Items.Clear;
  for i := -1 to waveInGetNumDevs - 1 do
  begin
    cbDriver.Items.Add(GetWaveInName(i));
  end;
  cbDriver.ItemIndex := 0;
end;
begin
  cbChannels.ItemIndex := 1;
  cbResolution.ItemIndex := 1;
  Reg := TRegistry.Create;
  GetDrivers;
  with Reg do
  try
    if OpenKey('SOFTWARE', False) and
       OpenKey('MiLo', False) and
       OpenKey('CDWAV', False)
    then
    try
       RetrieveFromregistry(Reg);
    except
    end;
  finally
    Reg.Free;
    inherited;
  end;
  rbModeClick(nil);
end;

procedure TDlgRecord.FormDestroy(Sender: TObject);
var Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  with Reg do
  try
    if OpenKey('SOFTWARE', True) and
       OpenKey('MiLo', True) and
       OpenKey('CDWAV', True) then
    begin
      StoreInRegistry(reg);
    end;
  finally
    Reg.Free;
    inherited;
  end;
end;

procedure TDlgRecord.FormatChanged;
begin
  eFilenameChange(nil);
  btnStart.Enabled :=
    (rbTestMode.Checked or rbAppendMode.Checked or (eFilename.Text <> '')) and
    IsValid;
end;

procedure TDlgRecord.BtnHelpClick(Sender: TObject);
begin
  inherited;
  Application.HelpContext(1001);
end;

procedure TDlgRecord.cbDriverChange(Sender: TObject);
begin
  inherited;
  CheckValid;
end;

procedure TDlgRecord.rbModeClick(Sender: TObject);
begin
  inherited;
  btnStart.Enabled :=
    (rbTestMode.Checked or rbAppendMode.Checked or (eFilename.Text <> '')) and
    IsValid;
  GrpFormat.Enabled := not rbAppendMode.Checked;
end;

procedure TDlgRecord.SetAllowAppend;
begin
  rbAppendMode.Enabled := value;
end;

{function TDlgRecord.GetFileName;
begin
  Result := eFilename.Text;
end;

procedure TDlgRecord.SetFileName;
begin
  eFilename.Text := value;
  lFileExists.Visible := FileExists(value);
  rbToFile.Checked := True;
end;}

end.
