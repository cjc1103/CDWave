unit DlgRecShare;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RecordFrm, ComCtrls, StdCtrls, Buttons, Registry;

type
  TDlgRecordShare = class(TDlgRecord)
    GrpStop: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    cbStopTime: TCheckBox;
    cbStopSilence: TCheckBox;
    udStopTime: TUpDown;
    eStopTime: TEdit;
    eStopSilence: TEdit;
    udStopSilence: TUpDown;
    GrpOptions: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lSilence: TLabel;
    lLoud: TLabel;
    cbVoiceActivated: TCheckBox;
    eSilence: TEdit;
    eLoud: TEdit;
    udSilence: TUpDown;
    udLoud: TUpDown;
    GroupBox1: TGroupBox;
    dtScheduleTime: TDateTimePicker;
    cbSchedule: TCheckBox;
    procedure eSilenceChange(Sender: TObject);
    procedure eLoudChange(Sender: TObject);
    procedure eStopTimeExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure StoreInRegistry(Reg: TRegistry); override;
    procedure RetrieveFromRegistry(Reg: TRegistry); override;
    procedure FormatChanged; override;
  public
    { Public declarations }
    procedure GetSettings(var Settings: TRecSettings); override;
  end;

implementation

uses MMSystem;
{$R *.DFM}

procedure TDlgRecordShare.StoreInRegistry(Reg: TRegistry);
begin
  inherited;
  with Reg do begin
      WriteBool('StopOnTime', cbStopTime.Checked);
      WriteBool('StopOnSilence', cbStopSilence.Checked);
      WriteBool('StartOnVoice', cbVoiceActivated.Checked);
      WriteInteger('Loud', udLoud.Position);
      WriteInteger('Silence', udSilence.Position);
      WriteInteger('StopTime', udStopTime.Position);
      WriteInteger('StopSilence', udStopSilence.Position);
  end;
end;

procedure TDlgRecordShare.RetrieveFromRegistry(Reg: TRegistry);
begin
  inherited;
  with Reg do begin
         cbStopTime.Checked       := ReadBool('StopOnTime');
         cbStopSilence.Checked    := ReadBool('StopOnSilence');
         cbVoiceActivated.Checked := ReadBool('StartOnVoice');
         udLoud.Position          := ReadInteger('Loud');
         udSilence.Position       := ReadInteger('Silence');
         udStopTime.Position      := ReadInteger('StopTime');
         udStopSilence.Position   := ReadInteger('StopSilence');
  end;
end;

procedure TDlgRecordShare.GetSettings(var Settings: TRecSettings);
begin
  inherited;
  with Settings do
  begin
    StopOnTime := cbStopTime.Checked;
    StopOnSilence := cbStopSilence.Checked;
    StartOnVoice := cbVoiceActivated.Checked;
    Loud := udLoud.Position;
    Silence := udSilence.Position;
    if StopOnTime then
      StopTime := DWORD(udStopTime.Position) * 60 * WaveFormat.nAvgBytesPerSec  {in bytes}
    else
      StopTime := high(DWORD)-1048576; // 4GB - 1MB
    if StopOnSilence then
      StopSilence := DWORD(udStopSilence.Position) * WaveFormat.nAvgBytesPerSec {in bytes}
    else
      StopSilence := StopTime;
    StartSchedule := cbSchedule.Checked;
    if StartSchedule then
    begin
      ScheduleTime := dtScheduleTime.Time;
    end else
      ScheduleTime := 0;
  end;
end;

procedure TDlgRecordShare.eSilenceChange(Sender: TObject);
var s: string;
begin
  str(udSilence.Position * (100 / 32768):5:2, s);
  lSilence.Caption := s;
end;

procedure TDlgRecordShare.eLoudChange(Sender: TObject);
var s: string;
begin
  str(udLoud.Position * (100 / 32768):5:2, s);
  lLoud.Caption := s;
end;

procedure TDlgRecordShare.FormatChanged;
var m: Cardinal;
begin
  inherited;
  if (WaveFormat.nAvgBytesPerSec <> 0) then
  begin
    m := (high(Cardinal)-1048576) div (60 * WaveFormat.nAvgBytesPerSec);
    if m > Cardinal(high(SmallInt)) then
      udStopTime.Max := high(SmallInt)
    else
      udStopTime.Max := SmallInt(m);
    eStopTime.Text := IntToStr(udStopTime.Position);
  end;
end;

procedure TDlgRecordShare.eStopTimeExit(Sender: TObject);
begin
  inherited;
  udStopTime.Position := StrToInt(eStopTime.Text);
end;

procedure TDlgRecordShare.FormActivate(Sender: TObject);
var h,m,s,ms: Word;
begin
  inherited;
  dtScheduleTime.Date := Date;
  DecodeTime(Time, h, m, s, ms);
  dtScheduleTime.Time := EncodeTime(h, m, 0, 0);
end;

end.
