unit SelectDirFrm;

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormatFrm, StdCtrls, Buttons, FileCtrl, mmsystem, msacm, ComCtrls,
  Menus, ExtCtrls, ActnList, ImgList, Lame_Enc, Vorbis, FLAC, FLAC_Enc,
  APE, DirNameFrm, ShlBrowseFolder;

type
  TSelectDirDlg = class(TFormatForm)
    Panel3: TPanel;
    Panel4: TPanel;
    eDirectory: TEdit;
    btnBrowse: TBitBtn;
    Panel2: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    btnHelp: TBitBtn;
    lblLoc: TLabel;
    Panel1: TPanel;
    PanelFormat: TPanel;
    ModePanel: TPanel;
    Label1: TLabel;
    cbMode: TComboBox;
    grpMP3: TGroupBox;
    lMP3Status: TLabel;
    lBadMP3Format: TLabel;
    Label11: TLabel;
    cbMP3Quality: TComboBox;
    rbPreset: TRadioButton;
    rbVBR: TRadioButton;
    rbCBR: TRadioButton;
    cbBitrate: TComboBox;
    grpOGG: TGroupBox;
    lOGGStatus: TLabel;
    Label6: TLabel;
    lBadOGGFormat: TLabel;
    cbOGGQuality: TComboBox;
    GrpNoConversion: TGroupBox;
    lblDirectEx: TLabel;
    cbOld24: TCheckBox;
    grpFLAC: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    cbFLACLevel: TComboBox;
    grpAPE: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    cbAPELevel: TComboBox;
    grpSpace: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    lRequired: TLabel;
    lAvailable: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetACMFormatFilter(var afc: TACMFORMATCHOOSE); override;
    procedure btnHelpClick(Sender: TObject);
    procedure cbModeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbOGGQualityChange(Sender: TObject);
    procedure cbMP3QualityChange(Sender: TObject);
    procedure cbFLACLevelChange(Sender: TObject);
    procedure rbPresetClick(Sender: TObject);
    procedure cbAPELevelChange(Sender: TObject);
    procedure eDirectoryChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
    FActivePanel: TControl;
    function GetConvertToFormat: PWaveFormatEx;
    procedure CalculateSpaceRequirements;
    procedure InitMP3format;
    procedure InitOGGformat;
    procedure InitFLACFormat;
    procedure InitAPEFormat;
    procedure UpdateOKbutton;
  protected
    FRequired, FAvailable: Int64;
    FValidPath: Boolean;
    FSourceWaveFormat: PWaveFormatEx;
    FOld24Format: TWaveFormatEx;
    FMP3Format: TBEWaveFormat;
    FOGGFormat: TOGGWaveFormat;
    FFLACFormat: TFlacWaveFormat;
    FAPEFormat: TAPEWaveFormat;
    FDirNameForm: TDirNameForm;
    procedure FormatChanged; override;
    function GetDirectory: string;
    procedure SetDirectory(const Value: string);
    procedure SetRequired(Value: Int64);
    procedure SetAvailable(Value: Int64);
    procedure SetSourceWaveFormat(fmt: PWaveFormatEx);
    function  GetSourceWaveFormat: PWaveFormatEx;
    function GetDirName: string;
    property Available: Int64 read FAvailable write SetAvailable;
  public
    NewFolderName: string;
    property SourceWaveFormat: PWaveFormatEx read GetSourceWaveFormat write SetSourceWaveFormat;
    property ConvertToFormat: PWaveFormatEx read GetConvertToFormat;
    property Required: Int64 write SetRequired;
    property Directory: string read GetDirectory write SetDirectory;
  end;


implementation

uses Utils, Registry, Settings, WaveUtil;
{$R *.DFM}

resourcestring
  strNoVorbisDll = 'VORBIS.DLL not found';
  strNoLameDll = 'LAME_ENC.DLL not found';
  strNoCreateDir = 'Select a folder, cannot create a directory here';
  strCreateDirFailed = 'Failed to create directory';
  strSelectOutDir = 'Select output directory';
type

    TMP3Format = record
      name:   string;
      preset: LongInt;
      bitrate: Word;
    end;

const
  MP3FormatsCount = 11;
  MP3DefaultFormat = 6;
  MP3Formats: array[0..MP3FormatsCount-1] of TMP3Format = (
   (name: 'Voice (56k, mono)';  preset: LQP_VOICE; bitrate: 56),
   (name: 'Radio (~112k)';  preset: LQP_RADIO; bitrate: 112),
   (name: 'Tape (~112k)';   preset: LQP_TAPE; bitrate: 112),
   (name: 'HiFi (~160k)';   preset: LQP_HIFI; bitrate: 160),
   (name: 'CD (~192k)';     preset: LQP_CD; bitrate: 192),
   (name: 'Studio (~256k)';  preset: LQP_STUDIO; bitrate: 256),
   (name: 'Standard (170-210k)'; preset: LQP_STANDARD; bitrate: 192),
   (name: 'Fast Standard';      preset: LQP_FAST_STANDARD; bitrate: 192),
   (name: 'Extreme (200-240k)';  preset: LQP_EXTREME; bitrate: 224),
   (name: 'Fast Extreme';       preset: LQP_FAST_EXTREME; bitrate: 224),
   (name: 'Insane (320 kbps)';  preset: LQP_INSANE; bitrate: 320)
  );



const OGGKBPS: array[0..10] of Integer = (60,80,96,112,128,160,192,224,256,320,498);

procedure TSelectDirDlg.SetDirectory;
var l: Integer;
    dir: string;
begin
  if (value <> '') then
  begin
    l := Length(value);
    if (l > 3) and (value[l] = '\') then
      dir := copy(value, 0, l-1)
    else
      dir := value;
    if DirectoryExists(dir) then
    begin
      eDirectory.Text := dir;
    end;
  end;
end;

function TSelectDirDlg.GetDirectory;
begin
  Result := eDirectory.Text;
  if not DirectoryExists(result) then
  begin
    raise Exception.Create('Directory does not exist');
  end;
  if Result[Length(Result)] <> '\' then
     result := Result + '\';
  //Result := DirectoryTree1.Directory;
end;

procedure TSelectDirDlg.CalculateSpaceRequirements;
var v: int64;
begin
  if (cbMode.ItemIndex = 0) or (SourceWaveFormat = nil) then
    v := FRequired
  else begin
    v := FRequired;
    v := v * int64(ConvertToFormat.nAvgBytesPerSec);
    v := v div SourceWaveFormat.nAvgBytesPerSec;
  end;
  lRequired.Caption := IntToStr(v shr 10);
  if Available < v then
    lAvailable.Font.Color := clMaroon
  else
    lAvailable.Font.Color := clWindowText;
end;

procedure TSelectDirDlg.SetRequired;
begin
  FRequired := Value;
  CalculateSpaceRequirements;
end;

procedure TSelectDirDlg.SetAvailable;
begin
  FAVailable := Value;
  lAvailable.Caption := IntToStr(Value shr 10);
  CalculateSpaceRequirements;
end;

procedure TSelectDirDlg.InitOGGformat;
var q: Integer;
    kbps: Integer;
procedure Validformat(value: Boolean);
begin
  lBadOGGFormat.Visible := not value;
  grpOgg.Tag := Integer(not value);
end;
begin
  FillChar(FOGGFormat, sizeof(FOGGFormat), #0);
  FOGGFormat.WaveFormat.wFormatTag := WAVE_FORMAT_OGG_VORBIS;
  FOGGFormat.WaveFormat.cbSize := SizeOf(FOGGFormat.OGG);
  with FOGGFormat.OGG do
  begin
      dwConfig := VB_CONFIG_MP3;
      dwStructVersion := VB_ENC_STRUCT_VERSION;
      dwStructSize := SizeOf(FOGGFormat.OGG);
      // BASIC ENCODER SETTINGS
      dwChannels := 2;
      dwSampleRate := 44100;
      //   lMinBitrate := 96000;    // min bitrate
      //   lNomBitrate := 128000;   // Normal bitrate
      //   lMaxBitrate := 192000;   // Max bitrate
      //   dwInfoMode := VB_MODE_B; // What's the purpose of this parameter?
      // The relation between fQuality and nominal bitrate is as follows
      //   0.0 :  60KBPS, 0.1 :  80KBPS, 0.2 :  96KBPS, 0.3 : 112KBPS
      //   0.4 : 128KBPS, 0.5 : 160KBPS, 0.6 : 192KBPS, 0.7 : 224KBPS
      //   0.8 : 256KBPS, 0.9 : 320KBPS, 1.0 : 498KBPS
      //  note) The average bitrate is variable according to the audio data.
      if not grpOGG.Enabled then
      begin
        ValidFormat(False);
        Exit;
      end;
      q := cbOGGQuality.ItemIndex;
      kbps := OGGKBPS[q];
      fQuality := q / 10;
  end;
  FOGGFormat.WaveFormat.nAvgBytesPerSec := kbps * 128; // nominal bitrate
  if SourceWaveFormat <> nil then
  begin
//    if SourceWaveformat.wBitsPerSample = 16 then
//    begin
      FOGGFormat.WaveFormat.wBitsPerSample := 16;
      ValidFormat(True);
//    end else
//      Validformat(False);
    FOGGFormat.WaveFormat.nChannels := SourceWaveformat.nChannels;
    FOGGFormat.WaveFormat.nSamplesPerSec := SourceWaveformat.nSamplesPerSec;
    FOGGFormat.OGG.dwSampleRate := SourceWaveformat.nSamplesPerSec;
    FOGGFormat.OGG.dwChannels := SourceWaveformat.nChannels;
  end;
end;

procedure TSelectDirDlg.InitMP3format;
var kbps: Integer;
procedure Validformat(value: Boolean);
begin
  lBadMP3Format.Visible := not value;
  grpMP3.Tag := Integer(not value);
end;
begin
  FillChar(FMP3Format, SizeOf(FMP3Format), #0);
  FMP3Format.WaveFormat.cbSize := sizeof(BE_CONFIG);
  FMP3Format.WaveFormat.wFormatTag := WAVE_FORMAT_LAME_MP3;
  FMP3Format.BE.dwConfig := BE_CONFIG_LAME;
  FMP3Format.BE.format.LHV1.dwStructVersion := 1;
  FMP3Format.BE.format.LHV1.dwStructSize := sizeof(BE_CONFIG);
  FMP3Format.BE.format.LHV1.dwSampleRate := 44100;	    // INPUT FREQUENCY
  FMP3Format.BE.format.LHV1.dwReSampleRate := 0;	    // DON"T RESAMPLE
  FMP3Format.BE.format.LHV1.nMode := BE_MP3_MODE_JSTEREO;   // OUTPUT IN STREO
  FMP3Format.BE.format.LHV1.dwBitrate := 0;
  FMP3Format.BE.format.LHV1.dwMaxBitrate := 320;
  FMP3Format.BE.format.LHV1.nPreset := LQP_VERYHIGH_QUALITY;// QUALITY PRESET SETTING
  FMP3Format.BE.format.LHV1.dwMpegVersion := MPEG1;	    // MPEG VERSION (I or II)
  FMP3Format.BE.format.LHV1.dwPsyModel := 0;		    // USE DEFAULT PSYCHOACOUSTIC MODEL
  FMP3Format.BE.format.LHV1.dwEmphasis := 0;		    // NO EMPHASIS TURNED ON
  //FMP3Format.BE.format.LHV1.bOriginal := TRUE;	    // SET ORIGINAL FLAG
  FMP3Format.BE.format.LHV1.bNoRes := TRUE;		    // No Bit resorvoir
  FMP3Format.WaveFormat.cbSize := Sizeof(FMP3Format.BE);
  if not grpMP3.Enabled then
  begin
    ValidFormat(False);
    Exit;
  end;
  with FMP3Format.BE.format.LHV1 do
  begin
    if rbPreset.Checked then
    begin
      // Using one of the presets
      nPreset := MP3Formats[cbMP3Quality.ItemIndex].preset;
      kbps := MP3Formats[cbMP3Quality.ItemIndex].bitrate;
      bWriteVBRHeader := TRUE;
      nVbrMethod := VBR_METHOD_DEFAULT;
    end else
    begin
      // Using CBR or VBR
      kbps := StrToInt(cbBitrate.Text);
      //nPreset := LQP_NOPRESET;
      if rbVBR.Checked then
      begin
        // Using VBR
        nPreset := LQP_ABR;
        dwBitrate := 32;
        dwMaxBitrate := 320;
        bEnableVBR := True;
        bWriteVBRHeader := TRUE;
        nVBRQuality := 2;
        dwVbrAbr_bps := kbps * 1000; //0; //kbps;
        nVbrMethod := VBR_METHOD_ABR; // VBR_METHOD_DEFAULT;
      end else begin
        // Using CBR
        nPreset := LQP_CBR;
        dwBitrate := kbps;
        dwMaxBitrate := 0;
        bEnableVBR := False;
        bWriteVBRHeader := False;
        nVbrMethod := VBR_METHOD_NONE;
      end;
    end;
  end;
  FMP3Format.WaveFormat.nAvgBytesPerSec := kbps * 128;
  if SourceWaveFormat <> nil then
  begin
//    if SourceWaveformat.wBitsPerSample = 16 then
//    begin
      FMP3Format.WaveFormat.wBitsPerSample := 16;
      Validformat(True);
//    end else
//      ValidFormat(False);
    FMP3Format.WaveFormat.nChannels := SourceWaveformat.nChannels;
    case SourceWaveformat.nChannels of
      1: FMP3Format.BE.format.LHV1.nMode := BE_MP3_MODE_MONO;
      2: FMP3Format.BE.format.LHV1.nMode := BE_MP3_MODE_JSTEREO;
      else ValidFormat(False);
    end;
    FMP3Format.WaveFormat.nSamplesPerSec :=  SourceWaveformat.nSamplesPerSec;
    FMP3Format.BE.format.LHV1.dwSampleRate := SourceWaveformat.nSamplesPerSec;
  end;
end;

procedure TSelectDirDlg.InitFLACFormat;
begin
  if SourceWaveformat <> nil then
  begin
    Move(SourceWaveFormat^, FFLACFormat, sizeof(TWaveFormat));
    FFLACFormat.WaveFormat.wFormatTag := WAVE_FORMAT_FLAC;
    FFLACFormat.WaveFormat.cbSize := sizeof(TFlacWaveFormat) - sizeof(TWaveFormatEx);
    FFLACFormat.Level := cbFLACLevel.ItemIndex;
    // Expect 40% compression //
    FFLACFormat.WaveFormat.nAvgBytesPerSec := (SourceWaveFormat.nAvgBytesPerSec * 6) div 10;
  end;
end;

procedure TSelectDirDlg.InitAPEFormat;
begin
  if SourceWaveformat <> nil then
  begin
    Move(SourceWaveFormat^, FAPEFormat, sizeof(TWaveFormat));
    FAPEFormat.WaveFormat.wFormatTag := WAVE_FORMAT_APE;
    FAPEFormat.WaveFormat.cbSize := sizeof(TAPEWaveFormat) - sizeof(TWaveFormatEx);
    FAPEFormat.Level := cbAPELevel.ItemIndex * 1000 + 1000;
    // Expect 40% compression //
    FAPEFormat.WaveFormat.nAvgBytesPerSec := (SourceWaveFormat.nAvgBytesPerSec * 6) div 10;
  end;
end;

procedure TSelectDirDlg.FormCreate(Sender: TObject);
var
  reg: TRegistry;
  version: BE_VERSION;	// version info
  i: Integer;
  path: string;
begin
  inherited;
  Reg := TRegistry.Create;
  with Reg do
  try
    if OpenKey(CDWAVRegKey, False) then
      path := reg.ReadString('SaveDir')
    else
      path := GetCurrentDir; //.CurrentDirectory;
  except
    path := GetCurrentDir;
    //DirectoryTree1.CurrentDirectory;
  end;
  Directory := path;
  GrpFormat.Parent := PanelFormat;
  GrpFormat.Align := alTop;
  cbMode.Items.Objects[0] := grpNoConversion;
  cbMode.Items.Objects[1] := grpFormat;
  cbMode.Items.Objects[2] := grpMP3;
  cbMode.Items.Objects[3] := grpOGG;
  cbMode.Items.Objects[4] := grpFLAC;
  cbMode.Items.Objects[5] := grpAPE;
  try
    InitLame;
    procBeVersion(version);
    lMP3Status.Caption :=
        'lame_enc.dll ' + IntToStr(version.byDLLMajorVersion) +
        '.' + IntToStr(version.byDLLMajorVersion) + '  ' +
        IntToStr(version.wYear) + '-' + IntToStr(version.byMonth) + '-' +
        IntToStr(version.byDay);
    for i := 0 to MP3FormatsCount-1 do
      cbMP3Quality.Items.Add(MP3Formats[i].name);
    cbMP3Quality.ItemIndex := MP3DefaultFormat;
    cbBitrate.ItemIndex := 5;
  except
    lMP3Status.Caption := strNoLameDll;
    grpMP3.Enabled := False;
    grpMP3.Tag := 1;
  end;
  InitMP3format;
  try
    InitVorbisDLL;
    lOGGStatus.Caption := VorbisDLLVersionStr;
    for i := 0 to 10 do
      cbOGGQuality.Items.Add(IntToStr(i) + ' (kbps: ' + IntToStr(OGGKBPS[i]) + ')');
    cbOGGQuality.ItemIndex := 5;
  except
    lOGGStatus.Caption := strNoVorbisDll;
    grpOGG.Enabled := False;
    grpOGG.Tag := 1;
  end;
  InitOGGFormat;
  if LibFLACLoaded then
  begin
    cbFLACLevel.ItemIndex := 5;
    InitFLACFormat;
  end else begin
    grpFLAC.Tag := 1;
    grpFLAC.Enabled := False;
  end;
  APELoadDLL;
  if LibAPELoaded then
  begin
    InitAPEFormat;
  end else begin
    grpAPE.Tag := 1;
    grpAPE.Enabled := False;
  end;
  cbMode.ItemIndex := 0;
end;

procedure TSelectDirDlg.FormDestroy(Sender: TObject);
var
  reg: TRegistry;
begin
  inherited;
  Reg := TRegistry.Create;
  with Reg do
  try
    if OpenKey(CDWAVRegKey, True) then
    begin
      reg.WriteString('SaveDir', Directory);
    end;
  except
  end;
end;

procedure TSelectDirDlg.GetACMFormatFilter;
begin
  afc.fdwStyle := ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT;
  afc.pwfxEnum := SourceWaveFormat;
  afc.fdwEnum  := ACM_FORMATENUMF_CONVERT;
end;

function TSelectDirDlg.GetConvertToFormat;
begin
  case cbMode.ItemIndex of
   1: Result := WaveFormat;
   2: Result := @FMP3Format;
   3: Result := @FOGGFormat;
   4: Result := @FFLACFormat;
   5: REsult := @FAPEFormat;
   else Result := nil;
  end;
end;

procedure TSelectDirDlg.SetSourceWaveFormat;
begin
  FSourceWaveFormat := fmt;
  CopyWaveFormat(fmt);
  InitMP3format;
  InitOGGFormat;
  InitFLACFormat;
  InitAPEFormat;
end;

function TSelectDirDlg.GetSourceWaveFormat;
begin
  if cbOld24.Enabled and cbOld24.Checked and (cbMode.ItemIndex = 0) then
  begin
    Move(FSourceWaveFormat^, FOld24Format, sizeof(TWaveFormatEx));
    FOld24Format.wFormatTag := WAVE_FORMAT_PCM;
    FOld24Format.cbSize := 0;
    Result := @FOld24Format;
  end else
    result := FSourceWaveFormat;
end;

procedure TSelectDirDlg.FormatChanged;
begin
  cbOld24.Enabled :=
    (WaveFormat.wFormatTag = WAVE_FORMAT_EXTENSIBLE) and
    (cbMode.ItemIndex = 0) and
    (WaveFormat.nChannels <= 2) and
    (WaveFormat.wBitsPerSample = 24);
  CalculateSpaceRequirements;
  UpdateOKbutton;
end;

procedure TSelectDirDlg.btnHelpClick(Sender: TObject);
begin
  inherited;
  Application.HelpContext(HelpContext);
end;

procedure TSelectDirDlg.cbModeChange(Sender: TObject);
var c: Tcontrol;
begin
  inherited;
  c := cbMode.Items.Objects[cbMode.ItemIndex] as TControl;
  if c <> FActivePanel then
  begin
    if FActivePanel <> nil then FActivePanel.Visible := False;
    if c <> nil then c.Visible := True;
    FActivePanel := c;
  end;
  FormatChanged;
end;

procedure TSelectDirDlg.FormShow(Sender: TObject);
begin
  inherited;
  cbModeChange(cbMode);
end;

procedure TSelectDirDlg.cbOGGQualityChange(Sender: TObject);
begin
  inherited;
  InitOGGformat;
  FormatChanged;
end;

procedure TSelectDirDlg.cbMP3QualityChange(Sender: TObject);
begin
  inherited;
  InitMP3Format;
  FormatChanged;
end;

procedure TSelectDirDlg.cbFLACLevelChange(Sender: TObject);
begin
  inherited;
  InitFLACFormat;
  FormatChanged;
end;

procedure TSelectDirDlg.cbAPELevelChange(Sender: TObject);
begin
  inherited;
  InitAPEFormat;
  FormatChanged;
end;


procedure TSelectDirDlg.rbPresetClick(Sender: TObject);
begin
  inherited;
  cbMP3Quality.Enabled := rbPreset.Checked;
  cbBitrate.Enabled := not rbPreset.Checked;
  cbMP3QualityChange(sender);
end;

procedure TSelectDirDlg.UpdateOKbutton;
var valid: boolean;
begin
  valid := self.FValidPath;
  if (FActivePanel <> nil) then
    valid := valid and (FActivePanel.Tag = 0);
  btnOK.Enabled := valid;
end;

function TSelectDirDlg.GetDirName: string;
begin
  if not assigned(FDirNameForm) then
      FDirNameForm := TDirNameForm.Create(self);
  FDirNameForm.eDirName.Text := NewFolderName;
  if FDirNameForm.ShowModal <> mrOk then Abort;
  Result := FDirNameForm.eDirName.Text;
end;

procedure TSelectDirDlg.eDirectoryChange(Sender: TObject);
begin
  inherited;
  FValidPath :=  DirectoryExists(eDirectory.Text);
  if FValidPath then
  begin
    Available := CalcDiskFreeSpace(Directory);
  end else begin
    Available := 0;
  end;
  UpdateOKbutton;
end;

procedure TSelectDirDlg.btnBrowseClick(Sender: TObject);
var dir: string;
begin
  inherited;
  dir := BrowseForFolder(Handle, strSelectOutDir, eDirectory.Text);
  if dir <> '' then
  begin
    eDirectory.Text := dir;
    eDirectoryChange(nil);
  end;
end;

end.
