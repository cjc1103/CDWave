unit SelectDirFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormatFrm, StdCtrls, Buttons, FileCtrl, mmsystem, msacm, ComCtrls,
  DirectoryTree, Menus, ExtCtrls, ActnList, ImgList, Lame, Lame_fmt,
  Vorbis, FLAC, FLAC_Enc;

type
  TSelectDirDlg = class(TFormatForm)
    DirectoryTree1: TDirectoryTree;
    PopupMenu1: TPopupMenu;
    Create1: TMenuItem;
    Rename1: TMenuItem;
    Delete1: TMenuItem;
    ActionList1: TActionList;
    CreateDir: TAction;
    DeleteDir: TAction;
    RenameDir: TAction;
    RefreshDir: TAction;
    Refresh1: TMenuItem;
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    grpSpace: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    lRequired: TLabel;
    lAvailable: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ButtonPanel: TPanel;
    bntCreateDir: TButton;
    btnDeleteDir: TButton;
    Button2: TButton;
    Button1: TButton;
    PanelFormat: TPanel;
    btnHelp: TBitBtn;
    ModePanel: TPanel;
    cbMode: TComboBox;
    Label1: TLabel;
    grpMP3: TGroupBox;
    lMP3Status: TLabel;
    grpOGG: TGroupBox;
    lOGGStatus: TLabel;
    cbOGGQuality: TComboBox;
    Label6: TLabel;
    lBadMP3Format: TLabel;
    lBadOGGFormat: TLabel;
    GrpNoConversion: TGroupBox;
    cbOld24: TCheckBox;
    Label10: TLabel;
    cbMP3Quality: TComboBox;
    Label11: TLabel;
    grpFLAC: TGroupBox;
    Label12: TLabel;
    cbFLACLevel: TComboBox;
    Label13: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GetACMFormatFilter(var afc: TACMFORMATCHOOSE); override;
    procedure DirectoryTree1Change(Sender: TObject; Node: TTreeNode);
    procedure bntCreateDirClick(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure btnDeleteDirClick(Sender: TObject);
    procedure RefreshDirExecute(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbModeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbOGGQualityChange(Sender: TObject);
    procedure cbMP3QualityChange(Sender: TObject);
    procedure cbFLACLevelChange(Sender: TObject);
  private
    { Private declarations }
    FActivePanel: TControl;
    function GetConvertToFormat: PWaveFormatEx;
    procedure CalculateSpaceRequirements;
    procedure InitMP3format;
    procedure InitOGGformat;
    procedure InitFLACFormat;
  protected
    FRequired, FAvailable: Int64;
    FSourceWaveFormat: PWaveFormatEx;
    FOld24Format: TWaveFormatEx;
    FMP3Format: TLameWaveFormat;
    FOGGFormat: TOGGWaveFormat;
    FFLACFormat: TFlacWaveFormat;
    procedure FormatChanged; override;
    function GetDirectory: string;
    procedure SetDirectory(const Value: string);
    procedure SetRequired(Value: Int64);
    procedure SetAvailable(Value: Int64);
    procedure SetSourceWaveFormat(fmt: PWaveFormatEx);
    function  GetSourceWaveFormat: PWaveFormatEx;
    property Available: Int64 read FAvailable write SetAvailable;
  public
    procedure SetNewFolderName(value: string);
    property SourceWaveFormat: PWaveFormatEx read GetSourceWaveFormat write SetSourceWaveFormat;
    property ConvertToFormat: PWaveFormatEx read GetConvertToFormat;
    property Required: Int64 write SetRequired;
    property Directory: string read GetDirectory write SetDirectory;
  end;


implementation

uses Utils, Registry, Settings, WaveUtil;
{$R *.DFM}

type

    TMP3Format = record
      name:   string;
      preset: preset_mode;
      bitrate: Word;
    end;

const
  MP3FormatsCount = 6;
  MP3Formats: array[0..MP3FormatsCount-1] of TMP3Format = (
   (name: 'R3Mix';         preset: R3MIX; bitrate: 128),
   (name: 'Standard';      preset: STANDARD; bitrate: 170),
   (name: 'Extreme';       preset: EXTREME; bitrate: 200),
   (name: 'Insane';   preset: INSANE; bitrate: 320),
   (name: 'Standard - fast';     preset: STANDARD_FAST; bitrate: 170),
   (name: 'Extreme - fast';  preset: EXTREME_FAST; bitrate: 200)
  );
{    ABR_8 = 8;
    ABR_320 = 320;
    R3MIX = 1000;
    STANDARD = 1001;
    EXTREME = 1002;
    INSANE = 1003;
    STANDARD_FAST = 1004;
    EXTREME_FAST = 1005;
}


const OGGKBPS: array[0..10] of Integer = (60,80,96,112,128,160,192,224,256,320,498);

procedure TSelectDirDlg.SetDirectory;
begin
  if (value <> '') then
  begin
    DirectoryTree1.Directory := value;
  end;
end;

function TSelectDirDlg.GetDirectory;
begin
  Result := DirectoryTree1.Directory;
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
    if SourceWaveformat.wBitsPerSample = 16 then
    begin
      FOGGFormat.WaveFormat.wBitsPerSample := 16;
      ValidFormat(True);
    end else
      Validformat(False);
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
  FMP3Format.WaveFormat.cbSize := sizeof(TLameWaveFormat) - SizeOf(TWaveFormatEx);
  FMP3Format.WaveFormat.wFormatTag := WAVE_FORMAT_LAME_MP3;
  if not grpMP3.Enabled then
  begin
    ValidFormat(False);
    Exit;
  end;
  FMP3Format.preset := MP3Formats[cbMP3Quality.ItemIndex].preset;
  kbps := MP3Formats[cbMP3Quality.ItemIndex].bitrate;
  FMP3Format.WaveFormat.nAvgBytesPerSec := kbps * 128;
  if SourceWaveFormat <> nil then
  begin
    if SourceWaveformat.wBitsPerSample = 16 then
    begin
      FMP3Format.WaveFormat.wBitsPerSample := 16;
      Validformat(True);
    end else
      ValidFormat(False);
    FMP3Format.WaveFormat.nChannels := SourceWaveformat.nChannels;
    if not (SourceWaveformat.nChannels in [1,2]) then
      ValidFormat(False);
    FMP3Format.WaveFormat.nSamplesPerSec := SourceWaveformat.nSamplesPerSec;
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

procedure TSelectDirDlg.FormCreate(Sender: TObject);
var
  reg: TRegistry;
  //version: BE_VERSION;	// version info
  i: Integer;
begin
  inherited;
  Reg := TRegistry.Create;
  with Reg do
  try
    if OpenKey(CDWAVRegKey, False) then
      Directory := reg.ReadString('SaveDir')
    else
      DirectoryTree1.CurrentDirectory;
  except
    DirectoryTree1.CurrentDirectory;
  end;
  GrpFormat.Parent := PanelFormat;
  GrpFormat.Align := alTop;
  cbMode.Items.Objects[0] := grpNoConversion;
  cbMode.Items.Objects[1] := grpFormat;
  cbMode.Items.Objects[2] := grpMP3;
  cbMode.Items.Objects[3] := grpOGG;
  cbMode.Items.Objects[4] := grpFLAC;
  try
    LoadLame;
    if not LameLoaded then raise Exception.Create('LAME_ENC.DLL not found');
    //lMP3Status.Caption := get_lame_version;
    for i := 0 to MP3FormatsCount-1 do
      cbMP3Quality.Items.Add(MP3Formats[i].name);
    cbMP3Quality.ItemIndex := MP3FormatsCount-1;
  except
    on e: Exception do
    begin
      lMP3Status.Caption := e.Message;
      grpMP3.Enabled := False;
      grpMP3.Tag := 1;
    end;
  end;
  InitMP3format;
  try
    InitVorbisDLL;
    lOGGStatus.Caption := VorbisDLLVersionStr;
    for i := 0 to 10 do
      cbOGGQuality.Items.Add(IntToStr(i) + ' (kbps: ' + IntToStr(OGGKBPS[i]) + ')');
    cbOGGQuality.ItemIndex := 5;
  except
    lOGGStatus.Caption := 'VORBIS.DLL not found';
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
  if (FActivePanel <> nil) then btnOK.Enabled := (FActivePanel.Tag = 0) 
end;

procedure TSelectDirDlg.DirectoryTree1Change(Sender: TObject;
  Node: TTreeNode);
begin
  inherited;
  Available := CalcDiskFreeSpace(DirectoryTree1.Directory);
end;

procedure TSelectDirDlg.bntCreateDirClick(Sender: TObject);
begin
  inherited;
  DirectoryTree1.CreateDirectory;
end;

procedure TSelectDirDlg.SetNewFolderName;
begin
  DirectoryTree1.NewFolderName := value;
end;

procedure TSelectDirDlg.Rename1Click(Sender: TObject);
begin
  inherited;
  DirectoryTree1.Selected.EditText;
end;

procedure TSelectDirDlg.btnDeleteDirClick(Sender: TObject);
begin
  inherited;
  DirectoryTree1.DeleteDirectory;
end;

procedure TSelectDirDlg.RefreshDirExecute(Sender: TObject);
begin
  inherited;
  DirectoryTree1.Refresh;
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

end.
