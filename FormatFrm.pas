unit FormatFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MMsystem, msacm;


type
  TFormatForm = class(TForm)
    GrpFormat: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lUnsupported: TLabel;
    cbFrequency: TComboBox;
    cbChannels: TComboBox;
    cbResolution: TComboBox;
    btnACM: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnACMClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PCMChange(Sender: TObject);
  private
    { Private declarations }
    FIsValid: Boolean;
    FWaveFormat: PWaveFormatEx;
    FWaveFormatMaxSize: Cardinal;
    function getWaveFormatSize: Cardinal;
    procedure GetWaveFormat;
  protected
    procedure CheckValid;
    function FormatValid: Boolean; virtual;
    procedure FormatChanged; virtual;
    procedure GetACMFormatFilter(var afc: TACMFORMATCHOOSE); virtual;
    procedure CopyWaveFormat(fmt: PWaveFormatEx);
  public
    { Public declarations }
    procedure SetWaveFormat();
    property IsValid: Boolean read FIsValid;
    property WaveFormat: PWaveFormatEx read FWaveFormat write CopyWaveFormat;
    property WaveFormatSize: Cardinal read getWaveFormatSize;
  end;

var
  FormatForm: TFormatForm;

implementation

uses WaveUtil;

{$R *.DFM}
resourcestring
  ACMStr = 'Select ACM format';
  InvalidFlagStr = 'At least one flag is invalid.';
  InvalidHandleStr = 'The specified handle is invalid.';
  InvalidParamStr = 'At least one parameter is invalid.';
  NoDriverStr = 'A suitable driver is not available to provide valid format selections.';

const Channels: array[0..3] of Word = (1,2,4,6);
const ChannelMasks: array[0..3] of DWORD =
   (SPEAKER_FRONT_CENTER,
    SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT,
    SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
      SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT,
    SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
      SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT or
      SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY);


procedure TFormatForm.GetWaveFormat;
begin
  CreatePCMWaveFormat(
    PWaveFormatExtensible(WaveFormat)^,
    StrToInt(cbFrequency.Text),
    Channels[cbChannels.ItemIndex],
    (cbResolution.ItemIndex+1) shl 3);
end;

procedure TFormatForm.SetWaveFormat;
begin
  with WaveFormat^ do
  begin
    case nChannels of
      1, 2: cbChannels.ItemIndex := nChannels - 1;
      4:    cbChannels.ItemIndex := 2;
      5,6:  cbChannels.ItemIndex := 3;
      else  cbChannels.ItemIndex := -1;
    end;
    cbFrequency.Text := IntToStr(nSamplesPerSec);
    cbResolution.ItemIndex := wBitsPerSample shr 3 - 1;
  end;
  CheckValid;
end;

procedure TFormatForm.FormCreate(Sender: TObject);
begin
  if acmMetrics(0, ACM_METRIC_MAX_SIZE_FORMAT, @FWaveFormatMaxSize) <> MMSYSERR_NOERROR then
  begin
    // Workaround, let's guess 4k is enough...
    FWaveFormatMaxSize := 4096;
  end;
  getmem(FWaveFormat, FWaveFormatMaxSize);
  getWaveFormat();
  CheckValid;
end;

function TFormatForm.FormatValid: Boolean;
begin
 Result := True;
end;

procedure TFormatForm.FormatChanged;
begin
end;

procedure TFormatForm.GetACMFormatFilter;
const enumWaveFormat: TWaveFormatEx = (
    wFormatTag : WAVE_FORMAT_PCM;
    nChannels  : 2;
    nSamplesPerSec : 44100;
    nAvgBytesPerSec : 44100 * 4;
    nBlockAlign : 4;
    wBitsPerSample : 16;
    cbSize : 0;
  );
begin
  afc.fdwStyle := ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT;
  afc.pwfxEnum := @enumWaveFormat;
  afc.fdwEnum  := ACM_FORMATENUMF_WFORMATTAG or ACM_FORMATENUMF_HARDWARE or
                  ACM_FORMATENUMF_INPUT;
end;

procedure TFormatForm.btnACMClick(Sender: TObject);
var
  afc: TACMFORMATCHOOSE;
  res: MMRESULT;
begin
  FillChar(afc, sizeof(afc), 0);
  afc.cbStruct := sizeof(afc);
  afc.hwndOwner := Handle;             // hwnd of parent window
  afc.cbwfx := FWaveFormatMaxSize;
  afc.pwfx := WaveFormat;
  afc.pszTitle := PChar(ACMStr);
  GetACMFormatFilter(afc);
  res := acmFormatChoose(@afc);
  case res of
    MMSYSERR_INVALFLAG:	raise Exception.Create(InvalidFlagStr);
    MMSYSERR_INVALHANDLE: raise Exception.Create(InvalidHandleStr);
    MMSYSERR_INVALPARAM: raise Exception.Create(InvalidParamStr);
    MMSYSERR_NODRIVER: raise Exception.Create(NoDriverStr);
    MMSYSERR_NOERROR: setWaveFormat;
  end;
end;

procedure TFormatForm.FormDestroy(Sender: TObject);
begin
  freemem(FWaveFormat, FWaveFormatMaxSize);
end;

function TFormatForm.getWaveFormatSize;
begin
  result := WaveUtil.getWaveFormatSize(WaveFormat);
end;

procedure TFormatForm.CopyWaveFormat;
begin
  WaveUtil.CopyWaveFormat(fmt, FWaveFormat);
  SetWaveFormat;
end;

procedure TFormatForm.PCMChange(Sender: TObject);
begin
  GetWaveFormat;
  CheckValid;
end;

procedure TFormatForm.CheckValid;
begin
  FIsValid := FormatValid;
  lUnsupported.Visible := not IsValid;
  FormatChanged;
end;

end.
