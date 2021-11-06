unit working;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, ComCtrls,
  Buttons, ExtCtrls, MMSystem, msacm, WaveUtil,
  lame, lame_fmt, vorbis, FLAC,
  FLAC_Enc, AKRip32, MyAspi32;

const
 DefaultWriteBufferSize = 2352 * 256;
{$IFDEF DISP}
  ReadBufferMult  = 75;
{$ELSE}
  ReadBufferMult  = 256;
{$ENDIF}

type
   TShortArray = array[0..MAXINT div sizeof(SmallInt) - 1] of Smallint;
   PShortArray = ^TShortArray;
   TByteArray = array[0..MAXINT div sizeof(SmallInt) - 1] of Byte;

type
  TLoadingDlg = class(TForm)
    btnAbort: TBitBtn;
    GroupBox1: TGroupBox;
    lDescription: TLabel;
    Gauge: TProgressBar;
    procedure btnAbortClick(Sender: TObject);
  private
    { Private declarations }
    Canceled: Boolean;
  public
    { Public declarations }
    procedure LoadFile(
        WaveFile: HMMIO;
        WaveSize: Cardinal;
        StatsList: PSectorStatsList;
        StatsSize: Cardinal;
        SectorSize: Cardinal;
        BitsPerSample: Word);
    procedure SaveTracks(
        Tracks: TListItems;
        WaveFile: HMMIO;
        Directory: String;
        WaveOffset: Cardinal;
        Format: PWaveFormatEx;
        ConvertToFormat: PWaveFormatEx);
    procedure DAE(
        CDHandle: HCDROM;
        FileName: string;
        JitterCorrect: boolean;
        out StatsHandle: HGLOBAL;
        out SectorStatsSize: Cardinal;
        out DataSize: Cardinal;
        TrackList: TList);
  end;

  TWaveWriter = class(TObject)
  protected
    FBuffer: Pointer;
    FBufferSize: Cardinal;
    FFileName: string;
    FromFormat, ToFormat: PWaveFormatEx;
  public
    ToWrite: Cardinal;
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    function Write(Bytes: Cardinal): Cardinal; virtual; abstract;
    property Buffer: Pointer read FBuffer;
    procedure IsLastWrite; virtual;
    procedure InitFile; virtual;
    procedure DoneFile; virtual;
    property BufferSize: Cardinal read FBufferSize;
    property FileName: string read FFileName write FFileName;
  end;

  TMMIOWaveWriter = class(TWaveWriter)
  protected
    ToFile: HMMIO;
    ckOutRIFF, ckOut: TMMCKInfo;
  public
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    destructor Destroy; override;
    function Write(Bytes: Cardinal): Cardinal; override;
    procedure InitFile; override;
    procedure DoneFile; override;
  end;

    { TODO
·	The acmStreamOpen function opens a conversion stream.
·	The acmStreamSize function calculates the appropriate size of the source or destination buffer.
·	The acmStreamPrepareHeader function prepares source and destination buffers to be used in a conversion.
·	The acmStreamConvert function converts data in a source buffer into the destination format, writing the converted data into the destination buffer.
·	The acmStreamUnprepareHeader function cleans up the source and destination buffers prepared by acmStreamPrepareHeader. You must call this function before freeing the source and destination buffers.
·	The acmStreamClose function closes a conversion stream.
  WaveWriter createn...
    }
  TACMWaveWriter = class(TMMIOWaveWriter)
  protected
    acmStream: HACMSTREAM;
    ash: TACMSTREAMHEADER;
    writeFlags: Cardinal;
  public
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    procedure IsLastWrite; override;
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
    destructor Destroy; override;
  end;

  TLinearWaveWriter = class(TMMIOWaveWriter)
  protected
    factor: Integer;
    Dst: PShortArray;
    DstSize: Cardinal;
  public
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    function Write(Bytes: Cardinal): Cardinal; override;
    destructor destroy; override;
  end;

  TFileWaveWriter = class(TWaveWriter)
  protected
    hFile: THandle;
    procedure FileWrite(var buffer; bytes: Cardinal);
  public
    procedure InitFile; override;
    procedure DoneFile; override;
  end;

  TLameWaveWriter = class(TFileWaveWriter)
  protected
    lameFlags: PLame_global_flags;
    dwMP3Buffer: Cardinal;
    pMP3Buffer: Pointer;
  public
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    destructor destroy; override;
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
  end;

  TVorbisWaveWriter = class(TFileWaveWriter)
  protected
    pOGGBuffer: Pointer;
    vbHandle : HVB_STREAM;
    vbAlbumInfo : TVB_ALBUM_INFO;
    chTitle, chComment, chEncodedBy, chDateTime: string;
  public
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
  end;

  TFlacWaveWriter = class(TFileWaveWriter)
  protected
     stream: PFLAC__SeekableStreamEncoder;
     F32Buffer: PFLACIntBuf;
  public
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    destructor Destroy; override;
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
  end;

implementation

uses SelectDirFrm
{$IFDEF DISP}
         ,DisplayFormTest
{$ENDIF}
;


// -----

resourcestring
  NoAllocMemStr = 'Unable to allocate memory.';
  NoWriteStr = 'Error writing file.';
  NoOpenConvStr = 'Unable to open conversion stream.';
  NoBufSizeStr = 'Unable to determine destination buffer size.';
  NoAllocDestStr = 'Cannot allocate dest buffer.';
  NoHeaderStr = 'Cannot prepare header.';
  AcmErrorStr = 'ACM Converter reported an error.';
  NoWriteDestStr = 'Unable to write to destination file.';
  ReadStr = 'Reading WAV data';
  NoReadStr = 'Error occurred: '#10'%s'#10'While reading file.';
  CancelStr = 'Cancelled by user.';
  WriteStr = 'Writing tracks';
  NoSeekInStr = 'Seek failed on input file.';
  WritingStr = 'Writing: ';
  NoReadStr2 = 'Unable to read source file.';
  ZeroBytesStr = 'Reported 0 bytes written, cannot continue.';
  DAEErr = 'Error during audio extraction';
  DAEStr = 'Digital Audio Extraction';

constructor TWaveWriter.Create;
begin
  FromFormat := AFromFormat;
  ToFormat := AToFormat;
  FBufferSize := DefaultWriteBufferSize;
end;

procedure TWaveWriter.IsLastWrite;
begin
end;
procedure TWaveWriter.InitFile;
begin
end;
procedure TWaveWriter.DoneFile;
begin
end;

// -----

constructor TMMIOWaveWriter.Create;
begin
  inherited;
  FBuffer := Pointer(GlobalAlloc(GMEM_FIXED, BufferSize));
  if FBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
end;

destructor TMMIOWaveWriter.Destroy;
begin
  if (Buffer <> nil) then
  begin
    GlobalFree(Cardinal(Buffer));
    FBuffer := nil;
  end;
  inherited;
end;

function TMMIOWaveWriter.Write;
begin
  Result := mmioWrite(ToFile, Buffer, Bytes);
  if Result = Cardinal(-1) then raise Exception.Create(NoWriteStr);
  FBufferSize := DefaultWriteBufferSize;
end;

procedure TMMIOWaveWriter.InitFile;
begin
  ToFile := OpenWaveFile(FileName, nil, GENERIC_WRITE, CREATE_ALWAYS);
  WriteWaveHeader(ToFile, ckOutRIFF, ckOut, ToFormat, ToWrite);
end;

procedure TMMIOWaveWriter.DoneFile;
begin
   mmioAscend(ToFile, @ckOut, 0);
   mmioAscend(ToFile, @ckOutRIFF, 0);
   mmioClose(ToFile, 0);
end;



// -----

constructor TACMWaveWriter.Create;
begin
  inherited;
  FillChar(ash, sizeof(ash), 0);
  ash.cbStruct := sizeof(ash);
  ash.pbSrc := Buffer;
end;

procedure TACMWaveWriter.IsLastWrite;
begin
  WriteFlags := (Writeflags and not ACM_STREAMCONVERTF_BLOCKALIGN) or ACM_STREAMCONVERTF_END;
end;

procedure TACMWaveWriter.InitFile;
var err: MMRESULT;
begin
  inherited;
  ash.cbSrcLength := BufferSize;
  err := acmStreamOpen(@acmStream, 0, FromFormat, ToFormat, nil, 0, 0,
                       ACM_STREAMOPENF_NONREALTIME);
  if err <> 0 then raise Exception.Create(NoOpenConvStr + ' code: ' + IntToStr(err));

  if acmStreamSize(acmStream, ash.cbSrcLength, @ash.cbDstLength, ACM_STREAMSIZEF_SOURCE) <> 0 then
    raise Exception.Create(NoBufSizeStr);
  ash.pbDst := Pointer(GlobalAlloc(GMEM_FIXED, ash.cbDstLength));
  if ash.pbDst = nil then raise Exception.Create(NoAllocDestStr);
  ash.cbDstLengthUsed := 0;
  if acmStreamPrepareHeader(acmStream, @ash, 0) <> 0 then
    raise Exception.Create(NoHeaderStr);
  WriteFlags := ACM_STREAMCONVERTF_BLOCKALIGN or ACM_STREAMCONVERTF_START;
end;

procedure TACMWaveWriter.DoneFile;
begin
  if (ash.fdwStatus and ACMSTREAMHEADER_STATUSF_PREPARED) <> 0 then
    acmStreamUnprepareHeader(acmStream, @ash, 0);
  if (ash.pbDst <> nil) then
  begin
    GlobalFree(Cardinal(ash.pbDst));
    ash.pbDst := nil;
    ash.cbDstLength := 0;
  end;
  acmStreamClose(acmStream, 0);
  acmStream := 0;
  inherited;
end;

function TACMWaveWriter.Write;
begin
  ash.cbSrcLength := Bytes;
  ash.cbDstLengthUsed := 0;
  if acmStreamConvert(acmStream, @ash, WriteFlags) <> 0 then
    raise Exception.Create(AcmErrorStr);
  if mmioWrite(ToFile, PChar(ash.pbDst), ash.cbDstLengthUsed) <> Integer(ash.cbDstLengthUsed) then
    raise Exception.Create(NoWriteDestStr);
  WriteFlags := ACM_STREAMCONVERTF_BLOCKALIGN;
  Result := ash.cbSrcLengthUsed;
end;

destructor TACMWaveWriter.Destroy;
begin
  if acmStream <> 0 then DoneFile;
  inherited;
end;
// ----

constructor TLinearWaveWriter.create;
begin
  inherited;
  factor := AToFormat.nSamplesPerSec div AFromFormat.nSamplesPerSec;
  DstSize := BufferSize * Cardinal(factor);
  Dst := Pointer(GlobalAlloc(GMEM_FIXED, DstSize));
end;

function TLinearWaveWriter.write;
var DestSize: Cardinal;
    Src: PShortArray;
    i, j, k: Integer;
    l, r, pl, pr: SmallInt;
begin
  bytes := (bytes shr 2)  * 4;
  DestSize := bytes * Cardinal(factor);
  if DestSize > DstSize then
  begin
     destsize := DstSize;
     bytes := DstSize div Cardinal(factor);
  end;
  Src := PShortArray(Buffer);
  k := 0;
  pl := Src[0];
  pr := Src[1];
  for i := 1 to (bytes shr 2) do
  begin
    l := Src[2 * i];
    r := Src[2 * i + 1];
    for j := 0 to factor-1 do
    begin
      Dst[k] := (pl * (factor - j) + l * j) div factor;   // Left
      inc(k);
      Dst[k] := (pr * (factor - j) + r * j) div factor;   // Right
      inc(k);
    end;
    pl := l;
    pr := r;
  end;
  if mmioWrite(ToFile, PChar(Dst), destsize) <> Integer(destsize) then
    raise Exception.Create(NoWriteDestStr);
  result := bytes;
end;

destructor TLinearWaveWriter.destroy;
begin
  if Dst <> nil then GlobalFree(Cardinal(Dst));
  inherited;
end;
//------

procedure TFileWaveWriter.InitFile;
begin
  inherited;
  hFile := CreateFile(
          PChar(FileName),
          GENERIC_WRITE,
          FILE_SHARE_READ,
          nil,
          CREATE_ALWAYS,
          FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,
          0);
  if hFile = INVALID_HANDLE_VALUE then RaiseLastWin32Error;
end;

procedure TFileWaveWriter.DoneFile;
begin
  if hFile <> INVALID_HANDLE_VALUE then CloseHandle(hFile);
  inherited;
end;

procedure TFileWaveWriter.FileWrite;
begin
  if sysUtils.FileWrite(hFile, buffer, bytes) <> integer(bytes) then
    raise Exception.Create(NoWriteDestStr);
end;

//------

constructor TLameWaveWriter.Create;
begin
  inherited;
  FBufferSize := $4000;
  dwMP3Buffer := (BufferSize shr 1) + (BufferSize shr 3) + 7200;
  FBuffer := Pointer(GlobalAlloc(GMEM_FIXED, BufferSize));
  if FBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
  pMP3Buffer := Pointer(GlobalAlloc(GMEM_FIXED, dwMP3Buffer));
end;

destructor TLameWaveWriter.destroy;
begin
  try
    if Buffer <> nil then GlobalFree(Cardinal(Buffer));
    if pMP3Buffer <> nil then GlobalFree(Cardinal(pMP3Buffer));
  finally
    inherited;
  end;
end;

procedure TLameWaveWriter.InitFile;
var dwSamples: Cardinal;
begin
  FileName := ChangeFileExt(FileName, '.mp3');
  inherited;
  lameFlags := lame_init;
  dwSamples := ToWrite div (Fromformat.nChannels * FromFormat.wBitsPerSample shr 3);
  lame_set_num_samples(lameFlags, dwSamples);
  lame_set_in_samplerate(lameFlags, Fromformat.nSamplesPerSec);
  lame_set_num_channels(lameFlags, FromFormat.nChannels); // // not Finput.Channels see the note below
  // lame_set_VBR
  // lame_set_brate(lameFlags, 128);
  // lame_set_preset(lameFlags, PLameWaveFormat(ToFormat).preset);
  if lame_init_params(lameFlags) = -1 then
     raise Exception.Create('LAME - Init parameters call failed, cannot encode');
end;

procedure TLameWaveWriter.DoneFile;
var dwWrite: Cardinal;
begin
 try
  dwWrite := lame_encode_flush(lameFlags, pMP3Buffer, dwMP3Buffer);
  if dwWrite > 0 then FileWrite(pMP3Buffer^, dwWrite);
 finally
  lame_close(lameFlags);
  inherited;
 end;
end;


function TLameWaveWriter.Write(bytes: Cardinal): Cardinal;
var dwWrite: Integer;
begin
  dwWrite := 0;
  case FromFormat.nChannels of
  1: dwWrite := lame_encode_buffer(lameFlags, Buffer, Buffer, bytes div 4, pMP3Buffer, dwMP3Buffer);
  2: dwWrite := lame_encode_buffer_interleaved(lameFlags, Buffer, bytes div 4, pMP3Buffer, dwMP3Buffer);
  end;
  if dwWrite < 0 then
    raise Exception.Create('LAME encoding failed, code=' + IntToStr(dwWrite));
  if dwWrite > 0 then FileWrite(pMP3Buffer^, dwWrite);
  Result := bytes;
end;

// - - - -

procedure TVorbisWaveWriter.InitFile;
var Samples, OutBufsize: DWORD;
begin
  FileName := ChangeFileExt(FileName, '.ogg');
  inherited;
  with vbAlbumInfo do
  begin
      dwSize := sizeof(TVB_ALBUM_INFO);
      dwVersion := VB_ALB_STRUCT_VERSION;
      dwRecords := 1;
      infoRecord.dwSize := sizeof(TVB_ALBUM_INFO_RECORD);
      infoRecord.dwVersion := VB_ALB_STRUCT_VERSION;
      chTitle := ChangeFileExt(ExtractFileName(FileName), '');
      infoRecord.strTitle := PChar(chTitle);
      chComment := 'DLL: ' + VorbisDLLVersionStr;
      infoRecord.strComment := PChar(chComment);
      chEncodedBy :=  'CD Wave';
      infoRecord.strEncodedBy := PChar(chEncodedBy);
      chDateTime := DateToStr(Now);
      infoRecord.strDateTime := PChar(chDateTime);
  end;
  Samples := ToWrite div 2;
  OGGCall(vbEncOpen(@(POGGWaveFormat(ToFormat).OGG), @vbAlbumInfo, Samples, OutBufSize, vbHandle));
  FBufferSize := Samples * 2;
  FBuffer := Pointer(GlobalAlloc(GMEM_FIXED, BufferSize));
  if FBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
  pOGGBuffer := Pointer(GlobalAlloc(GMEM_FIXED, OutBufSize));
  if pOGGBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
end;

procedure TVorbisWaveWriter.DoneFile;
var dwWrite: Cardinal;
begin
  try
    OGGCall(vbEncClose(vbHandle, pOGGBuffer, dwWrite));
    if dwWrite > 0 then FileWrite(pOGGBuffer^, dwWrite);
  finally
    if (Buffer <> nil) then
    begin
      GlobalFree(Cardinal(Buffer));
      FBuffer := nil;
    end;
    if pOGGBuffer <> nil then
    begin
      GlobalFree(Cardinal(pOGGBuffer));
      pOGGBuffer := nil;
    end;
    inherited;
  end;
end;

function TVorbisWaveWriter.Write(bytes: Cardinal): Cardinal;
var dwWrite: Cardinal;
begin
  dwWrite := 0;
  OGGCall(vbEncWrite(vbHandle,(bytes div 2), Buffer, pOGGBuffer, dwWrite));
  if dwWrite > 0 then FileWrite(pOGGBuffer^, dwWrite);
  Result := bytes;
end;

//-----

  function EncSeekCBFunc(encoder : PFLAC__SeekableStreamEncoder;
                      absolute_byte_offset : FLAC__uint64;
                      client_data : Pointer) : Integer; cdecl;
  var
    FLACOut : TFlacWaveWriter;
  begin
    FLACOut := TFlacWaveWriter(client_data);
    Result := FLAC__SEEKABLE_STREAM_ENCODER_SEEK_STATUS_OK;
    try
      FileSeek(FLACOut.hFile, absolute_byte_offset, SEEK_SET);
    except
      Result := FLAC__SEEKABLE_STREAM_ENCODER_SEEK_ERROR;
    end;
  end;

  function EncWriteCBFunc(encoder : PFLAC__SeekableStreamEncoder;
                                buffer : PFLAC__byte;
                                bytes, samples, current_frame : LongWord;
                                client_data : Pointer) : Integer; cdecl;
  var
    FLACOut : TFlacWaveWriter;
  begin
    FLACOut := TFlacWaveWriter(client_data);
    if FileWrite(FLACOut.hFile, buffer^, bytes) = Integer(bytes) then
      Result := FLAC__SEEKABLE_STREAM_ENCODER_OK
    else
      Result := FLAC__SEEKABLE_STREAM_ENCODER_WRITE_ERROR;
  end;


constructor TFlacWaveWriter.Create;
var F32BufferSize: Cardinal;
begin
  inherited;
  stream := FLAC__seekable_stream_encoder_new;
  if stream = nil then raise Exception.Create('Unable to create FLAC encoder');
  FBufferSize := 4608 * (FromFormat.wBitsPerSample shr 3) * FromFormat.nChannels;
  F32BufferSize := 4608 * sizeof(FLAC__int32) * FromFormat.nChannels;
  FBuffer := Pointer(GlobalAlloc(GMEM_FIXED, BufferSize));
  if FBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
  F32Buffer := Pointer(GlobalAlloc(GMEM_FIXED, F32BufferSize));
  if F32Buffer = nil then
    raise Exception.Create(NoAllocMemStr);
end;

destructor TFlacWaveWriter.Destroy;
begin
  if stream <> nil then FLAC__seekable_stream_encoder_delete(stream);
  if (Buffer <> nil) then
  begin
    GlobalFree(Cardinal(Buffer));
    FBuffer := nil;
  end;
  if (F32Buffer <> nil) then
  begin
    GlobalFree(Cardinal(F32Buffer));
    F32Buffer := nil;
  end;
  inherited;
end;

type
  TFlacConfig = record
    lpc,
    block: Integer;
    rice_min, rice_max: Integer;
    midside, a_midside, exhaustive: boolean;
  end;
{
  -0, --compression-level-0, --fast  Synonymous with -l 0 -b 1152 -r 2,2
  -1, --compression-level-1          Synonymous with -l 0 -b 1152 -M -r 2,2
  -2, --compression-level-2          Synonymous with -l 0 -b 1152 -m -r 3
  -3, --compression-level-3          Synonymous with -l 6 -b 4608 -r 3,3
  -4, --compression-level-4          Synonymous with -l 8 -b 4608 -M -r 3,3
  -5, --compression-level-5          Synonymous with -l 8 -b 4608 -m -r 3,3
  -6, --compression-level-6          Synonymous with -l 8 -b 4608 -m -r 4
  -7, --compression-level-7          Synonymous with -l 8 -b 4608 -m -e -r 6
  -8, --compression-level-8, --best  Synonymous with -l 12 -b 4608 -m -e -r 6}
const
  FlacConfig: array[0..8] of TFlacConfig = (
  (lpc: 0; block: 1152; rice_min: 2; rice_max: 2; midside: False; a_midside: False; exhaustive: False),
  (lpc: 0; block: 1152; rice_min: 2; rice_max: 2; midside: True;  a_midside: True;  exhaustive: False),
  (lpc: 0; block: 1152; rice_min: 0; rice_max: 3; midside: True;  a_midside: False; exhaustive: False),
  (lpc: 6; block: 4608; rice_min: 3; rice_max: 3; midside: False; a_midside: False; exhaustive: False),
  (lpc: 8; block: 4608; rice_min: 3; rice_max: 3; midside: True;  a_midside: True;  exhaustive: False),
  (lpc: 8; block: 4608; rice_min: 3; rice_max: 3; midside: True;  a_midside: False; exhaustive: False),
  (lpc: 8; block: 4608; rice_min: 0; rice_max: 4; midside: True;  a_midside: False; exhaustive: False),
  (lpc: 8; block: 4608; rice_min: 0; rice_max: 6; midside: True;  a_midside: False; exhaustive: True),
  (lpc:12; block: 4608; rice_min: 0; rice_max: 6; midside: True;  a_midside: False; exhaustive: True)
  );

procedure TFlacWaveWriter.InitFile;
var cfg: TFlacConfig;
begin
  FileName := ChangeFileExt(FileName, '.flac');
  inherited;
  FLAC__seekable_stream_encoder_set_client_data(stream, self);
  FLAC__seekable_stream_encoder_set_channels(stream, FromFormat.nChannels);
  FLAC__seekable_stream_encoder_set_bits_per_sample(stream, FromFormat.wBitsPerSample);
  FLAC__seekable_stream_encoder_set_sample_rate(stream, FromFormat.nSamplesPerSec);
  FLAC__seekable_stream_encoder_set_total_samples_estimate(stream, ToWrite div FromFormat.nBlockAlign);
  FLAC__seekable_stream_encoder_set_seek_callback(stream, EncSeekCBFunc);
  FLAC__seekable_stream_encoder_set_write_callback(stream, EncWriteCBfunc);
  cfg := FlacConfig[PFlacWaveFormat(ToFormat).Level];
  with cfg do
  begin
    FLAC__seekable_stream_encoder_set_max_lpc_order(stream, lpc);
    FLAC__seekable_stream_encoder_set_blocksize(stream, block);
    if rice_min > 0 then
      FLAC__seekable_stream_encoder_set_min_residual_partition_order(stream, rice_min);
    FLAC__seekable_stream_encoder_set_max_residual_partition_order(stream, rice_max);
    if ToFormat.nChannels > 1 then
    begin
      FLAC__seekable_stream_encoder_set_do_mid_side_stereo(stream, midside);
      FLAC__seekable_stream_encoder_set_loose_mid_side_stereo(stream, a_midside);
    end;
    FLAC__seekable_stream_encoder_set_do_exhaustive_model_search(stream, exhaustive);
  end;
  if FLAC__seekable_stream_encoder_init(stream) <> FLAC__SEEKABLE_STREAM_ENCODER_OK then
    raise Exception.Create('Failed to initialize FLAC stream encoder');
end;

procedure TFlacWaveWriter.DoneFile;
begin
  try
    if FLAC__seekable_stream_encoder_get_state(stream) <>
       FLAC__SEEKABLE_STREAM_ENCODER_UNINITIALIZED then
      FLAC__seekable_stream_encoder_finish(stream);
  finally
    inherited;
  end;
end;

function TFlacWaveWriter.Write;
var i: Integer;
    s: Integer;
    flacBuf: PFLACIntBuf;
    buf: pointer;
begin
  s := bytes div (FromFormat.wBitsPerSample shr 3); // # samples
  flacBuf := F32Buffer;
  buf := buffer;
  case FromFormat.wBitsPerSample of
    0..8:   for i := 0 to s-1 do flacBuf[i] := PByteArray(Buf)[i] - $80;
    9..16:  for i := 0 to s-1 do flacBuf[i] := PShortArray(Buf)[i];
    17..24: for i := 0 to s-1 do flacBuf[i] := PInteger(@(PArray24(Buf)[i]))^ and $00FFFFFF;
    else flacBuf := Buffer;
  end;
  if not FLAC__seekable_stream_encoder_process_interleaved(stream, @flacBuf[0], s div Fromformat.nChannels) then
    raise Exception.Create('FLAC Stream write failed?');
  result := bytes;
end;

{-----------------------------------------------------------------------------}


{$R *.DFM}
procedure TLoadingDlg.LoadFile;
var
   i, Carry, NumRead, NumToRead: Cardinal;
   Sector: PChar;
   BufferSize: Cardinal;
begin
  Canceled := False;
  Gauge.Min := 0;
  Gauge.Position := 0;
  Gauge.Max := StatsSize;
  Carry := 0;
  lDescription.Caption := ReadStr;
{$IFDEF DISP}
  DisplayForm.InitFile((StatsSize * SectorSize) shr 2);
{$ENDIF}
  BufferSize := SectorSize * ReadBufferMult;
  Sector := PChar(GlobalAlloc(GMEM_FIXED, BufferSize)); //  GetMem(Sector, BufferSize);
  if Sector = nil then
    raise Exception.Create(NoAllocMemStr);
  try
    Show;
    EnableWindow(TForm(Owner).Handle, False);
    i := 0;
    while i < StatsSize do
    begin
      if WaveSize < BufferSize then
         NumToRead := WaveSize
      else
         NumToRead := BufferSize;
      NumRead := mmioRead(Wavefile, Sector, NumToRead);
      if NumRead = $FFFFFFFF then
          raise EWin32Error.CreateFmt(
            NoReadStr,
            [SysErrorMessage(GetLastError)]);
      if NumRead = 0 then break;
      dec(WaveSize, NumRead);
      inc(i, FillSectorStats(
        StatsList, i, Carry,
        Sector, NumRead,  SectorSize, BitsPerSample));
{$IFDEF DISP}
      DisplayForm.Analyze(Sector, NumRead shr 2);
{$ENDIF}
//      if (i and $7F)=0 then
      begin
        Gauge.Position := i;
        Application.ProcessMessages;
        if Canceled then raise Exception.Create(CancelStr);
      end;
    end;
  finally
    GlobalFree(Cardinal(Sector)); // FreeMem(Sector, BufferSize);
    EnableWindow(TForm(Owner).Handle, True);
    Hide;
  end;
end;

procedure TLoadingDlg.SaveTracks;
var At: Cardinal;
    ToRead, TotWritten, ToWrite, BytesWritten, BytesLeft: Cardinal;
    GaugePosition: Cardinal;
    DestFormat: PWaveFormatEx;
    WaveWriter: TWaveWriter;
    Buffer: PChar;
    BufferSize: Cardinal;
begin
  ToWrite := 0;
  with Tracks do
    for At := 0 to Count-1 do
      if Item[At].Checked then inc(ToWrite, PSplitInfo(Item[At].Data).Size);
  Canceled := False;
  Gauge.Min := 0;
  Gauge.Position := 0;
  GaugePosition := 0;
  Gauge.Max := ToWrite shr 1; // avoid out-of-range...
  lDescription.Caption := WriteStr;
  if ConvertToFormat <> nil then
  begin
    DestFormat := ConvertToFormat;
    case DestFormat.wFormatTag of
      WAVE_FORMAT_LAME_MP3: WaveWriter := TLameWaveWriter.Create(Format, DestFormat);
      WAVE_FORMAT_OGG_VORBIS: WaveWriter := TVorbisWaveWriter.Create(Format, DestFormat);
      WAVE_FORMAT_FLAC: WaveWriter := TFlacWaveWriter.Create(Format, DestFormat);
    else
    {
    if (DestFormat.wFormatTag = WAVE_FORMAT_PCM) and
       (Destformat.nChannels = 2) and
       (DestFormat.wBitsPerSample = 16) and
       (DestFormat.nSamplesPerSec > Format.nSamplesPerSec) and
       (DestFormat.nSamplesPerSec mod Format.nSamplesPerSec = 0)
    then
      WaveWriter := TLinearWaveWriter.Create(Format, DestFormat)
    else}
      WaveWriter := TACMWaveWriter.Create(Format, DestFormat);
    end;
  end else begin
    DestFormat := Format;
    WaveWriter := TMMIOWaveWriter.Create(Format, DestFormat);
  end;
  try
      EnableWindow(TForm(Owner).Handle, False);
      Show;
      Screen.Cursor := crHourGlass;
      Application.ProcessMessages;
      with Tracks do
      begin
        for At := 0 to Count-1 do
        if Item[At].Checked then
        begin
          if mmioSeek(WaveFile, PSplitInfo(Item[At].Data).Start+WaveOffset, SEEK_SET) = -1 then
            raise Exception.Create(NoSeekInStr);
          ToWrite := PSplitInfo(Item[At].Data).Size;
          WaveWriter.FileName := Directory + Item[At].Caption + '.wav';
          WaveWriter.ToWrite := ToWrite;
          lDescription.Caption := WritingStr + Item[At].Caption;
          try
            TotWritten := 0;
            WaveWriter.InitFile;
            Buffer := WaveWriter.Buffer;
            BufferSize := WaveWriter.BufferSize;
            repeat
              if (ToWrite - TotWritten) > BufferSize then
                ToRead := BufferSize
              else begin
                ToRead := ToWrite - TotWritten;
                WaveWriter.IsLastWrite;
              end;
              if (ToRead <> 0) and (mmioRead(Wavefile, Buffer, ToRead) <> integer(ToRead)) then
                raise Exception.Create(NoReadStr2);
              BytesWritten := WaveWriter.write(ToRead);
              if BytesWritten = 0 then raise Exception.Create(ZeroBytesStr);
              Buffer := WaveWriter.Buffer;
              BytesLeft := ToRead - BytesWritten;
              if BytesLeft <> 0 then
              begin
                Move(Buffer[BytesWritten], Buffer^, BytesLeft);
                Buffer := @Buffer[BytesLeft];
              end;
              BufferSize := WaveWriter.BufferSize - BytesLeft;
              inc(TotWritten, ToRead);
              inc(GaugePosition, ToRead);
              Gauge.Position := integer(GaugePosition shr 1);
              Application.ProcessMessages;
              if Canceled then raise Exception.Create(CancelStr);
            until (TotWritten = ToWrite);
          finally
            WaveWriter.DoneFile;
          end;
        end;
      end;
  finally
    WaveWriter.Free;
    EnableWindow(TForm(Owner).Handle, True);
    Hide;
    Screen.Cursor := crDefault;
  end;
end;

procedure TLoadingDlg.DAE;
const
  FramesPerRead = 23;
var
  PBuf, POverlap: PTRACKBUF;
  liStatus: Integer;
  ToFile: HMMIO;
  MyTOC: TOC;
  ckOutRIFF, ckOut: TMMCKInfo;
  StartSector, EndSector, TotalSectors, Frames, Sector, i: Cardinal;
  SectorStatsList: PSectorStatsList;
  Data: PChar;
begin
  FillChar(MyToc, sizeof(MyTOC), 0);
  Screen.Cursor := crHourglass;
  Canceled := False;
  try
    if ReadTOC(CDHandle, MyTOC) <> 1 then
      raise Exception.Create('Error reading TOC');
    PBuf := newTrackBuf(FramesPerRead);
    if JitterCorrect then POverlap := newTrackBuf(3) else POverlap := nil;
    StartSector := BigToLittleEndian(MyTOC.tracks[myTOC.firstTrack-1].addr);
    EndSector := BigToLittleEndian(MyTOC.tracks[myTOC.lastTrack].addr);
    TotalSectors := EndSector - StartSector;
    DataSize := TotalSectors * 2352;
    SectorStatsSize := TotalSectors;
    StatsHandle := GlobalAlloc(GHND, TotalSectors * SizeOf(TSectorStats));
    if StatsHandle = 0 then
      raise EWin32Error.Create(
        NoAllocMemStr + #13#10 + SysErrorMessage(GetLastError));
    SectorStatsList := PSectorStatsList(GlobalLock(StatsHandle));
    try
      ToFile := OpenWaveFile(FileName, nil, GENERIC_WRITE, CREATE_ALWAYS);
      WriteWaveHeader(ToFile, ckOutRIFF, ckOut, @RedBookFormat, DataSize);
      Gauge.Min := 0;
      Gauge.Max := EndSector;
      lDescription.Caption := DAEStr;
      EnableWindow(TForm(Owner).Handle, False);
      Show;
      try
        Sector := 0;
        while StartSector < EndSector do
        begin
          Gauge.Position := StartSector;
          Application.ProcessMessages;
          if Canceled then raise Exception.Create(CancelStr);
          PBuf^.startFrame := StartSector;
          Frames := EndSector - StartSector;
          if Frames > FramesPerRead then Frames := FramesPerRead;
          PBuf^.numFrames := Frames;
          if JitterCorrect then
            liStatus := ReadCDAudioLBAEx(CDHandle, PBuf, POverlap)
          else
            liStatus := ReadCDAudioLBA(CDHandle, PBuf);
          if liStatus = SS_ERR then raise Exception.Create(DAEErr);
          Data := PChar(Addr(PBuf^.buf)) + PBuf^.startOffset;
          if mmioWrite(Tofile, Data, PBuf^.len) <> Integer(PBuf^.len) then
             raise Exception.Create(NoWriteStr);
          for i := 0 to (PBuf^.len div 2352)-1 do
          begin
            SectorStatsList[Sector] := SectorLevel16(Data, 2352 shr 2);
            inc(sector);
            inc(Data, 2352);
          end;
          inc(StartSector, PBuf.numFrames);
        end;
        TrackList.Capacity := myTOC.lastTrack - myTOC.firstTrack + 1;
        StartSector := BigToLittleEndian(MyTOC.tracks[myTOC.firstTrack-1].addr);
        for i := myTOC.firstTrack-1 to myTOC.lastTrack-1 do
        begin
          Sector := BigToLittleEndian(MyTOC.tracks[i].addr);
          TrackList.Add(Pointer((Sector - StartSector) * 2352));
        end;
      finally
        mmioAscend(ToFile, @ckOut, 0);
        mmioAscend(ToFile, @ckOutRIFF, 0);
        mmioClose(ToFile, 0);
        DisposeTrackBuf(PBuf);
        if JitterCorrect then DisposeTrackBuf(POverlap);
        EnableWindow(TForm(Owner).Handle, True);
        Hide;
      end;
    except
      GlobalUnLock(StatsHandle);
      GlobalFree(StatsHandle);
      raise;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TLoadingDlg.btnAbortClick(Sender: TObject);
begin
  Canceled := True;
end;

end.
