unit WaveWriters;

interface

uses
  Windows, SysUtils, Classes, MMSystem, msacm,
  WaveUtil,
  lame_enc, vorbis, FLAC,
  FLAC_Enc, APE;

type
  TWaveWriter = class(TObject)
  protected
    FBuffer: Pointer;
    FBufferSize: Cardinal;
    FFileName: string;
    FromFormat, ToFormat: PWaveFormatEx;
  public
    ToWrite: Cardinal;
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    destructor Destroy; override;
    function Write(Bytes: Cardinal): Cardinal; virtual; abstract;
    property Buffer: Pointer read FBuffer;
    procedure IsLastWrite; virtual;
    procedure InitFile; virtual;
    procedure DoneFile; virtual;
    property BufferSize: Cardinal read FBufferSize;
    property FileName: string read FFileName write FFileName;
  end;

  // Filter - writes to a destination writer. Takes
  // ownership of target writer (calls Free when Freeing this filter)
  TWaveWriterFilter = class(TWaveWriter)
  protected
    FDestination: TWaveWriter;
  public
    constructor Create(AFromFormat: PWaveFormatEx; ADestination: TWaveWriter);
    property Destination: TWaveWriter read FDestination;
    procedure IsLastWrite; override;
    destructor Destroy; override;
  end;


  TSimpleWaveFilter = class(TWaveWriterFilter)
  protected
    BitFactor, BitDivider: Cardinal;
    procedure Initialize; virtual; abstract;
    procedure Convert(bytes: Cardinal); virtual; abstract;
  public
    constructor Create(AFromFormat: PWaveFormatEx; ADestination: TWaveWriter);
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
  end;

  TBitConvertorProc = procedure(inbuffer, outbuffer: Pointer; size: Cardinal);

  TBitsWaveFilter = class(TSimpleWaveFilter)
  private
    Convertor: TBitConvertorProc;
  public
    procedure Initialize; override;
    procedure Convert(bytes: Cardinal); override;
  end;

  TChannelWaveFilter = class(TSimpleWaveFilter)
  public
    procedure Initialize; override;
    procedure Convert(bytes: Cardinal); override;
  end;

  TRateWaveFilter = class(TWaveWriterFilter)
  private
    BitFactor, BitDivider: Cardinal;
    FInBuffer: Pointer;
    FInBufferSize: Cardinal;
    FOutSamples: Cardinal;
    FCarry: Cardinal;
  public
    constructor Create(AFromFormat: PWaveFormatEx; ADestination: TWaveWriter);
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
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
    hbeStream: HBE_STREAM;
    dwMP3Buffer: Cardinal;
    pMP3Buffer: Pointer;
  public
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
     stream: PFLAC__StreamEncoder;
     F32Buffer: PFLACIntBuf;
  public
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    destructor Destroy; override;
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
  end;

  TAPEWaveWriter = class(TWaveWriter)
  protected
    hAPECompress: APE_COMPRESS_HANDLE;
  public
    constructor Create(AFromFormat, AToFormat: PWaveFormatEx);
    destructor Destroy; override;
    procedure InitFile; override;
    procedure DoneFile; override;
    function Write(Bytes: Cardinal): Cardinal; override;
  end;

function CreateWaveWriter(FromFormat, ToFormat: PWaveFormatEx): TWaveWriter;

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

implementation

const
 //DefaultWriteBufferSize = 2352 * 32;
 DefaultWriteBufferSize = 2352 * 80;
{$IFDEF DISP}
  ReadBufferMult  = 75;
{$ELSE}
  ReadBufferMult  = 256;
{$ENDIF}

function CreateWaveWriter(FromFormat, ToFormat: PWaveFormatEx): TWaveWriter;
var Intermediate: TWaveFormatExtensible;
begin
  if ToFormat <> nil then
  begin
    case ToFormat.wFormatTag of
      WAVE_FORMAT_LAME_MP3:
        begin
          // only 16 bit supported
          if FromFormat.wBitsPerSample <> 16 then
          begin
            CreatePCMWaveFormat(Intermediate, FromFormat.nSamplesPerSec,
                                FromFormat.nChannels, 16);
            Result := TLameWaveWriter.Create(@Intermediate, ToFormat);
            Result := TBitsWaveFilter.Create(FromFormat, Result);
          end else
            Result := TLameWaveWriter.Create(FromFormat, ToFormat);
        end;
      WAVE_FORMAT_OGG_VORBIS:
        begin
          // only 16 bit supported
          if FromFormat.wBitsPerSample <> 16 then
          begin
            CreatePCMWaveFormat(Intermediate, FromFormat.nSamplesPerSec,
                                FromFormat.nChannels, 16);
            Result := TVorbisWaveWriter.Create(@Intermediate, ToFormat);
            Result := TBitsWaveFilter.Create(FromFormat, Result);
          end else
            Result := TVorbisWaveWriter.Create(FromFormat, ToFormat);
        end;
      WAVE_FORMAT_FLAC:
        // FLAC supports all (8, 16 and 24) bits per sample
        Result := TFlacWaveWriter.Create(FromFormat, ToFormat);
      WAVE_FORMAT_APE:
        Result := TAPEWaveWriter.Create(FromFormat, ToFormat);
    else
    if IsPCM(FromFormat) and IsPCM(ToFormat)
    then begin
      Result := TMMIOWaveWriter.Create(ToFormat, ToFormat);
      if (FromFormat.nChannels = ToFormat.nChannels) and
         (FromFormat.nSamplesPerSec = ToFormat.nSamplesPerSec) then
      begin
        if ToFormat.wBitsPerSample <> FromFormat.wBitsPerSample then
        begin
          CreatePCMWaveFormat(Intermediate, ToFormat.nSamplesPerSec,
                              ToFormat.nChannels, FromFormat.wBitsPerSample);
          Result := TBitsWaveFilter.Create(@Intermediate, Result);
        end;
      end else
      begin
        CreatePCMWaveFormat(Intermediate, ToFormat.nSamplesPerSec,
                            ToFormat.nChannels, 32);
        if Intermediate.Format.wBitsPerSample <> ToFormat.wBitsPerSample then
          Result := TBitsWaveFilter.Create(@Intermediate, Result);
        CreatePCMWaveFormat(Intermediate,
                            Intermediate.Format.nSamplesPerSec,
                            FromFormat.nChannels,
                            Intermediate.Format.wBitsPerSample);
        if Intermediate.Format.nChannels <> ToFormat.nChannels then
          Result := TChannelWaveFilter.Create(@Intermediate, Result);
        CreatePCMWaveFormat(Intermediate,
                            FromFormat.nSamplesPerSec,
                            Intermediate.Format.nChannels,
                            Intermediate.Format.wBitsPerSample);
        if Intermediate.Format.nSamplesPerSec <> ToFormat.nSamplesPerSec then
          Result := TRateWaveFilter.Create(@Intermediate, Result);
        if Intermediate.Format.wBitsPerSample <> FromFormat.wBitsPerSample then
          Result := TBitsWaveFilter.Create(FromFormat, Result);
      end;
    end else
      Result := TACMWaveWriter.Create(FromFormat, ToFormat);
    end;
  end else begin
    Result := TMMIOWaveWriter.Create(FromFormat, FromFormat);
  end;
end;




constructor TWaveWriter.Create;
begin
  inherited Create;
  FromFormat := CloneWaveFormat(AFromFormat);
  ToFormat := CloneWaveFormat(AToFormat);
  FBufferSize := DefaultWriteBufferSize;
end;

destructor TWaveWriter.Destroy;
begin
  FreeMem(ToFormat);
  FreeMem(FromFormat);
  inherited;
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

constructor TWaveWriterFilter.Create;
begin
  inherited Create(AFromFormat, ADestination.FromFormat);
  FDestination := ADestination;
end;

procedure TWaveWriterFilter.IsLastWrite;
begin
  Destination.IsLastWrite;
end;

destructor TWaveWriterFilter.Destroy;
begin
  try
    Destination.Free;
  finally
    inherited;
  end;
end;

// ------

constructor TSimpleWaveFilter.Create;
begin
  inherited;
  BitFactor := 1;
  BitDivider := 1;
  Initialize;
end;

procedure TSimpleWaveFilter.InitFile;
begin
  Destination.FFileName := FileName;
  Destination.ToWrite := MulDiv(ToWrite, BitFactor, BitDivider);
  Destination.InitFile;
  FBufferSize := MulDiv(Destination.BufferSize, BitDivider, BitFactor);
  FBuffer := Pointer(GlobalAlloc(GMEM_FIXED, BufferSize));
  if FBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
end;

procedure TSimpleWaveFilter.DoneFile;
begin
  try
    if (Buffer <> nil) then
    begin
      GlobalFree(Cardinal(Buffer));
      FBuffer := nil;
    end;
  finally
    Destination.DoneFile;
  end;
end;

function TSimpleWaveFilter.Write;
var r: Cardinal;
begin
  Convert(bytes);
  r := Destination.Write(MulDiv(bytes, BitFactor, BitDivider));
  Result := MulDiv(r, BitDivider, BitFactor);
end;

// ----
procedure Cnv32to8(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size shr 2) - 1 do
  begin
    PArray8(outbuffer)[i] := (PArray32(inbuffer)[i] shr 24) + $80;
  end;
end;

procedure Cnv32to16(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size shr 2) - 1 do
  begin
    PArray16(outbuffer)[i] := (PArray32(inbuffer)[i] shr 16);
  end;
end;

procedure Cnv32to24(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size shr 2) - 1 do
  begin
    PArray24(outbuffer)[i] := PThreeByte(PChar(@(PArray32(inbuffer)[i]))+1)^;
  end;
end;


procedure Cnv24to32(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size div 3) - 1 do
  begin
    PArray32(outbuffer)[i] := PInteger(@(PArray24(inbuffer)[i]))^ shl 8;
  end;
end;

procedure Cnv24to16(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size div 3) - 1 do
  begin
    PArray16(outbuffer)[i] := PArray24(inbuffer)[i].h;
  end;
end;

procedure Cnv24to8(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size div 3) - 1 do
  begin
    PArray8(outbuffer)[i] := Hi(PArray24(inbuffer)[i].h) + $80;
  end;
end;

procedure Cnv16to32(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size shr 1) - 1 do
  begin
    PArray32(outbuffer)[i] := PArray16(inbuffer)[i] shl 16;
  end;
end;


procedure Cnv16to24(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size shr 1) - 1 do
  begin
    PArray24(outbuffer)[i].h := PArray16(inbuffer)[i];
    PArray24(outbuffer)[i].l := 0;
  end;
end;

procedure Cnv16to8(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to (size shr 1) - 1 do
  begin
    PArray8(outbuffer)[i] := Hi(PArray16(inbuffer)[i]) + $80;
  end;
end;

procedure Cnv8to32(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to size - 1 do
  begin
    PArray32(outbuffer)[i] := (PArray8(inbuffer)[i] - $80) shl 24;
  end;
end;

procedure Cnv8to24(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to size - 1 do
  begin
    PArray24(outbuffer)[i].h := (PArray8(inbuffer)[i] - $80) shl 8;
    PArray24(outbuffer)[i].l := 0;
  end;
end;

procedure Cnv8to16(inbuffer, outbuffer: Pointer; size: Cardinal);
var i: Integer;
begin
  for i := 0 to size - 1 do
  begin
    PArray16(outbuffer)[i] := (PArray8(inbuffer)[i] - $80) shl 8;
  end;
end;

const BitConvertors: array[0..3, 0..3] of TBitConvertorProc =
      ((nil,      Cnv8to16,  Cnv8to24,  Cnv8to32 ),
       (Cnv16to8, nil,       Cnv16to24, Cnv16to32),
       (cnv24to8, Cnv24to16, nil,       Cnv24to32),
       (Cnv32to8, Cnv32to16, Cnv32to24, nil      ));

const BitFactors: array[0..3, 0..3] of Word =
     ((12, 24, 36, 48),
      ( 6, 12, 18, 24),
      ( 4,  8, 12, 16),
      ( 3,  6,  9, 12));

procedure TBitsWaveFilter.Initialize;
var frombits, tobits: Integer;
begin
  BitDivider := 12;
  frombits := FromFormat.wBitsPerSample;
  tobits := Toformat.wBitsPerSample;
  BitFactor := BitFactors[frombits shr 3 - 1, tobits shr 3 - 1];
  Convertor := BitConvertors[frombits shr 3 - 1, tobits shr 3 - 1];
  if @Convertor = nil then
    raise Exception.CreateFmt('Cannot convert %d bits to %d bits',
                              [frombits, tobits]);
end;

procedure TBitsWaveFilter.Convert;
begin
  Convertor(Buffer, Destination.Buffer, bytes);
end;

// -----

procedure TChannelWaveFilter.Initialize;
begin
  if FromFormat.wBitsPerSample <> 32 then
     raise Exception.Create('FromFormat must have 32 bits for Channel filter');
  if ToFormat.wBitsPerSample <> 32 then
     raise Exception.Create('ToFormat must have 32 bits for Channel filter');
  BitDivider := FromFormat.nChannels;
  BitFactor := ToFormat.nChannels;
end;

procedure TChannelWaveFilter.Convert;
var i,j,k, v: Integer;
    samples: Cardinal;
    inbuffer, outbuffer: PArray32;
begin
  samples := (bytes shr 2) div BitDivider; // # of samples
  inbuffer := Buffer;
  outbuffer := Destination.Buffer;
  if BitFactor = 1 then
  begin
       // Convert to mono, average channels into one
       j := 0;
       for i := 0 to samples-1 do
       begin
         v := inbuffer[j] div Integer(BitDivider);
         inc(j);
         for k := 1 to BitDivider-1 do
         begin
           inc(v, inbuffer[j] div Integer(BitDivider));
           inc(j);
         end;
         outbuffer[i] := v;
       end;
  end else
  if BitDivider = 1 then
  begin
       // Convert mono to 'all' channels
       j := 0;
       for i := 0 to samples-1 do
       begin
         v := inbuffer[i];
         for k := 0 to BitDivider-1 do
         begin
           outbuffer[j] := v;
           inc(j);
         end;
       end;
  end else
    raise Exception.Create('Can only convert to/from MONO (for now)');
end;

// ----

constructor TRateWaveFilter.Create;
begin
  inherited;
  if (FromFormat.wBitsPerSample <> 32) or
     (ToFormat.wBitsPerSample <> 32) then
    raise Exception.Create('Formats must be 32 bits for Rate filter');
  if (Toformat.nChannels <> FromFormat.nChannels) then
    raise Exception.Create('nChannels must be equal for Rate filter');
  BitDivider := FromFormat.nSamplesPerSec;
  BitFactor := ToFormat.nSamplesPerSec;
end;

procedure TRateWaveFilter.InitFile;
begin
  Destination.FFileName := FileName;
  Destination.ToWrite := MulDiv(ToWrite, BitFactor, BitDivider);
  Destination.InitFile;
  FBufferSize := MulDiv(Destination.BufferSize, BitDivider, BitFactor);
  FOutSamples := (Destination.BufferSize shr 2) div ToFormat.nChannels;
  FBufferSize := (FBufferSize div FromFormat.nBlockAlign) * FromFormat.nBlockAlign;
  FInBufferSize := FBufferSize + 4096; // TODO: Formule verzinnen
  FInBuffer := Pointer(GlobalAlloc(GMEM_FIXED, FInBufferSize));
  if FInBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
  FBuffer := FInBuffer;
  FCarry := 0;
end;

procedure TRateWaveFilter.DoneFile;
begin
  try
    if (Buffer <> nil) then
    begin
      GlobalFree(Cardinal(FInBuffer));
      FInBuffer := nil;
    end;
  finally
    Destination.DoneFile;
  end;
end;


function TRateWaveFilter.Write;
var inbuf, outbuf: PArray32;
    i, p, q, c: Integer;
    nChannels: Integer;
    outSamples: Integer;
    s: Cardinal;
begin
  inbuf := Buffer;
  outbuf := Destination.Buffer;
  nChannels := FromFormat.nChannels;
  s := Destination.BufferSize;
  outSamples := (Int64(Integer(bytes shr 2) div (nChannels)) * BitFactor) div BitDivider;
  //MulDiv((Integer(bytes shr 2) div (nChannels)), BitFactor, BitDivider);
  for i := 0 to outSamples - 1 do
  begin
    p := i * nChannels;
    q := Integer(int64(i) * BitDivider div BitFactor) * nChannels;
    for c := 0 to nChannels-1 do
    begin
      if Cardinal(p+c) shl 2 >= s then
         raise Exception.Create('Write outside buffer q=' + IntToStr(q) + ' s=' + IntToStr(s));
      if Cardinal(q+c) shl 2 >= BufferSize then
         raise Exception.Create('Read outside buffer p=' + IntToStr(p) + ' s=' + IntToStr(Buffersize));
      outbuf[p+c] := inbuf[q+c];
    end
  end;
  bytes :=
     Destination.Write(outSamples * nChannels shl 2);
  Result := (Int64(bytes) * BitDivider) div BitFactor;
  //Result := Bytes;
end;

// ----

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
  if hFile = INVALID_HANDLE_VALUE then RaiseLastOSError;
end;

procedure TFileWaveWriter.DoneFile;
begin
  if hFile <> INVALID_HANDLE_VALUE then CloseHandle(hFile);
  inherited;
end;

procedure TFileWaveWriter.FileWrite;
begin
  if sysUtils.FileWrite(hFile, buffer, bytes) <> integer(bytes) then
    raise Exception.Create('Write to file failed');
end;

//------

procedure TLameWaveWriter.InitFile;
var dwSamples: Cardinal;
begin
  FileName := ChangeFileExt(FileName, '.mp3');
  inherited;
  dwSamples := 0;
  dwMP3Buffer := 0;
  LameCall(procbeInitStream(@(PBEWaveFormat(Toformat).BE), dwSamples, dwMP3Buffer, hbeStream));
  FBufferSize := dwSamples * 2;
  FBuffer := Pointer(GlobalAlloc(GMEM_FIXED, BufferSize));
  if FBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
  pMP3Buffer := Pointer(GlobalAlloc(GMEM_FIXED, dwMP3Buffer * 8));
end;

procedure TLameWaveWriter.DoneFile;
var dwWrite: Cardinal;
begin
 try
  dwWrite := 0;
  LameCall(procBeDeinitStream(hbeStream, pMP3Buffer, dwWrite));
  if dwWrite > 0 then FileWrite(pMP3Buffer^, dwWrite);
 finally
  procBeCloseStream(hbeStream);
  if (Buffer <> nil) then
  begin
    GlobalFree(Cardinal(Buffer));
    FBuffer := nil;
  end;
  if pMP3Buffer <> nil then
  begin
    GlobalFree(Cardinal(pMP3Buffer));
    pMP3Buffer := nil;
  end;
  inherited;
  if PBEWaveFormat(Toformat).BE.format.LHV1.bWriteVBRHeader then
    procBeWriteVBRHeader(PChar(FileName));
 end;
end;


function TLameWaveWriter.Write(bytes: Cardinal): Cardinal;
var dwWrite: Cardinal;
begin
  dwWrite := 0;
  LameCall(procBeEncodeChunk(hbeStream, bytes div 2,
                             Buffer, pMP3Buffer, dwWrite));
  if dwWrite > dwMP3Buffer then
  begin
    dwWrite := dwMP3Buffer;
  end;
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

  function EncSeekCBFunc(encoder : PFLAC__StreamEncoder;
                      absolute_byte_offset : FLAC__uint64;
                      client_data : Pointer) : Integer; cdecl;
  var
    res: Cardinal;
  begin
    Result := FLAC__STREAM_ENCODER_SEEK_STATUS_OK;
    res := SetFilePointer(TFlacWaveWriter(client_data).hFile,
                          Int64Rec(absolute_byte_offset).Lo,
                          @Int64Rec(absolute_byte_offset).Hi,
                          SEEK_SET);
    if (res = $FFFFFFFF) and (GetLastError <> NO_ERROR) then
      Result := FLAC__STREAM_ENCODER_SEEK_ERROR;
  end;

  function EncWriteCBFunc(encoder : PFLAC__StreamEncoder;
                                buffer : PFLAC__byte;
                                bytes, samples, current_frame : LongWord;
                                client_data : Pointer) : Integer; cdecl;
  begin
    if FileWrite(TFlacWaveWriter(client_data).hFile, buffer^, bytes) =
       Integer(bytes)
    then
      Result := FLAC__STREAM_ENCODER_OK
    else
      Result := FLAC__STREAM_ENCODER_WRITE_ERROR;
  end;

function EncTellCBfunc(encoder: PFLAC__StreamEncoder;
                       var absolute_byte_offset: FLAC__uint64;
                       client_data: Pointer): Integer; cdecl;
var f: TFlacWaveWriter;
    r, h: Cardinal;
begin
  f := client_data;
  h := 0;
  // 64 - bit file sizes not fully supported here...
  r := SetFilePointer(f.HFile, 0, @h, FILE_CURRENT);
  if (r = $FFFFFFFF) and (GetLastError <> NO_ERROR) then
    Result := FLAC__STREAM_ENCODER_TELL_STATUS_ERROR
  else begin
    Result := FLAC__STREAM_ENCODER_TELL_STATUS_OK;
    Int64Rec(absolute_byte_offset).Lo := r;
    Int64Rec(absolute_byte_offset).Hi := h;
  end;
end;


{
  FLAC__StreamEncoderMetadataCallback = procedure(encoder: PFLAC__StreamEncoder;
                                              const metadata: PFLAC__StreamMetadata;
                                              client_data : Pointer); cdecl;

 }


constructor TFlacWaveWriter.Create;
var F32BufferSize: Cardinal;
begin
  inherited;
  stream := FLAC__stream_encoder_new;
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
  if stream <> nil then FLAC__stream_encoder_delete(stream);
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
    err: Integer;
begin
  FileName := ChangeFileExt(FileName, '.flac');
  inherited;
  FLAC__stream_encoder_set_channels(stream, FromFormat.nChannels);
  FLAC__stream_encoder_set_bits_per_sample(stream, FromFormat.wBitsPerSample);
  FLAC__stream_encoder_set_sample_rate(stream, FromFormat.nSamplesPerSec);
  FLAC__stream_encoder_set_total_samples_estimate(stream, ToWrite div FromFormat.nBlockAlign);
  cfg := FlacConfig[PFlacWaveFormat(ToFormat).Level];
  with cfg do
  begin
    FLAC__stream_encoder_set_max_lpc_order(stream, lpc);
    FLAC__stream_encoder_set_blocksize(stream, block);
    if rice_min > 0 then
      FLAC__stream_encoder_set_min_residual_partition_order(stream, rice_min);
    FLAC__stream_encoder_set_max_residual_partition_order(stream, rice_max);
    if ToFormat.nChannels > 1 then
    begin
      FLAC__stream_encoder_set_do_mid_side_stereo(stream, midside);
      FLAC__stream_encoder_set_loose_mid_side_stereo(stream, a_midside);
    end;
    FLAC__stream_encoder_set_do_exhaustive_model_search(stream, exhaustive);
  end;
  err := FLAC__stream_encoder_init_stream(stream, EncWriteCBFunc, EncSeekCBFunc, EncTellCBFunc, nil, self);
  if err <> FLAC__STREAM_ENCODER_OK then
    raise Exception.Create('Failed to initialize FLAC stream encoder, code: ' + IntToStr(err));
end;

procedure TFlacWaveWriter.DoneFile;
begin
  try
    if FLAC__stream_encoder_get_state(stream) <>
       FLAC__STREAM_ENCODER_UNINITIALIZED then
      FLAC__stream_encoder_finish(stream);
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
    // FLAC wil 24-bit signed hebben?
    // nok 17..24: for i := 0 to s-1 do flacBuf[i] := PInteger(@(PArray24(Buf)[i]))^ and $00FFFFFF ;
    // nok 17..24: for i := 0 to s-1 do flacBuf[i] := (PInteger(@(PArray24(Buf)[i]))^ shl 8) shr 8;
    17..24: for i := 0 to s-1 do
            begin
              flacBuf[i] := (PArray24(Buf)[i].h shl 8) or PArray24(Buf)[i].l;
            end;
    else flacBuf := Buffer;
  end;
  if not FLAC__stream_encoder_process_interleaved(stream, @flacBuf[0], s div Fromformat.nChannels) then
    raise Exception.Create('FLAC Stream write failed?');
  result := bytes;
end;

constructor TAPEWaveWriter.Create(AFromFormat, AToFormat: PWaveFormatEx);
begin
  inherited;
  APELoadDLL;
  if not LibAPELoaded then
      raise Exception.Create('Cannot write APE files - unable to open MACDll.dll.');
  FBuffer := Pointer(GlobalAlloc(GMEM_FIXED, BufferSize));
  if FBuffer = nil then
    raise Exception.Create(NoAllocMemStr);
end;

destructor TAPEWaveWriter.Destroy;
begin
  if (Buffer <> nil) then
  begin
    GlobalFree(Cardinal(Buffer));
    FBuffer := nil;
  end;
  inherited;
end;

procedure TAPEWaveWriter.DoneFile;
begin
  if assigned(hAPECompress) then
  try
    APECall(APECompress_Finish(hAPECompress, nil, 0, 0));
  finally
    APECompress_Destroy(hAPECompress);
    hAPECompress := nil;
  end;
  inherited;
end;

procedure TAPEWaveWriter.InitFile;
var
  r: Integer;
begin
  inherited;
  r := 0;
  FileName := ChangeFileExt(FileName, '.ape');
  hAPECompress := APECompress_Create(@r);
  if (hAPECompress = nil) then
      APECall(r);
  APECall(APECompress_Start(hAPECompress, PChar(FileName), FromFormat, ToWrite,
      PAPEWaveFormat(ToFormat).Level));
end;

function TAPEWaveWriter.Write(Bytes: Cardinal): Cardinal;
begin
    APECall(APECompress_AddData(hAPECompress, Buffer, Bytes));
    Result := Bytes;
end;

end.

