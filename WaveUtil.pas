unit WaveUtil;
interface

uses Windows, MMSystem;
{
 Surround spul afkomstig van:

 ms-help://MS.VSCC/MS.MSDNVS/dx8_c/directx_cpp/htm/_dx_waveformatextensible_dxaudio.htm
}

const
WAVE_FORMAT_EXTENSIBLE = $FFFE;

WAVE_FORMAT_DIRECT = $0008;
WAVE_FORMAT_DIRECT_QUERY = WAVE_FORMAT_QUERY or WAVE_FORMAT_DIRECT;

WFE_GUID: TGUID = '{00000001-0000-0010-8000-00aa00389b71}';

SPEAKER_FRONT_LEFT              = $1;
SPEAKER_FRONT_RIGHT             = $2;
SPEAKER_FRONT_CENTER            = $4;
SPEAKER_LOW_FREQUENCY           = $8;
SPEAKER_BACK_LEFT               = $10;
SPEAKER_BACK_RIGHT              = $20;
SPEAKER_FRONT_LEFT_OF_CENTER    = $40;
SPEAKER_FRONT_RIGHT_OF_CENTER   = $80;
SPEAKER_BACK_CENTER             = $100;
SPEAKER_SIDE_LEFT               = $200;
SPEAKER_SIDE_RIGHT              = $400;
SPEAKER_TOP_CENTER              = $800;
SPEAKER_TOP_FRONT_LEFT          = $1000;
SPEAKER_TOP_FRONT_CENTER        = $2000;
SPEAKER_TOP_FRONT_RIGHT         = $4000;
SPEAKER_TOP_BACK_LEFT           = $8000;
SPEAKER_TOP_BACK_CENTER         = $10000;
SPEAKER_TOP_BACK_RIGHT          = $20000;

WAVE_FORMAT_48M08 =     $00001000;
WAVE_FORMAT_48S08 =     $00002000;
WAVE_FORMAT_48M16 =     $00004000;
WAVE_FORMAT_48S16 =     $00008000;
WAVE_FORMAT_96M08 =     $00010000;
WAVE_FORMAT_96S08 =     $00020000;
WAVE_FORMAT_96M16 =     $00040000;
WAVE_FORMAT_96S16 =     $00080000;


RedBookFormat: TWaveFormatEx = (
    wFormatTag : WAVE_FORMAT_PCM;
    nChannels  : 2;
    nSamplesPerSec : 44100;
    nAvgBytesPerSec : 44100 * 4;
    nBlockAlign : 4;
    wBitsPerSample : 16;
    cbSize : 0;
  );

type
    PWAVEFORMATEXTENSIBLE = ^TWAVEFORMATEXTENSIBLE;
    TWAVEFORMATEXTENSIBLE = record
        Format: TWAVEFORMATEX;
        wSamples: WORD;
        //(wValidBitsPerSample: WORD); //       /* bits of precision  */
        //(wSamplesPerBlock: WORD);          //* valid if wBitsPerSample==0 */
        //(wReserved: WORD);                 //* If neither applies, set to zero. */
        dwChannelMask: DWORD;      //* which channels are */
                                   //* present in stream  */
        SubFormat: TGUID;
    end;


type
  PWaveInfo = ^TWaveInfo;
  TWaveInfo = record
    hHeader,
    hData: THandle;
  end;

  PSectorStats = ^TSectorStats;
  TSectorStats = record
    Max, Min: SmallInt;
  end;
  PSectorStatsList = ^TSectorStatsList;
  TSectorStatsList = array[0..(MaxInt div sizeof(TSectorStats))-1] of TSectorStats;
  TWaveFormRec = record
    Lo, Hi: Integer;
  end;
  PWaveForm= ^TWaveForm;
  TWaveForm = array[0..(MaxInt div sizeof(TWaveFormRec))-1] of TWaveFormRec;

  PSplitInfo = ^TSplitInfo;
  TSplitInfo = record
    Start, Size: Cardinal;
    Color: Integer;
    KeepName: Boolean;
  end;

  PCardinal = ^Cardinal;

  PThreeByte = ^TThreeByte;
  TThreeByte = packed record
    l: Byte;
    h: Smallint; // 24-bit is signed...
  end;

  TShortArray = array[0..MAXINT div sizeof(SmallInt) - 1] of Smallint;
  PShortArray = ^TShortArray;
  TByteArray = array[0..MAXINT div sizeof(SmallInt) - 1] of Byte;
  TArray32 = array[0..(MaxInt div sizeof(Integer))-1] of Integer;
  PArray32 = ^TArray32;
  TArray24 = array[0..(MaxInt div sizeof(TThreeByte))-1] of TThreeByte;
  PArray24 = ^TArray24;
  TArray16 = array[0..(MaxInt div sizeof(SmallInt))-1] of SmallInt;
  PArray16 = ^TArray16;
  TArray8 = array[0..(MaxInt div sizeof(Byte))-1] of Byte;
  PArray8 = ^TArray8;

{  TSectorStatistics = class(TObject)
  private
    RefCount: Integer;
  public
    Size: Integer;
    property SectorStatsList: PSectorStatsList;
    destructor Destroy; override;
    procedure  Enter;
    procedure  Leave;
    constructor Create;
  end;}

function AllocWave(Size: Longint): PWaveHdr;
procedure FreeWave(Wave: PWaveHdr);

function getWaveFormatSize(const WaveFormat: PWaveFormatEx): Cardinal;
procedure CopyWaveFormat(const src: PWaveFormatEx; dst: PWaveFormatEx);
function CloneWaveFormat(const src: PWaveFormatEx): PWaveFormatEx;
procedure CreatePCMWaveFormat(
          var WaveFormat: TWaveFormatExtensible;
          Frequency, Channels, Bits: Integer);
function IsPCM(const fmt: PWaveFormatEx): boolean;

function OpenWaveFile(const FileName: string; WaveFileSize: PCardinal;
         Mode, CreationDistribution: Cardinal): HMMIO;
procedure WriteWaveHeader(toFile: HMMIO; var ckOutRIFF, ckOut: TMMCKInfo;
                          Format: PWaveFormatEx; DataBytes: Cardinal);
procedure mmioAscendSafe(hfile: HMMIO; var ckOut: TMMCKInfo; Size: Cardinal);

procedure FastCopy(Dest, Source: Pointer; Size:Cardinal); {size is half the buffer!}
function SectorLevel8(Sector: Pointer; Size: Cardinal): TSectorStats;
function SectorLevel16(Sector: Pointer; Size: Cardinal): TSectorStats;
function SectorLevel24(Sector: Pointer; Size: Cardinal): TSectorStats;
// Calculate sector stats into StatsList. Returns # of records filled.
function FillSectorStats(
  StatsList: PSectorStatsList;
  Offset: Cardinal;
  var Carry: Cardinal;
  Data: Pointer;
  DataSize: Cardinal;
  SectorSize: Cardinal;
  BitsPerSample: Word): Cardinal;
procedure QuadMemCopy(Dest, Source: Pointer; NumQuads: Cardinal);
procedure QuadZero(Dest: Pointer; NumQuads: Cardinal);
procedure QuadFill(Dest: Pointer; NumQuads: Cardinal; Value: Cardinal);

resourcestring
  NoRiffStr = 'Cannot create RIFF chunk.';
  NoFmtStr = 'Cannot create fmt chunk.';
  NoWriteFmtStr = 'Cannot write to fmt chunk.';
  NoAscWriteStr = 'Cannot ascend/write.';
  NoDataStr = 'Cannot create data chunk.';
  NoOpenStr = 'Error occurred: '#10'%s'#10'While opening file: "%s"';

implementation

uses SysUtils;

function OpenWaveFile;
var
   mmioInfo: TMMIOINFO;
   mmMode: Cardinal;
begin
  FillChar(mmioInfo, sizeof(TMMIOINFO), 0);
  mmioInfo.fccIOProc := FOURCC_DOS;
  mmioInfo.adwInfo[0] := CreateFile(
          PChar(FileName),
          Mode,
          FILE_SHARE_READ,
          nil,
          CreationDistribution,
          FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,
          0);
  if mmioInfo.adwInfo[0] = INVALID_HANDLE_VALUE then
          raise EOSError.CreateFmt(
            NoOpenStr,
            [SysErrorMessage(GetLastError), FileName]);
  if WaveFileSize <> nil then
    WaveFileSize^ := GetFileSize(mmioInfo.adwInfo[0], nil);
  mmMode := MMIO_DENYWRITE;
  if (Mode and GENERIC_READ) <> 0 then mmMode := mmMode or MMIO_READ;
  if (Mode and GENERIC_WRITE) <> 0 then mmMode := mmMode or MMIO_WRITE;
  Result := mmioOpen(nil, @mmioInfo, mmMode);
  if Result=0 then
    raise Exception.Create('Unable to open file "' + FileName + '".');
end;

procedure WriteWaveHeader;
var e: MMRESULT;
procedure Err(Msg: string);
begin
  mmioClose(ToFile, 0);
  ToFile := 0;
  raise Exception.Create(Msg);
end;
begin
  ckOutRIFF.fccType := mmioStringToFOURCC('WAVE', 0);
  ckOut.ckid := mmioStringToFOURCC('fmt ',0);
  if Format.wFormatTag = WAVE_FORMAT_PCM then
    ckOut.ckSize := sizeof(TWaveFormatEx) - 2
  else
    ckOut.ckSize := sizeof(TWaveFormatEx) + Format.cbSize;
  // The RIFF size is format + data + 20 bytes //
  ckOutRIFF.ckSize := DataBytes + ckOut.ckSize + 20;
  e := mmioCreateChunk(ToFile, @ckOutRIFF, MMIO_CREATERIFF);
  if e <> 0 then Err(NoRiffStr + IntToStr(e));
  if mmioCreateChunk(ToFile, @ckOut, 0) <> 0 then Err(NoFmtStr);
  if mmioWrite(ToFile,  PChar(Format), ckOut.ckSize) <> integer(ckOut.ckSize)
    then Err(NoWriteFmtStr);
  if mmioAscend(ToFile, @ckOut, 0) <> 0 then Err(NoAscWriteStr);
  ckOut.ckid := mmioStringToFOURCC('data',0);
  ckOut.cksize := DataBytes;
  if mmioCreateChunk(ToFile, @ckOut, 0) <> 0 then
    Err(NoDataStr);
end;

procedure mmioAscendSafe(hfile: HMMIO; var ckOut: TMMCKInfo; Size: Cardinal);
//var err: MMRESULT;
begin
//  err := mmioAscend(hfile, @ckOut, 0);
//  if err <> MMSYSERR_NOERROR then
//  begin
//    OutputDebugString(PChar('mmioAscend failed, code: ' + IntToStr(err)));
    if mmioSeek(hfile, ckOut.dwDataOffset - 4, SEEK_SET) = -1 then
    begin
      OutputDebugString(PChar('mmioSeek failed too, aborting'));
      Exit;
    end;
    mmioWrite(hfile, @Size, sizeof(size));
    ckOut.cksize := Size;
//  end;
end;

function AllocWave;
var
    memh, memd: THandle;
    WaveInfo: PWaveInfo;
begin
   memh := GlobalAlloc(GMEM_SHARE or GHND, Sizeof(TWaveHdr));
   if memh=0 then
      RaiseLastOSError;
   memd := GlobalAlloc(GMEM_SHARE or GHND, Size);
   if memd=0 then begin
      GlobalFree(memh);
      RaiseLastOSError;
   end;
   Result := GlobalLock(memh);
   new(WaveInfo);
   WaveInfo.hData := memd;
   WaveInfo.hHeader := memh;
   with Result^ do begin
     lpData          := GlobalLock(memd);
     dwBufferLength  := Size;
     dwBytesRecorded := 0;
     dwFlags         := 0;
     dwLoops         := 0;
     dwUser          := longint(WaveInfo);
   end;
end;

procedure FreeWave;
var memh, memd: THandle;
begin
  memh := PWaveInfo(Wave^.dwUser)^.hData;
  memd := PWaveInfo(Wave^.dwUser)^.hHeader;
  dispose(PWaveInfo(Wave^.dwUser));
  GlobalUnlock(memh);
  GlobalFree(memh);
  GlobalUnlock(memd);
  GlobalFree(memd);
end;

procedure FastCopy(Dest, Source: Pointer; Size:Cardinal); assembler;
asm
  push EDI
  push ESI
  mov  ECX, Size
  mov  ESI, Source
  mov  EDI, Dest
  cld
@@1:
  lodsd
  stosd
  lodsd
  loop @@1
  pop  ESI
  pop  EDI
end;

function SectorLevel8(Sector: Pointer; Size: Cardinal): TSectorStats; assembler;
asm
  push ESI
  push EBX
  mov  ESI, Sector
  mov  ECX, Size
  mov  bl, $80
  mov  dl, bl
  jecxz @@4
  cld
@@1:
  lodsb
  cmp  AL, DL
  ja   @@2
  cmp  AL, BL
  jb   @@3
  loop @@1
  jmp  @@4
@@2:
  mov DL, AL
  loop @@1
  jmp  @@4
@@3:
  mov  BL, AL
  loop @@1
@@4:
  mov  al, $80  {Adjust for un-signed}
  sub  dl, al
  sub  bl, al
  xor  EAX, EAX {move values into EAX register}
  mov  AH, BL
  shl  EAX, 16
  not  AL
  mov  AH, DL
  pop  EBX
  pop  ESI
end;


function SectorLevel16(Sector: Pointer; Size: Cardinal): TSectorStats; assembler;
asm
  push ESI
  push EBX
  mov  ESI, Sector
  mov  ECX, Size
  xor  EDX, EDX
  xor  EBX, EBX
  jecxz @@4
  cld
@@1:
  lodsw
  cmp  AX, DX
  jg   @@2
  cmp  AX, BX
  jl   @@3
  loop @@1
  jmp  @@4
@@2:
  mov DX, AX
  loop @@1
  jmp  @@4
@@3:
  mov  BX, AX
  loop @@1
@@4:
  mov  AX, BX
  pop  EBX
  pop  ESI
  shl  EAX, 16
  mov  AX, DX
end;

function SectorLevel24(Sector: Pointer; Size: Cardinal): TSectorStats;
var i: Integer;
    v: SmallInt;
begin
  Result.Min := 0;
  Result.Max := 0;
  for i := 0 to Size-1 do
  begin
    v := PArray24(Sector)[i].h;
    if v > Result.Max then Result.Max := v
    else if v < Result.Min then Result.Min := v;
  end;
end;

function FillSectorStats;
var
  n, j, BlockSize: Cardinal;
  Stats: TSectorStats;
begin
  if (Carry > 0) then
  begin
    BlockSize := SectorSize - Carry;
    case BitsPerSample of
     8: Stats := SectorLevel8(Data, BlockSize shr 1);
    16: Stats := SectorLevel16(Data, BlockSize shr 2);
    24: Stats := SectorLevel24(Data, BlockSize div 3);
    end;
    if Stats.Min < StatsList[Offset - 1].Min then
      StatsList[Offset - 1].Min := Stats.Min;
    if Stats.Max > StatsList[Offset - 1].Max then
      StatsList[Offset - 1].Max := Stats.Max;
    Data := @(PChar(Data)[BlockSize]);
    dec(DataSize, BlockSize);
    Carry := 0;
  end;
  n := DataSize div SectorSize;
  Result := 0;
  if n > 0 then
  begin
    case BitsPerSample of
    8: begin
      BlockSize := SectorSize shr 1;
      for j:=0 to n-1 do
      begin
        StatsList[Offset + Result] := SectorLevel8(Data, BlockSize);
        Data := @(PChar(Data)[SectorSize]); // Data += SectorSize
        inc(Result);
      end;
    end;
    16: begin
      BlockSize := SectorSize shr 2;
      for j := 0 to n-1 do
      begin
        StatsList[Offset + Result] := SectorLevel16(Data, BlockSize);
        Data := @(PChar(Data)[SectorSize]); // Data += SectorSize
        inc(Result);
      end;
    end;
    24: begin
      BlockSize := SectorSize div 3;
      for j := 0 to n-1 do
      begin
        StatsList[Offset + Result] := SectorLevel24(Data, BlockSize);
        Data := @(PChar(Data)[SectorSize]); // Data += SectorSize
        inc(Result);
      end;
    end;
    end{case};
  end;
  n := DataSize mod SectorSize;
  if (n > 0) then // See if there are "left-overs"
  begin
    Carry := n;
    case BitsPerSample of
    8:  StatsList[Offset + Result] := SectorLevel8(Data, n shr 1);
    16: StatsList[Offset + Result] := SectorLevel16(Data, n shr 2);
    24: StatsList[Offset + Result] := SectorLevel24(Data, n div 3);
    end;
    inc(Result);
  end;
end;


procedure QuadMemCopy(Dest, Source: Pointer; NumQuads: Cardinal); assembler;
asm
  push ESI
  push EDI
  mov  ESI, Source
  mov  EDI, Dest
  mov  ECX, NumQuads
  cld
  rep  movsd
  pop  EDI
  pop  ESI
end;

procedure QuadZero(Dest: Pointer; NumQuads: Cardinal); assembler;
asm
  push EDI
  mov  EDI, Dest
  mov  ECX, NumQuads
  xor  EAX, EAX
  cld
  rep  stosd
  pop  EDI
end;

procedure QuadFill(Dest: Pointer; NumQuads: Cardinal; Value: Cardinal); assembler;
asm
  push EDI
  mov  EDI, Dest
  mov  ECX, NumQuads
  mov  EAX, Value
  cld
  rep  stosd
  pop  EDI
end;

{destructor TSectorStatistics.Destroy;
begin
end;

    procedure  TSectorStatistics.Enter;
    procedure  TSectorStatistics.Leave;
    constructor TSectorStatistics.Create;
 }



function getWaveFormatSize(const WaveFormat: PWaveFormatEx): Cardinal;
begin
  result := sizeof(TWaveFormatEx) + WaveFormat.cbSize;
end;

procedure CopyWaveFormat(const src: PWaveFormatEx; dst: PWaveFormatEx);
begin
  Move(src^, dst^, sizeof(TWaveFormatEx) + src.cbSize);
end;

function CloneWaveFormat(const src: PWaveFormatEx): PWaveFormatEx;
var size: Integer;
begin
  size := getWaveFormatSize(src);
  GetMem(Result, size);
  move(src^, Result^, size);
end;

const ChannelMasks: array[0..3] of DWORD =
   (SPEAKER_FRONT_CENTER,
    SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT,
    SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
      SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT,
    SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
      SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT or
      SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY);

procedure CreatePCMWaveFormat(
          var WaveFormat: TWaveFormatExtensible;
          Frequency, Channels, Bits: Integer);
begin
  with WaveFormat.Format do
  begin
    wBitsPerSample := Bits;
    nChannels      := Channels;
    if (wBitsPerSample > 16) or
       (nChannels > 2) then
    begin
       wFormatTag := WAVE_FORMAT_EXTENSIBLE;
       WaveFormat.wSamples := wBitsPerSample;
       case channels of
       1: WaveFormat.dwChannelMask := SPEAKER_FRONT_CENTER;
       2: WaveFormat.dwChannelMask := SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT;
       4: WaveFormat.dwChannelMask := SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
                                      SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT;
       6: WaveFormat.dwChannelMask := SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
                                      SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT or
                                      SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY;
       else WaveFormat.dwChannelMask := 0;
       end;
       WaveFormat.SubFormat := WFE_GUID;
       cbSize := 22;
    end else begin
       wFormatTag     := WAVE_FORMAT_PCM;
       cbSize := 0;
    end;
    nSamplesPerSec := Frequency;
    if nSamplesPerSec <= 0 then nSamplesPerSec := 1;
    nBlockAlign    := nChannels * (wBitsPerSample shr 3);
    nAvgBytesPerSec:= nSamplesPerSec * nBlockalign;
  end;
end;

function IsPCM(const fmt: PWaveFormatEx): boolean;
begin
  if fmt.wFormatTag = WAVE_FORMAT_PCM then
    Result := True
  else if fmt.wFormatTag = WAVE_FORMAT_EXTENSIBLE then
    Result := CompareMem(@PWaveFormatExtensible(fmt).SubFormat, @WFE_GUID, sizeof(TGUID))
  else
    Result := False;
end;

end.
