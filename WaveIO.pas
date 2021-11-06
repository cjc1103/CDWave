unit WaveIO;

interface

uses Windows, MMSystem, Controls, Forms, Dialogs, SysUtils, WaveUtil, FLAC, APE;

type
    TWaveFile = class
    protected
      FPCMWaveFormat: TWAVEFORMATEXTENSIBLE;
      FWaveSize: Cardinal;
      function GetIsOpen: Boolean; virtual; abstract;
      // Return native format. By default, just returns @PCMWaveFormat
      function GetNativeWaveFormat: PWaveFormatEx; virtual;
    public
      destructor Destroy; override;
      // Open file, enable all properties, set IsOpen to TRUE
      procedure Open(const AFileName: string); virtual; abstract;
      // Close file, set IsOpen to FALSE
      procedure Close; virtual; abstract;
      procedure SetFilePos(bytes: Cardinal); virtual; abstract;
      function GetFilePos: Cardinal; virtual; abstract;
      function Read(Buffer: PChar; Bytes: Cardinal): Cardinal; virtual; abstract;
      // Properties
      property PCMWaveFormat: TWaveFormatEx read FPCMWaveFormat.Format;
      property PCMWaveFormatEx: TWaveFormatExtensible read FPCMWaveFormat;
      //property NativeWaveFormat: PWaveFormatEx read GetNativeWaveFormat;
      // Determine if file is open (i.e. Close should be called)
      property IsOpen: Boolean read GetIsOpen;
      // Size of waveform data in Bytes
      property WaveSize: Cardinal read FWaveSize;
      // Current position in file, in bytes
      property FilePos: Cardinal read GetFilePos write SetFilePos;
    end;

    // Regular WAV file (PCM)
    TMMIOWaveFile = class(TWaveFile)
    private
      hFile: THANDLE;
      FWaveOffset:Cardinal;
    protected
      function GetIsOpen: Boolean; override;
      //function GetNativeWaveFormat: PWaveFormatEx; override;
    public
      procedure Open(const AFileName: string); override;
//      procedure Assign(AWaveFile: HMMIO; AWaveOffset: Cardinal;
//                       AWaveSize: Cardinal; AWaveFormat: PWaveFormatEx);
      procedure Close; override;
      procedure SetFilePos(bytes: Cardinal); override;
      function GetFilePos: Cardinal; override;
      function Read(Buffer: PChar; Bytes: Cardinal): Cardinal; override;
    end;

    TW64WaveFile = class(TWaveFile)
    private
      hFile: THANDLE;
      FWaveOffset: Int64;
    protected
      function GetIsOpen: Boolean; override;
    public
      procedure Open(const AFileName: string); override;
      procedure Close; override;
      procedure SetFilePos(bytes: Cardinal); override;
      function GetFilePos: Cardinal; override;
      function Read(Buffer: PChar; Bytes: Cardinal): Cardinal; override;
    end;

    TFLACWaveFile = class(TWaveFile)
    private
      HFile: THandle;
      FStream: PFLAC__StreamDecoder;
      FStatus: Integer;
      AtEOF: boolean;
      // "Overflow" buffer (when we get more data than fits in the Data buffer
      FCarryBuffer: PChar;
      FCarryCapacity: Cardinal;
      FCarrySize: Cardinal;
      // Data buffer (provided by client - may be nil when no client yet...)
      FDataBuffer: PChar;
      FDataPos: Cardinal;
      FDataSize: Cardinal;
      FDataCapacity: Cardinal;
      FDataReady: Boolean;
      procedure EnsureCarryCapacity(value: Cardinal);
      procedure FetchCarry;
      procedure OnData(frame : PFLAC__FrameHeader;
                       buffer : PFLACChannels);
      procedure OnMetadata(metadata : PFLAC__StreamMetadata);
    protected
      function GetIsOpen: Boolean; override;
    public
      procedure Open(const AFileName: string); override;
      procedure Close; override;
      procedure SetFilePos(bytes: Cardinal); override;
      function GetFilePos: Cardinal; override;
      function Read(Buffer: PChar; Bytes: Cardinal): Cardinal; override;
    end;

    TAPEWaveFile = class(TWaveFile)
    private
      hAPEDecompress: APE_DECOMPRESS_HANDLE;
      FFilePos: Cardinal;
    protected
      function GetIsOpen: Boolean; override;
    public
      procedure Open(const AFileName: string); override;
      procedure Close; override;
      procedure SetFilePos(bytes: Cardinal); override;
      function GetFilePos: Cardinal; override;
      function Read(Buffer: PChar; Bytes: Cardinal): Cardinal; override;
    end;

    function OpenFile(const FileName: string): TWaveFile;

implementation


resourcestring
  NoRiffStr = 'Cannot open RIFF chunk';
  LockMemStr = 'Error occurred:'#10'%s'#10'while attempting to lock memory.';
  TotTimeStr = 'Total time';
  NoRiffStr2 = 'Cannot open RIFF chunk, file is not a valid WAVE file';
  HeaderStr = 'File header reports a size that is different than the system reports. ' +
        'Do you want to adjust the size? (Yes to override header, No to trust ' +
        'header, Cancel to abort)';
  NoDescrStr = 'No fmt descriptor.';
  LengthStr = 'Bad fmt length.';
  TruncStr = 'Truncated file.';
  NoAscStr = 'Cannot ascend out fmt.';
  OnlyPCMStr = 'Bad format - only PCM format supported.';
  Only8Or16Str = 'Bad format - CDWAV can only handle 8, 16 or 24-bit sampled files.';
  NoDescStr2 = 'Cannot descend into data part, code: ';
  TruncOKStr = 'File is truncated. OK to continue anyway?';
  ZeroSizeStr = 'The data header reports that the file has a size of ZERO. Press OK to ' +
        'continue and assume the rest of the file is WAVE data.';
  LowSizeStr = 'The data header reports that the file has a size of only %s' +
          ' bytes, which is suspiciously low. Assume that the rest is data?' +
          ' (Yes to correct, No if you trust the header)';
  NoSeekStr = 'Seek failed on WAV file.';
  NoPosStr = 'Error seeking current position in wave file.';
  NoReadStr = 'Failed to read from file';
  NoFlacMemory = 'Unable to open FLAC stream (no memory?)';
  NoFlacInit = 'FLAC Stream failed to initialize, cannot continue';
  NoExtStr = 'Unknown file extension: %s';
  FlacError = 'FLAC decoder failure: Code %d.';
  TooBigStr = 'File is to large, CD Wave can only handle files up to 4GB';

function OpenFile(const FileName: string): TWaveFile;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  if Ext = '.wav' then
    Result := TMMIOWaveFile.Create
  else if Ext = '.flac' then
    Result := TFLACWaveFile.Create
  else if Ext = '.ape' then
    Result := TAPEWaveFile.Create
  else if Ext = '.w64' then
    Result := TW64WaveFile.Create
  else
    raise Exception.CreateFmt(NoExtStr, [Ext]);
  Result.Open(FileName);
end;


// -----------------------------------------------------------------------------
destructor TWaveFile.Destroy;
begin
  try
    if IsOpen then Close;
  finally
    inherited;
  end;
end;

function TWaveFile.GetNativeWaveFormat;
begin
  Result := @FPCMWaveFormat;
end;

// -----------------------------------------------------------------------------

{procedure TMMIOWaveFile.Assign;
begin
  FWaveFile := AWaveFile;
  FWaveSize := AWaveSize;
  FWaveOffset := AWaveOffset;
  CopyWaveFormat(AWaveFormat, @FPCMWaveFormat);
end;}

procedure TMMIOWaveFile.Open;
var
  WaveFileSize: Cardinal;
  ckInRIFF, ckIn: TMMCKInfo;
  res: MMRESULT;
  Size: Cardinal;
  SectorSize: Cardinal;
  mmioInfo: TMMIOINFO;
  FWaveFile: HMMIO;
begin
  FillChar(mmioInfo, sizeof(TMMIOINFO), 0);
  mmioInfo.fccIOProc := FOURCC_DOS;
  {
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
  }
 hFile := CreateFile(
          PChar(AFileName),
          GENERIC_READ,
          FILE_SHARE_READ,
          nil,
          OPEN_EXISTING,
          FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,
          0);
 if hFile = INVALID_HANDLE_VALUE then
          raise EOSError.CreateFmt(
            NoOpenStr,
            [SysErrorMessage(GetLastError), AFileName]);
 try
  WaveFileSize := GetFileSize(hFile, nil);
  mmioInfo.adwInfo[0] := hFile;
  FWaveFile := mmioOpen(nil, @mmioInfo, MMIO_DENYWRITE or MMIO_READ);
  //OpenWaveFile(AFileName, @WaveFileSize, GENERIC_READ, OPEN_EXISTING);
  try
    ckInRIFF.fccType := mmioStringToFOURCC('WAVE', 0);
    ckInRIFF.ckSize := WaveFileSize - 8;
    ckInRIFF.dwFlags := 0;
    res := mmioDescend(FWaveFile, @ckInRIFF, nil, MMIO_FINDRIFF);
    if (res <> 0) and (res <> MMIOERR_INVALIDFILE) then
      raise Exception.Create(NoRiffStr2);
    if ckInRIFF.cksize + ckInRIFF.dwDataOffset <> WaveFileSize  then
    begin
      case MessageDlg(
        HeaderStr,
        mtWarning, mbYesNoCancel, 0) of
        mrYes: ckInRIFF.cksize := WaveFileSize - ckInRIFF.dwDataOffset;
        mrNo: ;
        else abort;
      end;
    end;
    if res = MMIOERR_INVALIDFILE then
    begin
      mmioSeek(FWaveFile, ckInRIFF.dwDataOffset + 4, SEEK_SET);
    end;
    FillChar(FPCMWaveFormat, SizeOf(FPCMWaveFormat), 0);
    ckIn.ckid := mmioStringToFOURCC('fmt ',0);
    if (ckInRIFF.cksize > $7FFFFFFF) then
    begin
      // Workaround 'invalid parameter' problems...
      Size := ckInRIFF.cksize;
      ckInRIFF.cksize := $10000;
      res := mmioDescend(FWaveFile, @ckIn, @ckInRIFF, MMIO_FINDCHUNK);
      ckInRIFF.cksize := Size;
    end else
      res := mmioDescend(FWaveFile, @ckIn, @ckInRIFF, MMIO_FINDCHUNK);
    if res <> 0 then
      raise Exception.Create(NoDescrStr);
    if ckIn.ckSize < sizeof(TWaveFormat) then
      raise Exception.Create(LengthStr);
    if ckIn.ckSize > sizeof(FPCMWaveFormat) then
        Size := sizeof(FPCMWaveFormat)
    else
        Size := ckIn.cksize;
    if mmioRead(FWaveFile, @FPCMWaveFormat, Size) <> integer(Size) then
      raise Exception.Create(TruncStr);
    if mmioAscend(FWaveFile, @ckIn, 0)<>0 then raise Exception.Create(NoAscStr);
    if not IsPCM(@FPCMWaveFormat.Format) then
      raise Exception.Create(OnlyPCMStr);
    if not (FPCMWaveFormat.Format.wBitsPerSample in [8,16,24]) then
      raise Exception.Create(Only8Or16Str);
    if (FPCMWaveFormat.Format.cbSize + sizeof(FPCMWaveFormat.Format)) > Size then
    begin
      if Size < sizeof(FPCMWaveFormat.Format) then
        FPCMWaveFormat.Format.cbSize := 0
      else
        FPCMWaveFormat.Format.cbSize := Size - sizeof(FPCMWaveFormat.Format);
      //MessageDlg('FMT size adjusted, header is corrupt', mtWarning, [mbOk], 0);
    end;
    // Fix the header if needed (e.g. SoundForge uses an invalid 24bit fmt)
    if (FPCMWaveFormat.Format.wBitsPerSample > 16) and
       (FPCMWaveFormat.Format.wFormatTag = WAVE_FORMAT_PCM) then
    begin
       FPCMWaveFormat.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
       FPCMWaveFormat.Format.cbSize := 22;
       FPCMWaveFormat.wSamples := FPCMWaveFormat.Format.wBitsPerSample;
       FPCMWaveFormat.SubFormat := WFE_GUID;
       FPCMWaveFormat.dwChannelMask := 0;
    end;
    SectorSize := (FPCMWaveFormat.Format.nAvgBytesPerSec div
                   (75 * FPCMWaveFormat.Format.nBlockAlign)) *
                  FPCMWaveFormat.Format.nBlockAlign;
    ckIn.ckid := mmioStringToFOURCC('data',0);
    if (ckInRIFF.cksize > $7FFFFFFF) then
    begin
      // Workaround 'invalid parameter' problems...
      Size := ckInRIFF.cksize;
      ckInRIFF.cksize := $10000;
      res := mmioDescend(FWaveFile, @ckIn, @ckInRIFF, MMIO_FINDCHUNK);
      ckInRIFF.cksize := Size;
    end else
      res := mmioDescend(FWaveFile, @ckIn, @ckInRIFF, MMIO_FINDCHUNK);
    if res <> 0 then
      raise Exception.Create(NoDescStr2 + IntToStr(res));
    FWaveSize := ckIn.ckSize;
    FWaveOffset := ckIn.dwDataOffset;
    if (WaveFileSize < FWaveSize + FWaveOffset) then
    begin
      if MessageDlg(
        TruncOKStr,
        mtWarning,
        mbOKCancel,
        0) <> mrOk then abort;
      FWaveSize := WaveFileSize - FWaveOffset;
    end;
    if (FWaveSize = 0) then
    begin
      if MessageDlg(
        ZeroSizeStr,
        mtWarning,
        mbOKCancel,
        0) <> mrOk then abort;
      FWaveSize := WaveFileSize - FWaveOffset;
      ckIn.ckSize := FWaveSize;
    end else if (FWaveSize < SectorSize) then // less than one sector?
    begin
      if WaveFileSize - FWaveOffset > SectorSize then
      begin
        case MessageDlg(
          Format(LowSizeStr, [IntToStr(WaveSize)]),
          mtWarning,
          mbYesNoCancel,
          0)
        of
          mrYes: begin
              FWaveSize := WaveFileSize - FWaveOffset;
              ckIn.ckSize := FWaveSize;
            end;
          mrCancel: abort;
        end;
      end;
    end;
  finally
    mmioClose(FWaveFile, MMIO_FHOPEN);
  end;
 except
    CloseHandle(hFile);
    hFile := 0;
    raise;
 end;
end;

procedure TMMIOWaveFile.Close;
begin
    CloseHandle(hFile);
    hFile := 0;
end;

function TMMIOWaveFile.GetIsOpen;
begin
  Result := hFile <> 0;
end;

procedure TMMIOWaveFile.SetFilePos;
var h: DWORD;
begin
  h := 0;
  if SetFilePointer(hFile, FWaveOffset + bytes, @h, FILE_BEGIN) = $FFFFFFFF then
    RaiseLastOSError;
end;

function TMMIOWaveFile.GetFilePos;
var h: DWORD;
begin
  h := 0;
  Result := SetFilePointer(hFile, 0, @h, FILE_CURRENT);
  if (Result = $FFFFFFFF) then
    RaiseLastOSError;
//    raise Exception.Create(NoPosStr);
  dec(Result, FWaveOffset);
end;

function TMMIOWaveFile.Read;
begin
  if not ReadFile(hFile, Buffer^, bytes, Result, nil) then
//  Result := mmioRead(FWaveFile, Buffer, Bytes);
//  if Result = Cardinal(-1) then
          raise EOSError.CreateFmt(
            NoReadStr,
            [SysErrorMessage(GetLastError)]);
end;

// -----------------------------------------------------------------------------

function fdReadCallback(decoder : PFLAC__StreamDecoder;
                        buffer : PFLAC__byte;
                        var bytes : LongWord;
                        client_data : Pointer) : Integer; cdecl;
var f: TFLACWaveFile;
begin
  f := client_data;
  if ReadFile(f.HFile, buffer^, bytes, bytes, nil) then
  begin
    f.AtEOF := bytes = 0;
    Result := FLAC__STREAM_DECODER_READ_STATUS_OK
  end else
    Result := FLAC__STREAM_DECODER_READ_STATUS_ERROR;
end;

function fdSeekCallback(decoder : PFLAC__StreamDecoder;
                        absolute_byte_offset : FLAC__uint64;
                        client_data : Pointer) : Integer; cdecl;
var f: TFLACWaveFile;
    h: DWORD;
begin
  f := client_data;
  // 64 - bit file sizes not fully supported here...
  h := Int64Rec(absolute_byte_offset).Hi;
  if SetFilePointer(f.HFile, absolute_byte_offset, @h, FILE_BEGIN) = $FFFFFFFF then
    Result := FLAC__STREAM_DECODER_SEEK_STATUS_ERROR
  else
    Result := FLAC__STREAM_DECODER_SEEK_STATUS_OK
end;

function fdTellCallback(decoder : PFLAC__StreamDecoder;
                        var absolute_byte_offset : FLAC__uint64;
                        client_data : Pointer) : Integer; cdecl;
var f: TFLACWaveFile;
    r, h: Cardinal;
begin
  f := client_data;
  h := 0;
  // 64 - bit file sizes not fully supported here...
  r := SetFilePointer(f.HFile, 0, @h, FILE_CURRENT);
  if (r = $FFFFFFFF) and (GetLastError <> NO_ERROR) then
    Result := FLAC__STREAM_DECODER_TELL_STATUS_ERROR
  else begin
    Result := FLAC__STREAM_DECODER_TELL_STATUS_OK;
    Int64Rec(absolute_byte_offset).Lo := r;
    Int64Rec(absolute_byte_offset).Hi := h;
  end;
end;

function fdLengthCallback(decoder : PFLAC__StreamDecoder;
                          var stream_length : FLAC__uint64;
                          client_data : Pointer) : Integer; cdecl;
var f: TFLACWaveFile;
    r: Cardinal;
begin
  f := client_data;
  // 64 - bit file sizes not fully supported here...
  r := GetFileSize(f.Hfile, @Int64Rec(stream_length).Hi);
  if (r = $FFFFFFFF) and (GetLastError <> NO_ERROR) then
    Result := FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR
  else begin
    Result := FLAC__STREAM_DECODER_LENGTH_STATUS_OK ;
    Int64Rec(stream_length).Lo := r;
  end;
end;

function fdEofCallback(decoder : PFLAC__StreamDecoder;
                       client_data : Pointer) : FLAC__bool; cdecl;
begin
  Result := TFlacWaveFile(client_data).AtEOF;
end;

function fdWriteCallback(decoder : PFLAC__StreamDecoder;
                         frame : PFLAC__FrameHeader;
                         buffer : PFLACChannels;
                         client_data : Pointer) : Integer; cdecl;
begin
  // Do something intelligent here...
  TFlacWaveFile(client_data).OnData(frame, buffer);
  Result := FLAC__STREAM_DECODER_READ_STATUS_OK;
end;

procedure fdMetadataCallback(decoder : PFLAC__StreamDecoder;
                             metadata : PFLAC__StreamMetadata;
                             client_data : Pointer); cdecl;
begin
  // Do something intelligent here...
  if metadata.metadataType = 0 then
    TFlacWaveFile(client_data).OnMetadata(metadata);
end;


procedure fdErrorCallback(decoder : PFLAC__StreamDecoder;
                          status : Integer;
                          client_data : Pointer); cdecl;
begin
  TFlacWaveFile(client_data).FStatus := status;
end;

procedure TFLACWaveFile.Open;
var
  err: Cardinal;
begin
  HFile := CreateFile(
          PChar(AFileName),
          GENERIC_READ,
          FILE_SHARE_READ,
          nil,
          OPEN_EXISTING,
          FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,
          0);
  if HFile = INVALID_HANDLE_VALUE then
    raise EOSError.CreateFmt(
            NoOpenStr,
            [SysErrorMessage(GetLastError), AFileName]);
  FStream := FLAC__stream_decoder_new;
  if FStream = nil then
  begin
    CloseHandle(HFile);
    raise Exception.Create(NoFlacMemory);
  end;
  try
    err := FLAC__stream_decoder_init_stream(FStream, fdReadCallback,
                fdSeekCallback, fdTellCallback, fdLengthCallback, fdEofCallback,
                fdWriteCallback, fdMetadataCallback, fdErrorCallback, self);
    if err <> 0 then
        raise Exception.Create(NoFlacInit + ': ' + IntToStr(err));
    FLAC__stream_decoder_process_until_end_of_metadata(FStream);
    // Decode first block te fetch the format stuff
    //while FPCMWaveFormat.Format.wBitsPerSample = 0 do
    //  FLAC__stream_decoder_process_single(FStream);
    if not FLAC__stream_decoder_seek_absolute(FStream, 0) then
      raise Exception.CreateFmt(FlacError, [FLAC__stream_decoder_get_stream_decoder_state(FStream)]);
    {if not FLAC__stream_decoder_process_until_end_of_stream(FStream) then
      raise Exception.Create('Process until end failed: ' + IntToStr(FLAC__stream_decoder_get_stream_decoder_state(FStream)));}
  except
    // clean it all up, return to initial state on error
    FLAC__stream_decoder_delete(FStream);
    FStream := nil;
    CloseHandle(HFile);
    raise;
  end;
end;

procedure TFLACWaveFile.Close;
begin
  FLAC__stream_decoder_finish(FStream);
  FLAC__stream_decoder_delete(FStream);
  FStream := nil;
  CloseHandle(HFile);
  HFile := 0;
  if FCarryBuffer <> nil then begin
    GlobalFree(Cardinal(FCarryBuffer));
    FCarryBuffer := nil;
  end;
end;

function TFLACWaveFile.GetIsOpen;
begin
  Result := FStream <> nil;
end;

procedure TFLACWaveFile.SetFilePos;
begin
  AtEOF := False;
  FCarrySize := 0;
  if not
   //(FLAC__stream_decoder_reset(FStream) and
   FLAC__stream_decoder_seek_absolute(FStream, bytes div FPCMWaveformat.Format.nBlockAlign) then
     raise Exception.CreateFmt(FlacError, [FLAC__stream_decoder_get_stream_decoder_state(FStream)]);
end;

function TFLACWaveFile.GetFilePos;
var pos: FLAC__uint64;
begin
  FLAC__stream_decoder_get_decode_position(FStream, pos);
  Result := Pos * FPCMWaveFormat.Format.nBlockAlign;
end;

function TFLACWaveFile.Read;
var res: Integer;
begin
  FDataBuffer := Buffer;
  FDataSize := 0;
  FDataCapacity := Bytes;
  FDataPos := 0;
  FDataReady := False;
  FetchCarry;
  res := 0;
  while (FDataSize < bytes) and
        (res < FLAC__STREAM_DECODER_END_OF_STREAM) and
        FLAC__stream_decoder_process_single(FStream) do
        res := FLAC__stream_decoder_get_state(FStream);
  if (res > FLAC__STREAM_DECODER_END_OF_STREAM) then
    raise Exception.Create('FLAC decoder failure: ' + IntToStr(res));
  FDataBuffer := nil;
  FDataCapacity := 0;
  result := FDataSize;
end;

procedure TFLACWaveFile.EnsureCarryCapacity;
begin
  if value > FCarryCapacity then
  begin
    if FCarryBuffer = nil then
      FCarryBuffer := Pointer(GlobalAlloc(GMEM_FIXED, value))
    else
      FCarryBuffer := Pointer(GlobalReAlloc(Cardinal(FCarryBuffer), value, GMEM_MOVEABLE));
    if FCarryBuffer = nil then
      RaiseLastOSError;
    FCarryCapacity := Value;
  end;
end;

procedure TFLACWaveFile.FetchCarry;
var Size: Cardinal;
begin
  Size := FCarrySize;
  if Size > 0 then
  begin
    if FDataPos + Size > FDataCapacity then
    begin
      Size := FDataCapacity - FDataPos;
      move(FCarryBuffer^, FDataBuffer[FDataPos], Size);
      dec(FCarrySize, Size);
      move(FCarryBuffer[Size], FCarryBuffer[0], FCarrySize);
    end else begin
      move(FCarryBuffer^, FDataBuffer[FDataPos], Size);
      FCarrySize := 0;
    end;
    inc(FDataPos, Size);
    inc(FDataSize, Size);
  end;
end;

function MergeSamples(src: PFLACChannels; dst: PChar; nSamples: Cardinal;
                      frame: PFLAC__FrameHeader; offset: Cardinal): Cardinal;
var i, c, j: Cardinal;
begin
  j := 0;
  case frame.bits_per_sample of
    0..8:   begin
              for i := offset to offset+nSamples-1 do
              for c := 0 to frame.channels-1 do
              begin
                PArray8(dst)[j] := src[c][i] + $80;
                inc(j);
              end;
              Result := nSamples * frame.channels;
            end;
    9..16:  begin
              for i := offset to offset+nSamples-1 do
              for c := 0 to frame.channels-1 do
              begin
                PArray16(dst)[j] := src[c][i];
                inc(j);
              end;
              Result := nSamples * frame.channels * 2;
            end;
    17..24: begin
              for i := offset to offset+nSamples-1 do
              for c := 0 to frame.channels-1 do
              begin
                PArray24(dst)[j] := PThreeByte(@src[c][i])^;
                inc(j);
              end;
              Result := nSamples * frame.channels * 3;
            end;
    else    begin
              for i := offset to offset+nSamples-1 do
              for c := 0 to frame.channels-1 do
              begin
                PArray32(dst)[j] := src[c][i];
                inc(j);
              end;
              Result := nSamples * frame.channels * 4;
            end;
  end;
end;

procedure TFLACWaveFile.OnData;
var
  size, samples: Cardinal;
begin
   if FDataBuffer = nil then
   begin
     FCarrySize := MergeSamples(Buffer, FCarryBuffer, frame.blocksize, frame, 0);
     Exit;
   end;
   size := frame.blocksize;
   if (FDataPos + (size * PCMWaveFormat.nBlockAlign)) > FDataCapacity then
   begin
     // copy partial, remainder to 'carry' buffer
     samples := (FDataCapacity - FDataPos) div PCMWaveFormat.nBlockAlign;
     if samples <> 0 then
       size := MergeSamples(Buffer, @FDataBuffer[FDataPos], samples, frame, 0)
     else
       size := 0;
     FCarrySize := MergeSamples(Buffer, FCarryBuffer, frame.blocksize-samples, frame, samples);
   end else begin
     // copy all to data buffer
     size := MergeSamples(Buffer, @FDataBuffer[FDataPos], size, frame, 0);
   end;
   inc(FDataPos, size);
   inc(FDataSize, size);
end;

procedure TFLACWaveFile.OnMetadata;
begin
  with metadata.data do
  begin
    CreatePCMWaveFormat(FPCMWaveFormat,
      sample_rate,
      channels,
      bits_per_sample);
    FWaveSize :=
      total_samples * FPCMWaveFormat.Format.nBlockAlign;
    EnsureCarryCapacity(max_blocksize * FPCMWaveFormat.Format.nBlockAlign);
  end;
end;

{ TAPEWaveFile }

function TAPEWaveFile.GetIsOpen: Boolean;
begin
  Result := hAPEDecompress <> nil;
end;

procedure TAPEWaveFile.Open(const AFileName: string);
var
  code: Integer;
  rate, channels, bits: Integer;
begin
  APELoadDLL;
  if not LibAPELoaded then
      raise Exception.Create('Cannot open APE files - unable to open MACDll.dll.');
  code := 0;
  hAPEDecompress := APEDecompress_Create(PChar(AFileName), @code);
  if (hAPEDecompress = nil) then
      raise Exception.CreateFmt('Failed to open APE file, error code: %d.', [code]);
  channels := APEDecompress_GetInfo(hAPEDecompress, APE_INFO_CHANNELS, 0, 0);
  bits := APEDecompress_GetInfo(hAPEDecompress,  APE_INFO_BITS_PER_SAMPLE, 0, 0);
  rate := APEDecompress_GetInfo(hAPEDecompress, APE_INFO_SAMPLE_RATE, 0, 0);
  CreatePCMWaveFormat(FPCMWaveFormat, rate, channels, bits);
  FWaveSize := APEDecompress_GetInfo(hAPEDecompress, APE_DECOMPRESS_TOTAL_BLOCKS, 0, 0)
    * FPCMWaveFormat.Format.nBlockAlign;
end;

procedure TAPEWaveFile.Close;
begin
  APEDecompress_Destroy(hAPEDecompress);
  hAPEDecompress := nil;
end;

function TAPEWaveFile.GetFilePos: Cardinal;
begin
  Result := FFilePos;
//  OutputDebugString(PChar('TAPEWaveFile.GetFilePos returns: ' + IntToStr(Result)));
//  raise Exception.Create('TAPEWaveFile.GetFilePos - Not implemented yet');
end;

function TAPEWaveFile.Read(Buffer: PChar; Bytes: Cardinal): Cardinal;
var BlocksRead: Integer;
begin
  APECall(APEDecompress_GetData(hAPEDecompress, Buffer, Bytes div FPCMWaveFormat.Format.nBlockAlign, BlocksRead));
  Result := BlocksRead * FPCMWaveFormat.Format.nBlockAlign;
  inc(FFilePos, Result);
end;

procedure TAPEWaveFile.SetFilePos(bytes: Cardinal);
begin
//  OutputDebugString(PChar('TAPEWaveFile.SetFilePos to: ' + IntToStr(bytes)));
  if FFilePos <> bytes then
  begin
    APECall(APEDecompress_Seek(hAPEDecompress, Bytes div FPCMWaveFormat.Format.nBlockAlign));
    FFilePos := bytes;
  end;
end;

{------------------------------------------------------------------------------}
const
  CKGUID_RIFF: TGUID = '{66666972-912E-11CF-A5D6-28DB04C10000}';
  CKGUID_LIST: TGUID = '{7473696C-912F-11CF-A5D6-28DB04C10000}';
  CKGUID_WAVE: TGUID = '{65766177-ACF3-11D3-8CD1-00C04F8EDB8A}';
  CKGUID_FMT : TGUID = '{20746D66-ACF3-11D3-8CD1-00C04F8EDB8A}';
  CKGUID_FACT: TGUID = '{74636166-ACF3-11D3-8CD1-00C04F8EDB8A}';
  CKGUID_DATA: TGUID = '{61746164-ACF3-11D3-8CD1-00C04F8EDB8A}';
  CKGUID_LEVL: TGUID = '{6C76656C-ACF3-11D3-8CD1-00C04F8EDB8A}';
  CKGUID_JUNK: TGUID = '{6b6E756A-ACF3-11D3-8CD1-00C04f8EDB8A}';
  CKGUID_BEXT: TGUID = '{74786562-ACF3-11D3-8CD1-00C04F8EDB8A}';
  //MARKER 	{ ABF76256-392D-11D2-86C7-00C04F8EDB8A }
  //SUMMARYLIST 	{ 925F94BC-525A-11D2-86DC-00C04F8EDB8A }

type
  TW64RIFF = packed record
    riff: TGUID;
    size: Int64;
    filetype: TGUID;
  end;
  TW64CHUNK = packed record
    fourcc: TGUID;
    size:   Int64;
  end;

procedure TW64WaveFile.Open;
var
  WaveFileSize: Int64;
  Size: DWORD;
  ChunkSize, ChunkDataSize: Int64;
  RiffHead: TW64RIFF;
  RiffChunk: TW64CHUNK;
  GotData, GotFmt: Boolean;
  h: DWORD;
begin
  hFile := CreateFile(
          PChar(AFileName),
          GENERIC_READ,
          FILE_SHARE_READ,
          nil,
          OPEN_EXISTING,
          FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,
          0);
  if hFile = INVALID_HANDLE_VALUE then
          raise EOSError.CreateFmt(
            NoOpenStr,
            [SysErrorMessage(GetLastError), AFileName]);
  try
    Int64Rec(WaveFileSize).Lo := GetFileSize(hFile, @Int64Rec(WaveFileSize).Hi);
    if Read(@RiffHead, sizeof(RiffHead)) < sizeof(RiffHead) then
        raise Exception.Create(TruncStr);
    if not CompareMem(@RiffHead.riff, @CKGUID_RIFF, sizeof(TGUID)) then
        raise Exception.Create(NoRiffStr2);
    if not CompareMem(@RiffHead.filetype, @CKGUID_WAVE, sizeof(TGUID)) then
        raise Exception.Create(NoRiffStr2);
    GotData := False;
    GotFmt := False;
    repeat
      // Read chunk
      if Read(@RiffChunk, sizeof(RiffChunk)) < sizeof(RiffChunk) then
        raise Exception.Create(TruncStr);
      // The w64 header size includes the chunk header (24 bytes)
      ChunkDataSize := RiffChunk.size - sizeof(RiffChunk);
      // Round size up to 8-byte align (padding)
      ChunkSize := (((ChunkDataSize - 1) shr 3) + 1) shl 3;
      // Determine type, and do something with the useful ones.
      if CompareMem(@RiffChunk.fourcc, @CKGUID_FMT, sizeof(TGUID)) then
      begin
        // FMT : Wave format
        if ChunkDataSize < sizeof(TWaveFormat) then
          raise Exception.Create(LengthStr);
        if ChunkDataSize > sizeof(FPCMWaveFormat) then
          Size := sizeof(FPCMWaveFormat)
        else
          Size := ChunkDataSize;
        if Read(@FPCMWaveFormat, Size) <> Size then
          raise Exception.Create(TruncStr);
        if not IsPCM(@FPCMWaveFormat.Format) then
          raise Exception.Create(OnlyPCMStr);
        if not (FPCMWaveFormat.Format.wBitsPerSample in [8,16,24]) then
          raise Exception.Create(Only8Or16Str);
        // Fix the header if needed (e.g. SoundForge uses an invalid 24bit fmt)
        if (FPCMWaveFormat.Format.wBitsPerSample > 16) and
          (FPCMWaveFormat.Format.wFormatTag = WAVE_FORMAT_PCM) then
        begin
          FPCMWaveFormat.Format.wFormatTag := WAVE_FORMAT_EXTENSIBLE;
          FPCMWaveFormat.Format.cbSize := 22;
          FPCMWaveFormat.wSamples := FPCMWaveFormat.Format.wBitsPerSample;
          FPCMWaveFormat.SubFormat := WFE_GUID;
          FPCMWaveFormat.dwChannelMask := 0;
        end;
        if ChunkSize > Size then
          // Skip padding etc.
          SetFilePointer(hFile, ChunkSize - Size, nil, FILE_CURRENT);
        GotFmt := True;
      end
      else if CompareMem(@RiffChunk.fourcc, @CKGUID_DATA, sizeof(TGUID)) then
      begin
        GotData := True;
        FWaveOffset :=  SetFilePointer(hFile, 0, @h, FILE_CURRENT);
        if (FWaveOffset = $FFFFFFFF) then
            RaiseLastOSError;
        if ChunkDataSize > $FFFFFFFE then
            raise Exception.Create(TooBigStr);
        FWaveSize := ChunkDataSize;
      end else
      begin
        // Unkown chunk, skip over it.
        SetFilePointer(hfile, Int64Rec(ChunkSize).Lo, @Int64Rec(ChunkSize).Hi, FILE_CURRENT);
      end;
    until GotFmt and GotData;
    SetFilePos(0);
  except
    Close;
    raise;
  end;
end;

procedure TW64WaveFile.Close;
begin
  CloseHandle(hFile);
  hFile := 0;
end;

function TW64WaveFile.GetIsOpen;
begin
  Result := hFile <> 0;
end;

procedure TW64WaveFile.SetFilePos;
var h: DWORD;
begin
  h := 0;
  if SetFilePointer(hFile, FWaveOffset + bytes, @h, FILE_BEGIN) = $FFFFFFFF then
    RaiseLastOSError;
end;

function TW64WaveFile.GetFilePos;
var h: DWORD;
begin
  h := 0;
  Result := SetFilePointer(hFile, 0, @h, FILE_CURRENT);
  if (Result = $FFFFFFFF) then
    RaiseLastOSError;
//    raise Exception.Create(NoPosStr);
  dec(Result, FWaveOffset);
end;

function TW64WaveFile.Read;
begin
  if not ReadFile(hFile, Buffer^, bytes, Result, nil) then
    raise EOSError.CreateFmt(
            NoReadStr,
            [SysErrorMessage(GetLastError)]);
end;


end.

