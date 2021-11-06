unit APE;

interface

uses Forms, Windows, Sysutils, MMSystem;

const WAVE_FORMAT_APE = $FFF4;

type
  TAPEWaveFormat = packed record
    WaveFormat: TWaveFormatEx;
    Level: Integer;
  end;
  PAPEWaveFormat = ^TAPEWaveFormat;


type
  APE_COMPRESS_HANDLE = pointer;
  APE_DECOMPRESS_HANDLE = pointer;
  APE_DECOMPRESS_FIELDS = Integer;

const
  LibAPEPath = 'MACDll.dll';
    APE_INFO_FILE_VERSION = 1000;               // version of the APE file * 1000 (3.93 = 3930) [ignored, ignored]
    APE_INFO_COMPRESSION_LEVEL = 1001;          // compression level of the APE file [ignored, ignored]
    APE_INFO_FORMAT_FLAGS = 1002;               // format flags of the APE file [ignored, ignored]
    APE_INFO_SAMPLE_RATE = 1003;                // sample rate (Hz) [ignored, ignored]
    APE_INFO_BITS_PER_SAMPLE = 1004;            // bits per sample [ignored, ignored]
    APE_INFO_BYTES_PER_SAMPLE = 1005;           // number of bytes per sample [ignored, ignored]
    APE_INFO_CHANNELS = 1006;                   // channels [ignored, ignored]
    APE_INFO_BLOCK_ALIGN = 1007;                // block alignment [ignored, ignored]
    APE_INFO_BLOCKS_PER_FRAME = 1008;           // number of blocks in a frame (frames are used internally)  [ignored, ignored]
    APE_INFO_FINAL_FRAME_BLOCKS = 1009;         // blocks in the final frame (frames are used internally) [ignored, ignored]
    APE_INFO_TOTAL_FRAMES = 1010;               // total number frames (frames are used internally) [ignored, ignored]
    APE_INFO_WAV_HEADER_BYTES = 1011;           // header bytes of the decompressed WAV [ignored, ignored]
    APE_INFO_WAV_TERMINATING_BYTES = 1012;      // terminating bytes of the decompressed WAV [ignored, ignored]
    APE_INFO_WAV_DATA_BYTES = 1013;             // data bytes of the decompressed WAV [ignored, ignored]
    APE_INFO_WAV_TOTAL_BYTES = 1014;            // total bytes of the decompressed WAV [ignored, ignored]
    APE_INFO_APE_TOTAL_BYTES = 1015;            // total bytes of the APE file [ignored, ignored]
    APE_INFO_TOTAL_BLOCKS = 1016;               // total blocks of audio data [ignored, ignored]
    APE_INFO_LENGTH_MS = 1017;                  // length in ms (1 sec = 1000 ms) [ignored, ignored]
    APE_INFO_AVERAGE_BITRATE = 1018;            // average bitrate of the APE [ignored, ignored]
    APE_INFO_FRAME_BITRATE = 1019;              // bitrate of specified APE frame [frame index, ignored]
    APE_INFO_DECOMPRESSED_BITRATE = 1020;       // bitrate of the decompressed WAV [ignored, ignored]
    APE_INFO_PEAK_LEVEL = 1021;                 // peak audio level (obsolete) (-1 is unknown) [ignored, ignored]
    APE_INFO_SEEK_BIT = 1022;                   // bit offset [frame index, ignored]
    APE_INFO_SEEK_BYTE = 1023;                  // byte offset [frame index, ignored]
    APE_INFO_WAV_HEADER_DATA = 1024;            // error code [buffer *, max bytes]
    APE_INFO_WAV_TERMINATING_DATA = 1025;       // error code [buffer *, max bytes]
    APE_INFO_WAVEFORMATEX = 1026;               // error code [waveformatex *, ignored]
    APE_INFO_IO_SOURCE = 1027;                  // I/O source (CIO *) [ignored, ignored]
    APE_INFO_FRAME_BYTES = 1028;                // bytes (compressed) of the frame [frame index, ignored]
    APE_INFO_FRAME_BLOCKS = 1029;               // blocks in a given frame [frame index, ignored]
    APE_INFO_TAG = 1030;                        // point to tag (CAPETag *) [ignored, ignored]

    APE_DECOMPRESS_CURRENT_BLOCK = 2000;        // current block location [ignored, ignored]
    APE_DECOMPRESS_CURRENT_MS = 2001;           // current millisecond location [ignored, ignored]
    APE_DECOMPRESS_TOTAL_BLOCKS = 2002;         // total blocks in the decompressors range [ignored, ignored]
    APE_DECOMPRESS_LENGTH_MS = 2003;            // total blocks in the decompressors range [ignored, ignored]
    APE_DECOMPRESS_CURRENT_BITRATE = 2004;      // current bitrate [ignored, ignored]
    APE_DECOMPRESS_AVERAGE_BITRATE = 2005;      // average bitrate (works with ranges) [ignored, ignored]

    APE_INTERNAL_INFO = 3000;                   // for internal use -- don't use (returns APE_FILE_INFO *) [ignored, ignored]

    APE_COMPRESSION_LEVEL_FAST =         1000;
    APE_COMPRESSION_LEVEL_NORMAL =       2000;
    APE_COMPRESSION_LEVEL_HIGH =         3000;
    APE_COMPRESSION_LEVEL_EXTRA_HIGH =   4000;
    APE_COMPRESSION_LEVEL_INSANE =       5000;

    MAC_FORMAT_FLAG_8_BIT              =   1;    // is 8-bit [OBSOLETE]
    MAC_FORMAT_FLAG_CRC                =   2;    // uses the new CRC32 error detection [OBSOLETE]
    MAC_FORMAT_FLAG_HAS_PEAK_LEVEL     =   4;    // uint32 nPeakLevel after the header [OBSOLETE]
    MAC_FORMAT_FLAG_24_BIT             =   8;    // is 24-bit [OBSOLETE]
    MAC_FORMAT_FLAG_HAS_SEEK_ELEMENTS  =  16;    // has the number of seek elements after the peak level
    MAC_FORMAT_FLAG_CREATE_WAV_HEADER  =  32;    // create the wave header on decompression (not stored)

    CREATE_WAV_HEADER_ON_DECOMPRESSION = -1;
    MAX_AUDIO_BYTES_UNKNOWN  = -1;

var
  LibAPELoaded: Boolean = false;
  APEGetVersionNumber: function: Integer; stdcall;
  APEGetInterfaceCompatibility: function (nVersion: Integer; bDisplayWarningsOnFailure: LongBool = true; hwndParent: HWND = 0): Integer; stdcall;
  APEShowFileInfoDialog: function (const pFilename: PChar; hwndWindow: HWND): Integer; stdcall;
  APETagFileSimple: function(const pFilename, pArtist, pAlbum, pTitle, pComment, pGenre, pYear, pTrack: PChar; bClearFirst: LongBool; bUseOldID3: LongBool): Integer; stdcall;
//  APEGetID3Tag(const pFilename: PChar; var ID3Tag: TID3_TAG);
  APERemoveTag: function(const pFilename: PChar): Integer; stdcall;

  APECompress_Create: function(Errorcode: PInteger = nil): APE_COMPRESS_HANDLE; stdcall;
  APECompress_Destroy: procedure(hAPECompress: APE_COMPRESS_HANDLE); stdcall;
  APECompress_Start: function(hAPECompress: APE_COMPRESS_HANDLE; const pOutputFilename: PChar; const pwfeInput: PWAVEFORMATEX; nMaxAudioBytes:Integer = MAX_AUDIO_BYTES_UNKNOWN; nCompressionLevel:Integer = APE_COMPRESSION_LEVEL_NORMAL; const pHeaderData: PChar = nil; nHeaderBytes: Integer = CREATE_WAV_HEADER_ON_DECOMPRESSION): Integer; stdcall;
  APECompress_AddData: function(hAPECompress: APE_COMPRESS_HANDLE; pData: PChar; nBytes: Integer): Integer; stdcall;
  APECompress_GetBufferBytesAvailable: function(hAPECompress: APE_COMPRESS_HANDLE): Integer; stdcall;
  APECompress_LockBuffer: function(hAPECompress: APE_COMPRESS_HANDLE; var BytesAvailable: Integer): PChar; stdcall;
  APECompress_UnlockBuffer: function(hAPECompress: APE_COMPRESS_HANDLE; nBytesAdded: Integer; bProcess: LongBool = TRUE): Integer; stdcall;
  APECompress_Finish: function(hAPECompress: APE_COMPRESS_HANDLE; pTerminatingData: PChar; nTerminatingBytes: Integer; nWAVTerminatingBytes: Integer): Integer; stdcall;
  APECompress_Kill: function(hAPECompress: APE_COMPRESS_HANDLE): Integer; stdcall;

  APEDecompress_Create: function(FileName: PChar; ErrorCode: PInteger): APE_DECOMPRESS_HANDLE; stdcall;
  APEDecompress_Destroy: procedure(hAPEDecompress: APE_DECOMPRESS_HANDLE); stdcall;
  APEDecompress_GetData:  function(hAPEDecompress: APE_DECOMPRESS_HANDLE; Buffer: Pointer; nBlocks: Integer; var nBlocksRetrieved: Integer): Integer; stdcall;
  APEDecompress_Seek: function(hAPEDecompress: APE_DECOMPRESS_HANDLE; nBlockOffset: Integer): Integer; stdcall;
  APEDecompress_GetInfo: function(hAPEDecompress: APE_DECOMPRESS_HANDLE; Field: APE_DECOMPRESS_FIELDS;  nParam1, nParam2: Integer): Integer; stdcall;

procedure APECall(res: Integer);
procedure APELoadDLL();

implementation

var
  Libhandle: HMODULE = 0;

procedure APECall(res: Integer);
begin
  if res <> 0 then
    Raise Exception.CreateFmt('APE function call failed, error code: %d.', [res]);
end;

procedure APELoadDLL;
begin
  if LibAPELoaded then
    Exit;
  try
  Libhandle := LoadLibraryEx(LibAPEPath, 0, 0);
  if Libhandle <> 0 then
  begin
    LibAPELoaded := True;
    APEGetVersionNumber := GetProcAddress(Libhandle, 'GetVersionNumber');
    APEGetInterfaceCompatibility := GetProcAddress(Libhandle, 'GetInterfaceCompatibility');
    APEShowFileInfoDialog := GetProcAddress(Libhandle, 'ShowFileInfoDialog');
    APETagFileSimple := GetProcAddress(Libhandle, 'TagFileSimple');
    APERemoveTag := GetProcAddress(Libhandle, 'RemoveTag');

    APECompress_Create := GetProcAddress(Libhandle, 'c_APECompress_Create');
    APECompress_Destroy:= GetProcAddress(Libhandle, 'c_APECompress_Destroy');
    APECompress_Start:= GetProcAddress(Libhandle, 'c_APECompress_Start');
    APECompress_AddData:= GetProcAddress(Libhandle, 'c_APECompress_AddData');
    APECompress_GetBufferBytesAvailable:= GetProcAddress(Libhandle, 'c_APECompress_GetBufferBytesAvailable');
    APECompress_LockBuffer:= GetProcAddress(Libhandle, 'c_APECompress_LockBuffer');
    APECompress_UnlockBuffer:= GetProcAddress(Libhandle, 'c_APECompress_UnlockBuffer');
    APECompress_Finish:= GetProcAddress(Libhandle, 'c_APECompress_Finish');
    APECompress_Kill:= GetProcAddress(Libhandle, 'c_APECompress_Kill');

    APEDecompress_Create := GetProcAddress(Libhandle, 'c_APEDecompress_Create');
    APEDecompress_Destroy := GetProcAddress(Libhandle, 'c_APEDecompress_Destroy');
    APEDecompress_GetData := GetProcAddress(Libhandle, 'c_APEDecompress_GetData');
    APEDecompress_Seek := GetProcAddress(Libhandle, 'c_APEDecompress_Seek');
    APEDecompress_GetInfo := GetProcAddress(Libhandle, 'c_APEDecompress_GetInfo');
    if @APEDecompress_GetInfo = nil then
      LibAPELoaded := False;
  end;
  except
    on e: Exception do
    begin
      LibAPELoaded := False;
      Application.MessageBox(PChar(e.Message), 'Error loading MAC (APE) library');
    end;
  end;
end;

initialization

finalization

  if Libhandle <> 0 then FreeLibrary(Libhandle);

end.
