unit lame_enc;

// a "just a bit more than what I need" convertion of the lame_enc.dll header

interface

uses windows, mmsystem;

const
        WAVE_FORMAT_LAME_MP3 = $FFFF;
	BE_CONFIG_LAME = 256;

	VBR_METHOD_NONE			= -1;
	VBR_METHOD_DEFAULT		=  0;
	VBR_METHOD_OLD			=  1;
	VBR_METHOD_NEW			=  2;
	VBR_METHOD_MTRH			=  3;
	VBR_METHOD_ABR			=  4;

	BE_MP3_MODE_STEREO	=	0;
	BE_MP3_MODE_JSTEREO	 =	1;
	BE_MP3_MODE_DUALCHANNEL=	2;
	BE_MP3_MODE_MONO=		3;

	// QUALITY PRESETS
	LQP_NOPRESET=-1;
	LQP_NORMAL_QUALITY=0;
	LQP_LOW_QUALITY=1;
	LQP_HIGH_QUALITY=2;
	LQP_VOICE_QUALITY=3;
	LQP_R3MIX_QUALITY=4;
	LQP_VERYHIGH_QUALITY=5;
	// NEW QUALITY PRESETS (3.93 and up)
	LQP_STANDARD	       	= 6;
	LQP_FAST_STANDARD	= 7;
	LQP_EXTREME		= 8;
	LQP_FAST_EXTREME	= 9;
	LQP_INSANE		= 10;
	LQP_ABR			= 11;
	LQP_CBR			= 12;


	// NEW (?) PRESET VALUES
	LQP_PHONE	=1000;
	LQP_SW		=2000;
	LQP_AM		=3000;
	LQP_FM		=4000;
	LQP_VOICE	=5000;
	LQP_RADIO	=6000;
	LQP_TAPE	=7000;
	LQP_HIFI	=8000;
	LQP_CD		=9000;
	LQP_STUDIO	=10000;

	MPEG1	= 1;
	MPEG2	= 0;

	BE_MAX_HOMEPAGE	= 128;

	BE_ERR_SUCCESSFUL =                $00000000;
	BE_ERR_INVALID_FORMAT =            $00000001;
	BE_ERR_INVALID_FORMAT_PARAMETERS = $00000002;
	BE_ERR_NO_MORE_HANDLES =           $00000003;
	BE_ERR_INVALID_HANDLE =		   $00000004;
	BE_ERR_BUFFER_TOO_SMALL =      	   $00000005;

type
	BE_ERR = DWORD;

	HBE_STREAM = DWORD;
        PHBE_STREAM = ^HBE_STREAM;


	VBRMETHOD = LongInt;

	TMP3 = packed record
		dwSampleRate:	DWORD; // 48000, 44100 and 32000 allowed
		byMode: 	BYTE;  // BE_MP3_MODE_STEREO, BE_MP3_MODE_DUALCHANNEL, BE_MP3_MODE_MONO
		wBitrate:	WORD;  // 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256 and 320 allowed
		bPrivate:	BOOL;
		bCRC:	        BOOL;
		bCopyright:	BOOL;
		bOriginal:	BOOL;
        end;

	TLHV1 = packed record
			// STRUCTURE INFORMATION
			dwStructVersion: DWORD;
			dwStructSize: DWORD;

			// BASIC ENCODER SETTINGS
			dwSampleRate: DWORD;	// SAMPLERATE OF INPUT FILE
			dwReSampleRate: DWORD;	// DOWNSAMPLERATE, 0=ENCODER DECIDES
			nMode: LongInt;	        // BE_MP3_MODE_STEREO, BE_MP3_MODE_DUALCHANNEL, BE_MP3_MODE_MONO
			dwBitrate: DWORD;	// CBR bitrate, VBR min bitrate
			dwMaxBitrate: DWORD;	// CBR ignored, VBR Max bitrate
			nPreset: LongInt; 	// Quality preset, use one of the settings of the LAME_QUALITY_PRESET enum
			dwMpegVersion: DWORD;	// FUTURE USE, MPEG-1 OR MPEG-2
			dwPsyModel: DWORD;      // FUTURE USE, SET TO 0
			dwEmphasis: DWORD;	// FUTURE USE, SET TO 0

			// BIT STREAM SETTINGS
			bPrivate: BOOL;		// Set Private Bit (TRUE/FALSE)
			bCRC: BOOL;	  	// Insert CRC (TRUE/FALSE)
			bCopyright: BOOL; 	// Set Copyright Bit (TRUE/FALSE)
			bOriginal: BOOL;  	// Set Original Bit (TRUE/FALSE)

			// VBR STUFF
			bWriteVBRHeader: BOOL;	// WRITE XING VBR HEADER (TRUE/FALSE)
			bEnableVBR: BOOL;       // USE VBR ENCODING (TRUE/FALSE)
			nVBRQuality: LongInt;	// VBR QUALITY 0..9
			dwVbrAbr_bps: DWORD;	// Use ABR in stead of nVBRQuality
			nVbrMethod: VBRMETHOD;
			bNoRes: BOOL;		// Disable Bit resorvoir

			btReserved: array[1..(255-3*sizeof(DWORD))] of Byte;
        end;

  TAAC = packed record
			dwSampleRate: DWORD	;
			byMode: BYTE	;
			wBitrate: WORD	;
			byEncodingMethod: BYTE	;
  end;

  TFormat = packed record
	    case Integer of
              1: (mp3: TMP3);
              2: (LHV1: TLHV1);
              3: (aac: TAAC);
  end;

  PBE_CONFIG = ^BE_CONFIG;
  BE_CONFIG = packed record
	dwConfig: DWORD;
        format: TFormat;
  end;

  PBEWaveFormat = ^TBEWaveFormat;
  TBEWaveFormat = packed record
     WaveFormat: TWaveformatEx;
     BE: BE_CONFIG;
  end;

  PBE_VERSION = ^BE_VERSION;
  BE_VERSION = packed record
	// BladeEnc DLL Version number
	byDLLMajorVersion: BYTE	;
	byDLLMinorVersion: BYTE	;

	// BladeEnc Engine Version Number
	byMajorVersion: BYTE	;
	byMinorVersion: BYTE	;

	// DLL Release date
	byDay: BYTE	;
	byMonth: BYTE	;
	wYear: WORD	;

	// BladeEnc	Homepage URL
	zHomepage: array[0..BE_MAX_HOMEPAGE] of Char;

	byAlphaLevel: BYTE	;
	byBetaLevel: BYTE	;
	byMMXEnabled: BYTE	;

	btReserved: array[1..125] of Byte;
end;

// function prototypes
TbeInitStream = function (pbeConfig: PBE_CONFIG; var dwSamples: DWORD; var dwBufferSize: DWORD; var hbeStream: HBE_STREAM) : BE_ERR; cdecl;
TbeVersion = procedure (var beVersion: BE_VERSION); cdecl;
TbeEncodeChunk = function (hbeStream: HBE_STREAM; nSamples: DWORD; Samples: pSmallInt; pOutput: PBYTE; var dwOutput: DWORD) : BE_ERR; cdecl;
TbeDeinitStream = function (hbeStream: HBE_STREAM; pOutput: PBYTE; var dwOutput: DWORD) : BE_ERR; cdecl;
TbeCloseStream = function (hbeStream: HBE_STREAM) : BE_ERR; cdecl;
TbeWriteVBRHeader = function (lpszFileName: LPCSTR) : BE_ERR; cdecl;

const
    // Change this before calling InitLame if the file is named differently
    // or not located in the PATH or EXE directory.
    beDLLName: string = 'lame_enc.dll';
    // DLL handle
    beDLL: THandle = INVALID_HANDLE_VALUE;
    procBeInitStream: TbeInitStream = nil;
    procBeVersion: TbeVersion = nil;
    procBeEncodeChunk: TbeEncodeChunk = nil;
    procBeDeinitStream: TbeDeinitStream = nil;
    procBeCloseStream: TbeCloseStream = nil;
    procBeWriteVBRHeader: TbeWriteVBRHeader = nil;

// Before using any of these 'constants', call InitLame to load the DLL.
// InitLame and DoneLame are NOT MT-Safe.
// Raises Win32exception if the DLL could not be loaded or not all proc
// entry points found. You can call it multiple times, calling it when
// already loaded has no effect at all (if you wish to re-load the DLL,
// call DoneLame first).
procedure InitLame;

// When done, call DoneLame to release the DLL. It is OK to call even when
// InitLame failed (it checks if Init was successfull). You do not need
// to call this on program termination, Windows will release the DLL in
// that case. You only need DoneLame if you wish to re-load the DLL, or if
// you insist on saving a few bytes memory.
procedure DoneLame;

// Handy wrapper, to check return code and raise exception when not 0.
procedure LameCall(beResult: BE_ERR);

implementation

uses SysUtils; // Win32 error stuff

procedure InitLame;
var dll: THandle;
begin
  if beDLL = INVALID_HANDLE_VALUE then
  begin
    dll := LoadLibrary(PChar(beDLLName));
    if dll = 0 then begin
      RaiseLastWin32Error;
    end;
    beDLL := dll;
    procBeInitStream := GetProcAddress(dll, 'beInitStream');
    procBeVersion := GetProcAddress(dll, 'beVersion');
    procBeEncodeChunk := GetProcAddress(dll, 'beEncodeChunk');
    procBeDeinitStream := GetProcAddress(dll, 'beDeinitStream');
    procBeCloseStream := GetProcAddress(dll, 'beCloseStream');
    procBeWriteVBRHeader := GetProcAddress(dll, 'beWriteVBRHeader');
    // validate
    if (@procBeInitStream = nil) or (@procBeVersion = nil) or
       (@procBeEncodeChunk = nil) or (@procBeDeinitStream = nil) or
       (@procBeCloseStream = nil) or (@procBeWriteVBRHeader = nil) then
       begin
         DoneLame;
         RaiseLastWin32Error;
       end;
   end;
end;

procedure DoneLame;
var dll: THandle;
begin
  dll := beDLL;
  if dll <> INVALID_HANDLE_VALUE then
  begin
    FreeLibrary(dll);
    beDLL := INVALID_HANDLE_VALUE;
  end;
end;

procedure LameCall(beResult: BE_ERR);
begin
  if beResult <> 0 then
    raise Exception.CreateFmt('LAME Encoder error: %d', [beResult]);
end;

end.

