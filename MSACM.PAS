{************************************************************}
{                                                            }
{       Delphi Runtime Library                               }
{       Windows 32bit API Interface Unit                     }
{       Microsoft Audio Compression Manager Interface unit   }
{       programmed by M.yanagisawa.                          }
{                                                            }
{************************************************************}

unit msacm;

interface

uses Windows,MMsystem,MMreg;

{
       Audio Compression Manager Common Dialogs Identifiers
}
const

  DLG_ACMFORMATCHOOSE_ID            = 70;
  IDD_ACMFORMATCHOOSE_BTN_HELP      = 9;
  IDD_ACMFORMATCHOOSE_CMB_CUSTOM    = 100;
  IDD_ACMFORMATCHOOSE_CMB_FORMATTAG = 101;
  IDD_ACMFORMATCHOOSE_CMB_FORMAT    = 102;
  IDD_ACMFORMATCHOOSE_BTN_SETNAME   = 103;
  IDD_ACMFORMATCHOOSE_BTN_DELNAME   = 104;


  DLG_ACMFILTERCHOOSE_ID            = 71;
  IDD_ACMFILTERCHOOSE_BTN_HELP      = 9;
  IDD_ACMFILTERCHOOSE_CMB_CUSTOM    = 100;
  IDD_ACMFILTERCHOOSE_CMB_FILTERTAG = 101;
  IDD_ACMFILTERCHOOSE_CMB_FILTER    = 102;
  IDD_ACMFILTERCHOOSE_BTN_SETNAME   = 103;
  IDD_ACMFILTERCHOOSE_BTN_DELNAME   = 104;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

const
  DRV_MAPPER_PREFERRED_INPUT_GET  = (DRV_USER + 0);

  DRV_MAPPER_PREFERRED_OUTPUT_GET = (DRV_USER + 2);

  DRVM_MAPPER_STATUS              = $2000;

  WIDM_MAPPER_STATUS              = (DRVM_MAPPER_STATUS + 0);
  WAVEIN_MAPPER_STATUS_DEVICE     = 0;
  WAVEIN_MAPPER_STATUS_MAPPED     = 1;
  WAVEIN_MAPPER_STATUS_FORMAT     = 2;

  WODM_MAPPER_STATUS              = (DRVM_MAPPER_STATUS + 0);
  WAVEOUT_MAPPER_STATUS_DEVICE    = 0;
  WAVEOUT_MAPPER_STATUS_MAPPED    = 1;
  WAVEOUT_MAPPER_STATUS_FORMAT    = 2;

{--------------------------------------------------------------------------

    ACM General API's and Defines




 --------------------------------------------------------------------------


    there are four types of 'handles' used by the ACM. the first three
    are unique types that define specific objects:

    HACMDRIVERID: used to _identify_ an ACM driver. this identifier can be
    used to _open_ the driver for querying details, etc about the driver.

    HACMDRIVER: used to manage a driver (codec, filter, etc). this handle
    is much like a handle to other media drivers--you use it to send
    messages to the converter, query for capabilities, etc.

    HACMSTREAM: used to manage a 'stream' (conversion channel) with the
    ACM. you use a stream handle to convert data from one format/type
    to another--much like dealing with a file handle.


    the fourth handle type is a generic type used on ACM functions that
    can accept two or more of the above handle types (for example the
    acmMetrics and acmDriverID functions).

    HACMOBJ: used to identify ACM objects. this handle is used on functions
    that can accept two or more ACM handle types.
}

type
  PHACMDRIVERID = ^HACMDRIVERID;
  HACMDRIVERID = Integer;
  PHACMDRIVER = ^HACMDRIVER;
  HACMDRIVER = Integer;
  PHACMSTREAM = ^HACMSTREAM;
  HACMSTREAM = Integer;
  PHACMOBJ = ^HACMOBJ;
  HACMOBJ = Integer;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

    ACM Error Codes

    Note that these error codes are specific errors that apply to the ACM
    directly--general errors are defined as MMSYSERR_*.

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

type
  MMRESULT = UINT;

const
  ACMERR_BASE         = 512;
  ACMERR_NOTPOSSIBLE  = (ACMERR_BASE + 0);
  ACMERR_BUSY         = (ACMERR_BASE + 1);
  ACMERR_UNPREPARED   = (ACMERR_BASE + 2);
  ACMERR_CANCELED     = (ACMERR_BASE + 3);

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    ACM Window Messages

    These window messages are sent by the ACM or ACM drivers to notify
    applications of events.

    Note that these window message numbers will also be defined in
    mmsystem.

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
const
  MM_ACM_OPEN      = MM_STREAM_OPEN;   { conversion callback messages }
  MM_ACM_CLOSE     = MM_STREAM_CLOSE;
  MM_ACM_DONE      = MM_STREAM_DONE;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmGetVersion()

    the ACM version is a 32 bit number that is broken into three parts as 
    follows:

        bits 24 - 31:   8 bit _major_ version number
        bits 16 - 23:   8 bit _minor_ version number
        bits  0 - 15:   16 bit build number

    this is then displayed as follows:

        bMajor = (BYTE)(dwVersion >> 24)
        bMinor = (BYTE)(dwVersion >> 16) & 
        wBuild = LOWORD(dwVersion)

  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmGetVersion:DWORD; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

   acmMetrics()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function acmMetrics(hao: HACMOBJ; uMetric: UINT; pMetric: Pointer):MMRESULT; stdcall;

const
  ACM_METRIC_COUNT_DRIVERS          = 1;
  ACM_METRIC_COUNT_CODECS           = 2;
  ACM_METRIC_COUNT_CONVERTERS       = 3;
  ACM_METRIC_COUNT_FILTERS          = 4;
  ACM_METRIC_COUNT_DISABLED         = 5;
  ACM_METRIC_COUNT_HARDWARE         = 6;
  ACM_METRIC_COUNT_LOCAL_DRIVERS    = 20;
  ACM_METRIC_COUNT_LOCAL_CODECS     = 21;
  ACM_METRIC_COUNT_LOCAL_CONVERTERS = 22;
  ACM_METRIC_COUNT_LOCAL_FILTERS    = 23;
  ACM_METRIC_COUNT_LOCAL_DISABLED   = 24;
  ACM_METRIC_HARDWARE_WAVE_INPUT    = 30;
  ACM_METRIC_HARDWARE_WAVE_OUTPUT   = 31;
  ACM_METRIC_MAX_SIZE_FORMAT        = 50;
  ACM_METRIC_MAX_SIZE_FILTER        = 51;
  ACM_METRIC_DRIVER_SUPPORT         = 100;
  ACM_METRIC_DRIVER_PRIORITY        = 101;

{ --------------------------------------------------------------------------

    ACM Drivers




  --------------------------------------------------------------------------}

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmDriverEnum()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
type
  TFNACMDriverEnumCb = function(hadid: HACMDRIVERID; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;

function acmDriverEnum(fnCallBack: TFNACMDriverEnumCb; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

const
  ACM_DRIVERENUMF_NOLOCAL   =  $40000000;
  ACM_DRIVERENUMF_DISABLED  =  $80000000;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

    acmDriverID()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmDriverID(hao: HACMOBJ; phadid: PHACMDriverID; fdwDriverID: DWORD):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmDriverAdd()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmDriverAddA(phadid: PHACMDriverID; hinstModule: HINST; lParam: LPARAM; dwPriority: DWORD; fdwAdd: DWORD):MMRESULT; stdcall;

function acmDriverAddW(phadid: PHACMDriverID; hinstModule: HINST; lParam: LPARAM; dwPriority: DWORD; fdwAdd: DWORD):MMRESULT; stdcall;

function acmDriverAdd(phadid: PHACMDriverID; hinstModule: HINST; lParam: LPARAM; dwPriority: DWORD; fdwAdd: DWORD):MMRESULT; stdcall;

const
  ACM_DRIVERADDF_FUNCTION   = $00000003;  { lParam is a procedure }
  ACM_DRIVERADDF_NOTIFYHWND = $00000004;  { lParam is notify hwnd }
  ACM_DRIVERADDF_TYPEMASK   = $00000007;  { driver type mask }
  ACM_DRIVERADDF_LOCAL      = $00000000;  { is local to current task }
  ACM_DRIVERADDF_GLOBAL     = $00000008;  { is global }

{
    prototype for ACM driver procedures that are installed as _functions_
    or _notifations_ instead of as a standalone installable driver.
}
type
  PFNACMDriverProc = ^TFNACMDriverProc;
  TFNACMDriverProc = function(dwID: DWORD; hadid: HACMDriverID; uMsg: UINT; lParam1: LPARAM; lParam2: LPARAM):LRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmDriverRemove()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmDriverRemove(hadid: HACMDriverID; fdwRemove: DWORD):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmDriverOpen()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmDriverOpen(phad: PHACMDriver; hadid: HACMDriverID; fdwOpen: DWORD):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmDriverClose()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmDriverClose(had: HACMDriver; fdwClose: DWORD):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmDriverMessage()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmDriverMessage(had: HACMDriver; uMsg: UINT; lParam1: LPARAM; lParam2: LPARAM):LRESULT; stdcall;

const
  ACMDM_USER           = (DRV_USER + $0000);
  ACMDM_RESERVED_LOW   = (DRV_USER + $2000);
  ACMDM_RESERVED_HIGH  = (DRV_USER + $2FFF);

  ACMDM_BASE           = ACMDM_RESERVED_LOW;

  ACMDM_DRIVER_ABOUT   = (ACMDM_BASE + 11);

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmDriverPriority


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmDriverPriority(hadid: HACMDriverID; dwPriority: DWORD; fdwPriority: DWORD):MMRESULT; stdcall;

const
  ACM_DRIVERPRIORITYF_ENABLE    = $00000001;
  ACM_DRIVERPRIORITYF_DISABLE   = $00000002;
  ACM_DRIVERPRIORITYF_ABLEMASK  = $00000003;
  ACM_DRIVERPRIORITYF_BEGIN     = $00010000;
  ACM_DRIVERPRIORITYF_END       = $00020000;
  ACM_DRIVERPRIORITYF_DEFERMASK = $00030000;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmDriverDetails()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

{
   ACMDRIVERDETAILS

    the ACMDRIVERDETAILS structure is used to get various capabilities from
    an ACM driver (codec, converter, filter).
}

const
  ACMDRIVERDETAILS_SHORTNAME_CHARS = 32;
  ACMDRIVERDETAILS_LONGNAME_CHARS  = 128;
  ACMDRIVERDETAILS_COPYRIGHT_CHARS = 80;
  ACMDRIVERDETAILS_LICENSING_CHARS = 128;
  ACMDRIVERDETAILS_FEATURES_CHARS  = 512;

type
  PACMDriverDetailsA = ^TACMDriverDetailsA;
  PACMDriverDetailsW = ^TACMDriverDetailsW;
  PACMDriverDetails = PACMDriverDetailsA;

  TACMDriverDetailsA = packed record
    cbStruct	: DWORD;       { number of valid bytes in structure }

    fccType		: FOURCC;       { compressor type 'audc' }
    fccComp		: FOURCC;       { sub-type (not used; reserved) }

    wMid		: WORD;        { manufacturer id }
    wPid		: WORD;        { product id }

    vdwACM		: DWORD;       { version of the ACM *compiled* for }
    vdwDriver	: DWORD;       { version of the driver }

    fdwSupport	: DWORD;       { misc. support flags }
    cFormatTags	: DWORD;       { total unique format tags supported }
    cFilterTags	: DWORD;       { total unique filter tags supported }

    hicon		: HICON;	   { handle to custom icon }

    szShortName : array [0..ACMDRIVERDETAILS_SHORTNAME_CHARS-1] of char;
    szLongName  : array [0..ACMDRIVERDETAILS_LONGNAME_CHARS-1] of char;
    szCopyright : array [0..ACMDRIVERDETAILS_COPYRIGHT_CHARS-1] of char;
    szLicensing : array [0..ACMDRIVERDETAILS_LICENSING_CHARS-1] of char;
    szFeatures	: array [0..ACMDRIVERDETAILS_FEATURES_CHARS-1] of char;

  end;

  TACMDriverDetailsW = packed record
    cbStruct	: DWORD;       { number of valid bytes in structure }

    fccType		: DWORD;       { compressor type 'audc' }
    fccComp		: DWORD;       { sub-type (not used; reserved) }

    wMid		: WORD;        { manufacturer id }
    wPid		: WORD;        { product id }

    vdwACM		: DWORD;       { version of the ACM *compiled* for }
    vdwDriver	: DWORD;       { version of the driver }

    fdwSupport	: DWORD;       { misc. support flags }
    cFormatTags	: DWORD;       { total unique format tags supported }
    cFilterTags	: DWORD;       { total unique filter tags supported }

    hicon		: HICON;	   { handle to custom icon }

    szShortName : array [0..ACMDRIVERDETAILS_SHORTNAME_CHARS-1] of WCHAR;
    szLongName  : array [0..ACMDRIVERDETAILS_LONGNAME_CHARS-1] of WCHAR;
    szCopyright : array [0..ACMDRIVERDETAILS_COPYRIGHT_CHARS-1] of WCHAR;
    szLicensing : array [0..ACMDRIVERDETAILS_LICENSING_CHARS-1] of WCHAR;
    szFeatures	: array [0..ACMDRIVERDETAILS_FEATURES_CHARS-1] of WCHAR;

  end;
  TACMDriverDetails = TACMDriverDetailsA;


{
    ACMDRIVERDETAILS.fccType

    ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC: the FOURCC used in the fccType
    field of the ACMDRIVERDETAILS structure to specify that this is an ACM
    codec designed for audio.


    ACMDRIVERDETAILS.fccComp

    ACMDRIVERDETAILS_FCCCOMP_UNDEFINED: the FOURCC used in the fccComp
    field of the ACMDRIVERDETAILS structure. this is currently an unused
    field.
}

const
  FOURCC_ACMDRIVERDETAILS_FCCTYPE_AUDIOCODEC = $63647561;  { 'audc' }
  FOURCC_ACMDRIVERDETAILS_FCCCOMP_UNDEFINED  = $00000000;  { $0000  }

{
    the following flags are used to specify the type of conversion(s) that
    the converter/codec/filter supports. these are placed in the fdwSupport
    field of the ACMDRIVERDETAILS structure. note that a converter can
    support one or more of these flags in any combination.

    ACMDRIVERDETAILS_SUPPORTF_CODEC: this flag is set if the driver supports
    conversions from one format tag to another format tag. for example, if a
    converter compresses WAVE_FORMAT_PCM to WAVE_FORMAT_ADPCM, then this bit
    should be set.

    ACMDRIVERDETAILS_SUPPORTF_CONVERTER: this flags is set if the driver
    supports conversions on the same format tag. as an example, the PCM
    converter that is built into the ACM sets this bit (and only this bit)
    because it converts only PCM formats (bits, sample rate).

    ACMDRIVERDETAILS_SUPPORTF_FILTER: this flag is set if the driver supports
    transformations on a single format. for example, a converter that changed
    the 'volume' of PCM data would set this bit. 'echo' and 'reverb' are
    also filter types.

    ACMDRIVERDETAILS_SUPPORTF_HARDWARE: this flag is set if the driver supports
    hardware input and/or output through a waveform device.

    ACMDRIVERDETAILS_SUPPORTF_ASYNC: this flag is set if the driver supports
    async conversions.


    ACMDRIVERDETAILS_SUPPORTF_LOCAL: this flag is set _by the ACM_ if a
    driver has been installed local to the current task. this flag is also
    set in the fdwSupport argument to the enumeration callback function
    for drivers.

    ACMDRIVERDETAILS_SUPPORTF_DISABLED: this flag is set _by the ACM_ if a
    driver has been disabled. this flag is also passed set in the fdwSupport
    argument to the enumeration callback function for drivers.
}

const
  ACMDRIVERDETAILS_SUPPORTF_CODEC     = $00000001;
  ACMDRIVERDETAILS_SUPPORTF_CONVERTER = $00000002;
  ACMDRIVERDETAILS_SUPPORTF_FILTER    = $00000004;
  ACMDRIVERDETAILS_SUPPORTF_HARDWARE  = $00000008;
  ACMDRIVERDETAILS_SUPPORTF_ASYNC     = $00000010;
  ACMDRIVERDETAILS_SUPPORTF_LOCAL     = $40000000;
  ACMDRIVERDETAILS_SUPPORTF_DISABLED  = $80000000;

function acmDriverDetailsA(hadid: HACMDriverID; padd: PACMDriverDetailsA; fdwDetails: DWORD ):MMRESULT; stdcall;

function acmDriverDetailsW(hadid: HACMDriverID; padd: PACMDriverDetailsW; fdwDetails: DWORD ):MMRESULT; stdcall;

function acmDriverDetails(hadid: HACMDriverID; padd: PACMDriverDetails; fdwDetails: DWORD ):MMRESULT; stdcall;
 
{  -------------------------------------------------------------------------

    ACM Format Tags




  --------------------------------------------------------------------------}

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFormatTagDetails()



  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

const
  ACMFORMATTAGDETAILS_FORMATTAG_CHARS = 48;

type
  PACMFormatTagDetailsA = ^TACMFormatTagDetailsA;
  PACMFormatTagDetailsW = ^TACMFormatTagDetailsW;
  PACMFormatTagDetails = ^TACMFormatTagDetails;

  TACMFormatTagDetailsA = packed record
	cbStruct			: DWORD;
    dwFormatTagIndex	: DWORD;
    dwFormatTag			: DWORD;
    cbFormatSize		: DWORD;
    fdwSupport			: DWORD;
    cStandardFormats	: DWORD;
    szFormatTag			: array [0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of char;
  end;

  TACMFormatTagDetailsW = packed record
	cbStruct			: DWORD;
    dwFormatTagIndex	: DWORD;
    dwFormatTag			: DWORD;
    cbFormatSize		: DWORD;
    fdwSupport			: DWORD;
    cStandardFormats	: DWORD;
    szFormatTag			: array [0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of WCHAR;
  end;

  TACMFormatTagDetails = TACMFormatTagDetailsA;

function acmFormatTagDetailsA(had: HACMDriver; paftd: PACMFormatTagDetailsA; fdwDetails: DWORD):MMRESULT; stdcall;

function acmFormatTagDetailsW(had: HACMDriver; paftd: PACMFormatTagDetailsW; fdwDetails: DWORD):MMRESULT; stdcall;

function acmFormatTagDetails(had: HACMDriver; paftd: PACMFormatTagDetails; fdwDetails: DWORD):MMRESULT; stdcall;

const
  ACM_FORMATTAGDETAILSF_INDEX       = $00000000;
  ACM_FORMATTAGDETAILSF_FORMATTAG   = $00000001;
  ACM_FORMATTAGDETAILSF_LARGESTSIZE = $00000002;
  ACM_FORMATTAGDETAILSF_QUERYMASK   = $0000000F;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFormatTagEnum()



  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
type
  TFNACMFormatTagEnumCbA = function(hadid: HACMDriverID; paftd: PACMFormatTagDetailsA; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFormatTagEnumCbW = function(hadid: HACMDriverID; paftd: PACMFormatTagDetailsW; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFormatTagEnumCb = TFNACMFormatTagEnumCbA;

function acmFormatTagEnumA(had: HACMDriver; paftd: PACMFormatTagDetailsA; fnCallback: TFNACMFormatTagEnumCbA; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

function acmFormatTagEnumW(had: HACMDriver; paftd: PACMFormatTagDetailsA; fnCallback: TFNACMFormatTagEnumCbW; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

function acmFormatTagEnum(had: HACMDriver; paftd: PACMFormatTagDetailsA; fnCallback: TFNACMFormatTagEnumCb; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;


{ --------------------------------------------------------------------------

    ACM Formats




  -------------------------------------------------------------------------- }

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFormatDetails()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

const
  ACMFORMATDETAILS_FORMAT_CHARS = 128;

type
  PACMFormatDetailsA = ^TACMFormatDetailsA;
  PACMFormatDetailsW = ^TACMFormatDetailsW;
  PACMFormatDetails = ^TACMFormatDetails;

  TACMFormatDetailsA = packed record
    cbStruct		: DWORD;
    dwFormatIndex	: DWORD;
    dwFormatTag		: DWORD;
    fdwSupport		: DWORD;
    pwfx			: PWaveFormatEx;
    cbwfx			: DWORD;
    szFormat		: array [0..ACMFORMATDETAILS_FORMAT_CHARS-1] of char;
  end;

  TACMFormatDetailsW = packed record
    cbStruct		: DWORD;
    dwFormatIndex	: DWORD;
    dwFormatTag		: DWORD;
    fdwSupport		: DWORD;
    pwfx			: PWaveFormatEx;
    cbwfx			: DWORD;
    szFormat		: array [0..ACMFORMATDETAILS_FORMAT_CHARS-1] of WCHAR;
  end;
  TACMFormatDetails = TACMFormatDetailsA;

function acmFormatDetailsA(had: HACMDriver; pafd: PACMFormatDetailsA; fdwDetails: DWORD):MMRESULT; stdcall;

function acmFormatDetailsW(had: HACMDriver; pafd: PACMFormatDetailsW; fdwDetails: DWORD):MMRESULT; stdcall;

function acmFormatDetails(had: HACMDriver; pafd: PACMFormatDetails; fdwDetails: DWORD):MMRESULT; stdcall;

const
  ACM_FORMATDETAILSF_INDEX     = $00000000;
  ACM_FORMATDETAILSF_FORMAT    = $00000001;
  ACM_FORMATDETAILSF_QUERYMASK = $0000000F;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmFormatEnum()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
type
  TFNACMFormatEnumCbA = function(hadid: HACMDriverID; paftd: PACMFormatDetailsA; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFormatEnumCbW = function(hadid: HACMDriverID; paftd: PACMFormatDetailsW; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFormatEnumCb = TFNACMFormatEnumCbA;

function acmFormatEnumA(had: HACMDriver; pafd: PACMFormatDetailsA; fnCallback: TFNACMFormatEnumCbA; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

function acmFormatEnumW(had: HACMDriver; pafd: PACMFormatDetailsW; fnCallback: TFNACMFormatEnumCbW; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

function acmFormatEnum(had: HACMDriver; pafd: PACMFormatDetails; fnCallback: TFNACMFormatEnumCb; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

const
  ACM_FORMATENUMF_WFORMATTAG      = $00010000;
  ACM_FORMATENUMF_NCHANNELS       = $00020000;
  ACM_FORMATENUMF_NSAMPLESPERSEC  = $00040000;
  ACM_FORMATENUMF_WBITSPERSAMPLE  = $00080000;
  ACM_FORMATENUMF_CONVERT         = $00100000;
  ACM_FORMATENUMF_SUGGEST         = $00200000;
  ACM_FORMATENUMF_HARDWARE        = $00400000;
  ACM_FORMATENUMF_INPUT           = $00800000;
  ACM_FORMATENUMF_OUTPUT          = $01000000;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFormatSuggest()



  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmFormatSuggest(had: HACMDriver; pwfxSrc: PWaveFormatEx; pwfxDst: PWaveFormatEx; cbwfxDst: DWORD; fdwSuggest: DWORD):MMRESULT; stdcall;

const
  ACM_FORMATSUGGESTF_WFORMATTAG     = $00010000;
  ACM_FORMATSUGGESTF_NCHANNELS      = $00020000;
  ACM_FORMATSUGGESTF_NSAMPLESPERSEC = $00040000;
  ACM_FORMATSUGGESTF_WBITSPERSAMPLE = $00080000;

  ACM_FORMATSUGGESTF_TYPEMASK       = $00FF0000;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmFormatChoose()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
const
  ACMHELPMSGSTRING      : array [0..13] of char = 'acmchoose_help';
  ACMHELPMSGSTRINGA     : array [0..13] of char = 'acmchoose_help';
  ACMHELPMSGSTRINGW     : array [0..13] of WCHAR = WideString('acmchoose_help');
  ACMHELPMSGCONTEXTMENU   : array [0..20] of char = 'acmchoose_contextmenu';
  ACMHELPMSGCONTEXTMENUA  : array [0..20] of char = 'acmchoose_contextmenu';
  ACMHELPMSGCONTEXTMENUW  : array [0..20] of WCHAR = WideString('acmchoose_contextmenu');
  ACMHELPMSGCONTEXTHELP   : array [0..20] of char = 'acmchoose_contexthelp';
  ACMHELPMSGCONTEXTHELPA   : array [0..20] of char = 'acmchoose_contexthelp';
  ACMHELPMSGCONTEXTHELPW   : array [0..20] of WCHAR = WideString('acmchoose_contexthelp');

{
  ACMHELPMSGSTRING      : array [0..13] of char = 'acmchoose_help';
  ACMHELPMSGSTRINGA     : array [0..13] of char = 'acmchoose_help';
  ACMHELPMSGSTRINGW     : array [0..13] of WCHAR = 'acmchoose_help';
  ACMHELPMSGCONTEXTMENU   : array [0..20] of char = 'acmchoose_contextmenu';
  ACMHELPMSGCONTEXTMENUA  : array [0..20] of char = 'acmchoose_contextmenu';
  ACMHELPMSGCONTEXTMENUW  : array [0..20] of WCHAR = 'acmchoose_contextmenu';
  ACMHELPMSGCONTEXTHELP   : array [0..20] of char = 'acmchoose_contexthelp';
  ACMHELPMSGCONTEXTHELPA   : array [0..20] of char = 'acmchoose_contexthelp';
  ACMHELPMSGCONTEXTHELPW   : array [0..20] of WCHAR = 'acmchoose_contexthelp';
 }
{
    MM_ACM_FORMATCHOOSE is sent to hook callbacks by the Format Chooser
    Dialog...
}
const
  MM_ACM_FORMATCHOOSE    = $8000;

  FORMATCHOOSE_MESSAGE   = 0;
  FORMATCHOOSE_FORMATTAG_VERIFY = (FORMATCHOOSE_MESSAGE+0);
  FORMATCHOOSE_FORMAT_VERIFY    = (FORMATCHOOSE_MESSAGE+1);
  FORMATCHOOSE_CUSTOM_VERIFY    = (FORMATCHOOSE_MESSAGE+2);

type
  TFNACMFormatChooseHookProcA = function(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM):UINT; stdcall;
  TFNACMFormatChooseHookProcW = function(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM):UINT; stdcall;
  TFNACMFormatChooseHookProc = TFNACMFormatChooseHookProcA;

{ ---------------------------------------------------------------------------- }

type
  PACMFormatChooseA = ^TACMFormatChooseA;
  PACMFormatChooseW = ^TACMFormatChooseW;
  PACMFormatChoose = ^TACMFormatChoose;

  TACMFormatChooseA = packed record
    cbStruct		: DWORD;         { sizeof(ACMFORMATCHOOSE) }
    fdwStyle		: DWORD;         { chooser style flags }
    
    hwndOwner		: HWND;          { caller's window handle }

    pwfx			: PWaveFormatEx; { ptr to wfx buf to receive choice }
    cbwfx			: DWORD;         { size of mem buf for pwfx }
    pszTitle		: LPCSTR;         { dialog box title bar }
    
    szFormatTag		: array [0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of char;
    szFormat		: array [0..ACMFORMATDETAILS_FORMAT_CHARS-1] of char;

    pszName			: LPCSTR;         { custom name selection }
    cchName			: DWORD;         { size in chars of mem buf for pszName }

    fdwEnum			: DWORD;         { format enumeration restrictions }
    pwfxEnum		: PWaveFormatEx; { format describing restrictions }
    
    hInstance		: HINST;         { app instance containing dlg template }
    pszTemplateName	: LPCSTR;    	 { custom template name }
    lCustData		: LPARAM;        { data passed to hook fn. }
    pfnHook			: TFNACMFormatChooseHookProcA;   { ptr to hook function }
  end;

  TACMFormatChooseW = packed record
    cbStruct		: DWORD;         { sizeof(ACMFORMATCHOOSE) }
    fdwStyle		: DWORD;         { chooser style flags }
    
    hwndOwner		: HWND;          { caller's window handle }

    pwfx			: PWaveFormatEx; { ptr to wfx buf to receive choice }
    cbwfx			: DWORD;         { size of mem buf for pwfx }
    pszTitle		: LPWSTR;         { dialog box title bar }
    
    szFormatTag		: array [0..ACMFORMATTAGDETAILS_FORMATTAG_CHARS-1] of WCHAR;
    szFormat		: array [0..ACMFORMATDETAILS_FORMAT_CHARS-1] of WCHAR;

    pszName			: LPWSTR;         { custom name selection }
    cchName			: DWORD;         { size in chars of mem buf for pszName }

    fdwEnum			: DWORD;         { format enumeration restrictions }
    pwfxEnum		: PWaveFormatEx; { format describing restrictions }
    
    hInstance		: HINST;         { app instance containing dlg template }
    pszTemplateName	: LPWSTR;    	 { custom template name }
    lCustData		: LPARAM;        { data passed to hook fn. }
    pfnHook			: TFNACMFormatChooseHookProcA;   { ptr to hook function }
  end;
  TACMFormatChoose = TACMFormatChooseA;


{
    ACMFORMATCHOOSE.fdwStyle
}

const
  ACMFORMATCHOOSE_STYLEF_SHOWHELP              = $00000004;
  ACMFORMATCHOOSE_STYLEF_ENABLEHOOK            = $00000008;
  ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATE        = $00000010;
  ACMFORMATCHOOSE_STYLEF_ENABLETEMPLATEHANDLE  = $00000020;
  ACMFORMATCHOOSE_STYLEF_INITTOWFXSTRUCT       = $00000040;
  ACMFORMATCHOOSE_STYLEF_CONTEXTHELP           = $00000080;

function acmFormatChooseA(pafmtc: PACMFormatChooseA):MMRESULT; stdcall;
function acmFormatChooseW(pafmtc: PACMFormatChooseW):MMRESULT; stdcall;
function acmFormatChoose(pafmtc: PACMFormatChoose):MMRESULT; stdcall;

{ --------------------------------------------------------------------------

    ACM Filter Tags




  -------------------------------------------------------------------------- }

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFilterTagDetails()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
const
  ACMFILTERTAGDETAILS_FILTERTAG_CHARS = 48;

type
  PACMFilterTagDetailsA = ^TACMFilterTagDetailsA;
  PACMFilterTagDetailsW = ^TACMFilterTagDetailsW;
  PACMFilterTagDetails = ^TACMFilterTagDetails;

  TACMFilterTagDetailsA = packed record
    cbStruct			: DWORD;
    dwFilterTagIndex	: DWORD;
    dwFilterTag			: DWORD;
    cbFilterSize		: DWORD;
    fdwSupport			: DWORD;
    cStandardFilters	: DWORD;
    szFilterTag			: array [0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of char;
  end;

  TACMFilterTagDetailsW = packed record
    cbStruct			: DWORD;
    dwFilterTagIndex	: DWORD;
    dwFilterTag			: DWORD;
    cbFilterSize		: DWORD;
    fdwSupport			: DWORD;
    cStandardFilters	: DWORD;
    szFilterTag			: array [0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of WCHAR;
  end;
  TACMFilterTagDetails = TACMFilterTagDetailsA;

function acmFilterTagDetailsA(had: HACMDriver; paftd: PACMFilterTagDetailsA; fdwDetails: DWORD):MMRESULT; stdcall;

function acmFilterTagDetailsW(had: HACMDriver; paftd: PACMFilterTagDetailsW; fdwDetails: DWORD):MMRESULT; stdcall;

function acmFilterTagDetails(had: HACMDriver; paftd: PACMFilterTagDetails; fdwDetails: DWORD):MMRESULT; stdcall;

const
  ACM_FILTERTAGDETAILSF_INDEX       = $00000000;
  ACM_FILTERTAGDETAILSF_FILTERTAG   = $00000001;
  ACM_FILTERTAGDETAILSF_LARGESTSIZE = $00000002;
  ACM_FILTERTAGDETAILSF_QUERYMASK   = $0000000F;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFilterTagEnum()



  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

type
  TFNACMFilterTagEnumCbA = function(hadid: HACMDriverID; paftd: PACMFilterTagDetailsA; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFilterTagEnumCbW = function(hadid: HACMDriverID; paftd: PACMFilterTagDetailsW; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFilterTagEnumCb = TFNACMFilterTagEnumCbA;

function acmFilterTagEnumA(had: HACMDriver; paftd: PACMFilterTagDetailsA; fnCallback: TFNACMFilterTagEnumCbA; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;
function acmFilterTagEnumW(had: HACMDriver; paftd: PACMFilterTagDetailsW; fnCallback: TFNACMFilterTagEnumCbW; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;
function acmFilterTagEnum(had: HACMDriver; paftd: PACMFilterTagDetails; fnCallback: TFNACMFilterTagEnumCb; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

{ --------------------------------------------------------------------------

    ACM Filters




  -------------------------------------------------------------------------- }

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFilterDetails()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

const
  ACMFILTERDETAILS_FILTER_CHARS = 128;

type
  PACMFilterDetailsA = ^TACMFilterDetailsA;
  PACMFilterDetailsW = ^TACMFilterDetailsW;
  PACMFilterDetails = ^TACMFilterDetails;

  TACMFilterDetailsA = packed record
    cbStruct		: DWORD;
    dwFilterIndex	: DWORD;
    dwFilterTag		: DWORD;
    fdwSupport		: DWORD;
    pwfltr			: PWaveFilter;
    cbwfltr			: DWORD;
    szFilter		: array [0..ACMFILTERDETAILS_FILTER_CHARS-1] of char;
  end;

  TACMFilterDetailsW = packed record
    cbStruct		: DWORD;
    dwFilterIndex	: DWORD;
    dwFilterTag		: DWORD;
    fdwSupport		: DWORD;
    pwfltr			: PWaveFilter;
    cbwfltr			: DWORD;
    szFilter		: array [0..ACMFILTERDETAILS_FILTER_CHARS-1] of WCHAR;
  end;
  TACMFilterDetails = TACMFilterDetailsA;

function acmFilterDetailsA(had: HACMDriver; pafd: PACMFilterDetailsA; fdwDetails: DWORD):MMRESULT; stdcall;
function acmFilterDetailsW(had: HACMDriver; pafd: PACMFilterDetailsW; fdwDetails: DWORD):MMRESULT; stdcall;
function acmFilterDetails(had: HACMDriver; pafd: PACMFilterDetails; fdwDetails: DWORD):MMRESULT; stdcall;

const
  ACM_FILTERDETAILSF_INDEX     = $00000000;
  ACM_FILTERDETAILSF_FILTER    = $00000001;
  ACM_FILTERDETAILSF_QUERYMASK = $0000000F;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmFilterEnum()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
type
  TFNACMFilterEnumCbA = function(hadid: HACMDriverID; paftd: PACMFilterDetailsA; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFilterEnumCbW = function(hadid: HACMDriverID; paftd: PACMFilterDetailsW; dwInstance: DWORD; fdwSupport: DWORD):BOOL; stdcall;
  TFNACMFilterEnumCb = TFNACMFilterEnumCbA;

function acmFilterEnumA(had: HACMDriver; pafd: PACMFilterDetailsA; fnCallback: TFNACMFilterEnumCbA; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;
function acmFilterEnumW(had: HACMDriver; pafd: PACMFilterDetailsW; fnCallback: TFNACMFilterEnumCbW; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;
function acmFilterEnum(had: HACMDriver; pafd: PACMFilterDetails; fnCallback: TFNACMFilterEnumCb; dwInstance: DWORD; fdwEnum: DWORD):MMRESULT; stdcall;

const
  ACM_FILTERENUMF_DWFILTERTAG  = $00010000;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmFilterChoose()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

{
    MM_ACM_FILTERCHOOSE is sent to hook callbacks by the Filter Chooser
    Dialog...
}
const
  MM_ACM_FILTERCHOOSE           = $8000;

  FILTERCHOOSE_MESSAGE          = 0;
  FILTERCHOOSE_FILTERTAG_VERIFY = (FILTERCHOOSE_MESSAGE+0);
  FILTERCHOOSE_FILTER_VERIFY    = (FILTERCHOOSE_MESSAGE+1);
  FILTERCHOOSE_CUSTOM_VERIFY    = (FILTERCHOOSE_MESSAGE+2);

type
  TFNACMFilterChooseHookProcA = function(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM):UINT; stdcall;
  TFNACMFilterChooseHookProcW = function(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM):UINT; stdcall;
  TFNACMFilterChooseHookProc = TFNACMFilterChooseHookProcA;

{
    ACMFILTERCHOOSE
}

type
  PACMFilterChooseA = ^TACMFilterChooseA;
  PACMFilterChooseW = ^TACMFilterChooseW;
  PACMFilterChoose = ^TACMFilterChoose;

  TACMFilterChooseA = packed record
	cbStruct		: DWORD;         { sizeof(ACMFILTERCHOOSE) }

    fdwStyle		: DWORD;         { chooser style flags }

    hwndOwner		: HWND;          { caller's window handle }

    pwfltr			: PWaveFilter;   { ptr to wfltr buf to receive choice }
    cbwfltr			: DWORD;         { size of mem buf for pwfltr }

    pszTitle		: LPCSTR;

    szFilterTag		: array [0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of char;
    szFilter		: array [0..ACMFILTERDETAILS_FILTER_CHARS-1] of char;
    pszName			: LPCSTR;        { custom name selection }
    cchName			: DWORD;         { size in chars of mem buf for pszName }

    fdwEnum			: DWORD;         { filter enumeration restrictions }
    pwfltrEnum		: PWaveFilter;   { filter describing restrictions }
    
    hInstance		: HINST;         { app instance containing dlg template }
    pszTemplateName	: LPCSTR;    	 { custom template name }
    lCustData		: LPARAM;        { data passed to hook fn. }
    pfnHook			: TFNACMFilterChooseHookProcA;    { ptr to hook function }
  end;

  TACMFilterChooseW = packed record
	cbStruct		: DWORD;         { sizeof(ACMFILTERCHOOSE) }

    fdwStyle		: DWORD;         { chooser style flags }

    hwndOwner		: HWND;          { caller's window handle }

    pwfltr			: PWaveFilter;   { ptr to wfltr buf to receive choice }
    cbwfltr			: DWORD;         { size of mem buf for pwfltr }

    pszTitle		: LPCWSTR;

    szFilterTag		: array [0..ACMFILTERTAGDETAILS_FILTERTAG_CHARS-1] of WCHAR;
    szFilter		: array [0..ACMFILTERDETAILS_FILTER_CHARS-1] of WCHAR;
    pszName			: LPCWSTR;       { custom name selection }
    cchName			: DWORD;         { size in chars of mem buf for pszName }

    fdwEnum			: DWORD;         { filter enumeration restrictions }
    pwfltrEnum		: PWaveFilter;   { filter describing restrictions }
    
    hInstance		: HINST;         { app instance containing dlg template }
    pszTemplateName	: LPCWSTR;    	 { custom template name }
    lCustData		: LPARAM;        { data passed to hook fn. }
    pfnHook			: TFNACMFilterChooseHookProcA;    { ptr to hook function }
  end;
  TACMFilterChoose = TACMFilterChooseA;

{
    ACMFILTERCHOOSE.fdwStyle
}

const
  ACMFILTERCHOOSE_STYLEF_SHOWHELP             = $00000004;
  ACMFILTERCHOOSE_STYLEF_ENABLEHOOK           = $00000008;
  ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATE       = $00000010;
  ACMFILTERCHOOSE_STYLEF_ENABLETEMPLATEHANDLE = $00000020;
  ACMFILTERCHOOSE_STYLEF_INITTOFILTERSTRUCT   = $00000040;
  ACMFILTERCHOOSE_STYLEF_CONTEXTHELP          = $00000080;


function acmFilterChooseA(pafltrc: PACMFilterChooseA):MMRESULT; stdcall;
function acmFilterChooseW(pafltrc: PACMFilterChooseW):MMRESULT; stdcall;
function acmFilterChoose(pafltrc: PACMFilterChoose):MMRESULT; stdcall;


{ --------------------------------------------------------------------------

    ACM Stream API's



  -------------------------------------------------------------------------- }

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmStreamOpen()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

type
  PACMStreamHeader = ^TACMStreamHeader;
  TACMStreamHeader = packed record
    cbStruct			: DWORD;              { sizeof(ACMSTREAMHEADER) }
    fdwStatus			: DWORD;              { ACMSTREAMHEADER_STATUSF_* }
    dwUser				: DWORD;              { user instance data for hdr }
    pbSrc				: PBYTE;
    cbSrcLength			: DWORD;
    cbSrcLengthUsed		: DWORD;
    dwSrcUser			: DWORD;              { user instance data for src }
    pbDst				: PBYTE;
    cbDstLength			: DWORD;
    cbDstLengthUsed		: DWORD;
    dwDstUser			: DWORD;              { user instance data for dst }
    dwReservedDriver	: array [0..9] of DWORD; { driver reserved work space }
  end;

{
    ACMSTREAMHEADER.fdwStatus

    ACMSTREAMHEADER_STATUSF_DONE: done bit for async conversions.
}
const
  ACMSTREAMHEADER_STATUSF_DONE     = $00010000;
  ACMSTREAMHEADER_STATUSF_PREPARED = $00020000;
  ACMSTREAMHEADER_STATUSF_INQUEUE  = $00100000;

function acmStreamOpen
(
    phas		: PHACMStream;   	{ pointer to stream handle }
    had			: HACMDriver;       { optional driver handle }
    pwfxSrc		: PWaveFormatEx;    { source format to convert }
    pwfxDst		: PWaveFormatEx;    { required destination format }
    pwfltr		: PWaveFilter;     	{ optional filter }
    dwCallback	: DWORD; 			{ callback }
    dwInstance	: DWORD; 			{ callback instance data }
    fdwOpen		: DWORD     		{ ACM_STREAMOPENF_* and CALLBACK_* }
):MMRESULT; stdcall;

const
  ACM_STREAMOPENF_QUERY        = $00000001;
  ACM_STREAMOPENF_ASYNC        = $00000002;
  ACM_STREAMOPENF_NONREALTIME  = $00000004;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmStreamClose()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmStreamClose(has: HACMStream; fdwClose: DWORD):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmStreamSize()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmStreamSize(has: HACMStream; cbInput: DWORD; pdwOutputBytes: LPDWORD; fdwSize: DWORD):MMRESULT; stdcall;

const
  ACM_STREAMSIZEF_SOURCE      = $00000000;
  ACM_STREAMSIZEF_DESTINATION = $00000001;
  ACM_STREAMSIZEF_QUERYMASK   = $0000000F;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmStreamReset()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmStreamReset(has: HACMStream; fdwReset: DWORD):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmStreamMessage()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmStreamMessage(has: HACMStream; uMsg: UINT; lParam1: LPARAM; lParam2: LPARAM):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    acmStreamConvert()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmStreamConvert(has: HACMStream; pash: PACMStreamHeader; fdwConvert: DWORD):MMRESULT; stdcall;

const
  ACM_STREAMCONVERTF_BLOCKALIGN = $00000004;
  ACM_STREAMCONVERTF_START      = $00000010;
  ACM_STREAMCONVERTF_END        = $00000020;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmStreamPrepareHeader()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmStreamPrepareHeader(has: HACMStream; pash: PACMStreamHeader; fdwPrepare: DWORD):MMRESULT; stdcall;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    acmStreamUnprepareHeader()


  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }

function acmStreamUnprepareHeader(has: HACMStream; pash: PACMStreamHeader; fdwUnprepare: DWORD):MMRESULT; stdcall;

const
	msacmt = 'msacm32.dll';

implementation

function acmGetVersion; external msacmt name 'acmGetVersion';
function acmMetrics; external msacmt name 'acmMetrics';

function acmDriverEnum; external msacmt name 'acmDriverEnum';
function acmDriverID; external msacmt name 'acmDriverID';
function acmDriverAddA; external msacmt name 'acmDriverAddA';
function acmDriverAddW; external msacmt name 'acmDriverAddW';
function acmDriverAdd; external msacmt name 'acmDriverAddA';
function acmDriverRemove; external msacmt name 'acmDriverRemove';
function acmDriverOpen; external msacmt name 'acmDriverOpen';
function acmDriverClose; external msacmt name 'acmDriverClose';
function acmDriverMessage; external msacmt name 'acmDriverMessage';
function acmDriverPriority; external msacmt name 'acmDriverPriority';
function acmDriverDetailsA; external msacmt name 'acmDriverDetailsA';
function acmDriverDetailsW; external msacmt name 'acmDriverDetailsW';
function acmDriverDetails; external msacmt name 'acmDriverDetailsA';

function acmFormatTagDetailsA; external msacmt name 'acmFormatTagDetailsA';
function acmFormatTagDetailsW; external msacmt name 'acmFormatTagDetailsW';
function acmFormatTagDetails; external msacmt name 'acmFormatTagDetailsA';
function acmFormatTagEnumA; external msacmt name 'acmFormatTagEnumA';
function acmFormatTagEnumW; external msacmt name 'acmFormatTagEnumW';
function acmFormatTagEnum; external msacmt name 'acmFormatTagEnumA';
function acmFormatDetailsA; external msacmt name 'acmFormatDetailsA';
function acmFormatDetailsW; external msacmt name 'acmFormatDetailsW';
function acmFormatDetails; external msacmt name 'acmFormatDetailsA';
function acmFormatEnumA; external msacmt name 'acmFormatEnumA';
function acmFormatEnumW; external msacmt name 'acmFormatEnumW';
function acmFormatEnum; external msacmt name 'acmFormatEnumA';
function acmFormatSuggest; external msacmt name 'acmFormatSuggest';
function acmFormatChooseA; external msacmt name 'acmFormatChooseA';
function acmFormatChooseW; external msacmt name 'acmFormatChooseW';
function acmFormatChoose; external msacmt name 'acmFormatChooseA';

function acmFilterTagDetailsA; external msacmt name 'acmFilterTagDetailsA';
function acmFilterTagDetailsW; external msacmt name 'acmFilterTagDetailsW';
function acmFilterTagDetails; external msacmt name 'acmFilterTagDetailsA';
function acmFilterTagEnumA; external msacmt name 'acmFilterTagEnumA';
function acmFilterTagEnumW; external msacmt name 'acmFilterTagEnumW';
function acmFilterTagEnum; external msacmt name 'acmFilterTagEnumA';
function acmFilterDetailsA; external msacmt name 'acmFilterDetailsA';
function acmFilterDetailsW; external msacmt name 'acmFilterDetailsW';
function acmFilterDetails; external msacmt name 'acmFilterDetailsA';
function acmFilterEnumA; external msacmt name 'acmFilterEnumA';
function acmFilterEnumW; external msacmt name 'acmFilterEnumW';
function acmFilterEnum; external msacmt name 'acmFilterEnumA';
function acmFilterChooseA; external msacmt name 'acmFilterChooseA';
function acmFilterChooseW; external msacmt name 'acmFilterChooseW';
function acmFilterChoose; external msacmt name 'acmFilterChooseA';

function acmStreamOpen; external msacmt name 'acmStreamOpen';
function acmStreamClose; external msacmt name 'acmStreamClose';
function acmStreamSize; external msacmt name 'acmStreamSize';
function acmStreamReset; external msacmt name 'acmStreamReset';
function acmStreamMessage; external msacmt name 'acmStreamMessage';
function acmStreamConvert; external msacmt name 'acmStreamConvert';
function acmStreamPrepareHeader; external msacmt name 'acmStreamPrepareHeader';
function acmStreamUnprepareHeader; external msacmt name 'acmStreamUnprepareHeader';

end.
