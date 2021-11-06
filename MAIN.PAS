unit Main;

interface

uses
  SysUtils, Windows, ShellAPI, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, Grids,
  MMSystem, Spin, DlgRecShare, AutoCut, TimeSplitFrm,
  Settings, SelectDirFrm, WaveUtil, working {, WaveLoad}, RecThd, //OgProExe,
  RecordFrm, CommCtrl, VolumeMeter, ImgList, ActnList, ToolWin, DlgJumpTo,
  DAEFrm, WaveIO;

const
  BufferSize = 2352 * 10; // 2/15 second (CD quality) per buffer
  NumBuffers = 30;        // 30 buffers -> 4 seconds
  AppName    = 'CD Wave Editor';
  WM_SHOULDSTOP = WM_USER + 1;
  rcmWindow   = 0;
  rcmFunction = 1;
  rcmThread   = 2;

const
  DUTCH = LANG_DUTCH;

type
  TWaveStatus =
    (wsNotInitialized, wsNone, wsOpen, wsPlaying, wsPaused, wsRecord);

  TWaveBuffers = array[0..NumBuffers-1] of PWaveHdr;
  TWaveOutException = class(Exception)
  public
    constructor Create(ErrCode: MMResult; context: string);
  end;
  TWaveInException = class(Exception)
  public
    constructor Create(ErrCode: MMResult);
  end;
  PFastBuffer = ^TFastBuffer;
  TFastBuffer = array[0..2*BufferSize - 1] of Byte;
  PCardinal = ^Cardinal;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileExitItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    StatusLine: TStatusBar;
    OpenDialog: TOpenDialog;
    TimeDragPanel: TPanel;
    pbTimeRange: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    pbWave: TPaintBox;
    TrackList: TListView;
    LedImages: TImageList;
    TrackPopupMenu: TPopupMenu;
    Deletetracksplit1: TMenuItem;
    Panel4: TPanel;
    Splitter2: TSplitter;
    HelpContents1: TMenuItem;
    N1: TMenuItem;
    Actions1: TMenuItem;
    Record1: TMenuItem;
    N2: TMenuItem;
    Play1: TMenuItem;
    Stop1: TMenuItem;
    Pause1: TMenuItem;
    Ffwd1: TMenuItem;
    Back1: TMenuItem;
    Skip1: TMenuItem;
    Split1: TMenuItem;
    Autosplit1: TMenuItem;
    Clearallsplits1: TMenuItem;
    ClearAllsplits2: TMenuItem;
    Preferences1: TMenuItem;
    N3: TMenuItem;
    Savecuesheet1: TMenuItem;
    Loadcuesheet1: TMenuItem;
    OpenCueDialog: TOpenDialog;
    SaveCueDialog: TSaveDialog;
    pmPositionMode: TPopupMenu;
    Total2: TMenuItem;
    Remaining1: TMenuItem;
    Track2: TMenuItem;
    Trackremaining1: TMenuItem;
    Renametrack1: TMenuItem;
    VolumePanel: TPanel;
    VolumeMeter1: TVolumeMeter;
    Panel3: TPanel;
    Panel5: TPanel;
    lMarkerPos: TLabel;
    Label2: TLabel;
    SpinButton1: TSpinButton;
    BtnSplit: TBitBtn;
    Panel6: TPanel;
    lPosition: TLabel;
    lTimeDisplay: TLabel;
    pExtra: TPanel;
    lExtra: TLabel;
    lDesExtra: TLabel;
    Panel7: TPanel;
    pbSignalLed: TPaintBox;
    pbWarnLed: TPaintBox;
    pbClipLed: TPaintBox;
    N6: TMenuItem;
    Checkall1: TMenuItem;
    Uncheckall1: TMenuItem;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    ActionImageList: TImageList;
    FileNewAction: TAction;
    FileOpenAction: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    FileSaveAction: TAction;
    RecordAction: TAction;
    Play: TAction;
    Stop: TAction;
    FastForward: TAction;
    Pause: TAction;
    Back: TAction;
    Skip: TAction;
    AutoSplitAction: TAction;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton7: TToolButton;
    BtnPlay: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    HelpContents: TAction;
    About: TAction;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    Preferences: TAction;
    btnAutoSplit: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    PositionTimer: TTimer;
    FilePropertiesAction: TAction;
    ToolButton20: TToolButton;
    Properties1: TMenuItem;
    TimeSplitAction: TAction;
    Splitatintervals1: TMenuItem;
    SplitAction: TAction;
    JumpForward: TAction;
    JumpBack: TAction;
    JumpBigForward: TAction;
    JumpBigBack: TAction;
    JumpTo: TAction;
    FileAppendAction: TAction;
    Append1: TMenuItem;
    RecordTimer: TTimer;
    JumpOneForward: TAction;
    JumpOneBack: TAction;
    Jumptotrack1: TMenuItem;
    Jumpto1: TMenuItem;
    PlaySpace: TAction;
    CheckTrackToggle: TAction;
    UnCheckthisone1: TMenuItem;
    ActionClearAllSplits: TAction;
    ActionJumpTrack: TAction;
    ActionDisplayFormat: TAction;
    WaveScrollBar: TScrollBar;
    Split2: TMenuItem;
    ActionRemoveSmallSplits: TAction;
    Unchecksmallsplits1: TMenuItem;
    Unchecksmallsplits2: TMenuItem;
    ActionBoundsCheck: TAction;
    Boundscheck1: TMenuItem;
    ToolButton5: TToolButton;
    ActionPlayTrack: TAction;
    Playtrack1: TMenuItem;
    DAEAction: TAction;
    ToolButton6: TToolButton;
    DAE1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pbWaveMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbTimeRangePaint(Sender: TObject);
    procedure pbWaveMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbWaveMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure BtnSplitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnPlayClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure TrackListDeletion(Sender: TObject; Item: TListItem);
    procedure TrackListEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure pbWavePaint(Sender: TObject);
    procedure BtnRecordClick(Sender: TObject);
    procedure BtnFastClick(Sender: TObject);
    procedure BtnPauseClick(Sender: TObject);
    procedure pbClipLedPaint(Sender: TObject);
    procedure pbClipLedClick(Sender: TObject);
    procedure pbWarnLedPaint(Sender: TObject);
    procedure BtnBackClick(Sender: TObject);
    procedure BtnSkipClick(Sender: TObject);
    procedure pbSignalLedPaint(Sender: TObject);
    procedure Deletetracksplit1Click(Sender: TObject);
    procedure TrackPopupMenuPopup(Sender: TObject);
    procedure WaveBarPaint(Sender: TObject);
    procedure pbTimeRangeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbTimeRangeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbTimeRangeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HelpContents1Click(Sender: TObject);
    procedure Clearallsplits1Click(Sender: TObject);
    procedure Preferences1Click(Sender: TObject);
    procedure Savecuesheet1Click(Sender: TObject);
    procedure Loadcuesheet1Click(Sender: TObject);
    procedure TrackListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure Total2Click(Sender: TObject);
    procedure lTimeDisplayClick(Sender: TObject);
    procedure ddeCDWAVExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure Renametrack1Click(Sender: TObject);
    procedure TrackListDblClick(Sender: TObject);
    procedure Checkall1Click(Sender: TObject);
    procedure Uncheckall1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PositionTimerTimer(Sender: TObject);
    procedure FilePropertiesActionExecute(Sender: TObject);
    procedure JumpForwardExecute(Sender: TObject);
    procedure JumpBackExecute(Sender: TObject);
    procedure JumpBigBackExecute(Sender: TObject);
    procedure JumpBigForwardExecute(Sender: TObject);
    procedure JumpToExecute(Sender: TObject);
    procedure FileAppendActionExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure RecordTimerTimer(Sender: TObject);
    procedure JumpOneForwardExecute(Sender: TObject);
    procedure JumpOneBackExecute(Sender: TObject);
    procedure ActionNotEditingUpdate(Sender: TObject);
    procedure PlaySpaceUpdate(Sender: TObject);
    procedure PlaySpaceExecute(Sender: TObject);
    procedure CheckTrackToggleExecute(Sender: TObject);
    procedure CheckTrackToggleUpdate(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
    procedure WaveScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure TrackListCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure Unchecksmallsplits1Click(Sender: TObject);
    procedure ActionBoundsCheckExecute(Sender: TObject);
    procedure ActionPlayTrackExecute(Sender: TObject);
    procedure ActionPlayableTrackUpdate(Sender: TObject);
    procedure PlayUpdate(Sender: TObject);
    procedure DAEActionExecute(Sender: TObject);
    procedure AutoSplitActionExecute(Sender: TObject);
    procedure TimeSplitActionExecute(Sender: TObject);
  private
    AutoCutDlg: TForm;
    TimeSplitForm: TForm;
    procedure womDone(var Msg: TMessage); message MM_WOM_DONE;
    procedure wimData(var Msg: TMessage); message MM_WIM_DATA;
    procedure wimOpen(var Msg: TMessage); message MM_WIM_OPEN;
    procedure wimClose(var Msg: TMessage); message MM_WIM_CLOSE;
    procedure WMRData(var Msg: TMessage);  message WM_RDATA;
    procedure WMRStarted(var Msg: TMessage);  message WM_RSTARTED;
    procedure ShouldStop(var Msg: TMessage);  message WM_SHOULDSTOP;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMDropfiles(var msg: TMessage); message WM_DROPFILES;
    procedure WMSysCommand(var Msg: TMessage); message WM_SYSCOMMAND;
  protected
    FWaveFormat: PWaveFormatEx;
    FWaveStatus: TWaveStatus; {current status}
    FCursorPos: Cardinal;
    FMarkerPos: Cardinal;
    FastBuffer: PFastBuffer;
    FStopPlay:  Cardinal;
    FTimeDisplayMode: Integer;
    SectorStatsList: PSectorStatsList;
    SectorStatsHandle: HGLOBAL;
    FSectorStatsSize: Cardinal;
    WaveForm: PWaveForm;
    WaveFormSize:Cardinal;
    ZoomWaveForm: PWaveForm;
    ZoomWaveFormSize: Cardinal;
    ZoomWaveFormHeight: Integer;
    FZoomOffset:  Cardinal;
    RecPosition: Cardinal;
    RecOffset:   Cardinal;
    RecSilent:   Cardinal;
    RecThread:   TWaveRecordThd;
    RecSettings: TRecSettings;
    PriorityBoost: Integer;
    Colors: array[0..7] of Integer;
    RecCallbackMode: Integer;
    FWaveSpeed: Boolean;
    WaveZoomMBD: Boolean;
    WaveZoomLastMove: Integer;
    WaveBarMBD: Boolean;
    TimeRangeMBD: Boolean;
    TimeRangeLastMove: Integer;
    FHasClipped: Boolean;
    FClipWarning:Boolean;
    FHasSignal:  Boolean;
    FHasChanged: Boolean;
    ConfChanges: Boolean;
    BottomFollowsPlayback: boolean;
    RewindOnStop: Boolean;
    QuitAfterRecording: Boolean;
    SplitAtMarker: Boolean;
    procedure SetWaveStatus(Value: TWaveStatus);
    function GetIsPlaying: Boolean;
    function GetIsRecording: Boolean;
    procedure SetCursorPos(Value: Cardinal);
    procedure SetMarkerPos(Value: Cardinal);
    procedure SetZoomOffset(Value: Cardinal);
    procedure SetWaveSpeed(Value: Boolean);
    procedure SetHasClipped(Value: Boolean);
    procedure SetClipWarning(Value: Boolean);
    procedure SetHasSignal(Value: Boolean);
    procedure SetFlatButtons(Value: Boolean);
    function GetFlatButtons: Boolean;
    function  GetNiceColor(Index: Integer): Integer;
    procedure WavePaintLine(At: Integer);
    procedure ZoomWavePaintLine(At: Integer; BackClr: Integer);
    procedure RecalcWaveForm;
    procedure RecalcZoomWaveForm;
    function FindTrack(At: Cardinal): Integer;
    function StrToTime(s: string): Cardinal;
    function GetSelectedTime: Integer;
    procedure ShowSelectedTime;
    procedure SetTimeDisplayMode(Value: Integer);
    procedure UpdateTimeDisplay;
    function GetShortcutsEnabled: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    function createAboutBox: TForm;
    function createRecordDialog: TDlgRecord;
    procedure ResetCaption;
    procedure SetSectorStatsSize(value: Cardinal);
    property SectorStatsSize: Cardinal read FSectorStatsSize write SetSectorStatsSize;
    procedure SetStopPlay(value: Cardinal);
  public
    SettingsDialog: TSettingsDialog;
    DlgRecord: TDlgRecord;
    SelectDirDlg: TSelectDirDlg;
    JumpToDlg: TJumpToDlg;
    DAEForm: TDAEForm;
    SectorSize: Cardinal;
    OutDevice: DWORD;
    WaveFile: TWaveFile;
    WavePlayOffset: Cardinal;
    WaveBuffers: TWaveBuffers;
    WaveOut: HWaveOut;
    WaveIn: HWaveIn;
    WaveName: string;
    WaveFileName: string;
    function MakeTime(t: Cardinal): string;
    procedure CopyWaveFormat(Fmt: PWaveFormatEx);
    procedure SetLoadedFile(FileName: string; StatsHandle: HGLOBAL; StatsSize, aSectorSize, RecordedSize: Cardinal; Fmt: PWaveFormatEx);
    procedure LoadFile(FileName: string);
    procedure AppendFile(FileName: string);
    procedure LoadCueSheet(const FileName: string);
    procedure CloseFile;
    procedure CloseFileForAppend;
    procedure InitTracks;
    procedure SeekTo(Value: Cardinal);
    procedure JumpBy(distance: Integer);
    function GetWaveOutPos: Cardinal;
    function GetWaveInPos: Cardinal;
    function SendBuffer(Buf: PWaveHdr): Boolean;
    procedure InsertSplit(Start, Size: Cardinal; At: Integer);
    procedure RenumberSplits;
    procedure AddSplitAt(Position: Cardinal);
    procedure ApplySettings;
    procedure MarkAllTracks(Checked: Boolean);
    procedure StartPlayback;
    procedure StartRecording;
    procedure AutoSplit(MinLevel, MaxLevel: SmallInt; Time, RelPos: Cardinal);
    procedure TimeSplit(StartAt, Every: Cardinal);
    procedure TimeSplitFuzzy(StartAt, Every, fuzz: Cardinal);
    property WaveStatus: TWaveStatus read FWaveStatus write SetWaveStatus;
    property IsPlaying: boolean read GetIsPlaying;
    property IsRecording: boolean read GetIsRecording;
    property CursorPos: Cardinal read FCursorPos write SetCursorPos;
    property MarkerPos: Cardinal read FMarkerPos write SetMarkerPos;
    property ZoomOffset: Cardinal read FZoomOffset write SetZoomOffset;
    property WaveSpeed: Boolean read FWaveSpeed write SetWaveSpeed;
    property HasClipped: Boolean read FHasClipped write SetHasClipped;
    property ClipWarning: Boolean read FClipWarning write SetClipWarning;
    property HasSignal: Boolean read FHasSignal write SetHasSignal;
    property FlatButtons: Boolean read GetFlatButtons write SetFlatButtons;
    property NiceColor[Index: Integer]: Integer read GetNiceColor;
    property TimeDisplayMode: Integer read FTimeDisplayMode write SetTimeDisplayMode;
    property ShortcutsEnabled: Boolean read GetShortcutsEnabled;
    property HasChanged: boolean read FHasChanged write FHaschanged;
    property WaveFormat: PWaveFormatEx read FWaveFormat;
    property StopPlay: Cardinal read FStopPlay write SetStopPlay;
  end;

var MainForm: TMainForm;

implementation

uses Registry, License, AboutFull, AboutShare,
{$IFDEF DISP}
  DisplayFormTest,
{$ENDIF}
  FormLocation, Properties, AuditionFrm;

{$R *.DFM}

resourcestring
  HelpStr = '.HLP';
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
  LoadStr = 'Loading ';
  AllocMemStr = 'Error occurred:'#10'%s'#10'while attempting to allocate memory.';
  UnSavedStr = 'There are unsaved changes. Press OK to ignore and continue, Cancel to abort and return.';
  NoSeekStr = 'Seek failed on WAV file.';
  NoWaveInStr = 'Cannot close wavein?';
  NoPosStr = 'Error seeking current position in wave file.';
  NoRecStr = 'Unable to create recording thread.';
  NoCallBackStr = 'Internal error: Unknown CALLBACK mode.';
  NoDataBlocksStr = 'Unable to add wave data blocks.';
  RecStopStr = 'Recording stopped because of error (disk full?)';
  SureLoseStr = 'Are you sure you want to lose all your split points and start over?';
  NoWaveKeyStr = 'FILE line does not have a WAVE keyword.';
  OpenStr = 'Open associated file: "%s"?';
  NoIndexStr = 'Line %s: Invalid INDEX line, time format too short.';
  ElapsedStr = 'Elapsed time';
  RemainStr = 'Remaining';
  TrackTimeStr = 'Track time';
  TrackRemStr = 'Track remain';
  RecordStr = 'Recording';
  MacroStr = 'DDE Macro Received: "%s"';
  OnlySingleStr = 'You can only drop a single file on CDWave.';
  NoTimeStr = 'Time format not understood (minutes or HH:MM).';
  NeedTimeStr = '/T (duration) needs time, e.g. /T1:30';
  IgnoreFileStr = 'Ignored file parameter: ';
  NoRecFileStr = 'Record (/R) supplied, but no file to record to.';
  StartRecStr = 'Start recording to: ';
  NoComLineStr = 'Error parsing commandline: ';
  ScrollStr = 'Scroll: ';
  UnregStr = ' - Unregistered';
  UnknownExtStr = ' Unknown file extension';
  WaveErrStr = 'Your audio/waveform driver reported the following error';
  WhileOpeneningStr = 'While opening audio device';
  WhilePrepareStr = 'While preparing a buffer for playback';
  strSilence = '(unused) ';

const
  FileStr  = 'FILE "%s" WAVE';
  TrackStr = '  TRACK %s AUDIO';
  TitleStr = '    TITLE "%s"';
  IndexStr = '    INDEX 01 ';

function TwoDigit(n: Integer): string;
begin
  Result := Format('%.2d', [n]);
end;

function TMainForm.StrToTime(s: String): Cardinal;
var p, c, r: Cardinal;
    nAvgBytesPerSec: Cardinal;
begin
  p := Pos(':', s);
  nAvgBytesPerSec := WaveFormat.nAvgBytesPerSec;
  if p > 0 then begin
    val(copy(s, 1, p-1), r, c);
    Result := r * 60 * nAvgBytesPerSec;
    Delete(s,1,p);
    p := Pos(':', s);
    if p > 0 then begin
      val(copy(s, 1, p-1), r, c);
      Result := Result + r * nAvgBytesPerSec;
      Delete(s,1,p);
      val(s, r, c);
      Result := Result + SectorSize * r;
    end else begin
      val(s, r, c);
      Result := Result + r * nAvgBytesPerSec;
    end;
  end else begin
    val(s, r, c);
    Result := SectorSize * r;
  end;
end;

function TMainForm.MakeTime(t: Cardinal): string;
var m, s, f: Cardinal;
begin
  s := t div WaveFormat.nAvgBytesPerSec; { in seconds }
  t := t div WaveFormat.nBlockAlign;     { in samples }
  f := ((t mod WaveFormat.nSamplesPerSec) * 75) div WaveFormat.nSamplesPerSec;
  m := s div 60;
  s := s mod 60;
  Result := Format('%.2d:%.2d.%.2d', [m, s, f]);
end;

constructor TWaveOutException.Create;
var Txt: array[0..255] of Char;
begin
  waveOutGetErrorText(ErrCode, Txt, sizeof(Txt));
  inherited Create(WaveErrStr + ':'#13#10 + string(Txt) +
        #13#10 + context);
end;

constructor TWaveInException.Create(ErrCode: MMResult);
var Txt: array[0..255] of Char;
begin
  waveInGetErrorText(ErrCode, Txt, sizeof(Txt));
  inherited Create(WaveErrStr + ':'#13#10 + string(Txt));
end;

procedure TMainForm.FormCreate(Sender: TObject);
var i: Integer;
  Reg: TRegistry;
begin
  RegInfo := TRegInfo.Create($A3B75123, $CD3F5681, $0918F543, CDWAVRegKey);
  ResetCaption;
  DragAcceptFiles(Handle, True);
  OutDevice := WAVE_MAPPER;
  CopyWaveFormat(@RedBookFormat);
  Application.OnHint := ShowHint;
  Application.HelpFile := ChangeFileExt(ParamStr(0), HelpStr);
  WaveStatus := wsNone;
  WaveZoomMBD := False;
  for i := 0 to NumBuffers-1 do
  begin
    WaveBuffers[i] := AllocWave(BufferSize);
  end;
  SettingsDialog := TSettingsDialog.Create(self);
  ApplySettings;
  Reg := TRegistry.Create;
  with Reg do
  try
    if OpenKey(CDWAVRegKey, False) then
    begin
      OpenDialog.InitialDir := reg.ReadString('OpenDir');
    end;
  except
  end;
  DlgRecord := createRecordDialog;
{$IFDEF DISP}
  DisplayForm := TDisplayForm.Create(self);
  Displayform.Show;
{$ENDIF}
end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  StatusLine.Panels[1].Text := Application.Hint;
end;

procedure TMainForm.FileNew(Sender: TObject);
begin
  CloseFile;
  pbWave.Repaint;
  pbTimeRange.Repaint;
end;

procedure TMainForm.FileOpen(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    LoadFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.FileSave(Sender: TObject);
var At: Integer;
    ToWrite: Integer;
    LoadingDlg: TLoadingDlg;
begin
  ToWrite := 0;
  with TrackList do
  for At := 0 to Items.Count-1 do
    if Items[At].Checked then inc(ToWrite, PSplitInfo(Items[At].Data).Size + 54);
  if SelectDirDlg = nil then
     SelectDirDlg := TSelectDirDlg.Create(self);
  SelectDirDlg.Required := ToWrite;
  SelectDirDlg.SourceWaveFormat := WaveFormat;
  SelectDirDlg.NewFolderName := WaveName;
  if SelectDirDlg.ShowModal = mrOk then
  begin
    BtnStopClick(nil);
    LoadingDlg := TLoadingDlg.Create(self);
    try
      LoadingDlg.SaveTracks(
        TrackList.Items,
        WaveFile,
        SelectDirdlg.Directory,
        SelectDirDlg.SourceWaveFormat,
        SelectDirDlg.ConvertToFormat);
    finally
      LoadingDlg.Free;
    end;
    HasChanged := False;
  end;
end;

procedure TMainForm.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HelpAbout(Sender: TObject);
begin
  with createAboutBox do
  try
    if ShowModal = mrRetry then ResetCaption;
  finally
    Free;
  end;
end;

procedure TMainForm.SetLoadedFile;
procedure Err(Msg: string);
begin
  FreeAndNil(WaveFile);
  WaveStatus := wsNone;
  raise Exception.Create(Msg);
end;
begin
  WaveFile := WaveIO.OpenFile(FileName);
  if RecordedSize <> WaveFile.WaveSize then
  begin
    OutputDebugString('Recorded vs. file size mismatch - re-opening');
    FreeAndNil(WaveFile);
    GlobalFree(StatsHandle);
    LoadFile(FileName); // Size mismatch.
  end else begin
    CopyWaveFormat(Fmt);
    SectorSize := ASectorSize;
    SectorStatsHandle := StatsHandle;
    SectorStatsList := PSectorStatsList(GlobalLock(SectorStatsHandle));
    if SectorStatsList = nil then
      raise EOSError.CreateFmt(
          LockMemStr,
          [SysErrorMessage(GetLastError)]);
    SectorStatsSize := StatsSize;
    WaveStatus := wsOpen;
    WaveFileName := FileName;
    WaveName := ExtractFileName(FileName);
    ResetCaption;
    InitTracks;
    lDesExtra.Caption := TotTimeStr;
    lExtra.Caption := MakeTime(WaveFile.WaveSize);
    RecalcWaveForm;
    RecalcZoomWaveForm;
    pbTimeRange.Repaint;
    pbWave.Repaint;
{$IFDEF DISP}
    DisplayForm.PaintBox1.Repaint;
{$ENDIF}
  end;
end;

procedure TMainForm.LoadFile;
procedure Err(Msg: string);
begin
  raise Exception.Create(Msg);
end;
var
   LoadingDlg:   TLoadingDlg;
   Size: Cardinal;
begin
  CloseFile;
  WaveFile := WaveIO.OpenFile(FileName);
  try
    CopyWaveFormat(@WaveFile.PCMWaveFormat);
    StatusLine.Panels[1].Text := LoadStr + IntToStr(WaveFile.WaveSize shr 10) + 'kb';
    Size := ((WaveFile.WaveSize-1) div SectorSize) + 1;
    SectorStatsHandle := GlobalAlloc(GHND, Size * SizeOf(TSectorStats));
    if SectorStatsHandle = 0 then
      raise EOSError.CreateFmt(
        AllocMemStr,
        [SysErrorMessage(GetLastError)]);
    try
      SectorStatsList := PSectorStatsList(GlobalLock(SectorStatsHandle));
      if SectorStatsList = nil then
        raise EOSError.CreateFmt(
          LockMemStr,
          [SysErrorMessage(GetLastError)]);
      SectorStatsSize := Size;
      self.Cursor := crHourGlass;
      LoadingDlg := TLoadingDlg.Create(self);
      try
        try
          // @@ TODO @@
          LoadingDlg.LoadFile(WaveFile, WaveFile.WaveSize,
                              SectorStatsList, Size, SectorSize,
                              WaveFormat.wBitsPerSample);
        except
          GlobalUnlock(SectorStatsHandle);
          SectorStatsList := nil;
          SectorStatsSize := 0;
          SectorStatsHandle := GlobalFree(SectorStatsHandle);
          raise;
        end;
      finally
        LoadingDlg.Free;
        Screen.Cursor := crDefault;
      end;
      WaveStatus := wsOpen;
      WaveFileName := FileName;
      WaveName := ExtractFileName(FileName);
      ResetCaption;
      InitTracks;
      lDesExtra.Caption := TotTimeStr;
      lExtra.Caption := MakeTime(WaveFile.WaveSize);
      RecalcWaveForm;
      RecalcZoomWaveForm;
      pbTimeRange.Repaint;
      pbWave.Repaint;
{$IFDEF DISP}
      DisplayForm.PaintBox1.Repaint;
{$ENDIF}
    except
      SectorStatsHandle := GlobalFree(SectorStatsHandle);
      raise;
    end;
  except
    FreeAndNil(WaveFile);
    WaveStatus := wsNone;
    raise;
  end;
end;

procedure TMainForm.AppendFile;
begin
end;

procedure TMainForm.CloseFileForAppend;
begin
  if WaveStatus > wsNone then
  begin
    if HasChanged and ConfChanges then
    begin
      if MessageDlg(UnSavedStr,
         mtConfirmation, mbOKCancel, 0) <> mrOK then
        Abort;
    end;
    if WaveStatus > wsOpen then BtnStopClick(nil);
    if SectorStatsList <> nil then
    begin
      GlobalUnlock(SectorStatsHandle);
      SectorStatsList := nil;
    end;
    if WaveForm <> nil then
    begin
      FreeMem(WaveForm, WaveFormSize * sizeof(TWaveFormRec));
      WaveForm := nil;
      WaveFormSize := 0;
    end;
    if ZoomWaveForm <> nil then
    begin
      FreeMem(ZoomWaveForm, ZoomWaveFormSize * sizeof(TWaveFormRec));
      ZoomWaveForm := nil;
      ZoomWaveFormSize := 0;
    end;
    FreeAndNil(WaveFile);
    CursorPos := 0;
    MarkerPos := 0;
    ZoomOffset := 0;
    HasChanged := False;
    WaveStatus := wsNone;
  end;
end;

procedure TMainForm.CloseFile;
begin
  if WaveStatus > wsNone then
  begin
    if HasChanged and ConfChanges then
    begin
      if MessageDlg(UnSavedStr,
         mtConfirmation, mbOKCancel, 0) <> mrOK then
        Abort;
    end;
    if WaveStatus > wsOpen then BtnStopClick(nil);
    if SectorStatsList <> nil then
    begin
      GlobalUnlock(SectorStatsHandle);
      SectorStatsList := nil;
      SectorStatsSize := 0;
      SectorStatsHandle := GlobalFree(SectorStatsHandle);
    end;
    if WaveForm <> nil then
    begin
      FreeMem(WaveForm, WaveFormSize * sizeof(TWaveFormRec));
      WaveForm := nil;
      WaveFormSize := 0;
    end;
    if ZoomWaveForm <> nil then
    begin
      FreeMem(ZoomWaveForm, ZoomWaveFormSize * sizeof(TWaveFormRec));
      ZoomWaveForm := nil;
      ZoomWaveFormSize := 0;
    end;
    FreeAndNil(WaveFile);
    WaveStatus := wsNone;
    TrackList.Items.Clear;
    WaveName := '';
    ResetCaption;
    CursorPos := 0;
    MarkerPos := 0;
    ZoomOffset := 0;
    WaveFileName := '';
    lDesExtra.Caption := '';
    lExtra.Caption := MakeTime(0);
{    pbTimeRange.Repaint;}
    HasChanged := False;
{$IFDEF DISP}
    DisplayForm.DoneFile;
{$ENDIF}
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    BtnStopClick(nil);
    CloseFile;
  except // aborted
    on e: Exception do
    begin
      MessageDlg(e.Message, mtError, [mbOk], 0);
      Action := caNone;
    end;
  end;
end;

procedure TMainForm.InitTracks;
var
  Item: TListItem;
  p: Integer;
  Info: PSplitInfo;
begin
  TrackList.Items.BeginUpdate;
  try
    TrackList.Items.Clear;
    Item := TrackList.Items.Add;
    p := Pos('.', WaveName);
    if (p>0) then WaveName := Copy(WaveName, 1, p-1);
    Item.Caption := WaveName;
    Item.SubItems.Add('00:00.00');
    Item.ImageIndex := -1;
    Item.Checked := True;
    new(Info);
    Info.Start := 0;
    Info.KeepName := False;
    Info.Color := Colors[0];
    Item.Data := Info;
    if assigned(WaveFile) then begin
      Item.SubItems.Add(MakeTime(WaveFile.WaveSize));
      Info.Size := WaveFile.WaveSize;
    end else begin
      Item.SubItems.Add('--:--.--');
      Info.Size := 0;
    end;
    ShowSelectedTime;
  finally
    TrackList.Items.EndUpdate;
  end;
end;

procedure TMainForm.SetWaveStatus(Value: TWaveStatus);
var
  IsOpen,
  IsPlaying,
  IsRecording: Boolean;
begin
 if Value <> WaveStatus then
 begin
  FWaveStatus := Value;
  IsOpen := (Value >= wsOpen) and (Value < wsRecord);
  IsPlaying := (Value >= wsPlaying) and (Value < wsRecord);
  IsRecording := Value = wsRecord;
  // Timer
  PositionTimer.Enabled := Value = wsPlaying;
  // File
  FileNewAction.Enabled := not IsRecording;
  FileOpenAction.Enabled := not IsRecording;
  FileAppendAction.Enabled := IsOpen;
  FileSaveAction.Enabled := IsOpen;
  FilePropertiesAction.Enabled := IsOpen;
  // Actions
  AutoSplitAction.Enabled := IsOpen;
  TimeSplitAction.Enabled := IsOpen;
  ActionClearAllSplits.Enabled := IsOpen;
  ActionRemoveSmallSplits.Enabled := IsOpen;
  SaveCueSheet1.Enabled := IsOpen;
  JumpTo.Enabled := IsOpen;
  pExtra.Visible := IsOpen;
  //Play.Enabled := (Value >= wsOpen) and not IsRecording;
  RecordAction.Enabled := (Value <= wsOpen) or IsRecording;
  SpinButton1.Enabled := IsOpen;
  SplitAction.Enabled := IsOpen or IsRecording;
  Pause.Enabled := IsPlaying;
  Back.Enabled := IsPlaying;
  Skip.Enabled := IsPlaying;
  Stop.Enabled := IsPlaying or IsRecording;
  FastForward.Enabled := IsPlaying;
  Pause.Checked := Value = wsPaused;
  //Play.Checked := IsPlaying;
  RecordAction.Checked := IsRecording;
  VolumePanel.Visible := IsRecording;
{  pbClipLed.Visible := IsRecording;
  pbWarnLed.Visible := IsRecording;
  pbSignalLed.Visible := IsRecording;}
  if IsRecording then begin
    pbTimeRange.Color := clBlack;
    TimeDisplayMode := 4;
  end;
 end;
end;

function TMainForm.GetIsPlaying;
begin
  result := (WaveStatus >= wsPlaying) and (WaveStatus < wsRecord)
end;

function TMainForm.GetIsRecording;
begin
  Result := (WaveStatus >= wsRecord);
end;

procedure TMainform.SetCursorPos;
var
  TopX, TopOldX, BottomX, BottomOldX: Integer;
  At: Integer;
  WaveSize: Cardinal;
begin
  if assigned(WaveFile) then
    WaveSize := WaveFile.WaveSize
  else
    WaveSize := 0;
  if (WaveSize>0) and (Value >= WaveSize) then
    Value := SectorSize * ((WaveSize - 1) div SectorSize);
  if Value <> FCursorPos then
  begin
    if WaveStatus < wsRecord then
    begin
      BottomOldX := (FCursorPos div SectorSize) - ZoomOffset;
      if (BottomOldX >= 0) and (BottomOldX < pbWave.Width) then
        ZoomWavePaintLine(BottomOldX, clBlack);
      BottomX := (Value div SectorSize) - ZoomOffset;
      if (BottomX >= 0) and (BottomX < pbWave.Width) then
      with pbWave.Canvas do begin
        Lock;
        Pen.Color := clWhite;
        MoveTo(BottomX, 0);
        LineTo(BottomX, pbWave.Height);
        Unlock;
      end else
        if (WaveStatus >= wsPlaying) then
        begin
          if BottomFollowsPlayback then
            ZoomOffset := (Value div SectorSize);
        end else begin
          at := Integer(Value div SectorSize) - (pbWave.Width div 2);
          if (at < 0) then ZoomOffset := 0 else ZoomOffset := Cardinal(at);
        end;
    end;
    if WaveSize > 0 then
    begin
      TopOldX := (int64(FCursorPos) * pbTimeRange.Width) div WaveSize;
      TopX := (int64(Value) * pbTimeRange.Width) div WaveSize;
      if TopX <> TopOldX then begin
        with pbTimeRange.Canvas do
        begin
          Lock;
          WavePaintLine(TopOldX);
          Pen.Color := clWhite;
          MoveTo(TopX, 0);
          LineTo(TopX, Height);
          UnLock;
        end;
      end;
    end;
    FCursorPos := Value;
  end;
  UpdateTimeDisplay;
end;

procedure TMainForm.SetMarkerPos;
var BottomOldX, BottomX: integer;
    WaveSize: Cardinal;
begin
  if assigned(WaveFile) then
    WaveSize := WaveFile.WaveSize
  else
    WaveSize := 0;
  if (WaveSize>0) and (Value >= WaveSize) then
      Value := SectorSize * ((WaveSize - 1) div SectorSize);
  if Value <> FMarkerPos then
  begin
    lMarkerPos.Caption := MakeTime(Value);
    BottomOldX := (FMarkerPos div SectorSize) - ZoomOffset;
    BottomX := (Value div SectorSize) - ZoomOffset;
    FMarkerPos := Value;
    if (BottomOldX >= 0) and (BottomOldX < pbWave.Width) then
        ZoomWavePaintLine(BottomOldX, clBlack);
    if (BottomX >= 0) and (BottomX < pbWave.Width) then
        ZoomWavePaintLine(BottomX, clBlack);
    if (WaveStatus < wsPlaying) and (WaveStatus >= wsOpen) then
        CursorPos := Value;
  end;
end;

procedure TMainForm.pbWaveMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    WaveZoomMBD := True;
    WaveZoomLastMove := -1;
    pbWaveMouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TMainForm.pbWaveMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var at: Cardinal;
begin
  if x < 0 then x := 0;
  if (WaveStatus >= wsOpen) and (WaveStatus < wsRecord) and (x <> WaveZoomLastMove) then
  with Sender as TPaintBox do
  begin
    WaveZoomLastMove := x;
    at := SectorSize * (ZoomOffset + Cardinal(x));
    if at >= WaveFile.WaveSize then
        Exit;
    StatusLine.Panels[0].Text := MakeTime(at);
    if WaveZoomMBD then
    begin
      CursorPos := at;
      MarkerPos := at;
      if IsPlaying then SeekTo(at);
    end;
  end;
end;

procedure TMainForm.pbWaveMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    WaveZoomMBD := False;
end;

procedure TMainForm.SpinButton1DownClick(Sender: TObject);
begin
  if MarkerPos > SectorSize then
    MarkerPos := MarkerPos - SectorSize
  else
    MarkerPos := 0;
end;

procedure TMainForm.SpinButton1UpClick(Sender: TObject);
begin
  MarkerPos := MarkerPos + SectorSize;
end;

function TMainForm.FindTrack(At: Cardinal): Integer;
begin
  Result := 0;
  with TrackList.Items do
  begin
    while (Result < Count) and (PSplitInfo(Item[Result].Data).Start <= At) do inc(Result);
    dec(Result);
  end;
end;

procedure AddItem(view: TListView; const caption: string);
var Item: TListItem;
begin
  Item := view.Items.Add;
  Item.Caption := caption;
end;

procedure TMainForm.BtnSplitClick(Sender: TObject);
var At: Cardinal;
    info: PSplitInfo;
begin
  if IsRecording then begin
    At := GetWaveInPos - RecOffset;
    TrackList.Items.BeginUpdate;
    with TrackList.Items do
    try
      At := SectorSize * (At div SectorSize);
      info := PSplitInfo(Item[Count-1].Data);
      info.Size := At - info.Start;
      Item[Count-1].SubItems[1] := MakeTime(info.Size);
      InsertSplit(At, 0, Count);
      Item[Count-1].MakeVisible(False);
    finally
      EndUpdate;
    end;
    //WaveSize := At + WaveFormat.nBlockAlign;
    //AddSplitAt(At);
  end else begin
    if IsPlaying and not SplitAtMarker then
      MarkerPos := SectorSize * (GetWaveOutPos div SectorSize);
    AddSplitAt(MarkerPos);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var i: Integer;
    reg: TRegistry;
    s: TFileName;
begin
  HasChanged := False; // Prevent abort action
  CloseFile;
  for i := 0 to NumBuffers-1 do
  begin
    FreeWave(WaveBuffers[i]);
  end;
  s := OpenDialog.FileName;
  if s <> '' then
  begin
    s := ExtractFilePath(s);
    Reg := TRegistry.Create;
    with Reg do
    try
      if OpenKey(CDWAVRegKey, True) then
      begin
        reg.WriteString('OpenDir', s);
      end;
    except
    end;
  end;
  if assigned(WaveFormat) then FreeMem(WaveFormat);
end;

procedure TMainForm.SeekTo;
var i: Integer;
    wasPaused: boolean;
begin
  WavePlayOffset := WaveFormat.nBlockAlign * (Value div WaveFormat.nBlockAlign);
  WaveFile.SetFilePos(WavePlayOffset);
  wasPaused := WaveStatus = wsPaused;
  WaveStatus := wsOpen;
  waveOutReset(WaveOut);
  // @@
  // When not using callback, clear WOM_DONE messages from Msg cue.
  // Application.ProcessMessages;
  if waveOutPause(WaveOut)<>0 then
     WaveStatus := wsPlaying
  else
     WaveStatus := wsPaused;
  for i := 0 to NumBuffers-1 do
  begin
    if WaveBuffers[i].dwBytesRecorded = 0 then
       SendBuffer(WaveBuffers[i]);
  end;
  if not wasPaused then
  begin
    if waveOutRestart(WaveOut) = 0 then
      WaveStatus := wsPlaying;
  end;
  if TrackList.Items.Count <> 0 then
    TrackList.Items[FindTrack(value)].ImageIndex := 0;
end;

{procedure WaveOutCallback(WaveOut: HWAVEOUT; wMsg: UINT;
                          dwInstance, dwParam1, dwParam2: DWORD); stdcall;
begin
  if wMsg = WOM_DONE then
  begin
    PostMessage(dwInstance, MM_WOM_DONE, WaveOut, dwParam1);
  end;
end;}


procedure waveOutProc(hwo: HWaveOut; uMsg: UINT;
           frm: TMainForm {dwInstance}; dwParam1, dwParam2: DWORD); stdcall;
begin
  case uMsg of
    WOM_DONE:
      begin
        PWaveHdr(dwParam1).dwBytesRecorded := 0;
        if frm.IsPlaying then
          PostMessage(frm.Handle, MM_WOM_DONE, hwo, dwParam1);
      end;
//    WOM_OPEN:
//      OutputDebugString('WOM_OPEN');
//    WOM_CLOSE:
//      OutputDebugString('WOM_CLOSE');
  end;
end;

procedure TMainForm.StartPlayback;
var
  i, Err: Integer;
begin
  if WaveStatus = wsOpen then
  begin
      //Err := WaveOutOpen(@WaveOut, OutDevice, WaveFormat, Handle, 0, CallBack_Window);
      Err := WaveOutOpen(@WaveOut, OutDevice, WaveFormat, DWORD(@waveOutProc),
                         Cardinal(self), CALLBACK_FUNCTION);
      if Err<>0 then
          raise TWaveOutException.Create(Err, WhileOpeneningStr);
      try
        for i := 0 to NumBuffers-1 do begin
          Err := WaveOutPrepareHeader(WaveOut, WaveBuffers[i], sizeof(TWaveHdr));
          if Err <> 0 then raise TWaveOutException.Create(Err, WhilePrepareStr);
        end;
        WavePlayOffset := WaveFormat.nBlockAlign * (CursorPos div WaveFormat.nBlockAlign);
        WaveFile.SetFilePos(WavePlayOffset);
        for i := 0 to NumBuffers-1 do begin
          if WaveBuffers[i].dwBytesRecorded = 0 then
            SendBuffer(WaveBuffers[i]);
        end;
      except
        WaveOutClose(WaveOut);
        WaveOut := HWaveOut(INVALID_HANDLE_VALUE);
        raise;
      end;
      WaveStatus := wsPlaying;
  end;
end;

procedure TMainForm.BtnPlayClick(Sender: TObject);
begin
  StopPlay := 0;
  case WaveStatus of
    wsPlaying:
      SeekTo(MarkerPos);
    wsPaused:
      BtnPauseClick(Sender);
    else
      StartPlayback;
  end;
end;

procedure TMainForm.BtnStopClick(Sender: TObject);
var
  i: Integer;
  OldStatus: TWaveStatus;
  Handle: HGLOBAL;
  RecSplitPos: array of Cardinal;
  RecSplitName: array of string;
  RecSplitCount: Integer;
  info: PSplitInfo;
begin
  StopPlay := 0;
  OldStatus := WaveStatus;
  if OldStatus >= wsOpen then
  begin
    WaveStatus := wsOpen;
    case OldStatus of
      wsPaused,
      wsPlaying: begin
                  WaveOutReset(WaveOut);
                  for i := 0 to NumBuffers-1 do
                    WaveOutUnprepareHeader(WaveOut, WaveBuffers[i], sizeof(TWAVEHDR));
                  WaveOutClose(WaveOut);
                  WaveSpeed := False;
                  if RewindOnStop then
                    CursorPos := MarkerPos;
                 end;
      wsRecord:  begin
                  if PriorityBoost > 0 then
                    SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
                  if RecThread <> nil then
                  begin
                    RecThread.Stopped := True;
                    WaveInReset(WaveIn);
                    if WaveInClose(WaveIn) <> 0 then begin
                      RecThread.Quit;
                      messageDlg(NoWaveInStr, mtError, [mbOK], 0);
                    end else
                    begin
                      if RecCallbackMode = rcmWindow then RecThread.Quit;
                      RecThread.WaitFor;
                    end;
                    for i:=0 to 3 do if pmPositionMode.Items[i].Checked then
                      TimeDisplayMode := i;
                    WaveStatus := wsNone;
                    CursorPos := 0;
                    Handle := RecThread.SectorStatsHandle;
                    if (RecSettings.RecordMode in [rmFile, rmAppend]) and
                       (RecSettings.FileName <> '') and
                       (RecThread.RecordedBytes <> 0) then
                    begin
                      RecSplitCount := TrackList.Items.Count;
                      SetLength(RecSplitPos, RecSplitCount);
                      SetLength(RecSplitName, RecSplitCount);
                      for i := 0 to RecSplitCount-1 do
                      with TrackList.Items do begin
                        info := PSplitInfo(Item[i].Data);
                        RecSplitPos[i] := info.Start;
                        if info.KeepName then
                          RecSplitName[i] := Item[i].Caption
                        else
                          RecSplitName[i] := '';
                      end;
                      if Handle = 0 then
                        LoadFile(RecSettings.FileName)
                      else
                        SetLoadedFile(
                          RecSettings.FileName,
                          Handle,
                          RecThread.SectorStatsSize,
                          RecThread.SectorSize,
                          RecThread.RecordedBytes,
                          RecThread.WaveFormat);
                      try
                        for i := 1 to RecSplitCount-1 do
                          AddSplitAt(RecSplitPos[i]);
                        for i := 0 to RecSplitCount-1 do
                        begin
                          if RecSplitName[i] <> '' then
                          begin
                            TrackList.Items[i].Caption := RecSplitName[i];
                            PSplitInfo(TrackList.Items[i].Data).KeepName := True;
                          end;
                        end;
                      except
                        on e: Exception do
                          Application.ShowException(e);
                      end;
                      SetLength(RecSplitPos, 0);
                      SetLength(RecSplitName, 0);
                      HasChanged := RecSplitCount > 0;
                    end else begin
                      if Handle <> 0 then GlobalFree(Handle);
                    end;
                    RecThread.Free;
                    RecThread := nil;
                  end;
                  if QuitAfterRecording then Close;
               end;
    end;
  end;
end;

procedure TMainForm.womDone;
begin
  // Called when (should be) playing and buffer is done
  // We set dwBytesRecorded to zero immediately on callback. If the
  // buffer was somehow filled between the callback and the POST
  // ignore this message.
  if IsPlaying and (PWaveHdr(Msg.lParam).dwBytesRecorded = 0) then
  begin
    SendBuffer(PWaveHdr(Msg.lParam));
  end;
end;

function TMainForm.SendBuffer(Buf: PWaveHdr): Boolean;
var NumRead: Integer;
    Limit, CurPos, ToRead, WaveSize: Cardinal;
begin
with Buf^ do begin
  CurPos := WaveFile.FilePos;
  if assigned(WaveFile) then
    WaveSize := WaveFile.WaveSize
  else
    WaveSize := 0;
  if FStopPlay <> 0 then
    Limit := FStopPlay
  else
    Limit := WaveSize;
  if Limit - CurPos < BufferSize then
    ToRead := Limit - CurPos
  else
    ToRead := BufferSize;
  if WaveSpeed then
  begin
    NumRead := WaveFile.Read(PChar(FastBuffer), BufferSize*2);
    if NumRead > 0 then
    begin
      NumRead := NumRead shr 1;
      FastCopy(Buf^.lpData, FastBuffer, NumRead shr 2);
    end;
  end else
    NumRead := WaveFile.Read(lpData, ToRead);
  if Numread > 0 then
  begin
    {dwFlags := dwFlags and not WHDR_DONE;}
    dwBufferLength := NumRead;
    WaveOutWrite(WaveOut, Buf, SizeOf(TWaveHdr));
    Result := False;
  end else begin
    dwBufferLength := 0;
    Result := True;
  end;
end;
end;

{* Used only in "max compatibility" mode *}
procedure TMainForm.wimOpen;
begin
  if RecThread <> nil then
     PostThreadMessage(RecThread.ThreadID, MM_WIM_OPEN, Msg.wParam, Msg.lParam);
end;
procedure TMainForm.wimClose;
begin
  if RecThread <> nil then
     PostThreadMessage(RecThread.ThreadID, MM_WIM_CLOSE, Msg.wParam, Msg.lParam);
end;
procedure TMainForm.wimData;
begin
  if RecThread <> nil then
     PostThreadMessage(RecThread.ThreadID, MM_WIM_DATA, Msg.wParam, Msg.lParam);
end;

function TMainForm.GetWaveOutPos;
var
  mmt: TMMTime;
begin
  Result := 0;
  if WaveStatus > wsOpen
  then begin
    mmt.wType := TIME_BYTES;
    if waveOutGetPosition(WaveOut, @mmt, sizeof(mmt)) = 0 then
      if WaveSpeed then
        Result := mmt.cb shl 1 + WavePlayOffset
      else
        Result := mmt.cb + WavePlayOffset;
  end;
end;

function TMainForm.GetWaveInPos;
var
  mmt: TMMTime;
begin
  Result := 0;
  if WaveStatus = wsRecord
  then begin
    mmt.wType := TIME_BYTES;
    if waveInGetPosition(WaveIn, @mmt, sizeof(mmt)) = 0 then
      Result := mmt.cb;
  end;
end;

procedure TMainForm.TrackListDeletion(Sender: TObject; Item: TListItem);
begin
  dispose(PSplitInfo(Item.Data));
  HasChanged := True;
  pbTimeRange.Invalidate;
  ShowSelectedTime;
end;

procedure TMainForm.TrackListEdited(Sender: TObject; Item: TListItem;
  var S: String);
begin
  PSplitInfo(Item.Data).KeepName := (s <> '');
end;

procedure TMainForm.WavePaintLine(At: Integer);
var Track: Integer;
begin
  with pbTimeRange do with Canvas do
  begin
    Pen.Color := clBlack;
    MoveTo(At,0); LineTo(At, Height);
    if WaveForm <> nil then begin
      Track := FindTrack((int64(WaveFile.WaveSize) * At) div Width);
      Pen.Color := PSplitInfo(TrackList.Items[Track].Data).Color;
      MoveTo(At, WaveForm[At].Lo);
      LineTo(At, WaveForm[At].Hi+1);
    end;
  end;
end;

procedure TMainForm.ZoomWavePaintLine;
begin
  with pbWave do with Canvas do
  if (ZoomWaveForm <> nil) then begin
    if At = Integer((MarkerPos div SectorSize) - ZoomOffset) then
    begin
      Pen.Color := clRed;
      MoveTo(At, 0);
      LineTo(At, ZoomWaveForm[At].Lo);
      MoveTo(At, ZoomWaveForm[At].Hi+1);
      LineTo(At, Height);
    end else begin
      Pen.Color := BackClr;
      MoveTo(At,0); LineTo(At, Height);
    end;
    Pen.Color := clLime;
    MoveTo(At, ZoomWaveForm[At].Lo);
    LineTo(At, ZoomWaveForm[At].Hi+1);
  end;
end;

procedure TMainForm.pbTimeRangePaint(Sender: TObject);
var i, t: Integer;
    l, h: Cardinal;
    info: PSplitInfo;
    TimeWidth: Integer;
begin
  if WaveStatus >= wsOpen then
  with Sender as TPaintBox do with Canvas do
  begin
    TimeWidth := Width;
    if Cardinal(TimeWidth) <> WaveFormSize then RecalcWaveForm;
    if WaveForm <> nil then
    with TrackList do begin
      l := 0;
      for t := 0 to Items.Count - 1 do
      begin
        info := PSplitInfo(Items[t].Data);
        Pen.Color := info.Color;
        h := (int64(info.Start + info.Size) * TimeWidth) div WaveFile.WaveSize;
        if h >= WaveFormSize then h := WaveFormSize - 1;
        for i := l to h do
        begin
          MoveTo(i, WaveForm[i].Lo);
          LineTo(i, WaveForm[i].Hi+1);
        end;
        l := h+1;
      end;
      l := (int64(CursorPos) * TimeWidth) div WaveFile.WaveSize;
      Pen.Color := clWhite;
      MoveTo(l, 0);
      LineTo(l, Height);
    end;
  end;
end;

{procedure TMainForm.ZoomWavePaintLine;
var i: Integer;
begin
    i := (at div SectorSize) - ZoomOffset;
    if (i >= 0) and (i < Width) then
    begin
      Pen.Color := clRed;
      MoveTo(i, 0);
      if i < Integer(SectorStatsSize) then
      begin
        LineTo(i, ZoomWaveForm[i].Lo);
        MoveTo(i, ZoomWaveForm[i].Hi+1);
      end;
      LineTo(i, Height);
    end;
end;}

procedure TMainForm.pbWavePaint(Sender: TObject);
var i: Integer;
    w: Cardinal;
begin
  if WaveStatus >= wsOpen then
  with Sender as TPaintBox do
  with Canvas do
  begin
    w := Width;
    if (w <> ZoomWaveFormSize) or (Height <> ZoomWaveFormHeight) then RecalcZoomWaveForm;
    if IsRecording then
    begin
      // #@#
    end else begin
      if SectorStatsSize < w then w := SectorStatsSize;
    end;
    if ZoomWaveForm <> nil then begin
      Pen.Color := clLime;
      for i := 0 to w-1 do begin
        MoveTo(i, ZoomWaveForm[i].Lo);
        LineTo(i, ZoomWaveForm[i].Hi+1);
      end;
    end;
   if not IsRecording then
   begin
    i := (CursorPos div SectorSize) - ZoomOffset;
    if (i >= 0) and (i < Width) then
    begin
      Pen.Color := clWhite;
      MoveTo(i, 0);
      LineTo(i, Height);
    end;
    i := (MarkerPos div SectorSize) - ZoomOffset;
    if (i >= 0) and (i < Width) then
    begin
      Pen.Color := clRed;
      MoveTo(i, 0);
      if i < Integer(SectorStatsSize) then
      begin
        LineTo(i, ZoomWaveForm[i].Lo);
        MoveTo(i, ZoomWaveForm[i].Hi+1);
      end;
      LineTo(i, Height);
    end;
   end;
   {limit := Width;
   if Cardinal(width) > SectorStatsSize then limit := SectorStatsSize;
   for i := 1 to TrackList.Items.Count-1 do
   begin
     info := TrackList.Items[i].Data;
     at := (info.Start div SectorSize) - ZoomOffset;
     if (at >= 0) and (at < limit) then
       ZoomWavePaintLine(at, info.Color);
   end;}
  end;
end;

procedure TMainForm.RecalcWaveForm;
var Size, i, j, f, t: Cardinal;
    Half: Integer;
    Min, Max: SmallInt;
begin
  with pbTimeRange do
  if SectorStatsList <> nil then begin
    if (WaveFormSize <> Cardinal(Width)) and (WaveForm <> nil) then
    begin
      FreeMem(WaveForm, WaveFormSize * sizeof(TWaveFormRec));
      WaveForm := nil;
    end;
    WaveFormSize := Width;
    Size := SectorStatsSize;
    if Waveform = nil then
        GetMem(WaveForm, WaveFormSize * sizeof(TWaveFormRec));
    Half := Height shr 1;
    t := 0;
    if WaveFormSize > Size then
    for i := 0 to Size-1 do
    begin
      f := t;
      t := (WaveFormSize * i) div Size;
      Min := Half + (Half * SectorStatsList[i].Min) div 32768;
      Max := Half + (Half * SectorStatsList[i].Max) shr 15;
      for j := f to t do
      begin
        WaveForm[j].Lo := Min;
        WaveForm[j].Hi := Max;
      end;
    end
    else
    for i := 0 to WaveFormSize-1 do
    begin
      f := t;
      t := (Size * i) div WaveFormSize;
      min := 0; max := 0;
      if t>f then
      for j := f to t-1 do
      begin
        if SectorStatsList[j].Min < min then min := SectorStatsList[j].Min;
        if SectorStatsList[j].Max > max then max := SectorStatsList[j].Max;
      end;
      WaveForm[i].Lo := Half + (Half*Min) div 32768;
      WaveForm[i].Hi := Half + (Half*Max) shr 15;
    end;
  end;
end;

procedure TMainForm.RecalcZoomWaveForm;
const Middle: TWaveFormRec = (Lo: -1; Hi:-1);
var Half: Integer;
    i, j, Size: Cardinal;
    NewWave: PWaveForm;
begin
  with pbWave do
  if WaveStatus = wsRecord then begin
    GetMem(NewWave, Width * sizeof(TWaveFormRec));
    if Cardinal(Width) > ZoomWaveFormSize then
      i := ZoomWaveFormSize
    else
      i := Width;
    if ZoomWaveForm <> nil then
    begin
      if ZoomWaveFormHeight = Height then
      begin
        QuadMemCopy(NewWave, ZoomWaveForm, (i * sizeof(TWaveFormRec)) shr 2);
        FreeMem(ZoomWaveForm, ZoomWaveFormSize * sizeof(TWaveFormRec));
      end else
        i := 0;
      if i < Cardinal(Width) then
        for j := i to Width-1 do
          NewWave[j] := middle;
    end else
      QuadFill(NewWave, (Width*sizeof(TWaveFormRec)) shr 2, $FFFFFFFF);
    ZoomWaveform := NewWave;
    ZoomWaveFormSize := Width;
    ZoomWaveFormHeight := Height;
  end else if SectorStatsList <> nil then begin
    ZoomWaveFormHeight := Height;
    if (ZoomWaveFormSize <> Cardinal(Width)) then
    begin
      if ZoomWaveForm <> nil then
      begin
        FreeMem(ZoomWaveForm, ZoomWaveFormSize * sizeof(TWaveFormRec));
        ZoomWaveForm := nil;
      end;
      ZoomWaveFormSize := Width;
    end;
    if ZoomWaveForm = nil then
        GetMem(ZoomWaveForm, ZoomWaveFormSize * sizeof(TWaveFormRec));
    Half := Height shr 1;
    Size := ZoomWaveFormSize;
    if Size > 0 then
    begin
      if Size > SectorStatsSize then
        Size := SectorStatsSize;
      for i := 0 to Size-1 do
      begin
        ZoomWaveForm[i].Lo := integer(Half + (Half*SectorStatsList[i+ZoomOffset].Min) div 32768);
        ZoomWaveForm[i].Hi := integer(Half + (Half*SectorStatsList[i+ZoomOffset].Max) shr 15);
      end;
    end;
  end;
end;

procedure TMainForm.BtnRecordClick(Sender: TObject);
begin
  RecordTimer.Enabled := False;
  DlgRecord.AllowAppend := (WaveStatus = wsOpen);
  if (WaveStatus < wsRecord) and
     (DlgRecord.ShowModal = mrOK) then
  begin
    DlgRecord.GetSettings(RecSettings);
    if RecSettings.StartSchedule then
    begin
      TimeDisplayMode := 4;
      RecordTimer.Enabled := True;
    end else
      StartRecording;
  end else
    RecordAction.Checked := False;
end;

procedure TMainForm.StartRecording;
var i: Integer;
    RecordingFormat: TWAVEFORMATEXTENSIBLE;
begin
  RecordTimer.Enabled := False;
  if RecSettings.RecordMode = rmAppend then
  begin
      CloseFileForAppend;
      RecSettings.FileName := WaveFileName;
      RecThread := TWaveRecordThd.Create(
                      Handle, WaveFileName,
                      WaveFormat, RecSettings,
                      SectorStatsHandle,
                      SectorStatsSize);
      SectorStatsHandle := 0;
      SectorStatsSize := 0;
  end else begin
      CloseFile;
      CopyWaveFormat(DlgRecord.WaveFormat);
      RecThread := TWaveRecordThd.Create(
                      Handle, RecSettings.FileName,
                      WaveFormat, RecSettings, 0, 0);
      WaveName := ChangeFileExt(ExtractFileName(RecSettings.FileName), '');
//      InitTracks;
      InsertSplit(0, 0, 0);
  end;
  if RecThread = nil then Raise Exception.Create(NoRecStr);
  WaveUtil.CopyWaveFormat(WaveFormat, @RecordingFormat);
  RecordingFormat.dwChannelMask := 0; // Someone claimed that this must be 0 for recording,
  // See    http://www.tech-archive.net/Archive/Development/microsoft.public.win32.programmer.mmedia/2006-02/msg00003.html
  case RecCallbackMode of
      rcmWindow:
        i := WaveInOpen(@WaveIn, RecSettings.Driver, @RecordingFormat, Handle, 0,
                        CallBack_Window);
      rcmFunction:
        i := WaveInOpen(@WaveIn, RecSettings.Driver, @RecordingFormat,
                        Integer(@WaveInCallback), RecThread.ThreadID,
                        CallBack_Function);
      rcmThread:
        i := WaveInOpen(@WaveIn, RecSettings.Driver, @RecordingFormat,
                        RecThread.ThreadID, 0, CallBack_Thread);
      else
        raise Exception.Create(NoCallBackStr);
  end;
  if i <> 0 then begin
      RecThread.FreeOnTerminate := True;
      RecThread.Quit;
      raise TWaveInException.Create(i);
  end;
  for i := 0 to NumBuffers-1 do begin
      WaveBuffers[i]^.dwBufferLength := BufferSize;
      WaveBuffers[i]^.dwFlags        := 0;
      WaveBuffers[i]^.dwLoops        := 0;
      WaveInPrepareHeader(WaveIn, WaveBuffers[i], sizeof(TWaveHdr));
      if WaveInAddBuffer(WaveIn, WaveBuffers[i], sizeof(TWaveHdr))<>0 then
           MessageDlg(NoDataBlocksStr, mtError, [mbOk], 0);
  end;
  HasClipped := False;
  HasSignal  := False;
  ClipWarning:= False;
  VolumeMeter1.ResetPeak;
  RecOffset  := 0;
  WaveStatus := wsRecord;
  RecPosition:= 0;
  RecSilent  := 0;
  RecalcWaveForm;
  RecalcZoomWaveForm;
  pbWave.Repaint;
  WaveInStart(WaveIn);
  case PriorityBoost of
      1: SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
      2: SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  end;
end;

procedure TMainForm.WMRStarted;
begin
  RecOffset := Msg.LParam;
end;

procedure TMainForm.WMRData;
var RecLevel: TSectorStats;
    Half: Integer;
    OldPos: Integer;
begin
  RecLevel := TSectorStats(msg.lParam);
  VolumeMeter1.Volume := RecLevel.Max;
  if (RecLevel.Min = -32768) or (RecLevel.Max = 32767) then HasClipped := True;
  ClipWarning := (RecLevel.Min < -RecSettings.Loud) or (RecLevel.Max > RecSettings.Loud);
  Hassignal := (msg.wParam and ifSignal) <> 0;
  if ZoomWaveForm <> nil then
  begin
    OldPos := RecPosition;
    inc(RecPosition); if RecPosition >= ZoomWaveFormSize then RecPosition := 0;
    Half := pbWave.Height shr 1;
    ZoomWaveForm[RecPosition].Lo := Half + (Half*RecLevel.Min) div 32768;
    ZoomWaveForm[RecPosition].Hi := Half + (Half*RecLevel.Max) shr 15;
    ZoomWavePaintLine(OldPos, clBlack);
    ZoomWavePaintLine(RecPosition, clWhite);
  end;
  if (msg.wParam and ifCanWrite) <> 0 then
    CursorPos := GetWaveInPos - RecOffset ;
  if (msg.wParam and ifError) <> 0 then
  begin
    BtnStopClick(nil);
    MessageDlg(RecStopStr, mtError, [mbOk], 0);
  end else
    if (msg.wParam and ifStop) <> 0 then BtnStopClick(nil);
end;

procedure TMainForm.BtnFastClick(Sender: TObject);
begin
  if (WaveStatus = wsPlaying) then
  begin
    WaveSpeed := not WaveSpeed;
  end else begin
    FastForward.Checked := WaveSpeed;
  end;
end;

procedure TMainForm.SetWaveSpeed;
var p: Cardinal;
begin
  if Value <> FWaveSpeed then
  begin
    FastForward.Checked := Value;
    if Value then
      new(FastBuffer)
    else if FastBuffer <> nil then
    begin
      dispose(FastBuffer);
      FastBuffer := nil;
    end;
    p := (GetWaveOutPos div WaveFormat.nBlockAlign) * WaveFormat.nBlockAlign;
    FWaveSpeed := Value;
    WaveFile.SetFilePos(p);
    WavePlayOffset := p;
    WaveOutReset(WaveOut);
  end;
end;

procedure TMainForm.BtnPauseClick(Sender: TObject);
begin
  if IsPlaying then
  begin
    if WaveStatus = wsPaused then
    begin
      if waveOutRestart(WaveOut)=0 then WaveStatus := wsPlaying;
    end else begin
      if waveOutPause(WaveOut)=0 then
      begin
        WaveStatus := wsPaused;
        CursorPos := GetWaveOutPos;
      end;
    end;
  end;
end;

procedure TMainForm.pbClipLedPaint(Sender: TObject);
begin
  with Sender as TPaintBox do
  begin
    LedImages.Draw(Canvas, 0, 0, ord(HasClipped));
  end;
end;

procedure TMainForm.pbClipLedClick(Sender: TObject);
begin
   HasClipped := False;
end;

procedure TMainForm.SetHasClipped(Value: Boolean);
begin
  if Value <> FHasClipped then
  begin
    FHasClipped := Value;
    pbCLipLed.Repaint;
  end;
end;

procedure TMainForm.pbWarnLedPaint(Sender: TObject);
begin
  with Sender as TPaintBox do
    LedImages.Draw(Canvas, 0, 0, ord(ClipWarning)+2);
end;

procedure TMainForm.SetClipWarning(Value: Boolean);
begin
  if Value <> FClipWarning then
  begin
    FClipWarning := Value;
    pbWarnLed.Repaint;
  end;
end;

procedure TMainForm.BtnBackClick(Sender: TObject);
var p: Cardinal;
    Track: Integer;
begin
 if WaveStatus = wsPlaying then
 begin
  p := GetWaveOutPos;
  Track := FindTrack(p);
  if Track < 0 then Track := 0;
  if Track < TrackList.Items.Count then
  begin
    if (Track>0) and (p - PSplitInfo(TrackList.Items[Track].Data).Start < WaveFormat.nAvgBytesPerSec)
      then dec(Track);
    SeekTo(PSplitInfo(TrackList.Items[Track].Data).Start);
  end;
 end;
end;

procedure TMainForm.BtnSkipClick(Sender: TObject);
var Track: Integer;
begin
 if WaveStatus = wsPlaying then
 begin
  Track := FindTrack(GetWaveOutPos);
  if (Track >= 0) and (Track < TrackList.Items.Count-1) then
  begin
    SeekTo(PSplitInfo(TrackList.Items[Track+1].Data).Start);
  end;
 end;
end;

procedure TMainForm.SetHasSignal(Value: Boolean);
begin
  if Value <> FHasSignal then
  begin
    FHasSignal := Value;
    pbSignalLed.Repaint;
  end;
end;

procedure TMainForm.pbSignalLedPaint(Sender: TObject);
begin
  with Sender as TPaintBox do
    LedImages.Draw(Canvas, 0, 0, ord(HasSignal)+4);
end;

procedure TMainForm.ShouldStop(var Msg: TMessage);
begin
  BtnStopClick(nil);
end;

procedure TMainForm.Deletetracksplit1Click(Sender: TObject);
var DelItem, SrcItem: TListItem;
    i: Integer;
begin
  with TrackList do
  if Items.Count > 1 then
  begin
    DelItem := Selected;
    if DelItem <> nil then
    begin
      i := DelItem.Index;
      if i=0 then
      begin
        SrcItem := Items[1];
        PSplitInfo(SrcItem.Data).Start := 0;
        inc(PSplitInfo(SrcItem.Data).Size, PSplitInfo(DelItem.Data).Size);
      end else begin
        SrcItem := Items[i-1];
        inc(PSplitInfo(SrcItem.Data).Size, PSplitInfo(DelItem.Data).Size);
      end;
      SrcItem.SubItems[0] := MakeTime(PSplitInfo(SrcItem.Data).Start);
      SrcItem.SubItems[1] := MakeTime(PSplitInfo(SrcItem.Data).Size);
      DelItem.Delete;
      RenumberSplits;
      pbTimeRange.Invalidate;
    end;
  end;
end;

procedure TMainForm.TrackPopupMenuPopup(Sender: TObject);
var
  Items: Integer;
begin
  Items := TrackList.Items.Count;
  Renametrack1.Enabled := Items>0;
  Deletetracksplit1.Enabled := (Items>1) and (TrackList.SelCount > 0);
end;

procedure TMainForm.WaveBarPaint(Sender: TObject);
begin
  with Sender as TPaintBox do with Canvas do
  if (SectorStatsSize > 0) then begin
    Pen.Color := clLime;
    Brush.Color := clLime;
    Brush.Style := bsSolid;
    Rectangle(
        (int64(ZoomOffset) * Width) div SectorStatsSize,
        0,
        (int64(ZoomOffset+ZoomWaveFormSize) * Width) div SectorStatsSize,
        Height);
  end;
end;

procedure TMainForm.SetZoomOffset(Value: Cardinal);
var ScrollBy: Integer;
    ScrollRect: TRect;
begin
  if (Value + Cardinal(pbWave.width) >= SectorStatsSize) then
  begin
     if (Cardinal(pbWave.width) <= SectorStatsSize) then
        Value := SectorStatsSize - Cardinal(pbWave.Width)
     else
        Value := 0;
  end;
  if Value <> ZoomOffset then begin
    ScrollBy := FZoomOffset - value;
    FZoomOffset := Value;
    RecalcZoomWaveForm;
    ScrollRect := pbWave.BoundsRect;
    ScrollWindowEx(panel2.Handle, ScrollBy, 0, @ScrollRect, @ScrollRect{nil}, 0, nil, SW_INVALIDATE);
    WaveScrollBar.Position := value;
  end;
end;

procedure TMainForm.pbTimeRangeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Button = mbLeft then
begin
  TimeRangeMBD := True;
  TimeRangeLastMove := -1;
  pbTimeRangeMouseMove(Sender, Shift, X, Y);
end;
end;

procedure TMainForm.pbTimeRangeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var At: Cardinal;
begin
if (x <> TimeRangeLastMove) and assigned(WaveFile) then
  with Sender as TPaintBox do begin
    TimeRangeLastMove := x;
    if (x <= 0) then
      At := 0
    else
      At := Cardinal((int64(x) * WaveFile.WaveSize) div Width);
    if (At >= WaveFile.WaveSize) then
        Exit;
    StatusLine.Panels[0].Text := MakeTime(At);
    if TimeRangeMBD then
    begin
      if IsPlaying then SeekTo(at);
      CursorPos := At;
      MarkerPos := At;
    end;
   end;
end;

procedure TMainForm.pbTimeRangeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Button = mbLeft then
  TimeRangeMBD := False;
end;

// Insert split when we already 'checked' all subconditions.
procedure TMainForm.InsertSplit(Start, Size: Cardinal; At: Integer);
var
  NewItem: TListItem;
  Info: PSplitInfo;
begin
  NewItem := TrackList.Items.Insert(At);
  new(Info);
  Info.Start := Start;
  Info.Size := Size;
  Info.KeepName:= False;
  NewItem.Data := Info;
  NewItem.SubItems.Add(MakeTime(Start));
  NewItem.SubItems.Add(MakeTime(Size));
  NewItem.Checked := True;
  NewItem.ImageIndex := -1;
  RenumberSplits;
end;

procedure TMainForm.AddSplitAt(Position: Cardinal);
var
  At: Integer;
  Tot, sStart, SplitPos: Cardinal;
begin
  SplitPos := SectorSize * (Position div SectorSize);
  if (SplitPos > 0) and assigned(WaveFile) and (SplitPos < WaveFile.WaveSize) then
  with TrackList.Items do
  try
    BeginUpdate;
    At := 0;
    Tot := 0;
    while (At < Count) and (Tot < SplitPos) do
    begin
       inc(Tot, PSplitInfo(Item[At].Data).Size);
       inc(At);
    end;
    if Tot > SplitPos then
    begin
      dec(At);
      sStart := Tot - PSplitInfo(Item[At].Data).Size;
      PSplitInfo(Item[At].Data).Size := SplitPos - sStart;
      Item[At].SubItems[1] := MakeTime(SplitPos - sStart);
      InsertSplit(SplitPos, Tot - SplitPos, At+1);
      Item[At+1].MakeVisible(false);
    end;
    HasChanged := True;
  finally
    EndUpdate;
    lDesExtra.Caption := 'Total tracks';
    ShowSelectedTime;
    pbTimeRange.Invalidate;
  end;
end;

procedure TMainForm.HelpContents1Click(Sender: TObject);
begin
  Application.HelpContext(0);
end;

procedure TMainForm.Clearallsplits1Click(Sender: TObject);
begin
  if TrackList.Items.Count > 0 then
  begin
    if MessageDlg(SureLoseStr, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      InitTracks;
      RecalcWaveForm;
      pbTimeRange.Invalidate;
    end;
  end;
end;

procedure TMainForm.Preferences1Click(Sender: TObject);
begin
  if SettingsDialog.ShowModal = mrOk then
  begin
      ApplySettings;
  end;
end;

procedure TMainForm.ApplySettings;
begin
  with SettingsDialog do
  begin
    PriorityBoost := SettingsDialog.PerfBoost.ItemIndex;
    if WaveStatus = wsRecord then
    case PriorityBoost of
       0: SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);
       1: SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);
       2: SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
    end;
    FlatButtons := cbFlat.Checked;
    Colors[0] := Panel1.Color;
    Colors[1] := Panel2.Color;
    Colors[2] := Panel3.Color;
    Colors[3] := Panel4.Color;
    Colors[4] := Panel5.Color;
    Colors[5] := Panel6.Color;
    Colors[6] := Panel7.Color;
    Colors[7] := Panel8.Color;
    RecCallbackMode := tbRecMode.Position;
    ConfChanges := cbConfChanges.Checked;
    OutDevice := cbOutDevice.ItemIndex - 1;
    BottomFollowsPlayback := cbFollowPlayback.Checked;
    RewindOnStop := cbRewindOnStop.Checked;
    SplitAtMarker := cbSplitAtMarker.Checked;
    PositionTimer.Interval := 1000 div udRefreshRate.Value;
  end;
end;

procedure TMainForm.SetFlatButtons(Value: Boolean);
begin
  Toolbar1.Flat := Value;
end;
function TMainForm.GetFlatButtons: Boolean;
begin
  result := Toolbar1.Flat;
end;

function TMainForm.GetNiceColor;
begin
  Result := Colors[Index and 7];
end;

procedure TMainForm.Savecuesheet1Click(Sender: TObject);
var f: TextFile;
    i: Integer;
    s: string;
begin
  if (WaveStatus >= wsOpen) and SaveCueDialog.Execute then
  begin
    AssignFile(f, SaveCueDialog.FileName);
    ReWrite(f);
    WriteLn(f, Format(FileStr, [WaveFileName]));
    for i := 0 to TrackList.Items.Count-1 do
    with TrackList.Items do begin
      WriteLn(f, Format(TrackStr, [TwoDigit(i+1)]));
      if PSplitInfo(item[i].data).KeepName then
        WriteLn(f, Format(TitleStr, [Item[i].Caption]));
      s := Item[i].SubItems[0];
      s[length(s) - 2] := ':';
      WriteLn(f, IndexStr + s);
    end;
    system.Close(f);
    HasChanged := False;
  end;
end;

procedure TMainForm.Loadcuesheet1Click(Sender: TObject);
begin
  if OpenCueDialog.Execute then
  begin
    LoadCueSheet(OpenCueDialog.FileName);
  end;
end;

procedure TMainForm.LoadCueSheet(const FileName: string);
var LineNum: Integer;
    p,q: Integer;
    s: string;
    name: string;
    tracktitle: string;
    f: TextFile;
    item: TListItem;
begin
    AssignFile(f, FileName);
    Reset(f);
    LineNum := 0;
    try
      if not eof(f) then
      begin
        repeat
          ReadLn(f, s);
          inc(LineNum);
          p := Pos('FILE', s);
        until eof(f) or (p>0);
        if p>0 then begin
          delete(s, 1, p+3);
          while s[1] = ' ' do delete(s, 1,1);
          if (s[1] = '"') then
          begin
            delete(s, 1, 1);
            q := pos('"', s);
          end else
            q := Pos('WAVE', s);
          if q>0 then begin
            delete(s, q, length(s));
            while s[Length(s)] = ' ' do delete(s, Length(s), 1);
            name := s;
          end else
            raise Exception.Create(NoWaveKeyStr);
          if Name <> WaveFileName then
          begin
           p := MessageDlg(Format(OpenStr, [name]), mtConfirmation,
                        [mbYes, mbNo, mbCancel], 0);
           if p = mrYes then
           begin
             BtnStopClick(nil);
             LoadFile(Name);
           end else
             if (p = mrNo) and (WaveStatus < wsOpen) then p := mrCancel;
          end else
            p := mrNo;
          if p <> mrCancel then
          begin
           tracktitle := '';
           TrackList.Items.BeginUpdate;
           try
            //TrackList.Items.Clear;
            InitTracks;
            while not eof(f) do
            begin
              ReadLn(f, s);
              inc(LineNum);
              p := Pos('INDEX 01 ', s);
              if p >0 then begin
                Delete(s, 1, p+8);
                while (Length(s)<>0) and (s[1] = ' ') do delete(s,1,1);
                if Length(s) < 8 then
                   raise Exception.Create(Format(NoIndexStr, [IntToStr(LineNum)]));
                p := StrToTime(s);
                AddSplitAt(p);
                if (tracktitle <> '') then begin
                  p := TrackList.Items.Count-1;
                  if p >= 0 then begin
                    item := TrackList.Items[p];
                    item.Caption := tracktitle;
                    PSplitInfo(item.data).KeepName := True;
                  end;
                end;
                tracktitle := '';
              end else begin
                p := Pos('TITLE ', s);
                if p > 0 then begin
                  delete(s, 1, p+5);
                  while (s[1] = ' ') do delete(s, 1, 1);
                  if s[1] = '"' then begin
                    delete(s, 1, 1);
                    p := pos('"', s);
                  end else
                    p := pos(' ', s);
                  if p > 0 then
                    delete(s, p, length(s));
                  tracktitle := s;
                end else if Pos('TRACK ', s) > 0 then begin
                  tracktitle := '';
                end;
              end;
            end;
          finally
            TrackList.Items.EndUpdate;
          end;
         end;
        end;
      end;
    finally
      system.Close(f);
      if TrackList.Items.Count = 0 then InitTracks;
    end;
end;

function TMainForm.GetSelectedTime: Integer;
var i: Integer;
begin
  Result := 0;
  if assigned(TrackList) then
    with TrackList.Items do
      for i := 0 to Count-1 do
       if Item[i].Checked then
         if assigned(Item[i].Data) then
           inc(Result, PSplitInfo(Item[i].Data).Size);
end;

procedure TMainForm.ShowSelectedTime;
begin
   lExtra.Caption := MakeTime(GetSelectedTime);
end;

procedure TMainForm.TrackListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Change = ctState then
  begin
    ShowSelectedTime;
    Renumbersplits;
  end;
end;

procedure TMainForm.Total2Click(Sender: TObject);
begin
  if (WaveStatus >= wsOpen) and (WaveStatus < wsRecord) then
  with Sender as TMenuItem do
  begin
    TimeDisplayMode := Tag;
  end;
end;

procedure TMainForm.SetTimeDisplayMode(Value: Integer);
const
  TimeModes: array[0..4] of string =
    (ElapsedStr, RemainStr, TrackTimeStr, TrackRemStr, RecordStr);
begin
  if (Value <> FTimeDisplayMode) then
  begin
    FTimeDisplayMode := Value;
    lTimeDisplay.Caption := TimeModes[Value];
    if Value < 4 then pmPositionMode.Items[Value].Checked := True;
    UpdateTimeDisplay;
  end;
end;

procedure TMainForm.UpdateTimeDisplay;
var p: Cardinal;
    t: Integer;
    s: string;
begin
if WaveStatus < wsRecord then begin
 s := '';
 p := CursorPos;
 if (TimeDisplayMode >= 2) then begin
   t := FindTrack(p);
   if t >=0 then begin
     s := Format('%.2d ', [t+1]);
     p := p - PSplitInfo(TrackList.Items[t].Data).Start;
     if (TimeDisplayMode and 1) <> 0 then
     begin
       p := PSplitInfo(TrackList.Items[t].Data).Size - p;
       s[3] := '-';
     end;
   end else s := '-- ';
 end else if TimeDisplayMode = 1 then
 begin
   p := WaveFile.WaveSize - p;
   s := '-'
 end;
 lPosition.Caption := s + MakeTime(p);
end else
 lPosition.Caption := MakeTime(CursorPos);
end;


procedure TMainForm.lTimeDisplayClick(Sender: TObject);
begin
  if (WaveStatus >= wsOpen) and (WaveStatus < wsRecord) then
    TimeDisplayMode := (TimeDisplayMode + 1) and 3;
end;

procedure TMainForm.ddeCDWAVExecuteMacro(Sender: TObject; Msg: TStrings);
begin
  MessageDlg(Format(MacroStr, [Msg[0]]), mtInformation, [mbOk], 0);
end;

procedure TMainForm.RenameTrack1Click(Sender: TObject);
var
  Item: TListItem;
begin
  Item := TrackList.ItemFocused;
  if assigned(Item) then
  begin
    ListView_EditLabel(TrackList.Handle, Item.Index);
  end;
end;

procedure TMainForm.TrackListDblClick(Sender: TObject);
var
  Item: TListItem;
  At: Cardinal;
begin
  Item := TrackList.ItemFocused;
  if assigned(Item) then
  begin
    At := PSplitInfo(Item.Data).Start;
    CursorPos := At;
    MarkerPos := At;
    if IsPlaying then SeekTo(At);
  end;
end;

procedure TMainForm.MarkAllTracks(Checked: Boolean);
var i: Integer;
begin
  with TrackList.Items do
  for i := 0 to Count - 1 do
  begin
    Item[i].Checked := Checked;
  end;
end;


procedure TMainForm.Checkall1Click(Sender: TObject);
begin
  MarkAllTracks(True);
end;

procedure TMainForm.Uncheckall1Click(Sender: TObject);
begin
  MarkAllTracks(False);
end;

function TMainform.GetShortcutsEnabled: Boolean;
begin
  result := not TrackList.IsEditing;
end;

procedure TMainForm.CreateParams;
var r: TRegistry;
begin
  inherited;
  r := TRegistry.Create;
  try
    r.OpenKey(CDWAVRegKey, False);
    RetrieveFormLocation(r, ClassName, self, Params);
    FitFormOnScreen(Params);
  finally
    r.Free;
  end;
end;

procedure TMainForm.WMDestroy;
var r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.OpenKey(CDWAVRegKey, True);
    StoreFormLocation(r, ClassName, self, soAll);
  finally
    r.Free;
    inherited;
  end;
end;

procedure TMainForm.WMDropfiles;
var len: Integer;
    FileName, ext: string;
begin
  try
    if (DragQueryFile(msg.WParam, $FFFFFFFF, nil, 0) <> 1) then
       raise Exception.Create(OnlySingleStr);
    len := DragQueryFile(msg.WParam, 0, nil, 0);
    SetString(FileName, nil, len+2);
    DragQueryFile(msg.WParam, 0, Pointer(FileName), len+2);
    SetLength(FileName, len);
    ext := LowerCase(ExtractFileExt(FileName));
    if ext = '.cue' then
      LoadCueSheet(FileName)
    else
      LoadFile(FileName)
  finally
    DragFinish(msg.WParam);
  end;
end;

procedure TMainform.WMSysCommand(var Msg: TMessage);
begin
  if IsRecording then
  begin
    if ((Msg.wParam and $FFFFFFF0) = SC_SCREENSAVE) or
       ((Msg.wParam and $FFFFFFF0) = SC_MONITORPOWER)
    then
      Msg.Result := 0
    else
      inherited;
  end else
    inherited;
end;


procedure TMainForm.FormShow(Sender: TObject);
var i: integer;
    s: String;
    TheFile: string;
    WriteDir: string;
    action: (aNothing, aPlay, aRecord, aSplit);
    splitSeconds, splitFuzzy: Cardinal;
function CalcSeconds(const ts: string): Cardinal;
var min, hr: Cardinal;
    pos: Integer;
begin
  hr := 0;
  min := 0;
  pos := 1;
  while pos <= length(ts) do
  begin
    case ts[pos] of
      ':': begin
             if (hr <> 0) then
                raise Exception.Create(NoTimeStr);
             hr := min;
             min :=0;
           end;
      '0'..'9': min := 10 * min + Cardinal(ord(ts[pos]) - ord('0'));
    end;
    inc(pos);
  end;
  Result := (hr * 60) + min;
end;
function CalcTime(const ts:string): Cardinal;
begin
  Result := WaveFormat.nAvgBytesPerSec  * CalcSeconds(ts) * 60;
end;
procedure WriteFiles;
var LoadingDlg: TLoadingDlg;
begin
    LoadingDlg := TLoadingDlg.Create(self);
    try
      LoadingDlg.SaveTracks(
        TrackList.Items,
        WaveFile,
        WriteDir,
        WaveFormat,
        nil);
    finally
      LoadingDlg.Free;
    end;
end;

begin
 try
  i := 1;
  action := aNothing;
  SplitSeconds := 0;
  SplitFuzzy := 0;
  WriteDir := '';
  DlgRecord.GetSettings(RecSettings); // use defaults
  CopyWaveFormat(DlgRecord.WaveFormat);
  while i <= ParamCount do
  begin
    s := ParamStr(i);
    if s[1] in ['/', '-'] then
    begin
      case UpCase(s[2]) of
        'P': action := aPlay;
        'R': action := aRecord;
        'S': begin
               if length(s) >= 3 then
                RecSettings.StopOnSilence := (s[3] <> '-')
               else
                RecSettings.StopOnSilence := True;
             end;
        'T': begin
                if length(s) < 3 then
                  raise Exception.Create(NeedTimeStr);
                RecSettings.StopTime := CalcTime(copy(s, 2, length(s) - 1));
                RecSettings.StopOnTime := True;
             end;
        'V': begin
               if length(s) >= 3 then
                RecSettings.StartOnVoice := (s[3] <> '-')
               else
                RecSettings.StartOnVoice := True;
             end;
        'Q': QuitAfterRecording := True;
        'O': SelectDirDlg.Directory := copy(s, 3, length(s) - 2);
        'I': begin
                action := aSplit;
                splitSeconds := CalcSeconds(copy(s, 3, length(s) - 2));
             end;
        'F': begin
                splitFuzzy := CalcSeconds(copy(s, 3, length(s) - 2));
             end;
        'W': begin
                WriteDir := copy(s, 3, length(s) - 2);
                if (WriteDir[length(WriteDir)] <> '\') then
                    WriteDir := WriteDir + '\';
             end;
      else
        raise Exception.Create('Unknown commandline option: ' + s);
      end;
    end else begin
      if (TheFile <> '') then StatusLine.Panels[1].Text := IgnoreFileStr + TheFile;
      TheFile := s;
    end;
    inc(i);
  end;
  if action = aRecord then
  begin
    if TheFile = '' then
      raise Exception.Create(NoRecFileStr);
    StatusLine.Panels[1].Text := StartRecStr + TheFile;
    if not RecSettings.StopOnSilence then
      RecSettings.StopSilence := RecSettings.StopTime;
    RecSettings.FileName := TheFile;
    RecSettings.RecordMode := rmFile;
    StartRecording;
  end else
    if TheFile <> '' then
    begin
      LoadFile(TheFile);
      case action of
        aPlay: BtnPlayClick(nil);
        aSplit: begin
                  if splitFuzzy <> 0 then
                    TimeSplitFuzzy(0, splitSeconds, splitFuzzy)
                  else
                    TimeSplit(0, splitSeconds);
                  if WriteDir <> '' then
                  begin
                    WriteFiles;
                    if QuitAfterRecording then Close;
                  end;
                end;
      end;
    end;
 except
    on e: Exception do
      MessageDlg(NoComLineStr + e.Message, mtError, [mbOk], 0);
 end
end;

procedure TMainForm.PositionTimerTimer(Sender: TObject);
begin
  if IsPlaying then
  begin
    CursorPos := GetWaveOutPos;
  end;
end;

procedure TMainForm.FilePropertiesActionExecute(Sender: TObject);
var i: Cardinal;
    minLevel: Smallint;
    maxLevel: Smallint;
    clips: Integer;
begin
  with TFilePropertiesForm.Create(self) do
  try
    lSamplesPerSec.Caption := IntToStr(WaveFormat.nSamplesPerSec);
    lBitsPerSample.Caption := IntToStr(WaveFormat.wBitsPerSample);
    lChannels.Caption := IntToStr(WaveFormat.nChannels);
    lAvgBytesPerSec.Caption := IntToStr(WaveFormat.nAvgBytesPerSec);
    lWaveSize.Caption := IntToStr(WaveFile.WaveSize);
    lTotalTime.Caption := MakeTime(WaveFile.WaveSize);
    clips := 0;
    minLevel := 0;
    maxLevel := -0;
    for i := 0 to SectorStatsSize - 1 do
    begin
      if SectorStatsList[i].Min < minLevel then
        minLevel := SectorStatsList[i].Min;
      if SectorStatsList[i].Max > maxLevel then
        maxLevel := SectorStatsList[i].Max;
      if (SectorStatsList[i].Min = -32768) or (SectorStatsList[i].Max = 32767) then
        inc(clips);
    end;
    lMaxSampleValue.Caption := IntToStr(maxLevel);
    lMaxSamplePercent.Caption := IntToStr(integer(maxLevel)*100 div 32767);
    lMinSampleValue.Caption := IntToStr(minLevel);
    lMinSamplePercent.Caption := IntToStr(integer(-minLevel)*100 div 32768);
    lClippedSectors.Caption := IntToStr(clips);
    ShowModal;
  finally
    free;
  end;
end;

procedure TMainForm.CopyWaveFormat;
var size: Cardinal;
begin
  if (fmt <> nil) then
  begin
    Size := sizeof(TWaveFormatEx) + fmt.cbSize;
    if (FWaveFormat <> nil) then
    begin
      if FWaveFormat.cbSize <> fmt.cbSize then
      begin
        FreeMem(FWaveFormat);
        GetMem(FWaveFormat, Size);
      end;
    end else
      GetMem(FWaveFormat, Size);
    Move(fmt^, FWaveFormat^, Size);
    SectorSize := (fmt.nAvgBytesPerSec div (75 * fmt.nBlockAlign)) * fmt.nBlockAlign;
  end;
end;

procedure TMainForm.JumpBy(distance: Integer);
var At: Cardinal;
begin
  if (WaveStatus >= wsOpen) and (WaveStatus < wsRecord) then
  begin
    At := CursorPos;
    if (distance < 0) then
    begin
     if (At < Cardinal(-distance)) then
       At := 0
     else
       dec(At, Cardinal(-distance));
    end else
      inc(At, Cardinal(distance));
    //@@
    CursorPos := At;
    if IsPlaying then
      SeekTo(CursorPos);
  end;
end;

procedure TMainForm.JumpOneForwardExecute(Sender: TObject);
begin
  JumpBy(SectorSize);
end;

procedure TMainForm.JumpOneBackExecute(Sender: TObject);
begin
  JumpBy(-SectorSize);
end;

procedure TMainForm.JumpForwardExecute(Sender: TObject);
begin
 JumpBy(WaveFormat.nAvgBytesPerSec);
end;

procedure TMainForm.JumpBackExecute(Sender: TObject);
begin
 JumpBy(-WaveFormat.nAvgBytesPerSec);
end;

procedure TMainForm.JumpBigBackExecute(Sender: TObject);
begin
 JumpBy(-10 * WaveFormat.nAvgBytesPerSec);
end;

procedure TMainForm.JumpBigForwardExecute(Sender: TObject);
begin
 JumpBy(10 * WaveFormat.nAvgBytesPerSec);
end;

procedure TMainForm.JumpToExecute(Sender: TObject);
begin
  if JumpToDlg = nil then
    JumpToDlg := TJumpToDlg.Create(self);
  with JumpToDlg do
  begin
     if JumpToDlg.ShowModal = mrOK then
     begin
       CursorPos := StrToTime(JumpToDlg.JumpToPos);
       MarkerPos := CursorPos;
       if IsPlaying then SeekTo(CursorPos);
     end;
  end;
end;

procedure TMainForm.FileAppendActionExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    AppendFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var Delta: Integer;
const Divider = 120;
begin
  //if IsOpen then
  begin
    if ssShift in Shift then
      Delta := WheelDelta * integer(SectorSize) div Divider
    else if ssCtrl in Shift then
      Delta := 10 * WheelDelta * Integer(WaveFormat.nAvgBytesPerSec) div Divider
    else
      Delta := WheelDelta * Integer(WaveFormat.nAvgBytesPerSec) div Divider;
    JumpBy(-Delta);
  end;
end;

procedure TMainform.ResetCaption;
var c: string;
begin
  c := AppName;
  if WaveName <> '' then
    c := c + ' - ' + WaveName;
  if not RegInfo.Registered then
    c := c + UnregStr;
  Caption := c;
end;

procedure TMainForm.RecordTimerTimer(Sender: TObject);
begin
  if RecSettings.StartSchedule then
  begin
    if frac(RecSettings.ScheduleTime) <= Time then
      StartRecording
    else begin
      lPosition.Caption := '-' + TimeToStr(Time - RecSettings.ScheduleTime);
    end;
  end
  else begin
    RecordTimer.Enabled := False;
  end;
end;

procedure TMainForm.ActionNotEditingUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not Tracklist.IsEditing;
end;

procedure TMainForm.PlaySpaceUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Play.Enabled and not Tracklist.IsEditing;
end;

procedure TMainForm.PlaySpaceExecute(Sender: TObject);
begin
  if IsPlaying then
    BtnPauseClick(Sender)
  else
    BtnPlayClick(Sender);
end;

procedure TMainForm.CheckTrackToggleExecute(Sender: TObject);
var item: TListItem;
begin
  item := TrackList.Selected;
  if assigned(item) then item.Checked := not item.Checked;
end;

procedure TMainForm.CheckTrackToggleUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := TrackList.Focused and not Tracklist.IsEditing;
end;

procedure TMainForm.Panel4Resize(Sender: TObject);
var w: Integer;
begin
  w := pbWave.Width;
  WaveScrollBar.SmallChange := w shr 2;
  WaveScrollBar.PageSize := w;
  WaveScrollBar.LargeChange := w;
  ZoomOffset := ZoomOffset; // Check zoom stuff
end;

procedure TMainForm.SetSectorStatsSize;
begin
  FSectorStatsSize := value;
  WaveScrollBar.Position := 0;
  if value > 0 then
  begin
    WaveScrollBar.Enabled := True;
    WaveScrollBar.PageSize := 0;
    WaveScrollBar.Max := value - 1;
    Panel4Resize(nil);
  end else begin
    //WaveScrollBar.Max := 1;
    WaveScrollBar.Enabled := False;
    // WaveScrollBar.Max := 0;
  end;
end;

procedure TMainForm.WaveScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Cardinal(ScrollPos) <> ZoomOffset then ZoomOffset := ScrollPos;
end;

procedure TMainForm.TrackListCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var Info: PSplitInfo;
begin
  if SubItem = 3 then
  begin
    info := PSplitInfo(Item.Data);
    if info <> nil then
    begin
      Sender.Canvas.Brush.Color := info.Color;
    end;
//    if info <> nil then
//      Sender.Canvas.Brush.Color := info.Color;
//    item.DisplayRect
  end;
end;

procedure TMainForm.Unchecksmallsplits1Click(Sender: TObject);
var info: PSplitInfo;
    i: Integer;
    smallsplit: Cardinal;
begin
  if not assigned(WaveFile) then Exit;
  smallsplit := WaveFile.WaveSize div 25;
  with TrackList.Items do
  begin
    for i := 0 to Count-1 do
    begin
      info := PSplitInfo(item[i].data);
      if info.Size < smallsplit then
        item[i].Checked := False;
    end;
  end;
end;

procedure TMainForm.ActionBoundsCheckExecute(Sender: TObject);
var
  info: PSplitInfo;
  bufSize: Cardinal;
  buf: array[0..1] of PWaveHdr;
  Err: Integer;
  AuditionForm: TAuditionForm;
  i: Integer;
begin
  if TrackList.Selected <> nil then
  begin
    BtnStopClick(nil);
    try
      screen.Cursor := crAppStart;
      info := PSplitInfo(TrackList.Selected.Data);
      BufSize := WaveFormat.nAvgBytesPerSec * 4;
      if info.Size < 2*BufSize then BufSize := info.Size div 2;
      BufSize := (BufSize div WaveFormat.nBlockAlign) * WaveFormat.nBlockAlign;
      for i := 0 to 1 do
        Buf[i] := WaveUtil.AllocWave(BufSize);
      try
        WaveFile.SetFilePos(Info.Start);
        Buf[0].dwBytesRecorded := WaveFile.Read(Buf[0].lpData, BufSize);
        WaveFile.SetFilePos(Info.Start+Info.Size-BufSize);
        Buf[1].dwBytesRecorded := Wavefile.Read(Buf[1].lpData, BufSize);
        AuditionForm := TAuditionForm.Create(self);
        AuditionForm.lTrack.Caption := TrackList.Selected.Caption;
        try
          Err := WaveOutOpen(@WaveOut, OutDevice, WaveFormat, AuditionForm.Handle, 0, CallBack_Window);
          if Err<>0 then raise TWaveOutException.Create(Err, WhileOpeneningStr);
          try
             for i := 0 to 1 do
             begin
               WaveOutPrepareHeader(WaveOut, Buf[i], sizeof(TWaveHdr));
               WaveOutWrite(WaveOut, Buf[i], SizeOf(TWaveHdr));
             end;
             AuditionForm.ShowModal;
             waveOutReset(WaveOut);
             Application.ProcessMessages;
             for i := 0 to 1 do
               WaveOutUnPrepareHeader(WaveOut, Buf[i], sizeof(TWaveHdr));
          finally
            WaveOutClose(WaveOut);
          end;
        finally
          AuditionForm.Free;
        end;
      finally
        for i := 0 to 1 do
          WaveUtil.FreeWave(Buf[i]);
      end;
    finally
      screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.ActionPlayTrackExecute(Sender: TObject);
var Item: TListItem;
    Info: PSplitInfo;
begin
  if not IsRecording then
  begin
    Item := TrackList.Selected;
    if assigned(Item) then
    begin
      if IsPlaying or IsRecording then BtnStopClick(nil);
      info := PSplitInfo(Item.Data);
      StopPlay := info.Start + info.Size;
      CursorPos := info.Start;
      if IsPlaying then
        SeekTo(info.Start)
      else
        StartPlayback;
    end;
  end;
end;

procedure TMainForm.SetStopPlay;
begin
  FStopPlay := value;
  ActionPlayTrack.Checked := value <> 0;
end;

procedure TMainForm.ActionPlayableTrackUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := assigned(TrackList.Selected) and
          (WaveStatus >= wsOpen) and not IsRecording;
end;

procedure TMainForm.PlayUpdate(Sender: TObject);
begin
  with (Sender as TAction) do
  begin
    Enabled := (WaveStatus >= wsOpen) and not IsRecording;
    Checked := IsPlaying;
    BtnPlay.Down := IsPlaying;
  end;
end;

procedure TMainForm.DAEActionExecute(Sender: TObject);
var
  loadingDlg: TLoadingDlg;
  LoadSize, StatsSize: Cardinal;
  StatsHandle: HGlobal;
  tracks: TList;
  i: Integer;
begin
  if DAEForm = nil then
     DAEForm := TDAEForm.Create(self);
  if DAEForm.ShowModal = mrOK then
  begin
    loadingDlg := TLoadingDlg.Create(self);
    try
      DAEForm.AquireCD;
      try
        Tracks := TList.Create;
        try
          loadingDlg.DAE(DAEForm.CDHandle, DAEForm.FileName, DAEForm.DoJitter,
                         StatsHandle, StatsSize, LoadSize, tracks);
          SetLoadedFile(    DAEForm.FileName,
                          StatsHandle,
                          StatsSize,
                          2352,
                          LoadSize,
                          @RedBookFormat);
          for i := 0 to Tracks.Count-1 do
          begin
            AddSplitAt(Cardinal(Tracks[i]));
          end;
        finally
          Tracks.Free;
        end;
      finally
        DAEForm.ReleaseCD;
      end;
    finally
      loadingDlg.Free;
    end;
  end;
end;

// Add split every .. seconds
procedure TMainForm.TimeSplit;
var i, p: Cardinal;
begin
  InitTracks;
  if StartAt <> 0 then
  begin
    i := StartAt div WaveFormat.nAvgBytesPerSec;
  end else begin
    i := every;
  end;
  p := WaveFormat.nAvgBytesPerSec * i;
  while p < WaveFile.WaveSize do
  begin
    AddSplitAt(p);
    inc(i, every);
    p := WaveFormat.nAvgBytesPerSec * i;
  end;
  pbTimeRange.Repaint;
end;

// Add split every .. seconds and find the "most silent" part in
// fuzz seconds around it.
procedure TMainForm.TimeSplitFuzzy;
var i, n, k, p: Cardinal;
    starts, ends, StartSilence: Cardinal;
    bestPos, bestSize: Cardinal;
    lowLevel: Integer;
    minLevel, maxLevel: SmallInt;
function SectorSilent(Level: TSectorStats): boolean;
begin
  Result := (Level.Max < MaxLevel) and (Level.Min > MinLevel);
end;
begin
  InitTracks;
  if StartAt <> 0 then
  begin
    i := StartAt div WaveFormat.nAvgBytesPerSec;
  end else begin
    i := every;
  end;
  p := WaveFormat.nAvgBytesPerSec * i;
  fuzz := Muldiv(fuzz, WaveFormat.nAvgBytesPerSec, SectorSize); // in sectors
  while p < WaveFile.WaveSize do
  begin
    // Determine interval
    bestPos := p div SectorSize;
    bestSize := 1;
    if bestPos < fuzz then
        starts := 0
    else
        starts := bestPos - fuzz;
    ends := (p div SectorSize) + fuzz;
    if (ends >= SectorStatsSize) then
        ends := SectorStatsSize - 1;
    minLevel := 0; maxLevel := 0;
    for n := starts to ends do
    begin
      if SectorStatsList[n].Min < minLevel then
         minLevel := SectorStatsList[n].Min
      else if SectorStatsList[n].Max > maxLevel then
        maxLevel := SectorStatsList[n].Max;
    end;
    minLevel := minLevel div 4;
    maxLevel := maxLevel div 4;
    // Look for best candidate (longest period below 25%)
    n := starts;
    while n < ends do
    begin
      StartSilence := n;
      while (n < ends) and SectorSilent(SectorStatsList[n]) do
        inc(n);
      if (n - StartSilence) > BestSize then
      begin
        // find most silent sector
        BestPos := (StartSilence + n) div 2;
        BestSize := n - StartSilence;
        lowLevel := maxLevel - minLevel;
        for k := StartSilence to n do
        begin
          if SectorStatsList[k].Max - SectorStatsList[k].Min <= lowLevel then
          begin
            BestPos := k;
            lowLevel := SectorStatsList[k].Max - SectorStatsList[k].Min;
          end;
        end;
      end;
      while (n < ends) and not SectorSilent(SectorStatsList[n]) do
        inc(n);
    end;
    AddSplitAt(BestPos * SectorSize);
    inc(i, every);
    p := WaveFormat.nAvgBytesPerSec * i;
  end;
  pbTimeRange.Repaint;
end;


procedure TMainForm.AutoSplit;
var
  i, StartSilence: Cardinal;
function SectorSilent(Level: TSectorStats): boolean;
begin
  Result := (Level.Max < MaxLevel) and (Level.Min > MinLevel);
end;
begin
    i := 0;
    InitTracks;
    while i < SectorStatsSize do
    begin
      StartSilence := i;
      while (i < SectorStatsSize) and SectorSilent(SectorStatsList[i]) do
        inc(i);
      if (i - StartSilence) > Time then AddSplitAt(((i*RelPos + StartSilence*(100-RelPos)) div 100) * SectorSize);
      while (i < SectorStatsSize) and not SectorSilent(SectorStatsList[i]) do
        inc(i);
    end;
    pbTimeRange.Repaint;
end;

function TMainForm.createAboutBox;
begin
  if RegInfo.Registered then
      Result := TAboutBoxFull.Create(self)
  else
      Result := TAboutBoxShare.Create(self);
end;

function TMainForm.createRecordDialog;
begin
  Result := TDlgRecordShare.Create(self);
end;

procedure TMainForm.AutoSplitActionExecute(Sender: TObject);
begin
  if AutoCutDlg = nil then
    AutoCutDlg := TAutoCutDlg.Create(self);
  AutoCutDlg.ShowModal;
end;

procedure TMainForm.TimeSplitActionExecute(Sender: TObject);
begin
  if TimeSplitForm = nil then
    TimeSplitForm := TTimeSplitForm.Create(self);
  TimeSplitForm.ShowModal;
end;

procedure TMainForm.RenumberSplits;
var At, track: Integer;
    it: TListItem;
begin
  track := 0;
  with TrackList.Items do
      for At := 0 to Count-1 do
      begin
       it := Item[At];
       if (it.data <> nil) then
        begin
        if not PSplitInfo(it.data).KeepName then
        begin
          if it.Checked then
          begin
            inc(track);
            it.Caption := WaveName + TwoDigit(track);
          end else begin
            it.Caption := strSilence;
          end;
        end;
        PSplitInfo(it.data).Color := NiceColor[At];
       end;
      end;
end;

end.
