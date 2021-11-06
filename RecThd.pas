unit RecThd;

interface

uses
  Classes, SysUtils, Windows, Messages, MMSystem, WaveUtil, RecordFrm, Dialogs,
  syncobjs;

const
  WM_RQUIT = WM_USER + 111;
  WM_RDATA = WM_USER + 112;
  WM_RSTARTED = WM_USER + 113;

  ifSignal = $01;
  ifLoud   = $02;
  ifClip   = $04;
  ifStop   = $08;
  ifCanWrite = $10;
  ifError  = $80;

  INITIAL_SECTORSTATSSIZE = 300 * 75; // 5 minutes
  GROW_SECTORSTATSSIZE = INITIAL_SECTORSTATSSIZE;

type
  TWaveRecordThd = class(TThread)
  private
    hWnd:       THandle;
    FSectorStatsList:   PSectorStatsList;
    FSectorStatsHandle: HGLOBAL;
    FSectorStatsSize:   Cardinal;
    SectorStatsCapacity: Cardinal;
    FSectorSize: Cardinal;
    WaveFile:   HMMIO;
    RecOffset:  Cardinal;
    RecSilent:  Cardinal;
    Recorded:   Cardinal;
    SectorStatsCarry: Cardinal;
    IsReady:    TEvent;
    ckOutRIFF, ckOut: TMMCKInfo;
    FWaveFormat: PWaveFormatEx;
    RecSettings:TRecSettings;
    FStopped:   Boolean;
    CanWrite:   Boolean;
    procedure MMWimData(wParam: HWaveIn; lParam: PWaveHdr);
    procedure SetSectorStatsCapacity(NewCapacity: Cardinal);
  protected
    procedure Execute; override;
  public
    constructor Create(AWindow: THandle; AFileName: string;
       const AFormat: PWaveFormatEx; const ASettings: TRecSettings;
       ASectorStatsHandle: HGLOBAL; ASectorStatsSize: Cardinal);
    destructor Destroy; override;
    procedure Quit;
    property Stopped: Boolean write FStopped;
    property RecordMode: TRecordMode read RecSettings.RecordMode;
    property SectorStatsHandle: HGLOBAL read FSectorStatsHandle;
    property SectorStatsSize: Cardinal read FSectorStatsSize;
    property SectorSize: Cardinal read FSectorSize;
    property WaveFormat: PWaveFormatEx read FWaveFormat;
    property RecordedBytes: Cardinal read Recorded;
  end;

procedure WaveInCallback(WaveIn: HWAVEIN; wMsg: UINT;
                          dwInstance, dwParam1, dwParam2: DWORD); stdcall;

implementation

resourcestring
  NoFileStr = 'Cannot start recording (blank filename).';
  NoRiffStr = 'Cannot create RIFF chunk.';
  NoCrFmtStr = 'Cannot create fmt chunk.';
  NoWriteFmtStr = 'Cannot write to fmt chunk';
  NoAscWriteStr = 'Cannot ascend/write.';
  NoDataStr = 'Cannot create data chunk.';
  NoRiffStr2 = 'Cannot open RIFF chunk, file is not a valid WAVE file.';
  NoDescStr = 'Cannot descend into data part.';
  NoAllocMemStr = 'Unable to allocate memory, code: ';
  NoSeekStr = 'Cannot seek on file.';
  TimeOutStr = 'Semaphore timeout. Recording thread not ready?';
  ZombieStr = 'PostThreadMessage() failed. May have created zombie thread.';

procedure WaveInCallback(WaveIn: HWAVEIN; wMsg: UINT;
                         dwInstance, dwParam1, dwParam2: DWORD); stdcall;
begin
  PostThreadMessage(dwInstance, wMsg, WaveIn, dwParam1);
end;

{ TWaveRecordThd }

procedure TWaveRecordThd.Execute;
var Msg: TMSG;
{$IFDEF LOGGING}
    f: TextFile;
{$ENDIF}
begin
  try
{$IFDEF LOGGING}
    Assign(f, 'c:\cdwavrec.log');
    ReWrite(f);
    Writeln(f, 'Init');
{$ENDIF}
    IsReady.SetEvent;
    while not Terminated and GetMessage(Msg, 0, 0, 0) do
    begin
      case Msg.message of
        MM_WIM_DATA: begin
{$IFDEF LOGGING}
                      Writeln(f, 'MM_WIM_DATA  ', msg.wParam);
{$ENDIF}
                      MMWimData(msg.wParam, PWaveHdr(msg.lParam));
                     end;
{$IFDEF LOGGING}
        MM_WIM_OPEN:  begin
                       Writeln(f, 'MM_WIM_OPEN  ', msg.wParam);
//                       PostMessage(hWnd, Msg.Message, msg.wParam, msg.lParam);
                      end;
{$ENDIF}
        MM_WIM_CLOSE: begin
{$IFDEF LOGGING}
                       Writeln(f, 'MM_WIM_CLOSE ', msg.wParam);
{$ENDIF}
                       Terminate;
                      end;
        WM_RQUIT:     begin
{$IFDEF LOGGING}
                       Writeln(f, 'WM_RQUIT');
{$ENDIF}
                       Terminate;
                      end;
      end{case};
    end;
{$IFDEF LOGGING}
    WriteLn(f, 'done');
{$ENDIF}
  finally
{$IFDEF LOGGING}
    close(f);
{$ENDIF}
    if Wavefile <> HMMIO(INVALID_HANDLE_VALUE) then
    begin
      mmioAscendSafe(WaveFile, ckOut, Recorded);
      mmioAscendSafe(WaveFile, ckOutRIFF, ckOut.dwDataOffset + Recorded - ckOutRIFF.dwDataOffset);
      mmioClose(WaveFile, 0);
      WaveFile := HMMIO(INVALID_HANDLE_VALUE);
    end;
    if (FSectorStatsList <> nil) then
    begin
      GlobalUnlock(FSectorStatsHandle);
      FSectorStatsList := nil;
      FSectorStatsHandle := GlobalReAlloc(
        FSectorStatsHandle,
        ((Recorded - 1) div SectorSize + 1) * SizeOf(TSectorStats),
        GMEM_MOVEABLE);
    end;
  end;
end;

constructor TWaveRecordThd.Create;
var
  Res:  MMRESULT;
begin
  inherited Create(True);
  IsReady := TEvent.Create(nil, True, False, '');
  hWnd := AWindow;
  FWaveFormat := AFormat;
  FSectorSize := (AFormat.nAvgBytesPerSec div (75 * AFormat.nBlockAlign)) * AFormat.nBlockAlign;
  RecSettings := ASettings;
  WaveFile := HMMIO(INVALID_HANDLE_VALUE);
  Priority := tpHighest;
  if (RecSettings.RecordMode in [rmFile, rmAppend]) then
  begin
    if (AFileName = '') then Raise Exception.Create(NoFileStr);
  end;
  try
    case RecSettings.RecordMode of
    rmFile: begin
        WaveFile := OpenWaveFile(AFileName, nil, GENERIC_WRITE, CREATE_ALWAYS);
        ckOutRIFF.fccType := mmioStringToFOURCC('WAVE', 0);
        if mmioCreateChunk(Wavefile, @ckOutRIFF, MMIO_CREATERIFF) <> 0 then
          raise Exception.Create(NoRiffStr);
        ckOut.ckid := mmioStringToFOURCC('fmt ',0);
        if AFormat.wFormatTag = WAVE_FORMAT_PCM then
          ckOut.ckSize := sizeof(TWaveFormatEx) - 2
        else
          ckOut.ckSize := sizeof(TWaveFormatEx) + AFormat.cbSize;
        if mmioCreateChunk(WaveFile, @ckOut, 0) <> 0 then
                raise Exception.Create(NoCrFmtStr);
        if mmioWrite(WaveFile, PChar(AFormat), ckOut.ckSize) <> Integer(ckOut.ckSize) then
                raise Exception.Create(NoWriteFmtStr);
        if mmioAscend(WaveFile, @ckOut, 0) <> 0 then
                raise Exception.Create(NoAscWriteStr);
        ckOut.ckid := mmioStringToFOURCC('data',0);
        if mmioCreateChunk(Wavefile, @ckOut, 0) <> 0 then
                raise Exception.Create(NoDataStr);
      end;
    rmAppend: begin
      WaveFile := OpenWaveFile(AFileName, nil, GENERIC_READ or GENERIC_WRITE, OPEN_EXISTING);
      ckOutRIFF.fccType := mmioStringToFOURCC('WAVE', 0);
      res := mmioDescend(WaveFile, @ckOutRIFF, nil, MMIO_FINDRIFF);
      if (res <> 0) then
         raise Exception.Create(NoRiffStr2);
      ckOut.ckid := mmioStringToFOURCC('data',0);
        if mmioDescend(WaveFile, @ckOut, @ckOutRIFF, MMIO_FINDCHUNK)<>0 then
          raise Exception.Create(NoDescStr);
      ckOutRIFF.dwFlags := ckOutRIFF.dwFlags or MMIO_DIRTY;
      ckOut.dwFlags := ckOut.dwFlags or MMIO_DIRTY;
      end;
    rmTest: begin
      end;
    end;
    SectorStatsCarry := 0;
    FSectorStatsSize := 0;
    CanWrite := not RecSettings.StartOnVoice;
    RecOffset:= 0;
    RecSilent:= 0;
    Recorded := 0;
    if (ASectorStatsHandle = 0) then
    begin
      SectorStatsCapacity := INITIAL_SECTORSTATSSIZE;
      FSectorStatsHandle := GlobalAlloc(GHND, SectorStatsCapacity * SizeOf(TSectorStats));
      if SectorStatsHandle = 0 then
        raise Exception.Create(NoAllocMemStr + IntToStr(GetLastError()));
    end else begin
      FSectorStatsHandle := ASectorStatsHandle;
      SectorStatsCapacity := GlobalSize(ASectorStatsHandle) div SizeOf(TSectorStats);
      FSectorStatsSize := ASectorStatsSize;
      Recorded := ASectorStatsSize * SectorSize;
      if mmioSeek(WaveFile, Recorded, SEEK_CUR) = -1 then
        raise Exception.Create(NoSeekStr); // jump to end...
    end;
    FSectorStatsList := PSectorStatsList(GlobalLock(SectorStatsHandle));
    Resume;
    try
      if IsReady.WaitFor(10240) <> wrSignaled then
        raise Exception.Create(TimeOutStr);
    except
      GlobalUnlock(FSectorStatsHandle);
      FSectorStatsList := nil;
      FSectorStatsHandle := GlobalFree(FSectorStatsHandle);
      raise;
    end;
  except
    if WaveFile <> 0 then mmioClose(WaveFile, 0);
    WaveFile := HMMIO(INVALID_HANDLE_VALUE);
    raise;
  end;
end;

destructor TWaveRecordThd.Destroy;
begin
  if Wavefile <> HMMIO(INVALID_HANDLE_VALUE) then
  begin
    mmioAscendSafe(WaveFile, ckOut, Recorded);
    mmioAscendSafe(WaveFile, ckOutRIFF, ckOut.dwDataOffset + Recorded - ckOutRIFF.dwDataOffset);
    mmioClose(WaveFile, 0);
    WaveFile := HMMIO(INVALID_HANDLE_VALUE);
  end;
  if FSectorStatsList <> nil then
  begin
    GlobalUnlock(FSectorStatsHandle);
    FSectorStatsList := nil;
  end;
  inherited;
end;

procedure TWaveRecordThd.MMWimData(wParam: HWaveIn; lParam: PWaveHdr);
var RecLevel: TSectorStats;
    i, n, oldSize: Cardinal;
    Info: Integer;
    HasSignal: Boolean;
begin
 Info := 0;
 HasSignal := false; // avoid warning //
 with lParam^ do begin
    dwFlags := dwFlags and not WHDR_DONE;
    if (not CanWrite) or (WaveFile = HMMIO(INVALID_HANDLE_VALUE)) then
    begin
        case WaveFormat.wBitsPerSample of
         8: RecLevel := SectorLevel8(lpData, dwBytesRecorded shr 1);
        16: RecLevel := SectorLevel16(lpData, dwBytesRecorded shr 2);
        24: RecLevel := SectorLevel24(lpData, dwBytesRecorded div 3);
        end;
        Hassignal :=
          (RecLevel.Min < -RecSettings.Silence) or
          (RecLevel.Max > RecSettings.Silence);
        if HasSignal then
        begin
          CanWrite := True;
          PostMessage(hWnd, WM_RSTARTED, Info, RecOffset);
        end;
    end;
    if Canwrite then
    begin
      if (dwBytesRecorded > 0) and (WaveFile <> HMMIO(INVALID_HANDLE_VALUE)) then
      begin
        if mmioWrite(WaveFile, lpData, dwBytesRecorded) = Integer(dwBytesRecorded) then
        begin
          inc(Recorded, dwBytesRecorded);
          OldSize := SectorStatsSize;
          n := OldSize + (dwBytesRecorded - 1) div SectorSize + 1;
          if (SectorStatsCapacity < n) then
            SetSectorStatsCapacity(n + GROW_SECTORSTATSSIZE);
          inc(FSectorStatsSize,
            FillSectorStats(
              FSectorStatsList, OldSize, SectorStatsCarry,
              lpdata, dwBytesRecorded, SectorSize, WaveFormat.wBitsPerSample));
          RecLevel.Min := 0;
          RecLevel.Max := 0;
          for i := OldSize to SectorStatsSize - 1 do
          begin
            if FSectorStatsList[i].Min < RecLevel.Min then RecLevel.Min := FSectorStatsList[i].Min;
            if FSectorStatsList[i].Max > RecLevel.Max then RecLevel.Max := FSectorStatsList[i].Max;
          end;
          Hassignal :=
            (RecLevel.Min < -RecSettings.Silence) or
            (RecLevel.Max > RecSettings.Silence);
        end else begin
          Info := ifStop or ifError;
        end;
      end;
      Info := Info or ifCanWrite;
    end else begin
      inc(RecOffset, dwBytesRecorded);
    end;
    if HasSignal then
    begin
      RecSilent := 0;
      Info := Info or ifSignal;
    end else begin
      if CanWrite then inc(RecSilent, dwBytesRecorded);
    end;
   if not FStopped then
    begin
      WaveInAddBuffer(wParam, lParam, sizeof(TWaveHdr));
      if (Recorded > RecSettings.StopTime) or
         (RecSilent > RecSettings.StopSilence) then Info := Info or ifStop;
      PostMessage(hWnd, WM_RDATA, Info, Integer(RecLevel));
    end;
 end;
end;

procedure TWaveRecordThd.Quit;
begin
  if not PostThreadMessage(ThreadID, WM_RQUIT, 0, 0) then
    MessageDlg(ZombieStr, mtError, [mbOk], 0);
end;

procedure TWaveRecordThd.SetSectorStatsCapacity;
begin
  GlobalUnlock(FSectorStatsHandle);
  FSectorStatsHandle := GlobalReAlloc(
    SectorStatsHandle,
    NewCapacity * SizeOf(TSectorStats),
    GMEM_MOVEABLE	or GMEM_ZEROINIT);
  SectorStatsCapacity := NewCapacity;
  FSectorStatsList := PSectorStatsList(GlobalLock(SectorStatsHandle));
end;

end.

