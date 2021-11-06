unit DisplayFormTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

const
  DLLName = 'Display';

{ C declarations:

long WINAPI  START( long numsamples, long *preferredchunksize, void **ref );
long WINAPI  INITPARAMS( void **parameters, long *paramsize );
long WINAPI  ANALYZE( void *ref, long numsamples, short *data );
long WINAPI  DISPLAY( void *ref, HDC paintDC, long startsample, long endsample,
                                RECT dspRegion, RECT updRegion );
long WINAPI  USERUI( void *ref, char *parameters, HWND parent );
long WINAPI  END( void *ref );

}
type
  TDispUserUI = function(Ref: pointer; Parameters: PChar; Parent: HWND): integer; stdcall;
  TDispStart = function(NumSamples: DWORD; var PreferredChunkSize: DWORD; var ref: Pointer): integer; stdcall;
  TDispEnd = function(ref: Pointer): integer; stdcall;
  TDispInitParams = function(var Parameters: pointer; var size: DWORD): integer; stdcall;
  TDispDisplay = function(ref: Pointer; PaintDC: HDC; StartSample, EndSample: DWORD;
                      DspRegion, UpdRegion: TRect): integer; stdcall;
  TDispAnalyze = function(ref: Pointer; NumberOfSamples: DWORD; data: Pointer): integer; stdcall;

type
  TDisplayForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { Private declarations }
    DisplayModule: HMODULE;
    DispUserUI: TDispUserUI;
    DispStart:  TDispStart;
    DispEnd:    TDispEnd;
    DispInitParams: TDispInitParams;
    DispDisplay: TDispDisplay;
    DispAnalyze: TDispAnalyze;
    DispRef:     Pointer;
    DispParms:   Pointer;
    DispParmSize: DWORD;
    DispChunkSize: DWORD;
    TotalSamples: DWORD;
    function GetProcAddress(FunctionName: string): pointer;
  public
    { Public declarations }
    procedure LogMessage(const msg: string);
    procedure InitFile(NumberOfSamples: DWORD);
    procedure DoneFile;
    procedure Analyze(data: Pointer; size: Cardinal);
    property ChunkSize: Cardinal read DispChunkSize;
  end;

var
  DisplayForm: TDisplayForm;

implementation

{$R *.DFM}

procedure TDisplayForm.LogMessage;
begin
  Memo1.Lines.Add(msg);
end;

function TDisplayForm.GetProcAddress;
begin
  result := Windows.GetProcAddress(DisplayModule, PChar(FunctionName));
  if not assigned(result) then
    raise Exception.CreateFmt('ERROR: Failed to load function "%s" [%d].', [FunctionName, GetLastError])
  else
    LogMessage('Function ' + FunctionName + ' linked');
end;

procedure TDisplayForm.FormCreate(Sender: TObject);
begin
  DisplayModule := LoadLibrary(DLLName);
  if (DisplayModule = 0) then
     LogMessage(Format('ERROR: Failed to load DLL "%s" [%d].', [DLLName, GetLastError]))
  else
  begin
     LogMessage('DLL "' + DLLName + '" loaded.');
     DispUserUI := GetProcAddress('USERUI');
     DispStart  := GetProcAddress('START');
     DispEnd    := GetProcAddress('END');
     DispInitParams := GetProcAddress('INITPARAMS');
     DispDisplay := GetProcAddress('DISPLAY');
     DispAnalyze := GetProcAddress('ANALYZE');
     DispChunkSize := 44100;
     DispParmSize := 0;
     DispParms := nil;
     DispInitParams(DispParms, DispParmSize);
  end;
end;

procedure TDisplayForm.Panel1Click(Sender: TObject);
begin
  if assigned(DispUserUI) then
  begin
     LogMessage('Calling UserUI');
     DispUserUI(DispRef, DispParms, Handle);
  end;
end;

procedure TDisplayForm.FormDestroy(Sender: TObject);
begin
  if (DisplayModule <> 0) then
  begin
    DoneFile;
    FreeLibrary(DisplayModule);
  end;
end;

procedure TDisplayForm.InitFile;
begin
  try
    DispStart(NumberOfSamples, DispChunkSize, DispRef);
  except
    on e: Exception do
      LogMessage(Format('START Exception: NumberOfSamples=%d msg=%s', [NumberOfSamples, e.Message]));
  end;
  TotalSamples := NumberOfSamples;
end;

procedure TDisplayForm.DoneFile;
begin
  if assigned(DispRef) then
  begin
    DispEnd(DispRef);
    DispRef := nil;
    TotalSamples := 0;
  end;
end;

procedure TDisplayForm.Analyze;
begin
  try
    DispAnalyze(DispRef, size, data);
  except
    on e: Exception do
      LogMessage(Format('ANALYZE Exception: size=%d msg=%s', [size, e.Message]));
  end;
end;

procedure TDisplayForm.PaintBox1Paint(Sender: TObject);
var
   r: TRect;
   s: Cardinal;
begin
  if assigned(DispRef) and (TotalSamples > 0) then
    with Sender as TPaintBox do
    begin
      r := BoundsRect;
      s := (r.Right - r.Left) * 44100;
      if s >= TotalSamples then
          s := TotalSamples - 1;
      DispDisplay(DispRef, Canvas.Handle, 0, s, r, r);
      LogMessage('Painted: ' + InttoStr(s));
    end;
end;

end.
