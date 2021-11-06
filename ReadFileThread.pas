unit ReadFileThread;

interface

uses
  Classes, MMSystem, syncobjs;

type
  TFileReaderThd = class(TThread)
  private
    { Private declarations }
    WaveFile: HMMIO;
    Event: TEvent;
    BytesToRead,
    BytesRead: Integer;
    Buffer: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(AWaveFile: HMMIO);
    destructor  Destroy; override;
  end;

implementation

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure FileReaderThd.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ FileReaderThd }

procedure TFileReaderThd.Execute;
var wr: TWaitResult;
begin
  { Place thread code here }
  while not Terminated do
  begin
    wr := Event.WaitFor(60000);
    if wr = wrSignaled then
    begin
      BytesRead := mmioRead(Wavefile, Buffer, BytesToRead);

    end;
  end;
end;

constructor TFileReaderThd.Create(AWaveFile: HMMIO);
begin
  inherited Create(True);
  WaveFile := AWaveFile;
  Event := TEvent.Create(nil, False, False, '');
  Resume;
end;

destructor TFileReaderThd.Destroy;
begin
 Event.Free;
 inherited;
end;


end.
