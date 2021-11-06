unit WaveLoad;

interface

uses
  Classes, Sysutils, Windows, MMsystem, WaveUtil;

type
  TWaveLoader = class(TThread)
  private
    { Private declarations }
    WaveFile: THandle;
  protected
    procedure Execute; override;
  public
    Size: Integer;
    SectorStatsList: PSectorStatsList;
    constructor Create(aWaveFile: string; AtOffset, aSize: Integer;
                 aSectorStatsList: PSectorStatsList; aOnTerminate: TNotifyEvent);
  end;

implementation

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TWaveLoader.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TWaveLoader }

procedure TWaveLoader.Execute;
var i, NumRead: Integer;
   Sector: array[0..2351] of Char;
begin
  { Place thread code here }
  for i := 0 to Size-1 do
  begin
    ReadFile(Wavefile, Sector, 2352, NumRead, nil);
    SectorStatsList[i] := SectorLevel(@Sector, NumRead shr 2);
    if Terminated then Break;
  end;
  CloseHandle(WaveFile);
  if Terminated then FreeMem(SectorStatsList);
end;

constructor TWaveLoader.Create;
begin
  inherited Create(False);
  WaveFile := CreateFile(PChar(aWaveFile), GENERIC_READ, FILE_SHARE_READ, nil,
               OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if WaveFile = INVALID_HANDLE_VALUE then
    raise Exception.Create('CreateFile() failed!');
  SetFilePointer(WaveFile, AtOffset, nil, FILE_BEGIN);
  Size := aSize;
  SectorStatsList := aSectorStatsList;
  FreeOnterminate := False;
  OnTerminate := aOnTerminate;
end;

end.
