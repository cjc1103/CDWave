unit DAEFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, AKRip32, ComCtrls, Buttons, ExtCtrls;

type
  TDAEForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    lVersion: TLabel;
    Label1: TLabel;
    cbDrives: TComboBox;
    lStatus: TLabel;
    lvTracks: TListView;
    Panel4: TPanel;
    btnOK: TBitBtn;
    BitBtn2: TBitBtn;
    Panel5: TPanel;
    eLocation: TEdit;
    btnBrowse: TBitBtn;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    cbJitter: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbDrivesChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
    FCDHandle: HCDROM;
    FCDList: CDLIST;
    function GetActiveCD: PCDREC;
    function GetFileName: string;
    function GetDoJitter: boolean;
  public
    { Public declarations }
    procedure AquireCD;
    procedure ReleaseCD;
    property  CDHandle: HCDROM read FCDHandle;
    property  FileName: string read GetFileName;
    property  DoJitter: boolean read GetDoJitter;
  end;

implementation

{$R *.DFM}

function TDAEForm.GetActiveCD;
begin
  if cbDrives.ItemIndex >= 0 then
  begin
    Result := PCDREC(cbDrives.Items.Objects[cbDrives.ItemIndex]);
  end else
    Result := nil;
end;

procedure TDAEForm.AquireCD;
var
  cdrec: PCDREC;
  MyGetCD: GETCDHAND;
begin
  cdrec := GetActiveCD;
  if cdrec = nil then raise Exception.Create('No drive selected');
  with MyGetCD do
  begin
    Size := SizeOf(GETCDHAND);
    Ver := 1; //-- Version of AKRip32 dll. Should be set to 1 as of this release.
    ha := cdrec.ha; //-- Host adapter number of CD-ROM unit
    tgt := cdrec.tgt; //-- Target number of CD-ROM unit
    lun := cdrec.lun; //-- LUN of CD-ROM unit
    readType := CDR_ANY; //-- Read algorithm to use: can be one of CDR_ANY, CDR_ATAPI1, CDR_ATAPI2, CDR_READ10, CDR_READ_D8, CDR_READ_D4 or CDR_READ_D4_1
    jitterCorr := DoJitter; //-- Ignored by the current implementation
    numJitter := 1; //-- The number of frames to check. Only affects ReadCDAudioLBAEx.
    numOverlap := 3; //-- The number of frames to overlap. Only affects ReadCDAudioLBAEx.
  end;
  FCDHandle := GetCDHandle(MygetCD);
  if FCDHandle = 0 then
    raise Exception.Create('Unable to open CD device');
end;

procedure TDAEForm.ReleaseCD;
begin
  CloseCDHandle(FCDHandle);
  FCDHandle := 0;
end;

procedure TDAEForm.FormCreate(Sender: TObject);
var
  Ver: DWord;
  i: Integer;
begin
  cbDrives.Items.Clear;
  if AKRipDLLLoaded then
  begin
    if @GetAKRipDllVersion <> nil then
    begin
      Ver := GetAKRipDllVersion;
      lVersion.Caption := Format('AK-Rip V%d.%d', [Hi(Ver), Lo(Ver)]);
    end else
      lVersion.Caption := 'MyAK-Rip';
    FCDList.Max := MAXCDLIST;
    GetCDList(FCDList);
    for i := 0 to FCDList.num-1 do
    begin
      CbDrives.Items.AddObject(FCDList.CD[i].id, @FCDList.CD[i]);
    end;
  end else
    lVersion.Caption := 'AKRIP32.DLL not found';
end;

function LBAToTime(addr: DWORD): string;
begin
 Result := Format('%d:%.2d.%.2d',
                  [addr div (60*75), (addr div 75) mod 60, addr mod 75]);
end;

procedure TDAEForm.Button1Click(Sender: TObject);
var
  MyTOC: TOC;
  i: Cardinal;
  addr: DWORD;
  ADR: Byte;
  item: TListItem;
begin
  lvTracks.Items.Clear;
  FillChar(MyToc, sizeof(MyTOC), 0);
  AquireCD;
  try
    //ModifyCDParms(FCDHandle, CDP_MSF, DWORD(true));
    if ReadTOC(FCDHandle, MyTOC, false) <> 1 then
      raise Exception.Create(ReadTOCError);
    lStatus.Caption := Format('%d tracks', [MyToc.lastTrack - MyToc.firstTrack + 1]);
    for i := MyToc.firstTrack to MyToc.lastTrack do
    begin
        //addr := MSFtoLBA(MyToc.tracks[i - 1].addr);
        addr := BigToLittleEndian(MyToc.tracks[i - 1].addr);
        ADR := MyToc.tracks[i - 1].ADR;
        Item := lvTracks.Items.Add;
        Item.Caption := Format('%.2d', [i]);
        Item.SubItems.Add(IntToStr(addr));
        Item.SubItems.Add(LBAToTime(addr));
        //Item.SubItems.Add(LBAToTime(MSFtoLBA(MyToc.tracks[i].addr) - addr));
        Item.SubItems.Add(LBAToTime(BigToLittleEndian(MyToc.tracks[i].addr) - addr));
        if (ADR and 4) <> 0 then
        begin
          Item.SubItems.Add('Data');
          Item.Checked := False;
        end else begin
          Item.SubItems.Add('Audio');
          Item.Checked := True;
        end;
    end;
  finally
    ReleaseCD;
  end;
end;

procedure TDAEForm.cbDrivesChange(Sender: TObject);
begin
  try
    Button1Click(sender);
  except
    on e: Exception do
      lStatus.Caption := e.Message;
  end;
end;

procedure TDAEForm.btnBrowseClick(Sender: TObject);
var
  dir, f: string;
begin
  f := eLocation.Text;
  if f <> '' then
  begin
    dir := ExtractFilePath(f);
    if dir <> '' then SaveDialog1.InitialDir := dir;
    SaveDialog1.FileName := f;
  end;
  if SaveDialog1.Execute then eLocation.Text := SaveDialog1.FileName;
end;

//-- helper function for ripping: writes a simple
//-- header for a proper WAV-file

procedure writeWavHeader(AFileStream: TFileStream; len: Cardinal);
type
  PWAVHDR = ^WAVHDR;
  WAVHDR = packed record
    riff: array[0..3] of Char; {* must be "RIFF"                *}
    len: DWord; {* #bytes + 44 - 8               *}
    cWavFmt: array[0..7] of Char; {* must be "WAVEfmt"             *}
    dwHdrLen: DWord;
    wFormat: Word;
    wNumChannels: Word;
    dwSampleRate: DWord;
    dwBytesPerSec: DWord;
    wBlockAlign: Word;
    wBitsPerSample: Word;
    cData: array[0..3] of Char; {* must be "data"               *}
    dwDataLen: DWord; {* #bytes                       *}
  end;
var
  wav: WAVHDR;
begin
  wav.riff := 'RIFF';
  wav.len := len + 44 - 8;
  wav.cWavFmt := 'WAVEfmt ';
  wav.dwHdrLen := 16;
  wav.wFormat := 1;
  wav.wNumChannels := 2;
  wav.dwSampleRate := 44100;
  wav.dwBytesPerSec := 44100 * 2 * 2;
  wav.wBlockAlign := 4;
  wav.wBitsPerSample := 16;
  wav.cData := 'data';
  wav.dwDataLen := len;
  AFileStream.Write(wav, SizeOf(WAVHDR));
end;


function TDAEForm.GetFileName;
begin
  Result := eLocation.Text;
end;

function TDAEForm.GetDoJitter;
begin
  Result := cbJitter.Checked;
end;

end.

