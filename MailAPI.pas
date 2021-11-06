unit MailAPI;

interface

uses
  Classes, Windows, Messages, SysUtils, Graphics, Controls, Forms, Dialogs,
  MAPI;

const
  crMail = 6;
  CursorLoaded: Boolean = False;

type
{  PMapiRecipDescList = ^TMapiRecipDescList;
  TMapiRecipDescList = array[0..MAXINT div sizeof(T MapiRecipDesc)-1] of TMapiRecipDesc;
  TMailList = class(TStrings)
  private
    FList: PMapiRecipDescList;
    FCount,
    FCapacity: Integer;
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
//    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
//    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
//    procedure SetUpdateState(Updating: Boolean); override;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
  end;
 }
  TMailAPI = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FSubject:string;
    FSessionHandle: THandle;
    FProfile: string;
    FPassword: string;
    FRecipDesc: PMapiRecipDesc;
    FRecipCount: Cardinal;
    FAsAttach: Boolean;
//    FMailList: TMailList;
    class procedure MapiError(Code: Cardinal);
    function GetActive: Boolean;
    procedure SetActive(Value: boolean);
  public
    { Public declarations }
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure LogOff;
    procedure LogOn;
    procedure SendMail(const Body: TStrings);
    procedure GetRecips;
    property SessionHandle: THandle read FSessionHandle;
//    property MailList: TMailList read FMailList;
    property RecipDesc: PMapiRecipDesc read FRecipDesc;
  published
    { Published declarations }
    property Active: boolean read GetActive write SetActive;
    property Subject: string read FSubject write FSubject;
    property Profile: string read FProfile write FProfile;
    property PassWord: string read FPassWord write FPassWord;
    property AsAttach: Boolean read FAsAttach write FAsAttach;
    property RecipCount: Cardinal read FRecipCount;
  end;

procedure Register;

implementation

uses Consts;

function GetTempFile(const BaseName: string): string;
var Path: array[0..255] of char;
    Res:  array[0..255] of char;
begin
  GetTempPath(sizeof(Path), Path);
  GetTempFileName(Path, PChar(BaseName), 0, Res);
  Result := string(res);
end;

constructor TMailAPI.Create(AOwner: Tcomponent);
var
  Cursor: HCursor;
begin
  inherited;
  FSessionHandle := INVALID_HANDLE_VALUE;
  if not CursorLoaded then
  begin
    Cursor := LoadCursor(HInstance, 'MAIL');
    CursorLoaded := Cursor <> 0;
    if CursorLoaded then
      Screen.Cursors[crMail] := Cursor;
  end;
end;

destructor TMailAPI.Destroy;
begin
  try
    if FRecipDesc <> nil then MAPIFreeBuffer(FRecipDesc);
    LogOff;
  finally
    Inherited;
  end;
end;

procedure TMailAPI.LogOff;
begin
  if FSessionHandle <> INVALID_HANDLE_VALUE then
  begin
    MapiError(MAPILogoff(FSessionHandle, 0, 0, 0));
    FSessionHandle := INVALID_HANDLE_VALUE;
  end;
end;

procedure TMailAPI.LogOn;
begin
  MapiError(MAPILogon(0, PChar(Pointer(Profile)), PChar(Pointer(Password)),
                      MAPI_LOGON_UI, 0, @FSessionHandle));
end;

class procedure TMailAPI.MapiError(Code: Cardinal);
begin
  if Code <> SUCCESS_SUCCESS then
    raise Exception.CreateFmt('MAPI mail error code = %d', [Code]);
end;

function TMailAPI.GetActive;
begin
  Result := (FSessionHandle <> INVALID_HANDLE_VALUE)
end;

procedure TMailAPI.SetActive;
begin
  if Value <> GetActive then
  begin
    if Value then
      LogOn
    else
      LogOff;
  end;
end;

procedure TMailAPI.SendMail;
var
  Msg: TMapiMessage;
  MapiFiles: TMapiFileDesc;
  Flags: Integer;
  BodyText: string;
  OldCursor: TCursor;
begin
  Active := True;
  Flags := MAPI_DIALOG;
  FillChar(Msg, sizeof(Msg), 0);
  Msg.lpszSubject := PChar(Subject);
  OldCursor := Screen.Cursor;
  if CursorLoaded then
    Screen.Cursor := crMail
  else
    Screen.Cursor := crHourGlass;
  try
    if assigned(Body) then
    begin
      if AsAttach then
      begin
        BodyText := GetTempFile('CM');
        Body.SaveToFile(BodyText);
        Fillchar(MapiFiles, sizeof(MapiFiles), 0);
        MapiFiles.lpszPathName := PChar(BodyText);
        MapiFiles.lpszFileName := 'MessageText.txt';
        MapiFiles.nPosition := Cardinal(-1);
        Msg.nFileCount := 1;
        Msg.lpFiles := @MapiFiles;
        Msg.lpszNoteText := '(See Attachment)';
      end else
      begin
        BodyText := Body.Text;
        Msg.lpszNoteText := PChar(BodyText);
      end;
    end;
    if RecipDesc <> nil then
    begin
      Msg.nRecipCount := RecipCount;
      Msg.lpRecips := RecipDesc;
      Flags := Flags and not MAPI_DIALOG;
    end;
    MapiError(MAPISendMail(0, 0, Msg, Flags, 0));
  finally
    if AsAttach and (BodyText <> '') then DeleteFile(BodyText);
    Screen.Cursor := OldCursor;
  end;
end;

procedure TMailAPI.GetRecips;
var
  r : Integer;
  nNewRecips: Integer;
  NewRecips: PMapiRecipDesc;
begin
  Active := True;
  nNewRecips := 0;
  NewRecips := nil;
  r :=  MAPIAddress(FSessionHandle, 0, 'Select recipients', 4, nil, RecipCount,
                    FRecipDesc^, 0, 0, @nNewRecips, NewRecips);
  if r = SUCCESS_SUCCESS then
  begin
    if FRecipDesc <> nil then MAPIFreeBuffer(FRecipDesc);
    FRecipCount := nNewRecips;
    FRecipDesc  := NewRecips;
  end else
    if r <> MAPI_E_USER_ABORT then
      Raise Exception.CreateFmt('MAPIAddress failed, code=%d', [r]);
end;


{function TMailList.Get;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := string(FList[Index].lpszName);
end;

function TMailList.GetCapacity;
begin
  Result := FCapacity
end;

function TMailList.GetCount;
begin
  Result := FCount
end;

function TMailList.Add;
begin
  Result := FCount;
  Insert(Result, S);
end;

function TMailList.IndexOf;
begin
  for Result := 0 to FCount - 1 do
  begin
    if StrComp(PChar(S), FList[Result].lpszName) = 0 then Exit;
  end;
  Result := -1;
end;

procedure TMailList.Put;
begin
//ni
end;

procedure TMailList.SetCapacity;
begin
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TMapiRecipDesc));
    FCapacity := NewCapacity;
  end;
end;

procedure TMailList.Clear;
begin
  SetCapacity(0);
end;}

procedure Register;
begin
  RegisterComponents('Handy', [TMailAPI]);
end;

end.

