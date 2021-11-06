unit License;

interface

type
   TKey = record
            lo, hi: Cardinal;
          end;

type TRegInfo = class
        private
          user: string;
          key: TKey;
          keya, keyb, keyc: Cardinal;
          RegKey: string;
          procedure Load;
          function isRegistered: Boolean;
        public
          constructor Create(a,b,c: Cardinal; const ARegKey: string);
          procedure EnterCode(const auser: string; const acode: string);
          property RegisteredUser: string read user;
          property Registered: boolean read isRegistered;
          function verify(const s: string): TKey;
        end;

var RegInfo: TRegInfo;

implementation

uses Registry, SysUtils, Dialogs;

resourcestring
  NoValidKeyStr = 'Key is not valid.';
  ThankRegStr = 'Thank you for registering';

function Code(const s: string; a, b: Cardinal): Cardinal;
var i: integer;
begin
  result := b;
  for i := 1 to length(s) do
    result := (result * a) + ord(s[i]);
end;

constructor TRegInfo.Create;
begin
  keya := a;
  keyb := b;
  keyc := c;
  RegKey := ARegKey;
  if ARegKey <> '' then Load;
end;

function TRegInfo.isRegistered(): Boolean;
var vkey: TKey;
begin
  Result := false;
  if (user <> '') and (key.lo <> 0) then
  begin
    vkey := verify(IntToStr(DiskSize(ord(ParamStr(0)[1]) - ord('A'))) + user);
    result := (vkey.lo = key.lo) and (vkey.hi = key.hi);
  end;
end;

procedure TRegInfo.Load;
var r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.OpenKey(RegKey, False);
    try
      user := r.ReadString('User');
      r.ReadBinaryData('Code', key, sizeof(key));
    except
      user := '';
      key.lo := 0;
      key.hi := 0;
    end;
  finally
    r.Free;
  end;
end;

function StringToKey(const s: string): TKey;
var c: string;
    v: Cardinal;
    i: Integer;
begin
  c := '$' + Copy(s, 1, 8);
  val(c, v, i);
  if i<>0 then raise Exception.Create(NoValidKeyStr);
  result.lo := v;
  c := '$' + Copy(s, 9, 8);
  val(c, v, i);
  if i<>0 then raise Exception.Create(NoValidKeyStr);
  result.hi := v;
end;

procedure TRegInfo.EnterCode;
var newkey, vkey: TKey;
    ss: int64;
    s: string;
    r: TRegistry;
begin
  user := auser;
  newkey := StringToKey(ACode);
  vkey := verify(auser);
  if (vkey.lo = newkey.lo) and (vkey.hi = newkey.hi) then
  begin
    ss := DiskSize(ord(ParamStr(0)[1]) - ord('A'));
    s := IntToStr(ss) + user;
    newkey := verify(s);
    r := TRegistry.Create;
    try
      r.OpenKey(RegKey, True);
      r.WriteString('User', auser);
      r.WriteBinaryData('Code', newkey, sizeof(newkey));
    finally
      r.Free;
    end;
    MessageDlg(ThankRegStr, mtInformation, [mbOk], 9001);
  end else
    raise Exception.Create(NoValidKeyStr);
  Load;
end;

function TRegInfo.verify;
begin
  result.lo := Code(s, keya, keyb);
  result.hi := Code(s, keyc, result.lo);
end;

end.

