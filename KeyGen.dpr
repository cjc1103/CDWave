program KeyGen;
{$APPTYPE CONSOLE}
uses
  SysUtils, License;

var user: string;
    RegInfo: TRegInfo;
    Key: TKey;
begin
  // Insert user code here
  RegInfo := TRegInfo.Create($A3B75123, $CD3F5681, $0918F543, '');
  while true do
  begin
  Write('user name: ');
  ReadLn(user);
  if user = '' then exit;
  Key := RegInfo.verify(user);
  Writeln(' Reg code: ', Format('%.8x%.8x', [Key.lo, Key.hi]));
  end;
end.