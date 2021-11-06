unit KeyGenFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, License, ClipBrd;

type
  TKeyGenform = class(TForm)
    eUsername: TEdit;
    eRegkey: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure eUsernameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  KeyGenform: TKeyGenform;

implementation

{$R *.DFM}

procedure TKeyGenform.FormCreate(Sender: TObject);
begin
  RegInfo := TRegInfo.Create($A3B75123, $CD3F5681, $0918F543, '');
end;

procedure TKeyGenform.eUsernameChange(Sender: TObject);
var Key: TKey;
begin
  Key := RegInfo.Verify(eUsername.Text);
  eRegKey.Text := Format('%.8x%.8x', [Key.lo, Key.hi]);
  Clipboard.AsText :=
    'User name: ' + eUsername.Text + #13#10 +
    'Reg. code: ' + eRegKey.Text + #13#10;
end;

end.
