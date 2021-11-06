unit AboutFull;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  About, StdCtrls, ExtCtrls;

type
  TAboutBoxFull = class(TAboutBox)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBoxFull: TAboutBoxFull;

implementation

uses License;

{$R *.DFM}
resourcestring
  RegStr = 'Registered to: ';

procedure TAboutBoxFull.FormCreate(Sender: TObject);
begin
  inherited;
  Comments2.Caption := RegStr + RegInfo.RegisteredUser;
end;

end.
