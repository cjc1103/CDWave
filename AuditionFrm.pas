unit AuditionFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, MMSystem;

type
  TAuditionForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    lTrack: TLabel;
    lPlaying: TLabel;
  private
    { Private declarations }
    count: Integer;
    procedure womDone(var Msg: TMessage); message MM_WOM_DONE;
  public
    { Public declarations }
  end;

resourcestring
  lastSecondsStr = 'Last seconds';

implementation

{$R *.DFM}
procedure TAuditionForm.womDone;
begin
  OutputDebugString('WOM DONE!');
  if count <> 0 then
    ModalResult := mrOK
  else begin
    inc(count);
    lPlaying.Caption := lastSecondsStr;
  end;
end;

end.

