unit DlgJumpTo;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, Mask;

type
  TJumpToDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    eJumpTo: TMaskEdit;
  private
    { Private declarations }
    function getJumpToPos: string;
  public
    { Public declarations }
    property JumpToPos: string read getJumpToPos;
  end;

implementation

{$R *.DFM}

function TJumpToDlg.getJumpToPos: string;
begin
  result := eJumpTo.Text;
end;


end.
