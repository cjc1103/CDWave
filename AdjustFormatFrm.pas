unit AdjustFormatFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormatFrm, StdCtrls, Buttons;

type
  TAdjustFormatForm = class(TFormatForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AdjustFormatForm: TAdjustFormatForm;

implementation

{$R *.DFM}

end.
