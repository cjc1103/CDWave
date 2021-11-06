unit AboutShare;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  About, StdCtrls, ExtCtrls;

type
  TAboutBoxShare = class(TAboutBox)
    btnRegister: TButton;
    procedure btnRegisterClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses RegisterFrm;
{$R *.DFM}

procedure TAboutBoxShare.btnRegisterClick(Sender: TObject);
begin
  with TRegisterForm.Create(self) do
  try
    if ShowModal = mrOk then
        self.ModalResult := mrRetry;
  finally
    Free;
  end;
end;

end.
