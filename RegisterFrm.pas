unit RegisterFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TRegisterForm = class(TForm)
    Label3: TLabel;
    btnOK: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    eRegname: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    eRegcode: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RegisterForm: TRegisterForm;

implementation

{$R *.DFM}

uses License;

procedure TRegisterForm.BitBtn1Click(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TRegisterForm.btnOKClick(Sender: TObject);
begin
  try
     RegInfo.EnterCode(eRegname.Text, eRegCode.Text);
  except
     ModalResult := mrCancel;
     raise;
  end;
end;

procedure TRegisterForm.FormCreate(Sender: TObject);
begin
  eRegname.Text := RegInfo.RegisteredUser;
end;

end.
