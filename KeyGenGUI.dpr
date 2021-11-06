program KeyGenGUI;

uses
  Forms,
  KeyGenFrm in 'KeyGenFrm.pas' {KeyGenform};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TKeyGenform, KeyGenform);
  Application.Run;
end.
