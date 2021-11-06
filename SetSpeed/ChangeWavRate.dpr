program ChangeWavRate;

uses
  Forms,
  SetSpeedMain in 'SetSpeedMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Change WAV Rate';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
