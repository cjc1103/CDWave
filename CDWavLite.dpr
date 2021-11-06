program CDWavLite;

uses
  Forms,
  Main in 'MAIN.PAS' {MainForm},
  MainLite in 'MainLite.pas' {MainFormLite};

{$R *.RES}

begin
  Application.Initialize;
  Application.HelpFile := 'CDWAVLT.hlp';
  Application.Title := 'CD Wave Editor Lite';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
