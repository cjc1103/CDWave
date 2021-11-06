program CDWavLT;

uses
  Forms,
  Main in 'MAIN.PAS' {MainForm},
  MainLite in 'MainLite.pas' {MainFormLite},
  About in 'About.pas' {AboutBox},
  AboutLite in 'AboutLite.pas' {AboutBoxLite};

{$R *.RES}

begin
  Application.Initialize;
  Application.HelpFile := 'CDWAVLT.hlp';
  Application.Title := 'CD Wave Editor Lite';
  Application.CreateForm(TMainFormLite, MainFormLite);
  Application.Run;
end.
