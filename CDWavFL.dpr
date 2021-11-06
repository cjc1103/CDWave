program CDWavFL;

uses
  Forms,
  Main in 'MAIN.PAS' {MainForm},
  MainShare in 'MainShare.pas' {MainFormShare},
  MainFull in 'MainFull.pas' {MainFormFull},
  About in 'About.pas' {AboutBox},
  AboutFull in 'AboutFull.pas' {AboutBoxFull};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CD Wave Editor';
  Application.CreateForm(TMainFormFull, MainFormFull);
  Application.Run;
end.
