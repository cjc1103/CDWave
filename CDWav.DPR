program CDWav;

{%File 'CDWav.DPR'}

uses
  Forms,
  MAIN in 'MAIN.PAS' {MainForm},
  About in 'About.pas' {AboutBox},
  working in 'working.pas' {LoadingDlg},
  Utils in 'Utils.pas',
  AutoCut in 'AutoCut.pas' {AutoCutDlg},
  Settings in 'Settings.pas' {SettingsDialog},
  RecThd in 'RecThd.pas',
  mmutil in 'mmutil.pas',
  FormatFrm in 'FormatFrm.pas' {FormatForm},
  RecordFrm in 'RecordFrm.pas' {DlgRecord},
  SelectDirFrm in 'SelectDirFrm.pas' {SelectDirDlg},
  FileAssociations in 'FileAssociations.pas' {FileAssociationDlg},
  ShellUtils in 'ShellUtils.pas',
  AboutShare in 'AboutShare.pas' {AboutBoxShare},
  DlgRecShare in 'DlgRecShare.pas' {DlgRecordShare},
  Properties in 'Properties.pas' {FilePropertiesForm},
  TimeSplitFrm in 'TimeSplitFrm.pas' {TimeSplitForm},
  DlgJumpTo in 'DlgJumpTo.pas' {JumpToDlg},
  AboutFull in 'AboutFull.pas' {AboutBoxFull},
  License in 'License.pas',
  RegisterFrm in 'RegisterFrm.pas' {RegisterForm},
  AuditionFrm in 'AuditionFrm.pas' {AuditionForm},
  FLAC_Enc in '..\Components\FLAC_Enc.pas',
  DAEFrm in 'DAEFrm.pas' {DAEForm},
  WaveUtil in 'WaveUtil.pas',
  WaveIO in 'WaveIO.pas',
  WaveWriters in 'WaveWriters.pas',
  APE in '..\Components\APE.pas',
  DirNameFrm in 'DirNameFrm.pas' {DirNameForm};

{$R *.RES}
begin
  Application.Title := 'CD Wave Editor';
  Application.HelpFile := 'CDWAV.hlp';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
