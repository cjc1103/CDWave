unit About;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments1: TLabel;
    OKButton: TButton;
    Label1: TLabel;
    Comments2: TLabel;
    Release: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lPlatform: TLabel;
    lBuild: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.DFM}

uses utils, VersionInfo;

resourcestring
  WindowsStr = 'Windows 98 or 95 OSR2';
  VersionStr = 'Version';

procedure TAboutBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
var
  VersionInfo: TOSVERSIONINFO;
  info: TVersionInfo;
begin
  info := TVersionInfo.Create;
  Version.Caption := VersionStr + ' ' + info.ProductVersion;
  info.Free;
  FillChar(VersionInfo, sizeof(VersionInfo), 0);
  VersionInfo.dwOSVersionInfoSize := sizeof(VersionInfo);
  GetVersionEx(VersionInfo);
  if VersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
    lPlatform.Caption := 'Windows NT'
  else
  begin
    if (VersionInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and
       (LOWORD(VersionInfo.dwBuildNumber) > 1000) then
      lPlatform.Caption := WindowsStr
    else
      lPlatform.Caption := 'Windows 95 (pre OSR2)';
  end;
  lBuild.Caption := Format('%.4x.%.4x', [HIWORD(VersionInfo.dwBuildNumber),
                 LOWORD(VersionInfo.dwBuildNumber)]);
end;

end.

