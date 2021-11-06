#define MyAppName "CD Wave Editor"
#define MyAppVersion GetFileVersion("..\Execute\CDWav.exe")
#define MyProductVersion GetStringFileInfo("..\Execute\CDWav.exe", PRODUCT_VERSION)
#define NoDotVersion Copy(MyProductVersion, 1, Pos('.', MyProductVersion)-1) + Copy(MyProductVersion, Pos('.', MyProductVersion)+1)

#pragma message "Detailed version info: " + MyAppVersion
#pragma message "Product version info: " + MyProductVersion
#pragma message "Without dots: " + NoDotVersion

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppName} {#MyProductVersion}
AppVersion={#MyAppVersion}
VersionInfoVersion={#MyAppVersion}
VersionInfoTextVersion={#MyProductVersion}
AppCopyright=Copyright © 1997-2008 Mike Looijmans
DefaultDirName={pf}\CD Wave
DefaultGroupName=CD Wave
UninstallDisplayIcon={app}\CDWav.exe
Compression=lzma/ultra
SolidCompression=yes
AppPublisher=MiLo Software
AppPublisherURL=http://www.cdwave.com/
AppSupportURL=http://www.cdwave.com/contact.html
AppUpdatesURL=http://www.cdwave.com/download.html
DisableStartupPrompt=yes
LicenseFile=license.txt
VersionInfoDescription=CD Wave Editor Setup
OutputBaseFilename=cdwav{#NoDotVersion}

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "nl"; MessagesFile: "compiler:Languages/Dutch.isl"; LicenseFile: license_nl.txt

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\Execute\CDWav.exe"; DestDir: "{app}"
Source: "..\Execute\CDWav.nl"; DestDir: "{app}"
; Source: "..\Execute\CDWav.en"; DestDir: "{app}"
Source: "..\Execute\CDWav.hlp"; DestDir: "{app}"
Source: "..\Execute\CDWav.cnt"; DestDir: "{app}"
Source: "..\Execute\vorbis.dll"; DestDir: "{app}"
Source: "..\Execute\libFLAC.dll"; DestDir: "{app}"
Source: "..\Execute\MyAKRIP.dll"; DestDir: "{app}"
Source: "..\Execute\MACDll.dll"; DestDir: "{app}"
Source: "..\Execute\lame_enc.dll"; DestDir: "{app}"
Source: "unicows.dll"; DestDir: "{sys}"; Flags: uninsneveruninstall
; Source: "Readme.txt"; DestDir: "{app}"; Flags: isreadme

[INI]
Filename: "{app}\CDWave.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.cdwave.com/"
Filename: "{app}\CDWaveFAQ.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.cdwave.com/faq.html"

[Icons]
Name: "{group}\CD Wave Editor"; Filename: "{app}\CDWav.exe"
Name: "{group}\{cm:ProgramOnTheWeb,CD Wave}"; Filename: "{app}\CDWave.url"
Name: "{group}\CD Wave FAQ"; Filename: "{app}\CDWaveFAQ.url"
Name: "{userdesktop}\CD Wave Editor"; Filename: "{app}\CDWav.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\CD Wave Editor"; Filename: "{app}\CDWav.exe"; Tasks: quicklaunchicon

[UninstallDelete]
Type: files; Name: "{app}\CDWave.url"
Type: files; Name: "{app}\CDWaveFAQ.url"

