unit MainLite;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MAIN, ActnList, Menus, ImgList, ComCtrls, ToolWin, StdCtrls, Buttons,
  Spin, VolumeMeter, ExtCtrls, RecordFrm;

type
  TMainFormLite = class(TMainForm)
  private
    { Private declarations }
  protected
    function createAboutBox: TForm; override;
    function createRecordDialog: TDlgRecord; override;
  public
    { Public declarations }
  end;

var
  MainFormLite: TMainFormLite;

implementation

uses AboutLite;

{$R *.DFM}

function TMainFormLite.createAboutBox;
begin
  Result := TAboutBoxLite.Create(self);
end;


function TMainFormLite.createRecordDialog;
begin
  Result := TDlgRecord.Create(self);
end;

end.
