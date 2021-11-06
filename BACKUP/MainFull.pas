unit MainFull;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MainShare, ActnList, Menus, ImgList, ComCtrls, ToolWin, StdCtrls,
  Buttons, Spin, VolumeMeter, ExtCtrls;

type
  TMainFormFull = class(TMainFormShare)
  private
    { Private declarations }
  protected
    function createAboutBox: TForm; override;
  public
    { Public declarations }
  end;

var
  MainFormFull: TMainFormFull;

implementation

{$R *.DFM}

uses AboutFull;

function TMainFormFull.createAboutBox;
begin
  Result := TAboutBoxFull.Create(self);
end;

end.
