unit MainShare;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MAIN, ActnList, Menus, ImgList, ComCtrls, ToolWin, StdCtrls, Buttons,
  Spin, VolumeMeter, ExtCtrls, RecordFrm;

type
  TMainFormShare = class(TMainForm)
  private
    { Private declarations }
  end;

var
  MainFormShare: TMainFormShare;

implementation

uses License, AboutShare, AboutFull, AutoCut, WaveUtil, DlgRecShare, TimeSplitFrm;
{$R *.DFM}


end.
