inherited MainFormShare: TMainFormShare
  Left = 500
  Top = 187
  PixelsPerInch = 96
  TextHeight = 13
  inherited ActionList1: TActionList
    inherited AutoSplitAction: TAction
      Visible = True
      OnExecute = AutoSplitActionExecute
    end
    inherited TimeSplitAction: TAction
      Visible = True
      OnExecute = TimeSplitActionExecute
    end
  end
end
