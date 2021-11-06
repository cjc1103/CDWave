object DirNameForm: TDirNameForm
  Left = 354
  Top = 258
  ActiveControl = eDirName
  BorderStyle = bsDialog
  Caption = 'Create directory'
  ClientHeight = 94
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblDirName: TLabel
    Left = 8
    Top = 8
    Width = 123
    Height = 13
    Caption = 'Enter new directory name:'
  end
  object eDirName: TEdit
    Left = 8
    Top = 32
    Width = 441
    Height = 21
    TabOrder = 0
    Text = 'eDirName'
  end
  object btnOk: TBitBtn
    Left = 288
    Top = 64
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 376
    Top = 64
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
end
