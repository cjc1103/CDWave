object KeyGenform: TKeyGenform
  Left = 207
  Top = 107
  BorderStyle = bsDialog
  Caption = 'CD Wave'
  ClientHeight = 62
  ClientWidth = 137
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object eUsername: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = eUsernameChange
  end
  object eRegkey: TEdit
    Left = 8
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 1
  end
end
