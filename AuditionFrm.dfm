object AuditionForm: TAuditionForm
  Left = 207
  Top = 289
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Bounds check'
  ClientHeight = 33
  ClientWidth = 209
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 107
    Height = 33
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object lTrack: TLabel
      Left = 8
      Top = 0
      Width = 43
      Height = 13
      Caption = 'Playing...'
    end
    object lPlaying: TLabel
      Left = 8
      Top = 16
      Width = 62
      Height = 13
      Caption = 'First seconds'
    end
  end
  object Panel2: TPanel
    Left = 107
    Top = 0
    Width = 102
    Height = 33
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 8
      Top = 4
      Width = 89
      Height = 25
      TabOrder = 0
      Kind = bkCancel
    end
  end
end
