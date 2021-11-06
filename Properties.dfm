object FilePropertiesForm: TFilePropertiesForm
  Left = 225
  Top = 137
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Properties'
  ClientHeight = 241
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 177
    Height = 113
    Caption = 'Audio format'
    TabOrder = 0
    object Label1: TLabel
      Left = 23
      Top = 15
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'Sample rate:'
    end
    object Label2: TLabel
      Left = 8
      Top = 39
      Width = 74
      Height = 13
      Alignment = taRightJustify
      Caption = 'Bits per sample:'
    end
    object Label3: TLabel
      Left = 35
      Top = 63
      Width = 47
      Height = 13
      Alignment = taRightJustify
      Caption = 'Channels:'
    end
    object Label4: TLabel
      Left = 12
      Top = 87
      Width = 70
      Height = 13
      Alignment = taRightJustify
      Caption = 'Avg. data rate:'
    end
    object Label5: TLabel
      Left = 144
      Top = 16
      Width = 16
      Height = 13
      Caption = 'Hz.'
    end
    object Label6: TLabel
      Left = 144
      Top = 88
      Width = 21
      Height = 13
      Caption = 'Bps.'
    end
    object lSamplesPerSec: TStaticText
      Left = 88
      Top = 16
      Width = 49
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00000'
      TabOrder = 0
    end
    object lBitsPerSample: TStaticText
      Left = 88
      Top = 40
      Width = 25
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00'
      TabOrder = 1
    end
    object lChannels: TStaticText
      Left = 88
      Top = 64
      Width = 25
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '0'
      TabOrder = 2
    end
    object lAvgBytesPerSec: TStaticText
      Left = 88
      Top = 88
      Width = 49
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '000000'
      TabOrder = 3
    end
  end
  object BitBtn1: TBitBtn
    Left = 147
    Top = 200
    Width = 75
    Height = 33
    TabOrder = 1
    Kind = bkOK
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 128
    Width = 177
    Height = 65
    Caption = 'Wave file'
    TabOrder = 2
    object Label7: TLabel
      Left = 8
      Top = 16
      Width = 79
      Height = 13
      Alignment = taRightJustify
      Caption = 'Total data bytes:'
    end
    object Label8: TLabel
      Left = 13
      Top = 40
      Width = 90
      Height = 13
      Alignment = taRightJustify
      Caption = 'Duration (mm:ss:ff):'
    end
    object lWaveSize: TStaticText
      Left = 96
      Top = 16
      Width = 73
      Height = 17
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '0'
      TabOrder = 0
    end
    object lTotalTime: TStaticText
      Left = 120
      Top = 40
      Width = 49
      Height = 17
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00:00.00'
      TabOrder = 1
    end
  end
  object GroupBox3: TGroupBox
    Left = 192
    Top = 8
    Width = 169
    Height = 185
    Caption = 'Dynamics'
    TabOrder = 3
    object Label9: TLabel
      Left = 8
      Top = 16
      Width = 140
      Height = 13
      Caption = 'Maximum sample level (16-bit)'
    end
    object Label10: TLabel
      Left = 56
      Top = 32
      Width = 35
      Height = 13
      Caption = '/32767'
    end
    object Label11: TLabel
      Left = 136
      Top = 32
      Width = 8
      Height = 13
      Caption = '%'
    end
    object Label12: TLabel
      Left = 56
      Top = 56
      Width = 35
      Height = 13
      Caption = '/32768'
    end
    object Label13: TLabel
      Left = 136
      Top = 56
      Width = 8
      Height = 13
      Caption = '%'
    end
    object Label14: TLabel
      Left = 8
      Top = 88
      Width = 126
      Height = 13
      Caption = 'Number of clipped sectors:'
    end
    object lMaxSampleValue: TStaticText
      Left = 12
      Top = 32
      Width = 45
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00000'
      TabOrder = 0
    end
    object lMaxSamplePercent: TStaticText
      Left = 112
      Top = 32
      Width = 24
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00'
      TabOrder = 1
    end
    object lMinSampleValue: TStaticText
      Left = 12
      Top = 56
      Width = 45
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00000'
      TabOrder = 2
    end
    object lMinSamplePercent: TStaticText
      Left = 112
      Top = 56
      Width = 24
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00'
      TabOrder = 3
    end
    object lClippedSectors: TStaticText
      Left = 88
      Top = 104
      Width = 49
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BorderStyle = sbsSunken
      Caption = '00'
      TabOrder = 4
    end
  end
end
