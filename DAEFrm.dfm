object DAEForm: TDAEForm
  Left = 771
  Top = 75
  Width = 359
  Height = 336
  Caption = 'Digital Audio Extraction'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 351
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 80
      Height = 13
      Caption = 'CDROM Device:'
    end
    object lStatus: TLabel
      Left = 8
      Top = 48
      Width = 32
      Height = 13
      Caption = 'lStatus'
    end
    object cbDrives: TComboBox
      Left = 8
      Top = 24
      Width = 337
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbDrivesChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 65
    Width = 351
    Height = 159
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object lvTracks: TListView
      Left = 0
      Top = 0
      Width = 351
      Height = 159
      Align = alClient
      Columns = <
        item
          Caption = 'Track'
        end
        item
          Alignment = taRightJustify
          Caption = 'LBA'
          Width = 64
        end
        item
          Alignment = taRightJustify
          Caption = 'Start'
          Width = 64
        end
        item
          Alignment = taRightJustify
          Caption = 'Size'
          Width = 64
        end
        item
          Caption = 'Type'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 224
    Width = 351
    Height = 85
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lVersion: TLabel
      Left = 0
      Top = 68
      Width = 37
      Height = 13
      Caption = 'lVersion'
    end
    object Panel4: TPanel
      Left = 166
      Top = 57
      Width = 185
      Height = 28
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TBitBtn
        Left = 22
        Top = 0
        Width = 75
        Height = 25
        TabOrder = 0
        Kind = bkOK
      end
      object BitBtn2: TBitBtn
        Left = 102
        Top = 0
        Width = 75
        Height = 25
        TabOrder = 1
        Kind = bkCancel
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 351
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 69
        Height = 13
        Caption = 'Destination file'
      end
      object eLocation: TEdit
        Left = 0
        Top = 16
        Width = 265
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object btnBrowse: TBitBtn
        Left = 272
        Top = 8
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object cbJitter: TCheckBox
        Left = 0
        Top = 40
        Width = 265
        Height = 17
        Caption = 'Use jitter correction'
        TabOrder = 2
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wave Audio|*.wav'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Select destination file'
    Left = 224
    Top = 230
  end
end
