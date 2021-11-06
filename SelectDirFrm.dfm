inherited SelectDirDlg: TSelectDirDlg
  Left = 290
  Top = 129
  HelpContext = 1030
  Caption = 'Select Output'
  ClientHeight = 385
  ClientWidth = 372
  Constraints.MinHeight = 412
  Constraints.MinWidth = 380
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited GrpFormat: TGroupBox
    Left = 88
    Top = 184
    Width = 193
    Height = 153
    Caption = 'WAV Format parameters'
    Visible = False
    inherited Label7: TLabel
      Top = 48
    end
    inherited Label8: TLabel
      Left = 8
      Top = 88
    end
    inherited Label9: TLabel
      Left = 88
      Top = 88
    end
    inherited lUnsupported: TLabel
      Left = 112
    end
    inherited cbFrequency: TComboBox
      Top = 64
    end
    inherited cbChannels: TComboBox
      Left = 8
      Top = 104
    end
    inherited cbResolution: TComboBox
      Left = 88
      Top = 104
    end
    inherited btnACM: TButton
      Left = 8
      Top = 16
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 372
    Height = 336
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 372
      Height = 104
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 0
      DesignSize = (
        372
        104)
      object lblLoc: TLabel
        Left = 8
        Top = 8
        Width = 76
        Height = 13
        Caption = 'Output Location'
      end
      object eDirectory: TEdit
        Left = 8
        Top = 24
        Width = 357
        Height = 21
        TabOrder = 0
        OnChange = eDirectoryChange
      end
      object btnBrowse: TBitBtn
        Left = 282
        Top = 48
        Width = 83
        Height = 25
        Anchors = [akLeft]
        Caption = 'Browse'
        TabOrder = 1
        OnClick = btnBrowseClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
          0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
          B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
          FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
          FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
          FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
          0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
          0555555555777777755555555555555555555555555555555555}
        NumGlyphs = 2
      end
      object grpSpace: TGroupBox
        Left = 9
        Top = 48
        Width = 169
        Height = 49
        Caption = 'Space'
        TabOrder = 2
        object Label2: TLabel
          Left = 16
          Top = 16
          Width = 46
          Height = 13
          Alignment = taRightJustify
          Caption = 'Required:'
        end
        object Label3: TLabel
          Left = 16
          Top = 32
          Width = 46
          Height = 13
          Alignment = taRightJustify
          Caption = 'Available:'
        end
        object lRequired: TLabel
          Left = 72
          Top = 16
          Width = 57
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = '0'
          ShowAccelChar = False
        end
        object lAvailable: TLabel
          Left = 72
          Top = 32
          Width = 57
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = '0'
          ShowAccelChar = False
        end
        object Label4: TLabel
          Left = 136
          Top = 16
          Width = 13
          Height = 13
          Caption = 'kB'
          ShowAccelChar = False
        end
        object Label5: TLabel
          Left = 136
          Top = 32
          Width = 13
          Height = 13
          Caption = 'kB'
          ShowAccelChar = False
        end
      end
    end
    object Panel1: TPanel
      Left = 0
      Top = 104
      Width = 372
      Height = 232
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object PanelFormat: TPanel
        Left = 0
        Top = 0
        Width = 372
        Height = 232
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ModePanel: TPanel
          Left = 0
          Top = 0
          Width = 372
          Height = 49
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object Label1: TLabel
            Left = 8
            Top = 8
            Width = 61
            Height = 13
            Caption = 'Output mode'
          end
          object cbMode: TComboBox
            Left = 8
            Top = 24
            Width = 185
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = cbModeChange
            Items.Strings = (
              'Direct WAV'
              'WAV'
              'MP3 (LAME)'
              'OGG Vorbis'
              'FLAC'
              'APE (Monkey'#39's)')
          end
        end
        object grpMP3: TGroupBox
          Left = 0
          Top = 49
          Width = 372
          Height = 152
          Align = alTop
          Caption = 'MP3 parameters'
          TabOrder = 1
          Visible = False
          object lMP3Status: TLabel
            Left = 8
            Top = 16
            Width = 185
            Height = 25
            AutoSize = False
            Caption = 'lMP3Status'
            WordWrap = True
          end
          object lBadMP3Format: TLabel
            Left = 8
            Top = 136
            Width = 110
            Height = 13
            Caption = 'Cannot convert to MP3'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clMaroon
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Visible = False
          end
          object Label11: TLabel
            Left = 24
            Top = 104
            Width = 89
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Bitrate (kbps):'
          end
          object cbMP3Quality: TComboBox
            Left = 24
            Top = 50
            Width = 169
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            OnChange = cbMP3QualityChange
          end
          object rbPreset: TRadioButton
            Left = 8
            Top = 32
            Width = 177
            Height = 17
            Caption = 'Preset'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = rbPresetClick
          end
          object rbVBR: TRadioButton
            Left = 8
            Top = 72
            Width = 185
            Height = 17
            Caption = 'Variable bitrate (VBR)'
            TabOrder = 2
            OnClick = rbPresetClick
          end
          object rbCBR: TRadioButton
            Left = 8
            Top = 88
            Width = 185
            Height = 17
            Caption = 'Constant bitrate (CBR)'
            TabOrder = 3
            OnClick = rbPresetClick
          end
          object cbBitrate: TComboBox
            Left = 120
            Top = 104
            Width = 73
            Height = 21
            Style = csDropDownList
            Enabled = False
            ItemHeight = 13
            TabOrder = 4
            OnChange = cbMP3QualityChange
            Items.Strings = (
              '64'
              '80'
              '96'
              '112'
              '128'
              '160'
              '192'
              '224'
              '256'
              '320')
          end
        end
        object grpOGG: TGroupBox
          Left = 0
          Top = 201
          Width = 372
          Height = 105
          Align = alTop
          Caption = 'OGG Vorbis parameters'
          TabOrder = 2
          Visible = False
          object lOGGStatus: TLabel
            Left = 8
            Top = 16
            Width = 185
            Height = 33
            AutoSize = False
            Caption = 'lOGGStatus'
            WordWrap = True
          end
          object Label6: TLabel
            Left = 8
            Top = 48
            Width = 141
            Height = 13
            Caption = 'Nominal output quality setting:'
          end
          object lBadOGGFormat: TLabel
            Left = 8
            Top = 88
            Width = 144
            Height = 13
            Caption = 'Cannot convert to OGG Vorbis'
            Visible = False
          end
          object cbOGGQuality: TComboBox
            Left = 8
            Top = 64
            Width = 185
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = cbOGGQualityChange
          end
        end
        object GrpNoConversion: TGroupBox
          Left = 0
          Top = 306
          Width = 372
          Height = 79
          Align = alTop
          Caption = 'WAV Parameters'
          TabOrder = 3
          object lblDirectEx: TLabel
            Left = 8
            Top = 16
            Width = 185
            Height = 33
            AutoSize = False
            Caption = 'Save audio in standard WAV format without conversion'
            WordWrap = True
          end
          object cbOld24: TCheckBox
            Left = 8
            Top = 48
            Width = 169
            Height = 17
            Hint = 'Save in a format compatible with some older programs'
            HelpContext = 1031
            Caption = 'Use alternate 24-bit format'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
        end
        object grpFLAC: TGroupBox
          Left = 0
          Top = 465
          Width = 372
          Height = 80
          Align = alTop
          Caption = 'FLAC parameters'
          TabOrder = 4
          Visible = False
          object Label12: TLabel
            Left = 8
            Top = 16
            Width = 157
            Height = 13
            Caption = 'Free Lossless Audio Compression'
          end
          object Label13: TLabel
            Left = 8
            Top = 32
            Width = 88
            Height = 13
            Caption = 'Compression level:'
          end
          object cbFLACLevel: TComboBox
            Left = 8
            Top = 48
            Width = 177
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = cbFLACLevelChange
            Items.Strings = (
              '0 - Fastest'
              '1'
              '2'
              '3'
              '4'
              '5 - Recommended'
              '6'
              '7'
              '8 - Best compression')
          end
        end
        object grpAPE: TGroupBox
          Left = 0
          Top = 385
          Width = 372
          Height = 80
          Align = alTop
          Caption = 'APE parameters'
          TabOrder = 5
          Visible = False
          object Label14: TLabel
            Left = 8
            Top = 16
            Width = 138
            Height = 13
            Caption = 'Monkey'#39's Audio Compression'
          end
          object Label15: TLabel
            Left = 8
            Top = 32
            Width = 88
            Height = 13
            Caption = 'Compression level:'
          end
          object cbAPELevel: TComboBox
            Left = 8
            Top = 48
            Width = 177
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 1
            TabOrder = 0
            Text = 'NORMAL'
            OnChange = cbAPELevelChange
            Items.Strings = (
              'FAST'
              'NORMAL'
              'HIGH'
              'EXTRA HIGH'
              'INSANE')
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 336
    Width = 372
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnCancel: TBitBtn
      Left = 261
      Top = 10
      Width = 75
      Height = 33
      TabOrder = 1
      Kind = bkCancel
    end
    object btnOk: TBitBtn
      Left = 181
      Top = 10
      Width = 75
      Height = 33
      TabOrder = 0
      Kind = bkOK
    end
    object btnHelp: TBitBtn
      Left = 341
      Top = 10
      Width = 27
      Height = 33
      TabOrder = 2
      OnClick = btnHelpClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333336633
        3333333333333FF3333333330000333333364463333333333333388F33333333
        00003333333E66433333333333338F38F3333333000033333333E66333333333
        33338FF8F3333333000033333333333333333333333338833333333300003333
        3333446333333333333333FF3333333300003333333666433333333333333888
        F333333300003333333E66433333333333338F38F333333300003333333E6664
        3333333333338F38F3333333000033333333E6664333333333338F338F333333
        0000333333333E6664333333333338F338F3333300003333344333E666433333
        333F338F338F3333000033336664333E664333333388F338F338F33300003333
        E66644466643333338F38FFF8338F333000033333E6666666663333338F33888
        3338F3330000333333EE666666333333338FF33333383333000033333333EEEE
        E333333333388FFFFF8333330000333333333333333333333333388888333333
        0000}
      NumGlyphs = 2
    end
  end
end
