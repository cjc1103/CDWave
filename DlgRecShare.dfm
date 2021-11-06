inherited DlgRecordShare: TDlgRecordShare
  Left = 328
  Top = 421
  ClientHeight = 432
  ClientWidth = 488
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  inherited BtnStart: TBitBtn
    Width = 81
    TabOrder = 4
  end
  inherited BtnCancel: TBitBtn
    Width = 81
    TabOrder = 5
  end
  inherited BtnHelp: TBitBtn
    Width = 81
    TabOrder = 6
  end
  object GrpStop: TGroupBox [5]
    Left = 8
    Top = 296
    Width = 385
    Height = 73
    Caption = 'Stop Criteria'
    TabOrder = 3
    object Label1: TLabel
      Left = 192
      Top = 26
      Width = 37
      Height = 13
      Caption = '&Minutes'
      FocusControl = eStopTime
    end
    object Label2: TLabel
      Left = 192
      Top = 50
      Width = 78
      Height = 13
      Caption = 'S&econds silence'
      FocusControl = eStopSilence
    end
    object cbStopTime: TCheckBox
      Left = 8
      Top = 24
      Width = 121
      Height = 17
      Hint = 'Check this if you want to limit the maximum recording time'
      Caption = 'Stop recording after:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object cbStopSilence: TCheckBox
      Left = 8
      Top = 48
      Width = 121
      Height = 17
      Hint = 
        'Check this if you want to end recording after the input goes sil' +
        'ent'
      Caption = 'Stop recording after:'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object udStopTime: TUpDown
      Left = 169
      Top = 22
      Width = 15
      Height = 21
      Associate = eStopTime
      Min = 1
      Max = 405
      Position = 30
      TabOrder = 2
      Wrap = False
    end
    object eStopTime: TEdit
      Left = 136
      Top = 22
      Width = 33
      Height = 21
      Hint = 'Record timeout'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '30'
      OnExit = eStopTimeExit
    end
    object eStopSilence: TEdit
      Left = 136
      Top = 46
      Width = 33
      Height = 21
      Hint = 'Time to wait silently before recording ends'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Text = '15'
    end
    object udStopSilence: TUpDown
      Left = 169
      Top = 46
      Width = 15
      Height = 21
      Associate = eStopSilence
      Min = 1
      Max = 300
      Position = 15
      TabOrder = 5
      Wrap = False
    end
  end
  object GrpOptions: TGroupBox [6]
    Left = 8
    Top = 224
    Width = 385
    Height = 65
    Caption = 'Options'
    TabOrder = 2
    object Label3: TLabel
      Left = 176
      Top = 16
      Width = 79
      Height = 13
      Alignment = taRightJustify
      Caption = '&Silence is below:'
      FocusControl = eSilence
    end
    object Label4: TLabel
      Left = 184
      Top = 40
      Width = 70
      Height = 13
      Alignment = taRightJustify
      Caption = '&Loud is above:'
      FocusControl = eLoud
    end
    object Label5: TLabel
      Left = 360
      Top = 16
      Width = 8
      Height = 13
      Caption = '%'
    end
    object Label6: TLabel
      Left = 360
      Top = 40
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lSilence: TLabel
      Left = 328
      Top = 16
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = '99.99'
    end
    object lLoud: TLabel
      Left = 328
      Top = 40
      Width = 27
      Height = 13
      Alignment = taRightJustify
      Caption = '99.99'
    end
    object cbVoiceActivated: TCheckBox
      Left = 8
      Top = 16
      Width = 161
      Height = 17
      Hint = 'Starts recording as soon as input raises above silence'
      Caption = '"&Voice activated" start'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object eSilence: TEdit
      Left = 256
      Top = 12
      Width = 49
      Height = 21
      Hint = 'Silence level'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '512'
      OnChange = eSilenceChange
    end
    object eLoud: TEdit
      Left = 256
      Top = 36
      Width = 49
      Height = 21
      Hint = 'Clip warning level'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Text = '24.576'
      OnChange = eLoudChange
    end
    object udSilence: TUpDown
      Left = 305
      Top = 12
      Width = 15
      Height = 21
      Associate = eSilence
      Min = 1
      Max = 32767
      Increment = 32
      Position = 512
      TabOrder = 2
      Wrap = False
    end
    object udLoud: TUpDown
      Left = 305
      Top = 36
      Width = 15
      Height = 21
      Associate = eLoud
      Min = 1
      Max = 32767
      Increment = 128
      Position = 24576
      TabOrder = 4
      Wrap = False
    end
  end
  object GroupBox1: TGroupBox [7]
    Left = 8
    Top = 376
    Width = 385
    Height = 49
    Caption = 'Schedule recording'
    TabOrder = 7
    object dtScheduleTime: TDateTimePicker
      Left = 208
      Top = 20
      Width = 81
      Height = 21
      CalAlignment = dtaRight
      Date = 37636.812019456
      Time = 37636.812019456
      Checked = False
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkTime
      ParseInput = False
      TabOrder = 0
    end
    object cbSchedule: TCheckBox
      Left = 8
      Top = 22
      Width = 193
      Height = 17
      Caption = 'Start recording at the following time:'
      TabOrder = 1
    end
  end
end
