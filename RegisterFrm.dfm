object RegisterForm: TRegisterForm
  Left = 531
  Top = 198
  HelpContext = 9001
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Register'
  ClientHeight = 169
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 315
    Height = 26
    Caption = 
      'Enter registration information just as you got it from RegNow! i' +
      'n the boxes below (use copy/paste to prevent typing errors).'
    WordWrap = True
  end
  object btnOK: TBitBtn
    Left = 32
    Top = 136
    Width = 75
    Height = 25
    TabOrder = 0
    OnClick = btnOKClick
    Kind = bkOK
  end
  object BitBtn1: TBitBtn
    Left = 144
    Top = 136
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = BitBtn1Click
    Kind = bkHelp
  end
  object BitBtn2: TBitBtn
    Left = 240
    Top = 136
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object Panel1: TPanel
    Left = 8
    Top = 48
    Width = 321
    Height = 73
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 3
    object Label1: TLabel
      Left = 48
      Top = 12
      Width = 54
      Height = 13
      Alignment = taRightJustify
      Caption = '&User name:'
      FocusControl = eRegname
    end
    object Label2: TLabel
      Left = 16
      Top = 40
      Width = 86
      Height = 13
      Alignment = taRightJustify
      Caption = '&Registration code:'
      FocusControl = eRegcode
    end
    object eRegname: TEdit
      Left = 112
      Top = 8
      Width = 121
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object eRegcode: TEdit
      Left = 112
      Top = 36
      Width = 121
      Height = 21
      TabOrder = 1
    end
  end
end
