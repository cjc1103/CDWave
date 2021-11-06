inherited AboutBoxShare: TAboutBoxShare
  Left = 325
  Top = 210
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    inherited Comments1: TLabel
      Top = 96
      Width = 257
      Caption = 
        'This program is shareware. You can copy and use it during the tr' +
        'ial period for no charge as long as you do not use it for commer' +
        'cial purposes or modify this program in any way.'
    end
    inherited Comments2: TLabel
      Caption = 
        'After the trial period of 31 days has expired, you should regist' +
        'er this product. Instructions are in the help file.'
    end
    inherited Release: TLabel
      Width = 88
      Caption = 'Shareware release'
    end
  end
  inherited OKButton: TButton
    Left = 63
  end
  object btnRegister: TButton
    Left = 160
    Top = 252
    Width = 75
    Height = 25
    Hint = 'Show registration form'
    Caption = '&Register'
    TabOrder = 2
    OnClick = btnRegisterClick
  end
end
