object frmAutoTranslate: TfrmAutoTranslate
  Left = 274
  Top = 109
  Width = 431
  Height = 183
  BorderIcons = [biSystemMenu]
  Caption = 'Auto Translation'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 114
    Height = 13
    Caption = 'Current Form Language:'
  end
  object Label2: TLabel
    Left = 45
    Top = 40
    Width = 81
    Height = 13
    Alignment = taRightJustify
    Caption = 'Build Groups For:'
  end
  object cbBaseLanguage: TComboBox
    Left = 132
    Top = 12
    Width = 149
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnChange = cbBaseLanguageChange
    Items.Strings = (
      'Chinese'
      'English'
      'French'
      'German'
      'Italian'
      'Japanese'
      'Korean'
      'Portugese'
      'Russian'
      'Spanish')
  end
  object btnTranslate: TButton
    Left = 312
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Translate'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
  end
  object lbTranslateTo: TCheckListBox
    Left = 132
    Top = 36
    Width = 149
    Height = 105
    ItemHeight = 13
    Items.Strings = (
      'Chinese'
      'French'
      'German'
      'Italian'
      'Japanese'
      'Korean'
      'Portugese'
      'Spanish')
    Sorted = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 312
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
  end
  object rgOptions: TRadioGroup
    Left = 288
    Top = 8
    Width = 121
    Height = 58
    Caption = 'Translation Options:'
    ItemIndex = 0
    Items.Strings = (
      'All Properties'
      'Untranslated Only')
    TabOrder = 2
  end
end
