object frmSelectComponent: TfrmSelectComponent
  Left = 321
  Top = 107
  Width = 168
  Height = 270
  BorderIcons = []
  Caption = 'Select Component'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 215
    Width = 160
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 3
      Top = 0
      Width = 157
      Height = 28
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 0
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Ok'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object Button2: TButton
        Left = 80
        Top = 2
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object lbComponents: TCheckListBox
    Left = 0
    Top = 65
    Width = 160
    Height = 150
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 160
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 88
      Height = 13
      Caption = 'Default Language:'
    end
    object chkAllNone: TCheckBox
      Left = 4
      Top = 46
      Width = 97
      Height = 17
      Caption = 'Select All/None'
      TabOrder = 0
      OnClick = chkAllNoneClick
    end
    object cbLanguage: TComboBox
      Left = 4
      Top = 20
      Width = 157
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
  end
end
