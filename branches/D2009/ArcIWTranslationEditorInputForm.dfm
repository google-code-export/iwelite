object frmEnterText: TfrmEnterText
  Left = 201
  Top = 109
  Width = 351
  Height = 251
  BorderIcons = [biMaximize]
  Caption = 'frmEnterText'
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
    Top = 188
    Width = 343
    Height = 34
    Align = alBottom
    TabOrder = 0
    object Panel2: TPanel
      Left = 184
      Top = 1
      Width = 158
      Height = 32
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 0
        Top = 4
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object Button2: TButton
        Left = 80
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object mText: TMemo
    Left = 0
    Top = 0
    Width = 343
    Height = 188
    Align = alClient
    TabOrder = 1
  end
end
