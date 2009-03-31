object frmSelectLang: TfrmSelectLang
  Left = 198
  Top = 109
  Width = 193
  Height = 211
  BorderIcons = [biSystemMenu]
  Caption = 'Select Language'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object lbLangs: TListBox
    Left = 0
    Top = 0
    Width = 185
    Height = 182
    Align = alClient
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnDblClick = lbLangsDblClick
  end
end
