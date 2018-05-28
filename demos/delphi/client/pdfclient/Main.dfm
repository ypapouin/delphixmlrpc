object fmMain: TfmMain
  Left = 180
  Top = 165
  Width = 483
  Height = 326
  Caption = 'fmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object memText: TMemo
    Left = 40
    Top = 40
    Width = 393
    Height = 217
    Lines.Strings = (
      '')
    TabOrder = 0
  end
  object btnLoad: TButton
    Left = 40
    Top = 8
    Width = 81
    Height = 25
    Caption = 'Load From File'
    TabOrder = 1
    OnClick = btnLoadClick
  end
  object btnConvert: TButton
    Left = 192
    Top = 264
    Width = 81
    Height = 33
    Caption = 'Convert'
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object odOpen: TOpenDialog
    Left = 136
    Top = 8
  end
  object sdSave: TSaveDialog
    Left = 304
    Top = 264
  end
end
