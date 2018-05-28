object Form1: TForm1
  Left = 215
  Top = 158
  Width = 305
  Height = 152
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblStateCode: TLabel
    Left = 16
    Top = 86
    Width = 55
    Height = 13
    Caption = '&State code:'
    FocusControl = edtStateCode
  end
  object Memo1: TMemo
    Left = 16
    Top = 8
    Width = 273
    Height = 65
    Lines.Strings = (
      '')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 200
    Top = 80
    Width = 89
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object edtStateCode: TEdit
    Left = 80
    Top = 82
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '42'
  end
end
