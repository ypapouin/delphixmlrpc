object Form1: TForm1
  Left = 142
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Hello Server'
  ClientHeight = 296
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start Server'
    TabOrder = 0
    OnClick = Button1Click
  end
  object lstMessages: TListBox
    Left = 8
    Top = 40
    Width = 425
    Height = 249
    ItemHeight = 13
    TabOrder = 1
  end
  object ebPort: TEdit
    Left = 120
    Top = 8
    Width = 65
    Height = 21
    TabOrder = 2
    Text = '8080'
    OnChange = ebPortChange
  end
  object udPort: TUpDown
    Left = 185
    Top = 8
    Width = 15
    Height = 21
    Associate = ebPort
    Max = 32767
    Position = 8080
    TabOrder = 3
  end
end
