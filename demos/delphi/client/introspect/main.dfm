object fmMain: TfmMain
  Left = 113
  Top = 112
  BorderStyle = bsDialog
  Caption = 'XML-RPC Server Explorer'
  ClientHeight = 376
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object gbxConnection: TGroupBox
    Left = 8
    Top = 0
    Width = 521
    Height = 121
    Caption = 'Connection'
    TabOrder = 0
    object lbHost: TLabel
      Left = 16
      Top = 24
      Width = 53
      Height = 13
      Caption = 'Host Name'
    end
    object lbPort: TLabel
      Left = 216
      Top = 24
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object lbEndPoint: TLabel
      Left = 16
      Top = 56
      Width = 46
      Height = 13
      Caption = 'End Point'
    end
    object lbProxyHost: TLabel
      Left = 16
      Top = 96
      Width = 51
      Height = 13
      Caption = 'Proxy Host'
    end
    object Label1: TLabel
      Left = 216
      Top = 96
      Width = 48
      Height = 13
      Caption = 'Proxy Port'
    end
    object edHost: TEdit
      Left = 80
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'www.dwp42.org'
    end
    object edPort: TEdit
      Left = 280
      Top = 16
      Width = 57
      Height = 21
      TabOrder = 1
      Text = '80'
    end
    object btnConnect: TButton
      Left = 432
      Top = 16
      Width = 65
      Height = 25
      Caption = 'Connect'
      TabOrder = 2
      OnClick = btnConnectClick
    end
    object edEndPoint: TEdit
      Left = 80
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 3
      Text = '/xml-rpc/server.php'
    end
    object edProxyHost: TEdit
      Left = 80
      Top = 88
      Width = 121
      Height = 21
      TabOrder = 4
      Text = 'fli4l.int.europa.de'
    end
    object edProxyPort: TEdit
      Left = 280
      Top = 88
      Width = 81
      Height = 21
      TabOrder = 5
      Text = '3128'
    end
  end
  object gbxMethods: TGroupBox
    Left = 8
    Top = 128
    Width = 225
    Height = 241
    Caption = 'Method List'
    TabOrder = 1
    object lbMethod: TListBox
      Left = 8
      Top = 16
      Width = 209
      Height = 217
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbMethodClick
    end
  end
  object gbxSignature: TGroupBox
    Left = 240
    Top = 128
    Width = 289
    Height = 65
    Caption = 'Method Signature'
    TabOrder = 2
    object memSignature: TMemo
      Left = 8
      Top = 16
      Width = 273
      Height = 41
      Lines.Strings = (
        '')
      TabOrder = 0
    end
  end
  object gbxHelpText: TGroupBox
    Left = 240
    Top = 200
    Width = 289
    Height = 169
    Caption = 'Help Text'
    TabOrder = 3
    object memHelp: TMemo
      Left = 8
      Top = 16
      Width = 273
      Height = 145
      Lines.Strings = (
        '')
      TabOrder = 0
    end
  end
end
