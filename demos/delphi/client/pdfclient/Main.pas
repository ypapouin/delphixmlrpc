
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{ Demo project                                          }
{                                                       }
{ for Delphi 6, 7                                       }
{ Release 2.0.0                                         }
{ Copyright (c) 2001-2003 by Team-DelphiXml-Rpc         }
{ e-mail: team-dxmlrpc@dwp42.org                        }
{ www: http://sourceforge.net/projects/delphixml-rpc/   }
{                                                       }
{ The initial developer of the code is                  }
{   Clifford E. Baeseman, codepunk@codepunk.com         }
{                                                       }
{ This file may be distributed and/or modified under    }
{ the terms of the GNU Lesser General Public License    }
{ (LGPL) version 2.1 as published by the Free Software  }
{ Foundation and appearing in the included file         }
{ license.txt.                                          }
{                                                       }
{*******************************************************}
{
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/delphi/client/pdfclient/Main.pas,v 1.1.1.1 2003-12-03 22:37:10 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XmlRpcClient, XmlRpcTypes;

type
  TfmMain = class(TForm)
    memText: TMemo;
    btnLoad: TButton;
    btnConvert: TButton;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    procedure btnLoadClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btnLoadClick(Sender: TObject);
begin
  if odOpen.Execute then
    memText.Lines.LoadFromFile(odOpen.FileName);
end;

procedure TfmMain.btnConvertClick(Sender: TObject);
var
  RpcCaller: TRpcCaller;
  RpcResult: IRpcResult;
  RpcFunction: IRpcFunction;
begin
  RpcCaller := TRpcCaller.Create;
  try
    RpcCaller.HostName := '192.168.0.4';
    RpcCaller.HostPort := 8012;
    RpcCaller.EndPoint := '/RPC2';
    RpcFunction := TRpcFunction.Create;
    RpcFunction.ObjectMethod := 'conversion.text2pdf';
    RpcFunction.AddItemBase64Str(memText.Text);
    RpcResult := RpcCaller.Execute(RpcFunction);

    {if we get a error display it}
    if RpcResult.IsError then
    begin
      ShowMessageFmt('Error: (%d) %s', [RpcResult.ErrorCode,
          RpcResult.ErrorMsg]);
      Exit;
    end;

    {save the file}
    sdSave.FileName := 'converted.pdf';
    if sdSave.Execute then
      RpcResult.Base64StrSaveToFile(sdSave.FileName);
  finally
    RpcCaller.Free;
  end;
end;

end.

