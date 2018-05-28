
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{ Test project                                          }
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/tests/validatorarrayofstruct/Unit1.pas,v 1.1.1.1 2003-12-03 22:37:55 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit Unit1;

{$DEFINE PROXY}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, XmlRpcClient, XmlRpcTypes;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  RpcCaller: TRpcCaller;
  RpcFunction: IRpcFunction;
  RpcArray: IRpcArray;
  RpcStruct1: IRpcStruct;
  RpcStruct2: IRpcStruct;
  RpcResult: IRpcResult;
begin
  RpcCaller := TRpcCaller.Create;
  try
{$IFDEF PROXY}
    RpcCaller.ProxyName := 'fli4l.int.europa.de';
    RpcCaller.ProxyPort := 3128;
{$ENDIF}
    RpcCaller.EndPoint := '/xml-rpc/server.php';
    RpcCaller.HostName := 'www.dwp42.org';
    RpcCaller.HostPort := 80;
    {
      for details about this Web Service take a look at:
      http://www.xmlrpc.com/validator1Docs
    }
    RpcFunction := TRpcFunction.Create;
    RpcFunction.ObjectMethod := 'validator1.arrayOfStructsTest';

    RpcStruct1 := TRpcStruct.Create;
    RpcStruct2 := TRpcStruct.Create;

    RpcStruct1.AddItem('moe', 1);
    RpcStruct1.AddItem('larry', 2);
    RpcStruct1.AddItem('curly', 3);

    RpcStruct2.AddItem('moe', 4);
    RpcStruct2.AddItem('larry', 5);
    RpcStruct2.AddItem('curly', 6);

    RpcArray := TRpcArray.Create;
    RpcArray.AddItem(RpcStruct1);
    RpcArray.AddItem(RpcStruct2);
    RpcArray.AddItem(RpcStruct2);

    RpcFunction.AddItem(RpcArray);

    Memo1.Lines.Add('executing call');
    RpcResult := RpcCaller.Execute(RpcFunction);
    if (RpcResult.IsError) then
    begin
      Memo1.Lines.Add('rpc call failed');
      Memo1.Lines.Add('Error Code:' + IntToStr(RpcResult.ErrorCode));
      Memo1.Lines.Add('Error Message: ' + RpcResult.ErrorMsg);
    end
    else
    begin
      Memo1.Lines.Add('rpc call test passed');
      Memo1.Lines.Add('Value:' + IntToStr(RpcResult.AsInteger));
    end;
  finally
    RpcCaller.Free;
  end;
end;

end.

