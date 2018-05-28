
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/delphi/client/simple/Hello.pas,v 1.1.1.1 2003-12-03 22:37:11 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit Hello;

{.$DEFINE PROXY}
{$DEFINE LOCAL}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, xmlrpctypes, xmlrpcclient;

type
  TForm1 = class(TForm)
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
  RpcResult: IRpcResult;
begin
  { A function and caller object is used to perform this
    miracle. Set the host name and port and the endpoint
    to be called. We then set the function name to be called
    and add a single string parameter. The function object is
    passed to the execute method and a tresult object is returned}

  RpcCaller := TRpcCaller.Create;
  try
{$IFDEF PROXY}
    RpcCaller.ProxyName := 'fli4l.int.europa.de';
    RpcCaller.ProxyPort := 3128;
{$ENDIF}
{$IFDEF LOCAL}
    RpcCaller.EndPoint := '/xml-rpc/server.php';
    RpcCaller.HostName := 'localhost';
    RpcCaller.HostPort := 8080;
{$ELSE}
    RpcCaller.EndPoint := '/xml-rpc/server.php';
    RpcCaller.HostName := 'www.dwp42.org';
    RpcCaller.HostPort := 80;
{$ENDIF}

    RpcFunction := TRpcFunction.Create;
    RpcFunction.ObjectMethod := 'interopEchoTests.echoString';
    RpcFunction.AddItem('Hello, world!');

    RpcResult := RpcCaller.Execute(RpcFunction);
    if RpcResult.IsError then
      ShowMessageFmt('Error: (%d) %s', [RpcResult.ErrorCode,
          RpcResult.ErrorMsg])
    else
      ShowMessage('Success: ' + RpcResult.AsString);
  finally
    RpcCaller.Free;
  end;
end;

end.

