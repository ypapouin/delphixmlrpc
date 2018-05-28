
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/delphi/server/simple/Main.pas,v 1.1.1.1 2003-12-03 22:37:13 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, XmlRpcServer, XmlRpcTypes;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FRpcServer: TRpcServer;
  public
    { Public declarations }
    { exposed delphi method }
    procedure HelloMethod(hread: TRpcThread; const MethodName: string;
      List: TList; Return: TRpcReturn);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  RpcMethodHandler: TRpcMethodHandler;
begin
  if not Assigned(FRpcServer) then
  begin
    FRpcServer := TRpcServer.Create;
    FRpcServer.ListenPort := 8080;
    FRpcServer.EnableIntrospect := True;
    RpcMethodHandler := TRpcMethodHandler.Create;
    try
      RpcMethodHandler.Name := 'interopEchoTests.echoString';
      RpcMethodHandler.Method := HelloMethod;
      RpcMethodHandler.Signature := 'string (string myval)';
      RpcMethodHandler.Help := 'Just a simple hello world example method';
      FRpcServer.RegisterMethodHandler(RpcMethodHandler);
      RpcMethodHandler := nil;
      FRpcServer.Active := True;
      ShowMessage('xml-rpc hello server has been started');
      Button1.Enabled := False;
    finally
      RpcMethodHandler.Free;
    end;
  end;
end;

procedure TForm1.HelloMethod(hread: TRpcThread; const MethodName: string; List: 
    TList; Return: TRpcReturn);
var
  Msg: string;
begin
  {The parameter list is sent to your method as a TList of parameters
   this must be casted to a parameter to be accessed. If a error occurs
   during the execution of your method the server will fall back to a global
   handler and try to recover in which case the stack error will be sent to
   the client}

   {grab the sent string}
  Msg := TRpcParameter(List[0]).AsString;

  {return a message showing what was sent}
  Return.AddItem('You just sent: ' + Msg);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FRpcServer.Free;
end;

end.

