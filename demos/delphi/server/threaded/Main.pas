
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/delphi/server/threaded/Main.pas,v 1.1.1.1 2003-12-03 22:37:15 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, SyncObjs, XmlRpcServer, XmlRpcTypes;

type
  TForm1 = class(TForm)
    Button1: TButton;
    lstMessages: TListBox;
    ebPort: TEdit;
    udPort: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ebPortChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRpcServer: TRpcServer;
    FRpcMethodHandler: TRpcMethodHandler;
    FMessage: string;
    FCriticalSection: TCriticalSection;
    procedure AddMessage;
  public
    procedure HelloMethod(Thread: TRpcThread; const MethodName: string;
        List: TList; Return: TRpcReturn);
  end;

var
  Form1: TForm1;

implementation
{$ifdef INDY10}
uses
  idsync;
{$endif}

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCriticalSection := TCriticalSection.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCriticalSection.Free;
  FRpcServer.Free;
end;

procedure TForm1.AddMessage;
begin
  if lstMessages.Items.Count > 100 then
    lstMessages.Clear;
  lstMessages.Items.Add(FMessage);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not Assigned(FRpcServer) then
    FRpcServer := TRpcServer.Create;
  if not FRpcServer.Active then
  begin
    FRpcServer.ListenPort := udPort.Position;
    if not Assigned(FRpcMethodHandler) then
    begin
      FRpcMethodHandler := TRpcMethodHandler.Create;
      try
        FRpcMethodHandler.Name := 'example.getHello';
        FRpcMethodHandler.Method := HelloMethod;
        FRpcServer.RegisterMethodHandler(FRpcMethodHandler);
        FRpcMethodHandler := nil;
      finally
        FRpcMethodHandler.Free;
      end;
    end;
    FRpcServer.Active := True;
    Button1.Caption := 'Stop Server';
    FMessage := 'xml-rpc hello server has been started';
    AddMessage;
  end
  else
  begin
    FRpcServer.Active := False;
    Button1.Caption := 'Start Server';
    FMessage := 'xml-rpc hello server has been stopped';
    AddMessage;
  end;
end;

procedure TForm1.HelloMethod(Thread: TRpcThread; const MethodName: string;
    List: TList; Return: TRpcReturn);
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
  Return.AddItem('You just sent ' + Msg);

  // synchronize the method handler with the VCL main thread
  FCriticalSection.Enter;
  try
    FMessage := IntToStr(GetCurrentThreadId) + ' ' + Msg;
    {$ifdef INDY9}
    Thread.Synchronize(AddMessage);
    {$endif}
    {$ifdef INDY10}
    Tidsync.SynchronizeMethod(AddMessage);
    {$endif}
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TForm1.ebPortChange(Sender: TObject);
var
  Port: Integer;
begin
  Port := StrToIntDef(ebPort.Text, 80);
  if ((Port > 65535) or (Port < 0)) then
    ebPort.Text := '80';
end;

end.

