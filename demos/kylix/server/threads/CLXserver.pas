
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/kylix/server/threads/CLXserver.pas,v 1.1.1.1 2003-12-03 22:37:19 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit CLXserver;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls,
  xmlrpcserver, xmlrpctypes,    { XML stuff}
  SynchClasses,                 { synch class }
  ThreadAllocator, QComCtrls               { allocator class}
  ;

type
  TForm1 = class(TForm)
    Button1: TButton;
    lstMessages: TListBox;
    rgrpThreadSynch: TRadioGroup;
    sePort: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FServer : TServer;
    FMethodHandler: TMethodHandler;
    FSynchroniser : TSynchroniser;
    Flist: TList;
    FReturn: TReturn;
    FAllocator : TThreadAllocator;
    procedure AddMessage(const Msg: string);
    function AllocateStringList: TObject;
    procedure DoHelloMethod(Sender: TObject);
    procedure HelloMethod(const plist: TList; const return: TReturn);
    procedure HelloMethod2(const plist: TList; const return: TReturn);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

  { bizarrely, this is declared in QForms, but not exposed }


function GetCurrentThreadID: Integer;
{$IFDEF MSWINDOWS}
  external 'kernel32.dll' name 'GetCurrentThreadId';
{$ENDIF}
{$IFDEF LINUX}
  external 'libpthread.so.0' name 'pthread_self';
{$ENDIF}


{$R *.xfm}

procedure TForm1.AddMessage(const Msg: string);
begin
  if lstMessages.Items.Count > 100 then
    lstMessages.Clear;
  lstMessages.Items.Add(IntToStr(GetCurrentThreadId) + ' ' + Msg);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  if Fserver = nil then
    FServer := TServer.Create;
  if not FServer.Active then
  begin
    FServer.ListenPort := sePort.Value;
    if FMethodHandler = nil then
    begin
      FMethodHandler := TMethodHandler.Create;
      FMethodHandler.Name := 'example.getHello';
      FServer.RegisterMethodHandler(FMethodHandler);
    end;

    if rgrpThreadSynch.ItemIndex = 0 then
      FMethodHandler.Method := HelloMethod
    else
    begin
      FMethodHandler.Method := HelloMethod2;
      FSynchroniser.Method := DoHelloMethod;
    end;

    try
      FServer.Active := true;
      Button1.Caption := 'Stop Server';
      AddMessage('xml-rpc hello server has been started');
    except
      on E : Exception do
      AddMessage(StringReplace(E.Message, #13#10, ' ', [rfReplaceAll]));
    end;
  end
  else
  begin
    FServer.Active := false;
    Button1.Caption := 'Start Server';
    AddMessage('xml-rpc hello server has been stopped');

  end;
  sePort.Enabled := not FServer.Active;
  rgrpThreadSynch.Enabled := not FServer.Active;

end;

procedure TForm1.HelloMethod(const plist: TList; const return: TReturn);
var
  szSent: string;
  StringList : TStringList;
begin
  {The parameter list is sent to your method as a TList of parameters
  this must be casted to a parameter to be accessed. If a error occurs
  during the execution of your method the server will fall back to a global
  handler and try to recover in which case the stack error will be sent to
  the client}

  {grab the sent string}
  szSent := TParameter(plist[0]).GetString;
  AddMessage('Sent ' + szSent);

  {return a message showing what was sent}
  return.AddParam('You just sent ' + szSent);

  { test creation and retrieval of per - thread objects }

  StringList := TStringList(FAllocator.ThreadObject);
  StringList.Values['Usage Count'] :=
    IntToStr(StrToIntDef(StringList.Values['Usage Count'], 0) + 1);
  AddMessage('StringList Usage Count ' + StringList.Values['Usage Count']);


end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  FSynchroniser := TSynchroniser.Create;
  FAllocator := TThreadAllocator.Create;
  FAllocator.CreationProc := AllocateStringList;
  sePort.Value := 1025;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin

  FAllocator.Free;
  fServer.Free;
  FMethodHandler.Free;
  FSynchroniser.Free;

end;

procedure TForm1.HelloMethod2(const plist: TList; const return: TReturn);
begin
  FList := plist;
  FReturn := return;
  FSynchroniser.RunMethod(self);
  fLIst := nil;
  FReturn := nil;
end;

procedure TForm1.DoHelloMethod(Sender: TObject);
begin
  HelloMethod(Flist, FReturn);
end;

function TForm1.AllocateStringList: TObject;
begin
  Result := TStringList.Create;
end;

end.
