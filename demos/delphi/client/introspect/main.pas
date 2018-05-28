
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/delphi/client/introspect/main.pas,v 1.1.1.1 2003-12-03 22:37:08 iwache Exp $
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
    gbxConnection: TGroupBox;
    gbxMethods: TGroupBox;
    gbxSignature: TGroupBox;
    gbxHelpText: TGroupBox;
    lbHost: TLabel;
    edHost: TEdit;
    lbPort: TLabel;
    edPort: TEdit;
    btnConnect: TButton;
    lbEndPoint: TLabel;
    edEndPoint: TEdit;
    lbProxyHost: TLabel;
    edProxyHost: TEdit;
    Label1: TLabel;
    edProxyPort: TEdit;
    lbMethod: TListBox;
    memSignature: TMemo;
    memHelp: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure lbMethodClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btnConnectClick(Sender: TObject);
var
  RpcCaller: TRpcCaller;
  RpcResult: IRpcResult;
  RpcFunction: IRpcFunction;
  RpcArray: IRpcArray;
  I: Integer;
begin
  lbMethod.Clear;
  {check host name}
  if Trim(edHost.Text) = '' then
  begin
    showmessage('Host name is required');
    Exit;
  end;
  {check port}
  if Trim(edPort.Text) = '' then
  begin
    showmessage('Port is required');
    Exit;
  end;
  {check end point}
  if Trim(edEndPoint.Text) = '' then
  begin
    showmessage('End Point is required');
    Exit;
  end;

  RpcCaller := TRpcCaller.Create;
  try
    RpcCaller.HostName := Trim(edHost.Text);
    RpcCaller.HostPort := StrToInt(Trim(edPort.Text));
    RpcCaller.EndPoint := Trim(edEndPoint.Text);
    if Trim(edProxyHost.Text) <> '' then
    begin
      RpcCaller.ProxyName := Trim(edProxyHost.Text);
      RpcCaller.ProxyPort := StrToInt(Trim(edProxyPort.Text));
    end;

    RpcFunction := TRpcFunction.Create;
    RpcFunction.ObjectMethod := 'system.listMethods';
    RpcResult := RpcCaller.Execute(RpcFunction);
    if RpcResult.IsError then
    begin
      ShowMessageFmt('Error: (%d) %s', [RpcResult.ErrorCode,
          RpcResult.ErrorMsg]);
      Exit;
    end;

    if RpcResult.IsArray then
    begin
      RpcArray := RpcResult.AsArray;
      for I := 0 to RpcArray.Count - 1 do
        lbMethod.Items.Add(RpcArray[I].AsString);
    end;
  finally
    RpcCaller.Free;
  end;
end;

procedure TfmMain.lbMethodClick(Sender: TObject);
var
  RpcCaller: TRpcCaller;
  RpcResult: IRpcResult;
  RpcFunction: IRpcFunction;
  HelpItems: IRpcArray;
  Signatures: IRpcArray;
  I: Integer;
begin
  memSignature.Clear;
  memHelp.Clear;
  if lbMethod.Items.Count > 0 then
  begin
    {check host name}
    if Trim(edHost.Text) = '' then
    begin
      showmessage('Host name is required');
      Exit;
    end;
    {check port}
    if Trim(edPort.Text) = '' then
    begin
      showmessage('Port is required');
      Exit;
    end;
    {check end point}
    if Trim(edEndPoint.Text) = '' then
    begin
      showmessage('End Point is required');
      Exit;
    end;

    RpcCaller := TRpcCaller.Create;
    try
      RpcCaller.HostName := Trim(edHost.Text);
      RpcCaller.HostPort := StrToInt(Trim(edPort.Text));
      RpcCaller.EndPoint := Trim(edEndPoint.Text);
      if Trim(edProxyHost.Text) <> '' then
      begin
        RpcCaller.ProxyName := Trim(edProxyHost.Text);
        RpcCaller.ProxyPort := StrToInt(Trim(edProxyPort.Text));
      end;

      RpcFunction := TRpcFunction.Create;
      RpcFunction.ObjectMethod := 'system.methodSignature';
      RpcFunction.AddItem(lbMethod.Items[lbMethod.ItemIndex]);
      RpcResult := RpcCaller.Execute(RpcFunction);
      if RpcResult.IsError then
      begin
        ShowMessageFmt('Error: (%d) %s', [RpcResult.ErrorCode,
            RpcResult.ErrorMsg]);
        Exit;
      end;

      if RpcResult.IsArray then
      begin
        HelpItems := RpcResult.AsArray;
        Signatures := HelpItems[0].AsArray;
        for I := 0 to Signatures.Count - 1 do
          memSignature.Lines.Add(Signatures[I].AsString);
      end;

      {create a new XML-RPC function and grab help text}
      RpcFunction := TRpcFunction.Create;
      RpcFunction.ObjectMethod := 'system.methodHelp';
      RpcFunction.AddItem(lbMethod.Items[lbMethod.ItemIndex]);
      RpcResult := RpcCaller.Execute(RpcFunction);
      if RpcResult.IsError then
      begin
        ShowMessageFmt('Error: (%d) %s', [RpcResult.ErrorCode,
            RpcResult.ErrorMsg]);
        Exit;
      end;
      if RpcResult.IsString then
        memHelp.Lines.Text := RpcResult.AsString;
    finally
      RpcCaller.Free;
    end;
  end;
end;

end.

