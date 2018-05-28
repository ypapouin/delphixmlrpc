
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
  $Header: d:\Archive\DeltaCopy\Backup\delphixml-rpc.cvs.sourceforge.net/dxmlrpc/demos/kylix/server/pdfdaemon/rpcserver.pas,v 1.1.1.1 2003-12-03 22:37:17 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: not supported by cvs2svn $
  ----------------------------------------------------------------------------
}
unit RpcServer;

interface

uses
{$IFDEF LINUX}
  Libc,
{$ENDIF}
  Classes, SysUtils, XmlRpcServer, XmlRpcTypes, XmlRpcCommon, IdHash,
  IdHashMessageDigest;

type
  TXmlRpcServer = class(TObject)
  private
    FRpcServer: TRpcServer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConversionText2pdf(Thread: TRpcThread; const MethodName: string;
        List: TList; Return: TRpcReturn);
    function CreateFileHash(Data: TStream): string;
  end;

implementation

constructor TXmlRpcServer.Create;
var
  RpcMethodHandler: TRpcMethodHandler;
begin
  FRpcServer := TRpcServer.Create;
  FRpcServer.ListenPort := 8012;
  RpcMethodHandler := TRpcMethodHandler.Create;
  try
    RpcMethodHandler.Name := 'conversion.text2pdf';
    RpcMethodHandler.Method := ConversionText2pdf;
    RpcMethodHandler.Signature := 'Base64 File (Base64 StringData)';
    RpcMethodHandler.Help := 'This method takes text data as a base64 type' + #10 +
      'and converts it to a adobe pdf document.';
    FRpcServer.RegisterMethodHandler(RpcMethodHandler);
    RpcMethodHandler := nil;
    FRpcServer.Active := True;
  finally
    RpcMethodHandler.Free;
  end;
end;

destructor TXmlRpcServer.Destroy;
begin
  FRpcServer.Free;
  inherited Destroy;
end;

{create a hash value for this stream}

function TXmlRpcServer.CreateFileHash(Data: TStream): string;
var
  md: TIdHashMessageDigest5;
begin
  md := TIdHashMessageDigest5.Create;
  Result := Hash128AsHex(md.HashValue(Data));
  md.Free;
end;

procedure TXmlRpcServer.ConversionText2pdf(Thread: TRpcThread; const
    MethodName: string; List: TList; Return: TRpcReturn);
var
  FileName: string;
  Storage: string;
  Stream: TStream;
begin
  Storage := '/opt/';
  if List.Count <> 1 then
  begin
    Return.SetError(101, 'You sent not enough or to many parameters');
    Exit;
  end;
  if not TRpcParameter(List[0]).IsBase64 then
  begin
    Return.SetError(102, 'The sent parameter was not a base64 type');
    Exit;
  end;

  {create a hash for the request}
  Stream := TMemoryStream.Create;
  try
    TRpcParameter(List[0]).Base64StrSaveToStream(Stream);
    Stream.Position := 0;
    FileName := CreateFileHash(Stream);
  finally
    Stream.Free;
  end;

  {if we have the cached copy send it}
  if FileExists(Storage + FileName + '.pdf') then
  begin
    Return.AddItemBase64StrFromFile(Storage + FileName + '.pdf');
    Exit;
  end;

  {save the request to storage}
  TRpcParameter(List[0]).Base64StrSaveToFile(Storage + FileName + '.txt');

  {lets check to see if the file made it to storage}
  if not FileExists(Storage + FileName + '.txt') then
  begin
    Return.SetError(103, 'Unable to write sent file to storage.');
  end;

{$IFDEF LINUX}
  {convert document to postscript}
  if Libc.system(PChar('a2ps -R --output=' + Storage + FileName + '.ps ' +
    Storage + FileName + '.txt')) = -1 then
  begin
    Return.SetError(104, 'Unable to convert sent file to postscript');
    Exit;
  end;

  {convert file to adobe pdf}
  if Libc.system(PChar('ps2pdf ' + Storage + FileName + '.ps ' + Storage +
    FileName + '.pdf')) = -1 then
  begin
    Return.SetError(105, 'Unable to convert postscript file to pdf');
  end;
{$ENDIF}  

  {if we have a pdf document in storage send it}
  if FileExists(Storage + FileName + '.pdf') then
    Return.AddItemBase64StrFromFile(Storage + FileName + '.pdf')
  else
    Return.SetError(106, 'Unable to find converted pdf file in storage');
end;

end.

