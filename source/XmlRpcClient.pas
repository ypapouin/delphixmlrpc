
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{ XmlRpcClient.pas                                      }
{                                                       }
{ for Delphi 6, 7, XE and Lazarus                       }
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
  $Header: /cvsroot-fuse/delphixml-rpc/dxmlrpc/source/XmlRpcClient.pas,v 1.2 2004/04/20 20:35:51 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: XmlRpcClient.pas,v $
  Revision 1.2  2004/04/20 20:35:51  iwache
  - New properties UserName, Password and BasicAuth
  added to TRpcCaller.
  - Bug in procedure TRpcClientParser.Parse fixed,
  CDATA sections for strings added.
  Thanks for both to Henrik Genssen - hinnack

  Revision 1.1.1.1  2003/12/03 22:37:51  iwache
  Initial import of release 2.0.0

  ----------------------------------------------------------------------------
}
unit XmlRpcClient;

interface

{$INCLUDE 'indy.inc'}

uses
  SysUtils, Classes, Contnrs, XmlRpcTypes, XmlRpcCommon, XmlRpcUnicode,
  IdHTTP,
  IdSSLOpenSSL,
{$IFDEF INDY9}
  IdHashMessageDigest,
  IdHash,
{$ENDIF}
  IdComponent,
  {$IFDEF UNICODE}
  LibXmlParserU;
  {$ELSE}
  LibXmlParser;
  {$ENDIF}

type

{$IFDEF INDY10}
    TIndyInteger = Int64;
{$ELSE}
    TIndyInteger = Integer;
{$ENDIF}


  TRpcClientParser = class(TObject)
  private
    FStack: TObjectStack;
    FStructNames: TXmlStringList;
    FRpcResult: IRpcResult;
    FParser: TXMLParser;
    FLastTag: TXmlString;
    FFixEmptyStrings: Boolean;
    procedure PushStructName(const Name: TXmlString);
    function PopStructName: TXmlString ;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Data: TXmlString);
    function GetTag: TXmlString;
    procedure StartTag;
    procedure EndTag;
    procedure DataTag;
    property FixEmptyStrings: Boolean read FFixEmptyStrings
        write FFixEmptyStrings;
  end;

  TRpcCaller = class(TRpcClientParser)
  private
    FHostName: TXmlString;
    FHostPort: Integer;
    FUserName: TXmlString;
    FPassword: TXmlString;
    FBasicAuth: Boolean;
    FProxyName: TXmlString;
    FProxyPort: Integer;
    FProxyUserName: TXmlString;
    FProxyPassword: TXmlString;
    FSSLEnable: Boolean;
    FSSLRootCertFile: TXmlString;
    FSSLCertFile: TXmlString;
    FSSLKeyFile: TXmlString;
    FEndPoint: TXmlString;
    FProxyBasicAuth: Boolean;
    FOnWork: TWorkEvent;
    FOnWorkBegin: TWorkBeginEvent;
    FOnWorkEnd: TWorkEndEvent;
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: TIndyInteger);
    procedure DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: TIndyInteger);
    procedure DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  protected
    FSession: TIdHTTP;
    function Post(const RawData: TXmlString): TXmlString; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property EndPoint: TXmlString read FEndPoint write FEndPoint;
    property HostName: TXmlString read FHostName write FHostName;
    property HostPort: Integer read FHostPort write FHostPort;
    property UserName: TXmlString read FUserName write FUserName;
    property Password: TXmlString read FPassword write FPassword;
    property BasicAuth: Boolean read FBasicAuth write FBasicAuth;
    property ProxyName: TXmlString read FProxyName write FProxyName;
    property ProxyPort: Integer read FProxyPort write FProxyPort;
    property ProxyUserName: TXmlString read FProxyUserName write FProxyUserName;
    property ProxyPassword: TXmlString read FProxyPassword write FProxyPassword;
    property ProxyBasicAuth: Boolean read FProxyBasicAuth write FProxyBasicAuth;
    property SSLEnable: Boolean read FSSLEnable write FSSLEnable;
    property SSLRootCertFile: TXmlString read FSSLRootCertFile write
      FSSLRootCertFile;
    property SSLCertFile: TXmlString read FSSLCertFile write FSSLCertFile;
    property SSLKeyFile: TXmlString read FSSLKeyFile write FSSLKeyFile;
    property OnWork: TWorkEvent read FOnWork write FOnWork;
    property OnWorkBegin: TWorkBeginEvent read FOnWorkBegin write FOnWorkBegin;
    property OnWorkEnd: TWorkEndEvent read FOnWorkEnd write FOnWorkEnd;
{$IFDEF INDY9}
    function Execute(RpcFunction: IRpcFunction; Ttl: Integer): IRpcResult; overload;
{$ENDIF}
    function Execute(const XmlRequest: TXmlString): IRpcResult; overload; virtual;
    function Execute(Value: IRpcFunction): IRpcResult; overload;
    procedure DeleteOldCache(Ttl: Integer);
  end;

const
  ERROR_EMPTY_RESULT = 600;
  ERROR_EMPTY_RESULT_MESSAGE = 'The xml-rpc server returned a empty response';
  ERROR_INVALID_RESPONSE = 601;
  ERROR_INVALID_RESPONSE_MESSAGE =
    'Invalid payload received from xml-rpc server';

implementation

{$IFDEF WIN32}
uses
  Windows;
{$ENDIF}

{------------------------------------------------------------------------------}
{ RPC PARSER CONSTRUCTOR                                                       }
{------------------------------------------------------------------------------}

constructor TRpcClientParser.Create;
begin
  inherited Create;
end;

destructor TRpcClientParser.Destroy;
begin
  //CLINTON - 16/9/2003
  FStructNames.Free;
  FStack.Free;
  FParser.Free;
  inherited Destroy;
end;

//CLINTON 16/9/2003
// push/pop StructName used to store prior struct member name
procedure TRpcClientParser.PushStructName(const Name: TXmlString);
begin
  FStructNames.Add(Name);
end ;

function TRpcClientParser.PopStructName: TXmlString ;
var 
  I: Integer ;
begin
  I := FStructNames.Count - 1;
  Result := fStructNames[I];
  FStructNames.Delete(I);
end ;

{------------------------------------------------------------------------------}
{ RETURN THE RESULT OBJECT  tastes great less filling ;)                       }
{------------------------------------------------------------------------------}

procedure TRpcClientParser.Parse(Data: TXmlString);
begin
  FRpcResult := TRpcResult.Create;

  { empty string fix }
  if (FFixEmptyStrings) then
    Data := FixEmptyString(Data);
  {simple error check}
  if not (Pos('xml', Data) > 0) then
  begin
    FRpcResult.SetError(ERROR_INVALID_RESPONSE, ERROR_INVALID_RESPONSE_MESSAGE);
    Exit;
  end;
  {empty response}
  if (Trim(Data) = '') then
  begin
    FRpcResult.SetError(ERROR_EMPTY_RESULT, ERROR_EMPTY_RESULT_MESSAGE);
    Exit;
  end;

  if not Assigned(FParser) then
    FParser := TXMLParser.Create;
  if not Assigned(FStack) then
    FStack := TObjectStack.Create;
  //CLINTON - 16/9/2003  
  if not Assigned(FStructNames) then  
    FStructNames := TXmlStringList.Create;
    

  FRpcResult.Clear;
  FParser.LoadFromString(Data);
  FParser.StartScan;
  FParser.Normalize := False;
  while FParser.Scan do
  begin
    case FParser.CurPartType of
      ptStartTag:
        StartTag;
      ptContent:
        DataTag;
      ptCData:
        DataTag;
      ptEndTag:
        EndTag;
    end;
  end;
end;

{------------------------------------------------------------------------------}
{ CACHED WEB CALL Time To Live calculated in minutes                           }
{------------------------------------------------------------------------------}

{$IFDEF INDY9}

function TRpcCaller.Execute(RpcFunction: IRpcFunction; Ttl: Integer):
    IRpcResult;
var
  Strings: TXmlStrings;
  XmlResult: TXmlString;
  XmlRequest: TXmlString;
  Hash: TXmlString;
  HashMessageDigest: TIdHashMessageDigest5;
begin
  XmlRequest := RpcFunction.RequestXML;
  HashMessageDigest := TIdHashMessageDigest5.Create;
  try
    { determine the md5 digest hash of the request }
    {$IFDEF INDY10}
    Hash := HashStringMD5AsHex(XmlRequest);
    {$ELSE}
    Hash := Hash128AsHex(HashMessageDigest.HashValue(XmlRequest));
    {$ENDIF}
  finally
    HashMessageDigest.Free;
  end;
  Strings := TXmlStringList.Create;
  try
    { if we have a cached file from a previous request
      that has not expired then load it }
    if FileExists(GetTempDir + Hash + '.csh') then
    begin
      if not FileIsExpired(GetTempDir + Hash + '.csh', Ttl) then
      begin
        Strings.LoadFromFile(GetTempDir + Hash + '.csh');
        Parse(Strings.Text);
      end;
    end
    else
    begin
      { ok we got here so we where expired or did not exist
        make the call and cache the result this time }
      XmlResult := Post(XmlRequest);
      Parse(XmlResult);

      { save XmlResult in to the cache }
      Strings.Text := XmlResult;
      Strings.SaveToFile(GetTempDir + Hash + '.csh');
    end;
  finally
    Strings.Free;
  end;
  RpcFunction.Clear;
end;

{$ENDIF}

{------------------------------------------------------------------------------}
{ NON - CACHED WEB CALL with IFunction parameter                                                     }
{------------------------------------------------------------------------------}

function TRpcCaller.Execute(Value: IRpcFunction): IRpcResult;
begin
  Result := Execute(Value.RequestXML);
  Value.Clear;
end;

{------------------------------------------------------------------------------}
{ NON - CACHED WEB CALL with XML string parameter                                                     }
{------------------------------------------------------------------------------}
function TRpcCaller.Execute(const XmlRequest: TXmlString): IRpcResult;
var
  XmlResponse: TXmlString;
begin
  XmlResponse := Post(XmlRequest);
  Parse(XmlResponse);
  Result := FRpcResult;
end;

{------------------------------------------------------------------------------}
{ DELETE ALL TEMPORARY EXPIRED DATA                                            }
{------------------------------------------------------------------------------}

procedure TRpcCaller.DeleteOldCache(Ttl: Integer);
var
  SearchRec: TSearchRec;
begin
  if FindFirst(GetTempDir + '*.csh', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory = 0) then
        if FileIsExpired(GetTempDir + SearchRec.Name, Ttl) then
          SysUtils.DeleteFile(GetTempDir + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TRpcCaller.DoWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: TIndyInteger);
begin
  if Assigned(OnWork) then
    OnWork(Self, AWorkMode, AWorkCount);
end;

procedure TRpcCaller.DoWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: TIndyInteger);
begin
  if Assigned(OnWorkBegin) then
    OnWorkBegin(Self, AWorkMode, AWorkCountMax);
end;

procedure TRpcCaller.DoWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if Assigned(OnWorkEnd) then
    OnWorkEnd(Self, AWorkMode);
end;

{------------------------------------------------------------------------------}
{ POST THE REQUEST TO THE RPC SERVER                                           }
{------------------------------------------------------------------------------}

function TRpcCaller.Post(const RawData: TXmlString): TXmlString;
var
  SendStream: TStringStream;
  ResponseStream: TStringStream;
  Session: TIdHttp;
  IdSSLIOHandlerSocket: TIdSSLIOHandlerSocketOpenSSL;
begin
  SendStream := nil;
  ResponseStream := nil;
  IdSSLIOHandlerSocket := nil;
  try
    SendStream := TStringStream.Create(RawData);
    SendStream.Position := 0;
    ResponseStream := TStringStream.Create;

    Session := TIdHttp.Create(nil);
    Session.OnWork := DoWork;
    Session.OnWorkBegin := DoWorkBegin;
    Session.OnWorkEnd := DoWorkEnd;

    try
      IdSSLIOHandlerSocket := nil;
      if (FSSLEnable) then
      begin
        IdSSLIOHandlerSocket := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
        IdSSLIOHandlerSocket.SSLOptions.RootCertFile := FSSLRootCertFile;
        IdSSLIOHandlerSocket.SSLOptions.CertFile := FSSLCertFile;
        IdSSLIOHandlerSocket.SSLOptions.KeyFile := FSSLKeyFile;
        Session.IOHandler := IdSSLIOHandlerSocket;
      end;


      { proxy setup }
      if (FProxyName <> '') then
      begin
        {proxy basic auth}
        if (FProxyBasicAuth) then
          Session.ProxyParams.BasicAuthentication := True;

        Session.ProxyParams.ProxyServer := FProxyName;
        Session.ProxyParams.ProxyPort := FProxyPort;
        Session.ProxyParams.ProxyUserName := FProxyUserName;
        Session.ProxyParams.ProxyPassword := FProxyPassword;
      end;

      { auth setup  FIX ADD hg}
      if (FUserName <> '') then
      begin
        Session.Request.BasicAuthentication := True;
        Session.Request.Username := FUserName;
        Session.Request.Password := FPassword;
      end;

      Session.Request.Accept := '*/*';
      Session.Request.ContentType := 'text/xml';
      Session.Request.Connection := 'Keep-Alive';
      //Session.Request.ContentLength := Length(RawData);
      Session.Request.ContentLength := SendStream.Size;

      if FSSLEnable then
      begin
        Session.Post('https://' + FHostName + ':' + IntToStr(FHostPort) +
          FEndPoint, SendStream, ResponseStream);
      end
        else
      begin
        if FHostPort = 80 then
          Session.Post('http://' + FHostName + FEndPoint, SendStream, ResponseStream)
        else
        begin
          Session.Post('http://' + FHostName + ':' + IntToStr(FHostPort) + FEndPoint, SendStream, ResponseStream);
        end;
      end;

      Result := ResponseStream.DataString;
    finally
      Session.Free;
    end;
  finally
    IdSSLIOHandlerSocket.Free;
    ResponseStream.Free;
    SendStream.Free;
  end;

end;


{------------------------------------------------------------------------------}

constructor TRpcCaller.Create;
begin
  inherited Create;
  FHostPort := 80;
  FSSLEnable := False;
  FProxyBasicAuth := False;
end;

destructor TRpcCaller.Destroy;
begin
  if Assigned(FSession) then
  begin
    FSession.Free;
    FSession := nil;
  end;

  inherited;
end;


{------------------------------------------------------------------------------}

procedure TRpcClientParser.DataTag;
var
  Data: TXmlString;

  TT: string;
begin
  Data := FParser.CurContent;
  if FParser.CurEncoding = 'UTF-8' then
  begin
    Data := System.UTF8ToUnicodeString(FParser.CurContent);
  end;

  { avoid to skip empty string values inside a struct }
  if ((FLastTag = 'STRING') and (FStructNames.Count > 0) and (not (Trim(Data) <> ''))) then
    Data := '[NULL]';
    
  { should never be empty }
  if not (Trim(Data) <> '') then
    Exit;
  { last tag empty ignore }
  if (FLastTag = '') then
    Exit;

  { struct name store for next pass}
  if (FLastTag = 'NAME') then
    if not (Trim(Data) <> '') then
      Exit;

  {this will handle the default
   string pain in the ass}
  if FLastTag = 'VALUE' then
    FLastTag := 'STRING';

  {ugly null string hack}
  if (FLastTag = 'STRING') then
    if (Data = '[NULL]') then
      Data := '';

  {if the tag was a struct name we will
   just store it for the next pass    }
  if (FLastTag = 'NAME') then
  begin
    // CLINTON 16/9/2003
    PushStructName(Data);
    Exit;
  end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TRpcStruct) then
    begin
      if (FLastTag = 'STRING') then
        TRpcStruct(FStack.Peek).LoadRawData(dtString, PopStructName, Data)
      else if (FLastTag = 'INT') then
        TRpcStruct(FStack.Peek).LoadRawData(dtInteger, PopStructName, Data)
      else if (FLastTag = 'I4') then
        TRpcStruct(FStack.Peek).LoadRawData(dtInteger, PopStructName, Data)
      else if (FLastTag = 'DOUBLE') then
        TRpcStruct(FStack.Peek).LoadRawData(dtFloat, PopStructName, Data)
      else if (FLastTag = 'DATETIME.ISO8601') then
        TRpcStruct(FStack.Peek).LoadRawData(dtDateTime, PopStructName, Data)
      else if (FLastTag = 'BASE64') then
        TRpcStruct(FStack.Peek).LoadRawData(dtBase64, PopStructName, Data)
      else if (FLastTag = 'BOOLEAN') then
        TRpcStruct(FStack.Peek).LoadRawData(dtBoolean, PopStructName, Data);
    end;

  if (FStack.Count > 0) then
    if (TObject(FStack.Peek) is TRpcArray) then
    begin
      if (FLastTag = 'STRING') then
        TRpcArray(FStack.Peek).LoadRawData(dtString, Data)
      else if (FLastTag = 'INT') then
        TRpcArray(FStack.Peek).LoadRawData(dtInteger, Data)
      else if (FLastTag = 'I4') then
        TRpcArray(FStack.Peek).LoadRawData(dtInteger, Data)
      else if (FLastTag = 'DOUBLE') then
        TRpcArray(FStack.Peek).LoadRawData(dtFloat, Data)
      else if (FLastTag = 'DATETIME.ISO8601') then
        TRpcArray(FStack.Peek).LoadRawData(dtDateTime, Data)
      else if (FLastTag = 'BASE64') then
        TRpcArray(FStack.Peek).LoadRawData(dtBase64, Data)
      else if (FLastTag = 'BOOLEAN') then
        TRpcArray(FStack.Peek).LoadRawData(dtBoolean, Data);
    end;

  {here we are just getting a single value}
  if FStack.Count = 0 then
  begin
    if (FLastTag = 'STRING') then
      FRpcResult.AsRawString := Data
    else if (FLastTag = 'INT') then
      FRpcResult.AsInteger := StrToInt(Data)
    else if (FLastTag = 'I4') then
      FRpcResult.AsInteger := StrToInt(Data)
    else if (FLastTag = 'DOUBLE') then
      FRpcResult.AsFloat := StrToFloat(Data)
    else if (FLastTag = 'DATETIME.ISO8601') then
      FRpcResult.AsDateTime := IsoToDateTime(Data)
    else if (FLastTag = 'BASE64') then
      FRpcResult.AsBase64Raw := Data
    else if (FLastTag = 'BOOLEAN') then
      FRpcResult.AsBoolean := StrToBool(Data);
  end;

  FLastTag := '';
end;

{------------------------------------------------------------------------------}

procedure TRpcClientParser.EndTag;
var
  RpcStruct: TRpcStruct;
  RpcArray: TRpcArray;
  Tag: TXmlString;
begin
  Tag := GetTag;

  {if we get a struct closure then
   we pop it off the stack do a peek on
   the item before it and add  it}
  if (Tag = 'STRUCT') then
  begin
    {last item is a struct}
    if (TObject(FStack.Peek) is TRpcStruct) then
      if (FStack.Count > 0) then
      begin
        RpcStruct := TRpcStruct(FStack.Pop);
        if (FStack.Count > 0) then
        begin
          if (TObject(FStack.Peek) is TRpcArray) then
            TRpcArray(FStack.Peek).AddItem(RpcStruct)
          else if (TObject(FStack.Peek) is TRpcStruct) then
            TRpcStruct(FStack.Peek).AddItem(PopStructName, RpcStruct)
        end
        else
          FRpcResult.AsStruct := RpcStruct;
        Exit;
      end;

    {last item is a array}
    if (TObject(FStack.Peek) is TRpcArray) then
      if (FStack.Count > 0) then
      begin
        RpcArray := TRpcArray(FStack.Pop);
        if (FStack.Count > 0) then
        begin
          if (TObject(FStack.Peek) is TRpcArray) then
            TRpcArray(FStack.Peek).AddItem(RpcArray)
          else if (TObject(FStack.Peek) is TRpcStruct) then
            TRpcStruct(FStack.Peek).AddItem(PopStructName, RpcArray);
        end
        else
          FRpcResult.AsArray := RpcArray;
        Exit;
      end;
  end;

  if (Tag = 'ARRAY') then
  begin
    if (TObject(FStack.Peek) is TRpcArray) then
      if (FStack.Count > 0) then
      begin
        RpcArray := TRpcArray(FStack.Pop);
        if (FStack.Count > 0) then
        begin
          if (TObject(FStack.Peek) is TRpcStruct) then
            TRpcStruct(FStack.Peek).AddItem(PopStructName, RpcArray)
          else if (TObject(FStack.Peek) is TRpcArray) then
            TRpcArray(FStack.Peek).AddItem(RpcArray);
        end
        else
          FRpcResult.AsArray := RpcArray;
        Exit;
      end;
  end;

  {if we get the params closure then we will pull the array
   and or struct and add it to the final result then clean up}
  if (Tag = 'PARAMS') then
    if (FStack.Count > 0) then
    begin
      if (TObject(FStack.Peek) is TRpcStruct) then
        FRpcResult.AsStruct := TRpcStruct(FStack.Pop)
      else if (TObject(FStack.Peek) is TRpcArray) then
        FRpcResult.AsArray := TRpcArray(FStack.Pop);

      //CLINTON 16/9/2003
      {free the stack and the stack of the Struct names}
      FreeAndNil(FStack);
      FreeAndNil(FStructNames);
    end;
end;

function TRpcClientParser.GetTag: TXmlString;
begin
  Result := FParser.CurName;
  if FParser.CurEncoding = 'UTF-8' then
  begin
    Result := System.UTF8ToUnicodeString(FParser.CurName);
  end;
  Result := UpperCase(Trim(Result));
end;

{------------------------------------------------------------------------------}

procedure TRpcClientParser.StartTag;
var
  Tag: TXmlString;
  RpcStruct: TRpcStruct;
  RpcArray: TRpcArray;
begin
  Tag := GetTag;

  if (Tag = 'STRUCT') then
  begin
    RpcStruct := TRpcStruct.Create;
    try
      FStack.Push(RpcStruct);
      RpcStruct := nil;
    finally
      RpcStruct.Free;
    end;
  end;

  if (Tag = 'ARRAY') then
  begin
    RpcArray := TRpcArray.Create;
    try
      FStack.Push(RpcArray);
      RpcArray := nil;
    finally
      RpcArray.Free;
    end;
  end;
  FLastTag := Tag;
end;

end.