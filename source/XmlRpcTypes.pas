
{*******************************************************}
{                                                       }
{ XML-RPC Library for Delphi, Kylix and DWPL (DXmlRpc)  }
{ XmlRpcTypes.pas                                       }
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
  $Header: /cvsroot/delphixml-rpc/dxmlrpc/source/XmlRpcTypes.pas,v 1.3 2004/04/20 20:33:48 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: XmlRpcTypes.pas,v $
  Revision 1.3  2004/04/20 20:33:48  iwache
  New procedures StrLoadFromStream, StrSaveToStream,
  StrLoadFromFile and StrSaveToFile to IRpcCustomItem
  and TRpcCustomItem added. Thanks to
  Henrik Genssen - hinnack

  Bug with double MimeEncodeString fixed in
  TRpcCustomArray.AddItemBase64Str.
  Thanks to Nicolas Seyer - nolics

  Bug of additional #13#10 appended lines  fixed in
  TRpcFunction.GetRequestXML, TRpcFunction.GetResponseXML,
  TRpcFunction.GetErrorXML and TRpcFunction.GetBodyXML.
  Thanks to Nicolas again

  Revision 1.2  2004/01/25 18:24:41  iwache
  New methods GetAsVariant and SetAsVariant and
  new property AsVariant added to TRpcCustomItem.

  Method TRpcCustomItem.GetAsString extended to
  convert different data types (Integer, Float, Base64,
  DateTime and Boolean) into Strings.

  Methods TRpcArray.GetAsXML, LoadRawData and
  TRpcStruct.GetAsXML, LoadRawData and
  TRpcFunction.GetBodyXML now use FloatToRpcStr
  resp. RpcStrToFloat for float convertion into
  XML string and vice versa.

  Revision 1.1.1.1  2003/12/03 22:37:46  iwache
  Initial import of release 2.0.0

  ----------------------------------------------------------------------------
}
unit XmlRpcTypes;

interface

uses
  SysUtils, Classes, Contnrs, DIMime, XmlRpcCommon, XmlRpcUnicode;

type
  IRpcArray = interface;
  IRpcStruct = interface;

  TDataType = (dtFloat, dtInteger, dtString, dtBoolean, dtDateTime, dtBase64,
      dtStruct, dtArray, dtError, dtNone, dtName, dtValue);

  IRpcCustomItem = interface(IInterface)
  ['{3441C47B-364D-4BE6-834E-E05C4FCAE9A6}']
    function GetAsRawString: TXmlString;
    procedure SetAsRawString(const Value: TXmlString);
    function GetAsString: TXmlString;
    procedure SetAsString(const Value: TXmlString);
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    function GetAsFloat: Double;
    procedure SetAsFloat(Value: Double);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(Value: Boolean);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetAsBase64Str: TXmlString;
    procedure SetAsBase64Str(const Value: TXmlString);
    function GetAsArray: IRpcArray;
    procedure SetAsArray(Value: IRpcArray);
    function GetAsStruct: IRpcStruct;
    procedure SetAsStruct(Value: IRpcStruct);
    function GetAsBase64Raw: TXmlString;
    procedure SetAsBase64Raw(const Value: TXmlString);
    function GetDataType: TDataType;

    procedure Clear;
    function IsArray: Boolean;
    function IsBase64: Boolean;
    function IsBoolean: Boolean;
    function IsDate: Boolean;
    function IsFloat: Boolean;
    function IsError: Boolean;
    function IsInteger: Boolean;
    function IsString: Boolean;
    function IsStruct: Boolean;
    procedure Base64StrLoadFromStream(Stream: TStream);
    procedure Base64StrSaveToStream(Stream: TStream);
    procedure Base64StrLoadFromFile(const FileName: TXmlString);
    procedure Base64StrSaveToFile(const FileName: TXmlString);
    procedure StrLoadFromStream(Stream: TStream);
    procedure StrSaveToStream(Stream: TStream);
    procedure StrLoadFromFile(const FileName: TXmlString);
    procedure StrSaveToFile(const FileName: TXmlString);
    property AsRawString: TXmlString read GetAsRawString write SetAsRawString;
    property AsString: TXmlString read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBase64Str: TXmlString read GetAsBase64Str write SetAsBase64Str;
    property AsBase64Raw: TXmlString read GetAsBase64Raw write SetAsBase64Raw;
    property AsArray: IRpcArray read GetAsArray write SetAsArray;
    property AsStruct: IRpcStruct read GetAsStruct write SetAsStruct;
    property DataType: TDataType read GetDataType;
  end;

  TRpcCustomItem = class(TInterfacedObject)
  private
    FDataType: TDataType;
    FString: TXmlString;
    FInteger: Integer;
    FFloat: Double;
    FBoolean: Boolean;
    FDateTime: TDateTime;
    FBase64: TXmlString;
    FStruct: IRpcStruct;
    FArray: IRpcArray;
    function GetAsRawString: TXmlString;
    procedure SetAsRawString(const Value: TXmlString);
    function GetAsString: TXmlString;
    procedure SetAsString(const Value: TXmlString);
    function GetAsInteger: Integer;
    procedure SetAsInteger(Value: Integer);
    function GetAsFloat: Double;
    procedure SetAsFloat(Value: Double);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(Value: Boolean);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(Value: TDateTime);
    function GetAsBase64Str: TXmlString;
    procedure SetAsBase64Str(const Value: TXmlString);
    function GetAsArray: IRpcArray;
    procedure SetAsArray(Value: IRpcArray);
    function GetAsStruct: IRpcStruct;
    function GetAsBase64Raw: TXmlString;
    procedure SetAsBase64Raw(const Value: TXmlString);
    function GetDataType: TDataType;
    function GetAsVariant: Variant;
    procedure SetAsVariant(Value: Variant);
  protected
    procedure SetAsStruct(Value: IRpcStruct); virtual;
  public
    procedure Clear;
    function IsArray: Boolean;
    function IsBase64: Boolean;
    function IsBoolean: Boolean;
    function IsDate: Boolean;
    function IsFloat: Boolean;
    function IsError: Boolean;
    function IsInteger: Boolean;
    function IsString: Boolean;
    function IsStruct: Boolean;
    procedure Base64StrLoadFromStream(Stream: TStream); virtual;
    procedure Base64StrSaveToStream(Stream: TStream); virtual;
    procedure Base64StrLoadFromFile(const FileName: TXmlString); virtual;
    procedure Base64StrSaveToFile(const FileName: TXmlString); virtual;
    procedure StrLoadFromStream(Stream: TStream); virtual;
    procedure StrSaveToStream(Stream: TStream); virtual;
    procedure StrLoadFromFile(const FileName: TXmlString); virtual;
    procedure StrSaveToFile(const FileName: TXmlString); virtual;
    property AsRawString: TXmlString read GetAsRawString write SetAsRawString;
    property AsString: TXmlString read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBase64Str: TXmlString read GetAsBase64Str write SetAsBase64Str;
    property AsBase64Raw: TXmlString read GetAsBase64Raw write SetAsBase64Raw;
    property AsArray: IRpcArray read GetAsArray write SetAsArray;
    property AsStruct: IRpcStruct read GetAsStruct write SetAsStruct;
    property DataType: TDataType read GetDataType;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  TRpcArrayItem = class(TRpcCustomItem)
  end;

  TRpcFunctionItem = class(TRpcCustomItem)
  end;

  TRpcStructItem = class(TRpcCustomItem)
  private
    FName: TXmlString;
  public
    property Name: TXmlString read FName write FName;
  end;

  IRpcResult = interface(IRpcCustomItem)
  ['{ACD2CA2C-65D1-4656-8FF1-F265237E090F}']
    function GetErrorCode: Integer;
    function GetErrorMsg: TXmlString;

    procedure SetError(Code: Integer; const Msg: TXmlString);
    function IsError: Boolean;
    property ErrorCode: Integer read GetErrorCode;
    property ErrorMsg: TXmlString read GetErrorMsg;
  end;

  TRpcResult = class(TRpcCustomItem, IRpcResult)
  private
    FErrorCode: Integer;
    FErrorMsg: TXmlString;
    function GetErrorCode: Integer;
    function GetErrorMsg: TXmlString;
  protected
    procedure SetAsStruct(Value: IRpcStruct); override;
  public
    procedure SetError(Code: Integer; const Msg: TXmlString);
    function IsError: Boolean;
    property ErrorCode: Integer read GetErrorCode;
    property ErrorMsg: TXmlString read GetErrorMsg;
  end;

  IRpcCustomArray = interface(IInterface)
  ['{8177A796-7C3B-4C01-901C-88A13DA61F85}']
    function GetItems(Index: Integer): TRpcArrayItem;
    procedure SetItems(Index: Integer; AItem: TRpcArrayItem);

    procedure AddItem(const Value: TXmlString); overload;
    procedure AddItem(Value: Integer); overload;
    procedure AddItem(Value: Boolean); overload;
    procedure AddItem(Value: Double); overload;
    procedure AddItem(Value: IRpcStruct); overload;
    procedure AddItem(Value: IRpcArray); overload;
    procedure AddItemBase64Raw(const Value: TXmlString);
    procedure AddItemBase64Str(const Value: TXmlString);
    procedure AddItemBase64StrFromFile(const FileName: TXmlString);
    procedure AddItemBase64StrFromStream(Stream: TStream);
    procedure AddItemDateTime(Value: TDateTime);
    procedure Clear;
    function Count: Integer;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TRpcArrayItem read GetItems write SetItems;
        default;
  end;

  TRpcCustomArray = class(TInterfacedObject)
  private
    FList: TObjectList;
    function InternalAddItem: TRpcArrayItem;
  protected
    function GetItems(Index: Integer): TRpcArrayItem;
    procedure SetItems(Index: Integer; AItem: TRpcArrayItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(const Value: TXmlString); overload;
    procedure AddItem(Value: Integer); overload;
    procedure AddItem(Value: Boolean); overload;
    procedure AddItem(Value: Double); overload;
    procedure AddItem(Value: IRpcStruct); overload;
    procedure AddItem(Value: IRpcArray); overload;
    procedure AddItemBase64Raw(const Value: TXmlString);
    procedure AddItemBase64Str(const Value: TXmlString);
    procedure AddItemBase64StrFromFile(const FileName: TXmlString);
    procedure AddItemBase64StrFromStream(Stream: TStream);
    procedure AddItemDateTime(Value: TDateTime);
    procedure Clear; virtual;
    function Count: Integer;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TRpcArrayItem read GetItems write SetItems;
        default;
  end;

  IRpcArray = interface(IRpcCustomArray)
  ['{595D98EE-1718-44ED-94E2-0F8F7A85C247}']
    function GetAsXML: TXmlString;
    procedure LoadRawData(DataType: TDataType; Value: TXmlString);
  end;

  TRpcArray = class(TRpcCustomArray, IRpcArray)
  public
    function GetAsXML: TXmlString;
    procedure LoadRawData(DataType: TDataType; Value: TXmlString);
  end;

  IRpcStruct = interface(IInterface)
  ['{7527E27A-6B61-41D6-9546-93DC816D8285}']
    function InternalAddItem(const Key: TXmlString): TRpcStructItem;
    function GetKeyList: TXmlStringList;

    function GetItems(Index: Integer): TRpcStructItem;
    procedure SetItems(Index: Integer; AItem: TRpcStructItem);
    function GetKeys(Key: TXmlString): TRpcStructItem;
    procedure SetKeys(Key: TXmlString; const AItem: TRpcStructItem);

    procedure AddItem(const Key: TXmlString; Value: Integer); overload;
    procedure AddItem(const Key: TXmlString; const Value: TXmlString); overload;
    procedure AddItem(const Key: TXmlString; Value: Double); overload;
    procedure AddItem(const Key: TXmlString; Value: Boolean); overload;
    procedure AddItem(const Key: TXmlString; Value: IRpcArray); overload;
    procedure AddItem(const Key: TXmlString; Value: IRpcStruct); overload;
    procedure AddItemDateTime(const Key: TXmlString; Value: TDateTime);
    procedure AddItemBase64Str(const Key: TXmlString; const Value: TXmlString);
    procedure AddItemBase64Raw(const Key: TXmlString; const Value: TXmlString);
    procedure AddItemBase64StrFromFile(const Key: TXmlString; const FileName: TXmlString);
    procedure AddItemBase64StrFromStream(const Key: TXmlString; Stream: TStream);
    function KeyExists(const Key: TXmlString): Boolean;
    procedure Delete(Index: Integer); overload;
    procedure Delete(const Key: TXmlString); overload;
    procedure Clear;
    function Count: Integer;
    function IndexOf(const Key: TXmlString): Integer;
    function GetAsXML: TXmlString;
    procedure LoadRawData(DataType: TDataType; const Key, Value: TXmlString);
    property KeyList: TXmlStringList read GetKeyList;
    property Items[Index: Integer]: TRpcStructItem read GetItems write SetItems;
    property Keys[Key: TXmlString]: TRpcStructItem read GetKeys write SetKeys; default;
  end;

  TRpcStruct = class(TInterfacedObject, IRpcStruct)
  private
    FKeyList: TXmlStringList;
    function InternalAddItem(const Key: TXmlString): TRpcStructItem;
    function GetKeyList: TXmlStringList;
  protected
    function GetItems(Index: Integer): TRpcStructItem;
    procedure SetItems(Index: Integer; AItem: TRpcStructItem);
    function GetKeys(Key: TXmlString): TRpcStructItem;
    procedure SetKeys(Key: TXmlString; const AItem: TRpcStructItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(const Key: TXmlString; Value: Integer); overload;
    procedure AddItem(const Key: TXmlString; const Value: TXmlString); overload;
    procedure AddItem(const Key: TXmlString; Value: Double); overload;
    procedure AddItem(const Key: TXmlString; Value: Boolean); overload;
    procedure AddItem(const Key: TXmlString; Value: IRpcArray); overload;
    procedure AddItem(const Key: TXmlString; Value: IRpcStruct); overload;
    procedure AddItemDateTime(const Key: TXmlString; Value: TDateTime);
    procedure AddItemBase64Str(const Key: TXmlString; const Value: TXmlString);
    procedure AddItemBase64Raw(const Key: TXmlString; const Value: TXmlString);
    procedure AddItemBase64StrFromFile(const Key: TXmlString; const FileName: TXmlString);
    procedure AddItemBase64StrFromStream(const Key: TXmlString; Stream: TStream);
    function KeyExists(const Key: TXmlString): Boolean;
    procedure Delete(Index: Integer); overload;
    procedure Delete(const Key: TXmlString); overload;
    procedure Clear;
    function Count: Integer;
    function IndexOf(const Key: TXmlString): Integer;
    function GetAsXML: TXmlString;
    procedure LoadRawData(DataType: TDataType; const Key, Value: TXmlString);
    property KeyList: TXmlStringList read GetKeyList;
    property Items[Index: Integer]: TRpcStructItem read GetItems write SetItems;
    property Keys[Key: TXmlString]: TRpcStructItem read GetKeys write SetKeys; default;
  end;

  IRpcFunction = interface(IRpcCustomArray)
  ['{8177A796-7C3B-4C01-901C-88A13DA61F85}']
    function GetRequestXML: TXmlString;
    function GetResponseXML: TXmlString;
    function GetErrorXML: TXmlString;
    function GetObjectMethod: TXmlString;
    procedure SetObjectMethod(const Value: TXmlString);

    procedure Clear;
    procedure SetError(Code: Integer; const Msg: TXmlString);
    property ObjectMethod: TXmlString read GetObjectMethod write SetObjectMethod;
    property RequestXML: TXmlString read GetRequestXML;
    property ResponseXML: TXmlString read GetResponseXML;
    property ErrorXML: TXmlString read GetErrorXML;
  end;

  TRpcFunction = class(TRpcCustomArray, IRpcFunction)
  private
    FObjectMethod: TXmlString;
    FErrorCode: Integer;
    FErrorMsg: TXmlString;
    function GetRequestXML: TXmlString;
    function GetResponseXML: TXmlString;
    function GetErrorXML: TXmlString;
    procedure GetBodyXML(Strings: TXmlStrings);
    function GetObjectMethod: TXmlString;
    procedure SetObjectMethod(const Value: TXmlString);
  public
    procedure Clear; override;
    procedure SetError(Code: Integer; const Msg: TXmlString);
    property ObjectMethod: TXmlString read GetObjectMethod write SetObjectMethod;
    property RequestXML: TXmlString read GetRequestXML;
    property ResponseXML: TXmlString read GetResponseXML;
    property ErrorXML: TXmlString read GetErrorXML;
  end;

  EXmlRpcError = class(Exception)
  end;

  TRpcParameter = TRpcResult;
  TRpcReturn = TRpcFunction;

implementation

uses Variants;

{
******************************** TCustomItem ***********************************
}

procedure TRpcCustomItem.Clear;
begin
  FDataType := dtNone;
  FString := '';
  FBase64 := '';
  FStruct := nil;
  FArray := nil;
end;

function TRpcCustomItem.GetAsRawString: TXmlString;
begin
  if (FDataType = dtString) then
    Result := FString
  else
    raise EXmlRpcError.Create('Item is not a TXmlString type')
end;

procedure TRpcCustomItem.SetAsRawString(const Value: TXmlString);
begin
  Clear;
  FDataType := dtString;
  FString := Value;
end;

function TRpcCustomItem.GetAsString: TXmlString;
begin
  case FDataType of
    dtString:
      Result := DecodeEntities(AsRawString);
    dtInteger:
      Result := IntToStr(AsInteger);
    dtFloat:
      Result := FloatToStr(AsFloat);
    dtBase64:
      Result := AsBase64Str;
    dtDateTime:
      Result := DateTimeToStr(AsDateTime);
    dtBoolean:
      Result := BoolToStr(AsBoolean, True);
    dtStruct:
      Result := AsStruct.GetAsXML;
    dtArray:
      Result := AsArray.GetAsXML;
  else
    raise EXmlRpcError.Create('Item type can not be converted into a AnsiString')
  end;
end;

procedure TRpcCustomItem.SetAsString(const Value: TXmlString);
begin
  AsRawString := EncodeEntities(Value);
end;

function TRpcCustomItem.GetAsInteger: Integer;
begin
  if (FDataType = dtInteger) then
    Result := FInteger
  else
    raise EXmlRpcError.Create('Item is not a integer type')
end;

procedure TRpcCustomItem.SetAsInteger(Value: Integer);
begin
  Clear;
  FDataType := dtInteger;
  FInteger := Value;
end;

function TRpcCustomItem.GetAsFloat: Double;
begin
  if (FDataType = dtFloat) then
    Result := FFloat
  else
    raise EXmlRpcError.Create('Item is not a double type')
end;

procedure TRpcCustomItem.SetAsFloat(Value: Double);
begin
  Clear;
  FDataType := dtFloat;
  FFloat := Value;
end;

function TRpcCustomItem.GetAsBoolean: Boolean;
begin
  if (FDataType = dtBoolean) then
    Result := FBoolean
  else
    raise EXmlRpcError.Create('Item is not a boolean type')
end;

procedure TRpcCustomItem.SetAsBoolean(Value: Boolean);
begin
  Clear;
  FDataType := dtBoolean;
  FBoolean := Value;
end;

function TRpcCustomItem.GetAsDateTime: TDateTime;
begin
  if (FDataType = dtDateTime) then
    Result := FDateTime
  else
    raise EXmlRpcError.Create('Item is not a date type')
end;

procedure TRpcCustomItem.SetAsDateTime(Value: TDateTime);
begin
  Clear;
  FDataType := dtDateTime;
  FDateTime := Value;
end;

function TRpcCustomItem.GetAsBase64Str: TXmlString;
begin
  if (FDataType = dtBase64) then
    Result := MimeDecodeString(FBase64)
  else
    raise
      EXmlRpcError.Create('Item is not a base64 type')
end;

procedure TRpcCustomItem.SetAsBase64Str(const Value: TXmlString);
begin
  Clear;
  FDataType := dtBase64;
  FBase64 := MimeEncodeString{NoCRLF}(Value);
end;

function TRpcCustomItem.GetAsVariant: Variant;
begin
  case FDataType of
    dtString:
      Result := DecodeEntities(AsRawString);
    dtInteger:
      Result := AsInteger;
    dtFloat:
      Result := AsFloat;
    dtBase64:
      Result := AsBase64Str;
    dtDateTime:
      Result := AsDateTime;
    dtBoolean:
      Result := AsBoolean;
//    dtStruct:
//      Result := '<STRUCT>';
//    dtArray:
//      Result := '<ARRAY>';
  else
    raise EXmlRpcError.Create('Item type can not be converted into a variant')
  end;
end;

procedure TRpcCustomItem.SetAsVariant(Value: Variant);
begin
  if VarIsStr(Value) then
    AsString := Value
  else if VarIsType(Value, varBoolean) then
    AsBoolean := Value
  else if VarIsOrdinal(Value) then
    AsInteger := Value
  else if VarIsFloat(Value) then
    AsFloat := Value
  else if VarIsType(Value, varDate) then
    AsDateTime := Value
  else
    raise EXmlRpcError.Create(
        'Variant type can not be converted into a XML-RPC item');
end;

function TRpcCustomItem.GetAsArray: IRpcArray;
begin
  if (FDataType = dtArray) then
    Result := FArray
  else
    raise EXmlRpcError.Create('Item is not a array type')
end;

procedure TRpcCustomItem.SetAsArray(Value: IRpcArray);
begin
  Clear;
  FDataType := dtArray;
  FArray := Value;
end;

function TRpcCustomItem.GetAsStruct: IRpcStruct;
begin
  if (FDataType = dtStruct) then
    Result := FStruct
  else
    raise EXmlRpcError.Create('Item is not a struct type')
end;

procedure TRpcCustomItem.SetAsStruct(Value: IRpcStruct);
begin
  Clear;
  FDataType := dtStruct;
  FStruct := Value;
end;

function TRpcCustomItem.GetAsBase64Raw: TXmlString;
begin
  if (FDataType = dtBase64) then
    Result := FBase64
  else
    raise EXmlRpcError.Create('Item is not a base64 type')
end;

procedure TRpcCustomItem.SetAsBase64Raw(const Value: TXmlString);
begin
  Clear;
  FDataType := dtBase64;
  FBase64 := Value;
end;

function TRpcCustomItem.IsArray: Boolean;
begin
  Result := (FDataType = dtArray);
end;

function TRpcCustomItem.IsBase64: Boolean;
begin
  Result := (FDataType = dtBase64);
end;

function TRpcCustomItem.IsBoolean: Boolean;
begin
  Result := (FDataType = dtBoolean);
end;

function TRpcCustomItem.IsDate: Boolean;
begin
  Result := (FDataType = dtDateTime);
end;

function TRpcCustomItem.IsFloat: Boolean;
begin
  Result := (FDataType = dtFloat);
end;

function TRpcCustomItem.IsError: Boolean;
begin
  Result := (FDataType = dtError);
end;

function TRpcCustomItem.IsInteger: Boolean;
begin
  Result := (FDataType = dtInteger);
end;

function TRpcCustomItem.IsString: Boolean;
begin
  Result := (FDataType = dtString);
end;

function TRpcCustomItem.IsStruct: Boolean;
begin
  Result := (FDataType = dtStruct);
end;

procedure TRpcCustomItem.Base64StrSaveToStream(Stream: TStream);
begin
  StringToStream(AsBase64Str, Stream);
end;

procedure TRpcCustomItem.Base64StrSaveToFile(const FileName: TXmlString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Base64StrSaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TRpcCustomItem.Base64StrLoadFromStream(Stream: TStream);
begin
  AsBase64Str := StreamToString(Stream);
end;

procedure TRpcCustomItem.Base64StrLoadFromFile(const FileName: TXmlString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Base64StrLoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TRpcCustomItem.StrLoadFromStream(Stream: TStream);
begin
  AsString := StreamToString(Stream);
end;

procedure TRpcCustomItem.StrSaveToStream(Stream: TStream);
begin
  StringToStream(AsString, Stream);
end;

procedure TRpcCustomItem.StrLoadFromFile(const FileName: TXmlString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    StrLoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TRpcCustomItem.StrSaveToFile(const FileName: TXmlString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    StrSaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TRpcCustomItem.GetDataType: TDataType;
begin
  Result := FDataType;
end;

{
******************************** TResult ***************************************
}

function TRpcResult.GetErrorCode: Integer;
begin
  if (FDataType = dtError) then
    Result := FErrorCode
  else
    raise EXmlRpcError.Create('Item is not an error type')
end;

function TRpcResult.GetErrorMsg: TXmlString;
begin
  if (FDataType = dtError) then
    Result := FErrorMsg
  else
    raise EXmlRpcError.Create('Item is not an error type');
end;

procedure TRpcResult.SetError(Code: Integer; const Msg: TXmlString);
begin
  Clear;
  FDataType := dtError;
  FErrorCode := Code;
  FErrorMsg := Msg;
end;

function TRpcResult.IsError: Boolean;
begin
  Result := (FDataType = dtError);
end;

procedure TRpcResult.SetAsStruct(Value: IRpcStruct);
var
  faultCode: Integer;
  faultString: String;
begin
  if Value.KeyExists('faultCode') then
  begin
    if Value.Keys['faultCode'].IsInteger then
      faultCode := Value['faultCode'].AsInteger
    else
      faultCode := -1;

    if Value.Keys['faultCode'].IsString then
      faultString := Value['faultCode'].AsString
    else
      faultString := '';

    if Value.KeyExists('faultString') then
    begin
      if Value.Keys['faultString'].IsString then
        faultString := faultString + #10#13 + Value['faultString'].AsString;
    end;

    SetError(faultCode, faultString);
  end
  else
    inherited SetAsStruct(Value);
end;

{
******************************** TCustomArray **********************************
}

constructor TRpcCustomArray.Create;
begin
  FList := TObjectList.Create(True);
end;

destructor TRpcCustomArray.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TRpcCustomArray.InternalAddItem: TRpcArrayItem;
var
  ArrayItem: TRpcArrayItem;
begin
  ArrayItem := TRpcArrayItem.Create;
  try
    FList.Add(ArrayItem);
    Result := ArrayItem;
    ArrayItem := nil;
  finally
    ArrayItem.Free;
  end;
end;

procedure TRpcCustomArray.AddItem(const Value: TXmlString);
begin
  InternalAddItem.AsString := Value;
end;

procedure TRpcCustomArray.AddItem(Value: Integer);
begin
  InternalAddItem.AsInteger := Value;
end;

procedure TRpcCustomArray.AddItem(Value: Boolean);
begin
  InternalAddItem.AsBoolean := Value;
end;

procedure TRpcCustomArray.AddItem(Value: Double);
begin
  InternalAddItem.AsFloat := Value;
end;

procedure TRpcCustomArray.AddItem(Value: IRpcStruct);
begin
  Assert(Assigned(Value));
  InternalAddItem.AsStruct := Value;
end;

procedure TRpcCustomArray.AddItem(Value: IRpcArray);
begin
  Assert(Assigned(Value));
  InternalAddItem.AsArray := Value;
end;

procedure TRpcCustomArray.AddItemBase64Raw(const Value: TXmlString);
begin
  InternalAddItem.AsBase64Raw := Value;
end;

procedure TRpcCustomArray.AddItemBase64Str(const Value: TXmlString);
begin
  // Bug with double MimeEncodeString fixed
  // Thanks to Nicolas Seyer - nolics
  InternalAddItem.AsBase64Str := Value;
end;

procedure TRpcCustomArray.AddItemBase64StrFromFile(const FileName: TXmlString);
begin
  InternalAddItem.Base64StrLoadFromFile(FileName);
end;

procedure TRpcCustomArray.AddItemBase64StrFromStream(Stream: TStream);
begin
  InternalAddItem.Base64StrLoadFromStream(Stream);
end;

procedure TRpcCustomArray.AddItemDateTime(Value: TDateTime);
begin
  InternalAddItem.AsDateTime := Value;
end;

procedure TRpcCustomArray.Clear;
begin
  FList.Clear;
  FList.Pack;
end;

function TRpcCustomArray.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TRpcCustomArray.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

function TRpcCustomArray.GetItems(Index: Integer): TRpcArrayItem;
begin
  Result := TRpcArrayItem(FList[Index]);
end;

procedure TRpcCustomArray.SetItems(Index: Integer; AItem: TRpcArrayItem);
begin
  FList[Index] := AItem;
end;


{
******************************** TArray ****************************************
}

function TRpcArray.GetAsXML: TXmlString;
var
  Index: Integer;
  Strings: TXmlStrings;
begin
  Strings := TXmlStringList.Create;
  try
    Strings.Add('<value>');
    Strings.Add('  <array>');
    Strings.Add('    <data>');
    for Index := 0 to Count - 1 do
    begin
      case Items[Index].DataType of
        dtString: Strings.Add('      <value><string>' +
            Items[Index].AsRawString +
            '</string></value>');
        dtInteger: Strings.Add('      <value><int>' +
            IntToStr(Items[Index].AsInteger) +
            '</int></value>');
        dtFloat: Strings.Add('      <value><double>' +
            FloatToRpcStr(Items[Index].AsFloat) +
            '</double></value>');
        dtBase64: Strings.Add('      <value><base64>' +
            Items[Index].AsBase64Raw +
            '</base64></value>');
        dtDateTime: Strings.Add('      <value><dateTime.iso8601>' +
            DateTimeToISO(Items[Index].AsDateTime) +
            '</dateTime.iso8601></value>');
        dtBoolean:
          if Items[Index].AsBoolean then
            Strings.Add('      <value><boolean>1</boolean></value>')
          else
            Strings.Add('      <value><boolean>0</boolean></value>');
        dtStruct: Strings.Add(Items[Index].AsStruct.GetAsXML);
        dtArray: Strings.Add(Items[Index].AsArray.GetAsXML);
      end;
    end;
    Strings.Add('  </data>');
    Strings.Add('</array>');
    Strings.Add('</value>');
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

procedure TRpcArray.LoadRawData(DataType: TDataType; Value: TXmlString);
begin
  case DataType of
    dtString:
      AddItem(Value);
    dtInteger:
      AddItem(StrToInt(Value));
    dtFloat:
      AddItem(RpcStrToFloat(Value));
    dtBoolean:
      AddItem(StrToBool(Value));
    dtDateTime:
      AddItemDateTime(IsoToDateTime(Value));
    dtBase64:
      AddItemBase64Raw(Value);
  end;
end;


{
******************************** TStruct ***************************************
}

constructor TRpcStruct.Create;
begin
  FKeyList := TXmlStringList.Create;
end;

destructor TRpcStruct.Destroy;
var
  Index : Integer;
begin
  for Index := FKeyList.Count - 1 downto 0 do
    FKeyList.Objects[Index].Free;
  FKeyList.Clear;

  FKeyList.Free;
  inherited Destroy;
end;

function TRpcStruct.GetKeyList: TXmlStringList;
begin
  Result := FKeyList;
end;

function TRpcStruct.GetItems(Index: Integer): TRpcStructItem;
begin
  Result := TRpcStructItem(FKeyList.Objects[Index]);
end;

procedure TRpcStruct.SetItems(Index: Integer; AItem: TRpcStructItem);
begin
  FKeyList.Objects[Index] := AItem;
end;

function TRpcStruct.IndexOf(const Key: TXmlString): Integer;
begin
  Result := FKeyList.IndexOf(Key);
  if Result < 0 then
    raise EXmlRpcError.CreateFmt('Key [%s] not found', [Key]);
end;

function TRpcStruct.GetKeys(Key: TXmlString): TRpcStructItem;
begin
  Result := TRpcStructItem(FKeyList.Objects[IndexOf(Key)]);
end;

procedure TRpcStruct.SetKeys(Key: TXmlString; const AItem: TRpcStructItem);
begin
  FKeyList.Objects[IndexOf(Key)] := AItem;
end;

function TRpcStruct.Count: Integer;
begin
  Result := FKeyList.Count;
end;

function TRpcStruct.KeyExists(const Key: TXmlString): Boolean;
begin
  Result := (FKeyList.IndexOf(Key) >= 0);
end;

procedure TRpcStruct.Delete(Index: Integer);
begin
  Items[Index].Free;
  FKeyList.Delete(Index);
end;

procedure TRpcStruct.Delete(const Key: TXmlString);
begin
  Delete(IndexOf(Key));
end;

procedure TRpcStruct.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FKeyList.Clear;
end;

function TRpcStruct.InternalAddItem(const Key: TXmlString): TRpcStructItem;
var
  StructItem: TRpcStructItem;
begin
  StructItem := TRpcStructItem.Create;
  try
    FKeyList.AddObject(Key, StructItem);
    Result := StructItem;
    StructItem := nil;
  finally
    StructItem.Free;
  end;
end;

procedure TRpcStruct.AddItem(const Key, Value: TXmlString);
begin
  InternalAddItem(Key).AsString := Value;
end;

procedure TRpcStruct.AddItem(const Key: TXmlString; Value: Double);
begin
  InternalAddItem(Key).AsFloat := Value;
end;

procedure TRpcStruct.AddItem(const Key: TXmlString; Value: Integer);
begin
  InternalAddItem(Key).AsInteger := Value;
end;

procedure TRpcStruct.AddItem(const Key: TXmlString; Value: IRpcArray);
begin
  Assert(Assigned(Value));
  InternalAddItem(Key).AsArray := Value;
end;

procedure TRpcStruct.AddItem(const Key: TXmlString; Value: Boolean);
begin
  InternalAddItem(Key).AsBoolean := Value;
end;

procedure TRpcStruct.AddItemDateTime(const Key: TXmlString; Value: TDateTime);
begin
  InternalAddItem(Key).AsDateTime := Value;
end;

procedure TRpcStruct.AddItem(const Key: TXmlString; Value: IRpcStruct);
begin
  Assert(Assigned(Value));
  InternalAddItem(Key).AsStruct := Value;
end;

procedure TRpcStruct.AddItemBase64Str(const Key, Value: TXmlString);
begin
  InternalAddItem(Key).AsBase64Str := Value;
end;

procedure TRpcStruct.AddItemBase64Raw(const Key, Value: TXmlString);
begin
  InternalAddItem(Key).AsBase64Raw := Value;
end;

procedure TRpcStruct.AddItemBase64StrFromFile(const Key: TXmlString; const FileName:
    TXmlString);
begin
  InternalAddItem(Key).Base64StrLoadFromFile(FileName);
end;

procedure TRpcStruct.AddItemBase64StrFromStream(const Key: TXmlString; Stream:
    TStream);
begin
  InternalAddItem(Key).Base64StrLoadFromStream(Stream);
end;

function TRpcStruct.GetAsXML: TXmlString;
var
  I: Integer;
  Strings: TXmlStrings;
begin
  Strings := TXmlStringList.Create;
  try
    Strings.Add('<value>');
    Strings.Add('  <struct>');
    for I := 0 to Count - 1 do
    begin
      Strings.Add('    <member>');
      case Items[I].DataType of
        dtString:
          begin
            Strings.Add('      <name>' + KeyList[I] + '</name>');
            Strings.Add('      <value><string>' +  Items[I].AsRawString
                + '</string></value>');
          end;
        dtInteger:
          begin
            Strings.Add('      <name>' + KeyList[I] + '</name>');
            Strings.Add('      <value><int>' + IntToStr(Items[I].AsInteger) +
                '</int></value>');
          end;
        dtFloat:
          begin
            Strings.Add('      <name>' + KeyList[I] + '</name>');
            Strings.Add('      <value><double>'
                + FloatToRpcStr(Items[I].AsFloat) + '</double></value>');
          end;
        dtBase64:
          begin
            Strings.Add('      <name>' + KeyList[I] + '</name>');
            Strings.Add('      <value><base64>' + Items[I].AsBase64Raw +
                '</base64></value>');
          end;
        dtDateTime:
          begin
            Strings.Add('      <name>' + KeyList[I] + '</name>');
            Strings.Add('      <value><dateTime.iso8601>' +
              DateTimeToISO(Items[I].AsDateTime) +
              '</dateTime.iso8601></value>');
          end;
        dtBoolean:
          begin
            Strings.Add('      <name>' +
              KeyList[I] +
              '</name>');
            if Items[I].AsBoolean then
              Strings.Add('      <value><boolean>1</boolean></value>')
            else
              Strings.Add('      <value><boolean>0</boolean></value>');
          end;
        dtStruct:
          begin
            Strings.Add('      <name>' + KeyList[I] + '</name>');
            Strings.Add(Items[I].AsStruct.GetAsXML);
          end;
        dtArray:
          begin
            Strings.Add('      <name>' + KeyList[I] + '</name>');
            Strings.Add(Items[I].AsArray.GetAsXML);
          end;
      end;
      Strings.Add('    </member>');
    end;
    Strings.Add('  </struct>');
    Strings.Add('</value>');
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

procedure TRpcStruct.LoadRawData(DataType: TDataType; const Key, Value: TXmlString);
begin
  case DataType of
    dtString:
      AddItem(Key, Value);
    dtInteger:
      AddItem(Key, StrToInt(Value));
    dtBoolean:
      AddItem(Key, StrToBool(Value));
    dtFloat:
      AddItem(Key, RpcStrToFloat(Value));
    dtDateTime:
      AddItemDateTime(Key, IsoToDateTime(Value));
    dtBase64:
      AddItemBase64Raw(Key, Value);
  end;
end;

{
******************************** TFunction *************************************
}

function TRpcFunction.GetObjectMethod: TXmlString;
begin
  Result := FObjectMethod;
end;

procedure TRpcFunction.SetObjectMethod(const Value: TXmlString);
begin
  FObjectMethod := Value;
end;

procedure TRpcFunction.Clear;
begin
  FErrorCode := 0;
  FObjectMethod := '';
  inherited Clear;
end;

procedure TRpcFunction.SetError(Code: Integer; const Msg: TXmlString);
begin
  Clear;
  FErrorCode := Code;
  FErrorMsg := Msg;
end;

function TRpcFunction.GetRequestXML: TXmlString;
var
  Strings: TXmlStrings;
begin
  Strings := TXmlStringList.Create;
  try
    Strings.Add('<?xml version="1.0"?>');
    Strings.Add('<methodCall>');
    Strings.Add('   <methodName>' + FObjectMethod + '</methodName>');
    GetBodyXML(Strings);
    Strings.Add('</methodCall>');
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

function TRpcFunction.GetResponseXML: TXmlString;
var
  Strings: TXmlStrings;
begin
  {if we have a error condition return the error instead}
  if FErrorCode > 0 then
  begin
    Result := GetErrorXML;
    Exit;
  end;

  Strings := TXmlStringList.Create;
  try
    Strings.Add('<?xml version="1.0"?>');
    Strings.Add('<methodResponse>');
    GetBodyXML(Strings);
    Strings.Add('</methodResponse>');
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

function TRpcFunction.GetErrorXML: TXmlString;
var
  Strings: TXmlStrings;
begin
  Strings := TXmlStringList.Create;
  try
    Strings.Add('<?xml version="1.0"?>');
    Strings.Add('<methodResponse>');
    Strings.Add('   <fault>');
    Strings.Add('      <value>');
    Strings.Add('        <struct>');
    Strings.Add('            <member>');
    Strings.Add('               <name>faultCode</name>');
    Strings.Add('               <value><int>' + IntToStr(FErrorCode)
        + '</int></value>');
    Strings.Add('               </member>');
    Strings.Add('            <member>');
    Strings.Add('               <name>faultString</name>');
    Strings.Add('               <value><string>' + FErrorMsg
        + '</string></value>');
    Strings.Add('               </member>');
    Strings.Add('            </struct>');
    Strings.Add('         </value>');
    Strings.Add('      </fault>');
    Strings.Add('   </methodResponse>');
    Result := Strings.Text;
  finally
    Strings.Free;
  end;
end;

procedure TRpcFunction.GetBodyXML(Strings: TXmlStrings);
var
  I: Integer;
begin
  Strings.Add('   <params>');
  for I := 0 to Count - 1 do
  begin
    Strings.Add('   <param>');
    case Items[I].DataType of
      dtInteger:
        Strings.Add('<value><int>' +
          IntToStr(Items[I].AsInteger) +
          '</int></value>');
      dtString:
        Strings.Add('<value><string>' +
          Items[I].AsRawString +
          '</string></value>');
      dtFloat:
        Strings.Add('<value><double>' +
          FloatToRpcStr(Items[I].AsFloat) +
          '</double></value>');
      dtBoolean:
        if Items[I].AsBoolean then
          Strings.Add('<value><boolean>1</boolean></value>')
        else
          Strings.Add('<value><boolean>0</boolean></value>');
      dtDateTime:
        Strings.Add('<value><dateTime.iso8601>' +
          DateTimeToISO(Items[I].AsDateTime) +
          '</dateTime.iso8601></value>');
      dtArray:
        Strings.Add(Items[I].AsArray.GetAsXML);
      dtStruct:
        Strings.Add(Items[I].AsStruct.GetAsXML);
      dtBase64:
        Strings.Add('<value><base64>' + Items[I].AsBase64Raw
            + '</base64></value>');
    end;
    Strings.Add('   </param>');
  end;
  Strings.Add('   </params>');
end;

end.

