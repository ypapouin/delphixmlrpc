{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dxmlrpc;

interface

uses
  DIMime, LibXmlParser, XmlRpcClient, XmlRpcCommon, XmlRpcServer, XmlRpcTypes, 
  XmlRpcWebServer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('dxmlrpc', @Register);
end.
