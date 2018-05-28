
unit XmlRpcUnicode;


interface

uses
  Windows, Classes, JclAnsiStrings, JclStrings;

type

{$IFDEF UNICODE}
    TAnsiStrings = TJclAnsiStrings;
    TAnsiStringList = TJclAnsiStringList;
{$ELSE}
    TAnsiStringList = TStringList;
{$ENDIF}

implementation

end.

