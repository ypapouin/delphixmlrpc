unit XmlRpcAnsiStrings;

interface

uses
  SYstem.SysUtils,
  System.Classes,
  System.Contnrs,
  System.Generics.collections;

const
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiLineFeed       = AnsiChar(#10);
  AnsiCrLf           = AnsiString(#13#10);

  {$IFDEF MSWINDOWS}
  AnsiLineBreak = AnsiCrLf;
  {$ELSE}
  AnsiLineBreak = AnsiLineFeed;
  {$ENDIF }

type
  TAnsiStringList = class
  private
    FStrings: TList<AnsiString>;
    FObjects: TObjectList;

    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
    function GetString(idx: Integer): AnsiString;
    procedure SetString(idx: Integer; const Value: AnsiString);
    function GetObject(idx: Integer): TObject;
    procedure SetObject(idx: Integer; const Value: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; inline;
    function Count: Integer; inline;
    function IndexOf(const Value: AnsiString): Integer;

    procedure Add(const Value: AnsiString); inline;
    procedure AddObject(const Value: AnsiString; AObject: TObject);
    procedure Delete(AIndex: Integer);

    property Text: AnsiString read GetText write SetText;
    property Strings[idx: Integer]: AnsiString read GetString write SetString; default;
    property Objects[idx: Integer]: TObject    read GetObject write SetObject;
  end;


implementation

function CharIsReturn(const C: AnsiChar): Boolean;
begin
  Result := (C = AnsiLineFeed) or (C = AnsiCarriageReturn);
end;


{ TAnsiStringList }

constructor TAnsiStringList.Create;
begin
  FStrings := TList<AnsiString>.Create;
  FObjects := TObjectList.Create(False);
end;

destructor TAnsiStringList.Destroy;
begin
  FStrings.Free;
  FObjects.Free;
  inherited;
end;

procedure TAnsiStringList.Add(const Value: AnsiString);
begin
  FStrings.Add(Value);
  FObjects.Add(nil);
end;

procedure TAnsiStringList.AddObject(const Value: AnsiString; AObject: TObject);
begin
  FStrings.Add(Value);
  FObjects.Add(AObject);
end;

procedure TAnsiStringList.Clear;
begin
  FStrings.Clear;
  FObjects.Clear;
end;

function TAnsiStringList.Count: Integer;
begin
  Result := FStrings.Count;
end;

procedure TAnsiStringList.Delete(AIndex: Integer);
begin
  FStrings.Delete(AIndex);
  FObjects.Delete(AIndex);
end;

function TAnsiStringList.GetObject(idx: Integer): TObject;
begin
  Result := FObjects[idx];
end;

procedure TAnsiStringList.SetObject(idx: Integer; const Value: TObject);
begin
  FObjects[idx] := Value;
end;

function TAnsiStringList.GetString(idx: Integer): AnsiString;
begin
  Result := FStrings[idx];
end;

procedure TAnsiStringList.SetString(idx: Integer; const Value: AnsiString);
begin
  FStrings[idx] := Value;
end;

function TAnsiStringList.GetText: AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    Result := Result + FStrings[I] + AnsiLineBreak;
end;

function TAnsiStringList.IndexOf(const Value: AnsiString): Integer;
begin
  Result := FStrings.IndexOf(Value);
end;

procedure TAnsiStringList.SetText(const Value: AnsiString);
var
  Index, Start, Len: Integer;
  S: AnsiString;
begin
  Clear;
  Len := Length(Value);
  Index := 1;
  while Index <= Len do
  begin
    Start := Index;
    while (Index <= Len) and not CharIsReturn(Value[Index]) do
      Inc(Index);

    S := Copy(Value, Start, Index - Start);
    Add(S);

    if (Index <= Len) and (Value[Index] = AnsiCarriageReturn) then
      Inc(Index);
    if (Index <= Len) and (Value[Index] = AnsiLineFeed) then
      Inc(Index);
  end;
end;


end.