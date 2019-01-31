unit XmlRpcWebServer;

{$INCLUDE 'indy.inc'}

interface
uses
  Classes,//TStringList, TStrings
  IdCustomHTTPServer,
  {$IFDEF INDY10}
  IdContext,
  {$ENDIF}
  XmlRpcServer;//TRpcServer
  
type
  TOnWebCommand = procedure(ASender: TObject; ACommand: string;
    AParams: TStrings; var AResult: boolean) of object;

type
  TRpcWebServer = class(TRpcServer)
  private
    FCommands:     TStringList;
    FRootFolder:   string;
    FOnWebCommand: TOnWebCommand;
  protected
    procedure SetRootFolder(AValue: string);
    function GetRootFolder: string;
    procedure DataPostedUnknownMethod(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    function ExecuteWebCommand(ACommand : String; AParams : TStrings) : Boolean;      
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function AddCommand(ACommand: string): boolean;
    function RemoveCommand(ACommand: string): boolean;
    function RemoveAllCommands: boolean;
    procedure AddMimeType(const Ext, MIMEType: string);
  published
    property RootFolder: string Read GetRootFolder Write SetRootFolder;
    property OnWebCommand: TOnWebCommand Read FOnWebCommand Write FOnWebCommand;
  end;

implementation

uses
  SysUtils;

{ TRpcWebServer }

procedure TRpcWebServer.SetRootFolder(AValue : String);
begin
  FRootFolder := IncludeTrailingPathDelimiter(AValue);
end;

function TRpcWebServer.GetRootFolder : String;
begin
  Result := FRootFolder;
end;

procedure TRpcWebServer.DataPostedUnknownMethod(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Document : String;
begin
  try
  Document := ARequestInfo.Document;
  if (Document = '/') then
    begin
      if FileExists(FRootFolder + 'index.html') then Document := 'index.html' else
      if FileExists(FRootFolder + 'index.htm') then Document := 'index.htm' else
      Document := '';
    end;
  if FCommands.IndexOf(Document) <> -1 then
    begin
      Document := '';
      if ExecuteWebCommand(Document,ARequestInfo.Params) then
        begin
          AResponseInfo.ResponseNo :=  204;
          AResponseInfo.ContentText := 'OK';
        end else
        begin
          AResponseInfo.ResponseNo :=  400;
          AResponseInfo.ContentText := 'FAILED';
        end;
    end;
  Document := StringReplace(Document,'/','\',[rfReplaceAll]);
  Document := FRootFolder + Document;
  Document := StringReplace(Document,'\\','\',[rfReplaceAll]);
  if FileExists(Document) then
    begin
      AResponseInfo.ContentType := Server.MIMETable.GetFileMIMEType(Document);
      AResponseInfo.ContentStream := TFileStream.Create(Document,fmOpenRead + fmShareDenyWrite);
    end else
    begin
      AResponseInfo.ResponseNo :=  404;
      AResponseInfo.ContentText := 'File not found: ' + ARequestInfo.Document;
    end;
  finally
  end;
end;

constructor TRpcWebServer.Create;
begin
  inherited Create;
  FCommands := TStringList.Create;
  AddMimeType('.exe','application/octet-stream');
end;

destructor TRpcWebServer.Destroy;
begin
  FreeAndNil(FCommands);
  inherited Destroy;
end;

function TRpcWebServer.AddCommand(ACommand: string): boolean;
begin
  Result := False;
  if FCommands.IndexOf('/' + ACommand) = -1 then
    begin
      FCommands.Add('/' + ACommand);
      Result := True;
    end;
end;

function TRpcWebServer.RemoveCommand(ACommand: string): boolean;
var
  CommandIdx : Integer;
begin
  Result := False;
  CommandIdx := FCommands.IndexOf('/' + ACommand);
  if CommandIdx <> -1 then
    begin
      FCommands.Delete(CommandIdx);
      Result := True;
    end;
end;

function TRpcWebServer.RemoveAllCommands: boolean;
begin
  FCommands.Clear;
  Result := True;
end;

function TRpcWebServer.ExecuteWebCommand(ACommand : String; AParams : TStrings) : Boolean;
begin
  Result := False;
  if Assigned(FOnWebCommand) then
    begin
      Result := True;
      FOnWebCommand(Self,ACommand,AParams,Result);
    end;
end;

procedure TRpcWebServer.AddMimeType(const Ext, MIMEType: string);
begin
  Server.MIMETable.AddMimeType(Ext,MIMEType);
end;

end.
