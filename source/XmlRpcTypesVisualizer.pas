unit XmlRpcTypesVisualizer;

interface

procedure Register;

implementation

uses
  Classes,
  Vcl.Forms,
  SysUtils,
{$IFDEF DEBUG}
  Winapi.Windows,
{$ENDIF}
  ToolsAPI;

resourcestring
  sRpcVisualizerName = 'XML-RPC Visualizer for Delphi';
  sRpcVisualizerDescription =
    'Displays IRpcArray, TRpcArray, IRpcStruc, TRpcStruct, instances in a human-readable format rather than a pointer';

type
  TDebuggerDXmlRpcVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizerValueReplacer, IOTAThreadNotifier,
    IOTAThreadNotifier160)
  private
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
  public
    { IOTADebuggerVisualizer }
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean);
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    { IOTADebuggerVisualizerValueReplacer }
    function GetReplacementValue(const Expression, TypeName,
      EvalResult: string): string;
    { IOTAThreadNotifier }
    procedure EvaluateComplete(const ExprStr: string; const ResultStr: string;
      CanModify: Boolean; ResultAddress: Cardinal; ResultSize: Cardinal;
      ReturnCode: Integer); overload;
    procedure ModifyComplete(const ExprStr: string; const ResultStr: string;
      ReturnCode: Integer);
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr: string; const ResultStr: string;
      CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
      ReturnCode: Integer); overload;
  end;

  TTypeType = (ttIRpcArray, ttTRpcArray, ttIRpcStruct, ttTRpcStruct,
    ttIRpcCustomItem, ttTRpcCustomItem);

  TRpcArrayVisualizerType = record
    TypeName: string;
    TypeType: TTypeType;
  end;

const
  RpcVisualizerTypes: array [0 .. 5] of TRpcArrayVisualizerType = ( //
    (TypeName: 'IRpcArray'; TypeType: ttIRpcArray), //
    (TypeName: 'TRpcArray'; TypeType: ttTRpcArray),   //
    (TypeName: 'IRpcStruct'; TypeType: ttIRpcStruct), //
    (TypeName: 'TRpcStruct'; TypeType: ttTRpcStruct), //
    (TypeName: 'IRpcCustomItem'; TypeType: ttIRpcCustomItem), //
    (TypeName: 'TRpcCustomItem'; TypeType: ttTRpcCustomItem) //
    );

  { TDebuggerDXmlRpcVisualizer }

procedure TDebuggerDXmlRpcVisualizer.AfterSave;
begin
  // don't care about this notification
end;

procedure TDebuggerDXmlRpcVisualizer.BeforeSave;
begin
  // don't care about this notification
end;

procedure TDebuggerDXmlRpcVisualizer.Destroyed;
begin
  // don't care about this notification
end;

procedure TDebuggerDXmlRpcVisualizer.Modified;
begin
  // don't care about this notification
end;

procedure TDebuggerDXmlRpcVisualizer.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin
  // don't care about this notification
end;

procedure TDebuggerDXmlRpcVisualizer.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: Cardinal;
  ReturnCode: Integer);
begin
{$IFDEF DEBUG}
  OutputDebugString('EvaluateComplete');
{$ENDIF}
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress),
    LongWord(ResultSize), ReturnCode);
end;

procedure TDebuggerDXmlRpcVisualizer.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress;
  ResultSize: LongWord; ReturnCode: Integer);
begin
  FCompleted := True;
  if ReturnCode = 0 then
    FDeferredResult := ResultStr;
{$IFDEF DEBUG}
  OutputDebugString(PChar(' [+] EvaluateComplete:' + IntToStr(ReturnCode)));
{$ENDIF}
end;

procedure TDebuggerDXmlRpcVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin
  // don't care about this notification
end;

function TDebuggerDXmlRpcVisualizer.GetReplacementValue(const Expression,
  TypeName, EvalResult: string): string;
var
  EvalExp: string;
  TypeType: TTypeType;
  I: Integer;
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array [0 .. 255] of Char;
  CanModify: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;

  function FormatResult(const LEvalResult: string; DTType: TTypeType;
    out ResStr: string): Boolean;
  begin
{$IFDEF DEBUG}
    OutputDebugString(PChar('GetReplacementValue:FormatResult-LEvalResult: ' +
      LEvalResult));
{$ENDIF}
    Result := True;
    try
      case DTType of
        ttIRpcArray:
          ResStr := LEvalResult;
        ttTRpcArray:
          ResStr := LEvalResult;
        ttIRpcStruct:
          ResStr := LEvalResult;
        ttTRpcStruct:
          ResStr := LEvalResult;
        ttIRpcCustomItem:
          ResStr := LEvalResult;
        ttTRpcCustomItem:
          ResStr := LEvalResult;
      end;
    except
      Result := False;
    end;

    ResStr := StringReplace(ResStr, '''', '', [rfIgnoreCase, rfReplaceAll]);
    ResStr := StringReplace(ResStr, '#$D', #13, [rfIgnoreCase, rfReplaceAll]);
    ResStr := StringReplace(ResStr, '#$A', #10, [rfIgnoreCase, rfReplaceAll]);

{$IFDEF DEBUG}
    OutputDebugString(PChar('GetReplacementValue:FormatResult-ResStr: '
      + ResStr));
{$ENDIF}
  end;

begin
{$IFDEF DEBUG}
  OutputDebugString(PChar('GetReplacementValue:' + TypeName));
{$ENDIF}

  TypeType := TTypeType(-1);
  for I := Low(RpcVisualizerTypes) to High(RpcVisualizerTypes) do
  begin
    if TypeName = RpcVisualizerTypes[I].TypeName then
    begin
      TypeType := RpcVisualizerTypes[I].TypeType;
      Break;
    end;
  end;
{$IFDEF DEBUG}
  OutputDebugString(PChar('TypeType:' + IntToStr(Ord(TypeType))));
{$ENDIF}

  Result := EvalResult;
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    CurProcess := DebugSvcs.CurrentProcess;
  if CurProcess <> nil then
  begin
    CurThread := CurProcess.CurrentThread;
    if CurThread <> nil then
    begin
      EvalExp := Expression + '.ToString';
      EvalRes := CurThread.Evaluate(EvalExp, @ResultStr, Length(ResultStr),
        CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);

{$IFDEF DEBUG}
      OutputDebugString(PChar('+ EvalExp:' + EvalExp));
      OutputDebugString(PChar('+ EvalRes:' + IntToStr(Ord(EvalRes))));
      OutputDebugString(PChar('+ ResultStr:' + ResultStr));
{$ENDIF}
      if EvalRes = erOK then
      begin
{$IFDEF DEBUG}
        OutputDebugString(PChar('EvalRes:erOK'));
{$ENDIF}
        if not FormatResult(ResultStr, TypeType, Result) then
          Result := EvalResult;
      end
      else if EvalRes = erDeferred then
      begin
{$IFDEF DEBUG}
        OutputDebugString(PChar('EvalRes:erDeferred'));
{$ENDIF}
        FCompleted := False;
        FDeferredResult := '';
        FNotifierIndex := CurThread.AddNotifier(Self);
        while not FCompleted do
        begin
          DebugSvcs.ProcessDebugEvents;
{$IFDEF DEBUG}
          OutputDebugString(PChar('not FCompleted'));
{$ENDIF}
        end;
        CurThread.RemoveNotifier(FNotifierIndex);
        FNotifierIndex := -1;
{$IFDEF DEBUG}
        OutputDebugString(PChar('FDeferredResult=' + FDeferredResult));
{$ENDIF}
        if (FDeferredResult = '') or not FormatResult(FDeferredResult, TypeType,
          Result) then
          Result := EvalResult;
      end
      else if EvalRes = erError then
      begin
{$IFDEF DEBUG}
        OutputDebugString(PChar('EvalRes:erError'));
{$ENDIF}
      end
      else if EvalRes = erBusy then
      begin
{$IFDEF DEBUG}
        OutputDebugString(PChar('EvalRes:erBusy'));
{$ENDIF}
      end;

    end
    else
    begin
{$IFDEF DEBUG}
      OutputDebugString(PChar('CurThread is nil'));
{$ENDIF}
    end;
  end
  else
  begin
{$IFDEF DEBUG}
    OutputDebugString(PChar('CurProcess is nil'));
{$ENDIF}
  end;

{$IFDEF DEBUG}
  OutputDebugString(PChar('GetReplacementValueResult:' + TypeName + ' > '
    + Result));
{$ENDIF}

end;

function TDebuggerDXmlRpcVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := Length(RpcVisualizerTypes);
{$IFDEF DEBUG}
  OutputDebugString(PChar('GetSupportedTypeCount:' + IntToStr(Result)));
{$ENDIF}
end;

procedure TDebuggerDXmlRpcVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  AllDescendants := False;
  TypeName := RpcVisualizerTypes[Index].TypeName;
{$IFDEF DEBUG}
  OutputDebugString(PChar('GetSupportedType:' + TypeName));
{$ENDIF}
end;

function TDebuggerDXmlRpcVisualizer.GetVisualizerDescription: string;
begin
  Result := sRpcVisualizerDescription;
{$IFDEF DEBUG}
  OutputDebugString(PChar('GetVisualizerDescription:' + Result));
{$ENDIF}
end;

function TDebuggerDXmlRpcVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
{$IFDEF DEBUG}
  OutputDebugString(PChar('GetVisualizerIdentifier:' + Result));
{$ENDIF}
end;

function TDebuggerDXmlRpcVisualizer.GetVisualizerName: string;
begin
  Result := sRpcVisualizerName;
{$IFDEF DEBUG}
  OutputDebugString(PChar('GetVisualizerName:' + Result));
{$ENDIF}
end;

var
  Vis: IOTADebuggerVisualizer;

procedure Register;
begin
  Vis := TDebuggerDXmlRpcVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(Vis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(Vis);
    Vis := nil;
  end;
end;

initialization

finalization

RemoveVisualizer;

end.
