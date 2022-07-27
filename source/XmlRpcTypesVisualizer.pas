unit XmlRpcTypesVisualizer;

interface

procedure Register;

implementation

uses
  Classes,
  Vcl.Forms,
  SysUtils,
  Winapi.Windows,
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
  OutputDebugString('EvaluateComplete');
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
  OutputDebugString(PChar(' [+] EvaluateComplete:' + IntToStr(ReturnCode)));
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
    OutputDebugString(PChar('GetReplacementValue:FormatResult-LEvalResult: ' +
      LEvalResult));
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

    OutputDebugString(PChar('GetReplacementValue:FormatResult-ResStr: '
      + ResStr));
  end;

begin
  OutputDebugString(PChar('GetReplacementValue:' + TypeName));

  TypeType := TTypeType(-1);
  for I := Low(RpcVisualizerTypes) to High(RpcVisualizerTypes) do
  begin
    if TypeName = RpcVisualizerTypes[I].TypeName then
    begin
      TypeType := RpcVisualizerTypes[I].TypeType;
      Break;
    end;
  end;
  OutputDebugString(PChar('TypeType:' + IntToStr(Ord(TypeType))));

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

      OutputDebugString(PChar('+ EvalExp:' + EvalExp));
      OutputDebugString(PChar('+ EvalRes:' + IntToStr(Ord(EvalRes))));
      OutputDebugString(PChar('+ ResultStr:' + ResultStr));
      if EvalRes = erOK then
      begin
        OutputDebugString(PChar('EvalRes:erOK'));
        if not FormatResult(ResultStr, TypeType, Result) then
          Result := EvalResult;
      end
      else if EvalRes = erDeferred then
      begin
        OutputDebugString(PChar('EvalRes:erDeferred'));
        FCompleted := False;
        FDeferredResult := '';
        FNotifierIndex := CurThread.AddNotifier(Self);
        while not FCompleted do
        begin
          DebugSvcs.ProcessDebugEvents;
          OutputDebugString(PChar('not FCompleted'));
        end;
        CurThread.RemoveNotifier(FNotifierIndex);
        FNotifierIndex := -1;
        OutputDebugString(PChar('FDeferredResult=' + FDeferredResult));
        if (FDeferredResult = '') or not FormatResult(FDeferredResult, TypeType,
          Result) then
          Result := EvalResult;
      end
      else if EvalRes = erError then
      begin
        OutputDebugString(PChar('EvalRes:erError'));
      end
      else if EvalRes = erBusy then
      begin
        OutputDebugString(PChar('EvalRes:erBusy'));
      end;

    end
    else
    begin
      OutputDebugString(PChar('CurThread is nil'));
    end;
  end
  else
  begin
    OutputDebugString(PChar('CurProcess is nil'));
  end;

  OutputDebugString(PChar('GetReplacementValueResult:' + TypeName + ' > '
    + Result));

end;

function TDebuggerDXmlRpcVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := Length(RpcVisualizerTypes);
  OutputDebugString(PChar('GetSupportedTypeCount:' + IntToStr(Result)));
end;

procedure TDebuggerDXmlRpcVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants: Boolean);
begin
  AllDescendants := False;
  TypeName := RpcVisualizerTypes[Index].TypeName;
  OutputDebugString(PChar('GetSupportedType:' + TypeName));
end;

function TDebuggerDXmlRpcVisualizer.GetVisualizerDescription: string;
begin
  Result := sRpcVisualizerDescription;
  OutputDebugString(PChar('GetVisualizerDescription:' + Result));
end;

function TDebuggerDXmlRpcVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
  OutputDebugString(PChar('GetVisualizerIdentifier:' + Result));
end;

function TDebuggerDXmlRpcVisualizer.GetVisualizerName: string;
begin
  Result := sRpcVisualizerName;
  OutputDebugString(PChar('GetVisualizerName:' + Result));
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
