unit CpuView.Core;

{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 6060 off : Case statement does not handle all possible cases}
{$ENDIF}

interface

{ TODO:
Добавить в скрипт поддержку "bp user32.MessageBoxW"
Настройки:
отображать имена функций вместо адресов
показывать опкоды инструкций
показывать строки исходоного кода
}

uses
  {$IFDEF FPC}
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes,
  Math,
  SysUtils,
  Controls,
  Generics.Collections,
  FWHexView.Common,
  FWHexView,
  FWHexView.MappedView,
  CpuView.Common,
  CpuView.Viewers,
  CpuView.Settings,
  CpuView.Stream,
  {$IFDEF MSWINDOWS}
  CpuView.Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  CpuView.Linux,
  {$ENDIF}
  CpuView.DebugerGate,
  CpuView.CPUContext;

type
  TAsmLine = record
    AddrVA: Int64;
    DecodedStr, HintStr: string;
    Len: Integer;
    JmpTo: Int64;
    CallInstruction: Boolean;
  end;

  TDumpViewRec = record
    View: TDumpView;
    Stream: TBufferedROStream;
    LastAddrVA: Int64;
  end;

  { TDumpViewList }

  TDumpViewList = class
  private
    FAddressMode: TAddressMode;
    FItemIndex: Integer;
    FItems: TList<TDumpViewRec>;
    FOnUpdated: TOnCacheUpdated;
    FUtils: TCommonUtils;
    FOnUpdate: TOnCacheUpdated;
    function CheckIndex(AValue: Integer): Boolean;
    function GetLastAddrVA: Int64;
    function GetStream: TBufferedROStream;
    function GetView: TDumpView;
    procedure SetAddressMode(AValue: TAddressMode);
    procedure SetItemIndex(AValue: Integer);
    procedure SetLastAddrVA(AValue: Int64);
    procedure SetOnUpdated(AValue: TOnCacheUpdated);
  public
    constructor Create(AUtils: TCommonUtils);
    destructor Destroy; override;
    function Add(AValue: TDumpView): Integer;
    function Count: Integer;
    procedure Delete(Index: Integer);
    procedure Reset;
    procedure Restore(DefAddrVA: Int64);
    procedure Update(Index: Integer; AddrVA: Int64);
    property AddressMode: TAddressMode read FAddressMode write SetAddressMode;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property LastAddrVA: Int64 read GetLastAddrVA write SetLastAddrVA;
    property Stream: TBufferedROStream read GetStream;
    property View: TDumpView read GetView;
    property OnUpdated: TOnCacheUpdated read FOnUpdated write SetOnUpdated;
  end;

  { TCpuViewCore }

  TCpuViewCore = class
  private
    FAddrIndex: TDictionary<Int64, Integer>;
    FAsmView: TAsmView;
    FAsmSelStart, FAsmSelEnd: Int64;
    FAsmStream: TBufferedROStream;
    FCacheList: TList<TAsmLine>;
    FCacheListIndex: Integer;
    FDebugger: TAbstractDebugger;
    FDisassemblyStream: TBufferedROStream;
    FDumpViewList: TDumpViewList;
    FInvalidReg: TRegionData;
    FJmpStack: TStack<Int64>;
    FKnownFunctionAddrVA: TDictionary<Int64, string>;
    FLockPushToJmpStack: Boolean;
    FLockSelChange: Boolean;
    FLastCtx: TCommonCpuContext;
    FRegView: TRegView;
    FShowCallFuncName: Boolean;
    FStackView: TStackView;
    FStackStream: TBufferedROStream;
    FOldAsmScroll: TOnVerticalScrollEvent;
    FOldAsmSelect: TNotifyEvent;
    FUtils: TCommonUtils;
    FReset: TNotifyEvent;
    function CanWork: Boolean;
    procedure DoReset;
    function GetAddrMode: TAddressMode;
    procedure OnAsmJmpTo(Sender: TObject; const AJmpAddr: Int64;
      AJmpState: TJmpState; var Handled: Boolean);
    procedure OnAsmScroll(Sender: TObject; AStep: TScrollStepDirection);
    procedure OnAsmSelectionChange(Sender: TObject);
    procedure OnBreakPointsChange(Sender: TObject);
    procedure OnDebugerChage(Sender: TObject);
    procedure OnDebugerStateChange(Sender: TObject);
    procedure SetAsmView(const Value: TAsmView);
    procedure SetDebugger(const Value: TAbstractDebugger);
    procedure SetRegView(const Value: TRegView);
    procedure SetShowCallFuncName(AValue: Boolean);
    procedure SetStackView(const Value: TStackView);
  protected
    procedure BuildAsmWindow(AAddress: Int64);
    function CacheVisibleRows: Integer;
    function GenerateCache(AAddress: Int64): Integer;
    function GetKnownFunctionAtAddr(AddrVA: Uint64): string;
    procedure LoadFromCache(AIndex: Integer);
    procedure RegViewQueryComment(Sender: TObject; AddrVA: UInt64;
      AColumn: TColumnType; var AComment: string);
    procedure RefreshView(Forced: Boolean = False);
    procedure RefreshAsmView(Forced: Boolean);
    procedure ResetCache;
    procedure StackViewQueryComment(Sender: TObject; AddrVA: UInt64;
      AColumn: TColumnType; var AComment: string);
    procedure UpdateStreamsProcessID;
  public
    constructor Create;
    destructor Destroy; override;
    function AddrInAsm(AddrVA: Int64): Boolean;
    function AddrInDump(AddrVA: Int64): Boolean;
    function AddrInStack(AddrVA: Int64): Boolean;
    procedure ShowDisasmAtAddr(AddrVA: Int64);
    procedure ShowDumpAtAddr(AddrVA: Int64);
    procedure ShowStackAtAddr(AddrVA: Int64);
    function UpdateRegValue(RegID: Integer; ANewRegValue: UInt64): Boolean;
    property AsmView: TAsmView read FAsmView write SetAsmView;
    property Debugger: TAbstractDebugger read FDebugger write SetDebugger;
    property DumpViewList: TDumpViewList read FDumpViewList;
    property RegView: TRegView read FRegView write SetRegView;
    property StackView: TStackView read FStackView write SetStackView;
  public
    property ShowCallFuncName: Boolean read FShowCallFuncName write SetShowCallFuncName;
    property OnReset: TNotifyEvent read FReset write FReset;
  end;

implementation

const
  DisasmBuffSize = 384;

{ TDumpViewList }

function TDumpViewList.CheckIndex(AValue: Integer): Boolean;
begin
  Result := (AValue >= 0) and (AValue < FItems.Count);
end;

function TDumpViewList.GetLastAddrVA: Int64;
begin
  if CheckIndex(ItemIndex) then
    Result := FItems.List[ItemIndex].LastAddrVA
  else
    Result := 0;
end;

function TDumpViewList.GetStream: TBufferedROStream;
begin
  if CheckIndex(ItemIndex) then
    Result := FItems.List[ItemIndex].Stream
  else
    Result := nil;
end;

function TDumpViewList.GetView: TDumpView;
begin
  if CheckIndex(ItemIndex) then
    Result := FItems.List[ItemIndex].View
  else
    Result := nil;
end;

procedure TDumpViewList.SetAddressMode(AValue: TAddressMode);
var
  I: Integer;
begin
  if AddressMode <> AValue then
  begin
    FAddressMode := AValue;
    for I := 0 to Count - 1 do
    begin
      FItems.List[ItemIndex].View.AddressMode := AValue;
      FItems.List[ItemIndex].View.FitColumnsToBestSize;
    end;
  end;
end;

procedure TDumpViewList.SetItemIndex(AValue: Integer);
begin
  if ItemIndex <> AValue then
  begin
    if CheckIndex(AValue) then
      FItemIndex := AValue
    else
      FItemIndex := -1;
  end;
end;

procedure TDumpViewList.SetLastAddrVA(AValue: Int64);
begin
  if CheckIndex(ItemIndex) then
    FItems.List[ItemIndex].LastAddrVA := AValue;
end;

procedure TDumpViewList.SetOnUpdated(AValue: TOnCacheUpdated);
var
  I: Integer;
begin
  FOnUpdated := AValue;
  for I := 0 to Count - 1 do
    FItems.List[ItemIndex].Stream.Stream.OnUpdated := AValue;
end;

constructor TDumpViewList.Create(AUtils: TCommonUtils);
begin
  FUtils := AUtils;
  FItems := TList<TDumpViewRec>.Create;
end;

destructor TDumpViewList.Destroy;
begin
  while Count > 0 do
    Delete(0);
  FItems.Free;
  inherited Destroy;
end;

function TDumpViewList.Add(AValue: TDumpView): Integer;
var
  RemoteStream: TRemoteStream;
  Data: TDumpViewRec;
begin
  Data.LastAddrVA := 0;
  Data.View := AValue;
  RemoteStream := TRemoteStream.Create(FUtils);
  Data.Stream := TBufferedROStream.Create(RemoteStream, soOwned);
  Result := FItems.Add(Data);
  AValue.AddressMode := AddressMode;
  AValue.FitColumnsToBestSize;
end;

function TDumpViewList.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TDumpViewList.Delete(Index: Integer);
begin
  if not CheckIndex(Index) then Exit;
  FItems.List[Index].View.SetDataStream(nil, 0);
  FItems.List[Index].Stream.Free;
  FItems.Delete(Index);
  if Index < ItemIndex then
    Dec(FItemIndex);
end;

procedure TDumpViewList.Reset;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FItems.List[I].View.SetDataStream(nil, 0);
end;

procedure TDumpViewList.Restore(DefAddrVA: Int64);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if FItems.List[I].LastAddrVA = 0 then
      FItems.List[I].LastAddrVA := DefAddrVA;
    Update(I, FItems.List[I].LastAddrVA);
  end;
end;

procedure TDumpViewList.Update(Index: Integer; AddrVA: Int64);
var
  RegData: TRegionData;
  AStream: TBufferedROStream;
  AView: TDumpView;
begin
  if not CheckIndex(Index) then Exit;
  if not FUtils.QueryRegion(AddrVA, RegData) then Exit;
  AView := FItems.List[Index].View;
  AStream := FItems.List[Index].Stream;
  AStream.Stream.OnUpdated := FOnUpdate;
  AStream.SetAddrWindow(RegData.BaseAddr, RegData.RegionSize);
  AView.SetDataStream(AStream, RegData.BaseAddr);
  AView.AddressMode := AddressMode;
  AView.FocusOnAddress(AddrVA, ccmSelectRow);
  FItems.List[Index].LastAddrVA := AddrVA;
end;

{ TCpuViewCore }

function TCpuViewCore.AddrInStack(AddrVA: Int64): Boolean;
var
  StackLim: TStackLimit;
begin
  StackLim := FDebugger.ThreadStackLimit;
  Result := (AddrVA <= StackLim.Base) and (AddrVA >= StackLim.Limit);
end;

procedure TCpuViewCore.BuildAsmWindow(AAddress: Int64);
const
  MM_HIGHEST_USER_ADDRESS32 = $7FFEFFFF;
  MM_HIGHEST_USER_ADDRESS64 = $7FFFFFFF0000;
var
  CacheIndex: Integer;
  StreamSize: Int64;
begin
  if FAsmStream = nil then Exit;
  FAsmView.BeginUpdate;
  try
    if not FAddrIndex.TryGetValue(AAddress, CacheIndex) then
    begin
      CacheIndex := GenerateCache(AAddress);
      if CacheIndex >= 0 then
        AAddress := FCacheList[CacheIndex].AddrVA;
    end;
    if CanWork and (FDebugger.PointerSize = 4) then
      StreamSize := MM_HIGHEST_USER_ADDRESS32
    else
      StreamSize := MM_HIGHEST_USER_ADDRESS64;
    FAsmStream.SetAddrWindow(AAddress, StreamSize);
    FAsmView.SetDataStream(FAsmStream, AAddress);
    LoadFromCache(CacheIndex);
  finally
    FAsmView.EndUpdate;
  end;
  if CacheIndex >= 0 then
  begin
    FAsmView.SelStart := Max(FAsmSelStart, FCacheList.List[CacheIndex].AddrVA);
    FAsmView.SelEnd := FAsmSelEnd;
  end;
end;

function TCpuViewCore.CacheVisibleRows: Integer;
begin
  if Assigned(AsmView) then
    Result := AsmView.VisibleRowCount shl 1
  else
    Result := 0;
end;

constructor TCpuViewCore.Create;
var
  RemoteStream: TRemoteStream;
begin
  FCacheList := TList<TAsmLine>.Create;
  FAddrIndex := TDictionary<Int64, Integer>.Create;
  FUtils := TCommonUtils.Create;
  FDumpViewList := TDumpViewList.Create(FUtils);
  RemoteStream := TRemoteStream.Create(FUtils);
  FAsmStream := TBufferedROStream.Create(RemoteStream, soOwned);
  FAsmStream.BufferSize := DisasmBuffSize;
  RemoteStream := TRemoteStream.Create(FUtils);
  FDisassemblyStream := TBufferedROStream.Create(RemoteStream, soOwned);
  FDisassemblyStream.BufferSize := DisasmBuffSize;
  RemoteStream := TRemoteStream.Create(FUtils);
  FStackStream := TBufferedROStream.Create(RemoteStream, soOwned);
  FKnownFunctionAddrVA := TDictionary<Int64, string>.Create;
  FShowCallFuncName := True;
  FJmpStack := TStack<Int64>.Create;
end;

destructor TCpuViewCore.Destroy;
begin
  FCacheList.Free;
  FDumpViewList.Free;
  FAddrIndex.Free;
  FAsmStream.Free;
  FDisassemblyStream.Free;
  FStackStream.Free;
  FKnownFunctionAddrVA.Free;
  FJmpStack.Free;
  FUtils.Free;
  inherited;
end;

function TCpuViewCore.AddrInAsm(AddrVA: Int64): Boolean;
var
  RegionData: TRegionData;
begin
  Result :=
    Assigned(Debugger) and
    Assigned(AsmView) and
    Assigned(AsmView.DataStream) and
    FUtils.QueryRegion(AddrVA, RegionData) and
    RegionData.Readable and RegionData.Executable;
end;

function TCpuViewCore.AddrInDump(AddrVA: Int64): Boolean;
var
  RegData: TRegionData;
begin
  if FInvalidReg.RegionSize = 0 then
    FUtils.QueryRegion(AddrVA, FInvalidReg);
  if AddrVA < FInvalidReg.RegionSize then
    Result := False
  else
    Result := FUtils.QueryRegion(AddrVA, RegData) and RegData.Readable;
end;

function TCpuViewCore.GenerateCache(AAddress: Int64): Integer;

  procedure BuildCacheFromBuff(FromAddr: Int64; FromBuff: PByte;
    BufSize: Integer);
  var
    List: TList<TInstruction>;
    Inst: TInstruction;
    AsmLine: TAsmLine;
    SpaceIndex: Integer;
  begin
    List := Debugger.Disassembly(FromAddr, FromBuff, BufSize);
    try
      for Inst in List do
      begin
        AsmLine.AddrVA := FromAddr;
        if Inst.Len > BufSize then
          Break;
        AsmLine.DecodedStr := Inst.AsString;
        AsmLine.HintStr := Inst.Hint;
        AsmLine.Len := Inst.Len;
        AsmLine.JmpTo := Inst.JmpTo;
        AsmLine.CallInstruction := False;
        if ShowCallFuncName and (AsmLine.JmpTo <> 0) then
        begin
          if AsmLine.DecodedStr.StartsWith('CALL') then
          begin
            SpaceIndex := Pos(' ', AsmLine.HintStr);
            if SpaceIndex = 0 then
              SpaceIndex := Length(AsmLine.HintStr) + 1;
            AsmLine.DecodedStr := 'CALL ' + Copy(AsmLine.HintStr, 1, SpaceIndex - 1);
            AsmLine.HintStr := '';
            AsmLine.CallInstruction := True;
          end;
        end;
        FAddrIndex.TryAdd(FromAddr, FCacheList.Count);
        FCacheList.Add(AsmLine);
        Inc(FromAddr, AsmLine.Len);
        Dec(BufSize, AsmLine.Len);
      end;
    finally
      List.Free;
    end;

    if BufSize > 0 then
    begin
      AsmLine.DecodedStr := '???';
      AsmLine.Len := BufSize;
      AsmLine.JmpTo := 0;
      FAddrIndex.TryAdd(FromAddr, FCacheList.Count);
      FCacheList.Add(AsmLine);
    end;
  end;

var
  WindowAddr: Int64;
  Count, TopCacheSize: Integer;
  Buff: array of Byte;
  RegData: TRegionData;
begin
  Result := -1;
  if FDisassemblyStream = nil then Exit;
  ResetCache;
  if FUtils.QueryRegion(AAddress, RegData) then
    WindowAddr := Max(AAddress - 128, RegData.AllocationBase)
  else
    WindowAddr := AAddress;
  TopCacheSize := AAddress - WindowAddr;

  FDisassemblyStream.SetAddrWindow(WindowAddr, DisasmBuffSize);
  SetLength(Buff{%H-}, DisasmBuffSize);
  Count := FDisassemblyStream.Read(Buff[0], DisasmBuffSize);
  if Count > 0 then
  begin
    if TopCacheSize > 0 then
      BuildCacheFromBuff(WindowAddr, @Buff[0], TopCacheSize);
    Result := FCacheList.Count;
    BuildCacheFromBuff(AAddress, @Buff[TopCacheSize], Count - TopCacheSize);
  end;
end;

function TCpuViewCore.GetKnownFunctionAtAddr(AddrVA: Uint64): string;
begin
  Result := '';
  if not FKnownFunctionAddrVA.TryGetValue(AddrVA, Result) then
  begin
    Result := Debugger.QuerySymbolAtAddr(AddrVA, qsName);
    // держим кэш на 1024 адреса, этого будет достаточно
    if FKnownFunctionAddrVA.Count > 1024 then
      FKnownFunctionAddrVA.Clear;
    FKnownFunctionAddrVA.Add(AddrVA, Result);
  end;
end;

procedure TCpuViewCore.LoadFromCache(AIndex: Integer);
var
  I: Integer;
  Line: TAsmLine;
begin
  FAsmView.BeginUpdate;
  try
    FAsmView.DataMap.Clear;
    FCacheListIndex := AIndex;
    if AIndex < 0 then Exit;
    Line := FCacheList.List[AIndex];
    if Line.Len = 0 then
      FAsmView.DataMap.AddComment(Line.AddrVA, Line.DecodedStr)
    else
      if Line.CallInstruction then
        FAsmView.DataMap.AddAsm(Line.AddrVA, Line.Len, Line.DecodedStr, '', Line.JmpTo, 5, Length(Line.DecodedStr) - 5)
      else
        FAsmView.DataMap.AddAsm(Line.AddrVA, Line.Len, Line.DecodedStr, Line.HintStr, Line.JmpTo, 0, 0);
    for I := 1 to Min(CacheVisibleRows, FCacheList.Count - AIndex - 1) do
    begin
      Line := FCacheList.List[I + AIndex];
      if Line.Len = 0 then
        FAsmView.DataMap.AddComment(Line.DecodedStr)
      else
        if Line.CallInstruction then
          FAsmView.DataMap.AddAsm(Line.Len, Line.DecodedStr, '', Line.JmpTo, 5, Length(Line.DecodedStr) - 5)
        else
          FAsmView.DataMap.AddAsm(Line.Len, Line.DecodedStr, Line.HintStr, Line.JmpTo, 0, 0);
    end;
  finally
    FAsmView.EndUpdate;
  end;
end;

procedure TCpuViewCore.RegViewQueryComment(Sender: TObject; AddrVA: UInt64;
  AColumn: TColumnType; var AComment: string);
begin
  AComment := GetKnownFunctionAtAddr(AddrVA);
end;

function TCpuViewCore.CanWork: Boolean;
begin
  Result := Assigned(FDebugger) and (FDebugger.DebugState = adsPaused);
end;

procedure TCpuViewCore.DoReset;
begin
  if Assigned(FReset) then
    FReset(Self);;
end;

function TCpuViewCore.GetAddrMode: TAddressMode;
begin
  case FDebugger.PointerSize of
    8: Result := am64bit;
    4: Result := am32bit;
    2: Result := am16bit;
  else
    Result := am8bit;
  end;
end;

procedure TCpuViewCore.OnAsmJmpTo(Sender: TObject; const AJmpAddr: Int64;
  AJmpState: TJmpState; var Handled: Boolean);
var
  NewJmpAddr: Int64;
begin
  case AJmpState of
    jsPushToUndo:
    begin
      Handled := AddrInAsm(AJmpAddr);
      if not Handled then Exit;
      ShowDisasmAtAddr(AJmpAddr);
    end;
    jsPopFromUndo:
    begin
      Handled := True;
      if FJmpStack.Count = 0 then Exit;
      NewJmpAddr := FJmpStack.Pop;
      FLockPushToJmpStack := True;
      try
        ShowDisasmAtAddr(NewJmpAddr);
      finally
        FLockPushToJmpStack := False;
      end;
      NewJmpAddr := FJmpStack.Pop;
      AsmView.FocusOnAddress(NewJmpAddr, ccmSelectRow);
    end;
  end;
end;

procedure TCpuViewCore.OnAsmScroll(Sender: TObject;
  AStep: TScrollStepDirection);
var
  NewCacheIndex: Integer;
begin
  if FCacheList.Count = 0 then Exit;
  NewCacheIndex := FCacheListIndex;
  case AStep of
    ssdLineUp: Dec(NewCacheIndex);
    ssdWheelUp: Dec(NewCacheIndex, FAsmView.WheelMultiplyer);
    ssdPageUp: Dec(NewCacheIndex, FAsmView.VisibleRowCount);
    ssdLineDown: Inc(NewCacheIndex);
    ssdWheelDown: Inc(NewCacheIndex, FAsmView.WheelMultiplyer);
    ssdPageDown: Inc(NewCacheIndex, FAsmView.VisibleRowCount);
  end;
  if NewCacheIndex < 0 then
    NewCacheIndex := GenerateCache(FCacheList[0].AddrVA);
  if NewCacheIndex >= FCacheList.Count - CacheVisibleRows then
    NewCacheIndex := GenerateCache(FCacheList[FCacheList.Count - CacheVisibleRows].AddrVA);
  FLockSelChange := True;
  if (NewCacheIndex >= 0) and (NewCacheIndex < FCacheList.Count) then
    BuildAsmWindow(FCacheList[NewCacheIndex].AddrVA)
  else
    FLockSelChange := False; // dbg...
  FLockSelChange := False;
  if Assigned(FOldAsmScroll) then
    FOldAsmScroll(Sender, AStep);
end;

procedure TCpuViewCore.OnAsmSelectionChange(Sender: TObject);
begin
  if FLockSelChange then Exit;
  FAsmSelStart := Min(FAsmView.SelStart, FAsmView.SelEnd);
  FAsmSelEnd := Max(FAsmView.SelStart, FAsmView.SelEnd);
  if Assigned(FOldAsmSelect) then
    FOldAsmSelect(Sender);
end;

procedure TCpuViewCore.OnBreakPointsChange(Sender: TObject);
var
  I: Integer;
  BP: TBasicBreakPoint;
begin
  AsmView.BreakPoints.Clear;
  for I := 0 to FDebugger.BreakPointList.Count - 1 do
  begin
    BP := FDebugger.BreakPointList[I];
    AsmView.BreakPoints.Add(Int64(BP.AddrVA), BP.Active);
  end;
  AsmView.Invalidate;
end;

procedure TCpuViewCore.OnDebugerChage(Sender: TObject);
begin
  FUtils.Update;
  RefreshView;
end;

procedure TCpuViewCore.OnDebugerStateChange(Sender: TObject);
begin
  case FDebugger.DebugState of
    adsStoped, adsStart:
      UpdateStreamsProcessID;
    adsPaused:
    begin
      if Assigned(FDebugger) and (FDebugger.CurrentInstructionPoint <> FAsmView.InstructionPoint) then
      begin
        FUtils.Update;
        RefreshView;
      end;
    end;
    adsRunning:
    begin
      {$message 'наверное нужно снимать стримы по аналогу с adsFinished'}
      if Assigned(FAsmView) then
      begin
        FAsmView.InstructionPoint := 0;
        FAsmView.CurrentIPIsActiveJmp := False;
        FAsmView.Invalidate;
      end;
      if Assigned(FStackView) then
      begin
        FStackView.Frames.Clear;
        FStackView.Invalidate;
      end;
      DoReset;
    end;
    adsFinished:
    begin
      if Assigned(FAsmView) then
        FAsmView.SetDataStream(nil, 0);
      if Assigned(FRegView) then
        FRegView.Context := nil;
      if Assigned(FStackView) then
        FStackView.SetDataStream(nil, 0);
      FDumpViewList.Reset;
      DoReset;
    end;
  end;
end;

procedure TCpuViewCore.RefreshView(Forced: Boolean);
var
  StackLim: TStackLimit;
begin
  if (FDebugger = nil) or not FDebugger.IsActive then Exit;
  FLastCtx := FDebugger.Context;
  RefreshAsmView(Forced);
  if Assigned(FRegView) then
    FRegView.Context := FLastCtx;
  if Assigned(FStackView) then
  begin
    StackLim := FDebugger.ThreadStackLimit;
    FDebugger.FillThreadStackFrames(StackLim, FLastCtx.StackPoint,
      FLastCtx.StackBase, FStackStream.Stream, FStackView.Frames);
    FStackStream.SetAddrWindow(StackLim.Limit, StackLim.Base - StackLim.Limit);
    FStackView.AddressMode := GetAddrMode;
    FStackView.SetDataStream(FStackStream, StackLim.Limit);
    FStackView.FramesUpdated;
    FStackView.FocusOnAddress(FLastCtx.StackPoint, ccmSelectRow);
  end;
  FDumpViewList.AddressMode := GetAddrMode;
  FDumpViewList.Restore(FDebugger.CurrentInstructionPoint);
end;

procedure TCpuViewCore.RefreshAsmView(Forced: Boolean);
begin
  if Assigned(FAsmView) then
  begin
    FAsmView.AddressMode := GetAddrMode;
    FAsmView.InstructionPoint := FDebugger.CurrentInstructionPoint;
    FAsmView.CurrentIPIsActiveJmp := FDebugger.IsActiveJmp;
    if Forced or not FAsmView.IsAddrVisible(FAsmView.InstructionPoint) then
      BuildAsmWindow(FAsmView.InstructionPoint);
    FAsmView.FocusOnAddress(FAsmView.InstructionPoint, ccmSelectRow);
  end;
end;

procedure TCpuViewCore.ResetCache;
begin
  FAddrIndex.Clear;
  FCacheList.Clear;
  FAsmSelStart := 0;
  FAsmSelEnd := 0;
  FCacheListIndex := 0;
  FKnownFunctionAddrVA.Clear;
  FJmpStack.Clear;
end;

procedure TCpuViewCore.StackViewQueryComment(Sender: TObject; AddrVA: UInt64;
  AColumn: TColumnType; var AComment: string);
var
  AStackValue: Int64;
begin
  case AColumn of
    ctNone: ;
    ctComment:
    begin
      AStackValue := 0;
      if not CanWork then Exit;
      FStackStream.Stream.Position := AddrVA;
      FStackStream.Stream.ReadBuffer(AStackValue, Debugger.PointerSize);
      AComment := GetKnownFunctionAtAddr(AStackValue);
    end;
  end;
end;

procedure TCpuViewCore.SetAsmView(const Value: TAsmView);
begin
  if AsmView <> Value then
  begin
    FAsmView := Value;
    if Value = nil then Exit;
    FOldAsmSelect := FAsmView.OnSelectionChange;
    FAsmView.OnJmpTo := OnAsmJmpTo;
    FAsmView.OnSelectionChange := OnAsmSelectionChange;
    FOldAsmScroll := FAsmView.OnVerticalScroll;
    FAsmView.OnVerticalScroll := OnAsmScroll;
    FAsmView.FitColumnsToBestSize;
    RefreshView;
  end;
end;

procedure TCpuViewCore.SetDebugger(const Value: TAbstractDebugger);
begin
  FDebugger := Value;
  if Assigned(Value) then
  begin
    FDebugger.OnChange := OnDebugerChage;
    FDebugger.OnStateChange := OnDebugerStateChange;
    FDebugger.OnBreakPointsChange := OnBreakPointsChange;
    FAsmStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
    FDisassemblyStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
    FDumpViewList.OnUpdated := FDebugger.UpdateRemoteStream;
    FStackStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
  end
  else
  begin
    FAsmStream.Stream.OnUpdated := nil;
    FDisassemblyStream.Stream.OnUpdated := nil;
    FDumpViewList.OnUpdated := nil;
    FStackStream.Stream.OnUpdated := nil;
  end;
  UpdateStreamsProcessID;
end;

procedure TCpuViewCore.SetRegView(const Value: TRegView);
begin
  if RegView <> Value then
  begin
    FRegView := Value;
    if Value = nil then Exit;
    FRegView.OnQueryComment := RegViewQueryComment;
    RefreshView;
  end;
end;

procedure TCpuViewCore.SetShowCallFuncName(AValue: Boolean);
begin
  if ShowCallFuncName <> AValue then
  begin
    FShowCallFuncName := AValue;
    ResetCache;
    RefreshView(True);
  end;
end;

procedure TCpuViewCore.SetStackView(const Value: TStackView);
begin
  if StackView <> Value then
  begin
    FStackView := Value;
    if Value = nil then Exit;
    Value.FitColumnsToBestSize;
    FStackView.OnQueryComment := StackViewQueryComment;
    RefreshView;
  end;
end;

procedure TCpuViewCore.ShowDisasmAtAddr(AddrVA: Int64);
begin
  if Assigned(FAsmView) then
  begin
    try
      if not FLockPushToJmpStack then
      begin
        FJmpStack.Push(FAsmView.SelectedInstructionAddr);
        FJmpStack.Push(FAsmView.RowToAddress(FAsmView.CurrentVisibleRow, 0));
      end;
      FAsmSelStart := 0;
      FAsmSelEnd := 0;
      if not FAsmView.IsAddrVisible(AddrVA) then
        BuildAsmWindow(AddrVA);
      FAsmView.FocusOnAddress(AddrVA, ccmSelectRow);
    except
      RefreshView;
      raise;
    end;
  end;
end;

procedure TCpuViewCore.ShowDumpAtAddr(AddrVA: Int64);
begin
  if FDumpViewList.Count = 0 then Exit;
  if (AddrVA = 0) and CanWork then
    AddrVA := FDebugger.CurrentInstructionPoint;
  FDumpViewList.Update(FDumpViewList.ItemIndex, AddrVA);
end;

procedure TCpuViewCore.ShowStackAtAddr(AddrVA: Int64);
begin
  if Assigned(FStackView) then
    FStackView.FocusOnAddress(AddrVA, ccmSelectRow);
end;

function TCpuViewCore.UpdateRegValue(RegID: Integer;
  ANewRegValue: UInt64): Boolean;
begin
  Result := False;
  if CanWork then
  begin
    Result := FDebugger.UpdateRegValue(RegID, ANewRegValue);
    if Result and Assigned(FRegView) then
      FRegView.RefreshSelected;
    RefreshAsmView(False);
  end;
end;

procedure TCpuViewCore.UpdateStreamsProcessID;
begin
  if Assigned(FDebugger) and FDebugger.IsActive then
    FUtils.ProcessID := FDebugger.ProcessID
  else
  begin
    FUtils.ProcessID := 0;
    ResetCache;
  end;
end;

end.
