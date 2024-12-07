////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Core.pas
//  * Purpose   : CPU-View mode kernel automates most of the utility actions.
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//

unit CpuView.Core;

{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 5024 off : Parameter "$1" not used}
  {$WARN 6060 off : Case statement does not handle all possible cases}
{$ENDIF}

interface

{ TODO:
  Add support to scryptor "bp user32.MessageBoxW"
}

{$message 'Min/Max col width to FWHexView for each coltype'}
{$message 'After "Run" command, stack data is not cleaned, "return AddrVA" from the frame is hanging around'}
{$message 'Connect all 10 bookmarks on AsmView'}
{$message 'Add a selection option to the dump so that you can keep track of multiple adjacent buffers'}
{$message 'The x87/SIMD registers are not editable'}
{$message 'View for Call param'}
{$message 'Highlighting of identical selected values in the dump window'}
{$message 'Run to user code'}
{$message 'Display strings in hint with disassembly for executable AddrVA and in addr validation (blue color)'}

uses
  {$IFDEF FPC}
  LCLType, LCLProc,
  {$ELSE}
  Windows,
  {$ENDIF}
  Classes,
  Math,
  SysUtils,
  Controls,
  Generics.Collections,
  Menus,
  FWHexView.Common,
  FWHexView,
  FWHexView.MappedView,
  CpuView.Common,
  CpuView.Viewers,
  CpuView.Stream,
  {$IFDEF MSWINDOWS}
  CpuView.Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  CpuView.Linux,
  {$ENDIF}
  CpuView.DebugerGate,
  CpuView.CPUContext,
  CpuView.DBase;

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

  TAddrCacheItem = record
    Region: TRegionData;
    Symbol: string;
    Disassembly: string;
  end;

  TCpuViewCore = class;

  { TDumpViewList }

  TDumpViewList = class
  private
    FAddressMode: TAddressMode;
    FCore: TCpuViewCore;
    FItemIndex: Integer;
    FItems: TListEx<TDumpViewRec>;
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
    constructor Create(ACore: TCpuViewCore; AUtils: TCommonUtils);
    destructor Destroy; override;
    function Add(AValue: TDumpView): Integer;
    function Count: Integer;
    procedure Delete(Index: Integer);
    procedure JumpClear;
    procedure Reset;
    procedure Restore(DefAddrVA: Int64);
    procedure Update(Index: Integer; AddrVA: Int64; ASelLength: Integer; PushToJmpStack: Boolean);
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
    FAsmJmpStack: TJumpStack;
    FAsmView: TAsmView;
    FAsmSelStart, FAsmSelEnd: Int64;
    FAsmStream: TBufferedROStream;
    FCacheList: TListEx<TAsmLine>;
    FCacheListIndex: Integer;
    FDBase: TCpuViewDBase;
    FDebugger: TAbstractDebugger;
    FDisassemblyStream: TBufferedROStream;
    FDumpViewList: TDumpViewList;
    FInDeepDbgInfo: Boolean;
    FInvalidReg: TRegionData;
    FLastStackLimit: TStackLimit;
    FLockSelChange: Boolean;
    FLastCachedAddrVA: Int64;
    FLastCtx: TCommonCpuContext;
    FRegView: TRegView;
    FSessionCache: TDictionary<Int64, TAddrCacheItem>;
    FShowCallFuncName: Boolean;
    FStackChains: Boolean;
    FStackView: TStackView;
    FStackStream: TBufferedROStream;
    FOldAsmScroll: TOnVerticalScrollEvent;
    FOldAsmSelect: TNotifyEvent;
    FUtils: TCommonUtils;
    FReset: TNotifyEvent;
    function CanWork: Boolean;
    function DisasmBuffSize: Integer;
    procedure DoReset;
    function GetAddrMode: TAddressMode;
    procedure OnAsmCacheEnd(Sender: TObject);
    procedure OnAsmJmpTo(Sender: TObject; const AJmpAddr: Int64;
      AJmpState: TJmpState; var Handled: Boolean);
    procedure OnAsmQueryComment(Sender: TObject; AddrVA: Int64;
      AColumn: TColumnType; var AComment: string);
    procedure OnAsmScroll(Sender: TObject; AStep: TScrollStepDirection);
    procedure OnAsmSelectionChange(Sender: TObject);
    procedure OnBreakPointsChange(Sender: TObject);
    procedure OnContextChange(Sender: TObject);
    procedure OnDebugerChage(Sender: TObject);
    procedure OnDebugerStateChange(Sender: TObject);
    procedure OnGetHint(Sender: TObject; const Param: THintParam;
      var Hint: string);
    procedure OnJmpTo(Sender: TObject; const AJmpAddr: Int64;
      AJmpState: TJmpState; var Handled: Boolean);
    procedure OnRegQueryComment(Sender: TObject; AddrVA: Int64;
      AColumn: TColumnType; var AComment: string);
    procedure OnRegQueryExternalComment(Sender: TObject;
      const AValue: TRegValue; ARegType: TExternalRegType; var AComment: string);
    function QueryCacheItem(AddrVA: Int64; out AItem: TAddrCacheItem): Boolean;
    procedure SetAsmView(const Value: TAsmView);
    procedure SetRegView(const Value: TRegView);
    procedure SetStackView(const Value: TStackView);
    procedure SynhronizeViewersWithContext;
  protected
    procedure BuildAsmWindow(AAddress: Int64);
    function CacheVisibleRows: Integer;
    function FormatAccessString(const ARegionData: TRegionData): string;
    function GenerateCache(AAddress: Int64): Integer;
    procedure LoadFromCache(AIndex: Integer);
    procedure OnQueryAddressType(Sender: TObject; AddrVA: Int64; var AddrType: TAddrType);
    procedure RefreshBreakPoints;
    procedure RefreshView(Forced: Boolean = False);
    // Forced - означает принудительную перестройку вьювера
    // Необходимо при изменении настроек отображения
    // -------------------------------------------------------------------------
    // Forced - means forced rebuilding of the viewer
    // Necessary when changing Viewer settings
    procedure RefreshAsmView(Forced: Boolean);
    procedure ResetCache;
    procedure StackViewQueryComment(Sender: TObject; AddrVA: Int64;
      AColumn: TColumnType; var AComment: string);
    procedure UpdateStreamsProcessID;
  public
    constructor Create(ADebuggerClass: TAbstractDebuggerClass);
    destructor Destroy; override;
    function AddrInAsm(AddrVA: Int64): Boolean;
    function AddrInDump(AddrVA: Int64): Boolean;
    function AddrInStack(AddrVA: Int64): Boolean;
    function QueryAccessStr(AddrVA: Int64): string;
    function QueryAddressType(AddrVA: Int64): TAddrType;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean;
    function QuerySymbolAtAddr(AddrVA: Int64): string;
    procedure ShowDisasmAtAddr(AddrVA: Int64; PushToJmpStack: Boolean = True);
    procedure ShowDumpAtAddr(AddrVA: Int64; PushToJmpStack: Boolean = True); overload;
    procedure ShowDumpAtAddr(AddrVA: Int64; ASelLength: Integer; PushToJmpStack: Boolean = True); overload;
    procedure ShowStackAtAddr(AddrVA: Int64);
    procedure UpdateAfterSettingsChange;
    function UpdateRegValue(RegID: Integer; ANewRegValue: Int64): Boolean;
  public
    property AsmView: TAsmView read FAsmView write SetAsmView;
    property DBase: TCpuViewDBase read FDBase;
    property Debugger: TAbstractDebugger read FDebugger;
    property DumpViewList: TDumpViewList read FDumpViewList;
    // Адрес от которого был построен последний кэш.
    // Необходим для правильного выполнения операций Undo/Redo в кэше переходов.
    // -------------------------------------------------------------------------
    // Address on the basis of which the current cache was built.
    // Necessary for proper Undo/Redo operations in the transition cache.
    property LastCachedAddrVA: Int64 read FLastCachedAddrVA;
    property LastStackLimit: TStackLimit read FLastStackLimit;
    property RegView: TRegView read FRegView write SetRegView;
    property StackView: TStackView read FStackView write SetStackView;
    property ShowCallFuncName: Boolean read FShowCallFuncName write FShowCallFuncName;
    property UseInDeepDbgInfo: Boolean read FInDeepDbgInfo write FInDeepDbgInfo;
    property UseStackChains: Boolean read FStackChains write FStackChains;
    property OnReset: TNotifyEvent read FReset write FReset;
  end;

implementation

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

constructor TDumpViewList.Create(ACore: TCpuViewCore; AUtils: TCommonUtils);
begin
  FCore := ACore;
  FUtils := AUtils;
  FItems := TListEx<TDumpViewRec>.Create;
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
  AValue.OnHint := FCore.OnGetHint;
  AValue.OnJmpTo := FCore.OnJmpTo;
  AValue.OnQueryAddressType := FCore.OnQueryAddressType;
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

procedure TDumpViewList.JumpClear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FItems.List[I].View.JumpClear;
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
    Update(I, FItems.List[I].LastAddrVA, 0, False);
  end;
end;

procedure TDumpViewList.Update(Index: Integer; AddrVA: Int64; ASelLength: Integer;
  PushToJmpStack: Boolean);
var
  RegData: TRegionData;
  AStream: TBufferedROStream;
  AView: TDumpView;
begin
  if not CheckIndex(Index) then Exit;
  AView := FItems.List[Index].View;
  if PushToJmpStack then
  begin
    AView.JumpToAddress(AddrVA, ASelLength);
    Exit;
  end;
  if not FCore.QueryRegion(AddrVA, RegData) then Exit;
  AStream := FItems.List[Index].Stream;
  AStream.Stream.OnUpdated := FOnUpdate;
  AStream.SetAddrWindow(RegData.BaseAddr, RegData.RegionSize);
  AView.SetDataStream(AStream, RegData.BaseAddr);
  AView.AddressMode := AddressMode;
  AView.FocusOnAddress(AddrVA, ccmSelectPointer);
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

function TCpuViewCore.QueryAccessStr(AddrVA: Int64): string;
var
  CacheItem: TAddrCacheItem;
begin
  Result := 'No access';
  if QueryCacheItem(AddrVA, CacheItem) then
    Result := FormatAccessString(CacheItem.Region);
end;

function TCpuViewCore.QueryAddressType(AddrVA: Int64): TAddrType;
var
  CacheItem: TAddrCacheItem;
begin
  Result := atNone;
  if not QueryCacheItem(AddrVA, CacheItem) then Exit;
  if (raExecute in CacheItem.Region.Access) then
    Exit(atExecute);
  if (AddrVA <= LastStackLimit.Base) and (AddrVA >= LastStackLimit.Limit) then
    Exit(atStack);
  if (raRead in CacheItem.Region.Access) then
    Result := atRead;
end;

function TCpuViewCore.QueryRegion(AddrVA: Int64; out RegionData: TRegionData
  ): Boolean;
var
  CacheItem: TAddrCacheItem;
begin
  RegionData := Default(TRegionData);
  Result := QueryCacheItem(AddrVA, CacheItem);
  if Result then
    RegionData := CacheItem.Region;
end;

function TCpuViewCore.QuerySymbolAtAddr(AddrVA: Int64): string;
var
  CacheItem: TAddrCacheItem;
begin
  if QueryCacheItem(AddrVA, CacheItem) then
    Result := CacheItem.Symbol
  else
    Result := '';
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
      begin
        FLastCachedAddrVA := AAddress;
        AAddress := FCacheList[CacheIndex].AddrVA;
      end;
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

function TCpuViewCore.FormatAccessString(const ARegionData: TRegionData
  ): string;
begin
  Result := 'No access';
  if (ARegionData.Access <> []) and not (raProtect in ARegionData.Access) then
  begin
    Result := '...';
    if raRead in ARegionData.Access then
      Result[1] := 'R';
    if raWrite in ARegionData.Access then
      Result[2] := 'W';
    if raExecute in ARegionData.Access then
      Result[3] := 'E';
  end;
end;

constructor TCpuViewCore.Create(ADebuggerClass: TAbstractDebuggerClass);
var
  RemoteStream: TRemoteStream;
begin
  FCacheList := TListEx<TAsmLine>.Create;
  FAddrIndex := TDictionary<Int64, Integer>.Create;
  FUtils := TCommonUtils.Create;
  FDumpViewList := TDumpViewList.Create(Self, FUtils);
  RemoteStream := TRemoteStream.Create(FUtils);
  FAsmStream := TBufferedROStream.Create(RemoteStream, soOwned);
  FAsmStream.BufferSize := DisasmBuffSize;
  RemoteStream := TRemoteStream.Create(FUtils);
  FDisassemblyStream := TBufferedROStream.Create(RemoteStream, soOwned);
  FDisassemblyStream.BufferSize := DisasmBuffSize;
  RemoteStream := TRemoteStream.Create(FUtils);
  FStackStream := TBufferedROStream.Create(RemoteStream, soOwned);
  FDebugger := ADebuggerClass.Create(nil, FUtils);
  FDebugger.OnChange := OnDebugerChage;
  FDebugger.OnContextChange := OnContextChange;
  FDebugger.OnStateChange := OnDebugerStateChange;
  FDebugger.OnBreakPointsChange := OnBreakPointsChange;
  FAsmStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
  FDisassemblyStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
  FDumpViewList.OnUpdated := FDebugger.UpdateRemoteStream;
  FStackStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
  FSessionCache := TDictionary<Int64, TAddrCacheItem>.Create;
  FShowCallFuncName := True;
  FAsmJmpStack := TJumpStack.Create;
  FDBase := TCpuViewDBase.Create;
end;

destructor TCpuViewCore.Destroy;
begin
  FDBase.Free;
  FCacheList.Free;
  FDumpViewList.Free;
  FAddrIndex.Free;
  FAsmStream.Free;
  FDisassemblyStream.Free;
  FStackStream.Free;
  FSessionCache.Free;
  FAsmJmpStack.Free;
  FDebugger.Free;
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
    QueryRegion(AddrVA, RegionData) and
    (raExecute in RegionData.Access);
end;

function TCpuViewCore.AddrInDump(AddrVA: Int64): Boolean;
var
  RegData: TRegionData;
begin
  if FInvalidReg.RegionSize = 0 then
    QueryRegion(AddrVA, FInvalidReg);
  if AddrVA < FInvalidReg.RegionSize then
    Result := False
  else
    Result := QueryRegion(AddrVA, RegData) and (raRead in RegData.Access);
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
  if QueryRegion(AAddress, RegData) then
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

procedure TCpuViewCore.RefreshBreakPoints;
var
  I: Integer;
  BP: TBasicBreakPoint;
begin
  if AsmView = nil then Exit;
  AsmView.BreakPoints.Clear;
  if (FDebugger = nil) or not FDebugger.IsActive then Exit;
  for I := 0 to FDebugger.BreakPointList.Count - 1 do
  begin
    BP := FDebugger.BreakPointList[I];
    AsmView.BreakPoints.Add(Int64(BP.AddrVA), BP.Active);
  end;
  AsmView.Invalidate;
end;

function TCpuViewCore.CanWork: Boolean;
begin
  Result := Assigned(FDebugger) and (FDebugger.DebugState = adsPaused);
end;

function TCpuViewCore.DisasmBuffSize: Integer;
begin
  Result := Max(400, CacheVisibleRows * 8);
end;

procedure TCpuViewCore.DoReset;
begin
  if Assigned(FReset) then
    FReset(Self);
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

procedure TCpuViewCore.OnAsmCacheEnd(Sender: TObject);
begin
  OnAsmScroll(Sender, ssdNone);
end;

procedure TCpuViewCore.OnAsmJmpTo(Sender: TObject; const AJmpAddr: Int64;
  AJmpState: TJmpState; var Handled: Boolean);
begin
  if (AJmpState in [jsJmpPushToStack..jsJmpRedo]) and AddrInAsm(AJmpAddr) then
    ShowDisasmAtAddr(AJmpAddr, False);
end;

procedure TCpuViewCore.OnAsmQueryComment(Sender: TObject; AddrVA: Int64;
  AColumn: TColumnType; var AComment: string);
begin
  case AColumn of
    ctWorkSpace: AComment := FDebugger.Context.RegQueryNamesAtAddr(AddrVA);
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
  if NewCacheIndex >= FCacheList.Count - FAsmView.VisibleRowCount then
  begin
    if NewCacheIndex < FCacheList.Count then
      NewCacheIndex := GenerateCache(FCacheList[NewCacheIndex].AddrVA)
    else
      NewCacheIndex := GenerateCache(FCacheList[FCacheList.Count - FAsmView.VisibleRowCount].AddrVA);
  end;
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
begin
  RefreshBreakPoints;
end;

procedure TCpuViewCore.OnContextChange(Sender: TObject);
begin
  SynhronizeViewersWithContext;
end;

procedure TCpuViewCore.OnDebugerChage(Sender: TObject);
begin
  FUtils.Update;
  RefreshView;
end;

procedure TCpuViewCore.OnDebugerStateChange(Sender: TObject);
begin
  case FDebugger.DebugState of
    adsError:
      if Assigned(FAsmView) then
        FAsmView.NoDataText := FDebugger.ErrorMessage;
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
      if Assigned(FAsmView) then
      begin
        FAsmView.InstructionPoint := 0;
        FAsmView.CurrentIPIsActiveJmp := False;
        FAsmView.Invalidate;
        FAsmView.JumpClear;
      end;
      if Assigned(FStackView) then
      begin
        FStackView.Frames.Clear;
        FStackView.Invalidate;
        FStackView.JumpClear;
      end;
      FDumpViewList.JumpClear;
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
      FAsmJmpStack.Clear;
      UpdateStreamsProcessID;
      DoReset;
    end;
  end;
end;

procedure TCpuViewCore.OnGetHint(Sender: TObject; const Param: THintParam;
  var Hint: string);
var
  AddressType: TAddrType;
  AccessStr, Symbol: string;
begin
  if Param.AddrVA <> 0 then
  begin
    AddressType := QueryAddressType(Param.AddrVA);
    if AddressType = atNone then Exit;
    AccessStr := QueryAccessStr(Param.AddrVA);
    Symbol := QuerySymbolAtAddr(Param.AddrVA);
    Hint := Format('Addr: 0x%x (%s) %s', [Param.AddrVA, AccessStr, Symbol]);
  end;
end;

procedure TCpuViewCore.OnJmpTo(Sender: TObject; const AJmpAddr: Int64;
  AJmpState: TJmpState; var Handled: Boolean);
var
  AddrType: TAddrType;
begin
  case AJmpState of
    jsQueryJump:
    begin
      AddrType := atNone;
      OnQueryAddressType(Sender, AJmpAddr, AddrType);
      case AddrType of
        atExecute: ShowDisasmAtAddr(AJmpAddr);
        atRead: ShowDumpAtAddr(AJmpAddr);
        atStack: ShowStackAtAddr(AJmpAddr);
      end;
      Handled := AddrType <> atNone;
    end;
    jsJmpPushToStack,
    jsJmpUndo,
    jsJmpRedo: ShowDumpAtAddr(AJmpAddr, False);
    jsJmpDone: Handled := False;
  end;
end;

procedure TCpuViewCore.OnQueryAddressType(Sender: TObject; AddrVA: Int64;
  var AddrType: TAddrType);
begin
  AddrType := QueryAddressType(AddrVA);
end;

procedure TCpuViewCore.OnRegQueryComment(Sender: TObject; AddrVA: Int64;
  AColumn: TColumnType; var AComment: string);
begin
  AComment := QuerySymbolAtAddr(AddrVA);
end;

procedure TCpuViewCore.OnRegQueryExternalComment(Sender: TObject;
  const AValue: TRegValue; ARegType: TExternalRegType; var AComment: string);
begin
  AComment := '';
  case ARegType of
    ertLastError: AComment := DBase.GetLastErrorStr(AValue.IntValue);
    ertLastStatus: AComment := DBase.GetLastStatusStr(AValue.DwordValue);
  end;
  if AComment <> '' then
    AComment := '(' + AComment + ')';
end;

function TCpuViewCore.QueryCacheItem(AddrVA: Int64; out AItem: TAddrCacheItem
  ): Boolean;
var
  AddrPtr: Int64;
  AddrPtrItem: TAddrCacheItem;
begin
  AItem := Default(TAddrCacheItem);
  Result := FSessionCache.TryGetValue(AddrVA, AItem);
  if not Result then
  try
    Result := FUtils.QueryRegion(AddrVA, AItem.Region);

    // Blocking of possible recursion
    if UseInDeepDbgInfo then
      FSessionCache.Add(AddrVA, AItem);

    if not Result then Exit;
    if raRead in AItem.Region.Access then
    begin
      AItem.Symbol := Debugger.QuerySymbolAtAddr(AddrVA, qsName);
      if AItem.Symbol = '' then
      begin
        // Lock stack chains
        if AddrInStack(AddrVA) and not UseStackChains then Exit;
        AddrPtr := 0;
        if Debugger.ReadMemory(AddrVA, AddrPtr, Debugger.PointerSize) then
        begin
          // Lock in-deep search
          if UseInDeepDbgInfo then
            QueryCacheItem(AddrPtr, AddrPtrItem)
          else
            AddrPtrItem.Symbol := Debugger.QuerySymbolAtAddr(AddrVA, qsName);

          if AddrPtrItem.Symbol <> '' then
            AItem.Symbol := Format('[0x%x] -> %s', [AddrPtr, AddrPtrItem.Symbol])
          else
            AItem.Symbol := Format('[0x%x]', [AddrPtr]);
        end;
      end;
    end;
  finally
    FSessionCache.AddOrSetValue(AddrVA, AItem);
  end;
end;

procedure TCpuViewCore.RefreshView(Forced: Boolean);
begin
  if (FDebugger = nil) or not FDebugger.IsActive then Exit;
  FLastCtx := FDebugger.Context;
  RefreshAsmView(Forced);
  if Assigned(FRegView) then
    FRegView.Context := FLastCtx;
  if Assigned(FStackView) then
  begin
    FLastStackLimit := FDebugger.ThreadStackLimit;
    FDebugger.FillThreadStackFrames(FLastStackLimit, FLastCtx.StackPoint,
      FLastCtx.StackBase, FStackStream.Stream, FStackView.Frames);
    FStackStream.SetAddrWindow(FLastStackLimit.Limit,
      FLastStackLimit.Base - FLastStackLimit.Limit);
    FStackView.AddressMode := GetAddrMode;
    FStackView.SetDataStream(FStackStream, FLastStackLimit.Limit);
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
    SynhronizeViewersWithContext;
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
  FSessionCache.Clear;
end;

procedure TCpuViewCore.StackViewQueryComment(Sender: TObject; AddrVA: Int64;
  AColumn: TColumnType; var AComment: string);
var
  AStackValue: Int64;
begin
  case AColumn of
    ctWorkSpace: AComment := FDebugger.Context.RegQueryNamesAtAddr(AddrVA);
    ctComment:
    begin
      AStackValue := 0;
      if not CanWork then Exit;
      FStackStream.Stream.Position := AddrVA;
      FStackStream.Stream.ReadBuffer(AStackValue, Debugger.PointerSize);
      AComment := QuerySymbolAtAddr(AStackValue);
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
    FAsmView.OnCacheEnd := OnAsmCacheEnd;
    FAsmView.OnJmpTo := OnAsmJmpTo;
    FAsmView.OnSelectionChange := OnAsmSelectionChange;
    FAsmView.OnQueryComment := OnAsmQueryComment;
    FOldAsmScroll := FAsmView.OnVerticalScroll;
    FAsmView.OnVerticalScroll := OnAsmScroll;
    FAsmView.FitColumnsToBestSize;
    FAsmView.ShortCuts.JmpBack.SecondaryShortCuts.Add(ShortCutToText(VK_SUBTRACT));
    FAsmView.ShortCuts.JmpTo.SecondaryShortCuts.Add(ShortCutToText(VK_ADD));
    RefreshBreakPoints;
    RefreshView;
  end;
end;

procedure TCpuViewCore.SetRegView(const Value: TRegView);
begin
  if RegView <> Value then
  begin
    FRegView := Value;
    if Value = nil then Exit;
    FRegView.OnQueryAddressType := OnQueryAddressType;
    FRegView.OnQueryComment := OnRegQueryComment;
    FRegView.OnQueryExternalHint := OnRegQueryExternalComment;
    RefreshView;
  end;
end;

procedure TCpuViewCore.SetStackView(const Value: TStackView);
begin
  if StackView <> Value then
  begin
    FStackView := Value;
    if Value = nil then Exit;
    Value.FitColumnsToBestSize;
    FStackView.OnHint := OnGetHint;
    FStackView.OnQueryComment := StackViewQueryComment;
    FStackView.OnQueryAddressType := OnQueryAddressType;
    RefreshView;
  end;
end;

procedure TCpuViewCore.SynhronizeViewersWithContext;
begin
  if Assigned(FAsmView) then
  begin
    FAsmView.AddressMode := GetAddrMode;
    FAsmView.InstructionPoint := FDebugger.CurrentInstructionPoint;
    FAsmView.CurrentIPIsActiveJmp := FDebugger.IsActiveJmp;
    FAsmView.Invalidate;
  end;
  if Assigned(FStackView) then
    FStackView.Invalidate;
end;

procedure TCpuViewCore.ShowDisasmAtAddr(AddrVA: Int64; PushToJmpStack: Boolean);
begin
  if Assigned(FAsmView) then
  begin
    if PushToJmpStack then
    begin
      FAsmView.JumpToAddress(AddrVA);
      Exit;
    end;
    try
      FAsmSelStart := 0;
      FAsmSelEnd := 0;
      if not FAsmView.IsAddrVisible(AddrVA) then
        BuildAsmWindow(AddrVA);
    except
      RefreshView;
      raise;
    end;
  end;
end;

procedure TCpuViewCore.ShowDumpAtAddr(AddrVA: Int64; PushToJmpStack: Boolean);
begin
  ShowDumpAtAddr(AddrVA, 0, PushToJmpStack);
end;

procedure TCpuViewCore.ShowDumpAtAddr(AddrVA: Int64; ASelLength: Integer;
  PushToJmpStack: Boolean);
begin
  if FDumpViewList.Count = 0 then Exit;
  if (AddrVA = 0) and CanWork then
    AddrVA := FDebugger.CurrentInstructionPoint;
  FDumpViewList.Update(FDumpViewList.ItemIndex, AddrVA, ASelLength, PushToJmpStack);
end;

procedure TCpuViewCore.ShowStackAtAddr(AddrVA: Int64);
begin
  if Assigned(FStackView) then
    FStackView.JumpToAddress(AddrVA);
end;

procedure TCpuViewCore.UpdateAfterSettingsChange;
begin
  ResetCache;
  RefreshView(True);
end;

function TCpuViewCore.UpdateRegValue(RegID: Integer;
  ANewRegValue: Int64): Boolean;
begin
  Result := False;
  if CanWork then
    Result := FDebugger.UpdateRegValue(RegID, ANewRegValue);
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
