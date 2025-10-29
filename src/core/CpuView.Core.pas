////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Core.pas
//  * Purpose   : CPU-View mode kernel automates most of the utility actions.
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2025.
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

{$I CpuViewCfg.inc}

{ TODO:
  Need ASLR Breakpoints
}

uses
  {$IFDEF FPC}
  LCLType,
  LCLIntf,
  LCLProc,
  {$ELSE}
  Windows,
  Menus,
  {$ENDIF}
  Classes,
  Math,
  SysUtils,
  Generics.Collections,
  FWHexView.Common,
  FWHexView,
  FWHexView.MappedView,
  FWHexView.AsmTokenizer,
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
  CpuView.DBase,
  CpuView.ExtendedHint,
  CpuView.TraceLog;

type
  TDumpViewRec = record
    View: TDumpView;
    Stream: TBufferedROStream;
    LastAddrVA, SelStart, SelEnd: Int64;
    Inited: Boolean;
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

  TCoreState = (csWaitDebugger, csDebuggerInit, csRun, csDisassembling);

  { TCpuViewCore }

  TCpuViewCore = class(TComponent)
  private
    FAddrIndex: TDictionary<Int64, Integer>;
    FAsmJmpStack: TJumpStack;
    FAsmView: TAsmView;
    FAsmSelStart, FAsmSelEnd: Int64;
    FAsmStream: TBufferedROStream;
    FCacheList: TListEx<TInstruction>;
    FCacheListIndex: Integer;
    FCoreState: TCoreState;
    FCtx: TCommonCpuContext;
    FDBase: TCpuViewDBase;
    FDebugger: TAbstractDebugger;
    FDisassemblyStream: TBufferedROStream;
    FDisplayStrings: Boolean;
    FDumpViewList: TDumpViewList;
    FExtendedHintData: TExtendedHintData;
    FExtendedHints: Boolean;
    FFindSymbolsDepth: Integer;
    FForceFindSymbols: Boolean;
    FForceGotoAddress: Boolean;
    FInvalidReg: TRegionData;
    FLastStackLimit: TStackLimit;
    FLockSelChange: Boolean;
    FLastAddrVA: Int64;
    FLastCachedAddrVA: Int64;
    FLastCtx: TCommonCpuContext;
    FMinimumStringLength: Integer;
    FPointerValues: TPointerValues;
    FRegView: TRegView;
    FSessionCache: TDictionary<Int64, TAddrCacheItem>;
    FShowCallFuncName: Boolean;
    FStackView: TStackView;
    FStackSelStart, FStackSelEnd: Int64;
    FStackStream: TBufferedROStream;
    FStringStream: TBufferedROStream;
    FThreadChange: Boolean;
    FOldAsmScroll: TOnVerticalScrollEvent;
    FOldAsmSelect: TNotifyEvent;
    FUtils: TCommonUtils;
    FReset: TNotifyEvent;
    FStateChange: TNotifyEvent;
    procedure BuildTraceLine(ACurrentAddrVA: Int64);
    function CanWork: Boolean;
    function DisasmBuffSize: Integer;
    procedure DoReset;
    procedure DoStateChange(Value: TCoreState);
    function GetAddrMode: TAddressMode;
    procedure OnAsmCacheEnd(Sender: TObject);
    procedure OnAsmJmpTo(Sender: TObject; const AJmpAddr: Int64;
      AJmpState: TJmpState; var {%H-}Handled: Boolean);
    procedure OnAsmQueryComment(Sender: TObject; AddrVA: Int64;
      AColumn: TColumnType; var AComment: string);
    procedure OnAsmScroll(Sender: TObject; AStep: TScrollStepDirection);
    procedure OnAsmSelectionChange(Sender: TObject);
    procedure OnBreakPointsChange(Sender: TObject);
    procedure OnContextChange(Sender: TObject);
    procedure OnDebugerStateChange(Sender: TObject);
    procedure OnGetHint(Sender: TObject; const Param: THintParam;
      var Hint: string);
    procedure OnJmpTo(Sender: TObject; const AJmpAddr: Int64;
      AJmpState: TJmpState; var Handled: Boolean);
    procedure OnRegQueryComment(Sender: TObject; AddrVA: Int64;
      {%H-}AColumn: TColumnType; var AComment: string);
    procedure OnRegQueryExternalComment(Sender: TObject;
      const AValue: TRegValue; ARegType: TExternalRegType; var AComment: string);
    procedure OnThreadChange(Sender: TObject);
    function QueryCacheItem(AddrVA: Int64; out AItem: TAddrCacheItem; CallIndex: Integer = 0): Boolean;
    function QueryDisasmAtAddr(AddrVA: Int64; out AItem: TAddrCacheItem): Boolean;
    function QueryPointerValueAtAddr(AddrVA: Int64; out AItem: TAddrCacheItem): Boolean;
    function QueryStringAtAddr(AddrVA: Int64; var AItem: TAddrCacheItem; out AsPtrValue: Int64): Boolean;
    procedure SetAsmView(const Value: TAsmView);
    procedure SetCtx(AValue: TCommonCpuContext);
    procedure SetDebugger(AValue: TAbstractDebugger);
    procedure SetRegView(const Value: TRegView);
    procedure SetStackView(const Value: TStackView);
    function SynhronizeViewersWithContext: Boolean;
  protected
    function AccessToAddrType(const AItem: TAddrCacheItem): TAddrType;
    procedure BuildAsmWindow(AAddress: Int64);
    function CacheVisibleRows: Integer;
    function GenerateCache(AAddress: Int64): Integer;
    procedure LoadFromCache(AIndex: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddrInAsm(AddrVA: Int64): Boolean;
    function AddrInDump(AddrVA: Int64): Boolean;
    function AddrInStack(AddrVA: Int64): Boolean;
    function QueryAccessStr(AddrVA: Int64): string;
    function QueryAddressType(AddrVA: Int64): TAddrType;
    function QueryModuleName(AddrVA: Int64): string;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean;
    function QuerySymbolAtAddr(AddrVA: Int64; IncludeInDeepSymbol: Boolean = True): string;
    procedure ShowDisasmAtAddr(AddrVA: Int64; PushToJmpStack: Boolean = True);
    procedure ShowDumpAtAddr(AddrVA: Int64; PushToJmpStack: Boolean = True); overload;
    procedure ShowDumpAtAddr(AddrVA: Int64; ASelLength: Integer; PushToJmpStack: Boolean = True); overload;
    procedure ShowStackAtAddr(AddrVA: Int64);
    procedure TraceTilReturn;
    procedure TraceToUserCode;
    procedure UpdateAfterSettingsChange;
    function UpdateRegValue(RegID: Integer; ANewRegValue: TRegValue): Boolean;
  public
    property AsmView: TAsmView read FAsmView write SetAsmView;
    property Context: TCommonCpuContext read FCtx write SetCtx;
    property CoreState: TCoreState read FCoreState;
    property DBase: TCpuViewDBase read FDBase;
    property Debugger: TAbstractDebugger read FDebugger write SetDebugger;
    property DisplayStrings: Boolean read FDisplayStrings write FDisplayStrings;
    property DumpViewList: TDumpViewList read FDumpViewList;
    property ExtendedHints: Boolean read FExtendedHints write FExtendedHints;
    property ExtendedHintPointerValues: TPointerValues read FPointerValues write FPointerValues;
    // Адрес от которого был построен последний кэш.
    // Необходим для правильного выполнения операций Undo/Redo в кэше переходов.
    // -------------------------------------------------------------------------
    // Address on the basis of which the current cache was built.
    // Necessary for proper Undo/Redo operations in the transition cache.
    property LastCachedAddrVA: Int64 read FLastCachedAddrVA;
    property LastStackLimit: TStackLimit read FLastStackLimit;
    property MinimumStringLength: Integer read FMinimumStringLength write FMinimumStringLength;
    property RegView: TRegView read FRegView write SetRegView;
    property StackView: TStackView read FStackView write SetStackView;
    property ShowCallFuncName: Boolean read FShowCallFuncName write FShowCallFuncName;
    property ForceFindSymbols: Boolean read FForceFindSymbols write FForceFindSymbols;
    property ForceFindSymbolsDepth: Integer read FFindSymbolsDepth write FFindSymbolsDepth;
    property Utils: TCommonUtils read FUtils;
    property OnReset: TNotifyEvent read FReset write FReset;
    property OnStateChange: TNotifyEvent read FStateChange write FStateChange;
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
  Data := Default(TDumpViewRec);
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
  begin
    FItems.List[I].SelStart := FItems.List[I].View.SelStart;
    FItems.List[I].SelEnd := FItems.List[I].View.SelEnd;
    FItems.List[I].View.JumpClear;
  end;
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
    Update(I, FItems.List[I].LastAddrVA, -1, False);
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
  if PushToJmpStack and AView.JumpToAddress(AddrVA, ASelLength) then
    Exit;
  if not FCore.QueryRegion(AddrVA, RegData) then Exit;
  AStream := FItems.List[Index].Stream;
  AStream.Stream.OnUpdated := FOnUpdate;
  AStream.SetAddrWindow(RegData.BaseAddr, RegData.RegionSize);
  AView.SetDataStream(AStream, RegData.BaseAddr);
  AView.AddressMode := AddressMode;
  if ASelLength >= 0 then
    AView.FocusOnAddress(AddrVA, ccmSelectPointer)
  else
  begin
    AView.FocusOnAddress(FItems.List[Index].SelStart, ccmReset);
    AView.SelStart := FItems.List[Index].SelStart;
    AView.SelEnd := FItems.List[Index].SelEnd;
  end;
  FItems.List[Index].LastAddrVA := AddrVA;
  if not FItems.List[Index].Inited then
  begin
    FItems.List[Index].Inited := True;
    AView.FitColumnsToBestSize;
  end;
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
    Result := CacheItem.Region.ToString;
end;

function TCpuViewCore.QueryAddressType(AddrVA: Int64): TAddrType;
var
  CacheItem: TAddrCacheItem;
begin
  if QueryCacheItem(AddrVA, CacheItem) then
    Result := AccessToAddrType(CacheItem)
  else
    Result := atNone;
end;

function TCpuViewCore.QueryModuleName(AddrVA: Int64): string;
begin
  FUtils.QueryModuleName(AddrVA, Result);
end;

function TCpuViewCore.QueryPointerValueAtAddr(AddrVA: Int64;
  out AItem: TAddrCacheItem): Boolean;
begin
  AItem := Default(TAddrCacheItem);
  Result := QueryCacheItem(AddrVA, AItem) and (raRead in AItem.Region.Access);
  if not Result then Exit;
  if not AItem.ExtendedDataPresent then
  begin
    Debugger.ReadMemory(AddrVA, AItem.PointerValue[0], SizeOf(AItem.PointerValue));
    AItem.ExtendedDataPresent := True;
    FSessionCache.AddOrSetValue(AddrVA, AItem);
  end;
end;

function TCpuViewCore.QueryStringAtAddr(AddrVA: Int64;
  var AItem: TAddrCacheItem; out AsPtrValue: Int64): Boolean;
var
  pCursor, pStartChar, pMaxPos: PByte;
  Len: Integer;
  Unicode: Boolean;

  procedure NextChar;
  begin
    Len := 0;
    pStartChar := nil;
    Inc(pCursor);
  end;

  function StrEnd: Boolean;
  var
    ABuff: AnsiString;
    UBuff: UnicodeString;
  begin
    Result := False;
    if Len >= MinimumStringLength then
    begin
      if Unicode then
      begin
        if PWord(pCursor)^ <> 0 then
          Exit;
        SetLength(UBuff{%H-}, Len);
        Move(pStartChar^, UBuff[1], Len * 2);
        AItem.Symbol := 'L"' + string(UBuff) + '"';
      end
      else
      begin
        if pCursor^ <> 0 then
          Exit;
        SetLength(ABuff{%H-}, Len);
        Move(pStartChar^, ABuff[1], Len);
        AItem.Symbol := '"' + string(ABuff) + '"';
      end;
      AItem.InDeepSymbol := AItem.Symbol;
      AItem.AddrType := atString;
      Result := True;
    end;
  end;

begin
  Result := False;
  AsPtrValue := 0;
  FStringStream.SetAddrWindow(AddrVA, FStringStream.BufferSize);
  if FStringStream.Read(AsPtrValue, Debugger.PointerSize) <> Debugger.PointerSize then Exit;
  if not DisplayStrings then Exit;
  pCursor := FStringStream.Memory;
  if pCursor = nil then Exit;
  pStartChar := pCursor;
  Len := 0;
  pMaxPos := pCursor + FStringStream.BufferSize;
  while pCursor < pMaxPos do
  begin
    if pCursor^ in [10, 13, 32..126] then
    begin
      if Len = 0 then
      begin
        case PWord(pCursor)^ of
          10, 13, 32..126:
          begin
            Unicode := True;
            pStartChar := pCursor;
            Inc(pCursor, 2);
          end;
        else
          Unicode := False;
          pStartChar := pCursor;
          Inc(pCursor);
        end;
        Len := 1;
      end
      else
      begin
        if Unicode then
        begin
          case PWord(pCursor)^ of
            10, 13, 32..126:
            begin
              Inc(Len);
              Inc(pCursor, 2);
            end;
          else
            Result := StrEnd;
            Break;
          end;
        end
        else
        begin
          Inc(Len);
          Inc(pCursor);
        end;
      end;
    end
    else
    begin
      Result := StrEnd;
      Break;
    end;
  end;
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

function TCpuViewCore.QuerySymbolAtAddr(AddrVA: Int64; IncludeInDeepSymbol: Boolean): string;
var
  CacheItem: TAddrCacheItem;
begin
  if QueryCacheItem(AddrVA, CacheItem) then
  begin
    if IncludeInDeepSymbol then
      Result := CacheItem.InDeepSymbol
    else
      Result := CacheItem.Symbol;
  end
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

constructor TCpuViewCore.Create(AOwner: TComponent);
var
  RemoteStream: TRemoteStream;
begin
  FCacheList := TListEx<TInstruction>.Create;
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
  FSessionCache := TDictionary<Int64, TAddrCacheItem>.Create;
  FShowCallFuncName := True;
  FAsmJmpStack := TJumpStack.Create;
  FDBase := TCpuViewDBase.Create;
  RemoteStream := TRemoteStream.Create(FUtils);
  FStringStream := TBufferedROStream.Create(RemoteStream, soOwned);
  FStringStream.BufferSize := 1024;
  MinimumStringLength := 4;
  DisplayStrings := True;
end;

destructor TCpuViewCore.Destroy;
begin
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
  FDBase.Free;
  FStringStream.Free;
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
    QueryRegion(0, FInvalidReg);
  if AddrVA < FInvalidReg.RegionSize then
    Result := False
  else
    Result := QueryRegion(AddrVA, RegData) and (raRead in RegData.Access);
end;

function TCpuViewCore.GenerateCache(AAddress: Int64): Integer;

  function BuildCacheFromBuff(FromAddr, LastKnownAddrVA: Int64; FromBuff: PByte;
    BufSize: Integer): Integer;
  var
    List: TList<TInstruction>;
    Inst: TInstruction;
  begin
    List := Debugger.Disassembly(FromAddr, LastKnownAddrVA, FromBuff, BufSize,
      Debugger.ShowSourceLines, ShowCallFuncName);
    try
      for Inst in List do
      begin
        if Inst.Len > BufSize then
          Break;
        FAddrIndex.TryAdd(FromAddr, FCacheList.Count);
        FCacheList.Add(Inst);
        Inc(FromAddr, Inst.Len);
        Dec(BufSize, Inst.Len);
      end;
    finally
      List.Free;
    end;

    // Этот код нужен только тогда, когда пользователь указал адрес вручную,
    // в этом случае можно прыгнуть в середину инструкции

    // This code is only needed when the user entered the address manually,
    // in which case you can jump to the middle of the instructions

    if (BufSize > 0) and FForceGotoAddress then
    begin
      Inst.AddrVA := FromAddr;
      Inst.Mnemonic := InvalidAsmLine;
      Inst.Len := BufSize;
      Inst.JmpTo := 0;
      FAddrIndex.TryAdd(FromAddr, FCacheList.Count);
      FCacheList.Add(Inst);
      BufSize := 0;
    end;

    Result := BufSize;
  end;

const
  LastInstructionReserve = 15;
  TopCacheMinLimit = 128;
var
  WindowAddr, LastKnownAddrVA: Int64;
  Count, TopCacheMaxLimit, TopCacheSize, Missed: Integer;
  Buff: array of Byte;
  RegData: TRegionData;
begin
  Result := -1;
  if FDisassemblyStream = nil then Exit;

  DoStateChange(csDisassembling);
  try

    ResetCache;
    if QueryRegion(AAddress, RegData) then
    begin
      WindowAddr := Max(AAddress - TopCacheMinLimit, RegData.AllocationBase);
      WindowAddr := FDebugger.QuerySymbolAtAddr(WindowAddr, qsAddrVA).AddrVA;
    end
    else
      WindowAddr := AAddress - TopCacheMinLimit;
    TopCacheSize := AAddress - WindowAddr;
    TopCacheMaxLimit := Debugger.PreferededDasmBufSize;
    if TopCacheSize > TopCacheMaxLimit then
    begin
      TopCacheSize := TopCacheMaxLimit;
      WindowAddr := AAddress - TopCacheMaxLimit;
    end;
    Missed := 0;

    SetLength(Buff{%H-}, TopCacheSize + DisasmBuffSize);
    FDisassemblyStream.SetAddrWindow(WindowAddr, Length(Buff));
    Count := FDisassemblyStream.Read(Buff[0], Length(Buff));
    if Count > 0 then
    begin
      if TopCacheSize > 0 then
        Missed := BuildCacheFromBuff(WindowAddr, 0, @Buff[0], TopCacheSize);
      if FCacheList.Count > 0 then
      begin
        Result := FCacheList.Count;
        LastKnownAddrVA := FCacheList.Last.AddrVA;
      end
      else
        LastKnownAddrVA := 0;
      Dec(TopCacheSize, Missed);
      Dec(AAddress, Missed);

      // LastInstructionReserve в конце резервирует гарантированные 16 байт
      // в конце стрима для последней инструкции.
      // Необходимо для движка дизассемблера, так как в нем не контролируется
      // размер переданного на дизассемблирование буфера.

      // LastInstructionReserve at the end reserves a guaranteed 16 bytes
      // at the end of the stream for the last instruction.
      // It is necessary for the disassembler engine because it does not control
      // the size of the buffer passed to the disassembly.

      BuildCacheFromBuff(AAddress, LastKnownAddrVA, @Buff[TopCacheSize],
        Count - TopCacheSize - LastInstructionReserve);
    end;

  finally
    DoStateChange(csRun);
  end;
end;

procedure TCpuViewCore.LoadFromCache(AIndex: Integer);
var
  I: Integer;
  Line: TInstruction;
begin
  FAsmView.BeginUpdate;
  try
    FAsmView.DataMap.Clear;
    FCacheListIndex := AIndex;
    if AIndex < 0 then Exit;
    Line := FCacheList.List[AIndex];
    if Line.Len = 0 then
      FAsmView.DataMap.AddComment(Line.AddrVA, Line.Mnemonic)
    else
      if Line.JmpToPosStart > 0 then
        FAsmView.DataMap.AddAsm(Line.AddrVA, Line.Len, Line.Mnemonic, Line.Hint,
          Line.JmpTo, Line.JmpToPosStart, Line.JmpToPosLength)
      else
        FAsmView.DataMap.AddAsm(Line.AddrVA, Line.Len, Line.Mnemonic, Line.Hint, Line.JmpTo, 0, 0);
    for I := 1 to Min(CacheVisibleRows, FCacheList.Count - AIndex - 1) do
    begin
      Line := FCacheList.List[I + AIndex];
      if Line.Len = 0 then
        FAsmView.DataMap.AddComment(Line.Mnemonic)
      else
        if Line.JmpToPosStart > 0 then
          FAsmView.DataMap.AddAsm(Line.Len, Line.Mnemonic, Line.Hint, Line.JmpTo,
            Line.JmpToPosStart, Line.JmpToPosLength)
        else
          FAsmView.DataMap.AddAsm(Line.Len, Line.Mnemonic, Line.Hint, Line.JmpTo, 0, 0);
    end;
  finally
    FAsmView.EndUpdate;
  end;
end;

procedure TCpuViewCore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Context) then
    Context := nil;
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

procedure TCpuViewCore.BuildTraceLine(ACurrentAddrVA: Int64);
var
  ARowIndex: Int64;
  ARowData: TRowColumnData;
begin
  if (FLastAddrVA = ACurrentAddrVA) then Exit;

  if (ACurrentAddrVA = 0) and (FDebugger.DebugState = adsFinished) then
  begin
    TraceLog.Log('debug stop');
    FLastAddrVA := 0;
    Exit;
  end;

  if FDebugger.DebugState <> adsPaused then Exit;

  if FLastAddrVA = 0 then
    TraceLog.Log('debug start');

  ARowIndex := FAsmView.AddressToRowIndex(ACurrentAddrVA);
  ARowData := FAsmView.RowColumnData(ARowIndex);

  FLastAddrVA := ACurrentAddrVA;

  TraceLog.Log(Format('%s | %.16x | %s (%s)',
    [
    FormatDateTime('hh:mm:ss.zzz', Now),
    ACurrentAddrVA,
    ARowData.Columns[ctDescription],
    QuerySymbolAtAddr(ACurrentAddrVA, False)
    ]), False);
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

procedure TCpuViewCore.DoStateChange(Value: TCoreState);
begin
  FCoreState := Value;
  if Assigned(FStateChange) then
    FStateChange(Self);
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
    ssdWheelUp: Dec(NewCacheIndex, FAsmView.WheelRowCount);
    ssdPageUp: Dec(NewCacheIndex, FAsmView.VisibleRowCount);
    ssdLineDown: Inc(NewCacheIndex);
    ssdWheelDown: Inc(NewCacheIndex, FAsmView.WheelRowCount);
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

procedure TCpuViewCore.OnDebugerStateChange(Sender: TObject);
var
  CurrentIP: Int64;
begin
  case FDebugger.DebugState of
    adsError:
      if Assigned(FAsmView) then
        FAsmView.NoDataText := FDebugger.ErrorMessage;
    adsStoped, adsStart:
      UpdateStreamsProcessID;
    adsPaused:
    begin
      CurrentIP := FDebugger.CurrentInstructionPoint;
      if Assigned(FDebugger) and (FThreadChange or (CurrentIP <> FAsmView.InstructionPoint)) then
      begin
        FThreadChange := False;
        FUtils.Update;
        RefreshView;
        BuildTraceLine(CurrentIP);
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
        FStackSelStart := FStackView.SelStart;
        FStackSelEnd := FStackView.SelEnd;
        FStackView.Frames.Clear;
        FStackView.FramesUpdated;
        FStackView.JumpClear;
      end;
      FDumpViewList.JumpClear;
      FSessionCache.Clear;
      DoReset;
    end;
    adsFinished:
    begin
      BuildTraceLine(0);
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

  procedure FillCacheItem(Index: Integer; AddrVA: Int64);
  var
    AItem: TAddrCacheItem;
  begin
    QueryCacheItem(AddrVA, AItem);
    case AccessToAddrType(AItem) of
      atExecute: QueryDisasmAtAddr(AddrVA, AItem);
      atRead, atStack: QueryPointerValueAtAddr(AddrVA, AItem);
    end;
    FExtendedHintData.AddrChain[Index] := AItem;
  end;

var
  ChainAddrVA: Int64;
  AItem: TAddrCacheItem;
  AccessStr, Symbol: string;
  I: Integer;
  P: TPoint;
  DrawMetrics: TDrawMetrics;
begin
  if (Param.AddrVA <> 0) and Assigned(AsmView) and Assigned(RegView) then
  begin

    if not ExtendedHints then
    begin
      AccessStr := QueryAccessStr(Param.AddrVA);
      Symbol := QuerySymbolAtAddr(Param.AddrVA);
      Hint := Format('Addr: 0x%x (%s) %s', [Param.AddrVA, AccessStr, Symbol]);
      Exit;
    end;

    Finalize(FExtendedHintData);
    FExtendedHintData := Default(TExtendedHintData);

    Hint := 'External hint...';
    Param.HintInfo.HintWindowClass := TExtendedHintWindow;
    QueryCacheItem(Param.AddrVA, AItem);
    SetLength(FExtendedHintData.AddrChain, 1 + AItem.InDeepCount);
    FillCacheItem(0, Param.AddrVA);
    if AItem.InDeepCount > 0 then
    begin
      ChainAddrVA := Param.AddrVA;
      for I := 0 to AItem.InDeepCount - 1 do
      begin
        Debugger.ReadMemory(ChainAddrVA, ChainAddrVA, Debugger.PointerSize);
        FillCacheItem(I + 1, ChainAddrVA);
      end;
    end;
    AsmView.FillDrawMetrics(DrawMetrics);
    FExtendedHintData.BorderWidth := DrawMetrics.SplitMargin;
    FExtendedHintData.CharWidth := DrawMetrics.CharWidth;
    FExtendedHintData.ColorMap := AsmView.ColorMap;
    FExtendedHintData.Colors[atExecute] := RegView.ColorMap.AddrExecuteColor;
    FExtendedHintData.Colors[atRead] := RegView.ColorMap.AddrReadColor;
    FExtendedHintData.Colors[atStack] := RegView.ColorMap.AddrStackColor;
    FExtendedHintData.Colors[atString] := RegView.ColorMap.AddrStringColor;
    FExtendedHintData.Font := AsmView.Font;
    FExtendedHintData.RowHeight := DrawMetrics.RowHeight;
    FExtendedHintData.Tokenizer := AsmView.Tokenizer;
    FExtendedHintData.PointerValues := ExtendedHintPointerValues;
    if FExtendedHintData.AddrChain[0].AddrType = atNone then
    begin
      Move(Param.AddrVA, FExtendedHintData.AddrChain[0].PointerValue[0], FDebugger.PointerSize);
      Exclude(FExtendedHintData.PointerValues, bvmHex64);
      if FDebugger.PointerSize = 4 then
      begin
        Exclude(FExtendedHintData.PointerValues, bvmInt64);
        Exclude(FExtendedHintData.PointerValues, bvmUInt64);
        Exclude(FExtendedHintData.PointerValues, bvmFloat64);
      end;
    end;
    Param.HintInfo.HintData := @FExtendedHintData;
    if GetCursorPos(P{%H-}) then
      Param.HintInfo.HintPos.Y := P.Y + GetSystemMetrics(SM_CYCURSOR) shr 1;
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
        atRead, atReadLinked, atString: ShowDumpAtAddr(AJmpAddr);
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
    ertRegID: AComment := DBase.GetRegHintStr(AValue.IntValue);
  end;
end;

procedure TCpuViewCore.OnThreadChange(Sender: TObject);
begin
  FThreadChange := True;
end;

function TCpuViewCore.QueryCacheItem(AddrVA: Int64; out AItem: TAddrCacheItem;
  CallIndex: Integer): Boolean;
var
  AddrPtr: Int64;
  AddrPtrItem: TAddrCacheItem;
begin
  AItem := Default(TAddrCacheItem);
  Result := FSessionCache.TryGetValue(AddrVA, AItem);

  // If the process memory is read through a remote console debugger, then you also need to check for locking.
  // This is not required at this time, and memory reading is implemented independently.
  {$IFNDEF USE_MANUAL_READMEMORY}
  if not Result and Debugger.IsDebuggerLocked then
    Exit;
  {$ENDIF}

  if not Result then
  try
    AItem.AddrVA := AddrVA;
    Result := FUtils.QueryRegion(AddrVA, AItem.Region);

    // Blocking of possible recursion
    if ForceFindSymbols then
      FSessionCache.Add(AddrVA, AItem);

    if not Result then Exit;

    AItem.AddrType := AccessToAddrType(AItem);

    if raRead in AItem.Region.Access then
    begin
      AItem.Symbol := Debugger.QuerySymbolAtAddr(AddrVA, qsName).Description;
      AItem.InDeepSymbol := AItem.Symbol;

      if (raExecute in AItem.Region.Access) and (AItem.Symbol <> '') then
        AItem.IsUserCode := FDebugger.IsUserCode(AddrVA);

      // Lock in-deep search
      if (CallIndex > 0) and not ForceFindSymbols then Exit;
      if CallIndex >= ForceFindSymbolsDepth then Exit;

      // The task of deep search is to find executable code
      if not (raExecute in AItem.Region.Access) then
      begin
        AddrPtr := 0;
        if not QueryStringAtAddr(AddrVA, AItem, AddrPtr) then
        begin
          AddrPtrItem := Default(TAddrCacheItem);
          if ForceFindSymbols then
            QueryCacheItem(AddrPtr, AddrPtrItem, CallIndex + 1)
          else
            AddrPtrItem.InDeepSymbol := Debugger.QuerySymbolAtAddr(AddrVA, qsName).Description;
          if AddrPtrItem.InDeepSymbol <> '' then
          begin
            AItem.InDeepSymbol := Trim(Format('%s [0x%x] -> %s', [AItem.Symbol, AddrPtr, AddrPtrItem.InDeepSymbol]));
            AItem.InDeepCount := AddrPtrItem.InDeepCount + 1;
          end;
        end;
      end;
    end;

  finally
    FSessionCache.AddOrSetValue(AddrVA, AItem);
  end;
end;

function TCpuViewCore.QueryDisasmAtAddr(AddrVA: Int64;
  out AItem: TAddrCacheItem): Boolean;
const
  BuffSize = 150; // 10 instructions of maximum size (15)
var
  Buff: array of Byte;
  List: TList<TInstruction>;
  AsmLine: TInstruction;
  I: Integer;
begin
  AItem := Default(TAddrCacheItem);
  Result := QueryCacheItem(AddrVA, AItem) and (raExecute in AItem.Region.Access);
  if not Result then Exit;
  if not AItem.ExtendedDataPresent then
  begin
    SetLength(Buff{%H-}, BuffSize);
    if Debugger.ReadMemory(AddrVA, Buff[0], BuffSize) then
    begin
      List := Debugger.Disassembly(AddrVA, 0, @Buff[0], BuffSize, False, ShowCallFuncName);
      try
        if List.Count = 0 then Exit(False);
        for I := 0 to Min(9, List.Count - 1) do
        begin
          AsmLine := List[I];
          if I = 0 then
            AItem.FirstAsmLine := AsmLine.Mnemonic
          else
            AItem.AsmLines := AItem.AsmLines + AsmLine.Mnemonic + sLineBreak;
          AItem.HintLines := AItem.HintLines + AsmLine.Hint + sLineBreak;
          AItem.Linked[I] := AsmLine.JmpTo <> 0;
          if Trim(AsmLine.Mnemonic) = Debugger.EndOnProcToken then Break;
        end;
      finally
        List.Free;
      end;
      AItem.ExtendedDataPresent := True;
      FSessionCache.AddOrSetValue(AddrVA, AItem);
    end;
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
    FStackView.FitColumnsToBestSize;
    FStackView.SetDataStream(FStackStream, FLastStackLimit.Limit);
    FStackView.FramesUpdated;
    FStackView.FocusOnAddress(FLastCtx.StackPoint, ccmSelectRow);
    if FStackSelStart <> 0 then
    begin
      FStackView.SelStart := FStackSelStart;
      FStackView.SelEnd := FStackSelEnd;
    end;
  end;
  FDumpViewList.AddressMode := GetAddrMode;
  FDumpViewList.Restore(FDebugger.CurrentInstructionPoint);

  // Data for validated addresses is displayed when it is rendered and is taken from the cache.
  // However, since ProcessMessage is called while waiting for the pipe handle,
  // the rendering event will occur when the debugger is blocked.
  // Therefore, the cache must be updated immediately by forcing a call.
  {$IFNDEF USE_MANUAL_READMEMORY}
  FRegView.Repaint;
  FStackView.Repaint;
  {$ENDIF}

  DoStateChange(FCoreState);
end;

procedure TCpuViewCore.RefreshAsmView(Forced: Boolean);
begin
  if Assigned(FAsmView) then
  begin
    if not SynhronizeViewersWithContext then Exit;
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
  ARowIndex: Int64;
  ARawData: TBytes;
  AStackValue: Int64;
begin
  case AColumn of
    ctWorkSpace: AComment := FDebugger.Context.RegQueryNamesAtAddr(AddrVA);
    ctComment:
    begin
      AStackValue := 0;
      if not CanWork then Exit;
      ARowIndex := FStackView.AddressToRowIndex(AddrVA);
      if FStackView.ReadRowData(ARowIndex, ARawData) then
      begin
        AStackValue := 0;
        Move(ARawData[0], AStackValue, Length(ARawData));
        AComment := QuerySymbolAtAddr(AStackValue);
      end;
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
    FAsmView.OnQueryAddressType := OnQueryAddressType;
    FAsmView.OnQueryComment := OnAsmQueryComment;
    FAsmView.OnHint := OnGetHint;
    FOldAsmScroll := FAsmView.OnVerticalScroll;
    FAsmView.OnVerticalScroll := OnAsmScroll;
    FAsmView.FitColumnsToBestSize;
    FAsmView.ShortCuts.JmpBack.SecondaryShortCuts.Add(ShortCutToText(VK_SUBTRACT));
    FAsmView.ShortCuts.JmpTo.SecondaryShortCuts.Add(ShortCutToText(VK_ADD));
    RefreshBreakPoints;
    RefreshView;
  end;
end;

procedure TCpuViewCore.SetCtx(AValue: TCommonCpuContext);
begin
  if FCtx = AValue then Exit;
  if Assigned(FCtx) then
    FCtx.RemoveFreeNotification(Self);
  FCtx := AValue;
  if Assigned(FCtx) then
    FCtx.FreeNotification(Self);
  if Assigned(FDebugger) then
    FDebugger.Context := Context;
end;

procedure TCpuViewCore.SetDebugger(AValue: TAbstractDebugger);
begin
  if FDebugger = AValue then Exit;
  if FDebugger <> nil then
    FreeAndNil(FDebugger);
  FDebugger := AValue;
  if FDebugger <> nil then
  begin
    FDebugger.Context := Context;
    FDebugger.OnBreakPointsChange := OnBreakPointsChange;
    FDebugger.OnContextChange := OnContextChange;
    FDebugger.OnStateChange := OnDebugerStateChange;
    FDebugger.OnThreadChange := OnThreadChange;
    FAsmStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
    FDisassemblyStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
    FDumpViewList.OnUpdated := FDebugger.UpdateRemoteStream;
    FStackStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
    FStringStream.Stream.OnUpdated := FDebugger.UpdateRemoteStream;
    DoStateChange(csDebuggerInit);
    RefreshBreakPoints;
    DoStateChange(csRun);
  end
  else
  begin
    DoStateChange(csWaitDebugger);
    FAsmStream.Stream.OnUpdated := nil;
    FDisassemblyStream.Stream.OnUpdated := nil;
    FDumpViewList.OnUpdated := nil;
    FStackStream.Stream.OnUpdated := nil;
    FStringStream.Stream.OnUpdated := nil;
  end;
end;

procedure TCpuViewCore.SetRegView(const Value: TRegView);
begin
  if RegView <> Value then
  begin
    FRegView := Value;
    if Value = nil then Exit;
    FRegView.OnHint := OnGetHint;
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
    FStackView.OnHint := OnGetHint;
    FStackView.OnQueryComment := StackViewQueryComment;
    FStackView.OnQueryAddressType := OnQueryAddressType;
    RefreshView;
  end;
end;

function TCpuViewCore.SynhronizeViewersWithContext: Boolean;
var
  CurrentInstructionPoint: Int64;
begin
  CurrentInstructionPoint := FDebugger.CurrentInstructionPoint;
  Result := CurrentInstructionPoint <> 0;
  if not Result then Exit;
  if Assigned(FAsmView) then
  begin
    FAsmView.AddressMode := GetAddrMode;
    FAsmView.Tokenizer.TokenizerMode := FDebugger.GetTokenizerMode;
    FAsmView.InstructionPoint := CurrentInstructionPoint;
    FAsmView.CurrentIPIsActiveJmp := FDebugger.IsActiveJmp;
    FAsmView.Invalidate;
  end;
  if Assigned(FStackView) then
    FStackView.Invalidate;
end;

function TCpuViewCore.AccessToAddrType(const AItem: TAddrCacheItem): TAddrType;
begin
  Result := atNone;
  if AItem.AddrType = atString then
    Exit(atString);
  if (raExecute in AItem.Region.Access) then
    Exit(atExecute);
  if (AItem.AddrVA <= LastStackLimit.Base) and (AItem.AddrVA >= LastStackLimit.Limit) then
    Exit(atStack);
  if (raRead in AItem.Region.Access) then
  begin
    if AItem.InDeepCount > 0 then
      Result := atReadLinked
    else
      Result := atRead;
  end;
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
      begin
        FForceGotoAddress := True;
        BuildAsmWindow(AddrVA);
        FForceGotoAddress := False;
      end;
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

procedure TCpuViewCore.TraceTilReturn;
var
  RetAddrVA: Int64;
begin
  RetAddrVA := FDebugger.GetReturnAddrVA;
  if (RetAddrVA = 0) or FDebugger.IsUserCode(RetAddrVA) then
    FDebugger.TraceTilReturn
  else
    FDebugger.TraceTo(RetAddrVA);
end;

procedure TCpuViewCore.TraceToUserCode;
var
  UserCodeAddrVA: Int64;
  AddrVAList: array of Int64;
  AStack: array of Byte;
  I, APointerSize, Cnt: Integer;
  AItem: TAddrCacheItem;
begin
  UserCodeAddrVA := FDebugger.GetUserCodeAddrVA;
  case UserCodeAddrVA of
    UserCodeAddrVANotFound:
    begin
      SetLength(AStack{%H-}, FLastStackLimit.Base - FLastCtx.StackPoint);
      FDebugger.ReadMemory(FLastCtx.StackPoint, AStack[0], Length(AStack));
      APointerSize := Debugger.PointerSize;
      Cnt := 0;
      I := 0;
      while I < Length(AStack) do
      begin
        if APointerSize = 8 then
          UserCodeAddrVA := PInt64(@AStack[I])^
        else
          UserCodeAddrVA := PInteger(@AStack[I])^;
        Inc(I, APointerSize);
        QueryCacheItem(UserCodeAddrVA, AItem);
        if AItem.IsUserCode then
        begin
          Inc(Cnt);
          SetLength(AddrVAList{%H-}, Cnt + 1);
          AddrVAList[Cnt] := UserCodeAddrVA;
          Inc(Cnt);
        end;
      end;
      if Cnt > 0 then
        FDebugger.TraceToList(AddrVAList);
    end;
    UserCodeAddrVAFound:;
  else
    FDebugger.TraceTo(UserCodeAddrVA);
  end;
end;

procedure TCpuViewCore.UpdateAfterSettingsChange;
begin
  ResetCache;
  RefreshView(True);
end;

function TCpuViewCore.UpdateRegValue(RegID: Integer; ANewRegValue: TRegValue
  ): Boolean;
begin
  Result := False;
  if CanWork then
  begin
    Result := FDebugger.UpdateRegValue(RegID, ANewRegValue);
    if Result then
      TraceLog.Log(Format('Set %s = %s', [
        Context.RegQueryString(RegID, rqstDisplayName),
        Trim(Context.RegQueryString(RegID, rqstValue))]));
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
