////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.CommonDebug.pas
//  * Purpose   : Implementation of a generic debugger class
//  *           : without binding to the final backend.
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

unit CpuView.CommonDebug;

{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}

{$DEFINE DEBUG_LOG}

interface

{$I CpuViewCfg.inc}

uses
  LCLType,
  LCLIntf,
  LazVersion,

  SysUtils,
  Classes,
  Forms,
  Generics.Collections,
  Generics.Defaults,

  // IdeDebugger
  Debugger,
  BaseDebugManager,

  // DebuggerIntf
  DbgIntfDebuggerBase,
  DbgIntfMiscClasses,

  // IDEIntf
  LazIDEIntf,
  IDEWindowIntf,
  SrcEditorIntf,

  // LazDebuggerIntf
  LazDebuggerIntfBaseTypes,
  LazDebuggerIntf,

  // LazUtils
  LazLoggerBase,

  // CodeTools
  CodeCache,
  CodeToolManager,

  // FpDebug
  FpDbgLoader,
  FpDbgDwarfDataClasses,
  FpDbgDwarfFreePascal,
  FpdMemoryTools,
  FpDbgInfo,
  fpDbgSymTableContext,
  fpDbgSymTable,

  FWHexView.Common,
  CpuView.Common,
  CpuView.Stream,
  CpuView.CPUContext,
  CpuView.DebugerGate,
  CpuView.Design.DbgLog;

{$message 'Disable when stable = 4.0'}
{$if laz_major >= 4}
  {$define ExtendedFpDebug}
{$endif}

type

  TLocalSymbol = record
    AddrVA, EndVA: Int64;
    Duplicate: Boolean;
    Name: string;
  end;

  TLocalSymbols = TListEx<TLocalSymbol>;

  { TLocalLibrary }

  TLocalLibrary = class
  private
    FHighAddr: TDBGPtr;
    FLowAddr: TDBGPtr;
    FImageBase: TDBGPtr;
    FPath: string;
    FSymbols: TLocalSymbols;
  public
    constructor Create;
    destructor Destroy; override;
    property ImageBase: TDBGPtr read FImageBase write FImageBase;
    property HighAddr: TDBGPtr read FHighAddr write FHighAddr;
    property LowAddr: TDBGPtr read FLowAddr write FLowAddr;
    property Path: string read FPath write FPath;
    property Symbols: TLocalSymbols read FSymbols;
  end;

  { TDwarfData }

  TDwarfData = class
  private
    FData: TFpDwarfInfo;
    FImageLoaderList: TDbgImageLoaderList;
    FLoader: TDbgImageLoader;
    FUnitsPresent: Boolean;
  public
    constructor Create(const APath: string; AMemModel: TFpDbgMemModel;
      ALoader: TDbgImageLoader);
    destructor Destroy; override;
    function FindProcSymbol(AAddress: TDbgPtr): TFpSymbol;
    function GetLineAddresses(const AFileName: String; ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean;
    property UnitsPresent: Boolean read FUnitsPresent;
  end;

  { TDebugSymbolsStorage }

  TDebugSymbolsStorage = class
  private
    FAutoLoadLibrary: Boolean;
    FExecutableFilePath: string;
    FExecutableImageBase: Int64;
    FDbgInfoList: TObjectList<TDwarfData>;
    FLocalSymbols: TObjectList<TLocalLibrary>;
    FLoadedLibs: TStringList;
    FMemModel: TFpDbgMemModel;
    FUtils: TCommonAbstractUtils;
    procedure AddLocalLib(const APath: string; ALocalLib: TLocalLibrary);
    function InternalFindProcSymbol(AAddress: TDbgPtr; out ASymbol: TFpSymbol): Boolean;
    function IsLocalLibPresent(AAddress: TDbgPtr): Integer;
  public
    constructor Create(AUtils: TCommonAbstractUtils);
    destructor Destroy; override;
    procedure AddDbgInfo(const APath: string; AImageBase: TDbgPtr; ADbgInfo: TFpSymbolInfo); overload;
    procedure AddDbgInfo(const APath: string; AImageBase: TDbgPtr; AImageSize: Integer); overload;
    procedure Clear;
    function Count: Integer;
    function FindProcSymbol(AAddress: TDbgPtr): TFpSymbol;
    function GetLineAddresses(const AFileName: string; ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean;
    function GetRemoteModuleHandle(const ALibraryName: string): TRemoteModule;
    function GetRemoteModules: TList<TRemoteModule>;
    function GetRemoteProcAddress(const AModule: TRemoteModule; const AProcName: string): Int64;
    function GetRemoteProcList(const AModule: TRemoteModule): TList<TRemoteProc>;
    procedure UpdateLibraries(AList: TImageDataList);
    property AutoLoadLibrary: Boolean read FAutoLoadLibrary write FAutoLoadLibrary;
    property ExecutableFilePath: string read FExecutableFilePath write FExecutableFilePath;
    property ExecutableImageBase: Int64 read FExecutableImageBase write FExecutableImageBase;
  end;

  TCheckUserCodeState = (ucFound, ucNotFound, ucNeedWait);

  TUpdateWaitingState = (uwsBreakpoint, uwsCheckUserCode);
  TUpdateWaitingStates = set of TUpdateWaitingState;

  { TCommonDebugGate }

  TCommonDebugGate = class(TAbstractDebugger)
  private
    FBreakPoints: TIDEBreakPoints;
    FBreakpointsNotification: TIDEBreakPointsNotification;
    FCallStackMonitor: TIdeCallStackMonitor;
    FCallStackNotification: TCallStackNotification;
    FCurrentThreadID: Cardinal;
    FDebugger: TDebuggerIntf;
    FDebugStorage: TDebugSymbolsStorage;
    FErrorOnInit: Boolean;
    FInPauseStateProcessing: Boolean;
    FPreviosSrcLine: Integer;
    FPreviosSrcFuncName, FPreviosSrcFileName: string;
    FRegisterInDebugBoss, FRegisterDestroyNotification: Boolean;
    FReturnAddrVA: Int64;
    FSnapshotManager:  TSnapshotManager;
    FSupportStream: TRemoteStream;
    FTemporaryIP: TDictionary<Integer, Int64>;
    FThreadsMonitor: TIdeThreadsMonitor;
    FThreadsNotification: TThreadsNotification;
    FUpdateWaiting: TUpdateWaitingStates;
    FUserCodeAddrVA: Int64;
    procedure OnBreakPointChanged(const {%H-}ASender: TIDEBreakPoints;
      const {%H-}ABreakpoint: TIDEBreakPoint);
    procedure OnCallStackChanged(Sender: TObject);
    procedure OnDebuggerDestroy(Sender: TObject);
    procedure OnState(ADebugger: TDebuggerIntf; AOldState: TDBGState);
    procedure OnThreadsChange(Sender: TObject);
  protected
    procedure BreakPointChanged; virtual;
    function CanShowBreakPoint(AValue: TIDEBreakPoint): Boolean; virtual;
    procedure DoCurrentThreadChange; virtual;
    procedure DoPauseProcessing(AStart: Boolean); virtual;
    function GetSymbolAtAddr(AddrVA: Int64): TFpSymbol; virtual;
    function GetLineAddresses(const AFileName: string;
      ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean; virtual;
    function IsMainThreadId: Boolean; virtual;
    function FormatSymbol(AddrVA: Int64; ASym: TFpSymbol; AParam: TQuerySymbol): string;
    procedure RefreshCallStack(AStack: TIdeCallStack); virtual;
    procedure Reset; virtual;
    procedure UpdateAll;
    function UpdateContext: Boolean; override;
    procedure UpdateDebugger(ADebugger: TDebuggerIntf); virtual;
    procedure UpdateCurrentThreadID;
  protected
    function CheckUserCodeWithIdeCallStack: TCheckUserCodeState;
    function CurrentInstruction: TInstruction;
    procedure InitDebugger;
    property BreakPoints: TIDEBreakPoints read FBreakPoints;
    property CallStackMonitor: TIdeCallStackMonitor read FCallStackMonitor;
    property Debugger: TDebuggerIntf read FDebugger;
    property DebugStorage: TDebugSymbolsStorage read FDebugStorage;
    property ErrorOnInit: Boolean read FErrorOnInit write FErrorOnInit;
    property InPauseStateProcessing: Boolean read FInPauseStateProcessing;
    property SnapshotManager: TSnapshotManager read FSnapshotManager;
    property SupportStream: TRemoteStream read FSupportStream;
    property TemporaryIP: TDictionary<Integer, Int64> read FTemporaryIP;
    property ThreadsMonitor: TIdeThreadsMonitor read FThreadsMonitor;
    property UpdateWaiting: TUpdateWaitingStates read FUpdateWaiting write FUpdateWaiting;
    property UserCodeAddrVA: Int64 read FUserCodeAddrVA write FUserCodeAddrVA;
  public
    constructor Create(AOwner: TComponent; AUtils: TCommonAbstractUtils); override;
    destructor Destroy; override;
    function CommandAvailable(ACommand: TInterfaceDebugCommand): Boolean; override;
    function CurrentInstructionPoint: Int64; override;
    function CurrentThreadID: Cardinal; override;
    function DebugState: TAbstractDebugState; override;
    procedure FillThreadStackFrames(ALimit: TStackLimit;
      AddrStack, AddrFrame: Int64; AStream: TRemoteStream;
      AFrames: TList<TStackFrame>); override;
    function GetReturnAddrVA: Int64; override;
    function GetSourceLine(AddrVA: Int64; out ASourcePath: string;
      out ASourceLine: Integer): Boolean; override;
    function GetUserCodeAddrVA: Int64; override;
    function IsActive: Boolean; override;
    function IsActiveJmp: Boolean; override;
    function IsUserCode(AAddrVA: Int64): Boolean; override;
    procedure Pause; override;
    function QuerySymbolAtAddr(AddrVA: Int64; AParam: TQuerySymbol): TQuerySymbolValue; override;
    function ReadMemory(AddrVA: Int64; var Buff; Size: Integer): Boolean; override;
    procedure Run; override;
    procedure SetNewIP(AddrVA: Int64); override;
    procedure Stop; override;
    function ThreadStackLimit: TStackLimit; override;
    procedure ToggleBreakPoint(AddrVA: Int64); override;
    procedure TraceIn; override;
    procedure TraceOut; override;
    procedure TraceTilReturn; override;
    function UpdateRegValue(RegID: Integer; ANewRegValue: TRegValue): Boolean; override;
    procedure UpdateRemoteModulesData; override;
  end;

  function LocalSymbolComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF}
    A, B: TLocalSymbol): Integer;

implementation

function LocalSymbolComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF}
  A, B: TLocalSymbol): Integer;
begin
  if A.AddrVA < B.AddrVA then
    Result := -1
  else
    if A.AddrVA = B.AddrVA then
      Result := 0
    else
      Result := 1;
end;

// Патч для TArrayHelper<T>.BinarySearch я делал для FPC 3.3.1,
// но необходима поддержка стабильной версии 3.2.2,
// поэтому поиск будет выполнятся самостоятельно.

// I made a patch for TArrayHelper<T>.BinarySearch for FPC 3.3.1,
// but support for stable version 3.2.2 is required,
// so the search will be done by itself.

function SearchLocalSymbol(List: TLocalSymbols; AddrVA: Int64;
  out AFoundIndex: SizeInt): Boolean;
var
  imin, imax, imid: Int32;
  LCompare: SizeInt;
  AItem: TLocalSymbol;
begin
  if List.Count = 0 then
  begin
    AFoundIndex := -1;
    Exit(False);
  end;
  imin := 0;
  imax := List.Count - 1;
  AItem.AddrVA := AddrVA;
  while (imin < imax) do
  begin
    imid := imin + ((imax - imin) shr 1);
    LCompare := LocalSymbolComparer(List.List[imid], AItem);
    if (LCompare < 0) then
      imin := imid + 1
    else
    begin
      imax := imid;
      if LCompare = 0 then
      begin
        AFoundIndex := imid;
        Exit(True);
      end;
    end;
  end;
  AFoundIndex := imin;
  LCompare := LocalSymbolComparer(List.List[imin], AItem);
  Result := (imax = imin) and (LCompare = 0);
  if not Result and (LCompare < 0) then
    Inc(AFoundIndex);
end;

constructor TLocalLibrary.Create;
begin
  FSymbols := TListEx<TLocalSymbol>.Create(
    TComparer<TLocalSymbol>.Construct(LocalSymbolComparer));
end;

destructor TLocalLibrary.Destroy;
begin
  FSymbols.Free;
  inherited Destroy;
end;

{ TDwarfData }

constructor TDwarfData.Create(const APath: string; AMemModel: TFpDbgMemModel;
  ALoader: TDbgImageLoader);
begin
  FImageLoaderList := TDbgImageLoaderList.Create(True);
  FLoader := ALoader;
  FLoader.AddToLoaderList(FImageLoaderList);
  FData := TFpDwarfInfo.Create(FImageLoaderList, nil, AMemModel);
  FUnitsPresent := FData.LoadCompilationUnits > 0;
end;

destructor TDwarfData.Destroy;
begin
  FData.Free;
  FImageLoaderList.Free;
  inherited;
end;

function TDwarfData.FindProcSymbol(AAddress: TDbgPtr): TFpSymbol;
begin
  Result := FData.FindProcSymbol(AAddress);
end;

function TDwarfData.GetLineAddresses(const AFileName: String; ALine: Cardinal;
  var AResultList: TDBGPtrArray): Boolean;
begin
  Result := FData.GetLineAddresses(AFileName, ALine, AResultList);
end;

{ TDebugSymbolsStorage }

procedure TDebugSymbolsStorage.AddLocalLib(const APath: string;
  ALocalLib: TLocalLibrary);
var
  I, L: Integer;
  RegionData: TRegionData;
begin
  L := ALocalLib.Symbols.Count - 1;
  ALocalLib.Symbols.Sort;
  ALocalLib.LowAddr := ALocalLib.Symbols.List[0].AddrVA;
  for I := 1 to L - 1 do
  begin
    ALocalLib.Symbols.List[I].Duplicate :=
      ALocalLib.Symbols.List[I - 1].AddrVA = ALocalLib.Symbols.List[I].AddrVA;
    ALocalLib.Symbols.List[I - 1].EndVA := ALocalLib.Symbols.List[I].AddrVA;
  end;
  if FUtils.QueryRegion(ALocalLib.Symbols.List[L].AddrVA, RegionData) then
  begin
    ALocalLib.Symbols.List[L].EndVA := RegionData.BaseAddr + RegionData.RegionSize;
    ALocalLib.HighAddr := ALocalLib.Symbols.List[L].EndVA;
  end
  else
    ALocalLib.HighAddr := ALocalLib.Symbols.List[L].AddrVA;
  FLocalSymbols.Add(ALocalLib);
end;

function TDebugSymbolsStorage.InternalFindProcSymbol(AAddress: TDbgPtr; out
  ASymbol: TFpSymbol): Boolean;
var
  I: Integer;
  Idx: SizeInt;
  LocalLib: TLocalLibrary;
  LocalSymbol: TLocalSymbol;
  Found: Boolean;
  Pfx: string;
begin
  Result := False;
  ASymbol := nil;
  for I := 0 to FLocalSymbols.Count - 1 do
  begin
    LocalLib := FLocalSymbols[I];
    if (AAddress <> 0) and (AAddress = LocalLib.ImageBase) then
    begin
      ASymbol := TFpSymbolTableProc.Create('Instance of ' + ExtractFileName(LocalLib.Path), AAddress);
      Result := True;
      Break;
    end;
    if (AAddress >= LocalLib.LowAddr) and (AAddress < LocalLib.HighAddr) then
    begin
      Result := True;
      Found := SearchLocalSymbol(LocalLib.Symbols, AAddress, Idx);
      if Found then
        LocalSymbol := LocalLib.Symbols[Idx]
      else
      begin
        if Idx > 0 then Dec(Idx);
        if Idx >= 0 then
        begin
          LocalSymbol := LocalLib.Symbols[Idx];
          Found := (AAddress >= LocalSymbol.AddrVA) and (AAddress < LocalSymbol.EndVA);
        end;
      end;
      if Found then
      begin
        while LocalSymbol.Duplicate do
        begin
          Dec(Idx);
          LocalSymbol := LocalLib.Symbols[Idx];
        end;
        if LocalLib.Path = '' then
          Pfx := ''
        else
          Pfx := ChangeFileExt(ExtractFileName(LocalLib.Path), ':');
        ASymbol := TFpSymbolTableProc.Create(Pfx + LocalSymbol.Name, LocalSymbol.AddrVA);
      end;
      Break;
    end;
  end;
end;

function TDebugSymbolsStorage.IsLocalLibPresent(AAddress: TDbgPtr): Integer;
var
  I: Integer;
begin
  for I := 0 to FLocalSymbols.Count - 1 do
    if FLocalSymbols[I].ImageBase = AAddress then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

constructor TDebugSymbolsStorage.Create(AUtils: TCommonAbstractUtils);
begin
  FDbgInfoList := TObjectList<TDwarfData>.Create;
  FLocalSymbols := TObjectList<TLocalLibrary>.Create;
  FLoadedLibs := TStringList.Create;
  FLoadedLibs.Sorted := True;
  FMemModel := TFpDbgMemModel.Create;
  FUtils := AUtils;
end;

destructor TDebugSymbolsStorage.Destroy;
begin
  FDbgInfoList.Free;
  FLocalSymbols.Free;
  FLoadedLibs.Free;
  FMemModel.Free;
  inherited Destroy;
end;

procedure TDebugSymbolsStorage.AddDbgInfo(const APath: string;
  AImageBase: TDbgPtr; ADbgInfo: TFpSymbolInfo);
{$ifdef ExtendedFpDebug}
var
  Symbol: TFpSymbol;
  LocalLib: TLocalLibrary;
  LocalSymbol: TLocalSymbol;
  I, L: Integer;
begin
  L := ADbgInfo.SymbolCount - 1;
  if L > 0 then
  begin
    LocalLib := TLocalLibrary.Create;
    LocalLib.Path := APath;
    LocalLib.ImageBase := AImageBase;
    LocalSymbol := Default(TLocalSymbol);
    for I := 0 to L do
    begin
      Symbol := ADbgInfo.Symbols[I];
      try
        LocalSymbol.Name := Symbol.Name;
        LocalSymbol.AddrVA := Symbol.Address.Address;
        LocalLib.Symbols.Add(LocalSymbol);
      finally
        Symbol.ReleaseReference;
      end;
    end;
    AddLocalLib(APath, LocalLib);
  end;
end;
{$else}
begin
end;
{$endif}

procedure TDebugSymbolsStorage.AddDbgInfo(const APath: string;
  AImageBase: TDbgPtr; AImageSize: Integer);
var
  Dwarf: TDwarfData;
  Loader: TDbgImageLoader;
  SymbolList: TfpSymbolList;
  Symbol: TFpSymbol;
  LocalLib: TLocalLibrary;
  LocalSymbol: TLocalSymbol;
  LinkedSymbol: PfpLinkerSymbol;
  I, L: Integer;
begin
  if not FileExists(APath) then Exit;
  if FLoadedLibs.IndexOf(APath) >= 0 then Exit;
  FLoadedLibs.Add(APath);
  Loader := TDbgImageLoader.Create(APath);
  if Loader.IsValid then
  begin
    Dwarf := TDwarfData.Create(APath, FMemModel, Loader);
    if Dwarf.UnitsPresent then
      FDbgInfoList.Add(Dwarf)
    else
      Dwarf.Free;
  end
  else
    Loader.Free;

  Loader := TDbgImageLoaderLibrary.Create(APath, nil, AImageBase);
  try
    SymbolList := TfpSymbolList.Create;
    try
      Loader.ParseSymbolTable(SymbolList);
      L := SymbolList.Count - 1;
      LocalLib := TLocalLibrary.Create;
      LocalLib.ImageBase := AImageBase;
      // AImageBase = 0 only for executable files
      if AImageBase <> 0 then
        LocalLib.Path := APath;
      if L > 0 then
      begin
        LocalSymbol := Default(TLocalSymbol);
        for I := 0 to L do
        begin
          LinkedSymbol := SymbolList.DataPtr[I];
          Symbol := TFpSymbolTableProc.Create(LinkedSymbol^.Name, SymbolList.Keys[I]);
          try
            LocalSymbol.Name := Symbol.Name;
            LocalSymbol.AddrVA := Symbol.Address.Address;
          finally
            Symbol.Free;
          end;
          LocalLib.Symbols.Add(LocalSymbol);
        end;
        AddLocalLib(APath, LocalLib);
      end
      else
      begin
        LocalLib.LowAddr := AImageBase;
        LocalLib.HighAddr := AImageBase + AImageSize;
        FLocalSymbols.Add(LocalLib);
      end;
    finally
      SymbolList.Free;
    end;
  finally
    Loader.Free;
  end;
end;

procedure TDebugSymbolsStorage.Clear;
begin
  FDbgInfoList.Clear;
  FLocalSymbols.Clear;
  FLoadedLibs.Clear;
end;

function TDebugSymbolsStorage.Count: Integer;
begin
  Result := FLocalSymbols.Count;
end;

function TDebugSymbolsStorage.FindProcSymbol(AAddress: TDbgPtr): TFpSymbol;
var
  I: Integer;
  LibPath: string;
  RegionData: TRegionData;
begin
  if (ExecutableImageBase <> 0) and (AAddress = ExecutableImageBase) then
  begin
    Result := TFpSymbolTableProc.Create(
      'Instance of ' + ExtractFileName(ExecutableFilePath), AAddress);
    Exit;
  end;
  for I := 0 to FDbgInfoList.Count - 1 do
  begin
    Result := FDbgInfoList[I].FindProcSymbol(AAddress);
    if Result <> nil then Exit;
  end;
  if InternalFindProcSymbol(AAddress, Result) then Exit;
  if AutoLoadLibrary then
  begin
    if FUtils.QueryModuleName(AAddress, LibPath) then
    begin
      FUtils.QueryRegion(AAddress, RegionData);
      AddDbgInfo(LibPath, RegionData.AllocationBase, RegionData.RegionSize);
      InternalFindProcSymbol(AAddress, Result);
    end;
  end;
end;

function TDebugSymbolsStorage.GetLineAddresses(const AFileName: string;
  ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FDbgInfoList.Count - 1 do
  begin
    Result := FDbgInfoList[I].GetLineAddresses(AFileName, ALine, AResultList);
    if Result then Exit;
  end;
end;

function TDebugSymbolsStorage.GetRemoteModuleHandle(const ALibraryName: string
  ): TRemoteModule;
var
  I: TLocalLibrary;
begin
  Result := Default(TRemoteModule);
  for I in FLocalSymbols do
  begin
    if not AnsiSameStr(ExtractFileName(I.Path), ALibraryName) then Continue;
    Result.hInstance := 1; // Means that the information has been found
    Result.ImageBase := Int64(I.ImageBase);
    Result.LibraryPath := I.Path;
  end;
end;

function TDebugSymbolsStorage.GetRemoteModules: TList<TRemoteModule>;
var
  I: TLocalLibrary;
  RemoteModule: TRemoteModule;
begin
  Result := TList<TRemoteModule>.Create;
  for I in FLocalSymbols do
  begin
    if I.ImageBase = 0 then Continue;
    if I.Path = ExecutableFilePath then Continue;
    RemoteModule.hInstance := 1; // Means that the information has been found
    RemoteModule.ImageBase := Int64(I.ImageBase);
    RemoteModule.LibraryPath := I.Path;
    Result.Add(RemoteModule);
  end;
end;

function TDebugSymbolsStorage.GetRemoteProcAddress(
  const AModule: TRemoteModule; const AProcName: string): Int64;
var
  I: TLocalLibrary;
  LocalSymbol: TLocalSymbol;
begin
  Result := 0;
  for I in FLocalSymbols do
  begin
    if I.ImageBase <> AModule.ImageBase then Continue;
    for LocalSymbol in I.Symbols do
      if AnsiSameStr(LocalSymbol.Name, AProcName) then
      begin
        Result := LocalSymbol.AddrVA;
        Exit;
      end;
  end;
end;

function TDebugSymbolsStorage.GetRemoteProcList(const AModule: TRemoteModule
  ): TList<TRemoteProc>;
var
  I: TLocalLibrary;
  LocalSymbol: TLocalSymbol;
  RemoteProc: TRemoteProc;
begin
  Result := TList<TRemoteProc>.Create;
  for I in FLocalSymbols do
  begin
    if I.ImageBase <> AModule.ImageBase then Continue;
    for LocalSymbol in I.Symbols do
    begin
      RemoteProc.AddrVA := LocalSymbol.AddrVA;
      RemoteProc.FuncName := LocalSymbol.Name;
      Result.Add(RemoteProc);
    end;
  end;
end;

procedure TDebugSymbolsStorage.UpdateLibraries(AList: TImageDataList);
var
  I: TImageData;
begin
  for I in AList do
  begin
    if I.ImagePath = ExecutableFilePath then Continue;
    if IsLocalLibPresent(I.ImageBase) < 0 then
      AddDbgInfo(I.ImagePath, TDBGPtr(I.ImageBase), I.Size);
  end;
end;

{ TCommonDebugGate }

function TCommonDebugGate.FormatSymbol(AddrVA: Int64; ASym: TFpSymbol;
  AParam: TQuerySymbol): string;
var
  PasSource: TCodeBuffer;
  Editor: TSourceEditorInterface;
  ASrcLineNumber: Cardinal;
  ASrcFileName: string;
begin
  Result := '';
  if ASym = nil then Exit;

  if (AParam = qsName) or (ASym.Line <= 0) then
  begin
    Result := ASym.Name;
    if Result = '' then Exit;
    if ASym.{%H-}Parent <> nil then
      Result := ASym.{%H-}Parent.Name + '.' + Result;
    if ASym.Line > 0 then
      Result := Format('%s %s:%d', [
        Result, ExtractFileName(ASym.FileName), ASym.Line])
    else
    begin
      if AParam <> qsName then
      begin
        if FPreviosSrcFuncName = Result then
        begin
          Result := '';
          Exit;
        end;
        FPreviosSrcFuncName := Result;
      end;
      if AddrVA <> ASym.Address.Address then
        Result := Format('%s+%d', [Result, AddrVA - ASym.Address.Address]);
    end;
    Exit;
  end;

  ASrcFileName := ASym.FileName;
  ASrcLineNumber := ASym.Line;
  if (FPreviosSrcLine = ASrcLineNumber) and (FPreviosSrcFileName = ASrcFileName) then
    Exit;
  FPreviosSrcLine := ASrcLineNumber;
  FPreviosSrcFileName := ASrcFileName;
  FPreviosSrcFuncName := ASym.Name;

  if DebugBoss.GetFullFilename(ASrcFileName, False) then
  begin
    PasSource := CodeToolBoss.LoadFile(ASrcFileName, True, False);
    if Assigned(PasSource) then
    begin
      Editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(ASrcFileName);
      if Editor <> nil then
        ASrcLineNumber := Editor.DebugToSourceLine(ASrcLineNumber);
      Result := Format('%s:%d %s', [
        ExtractFileName(ASrcFileName), ASrcLineNumber,
        Trim(PasSource.GetLine(ASrcLineNumber - 1, False))]);
    end;
  end;
end;

procedure TCommonDebugGate.RefreshCallStack(AStack: TIdeCallStack);
begin
  // dummee...
end;

procedure TCommonDebugGate.OnBreakPointChanged(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
begin
  BreakPointChanged;
end;

procedure TCommonDebugGate.OnCallStackChanged(Sender: TObject);
begin
  if uwsCheckUserCode in UpdateWaiting then
    if CheckUserCodeWithIdeCallStack <> ucNeedWait then
      UpdateWaiting := UpdateWaiting - [uwsCheckUserCode];
end;

procedure TCommonDebugGate.OnDebuggerDestroy(Sender: TObject);
begin
  Reset;
end;

procedure TCommonDebugGate.OnState(ADebugger: TDebuggerIntf;
  AOldState: TDBGState);
begin
  case ADebugger.State of
    dsNone,
    dsStop,
    dsDestroying:
      Reset;
    dsPause,
    dsInit:
    begin
      DoPauseProcessing(True);
      try
        FTemporaryIP.Clear;
        if FDebugger <> ADebugger then
          UpdateDebugger(ADebugger)
        else
        begin
          UpdateCurrentThreadID;
          UpdateContext;
          if uwsBreakpoint in UpdateWaiting then
            BreakPointChanged;
        end;
      finally
        DoPauseProcessing(False);
      end;
    end;
  end;
  DoStateChange;
end;

procedure TCommonDebugGate.OnThreadsChange(Sender: TObject);
begin
  UpdateCurrentThreadID;
end;

function TCommonDebugGate.CanShowBreakPoint(AValue: TIDEBreakPoint): Boolean;
begin
  Result := True;
end;

procedure TCommonDebugGate.BreakPointChanged;
var
  I, A: Integer;
  BP: TIDEBreakPoint;
  BPList: TDBGPtrArray;
  BBP: TBasicBreakPoint;
  DuplicateController: TDictionary<Int64, Boolean>;
  LineAddressesPresent: Boolean;
begin
  CpuViewDebugLog.Log(ClassName + ': BreakPointChanged start', True);

  if IsDebuggerLocked then
  begin
    UpdateWaiting := UpdateWaiting + [uwsBreakpoint];
    CpuViewDebugLog.Log(ClassName + ': BreakPointChanged not ready yet.', False);
    Exit;
  end;

  BreakPointList.Clear;
  if BreakPoints = nil then Exit;
  DuplicateController := TDictionary<Int64, Boolean>.Create;
  try
    for I := 0 to BreakPoints.Count - 1 do
    begin
      // вот тут идут дубли - надо их контролировать!!!
      // this is where the duplicates come in - we need to control them!!!!
      BP := BreakPoints.Items[I];

      if not CanShowBreakPoint(BP) then
        Continue;

      case BP.Kind of
        bpkAddress:
        begin
          BBP.AddrVA := BP.Address;
          BBP.Active := BP.Enabled;
          if DuplicateController.TryAdd(BBP.AddrVA, BBP.Active) then
            BreakPointList.Add(BBP);
        end;
        bpkSource:
        begin
          BPList := nil;

          // Can raise an exception: "String list does not allow duplicates"
          // Investigate the problem, possibly due to a previously missing pause test
          LineAddressesPresent := GetLineAddresses(BP.Source, BP.Line, BPList);

          if not LineAddressesPresent then
            Continue;
          for A := 0 to Length(BPList) - 1 do
          begin
            BBP.AddrVA := BPList[A];
            BBP.Active := BP.Enabled;
            if DuplicateController.TryAdd(BBP.AddrVA, BBP.Active) then
              BreakPointList.Add(BBP);
          end;
        end
      else
        Continue;
      end;
    end;
  finally
    DuplicateController.Free;
  end;

  UpdateWaiting := UpdateWaiting - [uwsBreakpoint];
  DoBreakPointsChange;

  CpuViewDebugLog.Log(ClassName + ': BreakPointChanged end', False);
end;

procedure TCommonDebugGate.DoCurrentThreadChange;
begin
  if not IsDebuggerLocked then
  begin
    UpdateContext;
    DoThreadChange;
    DoStateChange;
  end;
end;

procedure TCommonDebugGate.DoPauseProcessing(AStart: Boolean);
begin
  FInPauseStateProcessing := AStart;
end;

function TCommonDebugGate.CurrentThreadID: Cardinal;
begin
  Result := inherited;
  // A solution to support GDB/LLDB where thread information may not yet be
  // available when the debugger enters pause mode.
  if Result = 0 then
    Result := FCurrentThreadID;
end;

function TCommonDebugGate.GetSymbolAtAddr(AddrVA: Int64): TFpSymbol;
begin
  if UseDebugInfo then
    Result := DebugStorage.FindProcSymbol(AddrVA)
  else
    Result := nil;
end;

function TCommonDebugGate.GetLineAddresses(const AFileName: string;
  ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean;
begin
  Result := FDebugStorage.GetLineAddresses(AFileName, ALine, AResultList);
end;

function TCommonDebugGate.CommandAvailable(ACommand: TInterfaceDebugCommand
  ): Boolean;
const
  RemapCmd: array [idcRun..idcStepOut] of TDBGCommand = (
    dcRun, dcRunTo, dcPause, dcStepInto, dcStepOver, dcStepOut
  );
var
  AvailableCommands: TDBGCommands;
begin
  Result := False;
  if Debugger = nil then Exit;
  if ACommand = idcBreakPoint then
    Exit(DebugState = adsPaused);
  if ACommand = idcRunToUserCode then
    Exit(GetUserCodeAddrVA <> 0);
  if Debugger.State = dsStop then
    AvailableCommands := Debugger.SupportedCommandsFor(dsStop)
  else
    AvailableCommands := Debugger.Commands;
  Result := RemapCmd[ACommand] in AvailableCommands;
end;

function TCommonDebugGate.CurrentInstructionPoint: Int64;
var
  AEntry: TThreadEntry;
begin
  Result := 0;
  if not IsDebuggerLocked then
  begin
    if not TemporaryIP.TryGetValue(CurrentThreadID, Result) then
    begin
      if IsMainThreadId then
        Result := Debugger.GetLocation.Address
      else
        if Assigned(ThreadsMonitor) then
        begin
          AEntry := ThreadsMonitor.Threads.EntryById[CurrentThreadID];
          if Assigned(AEntry) then
            Result := AEntry.TopFrame.Address;
          if Result = 0 then
            Result := Debugger.GetLocation.Address;
        end;
    end;
  end;
end;

function TCommonDebugGate.IsActive: Boolean;
begin
  Result := not (DebugState in [adsStoped, adsFinished]);
end;

function TCommonDebugGate.IsActiveJmp: Boolean;
var
  Inst: TInstruction;
begin
  Result := False;
  Inst := CurrentInstruction;
  if Inst.JmpTo = 0 then Exit;
  Result := Context.IsActiveJump(Inst.Mnemonic);
end;

function TCommonDebugGate.IsUserCode(AAddrVA: Int64): Boolean;
var
  Sym: TFpSymbol;
begin
  Sym := GetSymbolAtAddr(AAddrVA);
  Result := Assigned(Sym) and (Sym.FileName <> '');
end;

procedure TCommonDebugGate.Pause;
begin
  Debugger.Pause;
end;

function TCommonDebugGate.QuerySymbolAtAddr(AddrVA: Int64; AParam: TQuerySymbol
  ): TQuerySymbolValue;
var
  ASymbol: TFpSymbol;
begin
  ASymbol := GetSymbolAtAddr(AddrVA);
  if ASymbol = nil then
  begin
    Result.AddrVA := AddrVA;
    Result.Description := '';
  end
  else
  begin
    Result.AddrVA := ASymbol.Address.Address;
    if AParam = qsAddrVA then
      Result.Description := ''
    else
      Result.Description := FormatSymbol(AddrVA, ASymbol, AParam);
    ASymbol.ReleaseReference;
  end;
end;

function TCommonDebugGate.ReadMemory(AddrVA: Int64; var Buff; Size: Integer
  ): Boolean;
begin
  SupportStream.Position := AddrVA;
  Result := SupportStream.Read(Buff, Size) = Size;
end;

procedure TCommonDebugGate.Run;
begin
  Debugger.Run;
end;

procedure TCommonDebugGate.SetNewIP(AddrVA: Int64);
var
  RegIP: TRegValue;
begin
  RegIP := Default(TRegValue);
  RegIP.QwordValue := AddrVA;
  UpdateRegValue(Context.InstructonPointID, RegIP);
end;

procedure TCommonDebugGate.Stop;
begin
  Debugger.Stop;
end;

function TCommonDebugGate.ThreadStackLimit: TStackLimit;
begin
  Result := Utils.GetThreadStackLimit(CurrentThreadID, PointerSize = 4);
end;

procedure TCommonDebugGate.ToggleBreakPoint(AddrVA: Int64);
var
  Sym: TFpSymbol;
  Bp: TIDEBreakPoint;
  AFileName: string;
  I, ALine: Integer;
  BPList: TDBGPtrArray;
  AddrIsSourceLine: Boolean;
begin
  CpuViewDebugLog.Log(Format(ClassName + ': ToggleBreakPoint(0x%x)', [AddrVA]), True);
  AFileName := '';
  ALine := -1;
  Bp := nil;
  AddrIsSourceLine := False;
  Sym := GetSymbolAtAddr(AddrVA);
  if Assigned(Sym) then
  try
    AFileName := Sym.FileName;
    ALine := Sym.Line;
  finally
    Sym.ReleaseReference;
  end;
  if ALine > 0 then
  begin
    if GetLineAddresses(AFileName, ALine, BPList{%H-}) then
    begin
      for I := 0 to Length(BPList) - 1 do
        if BPList[I] = AddrVA then
        begin
          AddrIsSourceLine := True;
          Bp := BreakPoints.Find(AFileName, ALine);
          Break;
        end;
    end
  end;
  if Bp = nil then
    Bp := BreakPoints.Find(AddrVA);
  if Assigned(Bp) then
  begin
    Bp.ReleaseReference;
    CpuViewDebugLog.Log(ClassName + ': ToggleBreakPoint end', False);
    Exit;
  end;
  DebugBoss.LockCommandProcessing;
  try
    if AddrIsSourceLine then
      DebugBoss.DoCreateBreakPoint(AFileName, ALine, True, Bp)
    else
      DebugBoss.DoCreateBreakPoint(AddrVA, True, Bp);
  finally
    DebugBoss.UnLockCommandProcessing;
  end;

  CpuViewDebugLog.Log(ClassName + ': ToggleBreakPoint end', False);
end;

procedure TCommonDebugGate.TraceIn;
begin
  Debugger.StepIntoInstr;
end;

procedure TCommonDebugGate.TraceOut;
begin
  Debugger.StepOverInstr;
end;

procedure TCommonDebugGate.TraceTilReturn;
begin
  Debugger.StepOut;
end;

function TCommonDebugGate.UpdateRegValue(RegID: Integer; ANewRegValue: TRegValue
  ): Boolean;
begin
  if IsDebuggerLocked then Exit(False);
  Context.ThreadID := CurrentThreadID;
  Context.BeginUpdate;
  try
    Result := Context.RegSetValue(RegID, ANewRegValue);
    if Result and (RegID = Context.InstructonPointID) then
      TemporaryIP.AddOrSetValue(Context.ThreadID, ANewRegValue.QwordValue);
  finally
    Context.EndUpdate;
  end;
end;

procedure TCommonDebugGate.UpdateRemoteModulesData;
var
  AList: TImageDataList;
begin
  AList := Utils.QueryLoadedImages(PointerSize = 4);
  try
    DebugStorage.UpdateLibraries(AList);
  finally
    AList.Free;
  end;
end;

function TCommonDebugGate.IsMainThreadId: Boolean;
begin
  Result := True;
end;

procedure TCommonDebugGate.Reset;
begin
  FTemporaryIP.Clear;
  if Assigned(FDebugger) then
  begin
    if FRegisterDestroyNotification then
    begin
      FDebugger.RemoveNotifyEvent(dnrDestroy, OnDebuggerDestroy);
      FRegisterDestroyNotification := False;
    end;
    FDebugger := nil;
  end;

  Utils.ProcessID := 0;
  FCurrentThreadID := 0;

  if Assigned(FBreakPoints) then
  begin
    FBreakPoints.RemoveNotification(FBreakpointsNotification);
    FBreakPoints := nil;
  end;
  if Assigned(FThreadsMonitor) then
  begin
    FThreadsMonitor.RemoveNotification(FThreadsNotification);
    FThreadsMonitor := nil;
  end;
  if Assigned(FCallStackMonitor) then
  begin
    FCallStackMonitor.RemoveNotification(FCallStackNotification);
    FCallStackMonitor := nil;
  end;
  FSnapshotManager := nil;
  FDebugStorage.Clear;
end;

procedure TCommonDebugGate.UpdateAll;
begin
  UpdateContext;
  DoThreadChange;
  if uwsBreakpoint in UpdateWaiting then
    BreakPointChanged;
  DoStateChange;
end;

function TCommonDebugGate.UpdateContext: Boolean;
begin
  if IsDebuggerLocked then
    Result := False
  else
    Result := inherited;
end;

procedure TCommonDebugGate.UpdateDebugger(ADebugger: TDebuggerIntf);
begin
  if Assigned(FDebugger) then Reset;
  FDebugger := ADebugger;
  if Assigned(FDebugger) then
  begin
    FDebugger.AddNotifyEvent(dnrDestroy, OnDebuggerDestroy);
    FRegisterDestroyNotification := True;
  end;
end;

procedure TCommonDebugGate.UpdateCurrentThreadID;
var
  ACurrentThreadID: TThreadID;
begin
  ACurrentThreadID := GetThreadTargetID;
  if (ACurrentThreadID = 0) or (ACurrentThreadID = FCurrentThreadID) then Exit;
  FCurrentThreadID := ACurrentThreadID;
  DoCurrentThreadChange;
end;

function TCommonDebugGate.CheckUserCodeWithIdeCallStack: TCheckUserCodeState;
var
  Snap: TSnapshot;
  IdeStack: TIdeCallStack;
  Entry: TIdeCallStackEntry;
  I: Integer;
begin
  Result := ucNotFound;
  FReturnAddrVA := 0;
  if CallStackMonitor = nil then Exit;
  if Assigned(SnapshotManager) then
    Snap := SnapshotManager.SelectedEntry
  else
    Snap := nil;
  if Snap = nil then
    IdeStack := CallStackMonitor.CurrentCallStackList.EntriesForThreads[GetThreadID]
  else
    IdeStack := CallStackMonitor.Snapshots[Snap].EntriesForThreads[GetThreadID];
  if IdeStack <> nil then
  begin
    RefreshCallStack(IdeStack);
    if IdeStack.CountValidity <> ddsValid then Exit(ucNeedWait);
    IdeStack.PrepareRange(0, IdeStack.Count);
    for I := 0 to IdeStack.Count - 1 do
    begin
      Entry := IdeStack.Entries[I];
      if (Entry = nil) or (Entry.Validity <> ddsValid) then
        Exit(ucNeedWait);
      if I = 1 then
        FReturnAddrVA := Entry.Address;
      if IsUserCode(Entry.Address) then
      begin
        FUserCodeAddrVA := Entry.Address;
        Result := ucFound;
        Break;
      end;
    end;
  end;
end;

function TCommonDebugGate.CurrentInstruction: TInstruction;
var
  CurrentIP: Int64;
  ALen: Integer;
  InstructionOpcode: array of Byte;
  List: TListEx<TInstruction>;
begin
  Result := Default(TInstruction);
  CurrentIP := CurrentInstructionPoint;
  SetLength(InstructionOpcode{%H-}, 16);
  SupportStream.Position := CurrentIP;
  ALen := SupportStream.Read(InstructionOpcode[0], 16);
  if ALen = 0 then Exit;
  List := Disassembly(CurrentIP, 0, @InstructionOpcode[0], 16, False, False);
  try
    Result := List[0];
  finally
    List.Free;
  end;
end;

procedure TCommonDebugGate.InitDebugger;
var
  {$IFDEF LINUX}
  ImageDataList: TImageDataList;
  I: Integer;
  {$ELSE}
  Loader: TDbgImageLoader;
  {$ENDIF}
begin
  if Assigned(DebugBoss) then
  begin
    if Assigned(DebugBoss.BreakPoints) then
    begin
      FBreakPoints := DebugBoss.BreakPoints;
      FBreakPoints.AddNotification(FBreakpointsNotification);
      BreakPointChanged;
    end;
    if Assigned(DebugBoss.Threads) then
    begin
      FThreadsMonitor := DebugBoss.Threads;
      FThreadsMonitor.AddNotification(FThreadsNotification);
    end;
    if Assigned(DebugBoss.Snapshots) then
      FSnapshotManager := DebugBoss.Snapshots;
    if Assigned(DebugBoss.CallStack) then
    begin
      FCallStackMonitor := DebugBoss.CallStack;
      FCallStackMonitor.AddNotification(FCallStackNotification);
    end;
  end;
  Utils.ProcessID := ProcessID;
  Utils.ThreadID := CurrentThreadID;
  if FileExists(DebugStorage.ExecutableFilePath) then
  begin
    {$IFDEF LINUX}
    ImageDataList := Utils.QueryLoadedImages(PointerSize = 4);
    try
      for I := 0 to ImageDataList.Count - 1 do
        if ImageDataList.List[I].ImagePath = DebugStorage.ExecutableFilePath then
        begin
          DebugStorage.ExecutableImageBase := ImageDataList.List[I].ImageBase;
          Break;
        end;
    finally
      ImageDataList.Free;
    end;
    {$ELSE}
    Loader := TDbgImageLoader.Create(DebugStorage.ExecutableFilePath);
    try
      DebugStorage.ExecutableImageBase := Int64(Loader.ImageBase);
    finally
      Loader.Free;
    end;
    {$ENDIF}
  end;
  if Debugger.State = dsPause then
    UpdateContext;
end;

constructor TCommonDebugGate.Create(AOwner: TComponent;
  AUtils: TCommonAbstractUtils);
begin
  inherited Create(AOwner, AUtils);
  FTemporaryIP := TDictionary<Integer, Int64>.Create;
  FSupportStream := TRemoteStream.Create(Utils);
  FSupportStream.OnUpdated := UpdateRemoteStream;
  FBreakpointsNotification := TIDEBreakPointsNotification.Create;
  FBreakpointsNotification.AddReference;
  FBreakpointsNotification.OnAdd := OnBreakPointChanged;
  FBreakpointsNotification.OnUpdate := OnBreakPointChanged;
  FBreakpointsNotification.OnRemove := OnBreakPointChanged;
  FThreadsNotification := TThreadsNotification.Create;
  FThreadsNotification.AddReference;
  FThreadsNotification.OnChange := OnThreadsChange;
  FCallStackNotification := TCallStackNotification.Create;
  FCallStackNotification.AddReference;
  FCallStackNotification.OnChange := OnCallStackChanged;
  FDebugStorage := TDebugSymbolsStorage.Create(AUtils);
  if Assigned(DebugBoss) then
  begin
    DebugBoss.RegisterStateChangeHandler(OnState);
    FRegisterInDebugBoss := True;
    UpdateDebugger(DebugBoss.Snapshots.Debugger);
  end;
end;

destructor TCommonDebugGate.Destroy;
begin
  if FRegisterInDebugBoss and Assigned(DebugBoss) then
    DebugBoss.UnregisterStateChangeHandler(OnState);
  Reset;
  FSupportStream.Free;
  FBreakpointsNotification.OnAdd := nil;
  FBreakpointsNotification.OnRemove := nil;
  FBreakpointsNotification.OnUpdate := nil;
  ReleaseRefAndNil(FBreakpointsNotification);
  FThreadsNotification.OnChange := nil;
  ReleaseRefAndNil(FThreadsNotification);
  FCallStackNotification.OnChange := nil;
  ReleaseRefAndNil(FCallStackNotification);
  FTemporaryIP.Free;
  FDebugStorage.Free;
  inherited Destroy;
end;

function TCommonDebugGate.DebugState: TAbstractDebugState;
begin
  if Assigned(Debugger) then
  begin
    case Debugger.State of
      dsInit: Result := adsStart;
      dsPause: Result := adsPaused;
      dsRun: Result := adsRunning;
      dsInternalPause: Result := adsRunning;
      dsStop: Result := adsStoped;
    else
      Result := adsFinished;
    end;
  end
  else
    if FErrorOnInit then
      Result := adsError
    else
      Result := adsFinished;
end;

procedure TCommonDebugGate.FillThreadStackFrames(ALimit: TStackLimit;
  AddrStack, AddrFrame: Int64; AStream: TRemoteStream; AFrames: TList<
  TStackFrame>);
var
  I: Integer;
  CallStackCheck: TCheckUserCodeState;
  FrameRetAddrVA: Int64;
begin
  inherited;
  UserCodeAddrVA := UserCodeAddrVAFound;
  UpdateWaiting := UpdateWaiting - [uwsCheckUserCode];
  if IsUserCode(CurrentInstructionPoint) then Exit;

  if IsDebuggerLocked then Exit;

  // First we check IdeCallStack
  CallStackCheck := CheckUserCodeWithIdeCallStack;
  if CallStackCheck = ucNeedWait then
    UpdateWaiting := UpdateWaiting + [uwsCheckUserCode];
  if CallStackCheck = ucFound then Exit;

  // If no suitable address is found, we look for a suitable one in the stack frames

  FrameRetAddrVA := 0;
  for I := 0 to AFrames.Count - 1 do
  begin
    ReadMemory(AFrames[I].AddrPC, FrameRetAddrVA, PointerSize);
    if IsUserCode(FrameRetAddrVA) then
    begin
      UserCodeAddrVA := FrameRetAddrVA;
      Exit;
    end;
  end;

  UserCodeAddrVA := UserCodeAddrVANotFound;
end;

function TCommonDebugGate.GetReturnAddrVA: Int64;
begin
  Result := FReturnAddrVA;
end;

function TCommonDebugGate.GetSourceLine(AddrVA: Int64; out ASourcePath: string;
  out ASourceLine: Integer): Boolean;
var
  Sym: TFpSymbol;
  PasSource: TCodeBuffer;
  Editor: TSourceEditorInterface;
begin
  Result := False;
  ASourcePath := '';
  ASourceLine := 0;
  Sym := GetSymbolAtAddr(AddrVA);
  if Sym = nil then Exit;
  try
    ASourcePath := Sym.FileName;
    ASourceLine := Sym.Line;
  finally
    Sym.ReleaseReference;
  end;
  if ASourceLine <= 0 then Exit;
  if DebugBoss.GetFullFilename(ASourcePath, False) then
  begin
    PasSource := CodeToolBoss.LoadFile(ASourcePath, True, False);
    if Assigned(PasSource) then
    begin
      Editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(ASourcePath);
      if Editor <> nil then
        ASourceLine := Editor.DebugToSourceLine(ASourceLine);
      Result := ASourceLine > 0;
    end;
  end;
end;

function TCommonDebugGate.GetUserCodeAddrVA: Int64;
begin
  if DebugState = adsPaused then
    Result := FUserCodeAddrVA
  else
    Result := 0;
end;

end.
