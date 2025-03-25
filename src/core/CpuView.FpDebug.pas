////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.FpDebug.pas
//  * Purpose   : Implementation of gateway to Lazarus FpDebug debugger.
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

unit CpuView.FpDebug;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$DEFINE DEBUG_LOG}

interface

{$I CpuViewCfg.inc}

uses
  LCLType,
  LCLIntf,
  Maps,

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

  // LazDebuggerFp
  FpDebugDebugger,
  FpDebugDebuggerBase,
  FpDebugDebuggerWorkThreads,
  FpDebugDebuggerUtils,

  // fpdebug
  FPDbgController,
  FpDbgClasses,
  FpDbgInfo,
  FpDbgUtil,
  fpDbgSymTableContext,

  // LazUtils
  LazLoggerBase,

  // CodeTools
  CodeCache,
  CodeToolManager,

  {$IFDEF MSWINDOWS}
  CpuView.Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  CpuView.Linux,
  {$ENDIF}

  FWHexView.Common,
  CpuView.Common,
  CpuView.Stream,
  CpuView.CPUContext,
  CpuView.DebugerGate,
  CpuView.Design.DbgLog;

type

  TCpuViewDebugGate = class;

  { TThreadWorkerMaskBreakpoints }

  TThreadWorkerMaskBreakpoints = class(TFpDbgDebggerThreadWorkerItem)
  private
    FBuff: PByte;
    FAddrVA: Int64;
    FSize: Int64;
  protected
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase; pBuff: PByte;
      AAddrVA: Int64; ASize: Int64);
  end;

  { TThreadWorkerChangeThreadContext }

  TThreadWorkerChangeThreadContext = class(TFpDbgDebggerThreadWorkerItem)
  private
    FDbgIntf: TCpuViewDebugGate;
  protected
    procedure DoExecute; override;
  public
    constructor Create(ADbgIntf: TCpuViewDebugGate);
  end;

  TUpdateWaitingState = (uwsBreakpoint, uwsContext);
  TUpdateWaitingStates = set of TUpdateWaitingState;

  TCheckUserCodeState = (ucFound, ucNotFound, ucNeedWait);

  TLocalSymbol = record
    AddrVA, EndVA: Int64;
    Name: string;
  end;

  TLocalSymbols = TListEx<TLocalSymbol>;

  { TLocalLibrary }

  TLocalLibrary = class
  private
    FHighAddr: TDBGPtr;
    FLowAddr: TDBGPtr;
    FName: string;
    FSymbols: TLocalSymbols;
  public
    constructor Create;
    destructor Destroy; override;
    property HighAddr: TDBGPtr read FHighAddr write FHighAddr;
    property LowAddr: TDBGPtr read FLowAddr write FLowAddr;
    property Name: string read FName write FName;
    property Symbols: TLocalSymbols read FSymbols;
  end;

  { TCpuViewDebugGate }

  TCpuViewDebugGate = class(TAbstractDebugger)
  private
    FBreakPoints: TIDEBreakPoints;
    FBreakpointsNotification: TIDEBreakPointsNotification;
    FCallStackMonitor: TIdeCallStackMonitor;
    FCallStackNotification: TCallStackNotification;
    FDebugger: TDebuggerIntf;
    FDbgController: TDbgController;
    FErrorOnInit: Boolean;
    FProcess: TDbgProcess;
    FRegisterInDebugBoss, FRegisterDestroyNotification: Boolean;
    FSupportStream: TRemoteStream;
    FPreviosSrcLine: Integer;
    FPreviosSrcFuncName, FPreviosSrcFileName: string;
    FLocalSymbols: TObjectList<TLocalLibrary>;
    FLockTimeOut: Int64;
    FSnapshotManager:  TSnapshotManager;
    FTemporaryIP: TDictionary<Integer, Int64>;
    FThreadsMonitor: TIdeThreadsMonitor;
    FThreadsNotification: TThreadsNotification;
    FUpdateWaiting: TUpdateWaitingStates;
    FUserCodeAddrVA: Int64;
    FWaitCheckUserCode: Boolean;
    procedure BreakPointChanged;
    function CheckCanWork: Boolean;
    function CheckUserCodeWithIdeCallStack: TCheckUserCodeState;
    function CurrentInstruction: TInstruction;
    function FormatSymbol(AddrVA: Int64; ASym: TFpSymbol; AParam: TQuerySymbol): string;
    function GetSymbolAtAddr(AddrVA: Int64): TFpSymbol;
    procedure DoDebuggerDestroy(Sender: TObject);
    function FormatAsmCode(const Value: string; var AnInfo: TDbgInstInfo;
        CodeSize: Integer): string;
    function IsMainThreadId: Boolean;
    procedure OnBreakPointChanged(const {%H-}ASender: TIDEBreakPoints;
      const {%H-}ABreakpoint: TIDEBreakPoint);
    procedure OnCallStackChanged(Sender: TObject);
    procedure OnCallStackCtxChanged(Sender: TObject);
    procedure OnState(ADebugger: TDebuggerIntf; AOldState: TDBGState);
    procedure Reset;
    procedure StopAllWorkers;
    procedure UpdateDebugger(ADebugger: TDebuggerIntf);
    procedure UpdateLocalSymbols;
  protected
    procedure UpdateContext; override;
  public
    constructor Create(AOwner: TComponent; AUtils: TCommonAbstractUtils); override;
    destructor Destroy; override;
    function CommandAvailable(ACommand: TInterfaceDebugCommand): Boolean; override;
    function CurrentInstructionPoint: Int64; override;
    function DebugState: TAbstractDebugState; override;
    function Disassembly(AddrVA: Int64; pBuff: PByte; nSize: Integer;
      AShowSourceLines: Boolean): TListEx<TInstruction>; override;
    function IsActive: Boolean; override;
    function IsActiveJmp: Boolean; override;
    function IsUserCode(AAddrVA: Int64): Boolean; override;
    procedure FillThreadStackFrames(ALimit: TStackLimit;
      AddrStack, AddrFrame: Int64; AStream: TRemoteStream;
      AFrames: TList<TStackFrame>); override;
    function GetRemoteModuleHandle(const ALibraryName: string): TRemoteModule; override;
    function GetRemoteModules: TList<TRemoteModule>; override;
    function GetRemoteProcAddress(ALibHandle: TLibHandle; const AProcName: string): Int64; override;
    function GetSourceLine(AddrVA: Int64; out ASourcePath: string;
      out ASourceLine: Integer): Boolean; override;
    function GetUserCodeAddrVA: Int64; override;
    procedure Pause; override;
    function PointerSize: Integer; override;
    function ProcessID: Cardinal; override;
    function QuerySymbolAtAddr(AddrVA: Int64; AParam: TQuerySymbol): TQuerySymbolValue; override;
    function ReadMemory(AddrVA: Int64; var Buff; Size: Integer): Boolean; override;
    procedure Run; override;
    procedure Stop; override;
    procedure SetNewIP(AddrVA: Int64); override;
    procedure ToggleBreakPoint(AddrVA: Int64); override;
    function ThreadStackLimit: TStackLimit; override;
    function ThreadID: Cardinal; override;
    procedure TraceIn; override;
    procedure TraceOut; override;
    procedure TraceTilReturn; override;
    procedure TraceTo(AddrVA: Int64); override;
    procedure TraceToList(AddrVA: array of Int64); override;
    function UpdateRegValue(RegID: Integer; ANewRegValue: Int64): Boolean; override;
    procedure UpdateRemoteStream(pBuff: PByte; AAddrVA: Int64; ASize: Int64); override;
    property Debugger: TDebuggerIntf read FDebugger;
    property DbgController: TDbgController read FDbgController;
  end;

implementation

type
  TDbgProcessAccess = class(TDbgProcess);
  TFpDebugDebuggerAccess = class(TFpDebugDebugger);

{ TThreadWorkerMaskBreakpoints }

procedure TThreadWorkerMaskBreakpoints.DoExecute;
begin
  if Assigned(FDebugger.DbgController) then
    TDbgProcessAccess(FDebugger.DbgController.CurrentProcess).MaskBreakpointsInReadData(FAddrVA, FSize, FBuff^);
end;

constructor TThreadWorkerMaskBreakpoints.Create(ADebugger: TFpDebugDebuggerBase;
  pBuff: PByte; AAddrVA: Int64; ASize: Int64);
begin
  inherited Create(ADebugger, twpContinue);
  FAddrVA := AAddrVA;
  FBuff := pBuff;
  FSize := ASize;
end;

{ TThreadWorkerChangeThreadContext }

procedure TThreadWorkerChangeThreadContext.DoExecute;
var
  DebugThread: TDbgThread;
begin
  FDbgIntf.DbgController.CurrentProcess.GetThread(FDbgIntf.ThreadID, DebugThread);
  if DebugThread <> nil then
    DebugThread.BeforeContinue;
end;

constructor TThreadWorkerChangeThreadContext.Create(ADbgIntf: TCpuViewDebugGate);
begin
  inherited Create((ADbgIntf.Debugger as TFpDebugDebugger), twpContinue);
  FDbgIntf := ADbgIntf;
end;

{ TLocalLibrary }

function LocalSymbolComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} A, B: TLocalSymbol): Integer;
begin
  if A.AddrVA < B.AddrVA then
    Result := -1
  else
    if A.AddrVA = B.AddrVA then
      Result := 0
    else
      Result := 1;
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

{ TCpuViewDebugGate }

function TCpuViewDebugGate.CheckCanWork: Boolean;
begin
  Result := False;
  if FDbgController = nil then Exit;
  if FDebugger = nil then Exit;
  if FDebugger.State <> dsPause then Exit;
  Result := True;
end;

function TCpuViewDebugGate.CheckUserCodeWithIdeCallStack: TCheckUserCodeState;
var
  Snap: TSnapshot;
  IdeStack: TIdeCallStack;
  Entry: TIdeCallStackEntry;
  I: Integer;
begin
  Result := ucNotFound;
  if FCallStackMonitor = nil then Exit;
  if Assigned(FSnapshotManager) then
    Snap := FSnapshotManager.SelectedEntry
  else
    Snap := nil;
  if Snap = nil then
    IdeStack := FCallStackMonitor.CurrentCallStackList.EntriesForThreads[ThreadID]
  else
    IdeStack := FCallStackMonitor.Snapshots[Snap].EntriesForThreads[ThreadID];
  if IdeStack <> nil then
  begin
    IdeStack.CountLimited(50);
    if IdeStack.CountValidity <> ddsValid then Exit(ucNeedWait);
    IdeStack.PrepareRange(0, IdeStack.Count);
    for I := 0 to IdeStack.Count - 1 do
    begin
      Entry := IdeStack.Entries[I];
      if (Entry = nil) or (Entry.Validity <> ddsValid) then
        Exit(ucNeedWait);
      if IsUserCode(Entry.Address) then
      begin
        FUserCodeAddrVA := Entry.Address;
        Result := ucFound;
        Break;
      end;
    end;
  end;
end;

procedure TCpuViewDebugGate.BreakPointChanged;
var
  I, A: Integer;
  BP: TIDEBreakPoint;
  BPList: TDBGPtrArray;
  BBP: TBasicBreakPoint;
  DuplicateController: TDictionary<Int64, Boolean>;
  LineAddressesPresent: Boolean;
begin
  CpuViewDebugLog.Log('DebugGate: BreakPointChanged start', True);

  if not CheckCanWork then
  begin
    Include(FUpdateWaiting, uwsBreakpoint);
    CpuViewDebugLog.Log('DebugGate: BreakPointChanged not ready yet.', False);
    Exit;
  end;

  BreakPointList.Clear;
  if FBreakPoints = nil then Exit;
  if FProcess = nil then Exit;
  DuplicateController := TDictionary<Int64, Boolean>.Create;
  try
    for I := 0 to FBreakPoints.Count - 1 do
    begin
      // вот тут идут дубли - надо их контролировать!!!
      // this is where the duplicates come in - we need to control them!!!!
      BP := FBreakPoints.Items[I];
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
          LineAddressesPresent := FProcess.GetLineAddresses(BP.Source, BP.Line, BPList);

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

  Exclude(FUpdateWaiting, uwsBreakpoint);
  DoBreakPointsChange;

  CpuViewDebugLog.Log('DebugGate: BreakPointChanged end', False);
end;

function TCpuViewDebugGate.CurrentInstruction: TInstruction;
var
  CurrentIP: Int64;
  ALen, I: Integer;
  InstructionOpcode: array of Byte;
  List: TListEx<TInstruction>;
begin
  Result := Default(TInstruction);
  CurrentIP := CurrentInstructionPoint;
  SetLength(InstructionOpcode, 16);
  FSupportStream.Position := CurrentIP;
  ALen := FSupportStream.Read(InstructionOpcode[0], 16);
  if ALen = 0 then Exit;
  List := Disassembly(CurrentIP, @InstructionOpcode[0], 16, False);
  try
    Result := List[0];
  finally
    List.Free;
  end;
end;

function TCpuViewDebugGate.FormatSymbol(AddrVA: Int64; ASym: TFpSymbol; AParam: TQuerySymbol
  ): string;
var
  PasSource: TCodeBuffer;
  Editor: TSourceEditorInterface;
  ASrcLineNumber: Cardinal;
  ASrcFileName: string;
begin
  Result := '';
  if ASym = nil then Exit;
  try

    if (AParam = qsName) or (ASym.Line <= 0) then
    begin
      Result := ASym.Name;
      if Result = '' then Exit;
      if ASym.Parent <> nil then
        Result := ASym.Parent.Name + '.' + Result;
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
  finally
    ASym.ReleaseReference;
  end;
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

function TCpuViewDebugGate.GetSymbolAtAddr(AddrVA: Int64): TFpSymbol;
var
  I: Integer;
  Idx: SizeInt;
  LocalSymbol: TLocalSymbol;
  Found: Boolean;
begin
  if UseDebugInfo and Assigned(FDbgController) then
  begin
    Result := nil;
    if UseCacheFoExternalSymbols then
    begin
      for I := 0 to FLocalSymbols.Count - 1 do
        if (AddrVA >= FLocalSymbols[I].LowAddr) and (AddrVA < FLocalSymbols[I].HighAddr) then
        begin
          Found := SearchLocalSymbol(FLocalSymbols[I].Symbols, AddrVA, Idx);
          if Found then
            LocalSymbol := FLocalSymbols[I].Symbols[Idx]
          else
          begin
            if Idx > 0 then Dec(Idx);
            if Idx >= 0 then
            begin
              LocalSymbol := FLocalSymbols[I].Symbols[Idx];
              Found := (AddrVA >= LocalSymbol.AddrVA) and (AddrVA < LocalSymbol.EndVA);
            end;
          end;
          if Found then
            Result := TFpSymbolTableProc.Create(LocalSymbol.Name, LocalSymbol.AddrVA);
        end;
    end;
    if Result = nil then
      Result := FDbgController.CurrentProcess.FindProcSymbol(AddrVA);
  end
  else
    Result := nil;
end;

procedure TCpuViewDebugGate.DoDebuggerDestroy(Sender: TObject);
begin
  CpuViewDebugLog.Log('DebugGate: DoDebuggerDestroy start', True);

  Reset;

  CpuViewDebugLog.Log('DebugGate: DoDebuggerDestroy end', False);
end;

function TCpuViewDebugGate.FormatAsmCode(const Value: string;
  var AnInfo: TDbgInstInfo; CodeSize: Integer): string;
var
  I, Len, Cursor: Integer;
  NumberMode, SignRip, RipMode: Boolean;

  function IsNumber: Boolean;
  begin
    Result := CharInSet(Value[I], ['0'..'9', 'a'..'f', 'A'..'F']);
  end;

  procedure AppendChar(var AResult: string);
  begin
    // upcase
    if CharInSet(Value[I], ['a'..'z']) then
      AResult[Cursor] := Char(Byte(Value[I]) - Byte('a') + Byte('A'))
    else
      AResult[Cursor] := Value[I];
    Inc(Cursor);
    Inc(I);
  end;

  procedure MoveNumber(var AResult: string);
  var
    Digit: Byte;
  begin
    if not IsNumber then
    begin
      AResult[Cursor] := '0';
      Inc(Cursor);
      Exit;
    end;
    while IsNumber do
    begin
      if RipMode then
      begin
        Digit := Byte(Value[I]);
        if Byte(Digit - Byte('0')) <= 9 then
          Dec(Digit, Byte('0'))
        else if Byte(Digit - Byte('a')) < 6 then
          Dec(Digit, Byte('a') - 10)
        else if Byte(Digit - Byte('A')) < 6 then
          Dec(Digit, Byte('A') - 10)
        else
          Digit := 0; // impossible situation, just for debugging
        AnInfo.InstrTargetOffs := (AnInfo.InstrTargetOffs shl 4) or Digit;
      end;
      AppendChar(AResult);
    end;
    if RipMode then
    begin
      if SignRip then
        AnInfo.InstrTargetOffs := -AnInfo.InstrTargetOffs;
      Inc(AnInfo.InstrTargetOffs, CodeSize);
    end;
  end;


begin
  Len := Length(Value);
  SetLength(Result, Len shl 1);
  I := 1;
  Cursor := 1;
  NumberMode := False;
  while I <= Len do
  begin

    // trim lead zeros
    if NumberMode then
    begin
      if Value[I] = '0' then
      begin
        Inc(I);
        Continue;
      end;
      NumberMode := False;
      MoveNumber(Result);
      Continue;
    end;

    // detect number start
    if Value[I] = '$' then
    begin
      // detect RIP
      if (AnInfo.InstrTargetOffs = 0) and (Cursor > 5) then
      begin
        RipMode :=
          (Result[Cursor - 4] = 'R') and
          (Result[Cursor - 3] = 'I') and
          (Result[Cursor - 2] = 'P');
        SignRip := Result[Cursor - 1] = '-';
      end
      else
        RipMode := False;
      Result[Cursor] := '0';
      Inc(Cursor);
      Result[Cursor] := 'x';
      Inc(Cursor);
      Inc(I);
      NumberMode := True;
      Continue;
    end;

    AppendChar(Result);
  end;

  if NumberMode then
    MoveNumber(Result);

  SetLength(Result, Cursor - 1);
end;

function TCpuViewDebugGate.IsMainThreadId: Boolean;
begin
  if Assigned(FDbgController) then
    Result := ThreadID = FDbgController.CurrentProcess.ThreadID
  else
    Result := True;
end;

function TCpuViewDebugGate.IsUserCode(AAddrVA: Int64): Boolean;
var
  Sym: TFpSymbol;
begin
  Result := False;
  if FDbgController = nil then Exit;
  Sym := FDbgController.CurrentProcess.FindProcSymbol(AAddrVA);
  if Sym <> nil then
    Result := Sym.FileName <> '';
end;

procedure TCpuViewDebugGate.OnBreakPointChanged(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
begin
  BreakPointChanged;
end;

procedure TCpuViewDebugGate.OnCallStackChanged(Sender: TObject);
begin
  if FWaitCheckUserCode then
    FWaitCheckUserCode := CheckUserCodeWithIdeCallStack = ucNeedWait;
end;

procedure TCpuViewDebugGate.OnCallStackCtxChanged(Sender: TObject);
begin
  CpuViewDebugLog.Log('DebugGate: OnCallStackCtxChanged start', True);

  if not CheckCanWork then
  begin
    Include(FUpdateWaiting, uwsContext);
    CpuViewDebugLog.Log('DebugGate: OnCallStackCtxChanged not ready yet.', False);
    Exit;
  end;

  UpdateContext;
  DoThreadChange;
  DoStateChange;

  CpuViewDebugLog.Log('DebugGate: OnCallStackCtxChanged end', False);
end;

procedure TCpuViewDebugGate.OnState(ADebugger: TDebuggerIntf;
  AOldState: TDBGState);
begin
  CpuViewDebugLog.Log(Format('DebugGate: OnState(%s, %s)', [dbgs(ADebugger.State), dbgs(AOldState)]), True);

  case ADebugger.State of
    dsNone,
    dsStop,
    dsError,
    dsDestroying:
      Reset;
    dsIdle, dsRun, dsInternalPause:;
    dsPause,
    dsInit:
    begin
      FTemporaryIP.Clear;
      if FDebugger <> ADebugger then
        UpdateDebugger(ADebugger)
      else
        UpdateContext;
      if uwsBreakpoint in FUpdateWaiting then
        BreakPointChanged;
    end;
  end;
  DoStateChange;

  CpuViewDebugLog.Log('DebugGate: OnState end', False);
end;

procedure TCpuViewDebugGate.Reset;
begin
  CpuViewDebugLog.Log('DebugGate: Reset start', True);

  FTemporaryIP.Clear;
  if Assigned(FDebugger) then
  begin
    if FRegisterDestroyNotification then
    begin
      FDebugger.RemoveNotifyEvent(dnrDestroy, DoDebuggerDestroy);
      FRegisterDestroyNotification := False;
    end;
    FDebugger := nil;
  end;
  {$message 'Bad approach, utilitarianism should stand alone'}
  {$IFDEF LINUX}
  LinuxDebugger := nil;
  {$ENDIF}
  FDbgController := nil;
  FProcess := nil;
  Utils.ProcessID := 0;
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
  FLocalSymbols.Clear;

  CpuViewDebugLog.Log('DebugGate: Reset end', False);
end;

procedure TCpuViewDebugGate.StopAllWorkers;
begin
  CpuViewDebugLog.Log('DebugGate: StopAllWorkers start', True);

  if Assigned(FDebugger) then
    TFpDebugDebuggerAccess(FDebugger).StopAllWorkers(True);

  CpuViewDebugLog.Log('DebugGate: StopAllWorkers end', False);
end;

procedure TCpuViewDebugGate.UpdateContext;
begin
  CpuViewDebugLog.Log('DebugGate: UpdateContext start', True);

  if not CheckCanWork then
  begin
    Include(FUpdateWaiting, uwsContext);
    CpuViewDebugLog.Log('DebugGate: UpdateContext not ready yet.', False);
    Exit;
  end;
  inherited UpdateContext;
  UpdateLocalSymbols;
  Exclude(FUpdateWaiting, uwsContext);

  CpuViewDebugLog.Log('DebugGate: UpdateContext end', False);
end;

procedure TCpuViewDebugGate.UpdateDebugger(ADebugger: TDebuggerIntf);
begin
  CpuViewDebugLog.Log('DebugGate: UpdateDebugger start', True);

  FDebugger := ADebugger;
  if Assigned(FDebugger) then
  begin
    if FDebugger is TFpDebugDebugger then
    begin
      FErrorOnInit := False;
      DoError('');
      {$IFDEF LINUX}
      LinuxDebugger := TFpDebugDebugger(FDebugger);
      {$ENDIF}
      FDbgController := TFpDebugDebugger(FDebugger).DbgController;
      FProcess := FDbgController.CurrentProcess;
      if Assigned(FProcess) and not FProcess.GotExitProcess then
      begin
        FDebugger.AddNotifyEvent(dnrDestroy, DoDebuggerDestroy);
        FRegisterDestroyNotification := True;
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
        if ADebugger.State = dsPause then
          UpdateContext;
        CpuViewDebugLog.Log('DebugGate: UpdateDebugger end', False);
        Exit;
      end;
    end
    else
    begin
      FErrorOnInit := True;
      DoError('Unsupported debugger: "' + FDebugger.ClassName + '". Requires FpDebug.');
    end;
  end;

  Reset;

  CpuViewDebugLog.Log('DebugGate: UpdateDebugger end', False);
end;

procedure TCpuViewDebugGate.UpdateLocalSymbols;
var
  LibraryMap: TLibraryMap;
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
  Symbol: TFpSymbol;
  LocalLib: TLocalLibrary;
  LocalSymbol: TLocalSymbol;
  I, L: Integer;
  RegionData: TRegionData;
begin
  if FDbgController = nil then Exit;
  LibraryMap := FDbgController.CurrentProcess.LibMap;
  if LibraryMap = nil then Exit;
  if LibraryMap.Count = FLocalSymbols.Count then Exit;
  FLocalSymbols.Clear;
  Iterator := TMapIterator.Create(LibraryMap);
  try
    while not Iterator.EOM do
    begin
      Iterator.GetData(Lib);
      LocalLib := TLocalLibrary.Create;
      LocalLib.Name := Lib.Name;
      LocalSymbol := Default(TLocalSymbol);
      FLocalSymbols.Add(LocalLib);
      L := Lib.SymbolTableInfo.SymbolCount - 1;
      if L > 0 then
      begin
        for I := 0 to L do
        begin
          Symbol := Lib.SymbolTableInfo.Symbols[I];
          LocalSymbol.Name := Symbol.Name;
          LocalSymbol.AddrVA := Symbol.Address.Address;
          LocalLib.Symbols.Add(LocalSymbol);
        end;
        LocalLib.Symbols.Sort;
        LocalLib.LowAddr := LocalLib.Symbols.List[0].AddrVA;
        for I := 1 to L do
          LocalLib.Symbols.List[I - 1].EndVA := LocalLib.Symbols.List[I].AddrVA;
        if Utils.QueryRegion(LocalLib.Symbols.List[L].AddrVA, RegionData) then
          LocalLib.Symbols.List[L].EndVA := RegionData.BaseAddr + RegionData.RegionSize;
        LocalLib.HighAddr := LocalLib.Symbols.List[L].EndVA;
      end;
      Iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

procedure TCpuViewDebugGate.FillThreadStackFrames(ALimit: TStackLimit;
  AddrStack, AddrFrame: Int64; AStream: TRemoteStream; AFrames: TList<
  TStackFrame>);
var
  I: Integer;
  CallStackCheck: TCheckUserCodeState;
  FrameRetAddrVA: Int64;
begin
  inherited;
  FUserCodeAddrVA := UserCodeAddrVAFound;
  FWaitCheckUserCode := False;
  if FDbgController = nil then Exit;
  if IsUserCode(CurrentInstructionPoint) then Exit;

  // First we check IdeCallStack
  CallStackCheck := CheckUserCodeWithIdeCallStack;
  FWaitCheckUserCode := CallStackCheck = ucNeedWait;
  if CallStackCheck = ucFound then Exit;;

  // If no suitable address is found, we look for a suitable one in the stack frames

  FrameRetAddrVA := 0;
  for I := 0 to AFrames.Count - 1 do
  begin
    ReadMemory(AFrames[I].AddrPC, FrameRetAddrVA, PointerSize);
    if IsUserCode(FrameRetAddrVA) then
    begin
      FUserCodeAddrVA := FrameRetAddrVA;
      Exit;
    end;
  end;

  FUserCodeAddrVA := UserCodeAddrVANotFound;
end;

function TCpuViewDebugGate.GetRemoteModuleHandle(const ALibraryName: string
  ): TRemoteModule;
var
  LibraryMap: TLibraryMap;
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
  AName: string;
begin
  Result := Default(TRemoteModule);
  if FDbgController = nil then Exit;
  LibraryMap := FDbgController.CurrentProcess.LibMap;
  if LibraryMap = nil then Exit;
  Iterator := TMapIterator.Create(LibraryMap);
  try
    while not Iterator.EOM do
    begin
      Iterator.GetData(Lib);
      AName := ExtractFileName(Lib.Name);
      {$IFDEF LINUX}
      // ignore the library version in the extension
      if AName.StartsWith(ALibraryName, False) then
      {$ELSE}
      if SameText(AName, ALibraryName) then
      {$ENDIF}
      begin
        Result.hInstance := Lib.ModuleHandle;
        Result.ImageBase := Lib.LoaderList.ImageBase + Lib.LoaderList.RelocationOffset;
        Result.LibraryPath := Lib.Name;
        Break;
      end;
      Iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

function TCpuViewDebugGate.GetRemoteModules: TList<TRemoteModule>;
var
  LibraryMap: TLibraryMap;
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
  AName: string;
  Item: TRemoteModule;
begin
  Result := TList<TRemoteModule>.Create;
  if FDbgController = nil then Exit;
  LibraryMap := FDbgController.CurrentProcess.LibMap;
  if LibraryMap = nil then Exit;
  Iterator := TMapIterator.Create(LibraryMap);
  try
    while not Iterator.EOM do
    begin
      Iterator.GetData(Lib);
      Item.hInstance := Lib.ModuleHandle;
      Item.ImageBase := Lib.LoaderList.ImageBase + Lib.LoaderList.RelocationOffset;
      Item.LibraryPath := Lib.Name;
      Result.Add(Item);
      Iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

function TCpuViewDebugGate.GetRemoteProcAddress(ALibHandle: TLibHandle;
  const AProcName: string): Int64;
var
  LibraryMap: TLibraryMap;
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
  Sym: TFpSymbol;
begin
  Result := 0;
  if FDbgController = nil then Exit;
  LibraryMap := FDbgController.CurrentProcess.LibMap;
  if LibraryMap = nil then Exit;
  Iterator := TMapIterator.Create(LibraryMap);
  try
    while not Iterator.EOM do
    begin
      Iterator.GetData(Lib);
      if Lib.ModuleHandle = ALibHandle then
      begin
        Sym := nil;
        if Lib.DbgInfo <> nil then
          Sym := Lib.DbgInfo.FindProcSymbol(AProcName);
        if (Sym = nil) and (Lib.SymbolTableInfo <> nil) then
          Sym := Lib.SymbolTableInfo.FindProcSymbol(AProcName);
        if Sym <> nil then
          Result := Sym.Address.Address;
        Break;
      end;
      Iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

constructor TCpuViewDebugGate.Create(AOwner: TComponent;
  AUtils: TCommonAbstractUtils);
begin
  inherited;
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
  FThreadsNotification.OnChange := OnCallStackCtxChanged;
  FCallStackNotification := TCallStackNotification.Create;
  FCallStackNotification.AddReference;
  FCallStackNotification.OnChange := OnCallStackChanged;
  FLocalSymbols := TObjectList<TLocalLibrary>.Create;
  if Assigned(DebugBoss) then
  begin
    DebugBoss.RegisterStateChangeHandler(OnState);
    FRegisterInDebugBoss := True;
    UpdateDebugger(DebugBoss.Snapshots.Debugger);
  end;
end;

destructor TCpuViewDebugGate.Destroy;
begin
  if FRegisterInDebugBoss and Assigned(DebugBoss) then
    DebugBoss.UnregisterStateChangeHandler(OnState);
  Reset;
  FLocalSymbols.Free;
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
  inherited Destroy;
end;

function TCpuViewDebugGate.CommandAvailable(ACommand: TInterfaceDebugCommand
  ): Boolean;
const
  RemapCmd: array [idcRun..idcStepOut] of TDBGCommand = (
    dcRun, dcRunTo, dcPause, dcStepInto, dcStepOver, dcStepOut
  );
var
  AvailableCommands: TDBGCommands;
begin
  Result := False;
  if FDebugger = nil then Exit;
  if ACommand = idcBreakPoint then
    Exit(DebugState = adsPaused);
  if ACommand = idcRunToUserCode then
    Exit(GetUserCodeAddrVA <> 0);
  if FDebugger.State = dsStop then
    AvailableCommands := FDebugger.SupportedCommandsFor(dsStop)
  else
    AvailableCommands := FDebugger.Commands;
  Result := RemapCmd[ACommand] in AvailableCommands;
end;

function TCpuViewDebugGate.CurrentInstructionPoint: Int64;
var
  AEntry: TThreadEntry;
begin
  Result := 0;
  if CheckCanWork then
  begin
    if not FTemporaryIP.TryGetValue(ThreadID, Result) then
    begin
      if IsMainThreadId then
        Result := FDebugger.GetLocation.Address
      else
        if Assigned(FThreadsMonitor) then
        begin
          AEntry := FThreadsMonitor.Threads.EntryById[ThreadID];
          if Assigned(AEntry) then
            Result := AEntry.TopFrame.Address;
          if Result = 0 then
            Result := FDebugger.GetLocation.Address;
        end;
    end;
  end;
end;

function TCpuViewDebugGate.DebugState: TAbstractDebugState;
begin
  if Assigned(FDebugger) then
  begin
    case FDebugger.State of
      dsInit: Result := adsStart;
      dsPause: Result := adsPaused;
      dsRun: Result := adsRunning;
      {$message 'for debug'}
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

function TCpuViewDebugGate.Disassembly(AddrVA: Int64; pBuff: PByte;
  nSize: Integer; AShowSourceLines: Boolean): TListEx<TInstruction>;
var
  Disasm: TDbgAsmDecoder;
  Process: TDbgProcess;
  Instruction: TInstruction;
  PrevVA: Int64;
  ACodeBytes, ACode, RipSimbol: string;
  AnInfo: TDbgInstInfo;
  ExternalAddr, RipAddr: Int64;
  SpaceIndex: Integer;
  HasMem: Boolean;
  PrevSymAddrVA, MissCount: TDBGPtr;
  Sym: TFpSymbol;
begin
  CpuViewDebugLog.Log(Format('DebugGate: Disassembly(AddrVA: 0x%x, nSize: %d)', [AddrVA, nSize]), True);

  Result := TListEx<TInstruction>.Create;
  if FDbgController = nil then Exit;
  Process := FDbgController.CurrentProcess;
  if Process = nil then Exit;
  Disasm := Process.Disassembler;
  if Disasm = nil then Exit;
  PrevSymAddrVA := 0;
  while nSize > 0 do
  begin
    Sym := GetSymbolAtAddr(AddrVA);
    if Sym <> nil then
    begin
      if PrevSymAddrVA = 0 then
        PrevSymAddrVA := Sym.Address.Address
      else
      begin
        if PrevSymAddrVA <> Sym.Address.Address then
        begin
          PrevSymAddrVA := Sym.Address.Address;
          if (AddrVA > PrevSymAddrVA) and (Result.Count > 0) then
          begin
            MissCount := AddrVA - PrevSymAddrVA;
            Instruction := Result.Last;
            if Instruction.Len > MissCount then
            begin
              Result.Delete(Result.Count - 1);
              Dec(Instruction.Len, MissCount);
              Instruction.AsString := '???';
              Instruction.JmpTo := 0;
              Instruction.Hint := '';
              Result.Add(Instruction);
              Dec(pBuff, MissCount);
              Dec(AddrVA, MissCount);
              Inc(nSize, MissCount);
            end;
          end;
        end;
      end;
    end;

    if AShowSourceLines then
    begin
      Instruction.AsString := FormatSymbol(AddrVA, Sym, qsSourceLine);
      if Instruction.AsString <> '' then
      begin
        Instruction.AddrVA := AddrVA;
        Instruction.Hint := '';
        Instruction.JmpTo := 0;
        Instruction.Len := 0;
        Result.Add(Instruction);
      end;
    end;

    PrevVA := {%H-}Int64(pBuff);
    Disasm.Disassemble(pBuff, ACodeBytes, ACode, AnInfo);
    Instruction.AddrVA := AddrVA;
    Instruction.AsString := FormatAsmCode(ACode, AnInfo, Length(ACodeBytes) shr 1);

    HasMem := (AnInfo.InstrType = itJump) and (Pos('[', Instruction.AsString) > 0);
    if HasMem then
    begin
      // Error in TX86AsmDecoder.Disassemble, AnInfo.InstrTargetOffs is not returned correctly.
      Dec(AnInfo.InstrTargetOffs, Length(ACodeBytes) shr 1);
      ExternalAddr := 0;
      ReadMemory(AnInfo.InstrTargetOffs, ExternalAddr, PointerSize);
    end
    else
      ExternalAddr := AddrVA + AnInfo.InstrTargetOffs;

    if AnInfo.InstrTargetOffs <> 0 then
    begin
      Instruction.Hint := QuerySymbolAtAddr(ExternalAddr, qsName).Description;
      if Instruction.Hint = '' then
        Instruction.Hint := '0x' + IntToHex(ExternalAddr, 1)
      else
        if HasMem then
          Instruction.Hint := '[0x' + IntToHex(ExternalAddr, 1) + '] -> ' + Instruction.Hint;
    end
    else
      Instruction.Hint := '';

    if AnInfo.InstrType = itJump then
    begin
      Instruction.JmpTo := ExternalAddr;
      if ShowFullAddress and not HasMem then
      begin
        SpaceIndex := Pos(' ', Instruction.AsString);
        if SpaceIndex > 0 then
        begin
          SetLength(Instruction.AsString, SpaceIndex);
          Instruction.AsString := Instruction.AsString + '0x' + IntToHex(ExternalAddr, 1);
        end;
      end;
    end
    else
    begin
      Instruction.JmpTo := 0;
      if AnInfo.InstrTargetOffs <> 0 then
      begin
        RipSimbol := '';
        RipAddr := 0;
        ReadMemory(ExternalAddr, RipAddr, PointerSize);
        if RipAddr <> 0 then
        begin
          RipSimbol := QuerySymbolAtAddr(RipAddr, qsName).Description;
          if Instruction.AsString.StartsWith('CALL') then
            Instruction.JmpTo := RipAddr;
        end;
        if RipSimbol = '' then
          Instruction.Hint := Format('RIP (0x%.1x) %s', [ExternalAddr, Instruction.Hint])
        else
          Instruction.Hint := Format('RIP (0x%.1x -> 0x%.1x) %s', [ExternalAddr, RipAddr, RipSimbol]);
      end;
    end;

    Instruction.Len := {%H-}Int64(pBuff) - PrevVA;
    Inc(AddrVA, Instruction.Len);
    Dec(nSize, Instruction.Len);
    if nSize >= 0 then
      Result.Add(Instruction);
  end;

  CpuViewDebugLog.Log('DebugGate: Disassembly end', False);
end;

function TCpuViewDebugGate.IsActive: Boolean;
begin
  Result := not (DebugState in [adsStoped, adsFinished]);
end;

function TCpuViewDebugGate.IsActiveJmp: Boolean;
var
  Inst: TInstruction;
begin
  Result := False;
  Inst := CurrentInstruction;
  if Inst.JmpTo = 0 then Exit;
  Result := Context.IsActiveJump(Inst.AsString);
end;

function TCpuViewDebugGate.GetSourceLine(AddrVA: Int64; out
  ASourcePath: string; out ASourceLine: Integer): Boolean;
var
  Sym: TFpSymbol;
  PasSource: TCodeBuffer;
  Editor: TSourceEditorInterface;
begin
  ASourcePath := '';
  ASourceLine := 0;
  if FDbgController = nil then Exit;
  Sym := FDbgController.CurrentProcess.FindProcSymbol(AddrVA);
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

function TCpuViewDebugGate.GetUserCodeAddrVA: Int64;
begin
  if DebugState = adsPaused then
    Result := FUserCodeAddrVA
  else
    Result := 0;
end;

procedure TCpuViewDebugGate.Pause;
begin
  CpuViewDebugLog.Log('DebugGate: Pause start', True);

  FDebugger.Pause;

  CpuViewDebugLog.Log('DebugGate: Pause end', False);
end;

function TCpuViewDebugGate.PointerSize: Integer;
begin
  if Assigned(FDbgController) then
    Result := FDbgController.CurrentProcess.PointerSize
  else
    Result := 0;
end;

function TCpuViewDebugGate.ProcessID: Cardinal;
begin
  if Assigned(FDbgController) then
    Result := FDbgController.CurrentProcess.ProcessID
  else
    Result := 0;
end;

function TCpuViewDebugGate.QuerySymbolAtAddr(AddrVA: Int64;
  AParam: TQuerySymbol): TQuerySymbolValue;
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
  end;
end;

function TCpuViewDebugGate.ReadMemory(AddrVA: Int64; var Buff; Size: Integer
  ): Boolean;
begin
  FSupportStream.Position := AddrVA;
  Result := FSupportStream.Read(Buff, Size) = Size;
end;

procedure TCpuViewDebugGate.Run;
begin
  CpuViewDebugLog.Log('DebugGate: Run start', True);

  FDebugger.Run;

  CpuViewDebugLog.Log('DebugGate: Run end', False);
end;

procedure TCpuViewDebugGate.Stop;
begin
  CpuViewDebugLog.Log('DebugGate: Stop start', True);

  FDebugger.Stop;

  CpuViewDebugLog.Log('DebugGate: Stop end', False);
end;

procedure TCpuViewDebugGate.SetNewIP(AddrVA: Int64);
begin
  CpuViewDebugLog.Log(Format('DebugGate: SetNewIP(0x%x)', [AddrVA]), True);

  UpdateRegValue(Context.InstructonPointID, AddrVA);

  CpuViewDebugLog.Log('DebugGate: SetNewIP end', False);
end;

procedure TCpuViewDebugGate.ToggleBreakPoint(AddrVA: Int64);
var
  Sym: TFpSymbol;
  Bp: TIDEBreakPoint;
  AFileName: string;
  I, ALine: Integer;
  BPList: TDBGPtrArray;
  AddrIsSourceLine: Boolean;
begin
  CpuViewDebugLog.Log(Format('DebugGate: ToggleBreakPoint(0x%x)', [AddrVA]), True);
  if FDbgController = nil then Exit;
  AFileName := '';
  ALine := -1;
  Bp := nil;
  AddrIsSourceLine := False;
  Sym := FDbgController.CurrentProcess.FindProcSymbol(AddrVA);
  if Assigned(Sym) then
  try
    AFileName := Sym.FileName;
    ALine := Sym.Line;
  finally
    Sym.ReleaseReference;
  end;
  if ALine > 0 then
  begin
    if FProcess.GetLineAddresses(AFileName, ALine, BPList{%H-}) then
    begin
      for I := 0 to Length(BPList) - 1 do
        if BPList[I] = AddrVA then
        begin
          AddrIsSourceLine := True;
          Bp := FBreakPoints.Find(AFileName, ALine);
          Break;
        end;
    end
  end;
  if Bp = nil then
    Bp := FBreakPoints.Find(AddrVA);
  if Assigned(Bp) then
  begin
    Bp.ReleaseReference;
    CpuViewDebugLog.Log('DebugGate: ToggleBreakPoint end', False);
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

  CpuViewDebugLog.Log('DebugGate: ToggleBreakPoint end', False);
end;

function TCpuViewDebugGate.ThreadStackLimit: TStackLimit;
begin
  Result := Utils.GetThreadStackLimit(ThreadId, PointerSize = 4);
end;

function TCpuViewDebugGate.ThreadID: Cardinal;
var
  Snapshot: TSnapshot;
begin
  if Assigned(FDbgController) then
  begin
    if Assigned(FSnapshotManager) then
      Snapshot := FSnapshotManager.SelectedEntry
    else
      Snapshot := nil;
    if Assigned(FThreadsMonitor) then
    begin
      if Assigned(Snapshot) then
        Result := FThreadsMonitor.Snapshots[Snapshot].CurrentThreadId
      else
        Result := FThreadsMonitor.CurrentThreads.CurrentThreadId;
    end
    else
      Result := FDbgController.CurrentProcess.ThreadID
  end
  else
    Result := 0;
end;

procedure TCpuViewDebugGate.TraceIn;
begin
  CpuViewDebugLog.Log('DebugGate: TraceIn start', True);

  FLockTimeOut := GetTickCount64;
  FDebugger.StepIntoInstr;

  CpuViewDebugLog.Log('DebugGate: TraceIn end', False);
end;

procedure TCpuViewDebugGate.TraceOut;
begin
  CpuViewDebugLog.Log('DebugGate: TraceOut start', True);

  FLockTimeOut := GetTickCount64;
  FDebugger.StepOverInstr;

  CpuViewDebugLog.Log('DebugGate: TraceOut end', False);
end;

procedure TCpuViewDebugGate.TraceTilReturn;
begin
  CpuViewDebugLog.Log('DebugGate: TraceTilReturn start', True);

  FDebugger.StepOut;

  CpuViewDebugLog.Log('DebugGate: TraceTilReturn end', False);
end;

procedure TCpuViewDebugGate.TraceTo(AddrVA: Int64);
var
  ALocation: TDBGPtrArray;
begin
  if not CheckCanWork then Exit;

  CpuViewDebugLog.Log(Format('DebugGate: TraceTo(0x%x)', [AddrVA]), True);

  StopAllWorkers;

  SetLength(ALocation{%H-}, 1);
  ALocation[0] := QWord(AddrVA);
  FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, ALocation));
  TFpDebugDebuggerAccess(FDebugger).StartDebugLoop;

  CpuViewDebugLog.Log('DebugGate: TraceTo end', False);
end;

procedure TCpuViewDebugGate.TraceToList(AddrVA: array of Int64);
var
  ALocation: TDBGPtrArray;
  I: Integer;
begin
  if not CheckCanWork then Exit;

  CpuViewDebugLog.Log('DebugGate: TraceToList', True);

  StopAllWorkers;

  SetLength(ALocation{%H-}, Length(AddrVA));
  for I := 0 to Length(AddrVA) - 1 do
    ALocation[I] := QWord(AddrVA[I]);
  FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, ALocation));
  TFpDebugDebuggerAccess(FDebugger).StartDebugLoop;

  CpuViewDebugLog.Log('DebugGate: TraceToList end', False);
end;

function TCpuViewDebugGate.UpdateRegValue(RegID: Integer; ANewRegValue: Int64
  ): Boolean;
var
  WorkQueue: TFpThreadPriorityWorkerQueue;
  WorkItem: TThreadWorkerChangeThreadContext;
  RegValue: TRegValue;
begin
  if not CheckCanWork then Exit;

  CpuViewDebugLog.Log(Format('DebugGate: UpdateRegValue(RegID: %d, ANewRegValue: 0x%x)', [RegID, ANewRegValue]), True);

  StopAllWorkers;

  WorkQueue := TFpDebugDebugger(FDebugger).WorkQueue;
  if Assigned(WorkQueue) then
  begin
    RegValue := Default(TRegValue);
    RegValue.QwordValue := ANewRegValue;
    WorkItem := TThreadWorkerChangeThreadContext.Create(Self);
    WorkQueue.PushItem(WorkItem);
    WorkQueue.WaitForItem(WorkItem, True);
    WorkItem.DecRef;
  end;

  RegValue := Default(TRegValue);
  RegValue.QwordValue := ANewRegValue;
  Context.ThreadID := ThreadID;
  Context.BeginUpdate;
  try
    Result := Context.RegSetValue(RegID, RegValue);
    if Result and (RegID = Context.InstructonPointID) then
      FTemporaryIP.AddOrSetValue(ThreadID, ANewRegValue);
  finally
    Context.EndUpdate;
  end;

  CpuViewDebugLog.Log('DebugGate: UpdateRegValue end', False);
end;

procedure TCpuViewDebugGate.UpdateRemoteStream(pBuff: PByte; AAddrVA: Int64;
  ASize: Int64);
var
  WorkQueue: TFpThreadPriorityWorkerQueue;
  WorkItem: TThreadWorkerMaskBreakpoints;
  AddrVA, Limit: Int64;
  NeedUpdate: Boolean;
  I: Integer;
begin
  if not CheckCanWork then Exit;

  NeedUpdate := False;
  Limit := AAddrVA + ASize;
  for I := 0 to BreakPointList.Count - 1 do
  begin
    AddrVA := BreakPointList.List[I].AddrVA;
    if (AddrVA >= AAddrVA) and (AddrVA < Limit) then
    begin
      NeedUpdate := True;
      Break;
    end;
  end;

  if not NeedUpdate then Exit;

  CpuViewDebugLog.Log(Format('DebugGate: UpdateRemoteStream(AAddrVA: 0x%x, ASize: %d)', [AAddrVA, ASize]), True);

  // Вызов опасен, если оставить его то будет AV при остановке процесса через паузу
  // The call is dangerous, if leave it there will be AV when stop the process via pause
  //StopAllWorkers;


  WorkQueue := TFpDebugDebugger(FDebugger).WorkQueue;
  if Assigned(WorkQueue) then
  begin
    WorkItem := TThreadWorkerMaskBreakpoints.Create(
      TFpDebugDebugger(FDebugger), pBuff, AAddrVA, ASize);
    WorkQueue.PushItem(WorkItem);
    WorkQueue.WaitForItem(WorkItem, True);
    WorkItem.DecRef;
  end;

  CpuViewDebugLog.Log('DebugGate: UpdateRemoteStream end', False);
end;

end.
