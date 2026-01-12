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
  {$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}

{$DEFINE DEBUG_LOG}

interface

{$I CpuViewCfg.inc}

uses
  LCLType,
  LCLIntf,
  LazVersion,
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

  // IDEIntf
  SrcEditorIntf,

  // LazDebuggerIntf
  LazDebuggerIntfBaseTypes,

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
  CodeToolManager,

  {$IFDEF LINUX}
  CpuView.Linux,
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  CpuView.Windows,
  {$ENDIF}

  FWHexView.Common,
  FWHexView.AsmTokenizer,
  CpuView.Common,
  CpuView.CommonDebug,
  CpuView.CPUContext,
  CpuView.Context.Params,
  CpuView.Context.Intel.Types,
  CpuView.Context.Intel,
  CpuView.DebugerGate,
  CpuView.Design.DbgLog;

{$message 'Disable when stable = 4.0'}
{$if laz_major >= 4}
  {$define ExtendedFpDebug}
{$endif}

type

  TFpDebugGate = class;

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
    FDbgIntf: TFpDebugGate;
  protected
    procedure DoExecute; override;
  public
    constructor Create(ADbgIntf: TFpDebugGate);
  end;

  { TFpDebugGate }

  TFpDebugGate = class(TCommonDebugGate)
  private
    FDbgController: TDbgController;
    FProcess: TDbgProcess;
    function FormatAsmCode(const Value: string; var AnInfo: TDbgInstInfo;
        CodeSize: Integer): string;
    procedure StopAllWorkers;
    procedure UpdateLocalSymbols;
  protected
    function GetSymbolAtAddr(AddrVA: Int64): TFpSymbol; override;
    function GetLineAddresses(const AFileName: string;
      ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean; override;
    function GetThreadID: Cardinal; override;
    function IsMainThreadId: Boolean; override;
    procedure InitContext(AValue: TCommonCpuContext); override;
    function GetEndOnProcToken: string; override;
    procedure Reset; override;
    procedure RefreshCallStack(AStack: TIdeCallStack); override;
    function UpdateContext: Boolean; override;
    procedure UpdateDebugger(ADebugger: TDebuggerIntf); override;
  public
    function Disassembly(AddrVA, LastKnownAddrVA: Int64; pBuff: PByte; nSize: Integer;
      AShowSourceLines, AShowCallFuncName: Boolean): TListEx<TInstruction>; override;
    function GetRemoteModuleHandle(const ALibraryName: string): TRemoteModule; override;
    function GetRemoteModules: TList<TRemoteModule>; override;
    function GetRemoteProcList(const AModule: TRemoteModule): TList<TRemoteProc>; override;
    function GetRemoteProcAddress(const AModule: TRemoteModule; const AProcName: string): Int64; override;
    function GetTokenizerMode: TTokenizerMode; override;
    function IsDebuggerLocked: Boolean; override;
    function PointerSize: Integer; override;
    function ProcessID: Cardinal; override;
    procedure TraceTo(AddrVA: Int64); override;
    procedure TraceToList(AddrVA: array of Int64); override;
    function UpdateRegValue(RegID: Integer; ANewRegValue: TRegValue): Boolean; override;
    procedure UpdateRemoteStream(pBuff: PByte; AAddrVA: Int64; ASize: Int64); override;
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
  FDbgIntf.DbgController.CurrentProcess.GetThread(FDbgIntf.CurrentThreadID, DebugThread);
  if DebugThread <> nil then
    DebugThread.BeforeContinue;
end;

constructor TThreadWorkerChangeThreadContext.Create(ADbgIntf: TFpDebugGate);
begin
  inherited Create((ADbgIntf.Debugger as TFpDebugDebugger), twpContinue);
  FDbgIntf := ADbgIntf;
end;

{ TFpDebugGate }

function TFpDebugGate.GetSymbolAtAddr(AddrVA: Int64): TFpSymbol;
begin
  Result := inherited;
  if UseDebugInfo and (Result = nil) and Assigned(FDbgController) then
    Result := FDbgController.CurrentProcess.FindProcSymbol(AddrVA);
end;

function TFpDebugGate.GetLineAddresses(const AFileName: string;
  ALine: Cardinal; var AResultList: TDBGPtrArray): Boolean;
begin
  if FProcess = nil then
    Result := False
  else
    Result := FProcess.GetLineAddresses(AFileName, ALine, AResultList);
end;

function TFpDebugGate.GetThreadID: Cardinal;
var
  Snapshot: TSnapshot;
begin
  if Assigned(FDbgController) then
  begin
    if Assigned(SnapshotManager) then
      Snapshot := SnapshotManager.SelectedEntry
    else
      Snapshot := nil;
    if Assigned(ThreadsMonitor) then
    begin
      if Assigned(Snapshot) then
        Result := ThreadsMonitor.Snapshots[Snapshot].CurrentThreadId
      else
        Result := ThreadsMonitor.CurrentThreads.CurrentThreadId;
    end
    else
      Result := FDbgController.CurrentProcess.ThreadID
  end
  else
    Result := 0;
end;

function TFpDebugGate.IsDebuggerLocked: Boolean;
begin
  if FDbgController = nil then
    Result := True
  else
    Result := inherited;
end;

function TFpDebugGate.FormatAsmCode(const Value: string;
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
  Result := '';
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

function TFpDebugGate.IsMainThreadId: Boolean;
begin
  if Assigned(FDbgController) then
    Result := CurrentThreadID = FDbgController.CurrentProcess.ThreadID
  else
    Result := True;
end;

procedure TFpDebugGate.InitContext(AValue: TCommonCpuContext);
begin
  ContextQueryParams.GetDefContext := GetIntelContext;
  ContextQueryParams.Get32Context := GetIntelWow64Context;
  ContextQueryParams.SetDefContext := SetIntelContext;
  ContextQueryParams.Set32Context := SetIntelWow64Context;
end;

function TFpDebugGate.GetEndOnProcToken: string;
begin
  Result := 'RET';
end;

procedure TFpDebugGate.Reset;
begin
  CpuViewDebugLog.Log('FpDebugGate: Reset start', True);

  inherited;

  {$message 'Bad approach, utilitarianism should stand alone'}
  {$IFDEF LINUX}
  LinuxDebugger := nil;
  {$ENDIF}

  FDbgController := nil;
  FProcess := nil;

  CpuViewDebugLog.Log('FpDebugGate: Reset end', False);
end;

procedure TFpDebugGate.RefreshCallStack(AStack: TIdeCallStack);
begin
  AStack.CountLimited(50);
end;

procedure TFpDebugGate.StopAllWorkers;
begin
  CpuViewDebugLog.Log('FpDebugGate: StopAllWorkers start', True);

  if Assigned(Debugger) then
    TFpDebugDebuggerAccess(Debugger).StopAllWorkers(True);

  CpuViewDebugLog.Log('FpDebugGate: StopAllWorkers end', False);
end;

function TFpDebugGate.UpdateContext: Boolean;
begin
  CpuViewDebugLog.Log('FpDebugGate: UpdateContext start', True);

  Result := inherited;
  if Result then
    UpdateLocalSymbols;

  CpuViewDebugLog.Log('FpDebugGate: UpdateContext end', False);
end;

procedure TFpDebugGate.UpdateDebugger(ADebugger: TDebuggerIntf);
begin
  CpuViewDebugLog.Log('FpDebugGate: UpdateDebugger start', True);

  inherited;

  if Assigned(Debugger) then
  begin
    if Debugger is TFpDebugDebugger then
    begin
      ErrorOnInit := False;
      DoError('');
      {$IFDEF LINUX}
      LinuxDebugger := TFpDebugDebugger(Debugger);
      {$ENDIF}
      FDbgController := TFpDebugDebugger(Debugger).DbgController;
      FProcess := FDbgController.CurrentProcess;
      DebugStorage.AutoLoadLibrary := False;
      DebugStorage.ExecutableFilePath := Debugger.FileName;
      if Assigned(FProcess) and not FProcess.GotExitProcess then
      begin
        InitDebugger;
        CpuViewDebugLog.Log('FpDebugGate: UpdateDebugger end', False);
        Exit;
      end;
    end
    else
    begin
      ErrorOnInit := True;
      DoError('Unsupported debugger: "' + Debugger.ClassName + '". Requires FpDebug.');
    end;
  end;

  Reset;

  CpuViewDebugLog.Log('FpDebugGate: UpdateDebugger end', False);
end;

procedure TFpDebugGate.UpdateLocalSymbols;
var
  LibraryMap: TLibraryMap;
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
begin
  if FDbgController = nil then Exit;
  LibraryMap := FDbgController.CurrentProcess.LibMap;
  if LibraryMap = nil then Exit;
  if LibraryMap.Count = DebugStorage.Count then Exit;
  DebugStorage.Clear;
  Iterator := TMapIterator.Create(LibraryMap);
  try
    while not Iterator.EOM do
    begin
      Iterator.GetData(Lib);
      DebugStorage.AddDbgInfo(Lib.Name, Lib.LoaderList.ImageBase, Lib.SymbolTableInfo);
      Iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

function TFpDebugGate.GetRemoteModuleHandle(const ALibraryName: string
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

function TFpDebugGate.GetRemoteModules: TList<TRemoteModule>;
var
  LibraryMap: TLibraryMap;
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
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

function TFpDebugGate.GetRemoteProcList(const AModule: TRemoteModule
  ): TList<TRemoteProc>;
var
  LibraryMap: TLibraryMap;
  Iterator: TMapIterator;
  Lib: TDbgLibrary;
  Sym: TFpSymbol;
  RemProc: TRemoteProc;
  I: Integer;
begin
  Result := TList<TRemoteProc>.Create;
  if FDbgController = nil then Exit;
  LibraryMap := FDbgController.CurrentProcess.LibMap;
  if LibraryMap = nil then Exit;
  Iterator := TMapIterator.Create(LibraryMap);
  try
    while not Iterator.EOM do
    begin
      Iterator.GetData(Lib);
      if Lib.ModuleHandle = AModule.hInstance then
      begin
        {$ifdef ExtendedFpDebug}
        for I := 0 to Lib.SymbolTableInfo.SymbolCount - 1 do
        begin
          Sym := Lib.SymbolTableInfo.Symbols[I];
          RemProc.FuncName := Sym.Name;
          RemProc.AddrVA := Sym.Address.Address;
          Result.Add(RemProc);
        end;
        {$endif}
        Break;
      end;
      Iterator.Next;
    end;
  finally
    Iterator.Free;
  end;
end;

function TFpDebugGate.GetRemoteProcAddress(const AModule: TRemoteModule;
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
      if Lib.ModuleHandle = AModule.hInstance then
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

function TFpDebugGate.GetTokenizerMode: TTokenizerMode;
begin
  Result := tmIntel;
end;

function TFpDebugGate.Disassembly(AddrVA, LastKnownAddrVA: Int64; pBuff: PByte;
  nSize: Integer; AShowSourceLines, AShowCallFuncName: Boolean): TListEx<
  TInstruction>;
var
  Disasm: TDbgAsmDecoder;
  Process: TDbgProcess;
  Instruction, SrcLine: TInstruction;
  PrevVA: Int64;
  ACodeBytes, ACode, RipSimbol: string;
  AnInfo: TDbgInstInfo;
  ExternalAddr, RipAddr: Int64;
  SpaceIndex, MemBracketIndex: Integer;
  HasMem, IsCallJmp: Boolean;
  PrevSymAddrVA, MissCount: TDBGPtr;
  Sym: TFpSymbol;
begin
  CpuViewDebugLog.Log(Format('FpDebugGate: Disassembly(AddrVA: 0x%x, nSize: %d)', [AddrVA, nSize]), True);

  Result := TListEx<TInstruction>.Create;
  if FDbgController = nil then Exit;
  Process := FDbgController.CurrentProcess;
  if Process = nil then Exit;
  Disasm := Process.Disassembler;
  if Disasm = nil then Exit;
  PrevSymAddrVA := LastKnownAddrVA;
  SrcLine := Default(TInstruction);
  while nSize > 0 do
  begin
    Sym := GetSymbolAtAddr(AddrVA);
    try
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
                Instruction.Mnemonic := InvalidAsmLine;
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
        SrcLine.AddrVA := AddrVA;
        SrcLine.Mnemonic := FormatSymbol(AddrVA, Sym, qsSourceLine);
      end;
    finally
      if Sym <> nil then
        Sym.ReleaseReference;
    end;

    PrevVA := {%H-}Int64(pBuff);
    Disasm.Disassemble(pBuff, ACodeBytes, ACode, AnInfo);
    Instruction := Default(TInstruction);
    Instruction.AddrVA := AddrVA;
    Instruction.Mnemonic := FormatAsmCode(ACode, AnInfo, Length(ACodeBytes) shr 1);
    RipSimbol := '';

    MemBracketIndex := Pos('[', Instruction.Mnemonic);
    HasMem := (AnInfo.InstrType = itJump) and (MemBracketIndex > 0);
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
      RipSimbol := QuerySymbolAtAddr(ExternalAddr, qsName).Description;
      if RipSimbol = '' then
        Instruction.Hint := '0x' + IntToHex(ExternalAddr, 1)
      else
        if HasMem then
          Instruction.Hint := '[0x' + IntToHex(ExternalAddr, 1) + '] -> ' + RipSimbol;
    end
    else
      Instruction.Hint := '';

    IsCallJmp :=
      Instruction.Mnemonic.StartsWith('CALL') or
      (Instruction.Mnemonic[1] = 'J');

    if (AnInfo.InstrType = itJump) and not HasMem then
    begin
      Instruction.JmpTo := ExternalAddr;
      SpaceIndex := Pos(' ', Instruction.Mnemonic);
      if SpaceIndex > 0 then
      begin
        // jmp/call +32 -> jmp/call 0x123456789
        if ShowFullAddress then
        begin
          SetLength(Instruction.Mnemonic, SpaceIndex);
          Instruction.Mnemonic := Instruction.Mnemonic + '0x' + IntToHex(ExternalAddr, 1);
        end;
        // call +32 -> call function_name with hiperlink
        if AShowCallFuncName and (Instruction.Mnemonic[1] = 'C') then
        begin
          Instruction.JmpToPosStart := SpaceIndex;
          if RipSimbol = '' then
            Instruction.JmpToPosLength := Length(Instruction.Mnemonic) - SpaceIndex
          else
          begin
            SetLength(Instruction.Mnemonic, SpaceIndex);
            Instruction.JmpToPosLength := Pos(' ', RipSimbol) - 1;
            if Instruction.JmpToPosLength <= 0 then
              Instruction.JmpToPosLength := Length(RipSimbol);
            Instruction.Mnemonic := Instruction.Mnemonic +
              Copy(RipSimbol, 1, Instruction.JmpToPosLength);
          end;
          Instruction.Hint := '';
        end
        else
        // jmp/call +32 -> jmp/call +32 with hiperlink
        begin
          Instruction.JmpToPosStart := SpaceIndex;
          Instruction.JmpToPosLength := Length(Instruction.Mnemonic) - SpaceIndex;
        end;
      end;
    end
    else
    begin
      Instruction.JmpTo := 0;
      if AnInfo.InstrTargetOffs <> 0 then
      begin
        RipAddr := 0;
        if (RipSimbol = '') or IsCallJmp then
        begin
          ReadMemory(ExternalAddr, RipAddr, PointerSize);
          if RipAddr <> 0 then
          begin
            RipSimbol := QuerySymbolAtAddr(RipAddr, qsName).Description;
            if IsCallJmp then
            begin
              Instruction.JmpTo := RipAddr;
              Instruction.JmpToPosStart := MemBracketIndex;
              Instruction.JmpToPosLength := Pos(']', Instruction.Mnemonic) - MemBracketIndex - 1;
            end;
          end;
        end;
        if RipAddr = 0 then
          Instruction.Hint := Format('RIP (0x%.1x) %s', [ExternalAddr, RipSimbol])
        else
          Instruction.Hint := Format('RIP (0x%.1x -> 0x%.1x) %s', [ExternalAddr, RipAddr, RipSimbol]);
      end;
    end;

    Instruction.Len := {%H-}Int64(pBuff) - PrevVA;
    Inc(AddrVA, Instruction.Len);
    Dec(nSize, Instruction.Len);
    if nSize >= 0 then
    begin
      if SrcLine.Mnemonic <> '' then
        Result.Add(SrcLine);
      Result.Add(Instruction);
    end;
  end;

  CpuViewDebugLog.Log('FpDebugGate: Disassembly end', False);
end;

function TFpDebugGate.PointerSize: Integer;
begin
  if Assigned(FDbgController) then
    Result := FDbgController.CurrentProcess.PointerSize
  else
    Result := 0;
end;

function TFpDebugGate.ProcessID: Cardinal;
begin
  if Assigned(FDbgController) then
    Result := FDbgController.CurrentProcess.ProcessID
  else
    Result := 0;
end;

procedure TFpDebugGate.TraceTo(AddrVA: Int64);
var
  ALocation: TDBGPtrArray;
begin
  if IsDebuggerLocked then Exit;

  CpuViewDebugLog.Log(Format('FpDebugGate: TraceTo(0x%x)', [AddrVA]), True);

  StopAllWorkers;

  SetLength(ALocation{%H-}, 1);
  ALocation[0] := QWord(AddrVA);
  FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, ALocation));
  TFpDebugDebuggerAccess(Debugger).StartDebugLoop;

  CpuViewDebugLog.Log('FpDebugGate: TraceTo end', False);
end;

procedure TFpDebugGate.TraceToList(AddrVA: array of Int64);
var
  ALocation: TDBGPtrArray;
  I: Integer;
begin
  if IsDebuggerLocked then Exit;

  CpuViewDebugLog.Log('FpDebugGate: TraceToList', True);

  StopAllWorkers;

  SetLength(ALocation{%H-}, Length(AddrVA));
  for I := 0 to Length(AddrVA) - 1 do
    ALocation[I] := QWord(AddrVA[I]);
  FDbgController.InitializeCommand(TDbgControllerRunToCmd.Create(FDbgController, ALocation));
  TFpDebugDebuggerAccess(Debugger).StartDebugLoop;

  CpuViewDebugLog.Log('FpDebugGate: TraceToList end', False);
end;

function TFpDebugGate.UpdateRegValue(RegID: Integer;
  ANewRegValue: TRegValue): Boolean;
var
  WorkQueue: TFpThreadPriorityWorkerQueue;
  WorkItem: TThreadWorkerChangeThreadContext;
begin
  if IsDebuggerLocked then Exit(False);

  CpuViewDebugLog.Log(Format('FpDebugGate: UpdateRegValue(RegID: %d, ANewRegValue: 0x%x)', [RegID, ANewRegValue.QwordValue]), True);

  StopAllWorkers;

  WorkQueue := TFpDebugDebugger(Debugger).WorkQueue;
  if Assigned(WorkQueue) then
  begin
    WorkItem := TThreadWorkerChangeThreadContext.Create(Self);
    WorkQueue.PushItem(WorkItem);
    WorkQueue.WaitForItem(WorkItem, True);
    WorkItem.DecRef;
  end;

  Result := inherited;

  CpuViewDebugLog.Log('FpDebugGate: UpdateRegValue end', False);
end;

procedure TFpDebugGate.UpdateRemoteStream(pBuff: PByte; AAddrVA: Int64;
  ASize: Int64);
var
  WorkQueue: TFpThreadPriorityWorkerQueue;
  WorkItem: TThreadWorkerMaskBreakpoints;
  AddrVA, Limit: Int64;
  NeedUpdate: Boolean;
  I: Integer;
begin
  if IsDebuggerLocked then Exit;

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

  CpuViewDebugLog.Log(Format('FpDebugGate: UpdateRemoteStream(AAddrVA: 0x%x, ASize: %d)', [AAddrVA, ASize]), True);

  // Вызов опасен, если оставить его то будет AV при остановке процесса через паузу
  // The call is dangerous, if leave it there will be AV when stop the process via pause
  //StopAllWorkers;

  WorkQueue := TFpDebugDebugger(Debugger).WorkQueue;
  if Assigned(WorkQueue) then
  begin
    WorkItem := TThreadWorkerMaskBreakpoints.Create(
      TFpDebugDebugger(Debugger), pBuff, AAddrVA, ASize);
    WorkQueue.PushItem(WorkItem);
    WorkQueue.WaitForItem(WorkItem, True);
    WorkItem.DecRef;
  end;

  CpuViewDebugLog.Log('FpDebugGate: UpdateRemoteStream end', False);
end;

end.
