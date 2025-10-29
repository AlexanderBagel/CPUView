////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.GdbDebug.pas
//  * Purpose   : Implementation of gateway to GDB.
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

// W:\lazarus\stable\lazarus\components\lazdebuggergdbmi
// W:\lazarus\stable\lazarus\components\lazdebuggers\lazdebuggerfpgdbmi

unit CpuView.GdbDebug;

{$MODE Delphi}
{$WARN 5024 off : Parameter "$1" not used}

{$DEFINE DEBUG_LOG}

interface

{$I CpuViewCfg.inc}

uses
  LCLType,
  LCLIntf,

  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  SysUtils,
  StrUtils,
  Classes,
  Forms,
  UTF8Process,
  Generics.Collections,
  Generics.Defaults,
  ExtCtrls,
  Pipes,

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
  DebugUtils,

  // LazDebuggerGdbMi
  GDBMIDebugger,
  GDBMIMiscClasses,

  // LazUtils
  LazLoggerBase,

  // FpDebug
  FpDbgInfo,

  // CodeTools
  CodeCache,
  CodeToolManager,

  FWHexView.Common,
  FWHexView.AsmTokenizer,
  CpuView.Common,
  CpuView.CommonDebug,
  CpuView.DebugerGate,
  CpuView.Design.DbgLog;

type

  { TGdbDebugger }

  TGdbDebugger = class(TGDBMIDebugger)
  public
    property CurrentCommand;
    property CurrentThreadId;
    property TargetPID;
    property TargetPtrSize;
    property Waiting: Boolean read GetWaiting;
  end;

  { TGdbInstruction }

  TGdbInstruction = record
    AddrVA: Int64;
    AsString, FuncName: string;
    Len: Integer;
    ExternalAddrVA, InstrTargetVA, Offset: QWord;
    IsJmp, IsRip: Boolean;
    InstrType: (itAny, itJump, itCall);
  end;

  TTemporaryBreakPoint = record
    BpAddrVA: Int64;
    BpInstance: TIDEBreakPoint;
  end;

  { TGdbAbstractDebugGate }

  TGdbAbstractDebugGate = class(TCommonDebugGate)
  private
    FBreakPointChangeLocked: Integer;
    FCtxRegNames, FCtxKnownRegs: TStringList;
    FDasmBaseStartAddr, FDasmNewFuncAddrVA: TDBGPtr;
    FDasmLastFuncName: string;
    FDasmLastOffset: Integer;
    FKillCommandDetected: Boolean;
    FProcess: TProcessUTF8;
    FTemporaryBreakPoints: TListEx<TTemporaryBreakPoint>;
    FThreadUpdateTimer: TTimer;
    function AddTemporaryBreakPoint(AddrVA: Int64): Boolean;
    procedure ClearTemporaryBreakPoints(AClearList: Boolean);
    {$IFNDEF USE_MANUAL_READMEMORY}
    function ReadData(AddrVA: Pointer; var Buff; ASize: Longint): Longint;
    {$ENDIF}
    procedure OnThreadUpdateTimer(Sender: TObject);
  protected
    procedure BreakPointChanged; override;
    function CanShowBreakPoint(AValue: TIDEBreakPoint): Boolean; override;
    procedure DoCurrentThreadChange; override;
    procedure DoPauseProcessing(AStart: Boolean); override;
    function ExecuteGdbCommand(const ACommand: String; const AValues: array of const;
      out AResult: TGDBMIExecResult): Boolean;
    function gdb: TGdbDebugger; inline;
    function GetThreadID: Cardinal; override;
    function GetThreadTargetID: Cardinal; override;
    procedure Reset; override;
    procedure UpdateDebugger(ADebugger: TDebuggerIntf); override;
  protected
    // functions specific to the processor architecture
    procedure CtxReset;
    procedure CtxUpdateRegIndex; virtual; abstract;
    function CtxUpdateRegNames: Boolean;
    function DasmExecute(AStartAddr, AnEndAddr, ALastKnownAddrVA: TDbgPtr; WithSrc: Boolean): TListEx<TGdbInstruction>;
    function DasmGetSourceLine(const ASourcePath, AFullPath, ASourceLine: TPCharWithLen): string;
    function DasmParseItem(AItem: PGDBMINameValue; AList: TListEx<TGdbInstruction>): Boolean; virtual; abstract;
    procedure DasmReset(AFull: Boolean);
    procedure FormatInstruction(var Inst: TInstruction;
      const GdbInst: TGdbInstruction; AShowCallFuncName: Boolean); virtual; abstract;
    property CtxRegNames: TStringList read FCtxRegNames;
    property CtxKnownRegs: TStringList read FCtxKnownRegs;
    property DasmBaseStartAddr: TDBGPtr read FDasmBaseStartAddr write FDasmBaseStartAddr;
    property DasmLastFuncName: string read FDasmLastFuncName write FDasmLastFuncName;
    property DasmLastOffset: Integer read FDasmLastOffset write FDasmLastOffset;
    property DasmNewFuncAddrVA: TDBGPtr read FDasmNewFuncAddrVA write FDasmNewFuncAddrVA;
  public
    constructor Create(AOwner: TComponent; AUtils: TCommonAbstractUtils); override;
    destructor Destroy; override;
    function DebugState: TAbstractDebugState; override;
    function Disassembly(AddrVA, LastKnownAddrVA: Int64; pBuff: PByte;
      nSize: Integer; AShowSourceLines, AShowCallFuncName: Boolean): TListEx<TInstruction>; override;
    function IsDebuggerLocked: Boolean; override;
    function GetRemoteModuleHandle(const ALibraryName: string): TRemoteModule; override;
    function GetRemoteModules: TList<TRemoteModule>; override;
    function GetRemoteProcList(const AModule: TRemoteModule): TList<TRemoteProc>; override;
    function GetRemoteProcAddress(const AModule: TRemoteModule; const AProcName: string): Int64; override;
    function PointerSize: Integer; override;
    function PreferededDasmBufSize: Integer; override;
    function ProcessID: Cardinal; override;
    procedure TraceTo(AddrVA: Int64); override;
    procedure TraceToList(AddrVA: array of Int64); override;
    procedure UpdateRemoteStream(pBuff: PByte; AAddrVA: Int64; ASize: Int64); override;
  end;

implementation

function InstructionComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF}
  A, B: TGdbInstruction): Integer;
begin
  if A.AddrVA < B.AddrVA then
    Result := -1
  else
    if A.AddrVA = B.AddrVA then
      Result := 0
    else
      Result := 1;
end;

{ TGdbAbstractDebugGate }

function TGdbAbstractDebugGate.PointerSize: Integer;
begin
  if Assigned(gdb) then
    Result := gdb.TargetPtrSize
  else
    Result := 0;
end;

function TGdbAbstractDebugGate.PreferededDasmBufSize: Integer;
begin
  Result := 256;
end;

function TGdbAbstractDebugGate.ProcessID: Cardinal;
begin
  if Assigned(gdb) then
    Result := Cardinal(gdb.TargetPID)
  else
    Result := 0;
end;

procedure TGdbAbstractDebugGate.TraceTo(AddrVA: Int64);
begin
  if IsDebuggerLocked then Exit;

  CpuViewDebugLog.Log(Format('TGdbAbstractDebugGate: TraceTo(0x%x)', [AddrVA]), True);

  AddTemporaryBreakPoint(AddrVA);
  Run;
  ClearTemporaryBreakPoints(True);

  CpuViewDebugLog.Log('TGdbAbstractDebugGate: TraceTo end', False);
end;

procedure TGdbAbstractDebugGate.TraceToList(AddrVA: array of Int64);
var
  I: Integer;
begin
  if IsDebuggerLocked then Exit;

  CpuViewDebugLog.Log('FpDebugGate: TraceToList', True);

  Inc(FBreakPointChangeLocked);
  try
    for I := 0 to Length(AddrVA) - 1 do
      AddTemporaryBreakPoint(AddrVA[I]);
  finally
    Dec(FBreakPointChangeLocked);
  end;
  Run;
  ClearTemporaryBreakPoints(True);

  CpuViewDebugLog.Log('FpDebugGate: TraceToList end', False);
end;

procedure TGdbAbstractDebugGate.UpdateRemoteStream(pBuff: PByte; AAddrVA: Int64;
  ASize: Int64);
begin
  // dummee...
end;

function TGdbAbstractDebugGate.AddTemporaryBreakPoint(AddrVA: Int64): Boolean;
var
  ABreakPoint: TTemporaryBreakPoint;
begin
  Result := IsBreakPointActive(AddrVA);
  if Result then Exit;
  Inc(FBreakPointChangeLocked);
  try
    ABreakPoint.BpAddrVA := AddrVA;
    ABreakPoint.BpInstance := BreakPoints.Add(AddrVA, True);
    FTemporaryBreakPoints.Add(ABreakPoint);
  finally
    Dec(FBreakPointChangeLocked);
    ABreakPoint.BpInstance.EndUpdate;
  end;
end;

procedure TGdbAbstractDebugGate.ClearTemporaryBreakPoints(AClearList: Boolean);
var
  I: Integer;
begin
  if FTemporaryBreakPoints = nil then Exit;
  Inc(FBreakPointChangeLocked);
  try
    for I := 0 to FTemporaryBreakPoints.Count - 1 do
      with FTemporaryBreakPoints.List[I] do
        if BpInstance <> nil then
        begin
          BpInstance.ReleaseReference;
          BpInstance := nil;
        end;
    if AClearList then
      FTemporaryBreakPoints.Clear;
  finally
    Dec(FBreakPointChangeLocked);
  end;
end;

function TGdbAbstractDebugGate.CtxUpdateRegNames: Boolean;
var
  ExecResult: TGDBMIExecResult;
  List: TGDBMINameValueList;
  I: Integer;
begin
  Result := ExecuteGdbCommand('-data-list-register-names', [], ExecResult) and (ExecResult.State = dsNone);
  if not Result then Exit;
  List := TGDBMINameValueList.Create(ExecResult, ['register-names']);
  try
    FCtxRegNames.Capacity := List.Count;
    for I := 0 to List.Count - 1 do
      FCtxRegNames.Add(LowerCase(UnQuote(List.GetString(I))));
  finally
    List.Free;
  end;
  CtxUpdateRegIndex;
end;

procedure TGdbAbstractDebugGate.CtxReset;
begin
  if Assigned(FCtxKnownRegs) then
    FCtxKnownRegs.Clear;
  if Assigned(FCtxRegNames) then
    FCtxRegNames.Clear;
end;

function TGdbAbstractDebugGate.DasmExecute(AStartAddr, AnEndAddr,
  ALastKnownAddrVA: TDbgPtr; WithSrc: Boolean): TListEx<TGdbInstruction>;
const
  CacheSize = 16;
var
  ExecResult: TGDBMIExecResult;
  ValueList: TGDBMINameValueList;
  I, StartAddrFix: Integer;
  PrevStartAddr: TDBGPtr;
  Inst: TGdbInstruction;
begin
  DasmReset(True);
  FDasmBaseStartAddr := AStartAddr;
  Result := TListEx<TGdbInstruction>.Create(
    TComparer<TGdbInstruction>.Construct(InstructionComparer));

  if IsDebuggerLocked then
  begin
    CpuViewDebugLog.Log('Unexpected call. DasmExecute locked !!!');
    Exit;
  end;

  StartAddrFix := 0;
  Inc(AnEndAddr, CacheSize);
  repeat
    ExecuteGdbCommand('-data-disassemble -s %u -e %u -- 0',
      [AStartAddr + StartAddrFix, AnEndAddr], ExecResult);
    ValueList := TGDBMINameValueList.Create(ExecResult);
    try
      ValueList.SetPath('asm_insns');
      for I := 0 to ValueList.Count - 1 do
      begin
        if not DasmParseItem(ValueList.Items[I], Result) then
        begin
          if FDasmNewFuncAddrVA <> 0 then
            AStartAddr := FDasmNewFuncAddrVA;
          Break;
        end;
      end;
      DasmReset(False);
    finally
      ValueList.Free;
    end;

    // не смогли дизассемблировать. если есть адрес предыдущей известной инструкции,
    // пробуем выполнить дизасссемблирование с него. если все еще не получилось,
    // то сдвигаемся на байт вперед до тех пор, пока не сможем выполнить дизассемблирование

    // could not disassemble. if there is the address of the previous known instruction,
    // try disassembling from it. if it still fails,
    // then we move forward byte by byte until we are able to disassemble it

    if (Result.Count = 0) and (FDasmNewFuncAddrVA = 0) then
    begin
      if ALastKnownAddrVA = 0 then
        Inc(StartAddrFix)
      else
        if AStartAddr <> ALastKnownAddrVA then
        begin
          PrevStartAddr := AStartAddr;
          AStartAddr := ALastKnownAddrVA;
        end
        else
        begin
          AStartAddr := PrevStartAddr;
          ALastKnownAddrVA := 0;
          StartAddrFix := 1;
        end;
      Continue;
    end;

    if StartAddrFix > 0 then
    begin
      Inst := Default(TGdbInstruction);
      Inst.AddrVA := AStartAddr;
      Inst.AsString := InvalidAsmLine;
      Result.Add(Inst);
      StartAddrFix := 0;
    end;

    // Emergency control
    if (Result.Count = 0) and (AStartAddr <> FDasmNewFuncAddrVA) then
    begin
      Inc(StartAddrFix);
      Continue;
    end;

    if Result.Count > 0 then
    begin
      for I := 0 to Result.Count - 2 do
        Result.List[I].Len := Result.List[I + 1].AddrVA - Result.List[I].AddrVA;

      // GDB может пропустить адрес в середине инструкции
      // и вернуть данные по началу инструкции которая будет расположена немного
      // дальше начала дизассемблируемого буфера, поэтому необходимо
      // выполнить коррекцию, добавив размер нераспознанных байт

      // GDB may miss an address in the middle of an instruction and return data
      // about the next instruction, which will be located a bit further away
      // than the beginning of the disassembled buffer, so an adjustment must be
      // made by adding the size of the unrecognized bytes

      if Result.First.AddrVA > AStartAddr then
      begin
        Inst := Default(TGdbInstruction);
        Inst.AddrVA := AStartAddr;
        Inst.AsString := InvalidAsmLine;
        Inst.Len := Result.First.AddrVA - AStartAddr;
        Result.Insert(0, Inst);
      end;

      if FDasmNewFuncAddrVA <> AStartAddr then
        AStartAddr := Result.Last.AddrVA;
      if Trim(Result.Last.AsString) = EndOnProcToken then
      begin
        Result.List[Result.Count - 1].Len := 1;
        Inc(AStartAddr);
      end;
    end;

  until Int64(AnEndAddr - AStartAddr) < CacheSize;

  // Если в список включена уже известная инструкция, необходимо её удалить

  // If an already known instruction is included in the list, you must delete it

  if Result.First.AddrVA = ALastKnownAddrVA then
    Result.Delete(0);
end;

function TGdbAbstractDebugGate.DasmGetSourceLine(const ASourcePath, AFullPath,
  ASourceLine: TPCharWithLen): string;
var
  PasSource: TCodeBuffer;
  Editor: TSourceEditorInterface;
  ASrcLineNumber: Cardinal;
  ASrcFileName, ASrcFilePath: string;
begin
  ASrcFileName := gdb.ConvertPathFromGdbToLaz(PCLenToString(ASourcePath, True));
  ASrcFilePath := gdb.ConvertPathFromGdbToLaz(PCLenToString(AFullPath, True));
  ASrcLineNumber := PCLenToInt(ASourceLine);
  if DebugBoss.GetFullFilename(ASrcFileName, False) then
  begin
    PasSource := CodeToolBoss.LoadFile(ASrcFileName, True, False);
    if (PasSource = nil) and FileExists(ASrcFilePath) then
      PasSource := CodeToolBoss.LoadFile(ASrcFilePath, True, False);
    if Assigned(PasSource) then
    begin
      Editor := SourceEditorManagerIntf.SourceEditorIntfWithFilename(ASrcFileName);
      if Editor <> nil then
        ASrcLineNumber := Editor.DebugToSourceLine(ASrcLineNumber);
      Result := Format('%s:%d %s', [
        ExtractFileName(ASrcFileName), ASrcLineNumber,
        Trim(PasSource.GetLine(ASrcLineNumber - 1, False))]);
    end
    else
      Result := Format('%s:%d', [ASrcFileName, ASrcLineNumber]);
  end;
end;

procedure TGdbAbstractDebugGate.DasmReset(AFull: Boolean);
begin
  if AFull then FDasmNewFuncAddrVA := 0;
  FDasmLastFuncName := '';
  FDasmLastOffset := 0;
end;

function TGdbAbstractDebugGate.ExecuteGdbCommand(const ACommand: String;
  const AValues: array of const; out AResult: TGDBMIExecResult): Boolean;
var
  I, ATimeOut, TotalBytesAvailable, ReadCount: Integer;
  Cmd, Buff, ResultBuff: string;
begin
  Result := False;
  if FProcess = nil then Exit;
  Cmd := Format(ACommand, AValues);
  if Cmd = '' then Exit;

  if IsDebuggerLocked then
  begin
    CpuViewDebugLog.Log(Format('Unexpected call. ExecuteGdbCommand "%s" locked !!!', [Cmd]));
    Exit;
  end;

  FProcess.Input.Write(Cmd[1], Length(Cmd));
  Cmd := LineEnding;
  FProcess.Input.Write(Cmd[1], Length(Cmd));
  ATimeOut := 500;
  TotalBytesAvailable := 0;
  while FProcess.Active and (ATimeOut > 0) do
  begin
    TotalBytesAvailable := FProcess.Output.NumBytesAvailable;
    if TotalBytesAvailable > 0 then Break;
    Dec(ATimeOut, 10);
    Sleep(10);
    TotalBytesAvailable := 0;
  end;
  if TotalBytesAvailable > 0 then
  begin
    AResult := Default(TGDBMIExecResult);
    SetLength(Buff{%H-}, $8000);
    ResultBuff := '';
    while True do
    begin
      ReadCount := FProcess.Output.Read(Buff[1], Length(Buff));
      ResultBuff := ResultBuff + Copy(Buff, 1, ReadCount);
      if ReadCount < Length(Buff) then Break;
    end;
    Result := ResultBuff.StartsWith('^done');
    if Result then
    begin
      I := 1;
      TotalBytesAvailable := Length(ResultBuff);
      while (I <= TotalBytesAvailable) and not (ResultBuff[I] in [#10,#13]) do
        Inc(I);
      AResult.Values := Copy(ResultBuff, 6, I - 6);
    end
    else
      AResult.State := dsError;
  end;
end;

function TGdbAbstractDebugGate.gdb: TGdbDebugger;
begin
  Result := TGdbDebugger(Debugger);
end;

{$IFNDEF USE_MANUAL_READMEMORY}
function TGdbAbstractDebugGate.ReadData(AddrVA: Pointer; var Buff; ASize: Longint): Longint;
var
  ExecResult: TGDBMIExecResult;
  MemDump: TGDBMIMemoryDumpResultList;
  pCursor: PByte;
  I: Integer;
begin
  if not ExecuteGdbCommand('-data-read-memory %u x 1 1 %u',
    [{%H-}QWord(AddrVA), ASize], ExecResult) or (ExecResult.State = dsError) then
  begin
    FillChar(Buff, ASize, 0);
    Exit(ASize);
  end;
  MemDump := TGDBMIMemoryDumpResultList.Create(ExecResult);
  try
    Result := MemDump.Count;
    if Result > ASize then
      Result := ASize;
    ASize := Result;
    I := 0;
    pCursor := @Buff;
    while ASize > 0 do
    begin
      //if ASize >= SizeOf(QWord) then
      //begin
      //  Move(MemDump.QWordAtIdx[I], pCursor^, SizeOf(QWord));
      //  Inc(pCursor, SizeOf(QWord));
      //  Inc(I, SizeOf(QWord));
      //  Dec(ASize, SizeOf(QWord));
      //  Continue;
      //end;
      if ASize >= SizeOf(DWord) then
      begin
        Move(MemDump.DWordAtIdx[I], pCursor^, SizeOf(DWord));
        Inc(pCursor, SizeOf(DWord));
        Inc(I, SizeOf(DWord));
        Dec(ASize, SizeOf(DWord));
        Continue;
      end;
      if ASize >= SizeOf(Word) then
      begin
        Move(MemDump.WordAtIdx[I], pCursor^, SizeOf(Word));
        Inc(pCursor, SizeOf(Word));
        Inc(I, SizeOf(Word));
        Dec(ASize, SizeOf(Word));
        Continue;
      end;
      pCursor^ := Byte(MemDump.ItemNum[I]);
      Break;
    end;
  finally
    MemDump.Free;
  end;
end;
{$ENDIF}

procedure TGdbAbstractDebugGate.OnThreadUpdateTimer(Sender: TObject);
begin
  FThreadUpdateTimer.Enabled := False;
  DoCurrentThreadChange;
end;

procedure TGdbAbstractDebugGate.BreakPointChanged;
begin
  if FBreakPointChangeLocked = 0 then
    inherited BreakPointChanged;
end;

function TGdbAbstractDebugGate.CanShowBreakPoint(AValue: TIDEBreakPoint): Boolean;
var
  I: Integer;
begin
  if FTemporaryBreakPoints = nil then
    Result := True
  else
  begin
    if AValue.Kind = bpkAddress then
      for I := 0 to FTemporaryBreakPoints.Count - 1 do
        if FTemporaryBreakPoints.List[I].BpAddrVA = AValue.Address then
        begin
          Result := False;
          Exit;
        end;
    Result := True;
  end;
end;

procedure TGdbAbstractDebugGate.DoCurrentThreadChange;
begin
  if Assigned(gdb) then
  begin
    // if not IsDebuggerLocked then <<< WRONG!!!
    if InPauseStateProcessing or (gdb.CurrentCommand = nil) then
    begin
      FThreadUpdateTimer.Enabled := False;
      inherited DoCurrentThreadChange;
    end
    else
      FThreadUpdateTimer.Enabled := True;
  end;
end;

procedure TGdbAbstractDebugGate.DoPauseProcessing(AStart: Boolean);
begin
  inherited DoPauseProcessing(AStart);
  FThreadUpdateTimer.Enabled := False;
  if AStart then
    ClearTemporaryBreakPoints(False);
end;

function TGdbAbstractDebugGate.GetThreadID: Cardinal;
var
  Snapshot: TSnapshot;
begin
  Result := 0;
  if gdb = nil then Exit;
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
  end;
end;

function TGdbAbstractDebugGate.GetThreadTargetID: Cardinal;
var
  I: Integer;
begin
  if (ThreadsMonitor = nil) or (ThreadsMonitor.CurrentThreads.Count = 0) then Exit(0);
  Result := inherited;
  if Result = 0 then Exit;
  for I := 0 to ThreadsMonitor.CurrentThreads.Count - 1 do
    if ThreadsMonitor.CurrentThreads[I].ThreadId = Result then
    begin
      Result := ThreadsMonitor.CurrentThreads[I].ThreadTargetId;
      Break;
    end;
end;

procedure TGdbAbstractDebugGate.Reset;
begin
  inherited;
  CtxReset;
  DasmReset(True);
  FProcess := nil;
  {$IFNDEF USE_MANUAL_READMEMORY}
  Utils.ReadDataEx := nil;
  {$ENDIF}
end;

procedure TGdbAbstractDebugGate.UpdateDebugger(ADebugger: TDebuggerIntf);
begin
  CpuViewDebugLog.Log('GdbDebugGate: UpdateDebugger start', True);

  inherited;

  if Assigned(Debugger) then
  begin
    if Debugger is TGDBMIDebugger then
    begin
      ErrorOnInit := False;
      DoError('');
      FProcess := TGDBMIDebugger(Debugger).DebugProcess;
      if Assigned(FProcess) and (Debugger.State in [dsInit, dsPause]) then
      begin
        {$IFNDEF USE_MANUAL_READMEMORY}
        Utils.ReadDataEx := ReadData;
        {$ENDIF}
        DebugStorage.AutoLoadLibrary := True;
        DebugStorage.ExecutableFilePath := Debugger.FileName;
        DebugStorage.AddDbgInfo(Debugger.FileName, 0, 0);
        InitDebugger;
        CpuViewDebugLog.Log('GdbDebugGate: UpdateDebugger end', False);
        Exit;
      end;
    end
    else
    begin
      ErrorOnInit := True;
      DoError('Unsupported debugger: "' + Debugger.ClassName + '". Requires TGDBMIDebugger.');
    end;
  end;

  Reset;

  CpuViewDebugLog.Log('GdbDebugGate: UpdateDebugger end', False);
end;

constructor TGdbAbstractDebugGate.Create(AOwner: TComponent;
  AUtils: TCommonAbstractUtils);
begin
  inherited Create(AOwner, AUtils);

  FCtxRegNames := TStringList.Create;
  FCtxKnownRegs := TStringList.Create;
  FTemporaryBreakPoints := TListEx<TTemporaryBreakPoint>.Create;

  // DebugManager uses a timer to start debugging, but at this point,
  // some information is missing for GDB, such as thread information.
  // Therefore, we will use the same approach.
  FThreadUpdateTimer := TTimer.Create(Self);
  FThreadUpdateTimer.Enabled := False;
  FThreadUpdateTimer.Interval := 1;
  FThreadUpdateTimer.OnTimer := OnThreadUpdateTimer;
end;

destructor TGdbAbstractDebugGate.Destroy;
begin
  ClearTemporaryBreakPoints(True);
  FTemporaryBreakPoints.Free;
  FThreadUpdateTimer.Free;
  FreeAndNil(FCtxRegNames);
  FreeAndNil(FCtxKnownRegs);
  inherited Destroy;
end;

function TGdbAbstractDebugGate.DebugState: TAbstractDebugState;
begin
  Result := inherited;
  if Assigned(gdb) and Assigned(gdb.CurrentCommand) then
  begin
    if gdb.CurrentCommand is TGDBMIDebuggerCommandKill then
      FKillCommandDetected := True;
    if gdb.CurrentCommand is TGDBMIDebuggerCommandDetach then
      FKillCommandDetected := True;
    if gdb.CurrentCommand is TGDBMIDebuggerCommandStartBase then
      FKillCommandDetected := False;
  end;
  if FKillCommandDetected and (Result = adsPaused) then
    Result := adsStoped;
end;

function TGdbAbstractDebugGate.Disassembly(AddrVA, LastKnownAddrVA: Int64;
  pBuff: PByte; nSize: Integer; AShowSourceLines, AShowCallFuncName: Boolean
  ): TListEx<TInstruction>;
var
  InstList: TList<TGdbInstruction>;
  I, SpaceIndex, MemBracketIndex: Integer;
  Inst: TInstruction;
  GdbInst: TGdbInstruction;
  Sym: TFpSymbol;
begin
  Assert(gdb <> nil, 'TGdbAbstractDebugGate.Disassembly: gdb = nil');
  gdb.LockCommandProcessing;
  try
    InstList := DasmExecute(AddrVA,
      AddrVA + nSize, LastKnownAddrVA, AShowSourceLines);
    InstList.Sort;
    Result := TListEx<TInstruction>.Create;
    for I := 0 to InstList.Count - 1 do
    begin
      GdbInst := InstList[I];
      if GdbInst.Len > nSize then Break;
      Dec(nSize, GdbInst.Len);
      Inst := Default(TInstruction);
      Inst.AddrVA := GdbInst.AddrVA;
      if AShowSourceLines then
      begin
        Sym := GetSymbolAtAddr(GdbInst.AddrVA);
        if Sym <> nil then
        try
          Inst.Mnemonic := FormatSymbol(Inst.AddrVA, Sym, qsSourceLine);
          if Inst.Mnemonic <> '' then
            Result.Add(Inst);
        finally
          Sym.ReleaseReference;
        end;
      end;
      Inst.Len := GdbInst.Len;
      Inst.Mnemonic := GdbInst.AsString;
      if GdbInst.IsRip and (GdbInst.ExternalAddrVA <> 0) then
        Inst.Hint := QuerySymbolAtAddr(GdbInst.ExternalAddrVA, qsName).Description
      else
      begin
        Inst.Hint := QuerySymbolAtAddr(GdbInst.InstrTargetVA, qsName).Description;
        GdbInst.ExternalAddrVA := GdbInst.InstrTargetVA;
      end;

      FormatInstruction(Inst, GdbInst, AShowCallFuncName);

      Result.Add(Inst);
    end;
  finally
    gdb.UnLockCommandProcessing;
  end;
end;

function TGdbAbstractDebugGate.IsDebuggerLocked: Boolean;
begin
  Result := inherited IsDebuggerLocked;
  if not Result then
  begin
    if gdb = nil then Exit;
    Result := gdb.Waiting;
  end;
end;

function TGdbAbstractDebugGate.GetRemoteModuleHandle(const ALibraryName: string
  ): TRemoteModule;
begin
  Result := DebugStorage.GetRemoteModuleHandle(ALibraryName);
end;

function TGdbAbstractDebugGate.GetRemoteModules: TList<TRemoteModule>;
begin
  Result := DebugStorage.GetRemoteModules;
end;

function TGdbAbstractDebugGate.GetRemoteProcList(const AModule: TRemoteModule): TList<
  TRemoteProc>;
begin
  Result := DebugStorage.GetRemoteProcList(AModule);
end;

function TGdbAbstractDebugGate.GetRemoteProcAddress(const AModule: TRemoteModule;
  const AProcName: string): Int64;
begin
  Result := DebugStorage.GetRemoteProcAddress(AModule, AProcName);
end;

end.
