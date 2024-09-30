unit CpuView.Linux;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  baseunix,
  LCLType,
  LCLIntf,
  Classes,
  SysUtils,
  Math,
  Generics.Collections,
  Generics.Defaults,

  FpDbgLinuxClasses,
  FpDbgLinuxExtra,
  FpDebugDebuggerWorkThreads,
  FpDebugDebuggerUtils,
  FpDebugDebuggerBase,
  FpDebugDebugger,
  LazDebuggerIntfBaseTypes,

  CpuView.Common,
  CpuView.IntelContext.Types;

type
  TMemoryBasicInformation = record
    AllocationBase, BaseAddress, RegionSize, OffsetInFile: UInt64;
    Read, Write, Execute, Shared: Boolean;
    MappedFile, Hint: string;
  end;

  TMemoryBasicInformationList = class(TList<TMemoryBasicInformation>);

  { TCommonUtils }

  TCommonUtils = class(TCommonAbstractUtils)
  private
    FProcessHandle: THandle;
    FMemList: TMemoryBasicInformationList;
    function GetMemList: TMemoryBasicInformationList;
  protected
    function VirtualQueryMBI(AQueryAddr: UInt64; out
      MBI: TMemoryBasicInformation): Boolean;
    procedure SetProcessID(const Value: Integer); override;
  public
    destructor Destroy; override;
    function GetThreadStackLimit(ThreadID: Integer; ThreadIs32: Boolean): TStackLimit; override;
    function NeedUpdateReadData: Boolean; override;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean; override;
    function ReadData(AddrVA: Pointer; var Buff; ASize: Longint): Longint; override;
    procedure Update; override;
  end;

  { TThreadWorkerFpTrace }

  TThreadWorkerFpTrace = class(TFpDbgDebggerThreadWorkerItem)
  private
    Fptrace_request: cint;
    Fpid: TPid;
    Faddr: Pointer;
    Fdata: pointer;
    FResult: PtrInt;
  protected
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase;
      ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer);
    property ThreadResult: PtrInt read FResult;
  end;

  function GetIntelContext(ThreadID: DWORD): TIntelThreadContext;
  function SetIntelContext(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
  function GetIntelWow64Context(ThreadID: DWORD): TIntelThreadContext;
  function SetIntelWow64Context(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;

var
  LinuxDebugger: TFpDebugDebuggerBase;

implementation

function ReadVirtualFile(const FilePath: string): TMemoryStream;
var
  F: TFileStream;
  LoadBuff: array of Byte;
  I: Integer;
begin
  Result := TMemoryStream.Create;
  if not FileExists(FilePath) then Exit;
  F := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LoadBuff{%H-}, 4096);
    repeat
      I := F.Read(LoadBuff, 4096);
      if I > 0 then
        Result.WriteBuffer(LoadBuff, I);
    until I = 0;
  finally
    F.Free;
  end;
  Result.Position := 0;
end;

function SkipChar(const buff: PByte): PByte;
begin
  Result := buff;
  if Result = nil then Exit;
  if Result^ = 0 then
    Result := nil
  else
    Inc(Result);
end;

function SkipWhiteSpace(const buff: PByte): PByte;
begin
  Result := buff;
  while Assigned(Result) and (Result^ in [9, 32]) do
    Inc(Result);
  if Result^ = 0 then
    Result := nil;
end;

function ScanDec(const buff: PByte; out Value: UInt64): PByte;
var
  Digit: Byte;
begin
  Value := 0;
  Digit := 0;
  Result := SkipWhiteSpace(buff);
  if Result = nil then Exit;
  while True do
  begin
    Digit := PByte(Result)^;
    if Byte(Digit - Byte('0')) <= 9 then
      Dec(Digit, Byte('0'))
    else
      Break;
    Value := (Value * 10) + Digit;
    Inc(Result);
  end;
end;

function ScanHex(const buff: PByte; out Value: UInt64): PByte;
var
  Digit: Byte;
begin
  Value := 0;
  Digit := 0;
  Result := SkipWhiteSpace(buff);
  if Result = nil then Exit;
  while True do
  begin
    Digit := PByte(Result)^;
    {$message 'Если не сделать каст к байту то Digit пойдет в RAX и вычитание с компарацией будет из RAX и все поплывет'}
    // т.к. должно быть в EAX который после MOVZX чистится
    if Byte(Digit - Byte('0')) <= 9 then
      Dec(Digit, Byte('0'))
    else if Byte(Digit - Byte('a')) < 6 then
      Dec(Digit, Byte('a') - 10)
    else if Byte(Digit - Byte('A')) < 6 then
      Dec(Digit, Byte('A') - 10)
    else
      Break;
    Value := (Value shl 4) or Digit;
    Inc(Result);
  end;
end;

function ScanChar(const buff: PByte; var AChar: Char): PByte;
begin
  Result := buff;
  AChar := #0;
  if Result = nil then Exit;
  AChar := Char(Result^);
  if AChar = #0 then
    Result := nil
  else
    Inc(Result);
end;

function ScanAccess(const buff: PByte; out Read, Write, Execute, Shared: Boolean): PByte;
var
  AccessChar: Char;
begin
  Read := False;
  Write := False;
  Execute := False;
  Shared := False;
  Result := SkipWhiteSpace(buff);
  if Result = nil then Exit;
  AccessChar := #0;
  Result := ScanChar(Result, AccessChar);
  Read := LowerCase(AccessChar) = 'r';
  Result := ScanChar(Result, AccessChar);
  Write := LowerCase(AccessChar) = 'w';
  Result := ScanChar(Result, AccessChar);
  Execute := LowerCase(AccessChar) = 'x';
  Result := ScanChar(Result, AccessChar);
  Shared := LowerCase(AccessChar) = 's';
end;

function LoadVirtualMemoryInformation(AProcessID: Integer): TMemoryBasicInformationList;
var
  MapsPath, Line: string;
  VMData: TStringList;
  M: TMemoryStream;
  InumDict: TDictionary<UInt64, UInt64>;
  MBI: TMemoryBasicInformation;
  I: Integer;
  HighIdx, DummyIdx, InumIdx: UInt64;
  buff: PByte;
  Delimiter: Char;
begin
  Result := TMemoryBasicInformationList.Create(TComparer<TMemoryBasicInformation>.Construct(
    function (const A, B: TMemoryBasicInformation): Integer
    begin
      if A.BaseAddress < B.BaseAddress then
        Result := -1
      else
        if A.BaseAddress = B.BaseAddress then
          Result := 0
        else
          Result := 1;
    end));
  MapsPath := '/proc/' + IntToStr(AProcessID) + '/maps';
  if not FileExists(MapsPath) then Exit;

  VMData := TStringList.Create;
  try

    M := ReadVirtualFile(MapsPath);
    try
      VMData.LoadFromStream(M);
    finally
      M.Free;
    end;

    MBI := Default(TMemoryBasicInformation);
    InumDict := TDictionary<UInt64, UInt64>.Create;
    try
      for I := 0 to VMData.Count - 1 do
      begin
        Line := VMData[I];
        if Line = '' then
          Continue;

        buff := PByte(@Line[1]);
        buff := ScanHex(buff, MBI.BaseAddress);
        buff := ScanChar(buff, Delimiter{%H-});
        buff := ScanHex(buff, HighIdx);
        MBI.RegionSize := HighIdx - MBI.BaseAddress;
        buff := ScanAccess(buff, MBI.Read, MBI.Write, MBI.Execute, MBI.Shared);
        buff := ScanHex(buff, MBI.OffsetInFile);

        buff := ScanHex(buff, DummyIdx);
        buff := ScanChar(buff, Delimiter);
        buff := ScanHex(buff, DummyIdx);

        buff := ScanHex(buff, InumIdx);
        if InumIdx = 0 then
          MBI.AllocationBase := MBI.BaseAddress
        else
        begin
          if not InumDict.TryGetValue(InumIdx, MBI.AllocationBase) then
          begin
            MBI.AllocationBase := MBI.BaseAddress;
            InumDict.Add(InumIdx, MBI.AllocationBase);
          end;
        end;

        buff := SkipWhiteSpace(buff);

        MBI.MappedFile := '';
        MBI.Hint := '';
        if buff <> nil then
        begin
          if buff^ = Byte('[') then
            MBI.Hint := PChar(buff)
          else
            MBI.MappedFile := PChar(buff);
        end;

        Result.Add(MBI);

      end;
    finally
      InumDict.Free;
    end;

  finally
    VMData.Free;
  end;
end;

function Do_fpPTrace(ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer): PtrInt;
var
  WorkQueue: TFpThreadPriorityWorkerQueue;
  WorkItem: TThreadWorkerFpTrace;
begin
  if LinuxDebugger = nil then Exit;
  if LinuxDebugger.State <> dsPause then Exit;
  WorkQueue := TFpDebugDebugger(LinuxDebugger).WorkQueue;
  if Assigned(WorkQueue) then
  begin
    WorkItem := TThreadWorkerFpTrace.Create(
      TFpDebugDebugger(LinuxDebugger), ptrace_request, pid, addr, data);
    WorkQueue.PushItem(WorkItem);
    WorkQueue.WaitForItem(WorkItem, True);
    Result := WorkItem.ThreadResult;
    WorkItem.DecRef;
  end;
end;

function GetIntelContext(ThreadID: DWORD): TIntelThreadContext;
var
  I: Integer;
  io: iovec;
  Regs: TUserRegs;
  FpRegs: user_fpxregs_struct32;
begin
  Result := Default(TIntelThreadContext);
  io.iov_base := @(Regs.regs32[0]);
  io.iov_len := SizeOf(Regs);
  if Do_fpPTrace(PTRACE_GETREGSET, ThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    Exit;

  {$IFDEF CPUX86}
  Result.x86Context := True;
  Result.Rax := Regs.regs32[EAX];
  Result.Rbx := Regs.regs32[EBX];
  Result.Rcx := Regs.regs32[ECX];
  Result.Rdx := Regs.regs32[EDX];
  Result.Rsp := Regs.regs32[UESP];
  Result.Rbp := Regs.regs32[EBP];
  Result.Rsi := Regs.regs32[ESI];
  Result.Rdi := Regs.regs32[EDI];
  Result.Rip := Regs.regs32[EIP];

  Result.EFlags := Regs.regs64[EFL];
  Result.SegGs := Regs.regs64[XGS];
  Result.SegFs := Regs.regs64[XFS];
  Result.SegEs := Regs.regs64[XES];
  Result.SegDs := Regs.regs64[XDS];
  Result.SegCs := Regs.regs64[XCS];
  Result.SegSs := Regs.regs64[XSS];
  {$ENDIF}

  {$IFDEF CPUX64}
  Result.Rax := Regs.regs64[RAX];
  Result.Rbx := Regs.regs64[RBX];
  Result.Rcx := Regs.regs64[RCX];
  Result.Rdx := Regs.regs64[RDX];
  Result.Rsp := Regs.regs64[RSP];
  Result.Rbp := Regs.regs64[RBP];
  Result.Rsi := Regs.regs64[RSI];
  Result.Rdi := Regs.regs64[RDI];
  Result.Rip := Regs.regs64[RIP];
  Result.R[8] := Regs.regs64[R8];
  Result.R[9] := Regs.regs64[R9];
  Result.R[10] := Regs.regs64[R10];
  Result.R[11] := Regs.regs64[R11];
  Result.R[12] := Regs.regs64[R12];
  Result.R[13] := Regs.regs64[R13];
  Result.R[14] := Regs.regs64[R14];
  Result.R[15] := Regs.regs64[R15];

  Result.EFlags := Regs.regs64[EFLAGS];
  Result.SegGs := Regs.regs64[GS];
  Result.SegFs := Regs.regs64[FS];;
  Result.SegEs := Regs.regs64[ES];
  Result.SegDs := Regs.regs64[DS];
  Result.SegCs := Regs.regs64[CS];
  Result.SegSs := Regs.regs64[SS];
  {$ENDIF}

  io.iov_base := @FpRegs;
  io.iov_len := SizeOf(FpRegs);
  if Do_fpPTrace(PTRACE_GETREGSET, ThreadID, Pointer(PtrUInt(NT_PRFPREG)), @io) <> 0 then
    Exit;

  Result.ControlWord := FpRegs.cwd;
  Result.StatusWord := FpRegs.swd;
  Result.TagWord := FpRegs.twd;
  //Result.ErrorOffset := Ctx.FltSave.ErrorOffset;
  //Result.ErrorSelector := Ctx.FltSave.ErrorSelector;
  //Result.DataOffset := Ctx.FltSave.DataOffset;
  //Result.DataSelector := Ctx.FltSave.DataSelector;
  Result.MxCsr := FpRegs.mxcsr;
  for I := 0 to 7 do
    Move(FpRegs.st_space[I * 4], Result.FloatRegisters[I * 10], 10);
  Result.XmmCount := 8;
  for I := 0 to Result.XmmCount - 1 do
    Move(FpRegs.xmm_space[I * 4], Result.Ymm[I].Low, 16);
  Result.YmmPresent := False;
end;

function SetIntelContext(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
begin
  Result := False;
end;

function GetIntelWow64Context(ThreadID: DWORD): TIntelThreadContext;
begin
  Result := Default(TIntelThreadContext);
end;

function SetIntelWow64Context(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
begin
  Result := False;
end;

{ TCommonUtils }

function TCommonUtils.GetMemList: TMemoryBasicInformationList;
begin
  if FMemList = nil then
    FMemList := LoadVirtualMemoryInformation(ProcessID);
  Result := FMemList;
end;

function TCommonUtils.VirtualQueryMBI(AQueryAddr: UInt64; out
  MBI: TMemoryBasicInformation): Boolean;
var
  List: TMemoryBasicInformationList;
  Index: SizeInt;
begin
  List := GetMemList;
  if List.Count = 0 then Exit(False);
  MBI.BaseAddress := AQueryAddr;
  Result := List.BinarySearch(MBI, Index);
  if Result then
    MBI := List[Index]
  else
  begin
    MBI := List[Index - 1];
    Result := (MBI.BaseAddress <= AQueryAddr) and
      (AQueryAddr < MBI.BaseAddress + MBI.RegionSize);
  end;
end;

procedure TCommonUtils.SetProcessID(const Value: Integer);
begin
  inherited SetProcessID(Value);
end;

destructor TCommonUtils.Destroy;
begin
  FMemList.Free;
  inherited Destroy;
end;

function TCommonUtils.GetThreadStackLimit(ThreadID: Integer; ThreadIs32: Boolean
  ): TStackLimit;
var
  io: iovec;
  Regs: TUserRegs;
  StackBase: UInt64;
  MBI: TMemoryBasicInformation;
begin
  Result := Default(TStackLimit);
  io.iov_base := @(Regs.regs32[0]);
  io.iov_len := SizeOf(Regs);
  if Do_fpPTrace(PTRACE_GETREGSET, ThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    Exit;
  {$IFDEF CPUX32}
  StackBase := Max(Regs.regs32[UESP], Regs.regs32[EBP]);
  {$ENDIF}
  {$IFDEF CPUX64}
  StackBase := Max(Regs.regs64[RSP], Regs.regs64[RBP]);
  {$ENDIF}
  if VirtualQueryMBI(StackBase, MBI) then
  begin
    Result.Limit := MBI.BaseAddress;
    Result.Base := MBI.BaseAddress + MBI.RegionSize;
  end;
end;

function TCommonUtils.NeedUpdateReadData: Boolean;
begin
  Result := False;
end;

function TCommonUtils.QueryRegion(AddrVA: Int64; out RegionData: TRegionData
  ): Boolean;
var
  MBI: TMemoryBasicInformation;
begin
  Result := VirtualQueryMBI(AddrVA, MBI);
  if Result then
  begin
    RegionData.AllocationBase := MBI.AllocationBase;
    RegionData.BaseAddr := MBI.BaseAddress;
    RegionData.RegionSize := MBI.RegionSize;
  end;
end;

function TCommonUtils.ReadData(AddrVA: Pointer; var Buff; ASize: Longint
  ): Longint;
begin

end;

procedure TCommonUtils.Update;
begin
  FreeAndNil(FMemList);
  if ProcessID = 0 then Exit;
  FMemList := LoadVirtualMemoryInformation(ProcessID);
end;

{ TThreadWorkerFpTrace }

procedure TThreadWorkerFpTrace.DoExecute;
begin
  FResult := fpPTrace(Fptrace_request, Fpid, Faddr, Fdata);
end;

constructor TThreadWorkerFpTrace.Create(ADebugger: TFpDebugDebuggerBase;
  ptrace_request: cint; pid: TPid; addr: Pointer; data: pointer);
begin
  inherited Create(ADebugger, twpContinue);
  Fptrace_request := ptrace_request;
  Fpid := pid;
  Faddr := addr;
  Fdata := data;
end;

end.
