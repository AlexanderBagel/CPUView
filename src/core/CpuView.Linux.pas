////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Linux.pas
//  * Purpose   : Implementing Linux-specific code.
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

unit CpuView.Linux;

{$I CpuViewCfg.inc}

{$MODE Delphi}

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

  FWHexView.Common,
  {$IFDEF USE_INTEL_CTX}
  CpuView.Context.Intel.Types,
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  CpuView.Context.Aarch64.Types,
  {$ENDIF}
  CpuView.Common;

type
  TMemoryBasicInformation = record
    AllocationBase, BaseAddress, RegionSize, OffsetInFile, InumIdx: Int64;
    Read, Write, Execute, Shared: Boolean;
    MappedFile, Hint: string;
  end;

  TMemoryBasicInformationList = class(TListEx<TMemoryBasicInformation>);

  { TCommonUtils }

  TCommonUtils = class(TCommonAbstractUtils)
  private
    FMemory: TFileStream;
    FMemList: TMemoryBasicInformationList;
    function GetMemList: TMemoryBasicInformationList;
  protected
    procedure SetProcessID(const Value: Integer); override;
    function VirtualQueryMBI(AQueryAddr: Int64; out
      MBI: TMemoryBasicInformation): Boolean;
  public
    destructor Destroy; override;
    function GetPageSize: Integer; override;
    function GetThreadExtendedData({%H-}AThreadID: Integer; {%H-}ThreadIs32: Boolean): TThreadExtendedData; override;
    function GetThreadStackLimit(AThreadID: Integer; ThreadIs32: Boolean): TStackLimit; override;
    function NeedUpdateReadData: Boolean; override;
    function QueryLoadedImages({%H-}AProcess32: Boolean): TImageDataList; override;
    function QueryModuleName(AddrVA: Int64; out AModuleName: string): Boolean; override;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean; override;
    function ReadData(AddrVA: Pointer; var Buff; ASize: Longint): Longint; override;
    function SetPageAccess(AddrVA: Pointer; Size: Integer; Flags: Cardinal): Boolean; override;
    function SetThreadExtendedData({%H-}AThreadID: Integer; {%H-}ThreadIs32: Boolean; const {%H-}AData: TThreadExtendedData): Boolean; override;
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

  { TThreadWorkerReadData }

  TThreadWorkerReadData = class(TFpDbgDebggerThreadWorkerItem)
  private
    FAddrVA: TDBGPtr;
    FBuff: Pointer;
    FSize, FResult: Cardinal;
  protected
    procedure DoExecute; override;
  public
    constructor Create(ADebugger: TFpDebugDebuggerBase;
      AAddrVA: TDBGPtr; ABuff: Pointer; ASize: Cardinal);
    property ThreadResult: Cardinal read FResult;
  end;

  {$IFDEF USE_INTEL_CTX}
  function GetIntelContext(AThreadID: DWORD): TIntelThreadContext;
  function SetIntelContext(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
  function GetIntelWow64Context(AThreadID: DWORD): TIntelThreadContext;
  function SetIntelWow64Context(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
  {$ENDIF}

  function LoadVirtualMemoryInformation(AProcessID, AThreadID: Integer;
    AddFirstEmpty: Boolean = True): TMemoryBasicInformationList;

var
  LinuxDebugger: TFpDebugDebuggerBase;

implementation

type
  user_fpregs_struct32 = record
      cwd : longint;
      swd : longint;
      twd : longint;
      fip : longint;
      fcs : longint;
      foo : longint;
      fos : longint;
      st_space : array[0..79] of Byte;
    end;

  puser_fpxregs_struct32 = ^user_fpxregs_struct32;
  puser_fpregs_struct64 = ^user_fpregs_struct64;

  // https://github.com/STMicroelectronics/gnu-tools-for-stm32/blob/12.3.rel1/src/gdb/gdbsupport/x86-xstate.h
  PXSaveHeader = ^_XSaveHeader;
  _XSaveHeader = record
    xsave_bv: qword;
    xcomp_bv: qword;
    reserved: array [0..5] of qword;
  end;

const
  X86_XSTATE_X87 = 1;
  X86_XSTATE_SSE = 2;
  X86_XSTATE_AVX = 4;

  X86_XSTATE_X87_MASK = X86_XSTATE_X87;
  X86_XSTATE_SSE_MASK = X86_XSTATE_X87 or X86_XSTATE_SSE;
  X86_XSTATE_AVX_MASK = X86_XSTATE_SSE_MASK or X86_XSTATE_AVX;

  _SC_PAGESIZE = 30;

  function sysconf (__name : longint) : longint; cdecl; external 'c';

function GetXSaveHeader(XSaveArea: PByte): PXSaveHeader;
begin
  Result := PXSaveHeader(XSaveArea + 512);
end;

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
      I := F.Read(LoadBuff[0], 4096);
      if I > 0 then
        Result.WriteBuffer(LoadBuff[0], I);
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

function ScanDec(const buff: PByte; out Value: Int64): PByte;
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

function ScanHex(const buff: PByte; out Value: Int64): PByte;
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
    {.$message 'If you do not make a cast to byte, Digit will go to RAX and subtraction with comparation will be from RAX and everything will be floating'}
    {.$message 'Если не сделать каст к байту то Digit пойдет в RAX и вычитание с компарацией будет из RAX и все поплывет'}
    // т.к. должно быть в EAX который после MOVZX чистится
    // because it must be in EAX which is cleared after MOVZX.
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

function DefaultMBIComparer({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} A, B: TMemoryBasicInformation): Integer;

begin
  if QWord(A.BaseAddress) < QWord(B.BaseAddress) then
    Result := -1
  else
    if A.BaseAddress = B.BaseAddress then
      Result := 0
    else
      Result := 1;
end;

function LoadVirtualMemoryInformation(AProcessID, AThreadID: Integer;
  AddFirstEmpty: Boolean): TMemoryBasicInformationList;
var
  MapsPath, Line: string;
  VMData: TStringList;
  M: TMemoryStream;
  InumDict: TDictionary<Int64, Int64>;
  FirstMBI, MBI: TMemoryBasicInformation;
  I: Integer;
  HighIdx, DummyIdx: Int64;
  buff: PByte;
  Delimiter: Char;
begin
  Result := TMemoryBasicInformationList.Create(TComparer<TMemoryBasicInformation>.Construct(@DefaultMBIComparer));
  if AThreadID = 0 then
    MapsPath := '/proc/' + IntToStr(AProcessID) + '/task/' + IntToStr(AProcessID) + '/maps'
  else
    MapsPath := '/proc/' + IntToStr(AProcessID) + '/task/' + IntToStr(AThreadID) + '/maps';
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
    InumDict := TDictionary<Int64, Int64>.Create;
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

        buff := ScanHex(buff, MBI.InumIdx);
        if MBI.InumIdx = 0 then
          MBI.AllocationBase := MBI.BaseAddress
        else
        begin
          if not InumDict.TryGetValue(MBI.InumIdx, MBI.AllocationBase) then
          begin
            MBI.AllocationBase := MBI.BaseAddress;
            InumDict.Add(MBI.InumIdx, MBI.AllocationBase);
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

        // special case... first empty region
        if AddFirstEmpty and (Result.Count = 0) then
        begin
          FirstMBI := Default(TMemoryBasicInformation);
          FirstMBI.RegionSize := MBI.AllocationBase;
          Result.Add(FirstMBI);
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
  Result := -1;
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

{$IFDEF USE_INTEL_CTX}
function IsAVXSupport: Boolean; assembler;
asm
  {$IFDEF CPUX64}
  push rbx
  {$ELSE}
  push ebx
  {$ENDIF}
  mov eax, 1
  cpuid
  test ecx, $10000000
  setnz al
  {$IFDEF CPUX64}
  pop rbx
  {$ELSE}
  pop ebx
  {$ENDIF}
end;

function GetXSaveArea: Cardinal; assembler;
asm
  {$IFDEF CPUX64}
  push rbx
  {$ELSE}
  push ebx
  {$ENDIF}
  mov eax, $D
  xor ecx, ecx
  cpuid
  mov eax, ebx
  {$IFDEF CPUX64}
  pop rbx
  {$ELSE}
  pop ebx
  {$ENDIF}
end;

function GetAVXOffset: Cardinal; assembler;
asm
  {$IFDEF CPUX64}
  push rbx
  {$ELSE}
  push ebx
  {$ENDIF}
  mov eax, $D
  mov ecx, 2
  cpuid
  mov eax, ebx
  {$IFDEF CPUX64}
  pop rbx
  {$ELSE}
  pop ebx
  {$ENDIF}
end;
{$ELSE}
function IsAVXSupport: Boolean; begin Result := False; end;
function GetXSaveArea: Cardinal; begin Result := 0; end;
function GetAVXOffset: Cardinal; begin Result := 0; end;
{$ENDIF USE_INTEL_CTX}

{$IFDEF USE_INTEL_CTX}
procedure LoadRegs32(AThreadID: DWORD; var AContext: TIntelThreadContext);
var
  io: iovec;
  Regs: TUserRegs;
 begin
  io.iov_base := @(Regs.regs32[0]);
  io.iov_len := SizeOf(Regs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    Exit;

  AContext.x86Context := True;
  AContext.Rax := Regs.regs32[EAX];
  AContext.Rbx := Regs.regs32[EBX];
  AContext.Rcx := Regs.regs32[ECX];
  AContext.Rdx := Regs.regs32[EDX];
  AContext.Rsp := Regs.regs32[UESP];
  AContext.Rbp := Regs.regs32[EBP];
  AContext.Rsi := Regs.regs32[ESI];
  AContext.Rdi := Regs.regs32[EDI];
  AContext.Rip := Regs.regs32[EIP];

  AContext.EFlags := Regs.regs32[EFL];
  AContext.SegGs := Regs.regs32[XGS];
  AContext.SegFs := Regs.regs32[XFS];
  AContext.SegEs := Regs.regs32[XES];
  AContext.SegDs := Regs.regs32[XDS];
  AContext.SegCs := Regs.regs32[XCS];
  AContext.SegSs := Regs.regs32[XSS];
end;

function SaveRegs32(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
var
  io: iovec;
  Regs: TUserRegs;
begin
  Result := False;

  io.iov_base := @Regs;
  io.iov_len := SizeOf(Regs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    Exit;

  Regs.regs32[EAX] := AContext.Rax;
  Regs.regs32[EBX] := AContext.Rbx;
  Regs.regs32[ECX] := AContext.Rcx;
  Regs.regs32[EDX] := AContext.Rdx;
  Regs.regs32[UESP] := AContext.Rsp;
  Regs.regs32[EBP] := AContext.Rbp;
  Regs.regs32[ESI] := AContext.Rsi;
  Regs.regs32[EDI] := AContext.Rdi;
  Regs.regs32[EIP] := AContext.Rip;

  Regs.regs32[EFL] := AContext.EFlags;

  Regs.regs32[XGS] := AContext.SegGs;
  Regs.regs32[XFS] := AContext.SegFs;
  Regs.regs32[XES] := AContext.SegEs;
  Regs.regs32[XDS] := AContext.SegDs;
  Regs.regs32[XCS] := AContext.SegCs;
  Regs.regs32[XSS] := AContext.SegSs;

  io.iov_base := @Regs;
  io.iov_len := SizeOf(Regs);
  Result := Do_fpPTrace(PTRACE_SETREGSET, AThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) = 0;
end;

procedure LoadMMX32(AThreadID: DWORD; var AContext: TIntelThreadContext);
var
  I: Integer;
  io: iovec;
  FpRegs: user_fpregs_struct32;
begin
  io.iov_base := @FpRegs;
  io.iov_len := SizeOf(FpRegs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRFPREG)), @io) <> 0 then
    Exit;

  AContext.ControlWord := FpRegs.cwd;
  AContext.StatusWord := FpRegs.swd;
  AContext.TagWord := FpRegs.twd;
  AContext.MMXPresent := True;
  for I := 0 to 7 do
    Move(FpRegs.st_space[I * 10], AContext.FloatRegisters[I], 10);
end;

function SaveMMX32(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
var
  I: Integer;
  io: iovec;
  FpRegs: user_fpregs_struct32;
begin
  Result := False;

  io.iov_base := @FpRegs;
  io.iov_len := SizeOf(FpRegs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRFPREG)), @io) <> 0 then
    Exit;

  FpRegs.cwd := AContext.ControlWord;
  FpRegs.swd := AContext.StatusWord;
  FpRegs.twd := AContext.TagWord;

  for I := 0 to 7 do
    Move(AContext.FloatRegisters[I], FpRegs.st_space[I * 10], 10);

  io.iov_base := @FpRegs;
  io.iov_len := SizeOf(FpRegs);
  Result := Do_fpPTrace(PTRACE_SETREGSET, AThreadID, Pointer(PtrUInt(NT_PRFPREG)), @io) = 0;
end;

procedure LoadAVX32(AThreadID: DWORD; var AContext: TIntelThreadContext);
var
  I: Integer;
  io: iovec;
  simdRegs: puser_fpxregs_struct32;
  XSaveArea, AVXOffset: Cardinal;
  XSaveAreaBuff: array of byte;
  XSaveHeader: PXSaveHeader;
begin
  if IsAVXSupport then
  begin
    XSaveArea := GetXSaveArea;
    AVXOffset := GetAVXOffset;
    AContext.XmmCount := Min(8, (XSaveArea - AVXOffset) div SizeOf(TXMMRegister));
    if AVXOffset + AContext.XmmCount * SizeOf(TXMMRegister) > XSaveArea then
      AVXOffset := 0;
  end
  else
  begin
    XSaveArea := SizeOf(user_fpxregs_struct32);
    AVXOffset := 0;
    AContext.XmmCount := 8;
  end;
  SetLength(XSaveAreaBuff{%H-}, XSaveArea);
  simdRegs := @XSaveAreaBuff[0];

  io.iov_base := simdRegs;
  io.iov_len := XSaveArea;
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_X86_XSTATE)), @io) <> 0 then
    Exit;

  AContext.MxCsr := simdRegs.mxcsr;

  if AVXOffset > 0 then
  begin
    XSaveHeader := GetXSaveHeader(@XSaveAreaBuff[0]);
    if XSaveHeader.xsave_bv and X86_XSTATE_AVX_MASK = 0 then
      Exit;
  end;

  // SSE
  for I := 0 to AContext.XmmCount - 1 do
    Move(simdRegs.xmm_space[I * 4], AContext.Ymm[I].Low, SizeOf(TXMMRegister));

  // AVX
  AContext.YmmPresent := AVXOffset > 0;
  if AContext.YmmPresent then
    for I := 0 to AContext.XmmCount - 1 do
      Move(XSaveAreaBuff[AVXOffset + I * SizeOf(TXMMRegister)], AContext.Ymm[I].High, SizeOf(TXMMRegister));
end;

function SaveAVX32(AThreadID: DWORD; {%H-}AContext: TIntelThreadContext;
  UpdateAVXRegister: Boolean): Boolean;
var
  I, XmmCount: Integer;
  io: iovec;
  XSaveArea, AVXOffset: Cardinal;
  XSaveHeader: PXSaveHeader;
  XSaveAreaBuff: array of Byte;
  simdRegs: puser_fpxregs_struct32;
begin
  Result := False;
  AVXOffset := 0;
  if IsAVXSupport then
  begin
    XSaveArea := GetXSaveArea;
    AVXOffset := GetAVXOffset;
    XmmCount := Min(8, (XSaveArea - AVXOffset) div SizeOf(TXMMRegister));
    if AVXOffset + XmmCount * SizeOf(TXMMRegister) > XSaveArea then
      AVXOffset := 0;
  end;

  if AVXOffset = 0 then
  begin
    XSaveArea := SizeOf(user_fpregs_struct32);
    AVXOffset := 0;
    XmmCount := 8;
  end;

  SetLength(XSaveAreaBuff{%H-}, XSaveArea);
  simdRegs := @XSaveAreaBuff[0];

  io.iov_base := simdRegs;
  io.iov_len := XSaveArea;
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_X86_XSTATE)), @io) <> 0 then
    Exit;

  if AVXOffset > 0 then
  begin
    XSaveHeader := GetXSaveHeader(@XSaveAreaBuff[0]);
    XSaveHeader.xsave_bv := XSaveHeader.xsave_bv or X86_XSTATE_SSE;
    if UpdateAVXRegister then
      XSaveHeader.xsave_bv := XSaveHeader.xsave_bv or X86_XSTATE_AVX;
  end;

  simdRegs.mxcsr := AContext.MxCsr;
  for I := 0 to XmmCount - 1 do
    Move(AContext.Ymm[I].Low, simdRegs.xmm_space[I * 4], SizeOf(TXMMRegister));
  if AVXOffset > 0 then
    for I := 0 to XmmCount - 1 do
      Move(AContext.Ymm[I].High, XSaveAreaBuff[AVXOffset + I * SizeOf(TXMMRegister)], SizeOf(TXMMRegister));

  io.iov_base := simdRegs;
  io.iov_len := XSaveArea;
  Result := Do_fpPTrace(PTRACE_SETREGSET, AThreadID, Pointer(PtrUInt(NT_X86_XSTATE)), @io) = 0;
end;

procedure LoadRegs64(AThreadID: DWORD; var AContext: TIntelThreadContext);
var
  io: iovec;
  Regs: TUserRegs;
begin
  io.iov_base := @Regs;
  io.iov_len := SizeOf(Regs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    Exit;

  AContext.Rax := Regs.regs64[RAX];
  AContext.Rbx := Regs.regs64[RBX];
  AContext.Rcx := Regs.regs64[RCX];
  AContext.Rdx := Regs.regs64[RDX];
  AContext.Rsp := Regs.regs64[RSP];
  AContext.Rbp := Regs.regs64[RBP];
  AContext.Rsi := Regs.regs64[RSI];
  AContext.Rdi := Regs.regs64[RDI];
  AContext.Rip := Regs.regs64[RIP];
  AContext.R[8] := Regs.regs64[R8];
  AContext.R[9] := Regs.regs64[R9];
  AContext.R[10] := Regs.regs64[R10];
  AContext.R[11] := Regs.regs64[R11];
  AContext.R[12] := Regs.regs64[R12];
  AContext.R[13] := Regs.regs64[R13];
  AContext.R[14] := Regs.regs64[R14];
  AContext.R[15] := Regs.regs64[R15];

  AContext.EFlags := Regs.regs64[EFLAGS];
  AContext.SegGs := Regs.regs64[GS];
  AContext.SegFs := Regs.regs64[FS];
  AContext.SegEs := Regs.regs64[ES];
  AContext.SegDs := Regs.regs64[DS];
  AContext.SegCs := Regs.regs64[CS];
  AContext.SegSs := Regs.regs64[SS];
end;

function SaveRegs64(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
var
  io: iovec;
  Regs: TUserRegs;
begin
  Result := False;

  io.iov_base := @Regs;
  io.iov_len := SizeOf(Regs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    Exit;

  Regs.regs64[RAX] := AContext.Rax;
  Regs.regs64[RBX] := AContext.Rbx;
  Regs.regs64[RCX] := AContext.Rcx;
  Regs.regs64[RDX] := AContext.Rdx;
  Regs.regs64[RSP] := AContext.Rsp;
  Regs.regs64[RBP] := AContext.Rbp;
  Regs.regs64[RSI] := AContext.Rsi;
  Regs.regs64[RDI] := AContext.Rdi;
  Regs.regs64[RIP] := AContext.Rip;
  Regs.regs64[R8] := AContext.R[8];
  Regs.regs64[R9] := AContext.R[9];
  Regs.regs64[R10] := AContext.R[10];
  Regs.regs64[R11] := AContext.R[11];
  Regs.regs64[R12] := AContext.R[12];
  Regs.regs64[R13] := AContext.R[13];
  Regs.regs64[R14] := AContext.R[14];
  Regs.regs64[R15] := AContext.R[15];

  Regs.regs64[EFLAGS] := AContext.EFlags;

  Regs.regs64[GS] := AContext.SegGs;
  Regs.regs64[FS] := AContext.SegFs;
  Regs.regs64[ES] := AContext.SegEs;
  Regs.regs64[DS] := AContext.SegDs;
  Regs.regs64[CS] := AContext.SegCs;
  Regs.regs64[SS] := AContext.SegSs;

  io.iov_base := @Regs;
  io.iov_len := SizeOf(Regs);
  Result := Do_fpPTrace(PTRACE_SETREGSET, AThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) = 0;
end;

procedure LoadMMX64(AThreadID: DWORD; var AContext: TIntelThreadContext);
var
  I: Integer;
  io: iovec;
  FpRegs: user_fpregs_struct64;
begin
  io.iov_base := @FpRegs;
  io.iov_len := SizeOf(FpRegs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRFPREG)), @io) <> 0 then
    Exit;

  AContext.ControlWord := FpRegs.cwd;
  AContext.StatusWord := FpRegs.swd;
  AContext.MxCsr := FpRegs.mxcsr;
  AContext.MMXPresent := True;

  for I := 0 to 7 do
    Move(FpRegs.st_space[I * 4], AContext.FloatRegisters[I], 10);

  AContext.TagWord := GetTagWordFromFXSave(FpRegs.swd, FpRegs.ftw, AContext.FloatRegisters);
end;

function SaveMMX64(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
var
  I: Integer;
  io: iovec;
  FpRegs: user_fpregs_struct64;
begin
  Result := False;

  io.iov_base := @FpRegs;
  io.iov_len := SizeOf(FpRegs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRFPREG)), @io) <> 0 then
    Exit;

  FpRegs.cwd := AContext.ControlWord;
  FpRegs.swd := AContext.StatusWord;
  FpRegs.mxcsr := AContext.MxCsr;
  FpRegs.ftw := GetFXSaveTagWordFromTagWord(AContext.TagWord);

  for I := 0 to 7 do
    Move(AContext.FloatRegisters[I], FpRegs.st_space[I * 4], 10);

  io.iov_base := @FpRegs;
  io.iov_len := SizeOf(FpRegs);
  Result := Do_fpPTrace(PTRACE_SETREGSET, AThreadID, Pointer(PtrUInt(NT_PRFPREG)), @io)= 0;
end;

procedure LoadAVX64(AThreadID: DWORD; var AContext: TIntelThreadContext);
var
  I: Integer;
  io: iovec;
  XSaveArea, AVXOffset: Cardinal;
  XSaveAreaBuff: array of byte;
  simdRegs: puser_fpregs_struct64;
  XSaveHeader: PXSaveHeader;
begin
  AVXOffset := 0;
  if IsAVXSupport then
  begin
    XSaveArea := GetXSaveArea;
    AVXOffset := GetAVXOffset;
    AContext.XmmCount := Min(16, (XSaveArea - AVXOffset) div SizeOf(TXMMRegister));
    if AVXOffset + AContext.XmmCount * SizeOf(TXMMRegister) > XSaveArea then
      AVXOffset := 0;
  end;

  if AVXOffset = 0 then
  begin
    XSaveArea := SizeOf(user_fpregs_struct64);
    AVXOffset := 0;
    AContext.XmmCount := 16;
  end;
  SetLength(XSaveAreaBuff{%H-}, XSaveArea);
  simdRegs := @XSaveAreaBuff[0];

  io.iov_base := simdRegs;
  io.iov_len := XSaveArea;
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, {%H-}Pointer(PtrUInt(NT_X86_XSTATE)), @io) <> 0 then
    Exit;

  if AVXOffset > 0 then
  begin
    XSaveHeader := GetXSaveHeader(@XSaveAreaBuff[0]);
    if XSaveHeader.xsave_bv and X86_XSTATE_AVX_MASK = 0 then
      Exit;
  end;

  // SSE
  for I := 0 to AContext.XmmCount - 1 do
    Move(simdRegs.xmm_space[I * 4], AContext.Ymm[I].Low, SizeOf(TXMMRegister));

  // AVX
  AContext.YmmPresent := AVXOffset > 0;
  if AContext.YmmPresent then
    for I := 0 to AContext.XmmCount - 1 do
      Move(XSaveAreaBuff[AVXOffset + I * SizeOf(TXMMRegister)], AContext.Ymm[I].High, SizeOf(TXMMRegister));
end;

function SaveAVX64(AThreadID: DWORD; const {%H-}AContext: TIntelThreadContext;
  UpdateAVXRegister: Boolean): Boolean;
var
  I, XMMCount: Integer;
  io: iovec;
  XSaveArea, AVXOffset: Cardinal;
  XSaveHeader: PXSaveHeader;
  XSaveAreaBuff: array of Byte;
  simdRegs: puser_fpregs_struct64;
begin
  Result := False;

  AVXOffset := 0;
  if IsAVXSupport then
  begin
    XSaveArea := GetXSaveArea;
    AVXOffset := GetAVXOffset;
    XmmCount := Min(16, (XSaveArea - AVXOffset) div SizeOf(TXMMRegister));
    if AVXOffset + XmmCount * SizeOf(TXMMRegister) > XSaveArea then
      AVXOffset := 0;
  end;

  if AVXOffset = 0 then
  begin
    XSaveArea := SizeOf(user_fpregs_struct64);
    AVXOffset := 0;
    XmmCount := 16;
  end;

  SetLength(XSaveAreaBuff{%H-}, XSaveArea);
  simdRegs := @XSaveAreaBuff[0];

  io.iov_base := simdRegs;
  io.iov_len := XSaveArea;
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_X86_XSTATE)), @io) <> 0 then
    Exit;

  if AVXOffset > 0 then
  begin
    XSaveHeader := GetXSaveHeader(@XSaveAreaBuff[0]);
    XSaveHeader.xsave_bv := XSaveHeader.xsave_bv or X86_XSTATE_SSE;
    if UpdateAVXRegister then
      XSaveHeader.xsave_bv := XSaveHeader.xsave_bv or X86_XSTATE_AVX;
  end;

  for I := 0 to XmmCount - 1 do
    Move(AContext.Ymm[I].Low, simdRegs.xmm_space[I * 4], SizeOf(TXMMRegister));
  if UpdateAVXRegister and (AVXOffset > 0) then
    for I := 0 to XmmCount - 1 do
      Move(AContext.Ymm[I].High, XSaveAreaBuff[AVXOffset + I * SizeOf(TXMMRegister)], SizeOf(TXMMRegister));

  io.iov_base := simdRegs;
  io.iov_len := XSaveArea;
  Result := Do_fpPTrace(PTRACE_SETREGSET, AThreadID, Pointer(PtrUInt(NT_X86_XSTATE)), @io) = 0;
end;

function GetIntelContext(AThreadID: DWORD): TIntelThreadContext;
begin
  Result := Default(TIntelThreadContext);

  {$IFDEF CPUX86}
  LoadRegs32(AThreadID, Result);
  LoadMMX32(AThreadID, Result);
  LoadAVX32(AThreadID, Result);
  {$ENDIF}

  {$IFDEF CPUX64}
  LoadRegs64(AThreadID, Result);
  LoadMMX64(AThreadID, Result);
  LoadAVX64(AThreadID, Result);
  {$ENDIF}
end;

function SetIntelContext(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
begin
  Result := False;
  case AContext.ContextLevel of
    0:
    begin
      {$IFDEF CPUX86}
      Result := SaveRegs32(AThreadID, AContext);
      {$ENDIF}

      {$IFDEF CPUX64}
      Result := SaveRegs64(AThreadID, AContext);
      {$ENDIF}
    end;
    1:
    begin
      {$IFDEF CPUX86}
      Result := SaveMMX32(AThreadID, AContext);
      {$ENDIF}

      {$IFDEF CPUX64}
      Result := SaveMMX64(AThreadID, AContext);
      {$ENDIF}
    end;
    2, 3:
    begin
      {$IFDEF CPUX86}
      Result := SaveAVX32(AThreadID, AContext, AContext.ContextLevel = 3);
      {$ENDIF}

      {$IFDEF CPUX64}
      Result := SaveAVX64(AThreadID, AContext, AContext.ContextLevel = 3);
      {$ENDIF}
    end;
  end;
end;

function GetIntelWow64Context(AThreadID: DWORD): TIntelThreadContext;
begin
  Result := Default(TIntelThreadContext);
  LoadRegs32(AThreadID, Result);
  LoadMMX32(AThreadID, Result);
  LoadAVX32(AThreadID, Result);
end;

function SetIntelWow64Context(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
begin
  Result := False;
  case AContext.ContextLevel of
    0: Result := SaveRegs32(AThreadID, AContext);
    1: Result := SaveMMX32(AThreadID, AContext);
    2, 3: Result := SaveAVX32(AThreadID, AContext, AContext.ContextLevel = 3);
  end;
end;
{$ENDIF USE_INTEL_CTX}

{ TCommonUtils }

function TCommonUtils.GetMemList: TMemoryBasicInformationList;
begin
  if FMemList = nil then
  begin
    if ProcessID <> 0 then
      FMemList := LoadVirtualMemoryInformation(ProcessID, ThreadID);
  end;
  Result := FMemList;
end;

procedure TCommonUtils.SetProcessID(const Value: Integer);
var
  MemPath: string;
begin
  inherited;
  FreeAndNil(FMemory);
  if (LinuxDebugger = nil) and (Value <> 0) then
  begin
    MemPath := '/proc/' + IntToStr(Value) + '/mem';
    if FileExists(MemPath) then
      FMemory := TFileStream.Create(MemPath, fmOpenRead or fmShareDenyWrite);
  end;
end;

function TCommonUtils.VirtualQueryMBI(AQueryAddr: Int64; out
  MBI: TMemoryBasicInformation): Boolean;
var
  List: TMemoryBasicInformationList;
  Index: SizeInt;
begin
  List := GetMemList;
  if List = nil then Exit(False);
  if List.Count = 0 then Exit(False);
  MBI.BaseAddress := AQueryAddr;
  Result := List.BinarySearch(MBI, Index);
  if Index < 0 then
  begin
    for Index := 0 to List.Count - 1 do
    begin
      MBI := List[Index];
      if (MBI.BaseAddress <= AQueryAddr) and
         (MBI.BaseAddress + MBI.RegionSize > AQueryAddr) then
        Exit(True);
    end;
    Exit;
  end;
  if Result then
    MBI := List[Index]
  else
  begin
    if Index > 0 then
      Dec(Index);
    MBI := List[Index];
    Result := (MBI.BaseAddress <= AQueryAddr) and
      (AQueryAddr < MBI.BaseAddress + MBI.RegionSize);
  end;
end;

destructor TCommonUtils.Destroy;
begin
  FMemList.Free;
  FMemory.Free;
  inherited Destroy;
end;

function TCommonUtils.GetPageSize: Integer;
begin
  Result := sysconf(_SC_PAGESIZE);
end;

function TCommonUtils.GetThreadExtendedData(AThreadID: Integer;
  ThreadIs32: Boolean): TThreadExtendedData;
begin
  Result := Default(TThreadExtendedData);
end;

function TCommonUtils.GetThreadStackLimit(AThreadID: Integer;
  ThreadIs32: Boolean): TStackLimit;
var
  io: iovec;
  Regs: TUserRegs;
  StackPointer: Int64;
  MBI: TMemoryBasicInformation;
begin
  Result := Default(TStackLimit);
  io.iov_base := @(Regs.regs32[0]);
  io.iov_len := SizeOf(Regs);
  if Do_fpPTrace(PTRACE_GETREGSET, AThreadID, Pointer(PtrUInt(NT_PRSTATUS)), @io) <> 0 then
    Exit;
  {$IFDEF CPUX32}
  StackPointer := Regs.regs32[UESP];
  {$ENDIF}
  {$IFDEF CPUX64}
  if ThreadIs32 then
    StackPointer := Regs.regs32[UESP]
  else
    StackPointer := Regs.regs64[RSP];
  {$ENDIF}
  if VirtualQueryMBI(StackPointer, MBI) then
  begin
    Result.Limit := MBI.BaseAddress;
    Result.Base := MBI.BaseAddress + MBI.RegionSize;
  end;
end;

function TCommonUtils.NeedUpdateReadData: Boolean;
begin
  Result := False;
end;

function TCommonUtils.QueryLoadedImages(AProcess32: Boolean): TImageDataList;
var
  I: Integer;
  ImageData: TImageData;
  List: TMemoryBasicInformationList;
  LastImagePath: string;
  MBI: TMemoryBasicInformation;
begin
  Result := TImageDataList.Create;
  List := GetMemList;
  if List = nil then Exit;
  LastImagePath := '';
  for I := 0 to List.Count - 1 do
  begin
    MBI := List[I];
    if not FileExists(MBI.MappedFile) then Continue;
    if LastImagePath = MBI.MappedFile then
      Inc(Result.List[Result.Count - 1].Size, MBI.RegionSize)
    else
    begin
      LastImagePath := MBI.MappedFile;
      ImageData.Size := MBI.RegionSize;
      ImageData.ImageBase := MBI.AllocationBase;
      ImageData.ImagePath := MBI.MappedFile;
      Result.Add(ImageData);
    end;
  end;
end;

function TCommonUtils.QueryModuleName(AddrVA: Int64; out AModuleName: string
  ): Boolean;
var
  MBI: TMemoryBasicInformation;
begin
  AModuleName := '';
  Result := VirtualQueryMBI(AddrVA, MBI);
  if Result then
  begin
    AModuleName := MBI.MappedFile;
    Result := FileExists(AModuleName);
  end;
end;

function TCommonUtils.QueryRegion(AddrVA: Int64; out RegionData: TRegionData
  ): Boolean;
var
  MBI: TMemoryBasicInformation;
begin
  RegionData := Default(TRegionData);
  Result := VirtualQueryMBI(AddrVA, MBI);
  if Result then
  begin
    RegionData.AllocationBase := MBI.AllocationBase;
    RegionData.BaseAddr := MBI.BaseAddress;
    RegionData.RegionSize := MBI.RegionSize;
    if MBI.Read then
      Include(RegionData.Access, raRead);
    if MBI.Write then
      Include(RegionData.Access, raWrite);
    if MBI.Execute then
      Include(RegionData.Access, raExecute);
  end;
end;

function TCommonUtils.ReadData(AddrVA: Pointer; var Buff; ASize: Longint
  ): Longint;
var
  WorkQueue: TFpThreadPriorityWorkerQueue;
  WorkItem: TThreadWorkerReadData;
  RemainingBytes: Longint;
  RemainingBuff: PByte;
begin
  Result := ASize;
  // Если нельзя читать, возвращаем пустой буфер
  // If it cannot be read, return an empty buffer
  if (LinuxDebugger = nil) or (LinuxDebugger.State <> dsPause) then
  begin
    if Assigned(FMemory) then
    begin
      try
        FMemory.Position := QWord(AddrVA);
        Result := FMemory.Read(Buff, ASize);
        Exit;
      except
        FillChar(Buff, ASize, 0);
      end;
    end;
    FillChar(Buff, ASize, 0);
    Exit;
  end;
  WorkQueue := TFpDebugDebugger(LinuxDebugger).WorkQueue;
  if Assigned(WorkQueue) then
  begin
    WorkItem := TThreadWorkerReadData.Create(
      TFpDebugDebugger(LinuxDebugger), {%H-}TDbgPtr(AddrVA), @Buff, Cardinal(ASize));
    WorkQueue.PushItem(WorkItem);
    WorkQueue.WaitForItem(WorkItem, True);
    Result := Longint(WorkItem.ThreadResult);
    WorkItem.DecRef;
  end;
  RemainingBytes := ASize - Result;
  if RemainingBytes > 0 then
  begin
    RemainingBuff := PByte(@Buff) + Result;
    FillChar(RemainingBuff^, RemainingBytes, 0);
    Result := ASize;
  end;
end;

function TCommonUtils.SetPageAccess(AddrVA: Pointer; Size: Integer;
  Flags: Cardinal): Boolean;
begin
  {$message 'SetPageAccess not yet implemented '}
  Result := False;
end;

function TCommonUtils.SetThreadExtendedData(AThreadID: Integer;
  ThreadIs32: Boolean; const AData: TThreadExtendedData): Boolean;
begin
  Result := False;
end;

procedure TCommonUtils.Update;
begin
  FreeAndNil(FMemList);
  if ProcessID = 0 then Exit;
  FMemList := LoadVirtualMemoryInformation(ProcessID, ThreadID);
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

{ TThreadWorkerReadData }

procedure TThreadWorkerReadData.DoExecute;
begin
  inherited DoExecute;
  LinuxDebugger.MemReader.ReadMemory(FAddrVA, FSize, FBuff, FResult);
end;

constructor TThreadWorkerReadData.Create(ADebugger: TFpDebugDebuggerBase;
  AAddrVA: TDBGPtr; ABuff: Pointer; ASize: Cardinal);
begin
  inherited Create(ADebugger, twpContinue);
  FAddrVA := AAddrVA;
  FBuff := ABuff;
  FSize := ASize;
end;

end.
