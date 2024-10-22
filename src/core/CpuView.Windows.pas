unit CpuView.Windows;

{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 5028 off : Local $1 "$2" is not used}
  {$WARN 4082 off : Converting pointers to signed integers may result in wrong comparison results and range errors, use an unsigned type instead.}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLType,
  LCLIntf,
  {$ENDIF}
  Windows,
  CpuView.Common,
  CpuView.IntelContext.Types;

type

  { TCommonUtils }

  TCommonUtils = class(TCommonAbstractUtils)
  private
    FProcessHandle: THandle;
  protected
    procedure SetProcessID(const Value: Integer); override;
  public
    destructor Destroy; override;
    function GetThreadExtendedData(ThreadID: Integer; ThreadIs32: Boolean): TThreadExtendedData; override;
    function GetThreadStackLimit(ThreadID: Integer; ThreadIs32: Boolean): TStackLimit; override;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean; override;
    function ReadData(AddrVA: Pointer; var Buff; ASize: Longint): Longint; override;
    function SetThreadExtendedData(ThreadID: Integer; ThreadIs32: Boolean; const AData: TThreadExtendedData): Boolean; override;
    procedure Update; override;
  end;

  function GetIntelContext(ThreadID: DWORD): TIntelThreadContext;
  function SetIntelContext(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
  function GetIntelWow64Context(ThreadID: DWORD): TIntelThreadContext;
  function SetIntelWow64Context(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;

implementation


const
  SIZE_OF_80387_REGISTERS = 80;
  MAXIMUM_SUPPORTED_EXTENSION = 512;
  WOW64_SIZE_OF_80387_REGISTERS = 80;
  WOW64_MAXIMUM_SUPPORTED_EXTENSION = 512;
  WOW64_CONTEXT_i386 = $00010000;
  WOW64_CONTEXT_i486 = $00010000;
  WOW64_CONTEXT_CONTROL = LongInt(WOW64_CONTEXT_i386 or $00000001);
  WOW64_CONTEXT_INTEGER = LongInt(WOW64_CONTEXT_i386 or $00000002);
  WOW64_CONTEXT_SEGMENTS = LongInt(WOW64_CONTEXT_i386 or $00000004);
  WOW64_CONTEXT_FLOATING_POINT = LongInt(WOW64_CONTEXT_i386 or $00000008);
  WOW64_CONTEXT_DEBUG_REGISTERS = LongInt(WOW64_CONTEXT_i386 or $00000010);
  WOW64_CONTEXT_EXTENDED_REGISTERS = LongInt(WOW64_CONTEXT_i386 or $00000020);
  WOW64_CONTEXT_FULL = (WOW64_CONTEXT_CONTROL or WOW64_CONTEXT_INTEGER or WOW64_CONTEXT_SEGMENTS);
  WOW64_CONTEXT_WITHOUT_EXTENDED = (WOW64_CONTEXT_FULL or  WOW64_CONTEXT_FLOATING_POINT or WOW64_CONTEXT_DEBUG_REGISTERS);
  WOW64_CONTEXT_ALL = (WOW64_CONTEXT_WITHOUT_EXTENDED or WOW64_CONTEXT_EXTENDED_REGISTERS);
  WOW64_CONTEXT_XSTATE = LongInt(WOW64_CONTEXT_i386 or $00000040);
  WOW64_CONTEXT_EXCEPTION_ACTIVE = $08000000;
  WOW64_CONTEXT_SERVICE_ACTIVE = $10000000;
  WOW64_CONTEXT_EXCEPTION_REQUEST = $40000000;
  WOW64_CONTEXT_EXCEPTION_REPORTING = $80000000;

type
  LONG = Longint;
  ULONG = Cardinal;
  NTSTATUS = LONG;

  _CLIENT_ID = record
    UniqueProcess: THandle;
    UniqueThread: THandle;
  end;
  CLIENT_ID = _CLIENT_ID;
  PCLIENT_ID = ^CLIENT_ID;
  TClientID = CLIENT_ID;
  PClientID = ^TClientID;

  PNT_TIB = ^_NT_TIB;
  _NT_TIB = record
    ExceptionList: Pointer; // ^_EXCEPTION_REGISTRATION_RECORD
    StackBase,
    StackLimit,
    SubSystemTib: Pointer;
    case Integer of
      0: (
        FiberData: Pointer
        );
      1: (
        Version: ULONG;
        ArbitraryUserPointer: Pointer;
        Self: PNT_TIB;
        )
  end;
  NT_TIB = _NT_TIB;
  PPNT_TIB = ^PNT_TIB;

  TWOW64_NT_TIB = record
    ExceptionList,
    StackBase,
    StackLimit,
    SubSystemTib,
    Version,
    ArbitraryUserPointer,
    Self: DWORD;
  end;

  KAFFINITY = ULONG_PTR;
  KPRIORITY = LONG;

  _THREAD_BASIC_INFORMATION = record // Information Class 0
    ExitStatus: NTSTATUS;
    TebBaseAddress: PNT_TIB;
    ClientId: CLIENT_ID;
    AffinityMask: KAFFINITY;
    Priority: KPRIORITY;
    BasePriority: KPRIORITY;
  end;
  THREAD_BASIC_INFORMATION = _THREAD_BASIC_INFORMATION;
  PTHREAD_BASIC_INFORMATION = ^THREAD_BASIC_INFORMATION;
  TThreadBasicInformation = THREAD_BASIC_INFORMATION;
  PThreadBasicInformation = ^TThreadBasicInformation;

  TWow64FloatingSaveArea = record
    ControlWord: DWORD;
    StatusWord: DWORD;
    TagWord: DWORD;
    ErrorOffset: DWORD;
    ErrorSelector: DWORD;
    DataOffset: DWORD;
    DataSelector: DWORD;
    RegisterArea: array[0..SIZE_OF_80387_REGISTERS - 1] of Byte;
    Cr0NpxState: DWORD;
  end;

  PWow64Context = ^TWow64Context;
  TWow64Context = record
    ContextFlags: DWORD;
    Dr0: DWORD;
    Dr1: DWORD;
    Dr2: DWORD;
    Dr3: DWORD;
    Dr6: DWORD;
    Dr7: DWORD;
    FloatSave: TWow64FloatingSaveArea;
    SegGs: DWORD;
    SegFs: DWORD;
    SegEs: DWORD;
    SegDs: DWORD;
    Edi: DWORD;
    Esi: DWORD;
    Ebx: DWORD;
    Edx: DWORD;
    Ecx: DWORD;
    Eax: DWORD;
    Ebp: DWORD;
    Eip: DWORD;
    SegCs: DWORD;
    EFlags: DWORD;
    Esp: DWORD;
    SegSs: DWORD;
    ExtendedRegisters: array[0..MAXIMUM_SUPPORTED_EXTENSION-1] of Byte;
  end;

const
  ThreadBasicInformation = 0;
  THREAD_GET_CONTEXT = 8;
  THREAD_SET_CONTEXT = 16;
  THREAD_SUSPEND_RESUME = 2;
  THREAD_QUERY_INFORMATION = $40;

  CONTEXT_i386 = $10000;
  CONTEXT_EXTENDED_REGISTERS = $20;

  XSTATE_LEGACY_SSE = 1;
  XSTATE_AVX = 2;
  XSTATE_MASK_AVX = 4;
  CONTEXT_ALL = CONTEXT_FULL or CONTEXT_FLOATING_POINT or CONTEXT_DEBUG_REGISTERS;

  function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
    dwThreadId: DWORD): THandle; stdcall; external kernel32;
  function GetThreadContext(hThread: THandle; Context: PContext): BOOL;
    stdcall; external kernel32;
  function GetEnabledXStateFeatures: Int64; stdcall; external kernel32;
  function InitializeContext(Buffer: Pointer; ContextFlags: DWORD;
    Context: PContext; var ContextLength: DWORD): BOOL; stdcall; external kernel32;
  function SetXStateFeaturesMask(Context: PContext; FeatureMask: Int64): BOOL;
    stdcall; external kernel32;
  function GetXStateFeaturesMask(Context: PContext; var FeatureMask: Int64): BOOL;
    stdcall; external kernel32;
  function LocateXStateFeature(Context: PContext; FeatureId: DWORD; Len: PDWORD): Pointer;
    stdcall; external kernel32;
  function NtQueryInformationThread(ThreadHandle: THandle;
    ThreadInformationClass: DWORD;
    ThreadInformation: Pointer; ThreadInformationLength: ULONG;
    ReturnLength: PULONG): NTSTATUS; stdcall; external 'ntdll.dll';
  function Wow64GetThreadContext(hThread: THandle; Context: PWow64Context): BOOL;
    stdcall; external kernel32;
  function Wow64SetThreadContext(hThread: THandle; const lpContext: TWow64Context): BOOL;
    stdcall; external kernel32;

// CONTEXT structure for AMD64 needs to be aligned on a 16-byte boundary
function Amd64Align(Src: Pointer): Pointer;
begin
  Result := {%H-}Pointer(({%H-}NativeUInt(Src) + 15) and not NativeUInt(15));
end;

function GetIntelContext(ThreadID: DWORD): TIntelThreadContext;
const
  {$IFDEF CPUX86}
  CONTEXT_XSTATE = $00010040;
  {$ELSE}
  CONTEXT_XSTATE = $00100040;
  {$ENDIF}
var
  hThread: THandle;
  Ctx: PContext;
  ContextBuff: array of Byte;
  ContextFlags: DWORD;
  FeatureMask: Int64;
  ContextSize, FeatureLength: DWORD;
  SimdReg: PXMMRegister;
  I: Integer;
begin
  Result := Default(TIntelThreadContext);
  if ThreadID = 0 then Exit;
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    ContextFlags := CONTEXT_ALL;

    {$IFDEF CPUX86}
    ContextFlags := ContextFlags or CONTEXT_EXTENDED_REGISTERS;
    SetLength(ContextBuff, SizeOf(TContext) + $10);
    Ctx := Amd64Align(@ContextBuff[0]);
    {$ENDIF}

    {$IFDEF CPUX64}
    ContextFlags := ContextFlags and not CONTEXT_i386 or $100000;
    SetLength(ContextBuff{%H-}, SizeOf(TContext) + $10);
    Ctx := Amd64Align(@ContextBuff[0]);
    {$ENDIF}

    Ctx.ContextFlags := ContextFlags;
    if not GetThreadContext(hThread, Ctx) then Exit;

    {$IFDEF CPUX86}
    Result.x86Context := True;
    Result.Rax := Ctx.Eax;
    Result.Rbx := Ctx.Ebx;
    Result.Rcx := Ctx.Ecx;
    Result.Rdx := Ctx.Edx;
    Result.Rsp := Ctx.Esp;
    Result.Rbp := Ctx.Ebp;
    Result.Rsi := Ctx.Esi;
    Result.Rdi := Ctx.Edi;
    Result.Rip := Ctx.Eip;
    Result.ControlWord := Ctx.FloatSave.ControlWord;
    Result.StatusWord := Ctx.FloatSave.StatusWord;
    Result.TagWord := Ctx.FloatSave.TagWord;
    Result.ErrorOffset := Ctx.FloatSave.ErrorOffset;
    Result.ErrorSelector := Ctx.FloatSave.ErrorSelector;
    Result.DataOffset := Ctx.FloatSave.DataOffset;
    Result.DataSelector := Ctx.FloatSave.DataSelector;
    Result.MxCsr := PDWORD(@Ctx.ExtendedRegisters[$18])^;
    Move(Ctx.FloatSave.RegisterArea[0], Result.FloatRegisters[0], SIZE_OF_80387_REGISTERS);
    {$ENDIF}

    {$IFDEF CPUX64}
    Result.Rax := Ctx.Rax;
    Result.Rbx := Ctx.Rbx;
    Result.Rcx := Ctx.Rcx;
    Result.Rdx := Ctx.Rdx;
    Result.Rsp := Ctx.Rsp;
    Result.Rbp := Ctx.Rbp;
    Result.Rsi := Ctx.Rsi;
    Result.Rdi := Ctx.Rdi;
    Result.Rip := Ctx.Rip;
    Result.R[8] := Ctx.R8;
    Result.R[9] := Ctx.R9;
    Result.R[10] := Ctx.R10;
    Result.R[11] := Ctx.R11;
    Result.R[12] := Ctx.R12;
    Result.R[13] := Ctx.R13;
    Result.R[14] := Ctx.R14;
    Result.R[15] := Ctx.R15;
    Result.ControlWord := Ctx.FltSave.ControlWord;
    Result.StatusWord := Ctx.FltSave.StatusWord;
    Result.ErrorOffset := Ctx.FltSave.ErrorOffset;
    Result.ErrorSelector := Ctx.FltSave.ErrorSelector;
    Result.DataOffset := Ctx.FltSave.DataOffset;
    Result.DataSelector := Ctx.FltSave.DataSelector;
    Result.MxCsr := Ctx.MxCsr;
    Move(Ctx.FltSave.FloatRegisters[0], Result.FloatRegisters[0], SizeOf(TFloatRegisters));
    Result.TagWord := GetTagWordFromFXSave(Result.StatusWord, Ctx.FltSave.TagWord, Result.FloatRegisters);
    {$ENDIF}

    Result.EFlags := Ctx.EFlags;
    Result.SegGs := Ctx.SegGs;
    Result.SegFs := Ctx.SegFs;
    Result.SegEs := Ctx.SegEs;
    Result.SegDs := Ctx.SegDs;
    Result.SegCs := Ctx.SegCs;
    Result.SegSs := Ctx.SegSs;

    Result.Dr0 := Ctx.Dr0;
    Result.Dr1 := Ctx.Dr1;
    Result.Dr2 := Ctx.Dr2;
    Result.Dr3 := Ctx.Dr3;
    Result.Dr6 := Ctx.Dr6;
    Result.Dr7 := Ctx.Dr7;

    FeatureMask := GetEnabledXStateFeatures;
    if FeatureMask and XSTATE_MASK_AVX = 0 then
      Exit;

    ContextSize := 0;

    {$IFDEF CPUX86}
    ContextFlags := WOW64_CONTEXT_ALL or CONTEXT_XSTATE;
    {$ENDIF}

    {$IFDEF CPUX64}
    ContextFlags := ContextFlags or CONTEXT_XSTATE;
    {$ENDIF}

    if InitializeContext(nil, ContextFlags, nil, ContextSize) and
      (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
      Exit;

    SetLength(ContextBuff, ContextSize + $10);
    if not InitializeContext(ContextBuff, ContextFlags,
      @Ctx, ContextSize) then
      Exit;

    if not SetXStateFeaturesMask(Ctx, XSTATE_MASK_AVX) then
      Exit;

    if not GetThreadContext(hThread, Ctx) then Exit;

    if not GetXStateFeaturesMask(Ctx, FeatureMask) then
      Exit;

    SimdReg := LocateXStateFeature(Ctx, XSTATE_LEGACY_SSE, @FeatureLength);
    if SimdReg = nil then
      Exit;

    Result.XmmCount := FeatureLength div SizeOf(Result.Ymm[0].Low);
    for I := 0 to Result.XmmCount - 1 do
    begin
      Result.Ymm[I].Low := SimdReg^;
      Inc(SimdReg);
    end;

    SimdReg := LocateXStateFeature(Ctx, XSTATE_AVX, nil);
    if SimdReg = nil then
      Exit;

    Result.YmmPresent := True;
    for I := 0 to Result.XmmCount - 1 do
    begin
      Result.Ymm[I].High := SimdReg^;
      Inc(SimdReg);
    end;

  finally
    CloseHandle(hThread);
  end;
end;

function SetIntelContext(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
var
  hThread: THandle;
  Ctx: PContext;
  ContextBuff: array of Byte;
  ContextFlags: DWORD;
begin
  Result := False;
  hThread := OpenThread(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    ContextFlags := CONTEXT_FULL;

    {$IFDEF CPUX86}
    ContextFlags := ContextFlags or CONTEXT_EXTENDED_REGISTERS;
    SetLength(ContextBuff{%H-}, SizeOf(TContext) + $10);
    Ctx := Amd64Align(@ContextBuff[0]);
    {$ENDIF}

    {$IFDEF CPUX64}
    ContextFlags := ContextFlags and not CONTEXT_i386 or $100000;
    SetLength(ContextBuff{%H-}, SizeOf(TContext) + $10);
    Ctx := Amd64Align(@ContextBuff[0]);
    {$ENDIF}

    Ctx.ContextFlags := ContextFlags;
    if not GetThreadContext(hThread, Ctx) then Exit;

    {$IFDEF CPUX86}
    Ctx.Eax := AContext.Rax;
    Ctx.Ebx := AContext.Rbx;
    Ctx.Ecx := AContext.Rcx;
    Ctx.Edx := AContext.Rdx;
    Ctx.Esp := AContext.Rsp;
    Ctx.Ebp := AContext.Rbp;
    Ctx.Esi := AContext.Rsi;
    Ctx.Edi := AContext.Rdi;
    Ctx.Eip := AContext.Rip;
    Ctx.FloatSave.ControlWord := AContext.ControlWord;
    Ctx.FloatSave.StatusWord := AContext.StatusWord;
    Ctx.FloatSave.TagWord := AContext.TagWord;
    PDWORD(@Ctx.ExtendedRegisters[$18])^ := AContext.MxCsr;
    {$ENDIF}

    {$IFDEF CPUX64}
    Ctx.Rax := AContext.Rax;
    Ctx.Rbx := AContext.Rbx;
    Ctx.Rcx := AContext.Rcx;
    Ctx.Rdx := AContext.Rdx;
    Ctx.Rsp := AContext.Rsp;
    Ctx.Rbp := AContext.Rbp;
    Ctx.Rsi := AContext.Rsi;
    Ctx.Rdi := AContext.Rdi;
    Ctx.Rip := AContext.Rip;
    Ctx.R8 := AContext.R[8];
    Ctx.R9 := AContext.R[9];
    Ctx.R10 := AContext.R[10];
    Ctx.R11 := AContext.R[11];
    Ctx.R12 := AContext.R[12];
    Ctx.R13 := AContext.R[13];
    Ctx.R14 := AContext.R[14];
    Ctx.R15 := AContext.R[15];
    Ctx.FltSave.ControlWord := AContext.ControlWord;
    Ctx.FltSave.StatusWord := AContext.StatusWord;
    Ctx.FltSave.TagWord := GetFXSaveTagWordFromTagWord(AContext.TagWord);
    Ctx.MxCsr := AContext.MxCsr;
    {$ENDIF}
    Ctx.EFlags := AContext.EFlags;

    Ctx.ContextFlags := ContextFlags;
    Result := SetThreadContext(hThread, Ctx^);
  finally
    CloseHandle(hThread);
  end;
end;

function GetIntelWow64Context(ThreadID: DWORD): TIntelThreadContext;
var
  hThread: THandle;
  Ctx: PWow64Context;
  ContextBuff: array of Byte;
  FeatureMask: Int64;
  SimdReg: PXMMRegister;
  I: Integer;
  ExtentedDisabled: Boolean;
begin
  Result := Default(TIntelThreadContext);
  if ThreadID = 0 then Exit;
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    SetLength(ContextBuff{%H-}, SizeOf(TContext) + $10);
    Ctx := Amd64Align(@ContextBuff[0]);
    Ctx.ContextFlags := WOW64_CONTEXT_ALL;
    ExtentedDisabled := False;
    if not Wow64GetThreadContext(hThread, Ctx) then
    begin
      Ctx.ContextFlags := WOW64_CONTEXT_WITHOUT_EXTENDED;
      ExtentedDisabled := True;
      if not Wow64GetThreadContext(hThread, Ctx) then
        Exit;
    end;

    Result.x86Context := True;
    Result.Rax := Ctx.Eax;
    Result.Rbx := Ctx.Ebx;
    Result.Rcx := Ctx.Ecx;
    Result.Rdx := Ctx.Edx;
    Result.Rsp := Ctx.Esp;
    Result.Rbp := Ctx.Ebp;
    Result.Rsi := Ctx.Esi;
    Result.Rdi := Ctx.Edi;
    Result.Rip := Ctx.Eip;
    Result.ControlWord := Ctx.FloatSave.ControlWord;
    Result.StatusWord := Ctx.FloatSave.StatusWord;
    Result.TagWord := Ctx.FloatSave.TagWord;
    Result.ErrorOffset := Ctx.FloatSave.ErrorOffset;
    Result.ErrorSelector := Ctx.FloatSave.ErrorSelector;
    Result.DataOffset := Ctx.FloatSave.DataOffset;
    Result.DataSelector := Ctx.FloatSave.DataSelector;
    Result.MxCsr := PDWORD(@Ctx.ExtendedRegisters[$18])^;
    Move(Ctx.FloatSave.RegisterArea[0], Result.FloatRegisters[0], WOW64_SIZE_OF_80387_REGISTERS);

    Result.EFlags := Ctx.EFlags;
    Result.SegGs := Ctx.SegGs;
    Result.SegFs := Ctx.SegFs;
    Result.SegEs := Ctx.SegEs;
    Result.SegDs := Ctx.SegDs;
    Result.SegCs := Ctx.SegCs;
    Result.SegSs := Ctx.SegSs;

    Result.Dr0 := Ctx.Dr0;
    Result.Dr1 := Ctx.Dr1;
    Result.Dr2 := Ctx.Dr2;
    Result.Dr3 := Ctx.Dr3;
    Result.Dr6 := Ctx.Dr6;
    Result.Dr7 := Ctx.Dr7;

    if ExtentedDisabled then Exit;

    FeatureMask := GetEnabledXStateFeatures;
    if FeatureMask and XSTATE_MASK_AVX = 0 then
      Exit;

    // https://maximumcrack.wordpress.com/2011/08/07/fpu-mmx-xmm-and-bbq/
    SimdReg := @Ctx.ExtendedRegisters[160];

    Result.XmmCount := 8;
    for I := 0 to Result.XmmCount - 1 do
    begin
      Result.Ymm[I].Low := SimdReg^;
      Inc(SimdReg);
    end;

    Result.YmmPresent := True;
    for I := 0 to Result.XmmCount - 1 do
    begin
      Result.Ymm[I].High := SimdReg^;
      Inc(SimdReg);
    end;

  finally
    CloseHandle(hThread);
  end;
end;

function SetIntelWow64Context(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
var
  hThread: THandle;
  Ctx: PWow64Context;
  ContextBuff: array of Byte;
begin
  Result := False;
  hThread := OpenThread(THREAD_GET_CONTEXT or THREAD_SET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    SetLength(ContextBuff{%H-}, SizeOf(TContext) + $10);
    Ctx := Amd64Align(@ContextBuff[0]);
    Ctx.ContextFlags := WOW64_CONTEXT_WITHOUT_EXTENDED;
    if not Wow64GetThreadContext(hThread, Ctx) then Exit;

    Ctx.Eax := AContext.Rax;
    Ctx.Ebx := AContext.Rbx;
    Ctx.Ecx := AContext.Rcx;
    Ctx.Edx := AContext.Rdx;
    Ctx.Esp := AContext.Rsp;
    Ctx.Ebp := AContext.Rbp;
    Ctx.Esi := AContext.Rsi;
    Ctx.Edi := AContext.Rdi;
    Ctx.Eip := AContext.Rip;
    Ctx.FloatSave.ControlWord := AContext.ControlWord;
    Ctx.FloatSave.StatusWord := AContext.StatusWord;
    Ctx.FloatSave.TagWord := AContext.TagWord;
    PDWORD(@Ctx.ExtendedRegisters[$18])^ := AContext.MxCsr;
    Ctx.EFlags := AContext.EFlags;

    Ctx.ContextFlags := WOW64_CONTEXT_WITHOUT_EXTENDED;
    Result := Wow64SetThreadContext(hThread, Ctx^);
  finally
    CloseHandle(hThread);
  end;
end;

function GetThreadNativeStackLimit(ProcessHandle: THandle; ThreadID: DWORD): TStackLimit;
var
  hThread: THandle;
  TBI: TThreadBasicInformation;
  TIB: NT_TIB;
  lpNumberOfBytesRead: SIZE_T;
begin
  Result := Default(TStackLimit);
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    if NtQueryInformationThread(hThread, ThreadBasicInformation, @TBI,
      SizeOf(TBI), nil) <> 0 then
      Exit;
    lpNumberOfBytesRead := 0;
    if not ReadProcessMemory(ProcessHandle,
      TBI.TebBaseAddress, @TIB, SizeOf(NT_TIB),
      lpNumberOfBytesRead) then Exit;
    Result.Base := {%H-}NativeUInt(TIB.StackBase);
    Result.Limit := {%H-}NativeUInt(TIB.StackLimit);
  finally
    CloseHandle(hThread);
  end;
end;

function GetThreadWow64StackLimit(ProcessHandle: THandle; ThreadID: DWORD): TStackLimit;
const
  ThreadBasicInformation = 0;
var
  hThread: THandle;
  TBI: TThreadBasicInformation;
  TIB: NT_TIB;
  lpNumberOfBytesRead: SIZE_T;
  WOW64_NT_TIB: TWOW64_NT_TIB;
begin
  Result := Default(TStackLimit);
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    if NtQueryInformationThread(hThread, ThreadBasicInformation, @TBI,
      SizeOf(TBI), nil) <> 0 then
      Exit;
    lpNumberOfBytesRead := 0;
    if not ReadProcessMemory(ProcessHandle,
      TBI.TebBaseAddress, @TIB, SizeOf(NT_TIB),
      lpNumberOfBytesRead) then Exit;
    if not ReadProcessMemory(ProcessHandle,
      TIB.ExceptionList, @WOW64_NT_TIB, SizeOf(TWOW64_NT_TIB),
      lpNumberOfBytesRead) then Exit;
    Result.Base := Int64(WOW64_NT_TIB.StackBase);
    Result.Limit := Int64(WOW64_NT_TIB.StackLimit);
  finally
    CloseHandle(hThread);
  end;
end;

function GetThreadNativeExtendedData(ProcessHandle: THandle; ThreadID: DWORD): TThreadExtendedData;
var
  hThread: THandle;
  TBI: TThreadBasicInformation;
  lpNumberOfBytesRead: SIZE_T;
begin
  Result := Default(TThreadExtendedData);
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    if NtQueryInformationThread(hThread, ThreadBasicInformation, @TBI,
      SizeOf(TBI), nil) <> 0 then
      Exit;
    lpNumberOfBytesRead := 0;
    ReadProcessMemory(ProcessHandle,
      PByte(TBI.TebBaseAddress) + $68, @Result.LastError, SizeOf(Result.LastError),
      lpNumberOfBytesRead);
    ReadProcessMemory(ProcessHandle,
      PByte(TBI.TebBaseAddress) + $1250, @Result.LastStatus, SizeOf(Result.LastStatus),
      lpNumberOfBytesRead);
  finally
    CloseHandle(hThread);
  end;
end;

function GetThreadWow64ExtendedData(ProcessHandle: THandle; ThreadID: DWORD): TThreadExtendedData;
var
  hThread: THandle;
  TBI: TThreadBasicInformation;
  TIB: NT_TIB;
  lpNumberOfBytesRead: SIZE_T;
begin
  Result := Default(TThreadExtendedData);
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    if NtQueryInformationThread(hThread, ThreadBasicInformation, @TBI,
      SizeOf(TBI), nil) <> 0 then
      Exit;
    lpNumberOfBytesRead := 0;
    if not ReadProcessMemory(ProcessHandle,
      TBI.TebBaseAddress, @TIB, SizeOf(NT_TIB),
      lpNumberOfBytesRead) then Exit;
    ReadProcessMemory(ProcessHandle,
      PByte(TIB.ExceptionList) + $34, @Result.LastError, SizeOf(Result.LastError),
      lpNumberOfBytesRead);
    ReadProcessMemory(ProcessHandle,
      PByte(TIB.ExceptionList) + $BF4, @Result.LastStatus, SizeOf(Result.LastStatus),
      lpNumberOfBytesRead);
  finally
    CloseHandle(hThread);
  end;
end;

function SetThreadNativeExtendedData(ProcessHandle: THandle; ThreadID: Integer;
  const AData: TThreadExtendedData): Boolean;
var
  hThread: THandle;
  TBI: TThreadBasicInformation;
  lpNumberOfBytesWritten: SIZE_T;
begin
  Result := False;
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    if NtQueryInformationThread(hThread, ThreadBasicInformation, @TBI,
      SizeOf(TBI), nil) <> 0 then
      Exit;
    lpNumberOfBytesWritten := 0;
    Result := WriteProcessMemory(ProcessHandle,
      PByte(TBI.TebBaseAddress) + $68, @AData.LastError, SizeOf(AData.LastError),
      lpNumberOfBytesWritten);
    if Result then
      Result := WriteProcessMemory(ProcessHandle,
      PByte(TBI.TebBaseAddress) + $1250, @AData.LastStatus, SizeOf(AData.LastStatus),
      lpNumberOfBytesWritten);
  finally
    CloseHandle(hThread);
  end;
end;

function SetThreadWow64ExtendedData(ProcessHandle: THandle; ThreadID: Integer;
  const AData: TThreadExtendedData): Boolean;
var
  hThread: THandle;
  TBI: TThreadBasicInformation;
  TIB: NT_TIB;
  lpNumberOfBytesWritten: SIZE_T;
begin
  Result := False;
  hThread := OpenThread(THREAD_GET_CONTEXT or
    THREAD_SUSPEND_RESUME or THREAD_QUERY_INFORMATION, False, ThreadID);
  if hThread = 0 then Exit;
  try
    if NtQueryInformationThread(hThread, ThreadBasicInformation, @TBI,
      SizeOf(TBI), nil) <> 0 then
      Exit;
    lpNumberOfBytesWritten := 0;
    if not ReadProcessMemory(ProcessHandle,
      TBI.TebBaseAddress, @TIB, SizeOf(NT_TIB),
      lpNumberOfBytesWritten) then Exit;
    Result := WriteProcessMemory(ProcessHandle,
      PByte(TIB.ExceptionList) + $34, @AData.LastError, SizeOf(AData.LastError),
      lpNumberOfBytesWritten);
    if Result then
      Result := WriteProcessMemory(ProcessHandle,
        PByte(TIB.ExceptionList) + $BF4, @AData.LastStatus, SizeOf(AData.LastStatus),
        lpNumberOfBytesWritten);
  finally
    CloseHandle(hThread);
  end;
end;

{ TCommonUtils }

destructor TCommonUtils.Destroy;
begin
  ProcessID := 0;
  inherited;
end;

function TCommonUtils.GetThreadExtendedData(ThreadID: Integer;
  ThreadIs32: Boolean): TThreadExtendedData;
begin
  if FProcessHandle = 0 then Exit(Default(TThreadExtendedData));
  {$IFDEF CPUX86}
  Result := GetThreadNativeExtendedData(FProcessHandle, ThreadID);
  {$ENDIF}
  {$IFDEF CPUX64}
  if ThreadIs32 then
    Result := GetThreadWow64ExtendedData(FProcessHandle, ThreadID)
  else
    Result := GetThreadNativeExtendedData(FProcessHandle, ThreadID);
  {$ENDIF}
end;

function TCommonUtils.GetThreadStackLimit(ThreadID: Integer;
  ThreadIs32: Boolean): TStackLimit;
begin
  if FProcessHandle = 0 then Exit(Default(TStackLimit));
  {$IFDEF CPUX86}
  Result := GetThreadNativeStackLimit(FProcessHandle, ThreadID);
  {$ENDIF}
  {$IFDEF CPUX64}
  if ThreadIs32 then
    Result := GetThreadWow64StackLimit(FProcessHandle, ThreadID)
  else
    Result := GetThreadNativeStackLimit(FProcessHandle, ThreadID);
  {$ENDIF}
end;

function TCommonUtils.QueryRegion(AddrVA: Int64;
  out RegionData: TRegionData): Boolean;
var
  MBI: TMemoryBasicInformation;
  dwLength: Cardinal;
begin
  RegionData := Default(TRegionData);
  if FProcessHandle = 0 then Exit(False);
  dwLength := SizeOf(TMemoryBasicInformation);
  Result := VirtualQueryEx(FProcessHandle, {%H-}Pointer(AddrVA), MBI{%H-}, dwLength) = dwLength;
  if Result then
  begin
    RegionData.AllocationBase := {%H-}Int64(MBI.AllocationBase);
    RegionData.BaseAddr := {%H-}Int64(MBI.BaseAddress);
    RegionData.RegionSize := Int64(MBI.RegionSize);
    if MBI.State = MEM_COMMIT then
    begin
      if MBI.Protect and (PAGE_NOACCESS or PAGE_GUARD) <> 0 then
      begin
        Include(RegionData.Access, raProtect);
        Exit;
      end;
      if MBI.Protect and (PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE) <> 0 then
      begin
        Include(RegionData.Access, raExecute);
        Include(RegionData.Access, raRead);
      end;
      if MBI.Protect and (PAGE_READONLY or PAGE_READWRITE) <> 0 then
        Include(RegionData.Access, raRead);
      if MBI.Protect and (
        PAGE_READWRITE or PAGE_EXECUTE_READWRITE or
        PAGE_WRITECOPY or PAGE_EXECUTE_WRITECOPY) <> 0 then
        Include(RegionData.Access, raWrite);
    end;
  end;
end;

function TCommonUtils.ReadData(AddrVA: Pointer; var Buff;
  ASize: Longint): Longint;

  function CanRead(MBI: TMemoryBasicInformation): Boolean;
  begin
    Result := MBI.State = MEM_COMMIT;
    if Result then
      Result := MBI.Protect and (
        PAGE_EXECUTE_READ or
        PAGE_EXECUTE_READWRITE or
        PAGE_READONLY or
        PAGE_READWRITE) <> 0;
    if Result then
      Result := (MBI.Protect and PAGE_GUARD) = 0;
  end;

var
  MBI: TMemoryBasicInformation;
  dwLength: Cardinal;
  RegionSize: NativeInt;
  ReadCount: NativeUInt;
begin
  Result := 0;
  if FProcessHandle = 0 then Exit;
  dwLength := SizeOf(TMemoryBasicInformation);
  if VirtualQueryEx(FProcessHandle,
    AddrVA, MBI, dwLength) <> dwLength then Exit;

  if NativeInt(MBI.BaseAddress) > 0 then
  begin
    RegionSize := NativeInt(MBI.RegionSize) -
      (NativeInt(AddrVA) - NativeInt(MBI.BaseAddress));
    if ASize > RegionSize then
      ASize := RegionSize;
  end
  else
    // принудительно отключаем все что не можем прочитать
    // forcibly disable anything we can't read.
    MBI.Protect := MBI.Protect or PAGE_GUARD;

  // не будем вмешиваться в память удаленного процесса
  // меняя атрибуты страницы, поэтому если стоит запрет на чтение
  // то просто возвращаем нулевой буфер заданного размера

  // we will not interfere with the memory of the remote process
  // by changing the page attributes, so if there is a read ban,
  // we just return a zero buffer of the specified size.
  if not CanRead(MBI) then
  begin
    FillChar(Buff, ASize, 0);
    Exit(ASize);
  end;

  if ReadProcessMemory(FProcessHandle, AddrVA, @Buff, ASize, ReadCount) then
    Result := Longint(ReadCount);
end;

function TCommonUtils.SetThreadExtendedData(ThreadID: Integer;
  ThreadIs32: Boolean; const AData: TThreadExtendedData): Boolean;
begin
  if FProcessHandle = 0 then Exit(False);
  {$IFDEF CPUX86}
  Result := SetThreadNativeExtendedData(FProcessHandle, ThreadID, AData);
  {$ENDIF}
  {$IFDEF CPUX64}
  if ThreadIs32 then
    Result := SetThreadWow64ExtendedData(FProcessHandle, ThreadID, AData)
  else
    Result := SetThreadNativeExtendedData(FProcessHandle, ThreadID, AData);
  {$ENDIF}
end;

procedure TCommonUtils.SetProcessID(const Value: Integer);
begin
  if ProcessID = Value then Exit;
  if FProcessHandle <> 0 then
  begin
    CloseHandle(FProcessHandle);
    FProcessHandle := 0;
  end;
  inherited;
  if ProcessID <> 0 then
    FProcessHandle := OpenProcess(
      PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_VM_WRITE, False, ProcessID);
end;

procedure TCommonUtils.Update;
begin
  // do nothing...
end;

end.
