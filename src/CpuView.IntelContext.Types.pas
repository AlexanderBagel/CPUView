unit CpuView.IntelContext.Types;

interface

uses
  Windows;

{$IFDEF FPC}
const
  WOW64_SIZE_OF_80387_REGISTERS = 80;
{$ENDIF}

type
  PXMMRegister = ^TXMMRegister;
  TXMMRegister = record
    Low: UInt64;
    High: UInt64;
  end;

  PYMMRegister = ^TYMMRegister;
  TYMMRegister = record
    Low: TXMMRegister;
    High: TXMMRegister;
  end;

  TIntelThreadContext = record
    x86Context: Boolean;
    Rax, Rbx, Rcx, Rdx, Rsp, Rbp, Rsi, Rdi, Rip: UInt64;
    R: array [8..15] of UInt64; // x64 specific
    EFlags: DWORD;
    SegGs, SegFs, SegEs, SegDs, SegCs, SegSs: DWORD;
    Dr0, Dr1, Dr2, Dr3, Dr6, Dr7: UInt64;
    LastError, LastStatus: DWORD;
    ControlWord, StatusWord, TagWord: Word;
    ErrorOffset, ErrorSelector, DataOffset, DataSelector, MxCsr: DWORD;
    FloatRegisters: array[0..WOW64_SIZE_OF_80387_REGISTERS - 1] of Byte;
    XmmCount: Integer;    // Zero - if the feature is unsupported by the processor
    YmmPresent: Boolean;  // False - if the feature is unsupported by the processor
    Ymm: array[0..15] of TYMMRegister; // 8..15 for x64 mode
  end;

implementation

end.
