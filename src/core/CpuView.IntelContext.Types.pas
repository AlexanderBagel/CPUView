////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.IntelContext.Types.pas
//  * Purpose   : Intel x86_64 processor utility methods and types.
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

unit CpuView.IntelContext.Types;

interface

uses
  FWHexView.Common;

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

  TMMxReg = packed record
    Value: UInt64;
    Reserved: Word;
  end;

  TFloatRegister = record
    case Integer of
      0: (MM: TMMxReg);
      1: (x87: TExtended80Support);
  end;

  TFloatRegisters = array [0..7] of TFloatRegister;

  PIntelThreadContext = ^TIntelThreadContext;
  TIntelThreadContext = packed record
    x86Context: Boolean;
    Rax, Rbx, Rcx, Rdx, Rsp, Rbp, Rsi, Rdi, Rip: UInt64;
    R: array [8..15] of UInt64; // x64 specific
    EFlags: Cardinal;
    SegGs, SegFs, SegEs, SegDs, SegCs, SegSs: Cardinal;
    Dr0, Dr1, Dr2, Dr3, Dr6, Dr7: UInt64;
    LastError, LastStatus: Cardinal;
    ControlWord, StatusWord, TagWord: Word;
    ErrorOffset, ErrorSelector, DataOffset, DataSelector, MxCsr: Cardinal;
    FloatRegisters: TFloatRegisters;
    XmmCount: Integer;    // Zero - if the feature is unsupported by the processor
    YmmPresent: Boolean;  // False - if the feature is unsupported by the processor
    Ymm: array[0..15] of TYMMRegister; // 8..15 for x64 mode
  end;

  function GetTagWordFromFXSave(AStatusWord: Word; ATagWord: Byte;
    const AFloatRegs: TFloatRegisters): Word;
  function GetFXSaveTagWordFromTagWord(ATagWord: Word): Byte;

implementation

function GetTagWordFromFXSave(AStatusWord: Word; ATagWord: Byte;
  const AFloatRegs: TFloatRegisters): Word;
var
  I, StackTop, RegIndex, PartResult: Integer;
begin
  Result := 0;
  StackTop := (AStatusWord shr 11) and 7;
  for I := 0 to 7 do
  begin
    if ATagWord and (1 shl I) <> 0 then
    begin
      RegIndex := (I + 8 - StackTop) mod 8;
      case GetFloatType(AFloatRegs[RegIndex].x87) of
        ftNormalized: PartResult := 0; // Valid
        ftZero: PartResult := 1; // Zero
      else
        PartResult := 2; // Special
      end;
    end
    else
      PartResult := 3; // Empty
    Inc(Result, PartResult shl (I shl 1));
  end;
end;

function GetFXSaveTagWordFromTagWord(ATagWord: Word): Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 7 do
    if (ATagWord shr (I shl 1)) and 3 <> 3 then
      Inc(Result, 1 shl I);
end;

end.
