////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Context.Aarch64.Types.pas
//  * Purpose   : Aarch64 processor utility methods and types.
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

unit CpuView.Context.Aarch64.Types;

{$mode objfpc}{$H+}

interface

//
// Specify the number of breakpoints and watchpoints that the OS
// will track. Architecturally, ARM64 supports up to 16. In practice,
// however, almost no one implements more than 4 of each.
//

const
  ARM64_MAX_BREAKPOINTS = 8;
  ARM64_MAX_WATCHPOINTS = 2;

//
// Context Frame
//
//  This frame has a several purposes: 1) it is used as an argument to
//  NtContinue, 2) it is used to constuct a call frame for APC delivery,
//  and 3) it is used in the user level thread creation routines.
//
//
// The flags field within this record controls the contents of a CONTEXT
// record.
//
// If the context record is used as an input parameter, then for each
// portion of the context record controlled by a flag whose value is
// set, it is assumed that that portion of the context record contains
// valid context. If the context record is being used to modify a threads
// context, then only that portion of the threads context is modified.
//
// If the context record is used as an output parameter to capture the
// context of a thread, then only those portions of the thread's context
// corresponding to set flags will be returned.
//
// CONTEXT_CONTROL specifies FP, LR, SP, PC, and CPSR
//
// CONTEXT_INTEGER specifies X0-X28
//
// CONTEXT_FLOATING_POINT specifies Fpcr, Fpsr and Q0-Q31 / D0-D31 / S0-S31
//
// CONTEXT_DEBUG_REGISTERS specifies up to 16 of DBGBVR, DBGBCR, DBGWVR,
//      DBGWCR.
//

type
  TArm64_Neon128 = record
    case Integer of
      0: (B: array [0..15] of Byte);
      1: (H: array [0..7] of Word);
      2: (S: array [0..3] of Single);
      3: (D: array [0..1] of Double);
      4: (Q: array [0..1] of QWord);
  end;

  TAarch64ThreadContext = packed record

    //
    // Control flags.
    //

    {* +0x000 *}    ContextFlags: DWORD;

    //
    // Integer registers
    //

    {* +0x004 *}    Cpsr: DWORD; // NZVF + DAIF + CurrentEL + SPSel
    {* +0x008 *}    X: array [0..28] of QWord;
    {* +0x0F0 *}    Fp: QWord;
    {* +0x0F8 *}    Lr: QWord;

    {* +0x100 *}    Sp: QWord;
    {* +0x108 *}    Pc: QWord;

    //
    // Floating Point/NEON Registers
    //

    {* +0x110 *}    V: array [0..30] of TArm64_Neon128;
    {* +0x310 *}    Fpcr: DWORD;
    {* +0x314 *}    Fpsr: DWORD;

    //
    // Debug registers
    //

    {* +0x318 *}    Bxr: array [0..ARM64_MAX_BREAKPOINTS - 1] of DWORD;
    {* +0x338 *}    Bvr: array [0..ARM64_MAX_BREAKPOINTS - 1] of QWord;
    {* +0x378 *}    Wcr: array [0..ARM64_MAX_WATCHPOINTS - 1] of DWORD;
    {* +0x380 *}    Wvr: array [0..ARM64_MAX_WATCHPOINTS - 1] of QWord;

    {* +0x390 *}

    // extended

    Aarch32Context: Boolean;
    ChangedRegID: Integer;
  end;

  TGetAarch64Context = function(AThreadID: DWORD): TAarch64ThreadContext;
  TSetAarch64Context = function(AThreadID: DWORD; const AContext: TAarch64ThreadContext): Boolean;
  TGetAarch32Context = function(AThreadID: DWORD): TAarch64ThreadContext;
  TSetAarch32Context = function(AThreadID: DWORD; const AContext: TAarch64ThreadContext): Boolean;

  TContextQueryParams = record
    GetDefContext: TGetAarch64Context;
    SetDefContext: TSetAarch64Context;
    Get32Context: TGetAarch64Context;
    Set32Context: TSetAarch64Context;
  end;

implementation

end.

