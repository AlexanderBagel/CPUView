////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.GdbDebug.Intel.pas
//  * Purpose   : Implementation of gateway to GDB for Intel x86_64 processor.
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

unit CpuView.GdbDebug.Intel;

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

  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}

  SysUtils,
  StrUtils,
  Classes,

  DebugUtils,
  GDBMIMiscClasses,
  LazDebuggerIntfBaseTypes,
  GDBMIDebugger,

  {$IFDEF LINUX}
  CpuView.Linux,
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  CpuView.Windows,
  {$ENDIF}

  FWHexView.Common,
  FWHexView.AsmTokenizer,
  CpuView.Common,
  CpuView.GdbDebug,
  CpuView.CPUContext,
  CpuView.DebugerGate,
  CpuView.Context.Params,
  CpuView.Context.Intel.Types,
  CpuView.Context.Intel;

type

  { TGdbIntelDebugGate }

  TGdbIntelDebugGate = class(TGdbAbstractDebugGate)
  class var
    FInstance: TGdbIntelDebugGate;
  private
    FLastIntelCtx: TIntelThreadContext;
    function CtxGetIntelContext(AThreadID: DWORD): TIntelThreadContext;
    function CtxSetIntelContext(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
  protected
    procedure CtxUpdateRegIndex; override;
    function DasmParseItem(AItem: PGDBMINameValue; AList: TListEx<TGdbInstruction>): Boolean; override;
    procedure InitContext(AValue: TCommonCpuContext); override;
    procedure FormatInstruction(var Inst: TInstruction;
      const GdbInst: TGdbInstruction; AShowCallFuncName: Boolean); override;
    function GetEndOnProcToken: string; override;
    class property Instance: TGdbIntelDebugGate read FInstance write FInstance;
  public
    constructor Create(AOwner: TComponent; AUtils: TCommonAbstractUtils); override;
    destructor Destroy; override;
    function GetTokenizerMode: TTokenizerMode; override;
    function IsExtendedSyntax: Boolean; override;
    function ThreadStackLimit: TStackLimit; override;
  end;

implementation

function GDBGetIntelContext(AThreadID: DWORD): TIntelThreadContext;
begin
  Result := TGdbIntelDebugGate.Instance.CtxGetIntelContext(AThreadID);
end;

function GDBSetIntelContext(AThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
begin
  Result := TGdbIntelDebugGate.Instance.CtxSetIntelContext(AThreadID, AContext);
end;

{ TGdbIntelDebugGate }

procedure TGdbIntelDebugGate.CtxUpdateRegIndex;
const
  ExtendedRegNames64: array[0..89] of string = (
    'rax', 'rbx', 'rcx', 'rdx', 'rbp', 'rsp', 'rsi', 'rdi',
    'rip', 'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15',
    'mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6', 'mm7',
    'r0', 'r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7',
    'st0', 'st1', 'st2', 'st3', 'st4', 'st5', 'st6', 'st7',
    'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7',
    'xmm8', 'xmm9', 'xmm10', 'xmm11', 'xmm12', 'xmm13', 'xmm14', 'xmm15',
    'ymm0', 'ymm1', 'ymm2', 'ymm3', 'ymm4', 'ymm5', 'ymm6', 'ymm7',
    'ymm8', 'ymm9', 'ymm10', 'ymm11', 'ymm12', 'ymm13', 'ymm14', 'ymm15',
    'eflags', 'gs', 'fs', 'es', 'ds', 'cs', 'ss',
    'dr0', 'dr1', 'dr2', 'dr3', 'dr6', 'dr7',
    'fctrl', 'fstat', 'ftag', 'mxcsr'
  );
  ExtendedRegNames32: array[0..89] of string = (
    'eax', 'ebx', 'ecx', 'edx', 'ebp', 'esp', 'esi', 'edi',
    'eip', '', '', '', '', '', '', '', '',
    'mm0', 'mm1', 'mm2', 'mm3', 'mm4', 'mm5', 'mm6', 'mm7',
    'r0', 'r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7',
    'st0', 'st1', 'st2', 'st3', 'st4', 'st5', 'st6', 'st7',
    'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7',
    '', '', '', '', '', '', '', '',
    'ymm0', 'ymm1', 'ymm2', 'ymm3', 'ymm4', 'ymm5', 'ymm6', 'ymm7',
    '', '', '', '', '', '', '', '',
    'eflags', 'gs', 'fs', 'es', 'ds', 'cs', 'ss',
    'dr0', 'dr1', 'dr2', 'dr3', 'dr6', 'dr7',
    'fctrl', 'fstat', 'ftag', 'mxcsr'
  );
var
  I: Integer;
begin
  if PointerSize = 8 then
  begin
    for I := 0 to Length(ExtendedRegNames64) - 1 do
      CtxKnownRegs.Add(ExtendedRegNames64[I]);
  end
  else
    for I := 0 to Length(ExtendedRegNames32) - 1 do
      CtxKnownRegs.Add(ExtendedRegNames32[I]);
  for I := 0 to CtxRegNames.Count - 1 do
    CtxRegNames.Objects[I] := {%H-}Pointer(CtxKnownRegs.IndexOf(CtxRegNames[I]));
end;

function TGdbIntelDebugGate.CtxGetIntelContext(AThreadID: DWORD
  ): TIntelThreadContext;

  function LoadV2Int64(AValue: string;
    var ARegVal: TYMMRegister): Boolean;
  var
    Idx: Integer;
    List: TStringList;
  begin
    Result := False;
    ARegVal := Default(TYMMRegister);
    Idx := Pos('_int64', AValue);
    if Idx = 0 then Exit;
    Idx := Pos('{', AValue, Idx);
    if Idx = 0 then Exit;
    AValue := Copy(AValue, Idx + 1, Pos('}', AValue, Idx) - Idx - 1);
    List := TStringList.Create;
    try
      List.Delimiter := ',';
      List.DelimitedText := AValue;
      Result := List.Count >= 2;
      if not Result then Exit;
      ARegVal.Low.Low := StrToUInt64Def(Trim(List[0]), 0);
      ARegVal.Low.High := StrToUInt64Def(Trim(List[1]), 0);
      if List.Count <> 4 then Exit;
      ARegVal.High.Low := StrToUInt64Def(Trim(List[2]), 0);
      ARegVal.High.High := StrToUInt64Def(Trim(List[3]), 0);
    finally
      List.Free;
    end;
  end;

var
  ExecResult: TGDBMIExecResult;
  List, ValueList: TGDBMINameValueList;
  I, Idx: Integer;
  Item: PGDBMINameValue;
  Value: string;
  FloatValue: Extended;
  Extended80Value: TExtended80Support;
begin
  Result := Default(TIntelThreadContext);
  if (CtxRegNames.Count = 0) and not CtxUpdateRegNames then Exit;
  if not ExecuteGdbCommand('-data-list-register-values x', [], ExecResult) or
    (ExecResult.State = dsError) then Exit;
  ValueList := TGDBMINameValueList.Create('');
  try
    List := TGDBMINameValueList.Create(ExecResult, ['register-values']);
    try
      for I := 0 to List.Count - 1 do
      begin
        Item := List.Items[I];
        ValueList.Init(Item^.Name);
        Idx := StrToIntDef(Unquote(ValueList.Values['number']), -1);
        if (Idx < 0) or (Idx >= CtxRegNames.Count) then Continue;
        Value := Unquote(ValueList.Values['value']);
        Idx := Integer(CtxRegNames.Objects[Idx]);
        case Idx of
          0: Result.Rax := StrToInt64Def(Value, 0);
          1: Result.Rbx := StrToInt64Def(Value, 0);
          2: Result.Rcx := StrToInt64Def(Value, 0);
          3: Result.Rdx := StrToInt64Def(Value, 0);
          4: Result.Rbp := StrToInt64Def(Value, 0);
          5: Result.Rsp := StrToInt64Def(Value, 0);
          6: Result.Rsi := StrToInt64Def(Value, 0);
          7: Result.Rdi := StrToInt64Def(Value, 0);
          8: Result.Rip := StrToInt64Def(Value, 0);
          9..16: Result.R[Idx - 1] := StrToInt64Def(Value, 0);
          17..24: // MM0..MM7
            Result.MMXPresent := True;
          25..32: ; // R0..R7
          41..56: // XMM0..XMM15
            if LoadV2Int64(Value, Result.Ymm[Idx - 41]) then
              Inc(Result.XmmCount);
          57..72: // YMM0..YMM15
            if LoadV2Int64(Value, Result.Ymm[Idx - 57]) then
              Result.YmmPresent := True;
          73: Result.EFlags := StrToUIntDef(Value, 0);
          74: Result.SegGs := StrToUIntDef(Value, 0);
          75: Result.SegFs := StrToUIntDef(Value, 0);
          76: Result.SegEs := StrToUIntDef(Value, 0);
          77: Result.SegDs := StrToUIntDef(Value, 0);
          78: Result.SegCs := StrToUIntDef(Value, 0);
          79: Result.SegSs := StrToUIntDef(Value, 0);
          80: Result.Dr0 := StrToInt64Def(Value, 0);
          81: Result.Dr1 := StrToInt64Def(Value, 0);
          82: Result.Dr2 := StrToInt64Def(Value, 0);
          83: Result.Dr3 := StrToInt64Def(Value, 0);
          84: Result.Dr6 := StrToInt64Def(Value, 0);
          85: Result.Dr7 := StrToInt64Def(Value, 0);
          86: Result.ControlWord := StrToUIntDef(Value, 0);
          87: Result.StatusWord := StrToUIntDef(Value, 0);
          88: Result.TagWord := StrToUIntDef(Value, 0);
          89: Result.MxCsr := StrToUIntDef(Value, 0);
        end;
        case Idx of
          80..85: Result.DebugPresent := True;
        end;
      end;
      Result.TagWord := GetTagWordFromFXSave(Result.StatusWord,
        Result.TagWord, Result.FloatRegisters);
    finally
      List.Free;
    end;
    if not ExecuteGdbCommand('-data-list-register-values N', [], ExecResult) or
      (ExecResult.State = dsError) then Exit;
    List := TGDBMINameValueList.Create(ExecResult,  ['register-values']);
    try
      for I := 0 to List.Count - 1 do
      begin
        Item := List.Items[I];
        ValueList.Init(Item^.Name);
        Idx := StrToIntDef(Unquote(ValueList.Values['number']), -1);
        if (Idx < 0) or (Idx >= CtxRegNames.Count) then Continue;
        Value := Unquote(ValueList.Values['value']);
        Idx := Integer(CtxRegNames.Objects[Idx]);
        case Idx of
          33..40: // ST0..ST7
          begin
            Value := StringReplace(Value, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
            Value := StringReplace(Value, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
            FloatValue := StrToFloatDef(Value, 0);
            if SizeOf(FloatValue) <> 10 then
            begin
              Extended80Value := DoubleToExtended80(FloatValue);
              Move(Extended80Value, Result.FloatRegisters[Idx - 33].x87.Bytes[0], SizeOf(Extended80Value));
            end
            else
              {%H-}Move(FloatValue, Result.FloatRegisters[Idx - 33].x87.Bytes[0], SizeOf(FloatValue));
          end;
        end;
      end;
    finally
      List.Free;
    end;
  finally
    ValueList.Free;
  end;
  FLastIntelCtx := Result;
end;

function TGdbIntelDebugGate.CtxSetIntelContext(AThreadID: DWORD;
  const AContext: TIntelThreadContext): Boolean;
const
  set_hex = '-gdb-set $%s := 0x%x';
  set_str = '-gdb-set $%s%d%s := %s';

  function SetX87Reg(const ARegName: string; ARegID: Integer): string;
  var
    FloatValue: Extended;
    Extended80Value: TExtended80Support;
  begin
    Extended80Value := Default(TExtended80Support);
    Move(AContext.FloatRegisters[ARegID],
      Extended80Value, SizeOf(Extended80Value));
    FloatValue := Extended80ToDouble(Extended80Value);
    Result := Format(set_str, [ARegName, ARegID, '',
      StringReplace(FloatToStr(FloatValue),
        FormatSettings.DecimalSeparator, '.', [rfReplaceAll])]);
  end;

var
  Cmd: string;
  ExecResult: TGDBMIExecResult;
begin
  case AContext.ChangedRegID of
    0: Cmd := Format(set_hex, ['rax', AContext.Rax]);
    1: Cmd := Format(set_hex, ['rbx', AContext.Rbx]);
    2: Cmd := Format(set_hex, ['rcx', AContext.Rcx]);
    3: Cmd := Format(set_hex, ['rdx', AContext.Rdx]);
    4: Cmd := Format(set_hex, ['rbp', AContext.Rbp]);
    5: Cmd := Format(set_hex, ['rsp', AContext.Rsp]);
    6: Cmd := Format(set_hex, ['rsi', AContext.Rsi]);
    7: Cmd := Format(set_hex, ['rdi', AContext.Rdi]);
    8: Cmd := Format(set_hex, ['rip', AContext.Rip]);
    9..16: Cmd := Format(set_hex, ['r' + IntToStr(AContext.ChangedRegID - 1),
      AContext.R[AContext.ChangedRegID - 1]]);
    17, 91..99: Cmd := Format(set_hex, ['eflags', AContext.EFlags]);
    18: Cmd := Format(set_hex, ['gs', AContext.SegGs]);
    19: Cmd := Format(set_hex, ['fs', AContext.SegFs]);
    20: Cmd := Format(set_hex, ['es', AContext.SegEs]);
    21: Cmd := Format(set_hex, ['ds', AContext.SegDs]);
    22: Cmd := Format(set_hex, ['cs', AContext.SegCs]);
    23: Cmd := Format(set_hex, ['ss', AContext.SegSs]);
    24: Cmd := Format(set_hex, ['dr0', AContext.Dr0]);
    25: Cmd := Format(set_hex, ['dr1', AContext.Dr1]);
    26: Cmd := Format(set_hex, ['dr2', AContext.Dr2]);
    27: Cmd := Format(set_hex, ['dr3', AContext.Dr3]);
    28: Cmd := Format(set_hex, ['dr6', AContext.Dr6]);
    29: Cmd := Format(set_hex, ['dr7', AContext.Dr7]);
    30, 124..132: Cmd := Format(set_hex, ['fctrl', AContext.ControlWord]);
    31, 110..123: Cmd := Format(set_hex, ['fstat', AContext.StatusWord]);
    32, 102..109: Cmd := Format(set_hex, ['ftag', GetFXSaveTagWordFromTagWord(AContext.TagWord)]);
    33..40:
    begin
      // MM0..MM7
      // My version of GDB does not support MMX registers,
      // so this code, which works via FPU conversion, does not work correctly.
      // It is not yet clear how to do it correctly.
      Cmd := SetX87Reg('st', AContext.ChangedRegID - 33);
    end;
    41..48:
    begin
      // R0..R7
      Cmd := SetX87Reg('st', AContext.ChangedRegID - 41);
    end;
    49..56:
    begin
      // ST0..ST7
      Cmd := SetX87Reg('st', AContext.ChangedRegID - 49);
    end;
    57, 133..147: Cmd := Format(set_hex, ['mxcsr', AContext.MxCsr]);
    58..73:
    begin
      // XMM0..XMM15
      Cmd := Format(set_str, ['xmm', AContext.ChangedRegID - 58, '.v2_int64[0]',
        '0x' + RawBufToHex(@AContext.Ymm[AContext.ChangedRegID - 58].Low.Low, 8, True)]);
      Result := ExecuteGdbCommand(Cmd, [], ExecResult) and (ExecResult.State <> dsError);
      if not Result then Exit;
      Cmd := Format(set_str, ['xmm', AContext.ChangedRegID - 58, '.v2_int64[1]',
        '0x' + RawBufToHex(@AContext.Ymm[AContext.ChangedRegID - 58].Low.High, 8, True)]);
    end;
    74..89:
    begin
      // YMM0..YMM15
      Cmd := Format(set_str, ['ymm', AContext.ChangedRegID - 74, '.v4_int64[0]',
        '0x' + RawBufToHex(@AContext.Ymm[AContext.ChangedRegID - 74].Low.Low, 8, True)]);
      Result := ExecuteGdbCommand(Cmd, [], ExecResult) and (ExecResult.State <> dsError);
      if not Result then Exit;
      Cmd := Format(set_str, ['ymm', AContext.ChangedRegID - 74, '.v4_int64[1]',
        '0x' + RawBufToHex(@AContext.Ymm[AContext.ChangedRegID - 74].Low.High, 8, True)]);
      Result := ExecuteGdbCommand(Cmd, [], ExecResult) and (ExecResult.State <> dsError);
      if not Result then Exit;
      Cmd := Format(set_str, ['ymm', AContext.ChangedRegID - 74, '.v4_int64[2]',
        '0x' + RawBufToHex(@AContext.Ymm[AContext.ChangedRegID - 74].High.Low, 8, True)]);
      Result := ExecuteGdbCommand(Cmd, [], ExecResult) and (ExecResult.State <> dsError);
      if not Result then Exit;
      Cmd := Format(set_str, ['ymm', AContext.ChangedRegID - 74, '.v4_int64[3]',
        '0x' + RawBufToHex(@AContext.Ymm[AContext.ChangedRegID - 74].High.High, 8, True)]);
    end
  else
    Exit(False);
  end;
  Result := ExecuteGdbCommand(Cmd, [], ExecResult) and (ExecResult.State <> dsError);
end;

function TGdbIntelDebugGate.DasmParseItem(AItem: PGDBMINameValue; AList: TListEx<
  TGdbInstruction>): Boolean;
var
  ValueList: TGDBMINameValueList;
  Inst: TGdbInstruction;
  RawData, ExtendedData: string;
  DelimiterIdx: Integer;
begin
  Result := False;
  Inst := Default(TGdbInstruction);
  if (AItem^.Name.Len > 1) and (AItem^.Name.Ptr[0] = '{') and (AItem^.Value.Len = 0) then
  begin
    ValueList := TGDBMINameValueList.Create(AItem^.Name);
    try
      Inst.AddrVA := PCLenToQWord(ValueList.ValuesPtr['address'], 0);
      Inst.Offset := PCLenToQWord(ValueList.ValuesPtr['offset'], 0);
      Inst.FuncName := UnEscapeBackslashed(PCLenToString(ValueList.ValuesPtr['func-name'], True), [uefTab], 16);
      Result := Inst.AddrVA = DasmNewFuncAddrVA;
      if not Result then
      begin
        DasmNewFuncAddrVA := 0;
        Result := (Inst.Offset = 0) or (DasmLastFuncName = '');
        if not Result then
        begin
          Result := (Inst.FuncName = DasmLastFuncName) {and} or (Inst.Offset > DasmLastOffset);
          if not Result then
          begin
            DasmNewFuncAddrVA := Inst.AddrVA - Inst.Offset;
            if AList.Count > 0 then
              AList.List[AList.Count - 1].AsString := InvalidAsmLine;
            Exit;
          end;
        end;
      end;
      if DasmBaseStartAddr > Inst.AddrVA then
        Exit;
      RawData := Trim(UnEscapeBackslashed(PCLenToString(ValueList.ValuesPtr['inst'], True), [uefTab], 16));
      DelimiterIdx := Pos('#', RawData);
      if DelimiterIdx > 0 then
      begin
        ExtendedData := Trim(Copy(RawData, DelimiterIdx + 1, Length(RawData)));
        SetLength(RawData, DelimiterIdx - 1);
        DelimiterIdx := Pos('<', ExtendedData);
        if DelimiterIdx > 0 then
          ExtendedData := Copy(ExtendedData, 1, DelimiterIdx - 1);
        TryStrToUInt64(Trim(ExtendedData), Inst.InstrTargetVA);
      end;
      DelimiterIdx := Pos('<', RawData);
      if DelimiterIdx > 0 then
      begin
        SetLength(RawData, DelimiterIdx - 1);
        DelimiterIdx := Pos(' ', RawData);
        ExtendedData := Trim(Copy(RawData, DelimiterIdx + 1, Length(RawData)));
        TryStrToUInt64(ExtendedData, Inst.InstrTargetVA);
      end;
      Inst.AsString := StringReplace(Trim(UpperCase(RawData)), '0X', '0x', [rfReplaceAll]);
      while Pos('  ', Inst.AsString) > 0 do
        Inst.AsString := StringReplace(Inst.AsString, '  ', ' ', [rfReplaceAll]);
      if Inst.AsString.StartsWith('CALL') or Inst.AsString.StartsWith('REX.W CALL') then
        Inst.InstrType := itCall;
      if Inst.AsString.StartsWith('REX.W J') or (Inst.AsString[1] = 'J') then
        Inst.InstrType := itJump;
      Inst.IsJmp := Inst.InstrType <> itAny;
      Inst.IsRip := Pos('RIP', Inst.AsString) > 0;
      if Inst.IsJmp and Inst.IsRip then
        ReadMemory(Inst.InstrTargetVA, Inst.ExternalAddrVA, PointerSize);
      AList.Add(Inst);
    finally
      if Trim(Inst.AsString) = EndOnProcToken then
        DasmReset(False)
      else
      begin
        DasmLastFuncName := Inst.FuncName;
        DasmLastOffset := Inst.Offset;
      end;
      ValueList.Free;
    end;
  end
  else
    Assert(False, 'ParseItem: Invalid data');
end;

procedure TGdbIntelDebugGate.InitContext(AValue: TCommonCpuContext);
begin
  ContextQueryParams.GetDefContext := GDBGetIntelContext;
  ContextQueryParams.SetDefContext := GDBSetIntelContext;
  // Wow64 not supported
  ContextQueryParams.Get32Context := GetIntelWow64Context;
  ContextQueryParams.Set32Context := SetIntelWow64Context;
end;

procedure TGdbIntelDebugGate.FormatInstruction(var Inst: TInstruction;
  const GdbInst: TGdbInstruction; AShowCallFuncName: Boolean);

  procedure FormatRipHint;
  var
    RipSymbol: string;
  begin
    RipSymbol := Inst.Hint;
    if RipSymbol = '' then
      RipSymbol := GdbInst.FuncName;
    if GdbInst.InstrTargetVA = GdbInst.ExternalAddrVA then
      Inst.Hint := Format('RIP (0x%.1x) %s', [GdbInst.ExternalAddrVA, RipSymbol])
    else
      Inst.Hint := Format('RIP (0x%.1x -> 0x%.1x) %s', [GdbInst.InstrTargetVA, GdbInst.ExternalAddrVA, RipSymbol]);
  end;

var
  SpaceIndex, MemBracketIndex: Integer;
begin
  if GdbInst.IsJmp then
  begin
    Inst.JmpTo := GdbInst.InstrTargetVA;
    if GdbInst.ExternalAddrVA <> 0 then
    begin
      if AShowCallFuncName then
      begin
        SpaceIndex := Pos(' ', Inst.Mnemonic);
        // REX.W prefix processing
        if Inst.Mnemonic[1] = 'R' then
          SpaceIndex := PosEx(' ', Inst.Mnemonic, SpaceIndex + 1);
        MemBracketIndex := Pos(')', Inst.Mnemonic);
        if GdbInst.IsRip then
        begin
          FormatRipHint;
          Inst.JmpTo := GdbInst.ExternalAddrVA;
          Inst.JmpToPosStart := SpaceIndex + 1;
          Inst.JmpToPosLength := MemBracketIndex - SpaceIndex - 1;
        end
        else
        begin
          if (GdbInst.InstrType = itCall) and (Inst.Hint <> '') then
          begin
            Inst.JmpToPosStart := SpaceIndex;
            SetLength(Inst.Mnemonic, SpaceIndex);
            Inst.JmpToPosLength := Pos(' ', Inst.Hint) - 1;
            if Inst.JmpToPosLength <= 0 then
              Inst.JmpToPosLength := Length(Inst.Hint);
            Inst.Mnemonic := Inst.Mnemonic +
              Copy(Inst.Hint, 1, Inst.JmpToPosLength);
            Inst.Hint := '';
          end
          else
          begin
            Inst.JmpToPosStart := SpaceIndex;
            Inst.JmpToPosLength := Length(Inst.Mnemonic) - SpaceIndex;
          end;
        end;
      end
      else
      begin
        if Inst.Hint = '' then
          Inst.Hint := '0x' + IntToHex(GdbInst.ExternalAddrVA, 1)
        else
          if GdbInst.IsRip then
            Inst.Hint := '[0x' + IntToHex(GdbInst.ExternalAddrVA, 1) + '] -> ' + Inst.Hint;
      end;
    end;
  end
  else
    if GdbInst.IsRip and (GdbInst.ExternalAddrVA <> 0) then
      FormatRipHint;
end;

function TGdbIntelDebugGate.GetEndOnProcToken: string;
begin
  if Debugger = nil then
    Exit('');
  if IsExtendedSyntax and (PointerSize = 8) then
    Result := 'RETQ'
  else
    Result := 'RET';
end;

constructor TGdbIntelDebugGate.Create(AOwner: TComponent;
  AUtils: TCommonAbstractUtils);
begin
  inherited Create(AOwner, AUtils);
  FInstance := Self;
end;

destructor TGdbIntelDebugGate.Destroy;
begin
  FInstance := nil;
  inherited Destroy;
end;

function TGdbIntelDebugGate.GetTokenizerMode: TTokenizerMode;
begin
  if IsExtendedSyntax then
    Result := tmAtAntT
  else
    Result := tmIntel;
end;

function TGdbIntelDebugGate.IsExtendedSyntax: Boolean;
begin
  if gdb = nil then
    Result := False
  else
    Result := (TGDBMIDebuggerProperties(gdb.GetProperties).AssemblerStyle <> gdasIntel)
end;

function TGdbIntelDebugGate.ThreadStackLimit: TStackLimit;
{$IFDEF LINUX}
var
  RegionData: TRegionData;
{$ENDIF}
begin
  {$IFDEF LINUX}
  Result := Default(TStackLimit);
  if Utils.QueryRegion(FLastIntelCtx.Rsp, RegionData) then
  begin
    Result.Limit := RegionData.BaseAddr;
    Result.Base := RegionData.BaseAddr + RegionData.RegionSize;
  end;
  {$ELSE}
  Result := inherited ThreadStackLimit;
  {$ENDIF}
end;

end.
