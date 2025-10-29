////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.GdbDebug.Aarch64.pas
//  * Purpose   : Implementation of gateway to GDB for ARM64 AArch64 processor.
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

unit CpuView.GdbDebug.Aarch64;

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
    {$MESSAGE FATAL 'Win + Arm64 not implemented!'}
  {$ENDIF}

  SysUtils,
  StrUtils,
  Classes,

  DebugUtils,
  GDBMIMiscClasses,
  LazDebuggerIntfBaseTypes,
  GDBMIDebugger,

  FWHexView.Common,
  FWHexView.AsmTokenizer,
  CpuView.Common,
  CpuView.GdbDebug,
  CpuView.CPUContext,
  CpuView.DebugerGate,
  CpuView.Context.Params,
  CpuView.Context.Aarch64.Types,
  CpuView.Context.Aarch64;

type

  { TGdbAarch64DebugGate }

  TGdbAarch64DebugGate = class(TGdbAbstractDebugGate)
  class var
    FInstance: TGdbAarch64DebugGate;
  private
    FLastAarch64Ctx: TAarch64ThreadContext;
    FTokenizer: TAsmTokenizer;
    function CtxGetAarch64Context(AThreadID: DWORD): TAarch64ThreadContext;
    function CtxSetAarch64Context(AThreadID: DWORD; const AContext: TAarch64ThreadContext): Boolean;
  protected
    procedure CtxUpdateRegIndex; override;
    function DasmParseItem(AItem: PGDBMINameValue; AList: TListEx<TGdbInstruction>): Boolean; override;
    procedure InitContext(AValue: TCommonCpuContext); override;
    procedure FormatInstruction(var Inst: TInstruction;
      const GdbInst: TGdbInstruction; AShowCallFuncName: Boolean); override;
    function GetEndOnProcToken: string; override;
    class property Instance: TGdbAarch64DebugGate read FInstance write FInstance;
  public
    constructor Create(AOwner: TComponent; AUtils: TCommonAbstractUtils); override;
    destructor Destroy; override;
    function GetTokenizerMode: TTokenizerMode; override;
    function ThreadStackLimit: TStackLimit; override;
  end;

implementation

function GDBGetAarch64Context(AThreadID: DWORD): TAarch64ThreadContext;
begin
  Result := TGdbAarch64DebugGate.Instance.CtxGetAarch64Context(AThreadID);
end;

function GDBSetAarch64Context(AThreadID: DWORD; const AContext: TAarch64ThreadContext): Boolean;
begin
  Result := TGdbAarch64DebugGate.Instance.CtxSetAarch64Context(AThreadID, AContext);
end;

function GDBGetAarch32Context(AThreadID: DWORD): TAarch64ThreadContext;
begin
  Result := TGdbAarch64DebugGate.Instance.CtxGetAarch64Context(AThreadID);
end;

function GDBSetAarch32Context(AThreadID: DWORD; const AContext: TAarch64ThreadContext): Boolean;
begin
  Result := TGdbAarch64DebugGate.Instance.CtxSetAarch64Context(AThreadID, AContext);
end;

{ TGdbAarch64DebugGate }

procedure TGdbAarch64DebugGate.CtxUpdateRegIndex;
const
  ExtendedRegNames64: array[0..66] of string = (
    'x0', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'x7', 'x8', 'x9',
    'x10', 'x11', 'x12', 'x13', 'x14', 'x15', 'x16', 'x17', 'x18', 'x19',
    'x20', 'x21', 'x22', 'x23', 'x24', 'x25', 'x26', 'x27', 'x28', 'x29',
    'x30', 'sp', 'pc', 'cpsr',
    'v0', 'v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8', 'v9',
    'v10', 'v11', 'v12', 'v13', 'v14', 'v15', 'v16', 'v17', 'v18', 'v19',
    'v20', 'v21', 'v22', 'v23', 'v24', 'v25', 'v26', 'v27', 'v28', 'v29',
    'v30', 'fpsr', 'fpcr'
  );
  ExtendedRegNames32: array[0..66] of string = (
    'r0', 'r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7', 'r8', 'r9',
    'r10', 'r11', 'r12', 'r13', 'r14', 'r15', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '',
    'sp', 'pc', 'cpsr',
    'q0', 'q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8', 'q9',
    'q10', 'q11', 'q12', 'q13', 'q14', 'q15', '', '', '', '',
    '', '', '', '', '', '', '', '', '', '', '', 'fpsr', 'fpcr'
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

function TGdbAarch64DebugGate.CtxGetAarch64Context(AThreadID: DWORD
  ): TAarch64ThreadContext;

  function LoadV2Int64(AValue: string;
    var ARegVal: TArm64_Neon128): Boolean;
  const
    UInt64Mark = 'u = {';
  var
    Idx: Integer;
    List: TStringList;
  begin
    Result := False;
    ARegVal := Default(TArm64_Neon128);
    Idx := Pos(UInt64Mark, AValue);
    if Idx = 0 then Exit;
    Inc(Idx, Length(UInt64Mark));
    AValue := Copy(AValue, Idx, Pos('}', AValue, Idx) - Idx);
    List := TStringList.Create;
    try
      List.Delimiter := ',';
      List.DelimitedText := AValue;
      Result := List.Count >= 2;
      if not Result then Exit;
      ARegVal.Q[0] := StrToUInt64Def(Trim(List[0]), 0);
      ARegVal.Q[1] := StrToUInt64Def(Trim(List[1]), 0);
    finally
      List.Free;
    end;
  end;

var
  ExecResult: TGDBMIExecResult;
  List, ValueList, SimdList: TGDBMINameValueList;
  I, A, Idx: Integer;
  Item: PGDBMINameValue;
  Value: string;
  FloatValue: Extended;
begin
  Result := Default(TAarch64ThreadContext);
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
          0..28: Result.X[Idx] := StrToInt64Def(Value, 0);
          29: Result.Fp := StrToInt64Def(Value, 0);
          30: Result.Lr := StrToInt64Def(Value, 0);
          31: Result.Sp := StrToInt64Def(Value, 0);
          32: Result.Pc := StrToInt64Def(Value, 0);
          33: Result.Cpsr := StrToInt64Def(Value, 0);
          34..64: LoadV2Int64(Value, Result.V[Idx - 34]);
          65: Result.Fpsr := StrToInt64Def(Value, 0);
          66: Result.Fpcr := StrToInt64Def(Value, 0);
        end;
      end;
    finally
      List.Free;
    end;
  finally
    ValueList.Free;
  end;
  FLastAarch64Ctx := Result;
end;

function TGdbAarch64DebugGate.CtxSetAarch64Context(AThreadID: DWORD;
  const AContext: TAarch64ThreadContext): Boolean;
const
  set_hex = '-gdb-set $%s := 0x%x';
  set_V = '-gdb-set $v%d.d.u[%d] := 0x%x';
var
  Cmd: string;
  ExecResult: TGDBMIExecResult;
begin
  case AContext.ChangedRegID of
    0..28: Cmd := Format(set_hex, ['x' + IntToStr(AContext.ChangedRegID),
      AContext.X[AContext.ChangedRegID]]);
    31: Cmd := Format(set_hex, ['x29', AContext.Fp]);
    32: Cmd := Format(set_hex, ['x30', AContext.Lr]);
    33: Cmd := Format(set_hex, ['sp', AContext.Sp]);
    34: Cmd := Format(set_hex, ['pc', AContext.Pc]);
    35, 134..148: Cmd := Format(set_hex, ['cpsr', AContext.Cpsr]);
    36..67: Cmd := Format(set_hex, ['s' + IntToStr(AContext.ChangedRegID - 36),
      PCardinal(@AContext.V[AContext.ChangedRegID - 36].S[0])^]);
    68..99: Cmd := Format(set_hex, ['d' + IntToStr(AContext.ChangedRegID - 68),
      AContext.V[AContext.ChangedRegID - 68].Q[0]]);
    100..131:
    begin
      Cmd := Format(set_V, [AContext.ChangedRegID - 100, 0,
        AContext.V[AContext.ChangedRegID - 100].Q[0]]);
      Result := ExecuteGdbCommand(Cmd, [], ExecResult) and (ExecResult.State <> dsError);
      if not Result then Exit;
      Cmd := Format(set_V, [AContext.ChangedRegID - 100, 1,
        AContext.V[AContext.ChangedRegID - 100].Q[1]]);
    end;
    132, 152..166: Cmd := Format(set_hex, ['fpcr', AContext.Fpcr]);
    133, 169..179: Cmd := Format(set_hex, ['fpsr', AContext.Fpsr]);
  else
    Exit(False);
  end;
  Result := ExecuteGdbCommand(Cmd, [], ExecResult) and (ExecResult.State <> dsError);
end;

function TGdbAarch64DebugGate.DasmParseItem(AItem: PGDBMINameValue; AList: TListEx<
  TGdbInstruction>): Boolean;
var
  ValueList: TGDBMINameValueList;
  Inst: TGdbInstruction;
  RawData, ExtendedData: string;
  DelimiterIdx, TokenLength: Integer;
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
      DelimiterIdx := Pos('// ', RawData);
      if DelimiterIdx > 0 then
        SetLength(RawData, DelimiterIdx - 1);
      DelimiterIdx := Pos('<', RawData);
      if DelimiterIdx > 0 then
        SetLength(RawData, DelimiterIdx - 1);
      RawData := Trim(RawData);
      DelimiterIdx := RawData.LastDelimiter(' ');
      if (Length(RawData) > DelimiterIdx) and (RawData[DelimiterIdx + 1] = '#') then
        Inc(DelimiterIdx);
      ExtendedData := Trim(Copy(RawData, DelimiterIdx + 1, Length(RawData)));
      if (Length(ExtendedData) > 0) and (ExtendedData[1] <> 'x') then
        TryStrToUInt64(ExtendedData, Inst.InstrTargetVA);
      Inst.AsString := StringReplace(Trim(UpperCase(RawData)), '0X', '0x', [rfReplaceAll]);
      while Pos('  ', Inst.AsString) > 0 do
        Inst.AsString := StringReplace(Inst.AsString, '  ', ' ', [rfReplaceAll]);
      TokenLength := Length(Inst.AsString);
      if FTokenizer.GetToken(@Inst.AsString[1], TokenLength) = ttJmp then
      begin
        if Inst.AsString.StartsWith('BL ') or Inst.AsString.StartsWith('BLR ') then
          Inst.InstrType := itCall
        else
          Inst.InstrType := itJump;
        Inst.IsJmp := True;
      end;
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

procedure TGdbAarch64DebugGate.InitContext(AValue: TCommonCpuContext);
begin
  ContextQueryParams.GetDefContext := GDBGetAarch64Context;
  ContextQueryParams.SetDefContext := GDBSetAarch64Context;
  ContextQueryParams.Get32Context := GDBGetAarch32Context;
  ContextQueryParams.Set32Context := GDBSetAarch32Context;
end;

procedure TGdbAarch64DebugGate.FormatInstruction(var Inst: TInstruction;
  const GdbInst: TGdbInstruction; AShowCallFuncName: Boolean);
var
  SpaceIndex: Integer;
begin
  if GdbInst.IsJmp then
  begin
    Inst.JmpTo := GdbInst.InstrTargetVA;
    if GdbInst.ExternalAddrVA <> 0 then
    begin
      if AShowCallFuncName then
      begin
        SpaceIndex := Inst.Mnemonic.LastDelimiter(' ') + 1;
        if (GdbInst.InstrType = itCall) and (Inst.Hint <> '') then
        begin
          Inst.JmpToPosStart := SpaceIndex;
          SetLength(Inst.Mnemonic, SpaceIndex);
          Inst.JmpToPosLength := Pos(' ', Inst.Hint) - 1;
          if Inst.JmpToPosLength <= 0 then
            Inst.JmpToPosLength := Length(Inst.Hint);
          Inst.Mnemonic := Inst.Mnemonic +
            Copy(Inst.Hint, 1, Inst.JmpToPosLength);
          Delete(Inst.Hint, 1, Inst.JmpToPosLength);
          Inst.Hint := Trim(Inst.Hint + ' [0x' +
            IntToHex(GdbInst.ExternalAddrVA, 1) + ']');
        end
        else
        begin
          Inst.JmpToPosStart := SpaceIndex;
          Inst.JmpToPosLength := Length(Inst.Mnemonic) - SpaceIndex;
        end;
      end
      else
      begin
        if Inst.Hint = '' then
          Inst.Hint := '0x' + IntToHex(GdbInst.ExternalAddrVA, 1)
        else
          Inst.Hint := '[0x' + IntToHex(GdbInst.ExternalAddrVA, 1) + '] -> ' + Inst.Hint;
      end;
    end;
  end
end;

function TGdbAarch64DebugGate.GetEndOnProcToken: string;
begin
  Result := 'RET';
end;

constructor TGdbAarch64DebugGate.Create(AOwner: TComponent;
  AUtils: TCommonAbstractUtils);
begin
  inherited Create(AOwner, AUtils);
  FInstance := Self;
  FTokenizer := TAsmTokenizer.Create;
  FTokenizer.TokenizerMode := tmArm;
end;

destructor TGdbAarch64DebugGate.Destroy;
begin
  FInstance := nil;
  FTokenizer.Free;
  inherited Destroy;
end;

function TGdbAarch64DebugGate.GetTokenizerMode: TTokenizerMode;
begin
  Result := tmArm;
end;

function TGdbAarch64DebugGate.ThreadStackLimit: TStackLimit;
{$IFDEF LINUX}
var
  RegionData: TRegionData;
{$ENDIF}
begin
  {$IFDEF LINUX}
  Result := Default(TStackLimit);
  if Utils.QueryRegion(FLastAarch64Ctx.Sp, RegionData) then
  begin
    Result.Limit := RegionData.BaseAddr;
    Result.Base := RegionData.BaseAddr + RegionData.RegionSize;
  end;
  {$ELSE}
  Result := inherited ThreadStackLimit;
  {$ENDIF}
end;

end.
