unit CpuView.IntelContext;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils,
  Classes,
  FWHexView.Common,
  CpuView.Common,
  CpuView.CPUContext,
{$IFDEF MSWINDOWS}
  CpuView.Windows,
{$ENDIF}
  CpuView.IntelContext.Types;

type

  TFPUMode = (fpuMMX, fpuR, fpuST);

  TIntelCpuMapMode = (icmDetailed, icmSimple);

  { TIntelCpuContext }

  TIntelCpuContext = class(TCommonCpuContext)
  strict private
    FContext: TIntelThreadContext;
    FMapMode: TIntelCpuMapMode;
    FFPUMode: TFPUMode;
    FShowDebug: Boolean;
    FShowXMM: Boolean;
    FShowYMM: Boolean;
    FShowFPU: Boolean;
    FSavedDetailed: array [0..3] of Boolean;
    procedure SetContext(const Value: TIntelThreadContext);
    procedure SetFPUMode(const Value: TFPUMode);
    procedure SetMapMode(const Value: TIntelCpuMapMode);
    procedure SetSavedDetailed(Index: Integer; Value: Boolean);
    procedure SetShowDebug(const Value: Boolean);
    procedure SetShowFPU(const Value: Boolean);
    procedure SetShowXMM(const Value: Boolean);
    procedure SetShowYMM(const Value: Boolean);
    procedure UpdateModifyed(const Value: TIntelThreadContext);
  protected
    procedure BuildMap; override;
    procedure DoChangeViewMode(RegID: Integer; const Value: TRegViewMode); override;
    function GetRegValue(RegID: Integer; out ARegValue: UInt64): Boolean; override;
    procedure InitKnownRegs; override;
    procedure UpdateLastRegData(RegID: Integer); override;
  protected
    function StToR(Index: Integer): Integer;
    function RToSt(Index: Integer): Integer;
    function ExtractControlWordPcRc: string;
    function ExtractControlWordPrecision: string;
    function ExtractControlWordPrecisionValue: Byte;
    function ExtractControlWordRounding: string;
    function ExtractControlWordRoundingValue: Byte;
    function ExtractJmpConditions: string;
    function ExtractStatusWordTop: string;
    function ExtractStatusWordTopValue: Byte;
    function ExtractTagWordSet(Index: Integer): string;
    function ExtractTagWordSetValue(Index: Integer): Byte;
    function ExtractMxCsrRounding: string;
    function ExtractMxCsrRoundingValue: Byte;
    procedure SetTagWordSetValue(Index: Integer; AValue: Byte);
    procedure SetStatusWordTopValue(AValue: Byte);
    procedure SetControlWordPrecisionValue(AValue: Byte);
    procedure SetControlWordRoundingValue(AValue: Byte);
    procedure SetMxCsrRoundingValue(AValue: Byte);
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitDefault; override;
    function InstructonPoint: UInt64; override;
    function IsActiveJump(const Value: string): Boolean; override;
    function PointerSize: Integer; override;
    function QueryRegValueByName(const RegName: string; out RegValue: UInt64): Boolean; override;
    function RegQueryValue(RowIndex, RegIndex: Integer; out ARegValue: UInt64): Boolean; override;
    function RegSetValueAtIndex(RegID, Index: Integer): string; override;
    function RegSetValueCount(RegID: Integer): Integer; override;
    function StackBase: UInt64; override;
    function StackPoint: UInt64; override;
    function Update(CurrentIP: UInt64 = 0): Boolean; override;
    function UpdateRegValue(RegID: Integer; ANewRegValue: UInt64): Boolean; override;
    property Context: TIntelThreadContext read FContext write SetContext;
    property FPUMode: TFPUMode read FFPUMode write SetFPUMode;
    property MapMode: TIntelCpuMapMode read FMapMode write SetMapMode;
    property ShowDebug: Boolean read FShowDebug write SetShowDebug;
    property ShowFPU: Boolean read FShowFPU write SetShowFPU;
    property ShowXMM: Boolean read FShowXMM write SetShowXMM;
    property ShowYMM: Boolean read FShowYMM write SetShowYMM;
  end;

implementation

{ TIntelCpuContext }

procedure TIntelCpuContext.BuildMap;

  function Add(Root: TRegMap; RegID: Integer): TRegMap;
  begin
    Result := Root.Add;
    Result.RegID := RegID;
  end;

  procedure AddSeparator;
  var
    ASeparator: TRegMap;
  begin
    ASeparator := Map.Add;
    ASeparator.RegID := -1;
  end;

var
  Row: TRegMap;
  I: Integer;
begin
  Map.Clear;

  Row := Add(Map, 0);          // RAX
  Add(Row, 158);               // RAX Debug Hint
  Row := Add(Map, 1);          // RBX
  Add(Row, 159);               // RBX Debug Hint
  Row := Add(Map, 2);          // RCX
  Add(Row, 160);               // RCX Debug Hint
  Row := Add(Map, 3);          // RDX
  Add(Row, 161);               // RDX Debug Hint
  Row := Add(Map, 4);          // RSP
  Add(Row, 162);               // RSP Debug Hint
  Row := Add(Map, 5);          // RBP
  Add(Row, 163);               // RBP Debug Hint
  Row := Add(Map, 6);          // RSI
  Add(Row, 164);               // RSI Debug Hint
  Row := Add(Map, 7);          // RDI
  Add(Row, 165);               // RDI Debug Hint

  if not Context.x86Context then
  begin
    AddSeparator;
    Row := Add(Map, 9);        // R8
    Add(Row, 167);             // R8 Debug Hint
    Row := Add(Map, 10);       // R9
    Add(Row, 168);             // R9 Debug Hint
    Row := Add(Map, 11);       // R10
    Add(Row, 169);             // R10 Debug Hint
    Row := Add(Map, 12);       // R11
    Add(Row, 170);             // R11 Debug Hint
    Row := Add(Map, 13);       // R12
    Add(Row, 171);             // R12 Debug Hint
    Row := Add(Map, 14);       // R13
    Add(Row, 172);             // R13 Debug Hint
    Row := Add(Map, 15);       // R14
    Add(Row, 173);             // R14 Debug Hint
    Row := Add(Map, 16);       // R15
    Add(Row, 174);             // R15 Debug Hint
  end;

  AddSeparator;

  Row := Add(Map, 8);          // RIP
  Add(Row, 166);               // RIP Debug Hint

  AddSeparator;

  if MapMode = icmDetailed then
  begin
    Row := Add(Map, 17);  // EFlags
    Add(Row, 90);         // Jmp Hint
    Row := Add(Map, 91);  // CF
    Add(Row, 92);         // PF
    Add(Row, 93);         // AF
    Row := Add(Map, 94);  // ZF
    Add(Row, 95);         // SF
    Add(Row, 96);         // TF
    Row := Add(Map, 97);  // IF
    Add(Row, 98);         // DF
    Add(Row, 99);         // OF

    AddSeparator;

    Add(Map, 100);        // LastError
    Add(Map, 101);        // LastStatus

    AddSeparator;

    Row := Add(Map, 18);  // SegGs
    Add(Row, 19);         // SegFs
    Row := Add(Map, 20);  // SegEs
    Add(Row, 21);         // SegDs
    Row := Add(Map, 22);  // SegCs
    Add(Row, 23);         // SegSs
  end
  else
  begin
    Row := Add(Map, 91);  // CF
    Add(Row, 20);         // SegEs
    Row := Add(Map, 92);  // PF
    Add(Row, 22);         // SegCs
    Row := Add(Map, 93);  // AF
    Add(Row, 23);         // SegSs
    Row := Add(Map, 94);  // ZF
    Add(Row, 21);         // SegDs
    Row := Add(Map, 95);  // SF
    Add(Row, 19);         // SegFs
    Row := Add(Map, 96);  // TF
    Add(Row, 18);         // SegGs
    Add(Map, 97);         // IF
    Row := Add(Map, 98);  // DF
    Add(Row, 100);        // LastError
    Row := Add(Map, 99);  // OF
    Add(Row, 101);        // LastStatus

    AddSeparator;

    Row := Add(Map, 17);  // EFlags
    Add(Row, 90);         // Jmp Hint
  end;

  if ShowFPU then
  begin
    AddSeparator;

    case FPUMode of
      fpuMMX: // MM0..MM7
        for I := 33 to 40 do
          Add(Map, I);
      fpuR:
        for I := 41 to 48 do
        begin
          Row := Add(Map, I);     // R0..R7
          Add(Row, 148 + I - 41); // STx Hint
        end;
      fpuST:
        for I := 49 to 56 do
        begin
          Row := Add(Map, I);     // ST0..ST7
          Add(Row, 148 + I - 49); // SRx Hint
        end;
    end;

    if MapMode = icmDetailed then
    begin
      AddSeparator;

      Add(Map, 32);         // x87TagWord
      Row := Add(Map, 102); // TW0
      Add(Row, 103);        // TW1
      Row := Add(Map, 104); // TW2
      Add(Row, 105);        // TW3
      Row := Add(Map, 106); // TW4
      Add(Row, 107);        // TW5
      Row := Add(Map, 108); // TW6
      Add(Row, 109);        // TW7

      AddSeparator;

      Row := Add(Map, 31);  // x87StatusWord
      Add(Row, 110);        // ES
      Row := Add(Map, 111); // IE
      Add(Row, 112);        // DE
      Add(Row, 113);        // ZE
      Add(Row, 114);        // OE
      Add(Row, 115);        // UE
      Row := Add(Map, 116); // B
      Add(Row, 117);        // C0
      Add(Row, 118);        // C1
      Add(Row, 119);        // C2
      Add(Row, 120);        // C3
      Row := Add(Map, 121); // PF
      Add(Row, 122);        // SF
      Add(Row, 123);        // TOP

      AddSeparator;

      Add(Map, 30);         // x87ControlWord
      Row := Add(Map, 124); // IM
      Add(Row, 125);        // DM
      Add(Row, 126);        // ZM
      Add(Row, 127);        // OM
      Add(Row, 128);        // UM
      Row := Add(Map, 129); // PM
      Add(Row, 130);        // PC
      Row := Add(Map, 131); // IC
      Add(Row, 132);        // RC
    end
    else
    begin
      Add(Map, 156);        // Hint Caption
      Row := Add(Map, 31);  // x87StatusWord
      Add(Row, 120);        // C3
      Add(Row, 119);        // C2
      Add(Row, 118);        // C1
      Add(Row, 117);        // C0
      Add(Row, 110);        // ES
      Add(Row, 122);        // SF
      Add(Row, 121);        // PF
      Add(Row, 115);        // UE
      Add(Row, 114);        // OE
      Add(Row, 113);        // ZE
      Add(Row, 112);        // DE
      Add(Row, 111);        // IE
      Row := Add(Map, 30);  // x87ControlWord
      Add(Row, 157);        // precision + rounding for Simple Mode
      Add(Row, 129);        // PM
      Add(Row, 128);        // UM
      Add(Row, 127);        // OM
      Add(Row, 126);        // ZM
      Add(Row, 125);        // DM
      Add(Row, 124);        // IM
    end;
  end;

  if ShowXMM or ShowYMM then
  begin

    AddSeparator;

    Add(Map, 57);         // MxCsr
    Row := Add(Map, 133); // IE
    Add(Row, 134);        // DE
    Add(Row, 135);        // ZE
    Add(Row, 136);        // UE
    Add(Row, 137);        // UE
    Add(Row, 138);        // PE
    Row := Add(Map, 139); // IM
    Add(Row, 140);        // DM
    Add(Row, 141);        // ZM
    Add(Row, 142);        // UM
    Add(Row, 143);        // UM
    Add(Row, 144);        // PM
    Row := Add(Map, 145); // DAZ
    Add(Row, 146);        // FTZ
    Add(Row, 147);        // RC

    // XMM0..XMM15
    if ShowXMM then
    begin
      AddSeparator;
      for I := 58 to 58 + Context.XmmCount - 1 do
        Add(Map, I);
    end;

    // YMM0..YMM15
    if ShowYMM and Context.YmmPresent then
    begin
      AddSeparator;
      for I := 74 to 74 + Context.XmmCount - 1 do
        Add(Map, I);
    end;

  end;

  // DR0..DR7
  if ShowDebug then
  begin
    AddSeparator;
    for I := 24 to 27 do
      Add(Map, I);

    AddSeparator;

    Add(Map, 28);
    Add(Map, 29);
  end;
end;

constructor TIntelCpuContext.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FFPUMode := fpuST;
  FShowDebug := True;
  FShowXMM := True;
  FShowFPU := True;
  for I := 0 to 3 do
    FSavedDetailed[I] := I <> 2;
end;

procedure TIntelCpuContext.DoChangeViewMode(RegID: Integer;
  const Value: TRegViewMode);
var
  I: Integer;
begin
  case RegID of
    33..40: // MMX
      for I := 33 to 40 do
        KnownRegs.List[I].ViewMode := Value;
    41..48: // x87 Rx
      for I := 41 to 48 do
        KnownRegs.List[I].ViewMode := Value;
    49..56: // x87 ST(x)
      for I := 49 to 56 do
        KnownRegs.List[I].ViewMode := Value;
    58..73: // XMM
      for I := 58 to 73 do
        KnownRegs.List[I].ViewMode := Value;
    74..89: // YMM
      for I := 74 to 89 do
        KnownRegs.List[I].ViewMode := Value;
  else
    KnownRegs.List[RegID].ViewMode := Value;
  end;
end;

function TIntelCpuContext.ExtractControlWordPcRc: string;
begin
  case ExtractControlWordRoundingValue of
    0: Result := 'NEAR';
    1: Result := 'DOWN';
    2: Result := '  UP';
    3: Result := 'ZERO';
  end;
  case ExtractControlWordPrecisionValue of
    0: Result := Result + ',24';
    1: Result := Result + ',??';
    2: Result := Result + ',53';
    3: Result := Result + ',64';
  end;
end;

function TIntelCpuContext.ExtractControlWordPrecision: string;
begin
  Result := RegSetValueAtIndex(130, ExtractControlWordPrecisionValue);
end;

function TIntelCpuContext.ExtractControlWordPrecisionValue: Byte;
begin
  Result := (Context.ControlWord shr 8) and 3;
end;

function TIntelCpuContext.ExtractControlWordRounding: string;
begin
  Result := RegSetValueAtIndex(132, ExtractControlWordRoundingValue);
end;

function TIntelCpuContext.ExtractControlWordRoundingValue: Byte;
begin
  Result := (Context.ControlWord shr 10) and 3;
end;

function TIntelCpuContext.ExtractJmpConditions: string;
var
  ZF, CF, SF, _OF: Boolean;
begin
  CF := FContext.EFlags and (1 shl 0) <> 0;
  ZF := FContext.EFlags and (1 shl 6) <> 0;
  SF := FContext.EFlags and (1 shl 7) <> 0;
  _OF := FContext.EFlags and (1 shl 11) <> 0;

  if ZF then
    Result := 'E,Z'
  else
    Result := 'NE,NZ';

  if CF or ZF then
    Result := Result + ',BE,NA'
  else
    Result := Result + ',A,NBE';

  if CF then
    Result := Result + ',B,NAE,C'
  else
    Result := Result + ',AE,NB,NC';

  if not (SF or ZF) then
    Result := Result + ',G,NLE';

  if SF = _OF then
    Result := Result + ',GE,NL'
  else
    Result := Result + ',L,NGE';

  if (SF <> _OF) or ZF then
    Result := Result + ',LE,NG'
end;

function TIntelCpuContext.ExtractMxCsrRounding: string;
begin
  Result := RegSetValueAtIndex(147, ExtractMxCsrRoundingValue);
end;

function TIntelCpuContext.ExtractMxCsrRoundingValue: Byte;
begin
  Result := (Context.MxCsr shr 13) and 3;
end;

function TIntelCpuContext.ExtractStatusWordTop: string;
var
  Idx: Byte;
begin
  Idx := ExtractStatusWordTopValue;
  Result := Format('ST0=%s (%d)', [RegSetValueAtIndex(123, Idx), Idx]);
end;

function TIntelCpuContext.ExtractStatusWordTopValue: Byte;
begin
  Result := (Context.StatusWord shr 11) and 7;
end;

function TIntelCpuContext.ExtractTagWordSet(Index: Integer): string;
begin
  Result := RegSetValueAtIndex(102, ExtractTagWordSetValue(Index));
end;

function TIntelCpuContext.ExtractTagWordSetValue(Index: Integer): Byte;
begin
  Index := Index shl 1;
  Result := (Context.TagWord shr Index) and 3;
end;

function TIntelCpuContext.GetRegValue(RegID: Integer; out ARegValue: UInt64): Boolean;
begin
  Result := True;
  case RegID of
    0: ARegValue := Context.Rax;
    1: ARegValue := Context.Rbx;
    2: ARegValue := Context.Rcx;
    3: ARegValue := Context.Rdx;
    4: ARegValue := Context.Rbp;
    5: ARegValue := Context.Rsp;
    6: ARegValue := Context.Rsi;
    7: ARegValue := Context.Rdi;
    8: ARegValue := Context.Rip;
    9..16: ARegValue := Context.R[RegID - 1];
    17: ARegValue := Context.EFlags;
    18: ARegValue := Context.SegGs;
    19: ARegValue := Context.SegFs;
    20: ARegValue := Context.SegEs;
    21: ARegValue := Context.SegDs;
    22: ARegValue := Context.SegCs;
    23: ARegValue := Context.SegSs;
    24: ARegValue := Context.Dr0;
    25: ARegValue := Context.Dr1;
    26: ARegValue := Context.Dr2;
    27: ARegValue := Context.Dr3;
    28: ARegValue := Context.Dr6;
    29: ARegValue := Context.Dr7;
    30: ARegValue := Context.ControlWord;
    31: ARegValue := Context.StatusWord;
    32: ARegValue := Context.TagWord;
    57: ARegValue := Context.MxCsr;
    91: ARegValue := ExtractBitValue(Context.EFlags, 0);        // CF
    92: ARegValue := ExtractBitValue(Context.EFlags, 2);        // PF
    93: ARegValue := ExtractBitValue(Context.EFlags, 4);        // AF
    94: ARegValue := ExtractBitValue(Context.EFlags, 6);        // ZF
    95: ARegValue := ExtractBitValue(Context.EFlags, 7);        // SF
    96: ARegValue := ExtractBitValue(Context.EFlags, 8);        // TF
    97: ARegValue := ExtractBitValue(Context.EFlags, 9);        // IF
    98: ARegValue := ExtractBitValue(Context.EFlags, 10);       // DF
    99: ARegValue := ExtractBitValue(Context.EFlags, 11);       // OF
    100: ARegValue := Context.LastError;
    101: ARegValue := Context.LastStatus;
    102..109: ARegValue := ExtractTagWordSetValue(RegID - 102); // TW set
    110: ARegValue := ExtractBitValue(Context.StatusWord, 7);   // ES
    111: ARegValue := ExtractBitValue(Context.StatusWord, 0);   // IE
    112: ARegValue := ExtractBitValue(Context.StatusWord, 1);   // DE
    113: ARegValue := ExtractBitValue(Context.StatusWord, 2);   // ZE
    114: ARegValue := ExtractBitValue(Context.StatusWord, 3);   // OE
    115: ARegValue := ExtractBitValue(Context.StatusWord, 4);   // UE
    116: ARegValue := ExtractBitValue(Context.StatusWord, 15);  // B
    117: ARegValue := ExtractBitValue(Context.StatusWord, 8);   // C0
    118: ARegValue := ExtractBitValue(Context.StatusWord, 9);   // C1
    119: ARegValue := ExtractBitValue(Context.StatusWord, 10);  // C2
    120: ARegValue := ExtractBitValue(Context.StatusWord, 14);  // C3
    121: ARegValue := ExtractBitValue(Context.StatusWord, 5);   // PE
    122: ARegValue := ExtractBitValue(Context.StatusWord, 6);   // SF
    123: ARegValue := ExtractStatusWordTopValue;                // TOP set
    124: ARegValue := ExtractBitValue(Context.ControlWord, 0);  // IM
    125: ARegValue := ExtractBitValue(Context.ControlWord, 1);  // DM
    126: ARegValue := ExtractBitValue(Context.ControlWord, 2);  // ZM
    127: ARegValue := ExtractBitValue(Context.ControlWord, 3);  // OM
    128: ARegValue := ExtractBitValue(Context.ControlWord, 4);  // UM
    129: ARegValue := ExtractBitValue(Context.ControlWord, 5);  // PM
    130: ARegValue := ExtractControlWordPrecisionValue;         // WordPrecision set
    131: ARegValue := ExtractBitValue(Context.ControlWord, 12); // IC
    132: ARegValue := ExtractControlWordRoundingValue;          // WordRounding set
    133: ARegValue := ExtractBitValue(Context.MxCsr, 0);        // IE
    134: ARegValue := ExtractBitValue(Context.MxCsr, 1);        // DE
    135: ARegValue := ExtractBitValue(Context.MxCsr, 2);        // ZE
    136: ARegValue := ExtractBitValue(Context.MxCsr, 3);        // OE
    137: ARegValue := ExtractBitValue(Context.MxCsr, 4);        // UE
    138: ARegValue := ExtractBitValue(Context.MxCsr, 5);        // PE
    139: ARegValue := ExtractBitValue(Context.MxCsr, 7);        // IM
    140: ARegValue := ExtractBitValue(Context.MxCsr, 8);        // DM
    141: ARegValue := ExtractBitValue(Context.MxCsr, 9);        // ZM
    142: ARegValue := ExtractBitValue(Context.MxCsr, 10);       // OM
    143: ARegValue := ExtractBitValue(Context.MxCsr, 11);       // UM
    144: ARegValue := ExtractBitValue(Context.MxCsr, 12);       // PM
    145: ARegValue := ExtractBitValue(Context.MxCsr, 6);        // DAZ
    146: ARegValue := ExtractBitValue(Context.MxCsr, 15);       // FTZ
    147: ARegValue := ExtractMxCsrRoundingValue;                // RC
  else
    ARegValue := 0;
    Result := False;
  end;
end;

procedure TIntelCpuContext.InitDefault;
var
  I: Integer;
begin
  FMapMode := icmDetailed;
  FFPUMode := fpuST;
  FShowDebug := True;
  FShowXMM := True;
  FShowYMM := False;
  FShowFPU := True;
  for I := 0 to 3 do
    FSavedDetailed[I] := I <> 2;
  UpdateMap(True);
end;

procedure TIntelCpuContext.InitKnownRegs;

  procedure Add(RegType: TContextRegType; SupportedViewMode: TRegViewModes;
    ModifyActions: TModifyActions = []);
  var
    R: TRegParam;
  begin
    R.RowIndex := -1;
    R.ColIndex := -1;
    R.RegType := RegType;
    R.Modifyed := False;
    case RegType of
      crtBitValue: ModifyActions := [maToggle];
      crtSetValue: ModifyActions := [maChange];
    end;
    R.ModifyActions := ModifyActions;
    R.SupportedViewMode := SupportedViewMode;
    R.ViewMode := rvmHex;
    KnownRegs.Add(R);
  end;

var
  vmDefRegMod: TRegViewModes;
  I: Integer;
begin
  KnownRegs.Clear;

  if Context.x86Context then
    vmDefRegMod := vmReg32
  else
    vmDefRegMod := vmReg64;

  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 000 - RAX
  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 001 - RBX
  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 002 - RCX
  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 003 - RDX
  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 004 - RBP
  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 005 - RSP
  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 006 - RSI
  Add(crtValue, vmDefRegMod, [maIncrement..maChange]);   // 007 - RDI
  Add(crtValue, vmDefOnly, [maChange]);                  // 008 - RIP

  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 009 - R8
  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 010 - R9
  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 011 - R10
  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 012 - R11
  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 013 - R12
  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 014 - R13
  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 015 - R14
  Add(crtValue, vmReg64, [maIncrement..maChange]);       // 016 - R15

  Add(crtExtra, vmDefOnly, [maChange]);                  // 017 - EFlags

  Add(crtValue, vmDefOnly);     // 018 - SegGs
  Add(crtValue, vmDefOnly);     // 019 - SegFs
  Add(crtValue, vmDefOnly);     // 020 - SegEs
  Add(crtValue, vmDefOnly);     // 021 - SegDs
  Add(crtValue, vmDefOnly);     // 022 - SegCs
  Add(crtValue, vmDefOnly);     // 023 - SegSs

  Add(crtValue, vmDefOnly);     // 024 - DR0
  Add(crtValue, vmDefOnly);     // 025 - DR1
  Add(crtValue, vmDefOnly);     // 026 - DR2
  Add(crtValue, vmDefOnly);     // 027 - DR3
  Add(crtValue, vmDefOnly);     // 028 - DR6
  Add(crtValue, vmDefOnly);     // 029 - DR7

  Add(crtExtra, vmDefOnly, [maChange]);     // 030 - ControlWord
  Add(crtExtra, vmDefOnly, [maChange]);     // 031 - StatusWord
  Add(crtExtra, vmDefOnly, [maChange]);     // 032 - TagWord

  Add(crtValue, vmX87Reg64);    // 033 - MM0
  Add(crtValue, vmX87Reg64);    // 034 - MM1
  Add(crtValue, vmX87Reg64);    // 035 - MM2
  Add(crtValue, vmX87Reg64);    // 036 - MM3
  Add(crtValue, vmX87Reg64);    // 037 - MM4
  Add(crtValue, vmX87Reg64);    // 038 - MM5
  Add(crtValue, vmX87Reg64);    // 039 - MM6
  Add(crtValue, vmX87Reg64);    // 040 - MM7

  Add(crtValue, vmX87Reg80);    // 041 - R0
  Add(crtValue, vmX87Reg80);    // 042 - R1
  Add(crtValue, vmX87Reg80);    // 043 - R2
  Add(crtValue, vmX87Reg80);    // 044 - R3
  Add(crtValue, vmX87Reg80);    // 045 - R4
  Add(crtValue, vmX87Reg80);    // 046 - R5
  Add(crtValue, vmX87Reg80);    // 047 - R6
  Add(crtValue, vmX87Reg80);    // 048 - R7

  Add(crtValue, vmX87Reg80);    // 049 - ST0
  Add(crtValue, vmX87Reg80);    // 050 - ST1
  Add(crtValue, vmX87Reg80);    // 051 - ST2
  Add(crtValue, vmX87Reg80);    // 052 - ST3
  Add(crtValue, vmX87Reg80);    // 053 - ST4
  Add(crtValue, vmX87Reg80);    // 054 - ST5
  Add(crtValue, vmX87Reg80);    // 055 - ST6
  Add(crtValue, vmX87Reg80);    // 056 - ST7

  Add(crtExtra, vmDefOnly, [maChange]);     // 057 - MxCsr

  Add(crtValue, vmSimdReg);     // 058 - XMM0
  Add(crtValue, vmSimdReg);     // 059 - XMM1
  Add(crtValue, vmSimdReg);     // 060 - XMM2
  Add(crtValue, vmSimdReg);     // 061 - XMM3
  Add(crtValue, vmSimdReg);     // 062 - XMM4
  Add(crtValue, vmSimdReg);     // 063 - XMM5
  Add(crtValue, vmSimdReg);     // 064 - XMM6
  Add(crtValue, vmSimdReg);     // 065 - XMM7
  Add(crtValue, vmSimdReg);     // 066 - XMM8
  Add(crtValue, vmSimdReg);     // 067 - XMM9
  Add(crtValue, vmSimdReg);     // 068 - XMM10
  Add(crtValue, vmSimdReg);     // 069 - XMM11
  Add(crtValue, vmSimdReg);     // 070 - XMM12
  Add(crtValue, vmSimdReg);     // 071 - XMM13
  Add(crtValue, vmSimdReg);     // 072 - XMM14
  Add(crtValue, vmSimdReg);     // 073 - XMM15

  Add(crtValue, vmSimdReg);     // 074 - YMM0
  Add(crtValue, vmSimdReg);     // 075 - YMM1
  Add(crtValue, vmSimdReg);     // 076 - YMM2
  Add(crtValue, vmSimdReg);     // 077 - YMM3
  Add(crtValue, vmSimdReg);     // 078 - YMM4
  Add(crtValue, vmSimdReg);     // 079 - YMM5
  Add(crtValue, vmSimdReg);     // 080 - YMM6
  Add(crtValue, vmSimdReg);     // 081 - YMM7
  Add(crtValue, vmSimdReg);     // 082 - YMM8
  Add(crtValue, vmSimdReg);     // 083 - YMM9
  Add(crtValue, vmSimdReg);     // 084 - YMM10
  Add(crtValue, vmSimdReg);     // 085 - YMM11
  Add(crtValue, vmSimdReg);     // 086 - YMM12
  Add(crtValue, vmSimdReg);     // 087 - YMM13
  Add(crtValue, vmSimdReg);     // 088 - YMM14
  Add(crtValue, vmSimdReg);     // 089 - YMM15

  // EFlags Hint

  Add(crtSelectableHint, vmDefOnly);      // 090 - JMP Conditions

  // EFlags bits

  Add(crtBitValue, vmDefOnly);  // 091 - CF
  Add(crtBitValue, vmDefOnly);  // 092 - PF
  Add(crtBitValue, vmDefOnly);  // 093 - AF
  Add(crtBitValue, vmDefOnly);  // 094 - ZF
  Add(crtBitValue, vmDefOnly);  // 095 - SF
  Add(crtBitValue, vmDefOnly);  // 096 - TF
  Add(crtBitValue, vmDefOnly);  // 097 - IF
  Add(crtBitValue, vmDefOnly);  // 098 - DF
  Add(crtBitValue, vmDefOnly);  // 099 - OF

  // Advanced Fields

  Add(crtExtra, vmDefOnly, [maZero, maChange]);     // 100 - LastError
  Add(crtExtra, vmDefOnly, [maZero, maChange]);     // 101 - LastStatus

  // x87TagWord sets

  Add(crtSetValue, vmDefOnly);  // 102 - TW0
  Add(crtSetValue, vmDefOnly);  // 103 - TW1
  Add(crtSetValue, vmDefOnly);  // 104 - TW2
  Add(crtSetValue, vmDefOnly);  // 105 - TW3
  Add(crtSetValue, vmDefOnly);  // 106 - TW4
  Add(crtSetValue, vmDefOnly);  // 107 - TW5
  Add(crtSetValue, vmDefOnly);  // 108 - TW6
  Add(crtSetValue, vmDefOnly);  // 109 - TW7

  // x87StatusWord bits

  Add(crtBitValue, vmDefOnly);  // 110 - ES
  Add(crtBitValue, vmDefOnly);  // 111 - IE
  Add(crtBitValue, vmDefOnly);  // 112 - DE
  Add(crtBitValue, vmDefOnly);  // 113 - ZE
  Add(crtBitValue, vmDefOnly);  // 114 - OE
  Add(crtBitValue, vmDefOnly);  // 115 - UE
  Add(crtBitValue, vmDefOnly);  // 116 - B
  Add(crtBitValue, vmDefOnly);  // 117 - C0
  Add(crtBitValue, vmDefOnly);  // 118 - C1
  Add(crtBitValue, vmDefOnly);  // 119 - C2
  Add(crtBitValue, vmDefOnly);  // 120 - C3
  Add(crtBitValue, vmDefOnly);  // 121 - PE
  Add(crtBitValue, vmDefOnly);  // 122 - SF
  Add(crtSetValue, vmDefOnly);  // 123 - TOP

  // x87ControlWord bits

  Add(crtBitValue, vmDefOnly);  // 124 - IM
  Add(crtBitValue, vmDefOnly);  // 125 - DM
  Add(crtBitValue, vmDefOnly);  // 126 - ZM
  Add(crtBitValue, vmDefOnly);  // 127 - OM
  Add(crtBitValue, vmDefOnly);  // 128 - UM
  Add(crtBitValue, vmDefOnly);  // 129 - PM
  Add(crtSetValue, vmDefOnly);  // 130 - PC
  Add(crtBitValue, vmDefOnly);  // 131 - IC
  Add(crtSetValue, vmDefOnly);  // 132 - RC

  // MxCsr bits

  Add(crtBitValue, vmDefOnly);  // 133 - IE
  Add(crtBitValue, vmDefOnly);  // 134 - DE
  Add(crtBitValue, vmDefOnly);  // 135 - ZE
  Add(crtBitValue, vmDefOnly);  // 136 - OE
  Add(crtBitValue, vmDefOnly);  // 137 - UE
  Add(crtBitValue, vmDefOnly);  // 138 - PE
  Add(crtBitValue, vmDefOnly);  // 139 - IM
  Add(crtBitValue, vmDefOnly);  // 140 - DM
  Add(crtBitValue, vmDefOnly);  // 141 - ZM
  Add(crtBitValue, vmDefOnly);  // 142 - UM
  Add(crtBitValue, vmDefOnly);  // 143 - UM
  Add(crtBitValue, vmDefOnly);  // 144 - PM
  Add(crtBitValue, vmDefOnly);  // 145 - DAZ
  Add(crtBitValue, vmDefOnly);  // 146 - FTZ
  Add(crtSetValue, vmDefOnly);  // 147 - RC

  // x87 Hint regs

  Add(crtSelectableHint, [rvmFloat80]);   // 148 - ST0/R0
  Add(crtSelectableHint, [rvmFloat80]);   // 149 - ST1/R1
  Add(crtSelectableHint, [rvmFloat80]);   // 150 - ST2/R2
  Add(crtSelectableHint, [rvmFloat80]);   // 151 - ST3/R3
  Add(crtSelectableHint, [rvmFloat80]);   // 152 - ST4/R4
  Add(crtSelectableHint, [rvmFloat80]);   // 153 - ST5/R5
  Add(crtSelectableHint, [rvmFloat80]);   // 154 - ST6/R6
  Add(crtSelectableHint, [rvmFloat80]);   // 155 - ST7/R7

  // fix non default view mode
  for I := 148 to 155 do
    KnownRegs.List[I].ViewMode := rvmFloat80;

  // x87 Hint Caption

  Add(crtHint, vmDefOnly); // 156 - Control Word Regs Description

  // x87ControlWord precision + rounding for Simple Mode

  Add(crtExtra, vmDefOnly);     // 157 - PC + RC

  // Def Reg Debug Data Hint

  Add(crtSelectableHint, vmDefOnly);   // 158 - RAX hint
  Add(crtSelectableHint, vmDefOnly);   // 159 - RBX hint
  Add(crtSelectableHint, vmDefOnly);   // 160 - RCX hint
  Add(crtSelectableHint, vmDefOnly);   // 161 - RDX hint
  Add(crtSelectableHint, vmDefOnly);   // 162 - RBP hint
  Add(crtSelectableHint, vmDefOnly);   // 163 - RSP hint
  Add(crtSelectableHint, vmDefOnly);   // 164 - RSI hint
  Add(crtSelectableHint, vmDefOnly);   // 165 - RDI hint
  Add(crtSelectableHint, vmDefOnly);   // 166 - RIP hint
  Add(crtSelectableHint, vmDefOnly);   // 167 - R8 hint
  Add(crtSelectableHint, vmDefOnly);   // 168 - R9 hint
  Add(crtSelectableHint, vmDefOnly);   // 169 - R10 hint
  Add(crtSelectableHint, vmDefOnly);   // 170 - R11 hint
  Add(crtSelectableHint, vmDefOnly);   // 171 - R12 hint
  Add(crtSelectableHint, vmDefOnly);   // 172 - R13 hint
  Add(crtSelectableHint, vmDefOnly);   // 173 - R14 hint
  Add(crtSelectableHint, vmDefOnly);   // 174 - R15 hint
end;

function TIntelCpuContext.InstructonPoint: UInt64;
begin
  Result := FContext.Rip;
end;

function TIntelCpuContext.IsActiveJump(const Value: string): Boolean;
var
  EFlags: Cardinal;
  ZF, CF, SF, _OF: Boolean;
begin
  Result := False;

  if Value.StartsWith('CALL') or Value.StartsWith('JMP') then
    Exit(True);

  EFlags := FContext.EFlags;
  CF := EFlags and (1 shl 0) <> 0;
  ZF := EFlags and (1 shl 6) <> 0;
  SF := EFlags and (1 shl 7) <> 0;
  _OF := EFlags and (1 shl 11) <> 0;

  if ZF then
  begin
    if Value.StartsWith('JE') then Exit(True);
    if Value.StartsWith('JZ') then Exit(True);
  end
  else
  begin
    if Value.StartsWith('JNE') then Exit(True);
    if Value.StartsWith('JNZ') then Exit(True);
  end;

  if CF or ZF then
  begin
    if Value.StartsWith('JBE') then Exit(True);
    if Value.StartsWith('JNA') then Exit(True);
  end
  else
  begin
    if Value.StartsWith('JA') then Exit(True);
    if Value.StartsWith('JNBE') then Exit(True);
  end;

  if CF then
  begin
    if Value.StartsWith('JB') then Exit(True);
    if Value.StartsWith('JNAE') then Exit(True);
    if Value.StartsWith('JC') then Exit(True);
  end
  else
  begin
    if Value.StartsWith('JAE') then Exit(True);
    if Value.StartsWith('JNB') then Exit(True);
    if Value.StartsWith('JNC') then Exit(True);
  end;

  if not (SF or ZF) then
  begin
    if Value.StartsWith('JG') then Exit(True);
    if Value.StartsWith('JNLE') then Exit(True);
  end;

  if SF = _OF then
  begin
    if Value.StartsWith('JGE') then Exit(True);
    if Value.StartsWith('JNL') then Exit(True);
  end
  else
  begin
    if Value.StartsWith('JL') then Exit(True);
    if Value.StartsWith('JNGE') then Exit(True);
  end;

  if (SF <> _OF) or ZF then
  begin
    if Value.StartsWith('JLE') then Exit(True);
    if Value.StartsWith('JNG') then Exit(True);
  end;
end;

function TIntelCpuContext.PointerSize: Integer;
begin
  case AddressMode of
    am32bit: Result := 4;
    am64bit: Result := 8;
  else
    Result := 0;
  end;
end;

function TIntelCpuContext.QueryRegValueByName(const RegName: string;
  out RegValue: UInt64): Boolean;
const
  RegsBuff: array[0..69] of string = (
    'RAX', 'EAX', 'AX', 'AH', 'AL',
    'RBX', 'EBX', 'BX', 'BH', 'BL',
    'RCX', 'ECX', 'CX', 'CH', 'CL',
    'RDX', 'EDX', 'DX', 'DH', 'DL',
    'RBP', 'EBP', 'BP', 'BPL',
    'RSP', 'ESP', 'SP', 'SPL',
    'RSI', 'ESI', 'SI', 'SIL',
    'RDI', 'EDI', 'DI', 'DIL',
    'RIP', 'EIP',
    'R8', 'R8D', 'R8W', 'R8B',
    'R9', 'R9D', 'R9W', 'R9B',
    'R10', 'R10D', 'R10W', 'R10B',
    'R11', 'R11D', 'R11W', 'R11B',
    'R12', 'R12D', 'R12W', 'R12B',
    'R13', 'R13D', 'R13W', 'R13B',
    'R14', 'R14D', 'R14W', 'R14B',
    'R15', 'R15D', 'R15W', 'R15B'
  );
  RegIndex: array[0..69] of Integer = (
    0, 0, 0, 0, 0,
    1, 1, 1, 1, 1,
    2, 2, 2, 2, 2,
    3, 3, 3, 3, 3,
    4, 4, 4, 4,
    5, 5, 5, 5,
    6, 6, 6, 6,
    7, 7, 7, 7,
    8, 8,
    9, 9, 9, 9,
    10, 10, 10, 10,
    11, 11, 11, 11,
    12, 12, 12, 12,
    13, 13, 13, 13,
    14, 14, 14, 14,
    15, 15, 15, 15,
    16, 16, 16, 16
  );
  ValueType: array[0..69] of Integer = (
    0, 1, 2, 3, 4,
    0, 1, 2, 3, 4,
    0, 1, 2, 3, 4,
    0, 1, 2, 3, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4,
    0, 1, 2, 4
  );
var
  I: Integer;
begin
  for I := 0 to Length(RegsBuff) - 1 do
    if AnsiSameText(RegName, RegsBuff[I]) then
    begin
      Result := GetRegValue(RegIndex[I], RegValue);
      case ValueType[I] of
        1: RegValue := Cardinal(RegValue);
        2: RegValue := Word(RegValue);
        3: RegValue := Byte(RegValue shr 8);
        4: RegValue := Byte(RegValue);
      end;
      Exit;
    end;
  RegValue := 0;
  Result := False;    
end;

function TIntelCpuContext.RegQueryValue(RowIndex, RegIndex: Integer; out ARegValue: UInt64): Boolean;
begin
  Result := GetRegValue(RegInfo(RowIndex, RegIndex).RegID, ARegValue);
end;

function TIntelCpuContext.RegSetValueAtIndex(RegID, Index: Integer): string;
begin
  Result := '';
  case RegID of
    102..109:  // x87 TagWord Tag WorkSet
    begin
      case Index of
        0: Result := '0 Nonzero';
        1: Result := '1 Zero';
        2: Result := '2 Spetial';
        3: Result := '3 Empty';
      end;
    end;
    123:  // x87 StatusWord Top
    begin
      Result := 'x87_R' + IntToStr(Index);
    end;
    130:  // x87 ControlWord Precision
    begin
      case Index of
        0: Result := 'Real4 (0)';
        1: Result := 'Not used (1)';
        2: Result := 'Real8 (2)';
        3: Result := 'Real10 (3)';
      end;
    end;
    132:  // x87 ControlWord Rounding
    begin
      case Index of
        0: Result := 'Round Near (0)';
        1: Result := 'Round Down (1)';
        2: Result := 'Round Up (2)';
        3: Result := 'Truncate (3)';
      end;
    end;
    147:  // MxCsr Rounding
    begin
      case Index of
        0: Result := 'Round Near (0)';
        1: Result := 'Toward Negative (1)';
        2: Result := 'Toward Positive (2)';
        3: Result := 'Toward Zero (3)';
      end;
    end;
  end;
end;

function TIntelCpuContext.RegSetValueCount(RegID: Integer): Integer;
begin
  case RegID of
    102..109,        // x87 TagWord Tag WorkSet
    130,             // x87 ControlWord Precision
    132,             // x87 ControlWord Rounding
    147:             // MxCsrRounding
      Result := 4;
    123:             // x87 StatusWord Top
      Result := 8;
  else
    Result := inherited;
  end;
end;

function TIntelCpuContext.RToSt(Index: Integer): Integer;
begin
  Result := Index - (Context.StatusWord shr 11) and 7;
  if Result < 0 then
    Inc(Result, 8);
end;

procedure TIntelCpuContext.SetContext(const Value: TIntelThreadContext);
var
  CtxBittessChanged: Boolean;
begin
  UpdateModifyed(Value);
  CtxBittessChanged := (FContext.Rip = 0) or
    (Value.x86Context <> FContext.x86Context);
  FContext := Value;
  if CtxBittessChanged then
    UpdateMap(True)
  else
    DoUpdate(cctDataChange);
end;

procedure TIntelCpuContext.SetControlWordPrecisionValue(AValue: Byte);
var
  Mask: Integer;
  NewControlWord: Word;
begin
  NewControlWord := (AValue and 3) shl 8;
  Mask := not (3 shl 8);
  FContext.ControlWord := (FContext.ControlWord and Mask) or NewControlWord;
end;

procedure TIntelCpuContext.SetControlWordRoundingValue(AValue: Byte);
var
  Mask: Integer;
  NewControlWord: Word;
begin
  NewControlWord := (AValue and 3) shl 10;
  Mask := not (3 shl 10);
  FContext.ControlWord := (FContext.ControlWord and Mask) or NewControlWord;
end;

procedure TIntelCpuContext.SetFPUMode(const Value: TFPUMode);
begin
  if FPUMode <> Value then
  begin
    FFPUMode := Value;
    UpdateMap;
  end;
end;

procedure TIntelCpuContext.SetMapMode(const Value: TIntelCpuMapMode);
begin
  if MapMode <> Value then
  begin
    FMapMode := Value;
    if Value = icmSimple then
    begin
      // Activate first checked by priority
      SetSavedDetailed(0, ShowFPU);
      SetSavedDetailed(1, ShowXMM);
      SetSavedDetailed(2, ShowYMM);
      SetSavedDetailed(3, ShowDebug);
    end
    else
    begin
      // Restore settings
      FShowFPU := FSavedDetailed[0];
      FShowXMM := FSavedDetailed[1];
      FShowYMM := FSavedDetailed[2];
      FShowDebug := FSavedDetailed[3];
    end;
    UpdateMap;
  end;
end;

procedure TIntelCpuContext.SetMxCsrRoundingValue(AValue: Byte);
var
  Mask: Integer;
  NewMxCsr: DWORD;
begin
  NewMxCsr := (AValue and 3) shl 13;
  Mask := not (3 shl 13);
  FContext.MxCsr := (FContext.MxCsr and Mask) or NewMxCsr;
end;

procedure TIntelCpuContext.SetSavedDetailed(Index: Integer; Value: Boolean);
begin
  if MapMode = icmDetailed then
    FSavedDetailed[Index] := Value
  else
  begin
    if not Value then Exit;
    FShowFPU := Index = 0;
    FShowXMM := Index = 1;
    FShowYMM := Index = 2;
    FShowDebug := Index = 3;
  end;
end;

procedure TIntelCpuContext.SetShowDebug(const Value: Boolean);
begin
  if ShowDebug <> Value then
  begin
    FShowDebug := Value;
    SetSavedDetailed(3, Value);
    UpdateMap;
  end;
end;

procedure TIntelCpuContext.SetShowFPU(const Value: Boolean);
begin
  if ShowFPU <> Value then
  begin
    FShowFPU := Value;
    SetSavedDetailed(0, Value);
    UpdateMap;
  end;
end;

procedure TIntelCpuContext.SetShowXMM(const Value: Boolean);
begin
  if ShowXMM <> Value then
  begin
    FShowXMM := Value;
    SetSavedDetailed(1, Value);
    UpdateMap;
  end;
end;

procedure TIntelCpuContext.SetShowYMM(const Value: Boolean);
begin
  if ShowYMM <> Value then
  begin
    FShowYMM := Value;
    SetSavedDetailed(2, Value);
    UpdateMap;
  end;
end;

procedure TIntelCpuContext.SetStatusWordTopValue(AValue: Byte);
var
  Mask: Integer;
  NewStatusWord: Word;
begin
  NewStatusWord := (AValue and 7) shl 11;
  Mask := not (7 shl 11);
  FContext.StatusWord := (FContext.StatusWord and Mask) or NewStatusWord;
end;

procedure TIntelCpuContext.SetTagWordSetValue(Index: Integer; AValue: Byte);
var
  Mask: Integer;
  NewTagWord: Word;
begin
  Index := Index shl 1;
  NewTagWord := (AValue and 3) shl Index;
  Mask := not (3 shl Index);
  FContext.TagWord := (FContext.TagWord and Mask) or NewTagWord;
end;

function TIntelCpuContext.StackBase: UInt64;
begin
  Result := FContext.Rbp;
end;

function TIntelCpuContext.StackPoint: UInt64;
begin
  Result := FContext.Rsp;
end;

function TIntelCpuContext.StToR(Index: Integer): Integer;
begin
  Result := Index + (Context.StatusWord shr 11) and 7;
  if Result > 7 then
    Dec(Result, 8);
end;

function TIntelCpuContext.Update(CurrentIP: UInt64): Boolean;
var
  ACtx: TIntelThreadContext;
begin
  if ThreadID = 0 then
    Exit(False)
  else
    Result := True;

  {$IFDEF CPUX64}
  if AddressMode = am32bit then
  begin
    if CurrentIP = 0 then
      Context := GetIntelWow64Context(ThreadID)
    else
    begin
      ACtx := GetIntelWow64Context(ThreadID);
      ACtx.Rip := CurrentIP;
      Context := ACtx;
    end;
    Exit;
  end;
  {$ENDIF}

  if CurrentIP = 0 then
    Context := GetIntelContext(ThreadID)
  else
  begin
    ACtx := GetIntelContext(ThreadID);
    ACtx.Rip := CurrentIP;
    Context := ACtx;
  end;
end;

procedure TIntelCpuContext.UpdateLastRegData(RegID: Integer);

  function ExtFmt(Index: Integer): string;
  var
    Ex: PExtended80Support;
    Len, MaxLen: Integer;
  begin
    Ex := PExtended80Support(@FContext.FloatRegisters[Index * 10]);
    Result := RawBufToViewMode(PByte(Ex), SizeOf(TExtended80Support),
      DefValueMetric(bvmFloat80), bvmFloat80, RegFormatModeNoAlign);
    MaxLen := DefValueMetric(bvmFloat80).CharCount;
    Len := Length(Result);
    if Len <> MaxLen then
    begin
      if GetFloatType(Ex^) in [ftNormalized..ftZero] then
      begin
        if Pos('e', Result) <> 0 then Exit;
        if Pos('.', Result) = 0 then
          Result := Result + '.' + StringOfChar('0', MaxLen - Len - 2)
        else
          Result := Result + StringOfChar('0', MaxLen - Len - 1);
      end;
    end;
  end;

  function _MM(const Value: string): string; overload;
  begin
    if MapMode = icmDetailed then
      Result := Value
    else
      Result := Trim(Value)
  end;

  function _MM(T, F: Integer): Integer; overload;
  begin
    if MapMode = icmDetailed then
      Result := T
    else
      Result := F;
  end;

  function _MM(T, F: string): string; overload;
  begin
    if MapMode = icmDetailed then
      Result := T
    else
      Result := F;
  end;

var
  Pfx: Char;
  RegSize, XmmPfxSize: Integer;
  Index: Integer;
  AHint: string;
  ARegValue: UInt64;
begin
  if FContext.x86Context then
  begin
    Pfx := 'E';
    RegSize := 4;
    XmmPfxSize := 5;
  end
  else
  begin
    Pfx := 'R';
    RegSize := 8;
    XmmPfxSize := 6;
  end;

  case RegID of
    0: FillReg(Pfx + 'AX', Trim(RegValueFmt(@FContext.Rax, RegSize)), 4, 2);
    1: FillReg(Pfx + 'BX', Trim(RegValueFmt(@FContext.Rbx, RegSize)), 4, 2);
    2: FillReg(Pfx + 'CX', Trim(RegValueFmt(@FContext.Rcx, RegSize)), 4, 2);
    3: FillReg(Pfx + 'DX', Trim(RegValueFmt(@FContext.Rdx, RegSize)), 4, 2);
    4: FillReg(Pfx + 'BP', Trim(RegValueFmt(@FContext.Rbp, RegSize)), 4, 2);
    5: FillReg(Pfx + 'SP', Trim(RegValueFmt(@FContext.Rsp, RegSize)), 4, 2);
    6: FillReg(Pfx + 'SI', Trim(RegValueFmt(@FContext.Rsi, RegSize)), 4, 2);
    7: FillReg(Pfx + 'DI', Trim(RegValueFmt(@FContext.Rdi, RegSize)), 4, 2);
    8: FillReg(Pfx + 'IP', RegValueFmt(@FContext.Rip, RegSize), 4, 2);
    9..16:
    begin
      Index := RegID - 9 + 8;
      FillReg(Pfx + IntToStr(Index), Trim(RegValueFmt(@FContext.R[Index], 8)), 4, 2);
    end;
    17: FillReg('EFLAGS', RegValueFmt(@FContext.EFlags, 4), 7, 2);
    18: FillReg('GS', RegValueFmt(@FContext.SegGs, 2), 3, 2);
    19: FillReg('FS', RegValueFmt(@FContext.SegFs, 2), 3);
    20: FillReg('ES', RegValueFmt(@FContext.SegEs, 2), 3, 2);
    21: FillReg('DS', RegValueFmt(@FContext.SegDs, 2), 3);
    22: FillReg('CS', RegValueFmt(@FContext.SegCs, 2), 3, 2);
    23: FillReg('SS', RegValueFmt(@FContext.SegSs, 2), 3);
    24: FillReg('DR0', RegValueFmt(@FContext.Dr0, RegSize), 4);
    25: FillReg('DR1', RegValueFmt(@FContext.Dr1, RegSize), 4);
    26: FillReg('DR2', RegValueFmt(@FContext.Dr2, RegSize), 4);
    27: FillReg('DR3', RegValueFmt(@FContext.Dr3, RegSize), 4);
    28: FillReg('DR6', RegValueFmt(@FContext.Dr6, RegSize), 4);
    29: FillReg('DR7', RegValueFmt(@FContext.Dr7, RegSize), 4);
    30: FillReg(_MM('x87ControlWord', 'FCW'), RegValueFmt(@FContext.ControlWord, 2), _MM(15, 4), 2);
    31: FillReg(_MM('x87StatusWord', 'FST'), RegValueFmt(@FContext.StatusWord, 2), _MM(14, 4), 2);
    32: FillReg('x87TagWord', RegValueFmt(@FContext.TagWord, 2), 11);
    33..40:
    begin
      Index := RegID - 33;
      FillReg('MM' + IntToStr(Index),
        RegValueFmt(@FContext.FloatRegisters[Index * 10], 8), 4);
    end;
    41..48:
    begin
      Index := RToSt(RegID - 41);
      FillReg('R' + IntToStr(RegID - 41),
        RegValueFmt(@FContext.FloatRegisters[Index * 10], 10), 3, 2);
    end;
    49..56:
    begin
      Index := RegID - 49;
      FillReg('ST' + IntToStr(Index),
        RegValueFmt(@FContext.FloatRegisters[Index * 10], 10), 4, 2);
    end;
    57: FillReg('MxCsr', RegValueFmt(@FContext.MxCsr, 4), 6);
    58..73:
    begin
      Index := RegID - 58;
      FillReg('XMM' + IntToStr(Index),
        RegValueFmt(@FContext.Ymm[Index].Low, 16), XmmPfxSize);
    end;
    74..89:
    begin
      Index := RegID - 74;
      FillReg('YMM' + IntToStr(Index),
        RegValueFmt(@FContext.Ymm[Index], 32), XmmPfxSize);
    end;
    90: FillReg('JMP:', ExtractJmpConditions, 5);
    91: FillReg(_MM(' CF'), ExtractBit(FContext.EFlags, 0), _MM(4, 3), 2);
    92: FillReg('PF', ExtractBit(FContext.EFlags, 2), 3, 2);
    93: FillReg('AF', ExtractBit(FContext.EFlags, 4), 3, 2);
    94: FillReg(_MM(' ZF'), ExtractBit(FContext.EFlags, 6), _MM(4, 3), 2);
    95: FillReg('SF', ExtractBit(FContext.EFlags, 7), 3, 2);
    96: FillReg('TF', ExtractBit(FContext.EFlags, 8), 3, 2);
    97: FillReg(_MM(' IF'), ExtractBit(FContext.EFlags, 9), _MM(4, 3), 2);
    98: FillReg('DF', ExtractBit(FContext.EFlags, 10), 3, 2);
    99: FillReg('OF', ExtractBit(FContext.EFlags, 11), 3, 2);
    100: FillReg('LastError', RegValueFmt(@FContext.LastError, 4), 11);
    101: FillReg('LastStatus', RegValueFmt(@FContext.LastStatus, 4), 11);
    102: FillReg(' TW_0', ExtractTagWordSet(0), 6, 9, 2);
    103: FillReg('TW_1', ExtractTagWordSet(1), 5, 9);
    104: FillReg(' TW_2', ExtractTagWordSet(2), 6, 9, 2);
    105: FillReg('TW_3', ExtractTagWordSet(3), 5, 9);
    106: FillReg(' TW_4', ExtractTagWordSet(4), 6, 9, 2);
    107: FillReg('TW_5', ExtractTagWordSet(5), 5, 9);
    108: FillReg(' TW_6', ExtractTagWordSet(6), 6, 9, 2);
    109: FillReg('TW_7', ExtractTagWordSet(7), 5, 9);
    110: FillReg(_MM('ES', 'Err'), ExtractBit(FContext.StatusWord, 7), _MM(3, 4), 1);
    111: FillReg(_MM(' IE', ''), ExtractBit(FContext.StatusWord, 0), _MM(4, 0), _MM(2, 1));
    112: FillReg(_MM('DE', ''), ExtractBit(FContext.StatusWord, 1), _MM(3, 0), _MM(2, 1));
    113: FillReg(_MM('ZE', ''), ExtractBit(FContext.StatusWord, 2), _MM(3, 0), _MM(2, 1));
    114: FillReg(_MM('OE', ''), ExtractBit(FContext.StatusWord, 3), _MM(3, 0), _MM(2, 1));
    115: FillReg(_MM('UE', ''), ExtractBit(FContext.StatusWord, 4), _MM(3, 0), _MM(2, 1));
    116: FillReg(' B', ExtractBit(FContext.StatusWord, 15), 4, 2);
    117: FillReg(_MM('C0', ''), ExtractBit(FContext.StatusWord, 8), _MM(3, 0), 2);
    118: FillReg(_MM('C1', ''), ExtractBit(FContext.StatusWord, 9), _MM(3, 0), _MM(2, 1));
    119: FillReg(_MM('C2', ''), ExtractBit(FContext.StatusWord, 10), _MM(3, 0), _MM(2, 1));
    120: FillReg(_MM('C3', 'Cond'), ExtractBit(FContext.StatusWord, 14), _MM(3, 5), 1);
    121: FillReg(_MM(' PE', ''), ExtractBit(FContext.StatusWord, 5), _MM(4, 0), _MM(2, 1));
    122: FillReg(_MM('SF', ''), ExtractBit(FContext.StatusWord, 6), _MM(3, 0), _MM(2, 1));
    123: FillReg('TOP', ExtractStatusWordTop, 4);
    124: FillReg(_MM(' IM', ''), ExtractBit(FContext.ControlWord, 0), _MM(4, 0), _MM(2, 1));
    125: FillReg(_MM('DM', ''), ExtractBit(FContext.ControlWord, 1), _MM(3, 0), _MM(2, 1));
    126: FillReg(_MM('ZM', ''), ExtractBit(FContext.ControlWord, 2), _MM(3, 0), _MM(2, 1));
    127: FillReg(_MM('OM', ''), ExtractBit(FContext.ControlWord, 3), _MM(3, 0), _MM(2, 1));
    128: FillReg(_MM('UM', ''), ExtractBit(FContext.ControlWord, 4), _MM(3, 0), _MM(2, 1));
    129: FillReg(_MM(' PM', 'Mask'), ExtractBit(FContext.ControlWord, 5), _MM(4, 8), _MM(2, 1));
    130: FillReg('PC', ExtractControlWordPrecision, 3);
    131: FillReg(' IC', ExtractBit(FContext.ControlWord, 12), 4, 2);
    132: FillReg('RC', ExtractControlWordRounding, 3);
    133: FillReg(' IE', ExtractBit(FContext.MxCsr, 0), 4, 2);
    134: FillReg('DE', ExtractBit(FContext.MxCsr, 1), 3, 2);
    135: FillReg('ZE', ExtractBit(FContext.MxCsr, 2), 3, 2);
    136: FillReg('OE', ExtractBit(FContext.MxCsr, 3), 3, 2);
    137: FillReg('UE', ExtractBit(FContext.MxCsr, 4), 3, 2);
    138: FillReg('PE', ExtractBit(FContext.MxCsr, 5), 3);
    139: FillReg(' IM', ExtractBit(FContext.MxCsr, 7), 4, 2);
    140: FillReg('DM', ExtractBit(FContext.MxCsr, 8), 3, 2);
    141: FillReg('ZM', ExtractBit(FContext.MxCsr, 9), 3, 2);
    142: FillReg('OM', ExtractBit(FContext.MxCsr, 10), 3, 2);
    143: FillReg('UM', ExtractBit(FContext.MxCsr, 11), 3, 2);
    144: FillReg('PM', ExtractBit(FContext.MxCsr, 12), 3);
    145: FillReg(' DAZ', ExtractBit(FContext.MxCsr, 6), 5, 2);
    146: FillReg('FTZ', ExtractBit(FContext.MxCsr, 15), 4, 2);
    147: FillReg('RC', ExtractMxCsrRounding, 3);
    148..155:
    begin
      if FPUMode = fpuST then
      begin
        Index := RegID - 148;
        FillReg('R' + IntToStr(StToR(Index)), ExtFmt(Index), 3);
      end
      else
      begin
        Index := RToSt(RegID - 148);
        FillReg('ST' + IntToStr(Index), ExtFmt(Index), 4);
      end;
    end;
    156: FillReg('               3 2 1 0      E S P U O Z D I', '', 43);
    157: FillReg('Prec', ExtractControlWordPcRc, 5, 2);
    158..173:
    begin
      AHint := '';
      if GetRegValue(RegID - 158, ARegValue) then
        DoQueryRegHint(ARegValue, AHint);
      FillReg('', AHint, 0, Length(AHint));
    end;
  else
    FillSeparator;
  end;
end;

procedure TIntelCpuContext.UpdateModifyed(const Value: TIntelThreadContext);

  function CheckReg(A, B: UInt64; RegID: Integer): Boolean;
  begin
    Result := A <> B;
    KnownRegs.List[RegID].Modifyed := Result;
  end;

  procedure CheckBit(A, B: DWORD; Index: Integer; RegID: Integer);
  begin
    KnownRegs.List[RegID].Modifyed :=
      A and (1 shl Index) <> B and (1 shl Index);
  end;

  procedure CheckDoubleSet(A, B: DWORD; Index: Integer; RegID: Integer);
  begin
    KnownRegs.List[RegID].Modifyed :=
      (A shr Index) and 3 <> (A shr Index) and 3;
  end;

  procedure CheckTripleSet(A, B: DWORD; Index: Integer; RegID: Integer);
  begin
    KnownRegs.List[RegID].Modifyed :=
      (A shr Index) and 7 <> (A shr Index) and 7;
  end;

  procedure CheckMem(A, B: PByte; Len: Integer; RegID: Integer);
  begin
    KnownRegs.List[RegID].Modifyed := not CompareMem(A, B, Len);
  end;

var
  I: Integer;
begin
  for I := 0 to KnownRegs.Count - 1 do
    KnownRegs.List[I].Modifyed := False;

  CheckReg(FContext.Rax, Value.Rax, 0);
  CheckReg(FContext.Rbx, Value.Rbx, 1);
  CheckReg(FContext.Rcx, Value.Rcx, 2);
  CheckReg(FContext.Rdx, Value.Rdx, 3);
  CheckReg(FContext.Rbp, Value.Rbp, 4);
  CheckReg(FContext.Rsp, Value.Rsp, 5);
  CheckReg(FContext.Rsi, Value.Rsi, 6);
  CheckReg(FContext.Rdi, Value.Rdi, 7);

  CheckReg(FContext.Rip, Value.Rip, 8);

  // R8..R15
  for I := 8 to 15 do
    CheckReg(FContext.R[I], Value.R[I], I + 1);

  if CheckReg(FContext.EFlags, Value.EFlags, 17) then
  begin
    CheckBit(FContext.EFlags, Value.EFlags, 0, 91);
    CheckBit(FContext.EFlags, Value.EFlags, 2, 92);
    CheckBit(FContext.EFlags, Value.EFlags, 4, 93);
    CheckBit(FContext.EFlags, Value.EFlags, 6, 94);
    CheckBit(FContext.EFlags, Value.EFlags, 7, 95);
    CheckBit(FContext.EFlags, Value.EFlags, 8, 96);
    CheckBit(FContext.EFlags, Value.EFlags, 9, 97);
    CheckBit(FContext.EFlags, Value.EFlags, 10, 98);
    CheckBit(FContext.EFlags, Value.EFlags, 11, 99);
  end;

  {$IFDEF MSWINDOWS}
  CheckReg(FContext.LastError, Value.LastError, 100);
  CheckReg(FContext.LastStatus, Value.LastStatus, 101);
  {$ENDIF}

  CheckReg(FContext.SegGs, Value.SegGs, 18);
  CheckReg(FContext.SegFs, Value.SegFs, 19);
  CheckReg(FContext.SegEs, Value.SegEs, 20);
  CheckReg(FContext.SegDs, Value.SegDs, 21);
  CheckReg(FContext.SegCs, Value.SegCs, 22);
  CheckReg(FContext.SegSs, Value.SegSs, 23);

  for I := 0 to 7 do
  begin
    // MMx
    CheckMem(@FContext.FloatRegisters[I * 10],
      @Value.FloatRegisters[I * 10], 8, 33 + I);
    // Rx
    CheckMem(@FContext.FloatRegisters[I * 10],
      @Value.FloatRegisters[I * 10], 10, 41 + I);
    // STx
    CheckMem(@FContext.FloatRegisters[I * 10],
      @Value.FloatRegisters[I * 10], 10, 49 + I);
  end;

  if CheckReg(FContext.TagWord, Value.TagWord, 32) then
  begin
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 0, 102);
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 1, 103);
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 2, 104);
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 3, 105);
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 4, 106);
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 5, 107);
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 6, 108);
    CheckDoubleSet(FContext.TagWord, Value.TagWord, 7, 109);
  end;

  if CheckReg(FContext.StatusWord, Value.StatusWord, 31) then
  begin
    CheckBit(FContext.StatusWord, Value.StatusWord, 7, 110);
    CheckBit(FContext.StatusWord, Value.StatusWord, 0, 111);
    CheckBit(FContext.StatusWord, Value.StatusWord, 1, 112);
    CheckBit(FContext.StatusWord, Value.StatusWord, 2, 113);
    CheckBit(FContext.StatusWord, Value.StatusWord, 3, 114);
    CheckBit(FContext.StatusWord, Value.StatusWord, 4, 115);
    CheckBit(FContext.StatusWord, Value.StatusWord, 15, 116);
    CheckBit(FContext.StatusWord, Value.StatusWord, 8, 117);
    CheckBit(FContext.StatusWord, Value.StatusWord, 9, 118);
    CheckBit(FContext.StatusWord, Value.StatusWord, 10, 119);
    CheckBit(FContext.StatusWord, Value.StatusWord, 14, 120);
    CheckBit(FContext.StatusWord, Value.StatusWord, 5, 121);
    CheckBit(FContext.StatusWord, Value.StatusWord, 6, 122);
    CheckTripleSet(FContext.StatusWord, Value.StatusWord, 11, 123);
  end;

  if CheckReg(FContext.ControlWord, Value.ControlWord, 30) then
  begin
    CheckBit(FContext.ControlWord, Value.ControlWord, 0, 124);
    CheckBit(FContext.ControlWord, Value.ControlWord, 1, 125);
    CheckBit(FContext.ControlWord, Value.ControlWord, 2, 126);
    CheckBit(FContext.ControlWord, Value.ControlWord, 3, 127);
    CheckBit(FContext.ControlWord, Value.ControlWord, 4, 128);
    CheckBit(FContext.ControlWord, Value.ControlWord, 5, 129);
    CheckDoubleSet(FContext.ControlWord, Value.ControlWord, 8, 130);
    CheckBit(FContext.ControlWord, Value.ControlWord, 12, 131);
    CheckDoubleSet(FContext.ControlWord, Value.ControlWord, 10, 132);
  end;

  if CheckReg(FContext.MxCsr, Value.MxCsr, 57) then
  begin
    CheckBit(FContext.MxCsr, Value.MxCsr, 0, 133);
    CheckBit(FContext.MxCsr, Value.MxCsr, 1, 134);
    CheckBit(FContext.MxCsr, Value.MxCsr, 2, 135);
    CheckBit(FContext.MxCsr, Value.MxCsr, 3, 136);
    CheckBit(FContext.MxCsr, Value.MxCsr, 4, 137);
    CheckBit(FContext.MxCsr, Value.MxCsr, 5, 138);
    CheckBit(FContext.MxCsr, Value.MxCsr, 7, 139);
    CheckBit(FContext.MxCsr, Value.MxCsr, 8, 140);
    CheckBit(FContext.MxCsr, Value.MxCsr, 9, 141);
    CheckBit(FContext.MxCsr, Value.MxCsr, 10, 142);
    CheckBit(FContext.MxCsr, Value.MxCsr, 11, 143);
    CheckBit(FContext.MxCsr, Value.MxCsr, 12, 144);
    CheckBit(FContext.MxCsr, Value.MxCsr, 6, 145);
    CheckBit(FContext.MxCsr, Value.MxCsr, 15, 146);
    CheckDoubleSet(FContext.MxCsr, Value.MxCsr, 13, 147);
  end;

  for I := 0 to 15 do
  begin
    CheckMem(@FContext.Ymm[I].Low, @Value.Ymm[I].Low, 16, 58 + I);
    CheckMem(@FContext.Ymm[I], @Value.Ymm[I], 32, 74 + I);
  end;

  CheckReg(FContext.Dr0, Value.Dr0, 24);
  CheckReg(FContext.Dr1, Value.Dr1, 25);
  CheckReg(FContext.Dr2, Value.Dr2, 26);
  CheckReg(FContext.Dr3, Value.Dr3, 27);
  CheckReg(FContext.Dr6, Value.Dr6, 28);
  CheckReg(FContext.Dr7, Value.Dr7, 29);
end;

function TIntelCpuContext.UpdateRegValue(RegID: Integer;
  ANewRegValue: UInt64): Boolean;
begin
  {$IFDEF CPUX64}
  if AddressMode = am32bit then
    FContext := GetIntelWow64Context(ThreadID)
  else
  {$ENDIF}
    FContext := GetIntelContext(ThreadID);

  case RegID of
    0: FContext.Rax := ANewRegValue;
    1: FContext.Rbx := ANewRegValue;
    2: FContext.Rcx := ANewRegValue;
    3: FContext.Rdx := ANewRegValue;
    4: FContext.Rbp := ANewRegValue;
    5: FContext.Rsp := ANewRegValue;
    6: FContext.Rsi := ANewRegValue;
    7: FContext.Rdi := ANewRegValue;
    8: FContext.Rip := ANewRegValue;
    9..16: FContext.R[RegID - 1] := ANewRegValue;
    17: FContext.EFlags := ANewRegValue;
    18: FContext.SegGs := ANewRegValue;
    19: FContext.SegFs := ANewRegValue;
    20: FContext.SegEs := ANewRegValue;
    21: FContext.SegDs := ANewRegValue;
    22: FContext.SegCs := ANewRegValue;
    23: FContext.SegSs := ANewRegValue;
    24: FContext.Dr0 := ANewRegValue;
    25: FContext.Dr1 := ANewRegValue;
    26: FContext.Dr2 := ANewRegValue;
    27: FContext.Dr3 := ANewRegValue;
    28: FContext.Dr6 := ANewRegValue;
    29: FContext.Dr7 := ANewRegValue;
    30: FContext.ControlWord := ANewRegValue;
    31: FContext.StatusWord := ANewRegValue;
    32: FContext.TagWord := ANewRegValue;
    57: FContext.MxCsr := ANewRegValue;
    91: SetBitValue(FContext.EFlags, 0, ANewRegValue);        // CF
    92: SetBitValue(FContext.EFlags, 2, ANewRegValue);        // PF
    93: SetBitValue(FContext.EFlags, 4, ANewRegValue);        // AF
    94: SetBitValue(FContext.EFlags, 6, ANewRegValue);        // ZF
    95: SetBitValue(FContext.EFlags, 7, ANewRegValue);        // SF
    96: SetBitValue(FContext.EFlags, 8, ANewRegValue);        // TF
    97: SetBitValue(FContext.EFlags, 9, ANewRegValue);        // IF
    98: SetBitValue(FContext.EFlags, 10, ANewRegValue);       // DF
    99: SetBitValue(FContext.EFlags, 11, ANewRegValue);       // OF
    {$message 'Эти два флага пока не работают'}
    100: FContext.LastError := ANewRegValue;
    101: FContext.LastStatus := ANewRegValue;
    102..109: SetTagWordSetValue(RegID - 102, ANewRegValue);
    110: SetBitValue(FContext.StatusWord, 7, ANewRegValue);   // ES
    111: SetBitValue(FContext.StatusWord, 0, ANewRegValue);   // IE
    112: SetBitValue(FContext.StatusWord, 1, ANewRegValue);   // DE
    113: SetBitValue(FContext.StatusWord, 2, ANewRegValue);   // ZE
    114: SetBitValue(FContext.StatusWord, 3, ANewRegValue);   // OE
    115: SetBitValue(FContext.StatusWord, 4, ANewRegValue);   // UE
    116: SetBitValue(FContext.StatusWord, 15, ANewRegValue);  // B
    117: SetBitValue(FContext.StatusWord, 8, ANewRegValue);   // C0
    118: SetBitValue(FContext.StatusWord, 9, ANewRegValue);   // C1
    119: SetBitValue(FContext.StatusWord, 10, ANewRegValue);  // C2
    120: SetBitValue(FContext.StatusWord, 14, ANewRegValue);  // C3
    121: SetBitValue(FContext.StatusWord, 5, ANewRegValue);   // PE
    122: SetBitValue(FContext.StatusWord, 6, ANewRegValue);   // SF
    123: SetStatusWordTopValue(ANewRegValue);
    124: SetBitValue(FContext.ControlWord, 0, ANewRegValue);  // IM
    125: SetBitValue(FContext.ControlWord, 1, ANewRegValue);  // DM
    126: SetBitValue(FContext.ControlWord, 2, ANewRegValue);  // ZM
    127: SetBitValue(FContext.ControlWord, 3, ANewRegValue);  // OM
    128: SetBitValue(FContext.ControlWord, 4, ANewRegValue);  // UM
    129: SetBitValue(FContext.ControlWord, 5, ANewRegValue);  // PM
    130: SetControlWordPrecisionValue(ANewRegValue);          // WordPrecision set
    131: SetBitValue(FContext.ControlWord, 12, ANewRegValue); // IC
    132: SetControlWordRoundingValue(ANewRegValue);           // WordRounding set
    133: SetBitValue(FContext.MxCsr, 0, ANewRegValue);        // IE
    134: SetBitValue(FContext.MxCsr, 1, ANewRegValue);        // DE
    135: SetBitValue(FContext.MxCsr, 2, ANewRegValue);        // ZE
    136: SetBitValue(FContext.MxCsr, 3, ANewRegValue);        // OE
    137: SetBitValue(FContext.MxCsr, 4, ANewRegValue);        // UE
    138: SetBitValue(FContext.MxCsr, 5, ANewRegValue);        // PE
    139: SetBitValue(FContext.MxCsr, 7, ANewRegValue);        // IM
    140: SetBitValue(FContext.MxCsr, 8, ANewRegValue);        // DM
    141: SetBitValue(FContext.MxCsr, 9, ANewRegValue);        // ZM
    142: SetBitValue(FContext.MxCsr, 10, ANewRegValue);       // OM
    143: SetBitValue(FContext.MxCsr, 11, ANewRegValue);       // UM
    144: SetBitValue(FContext.MxCsr, 12, ANewRegValue);       // PM
    145: SetBitValue(FContext.MxCsr, 6, ANewRegValue);        // DAZ
    146: SetBitValue(FContext.MxCsr, 15, ANewRegValue);       // FTZ
    147: SetMxCsrRoundingValue(ANewRegValue);                 // RC
  else
    Exit(False);
  end;

  {$IFDEF CPUX64}
  if AddressMode = am32bit then
    Result := SetIntelWow64Context(ThreadID, FContext)
  else
  {$ENDIF}
    Result := SetIntelContext(ThreadID, FContext);

  if Result then
  begin
    KnownRegs.List[RegID].Modifyed := True;
    case RegID of
      91..99: KnownRegs.List[17].Modifyed := True;            // EFlags
      102..109: KnownRegs.List[32].Modifyed := True;          // TagWord
      110..123: KnownRegs.List[31].Modifyed := True;          // StatusWord
      124..132: KnownRegs.List[30].Modifyed := True;          // ControlWord
      133..147: KnownRegs.List[57].Modifyed := True;          // MxCsr
    end;
    // Update LastReg cache...
    UpdateLastRegData(RegID);
    LastReg.Modifyed := True;
  end;
end;

end.
