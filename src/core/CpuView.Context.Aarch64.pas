////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Context.Aarch64.pas
//  * Purpose   : Aarch64 processor context implementation.
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

unit CpuView.Context.Aarch64;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  TypInfo,
  StrUtils,
  Generics.Collections,
  FWHexView.Common,
  CpuView.Common,
  CpuView.CPUContext,
  CpuView.XML,
  CpuView.Context.Params,
  CpuView.Context.Aarch64.Types;

type

  { TAarch64CtxSettings }

  TAarch64CtxSettings = class(TContextAbstractSettings)
  private const
    xmlName = 'aarch64';
    xmlFlags = 'flags';
    xmlShowDouble = 'showDouble';
    xmlShowSingle = 'showSingle';
    xmlShowVectored = 'showVectored';
    xmlRegList = 'regs';
    xmlRegID = 'id';
    SavedRegs = [0..28, 36, 68, 100];
  private
    FRegViewMode: TDictionary<Integer, TRegViewMode>;
    FShowDouble, FShowSingle, FShowVectored: Boolean;
  protected
    function GetContextName: string; override;
    procedure InternalLoadFromXML(Root: IXMLNode); override;
    procedure InternalSaveToXML(Root: IXMLNode); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InitDefault; override;
    // load/save session settings
    procedure LoadFromContext(ACtx: TAbstractCPUContext); override;
    procedure SaveToContext(ACtx: TAbstractCPUContext); override;
  end;

  { TAarch64CpuContext }

  TAarch64CpuContext = class(TCommonCpuContext)
  private
    FContext: TAarch64ThreadContext;
    FShowDouble: Boolean;
    FShowSingle: Boolean;
    FShowVectored: Boolean;
    function ExtractFpcrRMode: string;
    function ExtractFpcrRModeValue: Byte;
    function ExtractModeField: string;
    function ExtractModeFieldValue: Word;
    function GetRegExtendedIndex(const ARegName: string): Integer;
    function RegExtendedIndexIsSpecial(AIndex: Integer): Boolean;
    function RegExtendedIndexIsVectored(AIndex: Integer): Boolean;
    function RegExtendedIndexToMapIndex(AIndex: Integer): Integer;
    function RegExtendedIndexToRegSize(AIndex: Integer): Integer;
    procedure SetContext(AValue: TAarch64ThreadContext);
    procedure SetFpcrRModeValue(AValue: Byte);
    procedure SetShowDouble(AValue: Boolean);
    procedure SetShowSingle(AValue: Boolean);
    procedure SetShowVectored(AValue: Boolean);
    procedure UpdateModifyed(const Value: TAarch64ThreadContext);
    procedure UpdateQueryRegs;
  protected
    procedure BuildMap; override;
    procedure DoChangeViewMode(ARegID: TRegID; const Value: TRegViewMode); override;
    function ExtractJmpConditions: string;
    procedure InitKnownRegs; override;
    procedure UpdateLastRegData(ARegID: TRegID; SetModifyed: Boolean); override;
  public
    procedure InitDefault; override;
    function InstructonPoint: Int64; override;
    function InstructonPointID: TRegID; override;
    function IsActiveJump(const Value: string): Boolean; override;
    function RegDescriptor(const ARegName: string; out ADescriptor: TRegDescriptor): Boolean; overload; override;
    function RegQueryEnumString(ARegID: TRegID; AEnumIndex: Integer): string; override;
    function RegQueryEnumValuesCount(ARegID: TRegID): Integer; override;
    function RegQueryValue(ARegID: TRegID; out ARegValue: TRegValue): Boolean; overload; override;
    function RegQueryValue(const ARegName: string; out ARegValue: TRegValue): Boolean; overload; override;
    function RegSetValue(ARegID: TRegID; const ANewRegValue: TRegValue): Boolean; override;
    function StackBase: Int64; override;
    function StackPoint: Int64; override;
    function Update(ANewInstructionPoint: Int64 = 0): Boolean; override;
    property Context: TAarch64ThreadContext read FContext write SetContext;
    property ShowDouble: Boolean read FShowDouble write SetShowDouble;
    property ShowSingle: Boolean read FShowSingle write SetShowSingle;
    property ShowVectored: Boolean read FShowVectored write SetShowVectored;
  end;

implementation

const
  REG_VECTORED = 32;
  REG_SPECIAL = 64;

{ TAarch64CtxSettings }

constructor TAarch64CtxSettings.Create;
begin
  FRegViewMode := TDictionary<Integer, TRegViewMode>.Create;
end;

destructor TAarch64CtxSettings.Destroy;
begin
  FRegViewMode.Free;
  inherited Destroy;
end;

function TAarch64CtxSettings.GetContextName: string;
begin
  Result := xmlName;
end;

procedure TAarch64CtxSettings.InternalLoadFromXML(Root: IXMLNode);
var
  Flags, Regs, ItemNode: IXMLNode;
  I, RegID: Integer;
begin
  Flags := FindNode(Root, xmlFlags);
  if Flags = nil then Exit;
  FShowDouble := GetNodeAttr(Flags, xmlShowDouble);
  FShowSingle := GetNodeAttr(Flags, xmlShowSingle);
  FShowVectored := GetNodeAttr(Flags, xmlShowVectored);
  Regs := FindNode(Root, xmlRegList);
  if Regs = nil then Exit;
  for I := 0 to Regs.ChildNodes.Count - 1 do
  begin
    ItemNode := GetChildNode(Regs, I);
    RegID := GetNodeAttr(ItemNode, xmlRegID);
    FRegViewMode.AddOrSetValue(RegID, TRegViewMode(
      GetEnumValue(TypeInfo(TRegViewMode), GetNodeAttr(ItemNode, xmlMode))));
  end;
end;

procedure TAarch64CtxSettings.InternalSaveToXML(Root: IXMLNode);
var
  Flags, Regs, ItemNode: IXMLNode;
  RegID: Integer;
  ARegViewMode: TRegViewMode;
begin
  Flags := NewChild(Root, xmlFlags);
  SetNodeAttr(Flags, xmlShowDouble, FShowDouble);
  SetNodeAttr(Flags, xmlShowSingle, FShowSingle);
  SetNodeAttr(Flags, xmlShowVectored, FShowVectored);
  Regs := nil;
  for RegID in SavedRegs do
  begin
    if not FRegViewMode.TryGetValue(RegID, ARegViewMode) then Continue;
    if ARegViewMode = rvmHex then Continue;
    if Regs = nil then
      Regs := NewChild(Root, xmlRegList);
    ItemNode := NewChild(Regs, xmlItem);
    SetNodeAttr(ItemNode, xmlRegID, RegID);
    SetNodeAttr(ItemNode, xmlMode,
      GetEnumName(TypeInfo(TRegViewMode), Integer(ARegViewMode)));
  end;
end;

procedure TAarch64CtxSettings.InitDefault;
var
  I: Integer;
begin
  inherited;
  FShowDouble := True;
  FShowSingle := True;
  FShowVectored := True;
  FRegViewMode.Clear;
  for I in SavedRegs do
    FRegViewMode.Add(I, rvmHex);
end;

procedure TAarch64CtxSettings.LoadFromContext(ACtx: TAbstractCPUContext);
var
  Ctx: TAarch64CpuContext;
  RegID: Integer;
begin
  Ctx := ACtx as TAarch64CpuContext;
  FShowDouble := Ctx.ShowDouble;
  FShowSingle := Ctx.ShowSingle;
  FShowVectored := Ctx.ShowVectored;
  for RegID in SavedRegs do
    FRegViewMode.AddOrSetValue(RegID, Ctx.ViewMode[RegID]);
end;

procedure TAarch64CtxSettings.SaveToContext(ACtx: TAbstractCPUContext);
var
  Ctx: TAarch64CpuContext;
  RegID: Integer;
  ARegViewMode: TRegViewMode;
begin
  Ctx := ACtx as TAarch64CpuContext;
  Ctx.ShowDouble := FShowDouble;
  Ctx.ShowSingle := FShowSingle;
  Ctx.ShowVectored := FShowVectored;
  for RegID in SavedRegs do
    if FRegViewMode.TryGetValue(RegID, ARegViewMode) then
      Ctx.ViewMode[RegID] := ARegViewMode;
end;

{ TAarch64CpuContext }

procedure TAarch64CpuContext.InitDefault;
begin
  FShowVectored := True;
  UpdateMap(True);
end;

function TAarch64CpuContext.InstructonPoint: Int64;
begin
  Result := FContext.Pc;
end;

function TAarch64CpuContext.InstructonPointID: TRegID;
begin
  Result := 34;
end;

function TAarch64CpuContext.IsActiveJump(const Value: string): Boolean;
var
  NF, ZF, CF, VF: Boolean;
begin
  Result := False;

  if Value.StartsWith('B ') or Value.StartsWith('BL ') or Value.StartsWith('BLR ') or Value.StartsWith('BLX ') then
    Exit(True);

  {
  The N, Z, C, and V bits are identical to the SF, ZF, CF, and OF bits in the EFLAG register on x86.
  These bits are used to support conditional execution in conditionals and loops at the assembly level.
  We will cover condition codes used in Part 6:
  https://azeria-labs.com/arm-conditional-execution-and-branching-part-6/
  }

  // https://developer.arm.com/documentation/ddi0406/c/Application-Level-Architecture/Instruction-Details/Conditional-execution

  NF := FContext.Cpsr and (1 shl 31) <> 0;
  ZF := FContext.Cpsr and (1 shl 30) <> 0;
  CF := FContext.Cpsr and (1 shl 29) <> 0;
  VF := FContext.Cpsr and (1 shl 28) <> 0;

  if ZF then
  begin
    if Value.StartsWith('CBZ ') or Value.StartsWith('TBZ ') or Value.EndsWith('.EQ') then
      Exit(True);
  end
  else
    if Value.StartsWith('CBNZ ') or Value.StartsWith('TBNZ ') or Value.EndsWith('.NE') then
      Exit(True);

  if CF then
    if Value.EndsWith('.CS') then Exit(True)
  else
    if Value.EndsWith('.CC') then Exit(True);

  if NF then
    if Value.EndsWith('.MI') then Exit(True)
  else
    if Value.EndsWith('.PL') then Exit(True);

  if VF then
    if Value.EndsWith('.VS') then Exit(True)
  else
    if Value.EndsWith('.VC') then Exit(True);

  if CF and not ZF then
    if Value.EndsWith('.HI') then Exit(True);

  if ZF or not CF then
    if Value.EndsWith('.LS') then Exit(True);

  if NF = VF then
    if Value.EndsWith('.GE') then Exit(True)
  else
    if Value.EndsWith('.LT') then Exit(True);

  if (NF = VF) and not ZF then
    if Value.EndsWith('.GT') then Exit(True);

  if ZF or (NF <> VF) then
    if Value.EndsWith('.LE') then Exit(True);
end;

function TAarch64CpuContext.RegDescriptor(const ARegName: string; out
  ADescriptor: TRegDescriptor): Boolean;
var
  ExtIdx, Idx: Integer;
begin
  ExtIdx := GetRegExtendedIndex(ARegName);
  if ExtIdx < 0 then
  begin
    ADescriptor := Default(TRegDescriptor);
    Result := False;
  end
  else
  begin
    Idx := RegExtendedIndexToMapIndex(ExtIdx);
    if RegExtendedIndexIsVectored(ExtIdx) then
      // Map to Vxx regs
      Inc(Idx, 100);
    Result := RegDescriptor(Idx, ADescriptor);
  end;
end;

function TAarch64CpuContext.RegQueryEnumString(ARegID: TRegID;
  AEnumIndex: Integer): string;
begin
  Result := '';
  case ARegID of
    149:  // CPSR Mode Field (M)
    begin
      case AEnumIndex of
        %10000: Result := '10000 User';
        %10001: Result := '10001 FIQ';
        %10010: Result := '10010 IRQ';
        %10011: Result := '10011 Supervisor';
        %10110: Result := '10110 Monitor';
        %10111: Result := '10111 Abort';
        %11010: Result := '11010 Hyp';
        %11111: Result := '11111 System';
      else
        // Undefined 11011	PL1	Always	Both
        Result := IntToBin(AEnumIndex, 5) + ' Undefined';
      end;
    end;
    166: // FPCR RMode Field
    begin
      case AEnumIndex of
        0: Result := 'RN - Round to Nearest (0)';
        1: Result := 'RP - Round towards Plus Infinity (1)';
        2: Result := 'RM - Round towards Minus Infinity (2)';
        3: Result := 'RZ - Round towards Zero (3)';
      end;
    end;
  end;
end;

function TAarch64CpuContext.RegQueryEnumValuesCount(ARegID: TRegID): Integer;
begin
  case ARegID of
    149: Result := 9; // CPSR Mode Field (M)
    166: Result := 4; // FPCR RMode Field
  end;
end;

function TAarch64CpuContext.RegQueryValue(ARegID: TRegID; out
  ARegValue: TRegValue): Boolean;
begin
  Result := True;
  ARegValue := Default(TRegValue);
  case ARegID of
    // defaurt regs
    0..30: ARegValue.ValueSize := 8;
    // fp/lr/sp/pc
    31..34: ARegValue.ValueSize := 8;
    // cpsr
    35: ARegValue.ValueSize := 4;
    // S
    36..67: ARegValue.ValueSize := 4;
    // D
    68..99: ARegValue.ValueSize := 8;
    // V
    100..131: ARegValue.ValueSize := 16;
    // fpcr
    132: ARegValue.ValueSize := 4;
    // fpsr
    133: ARegValue.ValueSize := 4;
    // cpsr bit flags
    134..148: ARegValue.ValueSize := 1;
    // fpcr bit flags
    152..166: ARegValue.ValueSize := 1;
    // fpsr bit flags
    169..179: ARegValue.ValueSize := 1;
  else
    ARegValue.ValueSize := 0;
  end;
  case ARegID of
    0..28: ARegValue.QwordValue := Context.X[ARegID];
    29, 31: ARegValue.QwordValue := Context.Fp;
    30, 32: ARegValue.QwordValue := Context.Lr;
    33: ARegValue.QwordValue := Context.Sp;
    34: ARegValue.QwordValue := Context.Pc;
    35: ARegValue.DwordValue := Context.Cpsr;

    36..67: Move(Context.V[ARegID - 36], ARegValue.Ext16[0], 4);
    68..99: Move(Context.V[ARegID - 68], ARegValue.Ext16[0], 8);
    100..131: Move(Context.V[ARegID - 100], ARegValue.Ext16[0], 16);

    132: ARegValue.DwordValue := Context.Fpcr;
    133: ARegValue.DwordValue := Context.Fpsr;

    134: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 31); // CPSR - N
    135: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 30); // CPSR - Z
    136: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 29); // CPSR - C
    137: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 28); // CPSR - V
    138: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 27); // CPSR - Q
    139: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 16); // CPSR - GE0
    140: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 17); // CPSR - GE1
    141: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 18); // CPSR - GE2
    142: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 19); // CPSR - GE3
    143: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 09); // CPSR - E
    144: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 08); // CPSR - A
    145: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 07); // CPSR - I
    146: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 06); // CPSR - F
    147: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 24); // CPSR - J
    148: ARegValue.ByteValue := ExtractBitValue(FContext.Cpsr, 05); // CPSR - T

    152: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 26); // FPCR - AHP
    153: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 25); // FPCR - DN
    154: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 24); // FPCR - FZ
    155: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 19); // FPCR - FZ16
    156: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 15); // FPCR - IDE
    157: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 13); // FPCR - EBF
    158: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 12); // FPCR - IXE
    159: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 11); // FPCR - UFE
    160: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 10); // FPCR - OFE
    161: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 9);  // FPCR - DZE
    162: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 8);  // FPCR - IOE
    163: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 2);  // FPCR - NEP
    164: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 1);  // FPCR - AH
    165: ARegValue.ByteValue := ExtractBitValue(FContext.Fpcr, 0);  // FPCR - FIZ
    166: ARegValue.ByteValue := ExtractFpcrRModeValue;              // FPCR - RMode

    169: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 31); // FPSR - N
    170: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 30); // FPSR - Z
    171: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 29); // FPSR - C
    172: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 28); // FPSR - V
    173: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 27); // FPSR - QC
    174: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 7);  // FPSR - IDC
    175: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 4);  // FPSR - IXC
    176: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 3);  // FPSR - UFC
    177: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 2);  // FPSR - OFC
    178: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 1);  // FPSR - DZC
    179: ARegValue.ByteValue := ExtractBitValue(FContext.Fpsr, 0);  // FPSR - IOC

  else
    Result := False;
  end;
end;

function TAarch64CpuContext.RegQueryValue(const ARegName: string; out
  ARegValue: TRegValue): Boolean;
var
  Idx: Integer;
  FullRegValue: TRegValue;
begin
  ARegValue := Default(TRegValue);
  Idx := GetRegExtendedIndex(ARegName);
  if (Idx >= 0) and RegQueryValue(RegExtendedIndexToMapIndex(Idx), FullRegValue) then
  begin
    ARegValue.ValueSize := RegExtendedIndexToRegSize(Idx);
    case ARegValue.ValueSize of
      1: ARegValue.ByteValue := FullRegValue.ByteValue;
      2: ARegValue.WordValue := FullRegValue.WordValue;
      4: ARegValue.DwordValue := FullRegValue.DwordValue;
      8: ARegValue.QwordValue := FullRegValue.QwordValue;
      16: Move(FullRegValue.Ext16[0], ARegValue.Ext16[0], 16);
    end;
    Result := True;
    Exit;
  end;
  Result := False;
end;

function TAarch64CpuContext.RegSetValue(ARegID: TRegID;
  const ANewRegValue: TRegValue): Boolean;
begin
  FContext := ContextQueryParams.GetDefContext(ThreadID);
  FContext.ChangedRegID := ARegID;
  case ARegID of
    0..28: FContext.X[ARegID] := ANewRegValue.QwordValue;
    31: FContext.Fp := ANewRegValue.QwordValue;
    32: FContext.Lr := ANewRegValue.QwordValue;
    33: FContext.Sp := ANewRegValue.QwordValue;
    34: FContext.Pc := ANewRegValue.QwordValue;
    35: FContext.Cpsr := ANewRegValue.DwordValue;

    36..67: Move(ANewRegValue.Ext16[0], FContext.V[ARegID - 36], 4);
    68..99: Move(ANewRegValue.Ext16[0], FContext.V[ARegID - 68], 8);
    100..131: Move(ANewRegValue.Ext16[0], FContext.V[ARegID - 100], 16);

    132: FContext.Fpcr := ANewRegValue.DwordValue;
    133: FContext.Fpsr := ANewRegValue.DwordValue;

    134: SetBitValue(FContext.Cpsr, 31, ANewRegValue.ByteValue); // CPSR - N
    135: SetBitValue(FContext.Cpsr, 30, ANewRegValue.ByteValue); // CPSR - Z
    136: SetBitValue(FContext.Cpsr, 29, ANewRegValue.ByteValue); // CPSR - C
    137: SetBitValue(FContext.Cpsr, 28, ANewRegValue.ByteValue); // CPSR - V
    138: SetBitValue(FContext.Cpsr, 27, ANewRegValue.ByteValue); // CPSR - Q
    139: SetBitValue(FContext.Cpsr, 16, ANewRegValue.ByteValue); // CPSR - GE0
    140: SetBitValue(FContext.Cpsr, 17, ANewRegValue.ByteValue); // CPSR - GE1
    141: SetBitValue(FContext.Cpsr, 18, ANewRegValue.ByteValue); // CPSR - GE2
    142: SetBitValue(FContext.Cpsr, 19, ANewRegValue.ByteValue); // CPSR - GE3
    143: SetBitValue(FContext.Cpsr, 09, ANewRegValue.ByteValue); // CPSR - E
    144: SetBitValue(FContext.Cpsr, 08, ANewRegValue.ByteValue); // CPSR - A
    145: SetBitValue(FContext.Cpsr, 07, ANewRegValue.ByteValue); // CPSR - I
    146: SetBitValue(FContext.Cpsr, 06, ANewRegValue.ByteValue); // CPSR - F
    147: SetBitValue(FContext.Cpsr, 24, ANewRegValue.ByteValue); // CPSR - J
    148: SetBitValue(FContext.Cpsr, 05, ANewRegValue.ByteValue); // CPSR - T

    152: SetBitValue(FContext.Fpcr, 26, ANewRegValue.ByteValue); // FPCR - AHP
    153: SetBitValue(FContext.Fpcr, 25, ANewRegValue.ByteValue); // FPCR - DN
    154: SetBitValue(FContext.Fpcr, 24, ANewRegValue.ByteValue); // FPCR - FZ
    155: SetBitValue(FContext.Fpcr, 19, ANewRegValue.ByteValue); // FPCR - FZ16
    156: SetBitValue(FContext.Fpcr, 15, ANewRegValue.ByteValue); // FPCR - IDE
    157: SetBitValue(FContext.Fpcr, 13, ANewRegValue.ByteValue); // FPCR - EBF
    158: SetBitValue(FContext.Fpcr, 12, ANewRegValue.ByteValue); // FPCR - IXE
    159: SetBitValue(FContext.Fpcr, 11, ANewRegValue.ByteValue); // FPCR - UFE
    160: SetBitValue(FContext.Fpcr, 10, ANewRegValue.ByteValue); // FPCR - OFE
    161: SetBitValue(FContext.Fpcr, 9, ANewRegValue.ByteValue);  // FPCR - DZE
    162: SetBitValue(FContext.Fpcr, 8, ANewRegValue.ByteValue);  // FPCR - IOE
    163: SetBitValue(FContext.Fpcr, 2, ANewRegValue.ByteValue);  // FPCR - NEP
    164: SetBitValue(FContext.Fpcr, 1, ANewRegValue.ByteValue);  // FPCR - AH
    165: SetBitValue(FContext.Fpcr, 0, ANewRegValue.ByteValue);  // FPCR - FIZ
    166: SetFpcrRModeValue(ANewRegValue.ByteValue);              // FPCR - RMode

    169: SetBitValue(FContext.Fpsr, 31, ANewRegValue.ByteValue); // FPSR - N
    170: SetBitValue(FContext.Fpsr, 30, ANewRegValue.ByteValue); // FPSR - Z
    171: SetBitValue(FContext.Fpsr, 29, ANewRegValue.ByteValue); // FPSR - C
    172: SetBitValue(FContext.Fpsr, 28, ANewRegValue.ByteValue); // FPSR - V
    173: SetBitValue(FContext.Fpsr, 27, ANewRegValue.ByteValue); // FPSR - QC
    174: SetBitValue(FContext.Fpsr, 7, ANewRegValue.ByteValue);  // FPSR - IDC
    175: SetBitValue(FContext.Fpsr, 4, ANewRegValue.ByteValue);  // FPSR - IXC
    176: SetBitValue(FContext.Fpsr, 3, ANewRegValue.ByteValue);  // FPSR - UFC
    177: SetBitValue(FContext.Fpsr, 2, ANewRegValue.ByteValue);  // FPSR - OFC
    178: SetBitValue(FContext.Fpsr, 1, ANewRegValue.ByteValue);  // FPSR - DZC
    179: SetBitValue(FContext.Fpsr, 0, ANewRegValue.ByteValue);  // FPSR - IOC
  end;

  Result := ContextQueryParams.SetDefContext(ThreadID, FContext);
  if Result then
  begin
    KnownRegs.List[ARegID].Modifyed := True;
    case ARegID of
      134..151: KnownRegs.List[35].Modifyed := True;            // Cpsr
      152..166: KnownRegs.List[132].Modifyed := True;           // Fpcr
      169..179: KnownRegs.List[133].Modifyed := True;           // Fpsr
    end;
    // Update LastReg cache...
    UpdateLastRegData(ARegID, True);
    UpdateQueryRegs;
    DoChange(cctDataChange);
  end;
end;

procedure TAarch64CpuContext.SetContext(AValue: TAarch64ThreadContext);
var
  CtxBitnessChanged: Boolean;
begin
  if not CompareMem(@FContext, @AValue, SizeOf(TAarch64ThreadContext)) then
    UpdateModifyed(AValue);
  CtxBitnessChanged := (FContext.Pc = 0) or
    (AValue.Aarch32Context <> FContext.Aarch32Context);
  FContext := AValue;
  UpdateQueryRegs;
  if CtxBitnessChanged then
    UpdateMap(True)
  else
    DoChange(cctContextUpdated);
end;

procedure TAarch64CpuContext.SetFpcrRModeValue(AValue: Byte);
var
  Mask: Integer;
  NewControlWord: Integer;
begin
  NewControlWord := (AValue and 3) shl 22;
  Mask := not (3 shl 22);
  FContext.Fpcr := (FContext.Fpcr and Mask) or NewControlWord;
end;

procedure TAarch64CpuContext.SetShowDouble(AValue: Boolean);
begin
  if FShowDouble <> AValue then
  begin
    FShowDouble := AValue;
    UpdateMap;
  end;
end;

procedure TAarch64CpuContext.SetShowSingle(AValue: Boolean);
begin
  if FShowSingle <> AValue then
  begin
    FShowSingle := AValue;
    UpdateMap;
  end;
end;

procedure TAarch64CpuContext.SetShowVectored(AValue: Boolean);
begin
  if FShowVectored <> AValue then
  begin
    FShowVectored := AValue;
    UpdateMap;
  end;
end;

function TAarch64CpuContext.StackBase: Int64;
begin
  Result := FContext.Fp;
end;

function TAarch64CpuContext.StackPoint: Int64;
begin
  Result := FContext.Sp;
end;

function TAarch64CpuContext.Update(ANewInstructionPoint: Int64): Boolean;
var
  ACtx: TAarch64ThreadContext;
begin
  if ThreadID = 0 then
    Exit(False)
  else
    Result := True;

  {$IFDEF CPUAARCH64}
  if AddressMode = am32bit then
  begin
    if ANewInstructionPoint = 0 then
      ACtx := ContextQueryParams.Get32Context(ThreadID)
    else
    begin
      ACtx := ContextQueryParams.Get32Context(ThreadID);
      ACtx.Pc := ANewInstructionPoint;
    end;
  end
  else
  {$ENDIF}
  begin
    if ANewInstructionPoint = 0 then
      ACtx := ContextQueryParams.GetDefContext(ThreadID)
    else
    begin
      ACtx := ContextQueryParams.GetDefContext(ThreadID);
      ACtx.Pc := ANewInstructionPoint;
    end;
  end;

  ContextFeatures := [];
  Context := ACtx;
end;

procedure TAarch64CpuContext.UpdateModifyed(const Value: TAarch64ThreadContext);

  function CheckReg(A, B: Int64; RegID: Integer): Boolean;
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
      (A shr Index) and 3 <> (B shr Index) and 3;
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

  for I := 0 to 28 do
    CheckReg(FContext.X[I], Value.X[I], I);

  CheckReg(FContext.Fp, Value.Fp, 31);
  CheckReg(FContext.Lr, Value.Lr, 32);
  CheckReg(FContext.Sp, Value.Sp, 33);
  CheckReg(FContext.Pc, Value.Pc, 34);

  for I := 0 to 31 do
  begin
    CheckMem(@FContext.V[I], @Value.V[I], 4, 36 + I);                    // Single
    CheckMem(@FContext.V[I], @Value.V[I], 8, 68 + I);                    // Double
    CheckMem(@FContext.V[I], @Value.V[I], 16, 100 + I);                  // Vector
  end;

  if CheckReg(FContext.Cpsr, Value.Cpsr, 35) then
  begin
    CheckBit(FContext.Cpsr, Value.Cpsr, 31, 134); // CPSR - N
    CheckBit(FContext.Cpsr, Value.Cpsr, 30, 135); // CPSR - Z
    CheckBit(FContext.Cpsr, Value.Cpsr, 29, 136); // CPSR - C
    CheckBit(FContext.Cpsr, Value.Cpsr, 28, 137); // CPSR - V
    CheckBit(FContext.Cpsr, Value.Cpsr, 27, 138); // CPSR - Q
    CheckBit(FContext.Cpsr, Value.Cpsr, 16, 139); // CPSR - GE0
    CheckBit(FContext.Cpsr, Value.Cpsr, 17, 140); // CPSR - GE1
    CheckBit(FContext.Cpsr, Value.Cpsr, 18, 141); // CPSR - GE2
    CheckBit(FContext.Cpsr, Value.Cpsr, 19, 142); // CPSR - GE3
    CheckBit(FContext.Cpsr, Value.Cpsr, 09, 143); // CPSR - E
    CheckBit(FContext.Cpsr, Value.Cpsr, 08, 144); // CPSR - A
    CheckBit(FContext.Cpsr, Value.Cpsr, 07, 145); // CPSR - I
    CheckBit(FContext.Cpsr, Value.Cpsr, 06, 146); // CPSR - F
    CheckBit(FContext.Cpsr, Value.Cpsr, 24, 147); // CPSR - J
    CheckBit(FContext.Cpsr, Value.Cpsr, 05, 148); // CPSR - T
  end;

  if CheckReg(FContext.Fpcr, Value.Fpcr, 132) then
  begin
    CheckBit(FContext.Fpcr, Value.Fpcr, 26, 152); // FPCR - AHP
    CheckBit(FContext.Fpcr, Value.Fpcr, 25, 153); // FPCR - DN
    CheckBit(FContext.Fpcr, Value.Fpcr, 24, 154); // FPCR - FZ
    CheckBit(FContext.Fpcr, Value.Fpcr, 19, 155); // FPCR - FZ16
    CheckBit(FContext.Fpcr, Value.Fpcr, 15, 156); // FPCR - IDE
    CheckBit(FContext.Fpcr, Value.Fpcr, 13, 157); // FPCR - EBF
    CheckBit(FContext.Fpcr, Value.Fpcr, 12, 158); // FPCR - IXE
    CheckBit(FContext.Fpcr, Value.Fpcr, 11, 159); // FPCR - UFE
    CheckBit(FContext.Fpcr, Value.Fpcr, 10, 160); // FPCR - OFE
    CheckBit(FContext.Fpcr, Value.Fpcr, 9, 161);  // FPCR - DZE
    CheckBit(FContext.Fpcr, Value.Fpcr, 8, 162);  // FPCR - IOE
    CheckBit(FContext.Fpcr, Value.Fpcr, 2, 163);  // FPCR - NEP
    CheckBit(FContext.Fpcr, Value.Fpcr, 1, 164);  // FPCR - AH
    CheckBit(FContext.Fpcr, Value.Fpcr, 0, 165);  // FPCR - FIZ
    CheckDoubleSet(FContext.Fpcr, Value.Fpcr, 21, 166); // FPCR - RMode
  end;

  if CheckReg(FContext.Fpsr, Value.Fpsr, 133) then
  begin
    CheckBit(FContext.Fpsr, Value.Fpsr, 31, 169); // FPSR - N
    CheckBit(FContext.Fpsr, Value.Fpsr, 30, 170); // FPSR - Z
    CheckBit(FContext.Fpsr, Value.Fpsr, 29, 171); // FPSR - C
    CheckBit(FContext.Fpsr, Value.Fpsr, 28, 172); // FPSR - V
    CheckBit(FContext.Fpsr, Value.Fpsr, 27, 173); // FPSR - QC
    CheckBit(FContext.Fpsr, Value.Fpsr, 7, 174);  // FPSR - IDC
    CheckBit(FContext.Fpsr, Value.Fpsr, 4, 175);  // FPSR - IXC
    CheckBit(FContext.Fpsr, Value.Fpsr, 3, 176);  // FPSR - UFC
    CheckBit(FContext.Fpsr, Value.Fpsr, 2, 177);  // FPSR - OFC
    CheckBit(FContext.Fpsr, Value.Fpsr, 1, 178);  // FPSR - DZC
    CheckBit(FContext.Fpsr, Value.Fpsr, 0, 179);  // FPSR - IOC
  end;
end;

procedure TAarch64CpuContext.UpdateQueryRegs;

  procedure AddReg(const RegName: string; AddrVA: Int64);
  var
    Param: string;
  begin
    if QueryRegAtAddr.TryGetValue(AddrVA, Param) then
      Param := Param + ' ' + RegName
    else
      Param := RegName;
    QueryRegAtAddr.AddOrSetValue(AddrVA, Param);
  end;

var
  I: Integer;
  RegValue: TRegValue;
begin
  QueryRegAtAddr.Clear;
  // X0..X28, Fp, Lr, Sp, Pc
  for I in [0..28, 31..34] do
  begin
    FillRegData(I);
    RegQueryValue(I, RegValue);
    AddReg(LastReg.RegName, RegValue.QwordValue);
  end;
end;

procedure TAarch64CpuContext.BuildMap;

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

  // https://learn.microsoft.com/en-us/cpp/build/overview-of-arm-abi-conventions?view=msvc-170
  // https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170

  // Volatile Parameter/Result scratch registers

  Row := Add(Map, 0);          // X0
  Add(Row, 180);               // X0 Hint
  Row := Add(Map, 1);          // X1
  Add(Row, 181);               // X1 Hint
  Row := Add(Map, 2);          // X2
  Add(Row, 182);               // X2 Hint
  Row := Add(Map, 3);          // X3
  Add(Row, 183);               // X3 Hint
  Row := Add(Map, 4);          // X4
  Add(Row, 184);               // X4 Hint
  Row := Add(Map, 5);          // X5
  Add(Row, 185);               // X5 Hint
  Row := Add(Map, 6);          // X6
  Add(Row, 186);               // X6 Hint
  Row := Add(Map, 7);          // X7
  Add(Row, 187);               // X7 Hint
  Row := Add(Map, 8);          // X8
  Add(Row, 188);               // X8 Hint

  AddSeparator;

  // Volatile Scratch registers

  Row := Add(Map, 9);          // X9
  Add(Row, 189);               // X9 Hint
  Row := Add(Map, 10);         // X10
  Add(Row, 190);               // X10 Hint

  if not Context.Aarch32Context then
  begin
    Row := Add(Map, 11);       // X11 - Fp in aarch32
    Add(Row, 191);             // X11 Hint
  end;

  Row := Add(Map, 12);         // X12
  Add(Row, 192);               // X12 Hint

  if not Context.Aarch32Context then
  begin
    Row := Add(Map, 13);       // X13
    Add(Row, 193);             // X13 Hint
    Row := Add(Map, 14);       // X14
    Add(Row, 194);             // X14 Hint
    Row := Add(Map, 15);       // X15
    Add(Row, 195);             // X15 Hint

    AddSeparator;

    // Non-volatile Intra-procedure-call scratch registers

    Row := Add(Map, 16);       // X16
    Add(Row, 196);             // X16 Hint
    Row := Add(Map, 17);       // X17
    Add(Row, 197);             // X17 Hint
    Row := Add(Map, 18);       // X18 - KPCR/TEB
    Add(Row, 198);             // X18 Hint

    AddSeparator;

    // Non-volatile Scratch registers

    Row := Add(Map, 19);       // X19
    Add(Row, 199);             // X19 Hint
    Row := Add(Map, 20);       // X20
    Add(Row, 200);             // X20 Hint
    Row := Add(Map, 21);       // X21
    Add(Row, 201);             // X21 Hint
    Row := Add(Map, 22);       // X22
    Add(Row, 202);             // X22 Hint
    Row := Add(Map, 23);       // X23
    Add(Row, 203);             // X23 Hint
    Row := Add(Map, 24);       // X24
    Add(Row, 204);             // X24 Hint
    Row := Add(Map, 25);       // X25
    Add(Row, 205);             // X25 Hint
    Row := Add(Map, 26);       // X26
    Add(Row, 206);             // X26 Hint
    Row := Add(Map, 27);       // X27
    Add(Row, 207);             // X27 Hint
    Row := Add(Map, 28);       // X28
    Add(Row, 208);             // X28 Hint
  end;

  AddSeparator;

  Row := Add(Map, 31);         // Fp
  Add(Row, 209);               // Fp Hint
  Row := Add(Map, 32);         // Lr
  Add(Row, 210);               // Lr Hint
  Row := Add(Map, 33);         // Sp
  Add(Row, 211);               // Sp Hint
  Row := Add(Map, 34);         // Pc
  Add(Row, 212);               // Pc Hint

  AddSeparator;

  Row := Add(Map, 35);         // Cpsr
  Add(Row, 213);               // JMP Condition hint

  // CPSR Flags

  Row := Add(Map, 134);        // CPSR - N
  Add(Row, 135);               // CPSR - Z
  Add(Row, 136);               // CPSR - C
  Add(Row, 137);               // CPSR - V

  Row := Add(Map, 138);        // CPSR - Q
  Add(Row, 143);               // CPSR - E
  Add(Row, 144);               // CPSR - A
  Add(Row, 145);               // CPSR - I
  Add(Row, 146);               // CPSR - F

  Row := Add(Map, 139);        // CPSR - GE0
  Add(Row, 140);               // CPSR - GE1
  Add(Row, 141);               // CPSR - GE2
  Add(Row, 142);               // CPSR - GE3

  if ShowDouble or ShowSingle or ShowVectored then
  begin
    AddSeparator;
    Add(Map, 132);             // Fpcr
    Row := Add(Map, 152);      // AHP
    Add(Row, 153);             // DN
    Add(Row, 154);             // FZ
    Add(Row, 155);             // FZ16
    Row := Add(Map, 157);      // EBF
    Add(Row, 163);             // NEP
    Add(Row, 164);             // AH
    Add(Row, 165);             // FIZ
    Row := Add(Map, 156);      // IDE
    Add(Row, 158);             // IXE
    Add(Row, 159);             // UFE
    Add(Row, 160);             // OFE
    Add(Row, 161);             // DZE
    Add(Row, 162);             // IOE
    Add(Map, 166);             // RMode

    AddSeparator;
    Add(Map, 133);             // Fpsr
    Row := Add(Map, 169);      // FPSR - N
    Add(Row, 170);             // FPSR - Z
    Add(Row, 171);             // FPSR - C
    Add(Row, 172);             // FPSR - V
    Add(Row, 173);             // FPSR - QC
    Row := Add(Map, 174);      // FPSR - IDC
    Add(Row, 175);             // FPSR - IXC
    Add(Row, 176);             // FPSR - UFC
    Add(Row, 177);             // FPSR - OFC
    Add(Row, 178);             // FPSR - DZC
    Add(Row, 179);             // FPSR - IOC
  end;

  if ShowSingle then
  begin
    AddSeparator;
    Add(Map, 36);              // S0
    Add(Map, 37);              // S1
    Add(Map, 38);              // S2
    Add(Map, 39);              // S3
    Add(Map, 40);              // S4
    Add(Map, 41);              // S5
    Add(Map, 42);              // S6
    Add(Map, 43);              // S7
    Add(Map, 44);              // S8
    Add(Map, 45);              // S9
    Add(Map, 46);              // S10
    Add(Map, 47);              // S11
    Add(Map, 48);              // S12
    Add(Map, 49);              // S13
    Add(Map, 50);              // S14
    Add(Map, 51);              // S15
    Add(Map, 52);              // S16
    Add(Map, 53);              // S17
    Add(Map, 54);              // S18
    Add(Map, 55);              // S19
    Add(Map, 56);              // S20
    Add(Map, 57);              // S21
    Add(Map, 58);              // S22
    Add(Map, 59);              // S23
    Add(Map, 60);              // S24
    Add(Map, 61);              // S25
    Add(Map, 62);              // S26
    Add(Map, 63);              // S27
    Add(Map, 64);              // S28
    Add(Map, 65);              // S29
    Add(Map, 66);              // S30
    Add(Map, 67);              // S31
  end;

  if ShowDouble then
  begin
    AddSeparator;
    Add(Map, 68);              // D0
    Add(Map, 69);              // D1
    Add(Map, 70);              // D2
    Add(Map, 71);              // D3
    Add(Map, 72);              // D4
    Add(Map, 73);              // D5
    Add(Map, 74);              // D6
    Add(Map, 75);              // D7
    Add(Map, 76);              // D8
    Add(Map, 77);              // D9
    Add(Map, 78);              // D10
    Add(Map, 79);              // D11
    Add(Map, 80);              // D12
    Add(Map, 81);              // D13
    Add(Map, 82);              // D14
    Add(Map, 83);              // D15
    Add(Map, 84);              // D16
    Add(Map, 85);              // D17
    Add(Map, 86);              // D18
    Add(Map, 87);              // D19
    Add(Map, 88);              // D20
    Add(Map, 89);              // D21
    Add(Map, 90);              // D22
    Add(Map, 91);              // D23
    Add(Map, 92);              // D24
    Add(Map, 93);              // D25
    Add(Map, 94);              // D26
    Add(Map, 95);              // D27
    Add(Map, 96);              // D28
    Add(Map, 97);              // D29
    Add(Map, 98);              // D30
    Add(Map, 99);              // D31
  end;

  if ShowVectored then
  begin
    AddSeparator;
    Add(Map, 100);              // V0
    Add(Map, 101);              // V1
    Add(Map, 102);              // V2
    Add(Map, 103);              // V3
    Add(Map, 104);              // V4
    Add(Map, 105);              // V5
    Add(Map, 106);              // V6
    Add(Map, 107);              // V7
    Add(Map, 108);              // V8
    Add(Map, 109);              // V9
    Add(Map, 110);              // V10
    Add(Map, 111);              // V11
    Add(Map, 112);              // V12
    Add(Map, 113);              // V13
    Add(Map, 114);              // V14
    Add(Map, 115);              // V15
    Add(Map, 116);              // V16
    Add(Map, 117);              // V17
    Add(Map, 118);              // V18
    Add(Map, 119);              // V19
    Add(Map, 120);              // V20
    Add(Map, 121);              // V21
    Add(Map, 122);              // V22
    Add(Map, 123);              // V23
    Add(Map, 124);              // V24
    Add(Map, 125);              // V25
    Add(Map, 126);              // V26
    Add(Map, 127);              // V27
    Add(Map, 128);              // V28
    Add(Map, 129);              // V29
    Add(Map, 130);              // V30
    Add(Map, 131);              // V31
  end;

end;

procedure TAarch64CpuContext.DoChangeViewMode(ARegID: TRegID;
  const Value: TRegViewMode);
var
  I: Integer;
begin
  case ARegID of
    36..67: // S
      for I := 36 to 67 do
        KnownRegs.List[I].ViewMode := Value;
    68..99: // D
      for I := 68 to 99 do
        KnownRegs.List[I].ViewMode := Value;
    100..131: // V
      for I := 100 to 131 do
        KnownRegs.List[I].ViewMode := Value;
  else
    KnownRegs.List[ARegID].ViewMode := Value;
  end;
end;

function TAarch64CpuContext.ExtractJmpConditions: string;
var
  NF, ZF, CF, VF: Boolean;
begin

  // https://developer.arm.com/documentation/ddi0406/c/Application-Level-Architecture/Instruction-Details/Conditional-execution

  NF := FContext.Cpsr and (1 shl 31) <> 0;
  ZF := FContext.Cpsr and (1 shl 30) <> 0;
  CF := FContext.Cpsr and (1 shl 29) <> 0;
  VF := FContext.Cpsr and (1 shl 28) <> 0;

  if ZF then
    Result := 'EQ'
  else
    Result := 'NE';

  if CF then
    Result := Result + ',CS'
  else
    Result := Result + ',CC';

  if NF then
    Result := Result + ',MI'
  else
    Result := Result + ',PL';

  if VF then
    Result := Result + ',VS'
  else
    Result := Result + ',VC';

  if CF and not ZF then
    Result := Result + ',HI';

  if ZF or not CF then
    Result := Result + ',LS';

  if NF = VF then
    Result := Result + ',GE'
  else
    Result := Result + ',LT';

  if (NF = VF) and not ZF then
    Result := Result + ',GT';

  if ZF or (NF <> VF) then
    Result := Result + ',LE';
end;

procedure TAarch64CpuContext.InitKnownRegs;

  procedure Add(RegType: TContextRegType; SupportedViewMode: TRegViewModes;
    AFlags: TRegisterFlags = []);
  var
    R: TRegParam;
  begin
    R := Default(TRegParam);
    R.RowIndex := -1;
    R.ColIndex := -1;
    R.RegType := RegType;
    R.Flags := AFlags;
    R.SupportedViewMode := SupportedViewMode;
    KnownRegs.Add(R);
  end;

var
  vmDefRegMod: TRegViewModes;
  I: Integer;
begin
  KnownRegs.Clear;

  if Context.Aarch32Context then
    vmDefRegMod := vmReg32
  else
    vmDefRegMod := vmReg64;

  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 000 - X0
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 001 - X1
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 002 - X2
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 003 - X3
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 004 - X4
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 005 - X5
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 006 - X6
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 007 - X7
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 008 - X8
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 009 - X9
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 010 - X10
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 011 - X11
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 012 - X12
  Add(crtGeneralPurposeReg, vmDefRegMod, [rfIncrement..rfValidation]);    // 013 - X13

  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 014 - X14
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 015 - X15
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 016 - X16
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 017 - X17
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 018 - X18
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 019 - X19
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 020 - X20
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 021 - X21
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 022 - X22
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 023 - X23
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 024 - X24
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 025 - X25
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 026 - X26
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 027 - X27
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 028 - X28

  // Unused X29, X30

  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 029 - X29
  Add(crtGeneralPurposeReg, vmReg64, [rfIncrement..rfValidation]);        // 030 - X30

  Add(crtGeneralPurposeReg, vmDefOnly, [rfChangeValue, rfValidation]);    // 031 - Fp (Frame Pointer)
  Add(crtGeneralPurposeReg, vmDefOnly, [rfChangeValue, rfValidation]);    // 032 - Lr (Link Register)
  Add(crtGeneralPurposeReg, vmDefOnly, [rfChangeValue, rfValidation]);    // 033 - Sp (Stack Pointer)
  Add(crtGeneralPurposeReg, vmDefOnly, [rfChangeValue, rfValidation]);    // 034 - Pc
  Add(crtExtra, vmDefOnly, [rfChangeValue, rfHint]);                      // 035 - Cpsr

  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 036 - S0
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 037 - S1
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 038 - S2
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 039 - S3
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 040 - S4
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 041 - S5
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 042 - S6
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 043 - S7
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 044 - S8
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 045 - S9
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 046 - S10
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 047 - S11
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 048 - S12
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 049 - S13
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 050 - S14
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 051 - S15
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 052 - S16
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 053 - S17
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 054 - S18
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 055 - S19
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 056 - S20
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 057 - S21
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 058 - S22
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 059 - S23
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 060 - S24
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 061 - S25
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 062 - S26
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 063 - S27
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 064 - S28
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 065 - S29
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 066 - S30
  Add(crtFloatingPointReg, vmSingleReg, [rfZero, rfChangeValue]);         // 067 - S31

  for I := 36 to 67 do
    KnownRegs.List[I].ViewMode := rvmFloat32;

  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 068 - D0
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 069 - D1
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 070 - D2
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 071 - D3
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 072 - D4
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 073 - D5
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 074 - D6
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 075 - D7
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 076 - D8
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 077 - D9
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 078 - D10
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 079 - D11
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 080 - D12
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 081 - D13
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 082 - D14
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 083 - D15
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 084 - D16
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 085 - D17
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 086 - D18
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 087 - D19
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 088 - D20
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 089 - D21
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 090 - D22
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 091 - D23
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 092 - D24
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 093 - D25
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 094 - D26
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 095 - D27
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 096 - D28
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 097 - D29
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 098 - D30
  Add(crtFloatingPointReg, vmDoubleReg, [rfZero, rfChangeValue]);         // 099 - D31

  for I := 68 to 99 do
    KnownRegs.List[I].ViewMode := rvmFloat64;

  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 100 - V0
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 101 - V1
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 102 - V2
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 103 - V3
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 104 - V4
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 105 - V5
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 106 - V6
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 107 - V7
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 108 - V8
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 109 - V9
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 110 - V10
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 111 - V11
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 112 - V12
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 113 - V13
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 114 - V14
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 115 - V15
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 116 - V16
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 117 - V17
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 118 - V18
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 119 - V19
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 120 - V20
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 121 - V21
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 122 - V22
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 123 - V23
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 124 - V24
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 125 - V25
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 126 - V26
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 127 - V27
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 128 - V28
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 129 - V29
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 130 - V30
  Add(crtValue, vmSimdReg, [rfZero, rfChangeValue]);          // 131 - V31

  Add(crtExtra, vmDefOnly, [rfChangeValue, rfHint]);          // 132 - Fpcr
  Add(crtExtra, vmDefOnly, [rfChangeValue, rfHint]);          // 133 - Fpsr

  // Current Program Status Register (CPSR) Flags.

  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 134 - N
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 135 - Z
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 136 - C
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 137 - V
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 138 - Q
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 139 - GE0
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 140 - GE1
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 141 - GE2
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 142 - GE3
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 143 - E
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 144 - A
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 145 - I
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 146 - F

  // Unsupported CPSR Flags

  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 147 - J
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 148 - T
  Add(crtEnumValue, vmDefOnly, [rfChangeValue]);              // 149 - M
  Add(crtEnumValue, vmDefOnly, [rfChangeValue]);              // 150 - IT[0..2]
  Add(crtEnumValue, vmDefOnly, [rfChangeValue]);              // 151 - IT[3..7]

  // Floating-point Control Register (FPCR) Flags.

  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 152 - AHP
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 153 - DN
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 154 - FZ
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 155 - FZ16
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 156 - IDE
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 157 - EBF
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 158 - IXE
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 159 - UFE
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 160 - OFE
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 161 - DZE
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 162 - IOE
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 163 - NEP
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 164 - AH
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 165 - FIZ

  Add(crtEnumValue, vmDefOnly, [rfChangeValue, rfHint]);      // 166 - RMode

  // Unsupported FPCR Flags

  Add(crtEnumValue, vmDefOnly, [rfChangeValue, rfHint]);      // 167 - Stride
  Add(crtEnumValue, vmDefOnly, [rfChangeValue, rfHint]);      // 168 - Len

  // Floating-point Status Register (FPSR) Flags

  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 169 - N
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 170 - Z
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 171 - C
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 172 - V
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 173 - QC
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 174 - IDC
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 175 - IXC
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 176 - UFC
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 177 - OFC
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 178 - DZC
  Add(crtBitValue, vmDefOnly, [rfToggle, rfHint]);            // 179 - IOC

  // Hints

  Add(crtSelectableHint, vmDefOnly);                          // 180 - X0 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 181 - X1 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 182 - X2 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 183 - X3 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 184 - X4 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 185 - X5 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 186 - X6 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 187 - X7 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 188 - X8 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 189 - X9 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 190 - X10 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 191 - X11 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 192 - X12 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 193 - X13 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 194 - X14 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 195 - X15 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 196 - X16 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 197 - X17 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 198 - X18 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 199 - X19 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 200 - X20 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 201 - X21 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 202 - X22 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 203 - X23 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 204 - X24 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 205 - X25 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 206 - X26 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 207 - X27 Hint
  Add(crtSelectableHint, vmDefOnly);                          // 208 - X28 Hint

  Add(crtSelectableHint, vmDefOnly);                          // 209 - Fp Hint
  Add(crtSelectableHint, vmDefOnly);                          // 210 - Lr Hint
  Add(crtSelectableHint, vmDefOnly);                          // 211 - Sp Hint
  Add(crtSelectableHint, vmDefOnly);                          // 212 - Pc Hint

  // CPSR Hint

  Add(crtSelectableHint, vmDefOnly);                          // 213 - JMP Conditions
end;

function TAarch64CpuContext.ExtractFpcrRMode: string;
begin
  Result := RegQueryEnumString(166, ExtractFpcrRModeValue);
end;

function TAarch64CpuContext.ExtractFpcrRModeValue: Byte;
begin
  Result := (Context.Fpcr shr 22) and 3;
end;

function TAarch64CpuContext.ExtractModeField: string;
begin
  Result := RegQueryEnumString(149, ExtractModeFieldValue);
end;

function TAarch64CpuContext.ExtractModeFieldValue: Word;
begin
  Result := Context.Cpsr and %11111;
end;

function TAarch64CpuContext.GetRegExtendedIndex(const ARegName: string
  ): Integer;
var
  RegIdx: string;
  I: Integer;
begin
  Result := -1;
  RegIdx := Copy(ARegName, 2, Length(ARegName) - 1);
  if TryStrToInt(RegIdx, I) then
  begin
    case ARegName[1] of
      'X': Result := I + 8 shl 7;
      'W', 'R': Result := I + 4 shl 7;
      'B': Result := I + REG_VECTORED + 1 shl 7;
      'H': Result := I + REG_VECTORED + 2 shl 7;
      'S': Result := I + REG_VECTORED + 4 shl 7;
      'D': Result := I + REG_VECTORED + 8 shl 7;
      'Q': Result := I + REG_VECTORED + 16 shl 7;
    end;
  end
  else
  begin
    if ARegName = 'FP' then Result := 1 + REG_SPECIAL + 8 shl 7;
    if ARegName = 'LR' then Result := 2 + REG_SPECIAL + 8 shl 7;
    if ARegName = 'SP' then Result := 3 + REG_SPECIAL + 8 shl 7;
    if ARegName = 'PC' then Result := 4 + REG_SPECIAL + 8 shl 7;
  end;
end;

function TAarch64CpuContext.RegExtendedIndexIsSpecial(AIndex: Integer): Boolean;
begin
  Result := AIndex and REG_SPECIAL = REG_SPECIAL;
end;

function TAarch64CpuContext.RegExtendedIndexIsVectored(AIndex: Integer
  ): Boolean;
begin
  Result := AIndex and REG_VECTORED = REG_VECTORED;
end;

function TAarch64CpuContext.RegExtendedIndexToMapIndex(AIndex: Integer
  ): Integer;
begin
  Result := AIndex and $1F;
  if RegExtendedIndexIsSpecial(AIndex) then
    Inc(Result, 30);
  if RegExtendedIndexIsVectored(AIndex) then
    Inc(Result, 100);
end;

function TAarch64CpuContext.RegExtendedIndexToRegSize(AIndex: Integer): Integer;
begin
  Result := AIndex shr 7;
end;

procedure TAarch64CpuContext.UpdateLastRegData(ARegID: TRegID;
  SetModifyed: Boolean);
var
  Pfx: Char;
  RegSize, Index: Integer;
  AHint: string;
  ARegValue: TRegValue;
begin
  inherited;

  if FContext.Aarch32Context then
  begin
    Pfx := 'R';
    RegSize := 4;
  end
  else
  begin
    Pfx := 'X';
    RegSize := 8;
  end;

  case ARegID of
    0..28: FillReg(Pfx + IntToStr(ARegID), Trim(RegValueFmt(@FContext.X[ARegID], RegSize)), 4, 2);
    29..30:; // reserved
    31: FillReg('FP', Trim(RegValueFmt(@FContext.Fp, RegSize)), 3, 2);
    32: FillReg('LR', Trim(RegValueFmt(@FContext.Lr, RegSize)), 3, 2);
    33: FillReg('SP', Trim(RegValueFmt(@FContext.Sp, RegSize)), 3, 2);
    34: FillReg('PC', Trim(RegValueFmt(@FContext.Pc, RegSize)), 3, 2);
    35: FillReg('CPSR', Trim(RegValueFmt(@FContext.Cpsr, 4)), 5, 2);

    36..67: // Single
    begin
      Index := ARegID - 36;
      FillReg('S' + IntToStr(Index),
        RegValueFmt(@FContext.V[Index], 4), 4);
    end;

    68..99: // Double
    begin
      Index := ARegID - 68;
      FillReg('D' + IntToStr(Index),
        RegValueFmt(@FContext.V[Index], 8), 4);
    end;

    100..131: // Vector
    begin
      Index := ARegID - 100;
      FillReg('V' + IntToStr(Index),
        RegValueFmt(@FContext.V[Index], 16), 4);
    end;

    132: FillReg('FPCR', Trim(RegValueFmt(@FContext.Fpcr, 4)), 5, 2);
    133: FillReg('FPSR', Trim(RegValueFmt(@FContext.Fpsr, 4)), 5, 2);

    134: FillReg(' N', ExtractBit(FContext.Cpsr, 31), 3, 2);
    135: FillReg('Z', ExtractBit(FContext.Cpsr, 30), 2, 2);
    136: FillReg('C', ExtractBit(FContext.Cpsr, 29), 2, 2);
    137: FillReg('V', ExtractBit(FContext.Cpsr, 28), 2, 2);
    138: FillReg(' Q', ExtractBit(FContext.Cpsr, 27), 3, 2);
    139: FillReg(' GE0', ExtractBit(FContext.Cpsr, 16), 5, 2);
    140: FillReg('GE1', ExtractBit(FContext.Cpsr, 17), 4, 2);
    141: FillReg('GE2', ExtractBit(FContext.Cpsr, 18), 4, 2);
    142: FillReg('GE3', ExtractBit(FContext.Cpsr, 19), 4, 2);
    143: FillReg('E', ExtractBit(FContext.Cpsr, 9), 2, 2);
    144: FillReg('A', ExtractBit(FContext.Cpsr, 8), 2, 2);
    145: FillReg('I', ExtractBit(FContext.Cpsr, 7), 2, 2);
    146: FillReg('F', ExtractBit(FContext.Cpsr, 6), 2, 2);
    147: FillReg('J', ExtractBit(FContext.Cpsr, 24), 2, 2);
    148: FillReg('T', ExtractBit(FContext.Cpsr, 5), 2, 2);
    149: ; // FillReg(' M', ExtractModeField, 3, 2);

    152: FillReg(' AHP', ExtractBit(FContext.Fpcr, 26), 5, 2);
    153: FillReg('DN', ExtractBit(FContext.Fpcr, 25), 3, 2);
    154: FillReg('FZ', ExtractBit(FContext.Fpcr, 24), 3, 2);
    155: FillReg('FZ16', ExtractBit(FContext.Fpcr, 19), 5, 2);
    156: FillReg(' IDE', ExtractBit(FContext.Fpcr, 15), 5, 2);
    157: FillReg(' EBF', ExtractBit(FContext.Fpcr, 13), 5, 2);
    158: FillReg('IXE', ExtractBit(FContext.Fpcr, 12), 4, 2);
    159: FillReg('UFE', ExtractBit(FContext.Fpcr, 11), 4, 2);
    160: FillReg('OFE', ExtractBit(FContext.Fpcr, 10), 4, 2);
    161: FillReg('DZE', ExtractBit(FContext.Fpcr, 9), 4, 2);
    162: FillReg('IOE', ExtractBit(FContext.Fpcr, 8), 4, 2);
    163: FillReg('NEP', ExtractBit(FContext.Fpcr, 2), 4, 2);
    164: FillReg('AH', ExtractBit(FContext.Fpcr, 1), 3, 2);
    165: FillReg('FIZ', ExtractBit(FContext.Fpcr, 0), 4, 2);
    166: FillReg(' RMode', ExtractFpcrRMode, 7, 2);

    169: FillReg(' N', ExtractBit(FContext.Fpsr, 31), 3, 2);
    170: FillReg('Z', ExtractBit(FContext.Fpsr, 30), 2, 2);
    171: FillReg('C', ExtractBit(FContext.Fpsr, 29), 2, 2);
    172: FillReg('V', ExtractBit(FContext.Fpsr, 28), 2, 2);
    173: FillReg('QC', ExtractBit(FContext.Fpsr, 27), 3, 2);
    174: FillReg(' IDC', ExtractBit(FContext.Fpsr, 7), 5, 2);
    175: FillReg('IXC', ExtractBit(FContext.Fpsr, 4), 4, 2);
    176: FillReg('UFC', ExtractBit(FContext.Fpsr, 3), 4, 2);
    177: FillReg('OFC', ExtractBit(FContext.Fpsr, 2), 4, 2);
    178: FillReg('DZC', ExtractBit(FContext.Fpsr, 1), 4, 2);
    179: FillReg('IOC', ExtractBit(FContext.Fpsr, 0), 4, 2);

    180..208: // Xxx hints
    begin
      AHint := '';
      if RegQueryValue(ARegID - 180, ARegValue) then
        DoQueryRegHint(ARegValue.QwordValue, AHint);
      FillReg('', AHint, 0);
    end;

    209..212: // fp, lr, sp, pc hint
    begin
      AHint := '';
      if RegQueryValue(ARegID - 209 + 31, ARegValue) then
        DoQueryRegHint(ARegValue.QwordValue, AHint);
      FillReg('', AHint, 0);
    end;

    213: FillReg('JMP:', ExtractJmpConditions, 5);
  end;
end;

initialization

  RegisterContextSettingsClass(TAarch64CtxSettings);

end.

