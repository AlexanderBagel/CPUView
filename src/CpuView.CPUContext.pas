unit CpuView.CPUContext;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows, XMLIntf,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils,
  Classes,
  Generics.Collections,
  FWHexView.Common,
  CpuView.Common,
  CpuView.XML;

const
  // supported display types by each register type
  vmDefOnly: TRegViewModes = [rvmHex];
  vmReg8: TRegViewModes = [rvmHex, rvmOct, rvmBin, rvmIntB, rvmUIntB];
  vmReg16: TRegViewModes = [rvmHex, rvmOct, rvmBin, rvmIntW, rvmUIntW];
  vmReg32: TRegViewModes = [rvmHex, rvmOct, rvmBin, rvmIntD, rvmUIntD];
  vmReg64: TRegViewModes = [rvmHex, rvmOct, rvmBin, rvmIntQ, rvmUIntQ];
  vmX87Reg64: TRegViewModes = [rvmHex, rvmHexW, rvmFloat32, rvmFloat64];
  vmX87Reg80: TRegViewModes = [rvmHex, rvmHexW];
  vmSimdReg: TRegViewModes = [rvmHex..rvmHexQ, rvmIntW..rvmFloat64];

type
  TContextRegType = (
    crtValue,           // ordinary register
    crtExtra,           // special register EFlags, MSRCX etc...
    crtBitValue,        // flag containing the bit value of the register
    crtEnumValue,       // register part of several bits (enum)
    crtSelectableHint,  // hint available for user copying (e.g. current active JMP types)
    crtHint             // the usual “decorative” hint
    );

  // Types of supported operations with the register during a change
  TModifyAction = (maToggle, maIncrement, maZero, maChange);
  TModifyActions = set of TModifyAction;

  TRegID = Integer;

  TRegDescriptor = record
    RegID: TRegID;                 // link to KnownRegs
    RowIndex, RegIndex: Integer;   // link to Map
  end;

  // Type used when drawing each register in TRegView
  TRegister = record
    Modifyed: Boolean;             // flag that the register has been changed (to change the color)
    RegID,                         // unique register ID (for TRegParam request)
    RegNameSize,                   // name size in characters
    ValueSize,                     // value size in characters
    ValueSeparatorSize: Integer;   // delimiter size in characters
    ValueType: TContextRegType;    // register type
  end;

  // Advanced settings for each register
  TRegParam = record
    RowIndex, ColIndex: Integer; // link to Map
    RegType: TContextRegType;
    Modifyed: Boolean;
    ModifyActions: TModifyActions;
    SupportedViewMode: TRegViewModes;
    ViewMode: TRegViewMode;
    procedure Reset;
    procedure SetRowParam(RowIndex, ColIndex: Integer);
    function Valid: Boolean;
  end;

  TContextChangeType = (cctRemaped, cctContextUpdated, cctDataChange);
  TContextChangeEvent = procedure(Sender: TObject;
    AChangeType: TContextChangeType) of object;
  TContextQueryRegHintEvent = procedure(Sender: TObject; AddrVA: UInt64;
    AColumnType: TColumnType; var AHint: string) of object;

  TRegValue = record
    ValueSize: Integer;
    case Integer of
      1: (ByteValue: Byte);
      2: (WordValue: Word);
      3: (IntValue: Integer);
      4: (DwordValue: Cardinal);
      8: (QwordValue: UInt64);
      10: (Ext10: array [0..9] of Byte);
      16: (Ext16: array [0..15] of Byte);
      32: (Ext32: array [0..31] of Byte);
  end;

  TExternalRegType = (ertLastError, ertLastStatus);
  TContextQueryExternalRegHintEvent = procedure(Sender: TObject;
    const AValue: TRegValue; ARegType: TExternalRegType; var AHint: string) of object;

  TRegQueryStringType = (rqstName, rqstValue);

  TAbstractCPUContext = class;

  { TRegAbstractSettings }

  { TContextAbstractSettings }

  TContextAbstractSettings = class
  protected
    function GetContextName: string; virtual; abstract;
    procedure InternalLoadFromXML(Root: IXMLNode); virtual; abstract;
    procedure InternalSaveToXML(Root: IXMLNode); virtual; abstract;
  public
    constructor Create; virtual;
    procedure InitDefault; virtual; abstract;
    procedure LoadFromContext(ACtx: TAbstractCPUContext); virtual; abstract;
    procedure SaveToContext(ACtx: TAbstractCPUContext); virtual; abstract;
    procedure LoadFromXML(Root: IXMLNode);
    procedure SaveToXML(Root: IXMLNode);
  end;

  TContextSettingsClass = class of TContextAbstractSettings;

  { TAbstractCPUContext }

  // Minimum class with which TRegView works

  TAbstractCPUContext = class(TComponent)
  private
    FAddressMode: TAddressMode;
    FChangeList: TList<TContextChangeEvent>;
    FQueryHint: TContextQueryRegHintEvent;
    FQueryExternalHint: TContextQueryExternalRegHintEvent;
    FUpdateCount: Integer;
    FUtils: TCommonAbstractUtils;
  protected
    procedure DoChange(AChangeType: TContextChangeType);
    procedure DoQueryRegHint(AddrVA: UInt64; var AHint: string);
    procedure DoQueryExternalRegHint(const AValue: TRegValue; ARegType: TExternalRegType; var AHint: string);
    function GetViewMode(RegID: TRegID): TRegViewMode; virtual; abstract;
    procedure SetViewMode(RegID: TRegID; const Value: TRegViewMode); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    function Count: Integer; virtual; abstract;
    function EmptyRow(ARowIndex: Integer): Boolean; virtual; abstract;
    procedure EndUpdate;
    procedure InitDefault; virtual; abstract;
    function InstructonPoint: UInt64; virtual; abstract;
    function InstructonPointID: TRegID; virtual; abstract;
    function IsActiveJump(const Value: string): Boolean; virtual; abstract;
    function PointerSize: Integer; virtual; abstract;
    function RegCount(ARowIndex: Integer): Integer; virtual; abstract;
    function RegDescriptor(ARegID: TRegID; out ADescriptor: TRegDescriptor): Boolean; overload;
    function RegDescriptor(const ARegName: string; out ADescriptor: TRegDescriptor): Boolean; overload; virtual; abstract;
    procedure RegisterChangeNotification(Value: TContextChangeEvent);
    function RegInfo(ARegID: TRegID): TRegister; overload; virtual; abstract;
    function RegInfo(ARowIndex, ARegIndex: Integer): TRegister; overload; virtual; abstract;
    function RegParam(ARegID: TRegID; out AParam: TRegParam): Boolean; virtual; abstract;
    function RegQueryEnumString(ARegID: TRegID; AEnumIndex: Integer): string; virtual; abstract;
    function RegQueryEnumValuesCount(ARegID: TRegID): Integer; virtual; abstract;
    function RegQueryNamesAtAddr(AAddrVA: UInt64): string; virtual; abstract;
    function RegQueryString(ARegID: TRegID; AType: TRegQueryStringType): string; virtual; abstract;
    function RegQueryValue(ARegID: TRegID; out ARegValue: TRegValue): Boolean; overload; virtual; abstract;
    function RegQueryValue(const ARegName: string; out ARegValue: TRegValue): Boolean; overload; virtual; abstract;
    function RegSetValue(ARegID: TRegID; const ANewRegValue: TRegValue): Boolean; virtual; abstract;
    procedure UnRegisterChangeNotification(Value: TContextChangeEvent);
    function Update(ANewInstructionPoint: UInt64 = 0): Boolean; virtual; abstract;
  public
    property AddressMode: TAddressMode read FAddressMode write FAddressMode;
    property Utils: TCommonAbstractUtils read FUtils write FUtils;
    property ViewMode[RegID: TRegID]: TRegViewMode read GetViewMode write SetViewMode;
    property OnQueryExternalHint: TContextQueryExternalRegHintEvent read FQueryExternalHint write FQueryExternalHint;
    property OnQueryRegHint: TContextQueryRegHintEvent read FQueryHint write FQueryHint;
  end;

  // Service type for caching data of the last requested register
  TCustomRegData = record
    RegID: Integer; // link to KnownRegs
    RegType: TContextRegType;
    RegName, Value: string;
    Modifyed: Boolean;
    NameGlyphCount,
    ValueGlyphCount,
    ValueSeparatorSize: Integer;
  end;

  // так как под каждый тип процессора набор регистров будет разный
  // для хранения карты отображения регистров вводится промежуточный
  // класс, где каждая реализация контекста назначит свой уникальный
  // идентификатор каждому регистру, который она может обработать,

  // Since for each type of processor the set of registers will be different,
  // an intermediate class is introduced to store the register mapping map,
  // where each context implementation will assign a unique identifier
  // to each register it can process
  TRegMap = class
  strict private
    FRoot: Boolean;
    FItems: TObjectList<TRegMap>;
    FRegID: Integer;
  private
    function GetItem(Index: Integer): TRegMap;
  public
    constructor Create(Root: Boolean);
    destructor Destroy; override;
    function Add: TRegMap;
    function CheckRegIndex(ARow, AColumn: Integer): Boolean;
    procedure Clear;
    function Count: Integer;
    property Items[Index: Integer]: TRegMap read GetItem; default;
    property RegID: Integer read FRegID write FRegID; // link to KnownRegs
  end;

  TCommonCpuContext = class(TAbstractCPUContext)
  strict private
    FMap: TRegMap;
    FKnownRegs: TListEx<TRegParam>;
    FThreadID: Cardinal;
    procedure SynhronizeMapToKnownRegs;
  protected
    LastReg: TCustomRegData;
    procedure BuildMap; virtual; abstract;
    procedure DoChangeViewMode(RegID: Integer; const Value: TRegViewMode); virtual; abstract;
    function GetViewMode(RegID: Integer): TRegViewMode; override;
    procedure InitKnownRegs; virtual; abstract;
    procedure SetViewMode(RegID: Integer; const Value: TRegViewMode); override;
    procedure UpdateLastRegData(RegID: Integer); virtual; abstract;
    procedure UpdateMap(RebuildRegList: Boolean = False);
    property KnownRegs: TListEx<TRegParam> read FKnownRegs;
    property Map: TRegMap read FMap;
  protected
    function ExtractBit(Value: DWORD; Index: Integer): string;
    function ExtractBitValue(Value: DWORD; Index: Integer): Byte;
    procedure FillReg(const RegName, Value: string; NameGlyphCount: Integer); overload;
    procedure FillReg(const RegName, Value: string;
      NameGlyphCount, ValueSeparatorSize: Integer); overload;
    procedure FillReg(const RegName, Value: string;
      NameGlyphCount, ValueGlyphCount, ValueSeparatorSize: Integer); overload;
    procedure FillRegData(RegID: TRegID); overload;
    procedure FillRegData(RowIndex, ColIndex: Integer); overload;
    procedure FillSeparator;
    function RegValue(Value: PByte; ValueLen: Integer;
      AValueFmt: TRegViewMode): string;
    function RegValueFmt(Value: PByte; ValueLen: Integer): string;
    procedure SetBitValue(var AField: DWORD; Index, AValue: Integer); overload;
    procedure SetBitValue(var AField: Word; Index, AValue: Integer); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Count: Integer; override;
    function EmptyRow(ARowIndex: Integer): Boolean; override;
    function RegCount(ARowIndex: Integer): Integer; override;
    function RegInfo(ARegID: TRegID): TRegister; overload; override;
    function RegInfo(ARowIndex, AColIndex: Integer): TRegister; overload; override;
    function RegParam(ARegID: TRegID; out AParam: TRegParam): Boolean; override;
    function RegQueryString(ARegID: TRegID; AType: TRegQueryStringType): string; override;
    function StackBase: UInt64; virtual; abstract;
    function StackPoint: UInt64; virtual; abstract;
    property ThreadID: Cardinal read FThreadID write FThreadID;
  end;

  TCommonCpuContextClass = class of TCommonCpuContext;

  procedure RegisterContextSettingsClass(AValue: TContextSettingsClass);
  function GetContextSettingsClass: TContextSettingsClass;

implementation

var
  _ContextSettingsClass: TContextSettingsClass;

procedure RegisterContextSettingsClass(AValue: TContextSettingsClass);
begin
  _ContextSettingsClass := AValue;
end;

function GetContextSettingsClass: TContextSettingsClass;
begin
  Result := _ContextSettingsClass;
end;

{ TRegParam }

procedure TRegParam.Reset;
begin
  Self.RowIndex := -1;
  Self.ColIndex := -1;
end;

procedure TRegParam.SetRowParam(RowIndex, ColIndex: Integer);
begin
  Self.RowIndex := RowIndex;
  Self.ColIndex := ColIndex;
end;

function TRegParam.Valid: Boolean;
begin
  Result := Self.ColIndex > -1;
end;

{ TContextAbstractSettings }

constructor TContextAbstractSettings.Create;
begin
  // do nothing...
end;

procedure TContextAbstractSettings.LoadFromXML(Root: IXMLNode);
var
  Node: IXMLNode;
begin
  Node := FindNode(Root, GetContextName);
  if Assigned(Node) then
    InternalLoadFromXML(Node);
end;

procedure TContextAbstractSettings.SaveToXML(Root: IXMLNode);
begin
  InternalSaveToXML(NewChild(Root, GetContextName));
end;

{ TAbstractCPUContext }

procedure TAbstractCPUContext.DoChange(AChangeType: TContextChangeType);
var
  I: Integer;
begin
  if FUpdateCount <> 0 then Exit;
  for I := 0 to FChangeList.Count - 1 do
    FChangeList[I](Self, AChangeType);
end;

procedure TAbstractCPUContext.DoQueryRegHint(AddrVA: UInt64; var AHint: string);
begin
  if Assigned(FQueryHint) then
    FQueryHint(Self, AddrVA, ctComment, AHint);
end;

procedure TAbstractCPUContext.DoQueryExternalRegHint(const AValue: TRegValue;
  ARegType: TExternalRegType; var AHint: string);
begin
  if Assigned(FQueryExternalHint) then
    FQueryExternalHint(Self, AValue, ARegType, AHint);
end;

constructor TAbstractCPUContext.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChangeList := TList<TContextChangeEvent>.Create;
end;

destructor TAbstractCPUContext.Destroy;
begin
  FreeAndNil(FChangeList);
  inherited Destroy;
end;

procedure TAbstractCPUContext.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAbstractCPUContext.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoChange(cctDataChange);
end;

function TAbstractCPUContext.RegDescriptor(ARegID: TRegID;
  out ADescriptor: TRegDescriptor): Boolean;
var
  Param: TRegParam;
begin
  ADescriptor := Default(TRegDescriptor);
  Result := RegParam(ARegID, Param);
  if Result then
  begin
    ADescriptor.RegID := ARegID;
    ADescriptor.RowIndex := Param.RowIndex;
    ADescriptor.RegIndex := Param.ColIndex;
  end;
end;

procedure TAbstractCPUContext.RegisterChangeNotification(
  Value: TContextChangeEvent);
begin
  FChangeList.Add(Value);
end;

procedure TAbstractCPUContext.UnRegisterChangeNotification(
  Value: TContextChangeEvent);
var
  Index: Integer;
begin
  if FChangeList = nil then Exit;
  Index := FChangeList.IndexOf(Value);
  if Index >= 0 then
    FChangeList.Delete(Index);
end;

{ TCommonCpuContext }

function TCommonCpuContext.Count: Integer;
begin
  Result := Map.Count;
end;

constructor TCommonCpuContext.Create(AOwner: TComponent);
begin
  inherited;
  LastReg.RegID := -1;
  FMap := TRegMap.Create(True);
  FKnownRegs := TListEx<TRegParam>.Create;
  UpdateMap(True);
end;

destructor TCommonCpuContext.Destroy;
begin
  FKnownRegs.Free;
  FMap.Free;
  inherited;
end;

function TCommonCpuContext.EmptyRow(ARowIndex: Integer): Boolean;
var
  ARegID: Integer;
begin
  if (ARowIndex >= 0) and (ARowIndex < Map.Count) then
  begin
    ARegID := Map[ARowIndex].RegID;
    Result := ARegID < 0;
    if not Result then
      Result := KnownRegs[ARegID].RegType = crtHint;
  end
  else
    Result := True;
end;

function TCommonCpuContext.ExtractBit(Value: DWORD; Index: Integer): string;
const
  SimpleBoolStrs: array [Boolean] of string = ('0', '1');
begin
  Result := SimpleBoolStrs[ExtractBitValue(Value, Index) = 1];
end;

function TCommonCpuContext.ExtractBitValue(Value: DWORD; Index: Integer): Byte;
begin
  Result := (Value shr Index) and 1;
end;

procedure TCommonCpuContext.FillReg(const RegName, Value: string;
  NameGlyphCount, ValueSeparatorSize: Integer);
begin
  FillReg(RegName, Value, NameGlyphCount, Length(Value), ValueSeparatorSize);
end;

procedure TCommonCpuContext.FillReg(const RegName, Value: string;
  NameGlyphCount, ValueGlyphCount, ValueSeparatorSize: Integer);
begin
  LastReg.RegName := RegName;
  LastReg.Value := Value;
  LastReg.NameGlyphCount := NameGlyphCount;
  LastReg.ValueGlyphCount := ValueGlyphCount;
  LastReg.ValueSeparatorSize := ValueSeparatorSize;
end;

procedure TCommonCpuContext.FillRegData(RowIndex, ColIndex: Integer);
begin
  if Map.CheckRegIndex(RowIndex, ColIndex) then
    FillRegData(Map[RowIndex][ColIndex].RegID);
end;

procedure TCommonCpuContext.FillRegData(RegID: TRegID);
var
  RegParam: TRegParam;
begin
  // check cache
  if LastReg.RegID = RegID then Exit;

  if (RegID < 0) or (RegID >= KnownRegs.Count) then Exit;

  // real fill reg data
  LastReg.RegID := RegID;

  RegParam := KnownRegs[RegID];
  LastReg.RegType := RegParam.RegType;
  LastReg.Modifyed := RegParam.Modifyed;

  // update
  UpdateLastRegData(RegID);
end;

procedure TCommonCpuContext.FillReg(const RegName, Value: string;
  NameGlyphCount: Integer);
begin
  FillReg(RegName, Value, NameGlyphCount, Length(Value), 0);
end;

procedure TCommonCpuContext.FillSeparator;
begin
  FillReg('', '', 0, 0);
end;

function TCommonCpuContext.GetViewMode(RegID: Integer): TRegViewMode;
begin
  Result := KnownRegs[RegID].ViewMode;
end;

function TCommonCpuContext.RegCount(ARowIndex: Integer): Integer;
begin
  Result := Map[ARowIndex].Count;
end;

function TCommonCpuContext.RegInfo(ARegID: TRegID): TRegister;
begin
  FillRegData(ARegID);
  Result.Modifyed := LastReg.Modifyed;
  Result.RegID := LastReg.RegID;
  Result.RegNameSize := LastReg.NameGlyphCount;
  Result.ValueSize := LastReg.ValueGlyphCount;
  Result.ValueSeparatorSize := LastReg.ValueSeparatorSize;
  Result.ValueType := LastReg.RegType;
end;

function TCommonCpuContext.RegInfo(ARowIndex, AColIndex: Integer): TRegister;
begin
  if Map.CheckRegIndex(ARowIndex, AColIndex) then
    Result := RegInfo(Map[ARowIndex][AColIndex].RegID)
  else
    Result := Default(TRegister);
end;

function TCommonCpuContext.RegParam(ARegID: TRegID;
  out AParam: TRegParam): Boolean;
begin
  Result := (ARegID >= 0) and (ARegID < KnownRegs.Count);
  if Result then
    AParam := KnownRegs[ARegID];
end;

function TCommonCpuContext.RegQueryString(ARegID: TRegID;
  AType: TRegQueryStringType): string;
begin
  FillRegData(ARegID);
  case AType of
    rqstName: Result := LastReg.RegName;
    rqstValue: Result := LastReg.Value;
  else
    Result := '';
  end;
end;

function TCommonCpuContext.RegValue(Value: PByte; ValueLen: Integer;
  AValueFmt: TRegViewMode): string;
begin
  Result := '';
  case AValueFmt of
    rvmHex:
      Result := RawBufToHex(Value, ValueLen, True);
    rvmHexW:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmHex16), bvmHex16, RegFormatMode);
    rvmHexD:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmHex32), bvmHex32, RegFormatMode);
    rvmHexQ:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmHex64), bvmHex64, RegFormatMode);
      {$message 'Not yet implemented'}
    rvmOct: ;
    rvmBin:;
    rvmIntW:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmInt16), bvmInt16, RegFormatMode);
    rvmIntD:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmInt32), bvmInt32, RegFormatMode);
    rvmIntQ:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmInt64), bvmInt64, RegFormatMode);
    rvmUIntW:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmUInt16), bvmUInt16, RegFormatMode);
    rvmUIntD:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmUInt32), bvmUInt32, RegFormatMode);
    rvmUIntQ:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmUInt64), bvmUInt64, RegFormatMode);
    rvmFloat32:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmFloat32), bvmFloat32, RegFormatMode);
    rvmFloat64:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmFloat64), bvmFloat64, RegFormatMode);
    rvmFloat80:
      Result := RawBufToViewMode(Value, ValueLen, DefValueMetric(bvmFloat80), bvmFloat80, RegFormatMode);
  end;
end;

function TCommonCpuContext.RegValueFmt(Value: PByte; ValueLen: Integer): string;
begin
  Result := RegValue(Value, ValueLen, KnownRegs.List[LastReg.RegID].ViewMode);
end;

procedure TCommonCpuContext.SetBitValue(var AField: DWORD; Index,
  AValue: Integer);
var
  Mask: Integer;
begin
  AValue := (AValue and 1) shl Index;
  Mask := not (1 shl Index);
  AField := (AField and Mask) or DWORD(AValue);
end;

procedure TCommonCpuContext.SetBitValue(var AField: Word; Index,
  AValue: Integer);
var
  Mask: Integer;
begin
  AValue := (AValue and 1) shl Index;
  Mask := not (1 shl Index);
  AField := (AField and Mask) or AValue;
end;

procedure TCommonCpuContext.SetViewMode(RegID: Integer;
  const Value: TRegViewMode);
begin
  if Value in KnownRegs.List[RegID].SupportedViewMode then
  begin
    DoChangeViewMode(RegID, Value);
    DoChange(cctRemaped);
  end;
end;

procedure TCommonCpuContext.SynhronizeMapToKnownRegs;
var
  I, Row, Field: Integer;
begin
  for I := 0 to KnownRegs.Count - 1 do
    KnownRegs.List[I].Reset;
  for Row := 0 to Map.Count - 1 do
    for Field := 0 to Map[Row].Count - 1 do
    begin
      I := Map[Row][Field].RegID;
      if I >= 0 then
        KnownRegs.List[I].SetRowParam(Row, Field);
    end;
end;

procedure TCommonCpuContext.UpdateMap(RebuildRegList: Boolean);
begin
  if RebuildRegList then
    InitKnownRegs;
  BuildMap;
  SynhronizeMapToKnownRegs;
  DoChange(cctRemaped);
end;

{ TRegMap }

function TRegMap.Add: TRegMap;
begin
  if FItems = nil then
    FItems := TObjectList<TRegMap>.Create;
  Result := TRegMap.Create(False);
  FItems.Add(Result);
end;

function TRegMap.CheckRegIndex(ARow, AColumn: Integer): Boolean;
begin
  Result := False;
  if (ARow < 0) or (ARow >= Count) then Exit;
  if (AColumn < 0) or (AColumn >= Items[ARow].Count) then Exit;
  Result := True;
end;

procedure TRegMap.Clear;
begin
  FreeAndNil(FItems);
end;

function TRegMap.Count: Integer;
begin
  if RegID < 0 then Exit(0);
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
  if not FRoot then
    Inc(Result); // for self in GetItem()
end;

constructor TRegMap.Create(Root: Boolean);
begin
  FRoot := Root;
end;

destructor TRegMap.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TRegMap.GetItem(Index: Integer): TRegMap;
begin
  if FRoot then Exit(FItems[Index]);
  if Index = 0 then
    Result := Self
  else
    Result := FItems[Index - 1];
end;

end.
