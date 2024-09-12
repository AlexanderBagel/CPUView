unit CpuView.CPUContext;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  SysUtils,
  Classes,
  Generics.Collections,
  FWHexView.Common,
  CpuView.Common;

const
  // поддерживаемые виды отображения каждым типом регистра
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
    crtValue,           // обычный регистр
    crtExtra,           // специальный регистр EFlags, MSRCX etc...
    crtBitValue,        // флаг содержащий битовое значение регистра
    crtSetValue,        // флаг являющий составной частью сета
    crtSelectableHint,  // подсказка доступная для копирования пользователем (например текущие активные типы JMP)
    crtHint             // обычная "декоративная" подсказка
    );

  // Типы поддерживаемых операций с регистром при изменении
  TModifyAction = (maToggle, maIncrement, maZero, maChange);
  TModifyActions = set of TModifyAction;

  // Тип используемый при отрисовке каждого регистра в TRegView
  TRegister = record
    Modifyed: Boolean;             // флаг того что регистр был изменен (для изменения цвета)
    RegID,                         // уникальный ID регистра (для запроса TRegParam)
    RegNameSize,                   // размер имени в символах
    ValueSize,                     // размер значения в символах
    ValueSeparatorSize: Integer;   // размер разделителя в символах
    ValueType: TContextRegType;    // тип регистра
  end;

  // Расширенные настройки каждого регистра
  TRegParam = record
    RowIndex, ColIndex: Integer; // link to Map
    RegType: TContextRegType;
    Modifyed: Boolean;
    ModifyActions: TModifyActions;
    SupportedViewMode: TRegViewModes;
    ViewMode: TRegViewMode;
    function Customizable: Boolean;
    procedure Reset;
    procedure SetRowParam(RowIndex, ColIndex: Integer);
    function Valid: Boolean;
  end;

  TContextChangeType = (cctRemaped, cctDataChange);
  TContextChangeEvent = procedure(Sender: TObject;
    AChangeType: TContextChangeType) of object;
  TContextQueryRegHintEvent = procedure(Sender: TObject; AddrVA: UInt64; var AHint: string) of object;

  // Минимальный класс необходимый для работы StackView

  { TAbstractCPUContext }

  TAbstractCPUContext = class(TComponent)
  private
    FAddressMode: TAddressMode;
    FChange: TContextChangeEvent;
    FQueryHint: TContextQueryRegHintEvent;
  protected
    procedure DoQueryRegHint(AddrVA: UInt64; var AHint: string);
    procedure DoUpdate(AChangeType: TContextChangeType);
    function GetViewMode(RegID: Integer): TRegViewMode; virtual; abstract;
    procedure SetViewMode(RegID: Integer; const Value: TRegViewMode); virtual; abstract;
  public
    function Count: Integer; virtual; abstract;
    function EmptyRow(RowIndex: Integer): Boolean; virtual; abstract;
    procedure InitDefault; virtual; abstract;
    function PointerSize: Integer; virtual; abstract;
    function RegCount(RowIndex: Integer): Integer; virtual; abstract;
    function RegData(RowIndex, RegIndex: Integer; NameNeeded: Boolean): string; virtual; abstract;
    function RegInfo(RowIndex, RegIndex: Integer): TRegister; virtual; abstract;
    function RegParam(RegID: Integer; out Param: TRegParam): Boolean; virtual; abstract;
    function RegQuery(RegID: Integer; out RowIndex, RegIndex: Integer): Boolean;
    function RegQueryValue(RowIndex, RegIndex: Integer; out ARegValue: UInt64): Boolean; virtual; abstract;
    property AddressMode: TAddressMode read FAddressMode write FAddressMode;
    property ViewMode[RegID: Integer]: TRegViewMode read GetViewMode write SetViewMode;
    property OnChange: TContextChangeEvent read FChange write FChange;
    property OnQueryRegHint: TContextQueryRegHintEvent read FQueryHint write FQueryHint;
  end;

  // Служебный тип для кэширования данных последнего запрошенного регистра
  TCustomRegData = record
    Customizable: Boolean;
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

  {$message 'У контекста должно быть событие по которому Core должно отрефрешить вьювер'}
  TCommonCpuContext = class(TAbstractCPUContext)
  strict private
    FMap: TRegMap;
    FKnownRegs: TList<TRegParam>;
    FThreadID: Cardinal;
    procedure SynhronizeMapToKnownRegs;
  protected
    LastReg: TCustomRegData;
    procedure BuildMap; virtual; abstract;
    procedure DoChangeViewMode(RegID: Integer; const Value: TRegViewMode); virtual; abstract;
    function GetRegValue(RegID: Integer; out ARegValue: UInt64): Boolean; virtual; abstract;
    function GetViewMode(RegID: Integer): TRegViewMode; override;
    procedure InitKnownRegs; virtual; abstract;
    procedure SetViewMode(RegID: Integer; const Value: TRegViewMode); override;
    procedure UpdateLastRegData(RegID: Integer); virtual; abstract;
    procedure UpdateMap(RebuildRegList: Boolean = False);
    property KnownRegs: TList<TRegParam> read FKnownRegs;
    property Map: TRegMap read FMap;
  protected
    function ExtractBit(Value: DWORD; Index: Integer): string;
    function ExtractBitValue(Value: DWORD; Index: Integer): Byte;
    procedure FillReg(const RegName, Value: string; NameGlyphCount: Integer); overload;
    procedure FillReg(const RegName, Value: string;
      NameGlyphCount, ValueSeparatorSize: Integer); overload;
    procedure FillReg(const RegName, Value: string;
      NameGlyphCount, ValueGlyphCount, ValueSeparatorSize: Integer); overload;
    procedure FillSeparator;
    procedure GetRegData(RowIndex, ColIndex: Integer);
    function RegValue(Value: PByte; ValueLen: Integer;
      AValueFmt: TRegViewMode): string;
    function RegValueFmt(Value: PByte; ValueLen: Integer): string;
    procedure SetBitValue(var AField: DWORD; Index, AValue: Integer); overload;
    procedure SetBitValue(var AField: Word; Index, AValue: Integer); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Count: Integer; override;
    function EmptyRow(RowIndex: Integer): Boolean; override;
    function InstructonPoint: UInt64; virtual; abstract;
    function QueryRegValueByName(const RegName: string; out RegValue: UInt64): Boolean; virtual; abstract;
    function RegCount(RowIndex: Integer): Integer; override;
    function RegData(RowIndex, ColIndex: Integer;
      NameNeeded: Boolean): string; override;
    function RegInfo(RowIndex, ColIndex: Integer): TRegister; override;
    function RegParam(RegID: Integer; out Param: TRegParam): Boolean; override;
    function RegSetValueAtIndex(RegID, Index: Integer): string; virtual; abstract;
    function RegSetValueCount(RegID: Integer): Integer; virtual;
    function StackBase: UInt64; virtual; abstract;
    function StackPoint: UInt64; virtual; abstract;
    function Update(CurrentIP: UInt64 = 0): Boolean; virtual; abstract;
    function UpdateRegValue(RegID: Integer; ANewRegValue: UInt64): Boolean; virtual; abstract;
    property ThreadID: Cardinal read FThreadID write FThreadID;
  end;

  TCommonCpuContextClass = class of TCommonCpuContext;

implementation

{ TRegParam }

function TRegParam.Customizable: Boolean;
begin
  Result := (Self.RegType = crtValue) and (Self.SupportedViewMode <> vmDefOnly);
end;

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

{ TAbstractCPUContext }

procedure TAbstractCPUContext.DoQueryRegHint(AddrVA: UInt64; var AHint: string);
begin
  if Assigned(FQueryHint) then
    FQueryHint(Self, AddrVA, AHint);
end;

procedure TAbstractCPUContext.DoUpdate(AChangeType: TContextChangeType);
begin
  if Assigned(FChange) then
    FChange(Self, AChangeType);
end;

function TAbstractCPUContext.RegQuery(RegID: Integer; out RowIndex,
  RegIndex: Integer): Boolean;
var
  Param: TRegParam;
begin
  Result := RegParam(RegID, Param);
  if Result then
  begin
    RowIndex := Param.RowIndex;
    RegIndex := Param.ColIndex;
  end;
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
  FKnownRegs := TList<TRegParam>.Create;
  UpdateMap(True);
end;

destructor TCommonCpuContext.Destroy;
begin
  FKnownRegs.Free;
  FMap.Free;
  inherited;
end;

function TCommonCpuContext.EmptyRow(RowIndex: Integer): Boolean;
var
  ARegID: Integer;
begin
  if (RowIndex >= 0) and (RowIndex < Map.Count) then
  begin
    ARegID := Map[RowIndex].RegID;
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

procedure TCommonCpuContext.FillReg(const RegName, Value: string;
  NameGlyphCount: Integer);
begin
  FillReg(RegName, Value, NameGlyphCount, Length(Value), 0);
end;

procedure TCommonCpuContext.FillSeparator;
begin
  FillReg('', '', 0, 0);
end;

procedure TCommonCpuContext.GetRegData(RowIndex, ColIndex: Integer);
var
  RegID: Integer;
  RegParam: TRegParam;
begin
  if not Map.CheckRegIndex(RowIndex, ColIndex) then
    Exit;

  RegID := Map[RowIndex][ColIndex].RegID;

  // check cache
  if LastReg.RegID = RegID then Exit;

  // real fill reg data
  LastReg.RegID := RegID;

  RegParam := KnownRegs[RegID];
  LastReg.Customizable := RegParam.Customizable;
  LastReg.RegType := RegParam.RegType;
  LastReg.Modifyed := RegParam.Modifyed;

  // update
  UpdateLastRegData(RegID);
end;

function TCommonCpuContext.GetViewMode(RegID: Integer): TRegViewMode;
begin
  Result := KnownRegs[RegID].ViewMode;
end;

function TCommonCpuContext.RegCount(RowIndex: Integer): Integer;
begin
  Result := Map[RowIndex].Count;
end;

function TCommonCpuContext.RegData(RowIndex, ColIndex: Integer;
  NameNeeded: Boolean): string;
begin
  GetRegData(RowIndex, ColIndex);
  if NameNeeded then
    Result := LastReg.RegName
  else
    Result := LastReg.Value;
end;

function TCommonCpuContext.RegInfo(RowIndex, ColIndex: Integer): TRegister;
begin
  GetRegData(RowIndex, ColIndex);
  Result.Modifyed := LastReg.Modifyed;
  Result.RegID := LastReg.RegID;
  Result.RegNameSize := LastReg.NameGlyphCount;
  Result.ValueSize := LastReg.ValueGlyphCount;
  Result.ValueSeparatorSize := LastReg.ValueSeparatorSize;
  Result.ValueType := LastReg.RegType;
end;

function TCommonCpuContext.RegParam(RegID: Integer;
  out Param: TRegParam): Boolean;
begin
  Result := (RegID >= 0) and (RegID < KnownRegs.Count);
  if Result then
    Param := KnownRegs[RegID];
end;

function TCommonCpuContext.RegSetValueCount(RegID: Integer): Integer;
begin
  Result := 0;
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
      {$message 'Не реализовано'}
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
    DoUpdate(cctRemaped);
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
  DoUpdate(cctRemaped);
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
