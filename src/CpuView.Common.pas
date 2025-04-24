////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Common.pas
//  * Purpose   : Common classes and types for CPU-View
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

unit CpuView.Common;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type
  TAddrValidationType = (avtExecutable, avtReadable, avtStack, avtString);

  TRegViewMode = (
    rvmHex, rvmHexW, rvmHexD, rvmHexQ,
    rvmOct, rvmBin, rvmIntB, rvmUIntB,
    rvmIntW, rvmIntD, rvmIntQ,
    rvmUIntW, rvmUIntD, rvmUIntQ,
    rvmFloat32, rvmFloat64, rvmFloat80);
  TRegViewModes = set of TRegViewMode;

  TStackLimit = record
    Base, Limit: Int64;
  end;

  TStackFrame = record
    AddrStack,
    AddrFrame,
    AddrPC: Int64;
  end;

  TRegionAccess = (raRead, raWrite, raExecute, raProtect);
  TRegionAccessSet = set of TRegionAccess;

  { TRegionData }

  TRegionData = record
    AllocationBase,
    BaseAddr,
    RegionSize: Int64;
    Access: TRegionAccessSet;
    function ToString: string;
  end;

  // Extended thread information (if present)
  TThreadExtendedData = record
    LastError, LastStatus: Cardinal;
  end;

  TAddrType = (atNone, atExecute, atRead, atReadLinked, atStack, atString);

  TAddrCacheItem = record
    AddrType: TAddrType;
    AddrVA: Int64;
    Region: TRegionData;
    Symbol: string;
    InDeepSymbol: string;
    InDeepCount: Integer;
    IsUserCode: Boolean;
    ExtendedDataPresent: Boolean;
    FirstAsmLine, AsmLines, HintLines: string;
    case Integer of
      0: (PointerValue: array [0..9] of Byte);
      1: (Linked: array [0..9] of Boolean);
  end;

  { TCommonAbstractUtils }

  TCommonAbstractUtils = class
  private
    FProcessID: Integer;
    FThreadID: TThreadID;
  protected
    procedure SetProcessID(const Value: Integer); virtual;
    procedure SetThreadID(AValue: TThreadID); virtual;
  public
    function GetThreadExtendedData(AThreadID: Integer; ThreadIs32: Boolean): TThreadExtendedData; virtual; abstract;
    function GetThreadStackLimit(AThreadID: Integer; ThreadIs32: Boolean): TStackLimit; virtual; abstract;
    function NeedUpdateReadData: Boolean; virtual;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean; virtual; abstract;
    function QueryModuleName(AddrVA: Int64; out AModuleName: string): Boolean; virtual; abstract;
    function ReadData(AddrVA: Pointer; var Buff; ASize: Longint): Longint; virtual; abstract;
    function SetThreadExtendedData(AThreadID: Integer; ThreadIs32: Boolean; const AData: TThreadExtendedData): Boolean; virtual; abstract;
    procedure Update; virtual; abstract;
    property ProcessID: Integer read FProcessID write SetProcessID;
    property ThreadID: TThreadID read FThreadID write SetThreadID;
  end;

  TCommonAbstractUtilsClass = class of TCommonAbstractUtils;

  IGuiImplementation = interface
    ['{5360B5C5-2AEF-4DB8-847C-27C8B3282A63}']
    procedure OpenInDisassembler(AAddrVA: Int64);
    procedure OpenInDump(AAddrVA: Int64; ANewWindow: Boolean);
  end;

implementation

{ TRegionData }

function TRegionData.ToString: string;
begin
  Result := 'No access';
  if (Self.Access <> []) and not (raProtect in Self.Access) then
  begin
    Result := '...';
    if raRead in Self.Access then
      Result[1] := 'R';
    if raWrite in Self.Access then
      Result[2] := 'W';
    if raExecute in Self.Access then
      Result[3] := 'E';
  end;
end;

{ TCommonAbstractUtils }

function TCommonAbstractUtils.NeedUpdateReadData: Boolean;
begin
  Result := True;
end;

procedure TCommonAbstractUtils.SetThreadID(AValue: TThreadID);
begin
  if ThreadID <> AValue then
  begin
    FThreadID := AValue;
    Update;
  end;
end;

procedure TCommonAbstractUtils.SetProcessID(const Value: Integer);
begin
  FProcessID := Value;
end;

end.
