////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Common.pas
//  * Purpose   : Common classes and types for CPU-View
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

unit CpuView.Common;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

type
  TAddrValidationType = (avtExecutable, avtReadable, avtStack);

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

  TRegionData = record
    AllocationBase,
    BaseAddr,
    RegionSize: Int64;
    Access: TRegionAccessSet;
  end;

  // Extended thread information (if present)
  TThreadExtendedData = record
    LastError, LastStatus: Cardinal;
  end;

  { TCommonAbstractUtils }

  TCommonAbstractUtils = class
  private
    FProcessID: Integer;
  protected
    procedure SetProcessID(const Value: Integer); virtual;
  public
    function GetThreadExtendedData(ThreadID: Integer; ThreadIs32: Boolean): TThreadExtendedData; virtual; abstract;
    function GetThreadStackLimit(ThreadID: Integer; ThreadIs32: Boolean): TStackLimit; virtual; abstract;
    function NeedUpdateReadData: Boolean; virtual;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean; virtual; abstract;
    function QueryModuleName(AddrVA: Int64; out AModuleName: string): Boolean; virtual; abstract;
    function ReadData(AddrVA: Pointer; var Buff; ASize: Longint): Longint; virtual; abstract;
    function SetThreadExtendedData(ThreadID: Integer; ThreadIs32: Boolean; const AData: TThreadExtendedData): Boolean; virtual; abstract;
    procedure Update; virtual; abstract;
    property ProcessID: Integer read FProcessID write SetProcessID;
  end;

  TCommonAbstractUtilsClass = class of TCommonAbstractUtils;

implementation

{ TCommonAbstractUtils }

function TCommonAbstractUtils.NeedUpdateReadData: Boolean;
begin
  Result := True;
end;

procedure TCommonAbstractUtils.SetProcessID(const Value: Integer);
begin
  FProcessID := Value;
end;

end.
