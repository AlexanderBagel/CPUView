////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.ScriptExecutor.pas
//  * Purpose   : Inerface of abstract script executor.
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

unit CpuView.ScriptExecutor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I CpuViewCfg.inc}

uses
  SysUtils,
  Generics.Collections,
  FWHexView.Common,
  CpuView.CPUContext,
  CpuView.DebugerGate;

type
  TExpressionType = (etMem, etReg, etRip, etImm, etSizePfx, etSimdX87);
  TExpressionTypes = set of TExpressionType;
  TExpression = record
    Data: string;
    Value, MemValue: Int64;
    MemSize: Integer;
    Calculated: Boolean;
    Types: TExpressionTypes;
    SimdX87: array [0..31] of Byte;
  end;

  TExpressionList = class(TListEx<TExpression>);

  { TAbstractScriptExecutor }

  TAbstractScriptExecutor = class
  private
    FCalculatedList: TExpressionList;
    FCalculatedValue: Int64;
    FContext: TCommonCpuContext;
    FCurrentRIPOffset: Int64;
    FDebugger: TAbstractDebugger;
    function BreakPointPresent(AddrVA: Int64): Boolean;
    function ConvertToLibName(const AValue: string): string;
    function ExecuteGetModuleHandle(const Script: string; out ExecuteResult: string): Boolean;
    function ExecuteGetProcAddress(const Script: string; out ExecuteResult: string): Boolean;
    function ExecuteBreakPointSet(const Script: string; out ExecuteResult: string): Boolean;
    function ExecuteBreakPointClear(const Script: string; out ExecuteResult: string): Boolean;
    procedure ExtractParams(const Script: string; out FirstParam, SecondParam: string);
    function InternalGetProcAddress(const Script: string; out ExecuteResult: string; out AddrVA: Int64): Boolean;
  protected
    function DoExecute(const Script: string; out ExecuteResult: string): Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute(const Script: string; out ExecuteResult: string): Boolean;
    property CalculatedList: TExpressionList read FCalculatedList;
    property CalculatedValue: Int64 read FCalculatedValue write FCalculatedValue;
    property Context: TCommonCpuContext read FContext write FContext;
    property CurrentRIPOffset: Int64 read FCurrentRIPOffset write FCurrentRIPOffset;
    property Debugger: TAbstractDebugger read FDebugger write FDebugger;
  end;

implementation

function IndexOfString(const s: string; Values: array of string): Integer;
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
    if AnsiCompareText(s, Values[I]) = 0 then
      Exit(I);
  Result := -1;
end;

{ TAbstractScriptExecutor }

function TAbstractScriptExecutor.BreakPointPresent(AddrVA: Int64): Boolean;
var
  I: Integer;
begin
  for I := 0 to Debugger.BreakPointList.Count - 1 do
    if Debugger.BreakPointList.List[I].AddrVA = AddrVA then
      Exit(True);
  Result := False;
end;

function TAbstractScriptExecutor.ConvertToLibName(const AValue: string): string;
var
  ValidSuffix: string;
begin
  ValidSuffix := '.' + SharedSuffix;
  if SameText(ExtractFileExt(AValue), ValidSuffix) then
    Result := AValue
  else
    Result := AValue + ValidSuffix;
end;

function TAbstractScriptExecutor.ExecuteGetModuleHandle(const Script: string;
  out ExecuteResult: string): Boolean;
var
  LibName, Dummee: string;
  RemoteModule: TRemoteModule;
begin
  Result := False;
  ExtractParams(Script, LibName, Dummee);
  LibName := ConvertToLibName(LibName);
  RemoteModule := FDebugger.GetRemoteModuleHandle(LibName);
  if RemoteModule.hInstance = 0 then
    ExecuteResult := Format('"%s" not found.', [LibName])
  else
  begin
    ExecuteResult := Format('"%s" instance %x. Path: %s', [LibName, RemoteModule.ImageBase, RemoteModule.LibraryPath]);
    CalculatedValue := RemoteModule.ImageBase;
    Result := True;
  end;
end;

function TAbstractScriptExecutor.ExecuteGetProcAddress(const Script: string;
  out ExecuteResult: string): Boolean;
var
  AddrVA: Int64;
begin
  Result := InternalGetProcAddress(Script, ExecuteResult, AddrVA);
end;

function TAbstractScriptExecutor.ExecuteBreakPointSet(const Script: string; out
  ExecuteResult: string): Boolean;
var
  AddrVA: Int64;
begin
  Result := InternalGetProcAddress(Script, ExecuteResult, AddrVA);
  if Result then
  begin
    Result := not BreakPointPresent(AddrVA);
    if Result then
    begin
      Debugger.ToggleBreakPoint(AddrVA);
      ExecuteResult := ExecuteResult + ' breakpoint set';
    end
    else
      ExecuteResult := ExecuteResult + ' breakpoint is already set';
  end;
end;

function TAbstractScriptExecutor.ExecuteBreakPointClear(const Script: string;
  out ExecuteResult: string): Boolean;
var
  AddrVA: Int64;
begin
  Result := InternalGetProcAddress(Script, ExecuteResult, AddrVA);
  if Result then
  begin
    Result := BreakPointPresent(AddrVA);
    if Result then
    begin
      Debugger.ToggleBreakPoint(AddrVA);
      ExecuteResult := ExecuteResult + ' breakpoint remove';
    end
    else
      ExecuteResult := ExecuteResult + ' breakpoint not found';
  end;
end;

procedure TAbstractScriptExecutor.ExtractParams(const Script: string; out
  FirstParam, SecondParam: string);
var
  Idx1, Idx2: Integer;
begin
  FirstParam := '';
  SecondParam := '';
  Idx1 := Pos(' ', Script);
  Idx2 := Pos(':', Script);
  if Idx1 = 0 then Exit;
  if Idx2 = 0 then
    FirstParam := Copy(Script, Idx1 + 1, Length(Script))
  else
  begin
    FirstParam := Copy(Script, Idx1 + 1, Idx2 - Idx1 - 1);
    SecondParam:= Copy(Script, Idx2 + 1, Length(Script));
  end;
end;

function TAbstractScriptExecutor.InternalGetProcAddress(const Script: string;
  out ExecuteResult: string; out AddrVA: Int64): Boolean;
var
  LibName, LibNameWithExt, ProcName: string;
  RemoteModule: TRemoteModule;
begin
  Result := False;
  ExtractParams(Script, LibName, ProcName);
  LibNameWithExt := ConvertToLibName(LibName);
  RemoteModule := FDebugger.GetRemoteModuleHandle(LibNameWithExt);
  if RemoteModule.hInstance = 0 then
    ExecuteResult := Format('"%s" not found.', [LibNameWithExt])
  else
  begin
    AddrVA := FDebugger.GetRemoteProcAddress(RemoteModule.hInstance, ProcName);
    if AddrVA = 0 then
      ExecuteResult := Format('"%s:%s" not found.', [LibName, ProcName])
    else
    begin
      ExecuteResult := Format('"%s:%s" address: %x', [LibName, ProcName, AddrVA]);
      CalculatedValue := AddrVA;
      Result := True;
    end;
  end;
end;

constructor TAbstractScriptExecutor.Create;
begin
  FCalculatedList := TExpressionList.Create;
end;

destructor TAbstractScriptExecutor.Destroy;
begin
  FCalculatedList.Free;
  inherited;
end;

function TAbstractScriptExecutor.Execute(const Script: string;
  out ExecuteResult: string): Boolean;
var
  Command: string;
begin
  CalculatedValue := 0;
  try
    CalculatedList.Clear;
    Command := Copy(Script, 1, Pos(' ', Script) - 1);
    case IndexOfString(Command, ['gmh', 'getmodulehandle',
      'gpa', 'getprocaddress', 'bp', 'bc']) of
      0, 1: Result := ExecuteGetModuleHandle(Script, ExecuteResult);
      2, 3: Result := ExecuteGetProcAddress(Script, ExecuteResult);
      4: Result := ExecuteBreakPointSet(Script, ExecuteResult);
      5: Result := ExecuteBreakPointClear(Script, ExecuteResult);
    else
      Result := DoExecute(Script, ExecuteResult);
    end;
  except
    on E: Exception do
    begin
      ExecuteResult := E.ClassName + ' at parsing "' + Script + '"' +
        sLineBreak + E.Message;
      Result := False;
    end;
  end;
  if (ExecuteResult = '') and not Result then
    ExecuteResult := 'Unknown command/expression "' + Script + '"';
end;

end.
