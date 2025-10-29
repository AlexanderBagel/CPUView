////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.TraceLog.pas
//  * Purpose   : General log.
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

unit CpuView.TraceLog;

interface

uses
  Classes,
  SysUtils;

type

  { TTraceLog }

  TTraceLog = class
  private
    FData: TStringList;
    FChange: TNotifyEvent;
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Log(const Value: string; AddTime: Boolean = True);
    property Data: TStringList read FData;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  function TraceLog: TTraceLog;

implementation

var
  _TraceLog: TTraceLog;

function TraceLog: TTraceLog;
begin
  if _TraceLog = nil then
    _TraceLog := TTraceLog.Create;
  Result := _TraceLog;
end;

{ TTraceLog }

procedure TTraceLog.DoChange;
begin
  if Assigned(FChange) then
    FChange(Self);
end;

constructor TTraceLog.Create;
begin
  FData := TStringList.Create;
end;

destructor TTraceLog.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TTraceLog.Clear;
begin
  FData.Clear;
  DoChange;
end;

procedure TTraceLog.Log(const Value: string; AddTime: Boolean);
begin
  if AddTime then
    FData.Add(FormatDateTime('hh:mm:ss.zzz', Now) + ': ' + Value)
  else
    FData.Add(Value);
  if FData.Count > 500 then
    while FData.Count > 250 do
      FData.Delete(0);
  DoChange;
end;

initialization

  _TraceLog := TTraceLog.Create;

finalization

  FreeAndNil(_TraceLog);

end.
