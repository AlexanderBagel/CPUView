////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Design.CrashDump.pas
//  * Purpose   : Exception interceptor for Lazarus.
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

unit CpuView.Design.CrashDump;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Forms;

type

  { TExceptionLogger }

  TExceptionLogger = class
  private
    FEnabled: Boolean;
    FOldAppNoExceptionMessagesPresent: Boolean;
    FOldExceptionEvent: TExceptionEvent;
    procedure OnExceptionHandler(Sender: TObject; AException: Exception);
    procedure SetEnabled(AValue: Boolean);
  public
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

implementation

uses
  CpuView.Design.DbgLog;

{ TExceptionLogger }

procedure TExceptionLogger.OnExceptionHandler(Sender: TObject;
  AException: Exception);
var
  Dump: string;

  function DumpAddr(Addr: Pointer): string;
  begin
    try
      Result := BackTraceStrFunc(Addr);
    except
      Result := SysBackTraceStr(Addr);
    end;
    CpuViewDebugLog.Log(Result);
  end;

var
  FrameCount, FrameNumber: Integer;
  Frames: PPointer;
begin
  CpuViewDebugLog.Log('================== CRASH DUMP START ==================');
  Dump := DumpAddr(ExceptAddr);
  FrameCount := ExceptFrameCount;
  Frames := ExceptFrames;
  for FrameNumber := 0 to FrameCount - 1 do
    Dump := Dump + sLineBreak + DumpAddr(Frames[FrameNumber]);
  CpuViewDebugLog.Log('=================== CRASH DUMP END ===================');
  AException.Message := AException.Message + sLineBreak + Dump;
  Application.ShowException(AException);
end;

procedure TExceptionLogger.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if Enabled then
  begin
    FOldExceptionEvent := Application.OnException;
    Application.OnException := @OnExceptionHandler;
    FOldAppNoExceptionMessagesPresent := AppNoExceptionMessages in Application.Flags;
    Application.Flags := Application.Flags - [AppNoExceptionMessages];
  end
  else
  begin
    Application.OnException := FOldExceptionEvent;
    if FOldAppNoExceptionMessagesPresent then
      Application.Flags := Application.Flags + [AppNoExceptionMessages];
  end;
end;

destructor TExceptionLogger.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

end.
