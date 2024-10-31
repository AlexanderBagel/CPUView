unit CpuView.Design.DbgLog;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Linux, unixtype,
  {$ENDIF}
  SysUtils, Generics.Collections,
  LazLoggerBase, LazLogger, LazIDEIntf;

type

  { TCpuViewDebugLog }

  TCpuViewDebugLog = class
  private
    FEnabled, FReady: Boolean;
    FLevel: Integer;
    FLogger: TLazLoggerFile;
    FLoggerPath: string;
    FStopWatch: specialize TStack<Int64>;
    {$IFDEF WINDOWS}
    FFrequency: Int64;
    {$ENDIF}
    function GetStopWatch: Int64;
    procedure InternalLog(const AMessage: string; AStopWatch: Integer);
    procedure SetEnabled(AValue: Boolean);
  protected
    class var FInstance: TCpuViewDebugLog;
    class destructor ClassDestroy;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure Log(const AMessage: string); overload;
    procedure Log(const AMessage: string; IncLevel: Boolean); overload;
    procedure Log(const AMessage: string; IncLevel, NeedStopWatch: Boolean); overload;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  function CpuViewDebugLog: TCpuViewDebugLog;

implementation

function CpuViewDebugLog: TCpuViewDebugLog;
begin
  if TCpuViewDebugLog.FInstance = nil then
    TCpuViewDebugLog.FInstance := TCpuViewDebugLog.Create;
  Result := TCpuViewDebugLog.FInstance;
end;

{ TCpuViewDebugLog }

function TCpuViewDebugLog.GetStopWatch: Int64;
{$IFDEF LINUX}
var
  T: TTimeSpec;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  QueryPerformanceCounter(Result);
  Result := (10000000 * Result) div FFrequency;
  {$ENDIF WINDOWS}
  {$IFDEF LINUX}
  clock_gettime(CLOCK_MONOTONIC, @T);
  Result := ((T.tv_sec * 1000000000) + T.tv_nsec) div 100;
  {$ENDIF LINUX}
end;

procedure TCpuViewDebugLog.InternalLog(const AMessage: string;
  AStopWatch: Integer);
begin
  if Enabled and Assigned(FLogger) then
    if AStopWatch = 0 then
      FLogger.DebugLn(Format('%d: %s%s',
        [GetCurrentThreadId, StringOfChar(' ', FLevel), AMessage]))
    else
      FLogger.DebugLn(Format('%d: %s%s - elapsed: %d ticks',
        [GetCurrentThreadId, StringOfChar(' ', FLevel), AMessage, AStopWatch]));
end;

procedure TCpuViewDebugLog.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  FEnabled := AValue;
  if AValue then
  begin
    FLogger := TLazLoggerFile.Create;
    FLogger.Assign(DebugLogger);
    FLogger.LogName := FLoggerPath;
  end
  else
    FreeAndNil(FLogger);
end;

class destructor TCpuViewDebugLog.ClassDestroy;
begin
  FreeAndNil(FInstance);
end;

constructor TCpuViewDebugLog.Create;
begin
  FLoggerPath := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + 'cpuview' + PathDelim;
  ForceDirectories(FLoggerPath);
  FLoggerPath := FLoggerPath + 'debug.log';
  if FileExists(FLoggerPath) then
    DeleteFile(FLoggerPath);
  FStopWatch := specialize TStack<Int64>.Create;
  {$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(FFrequency);
  {$ENDIF}
end;

destructor TCpuViewDebugLog.Destroy;
begin
  Enabled := False;
  FStopWatch.Free;
  inherited Destroy;
end;

procedure TCpuViewDebugLog.Reset;
begin
  FLevel := 0;
  FStopWatch.Clear;
  if FReady then
    InternalLog('', 0);
  FReady := True;
end;

procedure TCpuViewDebugLog.Log(const AMessage: string);
begin
  InternalLog(AMessage, 0);
end;

procedure TCpuViewDebugLog.Log(const AMessage: string; IncLevel: Boolean);
begin
  Log(AMessage, IncLevel, True);
end;

procedure TCpuViewDebugLog.Log(const AMessage: string; IncLevel,
  NeedStopWatch: Boolean);
var
  AStopWatch, APrevStopWatch: Int64;
begin
  if IncLevel then
  begin
    InternalLog(AMessage, 0);
    Inc(FLevel, 2);
    if NeedStopWatch then
      FStopWatch.Push(GetStopWatch)
    else
      FStopWatch.Push(-1);
  end
  else
  begin
    AStopWatch := GetStopWatch;
    APrevStopWatch := FStopWatch.Pop;
    if APrevStopWatch = -1 then
      AStopWatch := 0
    else
      AStopWatch := AStopWatch - APrevStopWatch;
    Dec(FLevel, 2);
    InternalLog(AMessage, AStopWatch);
  end;
end;

end.
