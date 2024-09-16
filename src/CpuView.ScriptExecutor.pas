unit CpuView.ScriptExecutor;

interface

{$I CpuViewCfg.inc}

uses
  SysUtils,
  Generics.Collections,
  CpuView.CPUContext,
  CpuView.DebugerGate;

type
  TExpression = record
    Data: string;
    Value, MemValue: UInt64;
    MemPresent: Boolean;
    Hint: string;
  end;

  TExpressionList = class(TList<TExpression>);

  TAbstractScriptExecutor = class
  private
    FCalculatedList: TExpressionList;
    FContext: TCommonCpuContext;
    FCurrentIP: UInt64;
    FDebugger: TAbstractDebugger;
    FPointerSize: Integer;
  protected
    function DoExecute(const Script: string; out ExecuteResult: string): Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute(const Script: string; out ExecuteResult: string): Boolean;
    property CalculatedList: TExpressionList read FCalculatedList;
    property Context: TCommonCpuContext read FContext write FContext;
    property CurrentIP: UInt64 read FCurrentIP write FCurrentIP;
    property Debugger: TAbstractDebugger read FDebugger write FDebugger;
    property PointerSize: Integer read FPointerSize write FPointerSize;
  end;

implementation

{ TAbstractScriptExecutor }

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
begin
  // TODO:
  // сначала обработка установки брякпойнтов по имени библиотеки + функция
  // и только после неё DoExecute
  try
    Result := DoExecute(Script, ExecuteResult);
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
