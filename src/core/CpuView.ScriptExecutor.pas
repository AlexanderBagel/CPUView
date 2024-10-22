unit CpuView.ScriptExecutor;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I CpuViewCfg.inc}

uses
  SysUtils,
  Generics.Collections,
  CpuView.CPUContext,
  CpuView.DebugerGate;

type
  TExpressionType = (etMem, etReg, etRip, etImm, etSizePfx);
  TExpressionTypes = set of TExpressionType;
  TExpression = record
    Data: string;
    Value, MemValue: UInt64;
    MemSize: Integer;
    Calculated: Boolean;
    Types: TExpressionTypes;
  end;

  TExpressionList = class(TList<TExpression>);

  TAbstractScriptExecutor = class
  private
    FCalculatedList: TExpressionList;
    FContext: TCommonCpuContext;
    FCurrentRIPOffset: UInt64;
    FDebugger: TAbstractDebugger;
  protected
    function DoExecute(const Script: string; out ExecuteResult: string): Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute(const Script: string; out ExecuteResult: string): Boolean;
    property CalculatedList: TExpressionList read FCalculatedList;
    property Context: TCommonCpuContext read FContext write FContext;
    property CurrentRIPOffset: UInt64 read FCurrentRIPOffset write FCurrentRIPOffset;
    property Debugger: TAbstractDebugger read FDebugger write FDebugger;
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
  // first the processing of setting up the breakpoints by library
  // name + function and only after that DoExecute.
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
