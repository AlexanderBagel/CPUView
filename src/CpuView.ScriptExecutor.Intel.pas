unit CpuView.ScriptExecutor.Intel;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils,
  StrUtils,
  Generics.Collections,
  CpuView.ScriptExecutor,
  FWHexView.AsmTokenizer;

type
  TIntelScriptExecutor = class(TAbstractScriptExecutor)
  private
    FParser: TAsmTokenizer;
    function CalculateSingleExpression(var pData: PChar; var nSize: Integer;
      out AExpression: TExpression): Boolean;
    function CalculateRegValue(pData: PChar; nSize: Integer; out ExecuteResult: string): Boolean;
  protected
    function DoExecute(const Script: string; out ExecuteResult: string): Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

type
  TWaitState = (wsInstruction, wsSizePfx, wsMem, wsRegImm, wsOperand);
  TWaitStates = set of TWaitState;
  TToken = record
    Value: UInt64;
    Decrement: Boolean;
  end;

{ TIntelScriptExecutor }

function TIntelScriptExecutor.CalculateRegValue(pData: PChar; nSize: Integer;
  out ExecuteResult: string): Boolean;
var
  pNewData: PChar;
  AExpression: TExpression;
begin
  Result := False;
  ExecuteResult := '';
  CalculatedList.Clear;
  while nSize > 0 do
  begin
    pNewData := pData;
    if not CalculateSingleExpression(pNewData, nSize, AExpression) then
    begin
      if AExpression.Data <> '' then
        ExecuteResult := 'Error while processing an expression: "' + AExpression.Data + '"';
      Exit;
    end;
    pData := pNewData;
    CalculatedList.Add(AExpression);
  end;
  Result := True;
end;

function TIntelScriptExecutor.CalculateSingleExpression(var pData: PChar;
  var nSize: Integer; out AExpression: TExpression): Boolean;
var
  Tokens: TList<TToken>;
  TokenStr: string;
  TokenLen, I: Integer;
  Token: TToken;
  WaitState: TWaitStates;
  Sign, Multiply, Mem, RegPresent: Boolean;
  MemSize: Integer;
begin
  Result := False;
  if (Context = nil) or (Debugger = nil) then Exit;
  Tokens := TList<TToken>.Create;
  try
    Sign := False;
    Multiply := False;
    Mem := False;
    WaitState := [wsInstruction, wsSizePfx, wsMem, wsRegImm];
    MemSize := Debugger.PointerSize;
    RegPresent := False;
    FillChar(AExpression, SizeOf(AExpression), 0);
    while nSize > 0 do
    begin
      TokenLen := nSize;
      case FParser.GetToken(pData, TokenLen) of
        ttUnknown:
        begin
          TokenLen := 1;
          TokenStr := Copy(pData, 1, TokenLen);
          if pData^ <> ',' then
            AExpression.Data := AExpression.Data + TokenStr;
          case pData^ of
            '+':
            begin
              if not (wsOperand in WaitState) then
                Exit;
            end;
            '-':
            begin
              if not (wsOperand in WaitState) then
                Exit;
              Sign := True;
            end;
            '*':
            begin
              if Tokens.Count = 0 then Exit;
              if not (wsOperand in WaitState) then
                Exit;
              Multiply := True;
            end;
            '[':
            begin
              if not (wsMem in WaitState) then
                Exit;
              Mem := True;
            end;
            ']':
            begin
              if Mem then
              begin
                Inc(pData);
                Dec(nSize);
                if pData^ = ',' then
                begin
                  Inc(pData);
                  Dec(nSize);
                end;
              end
              else
                Exit;
              Break;
            end;
            ',':
            begin
              if Tokens.Count = 0 then
                Exit
              else
              begin
                Inc(pData);
                Dec(nSize);
              end;
              Break;
            end
          else
            Exit;
          end;
          WaitState := [wsRegImm];
        end;
        ttNumber:
        begin
          TokenStr := Copy(pData, 1, TokenLen);
          AExpression.Data := AExpression.Data + TokenStr;
          if wsRegImm in WaitState then
          begin
            if not TryStrToUInt64(TokenStr, Token.Value) then
              Exit;
            Token.Decrement := Sign;
            Sign := False;
            if Multiply then
            begin
              Multiply := False;
              Tokens.List[Tokens.Count - 1].Value := Tokens.Last.Value * Token.Value;
            end
            else
              Tokens.Add(Token);
          end
          else
            Exit;
          WaitState := [wsOperand];
        end;
        ttInstruction, ttJmp:
        begin
          if wsInstruction in WaitState then
            Exclude(WaitState, wsInstruction)
          else
          begin
            TokenStr := Copy(pData, 1, TokenLen);
            AExpression.Data := AExpression.Data + TokenStr;
            if Trim(TokenStr) <> 'PTR' then
              Exit;
          end;
        end;
        ttReg:
        begin
          TokenStr := Copy(pData, 1, TokenLen);
          AExpression.Data := AExpression.Data + TokenStr;
          if wsRegImm in WaitState then
          begin
            if (TokenStr = 'RIP') and Mem then
              Token.Value := CurrentRIPOffset
            else
              if not Context.QueryRegValueByName(TokenStr, Token.Value) then
                Exit;
            RegPresent := True;
            Token.Decrement := Sign;
            Sign := False;
            if Multiply then
            begin
              Multiply := False;
              Tokens.List[Tokens.Count - 1].Value := Tokens.Last.Value * Token.Value;
            end
            else
              Tokens.Add(Token);
          end
          else
            Exit;
          WaitState := [wsOperand];
        end;
        ttPrefix, ttKernel, ttNop:;
        ttSize:
        begin
          TokenStr := Trim(Copy(pData, 1, TokenLen));
          AExpression.Data := AExpression.Data + TokenStr + ' ';
          if wsSizePfx in WaitState then
          begin
            case IndexStr(TokenStr, ['BYTE', 'WORD', 'DWORD', 'QWORD']) of
              0: MemSize := 1;
              1: MemSize := 2;
              2: MemSize := 4;
              3: MemSize := 8;
            else
              Exit(False);
            end;
          end
          else
            Exit(False);
        end;
      end;
      Dec(nSize, TokenLen);
      Inc(pData, TokenLen);
    end;

    AExpression.Value := 0;
    for I := 0 to Tokens.Count - 1 do
      if Tokens.List[I].Decrement then
        Dec(AExpression.Value, Tokens.List[I].Value)
      else
        Inc(AExpression.Value, Tokens.List[I].Value);

    if Mem and Debugger.ReadMemory(AExpression.Value, AExpression.MemValue, MemSize) then
      AExpression.MemSize := MemSize;

    AExpression.RegPresent := RegPresent;
    Result := True;

    while (nSize > 0) and (pData^ = ' ') do
    begin
      Dec(nSize);
      Inc(pData);
    end;

  finally
    Tokens.Free;
  end;
end;

constructor TIntelScriptExecutor.Create;
begin
  inherited;
  FParser := TAsmTokenizer.Create;
end;

destructor TIntelScriptExecutor.Destroy;
begin
  FParser.Free;
  inherited;
end;

function TIntelScriptExecutor.DoExecute(const Script: string;
  out ExecuteResult: string): Boolean;
var
  UpCaseScript: string;
begin
  UpCaseScript := UpperCase(Script);
  if Script.StartsWith('? ') then
    UpCaseScript := Copy(UpCaseScript, 3,  Length(Script) - 2);
  Result := CalculateRegValue(@UpCaseScript[1], Length(UpCaseScript), ExecuteResult);
end;

end.
