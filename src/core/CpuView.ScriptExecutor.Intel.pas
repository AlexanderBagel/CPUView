////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.ScriptExecutor.Intel.pas
//  * Purpose   : Script executor taking into account Intel x86_64 processor architecture.
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

unit CpuView.ScriptExecutor.Intel;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils,
  StrUtils,
  Generics.Collections,
  FWHexView.Common,
  FWHexView.AsmTokenizer,
  CpuView.CPUContext,
  CpuView.ScriptExecutor;

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
    Value: Int64;
    Decrement: Boolean;
  end;

{ TIntelScriptExecutor }

function TIntelScriptExecutor.CalculateRegValue(pData: PChar; nSize: Integer;
  out ExecuteResult: string): Boolean;
var
  pNewData: PChar;
  AExpression: TExpression;
  PrevIsMemRip: Boolean;
begin
  Result := False;
  PrevIsMemRip := False;
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

    // fix previos rip size
    if CalculatedList.Count > 0 then
    begin
      if (AExpression.Types = [etImm]) and PrevIsMemRip then
        CalculatedList.List[CalculatedList.Count - 1].MemSize := 4;
    end;

    CalculatedList.Add(AExpression);
    PrevIsMemRip := (AExpression.Types * [etMem, etRip] = [etMem, etRip]) and
      not (etSizePfx in AExpression.Types);
  end;
  if (CalculatedList.Count > 0) and CalculatedList.List[0].Calculated then
    CalculatedValue := CalculatedList.List[0].Value;
  Result := True;
end;

function TIntelScriptExecutor.CalculateSingleExpression(var pData: PChar;
  var nSize: Integer; out AExpression: TExpression): Boolean;
var
  Tokens: TListEx<TToken>;
  TokenStr: string;
  TokenLen, I: Integer;
  Token: TToken;
  WaitState: TWaitStates;
  Sign, Multiply, Mem: Boolean;
  MemSize: Integer;
  RegValue: TRegValue;
  RegType: TRegType;
begin
  Result := False;
  if (Context = nil) or (Debugger = nil) then Exit;
  Tokens := TListEx<TToken>.Create;
  try
    Sign := False;
    Multiply := False;
    Mem := False;
    WaitState := [wsInstruction, wsSizePfx, wsMem, wsRegImm];
    MemSize := 0;
    AExpression := Default(TExpression);
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
              Include(AExpression.Types, etMem);
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
              if (Tokens.Count = 0) and (WaitState <> []) then
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
            if not TryStrToInt64(TokenStr, Token.Value) then
              Exit;
            Token.Decrement := Sign;
            Include(AExpression.Types, etImm);
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
          RegType := FParser.GetRegType(TokenStr);
          {%H-}case RegType of
            rtUnknown, rtRegSeg: Exit;
            rtX87, rtSimd64, rtSimd128, rtSimd256, rtSimd512:
            begin
              if Mem or Sign or Multiply or (Tokens.Count > 0) then Exit;
              Include(AExpression.Types, etSimdX87);
            end;
          end;
          if wsRegImm in WaitState then
          begin
            if (TokenStr = 'RIP') and Mem then
            begin
              RegValue.QwordValue := CurrentRIPOffset;
              Include(AExpression.Types, etRip);
            end
            else
              if Context.RegQueryValue(TokenStr, RegValue) then
                Include(AExpression.Types, etReg)
              else
                Exit;
            if (etSimdX87 in AExpression.Types) and not Mem then
            begin
              if RegType = rtSimd64 then
                Move(RegValue.QwordValue, AExpression.SimdX87[0], 8)
              else
                Move(RegValue.Ext32[0], AExpression.SimdX87[0], 32);
              {%H-}case RegType of
                rtX87: AExpression.MemSize := 10;
                rtSimd64: AExpression.MemSize := 8;
                rtSimd128: AExpression.MemSize := 16;
                rtSimd256: AExpression.MemSize := 32;
              end;
              WaitState := [];
            end
            else
            begin
              Token.Value := RegValue.QwordValue;
              Token.Decrement := Sign;
              Sign := False;
              if Multiply then
              begin
                Multiply := False;
                Tokens.List[Tokens.Count - 1].Value := Tokens.Last.Value * Token.Value;
              end
              else
                Tokens.Add(Token);
              WaitState := [wsOperand];
            end;
          end
          else
            Exit;
        end;
        ttPrefix, ttKernel, ttNop:;
        ttSize:
        begin
          TokenStr := Trim(Copy(pData, 1, TokenLen));
          AExpression.Data := AExpression.Data + TokenStr + ' ';
          if wsSizePfx in WaitState then
          begin
            case IndexStr(TokenStr, ['BYTE', 'WORD', 'DWORD', 'QWORD', 'TBYTE']) of
              0: MemSize := 1;
              1: MemSize := 2;
              2: MemSize := 4;
              3: MemSize := 8;
              4:
              begin
                MemSize := 10;
                Include(AExpression.Types, etSimdX87);
              end;
            else
              Exit(False);
            end;
            Include(AExpression.Types, etSizePfx);
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

    if Mem then
    begin
      if MemSize = 0 then
        MemSize := Debugger.PointerSize;
      if etSimdX87 in AExpression.Types then
      begin
        if Debugger.ReadMemory(AExpression.Value, AExpression.SimdX87[0], MemSize) then
          AExpression.MemSize := MemSize;
      end
      else
        if Debugger.ReadMemory(AExpression.Value, AExpression.MemValue, MemSize) then
          AExpression.MemSize := MemSize;
    end;

    AExpression.Calculated := (etReg in AExpression.Types) or (Mem and (AExpression.MemSize > 0));
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
  if Script.StartsWith('?') then
    UpCaseScript := Trim(Copy(UpCaseScript, 2,  Length(Script) - 1));
  UpCaseScript := StringReplace(UpCaseScript, '0X', '0x', [rfReplaceAll]);
  Result := CalculateRegValue(@UpCaseScript[1], Length(UpCaseScript), ExecuteResult);
end;

end.
