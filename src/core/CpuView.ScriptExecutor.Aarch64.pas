////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.ScriptExecutor.Aarch64.pas
//  * Purpose   : Script executor taking into account Aarch64 processor architecture.
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

unit CpuView.ScriptExecutor.Aarch64;

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

  { TAarch64ScriptExecutor }

  TAarch64ScriptExecutor = class(TAbstractScriptExecutor)
  private
    FParser: TAsmTokenizer;
    function CalculateRegValue(pData: PChar; nSize: Integer; out ExecuteResult: string): Boolean;
    function CalculateSingleExpression(var pData: PChar; var nSize: Integer;
      out AExpression: TExpression): Boolean;
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

{ TAarch64ScriptExecutor }

function TAarch64ScriptExecutor.CalculateRegValue(pData: PChar; nSize: Integer;
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
  if (CalculatedList.Count > 0) and CalculatedList.List[0].Calculated then
    CalculatedValue := CalculatedList.List[0].Value;
  Result := True;
end;

function TAarch64ScriptExecutor.CalculateSingleExpression(var pData: PChar;
  var nSize: Integer; out AExpression: TExpression): Boolean;
var
  Tokens: TListEx<TToken>;
  TokenStr: string;
  TokenLen, I: Integer;
  Token: TToken;
  WaitState: TWaitStates;
  Sign, Mem: Boolean;
  MemSize: Integer;
  RegValue: TRegValue;
  RegType: TRegType;
begin
  Result := False;
  if (Context = nil) or (Debugger = nil) then Exit;
  Tokens := TListEx<TToken>.Create;
  try
    Sign := False;
    Mem := False;
    WaitState := [wsInstruction, wsMem, wsRegImm];
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
          if pData^ = ',' then
          begin
            if Mem then
              AExpression.Data := AExpression.Data + ', ';
          end
          else
            AExpression.Data := AExpression.Data + TokenStr;
          case pData^ of
            '-':
            begin
              if not (wsOperand in WaitState) then
                Exit;
              Sign := True;
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
              if not Mem then
                Break;
            end;
            '!': Break;
          else
            nSize := 0;
            Break;
          end;
          WaitState := [wsRegImm];
        end;
        ttNumber:
        begin
          TokenStr := Copy(pData, 1, TokenLen);
          AExpression.Data := AExpression.Data + TokenStr;
          if wsRegImm in WaitState then
          begin
            if TokenStr.StartsWith('#') then
              TokenStr := Copy(TokenStr, 2, TokenLen);
            if not TryStrToInt64(TokenStr, Token.Value) then
              Exit;
            Token.Decrement := Sign;
            Include(AExpression.Types, etImm);
            Sign := False;
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
          end;
        end;
        ttReg:
        begin
          TokenStr := Copy(pData, 1, TokenLen);
          AExpression.Data := AExpression.Data + TokenStr;
          RegType := FParser.GetRegType(TokenStr);
          {%H-}case RegType of
            rtUnknown: Exit;
            rtSingle, rtDouble, rtSimd128:
            begin
              if Mem or Sign or (Tokens.Count > 0) then Exit;
              Include(AExpression.Types, etSimdX87);
            end;
          end;
          if wsRegImm in WaitState then
          begin
            if Context.RegQueryValue(TokenStr, RegValue) then
              Include(AExpression.Types, etReg)
            else
              Exit;
            if (etSimdX87 in AExpression.Types) and not Mem then
            begin
              Move(RegValue.Ext16[0], AExpression.SimdX87[0], 16);
              {%H-}case RegType of
                rtSingle: AExpression.MemSize := 4;
                rtDouble: AExpression.MemSize := 8;
                rtSimd128: AExpression.MemSize := 16;
              end;
              WaitState := [];
            end
            else
            begin
              Token.Value := RegValue.QwordValue;
              Token.Decrement := Sign;
              Sign := False;
              Tokens.Add(Token);
              WaitState := [wsOperand];
            end;
          end
          else
            Exit;
        end;
      else
        // do nothing...
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

constructor TAarch64ScriptExecutor.Create;
begin
  inherited;
  FParser := TAsmTokenizer.Create;
  FParser.TokenizerMode := tmArm;
end;

destructor TAarch64ScriptExecutor.Destroy;
begin
  FParser.Free;
  inherited;
end;

function TAarch64ScriptExecutor.DoExecute(const Script: string;
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
