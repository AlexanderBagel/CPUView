////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgCpuView.pas
//  * Purpose   : GUI debugger with implementation for Intel x86_64 processor.
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

unit dlgCpuViewImplementation;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6060 off : Case statement does not handle all possible cases}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ExtCtrls, Generics.Collections,

  dlgCpuView,

  FWHexView.Common,
  FWHexView.Actions,
  CpuView.CPUContext,
  CpuView.IntelContext,
  CpuView.ScriptExecutor,
  CpuView.ScriptExecutor.Intel;

type

  { TfrmCpuViewImpl }

  TfrmCpuViewImpl = class(TfrmCpuView)
    acFPU_MMX: TAction;
    acFPU_R: TAction;
    acFPU_ST: TAction;
    acRegSimpleMode: TAction;
    acRegShowDebug: TAction;
    acRegShowFPU: TAction;
    acRegShowXMM: TAction;
    acRegShowYMM: TAction;
    miRegIntelViewMode: TMenuItem;
    miRegIntelFPU: TMenuItem;
    miRegIntelFPUSt: TMenuItem;
    miRegIntelFPURx: TMenuItem;
    miRegIntelFPUMmx: TMenuItem;
    miRegIntelShowFPU: TMenuItem;
    miRegIntelShowXMM: TMenuItem;
    miRegIntelShowYMM: TMenuItem;
    miRegIntelShowDebug: TMenuItem;
    miRegIntelCopy: TMenuItem;
    pmIntelReg: TPopupMenu;
    miRegIntelSep1: TMenuItem;
    miRegIntelSep2: TMenuItem;
    procedure acFPU_MMXExecute(Sender: TObject);
    procedure acFPU_MMXUpdate(Sender: TObject);
    procedure acRegShowDebugExecute(Sender: TObject);
    procedure acRegShowDebugUpdate(Sender: TObject);
    procedure acRegShowFPUExecute(Sender: TObject);
    procedure acRegShowFPUUpdate(Sender: TObject);
    procedure acRegShowXMMExecute(Sender: TObject);
    procedure acRegShowXMMUpdate(Sender: TObject);
    procedure acRegShowYMMExecute(Sender: TObject);
    procedure acRegShowYMMUpdate(Sender: TObject);
    procedure acRegSimpleModeExecute(Sender: TObject);
    procedure acRegSimpleModeUpdate(Sender: TObject);
    procedure AsmViewSelectionChange(Sender: TObject);
    procedure edCommandsKeyPress(Sender: TObject; var Key: char);
    procedure pmHintPopup(Sender: TObject);
  private type
    THintMenuType = (hmtAddrVA, hmtSeparator, hmtCopy, hmtSelectAll);
    THintMenuParam = record
      Caption: string;
      AddrVA: Int64;
      MemSize: Integer;
      MenuType: THintMenuType;
    end;
  private const
    MNU_GOTOASM = -1;
    MNU_GOTODMP = -2;
    MNU_GOTOSTK = -3;
  private
    FContext: TIntelCpuContext;
    FScript: TIntelScriptExecutor;
    FHintMenuData: TList<THintMenuParam>;
    procedure OnHintMenuClick(Sender: TObject);
    procedure OnReset(Sender: TObject);
  protected
    procedure AfterDbgGateCreate; override;
    procedure BeforeDbgGateDestroy; override;
    function GetContext: TCommonCpuContext; override;
  public

  end;

implementation

{$R *.lfm}

{ TfrmCpuViewImpl }

procedure TfrmCpuViewImpl.acFPU_MMXUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.FPUMode = TFPUMode(TAction(Sender).Tag);
end;

procedure TfrmCpuViewImpl.acRegShowDebugExecute(Sender: TObject);
begin
  FContext.ShowDebug := not FContext.ShowDebug;
end;

procedure TfrmCpuViewImpl.acRegShowDebugUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowDebug;
end;

procedure TfrmCpuViewImpl.acRegShowFPUExecute(Sender: TObject);
begin
  FContext.ShowFPU := not FContext.ShowFPU;
end;

procedure TfrmCpuViewImpl.acRegShowFPUUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowFPU;
end;

procedure TfrmCpuViewImpl.acRegShowXMMExecute(Sender: TObject);
begin
  FContext.ShowXMM := not FContext.ShowXMM;
end;

procedure TfrmCpuViewImpl.acRegShowXMMUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowXMM;
end;

procedure TfrmCpuViewImpl.acRegShowYMMExecute(Sender: TObject);
begin
  FContext.ShowYMM := not FContext.ShowYMM;
end;

procedure TfrmCpuViewImpl.acRegShowYMMUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowYMM;
end;

procedure TfrmCpuViewImpl.acRegSimpleModeExecute(Sender: TObject);
begin
  if FContext.MapMode = icmDetailed then
    FContext.MapMode := icmSimple
  else
    FContext.MapMode := icmDetailed;
end;

procedure TfrmCpuViewImpl.acRegSimpleModeUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.MapMode = icmSimple;
end;

procedure TfrmCpuViewImpl.AsmViewSelectionChange(Sender: TObject);
var
  ExecuteResult, ValueAccess, MemValueAccess, Symbol, MemSymbol: string;
  I, Idx: Integer;
  Expression: TExpression;
  HintParam: THintMenuParam;
begin
  inherited;
  memHints.Lines.BeginUpdate;
  try
    memHints.Lines.Clear;
    FHintMenuData.Clear;
    FScript.CurrentRIPOffset := AsmView.SelectedInstructionAddr + AsmView.SelectedRawLength;
    if not FScript.Execute(
      AsmView.SelectedColumnAsString(ctDescription), ExecuteResult) then
      Exit;
    for I := 0 to FScript.CalculatedList.Count - 1 do
    begin
      Expression := FScript.CalculatedList[I];
      if not Expression.Calculated then Continue;
      Symbol := '';
      HintParam.MenuType := hmtAddrVA;
      HintParam.MemSize := Expression.MemSize;
      if Expression.MemSize > 0 then
      begin
        Idx := Pos('[', Expression.Data);
        HintParam.Caption := Copy(Expression.Data, Idx + 1, Length(Expression.Data));
        HintParam.Caption := StringReplace(HintParam.Caption, ']', '', [rfReplaceAll]);
      end
      else
      begin
        HintParam.Caption := Expression.Data;
        HintParam.MemSize := DbgGate.PointerSize;
      end;
      HintParam.AddrVA := Expression.Value;
      ValueAccess := Core.QueryAccessStr(HintParam.AddrVA);
      Symbol := Core.QuerySymbolAtAddr(HintParam.AddrVA);
      if Symbol <> '' then
        Symbol := ' ' + Symbol;
      if Core.AddrInDump(HintParam.AddrVA) then
        FHintMenuData.Add(HintParam);
      if Expression.MemSize > 0 then
      begin
        MemSymbol := Core.QuerySymbolAtAddr(Expression.MemValue);
        MemValueAccess := Core.QueryAccessStr(Expression.MemValue);
        ExecuteResult := Format('%s = [%x (%s)%s] -> %x (%s) %s',
          [Expression.Data, Expression.Value, ValueAccess, Symbol,
          Expression.MemValue, MemValueAccess, MemSymbol]);
        HintParam.AddrVA := Expression.MemValue;
        HintParam.Caption := Expression.Data;
        HintParam.MemSize := DbgGate.PointerSize;
        if Core.AddrInDump(HintParam.AddrVA) then
          FHintMenuData.Add(HintParam);
      end
      else
        ExecuteResult := Format('%s = %x (%s)%s',
          [Expression.Data, Expression.Value, ValueAccess, Symbol]);
      memHints.Lines.Add(ExecuteResult);
    end;
    HintParam.MenuType := hmtSeparator;
    FHintMenuData.Add(HintParam);
    HintParam.MenuType := hmtSelectAll;
    FHintMenuData.Add(HintParam);
    HintParam.MenuType := hmtCopy;
    FHintMenuData.Add(HintParam);
  finally
    memHints.Lines.EndUpdate;
  end;
end;

procedure TfrmCpuViewImpl.edCommandsKeyPress(Sender: TObject; var Key: char);
var
  ExecuteResult: string;
  Expression: TExpression;
begin
  if Key <> #13 then Exit;
  if Trim(edCommands.Text) = '' then Exit;
  FScript.CurrentRIPOffset := AsmView.SelectedInstructionAddr + AsmView.SelectedRawLength;
  if FScript.Execute(edCommands.Text, ExecuteResult) then
  begin
    Expression := FScript.CalculatedList[0];
    if Expression.MemSize > 0 then
      ExecuteResult := Format('%s = [%x] -> %x', [Expression.Data, Expression.Value, Expression.MemValue])
    else
      ExecuteResult := Format('%s = %x', [Expression.Data, Expression.Value]);
  end;
  StatusBar.Panels[2].Text := ExecuteResult;
end;

procedure TfrmCpuViewImpl.pmHintPopup(Sender: TObject);

  function AddMenuItem(AParent: TMenuItem; const ACaption: string;
    ATag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(pmHint);
    Result.Caption := ACaption;
    Result.Tag := ATag;
    Result.OnClick := OnHintMenuClick;
    AParent.Add(Result);
  end;

var
  AItem: TMenuItem;
  AParam: THintMenuParam;
  I: Integer;
begin
  pmHint.Items.Clear;
  for I := 0 to FHintMenuData.Count - 1 do
  begin
    AParam := FHintMenuData[I];
    case AParam.MenuType of
      hmtSeparator: AddMenuItem(pmHint.Items, '-', I);
      hmtCopy: AddMenuItem(pmHint.Items, 'Copy', I);
      hmtSelectAll: AddMenuItem(pmHint.Items, 'Select All', I);
      hmtAddrVA:
      begin
        AItem := AddMenuItem(pmHint.Items, AParam.Caption +
          ' = ' + IntToHex(AParam.AddrVA, 1), I);
        if Core.AddrInAsm(AParam.AddrVA) then
          AddMenuItem(AItem, 'Follow in Asm', MNU_GOTOASM);
        if Core.AddrInDump(AParam.AddrVA) then
          AddMenuItem(AItem, 'Follow in Dump', MNU_GOTODMP);
        if Core.AddrInStack(AParam.AddrVA) then
          AddMenuItem(AItem, 'Follow in Stack', MNU_GOTOSTK);
      end;
    end;
  end;
end;

procedure TfrmCpuViewImpl.OnHintMenuClick(Sender: TObject);

  function GetMnuParam: THintMenuParam;
  begin
    Result := FHintMenuData[TMenuItem(Sender).Parent.Tag];
  end;

var
  AParam: THintMenuParam;
begin
  case TMenuItem(Sender).Tag of
    MNU_GOTOSTK:
    begin
      AParam := GetMnuParam;
      Core.ShowStackAtAddr(AParam.AddrVA);
      ActiveControl := StackView;
    end;
    MNU_GOTODMP:
    begin
      AParam := GetMnuParam;
      Core.ShowDumpAtAddr(AParam.AddrVA);
      ActiveDumpView.SelStart := AParam.AddrVA;
      ActiveDumpView.SelEnd := AParam.AddrVA + AParam.MemSize - 1;
      ActiveControl := ActiveDumpView;
    end;
    MNU_GOTOASM:
    begin
      AParam := GetMnuParam;
      Core.ShowDisasmAtAddr(AParam.AddrVA);
      ActiveControl := AsmView;
    end;
  else
    case FHintMenuData[TMenuItem(Sender).Tag].MenuType of
      hmtCopy: memHints.CopyToClipboard;
      hmtSelectAll:
      begin
        memHints.SelectAll;
        ActiveControl := memHints;
      end;
    end;
  end;
end;

procedure TfrmCpuViewImpl.OnReset(Sender: TObject);
begin
  FHintMenuData.Clear;
  edCommands.Text := '';
  memHints.Text := '';
  UpdateStatusBar;
end;

procedure TfrmCpuViewImpl.AfterDbgGateCreate;
begin
  FScript := TIntelScriptExecutor.Create;
  FScript.Context := GetContext;
  FScript.Debugger := DbgGate;
  FHintMenuData := TList<THintMenuParam>.Create;
  Core.OnReset := OnReset;
end;

procedure TfrmCpuViewImpl.BeforeDbgGateDestroy;
begin
  FScript.Free;
  FHintMenuData.Free;
end;

procedure TfrmCpuViewImpl.acFPU_MMXExecute(Sender: TObject);
begin
  FContext.FPUMode := TFPUMode(TAction(Sender).Tag);
end;

function TfrmCpuViewImpl.GetContext: TCommonCpuContext;
begin
  if FContext = nil then
    FContext := TIntelCpuContext.Create(Self);
  Result := FContext;
end;

end.

