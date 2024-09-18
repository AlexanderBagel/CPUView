unit dlgCpuViewIntel;

{$mode Delphi}

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

  { TfrmCpuViewIntel }

  TfrmCpuViewIntel = class(TfrmCpuView)
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
    THintMenuParam = record
      Caption: string;
      AddrVA: Int64;
      IsMem: Boolean;
    end;
  private
    FContext: TIntelCpuContext;
    FScript: TIntelScriptExecutor;
    FHintMenuData: TList<THintMenuParam>;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    function GetContext: TCommonCpuContext; override;
  public

  end;

implementation

{$R *.lfm}

{ TfrmCpuViewIntel }

procedure TfrmCpuViewIntel.acFPU_MMXUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.FPUMode = TFPUMode(TAction(Sender).Tag);
end;

procedure TfrmCpuViewIntel.acRegShowDebugExecute(Sender: TObject);
begin
  FContext.ShowDebug := not FContext.ShowDebug;
end;

procedure TfrmCpuViewIntel.acRegShowDebugUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowDebug;
end;

procedure TfrmCpuViewIntel.acRegShowFPUExecute(Sender: TObject);
begin
  FContext.ShowFPU := not FContext.ShowFPU;
end;

procedure TfrmCpuViewIntel.acRegShowFPUUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowFPU;
end;

procedure TfrmCpuViewIntel.acRegShowXMMExecute(Sender: TObject);
begin
  FContext.ShowXMM := not FContext.ShowXMM;
end;

procedure TfrmCpuViewIntel.acRegShowXMMUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowXMM;
end;

procedure TfrmCpuViewIntel.acRegShowYMMExecute(Sender: TObject);
begin
  FContext.ShowYMM := not FContext.ShowYMM;
end;

procedure TfrmCpuViewIntel.acRegShowYMMUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowYMM;
end;

procedure TfrmCpuViewIntel.acRegSimpleModeExecute(Sender: TObject);
begin
  if FContext.MapMode = icmDetailed then
    FContext.MapMode := icmSimple
  else
    FContext.MapMode := icmDetailed;
end;

procedure TfrmCpuViewIntel.acRegSimpleModeUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.MapMode = icmSimple;
end;

procedure TfrmCpuViewIntel.AsmViewSelectionChange(Sender: TObject);
var
  ExecuteResult: string;
  I: Integer;
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
      if not Expression.RegPresent then Continue;
      HintParam.Caption := Expression.Data;
      HintParam.AddrVA := Expression.Value;
      HintParam.IsMem := False;
      if Core.AddrInDump(HintParam.AddrVA) then
        FHintMenuData.Add(HintParam);
      if Expression.MemPresent then
      begin
        ExecuteResult := Format('%s = [%x] -> %x', [Expression.Data, Expression.Value, Expression.MemValue]);
        HintParam.AddrVA := Expression.MemValue;
        HintParam.IsMem := True;
        if Core.AddrInDump(HintParam.AddrVA) then
          FHintMenuData.Add(HintParam);
      end
      else
        ExecuteResult := Format('%s = %x', [Expression.Data, Expression.Value]);
      memHints.Lines.Add(ExecuteResult);
    end;
  finally
    memHints.Lines.EndUpdate;
  end;
end;

procedure TfrmCpuViewIntel.edCommandsKeyPress(Sender: TObject; var Key: char);
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
    if Expression.MemPresent then
      ExecuteResult := Format('%s = [%x] -> %x', [Expression.Data, Expression.Value, Expression.MemValue])
    else
      ExecuteResult := Format('%s = %x', [Expression.Data, Expression.Value]);
  end;
  StatusBar.Panels[0].Text := ExecuteResult;
end;

procedure TfrmCpuViewIntel.pmHintPopup(Sender: TObject);
begin
  pmHint.Items.Clear;
end;

procedure TfrmCpuViewIntel.DoCreate;
begin
  FScript := TIntelScriptExecutor.Create;
  FScript.Context := FContext;
  FScript.Debugger := DbgGate;
  FHintMenuData := TList<THintMenuParam>.Create;
end;

procedure TfrmCpuViewIntel.DoDestroy;
begin
  FScript.Free;
  FContext.Free;
  FHintMenuData.Free;
end;

procedure TfrmCpuViewIntel.acFPU_MMXExecute(Sender: TObject);
begin
  FContext.FPUMode := TFPUMode(TAction(Sender).Tag);
end;

function TfrmCpuViewIntel.GetContext: TCommonCpuContext;
begin
  if FContext = nil then
    FContext := TIntelCpuContext.Create(Self);
  Result := FContext;
end;

end.

