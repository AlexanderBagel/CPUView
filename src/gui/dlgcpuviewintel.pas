unit dlgCpuViewIntel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ExtCtrls,

  dlgCpuView,

  FWHexView.Common,
  FWHexView.Actions,
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
    procedure FormDestroy(Sender: TObject);
  private
    FContext: TIntelCpuContext;
    FScript: TIntelScriptExecutor;
  protected
    procedure InitContext; override;
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
begin
  inherited;
  memHints.Lines.BeginUpdate;
  try
    memHints.Lines.Clear;
    FScript.CurrentRIPOffset := AsmView.SelectedInstructionAddr + AsmView.SelectedRawLength;
    if not FScript.Execute(
      AsmView.SelectedColumnAsString(ctDescription), ExecuteResult) then
      Exit;
    for I := 0 to FScript.CalculatedList.Count - 1 do
    begin
      Expression := FScript.CalculatedList[I];
      if not Expression.RegPresent then Continue;
      if Expression.MemPresent then
        ExecuteResult := Format('%s = [%x] -> %x', [Expression.Data, Expression.Value, Expression.MemValue])
      else
        ExecuteResult := Format('%s = %x', [Expression.Data, Expression.Value]);
      memHints.Lines.Add(ExecuteResult);
    end;
  finally
    memHints.Lines.EndUpdate;
  end;
end;

procedure TfrmCpuViewIntel.acFPU_MMXExecute(Sender: TObject);
begin
  FContext.FPUMode := TFPUMode(TAction(Sender).Tag);
end;

procedure TfrmCpuViewIntel.FormDestroy(Sender: TObject);
begin
  inherited;
  FScript.Free;
  FContext.Free;
end;

procedure TfrmCpuViewIntel.InitContext;
begin
  FContext := TIntelCpuContext.Create(Self);
  DbgGate.Context := FContext;
  FScript := TIntelScriptExecutor.Create;
  FScript.Context := FContext;
  FScript.Debugger := DbgGate;
end;

end.

