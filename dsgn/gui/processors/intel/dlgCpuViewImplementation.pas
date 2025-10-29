////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgCpuView.pas
//  * Purpose   : GUI debugger with implementation for Intel x86_64 processor.
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

unit dlgCpuViewImplementation;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6060 off : Case statement does not handle all possible cases}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls,

  dlgCpuView,

  CpuView.CPUContext,
  CpuView.Context.Intel,
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
    miRegIntelFit: TMenuItem;
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
    miRegIntelSep3: TMenuItem;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FContext: TIntelCpuContext;
    FScript: TIntelScriptExecutor;
  protected
    function GetContext: TCommonCpuContext; override;
    function ScriptExecutor: TAbstractScriptExecutor; override;
  public

  end;

implementation

{$R *.lfm}

{ TfrmCpuViewImpl }

procedure TfrmCpuViewImpl.acFPU_MMXUpdate(Sender: TObject);
begin
  if TAction(Sender).Tag = 0 then
    TAction(Sender).Enabled := cfMmx in FContext.ContextFeatures
  else
    TAction(Sender).Enabled := cfFloat in FContext.ContextFeatures;
  TAction(Sender).Checked := FContext.FPUMode = TFPUMode(TAction(Sender).Tag);
end;

procedure TfrmCpuViewImpl.acRegShowDebugExecute(Sender: TObject);
begin
  FContext.ShowDebug := not FContext.ShowDebug;
end;

procedure TfrmCpuViewImpl.acRegShowDebugUpdate(Sender: TObject);
begin
  {$IFDEF LINUX}
  TAction(Sender).Visible := False;
  {$ELSE}
  TAction(Sender).Enabled := cfDebug in FContext.ContextFeatures;
  TAction(Sender).Checked := FContext.ShowDebug and TAction(Sender).Enabled;
  {$ENDIF}
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
  TAction(Sender).Enabled := cfSse in FContext.ContextFeatures;
  TAction(Sender).Checked := FContext.ShowXMM and TAction(Sender).Enabled;
end;

procedure TfrmCpuViewImpl.acRegShowYMMExecute(Sender: TObject);
begin
  FContext.ShowYMM := not FContext.ShowYMM;
end;

procedure TfrmCpuViewImpl.acRegShowYMMUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := cfAvx in FContext.ContextFeatures;
  TAction(Sender).Checked := FContext.ShowYMM and TAction(Sender).Enabled;
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

procedure TfrmCpuViewImpl.FormCreate(Sender: TObject);
begin
  FContext := TIntelCpuContext.Create(Self);
  FScript := TIntelScriptExecutor.Create;
  inherited;
end;

procedure TfrmCpuViewImpl.FormDestroy(Sender: TObject);
begin
  inherited;
  FContext.Free;
  FScript.Free;
end;

procedure TfrmCpuViewImpl.acFPU_MMXExecute(Sender: TObject);
begin
  FContext.FPUMode := TFPUMode(TAction(Sender).Tag);
end;

function TfrmCpuViewImpl.GetContext: TCommonCpuContext;
begin
  Result := FContext;
end;

function TfrmCpuViewImpl.ScriptExecutor: TAbstractScriptExecutor;
begin
  Result := FScript;
end;

end.

