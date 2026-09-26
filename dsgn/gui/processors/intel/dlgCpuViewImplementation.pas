////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgCpuView.pas
//  * Purpose   : GUI debugger with implementation for Intel x86_64 processor.
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2026.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//  *
//  * SPDX-License-Identifier: MIT
//  * See LICENSE file in the project root for full license information.
//  *
//  ****************************************************************************
//

unit dlgCpuViewImplementation;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6060 off : Case statement does not handle all possible cases}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls, Generics.Collections,

  dlgCpuView,

  CpuView.Core,
  CpuView.CPUContext,
  CpuView.Context.Intel,
  {$IFDEF MSWINDOWS}
  ComCtrls,
  CpuView.Common,
  CpuView.Windows.Pdb,
  dlgPDBManager,
  {$ENDIF}
  CpuView.DebugerGate,
  CpuView.ScriptExecutor,
  CpuView.ScriptExecutor.Intel;

type
  TPdbSymbolVal = record
    LibIndex: Integer;
    SymbolName: string;
  end;

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
    acUtilsPdb: TAction;
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
    procedure acUtilsPdbExecute(Sender: TObject);
    procedure acUtilsPdbUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FContext: TIntelCpuContext;
    FScript: TIntelScriptExecutor;
    {$IFDEF MSWINDOWS}
    FPdbStorage: TPdbStorage;
    FLoadedPbdLibs: TStringList;
    FPdbSymbols: TDictionary<Int64, TPdbSymbolVal>;
    {$ENDIF}
  protected
    function GetContext: TCommonCpuContext; override;
    function DoQueryPdb(Sender: TObject; AddrVA: Int64;
      AParam: TQuerySymbol; out AValue: TQuerySymbolValue): Boolean;
    function ScriptExecutor: TAbstractScriptExecutor; override;
    procedure OnCoreStateChange(Sender: TObject); override;
    procedure UpdateDebugGateSettings; override;
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

procedure TfrmCpuViewImpl.acUtilsPdbExecute(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  frmPdbManager := TfrmPdbManager.Create(Self);
  try
    frmPdbManager.PdbStorage := FPdbStorage;
    frmPdbManager.Core := Core;
    frmPdbManager.ShowModal;
    if frmPdbManager.DownloadCount > 0 then
    begin
      FLoadedPbdLibs.Clear;
      FPdbSymbols.Clear;
    end;
  finally
    frmPdbManager.Free;
  end;
  {$ENDIF}
end;

procedure TfrmCpuViewImpl.acUtilsPdbUpdate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  acUtilsPdb.Visible := True;
  {$ELSE}
  acUtilsPdb.Visible := False;
  {$ENDIF}
end;

procedure TfrmCpuViewImpl.FormCreate(Sender: TObject);
begin
  FContext := TIntelCpuContext.Create(Self);
  FScript := TIntelScriptExecutor.Create;
  {$IFDEF MSWINDOWS}
  FPdbStorage := TPdbStorage.Create;
  FLoadedPbdLibs := TStringList.Create;
  FPdbSymbols := TDictionary<Int64, TPdbSymbolVal>.Create;
  acUtilsPdb.Visible := True;
  {$ENDIF}
  inherited;
end;

procedure TfrmCpuViewImpl.FormDestroy(Sender: TObject);
begin
  inherited;
  FContext.Free;
  FScript.Free;
  {$IFDEF MSWINDOWS}
  FPdbStorage.Free;
  FLoadedPbdLibs.Free;
  FPdbSymbols.Free;
  {$ENDIF}
end;

procedure TfrmCpuViewImpl.acFPU_MMXExecute(Sender: TObject);
begin
  FContext.FPUMode := TFPUMode(TAction(Sender).Tag);
end;

function TfrmCpuViewImpl.GetContext: TCommonCpuContext;
begin
  Result := FContext;
end;

function TfrmCpuViewImpl.DoQueryPdb(Sender: TObject; AddrVA: Int64;
  AParam: TQuerySymbol; out AValue: TQuerySymbolValue): Boolean;
{$IFDEF MSWINDOWS}
var
  PdbSymbol: TPdbSymbolVal;
  ModulePath, Pfx: string;
  PdbKey: TPdbKey;
  Pdb: TPdb;
  Sym: TPdbSymbol;
  I: Integer;
  SymAddrVA: Int64;
  RemoteModule: TRemoteModule;

  function GetPfx(const Value: string): string;
  begin
    Result := ChangeFileExt(ExtractFileName(Value), ':');
    if Settings.UsePdbPfx then
      Result := '[PDB]_' + Result;
  end;

{$ENDIF}
begin
  Result := False;
  {$IFDEF MSWINDOWS}
  if not Settings.UsePdb then Exit;
  if FPdbSymbols.TryGetValue(AddrVA, PdbSymbol) then
  begin
    AValue.AddrVA := AddrVA;
    Pfx := GetPfx(FLoadedPbdLibs[PdbSymbol.LibIndex]);
    AValue.Description := Pfx + UnDecoratePDBSymbolName(PdbSymbol.SymbolName);
    Exit(True);
  end;
  if not Core.Debugger.Utils.QueryModuleName(AddrVA, ModulePath) then Exit;
  if FLoadedPbdLibs.IndexOf(ModulePath) >= 0 then Exit;
  PdbSymbol.LibIndex := FLoadedPbdLibs.Add(ModulePath);
  if not GetImagePdbKey(ModulePath, PdbKey) then Exit;
  Pdb := FPdbStorage.QueryPDB(PdbKey);
  Pdb.BinaryPath := ModulePath;
  if Pdb.State <> psReady then Exit;
  RemoteModule := Core.Debugger.GetRemoteModuleHandle(ExtractFileName(Pdb.BinaryPath));
  if RemoteModule.ImageBase = 0 then Exit;
  Pfx := GetPfx(ModulePath);
  for I := 0 to Pdb.Count - 1 do
  begin
    Sym := Pdb[I];
    SymAddrVA := Int64(Sym.Rva) + RemoteModule.ImageBase;
    PdbSymbol.SymbolName := Sym.Name;
    FPdbSymbols.TryAdd(SymAddrVA, PdbSymbol);
    if SymAddrVA = AddrVA then
    begin
      AValue.AddrVA := AddrVA;
      AValue.Description := Pfx + UnDecoratePDBSymbolName(Sym.Name);
      Result := True;
    end;
  end;
  {$ENDIF}
end;

function TfrmCpuViewImpl.ScriptExecutor: TAbstractScriptExecutor;
begin
  Result := FScript;
end;

procedure TfrmCpuViewImpl.OnCoreStateChange(Sender: TObject);
begin
  inherited OnCoreStateChange(Sender);
  {$IFDEF MSWINDOWS}
  if Core.CoreState = csDebuggerInit then
  begin
    Core.Debugger.OnQueryExternalDebugInfo := DoQueryPdb;
    FLoadedPbdLibs.Clear;
    FPdbSymbols.Clear;
    FPdbStorage.SymConfig := Settings.SymConfig;
  end;
  {$ENDIF}
end;

procedure TfrmCpuViewImpl.UpdateDebugGateSettings;
begin
  inherited UpdateDebugGateSettings;
  {$IFDEF MSWINDOWS}
  FPdbStorage.SymConfig := Settings.SymConfig;
  {$ENDIF}
end;

end.

