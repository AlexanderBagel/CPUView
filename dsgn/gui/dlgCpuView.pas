////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgCpuView.pas
//  * Purpose   : Basic GUI debugger class without CPU-specific implementation code
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

unit dlgCpuView;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Menus, ComCtrls, ActnList, Clipbrd,

  IDEImagesIntf,
  BaseDebugManager,

  FWHexView,
  FWHexView.Actions,
  FWHexView.MappedView,

  CpuView.Core,
  CpuView.CPUContext,
  CpuView.Viewers,
  CpuView.DebugerGate,
  CpuView.FpDebug,
  CpuView.Actions,
  CpuView.Settings,
  CpuView.Design.Common,
  CpuView.Design.CrashDump,
  CpuView.Design.DbgLog,

  dlgCpuView.TemporaryLocker,
  dlgInputBox,

  uni_profiler;

type

  { TfrmCpuView }

  TfrmCpuView = class(TForm)
    acShowInDump: TAction;
    acShowInStack: TAction;
    acShowInAsm: TAction;
    acHighlightReg: TAction;
    acStackFollowRSP: TAction;
    acTEAnsi: TAction;
    acTEAscii: TAction;
    acTEUnicode: TAction;
    acTEUnicodeBE: TAction;
    acTEUtf7: TAction;
    acTEUtf8: TAction;
    acDbgStepIn: TAction;
    acDbgStepOver: TAction;
    acDbgStepOut: TAction;
    acDbgRunTo: TAction;
    acDbgToggleBp: TAction;
    acRegModifyInc: TAction;
    acRegModifyDec: TAction;
    acRegModifyToggle: TAction;
    acRegModifyZero: TAction;
    acRegModifyNewValue: TAction;
    acAsmReturnToIP: TAction;
    acAsmSetNewIP: TAction;
    acAsmShowSource: TAction;
    acShowInNewDump: TAction;
    acDumpsClosePage: TAction;
    acDumpsCloseAllToTheRight: TAction;
    acStackFollowRBP: TAction;
    acDbgRunToUserCode: TAction;
    acSBCopyPanelText: TAction;
    acSBCopyScriptorValue: TAction;
    acSaveRawDump: TAction;
    acViewGoto: TAction;
    acViewFitColumnToBestSize: TAction;
    ActionList: TActionList;
    AsmView: TAsmView;
    acVmHex: TCpuContextRegViewModeAction;
    acVmHexW: TCpuContextRegViewModeAction;
    acVmHexD: TCpuContextRegViewModeAction;
    acVmHexQ: TCpuContextRegViewModeAction;
    acVmOct: TCpuContextRegViewModeAction;
    acVmBin: TCpuContextRegViewModeAction;
    acVmIntB: TCpuContextRegViewModeAction;
    acVmUIntB: TCpuContextRegViewModeAction;
    acVmIntW: TCpuContextRegViewModeAction;
    acVmIntD: TCpuContextRegViewModeAction;
    acVmIntQ: TCpuContextRegViewModeAction;
    acVmUIntW: TCpuContextRegViewModeAction;
    acVmUIntD: TCpuContextRegViewModeAction;
    acVmUIntQ: TCpuContextRegViewModeAction;
    acVmFloat32: TCpuContextRegViewModeAction;
    acVmFloat64: TCpuContextRegViewModeAction;
    acVmFloat80: TCpuContextRegViewModeAction;
    DumpView: TDumpView;
    edCommands: TEdit;
    acCopy: THexViewCopyAction;
    acCopyBytes: THexViewCopyAction;
    acCopyAddress: THexViewCopyAction;
    acCopyPas: THexViewCopyAction;
    acCopyAsm: THexViewCopyAction;
    acDMHex8: THexViewByteViewModeAction;
    acDMHex16: THexViewByteViewModeAction;
    acDMHex32: THexViewByteViewModeAction;
    acDMHex64: THexViewByteViewModeAction;
    acDMInt8: THexViewByteViewModeAction;
    acDMInt16: THexViewByteViewModeAction;
    acDMInt32: THexViewByteViewModeAction;
    acDMInt64: THexViewByteViewModeAction;
    acDMUInt8: THexViewByteViewModeAction;
    acDMUInt16: THexViewByteViewModeAction;
    acDMUInt32: THexViewByteViewModeAction;
    acDMUInt64: THexViewByteViewModeAction;
    acDMFloat32: THexViewByteViewModeAction;
    acDMFloat64: THexViewByteViewModeAction;
    acDMFloat80: THexViewByteViewModeAction;
    acDMAddress: THexViewByteViewModeAction;
    acDMText: THexViewByteViewModeAction;
    memHints: TMemo;
    miStackDump: TMenuItem;
    miDumpSave: TMenuItem;
    miAsmDump: TMenuItem;
    miSBCopyText: TMenuItem;
    miSBCopyValue: TMenuItem;
    miStackFollowRbp: TMenuItem;
    miProfilerSaveDump: TMenuItem;
    miResetProfiler: TMenuItem;
    miDebugGenException: TMenuItem;
    miDumpsCloseRight: TMenuItem;
    miAsmRunTo: TMenuItem;
    miStackShowInNewDump: TMenuItem;
    miDumpShowInNewDump: TMenuItem;
    miAsmShowInNewDump: TMenuItem;
    miRegShowInNewDump: TMenuItem;
    miDumpsClosePage: TMenuItem;
    miStackShowInStack: TMenuItem;
    miDumpShowInDump: TMenuItem;
    miAsmSource: TMenuItem;
    miAsmCurrentIP: TMenuItem;
    miAsmSetNewIP: TMenuItem;
    miStackFollowRsp: TMenuItem;
    miStackGoto: TMenuItem;
    miDumpGoto: TMenuItem;
    miAsmGoto: TMenuItem;
    miRegFit: TMenuItem;
    miRegSep5: TMenuItem;
    miStackSep2: TMenuItem;
    miStackFit: TMenuItem;
    miDumpFit: TMenuItem;
    miDumpSep4: TMenuItem;
    miAsmFit: TMenuItem;
    miRegChangeValue: TMenuItem;
    miRegZeroValue: TMenuItem;
    miRegToggleFlag: TMenuItem;
    miRegDecValue: TMenuItem;
    miRegIncValue: TMenuItem;
    miTEAnsi: TMenuItem;
    miTEAscii: TMenuItem;
    miTEUnicode: TMenuItem;
    miTEUnicodeBE: TMenuItem;
    miTEUtf7: TMenuItem;
    miTEUtf8: TMenuItem;
    miDumpTextEncoding: TMenuItem;
    miRegDMHex: TMenuItem;
    miRegDMIntQ: TMenuItem;
    miRegDMUIntB: TMenuItem;
    miRegDMUIntW: TMenuItem;
    miRegDMUIntD: TMenuItem;
    miRegDMUIntQ: TMenuItem;
    miRegDMFloat32: TMenuItem;
    miRegDMFloat64: TMenuItem;
    miRegDMFloat80: TMenuItem;
    miRegCopy: TMenuItem;
    miRegCopyValue: TMenuItem;
    miDumpDMFloat80: TMenuItem;
    miDumpDMAddress: TMenuItem;
    miStackShowInAsm: TMenuItem;
    miStackShowInDump: TMenuItem;
    miStackCopyAddr: TMenuItem;
    miStackCopy: TMenuItem;
    miStackCopyValue: TMenuItem;
    miAsmShowInDump: TMenuItem;
    miAsmShowInStack: TMenuItem;
    miAsmCopyAddr: TMenuItem;
    miAsmCopy: TMenuItem;
    miDumpShowInAsm: TMenuItem;
    miDumpShowInStack: TMenuItem;
    miDumpDisplayMode: TMenuItem;
    miDumpDMHex: TMenuItem;
    miDumpDMHexW: TMenuItem;
    miDumpDMHexD: TMenuItem;
    miDumpDMHexQ: TMenuItem;
    miDumpDMIntB: TMenuItem;
    miDumpDMIntW: TMenuItem;
    miDumpDMIntD: TMenuItem;
    miDumpDMIntQ: TMenuItem;
    miDumpDMUIntB: TMenuItem;
    miDumpDMUIntW: TMenuItem;
    miDumpDMUIntD: TMenuItem;
    miDumpDMUIntQ: TMenuItem;
    miDumpDMFloat32: TMenuItem;
    miDumpDMFloat64: TMenuItem;
    miDumpDMText: TMenuItem;
    miDumpCopyAddr: TMenuItem;
    miDumpCopy: TMenuItem;
    miDumpCopyBytes: TMenuItem;
    miDumpCopyPas: TMenuItem;
    miDumpCopyAsm: TMenuItem;
    miDumpSep3: TMenuItem;
    miDumpSep2: TMenuItem;
    miDumpDMSep4: TMenuItem;
    miDumpDMSep3: TMenuItem;
    miDumpDMSep2: TMenuItem;
    miDumpDMSep1: TMenuItem;
    miDumpSep1: TMenuItem;
    miAsmSep2: TMenuItem;
    pnDumpStack: TPanel;
    pmDump: TPopupMenu;
    pmAsm: TPopupMenu;
    miStackSep1: TMenuItem;
    pmStack: TPopupMenu;
    miRegSep3: TMenuItem;
    miRegHighlight: TMenuItem;
    miRegSep2: TMenuItem;
    miRegShowInDump: TMenuItem;
    miRegShowInStack: TMenuItem;
    miRegShowInAsm: TMenuItem;
    miRegSep4: TMenuItem;
    miRegDMSep4: TMenuItem;
    miRegDMSep3: TMenuItem;
    miRegDMHexW: TMenuItem;
    miRegDMHexD: TMenuItem;
    miRegDMHexQ: TMenuItem;
    miRegDMOctal: TMenuItem;
    miRegDMBin: TMenuItem;
    miRegDMIntB: TMenuItem;
    miRegDMIntW: TMenuItem;
    miRegDMIntD: TMenuItem;
    miRegDMSep2: TMenuItem;
    miRegDMSep1: TMenuItem;
    miRegDisplayMode: TMenuItem;
    pcDumps: TPageControl;
    pnDumps: TPanel;
    pnAsmReg: TPanel;
    pnDebug: TPanel;
    pmRegSelected: TPopupMenu;
    pmHint: TPopupMenu;
    pmDumps: TPopupMenu;
    pmDebug: TPopupMenu;
    pmStatusBar: TPopupMenu;
    RegView: TRegView;
    miRegSep1: TMenuItem;
    miAsmSep3: TMenuItem;
    miAsmSep1: TMenuItem;
    miAsmSep0: TMenuItem;
    miDumpSep0: TMenuItem;
    miStackSep0: TMenuItem;
    SaveDialog: TSaveDialog;
    splitAsmDumps: TSplitter;
    splitDumpStack: TSplitter;
    splitAsmReg: TSplitter;
    StackView: TStackView;
    StatusBar: TStatusBar;
    tabDump0: TTabSheet;
    tmpZOrderLock: TTimer;
    ToolBar: TToolBar;
    tbStepIn: TToolButton;
    tbStepOut: TToolButton;
    tbSep1: TToolButton;
    tbBreakPoint: TToolButton;
    tbSep2: TToolButton;
    tbRunTillRet: TToolButton;
    tbRunTo: TToolButton;
    ToolButton1: TToolButton;
    procedure acAsmReturnToIPExecute(Sender: TObject);
    procedure acAsmReturnToIPUpdate(Sender: TObject);
    procedure acAsmSetNewIPExecute(Sender: TObject);
    procedure acAsmShowSourceExecute(Sender: TObject);
    procedure acAsmShowSourceUpdate(Sender: TObject);
    procedure acDbgRunToUserCodeExecute(Sender: TObject);
    procedure acDbgRunToUserCodeUpdate(Sender: TObject);
    procedure acDbgStepOutExecute(Sender: TObject);
    procedure acDbgRunToExecute(Sender: TObject);
    procedure acDbgRunToUpdate(Sender: TObject);
    procedure acDbgStepInExecute(Sender: TObject);
    procedure acDbgStepOverExecute(Sender: TObject);
    procedure acDbgToggleBpExecute(Sender: TObject);
    procedure acDumpsCloseAllToTheRightExecute(Sender: TObject);
    procedure acDumpsCloseAllToTheRightUpdate(Sender: TObject);
    procedure acDumpsClosePageExecute(Sender: TObject);
    procedure acDumpsClosePageUpdate(Sender: TObject);
    procedure acHighlightRegExecute(Sender: TObject);
    procedure acHighlightRegUpdate(Sender: TObject);
    procedure acRegModifyDecExecute(Sender: TObject);
    procedure acRegModifyIncExecute(Sender: TObject);
    procedure acRegModifyNewValueExecute(Sender: TObject);
    procedure acRegModifyToggleExecute(Sender: TObject);
    procedure acRegModifyZeroExecute(Sender: TObject);
    procedure acSaveRawDumpExecute(Sender: TObject);
    procedure acSaveRawDumpUpdate(Sender: TObject);
    procedure acSBCopyPanelTextExecute(Sender: TObject);
    procedure acSBCopyScriptorValueExecute(Sender: TObject);
    procedure acShowInAsmExecute(Sender: TObject);
    procedure acShowInAsmUpdate(Sender: TObject);
    procedure acShowInDumpExecute(Sender: TObject);
    procedure acShowInDumpUpdate(Sender: TObject);
    procedure acShowInNewDumpExecute(Sender: TObject);
    procedure acShowInStackExecute(Sender: TObject);
    procedure acShowInStackUpdate(Sender: TObject);
    procedure acStackFollowRBPExecute(Sender: TObject);
    procedure acStackFollowRBPUpdate(Sender: TObject);
    procedure acStackFollowRSPExecute(Sender: TObject);
    procedure acStackFollowRSPUpdate(Sender: TObject);
    procedure acTEAnsiExecute(Sender: TObject);
    procedure acTEAnsiUpdate(Sender: TObject);
    procedure ActionRegModifyUpdate(Sender: TObject);
    procedure acViewFitColumnToBestSizeExecute(Sender: TObject);
    procedure acViewGotoExecute(Sender: TObject);
    procedure AsmViewSelectionChange(Sender: TObject);
    procedure DumpViewSelectionChange(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure miDebugGenExceptionClick(Sender: TObject);
    procedure miProfilerSaveDumpClick(Sender: TObject);
    procedure miResetProfilerClick(Sender: TObject);
    procedure pcDumpsChange(Sender: TObject);
    procedure pmStatusBarPopup(Sender: TObject);
    procedure RegViewDblClick(Sender: TObject);
    procedure RegViewSelectedContextPopup(Sender: TObject; MousePos: TPoint;
      RowIndex: Int64; ColIndex: Integer; var Handled: Boolean);
    procedure RegViewSelectionChange(Sender: TObject);
    procedure StackViewSelectionChange(Sender: TObject);
    procedure tmpZOrderLockTimer(Sender: TObject);
    procedure DefaultActionUpdate(Sender: TObject);
  private
    FCore: TCpuViewCore;
    FContext: TCommonCpuContext;
    FDbgGate: TCpuViewDebugGate;
    FSBPanelText: string;
    FSBPanelValue: string;
    FSettings: TCpuViewSettins;
    FAsmViewSelectedAddr,
    FContextRegValue,
    FDumpSelectedValue,
    FStackSelectedValue: Int64;
    FDumpNameIdx: Integer;
    FContextRegName, FSourcePath: string;
    FContextRegister: TRegister;
    FContextRegisterParam: TRegParam;
    FQweryAddrViewerIndex: Integer;
    FSourceLine: Integer;
    FLastBounds: TRect;
    FCrashDump: TExceptionLogger;
    FExit1ShortCut, FExit2ShortCut: TShortCut;
    function ActiveViewerSelectedValue: Int64;
    function CheckAddressCallback(ANewAddrVA: Int64): Boolean;
    function CheckRegCallback(ANewAddrVA: Int64): Boolean;
    procedure InternalShowInDump(AddrVA: Int64);
  protected
    function ActiveDumpView: TDumpView;
    function ActiveView: TFWCustomHexView;
    function ActiveViewIndex: Integer;
    procedure AfterDbgGateCreate; virtual; abstract;
    procedure BeforeDbgGateDestroy; virtual; abstract;
    function GetContext: TCommonCpuContext; virtual; abstract;
    procedure InitStatusBarValues(APanelIndex: Integer); virtual;
    function ToDpi(Value: Integer): Integer;
    function ToDefaultDpi(Value: Integer): Integer;
    procedure LockZOrder;
    function MeasureCanvas: TBitmap;
    procedure UnlockZOrder;
    procedure UpdateStatusBar;
    property SBPanelText: string read FSBPanelText write FSBPanelText;
    property SBPanelValue: string read FSBPanelValue write FSBPanelValue;
  public
    procedure LoadSettings;
    procedure SaveSettings;
    property Core: TCpuViewCore read FCore;
    property DbgGate: TCpuViewDebugGate read FDbgGate;
    property Settings: TCpuViewSettins read FSettings;
  end;

var
  frmCpuView: TfrmCpuView;

implementation

{$R *.lfm}

{ TfrmCpuView }

procedure TfrmCpuView.FormCreate(Sender: TObject);
begin
  FCrashDump := TExceptionLogger.Create;
  FSettings := TCpuViewSettins.Create;
  FCore := TCpuViewCore.Create(TCpuViewDebugGate);
  FContext := GetContext;
  FDbgGate := FCore.Debugger as TCpuViewDebugGate;
  FDbgGate.Context := FContext;
  AfterDbgGateCreate;
  FCore.AsmView := AsmView;
  FCore.RegView := RegView;
  FCore.DumpViewList.Add(DumpView);
  FCore.StackView := StackView;
  acDbgToggleBp.ImageIndex := IDEImages.LoadImage('ActiveBreakPoint');
  acDbgRunTo.ImageIndex := IDEImages.LoadImage('menu_run_cursor');
  acDbgStepOut.ImageIndex := IDEImages.LoadImage('menu_stepout');
  acDbgStepIn.ImageIndex := IDEImages.LoadImage('menu_stepinto');
  acDbgStepOver.ImageIndex := IDEImages.LoadImage('menu_stepover');
  acAsmReturnToIP.ImageIndex := IDEImages.LoadImage('debugger_show_execution_point');
  acStackFollowRBP.ImageIndex := IDEImages.LoadImage('callstack_goto');
  acStackFollowRSP.ImageIndex := IDEImages.LoadImage('evaluate_up');
  acViewGoto.ImageIndex := IDEImages.LoadImage('address');
  acDumpsClosePage.ImageIndex := IDEImages.LoadImage('menu_exit');
  acDbgRunToUserCode.ImageIndex := IDEImages.LoadImage('menu_run_withdebugging');  ;
  ToolBar.Images := IDEImages.Images_16;
  pmAsm.Images := IDEImages.Images_16;
  pmDump.Images := IDEImages.Images_16;
  pmDumps.Images := IDEImages.Images_16;
  pmRegSelected.Images := IDEImages.Images_16;
  pmStack.Images := IDEImages.Images_16;
  SetHooks;
  LoadSettings;
  CpuViewDebugLog.Reset;
  CpuViewDebugLog.Log('TfrmCpuView: start', True, False);
end;

procedure TfrmCpuView.FormDeactivate(Sender: TObject);
begin
  if tmpZOrderLock.Enabled then
  begin
    SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    BringToFront;
  end;
end;

procedure TfrmCpuView.FormDestroy(Sender: TObject);
begin
  ResetHooks;
  SaveSettings;
  BeforeDbgGateDestroy;
  FDbgGate.Context := nil;
  FContext.Free;
  FCore.Free;
  FSettings.Free;
  FCrashDump.Free;
  CpuViewDebugLog.Log('TfrmCpuView: end', False);
  CpuViewDebugLog.Enabled := False;
end;

procedure TfrmCpuView.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then
    Close;
end;

procedure TfrmCpuView.miDebugGenExceptionClick(Sender: TObject);
begin
  raise Exception.Create('Test exception');
end;

procedure TfrmCpuView.miProfilerSaveDumpClick(Sender: TObject);
begin
  uprof.SaveToFile(DebugFolder + 'profile.txt');
end;

procedure TfrmCpuView.miResetProfilerClick(Sender: TObject);
begin
  uprof.Reset;
end;

procedure TfrmCpuView.pcDumpsChange(Sender: TObject);
begin
  Core.DumpViewList.ItemIndex := pcDumps.PageIndex;
end;

procedure TfrmCpuView.pmStatusBarPopup(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := StatusBar.ScreenToClient(P);
  SBPanelText := '';
  SBPanelValue := '';
  InitStatusBarValues(StatusBar.GetPanelIndexAt(P.X, P.Y));
end;

procedure TfrmCpuView.RegViewDblClick(Sender: TObject);
var
  Handled: Boolean;
begin
  if FDbgGate.DebugState <> adsPaused then Exit;
  FContextRegName := RegView.SelectedRegName;
  FContextRegister := RegView.SelectedRegister;
  if FContextRegister.RegID < 0 then Exit;
  Handled := RegView.Context.RegParam(FContextRegister.RegID, FContextRegisterParam) and
    (RegView.ReadDataAtSelStart(FContextRegValue, SizeOf(FContextRegValue)) > 0);
  if not Handled then Exit;
  if rfToggle in FContextRegisterParam.Flags then
  begin
    acRegModifyToggle.Execute;
    Exit;
  end;
  if rfChangeValue in FContextRegisterParam.Flags then
    acRegModifyNewValue.Execute;
end;

procedure TfrmCpuView.RegViewSelectedContextPopup(Sender: TObject;
  MousePos: TPoint; RowIndex: Int64; ColIndex: Integer; var Handled: Boolean);
begin
  if ColIndex >= 0 then
  begin
    FContextRegValue := 0;
    FContextRegName := RegView.SelectedRegName;
    FContextRegister := RegView.SelectedRegister;
    Handled := RegView.Context.RegParam(FContextRegister.RegID, FContextRegisterParam) and
      (RegView.ReadDataAtSelStart(FContextRegValue, SizeOf(FContextRegValue)) > 0);
    if Handled then
    begin
      MousePos := TWinControl(Sender).ClientToScreen(MousePos);
      pmRegSelected.Popup(MousePos.X, MousePos.Y);
    end;
  end;
end;

procedure TfrmCpuView.RegViewSelectionChange(Sender: TObject);
begin
  FContextRegName := RegView.SelectedRegName;
  FContextRegValue := 0;
  RegView.ReadDataAtSelStart(FContextRegValue, FDbgGate.PointerSize);
  UpdateStatusBar;
end;

procedure TfrmCpuView.StackViewSelectionChange(Sender: TObject);
begin
  FStackSelectedValue := 0;
  StackView.ReadDataAtSelStart(FStackSelectedValue, FDbgGate.PointerSize);
  UpdateStatusBar;
end;

procedure TfrmCpuView.tmpZOrderLockTimer(Sender: TObject);
begin
  UnlockZOrder;
end;

procedure TfrmCpuView.DefaultActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DbgGate.DebugState = adsPaused;
end;

function TfrmCpuView.ActiveViewerSelectedValue: Int64;
begin
  Result := 0;
  if AsmView.Focused then
    Exit(FAsmViewSelectedAddr);
  if ActiveDumpView.Focused then
    Exit(FDumpSelectedValue);
  if RegView.Focused then
    Exit(FContextRegValue);
  if StackView.Focused then
    Result := FStackSelectedValue;
end;

function TfrmCpuView.CheckAddressCallback(ANewAddrVA: Int64): Boolean;
begin
  case FQweryAddrViewerIndex of
    0: Result := FCore.AddrInAsm(ANewAddrVA);
    2: Result := FCore.AddrInStack(ANewAddrVA);
    3: Result := FCore.AddrInDump(ANewAddrVA);
  else
    Result := False;
  end
end;

function TfrmCpuView.CheckRegCallback(ANewAddrVA: Int64): Boolean;
begin
  Result := True;
end;

procedure TfrmCpuView.InternalShowInDump(AddrVA: Int64);
begin
  if ActiveViewIndex = 0 then
    Core.ShowDumpAtAddr(AddrVA, AsmView.SelectedRawLength)
  else
    Core.ShowDumpAtAddr(AddrVA);
  ActiveControl := ActiveDumpView;
end;

procedure TfrmCpuView.UpdateStatusBar;
const
  DbgStates: array [TAbstractDebugState] of string = (
    'Error', 'Stoped', 'Start', 'Paused', 'Running', 'Finished'
  );
var
  AddrVA: Int64;
  AccessStr, Symbol: string;
  AMeasureCanvas: TBitmap;
begin
  StatusBar.Panels[0].Text := Format('Pid: %d, Tid: %d, State: %s',
    [DbgGate.ProcessID, DbgGate.ThreadID, DbgStates[DbgGate.DebugState]]);
  StatusBar.Panels[1].Text := Format('Module: "%s"',
    [ExtractFileName(FCore.QueryModuleName(AsmView.SelectedInstructionAddr))]);
  AddrVA := ActiveViewerSelectedValue;
  AccessStr := FCore.QueryAccessStr(AddrVA);
  Symbol := FCore.QuerySymbolAtAddr(AddrVA);
  StatusBar.Panels[2].Text := Format('Addr:  0x%x (%s) %s', [AddrVA, AccessStr, Symbol]);
  AMeasureCanvas := MeasureCanvas;
  try
    StatusBar.Panels[0].Width :=
      AMeasureCanvas.Canvas.TextWidth(StatusBar.Panels[0].Text) + ToDpi(16);
    StatusBar.Panels[1].Width :=
      AMeasureCanvas.Canvas.TextWidth(StatusBar.Panels[1].Text) + ToDpi(16);
    StatusBar.Panels[2].Width :=
      AMeasureCanvas.Canvas.TextWidth(StatusBar.Panels[2].Text) + ToDpi(16);
  finally
    AMeasureCanvas.Free;
  end;
end;

procedure TfrmCpuView.LoadSettings;

  procedure AddActionShortCut(AAction: TAction; const AShortCut: TCpuViewShortCut);
  begin
    AAction.ShortCut := ShortCut(AShortCut.Key1, AShortCut.Shift1);
    AAction.SecondaryShortCuts.Clear;
    AAction.SecondaryShortCuts.Add(KeyShiftToText(AShortCut.Key2, AShortCut.Shift2));
  end;

  procedure AddViewerShortCut(AViewer: TFWCustomHexView;
    const AShortCut: TCpuViewShortCut; IsJmpIn: Boolean);
  var
    ViewerShortCut: TViewShortCut;
  begin
    if IsJmpIn then
      ViewerShortCut := TFWHexView(AViewer).ShortCuts.JmpTo
    else
      ViewerShortCut := TFWHexView(AViewer).ShortCuts.JmpBack;
    ViewerShortCut.ShortCut := ShortCut(AShortCut.Key1, AShortCut.Shift1);
    ViewerShortCut.SecondaryShortCuts.Clear;
    ViewerShortCut.SecondaryShortCuts.Add(KeyShiftToText(AShortCut.Key2, AShortCut.Shift2));
  end;

var
  R: TRect;
  ExitShortCut: TCpuViewShortCut;
  I: Integer;
begin
  Settings.Load(ConfigPath);
  Settings.SetSettingsToAsmView(AsmView);
  Settings.SetSettingsToDumpView(DumpView);
  for I := 1 to pcDumps.PageCount - 1 do
    Settings.SetSettingsToDumpView(pcDumps.Pages[I].Controls[0] as TDumpView);
  Settings.SetSettingsToRegView(RegView);
  Settings.SetSettingsToContext(DbgGate.Context);
  Settings.SetSettingsToStackView(StackView);
  R := Settings.CpuViewDlgSettings.BoundsRect;
  if R.IsEmpty then
    Position := poScreenCenter
  else
  begin
    Position := poDesigned;
    SetBounds(R.Left, R.Top, ToDpi(R.Width), ToDpi(R.Height));
  end;
  if Settings.CpuViewDlgSettings.Maximized then
    WindowState := wsMaximized
  else
    WindowState := wsNormal;
  RegView.Width := Round(Settings.CpuViewDlgSettings.SplitterPos[spTopHorz] * (ClientWidth / 100));
  StackView.Width := Round(Settings.CpuViewDlgSettings.SplitterPos[spBottomHorz] * (ClientWidth / 100));
  pnDumps.Height := Round(Settings.CpuViewDlgSettings.SplitterPos[spCenterVert] * (ClientHeight / 100));
  pnDebug.Top := pnDumps.Top + pnDumps.Height;

  Core.ExtendedHints := Settings.ExtendedHints;
  Core.ExtendedHintPointerValues := Settings.ExtendedHintPointerValues;

  if (Core.ShowCallFuncName <> Settings.ShowCallFuncName) or
    (Core.ForceFindSymbols <> Settings.ForceFindSymbols) or
    (Core.ForceFindSymbolsDepth <> Settings.ForceFindSymbolsDepth) or
    (Core.DisplayStrings <> Settings.DisplayStrings) or
    (Core.MinimumStringLength <> Settings.MinimumStringLength) or
    (DbgGate.ShowFullAddress <> Settings.ShowFullAddress) or
    (DbgGate.ShowSourceLines <> Settings.ShowSourceLines) or
    (DbgGate.UseDebugInfo <> Settings.UseDebugInfo) then
  begin
    Core.DisplayStrings := Settings.DisplayStrings;
    Core.MinimumStringLength := Settings.MinimumStringLength;
    Core.ShowCallFuncName := Settings.ShowCallFuncName;
    Core.ForceFindSymbols := Settings.ForceFindSymbols;
    Core.ForceFindSymbolsDepth := Settings.ForceFindSymbolsDepth;
    DbgGate.ShowFullAddress := Settings.ShowFullAddress;
    DbgGate.ShowSourceLines := Settings.ShowSourceLines;
    DbgGate.UseDebugInfo := Settings.UseDebugInfo;
    Core.UpdateAfterSettingsChange;
  end;

  FCrashDump.Enabled := Settings.UseCrashDump;
  CpuViewDebugLog.Enabled := Settings.UseDebugLog;

  AddActionShortCut(acDbgToggleBp, Settings.ShotCut[sctToggleBP]);
  AddActionShortCut(acDbgRunTo, Settings.ShotCut[sctRunTo]);
  AddActionShortCut(acDbgStepOut, Settings.ShotCut[sctStepOut]);
  AddActionShortCut(acDbgStepIn, Settings.ShotCut[sctStepIn]);
  AddActionShortCut(acDbgStepOver, Settings.ShotCut[sctStepOver]);
  AddActionShortCut(acAsmSetNewIP, Settings.ShotCut[sctNewIP]);
  AddActionShortCut(acAsmReturnToIP, Settings.ShotCut[sctReturnToDef]);
  AddActionShortCut(acStackFollowRSP, Settings.ShotCut[sctReturnToDef]);
  AddActionShortCut(acDbgRunToUserCode, Settings.ShotCut[sctRunToUser]);

  AddViewerShortCut(AsmView, Settings.ShotCut[sctViewerJmpIn], True);
  AddViewerShortCut(AsmView, Settings.ShotCut[sctViewerStepBack], False);
  AddViewerShortCut(DumpView, Settings.ShotCut[sctViewerJmpIn], True);
  AddViewerShortCut(DumpView, Settings.ShotCut[sctViewerStepBack], False);
  AddViewerShortCut(StackView, Settings.ShotCut[sctViewerJmpIn], True);
  AddViewerShortCut(StackView, Settings.ShotCut[sctViewerStepBack], False);
  AddViewerShortCut(RegView, Settings.ShotCut[sctViewerJmpIn], True);
  AddViewerShortCut(RegView, Settings.ShotCut[sctViewerStepBack], False);

  ExitShortCut := Settings.ShotCut[sctCloseCpuView];
  FExit1ShortCut := ShortCut(ExitShortCut.Key1, ExitShortCut.Shift1);
  FExit2ShortCut := ShortCut(ExitShortCut.Key2, ExitShortCut.Shift2);
end;

procedure TfrmCpuView.SaveSettings;
var
  DlgSettings: TCpuViewDlgSettings;
begin
  if not (Settings.SaveViewersSession or Settings.SaveFormSession) then Exit;
  if Settings.SaveViewersSession then
  begin
    Settings.GetSessionFromAsmView(AsmView);
    Settings.GetSessionFromDumpView(DumpView);
    Settings.GetSessionFromRegView(RegView);
    Settings.GetSessionFromContext(DbgGate.Context);
    Settings.GetSessionFromStackView(StackView);
  end;
  if Settings.SaveFormSession then
  begin
    DlgSettings.BoundsRect := FLastBounds;
    DlgSettings.BoundsRect.Width := ToDefaultDpi(FLastBounds.Width);
    DlgSettings.BoundsRect.Height := ToDefaultDpi(FLastBounds.Height);
    DlgSettings.Maximized := WindowState = wsMaximized;
    DlgSettings.SplitterPos[spTopHorz] := RegView.Width / (ClientWidth / 100);
    DlgSettings.SplitterPos[spBottomHorz] := StackView.Width / (ClientWidth / 100);
    DlgSettings.SplitterPos[spCenterVert] := pnDumps.Height / (ClientHeight / 100);
    Settings.CpuViewDlgSettings := DlgSettings;
  end;
  Settings.Save(ConfigPath);
end;

function TfrmCpuView.ActiveDumpView: TDumpView;
begin
  Result := pcDumps.ActivePage.Controls[0] as TDumpView;
end;

function TfrmCpuView.ActiveView: TFWCustomHexView;
begin
  case ActiveViewIndex of
    0: Result := AsmView;
    1: Result := RegView;
    2: Result := StackView;
    3: Result := ActiveDumpView;
  else
    Result := nil;
  end;
end;

function TfrmCpuView.ActiveViewIndex: Integer;
begin
  Result := -1;
  if AsmView.Focused then
    Exit(0);
  if RegView.Focused then
    Exit(1);
  if StackView.Focused then
    Exit(2);
  if ActiveDumpView.Focused then
    Exit(3);
end;

procedure TfrmCpuView.InitStatusBarValues(APanelIndex: Integer);
begin
  acSBCopyPanelText.Enabled := APanelIndex >= 0;
  if acSBCopyPanelText.Enabled then
    SBPanelText := StatusBar.Panels[APanelIndex].Text;
  if (APanelIndex = 2) and (ActiveViewerSelectedValue <> 0) then
    SBPanelValue := IntToHex(ActiveViewerSelectedValue, 1);
  acSBCopyScriptorValue.Enabled := SBPanelValue <> '';
end;

function TfrmCpuView.ToDpi(Value: Integer): Integer;
begin
  Result := MulDiv(Value, PixelsPerInch, 96);
end;

function TfrmCpuView.ToDefaultDpi(Value: Integer): Integer;
begin
  Result := MulDiv(Value, 96, PixelsPerInch);
end;

procedure TfrmCpuView.LockZOrder;
begin
  InterceptorOwner := Handle;
  InterceptorActive := True;
  tmpZOrderLock.Enabled := True;
end;

function TfrmCpuView.MeasureCanvas: TBitmap;
begin
  Result := TBitmap.Create;
  Result.Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  Result.Canvas.Font := Font;
end;

procedure TfrmCpuView.UnlockZOrder;
begin
  tmpZOrderLock.Enabled := False;
  InterceptorActive := False;
end;

procedure TfrmCpuView.ActionRegModifyUpdate(Sender: TObject);
begin
  TAction(Sender).Visible :=
    (FDbgGate.DebugState = adsPaused) and
    (FContextRegister.RegID >= 0) and
    (TRegisterFlag(TAction(Sender).Tag) in FContextRegisterParam.Flags);
end;

procedure TfrmCpuView.acViewFitColumnToBestSizeExecute(Sender: TObject);
begin
  case ActiveViewIndex of
    0: AsmView.FitColumnsToBestSize;
    1: RegView.FitColumnsToBestSize;
    2: StackView.FitColumnsToBestSize;
    3: ActiveDumpView.FitColumnsToBestSize;
  end;
end;

procedure TfrmCpuView.acViewGotoExecute(Sender: TObject);
var
  NewAddress: Int64;
begin
  FQweryAddrViewerIndex := ActiveViewIndex;
  if (FQweryAddrViewerIndex < 0) or (FQweryAddrViewerIndex = 1) then Exit;
  NewAddress := 0;
  if QueryAddress('Go to Address', 'Address:', NewAddress, CheckAddressCallback) then
    case FQweryAddrViewerIndex of
      0: FCore.ShowDisasmAtAddr(NewAddress);
      2: FCore.ShowStackAtAddr(NewAddress);
      3: InternalShowInDump(NewAddress);
    end;
end;

procedure TfrmCpuView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  frmCpuView := nil;
  CloseAction := caFree;
end;

procedure TfrmCpuView.DumpViewSelectionChange(Sender: TObject);
begin
  FDumpSelectedValue := 0;
  ActiveDumpView.ReadDataAtSelStart(FDumpSelectedValue, FDbgGate.PointerSize);
  UpdateStatusBar;
end;

procedure TfrmCpuView.FormChangeBounds(Sender: TObject);
begin
  if WindowState = wsNormal then
    FLastBounds := BoundsRect;
end;

procedure TfrmCpuView.acShowInDumpUpdate(Sender: TObject);
begin
  if RegView.Focused then
  begin
    if not ((FContextRegister.RegID >= 0) and
      (rfChangeValue in FContextRegisterParam.Flags) and
      (FContextRegisterParam.RegType = crtValue)) then
    begin
      TAction(Sender).Enabled := False;
      Exit;
    end;
  end;
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and
    Core.AddrInDump(ActiveViewerSelectedValue);
end;

procedure TfrmCpuView.acShowInNewDumpExecute(Sender: TObject);
var
  NewPage: TTabSheet;
  NewDump: TDumpView;
  AddrVA: Int64;
begin
  AddrVA := ActiveViewerSelectedValue;
  Inc(FDumpNameIdx);
  NewPage := pcDumps.AddTabSheet;
  NewPage.Caption := 'DUMP' + IntToStr(FDumpNameIdx);
  NewDump := TDumpView.Create(NewPage);
  NewDump.Parent := NewPage;
  NewDump.Align := alClient;
  NewDump.PopupMenu := pmDump;
  NewDump.OnSelectionChange := DumpViewSelectionChange;
  Settings.SetSettingsToDumpView(NewDump);
  Core.DumpViewList.Add(NewDump);
  pcDumps.ActivePage := NewPage;
  Core.DumpViewList.ItemIndex := NewPage.PageIndex;
  InternalShowInDump(AddrVA);
end;

procedure TfrmCpuView.acShowInStackExecute(Sender: TObject);
begin
  Core.ShowStackAtAddr(ActiveViewerSelectedValue);
  ActiveControl := StackView;
end;

procedure TfrmCpuView.acShowInStackUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and
    Core.AddrInStack(ActiveViewerSelectedValue);
end;

procedure TfrmCpuView.acStackFollowRBPExecute(Sender: TObject);
begin
  Core.ShowStackAtAddr(DbgGate.Context.StackBase);
  ActiveControl := StackView;
end;

procedure TfrmCpuView.acStackFollowRBPUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and
    Core.AddrInStack(DbgGate.Context.StackBase);
end;

procedure TfrmCpuView.acStackFollowRSPExecute(Sender: TObject);
begin
  Core.ShowStackAtAddr(DbgGate.Context.StackPoint);
  ActiveControl := StackView;
end;

procedure TfrmCpuView.acStackFollowRSPUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and
    Core.AddrInStack(DbgGate.Context.StackPoint);
end;

procedure TfrmCpuView.acTEAnsiExecute(Sender: TObject);
begin
  ActiveDumpView.Encoder.EncodeType :=
    TCharEncoderType(TAction(Sender).Tag);
end;

procedure TfrmCpuView.acTEAnsiUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := ActiveDumpView.Encoder.EncodeType =
    TCharEncoderType(TAction(Sender).Tag);
end;

procedure TfrmCpuView.AsmViewSelectionChange(Sender: TObject);
begin
  FAsmViewSelectedAddr := AsmView.SelectedInstructionAddr;
  UpdateStatusBar;
end;

procedure TfrmCpuView.acShowInDumpExecute(Sender: TObject);
begin
  InternalShowInDump(ActiveViewerSelectedValue);
end;

procedure TfrmCpuView.acShowInAsmUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and
    Core.AddrInAsm(ActiveViewerSelectedValue);
end;

procedure TfrmCpuView.acShowInAsmExecute(Sender: TObject);
begin
  Core.ShowDisasmAtAddr(ActiveViewerSelectedValue);
  ActiveControl := AsmView;
end;

procedure TfrmCpuView.acHighlightRegUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and (FContextRegName <> '');
end;

procedure TfrmCpuView.acRegModifyDecExecute(Sender: TObject);
begin
  Dec(FContextRegValue);
  FCore.UpdateRegValue(FContextRegister.RegID, FContextRegValue);
end;

procedure TfrmCpuView.acRegModifyIncExecute(Sender: TObject);
begin
  Inc(FContextRegValue);
  FCore.UpdateRegValue(FContextRegister.RegID, FContextRegValue);
end;

procedure TfrmCpuView.acRegModifyNewValueExecute(Sender: TObject);
var
  ACount, I: Integer;
  SetValues: array of string;
begin
  {%H-}case FContextRegister.ValueType of
    crtValue, crtExtra:
      if QueryAddress('Edit ' + FContextRegName, 'New value:', FContextRegValue, CheckRegCallback) then
        FCore.UpdateRegValue(FContextRegister.RegID, FContextRegValue);
    crtEnumValue:
    begin
      ACount := FDbgGate.Context.RegQueryEnumValuesCount(FContextRegister.RegID);
      if ACount = 0 then Exit;
      SetLength(SetValues{%H-}, ACount);
      for I := 0 to ACount - 1 do
        SetValues[I] := FDbgGate.Context.RegQueryEnumString(FContextRegister.RegID, I);
      I := Integer(FContextRegValue);
      if QuerySetList('Edit ' + FContextRegName, 'New value:', SetValues, I) then
      begin
        FContextRegValue := Int64(I);
        FCore.UpdateRegValue(FContextRegister.RegID, FContextRegValue);
      end;
    end;
  end;
end;

procedure TfrmCpuView.acRegModifyToggleExecute(Sender: TObject);
begin
  FContextRegValue := FContextRegValue xor 1;
  FCore.UpdateRegValue(FContextRegister.RegID, FContextRegValue);
end;

procedure TfrmCpuView.acRegModifyZeroExecute(Sender: TObject);
begin
  FContextRegValue := 0;
  FCore.UpdateRegValue(FContextRegister.RegID, FContextRegValue);
end;

procedure TfrmCpuView.acSaveRawDumpExecute(Sender: TObject);
var
  AStream: TFileStream;
begin
  if SaveDialog.Execute then
  begin
    AStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      ActiveView.CopySelectedToStream(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

procedure TfrmCpuView.acSaveRawDumpUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (ActiveViewIndex in [0, 2, 3]) and (ActiveView.SelectedRawLength > 0);
end;

procedure TfrmCpuView.acSBCopyPanelTextExecute(Sender: TObject);
begin
  Clipboard.AsText := SBPanelText;
end;

procedure TfrmCpuView.acSBCopyScriptorValueExecute(Sender: TObject);
begin
  Clipboard.AsText := SBPanelValue;
end;

procedure TfrmCpuView.acHighlightRegExecute(Sender: TObject);
begin
  if AsmView.HighlightReg = FContextRegName then
    AsmView.HighlightReg := ''
  else
    AsmView.HighlightReg := FContextRegName;
end;

procedure TfrmCpuView.acDbgStepOutExecute(Sender: TObject);
begin
  LockZOrder;
  DbgGate.TraceTilReturn;
end;

procedure TfrmCpuView.acAsmShowSourceUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and
    DbgGate.GetSourceLine(FAsmViewSelectedAddr, FSourcePath, FSourceLine) and
    (FSourceLine > 0);
end;

procedure TfrmCpuView.acDbgRunToUserCodeExecute(Sender: TObject);
begin
  LockZOrder;
  Core.TraceToUserCode;
end;

procedure TfrmCpuView.acDbgRunToUserCodeUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (DbgGate.DebugState = adsPaused) and
    DbgGate.CommandAvailable(idcRunToUserCode);
end;

procedure TfrmCpuView.acAsmShowSourceExecute(Sender: TObject);
begin
  DebugBoss.JumpToUnitSource(FSourcePath, FSourceLine, False);
end;

procedure TfrmCpuView.acAsmReturnToIPExecute(Sender: TObject);
begin
  Core.ShowDisasmAtAddr(DbgGate.CurrentInstructionPoint);
end;

procedure TfrmCpuView.acAsmReturnToIPUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (DbgGate.DebugState = adsPaused) and
    (DbgGate.CurrentInstructionPoint <> FAsmViewSelectedAddr);
end;

procedure TfrmCpuView.acAsmSetNewIPExecute(Sender: TObject);
begin
  Core.UpdateRegValue(DbgGate.Context.InstructonPointID, FAsmViewSelectedAddr);
end;

procedure TfrmCpuView.acDbgRunToExecute(Sender: TObject);
begin
  LockZOrder;
  DbgGate.TraceTo(FAsmViewSelectedAddr);
end;

procedure TfrmCpuView.acDbgRunToUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (DbgGate.DebugState = adsPaused) and
    (FAsmViewSelectedAddr <> 0);
end;

procedure TfrmCpuView.acDbgStepInExecute(Sender: TObject);
begin
  LockZOrder;
  DbgGate.TraceIn;
end;

procedure TfrmCpuView.acDbgStepOverExecute(Sender: TObject);
begin
  LockZOrder;
  DbgGate.TraceOut;
end;

procedure TfrmCpuView.acDbgToggleBpExecute(Sender: TObject);
begin
  FAsmViewSelectedAddr := AsmView.SelectedInstructionAddr;
  DbgGate.ToggleBreakPoint(FAsmViewSelectedAddr);
end;

procedure TfrmCpuView.acDumpsCloseAllToTheRightExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := pcDumps.PageCount - 1 downto pcDumps.PageIndex + 1 do
  begin
    Core.DumpViewList.Delete(I);
    pcDumps.Pages[I].Free;
  end;
end;

procedure TfrmCpuView.acDumpsCloseAllToTheRightUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := pcDumps.PageIndex < pcDumps.PageCount - 1;
end;

procedure TfrmCpuView.acDumpsClosePageExecute(Sender: TObject);
begin
  Core.DumpViewList.Delete(pcDumps.PageIndex);
  pcDumps.ActivePage.Free;
  Core.DumpViewList.ItemIndex := pcDumps.PageIndex;
end;

procedure TfrmCpuView.acDumpsClosePageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := pcDumps.PageIndex > 0;
end;

end.

