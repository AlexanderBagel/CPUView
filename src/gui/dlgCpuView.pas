unit dlgCpuView;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ComCtrls, ActnList, Generics.Collections,

  FWHexView,
  FWHexView.Actions,
  FWHexView.MappedView,

  CpuView.Core,
  CpuView.CPUContext,
  CpuView.Viewers,
  CpuView.DebugerGate,
  CpuView.FpDebug,
  CpuView.Actions;

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
    acDbgRun: TAction;
    acDbgPause: TAction;
    acDbgStepIn: TAction;
    acDbgStepOut: TAction;
    acDbgRunTilReturn: TAction;
    acDbgRunTo: TAction;
    acDbgToggleBp: TAction;
    acRegModifyInc: TAction;
    acRegModifyDec: TAction;
    acRegModifyToggle: TAction;
    acRegModifyZero: TAction;
    acRegModifyNewValue: TAction;
    acViewGoto: TAction;
    acViewFitColumnToBestSize: TAction;
    ActionList: TActionList;
    AsmView: TAsmView;
    Button1: TButton;
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
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
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
    miAsmSep1: TMenuItem;
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
    RegView: TRegView;
    miRegSep1: TMenuItem;
    miAsmSep2: TMenuItem;
    splitAsmDumps: TSplitter;
    splitDumpStack: TSplitter;
    splitAsmReg: TSplitter;
    StackView: TStackView;
    tabDump0: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure acDbgPauseExecute(Sender: TObject);
    procedure acDbgPauseUpdate(Sender: TObject);
    procedure acDbgRunExecute(Sender: TObject);
    procedure acDbgRunTilReturnExecute(Sender: TObject);
    procedure acDbgRunToExecute(Sender: TObject);
    procedure acDbgRunToUpdate(Sender: TObject);
    procedure acDbgRunUpdate(Sender: TObject);
    procedure acDbgStepInExecute(Sender: TObject);
    procedure acDbgStepOutExecute(Sender: TObject);
    procedure acDbgToggleBpExecute(Sender: TObject);
    procedure acHighlightRegExecute(Sender: TObject);
    procedure acHighlightRegUpdate(Sender: TObject);
    procedure acRegModifyDecExecute(Sender: TObject);
    procedure acRegModifyIncExecute(Sender: TObject);
    procedure acRegModifyNewValueExecute(Sender: TObject);
    procedure acRegModifyToggleExecute(Sender: TObject);
    procedure acRegModifyZeroExecute(Sender: TObject);
    procedure acShowInAsmExecute(Sender: TObject);
    procedure acShowInAsmUpdate(Sender: TObject);
    procedure acShowInDumpExecute(Sender: TObject);
    procedure acShowInDumpUpdate(Sender: TObject);
    procedure acShowInStackExecute(Sender: TObject);
    procedure acShowInStackUpdate(Sender: TObject);
    procedure acStackFollowRSPExecute(Sender: TObject);
    procedure acTEAnsiExecute(Sender: TObject);
    procedure acTEAnsiUpdate(Sender: TObject);
    procedure ActionRegModifyUpdate(Sender: TObject);
    procedure acViewFitColumnToBestSizeExecute(Sender: TObject);
    procedure acViewGotoExecute(Sender: TObject);
    procedure acViewGotoUpdate(Sender: TObject);
    procedure AsmViewJmpTo(Sender: TObject; const AJmpAddr: Int64;
      AJmpState: TJmpState; var Handled: Boolean);
    procedure AsmViewSelectionChange(Sender: TObject);
    procedure DumpViewSelectionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pmRegSelectedPopup(Sender: TObject);
    procedure RegViewSelectedContextPopup(Sender: TObject; MousePos: TPoint;
      RowIndex: Int64; ColIndex: Integer; var Handled: Boolean);
    procedure RegViewSelectionChange(Sender: TObject);
    procedure StackViewSelectionChange(Sender: TObject);
  private
    FCore: TCpuViewCore;
    FDbgGate: TCpuViewDebugGate;
    FAsmViewSelectedAddr,
    FContextRegValue,
    FDumpSelectedValue,
    FStackSelectedValue: UInt64;
    FContextRegName: string;
    FContextRegister: TRegister;
    FContextRegisterParam: TRegParam;
    FJmpStack: TStack<Int64>;
    function ActiveViewerSelectedValue: UInt64;
  protected
    function ActiveDumpView: TDumpView;
    function ActiveViewIndex: Integer;
    procedure InitContext; virtual; abstract;
  public
    property Core: TCpuViewCore read FCore;
    property DbgGate: TCpuViewDebugGate read FDbgGate;
  end;

var
  frmCpuView: TfrmCpuView;

implementation

uses
  Math,
  dlgInputBox;

{$R *.lfm}

{ TfrmCpuView }

procedure TfrmCpuView.FormCreate(Sender: TObject);
begin
  FCore := TCpuViewCore.Create;
  FDbgGate := TCpuViewDebugGate.Create(Self);
  InitContext;
  FCore.Debugger := FDbgGate;
  FCore.AsmView := AsmView;
  FCore.RegView := RegView;
  FCore.DumpView := DumpView;
  FCore.StackView := StackView;
  FJmpStack := TStack<Int64>.Create;
end;

procedure TfrmCpuView.FormDestroy(Sender: TObject);
begin
  FDbgGate.Context := nil;
  FCore.Free;
  FDbgGate.Free;
  FJmpStack.Free;
end;

procedure TfrmCpuView.pmRegSelectedPopup(Sender: TObject);
begin
  {$message 'Если не примут патч - забить и реализовать его прямо тут'}
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
end;

procedure TfrmCpuView.StackViewSelectionChange(Sender: TObject);
begin
  FStackSelectedValue := 0;
  StackView.ReadDataAtSelStart(FStackSelectedValue, FDbgGate.PointerSize);
end;

function TfrmCpuView.ActiveViewerSelectedValue: UInt64;
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

function TfrmCpuView.ActiveDumpView: TDumpView;
begin
  Result := DumpView;
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

procedure TfrmCpuView.ActionRegModifyUpdate(Sender: TObject);
begin
  TAction(Sender).Visible :=
    (FDbgGate.DebugState = adsPaused) and
    (FContextRegister.RegID >= 0) and
    (TModifyAction(TAction(Sender).Tag) in FContextRegisterParam.ModifyActions);
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
  AViewIndex: Integer;
  NewAddress: UInt64;
begin
  AViewIndex := ActiveViewIndex;
  if AViewIndex in [-1, 1] then Exit;
  NewAddress := 0;
  if QueryAddress('Go to Address', 'Address:', NewAddress,
    function({%H-}ANewAddrVA: UInt64): Boolean
    begin
      case AViewIndex of
        0: Result := FCore.AddrInAsm(ANewAddrVA);
        2: Result := FCore.AddrInStack(ANewAddrVA);
        3: Result := FCore.AddrInDump(ANewAddrVA);
      else
        Result := False;
      end
    end) then
      case AViewIndex of
        0:
        begin
          FCore.ShowDisasmAtAddr(NewAddress);
          AsmView.FocusOnAddress(NewAddress, ccmSelectRow);
        end;
        2:
        begin
          FCore.ShowStackAtAddr(NewAddress);
          StackView.FocusOnAddress(NewAddress, ccmSelectRow);
        end;
        3:
        begin
          FCore.ShowDumpAtAddr(NewAddress);
          ActiveDumpView.FocusOnAddress(NewAddress, ccmSetNewSelection);
        end;
      end;
end;

procedure TfrmCpuView.acViewGotoUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DbgGate.DebugState = adsPaused;
end;

procedure TfrmCpuView.AsmViewJmpTo(Sender: TObject; const AJmpAddr: Int64;
  AJmpState: TJmpState; var Handled: Boolean);
var
  NewJmpAddr: Int64;
begin
  case AJmpState of
    jsPushToUndo:
    begin
      Handled := Core.AddrInAsm(AJmpAddr);
      if not Handled then Exit;
      FJmpStack.Push(AsmView.SelectedInstructionAddr);
      FJmpStack.Push(AsmView.RowToAddress(AsmView.CurrentVisibleRow, 0));
      FCore.ShowDisasmAtAddr(AJmpAddr);
    end;
    jsPopFromUndo:
    begin
      Handled := True;
      if FJmpStack.Count = 0 then Exit;
      NewJmpAddr := FJmpStack.Pop;
      FCore.ShowDisasmAtAddr(NewJmpAddr);
      NewJmpAddr := FJmpStack.Pop;
      AsmView.FocusOnAddress(NewJmpAddr, ccmSelectRow);
    end;
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
end;

procedure TfrmCpuView.acShowInDumpUpdate(Sender: TObject);
begin
  if RegView.Focused then
  begin
    if not ((FContextRegister.RegID >= 0) and
      (maChange in FContextRegisterParam.ModifyActions) and
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

procedure TfrmCpuView.acStackFollowRSPExecute(Sender: TObject);
begin
  Core.ShowStackAtAddr(DbgGate.Context.StackPoint);
  ActiveControl := StackView;
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
end;

procedure TfrmCpuView.acShowInDumpExecute(Sender: TObject);
begin
  Core.ShowDumpAtAddr(ActiveViewerSelectedValue);
  ActiveDumpView.SelStart := ActiveViewerSelectedValue;
  ActiveDumpView.SelEnd := ActiveViewerSelectedValue + FDbgGate.PointerSize - 1;
  ActiveControl := ActiveDumpView;
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
    begin
      if QueryAddress('Edit ' + FContextRegName, 'New value:', FContextRegValue,
        function({%H-}ANewAddrVA: UInt64): Boolean
        begin
          Result := True;
        end) then
          FCore.UpdateRegValue(FContextRegister.RegID, FContextRegValue);
    end;
    crtSetValue:
    begin
      ACount := FDbgGate.Context.RegSetValueCount(FContextRegister.RegID);
      if ACount = 0 then Exit;
      SetLength(SetValues{%H-}, ACount);
      for I := 0 to ACount - 1 do
        SetValues[I] := FDbgGate.Context.RegSetValueAtIndex(FContextRegister.RegID, I);
      I := Integer(FContextRegValue);
      if QuerySetList('Edit ' + FContextRegName, 'New value:', SetValues, I) then
      begin
        FContextRegValue := UInt64(I);
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

procedure TfrmCpuView.acHighlightRegExecute(Sender: TObject);
begin
  if AsmView.HighlightReg = FContextRegName then
    AsmView.HighlightReg := ''
  else
    AsmView.HighlightReg := FContextRegName;
end;

procedure TfrmCpuView.acDbgRunExecute(Sender: TObject);
begin
  DbgGate.Run;
end;

procedure TfrmCpuView.acDbgRunTilReturnExecute(Sender: TObject);
begin
  DbgGate.TraceTilReturn;
end;

procedure TfrmCpuView.acDbgRunToExecute(Sender: TObject);
begin
  DbgGate.TraceTo(FAsmViewSelectedAddr);
end;

procedure TfrmCpuView.acDbgRunToUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (DbgGate.DebugState = adsPaused) and
    Core.AddrInAsm(FAsmViewSelectedAddr);
end;

procedure TfrmCpuView.acDbgPauseUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DbgGate.DebugState = adsRunning;
end;

procedure TfrmCpuView.acDbgPauseExecute(Sender: TObject);
begin
  DbgGate.Pause;
end;

procedure TfrmCpuView.acDbgRunUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := DbgGate.DebugState <> adsRunning;
end;

procedure TfrmCpuView.acDbgStepInExecute(Sender: TObject);
begin
  DbgGate.TraceIn;
end;

procedure TfrmCpuView.acDbgStepOutExecute(Sender: TObject);
begin
  DbgGate.TraceOut;
end;

procedure TfrmCpuView.acDbgToggleBpExecute(Sender: TObject);
begin
  FAsmViewSelectedAddr := AsmView.SelectedInstructionAddr;
  DbgGate.ToggleBreakPoint(FAsmViewSelectedAddr);
end;

end.

