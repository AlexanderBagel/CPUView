unit CpuView.Reg;

interface

{$I CpuViewCfg.inc}

uses
  Classes,
{$IFDEF FPC}
  ActnList,
  LCLType,
  LCLIntf,
  Forms,
  IDECommands,
  MenuIntf,
  ToolBarIntf,
  BaseDebugManager,
  IdeDebuggerBase,
  IDEOptEditorIntf,
  IDEOptionsIntf,
  IDEImagesIntf,
  dlgCpuView,
  dlgCpuViewImplementation,
  frmCpuViewOptions,
{$ELSE}
  Actions,
  DesignIntf,
  DesignEditors,
{$ENDIF}
  CpuView.Actions,
  CpuView.Viewers;

{$IFNDEF FPC}
type
  TCpuContextActionsSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
{$ENDIF}

  procedure Register;

implementation

{$IFDEF FPC}
procedure StartCpuView(Sender: TObject);
begin
  if DebugBoss = nil then Exit;
  if frmCpuView = nil then
    frmCpuView := TfrmCpuViewImpl.Create(DebugBoss);
  frmCpuView.BringToFront;
  frmCpuView.Show;
end;

procedure DoLCLRegister;
var
  Key: TIDEShortCut;
  ViewCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
  MenuCommand: TIDEMenuCommand;
  ToolBarCategory: TIDEToolButtonCategory;
  //MainEditorID: Integer;
begin
  Key := IDEShortCut(VK_C, [ssAlt, ssCtrl], VK_UNKNOWN, []);
  ViewCategory := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  IDECommand := RegisterIDECommand(ViewCategory, 'CPU-View', '', Key, nil, @StartCpuView);
  MenuCommand := RegisterIDEMenuCommand(itmViewDebugWindows, 'CPU-View', 'CPU-View', nil, nil, IDECommand);
  MenuCommand.ImageIndex := IDEImages.LoadImage('debugger');
  ToolBarCategory := IDEToolButtonCategories.FindCategory(CommandCategoryViewName);
  RegisterIDEButtonCommand(ToolBarCategory, 'CPU-View', IDECommand).ImageIndex := MenuCommand.ImageIndex;
  //MainEditorID := RegisterIDEOptionsEditor(GroupEnvironment,
  //  TCpuViewMainOptionsFrame, 14041979)^.Index;
  RegisterIDEOptionsEditor(GroupEnvironment, TCpuViewMainOptionsFrame, 14041979);
end;
{$ENDIF}

procedure Register;
begin
  {$IFDEF FPC}
  DoLCLRegister;
  {$ELSE}
  ForceDemandLoadState(dlDisable);
  RegisterSelectionEditor(TCpuContextRegViewModeAction, TCpuContextActionsSelectionEditor);
  {$ENDIF}
  RegisterActions('HexView Actions', [TCpuContextRegViewModeAction], nil);
  RegisterComponents('FWControls', [TAsmView, TDumpView, TStackView, TRegView]);
end;

{$IFNDEF FPC}

{ TCpuContextActionsSelectionEditor }

procedure TCpuContextActionsSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('CpuView.Actions');
end;
{$ENDIF}

end.
