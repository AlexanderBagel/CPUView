unit CpuView.Reg;

interface

uses
  Classes,
  {$IFDEF FPC}
  ActnList,
  CpuView.Reg.LCL,
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

procedure Register;
begin
  {$IFDEF FPC}
  DoRegister;
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
