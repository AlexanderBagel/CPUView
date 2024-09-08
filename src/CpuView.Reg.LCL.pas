unit CpuView.Reg.LCL;

interface

{$mode objfpc}{$H+}

{$I CpuViewCfg.inc}

uses
  LCLType,
  LCLIntf,
  Forms,
  Classes,
  IDECommands,
  MenuIntf,
  BaseDebugManager,
  IdeDebuggerBase,

  {$IFDEF USE_INTEL_CTX}
  dlgCpuViewIntel,
  {$ENDIF}
  dlgCpuView;

  procedure DoRegister;

implementation

procedure StartCpuView(Sender: TObject);
begin
  if DebugBoss = nil then Exit;
  if frmCpuView = nil then
    {$IFDEF USE_INTEL_CTX}
    frmCpuView := TfrmCpuViewIntel.Create(DebugBoss);
    {$ELSE}
    frmCpuView := TfrmCpuView.Create(DebugBoss);
    {$ENDIF}
  frmCpuView.BringToFront;
  frmCpuView.Show;
end;

procedure DoRegister;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  CmdCpuView: TIDECommand;
begin
  Key := IDEShortCut(VK_C, [ssAlt, ssCtrl],VK_UNKNOWN,[]);
  Cat := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  CmdCpuView := RegisterIDECommand(Cat, 'CPU-View', '', Key, nil, @StartCpuView);
  RegisterIDEMenuCommand(itmViewDebugWindows, 'CPU-View', 'CPU-View', nil, nil, CmdCpuView);
end;


end.
