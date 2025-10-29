{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CPUView_win_x86_64_D;

{$warn 5023 off : no warning about unused units}
interface

uses
  CpuView.Actions, CpuView.Common, CpuView.CPUContext, CpuView.Viewers, 
  CpuView.XML, CpuView.CommonDebug, CpuView.Context.Intel, 
  CpuView.Context.Intel.Types, CpuView.Context.Params, CpuView.Core, 
  CpuView.DBase, CpuView.DebugerGate, CpuView.ExtendedHint, CpuView.FpDebug, 
  CpuView.GdbDebug, CpuView.GdbDebug.Intel, CpuView.ScriptExecutor, 
  CpuView.ScriptExecutor.Intel, CpuView.Settings, CpuView.Stream, 
  CpuView.TraceLog, CpuView.Windows, CpuView.Windows.MMap, CpuView.Reg, 
  CpuView.Design.Common, CpuView.Design.CrashDump, CpuView.Design.DbgLog, 
  dlgCpuView, frmCpuViewBaseOptions, frmCpuViewColors, frmCpuViewOptions, 
  frmCpuViewShortCuts, dlgCpuViewImplementation, dlgCpuView.TemporaryLocker, 
  dlgInputBox, dlgTraceLog, dlgSimd87Editor, dlgProcExports, dlgMemoryMap, 
  dlgPageAccess, CpuView.Design.DpiFix, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CpuView.Reg', @CpuView.Reg.Register);
end;

initialization
  RegisterPackage('CPUView_win_x86_64_D', @Register);
end.
