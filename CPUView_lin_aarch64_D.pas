{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CPUView_lin_aarch64_D;

{$warn 5023 off : no warning about unused units}
interface

uses
  CpuView.Actions, CpuView.Common, CpuView.CPUContext, CpuView.Viewers, 
  CpuView.XML, CpuView.CommonDebug, CpuView.Context.Aarch64.Types, 
  CpuView.Context.Aarch64, CpuView.Context.Params, CpuView.Core, 
  CpuView.DBase, CpuView.DebugerGate, CpuView.ExtendedHint, CpuView.GdbDebug, 
  CpuView.GdbDebug.Aarch64, CpuView.Linux, CpuView.Linux.MMap, 
  CpuView.ScriptExecutor, CpuView.ScriptExecutor.Aarch64, CpuView.Settings, 
  CpuView.Stream, CpuView.TraceLog, CpuView.Reg, CpuView.Design.Common, 
  CpuView.Design.CrashDump, CpuView.Design.DbgLog, dlgCpuView, 
  frmCpuViewBaseOptions, frmCpuViewColors, frmCpuViewOptions, 
  frmCpuViewShortCuts, dlgCpuViewImplementation, dlgCpuView.TemporaryLocker, 
  dlgInputBox, dlgTraceLog, dlgSimd87Editor, dlgProcExports, dlgMemoryMap, 
  CpuView.Design.DpiFix, dlgPageAccess, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CpuView.Reg', @CpuView.Reg.Register);
end;

initialization
  RegisterPackage('CPUView_lin_aarch64_D', @Register);
end.
