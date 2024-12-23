{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CPUView_D;

{$warn 5023 off : no warning about unused units}
interface

uses
  CpuView.AllUnits, CpuView.Actions, CpuView.Common, CpuView.CPUContext, 
  CpuView.Viewers, CpuView.XML, CpuView.Core, CpuView.DBase, 
  CpuView.DebugerGate, CpuView.ExtendedHint, CpuView.FpDebug, 
  CpuView.Settings, CpuView.ScriptExecutor, CpuView.Stream, CpuView.Reg, 
  CpuView.Design.Common, CpuView.Design.CrashDump, CpuView.Design.DbgLog, 
  dlgCpuView, frmCpuViewBaseOptions, frmCpuViewColors, frmCpuViewOptions, 
  frmCpuViewShortCuts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CpuView.Reg', @CpuView.Reg.Register);
end;

initialization
  RegisterPackage('CPUView_D', @Register);
end.
