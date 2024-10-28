{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CPUView_D;

{$warn 5023 off : no warning about unused units}
interface

uses
  CpuView.Actions, CpuView.Common, CpuView.CPUContext, CpuView.Settings, 
  CpuView.Viewers, CpuView.XML, CpuView.Core, CpuView.DBase, 
  CpuView.DebugerGate, CpuView.FpDebug, CpuView.ScriptExecutor, 
  CpuView.Stream, CpuView.Reg, CpuView.Design.Common, dlgCpuView, 
  frmCpuViewOptions, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CpuView.Reg', @CpuView.Reg.Register);
end;

initialization
  RegisterPackage('CPUView_D', @Register);
end.
