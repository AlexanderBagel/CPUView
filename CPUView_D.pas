{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CPUView_D;

{$warn 5023 off : no warning about unused units}
interface

uses
  dlgCpuView, dlgCpuViewIntel, dlgInputBox, CpuView.Actions, CpuView.Common, 
  CpuView.Core, CpuView.CPUContext, CpuView.DebugerGate, CpuView.FpDebug, 
  CpuView.IntelContext, CpuView.IntelContext.Types, CpuView.Reg.LCL, 
  CpuView.Reg, CpuView.ScriptExecutor.Intel, CpuView.ScriptExecutor, 
  CpuView.Settings, CpuView.Stream, CpuView.Viewers, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CpuView.Reg', @CpuView.Reg.Register);
end;

initialization
  RegisterPackage('CPUView_D', @Register);
end.
