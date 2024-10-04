{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CPUView_D;

{$warn 5023 off : no warning about unused units}
interface

uses
  CpuView.Actions, CpuView.Common, CpuView.Core, CpuView.CPUContext, 
  CpuView.DebugerGate, CpuView.FpDebug, CpuView.Reg.LCL, CpuView.Reg, 
  CpuView.ScriptExecutor, CpuView.Settings, CpuView.Stream, CpuView.Viewers, 
  dlgCpuView, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CpuView.Reg', @CpuView.Reg.Register);
end;

initialization
  RegisterPackage('CPUView_D', @Register);
end.
