 ////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.AllUnits.pas
//  * Purpose   : All units enum for Lazarus.
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//

unit CpuView.AllUnits;

{$mode ObjFPC}{$H+}

interface

 {$I CpuViewCfg.inc}

 uses
  CpuView.Actions,
  CpuView.Common,
  CpuView.CPUContext,
  CpuView.Viewers,
  CpuView.XML,
  CpuView.Core,
  CpuView.DBase,
  CpuView.DebugerGate,
  CpuView.FpDebug,
  CpuView.IntelContext,
  CpuView.IntelContext.Types,
  CpuView.Settings,
  CpuView.ScriptExecutor,
  CpuView.Stream,

  {$IFDEF LINUX}
  CpuView.Linux,
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  CpuView.Windows,
  {$ENDIF}

  {$IFDEF USE_INTEL_CTX}
  CpuView.ScriptExecutor.Intel,
  dlgCpuViewImplementation,
  {$ENDIF}

  CpuView.Reg,
  CpuView.Design.Common,
  CpuView.Design.CrashDump,
  CpuView.Design.DbgLog,

  dlgCpuView,
  dlgCpuView.TemporaryLocker,
  dlgInputBox,
  frmCpuViewOptions;

implementation

end.
