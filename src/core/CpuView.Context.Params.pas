////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Context.Params.pas
//  * Purpose   : Common context settings.
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2025.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//

unit CpuView.Context.Params;

interface

{$I CpuViewCfg.inc}

uses
  {$IFDEF USE_INTEL_CTX}
  CpuView.Context.Intel.Types;
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  CpuView.Context.Aarch64.Types;
  {$ENDIF}

var
  ContextQueryParams: TContextQueryParams;

  function GetThreadStackPoint(AThreadID: Cardinal; IsProcess64: Boolean): Int64;

implementation

function GetThreadStackPoint(AThreadID: Cardinal; IsProcess64: Boolean): Int64;
begin
  {$IFDEF USE_INTEL_CTX}
  if IsProcess64 then
    Result := ContextQueryParams.GetDefContext(AThreadID).Rsp
  else
    Result := ContextQueryParams.Get32Context(AThreadID).Rsp;
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  if IsProcess64 then
    Result := ContextQueryParams.GetDefContext(AThreadID).Sp
  else
    Result := ContextQueryParams.Get32Context(AThreadID).Sp;
  {$ENDIF}
end;

end.
