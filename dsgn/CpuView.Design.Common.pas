////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Design.Common.pas
//  * Purpose   : Path settings for Lazarus.
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2026.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//  *
//  * SPDX-License-Identifier: MIT
//  * See LICENSE file in the project root for full license information.
//  *
//  ****************************************************************************
//

unit CpuView.Design.Common;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  LazIDEIntf;

  function DebugFolder: string;
  function ConfigPath: string;

implementation

function DebugFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + 'cpuview' + PathDelim;
end;

function ConfigPath: string;
begin
  Result := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + 'cpuview.xml';
end;

end.
