////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewPdb.pas
//  * Purpose   : PDB Symbols settings for CPU-View.
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

unit frmCpuViewPdb;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  LCLIntf, LCLType, LCLProc,
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ActnList, ExtCtrls,

  IDEOptEditorIntf, IDEImagesIntf,

  frmCpuViewBaseOptions,
  CpuView.Settings,
  CpuView.Windows.Pdb;

type

  { TCpuViewPdbFrame }

  TCpuViewPdbFrame = class(TCpuViewBaseOptionsFrame)
    acEditFirst: TAction;
    acClearFirst: TAction;
    acEditSecond: TAction;
    acClearSecond: TAction;
    btnReset: TButton;
    btnTestProxy: TButton;
    cbUsePdb: TCheckBox;
    cbProxy: TComboBox;
    cbProxyAuth: TComboBox;
    cbDebugPDB: TCheckBox;
    gbPdb: TGroupBox;
    gbProxy: TGroupBox;
    lblProxyAythType: TLabel;
    edProxyAddr: TLabeledEdit;
    edProxyPort: TLabeledEdit;
    edProxyUser: TLabeledEdit;
    edProxyPassword: TLabeledEdit;
    lblSymSrv: TLabel;
    memSymSrv: TMemo;
    procedure btnResetClick(Sender: TObject);
    procedure btnTestProxyClick(Sender: TObject);
    procedure cbProxyChange(Sender: TObject);
  private
    function GetProxy: TProxySettings;
    procedure UpdateFrameControl;
  protected
    procedure DoReadSettings; override;
    procedure DoWriteSettings; override;
  public
    function GetTitle: string; override;
  end;

implementation

{$R *.lfm}

{ TCpuViewPdbFrame }

procedure TCpuViewPdbFrame.btnResetClick(Sender: TObject);
begin
  Settings.Reset(spPdb);
  DoReadSettings;
end;

procedure TCpuViewPdbFrame.btnTestProxyClick(Sender: TObject);
const
  // CpuView does not have its own releases, so ProcessMemoryMap is used to check the proxy settings
  CheckUrl = 'https://api.github.com/repos/AlexanderBagel/ProcessMemoryMap/releases/latest';
var
  SymSrv: TSymSrv;
  Json: string;
begin
  SymSrv := TSymSrv.Create;
  try
    SymSrv.ProxySettings := GetProxy;
    if SymSrv.DownloadJson(CheckUrl, Json) then
      Application.MessageBox(PChar('Connection successful.'),
        PChar(Application.Title), MB_ICONINFORMATION)
    else
      Application.MessageBox(PChar('Connection failed.' + sLineBreak +
        sLineBreak +
        'Unable to connect to the proxy server. Please check the server address, ' +
        'port, and authentication settings.' + sLineBreak +
        sLineBreak + Format('Error (HTTP Status: %d, Last Error: %d): %s',
        [SymSrv.LastHttpStatus, SymSrv.LastErrorCode, SymSrv.LastError])),
        PChar(Application.Title), MB_ICONINFORMATION);
  finally
    SymSrv.Free;
  end;
end;

procedure TCpuViewPdbFrame.cbProxyChange(Sender: TObject);
begin
  UpdateFrameControl;
end;

function TCpuViewPdbFrame.GetProxy: TProxySettings;
begin
  Result := Default(TProxySettings);
  case cbProxy.ItemIndex of
    0: ;
    1: Result.Kind := pkDefault;
    2:
    begin
      Result.Host := edProxyAddr.Text;
      TryStrToInt(edProxyPort.Text, Result.Port);
      if cbProxyAuth.ItemIndex > 0 then
      begin
        Result.Kind := pkUserSettingsWithAuthority;
        case cbProxyAuth.ItemIndex of
          1: Result.AuthKind := pakAuto;
          2: Result.AuthKind := pakBasic;
          3: Result.AuthKind := pakNegotiate;
          4: Result.AuthKind := pakNTLM;
        end;
        Result.Login := edProxyUser.Text;
        Result.Password := edProxyPassword.Text;
      end
      else
        Result.Kind := pkUserSettings;
    end;
  end;
end;

procedure TCpuViewPdbFrame.UpdateFrameControl;
var
  ProxyLvl: Integer;
begin
  if cbProxy.ItemIndex < 2 then
    ProxyLvl := 0
  else
  begin
    if cbProxyAuth.ItemIndex = 0 then
      ProxyLvl := 1
    else
      ProxyLvl := 2;
  end;
  edProxyAddr.Enabled := ProxyLvl > 0;
  edProxyPort.Enabled := ProxyLvl > 0;
  cbProxyAuth.Enabled := ProxyLvl > 0;
  edProxyUser.Enabled := ProxyLvl = 2;
  edProxyPassword.Enabled := ProxyLvl = 2;
end;

procedure TCpuViewPdbFrame.DoReadSettings;
begin
  cbUsePdb.Checked := Settings.UsePdb;
  cbDebugPDB.Checked := Settings.UsePdbPfx;
  memSymSrv.Text := Settings.SymConfig;
  case Settings.SymSrvProxy.Kind of
    pkNone: cbProxy.ItemIndex := 0;
    pkDefault: cbProxy.ItemIndex := 1;
  else
    cbProxy.ItemIndex := 2;
    edProxyAddr.Text := Settings.SymSrvProxy.Host;
    edProxyPort.Text := IntToStr(Settings.SymSrvProxy.Port);
    if Settings.SymSrvProxy.Kind = pkUserSettingsWithAuthority then
    begin
      cbProxyAuth.ItemIndex := 1 + Byte(Settings.SymSrvProxy.AuthKind);
      edProxyUser.Text := Settings.SymSrvProxy.Login;
      edProxyPassword.Text := Settings.SymSrvProxy.Password;
    end;
  end;
  UpdateFrameControl;
end;

procedure TCpuViewPdbFrame.DoWriteSettings;
begin
  Settings.UsePdb := cbUsePdb.Checked;
  Settings.UsePdbPfx := cbDebugPDB.Checked;
  Settings.SymConfig := memSymSrv.Text;
  Settings.SymSrvProxy := GetProxy;
end;

function TCpuViewPdbFrame.GetTitle: string;
begin
  Result := 'PDB Symbols';
end;

end.

