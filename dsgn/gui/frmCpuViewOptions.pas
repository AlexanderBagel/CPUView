////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewOptions.pas
//  * Purpose   : Main settings window frame for Lazarus.
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

unit frmCpuViewOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  IDEOptEditorIntf, frmCpuViewBaseOptions;

type

  { TCpuViewMainOptionsFrame }

  TCpuViewMainOptionsFrame = class(TCpuViewBaseOptionsFrame)
    btnFontBrowse: TButton;
    btnReset: TButton;
    cbFont: TComboBox;
    cbDisplayFuncName: TCheckBox;
    cbShowOpcodes: TCheckBox;
    cbShowSourceLines: TCheckBox;
    cbSymbols: TCheckBox;
    cbForm: TCheckBox;
    cbViewers: TCheckBox;
    cbDbgLog: TCheckBox;
    cbDbgCrash: TCheckBox;
    cbAddrValidation: TCheckBox;
    FontDialog: TFontDialog;
    gbAsmView: TGroupBox;
    gbSessions: TGroupBox;
    lblFont: TLabel;
    procedure btnFontBrowseClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbSymbolsClick(Sender: TObject);
  private
    procedure UpdateCurrentFont(const AFontName: string);
    procedure UpdateDebugSymbolsIndepended;
    procedure UpdateFrameControl;
  protected
    procedure DoReadSettings; override;
    procedure DoWriteSettings; override;
    function IsMainFrame: Boolean; override;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
  end;

implementation

uses
  FWHexView,
  CpuView.Settings;

{$R *.lfm}

procedure TCpuViewMainOptionsFrame.btnFontBrowseClick(Sender: TObject);
begin
  FontDialog.Font.Name := cbFont.Text;
  if FontDialog.Execute then
    UpdateCurrentFont(FontDialog.Font.Name);
end;

procedure TCpuViewMainOptionsFrame.btnResetClick(Sender: TObject);
begin
  Settings.Reset(spSession);
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.cbSymbolsClick(Sender: TObject);
begin
  UpdateDebugSymbolsIndepended;
end;

procedure TCpuViewMainOptionsFrame.UpdateCurrentFont(const AFontName: string);
var
  Index: Integer;
begin
  Index := cbFont.Items.IndexOf(AFontName);
  if Index < 0 then
    Index := cbFont.Items.Add(AFontName);
  cbFont.ItemIndex := Index;
end;

procedure TCpuViewMainOptionsFrame.UpdateDebugSymbolsIndepended;
begin
  cbDisplayFuncName.Enabled := cbSymbols.Checked;
  cbShowSourceLines.Enabled := cbSymbols.Checked;
end;

procedure TCpuViewMainOptionsFrame.UpdateFrameControl;
begin
  UpdateCurrentFont(Settings.FontName);
  cbDisplayFuncName.Checked := Settings.ShowCallFuncName;
  cbShowOpcodes.Checked :=  Settings.ShowOpcodes;
  cbShowSourceLines.Checked := Settings.ShowSourceLines;
  cbSymbols.Checked := Settings.UseDebugInfo;
  cbAddrValidation.Checked := Settings.ValidationAddrVA;
  cbForm.Checked := Settings.SaveFormSession;
  cbViewers.Checked := Settings.SaveViewersSession;
  cbDbgLog.Checked := Settings.UseDebugLog;
  cbDbgCrash.Checked := Settings.UseCrashDump;
  UpdateDebugSymbolsIndepended;
end;

procedure TCpuViewMainOptionsFrame.DoReadSettings;
begin
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.DoWriteSettings;
begin
  Settings.FontName := cbFont.Text;
  Settings.ShowCallFuncName := cbDisplayFuncName.Checked;
  Settings.ShowOpcodes := cbShowOpcodes.Checked;
  Settings.ShowSourceLines := cbShowSourceLines.Checked;
  Settings.ValidationAddrVA := cbAddrValidation.Checked;
  Settings.UseDebugInfo := cbSymbols.Checked;
  Settings.SaveFormSession := cbForm.Checked;
  Settings.SaveViewersSession := cbViewers.Checked;
  Settings.UseDebugLog := cbDbgLog.Checked;
  Settings.UseCrashDump := cbDbgCrash.Checked;
end;

function TCpuViewMainOptionsFrame.IsMainFrame: Boolean;
begin
  Result := True;
end;

function TCpuViewMainOptionsFrame.GetTitle: string;
begin
  Result := 'CPU-View';
end;

procedure TCpuViewMainOptionsFrame.Setup({%H-}ADialog: TAbstractOptionsEditorDialog);
begin
  cbFont.Clear;
  {$IFDEF MSWINDOWS}
  cbFont.Items.Add('Consolas');
  {$ENDIF}
  {$IFDEF LINUX}
  cbFont.Items.Add('DejaVu Sans Mono');
  cbFont.Items.Add('Monospace');
  {$ENDIF}
  cbFont.ItemIndex := 0;
end;

end.

