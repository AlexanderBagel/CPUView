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
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, ColorBox,
  IDEOptEditorIntf, frmCpuViewBaseOptions;

type

  { TCpuViewMainOptionsFrame }

  TCpuViewMainOptionsFrame = class(TCpuViewBaseOptionsFrame)
    btnFontBrowse: TButton;
    btnReset: TButton;
    btnImport: TButton;
    btnExport: TButton;
    cbFont: TComboBox;
    cbDisplayFuncName: TCheckBox;
    cbShowOpcodes: TCheckBox;
    cbShowSourceLines: TCheckBox;
    cbSymbols: TCheckBox;
    cbForm: TCheckBox;
    cbViewers: TCheckBox;
    cbDbgLog: TCheckBox;
    cbDbgCrash: TCheckBox;
    cbColor: TColorBox;
    clbColors: TColorListBox;
    ColorDialog: TColorDialog;
    cbColorMode: TComboBox;
    FontDialog: TFontDialog;
    gbAsmView: TGroupBox;
    gbSessions: TGroupBox;
    gbColors: TGroupBox;
    lblColorMode: TLabel;
    lblFont: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure btnExportClick(Sender: TObject);
    procedure btnFontBrowseClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure clbColorsSelectionChange(Sender: TObject; User: boolean);
  private
    FLockColorChange: Boolean;
    procedure UpdateCurrentFont(const AFontName: string);
    procedure UpdateFrameControl;
  protected
    procedure DoReadSettings; override;
    procedure DoResetSettings; override;
    procedure DoWriteSettings; override;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
  end;

implementation

uses
  FWHexView;

{$R *.lfm}

procedure TCpuViewMainOptionsFrame.btnFontBrowseClick(Sender: TObject);
begin
  FontDialog.Font.Name := cbFont.Text;
  if FontDialog.Execute then
    UpdateCurrentFont(FontDialog.Font.Name);
end;

procedure TCpuViewMainOptionsFrame.btnExportClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Settings.ColorsExport(SaveDialog.FileName);
end;

procedure TCpuViewMainOptionsFrame.btnImportClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Settings.ColorsImport(OpenDialog.FileName);
    UpdateFrameControl;
  end;
end;

procedure TCpuViewMainOptionsFrame.btnResetClick(Sender: TObject);
begin
  ResetSettings;
end;

procedure TCpuViewMainOptionsFrame.cbColorChange(Sender: TObject);
var
  Index: Integer;
begin
  if FLockColorChange then Exit;
  Index := clbColors.ItemIndex;
  if Index < 0 then Exit;
  clbColors.Colors[Index] := cbColor.Selected;
  cbColorMode.ItemIndex := Integer(cmCustom);
end;

procedure TCpuViewMainOptionsFrame.clbColorsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if User then
  begin
    FLockColorChange := True;
    try
      cbColor.Selected := clbColors.Selected;
    finally
      FLockColorChange := False;
    end;
  end;
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

procedure TCpuViewMainOptionsFrame.UpdateFrameControl;
var
  I: Integer;
begin
  UpdateCurrentFont(Settings.FontName);
  cbDisplayFuncName.Checked := Settings.ShowCallFuncName;
  cbShowOpcodes.Checked :=  Settings.ShowOpcodes;
  cbShowSourceLines.Checked := Settings.ShowSourceLines;
  cbSymbols.Checked := Settings.UseDebugInfo;
  cbForm.Checked := Settings.SaveFormSession;
  cbViewers.Checked := Settings.SaveViewersSession;
  cbDbgLog.Checked := Settings.UseDebugLog;
  cbDbgCrash.Checked := Settings.UseCrashDump;
  cbColorMode.ItemIndex := Integer(Settings.ColorMode);
  clbColors.Items.Clear;
  for I := 0 to Settings.ColorsMap.Count - 1 do
  begin
    clbColors.Items.Add(Settings.ColorsMap[I].Description);
    clbColors.Colors[I] := Settings.Color[Settings.ColorsMap[I].Id];
  end;
end;

procedure TCpuViewMainOptionsFrame.DoReadSettings;
begin
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.DoResetSettings;
begin
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.DoWriteSettings;
var
  I: Integer;
begin
  Settings.FontName := cbFont.Text;
  Settings.ShowCallFuncName := cbDisplayFuncName.Checked;
  Settings.ShowOpcodes := cbShowOpcodes.Checked;
  Settings.ShowSourceLines := cbShowSourceLines.Checked;
  Settings.UseDebugInfo := cbSymbols.Checked;
  Settings.SaveFormSession := cbForm.Checked;
  Settings.SaveViewersSession := cbViewers.Checked;
  Settings.UseDebugLog := cbDbgLog.Checked;
  Settings.UseCrashDump := cbDbgCrash.Checked;
  Settings.ColorMode := TColorMode(cbColorMode.ItemIndex);
  for I := 0 to Settings.ColorsMap.Count - 1 do
    Settings.Color[Settings.ColorsMap[I].Id] := clbColors.Colors[I];
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

