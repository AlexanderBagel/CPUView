////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewColors.pas
//  * Purpose   : Color settings for all viewers.
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

unit frmCpuViewColors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, ColorBox,

  frmCpuViewBaseOptions,
  FWHexView,
  CpuView.Settings;

type

  { TCpuViewColorsFrame }

  TCpuViewColorsFrame = class(TCpuViewBaseOptionsFrame)
    btnExport: TButton;
    btnImport: TButton;
    btnReset: TButton;
    cbColor: TColorBox;
    cbColorMode: TComboBox;
    clbColors: TColorListBox;
    ColorDialog: TColorDialog;
    gbColors: TGroupBox;
    lblColorMode: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure btnExportClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure clbColorsSelectionChange(Sender: TObject; User: boolean);
  private
    FLockColorChange: Boolean;
    procedure UpdateFrameControl;
  protected
    procedure DoReadSettings; override;
    procedure DoWriteSettings; override;
  public
    function GetTitle: string; override;
  end;

implementation

{$R *.lfm}

{ TCpuViewColorsFrame }

procedure TCpuViewColorsFrame.btnImportClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Settings.ColorsImport(OpenDialog.FileName);
    UpdateFrameControl;
  end;
end;

procedure TCpuViewColorsFrame.btnResetClick(Sender: TObject);
begin
  Settings.Reset(spColors);
  UpdateFrameControl;
end;

procedure TCpuViewColorsFrame.cbColorChange(Sender: TObject);
var
  Index: Integer;
begin
  if FLockColorChange then Exit;
  Index := clbColors.ItemIndex;
  if Index < 0 then Exit;
  clbColors.Colors[Index] := cbColor.Selected;
  cbColorMode.ItemIndex := Integer(cmCustom);
end;

procedure TCpuViewColorsFrame.clbColorsSelectionChange(Sender: TObject;
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

procedure TCpuViewColorsFrame.UpdateFrameControl;
var
  I: Integer;
begin
  cbColorMode.ItemIndex := Integer(Settings.ColorMode);
  clbColors.Items.Clear;
  for I := 0 to Settings.ColorsMap.Count - 1 do
  begin
    clbColors.Items.Add(Settings.ColorsMap[I].Description);
    clbColors.Colors[I] := Settings.Color[Settings.ColorsMap[I].Id];
  end;
end;

procedure TCpuViewColorsFrame.btnExportClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Settings.ColorsExport(SaveDialog.FileName);
end;

procedure TCpuViewColorsFrame.DoReadSettings;
begin
  UpdateFrameControl;
end;

procedure TCpuViewColorsFrame.DoWriteSettings;
var
  I: Integer;
begin
  Settings.ColorMode := TColorMode(cbColorMode.ItemIndex);
  for I := 0 to Settings.ColorsMap.Count - 1 do
    Settings.Color[Settings.ColorsMap[I].Id] := clbColors.Colors[I];
end;

function TCpuViewColorsFrame.GetTitle: string;
begin
  Result := 'Colors';
end;

end.

