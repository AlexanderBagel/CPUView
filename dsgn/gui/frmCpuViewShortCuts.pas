////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewShortCuts.pas
//  * Purpose   : ShordCut settings for CPU-View dialog and all viewers.
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

unit frmCpuViewShortCuts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ButtonPanel,
  IDEOptEditorIntf, IdeInspectKeyGrapper, IDEImagesIntf,
  frmCpuViewBaseOptions;

type

  { TCpuViewShortCutsFrame }

  TCpuViewShortCutsFrame = class(TCpuViewBaseOptionsFrame)
    btnReset: TButton;
    btnEditFirst: TButton;
    btnEditSecond: TButton;
    btnClearFirst: TButton;
    btnClearSecond: TButton;
    cbShortCutMode: TComboBox;
    gbShortCuts: TGroupBox;
    lblShortCutMode: TLabel;
    tvShortCuts: TTreeView;
    procedure btnResetClick(Sender: TObject);
    procedure cbShortCutModeChange(Sender: TObject);
  private
    procedure UpdateFrameControl;
  protected
    procedure DoReadSettings; override;
    procedure DoWriteSettings; override;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
  end;

implementation

uses
  LCLType,
  LCLProc,
  CpuView.Settings;

{$R *.lfm}

const
  ShortCutTypeString: array [TShortCutType] of string = (
    'Open CpuView dialog',
    'Close CpuView dialog',
    'Go to address', 'Go back',
    'Step In', 'Step Out', 'Step Over', 'Toggle BreakPoint', 'Run To Cursor', 'Run To User Code',
    'Set New IP', 'Return To Current'
  );

{ TCpuViewShortCutsFrame }

procedure TCpuViewShortCutsFrame.cbShortCutModeChange(Sender: TObject);
begin
  Settings.ShotCutMode := TShortCutMode(cbShortCutMode.ItemIndex);
  UpdateFrameControl;
end;

procedure TCpuViewShortCutsFrame.btnResetClick(Sender: TObject);
begin
  Settings.Reset(spShortCuts);
  UpdateFrameControl;
end;

procedure TCpuViewShortCutsFrame.UpdateFrameControl;
var
  I: TShortCutType;
  ShortCut: TCpuViewShortCut;
  ShortCutStr: string;
begin
  cbShortCutMode.ItemIndex := Integer(Settings.ShotCutMode);
  for I := Low(TShortCutType) to High(TShortCutType) do
  begin
    ShortCutStr := ShortCutTypeString[I];
    ShortCut := Settings.ShotCut[I];
    if ShortCut.Key1 <> VK_UNKNOWN then
      ShortCutStr := Format('%s [%s]',
        [ShortCutStr, KeyAndShiftStateToKeyString(ShortCut.Key1, ShortCut.Shift1)]);
    if ShortCut.Key2 <> VK_UNKNOWN then
      ShortCutStr := Format('%s [%s]',
        [ShortCutStr, KeyAndShiftStateToKeyString(ShortCut.Key2, ShortCut.Shift2)]);
    tvShortCuts.Items[Integer(I)].Text := ShortCutStr;
  end;
end;

procedure TCpuViewShortCutsFrame.DoReadSettings;
begin
  UpdateFrameControl;
end;

procedure TCpuViewShortCutsFrame.DoWriteSettings;
begin
  Settings.ShotCutMode := TShortCutMode(cbShortCutMode.ItemIndex);
end;

function TCpuViewShortCutsFrame.GetTitle: string;
begin
  Result := 'ShortCuts';
end;

procedure TCpuViewShortCutsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);

  procedure Add(const AImageName: string);
  var
    NewNode: TTreeNode;
  begin
    NewNode := tvShortCuts.Items.Add(nil, '');
    NewNode.ImageIndex := IDEImages.LoadImage(AImageName);
    NewNode.SelectedIndex := NewNode.ImageIndex;
  end;

begin
  cbShortCutMode.ItemWidth := ADialog.Canvas.TextWidth('"' + cbShortCutMode.Items[1] + '"');
  tvShortCuts.Images := IDEImages.Images_16;
  tvShortCuts.Items.Clear;
  Add('');
  Add('');
  Add('');
  Add('');
  Add('menu_stepinto');
  Add('menu_stepout');
  Add('menu_stepover');
  Add('ActiveBreakPoint');
  Add('menu_run_cursor');
  Add('');
  Add('');
  Add('debugger_show_execution_point');
end;

end.

