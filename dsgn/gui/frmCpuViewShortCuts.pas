////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewShortCuts.pas
//  * Purpose   : ShordCut settings for CPU-View dialog and all viewers.
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

unit frmCpuViewShortCuts;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  LCLType, LCLProc,
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ActnList,

  IDEOptEditorIntf, IdeInspectKeyGrapper, IDEImagesIntf,

  frmCpuViewBaseOptions,
  CpuView.Settings;

type

  { TCpuViewShortCutsFrame }

  TCpuViewShortCutsFrame = class(TCpuViewBaseOptionsFrame)
    acEditFirst: TAction;
    acClearFirst: TAction;
    acEditSecond: TAction;
    acClearSecond: TAction;
    alButtons: TActionList;
    btnReset: TButton;
    btnEditFirst: TButton;
    btnEditSecond: TButton;
    btnClearFirst: TButton;
    btnClearSecond: TButton;
    cbShortCutMode: TComboBox;
    gbShortCuts: TGroupBox;
    lblInfo: TLabel;
    lblShortCutMode: TLabel;
    tvShortCuts: TTreeView;
    procedure acEditFirstExecute(Sender: TObject);
    procedure acEditFirstUpdate(Sender: TObject);
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

{$R *.lfm}

const
  ShortCutTypeString: array [TShortCutType] of string = (
    'Close CpuView dialog',
    'Go to address', 'Go back',
    'Step In', 'Step Out', 'Step Over', 'Toggle BreakPoint', 'Run To Cursor', 'Run To User Code',
    'Show Current Origin', 'Set New Origin'
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

procedure TCpuViewShortCutsFrame.acEditFirstUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := tvShortCuts.Selected <> nil;
end;

procedure TCpuViewShortCutsFrame.acEditFirstExecute(Sender: TObject);
var
  Idx: TShortCutType;
  ShortCut: TCpuViewShortCut;
  GrabForm: TIdeInspectKeyGrabForm;
begin
  Idx := TShortCutType(tvShortCuts.Selected.Index);
  ShortCut := Settings.ShotCut[Idx];
  case TAction(Sender).Tag of
    0:
    begin
      GrabForm := TIdeInspectKeyGrabForm.Create(Self);
      try
        GrabForm.KeyBox.Key := ShortCut.Key1;
        GrabForm.KeyBox.ShiftState := ShortCut.Shift1;
        GrabForm.Position := poScreenCenter;
        if GrabForm.ShowModal <> mrOK then Exit;
        ShortCut.Key1 := GrabForm.KeyBox.Key;
        ShortCut.Shift1 := GrabForm.KeyBox.ShiftState;
      finally
        GrabForm.Free;
      end;
    end;
    1:
    begin
      ShortCut.Key1 := VK_UNKNOWN;
      ShortCut.Shift1 := [];
    end;
    2:
    begin
      GrabForm := TIdeInspectKeyGrabForm.Create(Self);
      try
        GrabForm.KeyBox.Key := ShortCut.Key2;
        GrabForm.KeyBox.ShiftState := ShortCut.Shift2;
        GrabForm.Position := poScreenCenter;
        if GrabForm.ShowModal <> mrOK then Exit;
        ShortCut.Key2 := GrabForm.KeyBox.Key;
        ShortCut.Shift2 := GrabForm.KeyBox.ShiftState;
      finally
        GrabForm.Free;
      end;
    end;
    3:
    begin
      ShortCut.Key2 := VK_UNKNOWN;
      ShortCut.Shift2 := [];
    end;
  end;
  Settings.FillCustomShortCuts;
  Settings.ShotCut[Idx] := ShortCut;
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
  Add('menu_exit');
  Add('address');
  Add('menu_undo');
  Add('menu_stepinto');
  Add('menu_stepout');
  Add('menu_stepover');
  Add('ActiveBreakPoint');
  Add('menu_run_cursor');
  Add('menu_run_withdebugging');
  Add('debugger_show_execution_point');
  Add('');
end;

end.

