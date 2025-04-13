unit dlgSimd87Editor;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus,

  FWHexView.Common,
  CpuView.Viewers,
  CpuView.CPUContext;

type

  { TfrmSimd87Editor }

  TfrmSimd87Editor = class(TForm)
    btnCancel: TButton;
    mbOk: TButton;
    EditView: TEditView;
    pnEditViewMargins: TPanel;
    pnBottom: TPanel;
    pmEditView: TPopupMenu;
    rbHex: TRadioButton;
    rbSigned: TRadioButton;
    rbUnsigned: TRadioButton;
    procedure btnCancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure mbOkClick(Sender: TObject);
    procedure rbHexClick(Sender: TObject);
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure UpdateSize;
  end;

  function QuerySimd87RegValue(const Caption: string;
    AEditMode: TEditViewMode; var ARegValue: TRegValue): Boolean;

implementation

function QuerySimd87RegValue(const Caption: string;
  AEditMode: TEditViewMode; var ARegValue: TRegValue): Boolean;
var
  Simd87Editor: TfrmSimd87Editor;
  View: TEditView;
begin
  Simd87Editor := TfrmSimd87Editor.Create(Application);
  try
    Simd87Editor.Caption := Caption;
    View := Simd87Editor.EditView;
    View.RawData.EditMode := AEditMode;
    View.RawData.InitRegBuff(ARegValue.Ext32[0], ARegValue.ValueSize);
    View.RawData.PartViewType := pvtHex;
    View.Zoom(3);
    View.FitColumnsToBestSize;
    Simd87Editor.UpdateSize;
    Result := Simd87Editor.ShowModal = mrOK;
    if Result then
      View.RawData.GetBuff(ARegValue.Ext32[0], ARegValue.ValueSize);
  finally
    Simd87Editor.Free;
  end;
end;

{$R *.lfm}

{ TfrmSimd87Editor }

procedure TfrmSimd87Editor.rbHexClick(Sender: TObject);
begin
  EditView.RawData.PartViewType := TPartViewType(TRadioButton(Sender).Tag);
end;

procedure TfrmSimd87Editor.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double
  );
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  UpdateSize;
end;

procedure TfrmSimd87Editor.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmSimd87Editor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not EditView.InplaceEdit.Visible then
  begin
    if Key = VK_ESCAPE then ModalResult := mrCancel;
    if Key = VK_RETURN then ModalResult := mrOK;
  end;
end;

procedure TfrmSimd87Editor.FormShow(Sender: TObject);
begin
  if EditView.CanFocus then
  begin
    EditView.SetFocus;
    EditView.JumpToAddress(0);
    EditView.ClearSelection(False);
  end;
end;

procedure TfrmSimd87Editor.mbOkClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmSimd87Editor.UpdateSize;
begin
  if EditView.RawData.EditMode = evmX87 then
  begin
    rbHex.Visible := False;
    rbSigned.Visible := False;
    rbUnsigned.Visible := False;
  end;
  ClientWidth := EditView.PrefferededSize.X + pnEditViewMargins.Width - EditView.Width;
  ClientHeight := EditView.PrefferededSize.Y + pnBottom.Height + pnEditViewMargins.Height - EditView.Height;
  // just hide the right separator
  EditView.Header.ColumnWidth[ctOpcode] := EditView.Header.ColumnWidth[ctOpcode] + 100;
end;

end.

