////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgInputBox.pas
//  * Purpose   : Universal field editor for Lazarus.
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

unit dlgInputBox;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  TQueryAddrCallback = function(ANewAddrVA: Int64): Boolean of object;

  { TfrmInputBox }

  TfrmInputBox = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbList: TComboBox;
    edAddress: TEdit;
    lblPromt: TLabel;
    lblWrong: TLabel;
    procedure edAddressChange(Sender: TObject);
  protected
    FCallback: TQueryAddrCallback;
  end;

  function QueryAddress(const ACaption, APromt: string; var AAddrVA: Int64;
    ACallback: TQueryAddrCallback): Boolean;
  function QuerySetList(const ACaption, APromt: string;
    AList: array of string; var ItemIndex: Integer): Boolean;

implementation

{$R *.lfm}

function QueryAddress(const ACaption, APromt: string; var AAddrVA: Int64;
  ACallback: TQueryAddrCallback): Boolean;
var
  frmInputBox: TfrmInputBox;
  ANewAddrVA: Int64;
begin
  frmInputBox := TfrmInputBox.Create(Application);
  try
    frmInputBox.Caption := ACaption;
    frmInputBox.lblPromt.Caption := APromt;
    frmInputBox.FCallback := ACallback;
    frmInputBox.edAddress.Text := '0x' + IntToHex(AAddrVA, 1);
    Result := (frmInputBox.ShowModal = mrOK) and
      TryStrToInt64(frmInputBox.edAddress.Text, ANewAddrVA);
    if Result then
      AAddrVA := ANewAddrVA;
  finally
    frmInputBox.Free;
  end;
end;

function QuerySetList(const ACaption, APromt: string; AList: array of string;
  var ItemIndex: Integer): Boolean;
var
  frmInputBox: TfrmInputBox;
  AItem: string;
begin
  if ItemIndex >= Length(AList) then Exit(False);
  frmInputBox := TfrmInputBox.Create(Application);
  try
    frmInputBox.Caption := ACaption;
    frmInputBox.lblPromt.Caption := APromt;
    frmInputBox.edAddress.Visible := False;
    frmInputBox.cbList.Visible := True;
    for AItem in AList do
      frmInputBox.cbList.Items.Add(AItem);
    frmInputBox.cbList.ItemIndex := ItemIndex;
    Result := frmInputBox.ShowModal = mrOK;
    if Result then
      ItemIndex := frmInputBox.cbList.ItemIndex;
  finally
    frmInputBox.Free;
  end;
end;

{ TfrmInputBox }

procedure TfrmInputBox.edAddressChange(Sender: TObject);

  function TryGetAddr(out AInputAddr: Int64): Boolean;
  begin
    AInputAddr := 0;
    Result := TryStrToInt64(edAddress.Text, AInputAddr);
    if not Result then
      Result := TryStrToInt64('$' + edAddress.Text, AInputAddr);
  end;

var
  ANewAddrVA: Int64;
begin
  if edAddress.Visible then
  begin
    if not TryGetAddr(ANewAddrVA) then
      btnOk.Enabled := False
    else
      btnOk.Enabled := FCallback(ANewAddrVA);
    lblWrong.Visible := not btnOk.Enabled;
  end;
end;

end.

