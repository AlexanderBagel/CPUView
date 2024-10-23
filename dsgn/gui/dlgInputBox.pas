unit dlgInputBox;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  TQueryAddrCallback = function(ANewAddrVA: UInt64): Boolean of object;

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

  function QueryAddress(const ACaption, APromt: string; var AAddrVA: UInt64;
    ACallback: TQueryAddrCallback): Boolean;
  function QuerySetList(const ACaption, APromt: string;
    AList: array of string; var ItemIndex: Integer): Boolean;

implementation

{$R *.lfm}

function QueryAddress(const ACaption, APromt: string; var AAddrVA: UInt64;
  ACallback: TQueryAddrCallback): Boolean;
var
  frmInputBox: TfrmInputBox;
  ANewAddrVA: UInt64;
begin
  frmInputBox := TfrmInputBox.Create(Application);
  try
    frmInputBox.Caption := ACaption;
    frmInputBox.lblPromt.Caption := APromt;
    frmInputBox.FCallback := ACallback;
    frmInputBox.edAddress.Text := '0x' + IntToHex(AAddrVA, 1);
    Result := (frmInputBox.ShowModal = mrOK) and
      TryStrToUInt64(frmInputBox.edAddress.Text, ANewAddrVA);
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
var
  ANewAddrVA: UInt64;
begin
  if edAddress.Visible then
  begin
    if not TryStrToUInt64(edAddress.Text, ANewAddrVA) then
      btnOk.Enabled := False
    else
      btnOk.Enabled := FCallback(ANewAddrVA);
    lblWrong.Visible := not btnOk.Enabled;
  end;
end;

end.

