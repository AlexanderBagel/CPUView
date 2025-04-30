unit dlgPageAccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  laz.VirtualTrees;

type

  { TfrmPageAccess }

  TfrmPageAccess = class(TForm)
    btnSelAll: TButton;
    btnDeselAll: TButton;
    btnCancel: TButton;
    Button3: TButton;
    cbPageGuard: TCheckBox;
    gbPages: TGroupBox;
    gbAccess: TGroupBox;
    lvPages: TLazVirtualStringTree;
    rbNoAcccess: TRadioButton;
    rbReadOnly: TRadioButton;
    rbReadWrite: TRadioButton;
    rbExecute: TRadioButton;
    rbExecuteRead: TRadioButton;
    rbFull: TRadioButton;
    rbWriteCopy: TRadioButton;
    rbExecuteWriteCopy: TRadioButton;
    procedure btnDeselAllClick(Sender: TObject);
    procedure btnSelAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvPagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FAddrVA, FPageSize: Int64;
    FSelectedList: TList;
  public
    function ShowPageAccess(AddrVA, RegionSize, PageSize: Int64): TModalResult;
    property SelectedList: TList read FSelectedList;
  end;

var
  frmPageAccess: TfrmPageAccess;

implementation

{$R *.lfm}

{ TfrmPageAccess }

procedure TfrmPageAccess.FormCreate(Sender: TObject);
begin
  FSelectedList := TList.Create;
  {$IFDEF LINUX}
  rbWriteCopy.Visible := False;
  rbExecuteWriteCopy.Visible := False;
  cbPageGuard.Visible := False;
  {$ENDIF}
end;

procedure TfrmPageAccess.FormDestroy(Sender: TObject);
begin
  FSelectedList.Free;
end;

procedure TfrmPageAccess.lvPagesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText := IntToHex(FAddrVA + FPageSize * Node^.Index, 8);
end;

function TfrmPageAccess.ShowPageAccess(AddrVA, RegionSize, PageSize: Int64
  ): TModalResult;
var
  E: TVTVirtualNodeEnumerator;
begin
  FAddrVA := AddrVA;
  FPageSize := PageSize;
  lvPages.RootNodeCount := RegionSize div PageSize;
  FSelectedList.Clear;
  E := lvPages.Nodes.GetEnumerator;
  if E.MoveNext then
    lvPages.AddToSelection(E.Current);
  Result := ShowModal;
  if Result = mrOK then
  begin
    E := lvPages.SelectedNodes.GetEnumerator;
    while E.MoveNext do
      FSelectedList.Add({%H-}Pointer(FAddrVA + FPageSize * E.Current^.Index));
  end;
end;

procedure TfrmPageAccess.btnSelAllClick(Sender: TObject);
begin
  lvPages.SelectAll(True);
end;

procedure TfrmPageAccess.btnDeselAllClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := lvPages.SelectedNodes.GetEnumerator;
  while E.MoveNext do
    lvPages.RemoveFromSelection(E.Current);
  lvPages.Invalidate;
end;

end.

