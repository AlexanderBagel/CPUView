////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgProcExports.pas
//  * Purpose   : Dialog for displaying the list of exported functions
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

unit dlgProcExports;

{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Clipbrd, Menus,
  Generics.Collections, Generics.Defaults,
  laz.VirtualTrees,

  CpuView.Common,
  CpuView.Core,
  CpuView.DebugerGate;

type

  TRemoteExport = record
    AddrVA: Int64;
    Address,
    LibraryName,
    FunctionName,
    SearchFunctionName: string;
  end;
  TRemoteExports = TList<TRemoteExport>;

  { TfrmProcExports }

  TfrmProcExports = class(TForm)
    lvExports: TLazVirtualStringTree;
    miNextMatch: TMenuItem;
    miCopyLine: TMenuItem;
    miCopyFunc: TMenuItem;
    miCopyAddr: TMenuItem;
    miFollowAsm: TMenuItem;
    pmCopy: TPopupMenu;
    miSep1: TMenuItem;
    miSep2: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure lvExportsDblClick(Sender: TObject);
    procedure lvExportsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure lvExportsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo
      );
    procedure miCopyAddrClick(Sender: TObject);
    procedure miCopyFuncClick(Sender: TObject);
    procedure miCopyLineClick(Sender: TObject);
    procedure miFollowAsmClick(Sender: TObject);
    procedure miNextMatchClick(Sender: TObject);
  private
    SearchString: string;
    SearchPosition: Integer;
    FCore: TCpuViewCore;
    FCurrentList: TRemoteExports;
    FGui: IGuiImplementation;
    function Search(const Value: string): Boolean;
  public
    procedure ShowExports(ACore: TCpuViewCore;
      AGui: IGuiImplementation; const Value: TRemoteExports);
  end;

var
  frmProcExports: TfrmProcExports;

implementation

const
  RootCaption = 'CpuView - Exports';

{$R *.lfm}

{ TfrmProcExports }

procedure TfrmProcExports.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  frmProcExports := nil;
end;

procedure TfrmProcExports.FormDestroy(Sender: TObject);
begin
  FCurrentList.Free;
end;

procedure TfrmProcExports.FormKeyPress(Sender: TObject; var Key: char);
var
  TmpString, ClipboardString: string;
begin
  if Key = #8 then
  begin
    Delete(SearchString, Length(SearchString), 1);
    SearchPosition := 0;
    if SearchString = '' then
      Caption := RootCaption
    else
      Search(SearchString);
    Exit;
  end;
  if Key = #22 then
  begin
    ClipboardString := AnsiUpperCase(Trim(Clipboard.AsText));
    if ClipboardString = '' then Exit;
    TmpString := '';
    while TmpString <> ClipboardString do
    begin
      TmpString := TmpString + ClipboardString[Length(TmpString) + 1];
      if Search(TmpString) then
        SearchString := TmpString
      else
        Break;
    end;
  end;
  if Key = #27 then
  begin
    if SearchString = '' then
    begin
      Close;
      Exit;
    end;
    SearchString := '';
    Caption := RootCaption;
    SearchPosition := 0;
    Exit;
  end;
  if Key <= #32 then Exit;
  TmpString := SearchString + AnsiUpperCase(Key);
  if Search(TmpString) then
    SearchString := TmpString;
end;

procedure TfrmProcExports.lvExportsDblClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
  Address: Int64;
begin
  E := lvExports.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  if TryStrToInt64('$' + FCurrentList[E.Current^.Index].Address, Address) then
  begin
    if FCore.AddrInAsm(Address) then
      FGui.OpenInDisassembler(Address)
    else
      if FCore.AddrInDump(Address) then
        FGui.OpenInDump(Address, False)
      else
        Beep;
  end;
end;

procedure TfrmProcExports.lvExportsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := FCurrentList[Node.Index].Address;
    1: CellText := FCurrentList[Node.Index].LibraryName;
    2: CellText := FCurrentList[Node.Index].FunctionName;
  end;
end;

function DefaultExportDataComparer(
  {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} A, B: TRemoteExport): Integer;
begin
  Result := 0;
  case frmProcExports.lvExports.Header.SortColumn of
    0:
    begin
      if A.AddrVA > B.AddrVA then
        Result := 1
      else
        if A.AddrVA = B.AddrVA then
          Result := 0
        else
          Result := -1;
    end;
    1: Result := AnsiCompareStr(A.LibraryName, B.LibraryName);
    2: Result := AnsiCompareStr(A.FunctionName, B.FunctionName);
  end;
  if frmProcExports.lvExports.Header.SortDirection = sdDescending then
    Result := -Result;
end;

procedure TfrmProcExports.lvExportsHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  SearchPosition := 0;
  if lvExports.Header.SortColumn = HitInfo.Column then
  begin
    if lvExports.Header.SortDirection = sdAscending then
      lvExports.Header.SortDirection := sdDescending
    else
      lvExports.Header.SortDirection := sdAscending;
  end
  else
  begin
    lvExports.Header.SortDirection := sdAscending;
    lvExports.Header.SortColumn := HitInfo.Column;
  end;

  FCurrentList.Sort(TComparer<TRemoteExport>.Construct(DefaultExportDataComparer));
end;

procedure TfrmProcExports.miCopyAddrClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := lvExports.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Clipboard.AsText := FCurrentList[E.Current^.Index].Address;
end;

procedure TfrmProcExports.miCopyFuncClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := lvExports.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Clipboard.AsText := FCurrentList[E.Current^.Index].FunctionName;
end;

procedure TfrmProcExports.miCopyLineClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := lvExports.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Clipboard.AsText :=
    FCurrentList[E.Current^.Index].Address + #9 +
    FCurrentList[E.Current^.Index].LibraryName + #9 +
    FCurrentList[E.Current^.Index].FunctionName;
end;

procedure TfrmProcExports.miFollowAsmClick(Sender: TObject);
begin
  lvExportsDblClick(nil);
end;

procedure TfrmProcExports.miNextMatchClick(Sender: TObject);
begin
  if SearchString <> '' then
  begin
    Inc(SearchPosition);
    Search(SearchString);
  end;
end;

function TfrmProcExports.Search(const Value: string): Boolean;
var
  I: Integer;
  E: TVTVirtualNodeEnumerator;
  AddrStr: string;
  Address: Int64;
begin
  Result := False;
  if Value[1] = '$' then
    AddrStr := Value
  else
    AddrStr := '$' + Value;
  if TryStrToInt64(AddrStr, Address) then
    AddrStr := IntToHex(Address, 1)
  else
    AddrStr := '';
  for I := SearchPosition to FCurrentList.Count - 1 do
  begin
    Result := Pos(Value, FCurrentList[I].SearchFunctionName) > 0;
    if not Result then
      Result := Pos(AddrStr, FCurrentList[I].Address) > 0;
    if Result then
    begin
      SearchPosition := I;
      Caption := RootCaption + ' [' + Value + ']';
      Result := True;
      Break;
    end;
  end;
  if not Result then
  begin
    if SearchPosition > 0 then
    begin
      SearchPosition := 0;
      Result := Search(Value);
    end;
    Exit;
  end;
  E := lvExports.Nodes.GetEnumerator;
  repeat
    if not E.MoveNext then Break;
  until Integer(E.Current^.Index) = SearchPosition;
  if E.Current = nil then Exit;
  lvExports.Selected[E.Current] := True;
  lvExports.ScrollIntoView(E.Current, True);
end;

procedure TfrmProcExports.ShowExports(ACore: TCpuViewCore;
  AGui: IGuiImplementation; const Value: TRemoteExports);
begin
  Caption := RootCaption;
  FCore := ACore;
  FCurrentList := Value;
  FGui := AGui;
  lvExports.RootNodeCount := Value.Count;
  lvExports.Header.SortDirection := sdAscending;
  lvExports.Header.SortColumn := 0;
  Show;
end;

end.

