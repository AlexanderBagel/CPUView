unit dlgMemoryMap;

{$mode Delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus, Clipbrd, laz.VirtualTrees,
  Generics.Collections,

  CpuView.Common,
  CpuView.Core,

  {$IFDEF LINUX}
  CpuView.Linux.MMap,
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  CpuView.Windows.MMap,
  {$ENDIF}

  dlgInputBox;

type

  { TLazVirtualStringTree }

  TLazVirtualStringTree = class(laz.VirtualTrees.TLazVirtualStringTree)
  protected
    procedure AutoScale; override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const AText: string;
      CellRect: TRect; DrawFormat: Cardinal); override;
  end;

  { TfrmMemoryMap }

  TfrmMemoryMap = class(TForm)
    lblUrl: TLabel;
    lblPromt: TLabel;
    lvMemoryMap: TLazVirtualStringTree;
    miSearchAddr: TMenuItem;
    miCopyLine: TMenuItem;
    miCopyAddr: TMenuItem;
    miOpenInCpuView: TMenuItem;
    pnHint: TPanel;
    pmList: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lblUrlClick(Sender: TObject);
    procedure lvMemoryMapAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const {%H-}Elements: THeaderPaintElements);
    procedure lvMemoryMapBeforeItemErase(Sender: TBaseVirtualTree;
      {%H-}TargetCanvas: TCanvas; Node: PVirtualNode; const {%H-}ItemRect: TRect;
      var ItemColor: TColor; var {%H-}EraseAction: TItemEraseAction);
    procedure lvMemoryMapDblClick(Sender: TObject);
    procedure lvMemoryMapGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure lvMemoryMapHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure miCopyAddrClick(Sender: TObject);
    procedure miCopyLineClick(Sender: TObject);
    procedure miOpenInCpuViewClick(Sender: TObject);
    procedure miSearchAddrClick(Sender: TObject);
  private
    FAutoFited: Boolean;
    FCore: TCpuViewCore;
    FData: TList<TListItemData>;
    FGui: IGuiImplementation;
    FPid: Cardinal;
    FIs64Process: Boolean;
    FDelayedHighlight: Int64;
    function CheckAddressCallback(ANewAddrVA: Int64): Boolean;
    procedure HighLightAddr(AAddrVA: Int64);
    function PageTypeToStr(Value: TPageType): string;
    function Search(AAddrVA: Int64): Integer;
  public
    procedure Init(ACore: TCpuViewCore; AGui: IGuiImplementation;
      APid: Cardinal; AIs64Process: Boolean; AAddrVA: Int64);
    procedure Refresh(AAddrVA: Int64);
  end;

var
  frmMemoryMap: TfrmMemoryMap;

implementation

{$R *.lfm}

{ TLazVirtualStringTree }

procedure TLazVirtualStringTree.AutoScale;
var
  Enum: TVTVirtualNodeEnumerator;
begin
  BeginUpdate;
  try
    Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
    inherited AutoScale;
    DefaultNodeHeight := Scale96ToFont(DEFAULT_NODE_HEIGHT);
    Header.DefaultHeight := Scale96ToFont(DEFAULT_HEADER_HEIGHT);
    Header.Height := Header.DefaultHeight;
    Enum := Nodes.GetEnumerator;
    while Enum.MoveNext do
      NodeHeight[Enum.Current] := DefaultNodeHeight;
  finally
    EndUpdate;
  end;
end;

procedure TLazVirtualStringTree.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const AText: string; CellRect: TRect; DrawFormat: Cardinal);
begin
  PaintInfo.Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  inherited DoTextDrawing(PaintInfo, AText, CellRect, DrawFormat);
end;

{ TfrmMemoryMap }

procedure TfrmMemoryMap.lvMemoryMapDblClick(Sender: TObject);
var
  Address: Int64;
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Address := FData[E.Current^.Index].AddrVA;
  if FCore.AddrInAsm(Address) then
    FGui.OpenInDisassembler(Address)
  else
    if FCore.AddrInDump(Address) then
      FGui.OpenInDump(Address, False)
    else
      Beep;
end;

procedure TfrmMemoryMap.lvMemoryMapGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  {$IFDEF LINUX}
  if Column >= 3 then Inc(Column);
  if Column = 7 then Inc(Column);
  {$ENDIF}
  case Column of
    0: CellText := IntToHex(FData[Node.Index].AddrVA, 8);
    1: CellText := IntToHex(FData[Node.Index].Size, 8);
    2: CellText := FData[Node.Index].Image;
    3: CellText := FData[Node.Index].Section;
    4: CellText := FData[Node.Index].Contains;
    5: CellText := PageTypeToStr(FData[Node.Index].PageType);
    6: CellText := FData[Node.Index].Access;
    7: CellText := FData[Node.Index].IAccess;
    8: CellText := FData[Node.Index].MapedFile;
  end;
end;

procedure TfrmMemoryMap.lvMemoryMapHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements := [];
  PaintInfo.TargetCanvas.Font.PixelsPerInch := Font.PixelsPerInch;
  PaintInfo.TargetCanvas.Font.Assign(Font);
end;

procedure TfrmMemoryMap.miCopyAddrClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Clipboard.AsText := IntToHex(FData[E.Current^.Index].AddrVA, 1);
end;

procedure TfrmMemoryMap.miCopyLineClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Clipboard.AsText :=
    IntToHex(FData[E.Current^.Index].AddrVA, 8) + #9 +
    IntToHex(FData[E.Current^.Index].Size, 8) + #9 +
    FData[E.Current^.Index].Image + #9 +
    {$IFNDEF LINUX}
    FData[E.Current^.Index].Section + #9 +
    {$ENDIF}
    FData[E.Current^.Index].Contains + #9 +
    PageTypeToStr(FData[E.Current^.Index].PageType) + #9 +
    FData[E.Current^.Index].Access + #9 +
    {$IFNDEF LINUX}
    FData[E.Current^.Index].IAccess + #9 +
    {$ENDIF}
    FData[E.Current^.Index].MapedFile;
end;

procedure TfrmMemoryMap.miOpenInCpuViewClick(Sender: TObject);
begin
  lvMemoryMapDblClick(nil);
end;

procedure TfrmMemoryMap.miSearchAddrClick(Sender: TObject);
var
  NewAddress: Int64;
begin
  NewAddress := 0;
  if QueryAddress('Search Address', 'Address:', NewAddress, CheckAddressCallback) then
    HighLightAddr(NewAddress);
end;

function TfrmMemoryMap.CheckAddressCallback(ANewAddrVA: Int64): Boolean;
begin
  Result := Search(ANewAddrVA) >= 0;
end;

procedure TfrmMemoryMap.HighLightAddr(AAddrVA: Int64);
var
  E: TVTVirtualNodeEnumerator;
  Idx: Integer;
begin
  Idx := Search(AAddrVA);
  if Idx >= 0 then
  begin
    E := lvMemoryMap.Nodes.GetEnumerator;
    repeat
      if not E.MoveNext then Break;
    until Integer(E.Current^.Index) = Idx;
    if E.Current = nil then Exit;
    lvMemoryMap.Selected[E.Current] := True;
    lvMemoryMap.ScrollIntoView(E.Current, True);
  end;
end;

function TfrmMemoryMap.PageTypeToStr(Value: TPageType): string;
begin
  case Value of
    ptReserved: Result := 'RESERVED';
    ptImage: Result := 'Image';
    ptMapped: Result := 'Map';
    ptPrivate: Result := 'Private';
    ptThread: Result := 'Thread';
    ptHeap: Result := 'Heap';
    ptSystem: Result := 'System';
  else
    Result := 'FREE';
  end;
end;

function TfrmMemoryMap.Search(AAddrVA: Int64): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FData.Count - 1 do
    if (AAddrVA >= FData[I].AddrVA) and (AAddrVA < FData[I].EndAddrVA) then
      Exit(I);
end;

procedure TfrmMemoryMap.Refresh(AAddrVA: Int64);
var
  Map: TSimpleMemoryMap;
begin
  lvMemoryMap.BeginUpdate;
  try
    Map := TSimpleMemoryMap.Create(FPid, FIs64Process);
    try
      Map.FillProcessMemoryMap(FData);
      lvMemoryMap.RootNodeCount := FData.Count;
      if not FAutoFited then
      begin
        lvMemoryMap.Header.AutoFitColumns(False);
        lvMemoryMap.Header.Columns[4{$IFDEF LINUX}-1{$ENDIF}].Width := 350;
        lvMemoryMap.Header.AutoSizeIndex := 8{$IFDEF LINUX}-2{$ENDIF};
        FAutoFited := True;
      end;
      FDelayedHighlight := AAddrVA;
      HighLightAddr(AAddrVA);
    finally
      Map.Free;
    end;
  finally
    lvMemoryMap.EndUpdate;
  end;
end;

procedure TfrmMemoryMap.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caFree;
  frmMemoryMap := nil;
end;

procedure TfrmMemoryMap.FormCreate(Sender: TObject);
begin
  FData := TList<TListItemData>.Create;
  {$IFDEF LINUX}
  lvMemoryMap.Header.Columns.Delete(7);
  lvMemoryMap.Header.Columns.Delete(3);
  {$ENDIF}
end;

procedure TfrmMemoryMap.FormDestroy(Sender: TObject);
begin
  FData.Free;
end;

procedure TfrmMemoryMap.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TfrmMemoryMap.FormShow(Sender: TObject);
begin
  {$IFDEF LINUX}
  pnHint.Visible := False;
  {$ENDIF}
  if FDelayedHighlight <> 0 then
  begin
    HighLightAddr(FDelayedHighlight);
    FDelayedHighlight := 0;
  end;
end;

procedure TfrmMemoryMap.lblUrlClick(Sender: TObject);
begin
  OpenURL(lblUrl.Caption);
end;

procedure TfrmMemoryMap.lvMemoryMapAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  PaintInfo.TargetCanvas.Font.PixelsPerInch := Font.PixelsPerInch;
end;

procedure TfrmMemoryMap.lvMemoryMapBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
begin
  if Node = nil then Exit;
  if FData[Node.Index].Grayed then
    ItemColor := $EEEEEE;
end;

procedure TfrmMemoryMap.Init(ACore: TCpuViewCore; AGui: IGuiImplementation;
  APid: Cardinal; AIs64Process: Boolean; AAddrVA: Int64);
begin
  FCore := ACore;
  FGui := AGui;
  FPid := APid;
  FIs64Process := AIs64Process;
  Refresh(AAddrVA);
  Show;
end;

end.

