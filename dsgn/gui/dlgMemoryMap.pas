unit dlgMemoryMap;

{$mode Delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus, Clipbrd, laz.VirtualTrees,
  Generics.Collections, Math,

  CpuView.Common,
  CpuView.Core,
  CpuView.Design.DpiFix,

  {$IFDEF LINUX}
  CpuView.Linux,
  CpuView.Linux.MMap,
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Windows,
  CpuView.Windows,
  CpuView.Windows.MMap,
  {$ENDIF}

  dlgInputBox,
  dlgPageAccess;

type

  { TLazVirtualStringTree }

  TLazVirtualStringTree = class(TLazVSTWithDPI);

  { TfrmMemoryMap }

  TfrmMemoryMap = class(TForm)
    lblUrl: TLabel;
    lblPromt: TLabel;
    lvMemoryMap: TLazVirtualStringTree;
    miSetPageAccess: TMenuItem;
    miDump: TMenuItem;
    miSearchAddr: TMenuItem;
    miCopyLine: TMenuItem;
    miCopyAddr: TMenuItem;
    miOpenInCpuView: TMenuItem;
    pnHint: TPanel;
    pmList: TPopupMenu;
    SaveDialog: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
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
    procedure miDumpClick(Sender: TObject);
    procedure miOpenInCpuViewClick(Sender: TObject);
    procedure miSearchAddrClick(Sender: TObject);
    procedure miSetPageAccessClick(Sender: TObject);
    procedure pmListPopup(Sender: TObject);
  private
    FAutoFited: Boolean;
    FCore: TCpuViewCore;
    FPages: TList<TPageData>;
    FGui: IGuiImplementation;
    FPid: Cardinal;
    FIs64Process: Boolean;
    FDelayedHighlight: Int64;
    function CheckAddressCallback(ANewAddrVA: Int64): Boolean;
    procedure HighLightAddr(AAddrVA: Int64);
    function PageTypeToStr(Value: TPageType): string;
    function Search(AAddrVA: Int64; out Index, ChildIndex: Integer): Boolean;
  public
    procedure Init(ACore: TCpuViewCore; AGui: IGuiImplementation;
      APid: Cardinal; AIs64Process: Boolean; AAddrVA: Int64);
    procedure Refresh(AAddrVA: Int64);
  end;

var
  frmMemoryMap: TfrmMemoryMap;

implementation

{$R *.lfm}

type
  PNodeData = ^TNodeData;
  TNodeData = record
    RootIndex, ChildIndex: Integer;
  end;

{ TfrmMemoryMap }

procedure TfrmMemoryMap.lvMemoryMapDblClick(Sender: TObject);
var
  Address: Int64;
  E: TVTVirtualNodeEnumerator;
  pData: PNodeData;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  pData := lvMemoryMap.GetNodeData(E.Current);
  if pData^.ChildIndex < 0 then
    Address := FPages[pData^.RootIndex].AddrVA
  else
    Address := FPages[pData^.RootIndex].Contains[pData^.ChildIndex].AddrVA;
  if FCore.AddrInAsm(Address) then
    FGui.OpenInDisassembler(Address)
  else
    if FCore.AddrInDump(Address) then
      FGui.OpenInDump(Address, False)
    else
      SysUtils.Beep;
end;

procedure TfrmMemoryMap.lvMemoryMapGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  pData: PNodeData;
begin
  {$IFDEF LINUX}
  if Column >= 3 then Inc(Column);
  if Column = 7 then Inc(Column);
  {$ENDIF}
  pData := lvMemoryMap.GetNodeData(Node);
  if pData^.ChildIndex < 0 then
  begin
    case Column of
      0: CellText := IntToHex(FPages[pData^.RootIndex].AddrVA, 8);
      1: CellText := IntToHex(FPages[pData^.RootIndex].Size, 8);
      2: CellText := FPages[pData^.RootIndex].Image;
      3: CellText := FPages[pData^.RootIndex].Section;
      4:
        if lvMemoryMap.Expanded[Node] then
          CellText := ''
        else
          CellText := FPages[pData^.RootIndex].ContainsStr;
      5: CellText := PageTypeToStr(FPages[pData^.RootIndex].PageType);
      6: CellText := FPages[pData^.RootIndex].Access;
      7: CellText := FPages[pData^.RootIndex].IAccess;
      8: CellText := FPages[pData^.RootIndex].MapedFile;
    end;
    Exit;
  end;
  case Column of
    0: CellText := IntToHex(FPages[pData^.RootIndex].Contains[pData^.ChildIndex].AddrVA, 8);
    1: CellText := IntToHex(FPages[pData^.RootIndex].Contains[pData^.ChildIndex].Size, 8);
    4: CellText := FPages[pData^.RootIndex].Contains[pData^.ChildIndex].Caption;
  else
    CellText := '';
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
  Address: Int64;
  pData: PNodeData;
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  pData := lvMemoryMap.GetNodeData(E.Current);
  if pData^.ChildIndex < 0 then
    Address := FPages[pData^.RootIndex].AddrVA
  else
    Address := FPages[pData^.RootIndex].Contains[pData^.ChildIndex].AddrVA;
  Clipboard.AsText := IntToHex(Address, 1);
end;

procedure TfrmMemoryMap.miCopyLineClick(Sender: TObject);
var
  pData: PNodeData;
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  pData := lvMemoryMap.GetNodeData(E.Current);
  if pData^.ChildIndex < 0 then
    Clipboard.AsText :=
      IntToHex(FPages[pData^.RootIndex].AddrVA, 8) + #9 +
      IntToHex(FPages[pData^.RootIndex].Size, 8) + #9 +
      FPages[pData^.RootIndex].Image + #9 +
      {$IFNDEF LINUX}
      FPages[pData^.RootIndex].Section + #9 +
      {$ENDIF}
      FPages[pData^.RootIndex].ContainsStr + #9 +
      PageTypeToStr(FPages[pData^.RootIndex].PageType) + #9 +
      FPages[pData^.RootIndex].Access + #9 +
      {$IFNDEF LINUX}
      FPages[pData^.RootIndex].IAccess + #9 +
      {$ENDIF}
      FPages[pData^.RootIndex].MapedFile
  else
    Clipboard.AsText :=
      IntToHex(FPages[pData^.RootIndex].Contains[pData^.ChildIndex].AddrVA, 8) + #9 +
      IntToHex(FPages[pData^.RootIndex].Contains[pData^.ChildIndex].Size, 8) + #9 +
      FPages[pData^.RootIndex].Contains[pData^.ChildIndex].Caption;
end;

procedure TfrmMemoryMap.miDumpClick(Sender: TObject);
var
  F: TFileStream;
  pData: PNodeData;
  E: TVTVirtualNodeEnumerator;
  Buff: array of Byte;
  BuffSize: Integer;
  AddrVA, PageSize: Int64;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  pData := lvMemoryMap.GetNodeData(E.Current);
  if pData^.ChildIndex < 0 then
  begin
    AddrVA := FPages[pData^.RootIndex].AddrVA;
    PageSize := FPages[pData^.RootIndex].Size;
  end
  else
  begin
    AddrVA := FPages[pData^.RootIndex].Contains[pData^.ChildIndex].AddrVA;
    PageSize := FPages[pData^.RootIndex].Contains[pData^.ChildIndex].Size;
  end;
  SaveDialog.FileName := '0x' + IntToHex(AddrVA) + '.bin';
  if SaveDialog.Execute then
  begin
    F := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      BuffSize := Min(FCore.Debugger.Utils.GetPageSize, PageSize);
      SetLength(Buff, BuffSize);
      while PageSize > 0 do
      begin
        FCore.Debugger.Utils.ReadData(Pointer(AddrVA), Buff[0], BuffSize);
        F.Write(Buff[0], BuffSize);
        Inc(AddrVA, BuffSize);
        Dec(PageSize, BuffSize);
      end;
    finally
      F.Free;
    end;
  end;
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

procedure TfrmMemoryMap.miSetPageAccessClick(Sender: TObject);
{$IFDEF LINUX}
begin
  raise Exception.Create('SetPageAccess not yet implemented!');
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  E: TVTVirtualNodeEnumerator;
  Page: TPageData;
  pData: PNodeData;
  PageSize, I: Integer;
  Flags: Cardinal;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  pData := lvMemoryMap.GetNodeData(E.Current);
  frmPageAccess := TfrmPageAccess.Create(Application);
  try
    Page := FPages[pData^.RootIndex];
    PageSize := FCore.Debugger.Utils.GetPageSize;
    if frmPageAccess.ShowPageAccess(Page.AddrVA, Page.Size, PageSize) <> mrOK then Exit;
    Flags := 0;
    if frmPageAccess.rbNoAcccess.Checked then Flags := PAGE_NOACCESS;
    if frmPageAccess.rbReadOnly.Checked then Flags := PAGE_READONLY;
    if frmPageAccess.rbReadWrite.Checked then Flags := PAGE_READWRITE;
    if frmPageAccess.rbExecute.Checked then Flags := PAGE_EXECUTE;
    if frmPageAccess.rbExecuteRead.Checked then Flags := PAGE_EXECUTE_READ;
    if frmPageAccess.rbFull.Checked then Flags := PAGE_EXECUTE_READWRITE;
    if frmPageAccess.rbWriteCopy.Checked then Flags := PAGE_WRITECOPY;
    if frmPageAccess.rbExecuteWriteCopy.Checked then Flags := PAGE_EXECUTE_WRITECOPY;
    if frmPageAccess.cbPageGuard.Checked then Flags := Flags or PAGE_GUARD;
    for I := 0 to frmPageAccess.SelectedList.Count - 1 do
    begin
      if FCore.Debugger.Utils.SetPageAccess(frmPageAccess.SelectedList[I], PageSize, Flags) then
        TraceLog.Log(Format('New attributes have been set for page %p: %s',
          [frmPageAccess.SelectedList[I], ExtractAccessString(Flags)]), False)
      else
        TraceLog.Log(Format('Error setting attributes for page %p. %d - %s',
          [frmPageAccess.SelectedList[I], GetLastError, SysErrorMessage(GetLastError)]), False);
    end;
  finally
    frmPageAccess.Free;
  end;
  Refresh(Page.AddrVA);
end;
{$ENDIF}

procedure TfrmMemoryMap.pmListPopup(Sender: TObject);
var
  pData: PNodeData;
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  pData := lvMemoryMap.GetNodeData(E.Current);
  miDump.Enabled := not (FPages[pData^.RootIndex].PageType in [ptFree, ptReserved]);
  miOpenInCpuView.Enabled := miDump.Enabled;
  miSetPageAccess.Enabled := miDump.Enabled;
  {$IFDEF LINUX}
  miSetPageAccess.Visible := False;
  {$ENDIF}
end;

function TfrmMemoryMap.CheckAddressCallback(ANewAddrVA: Int64): Boolean;
var
  I, A: Integer;
begin
  Result := Search(ANewAddrVA, I, A);
end;

procedure TfrmMemoryMap.HighLightAddr(AAddrVA: Int64);
var
  E: TVTVirtualNodeEnumerator;
  Idx, cIdx: Integer;
  pData: PNodeData;
begin
  if Search(AAddrVA, Idx, cIdx) then
  begin
    E := lvMemoryMap.Nodes.GetEnumerator;
    repeat
      if not E.MoveNext then Break;
      pData := lvMemoryMap.GetNodeData(E.Current);
    until (pData^.RootIndex = Idx) and (pData^.ChildIndex = cIdx);
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

function TfrmMemoryMap.Search(AAddrVA: Int64; out Index, ChildIndex: Integer
  ): Boolean;
var
  I, A: Integer;
begin
  Result := False;
  Index := -1;
  ChildIndex := -1;
  for I := 0 to FPages.Count - 1 do
    if (AAddrVA >= FPages[I].AddrVA) and (AAddrVA < FPages[I].EndAddrVA) then
    begin
      Index := I;
      Result := True;
      for A := 0 to Length(FPages[I].Contains) - 1 do
        if (AAddrVA >= FPages[I].Contains[A].AddrVA) and (AAddrVA < FPages[I].Contains[A].EndAddrVA) then
        begin
          ChildIndex := A;
          Exit;
        end;
    end;
end;

procedure TfrmMemoryMap.Refresh(AAddrVA: Int64);
var
  Map: TSimpleMemoryMap;
  I, A: Integer;
  pData: PNodeData;
  ANode, AChildNode: PVirtualNode;
begin
  lvMemoryMap.BeginUpdate;
  try
    Map := TSimpleMemoryMap.Create(FPid, FIs64Process);
    try
      Map.FillProcessMemoryMap(FPages);
      lvMemoryMap.RootNodeCount := 0;
      lvMemoryMap.NodeDataSize := SizeOf(TPageData);
      for I := 0 to FPages.Count - 1 do
      begin
        ANode := lvMemoryMap.AddChild(lvMemoryMap.RootNode, nil);
        pData := lvMemoryMap.GetNodeData(ANode);
        pData^.RootIndex := I;
        pData^.ChildIndex := -1;
        for A := 0 to Length(FPages[I].Contains) - 1 do
        begin
          AChildNode := lvMemoryMap.AddChild(ANode, nil);
          pData := lvMemoryMap.GetNodeData(AChildNode);
          pData^.RootIndex := I;
          pData^.ChildIndex := A;
        end;
      end;
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
  FPages := TList<TPageData>.Create;
  {$IFDEF LINUX}
  lvMemoryMap.Header.Columns.Delete(7);
  lvMemoryMap.Header.Columns.Delete(3);
  {$ENDIF}
end;

procedure TfrmMemoryMap.FormDestroy(Sender: TObject);
begin
  FPages.Free;
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
var
  pData: PNodeData;
begin
  if Node = nil then Exit;
  pData := lvMemoryMap.GetNodeData(Node);
  if FPages[pData^.RootIndex].Grayed then
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

