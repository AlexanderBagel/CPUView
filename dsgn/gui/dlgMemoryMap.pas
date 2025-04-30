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

{ TfrmMemoryMap }

procedure TfrmMemoryMap.lvMemoryMapDblClick(Sender: TObject);
var
  Address: Int64;
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Address := FPages[E.Current^.Index].AddrVA;
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
begin
  {$IFDEF LINUX}
  if Column >= 3 then Inc(Column);
  if Column = 7 then Inc(Column);
  {$ENDIF}
  case Column of
    0: CellText := IntToHex(FPages[Node.Index].AddrVA, 8);
    1: CellText := IntToHex(FPages[Node.Index].Size, 8);
    2: CellText := FPages[Node.Index].Image;
    3: CellText := FPages[Node.Index].Section;
    4: CellText := FPages[Node.Index].Contains;
    5: CellText := PageTypeToStr(FPages[Node.Index].PageType);
    6: CellText := FPages[Node.Index].Access;
    7: CellText := FPages[Node.Index].IAccess;
    8: CellText := FPages[Node.Index].MapedFile;
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
  Clipboard.AsText := IntToHex(FPages[E.Current^.Index].AddrVA, 1);
end;

procedure TfrmMemoryMap.miCopyLineClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Clipboard.AsText :=
    IntToHex(FPages[E.Current^.Index].AddrVA, 8) + #9 +
    IntToHex(FPages[E.Current^.Index].Size, 8) + #9 +
    FPages[E.Current^.Index].Image + #9 +
    {$IFNDEF LINUX}
    FPages[E.Current^.Index].Section + #9 +
    {$ENDIF}
    FPages[E.Current^.Index].Contains + #9 +
    PageTypeToStr(FPages[E.Current^.Index].PageType) + #9 +
    FPages[E.Current^.Index].Access + #9 +
    {$IFNDEF LINUX}
    FPages[E.Current^.Index].IAccess + #9 +
    {$ENDIF}
    FPages[E.Current^.Index].MapedFile;
end;

procedure TfrmMemoryMap.miDumpClick(Sender: TObject);
var
  F: TFileStream;
  E: TVTVirtualNodeEnumerator;
  Page: TPageData;
  Buff: array of Byte;
  BuffSize: Integer;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  Page := FPages[E.Current^.Index];
  SaveDialog.FileName := '0x' + IntToHex(Page.AddrVA) + '.bin';
  if SaveDialog.Execute then
  begin
    F := TFileStream.Create(SaveDialog.FileName, fmCreate);
    try
      BuffSize := FCore.Debugger.Utils.GetPageSize;
      SetLength(Buff, BuffSize);
      while Page.Size > 0 do
      begin
        FCore.Debugger.Utils.ReadData(Pointer(Page.AddrVA), Buff[0], BuffSize);
        F.Write(Buff[0], BuffSize);
        Inc(Page.AddrVA, BuffSize);
        Dec(Page.Size, BuffSize);
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
  PageSize, I: Integer;
  Flags: Cardinal;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  frmPageAccess := TfrmPageAccess.Create(Application);
  try
    Page := FPages[E.Current^.Index];
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
  E: TVTVirtualNodeEnumerator;
begin
  E := lvMemoryMap.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit;
  miDump.Enabled := not (FPages[E.Current^.Index].PageType in [ptFree, ptReserved]);
  miOpenInCpuView.Enabled := miDump.Enabled;
  miSetPageAccess.Enabled := miDump.Enabled;
  {$IFDEF LINUX}
  miSetPageAccess.Visible := False;
  {$ENDIF}
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
  for I := 0 to FPages.Count - 1 do
    if (AAddrVA >= FPages[I].AddrVA) and (AAddrVA < FPages[I].EndAddrVA) then
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
      Map.FillProcessMemoryMap(FPages);
      lvMemoryMap.RootNodeCount := FPages.Count;
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
begin
  if Node = nil then Exit;
  if FPages[Node.Index].Grayed then
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

