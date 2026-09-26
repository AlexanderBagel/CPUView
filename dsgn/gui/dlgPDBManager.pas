////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgPDBManager.pas
//  * Purpose   : Debug PDB Symbol Loading Dialog
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2026.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//  *
//  * SPDX-License-Identifier: MIT
//  * See LICENSE file in the project root for full license information.
//  *
//  ****************************************************************************
//

unit dlgPDBManager;

{$mode Delphi}

interface

{$I CpuViewCfg.inc}

uses
  Windows, LCLIntf,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Menus,
  laz.VirtualTrees, Generics.Collections, Generics.Defaults, LazFileUtils,

  FWHexView.Common,

  CpuView.Core,
  CpuView.Common,
  CpuView.Design.DpiFix,
  CpuView.Windows.Pdb;

type
  TPdbFile = record
    PdbPath, BinaryPath, RemotePath: string;
    Key: TPdbKey;
    Downloaded, Size: Int64;
    State: TPdbState;
    SymCount: Integer;
    Pdb: TPdb;
  end;

  TPdbFileList = class(TListEx<TPdbFile>);

  { TStatusBar }

  TStatusBar = class(TStatusBarWithDPI);

  { TLazVirtualStringTree }

  TLazVirtualStringTree = class(TLazVSTWithDPI);

  { TfrmPdbManager }

  TfrmPdbManager = class(TForm)
    mnuOpenBinary: TMenuItem;
    mnuOpenPDB: TMenuItem;
    mnuStop: TMenuItem;
    mnuResume: TMenuItem;
    mnuPause: TMenuItem;
    mnuDownloadAll: TMenuItem;
    mnuDownloadSel: TMenuItem;
    pmPdb: TPopupMenu;
    Separator1: TMenuItem;
    vstPdbData: TLazVirtualStringTree;
    StatusBar: TStatusBar;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuDownloadAllClick(Sender: TObject);
    procedure mnuDownloadSelClick(Sender: TObject);
    procedure mnuOpenBinaryClick(Sender: TObject);
    procedure mnuOpenPDBClick(Sender: TObject);
    procedure mnuPauseClick(Sender: TObject);
    procedure mnuResumeClick(Sender: TObject);
    procedure mnuStopClick(Sender: TObject);
    procedure pmPdbPopup(Sender: TObject);
    procedure vstPdbDataAddToSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vstPdbDataAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure vstPdbDataBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure vstPdbDataGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vstPdbDataGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstPdbDataHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo
      );
  private
    FCore: TCpuViewCore;
    FPdbStorage: TPdbStorage;
    FFiles: TPdbFileList;
    FFilter: TList<Integer>;
    FInited: Boolean;
    FRemotePdb: TList<TPdb>;
    FQueue: TSymQueue;
    FBatchQueryMode, FClosePending: Boolean;
    FComparer: IEqualityComparer<TPdbKey>;
    FDict: TDictionary<TPdbKey, Integer>;
    FTotalSize, FSelCount, FSelSize: Integer;
    FSymSrvState, FPausedState: string;
    FDownloadCount: Integer;
    function CheckIsReadyForDownloadPresent: Boolean;
    function CheckIsSelectedReadyForDownload: Boolean;
    procedure CreateQueue(JobsCount: Integer);
    procedure DestroyQueue;
    function GetSelectedIndex: Integer;
    procedure Init;
    procedure InvalidateNode(PdbKey: TPdbKey);
    procedure Fill;
    procedure ReInit;
    procedure RunQueryThread;
    procedure ScanLoadedPdb;
    procedure Sort;
    procedure SymQueueBatchDone(Sender: TObject);
    procedure SymQueueJobDone(Sender: TObject; const Job: TSymJob; const Error: string);
    procedure SymQueueJobProgress(Sender: TObject; State: TSymSrvProgressState;
      Pdb: TPdb; BytesReceived, TotalBytes: Int64);
    procedure SymQueueStopped(Sender: TObject);
    procedure UpdateStatusBar;
    procedure UpdateStatusBarState(const Value: string);
  protected
    procedure DoShow; override;
  public
    property Core: TCpuViewCore read FCore write FCore;
    property DownloadCount: Integer read FDownloadCount;
    property PdbStorage: TPdbStorage read FPdbStorage write FPdbStorage;
  end;

var
  frmPdbManager: TfrmPdbManager;

implementation

{$R *.lfm}

{ TfrmPdbManager }

procedure TfrmPdbManager.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FClosePending then
  begin
    CanClose := False;
    Exit;
  end;
  if FQueue <> nil then
  begin
    CanClose := False;
    FQueue.Pause;
    try
      if MessageBox(Handle, PChar('Symbol server operation in progress.' + sLineBreak +
        'Stop it and close the window?'), PChar(Application.Title),
        MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDYES then
      begin
        DestroyQueue;
        FClosePending := True;
      end;
    finally
      if not FClosePending then
        FQueue.Resume;
    end;
  end;
end;

procedure TfrmPdbManager.FormCreate(Sender: TObject);
begin
  FFiles := TPdbFileList.Create;
  FFilter := TList<Integer>.Create;
  FRemotePdb := TList<TPdb>.Create;
  FComparer := TPdbKeyComparer.Create;
  FDict := TDictionary<TPdbKey, Integer>.Create(FComparer);
end;

procedure TfrmPdbManager.FormDestroy(Sender: TObject);
begin
  FFiles.Free;
  FFilter.Free;
  FRemotePdb.Free;
  FDict.Free;
  frmPdbManager := nil;
end;

procedure TfrmPdbManager.mnuDownloadAllClick(Sender: TObject);
var
  List: TList<TPdb>;
  Pdb: TPdb;
begin
  List := TList<TPdb>.Create;
  try
    for Pdb in FRemotePdb do
      if Pdb.State = psAvailableForLoading then
        List.Add(Pdb);
    if List.Count = 0 then Exit;
    CreateQueue(List.Count);
    UpdateStatusBarState('SYMSRV: Download...');
    for Pdb in List do
      FQueue.DownloadPdb(Pdb);
  finally
    List.Free;
  end;
end;

procedure TfrmPdbManager.mnuDownloadSelClick(Sender: TObject);
var
  E: TVTVirtualNodeEnumerator;
  Idx: Integer;
  List: TList<Integer>;
begin
  E := vstPdbData.SelectedNodes.GetEnumerator;
  List := TList<Integer>.Create;
  try
    while E.MoveNext do
    begin
      Idx := FFilter[E.Current^.Index];
      if FFiles.List[Idx].State = psAvailableForLoading then
        List.Add(Idx);
    end;
    if List.Count = 0 then Exit;
    CreateQueue(List.Count);
    UpdateStatusBarState('SYMSRV: Download...');
    for Idx in List do
      FQueue.DownloadPdb(PdbStorage.QueryPDB(FFiles.List[Idx].Key));
  finally
    List.Free;
  end;
end;

procedure TfrmPdbManager.mnuOpenBinaryClick(Sender: TObject);
begin
  OpenURL(FFiles.List[GetSelectedIndex].BinaryPath);
end;

procedure TfrmPdbManager.mnuOpenPDBClick(Sender: TObject);
begin
  OpenURL(FFiles.List[GetSelectedIndex].PdbPath);
end;

procedure TfrmPdbManager.mnuPauseClick(Sender: TObject);
begin
  FPausedState := FSymSrvState;
  FQueue.Pause;
  UpdateStatusBarState('SYMSRV: Pause');
end;

procedure TfrmPdbManager.mnuResumeClick(Sender: TObject);
begin
  FQueue.Resume;
  UpdateStatusBarState(FPausedState);
end;

procedure TfrmPdbManager.mnuStopClick(Sender: TObject);
begin
  DestroyQueue;
  FBatchQueryMode := True;
  SymQueueBatchDone(nil);
end;

procedure TfrmPdbManager.pmPdbPopup(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := GetSelectedIndex;
  mnuDownloadAll.Visible := (FQueue = nil) and CheckIsReadyForDownloadPresent;
  mnuDownloadSel.Visible := mnuDownloadAll.Visible and CheckIsSelectedReadyForDownload;
  mnuPause.Visible := Assigned(FQueue) and (FQueue.ContinueState = scsRun);
  mnuResume.Visible := Assigned(FQueue) and (FQueue.ContinueState = scsPause);
  mnuStop.Visible := Assigned(FQueue);
  mnuOpenPDB.Visible := (Idx >= 0) and FileExists(FFiles.List[Idx].PdbPath);
  mnuOpenBinary.Visible := (Idx >= 0) and FileExists(FFiles.List[Idx].BinaryPath);
end;

procedure TfrmPdbManager.vstPdbDataAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  E: TVTVirtualNodeEnumerator;
begin
  E := vstPdbData.SelectedNodes.GetEnumerator;
  FSelSize := 0;
  FSelCount := 0;
  while E.MoveNext do
  begin
    Inc(FSelCount);
    Inc(FSelSize, FFiles.List[FFilter[E.Current^.Index]].Size);
  end;
  UpdateStatusBar;
end;

procedure TfrmPdbManager.vstPdbDataAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
const
  ProgressBarHeight = 3;
  ValidProgressColor = $00D77800;
  InvalidProgressColor = $000078D7;
var
  PdbFile: TPdbFile;
  Percent: Double;
  FullRowRect, ProgressRect, VisiblePart: TRect;
begin
  PdbFile := FFiles[FFilter[Node.Index]];
  if PdbFile.State <> psLoading then
    Exit;

  if PdbFile.Size = 0 then
    Percent := 1.0
  else
  begin
    Percent := PdbFile.Downloaded / PdbFile.Size;
    if Percent > 1.0 then Percent := 1.0;
    if Percent < 0 then Percent := 0;
  end;

  FullRowRect := Rect(0, CellRect.Top, Sender.ClientWidth, CellRect.Bottom);

  ProgressRect := Rect(
    FullRowRect.Left,
    FullRowRect.Bottom - MulDiv(ProgressBarHeight, CurrentPPI, 96),
    FullRowRect.Left + Round(FullRowRect.Width * Percent),
    FullRowRect.Bottom);

  VisiblePart := TRect.Empty;
  if IntersectRect(VisiblePart, ProgressRect, CellRect) then
  begin
    if PdbFile.Size = 0 then
      TargetCanvas.Brush.Color := InvalidProgressColor
    else
      TargetCanvas.Brush.Color := ValidProgressColor;
    TargetCanvas.FillRect(VisiblePart);
  end;
end;

procedure TfrmPdbManager.vstPdbDataBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
const
  clPdbActual    = TColor($00C7E4B7);
  clPdbAvailable = TColor($00BFFFF4);
  clPdbLoading   = TColor($00FFAB94);
  clPdbBroken    = TColor($00A8A9F4);
  clPdbMissing   = TColor($00D4D9FF);
var
  PdbFile: TPdbFile;
  AColor: TColor;
begin
  if not (Column in [1, 2]) then Exit;
  PdbFile := FFiles[FFilter[Node.Index]];
  if (Column = 1) and (PdbFile.State <> psNotFoundAnywhere) then Exit;
  case PdbFile.State of
    psNotFoundLocally, psNotFoundAnywhere: AColor := clPdbMissing;
    psNoSections, psBroken: AColor := clPdbBroken;
    psAvailableForLoading: AColor := clPdbAvailable;
    psLoading: AColor := clPdbLoading;
    psReady: AColor := clPdbActual;
  else
    AColor := 0;
  end;
  if AColor <> 0 then
  begin
    TargetCanvas.Brush.Color := AColor;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TfrmPdbManager.vstPdbDataGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  CellText: string;
  CellRect: TRect;
  TextRect: TRect;
begin
  HintText := '';
  if Column < 0 then Exit;
  CellText := vstPdbData.Text[Node, Column];
  if CellText = '' then Exit;
  CellRect := vstPdbData.GetDisplayRect(Node, Column, True);
  InflateRect(CellRect, -1, 0);
  vstPdbData.Canvas.Font := vstPdbData.Font;
  TextRect := CellRect;
  DrawText(vstPdbData.Canvas, PChar(CellText), Length(CellText), TextRect,
    DT_CALCRECT or DT_SINGLELINE or DT_NOPREFIX);
  if (TextRect.Right - TextRect.Left) > (CellRect.Right - CellRect.Left) then
    HintText := CellText;
end;

function SizeToStr(Value: Int64): string;
const
  Units: array[0..4] of string = ('bytes', 'KB', 'MB', 'GB', 'TB');
var
  Size: Double;
  UnitIdx: Integer;
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

  if Value < 1024 then
    Exit(Format('%d %s', [Value, Units[0]]));

  Size := Value;
  UnitIdx := 0;
  while (Size >= 1024) and (UnitIdx < High(Units)) do
  begin
    Size := Size / 1024;
    Inc(UnitIdx);
  end;
  Result := Format('%.2f %s', [Size, Units[UnitIdx]], FS);
end;

procedure TfrmPdbManager.vstPdbDataGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
const
  PdbStateStr: array [TPdbState] of string = (
    'NotFoundLocally',
    'NotFoundAnywhere',
    'AvailableForLoad',
    'Loading',
    'NoSections',
    'Broken',
    'Ready'
  );
var
  PdbFile: TPdbFile;
begin
  PdbFile := FFiles[FFilter[Node.Index]];
  case Column of
    0: CellText := PdbFile.BinaryPath;
    1:
    begin
      if PdbFile.State = psAvailableForLoading then
        CellText := PdbFile.RemotePath
      else
        CellText := PdbFile.Key.Name;
    end;
    2:
    begin
      if (PdbFile.State = psNotFoundLocally) and Assigned(FQueue) then
        CellText := 'Query...'
      else
        CellText := PdbStateStr[PdbFile.State];
    end;
    3:
    begin
      if PdbFile.State = psLoading then
      begin
        if PdbFile.Size <> 0 then
          CellText := Format('%s of %s', [SizeToStr(PdbFile.Downloaded), SizeToStr(PdbFile.Size)])
        else
          CellText := SizeToStr(PdbFile.Downloaded);
      end
      else
      begin
        if PdbFile.Size <> 0 then
          CellText := SizeToStr(PdbFile.Size)
        else
          CellText := '';
      end;
    end;
    4:
    begin
      if PdbFile.State in [psNoSections, psReady] then
        CellText := IntToStr(PdbFile.SymCount)
      else
        CellText := '';
    end;
  end;
end;

procedure TfrmPdbManager.vstPdbDataHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  Sort;
end;

function TfrmPdbManager.CheckIsReadyForDownloadPresent: Boolean;
var
  Pdb: TPdb;
begin
  Result := False;
  for Pdb in FRemotePdb do
    if Pdb.State = psAvailableForLoading then
      Exit(True);
end;

function TfrmPdbManager.CheckIsSelectedReadyForDownload: Boolean;
var
  E: TVTVirtualNodeEnumerator;
begin
  Result := False;
  E := vstPdbData.SelectedNodes.GetEnumerator;
  while E.MoveNext do
    if FFiles.List[FFilter[E.Current^.Index]].State = psAvailableForLoading then
      Exit(True);
end;

procedure TfrmPdbManager.CreateQueue(JobsCount: Integer);
begin
  FQueue := TSymQueue.Create(PdbStorage, Min(JobsCount, 8));
  FQueue.OnBatchDone := SymQueueBatchDone;
  FQueue.OnJobDone := SymQueueJobDone;
  FQueue.OnProgress := SymQueueJobProgress;
  FQueue.OnStopped := SymQueueStopped;
  FBatchQueryMode := True;
end;

procedure TfrmPdbManager.DestroyQueue;
begin
  if Assigned(FQueue) then
  begin
    FQueue.OnJobDone := nil;
    FQueue.OnBatchDone := nil;
    FQueue.OnProgress := nil;
    UpdateStatusBarState('SYMSRV: Waiting for symbol server tasks to finish...');
    StatusBar.Repaint;
    FQueue.Stop;
  end;
end;

function TfrmPdbManager.GetSelectedIndex: Integer;
var
  E: TVTVirtualNodeEnumerator;
begin
  E := vstPdbData.SelectedNodes.GetEnumerator;
  if not E.MoveNext then Exit(-1);
  Result := FFilter[E.Current^.Index];
end;

procedure TfrmPdbManager.Init;
begin
  FFiles.Clear;
  FFilter.Clear;
  FRemotePdb.Clear;
  FDict.Clear;
  ScanLoadedPdb;
end;

procedure TfrmPdbManager.InvalidateNode(PdbKey: TPdbKey);
var
  I: Integer;
  Node: PVirtualNode;
begin
  for I := 0 to FFilter.Count - 1 do
    if IsPdbKeyEqual(PdbKey, FFiles.List[FFilter[I]].Key) then
    begin
      Node := vstPdbData.GetFirst;
      while (Node <> nil) and (Node^.Index < Cardinal(I)) do
        Node := vstPdbData.GetNext(Node);
      if Node = nil then
        vstPdbData.Invalidate
      else
        vstPdbData.InvalidateNode(Node);
    end;
end;

procedure TfrmPdbManager.Fill;
var
  I: Integer;
begin
  FTotalSize := 0;
  for I := 0 to FFiles.Count - 1 do
    Inc(FTotalSize, FFiles.List[I].Size);
  vstPdbData.BeginUpdate;
  try
    vstPdbData.RootNodeCount := FFiles.Count;
    Sort;
  finally
    vstPdbData.EndUpdate;
  end;
  UpdateStatusBar;
  UpdateStatusBarState(FSymSrvState);
end;

procedure TfrmPdbManager.ReInit;
begin
  Init;
  Fill;
end;

procedure TfrmPdbManager.RunQueryThread;
var
  Pdb: TPdb;
  List: TList<TPdb>;
begin
  if FRemotePdb.Count = 0 then Exit;
  List := TList<TPdb>.Create;
  try
    for Pdb in FRemotePdb do
      if Pdb.State = psNotFoundLocally then
        List.Add(Pdb);
    if List.Count = 0 then Exit;
    CreateQueue(List.Count);
    for Pdb in FRemotePdb do
      FQueue.QueryPdb(Pdb);
  finally
    List.Free;
  end;
  UpdateStatusBarState('SYMSRV: querying...');
end;

procedure TfrmPdbManager.ScanLoadedPdb;
var
  Pdb: TPdb;
  PdbFile: TPdbFile;
  PdbKey: TPdbKey;
  Idx: Integer;
  List: TImageDataList;
  Image: TImageData;
  LastImagePath: string;
begin
  LastImagePath := '';
  List := Core.Debugger.Utils.QueryLoadedImages(Core.Debugger.PointerSize = 4);
  for Image in List do
  begin
    if LastImagePath = Image.ImagePath then Continue;
    LastImagePath := Image.ImagePath;
    if not GetImagePdbKey(Image.ImagePath, PdbKey) then Continue;
    Pdb := PdbStorage.QueryPDB(PdbKey);
    PdbFile := Default(TPdbFile);
    PdbFile.Key := Pdb.Key;
    PdbFile.BinaryPath := Image.ImagePath;
    PdbFile.RemotePath := Pdb.GetRemotePath;
    PdbFile.PdbPath := Pdb.LocalPath;
    PdbFile.State := Pdb.State;
    if FileExists(Pdb.LocalPath) then
      PdbFile.Size := FileSizeUtf8(Pdb.LocalPath);
    PdbFile.SymCount := Pdb.Count;
    Idx := FFiles.Add(PdbFile);
    FDict.Add(PdbFile.Key, Idx);
    FFilter.Add(Idx);
    FFiles.List[Idx].Pdb := Pdb;
    if Pdb.State in [psNotFoundLocally, psAvailableForLoading] then
      FRemotePdb.Add(Pdb);
  end;
end;

function DefaultFilterDataComparer(
  {$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} A, B: Integer): Integer;
begin
  Result := 0;
  if A = B then Exit;
  case frmPdbManager.vstPdbData.Header.SortColumn of
    0: Result := AnsiCompareText(frmPdbManager.FFiles.List[A].BinaryPath, frmPdbManager.FFiles.List[B].BinaryPath);
    1: Result := AnsiCompareText(frmPdbManager.FFiles.List[A].Key.Name, frmPdbManager.FFiles.List[B].Key.Name);
    2: Result := Integer(frmPdbManager.FFiles.List[A].State) - Integer(frmPdbManager.FFiles.List[B].State);
    3: Result := frmPdbManager.FFiles.List[A].Size - frmPdbManager.FFiles.List[B].Size;
    4: Result := frmPdbManager.FFiles.List[A].SymCount - frmPdbManager.FFiles.List[B].SymCount;
  end;
  if frmPdbManager.vstPdbData.Header.SortDirection = sdDescending then
    Result := -Result;
end;

procedure TfrmPdbManager.Sort;
begin
  if vstPdbData.Header.SortColumn < 0 then Exit;
  FFilter.Sort(TComparer<Integer>.Construct(DefaultFilterDataComparer));
end;

procedure TfrmPdbManager.SymQueueBatchDone(Sender: TObject);
const
  StateDone: array [Boolean] of TPdbState = (psReady, psAvailableForLoading);
var
  I, Cnt: Integer;
begin
  Cnt := 0;
  for I := 0 to FRemotePdb.Count - 1 do
    if FRemotePdb[I].State = StateDone[FBatchQueryMode] then
      Inc(Cnt);
  if FBatchQueryMode then
    UpdateStatusBarState(Format('SYMSRV: %d file(s) available for download', [Cnt]))
  else
  begin
    UpdateStatusBarState(Format('SYMSRV: %d file(s) loaded', [Cnt]));
    ReInit;
  end;
  FQueue.Stop;
end;

procedure TfrmPdbManager.SymQueueJobDone(Sender: TObject; const Job: TSymJob;
  const Error: string);
var
  Idx: Integer;
begin
  if FDict.TryGetValue(Job.Pdb.Key, Idx) then
  begin
    FFiles.List[Idx].State := Job.Pdb.State;
    case Job.Pdb.State of
      psAvailableForLoading:
        FFiles.List[Idx].RemotePath := Job.Pdb.GetRemotePath;
      psNoSections, psReady:
      begin
        SavePdbInfo(Job.Pdb, FFiles.List[Idx].BinaryPath);
        FFiles.List[Idx].PdbPath := Job.Pdb.LocalPath;
        FFiles.List[Idx].Size := FileSizeUtf8(FFiles.List[Idx].PdbPath);
        Job.Pdb.LoadFromFile(Job.Pdb.LocalPath);
        FFiles.List[Idx].Downloaded := 0;
        FFiles.List[Idx].SymCount := Job.Pdb.Count;
        Inc(FDownloadCount);
      end;
    end;
    InvalidateNode(Job.Pdb.Key);
  end;
end;

procedure TfrmPdbManager.SymQueueJobProgress(Sender: TObject;
  State: TSymSrvProgressState; Pdb: TPdb; BytesReceived, TotalBytes: Int64);
var
  Idx: Integer;
begin
  FBatchQueryMode := False;
  if FDict.TryGetValue(Pdb.Key, Idx) then
  begin
    FFiles.List[Idx].State := psLoading;
    FFiles.List[Idx].Size := TotalBytes;
    FFiles.List[Idx].Downloaded := BytesReceived;
    InvalidateNode(Pdb.Key);
  end;
end;

procedure TfrmPdbManager.SymQueueStopped(Sender: TObject);
begin
  FreeAndNil(FQueue);
  if FClosePending then
  begin
    FClosePending := False;
    ModalResult := mrOK
  end
  else
    ReInit;
end;

procedure TfrmPdbManager.UpdateStatusBar;
begin
  StatusBar.Panels[0].Text := Format('Total: %d, size: %s', [vstPdbData.RootNodeCount, SizeToStr(FTotalSize)]);
  StatusBar.Panels[1].Text := Format('Select: %d, size: %s', [FSelCount, SizeToStr(FSelSize)]);
end;

procedure TfrmPdbManager.UpdateStatusBarState(const Value: string);
begin
  StatusBar.Panels[2].Text := Value;
  FSymSrvState := Value;
end;

procedure TfrmPdbManager.DoShow;
begin
  inherited DoShow;
  if not FInited then
  begin
    FInited := True;
    Init;
    if FRemotePdb.Count > 0 then
    begin
      vstPdbData.Header.SortColumn := 2;
      vstPdbData.Header.SortDirection := sdAscending;
    end;
    Fill;
    RunQueryThread;
  end;
end;

end.

