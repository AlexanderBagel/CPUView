////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewOptions.pas
//  * Purpose   : Main settings window frame for Lazarus.
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

unit frmCpuViewOptions;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LMessages, Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  Graphics, laz.VirtualTrees, ImgList, Spin, Math, Themes,
  {$IFDEF MSWINDOWS}
  Win32Themes, UxTheme,
  {$ENDIF}

  IDEOptEditorIntf,

  frmCpuViewBaseOptions,

  FWHexView,
  FWHexView.Common,

  CpuView.Common,
  CpuView.Settings,
  CpuView.ExtendedHint;

type

  { TSettingsVST }

  TSettingsVST = class(TLazVirtualStringTree)
  protected
    procedure AdjustImageBorder(AImages: TCustomImageList; ABidiMode: TBidiMode;
      VAlign: Integer; var R: TRect; var ImageInfo: TVTImageInfo); override;
    procedure AutoScale; override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const AText: string;
      CellRect: TRect; DrawFormat: Cardinal); override;
    procedure PaintCheckImage(ACanvas: TCanvas; const ImageInfo: TVTImageInfo;
      {%H-}ASelected: Boolean); override;
  end;

  TTVCheckStyle = (tvcsNone, tvcsChecked, tvcsUnchecked);

  { TCpuViewMainOptionsFrame }

  TCpuViewMainOptionsFrame = class(TCpuViewBaseOptionsFrame)
    btnFontBrowse: TButton;
    btnReset: TButton;
    cbFont: TComboBox;
    cbSymbols: TCheckBox;
    cbForm: TCheckBox;
    cbViewers: TCheckBox;
    cbDbgLog: TCheckBox;
    cbDbgCrash: TCheckBox;
    cbAddrValidation: TCheckBox;
    cbForceFindSymbols: TCheckBox;
    cbExtendedHints: TCheckBox;
    cbDisplayStrings: TCheckBox;
    FontDialog: TFontDialog;
    gbPerformance: TGroupBox;
    gbSessions: TGroupBox;
    gbViewersSetting: TGroupBox;
    ilSettings: TImageList;
    lblFont: TLabel;
    seMinStrLen: TSpinEdit;
    seFindSymbolsDepth: TSpinEdit;
    procedure btnFontBrowseClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbSymbolsClick(Sender: TObject);
  private
    tvSettings: TSettingsVST;
    function Add(Root: PVirtualNode; Index: Integer;
      ACheckStyle: TTVCheckStyle): PVirtualNode;
    procedure FillImageList;
    procedure FillSettingsView;
    procedure SaveExpandedState;
    procedure tvSettingsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvSettingsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure UpdateCurrentFont(const AFontName: string);
    procedure UpdateDebugSymbolsIndepended;
    procedure UpdateFrameControl;
  protected
    procedure DoReadSettings; override;
    procedure DoWriteSettings; override;
    function IsMainFrame: Boolean; override;
    procedure CMFontChanged(var Message: TLMessage); message CM_FONTCHANGED;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
  end;

implementation

var
  TreeExpandState: array [0..4] of Boolean;

{$R *.lfm}

{ TSettingsVST }

procedure TSettingsVST.AdjustImageBorder(AImages: TCustomImageList;
  ABidiMode: TBidiMode; VAlign: Integer; var R: TRect;
  var ImageInfo: TVTImageInfo);
var
  {$IFDEF MSWINDOWS}
  Details: TThemedElementDetails;
  {$ENDIF}
  CheckSize: Integer;
begin
  if ImageInfo.Images = Images then
    inherited
  else
  begin
    {$IFDEF MSWINDOWS}
    if ThemeServices.ThemesAvailable then
    begin
      Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
      CheckSize := ThemeServices.GetDetailSizeForPPI(Details, Font.PixelsPerInch).CY;
    end
    else
    {$ENDIF}
      CheckSize := AImages.Height;
    inherited AdjustImageBorder(AImages.Width, CheckSize, ABidiMode, VAlign, R, ImageInfo);
  end;
end;

procedure TSettingsVST.AutoScale;
var
  Enum: TVTVirtualNodeEnumerator;
begin
  Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  inherited AutoScale;
  DefaultNodeHeight := Scale96ToFont(DEFAULT_NODE_HEIGHT);
  Enum := Nodes.GetEnumerator;
  while Enum.MoveNext do
    NodeHeight[Enum.Current] := DefaultNodeHeight;
end;

procedure TSettingsVST.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const AText: string; CellRect: TRect; DrawFormat: Cardinal);
begin
  PaintInfo.Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  inherited DoTextDrawing(PaintInfo, AText, CellRect, DrawFormat);
end;

procedure TSettingsVST.PaintCheckImage(ACanvas: TCanvas;
  const ImageInfo: TVTImageInfo; ASelected: Boolean);
{$IFDEF MSWINDOWS}
var
  Details: TThemedElementDetails;
  CheckSize: Integer;
  R: TRect;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if ThemeServices.ThemesAvailable then
  begin
    Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    CheckSize := ThemeServices.GetDetailSizeForPPI(Details, Font.PixelsPerInch).CX;
    R := Bounds(ImageInfo.XPos, ImageInfo.YPos, CheckSize, CheckSize);
    DrawThemeBackground(
      TWin32ThemeServices(ThemeServices).ThemeForPPI[teButton, Font.PixelsPerInch],
      ACanvas.Handle, BP_CHECKBOX, ImageInfo.Index - 8, R, nil);
  end
  else
  {$ENDIF}
    inherited;
end;

{ TCpuViewMainOptionsFrame }

procedure TCpuViewMainOptionsFrame.btnFontBrowseClick(Sender: TObject);
begin
  FontDialog.Font.Name := cbFont.Text;
  if FontDialog.Execute then
    UpdateCurrentFont(FontDialog.Font.Name);
end;

procedure TCpuViewMainOptionsFrame.btnResetClick(Sender: TObject);
begin
  SaveExpandedState;
  Settings.Reset(spSession);
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.cbSymbolsClick(Sender: TObject);
begin
  UpdateDebugSymbolsIndepended;
end;

function TCpuViewMainOptionsFrame.Add(Root: PVirtualNode; Index: Integer;
  ACheckStyle: TTVCheckStyle): PVirtualNode;
begin
  Result := tvSettings.AddChild(Root, nil);
  PInteger(tvSettings.GetNodeData(Result))^ := Index;
  if ACheckStyle = tvcsNone then Exit;
  tvSettings.CheckType[Result] := ctCheckBox;
  if ACheckStyle = tvcsChecked then
    tvSettings.CheckState[Result] := csCheckedNormal
  else
    tvSettings.CheckState[Result] := csUncheckedNormal;
end;

procedure TCpuViewMainOptionsFrame.FillImageList;

  procedure AddColorRect(AColor: TColor);
  var
    Img: TBitmap;
    Offsets: Integer;
    R: TRect;
  begin
    Img := TBitmap.Create;
    try
      Img.SetSize(ilSettings.Width, ilSettings.Height);
      Img.Canvas.Brush.Color := clMaroon;
      R := Rect(0, 0, ilSettings.Width, ilSettings.Height);
      Img.Canvas.FillRect(R);
      Offsets := -Ceil(R.Height / 5);
      InflateRect(R, Offsets, Offsets);
      Img.Canvas.Brush.Style := bsSolid;
      Img.Canvas.Brush.Color := AColor;
      Img.Canvas.RoundRect(R, 2, 2);
      ilSettings.AddMasked(Img, clMaroon);
    finally
      Img.Free;
    end;
  end;

begin
  ilSettings.Clear;
  ilSettings.Width := tvSettings.DefaultNodeHeight;
  ilSettings.Height := tvSettings.DefaultNodeHeight;
  AddColorRect(Settings.Color[xmlAddrValidateE]);
  AddColorRect(Settings.Color[xmlAddrValidateR]);
  AddColorRect(clGray);
  AddColorRect(Settings.Color[xmlAddrValidateS]);
  AddColorRect(clWhite);
end;

procedure TCpuViewMainOptionsFrame.FillSettingsView;

  function BoolToTVCheckStyle(Value: Boolean): TTVCheckStyle;
  begin
    if Value then
      Result := tvcsChecked
    else
      Result := tvcsUnchecked;
  end;

var
  ANode: PVirtualNode;
  I: TPointerValue;
begin
  tvSettings.Clear;
  ANode := Add(nil, 100, tvcsNone);
  Add(ANode, 101, BoolToTVCheckStyle(Settings.ShowJumps));
  Add(ANode, 102, BoolToTVCheckStyle(Settings.ShowOpcodes));
  Add(ANode, 103, BoolToTVCheckStyle(Settings.ShowSourceLines));
  Add(ANode, 104, BoolToTVCheckStyle(Settings.ShowFullAddress));
  Add(ANode, 105, BoolToTVCheckStyle(Settings.ShowCallFuncName));
  Add(ANode, 106, BoolToTVCheckStyle(Settings.HintInAsm));
  tvSettings.Expanded[ANode] := TreeExpandState[0];

  ANode := Add(nil, 200, tvcsNone);
  Add(ANode, 201, BoolToTVCheckStyle(Settings.ValidationDump[avtExecutable]));
  Add(ANode, 202, BoolToTVCheckStyle(Settings.ValidationDump[avtReadable]));
  Add(ANode, 203, BoolToTVCheckStyle(Settings.ValidationDump[avtStack]));
  Add(ANode, 204, BoolToTVCheckStyle(Settings.ValidationDump[avtString]));
  Add(ANode, 205, BoolToTVCheckStyle(Settings.HintInDump));
  Add(ANode, 206, BoolToTVCheckStyle(Settings.DuplicatesDump));
  tvSettings.Expanded[ANode] := TreeExpandState[1];

  ANode := Add(nil, 300, tvcsNone);
  Add(ANode, 301, BoolToTVCheckStyle(Settings.ValidationReg[avtExecutable]));
  Add(ANode, 302, BoolToTVCheckStyle(Settings.ValidationReg[avtReadable]));
  Add(ANode, 303, BoolToTVCheckStyle(Settings.ValidationReg[avtStack]));
  Add(ANode, 304, BoolToTVCheckStyle(Settings.ValidationReg[avtString]));
  Add(ANode, 305, BoolToTVCheckStyle(Settings.HintInRegForReg));
  Add(ANode, 306, BoolToTVCheckStyle(Settings.HintInRegForFlag));
  tvSettings.Expanded[ANode] := TreeExpandState[2];

  ANode := Add(nil, 400, tvcsNone);
  Add(ANode, 401, BoolToTVCheckStyle(Settings.ValidationStack[avtExecutable]));
  Add(ANode, 402, BoolToTVCheckStyle(Settings.ValidationStack[avtReadable]));
  Add(ANode, 403, BoolToTVCheckStyle(Settings.ValidationStack[avtStack]));
  Add(ANode, 404, BoolToTVCheckStyle(Settings.ValidationStack[avtString]));
  Add(ANode, 405, BoolToTVCheckStyle(Settings.HintInStack));
  Add(ANode, 406, BoolToTVCheckStyle(Settings.DuplicatesStack));
  tvSettings.Expanded[ANode] := TreeExpandState[3];

  ANode := Add(nil, 500, tvcsNone);
  for I := Low(TPointerValue) to High(TPointerValue) do
    Add(ANode, 501 + Byte(I) - Byte(bvmHex64), BoolToTVCheckStyle(I in Settings.ExtendedHintPointerValues));
  tvSettings.Expanded[ANode] := TreeExpandState[4];
end;

procedure TCpuViewMainOptionsFrame.SaveExpandedState;
var
  Enum: TVTVirtualNodeEnumerator;
begin
  Enum := tvSettings.Nodes.GetEnumerator;
  while Enum.MoveNext do
  begin
    case PInteger(tvSettings.GetNodeData(Enum.Current))^ of
      100: TreeExpandState[0] := tvSettings.Expanded[Enum.Current];
      200: TreeExpandState[1] := tvSettings.Expanded[Enum.Current];
      300: TreeExpandState[2] := tvSettings.Expanded[Enum.Current];
      400: TreeExpandState[3] := tvSettings.Expanded[Enum.Current];
      500: TreeExpandState[4] := tvSettings.Expanded[Enum.Current];
    end;
  end;
end;

procedure TCpuViewMainOptionsFrame.tvSettingsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
begin
  case PInteger(tvSettings.GetNodeData(Node))^ of
    100: ImageIndex := 0;
    200: ImageIndex := 1;
    300: ImageIndex := 2;
    400: ImageIndex := 3;
    500: ImageIndex := 4;
  else
    ImageIndex := -1;
  end;
end;

procedure TCpuViewMainOptionsFrame.tvSettingsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
const
  stShowHint = 'Show Hint';
  stExecutebleAddr = 'Mark the Executable Address';
  stReadableAddr = 'Mark the Readable Address';
  stStackAddr = 'Mark the Stack Address';
  stStringAddr = 'Mark the String Address';
  stDuplicates = 'Highlighting of identical selected values';
begin
  case PInteger(tvSettings.GetNodeData(Node))^ of
    // Disassembly
    100: CellText := 'Disassembly View';
    101: CellText := 'Show Jump Lines';
    102: CellText := 'Show Instructions Opcodes';
    103: CellText := 'Show Source Lines';
    104: CellText := 'Show Full Address Instead Offset';
    105: CellText := 'Show Function Name Instead Call Address';
    106: CellText := stShowHint;
    // Dump
    200: CellText := 'Dump View';
    201: CellText := stExecutebleAddr;
    202: CellText := stReadableAddr;
    203: CellText := stStackAddr;
    204: CellText := stStringAddr;
    205: CellText := stShowHint;
    206: CellText := stDuplicates;
    // Register
    300: CellText := 'Register View';
    301: CellText := stExecutebleAddr;
    302: CellText := stReadableAddr;
    303: CellText := stStackAddr;
    304: CellText := stStringAddr;
    305: CellText := stShowHint + ' for Validated Address';
    306: CellText := stShowHint + ' for Flags';
    // Stack
    400: CellText := 'Stack View';
    401: CellText := stExecutebleAddr;
    402: CellText := stReadableAddr;
    403: CellText := stStackAddr;
    404: CellText := stStringAddr;
    405: CellText := stShowHint;
    406: CellText := stDuplicates;
    // Extended hint
    500: CellText := 'Extended hint';
    501: CellText := 'Hex data';
    502: CellText := 'Signed Byte (8-bit) data';
    503: CellText := 'Signed Short (16-bit) data';
    504: CellText := 'Signed Long (32-bit) data';
    505: CellText := 'Signed Long Long (64-bit) data';
    506: CellText := 'Unsigned Byte (8-bit) data';
    507: CellText := 'Unsigned Short (16-bit) data';
    508: CellText := 'Unsigned Long (32-bit) data';
    509: CellText := 'Unsigned Long Long (64-bit) data';
    510: CellText := 'Float (32-bit) data';
    511: CellText := 'Double (64-bit) data';
    512: CellText := 'Long Double (80-bit) data';
  end;
end;

procedure TCpuViewMainOptionsFrame.UpdateCurrentFont(const AFontName: string);
var
  Index: Integer;
begin
  Index := cbFont.Items.IndexOf(AFontName);
  if Index < 0 then
    Index := cbFont.Items.Add(AFontName);
  cbFont.ItemIndex := Index;
end;

procedure TCpuViewMainOptionsFrame.UpdateDebugSymbolsIndepended;
begin
  cbForceFindSymbols.Enabled := cbSymbols.Checked;
  seMinStrLen.Enabled := cbDisplayStrings.Checked;
  seFindSymbolsDepth.Enabled := cbForceFindSymbols.Checked;
end;

procedure TCpuViewMainOptionsFrame.UpdateFrameControl;
begin
  UpdateCurrentFont(Settings.FontName);
  cbSymbols.Checked := Settings.UseDebugInfo;
  cbAddrValidation.Checked := Settings.UseAddrValidation;
  cbForm.Checked := Settings.SaveFormSession;
  cbViewers.Checked := Settings.SaveViewersSession;
  cbDbgLog.Checked := Settings.UseDebugLog;
  cbDbgCrash.Checked := Settings.UseCrashDump;
  cbForceFindSymbols.Checked := Settings.ForceFindSymbols;
  cbExtendedHints.Checked := Settings.ExtendedHints;
  cbDisplayStrings.Checked := Settings.DisplayStrings;
  seMinStrLen.Value := Settings.MinimumStringLength;
  seFindSymbolsDepth.Value := Settings.ForceFindSymbolsDepth;
  FillImageList;
  FillSettingsView;
  UpdateDebugSymbolsIndepended;
end;

procedure TCpuViewMainOptionsFrame.DoReadSettings;
begin
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.DoWriteSettings;
var
  Enum: TVTVirtualNodeEnumerator;
  Checked: Boolean;
  Idx: Integer;
  APointerValues: TPointerValues;
begin
  Settings.FontName := cbFont.Text;
  Settings.UseAddrValidation := cbAddrValidation.Checked;
  Settings.UseDebugInfo := cbSymbols.Checked;
  Settings.SaveFormSession := cbForm.Checked;
  Settings.SaveViewersSession := cbViewers.Checked;
  Settings.UseDebugLog := cbDbgLog.Checked;
  Settings.UseCrashDump := cbDbgCrash.Checked;
  Settings.ForceFindSymbols := cbForceFindSymbols.Checked;
  Settings.ExtendedHints := cbExtendedHints.Checked;
  Settings.DisplayStrings := cbDisplayStrings.Checked;
  Settings.MinimumStringLength := seMinStrLen.Value;
  Settings.ForceFindSymbolsDepth := seFindSymbolsDepth.Value;
  APointerValues := [];
  Enum := tvSettings.Nodes.GetEnumerator;
  while Enum.MoveNext do
  begin
    Checked := tvSettings.CheckState[Enum.Current] = csCheckedNormal;
    Idx := PInteger(tvSettings.GetNodeData(Enum.Current))^;
    case Idx of
      100: TreeExpandState[0] := tvSettings.Expanded[Enum.Current];
      101: Settings.ShowJumps := Checked;
      102: Settings.ShowOpcodes := Checked;
      103: Settings.ShowSourceLines := Checked;
      104: Settings.ShowFullAddress := Checked;
      105: Settings.ShowCallFuncName := Checked;
      106: Settings.HintInAsm := Checked;
      200: TreeExpandState[1] := tvSettings.Expanded[Enum.Current];
      201: Settings.ValidationDump[avtExecutable] := Checked;
      202: Settings.ValidationDump[avtReadable] := Checked;
      203: Settings.ValidationDump[avtStack] := Checked;
      204: Settings.ValidationDump[avtString] := Checked;
      205: Settings.HintInDump := Checked;
      206: Settings.DuplicatesDump := Checked;
      300: TreeExpandState[2] := tvSettings.Expanded[Enum.Current];
      301: Settings.ValidationReg[avtExecutable] := Checked;
      302: Settings.ValidationReg[avtReadable] := Checked;
      303: Settings.ValidationReg[avtStack] := Checked;
      304: Settings.ValidationReg[avtString] := Checked;
      305: Settings.HintInRegForReg := Checked;
      306: Settings.HintInRegForFlag := Checked;
      400: TreeExpandState[3] := tvSettings.Expanded[Enum.Current];
      401: Settings.ValidationStack[avtExecutable] := Checked;
      402: Settings.ValidationStack[avtReadable] := Checked;
      403: Settings.ValidationStack[avtStack] := Checked;
      404: Settings.ValidationStack[avtString] := Checked;
      405: Settings.HintInStack := Checked;
      406: Settings.DuplicatesStack := Checked;
      500: TreeExpandState[4] := tvSettings.Expanded[Enum.Current];
      501..514:
      begin
        if Checked then
        begin
          Idx := Byte(bvmHex64) + Idx - 501;
          Include(APointerValues, PPointerValue(@Idx)^);
        end;
      end;
    end;
  end;
  Settings.ExtendedHintPointerValues := APointerValues;
end;

function TCpuViewMainOptionsFrame.IsMainFrame: Boolean;
begin
  Result := True;
end;

procedure TCpuViewMainOptionsFrame.CMFontChanged(var Message: TLMessage);
begin
  if Assigned(ilSettings) then
    FillImageList;
end;

function TCpuViewMainOptionsFrame.GetTitle: string;
begin
  Result := 'CPU-View';
end;

procedure TCpuViewMainOptionsFrame.Setup({%H-}ADialog: TAbstractOptionsEditorDialog);
begin
  tvSettings := TSettingsVST.Create(Self);
  tvSettings.Parent := gbViewersSetting;
  tvSettings.Align := alClient;
  tvSettings.Images := ilSettings;
  tvSettings.OnGetImageIndex := @tvSettingsGetImageIndex;
  tvSettings.OnGetText := @tvSettingsGetText;
  tvSettings.TreeOptions.MiscOptions :=
    [toCheckSupport, toFullRepaintOnResize, toToggleOnDblClick, toWheelPanning];
  tvSettings.TreeOptions.PaintOptions :=
    [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme];
  tvSettings.TreeOptions.SelectionOptions := [toFullRowSelect];
  tvSettings.NodeDataSize := SizeOf(Integer);
  cbFont.Clear;
  {$IFDEF MSWINDOWS}
  cbFont.Items.Add('Consolas');
  {$ENDIF}
  {$IFDEF LINUX}
  cbFont.Items.Add('DejaVu Sans Mono');
  cbFont.Items.Add('Monospace');
  {$ENDIF}
  cbFont.ItemIndex := 0;
end;

end.

