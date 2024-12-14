////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewOptions.pas
//  * Purpose   : Main settings window frame for Lazarus.
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

unit frmCpuViewOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, Graphics,
  IDEOptEditorIntf, frmCpuViewBaseOptions, laz.VirtualTrees, ImgList;

type

  { TSettingsVST }

  TSettingsVST = class(TLazVirtualStringTree)
  protected
    procedure AdjustImageBorder(AImages: TCustomImageList; ABidiMode: TBidiMode;
      VAlign: Integer; var R: TRect; var ImageInfo: TVTImageInfo); override;
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
    cbStackChains: TCheckBox;
    cbDasmPreview: TCheckBox;
    FontDialog: TFontDialog;
    gbPerformance: TGroupBox;
    gbSessions: TGroupBox;
    gbViewersSetting: TGroupBox;
    ilSettings: TImageList;
    lblFont: TLabel;
    procedure btnFontBrowseClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbSymbolsClick(Sender: TObject);
  private
    tvSettings: TSettingsVST;
    function Add(Root: PVirtualNode; Index: Integer;
      ACheckStyle: TTVCheckStyle): PVirtualNode;
    procedure FillImageList;
    procedure FillSettingsView;
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
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
  end;

implementation

uses
  LCLIntf, Math, Themes,
  FWHexView,
  CpuView.Common,
  CpuView.Settings;

{$R *.lfm}

{ TSettingsVST }

procedure TSettingsVST.AdjustImageBorder(AImages: TCustomImageList;
  ABidiMode: TBidiMode; VAlign: Integer; var R: TRect;
  var ImageInfo: TVTImageInfo);
var
  Details: TThemedElementDetails;
  CheckSize: Integer;
begin
  if ImageInfo.Images = Images then
    inherited
  else
  begin
    Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal);
    CheckSize := ThemeServices.GetDetailSizeForPPI(Details, Font.PixelsPerInch).CY;
    inherited AdjustImageBorder(AImages.Width, checkSize, ABidiMode, VAlign, R, ImageInfo);
  end;
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
    AFontHeight, Offsets: Integer;
    R: TRect;
  begin
    Img := TBitmap.Create;
    try
      Img.SetSize(ilSettings.Width, ilSettings.Height);
      Img.Canvas.Brush.Color := clMaroon;
      Img.Canvas.FillRect(Rect(0, 0, ilSettings.Width, ilSettings.Height));
      Img.Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
      Img.Canvas.Font.Name := Settings.FontName;
      Img.Canvas.Font.Height := Font.Height;
      AFontHeight := Min(Img.Canvas.TextHeight('J'), ilSettings.Height);
      R := Rect(0, 0, AFontHeight, AFontHeight);
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
  AddColorRect(Settings.Color[xmlAddrValidateE]);
  AddColorRect(Settings.Color[xmlAddrValidateR]);
  AddColorRect(clGray);
  AddColorRect(Settings.Color[xmlAddrValidateS]);
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
begin
  tvSettings.Clear;
  ANode := Add(nil, 100, tvcsNone);
  Add(ANode, 101, BoolToTVCheckStyle(Settings.ShowJumps));
  Add(ANode, 102, BoolToTVCheckStyle(Settings.ShowOpcodes));
  Add(ANode, 103, BoolToTVCheckStyle(Settings.ShowSourceLines));
  Add(ANode, 104, BoolToTVCheckStyle(Settings.ShowFullAddress));
  Add(ANode, 105, BoolToTVCheckStyle(Settings.ShowCallFuncName));
  Add(ANode, 106, BoolToTVCheckStyle(Settings.HintInAsm));

  ANode := Add(nil, 200, tvcsNone);
  Add(ANode, 201, tvcsUnchecked);
  Add(ANode, 202, tvcsUnchecked);
  Add(ANode, 203, tvcsUnchecked);
  Add(ANode, 204, tvcsUnchecked);

  ANode := Add(nil, 300, tvcsNone);
  Add(ANode, 301, BoolToTVCheckStyle(Settings.ValidationReg[avtExecutable]));
  Add(ANode, 302, BoolToTVCheckStyle(Settings.ValidationReg[avtReadable]));
  Add(ANode, 303, BoolToTVCheckStyle(Settings.ValidationReg[avtStack]));
  Add(ANode, 304, BoolToTVCheckStyle(Settings.HintInRegForReg));
  Add(ANode, 305, BoolToTVCheckStyle(Settings.HintInRegForFlag));

  ANode := Add(nil, 400, tvcsNone);
  Add(ANode, 401, tvcsUnchecked);
  Add(ANode, 402, tvcsUnchecked);
  Add(ANode, 403, tvcsUnchecked);
  Add(ANode, 404, tvcsUnchecked);
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
    204: CellText := stShowHint;
    // Register
    300: CellText := 'Register View';
    301: CellText := stExecutebleAddr;
    302: CellText := stReadableAddr;
    303: CellText := stStackAddr;
    304: CellText := stShowHint + ' for Validated Address';
    305: CellText := stShowHint + ' for Flags';
    // Stack
    400: CellText := 'Stack View';
    401: CellText := stExecutebleAddr;
    402: CellText := stReadableAddr;
    403: CellText := stStackAddr;
    404: CellText := stShowHint;
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
  cbStackChains.Enabled := cbSymbols.Checked;
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
  cbForceFindSymbols.Checked := Settings.InDeepDbgInfo;
  cbStackChains.Checked := Settings.StackChains;
  cbDasmPreview.Checked := Settings.DisassemblyInHint;
  FillImageList;
  FillSettingsView;
  UpdateDebugSymbolsIndepended;
end;

procedure TCpuViewMainOptionsFrame.DoReadSettings;
begin
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.DoWriteSettings;
begin
  Settings.FontName := cbFont.Text;
  Settings.UseAddrValidation := cbAddrValidation.Checked;
  Settings.UseDebugInfo := cbSymbols.Checked;
  Settings.SaveFormSession := cbForm.Checked;
  Settings.SaveViewersSession := cbViewers.Checked;
  Settings.UseDebugLog := cbDbgLog.Checked;
  Settings.UseCrashDump := cbDbgCrash.Checked;
  Settings.InDeepDbgInfo := cbForceFindSymbols.Checked;
  Settings.StackChains := cbStackChains.Checked;
  Settings.DisassemblyInHint := cbDasmPreview.Checked;
end;

function TCpuViewMainOptionsFrame.IsMainFrame: Boolean;
begin
  Result := True;
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

