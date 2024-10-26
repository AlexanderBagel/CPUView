﻿unit CpuView.Settings;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I CpuViewCfg.inc}

interface

uses
  Classes,
  SysUtils,
  Variants,
  Graphics,
  TypInfo,
  Generics.Collections,
  {$IFDEF FPC}
  LCLIntf,
  DOM,
  XMLRead,
  XMLWrite,
  {$ELSE}
  Windows,
  XMLIntf, xmldom, XMLDoc,
  {$ENDIF}
  FWHexView,
  FWHexView.Common,
  FWHexView.MappedView,
  CpuView.Common,
  CpuView.Viewers,
  CpuView.CPUContext,
  CpuView.IntelContext,
  CpuView.XML;

{$IFDEF FPC}
type
  IXMLNode = TDOMNode;
  IXMLDocument = TXMLDocument;
{$ENDIF}

const
  SettingRoot = 'config';
  SettingsName = 'cpuview.xml';
  xmlVersion = 'cfgver';
  xmlVersionData = '1.0';
  xmlGenerator = 'generator';
  xmlGeneratorData = 'CPU-View';
  xmlEncodingUTF8 = 'UTF-8';
  xmlBasic = 'basic';
  xmlColor = 'colors';
  xmlAsmView = 'asmview';
  xmlDumpView = 'dumpview';
  xmlRegView = 'regview';
  xmlStackView = 'stackview';
  xmlColumns = 'columns';
  xmlItem = 'item';
  xmlMode = 'mode';
  xmlLeft = 'left';
  xmlTop = 'top';
  xmlHeight = 'height';
  xmlWidth = 'width';
  xmlFont = 'font';
  xmlFontSize = 'size';
  xmlMaximized = 'maxstate';
  xmlSplitter = 'splitters';
  xmlSaveFormPos = 'saveForm';
  xmlSaveSplitters = 'saveProportions';
  xmlSaveFontSize = 'saveFontSize';
  xmlShowFuncName = 'showFuncName';
  xmlShowOpcodes = 'showOpcodes';
  xmlShowSrc = 'showSrc';
  xmlBackgroundColor = 'back';
  xmlBookmarkBackgroundColor = 'bookmarkBack';
  xmlBookmarkBorderColor = 'bookmarkBorder';
  xmlBookmarkTextColor = 'bookmarkText';
  xmlCaretColor = 'caret';
  xmlCaretTextColor = 'caretText';
  xmlGroupColor = 'group';
  xmlInfoBackgroundColor = 'infoBack';
  xmlInfoBorderColor = 'infoBorder';
  xmlInfoTextColor = 'infoText';
  xmlHeaderBackgroundColor = 'headerBack';
  xmlHeaderBorderColor = 'headerBorder';
  xmlHeaderColumnSeparatorColor = 'headerColumn';
  xmlHeaderTextColor = 'headerText';
  xmlRowSeparatorColor = 'rowSeparator';
  xmlSelectColor = 'select';
  xmlSelectInactiveColor = 'selectInactive';
  xmlTextColor = 'text';
  xmlTextCommentColor = 'textComment';
  xmlWorkSpaceTextColor = 'workSpaceText';
  xmlActiveJumpColor = 'asmActiveJmp';
  xmlArrowDownColor = 'asmArrowDown';
  xmlArrowDownSelectedColor = 'asmArrowDownSelected';
  xmlArrowUpColor = 'asmArrowUp';
  xmlArrowUpSelectedColor = 'asmArrowUpSelected';
  xmlJmpMarkColor = 'asmJmpMark';
  xmlJmpMarkTextColor = 'asmJmpMarkText';
  xmlBpActiveColor = 'asmBpActive';
  xmlBpActiveFontColor = 'asmBpActiveFont';
  xmlBpColor = 'asmBp';
  xmlBpDisabledColor = 'asmBpDisabled';
  xmlBpDisabledFontColor = 'asmBpDisabledFont';
  xmlBpFontColor = 'asmBpFont';
  xmlRegHighlightBackColor = 'asmRegHighlight';
  xmlRegHighlightFontColor = 'asmRegHighlightFont';
  xmlRIPBackgroundColor = 'asmRipBk';
  xmlRIPBackgroundFontColor = 'asmRipFont';
  xmlSourceLineColor = 'asmSourceLine';
  xmlSizePfxColor = 'asmSizePfx';
  xmlSeparatorBackgroundColor = 'asmSeparatorBack';
  xmlSeparatorBorderColor = 'asmSeparatorBorder';
  xmlSeparatorTextColor = 'asmSeparatorText';
  xmlNumberColor = 'asmNumber';
  xmlInstructionColor = 'asmInstruction';
  xmlInsRegColor = 'asmReg';
  xmlPrefixColor = 'asmPrefix';
  xmlJmpColor = 'asmJmp';
  xmlKernelColor = 'asmKernel';
  xmlNopColor = 'asmNop';
  xmlAddrPCColor = 'stkAddrPC';
  xmlAddrPCFontColor = 'stkAddrPCFont';
  xmlEmptyStackColor = 'stkEmpty';
  xmlFrameColor = 'stkFrame';
  xmlFrameActiveColor = 'stkFrameActive';
  xmlStackPointColor = 'stkStackPoint';
  xmlStackPointFontColor = 'stkStackPointFont';
  xmlHintColor = 'regHint';
  xmlRegColor = 'regReg';
  xmlValueColor = 'regValue';
  xmlValueModifiedColor = 'regValueModified';
  xmlView = 'viewmode';
  xmlAttrMode = 'mode';
  xmlByteView = 'data';
  xmlEncoder = 'encoder';
  xmlEncoderName = 'name';
  xmlEncoderCP = 'cp';

  // not used
  xmlContext = 'ctx';
  xmlContextName = 'name';

type
  TAsmSettings = record
    ColumnWidth: array [TColumnType] of Double;
    DisplayFuncNameInsteadCallAddr: Boolean;
    ShowOpcodes: Boolean;
    ShowSourceLines: Boolean;
    FontHeight: Double;
  end;

  TDumpSettings = record
    ByteViewMode: TByteViewMode;
    CodePage: Integer;
    EncodeType: TCharEncoderType;
    EncodingName: string;
    FontHeight: Double;
  end;

  TSplitters = (spTopHorz, spBottomHorz, spCenterVert);

  TCpuViewDlgSettings = record
    BoundsRect: TRect;
    Maximized: Boolean;
    SplitterPos: array [TSplitters] of Double;
  end;

  TCpuViewSettins = class
  strict private
    FAsmSettings: TAsmSettings;
    FColorMode: TColorMode;
    FColors: TDictionary<string, TColor>;
    FCpuViewDlgSettings: TCpuViewDlgSettings;
    FDumpSettings: TDumpSettings;
    FFontName: string;
    FRegFontHeight: Double;
    FSaveFontHeight: Boolean;
    FSaveFormPos: Boolean;
    FSaveViewerProportions: Boolean;
    FStackFontHeight: Double;
    FRegSettings: TContextAbstractSettings;
    FUseDebugInfo: Boolean;
    FUseDebugLog: Boolean;
    FUseCrashDump: Boolean;
    function GetColor(const Index: string): TColor;
    procedure SetColor(const Index: string; Value: TColor);
  private
    function DpiToDouble(AValue: Integer; AView: TFWCustomHexView): Double;
    function DoubleToDpi(AValue: Double; AView: TFWCustomHexView): Integer;

    procedure LoadFromAsmColorMap(Value: TAsmColorMap);
    procedure LoadFromDefaultColorMap(Value: THexViewColorMap);
    procedure LoadFromRegColorMap(Value: TRegistersColorMap);
    procedure LoadFromStackColorMap(Value: TStackColorMap);

    procedure LoadFromXML_AsmSettings(Root: IXMLNode);
    procedure LoadFromXML_BasicSettings(Root: IXMLNode);
    procedure LoadFromXML_Colors(Root: IXMLNode);
    procedure LoadFromXML_DumpSettings(Root: IXMLNode);
    procedure LoadFromXML_Full(Root: IXMLNode);
    procedure LoadFromXML_RegSettings(Root: IXMLNode);
    procedure LoadFromXML_StackSettings(Root: IXMLNode);

    procedure RestoreViewDefSettings(AView: TFWCustomHexView);

    procedure SaveToAsmColorMap(Value: TAsmColorMap);
    procedure SaveToDefaultColorMap(Value: THexViewColorMap);
    procedure SaveToRegColorMap(Value: TRegistersColorMap);
    procedure SaveToStackColorMap(Value: TStackColorMap);

    procedure SaveToXML_AsmSettings(Root: IXMLNode);
    procedure SaveToXML_BasicSettings(Root: IXMLNode);
    procedure SaveToXML_Colors(Root: IXMLNode);
    procedure SaveToXML_DumpSettings(Root: IXMLNode);
    procedure SaveToXML_Full(Root: IXMLNode);
    procedure SaveToXML_RegSettings(Root: IXMLNode);
    procedure SaveToXML_StackSettings(Root: IXMLNode);
  protected
    procedure InitDefault;
    procedure InitDefaultColors;
    function GetRegisterContextName: string; virtual; abstract;
    procedure LoadRegisterContext(Root: IXMLNode); virtual; abstract;
    procedure SaveRegisterContext(Root: IXMLNode); virtual; abstract;
  public
    constructor Create(ARegSettings: TContextAbstractSettings);
    destructor Destroy; override;

    procedure ColorsExport(const FilePath: string);
    procedure ColorsImport(const FilePath: string);

    procedure GetSessionFromAsmView(AAsmView: TAsmView);
    procedure GetSessionFromDumpView(ADumpView: TDumpView);
    procedure GetSessionFromRegView(ARegView: TRegView);
    procedure GetSessionFromStackView(AStackView: TStackView);

    procedure Reset;
    procedure Load(const FilePath: string);
    procedure Save(const FilePath: string);

    procedure SetSettingsToAsmView(AAsmView: TAsmView);
    procedure SetSettingsToDumpView(ADumpView: TDumpView);
    procedure SetSettingsToRegView(ARegView: TRegView);
    procedure SetSettingsToStackView(AStackView: TStackView);

    property ColorMode: TColorMode read FColorMode write FColorMode;
    property Color[const Index: string]: TColor read GetColor write SetColor;
    property CpuViewDlgSettings: TCpuViewDlgSettings read FCpuViewDlgSettings write FCpuViewDlgSettings;
    property FontName: string read FFontName write FFontName;
    property SaveFormPos: Boolean read FSaveFormPos write FSaveFormPos;
    property SaveFontHeight: Boolean read FSaveFontHeight write FSaveFontHeight;
    property SaveViewerProportions: Boolean read FSaveViewerProportions write FSaveViewerProportions;
    property UseDebugInfo: Boolean read FUseDebugInfo write FUseDebugInfo;
    property UseDebugLog: Boolean read FUseDebugLog write FUseDebugLog;
    property UseCrashDump: Boolean read FUseCrashDump write FUseCrashDump;
  end;

  {$message 'Контекст должен сам себя сериализовать в XML, этот класс выпилить!'}
  TIntelCpuViewSettins = class(TCpuViewSettins)
  private const
    xmlName = 'intel';
    xmlFlags = 'flags';
    xmlFPUMode = 'fpuMode';
    xmlMapMode = 'mapMode';
    xmlShowDebug = 'showDebug';
    xmlShowFPU = 'showFPU';
    xmlShowXMM = 'showXMM';
    xmlShowYMM = 'showYMM';
    xmlRegList = 'regs';
    xmlRegID = 'id';
  protected
    function GetRegisterContextName: string; override;
    procedure LoadRegisterContext(Root: IXMLNode); override;
    procedure SaveRegisterContext(Root: IXMLNode); override;
  end;

implementation

type
  TViewAccess = class(TFWCustomHexView);

{ TCpuViewSettins }

procedure TCpuViewSettins.ColorsExport(const FilePath: string);
var
  XMLDocument: IXMLDocument;
  Node: IXMLNode;
begin
  {$IFDEF FPC}
  XMLDocument := TXMLDocument.Create;
  try
    Node := XMLDocument.CreateElement(xmlColor);
    XMLDocument.AppendChild(Node);
    SaveToXML_Full(Node);
    WriteXML(XMLDocument, FilePath);
  finally
    XMLDocument.Free;
  end;
  {$ELSE}
  XMLDocument := NewXMLDocument;
  try
    XMLDocument.Active := True;
    XMLDocument.Encoding := xmlEncodingUTF8;
    XMLDocument.Options := XMLDocument.Options + [doNodeAutoIndent];
    Node := XMLDocument.AddChild(xmlColor);
    SaveToXML_Colors(Node);
    XMLDocument.SaveToFile(FilePath);
  finally
    XMLDocument.Active := False;
  end;
  {$ENDIF}
end;

procedure TCpuViewSettins.ColorsImport(const FilePath: string);
var
  XMLDocument: IXMLDocument;
begin
  if not FileExists(FilePath) then Exit;
  try
    {$IFDEF FPC}
    ReadXMLFile(XMLDocument, FilePath);
    {$ELSE}
    XMLDocument := LoadXMLDocument(FilePath);
    {$ENDIF}
    LoadFromXML_Colors(XMLDocument.DocumentElement);
  except
    InitDefaultColors;
  end;
end;

constructor TCpuViewSettins.Create(ARegSettings: TContextAbstractSettings);
begin
  FRegSettings := ARegSettings;
  FColors := TDictionary<string, TColor>.Create;
  Load(ExtractFilePath(ParamStr(0)) + SettingsName);
end;

destructor TCpuViewSettins.Destroy;
begin
  Save(ExtractFilePath(ParamStr(0)) + SettingsName);
  FColors.Free;
  inherited;
end;

function TCpuViewSettins.DoubleToDpi(AValue: Double;
  AView: TFWCustomHexView): Integer;
begin
  Result := Round(AValue * AView.CurrentPPI / 96);
end;

function TCpuViewSettins.DpiToDouble(AValue: Integer;
  AView: TFWCustomHexView): Double;
begin
  Result := AValue * 96 / AView.CurrentPPI;
end;

function TCpuViewSettins.GetColor(const Index: string): TColor;
begin
  if not FColors.TryGetValue(Index, Result) then
    Result := clDefault;
end;

procedure TCpuViewSettins.GetSessionFromAsmView(AAsmView: TAsmView);
var
  I: TColumnType;
begin
  if AAsmView = nil then Exit;
  // от вьювера загружаются только сессионные настройки
  // остальные хранятся сами по себе

  // only session settings are loaded from the viewer
  // the rest are kept on their own
  FillChar(FAsmSettings.ColumnWidth, SizeOf(FAsmSettings.ColumnWidth), 0);
  for I := Low(TColumnType) to High(TColumnType) do
    if I in AAsmView.Header.Columns then
      FAsmSettings.ColumnWidth[I] := DpiToDouble(AAsmView.Header.ColumnWidth[I],AAsmView);
  if SaveFontHeight then
    FAsmSettings.FontHeight := DpiToDouble(AAsmView.Font.Height, AAsmView);
end;

procedure TCpuViewSettins.GetSessionFromDumpView(ADumpView: TDumpView);
begin
  if ADumpView = nil then Exit;  
  FDumpSettings.ByteViewMode := ADumpView.ByteViewMode;
  FDumpSettings.CodePage := ADumpView.Encoder.CodePage;
  FDumpSettings.EncodeType := ADumpView.Encoder.EncodeType;
  FDumpSettings.EncodingName := ADumpView.Encoder.EncodingName;
  if SaveFontHeight then
    FDumpSettings.FontHeight := DpiToDouble(ADumpView.Font.Height, ADumpView);
end;

procedure TCpuViewSettins.GetSessionFromRegView(ARegView: TRegView);
begin
  if ARegView = nil then Exit;
  if SaveFontHeight then
    FRegFontHeight := DpiToDouble(ARegView.Font.Height, ARegView);
  FRegSettings.LoadFromContext(ARegView.Context);
end;

procedure TCpuViewSettins.GetSessionFromStackView(AStackView: TStackView);
begin
  if AStackView = nil then Exit;
  if SaveFontHeight then
    FStackFontHeight := DpiToDouble(AStackView.Font.Height, AStackView);
end;

procedure TCpuViewSettins.InitDefault;
const
  DefaultFontHeight = -12;
begin
  FAsmSettings := Default(TAsmSettings);
  FAsmSettings.ColumnWidth[ctWorkSpace] := 32;
  FAsmSettings.ColumnWidth[ctJmpLine] := 82;
  FAsmSettings.ColumnWidth[ctOpcode] := 170;
  FAsmSettings.ColumnWidth[ctDescription] := 250;
  FAsmSettings.ColumnWidth[ctComment] := 440;
  FAsmSettings.DisplayFuncNameInsteadCallAddr := True;
  FAsmSettings.ShowOpcodes := True;
  FAsmSettings.ShowSourceLines := True;
  FAsmSettings.FontHeight := DefaultFontHeight;

  InitDefaultColors;

  FCpuViewDlgSettings := Default(TCpuViewDlgSettings);
  FCpuViewDlgSettings.SplitterPos[spTopHorz] := 50;
  FCpuViewDlgSettings.SplitterPos[spBottomHorz] := 50;
  FCpuViewDlgSettings.SplitterPos[spCenterVert] := 75;

  FDumpSettings := Default(TDumpSettings);
  FDumpSettings.FontHeight := DefaultFontHeight;

  {$IFDEF MSWINDOWS}
  FFontName := 'Consolas';
  {$ENDIF}
  {$IFDEF UNIX}
  FFontName := 'DejaVu Sans Mono';
  {$ENDIF}

  FRegFontHeight := DefaultFontHeight;
  FRegSettings.InitDefault;

  FSaveFontHeight := True;
  FSaveFormPos := True;
  FSaveViewerProportions := True;

  FStackFontHeight := DefaultFontHeight;

  FUseDebugInfo := True;
  FUseDebugLog := True;
  FUseCrashDump := True;
end;

procedure TCpuViewSettins.InitDefaultColors;
var
  AsmColorMap: TAsmColorMap;
  RegColorMap: TRegistersColorMap;
  StackColorMap: TStackColorMap;
begin
  FColorMode := cmAuto;
  FColors.Clear;
  AsmColorMap := TAsmColorMap.Create(nil);
  try
    LoadFromDefaultColorMap(AsmColorMap);
    LoadFromAsmColorMap(AsmColorMap);
  finally
    AsmColorMap.Free;
  end;

  RegColorMap := TRegistersColorMap.Create(nil);
  try
    LoadFromRegColorMap(RegColorMap);
  finally
    RegColorMap.Free;
  end;

  StackColorMap := TStackColorMap.Create(nil);
  try
    LoadFromStackColorMap(StackColorMap);
  finally
    StackColorMap.Free;
  end;
end;

procedure TCpuViewSettins.Load(const FilePath: string);
var
  XMLDocument: IXMLDocument;
begin
  if not FileExists(FilePath) then Exit;
  try
    {$IFDEF FPC}
    ReadXMLFile(XMLDocument, FilePath);
    {$ELSE}
    XMLDocument := LoadXMLDocument(FilePath);
    {$ENDIF}
    LoadFromXML_Full(XMLDocument.DocumentElement);
  except
    InitDefault;
  end;
end;

procedure TCpuViewSettins.LoadFromAsmColorMap(Value: TAsmColorMap);
begin
  FColors.Add(xmlActiveJumpColor, Value.ActiveJmpColor);
  FColors.Add(xmlArrowDownColor, Value.ArrowDownColor);
  FColors.Add(xmlArrowDownSelectedColor, Value.ArrowDownSelectedColor);
  FColors.Add(xmlArrowUpColor, Value.ArrowUpColor);
  FColors.Add(xmlArrowUpSelectedColor, Value.ArrowUpSelectedColor);
  FColors.Add(xmlBpActiveColor, Value.BreakPointActiveColor);
  FColors.Add(xmlBpActiveFontColor, Value.BreakPointActiveFontColor);
  FColors.Add(xmlBpColor, Value.BreakPointColor);
  FColors.Add(xmlBpDisabledColor, Value.BreakPointDisabledColor);
  FColors.Add(xmlBpDisabledFontColor, Value.BreakPointDisabledFontColor);
  FColors.Add(xmlBpFontColor, Value.BreakPointFontColor);
  FColors.Add(xmlJmpMarkColor, Value.JmpMarkColor);
  FColors.Add(xmlJmpMarkTextColor, Value.JmpMarkTextColor);
  FColors.Add(xmlSeparatorBackgroundColor, Value.SeparatorBackgroundColor);
  FColors.Add(xmlSeparatorBorderColor, Value.SeparatorBorderColor);
  FColors.Add(xmlSeparatorTextColor, Value.SeparatorTextColor);
  FColors.Add(xmlNumberColor, Value.NumberColor);
  FColors.Add(xmlInstructionColor, Value.InstructionColor);
  FColors.Add(xmlInsRegColor, Value.RegColor);
  FColors.Add(xmlPrefixColor, Value.PrefixColor);
  FColors.Add(xmlJmpColor, Value.JmpColor);
  FColors.Add(xmlKernelColor, Value.KernelColor);
  FColors.Add(xmlNopColor, Value.NopColor);
  FColors.Add(xmlRegHighlightBackColor, Value.RegHighlightBackColor);
  FColors.Add(xmlRegHighlightFontColor, Value.RegHighlightFontColor);
  FColors.Add(xmlRIPBackgroundColor, Value.RIPBackgroundColor);
  FColors.Add(xmlRIPBackgroundFontColor, Value.RIPBackgroundFontColor);
  FColors.Add(xmlSizePfxColor, Value.SizePfxColor);
  FColors.Add(xmlSourceLineColor, Value.SourceLineColor);
end;

procedure TCpuViewSettins.LoadFromDefaultColorMap(Value: THexViewColorMap);
begin
  FColors.Add(xmlBackgroundColor, Value.BackgroundColor);
  FColors.Add(xmlBookmarkBackgroundColor, Value.BookmarkBackgroundColor);
  FColors.Add(xmlBookmarkBorderColor, Value.BookmarkBorderColor);
  FColors.Add(xmlBookmarkTextColor, Value.BookmarkTextColor);
  FColors.Add(xmlCaretColor, Value.CaretColor);
  FColors.Add(xmlCaretTextColor, Value.CaretTextColor);
  FColors.Add(xmlGroupColor, Value.GroupColor);
  FColors.Add(xmlInfoBackgroundColor, Value.InfoBackgroundColor);
  FColors.Add(xmlInfoBorderColor, Value.InfoBorderColor);
  FColors.Add(xmlInfoTextColor, Value.InfoTextColor);
  FColors.Add(xmlHeaderBackgroundColor, Value.HeaderBackgroundColor);
  FColors.Add(xmlHeaderBorderColor, Value.HeaderBorderColor);
  FColors.Add(xmlHeaderColumnSeparatorColor, Value.HeaderColumnSeparatorColor);
  FColors.Add(xmlHeaderTextColor, Value.HeaderTextColor);
  FColors.Add(xmlRowSeparatorColor, Value.RowSeparatorColor);
  FColors.Add(xmlSelectColor, Value.SelectColor);
  FColors.Add(xmlSelectInactiveColor, Value.SelectInactiveColor);
  FColors.Add(xmlTextColor, Value.TextColor);
  FColors.Add(xmlTextCommentColor, Value.TextCommentColor);
  FColors.Add(xmlWorkSpaceTextColor, Value.WorkSpaceTextColor);
end;

procedure TCpuViewSettins.LoadFromRegColorMap(Value: TRegistersColorMap);
begin
  FColors.Add(xmlHintColor, Value.HintColor);
  FColors.Add(xmlRegColor, Value.RegColor);
  FColors.Add(xmlValueColor, Value.ValueColor);
  FColors.Add(xmlValueModifiedColor, Value.ValueModifiedColor);
end;

procedure TCpuViewSettins.LoadFromStackColorMap(Value: TStackColorMap);
begin
  FColors.Add(xmlAddrPCColor, Value.AddrPCColor);
  FColors.Add(xmlAddrPCFontColor, Value.AddrPCFontColor);
  FColors.Add(xmlEmptyStackColor, Value.EmptyStackColor);
  FColors.Add(xmlFrameColor, Value.FrameColor);
  FColors.Add(xmlFrameActiveColor, Value.FrameActiveColor);
  FColors.Add(xmlStackPointColor, Value.StackPointColor);
  FColors.Add(xmlStackPointFontColor, Value.StackPointFontColor);
end;

procedure TCpuViewSettins.LoadFromXML_AsmSettings(Root: IXMLNode);
var
  I: Integer;
  ColNode, ItemNode: IXMLNode;
  Column: TColumnType;
begin
  if SaveFontHeight then
    FAsmSettings.FontHeight := XMLReadDouble(Root, xmlFontSize);

  FAsmSettings.DisplayFuncNameInsteadCallAddr := GetNodeAttr(Root, xmlShowFuncName);
  FAsmSettings.ShowOpcodes := GetNodeAttr(Root, xmlShowOpcodes);
  FAsmSettings.ShowSourceLines := GetNodeAttr(Root, xmlShowSrc);

  ColNode := FindNode(Root, xmlColumns);
  if ColNode = nil then Exit;
  for I := 0 to ColNode.ChildNodes.Count - 1 do
  begin
    ItemNode := GetChildNode(ColNode, I);
    Column := TColumnType(GetEnumValue(TypeInfo(TColumnType), GetNodeAttr(ItemNode, xmlItem)));
    FAsmSettings.ColumnWidth[Column] := XMLReadDouble(ItemNode, xmlWidth);
  end;
end;

procedure TCpuViewSettins.LoadFromXML_BasicSettings(Root: IXMLNode);
var
  I: Integer;
  SplittersNode, ItemNode: IXMLNode;
  Splitter: TSplitters;
begin
  if SaveFontHeight then
    FSaveFontHeight := GetNodeAttr(Root, xmlSaveFontSize);
  FSaveFormPos := GetNodeAttr(Root, xmlSaveFormPos);
  FSaveViewerProportions := GetNodeAttr(Root, xmlSaveSplitters);
  FFontName := GetNodeAttr(Root, xmlFont);
  if FSaveFormPos then
  begin
    FCpuViewDlgSettings.BoundsRect.Left := GetNodeAttr(Root, xmlLeft);
    FCpuViewDlgSettings.BoundsRect.Top := GetNodeAttr(Root, xmlTop);
    FCpuViewDlgSettings.BoundsRect.Width := GetNodeAttr(Root, xmlWidth);
    FCpuViewDlgSettings.BoundsRect.Height := GetNodeAttr(Root, xmlHeight);
    FCpuViewDlgSettings.Maximized := GetNodeAttr(Root, xmlMaximized);
  end;
  if FSaveViewerProportions then
  begin
    SplittersNode := FindNode(Root, xmlSplitter);
    if Assigned(SplittersNode) then
    begin
      for I := 0 to SplittersNode.ChildNodes.Count - 1 do
      begin
        ItemNode := GetChildNode(SplittersNode, I);
        Splitter := TSplitters(GetEnumValue(TypeInfo(TSplitters), GetNodeAttr(ItemNode, xmlItem)));
        FCpuViewDlgSettings.SplitterPos[Splitter] := XMLReadDouble(ItemNode, xmlWidth);
      end;
    end;
  end;
end;

procedure TCpuViewSettins.LoadFromXML_Colors(Root: IXMLNode);
var
  I: Integer;
  ColorValue: TColor;
  AttrList: TArray<string>;
begin
  AttrList := FColors.Keys.ToArray;
  for I := 0 to Length(AttrList) - 1 do
  begin
    ColorValue := GetNodeAttr(Root, AttrList[I]);
    FColors.AddOrSetValue(AttrList[I], ColorValue);
  end;
end;

procedure TCpuViewSettins.LoadFromXML_DumpSettings(Root: IXMLNode);
var
  Node: IXMLNode;
begin
  Node := FindNode(Root, xmlByteView);
  if Node = nil then Exit;
  FDumpSettings.ByteViewMode := TByteViewMode(
    GetEnumValue(TypeInfo(TByteViewMode), GetNodeAttr(Node, xmlAttrMode)));
  Node := FindNode(Root, xmlEncoder);
  if Node = nil then Exit;
  FDumpSettings.EncodeType := TCharEncoderType(
    GetEnumValue(TypeInfo(TCharEncoderType), GetNodeAttr(Node, xmlAttrMode)));
  FDumpSettings.EncodingName := GetNodeAttr(Node, xmlEncoderName);
  FDumpSettings.CodePage := GetNodeAttr(Node, xmlEncoderCP);
  if SaveFontHeight then
    FDumpSettings.FontHeight := XMLReadDouble(Root, xmlFontSize);
end;

procedure TCpuViewSettins.LoadFromXML_Full(Root: IXMLNode);
var
  Node: IXMLNode;
begin
  Node := FindNode(Root, xmlBasic);
  if Node = nil then Exit;
  LoadFromXML_BasicSettings(Node);
  Node := FindNode(Root, xmlColor);
  if Node = nil then Exit;
  LoadFromXML_Colors(Node);
  Node := FindNode(Root, xmlAsmView);
  if Node = nil then Exit;
  LoadFromXML_AsmSettings(Node);
  Node := FindNode(Root, xmlDumpView);
  if Node = nil then Exit;
  LoadFromXML_DumpSettings(Node);
  Node := FindNode(Root, xmlRegView);
  if Node = nil then Exit;
  LoadFromXML_RegSettings(Node);
  Node := FindNode(Root, xmlStackView);
  if Node = nil then Exit;
  LoadFromXML_StackSettings(Node);
end;

procedure TCpuViewSettins.LoadFromXML_RegSettings(Root: IXMLNode);
begin
  if SaveFontHeight then
    FRegFontHeight := XMLReadDouble(Root, xmlFontSize);
  FRegSettings.LoadFromXML(Root);
end;

procedure TCpuViewSettins.LoadFromXML_StackSettings(Root: IXMLNode);
begin
  if SaveFontHeight then
    FStackFontHeight := XMLReadDouble(Root, xmlFontSize);
end;

procedure TCpuViewSettins.Reset;
begin
  InitDefault;
end;

procedure TCpuViewSettins.RestoreViewDefSettings(AView: TFWCustomHexView);
begin
  AView.ResetViewState;
  TViewAccess(AView).Font.Name := FontName;
  SaveToDefaultColorMap(TViewAccess(AView).ColorMap);
end;

procedure TCpuViewSettins.Save(const FilePath: string);
var
  XMLDocument: IXMLDocument;
  Node: IXMLNode;
begin
  {$IFDEF FPC}
  XMLDocument := TXMLDocument.Create;
  try
    Node := XMLDocument.CreateElement(SettingRoot);
    XMLDocument.AppendChild(Node);
    TDOMElement(Node).SetAttribute(xmlVersion, xmlVersionData);
    TDOMElement(Node).SetAttribute(xmlGenerator, xmlGeneratorData);
    SaveToXML_Full(Node);
    WriteXML(XMLDocument, FilePath);
  finally
    XMLDocument.Free;
  end;
  {$ELSE}
  XMLDocument := NewXMLDocument;
  try
    XMLDocument.Active := True;
    XMLDocument.Encoding := xmlEncodingUTF8;
    XMLDocument.Options := XMLDocument.Options + [doNodeAutoIndent];
    Node := XMLDocument.AddChild(SettingRoot);
    Node.Attributes[xmlVersion] := xmlVersionData;
    Node.Attributes[xmlGenerator] := xmlGeneratorData;
    SaveToXML_Full(Node);
    XMLDocument.SaveToFile(FilePath);
  finally
    XMLDocument.Active := False;
  end;
  {$ENDIF}
end;

procedure TCpuViewSettins.SaveToAsmColorMap(Value: TAsmColorMap);
begin
  Value.ActiveJmpColor := Color[xmlActiveJumpColor];
  Value.ArrowDownColor := Color[xmlArrowDownColor];
  Value.ArrowDownSelectedColor := Color[xmlArrowDownSelectedColor];
  Value.ArrowUpColor := Color[xmlArrowUpColor];
  Value.ArrowUpSelectedColor := Color[xmlArrowUpSelectedColor];
  Value.BreakPointActiveColor := Color[xmlBpActiveColor];
  Value.BreakPointActiveFontColor := Color[xmlBpActiveFontColor];
  Value.BreakPointColor := Color[xmlBpColor];
  Value.BreakPointDisabledColor := Color[xmlBpDisabledColor];
  Value.BreakPointDisabledFontColor := Color[xmlBpDisabledFontColor];
  Value.BreakPointFontColor := Color[xmlBpFontColor];
  Value.JmpMarkColor := Color[xmlJmpMarkColor];
  Value.JmpMarkTextColor := Color[xmlJmpMarkTextColor];
  Value.SeparatorBackgroundColor := Color[xmlSeparatorBackgroundColor];
  Value.SeparatorBorderColor := Color[xmlSeparatorBorderColor];
  Value.SeparatorTextColor := Color[xmlSeparatorTextColor];
  Value.NumberColor := Color[xmlNumberColor];
  Value.InstructionColor := Color[xmlInstructionColor];
  Value.RegColor := Color[xmlInsRegColor];
  Value.PrefixColor := Color[xmlPrefixColor];
  Value.JmpColor := Color[xmlJmpColor];
  Value.KernelColor := Color[xmlKernelColor];
  Value.NopColor := Color[xmlNopColor];
  Value.RegHighlightBackColor := Color[xmlRegHighlightBackColor];
  Value.RegHighlightFontColor := Color[xmlRegHighlightFontColor];
  Value.RIPBackgroundColor := Color[xmlRIPBackgroundColor];
  Value.RIPBackgroundFontColor := Color[xmlRIPBackgroundFontColor];
  Value.SizePfxColor := Color[xmlSizePfxColor];
  Value.SourceLineColor := Color[xmlSourceLineColor];
end;

procedure TCpuViewSettins.SaveToDefaultColorMap(Value: THexViewColorMap);
begin
  Value.BackgroundColor := Color[xmlBackgroundColor];
  Value.BookmarkBackgroundColor := Color[xmlBookmarkBackgroundColor];
  Value.BookmarkBorderColor := Color[xmlBookmarkBorderColor];
  Value.BookmarkTextColor := Color[xmlBookmarkTextColor];
  Value.CaretColor := Color[xmlCaretColor];
  Value.CaretTextColor := Color[xmlCaretTextColor];
  Value.GroupColor := Color[xmlGroupColor];
  Value.InfoBackgroundColor := Color[xmlInfoBackgroundColor];
  Value.InfoBorderColor := Color[xmlInfoBorderColor];
  Value.InfoTextColor := Color[xmlInfoTextColor];
  Value.HeaderBackgroundColor := Color[xmlHeaderBackgroundColor];
  Value.HeaderBorderColor := Color[xmlHeaderBorderColor];
  Value.HeaderColumnSeparatorColor := Color[xmlHeaderColumnSeparatorColor];
  Value.HeaderTextColor := Color[xmlHeaderTextColor];
  Value.RowSeparatorColor := Color[xmlRowSeparatorColor];
  Value.SelectColor := Color[xmlSelectColor];
  Value.SelectInactiveColor := Color[xmlSelectInactiveColor];
  Value.TextColor := Color[xmlTextColor];
  Value.TextCommentColor := Color[xmlTextCommentColor];
  Value.WorkSpaceTextColor := Color[xmlWorkSpaceTextColor];
end;

procedure TCpuViewSettins.SaveToRegColorMap(Value: TRegistersColorMap);
begin
  Value.HintColor := Color[xmlHintColor];
  Value.RegColor := Color[xmlRegColor];
  Value.ValueColor := Color[xmlValueColor];
  Value.ValueModifiedColor := Color[xmlValueModifiedColor];
end;

procedure TCpuViewSettins.SaveToStackColorMap(Value: TStackColorMap);
begin
  Value.AddrPCColor := Color[xmlAddrPCColor];
  Value.AddrPCFontColor := Color[xmlAddrPCFontColor];
  Value.EmptyStackColor := Color[xmlEmptyStackColor];
  Value.FrameColor := Color[xmlFrameColor];
  Value.FrameActiveColor := Color[xmlFrameActiveColor];
  Value.StackPointColor := Color[xmlStackPointColor];
  Value.StackPointFontColor := Color[xmlStackPointFontColor];
end;

procedure TCpuViewSettins.SaveToXML_AsmSettings(Root: IXMLNode);
var
  I: TColumnType;
  ColNode, ItemNode: IXMLNode;
  Columns: TFWHexViewColumnTypes;
begin
  if SaveFontHeight then
    XMLWriteDouble(Root, xmlFontSize, FAsmSettings.FontHeight);

  SetNodeAttr(Root, xmlShowFuncName, FAsmSettings.DisplayFuncNameInsteadCallAddr);
  SetNodeAttr(Root, xmlShowOpcodes, FAsmSettings.ShowOpcodes);
  SetNodeAttr(Root, xmlShowSrc, FAsmSettings.ShowSourceLines);

  ColNode := NewChild(Root, xmlColumns);
  Columns := [ctWorkSpace..ctComment];
  if not FAsmSettings.ShowOpcodes then
    Exclude(Columns, ctOpcode);
  for I := Low(TColumnType) to High(TColumnType) do
    if I in Columns then
    begin
      ItemNode := NewChild(ColNode, xmlItem);
      SetNodeAttr(ItemNode, xmlItem, GetEnumName(TypeInfo(TColumnType), Integer(I)));
      XMLWriteDouble(ItemNode, xmlWidth, FAsmSettings.ColumnWidth[I]);
    end;
end;

procedure TCpuViewSettins.SaveToXML_BasicSettings(Root: IXMLNode);
var
  SplitNode, ItemNode: IXMLNode;
  I: TSplitters;
begin
  SetNodeAttr(Root, xmlSaveFontSize, FSaveFontHeight);
  SetNodeAttr(Root, xmlSaveFormPos, FSaveFormPos);
  SetNodeAttr(Root, xmlSaveSplitters, FSaveViewerProportions);
  SetNodeAttr(Root, xmlFont, FFontName);
  if FSaveFormPos then
  begin
    SetNodeAttr(Root, xmlLeft, FCpuViewDlgSettings.BoundsRect.Left);
    SetNodeAttr(Root, xmlTop, FCpuViewDlgSettings.BoundsRect.Top);
    SetNodeAttr(Root, xmlWidth, FCpuViewDlgSettings.BoundsRect.Width);
    SetNodeAttr(Root, xmlHeight, FCpuViewDlgSettings.BoundsRect.Height);
    SetNodeAttr(Root, xmlMaximized, FCpuViewDlgSettings.Maximized);
  end;
  if FSaveViewerProportions then
  begin
    SplitNode := NewChild(Root, xmlSplitter);
    for I := Low(TSplitters) to High(TSplitters) do
    begin
      ItemNode := NewChild(SplitNode, xmlItem);
      SetNodeAttr(ItemNode, xmlItem, GetEnumName(TypeInfo(TSplitters), Integer(I)));
      XMLWriteDouble(ItemNode, xmlWidth, FCpuViewDlgSettings.SplitterPos[I]);
    end;
  end;
end;

procedure TCpuViewSettins.SaveToXML_Colors(Root: IXMLNode);
var
  I: Integer;
  AttrList: TArray<string>;
begin
  SetNodeAttr(Root, xmlMode, GetEnumName(TypeInfo(TColorMode), Integer(FColorMode)));
  AttrList := FColors.Keys.ToArray;
  for I := 0 to Length(AttrList) - 1 do
    SetNodeAttr(Root, AttrList[I], Color[AttrList[I]]);
end;

procedure TCpuViewSettins.SaveToXML_DumpSettings(Root: IXMLNode);
var
  ByteView, Encoder: IXMLNode;
begin
  if SaveFontHeight then
    XMLWriteDouble(Root, xmlFontSize, FAsmSettings.FontHeight);
  ByteView := NewChild(Root, xmlByteView);
  SetNodeAttr(ByteView, xmlAttrMode,
    GetEnumName(TypeInfo(TByteViewMode), Integer(FDumpSettings.ByteViewMode)));
  Encoder := NewChild(Root, xmlEncoder);
  SetNodeAttr(Encoder, xmlAttrMode,
    GetEnumName(TypeInfo(TCharEncoderType), Integer(FDumpSettings.EncodeType)));
  SetNodeAttr(Encoder, xmlEncoderName, FDumpSettings.EncodingName);
  SetNodeAttr(Encoder, xmlEncoderCP, FDumpSettings.CodePage);
end;

procedure TCpuViewSettins.SaveToXML_Full(Root: IXMLNode);
begin
  {$IFDEF FPC}
  SaveToXML_BasicSettings(NewChild(Root, xmlBasic));
  SaveToXML_Colors(NewChild(Root, xmlColor));
  SaveToXML_AsmSettings(NewChild(Root, xmlAsmView));
  SaveToXML_DumpSettings(NewChild(Root, xmlDumpView));
  SaveToXML_RegSettings(NewChild(Root, xmlRegView));
  SaveToXML_StackSettings(NewChild(Root, xmlStackView));
  {$ELSE}
  SaveToXML_BasicSettings(Root.AddChild(xmlBasic));
  SaveToXML_Colors(Root.AddChild(xmlColor));
  SaveToXML_AsmSettings(Root.AddChild(xmlAsmView));
  SaveToXML_DumpSettings(Root.AddChild(xmlDumpView));
  SaveToXML_RegSettings(Root.AddChild(xmlRegView));
  SaveToXML_StackSettings(Root.AddChild(xmlStackView));
  {$ENDIF}
end;

procedure TCpuViewSettins.SaveToXML_RegSettings(Root: IXMLNode);
begin
  if SaveFontHeight then
    XMLWriteDouble(Root, xmlFontSize, FRegFontHeight);
  FRegSettings.SaveToXML(Root);
end;

procedure TCpuViewSettins.SaveToXML_StackSettings(Root: IXMLNode);
begin
  if SaveFontHeight then
    XMLWriteDouble(Root, xmlFontSize, FRegFontHeight);
end;

procedure TCpuViewSettins.SetColor(const Index: string; Value: TColor);
begin
  FColors.TryAdd(Index, Value);
end;

procedure TCpuViewSettins.SetSettingsToAsmView(AAsmView: TAsmView);
var
  I: TColumnType;
begin
  if AAsmView = nil then Exit;
  RestoreViewDefSettings(AAsmView);
  SaveToAsmColorMap(AAsmView.ColorMap);
  if SaveFontHeight then
    AAsmView.Font.Height := DoubleToDpi(FAsmSettings.FontHeight, AAsmView);
  for I := Low(TColumnType) to High(TColumnType) do
    if I in AAsmView.Header.Columns then
      FAsmSettings.ColumnWidth[I] := DoubleToDpi(AAsmView.Header.ColumnWidth[I], AAsmView)
end;

procedure TCpuViewSettins.SetSettingsToDumpView(ADumpView: TDumpView);
begin
  if ADumpView = nil then Exit;
  RestoreViewDefSettings(ADumpView);
  if SaveFontHeight then
    ADumpView.Font.Height := DoubleToDpi(FDumpSettings.FontHeight, ADumpView);
  ADumpView.ByteViewMode := FDumpSettings.ByteViewMode;
  ADumpView.Encoder.EncodeType := FDumpSettings.EncodeType;
  ADumpView.Encoder.CodePage := FDumpSettings.CodePage;
  ADumpView.Encoder.EncodingName := FDumpSettings.EncodingName;
end;

procedure TCpuViewSettins.SetSettingsToRegView(ARegView: TRegView);
begin
  if ARegView = nil then Exit;
  RestoreViewDefSettings(ARegView);
  if SaveFontHeight then
    ARegView.Font.Height := DoubleToDpi(FRegFontHeight, ARegView);
  SaveToRegColorMap(ARegView.ColorMap);
  FRegSettings.SaveToContext(ARegView.Context);
end;

procedure TCpuViewSettins.SetSettingsToStackView(AStackView: TStackView);
begin
  if AStackView = nil then Exit;
  RestoreViewDefSettings(AStackView);
  if SaveFontHeight then
    AStackView.Font.Height := DoubleToDpi(FStackFontHeight, AStackView);
  SaveToStackColorMap(AStackView.ColorMap);
end;

{ TIntelCpuViewSettins }

function TIntelCpuViewSettins.GetRegisterContextName: string;
begin
  Result := xmlName;
end;

procedure TIntelCpuViewSettins.LoadRegisterContext(Root: IXMLNode);
var
  Ctx: TIntelCpuContext;
  Flags, Regs, ItemNode: IXMLNode;
  I, RegID: Integer;
begin
//  Ctx := RegView.Context as TIntelCpuContext;
//  Flags := FindNode(Root, xmlFlags);
//  if Flags = nil then Exit;
//  Ctx.FPUMode := TFPUMode(
//    GetEnumValue(TypeInfo(TFPUMode), GetNodeAttr(Flags, xmlFPUMode)));
//  Ctx.MapMode := TIntelCpuMapMode(
//    GetEnumValue(TypeInfo(TIntelCpuMapMode), GetNodeAttr(Flags, xmlMapMode)));
//  Ctx.ShowDebug := GetNodeAttr(Flags, xmlShowDebug);
//  Ctx.ShowFPU := GetNodeAttr(Flags, xmlShowFPU);
//  Ctx.ShowXMM := GetNodeAttr(Flags, xmlShowXMM);
//  Ctx.ShowYMM := GetNodeAttr(Flags, xmlShowYMM);
//  Regs := FindNode(Root, xmlRegList);
//  if Regs = nil then Exit;
//  for I := 0 to Regs.ChildNodes.Count - 1 do
//  begin
//    ItemNode := GetChildNode(Regs, I);
//    RegID := GetNodeAttr(ItemNode, xmlRegID);
//    Ctx.ViewMode[RegID] := TRegViewMode(
//      GetEnumValue(TypeInfo(TRegViewMode), GetNodeAttr(ItemNode, xmlMode)));
//  end;
end;

procedure TIntelCpuViewSettins.SaveRegisterContext(Root: IXMLNode);
var
  Ctx: TIntelCpuContext;
  Flags, Regs, ItemNode: IXMLNode;
  I: Integer;
begin
//  Ctx := RegView.Context as TIntelCpuContext;
//  Flags := NewChild(Root, xmlFlags);
//  SetNodeAttr(Flags, xmlFPUMode,
//    GetEnumName(TypeInfo(TFPUMode), Integer(Ctx.FPUMode)));
//  SetNodeAttr(Flags, xmlMapMode,
//    GetEnumName(TypeInfo(TIntelCpuMapMode), Integer(Ctx.MapMode)));
//  SetNodeAttr(Flags, xmlShowDebug, Ctx.ShowDebug);
//  SetNodeAttr(Flags, xmlShowFPU, Ctx.ShowFPU);
//  SetNodeAttr(Flags, xmlShowXMM, Ctx.ShowXMM);
//  SetNodeAttr(Flags, xmlShowYMM, Ctx.ShowYMM);
//  Regs := nil;
//  for I in [0..7, 9..16, 33, 41, 49, 58, 74] do
//  begin
//    if Ctx.ViewMode[I] = rvmHex then Continue;
//    if Regs = nil then
//      Regs := NewChild(Root, xmlRegList);
//    ItemNode := NewChild(Regs, xmlItem);
//    SetNodeAttr(ItemNode, xmlRegID, I);
//    SetNodeAttr(ItemNode, xmlMode,
//      GetEnumName(TypeInfo(TRegViewMode), Integer(Ctx.ViewMode[I])));
//  end;
end;

end.
