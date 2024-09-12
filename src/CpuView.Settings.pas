unit CpuView.Settings;

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
  CpuView.IntelContext;

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
  xmlColor = 'colors';
  xmlAsmView = 'asmview';
  xmlDumpView = 'dumpview';
  xmlRegView = 'regview';
  xmlStackView = 'stackview';
  xmlColumns = 'columns';
  xmlItem = 'item';
  xmlMode = 'mode';
  xmlWidth = 'width';
  xmlFont = 'font';
  xmlFontSize = 'size';
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
  xmlRIPMarkColor = 'asmRip';
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
  xmlEmptyStackColor = 'stkEmpty';
  xmlFrameColor = 'stkFrame';
  xmlFrameActiveColor = 'stkFrameActive';
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
  xmlContext = 'ctx';
  xmlContextName = 'name';

type
  TCpuViewSettins = class
  strict private
    FAsmView: TAsmView;
    FDumpView: TDumpView;
    FRegView: TRegView;
    FStackView: TStackView;
    procedure LoadAsmSettings(Root: IXMLNode);
    procedure LoadColorMap(Root: IXMLNode);
    procedure LoadDumpSettings(Root: IXMLNode);
    procedure LoadRegSettings(Root: IXMLNode);
    procedure LoadStackSettings(Root: IXMLNode);
    procedure LoadFontSetting(AView: TFWCustomHexView; Root: IXMLNode);
    procedure SaveAsmSettings(Root: IXMLNode);
    procedure SaveColorMap(Root: IXMLNode);
    procedure SaveDumpSettings(Root: IXMLNode);
    procedure SaveRegSettings(Root: IXMLNode);
    procedure SaveStackSettings(Root: IXMLNode);
    procedure SaveFontSetting(AView: TFWCustomHexView; Root: IXMLNode);
  protected
    function GetRegisterContextName: string; virtual; abstract;
    procedure LoadRegisterContext(Root: IXMLNode); virtual; abstract;
    procedure SaveRegisterContext(Root: IXMLNode); virtual; abstract;
    property AsmView: TAsmView read FAsmView;
    property DumpView: TDumpView read FDumpView;
    property RegView: TRegView read FRegView;
    property StackView: TStackView read FStackView;
  public
    constructor Create(AsmView: TAsmView;
      DumpView: TDumpView; RegView: TRegView;
      StackView: TStackView);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
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

function FindNode(ANode: IXMLNode; const ANodeName: string): IXMLNode;
{$IFDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
  Result := nil;
  if ANode <> nil then
    {$IFDEF FPC}
    begin
      for I := 0 to ANode.ChildNodes.Count - 1 do
        if ANode.ChildNodes[I].NodeName = ANodeName then
        begin
          Result := ANode.ChildNodes[I];
          Break;
        end;
    end;
    {$ELSE}
    Result := ANode.ChildNodes.FindNode(ANodeName);
    {$ENDIF}
end;

function GetNodeAttr(Node: IXMLNode; const Attr: string): OleVariant;
{$IFDEF FPC}
var
  NamedAttr: TDOMNode;
{$ENDIF}
begin
  {$IFDEF FPC}
  NamedAttr := Node.Attributes.GetNamedItem(Attr);
  if NamedAttr = nil then
    Result := null
  else
    Result := NamedAttr.NodeValue;
  {$ELSE}
  Result := Node.Attributes[Attr];
  {$ENDIF}
end;

procedure SetNodeAttr(Node: IXMLNode; const Attr: string; Value: OleVariant);
begin
  {$IFDEF FPC}
  TDOMElement(Node).SetAttribute(Attr, Value);
  {$ELSE}
  Node.Attributes[Attr] := Value;
  {$ENDIF}
end;

function GetChildNode(Node: IXMLNode; Index: Integer): IXMLNode;
begin
  {$IFDEF FPC}
  Result := Node.ChildNodes[Index];
  {$ELSE}
  Result := Node.ChildNodes.Nodes[Index];
  {$ENDIF}
end;

procedure XMLWriteDouble(Node: IXMLNode; const Attr: string; Value: Double);
var
  FS: TFormatSettings;
begin
  FS := FormatSettings;
  FS.DecimalSeparator := '.';
  SetNodeAttr(Node, Attr, FloatToStr(Value, FS));
end;

function XMLReadDouble(Node: IXMLNode; const Attr: string): Double;
var
  Atrib: OleVariant;
  FS: TFormatSettings;
begin
  Result := 0;
  if Node <> nil then
  begin
    Atrib := GetNodeAttr(Node, Attr);
    FS := FormatSettings;
    FS.DecimalSeparator := '.';
    if not VarIsNull(Atrib) then
      TryStrToFloat(Atrib, Result, FS);
  end;
end;

function NewChild(Node: IXMLNode; const ChildName: string): IXMLNode;
begin
  {$IFDEF FPC}
  Result := Node.OwnerDocument.CreateElement(ChildName);
  Node.AppendChild(Result);
  {$ELSE}
  Result := Node.AddChild(ChildName);
  {$ENDIF}
end;

{ TCpuViewSettins }

constructor TCpuViewSettins.Create(AsmView: TAsmView; DumpView: TDumpView;
  RegView: TRegView; StackView: TStackView);
begin
  FAsmView := AsmView;
  FDumpView := DumpView;
  FRegView := RegView;
  FStackView := StackView;
  Load;
end;

destructor TCpuViewSettins.Destroy;
begin
  Save;
  inherited;
end;

procedure TCpuViewSettins.Load;
var
  Path: string;
  XMLDocument: IXMLDocument;
  Node: IXMLNode;
begin
  Path := ExtractFilePath(ParamStr(0)) + SettingsName;
  if not FileExists(Path) then Exit;
  {$IFDEF FPC}
  ReadXMLFile(XMLDocument, Path);
  {$ELSE}
  XMLDocument := LoadXMLDocument(Path);
  {$ENDIF}
  Node := FindNode(XMLDocument.DocumentElement, xmlColor);
  if Node = nil then Exit;
  LoadColorMap(Node);
  Node := FindNode(XMLDocument.DocumentElement, xmlAsmView);
  if Node = nil then Exit;
  LoadAsmSettings(Node);
  Node := FindNode(XMLDocument.DocumentElement, xmlDumpView);
  if Node = nil then Exit;
  LoadDumpSettings(Node);
  Node := FindNode(XMLDocument.DocumentElement, xmlRegView);
  if Node = nil then Exit;
  LoadRegSettings(Node);
  Node := FindNode(XMLDocument.DocumentElement, xmlStackView);
  if Node = nil then Exit;
  LoadStackSettings(Node);
end;

procedure TCpuViewSettins.LoadAsmSettings(Root: IXMLNode);

  function ToDpi(Value: Double): Integer;
  begin
    Result := Round(Value * FAsmView.CurrentPPI / 96);
  end;

var
  I: Integer;
  ColNode, ItemNode: IXMLNode;
  Column: TColumnType;
begin
  LoadFontSetting(FAsmView, Root);
  ColNode := FindNode(Root, xmlColumns);
  if ColNode = nil then Exit;
  for I := 0 to ColNode.ChildNodes.Count - 1 do
  begin
    ItemNode := GetChildNode(ColNode, I);
    Column := TColumnType(GetEnumValue(TypeInfo(TColumnType), GetNodeAttr(ItemNode, xmlItem)));
    FAsmView.Header.ColumnWidth[Column] := ToDpi(XMLReadDouble(ItemNode, xmlWidth));
  end;
end;

procedure TCpuViewSettins.LoadColorMap(Root: IXMLNode);
var
  Cm: THexViewColorMap;
  CmAsm: TAsmColorMap;
  CmStack: TStackColorMap;
  CmReg: TRegistersColorMap;

  procedure UpdateColorMap(Map: THexViewColorMap);
  begin
    Map.BackgroundColor := Cm.BackgroundColor;
    Map.BookmarkBackgroundColor := Cm.BookmarkBackgroundColor;
    Map.BookmarkBorderColor := Cm.BookmarkBorderColor;
    Map.BookmarkTextColor := Cm.BookmarkTextColor;
    Map.CaretColor := Cm.CaretColor;
    Map.CaretTextColor := Cm.CaretTextColor;
    Map.GroupColor := Cm.GroupColor;
    Map.InfoBackgroundColor := Cm.InfoBackgroundColor;
    Map.InfoBorderColor := Cm.InfoBorderColor;
    Map.InfoTextColor := Cm.InfoTextColor;
    Map.HeaderBackgroundColor := Cm.HeaderBackgroundColor;
    Map.HeaderBorderColor := Cm.HeaderBorderColor;
    Map.HeaderColumnSeparatorColor := Cm.HeaderColumnSeparatorColor;
    Map.HeaderTextColor := Cm.HeaderTextColor;
    Map.RowSeparatorColor := Cm.RowSeparatorColor;
    Map.SelectColor := Cm.SelectColor;
    Map.SelectInactiveColor := Cm.SelectInactiveColor;
    Map.TextColor := Cm.TextColor;
    Map.TextCommentColor := Cm.TextCommentColor;
  end;

begin
  Cm := FAsmView.ColorMap;
  CmAsm := FAsmView.ColorMap;
  CmReg := FRegView.ColorMap;
  CmStack := FStackView.ColorMap;

  Cm.ColorMode := TColorMode(GetEnumValue(TypeInfo(TColorMode), GetNodeAttr(Root, xmlMode)));

  // общие цвета для всех четырех вьюверов
  if Cm.ColorMode = cmCustom then
  begin
    Cm.BackgroundColor := GetNodeAttr(Root, xmlBackgroundColor);
    Cm.BookmarkBackgroundColor := GetNodeAttr(Root, xmlBookmarkBackgroundColor);
    Cm.BookmarkBorderColor := GetNodeAttr(Root, xmlBookmarkBorderColor);
    Cm.BookmarkTextColor := GetNodeAttr(Root, xmlBookmarkTextColor);
    Cm.CaretColor := GetNodeAttr(Root, xmlCaretColor);
    Cm.CaretTextColor := GetNodeAttr(Root, xmlCaretTextColor);
    Cm.GroupColor := GetNodeAttr(Root, xmlGroupColor);
    Cm.InfoBackgroundColor := GetNodeAttr(Root, xmlInfoBackgroundColor);
    Cm.InfoBorderColor := GetNodeAttr(Root, xmlInfoBorderColor);
    Cm.InfoTextColor := GetNodeAttr(Root, xmlInfoTextColor);
    Cm.HeaderBackgroundColor := GetNodeAttr(Root, xmlHeaderBackgroundColor);
    Cm.HeaderBorderColor := GetNodeAttr(Root, xmlHeaderBorderColor);
    Cm.HeaderColumnSeparatorColor := GetNodeAttr(Root, xmlHeaderColumnSeparatorColor);
    Cm.HeaderTextColor := GetNodeAttr(Root, xmlHeaderTextColor);
    Cm.RowSeparatorColor := GetNodeAttr(Root, xmlRowSeparatorColor);
    Cm.SelectColor := GetNodeAttr(Root, xmlSelectColor);
    Cm.SelectInactiveColor := GetNodeAttr(Root, xmlSelectInactiveColor);
    Cm.TextColor := GetNodeAttr(Root, xmlTextColor);
    Cm.TextCommentColor := GetNodeAttr(Root, xmlTextCommentColor);
  end;
  UpdateColorMap(CmAsm);
  UpdateColorMap(CmStack);
  UpdateColorMap(CmReg);

  // настройки цвета для дизассемблера
  if not VarIsNull(GetNodeAttr(Root, xmlArrowDownColor)) then
  begin
    CmAsm.ArrowDownColor := GetNodeAttr(Root, xmlArrowDownColor);
    CmAsm.ArrowDownSelectedColor := GetNodeAttr(Root, xmlArrowDownSelectedColor);
    CmAsm.ArrowUpColor := GetNodeAttr(Root, xmlArrowUpColor);
    CmAsm.ArrowUpSelectedColor := GetNodeAttr(Root, xmlArrowUpSelectedColor);
    CmAsm.BreakPointActiveColor := GetNodeAttr(Root, xmlBpActiveColor);
    CmAsm.BreakPointActiveFontColor := GetNodeAttr(Root, xmlBpActiveFontColor);
    CmAsm.BreakPointColor := GetNodeAttr(Root, xmlBpColor);
    CmAsm.BreakPointDisabledColor := GetNodeAttr(Root, xmlBpDisabledColor);
    CmAsm.BreakPointDisabledFontColor := GetNodeAttr(Root, xmlBpDisabledFontColor);
    CmAsm.BreakPointFontColor := GetNodeAttr(Root, xmlBpFontColor);
    CmAsm.JmpMarkColor := GetNodeAttr(Root, xmlJmpMarkColor);
    CmAsm.JmpMarkTextColor := GetNodeAttr(Root, xmlJmpMarkTextColor);
    CmAsm.SeparatorBackgroundColor := GetNodeAttr(Root, xmlSeparatorBackgroundColor);
    CmAsm.SeparatorBorderColor := GetNodeAttr(Root, xmlSeparatorBorderColor);
    CmAsm.SeparatorTextColor := GetNodeAttr(Root, xmlSeparatorTextColor);
    CmAsm.NumberColor := GetNodeAttr(Root, xmlNumberColor);
    CmAsm.InstructionColor := GetNodeAttr(Root, xmlInstructionColor);
    CmAsm.RegColor := GetNodeAttr(Root, xmlInsRegColor);
    CmAsm.PrefixColor := GetNodeAttr(Root, xmlPrefixColor);
    CmAsm.JmpColor := GetNodeAttr(Root, xmlJmpColor);
    CmAsm.KernelColor := GetNodeAttr(Root, xmlKernelColor);
    CmAsm.NopColor := GetNodeAttr(Root, xmlNopColor);
    CmAsm.RegHighlightBackColor := GetNodeAttr(Root, xmlRegHighlightBackColor);
    CmAsm.RegHighlightFontColor := GetNodeAttr(Root, xmlRegHighlightFontColor);
    CmAsm.RIPMarkColor := GetNodeAttr(Root, xmlRIPMarkColor);
    CmAsm.RIPBackgroundColor := GetNodeAttr(Root, xmlRIPBackgroundColor);
    CmAsm.RIPBackgroundFontColor := GetNodeAttr(Root, xmlRIPBackgroundFontColor);
    CmAsm.SizePfxColor := GetNodeAttr(Root, xmlSizePfxColor);
    CmAsm.SourceLineColor := GetNodeAttr(Root, xmlSourceLineColor);
  end;

  // настройки цвета для регистров
  if not VarIsNull(GetNodeAttr(Root, xmlHintColor)) then
  begin
    CmReg.HintColor := GetNodeAttr(Root, xmlHintColor);
    CmReg.RegColor := GetNodeAttr(Root, xmlRegColor);
    CmReg.ValueColor := GetNodeAttr(Root, xmlValueColor);
    CmReg.ValueModifiedColor := GetNodeAttr(Root, xmlValueModifiedColor);
  end;

  // настройки цвета для стека
  if not VarIsNull(GetNodeAttr(Root, xmlFrameColor)) then
  begin
    CmStack.EmptyStackColor := GetNodeAttr(Root, xmlEmptyStackColor);
    CmStack.FrameColor := GetNodeAttr(Root, xmlFrameColor);
    CmStack.FrameActiveColor := GetNodeAttr(Root, xmlFrameActiveColor);
  end;
end;

procedure TCpuViewSettins.LoadDumpSettings(Root: IXMLNode);
var
  Node: IXMLNode;
begin
  LoadFontSetting(FDumpView, Root);
  Node := FindNode(Root, xmlByteView);
  if Node = nil then Exit;
  FDumpView.ByteViewMode := TByteViewMode(
    GetEnumValue(TypeInfo(TByteViewMode), GetNodeAttr(Node, xmlAttrMode)));
  Node := FindNode(Root, xmlEncoder);
  if Node = nil then Exit;
  FDumpView.Encoder.EncodeType := TCharEncoderType(
    GetEnumValue(TypeInfo(TCharEncoderType), GetNodeAttr(Node, xmlAttrMode)));
  FDumpView.Encoder.EncodingName := GetNodeAttr(Node, xmlEncoderName);
  FDumpView.Encoder.CodePage := GetNodeAttr(Node, xmlEncoderCP);
end;

procedure TCpuViewSettins.LoadFontSetting(AView: TFWCustomHexView;
  Root: IXMLNode);
var
  Node: IXMLNode;
begin
  Node := FindNode(Root, xmlFont);
  if Assigned(Node) then
    TViewAccess(AView).Font.Height := Round(
      XMLReadDouble(Node, xmlFontSize) * AView.CurrentPPI / 96);
end;

procedure TCpuViewSettins.LoadRegSettings(Root: IXMLNode);
var
  Ctx: IXMLNode;
begin
  LoadFontSetting(FRegView, Root);
  Ctx := FindNode(Root, xmlContext);
  if Ctx = nil then Exit;
  if GetNodeAttr(Ctx, xmlContextName) <> GetRegisterContextName then Exit;
  LoadRegisterContext(Ctx);
end;

procedure TCpuViewSettins.LoadStackSettings(Root: IXMLNode);
begin
  LoadFontSetting(FStackView, Root);
end;

procedure TCpuViewSettins.Save;
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
    SaveColorMap(NewChild(Node, xmlColor));
    SaveAsmSettings(NewChild(Node, xmlAsmView));
    SaveDumpSettings(NewChild(Node, xmlDumpView));
    SaveRegSettings(NewChild(Node, xmlRegView));
    SaveStackSettings(NewChild(Node, xmlStackView));
    WriteXML(XMLDocument, ExtractFilePath(ParamStr(0)) + SettingsName);
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
    SaveColorMap(Node.AddChild(xmlColor));
    SaveAsmSettings(Node.AddChild(xmlAsmView));
    SaveDumpSettings(Node.AddChild(xmlDumpView));
    SaveRegSettings(Node.AddChild(xmlRegView));
    SaveStackSettings(Node.AddChild(xmlStackView));
    XMLDocument.SaveToFile(ExtractFilePath(ParamStr(0)) + SettingsName);
  finally
    XMLDocument.Active := False;
  end;
  {$ENDIF}
end;

procedure TCpuViewSettins.SaveAsmSettings(Root: IXMLNode);

  function ToDpi(Value: Integer): Double;
  begin
    Result := Value * 96 / FAsmView.CurrentPPI;
  end;

var
  I: TColumnType;
  ColNode, ItemNode: IXMLNode;
begin
  SaveFontSetting(FAsmView, Root);
  ColNode := NewChild(Root, xmlColumns);
  for I := Low(TColumnType) to High(TColumnType) do
    if I in TViewAccess(FAsmView).Header.Columns then
    begin
      ItemNode := NewChild(ColNode, xmlItem);
      SetNodeAttr(ItemNode, xmlItem, GetEnumName(TypeInfo(TColumnType), Integer(I)));
      XMLWriteDouble(ItemNode, xmlWidth, ToDpi(FAsmView.Header.ColumnWidth[I]));
    end;
end;

procedure TCpuViewSettins.SaveColorMap(Root: IXMLNode);
var
  Cm: THexViewColorMap;
  CmAsm: TAsmColorMap;
  CmStack: TStackColorMap;
  CmReg: TRegistersColorMap;
begin
  Cm := FAsmView.ColorMap;
  SetNodeAttr(Root, xmlMode, GetEnumName(TypeInfo(TColorMode), Integer(Cm.ColorMode)));
  // общие цвета для всех четырех вьюверов
  if Cm.ColorMode = cmCustom then
  begin
    SetNodeAttr(Root, xmlBackgroundColor, Cm.BackgroundColor);
    SetNodeAttr(Root, xmlBookmarkBackgroundColor, Cm.BookmarkBackgroundColor);
    SetNodeAttr(Root, xmlBookmarkBorderColor, Cm.BookmarkBorderColor);
    SetNodeAttr(Root, xmlBookmarkTextColor, Cm.BookmarkTextColor);
    SetNodeAttr(Root, xmlCaretColor, Cm.CaretColor);
    SetNodeAttr(Root, xmlCaretTextColor, Cm.CaretTextColor);
    SetNodeAttr(Root, xmlGroupColor, Cm.GroupColor);
    SetNodeAttr(Root, xmlInfoBackgroundColor, Cm.InfoBackgroundColor);
    SetNodeAttr(Root, xmlInfoBorderColor, Cm.InfoBorderColor);
    SetNodeAttr(Root, xmlInfoTextColor, Cm.InfoTextColor);
    SetNodeAttr(Root, xmlHeaderBackgroundColor, Cm.HeaderBackgroundColor);
    SetNodeAttr(Root, xmlHeaderBorderColor, Cm.HeaderBorderColor);
    SetNodeAttr(Root, xmlHeaderColumnSeparatorColor, Cm.HeaderColumnSeparatorColor);
    SetNodeAttr(Root, xmlHeaderTextColor, Cm.HeaderTextColor);
    SetNodeAttr(Root, xmlRowSeparatorColor, Cm.RowSeparatorColor);
    SetNodeAttr(Root, xmlSelectColor, Cm.SelectColor);
    SetNodeAttr(Root, xmlSelectInactiveColor, Cm.SelectInactiveColor);
    SetNodeAttr(Root, xmlTextColor, Cm.TextColor);
    SetNodeAttr(Root, xmlTextCommentColor, Cm.TextCommentColor);
  end;
  // настройки цвета для дизассемблера
  CmAsm := FAsmView.ColorMap;
  if CmAsm.ColorMode = cmCustom then
  begin
    SetNodeAttr(Root, xmlArrowDownColor, CmAsm.ArrowDownColor);
    SetNodeAttr(Root, xmlArrowDownSelectedColor, CmAsm.ArrowDownSelectedColor);
    SetNodeAttr(Root, xmlArrowUpColor, CmAsm.ArrowUpColor);
    SetNodeAttr(Root, xmlArrowUpSelectedColor, CmAsm.ArrowUpSelectedColor);
    SetNodeAttr(Root, xmlBpActiveColor, CmAsm.BreakPointActiveColor);
    SetNodeAttr(Root, xmlBpActiveFontColor, CmAsm.BreakPointActiveFontColor);
    SetNodeAttr(Root, xmlBpColor, CmAsm.BreakPointColor);
    SetNodeAttr(Root, xmlBpDisabledColor, CmAsm.BreakPointDisabledColor);
    SetNodeAttr(Root, xmlBpDisabledFontColor, CmAsm.BreakPointDisabledFontColor);
    SetNodeAttr(Root, xmlBpFontColor, CmAsm.BreakPointFontColor);
    SetNodeAttr(Root, xmlJmpMarkColor, CmAsm.JmpMarkColor);
    SetNodeAttr(Root, xmlJmpMarkTextColor, CmAsm.JmpMarkTextColor);
    SetNodeAttr(Root, xmlSeparatorBackgroundColor, CmAsm.SeparatorBackgroundColor);
    SetNodeAttr(Root, xmlSeparatorBorderColor, CmAsm.SeparatorBorderColor);
    SetNodeAttr(Root, xmlSeparatorTextColor, CmAsm.SeparatorTextColor);
    SetNodeAttr(Root, xmlNumberColor, CmAsm.NumberColor);
    SetNodeAttr(Root, xmlInstructionColor, CmAsm.InstructionColor);
    SetNodeAttr(Root, xmlInsRegColor, CmAsm.RegColor);
    SetNodeAttr(Root, xmlPrefixColor, CmAsm.PrefixColor);
    SetNodeAttr(Root, xmlJmpColor, CmAsm.JmpColor);
    SetNodeAttr(Root, xmlKernelColor, CmAsm.KernelColor);
    SetNodeAttr(Root, xmlNopColor, CmAsm.NopColor);
    SetNodeAttr(Root, xmlRegHighlightBackColor, CmAsm.RegHighlightBackColor);
    SetNodeAttr(Root, xmlRegHighlightFontColor, CmAsm.RegHighlightFontColor);
    SetNodeAttr(Root, xmlRIPMarkColor, CmAsm.RIPMarkColor);
    SetNodeAttr(Root, xmlRIPBackgroundColor, CmAsm.RIPBackgroundColor);
    SetNodeAttr(Root, xmlRIPBackgroundFontColor, CmAsm.RIPBackgroundFontColor);
    SetNodeAttr(Root, xmlSizePfxColor, CmAsm.SizePfxColor);
    SetNodeAttr(Root, xmlSourceLineColor, CmAsm.SourceLineColor);
  end;
  // настройки цвета для регистров
  CmReg := FRegView.ColorMap;
  if CmReg.ColorMode = cmCustom then
  begin
    SetNodeAttr(Root, xmlHintColor, CmReg.HintColor);
    SetNodeAttr(Root, xmlRegColor, CmReg.RegColor);
    SetNodeAttr(Root, xmlValueColor, CmReg.ValueColor);
    SetNodeAttr(Root, xmlValueModifiedColor, CmReg.ValueModifiedColor);
  end;
  // настройки цвета для стека
  CmStack := FStackView.ColorMap;
  if CmStack.ColorMode = cmCustom then
  begin
    SetNodeAttr(Root, xmlEmptyStackColor, CmStack.EmptyStackColor);
    SetNodeAttr(Root, xmlFrameColor, CmStack.FrameColor);
    SetNodeAttr(Root, xmlFrameActiveColor, CmStack.FrameActiveColor);
  end;
end;

procedure TCpuViewSettins.SaveDumpSettings(Root: IXMLNode);
var
  ByteView, Encoder: IXMLNode;
begin
  SaveFontSetting(FDumpView, Root);
  ByteView := NewChild(Root, xmlByteView);
  SetNodeAttr(ByteView, xmlAttrMode,
    GetEnumName(TypeInfo(TByteViewMode), Integer(FDumpView.ByteViewMode)));
  Encoder := NewChild(Root, xmlEncoder);
  SetNodeAttr(Encoder, xmlAttrMode,
    GetEnumName(TypeInfo(TCharEncoderType), Integer(FDumpView.Encoder.EncodeType)));
  SetNodeAttr(Encoder, xmlEncoderName, FDumpView.Encoder.EncodingName);
  SetNodeAttr(Encoder, xmlEncoderCP, FDumpView.Encoder.CodePage);
end;

procedure TCpuViewSettins.SaveFontSetting(AView: TFWCustomHexView;
  Root: IXMLNode);
begin
  XMLWriteDouble(NewChild(Root, xmlFont), xmlFontSize,
    TViewAccess(AView).Font.Height * 96 / AView.CurrentPPI);
end;

procedure TCpuViewSettins.SaveRegSettings(Root: IXMLNode);
var
  Ctx: IXMLNode;
begin
  SaveFontSetting(FRegView, Root);
  Ctx := NewChild(Root, xmlContext);
  SetNodeAttr(Ctx, xmlContextName, GetRegisterContextName);
  SaveRegisterContext(Ctx);
end;

procedure TCpuViewSettins.SaveStackSettings(Root: IXMLNode);
begin
  SaveFontSetting(FStackView, Root);
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
  Ctx := RegView.Context as TIntelCpuContext;
  Flags := FindNode(Root, xmlFlags);
  if Flags = nil then Exit;
  Ctx.FPUMode := TFPUMode(
    GetEnumValue(TypeInfo(TFPUMode), GetNodeAttr(Flags, xmlFPUMode)));
  Ctx.MapMode := TIntelCpuMapMode(
    GetEnumValue(TypeInfo(TIntelCpuMapMode), GetNodeAttr(Flags, xmlMapMode)));
  Ctx.ShowDebug := GetNodeAttr(Flags, xmlShowDebug);
  Ctx.ShowFPU := GetNodeAttr(Flags, xmlShowFPU);
  Ctx.ShowXMM := GetNodeAttr(Flags, xmlShowXMM);
  Ctx.ShowYMM := GetNodeAttr(Flags, xmlShowYMM);
  Regs := FindNode(Root, xmlRegList);
  if Regs = nil then Exit;
  for I := 0 to Regs.ChildNodes.Count - 1 do
  begin
    ItemNode := GetChildNode(Regs, I);
    RegID := GetNodeAttr(ItemNode, xmlRegID);
    Ctx.ViewMode[RegID] := TRegViewMode(
      GetEnumValue(TypeInfo(TRegViewMode), GetNodeAttr(ItemNode, xmlMode)));
  end;
end;

procedure TIntelCpuViewSettins.SaveRegisterContext(Root: IXMLNode);
var
  Ctx: TIntelCpuContext;
  Flags, Regs, ItemNode: IXMLNode;
  I: Integer;
begin
  Ctx := RegView.Context as TIntelCpuContext;
  Flags := NewChild(Root, xmlFlags);
  SetNodeAttr(Flags, xmlFPUMode,
    GetEnumName(TypeInfo(TFPUMode), Integer(Ctx.FPUMode)));
  SetNodeAttr(Flags, xmlMapMode,
    GetEnumName(TypeInfo(TIntelCpuMapMode), Integer(Ctx.MapMode)));
  SetNodeAttr(Flags, xmlShowDebug, Ctx.ShowDebug);
  SetNodeAttr(Flags, xmlShowFPU, Ctx.ShowFPU);
  SetNodeAttr(Flags, xmlShowXMM, Ctx.ShowXMM);
  SetNodeAttr(Flags, xmlShowYMM, Ctx.ShowYMM);
  Regs := nil;
  for I in [0..7, 9..16, 33, 41, 49, 58, 74] do
  begin
    if Ctx.ViewMode[I] = rvmHex then Continue;
    if Regs = nil then
      Regs := NewChild(Root, xmlRegList);
    ItemNode := NewChild(Regs, xmlItem);
    SetNodeAttr(ItemNode, xmlRegID, I);
    SetNodeAttr(ItemNode, xmlMode,
      GetEnumName(TypeInfo(TRegViewMode), Integer(Ctx.ViewMode[I])));
  end;
end;

end.
