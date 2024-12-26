////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Settings.pas
//  * Purpose   : CPU-View Core Settings.
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
  Generics.Collections,
  {$IFDEF FPC}
  LCLIntf,
  LCLType,
  LCLProc,
  DOM,
  XMLRead,
  XMLWrite,
  {$ELSE}
  Windows,
  XMLIntf, xmldom, XMLDoc,
  {$ENDIF}
  Menus,
  FWHexView,
  FWHexView.Common,
  FWHexView.MappedView,
  CpuView.Common,
  CpuView.Viewers,
  CpuView.CPUContext,
  CpuView.IntelContext,
  CpuView.XML,
  CpuView.ExtendedHint;

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
  xmlShortCuts = 'shortcuts';
  xmlColumns = 'columns';
  xmlLeft = 'left';
  xmlTop = 'top';
  xmlHeight = 'height';
  xmlWidth = 'width';
  xmlFont = 'font';
  xmlFontSize = 'size';
  xmlMaximized = 'maxstate';
  xmlSplitter = 'splitters';
  xmlSaveFormSession = 'sessionForm';
  xmlSaveViewersSession = 'sessionView';
  xmlShowFullAddress = 'showFullAddress';
  xmlShowFuncName = 'showFuncName';
  xmlShowJumps = 'showJumps';
  xmlShowOpcodes = 'showOpcodes';
  xmlShowSrc = 'showSrc';
  xmlUseDebugInfo = 'useDbgInfo';
  xmlDbgLog = 'DbgLog';
  xmlDbgDump = 'CrashDmp';
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
  xmlSelectTextContrastDarkColor = 'selectContrastDark';
  xmlSelectTextContrastLightColor = 'selectContrastLight';
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
  xmlAddrValidateE = 'addrExec';
  xmlAddrValidateR = 'addrRead';
  xmlAddrValidateS = 'addrStack';

  xmlValidation = 'useValidation';
  xmlHint = 'hint';
  xmlHintFlag = 'hintFlag';
  xmlForceFindSymbols = 'useForceFindSymbols';
  xmlExtendedHints = 'useExtendedHints';
  xmlHintPointerValues = 'useHintPointerValues';

  // not used
  xmlContext = 'ctx';
  xmlContextName = 'name';

type
  TAsmSettings = record
    ColumnWidth: array [TColumnType] of Double;
    DisplayFunc: Boolean;
    ShowFullAddress: Boolean;
    ShowJumps: Boolean;
    ShowOpcodes: Boolean;
    ShowSourceLines: Boolean;
    FontHeight: Double;
    Hints: Boolean;
  end;

  TDumpSettings = record
    ByteViewMode: TByteViewMode;
    CodePage: Integer;
    EncodeType: TCharEncoderType;
    EncodingName: string;
    FontHeight: Double;
    AddrValidation: array [TAddrValidationType] of Boolean;
    Hints: Boolean;
  end;

  TStackSettings = record
    FontHeight: Double;
    AddrValidation: array [TAddrValidationType] of Boolean;
    Hints: Boolean;
  end;

  TSplitters = (spTopHorz, spBottomHorz, spCenterVert);

  TCpuViewDlgSettings = record
    BoundsRect: TRect;
    Maximized: Boolean;
    SplitterPos: array [TSplitters] of Double;
  end;

  TColorMapItem = record
    Id: string;
    Description: string;
  end;

  TCpuViewShortCut = record
    Key1: Word;
    Shift1: TShiftState;
    Key2: Word;
    Shift2: TShiftState;
  end;

  TShortCutType = (
    sctCloseCpuView,
    sctViewerJmpIn, sctViewerStepBack,
    sctStepIn, sctStepOut, sctStepOver, sctToggleBP, sctRunTo, sctRunToUser,
    sctReturnToDef, sctNewIP);

  TShortCutMode = (scmDefault, scmMSVC, scmCustom);

  TSettingPart = (spAll, spSession, spColors, spShortCuts);

  { TCpuViewSettins }

  TCpuViewSettins = class
  strict private
    FAsmSettings: TAsmSettings;
    FColorMode: TColorMode;
    FColors: TDictionary<string, TColor>;
    FColorsMap: TList<TColorMapItem>;
    FCpuViewDlgSettings: TCpuViewDlgSettings;
    FDumpSettings: TDumpSettings;
    FExtendedHints: Boolean;
    FForceFindSymbols: Boolean;
    FFontName: string;
    FPointerValues: TPointerValues;
    FSaveFormSession: Boolean;
    FSaveViewersSession: Boolean;
    FShotCutMode: TShortCutMode;
    FShortCuts: array [TShortCutType] of TCpuViewShortCut;
    FStackSettings: TStackSettings;
    FRegSettings: TContextAbstractSettings;
    FUseDebugInfo: Boolean;
    FUseDebugLog: Boolean;
    FUseCrashDump: Boolean;
    FUseAddrValidation: Boolean;
    function GetColor(const Index: string): TColor;
    function GetDumpValidation(Index: TAddrValidationType): Boolean;
    function GetHintInRegForFlag: Boolean;
    function GetHintInRegForReg: Boolean;
    function GetShotCut(Index: TShortCutType): TCpuViewShortCut;
    function GetStackValidation(Index: TAddrValidationType): Boolean;
    function GetValidationReg(Index: TAddrValidationType): Boolean;
    procedure SetColor(const Index: string; Value: TColor);
    procedure SetDumpValidation(Index: TAddrValidationType; AValue: Boolean);
    procedure SetHintInRegForFlag(AValue: Boolean);
    procedure SetHintInRegForReg(AValue: Boolean);
    procedure SetShotCut(Index: TShortCutType; const AValue: TCpuViewShortCut);
    procedure SeStackValidation(Index: TAddrValidationType; AValue: Boolean);
    procedure SetValidationReg(Index: TAddrValidationType; AValue: Boolean);
  private
    function DpiToDouble(AValue: Integer; AView: TFWCustomHexView): Double;
    function DoubleToDpi(AValue: Double; AView: TFWCustomHexView): Integer;

    procedure LoadFromAddrHightLightColorMap(Value: TAddressViewColorMap);
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
    procedure LoadFromXML_ShortCuts(Root: IXMLNode);
    procedure LoadFromXML_StackSettings(Root: IXMLNode);

    procedure RestoreViewDefSettings(AView: TFWCustomHexView);

    procedure SaveToAddrHightLightColorMap(Value: TAddressViewColorMap);
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
    procedure SaveToXML_ShortCuts(Root: IXMLNode);
    procedure SaveToXML_StackSettings(Root: IXMLNode);
  protected
    procedure InitColorMap;
    procedure InitDefaultColors;
    procedure InitDefaultSession;
    procedure InitDefaultShortCuts;
    function GetRegisterContextName: string; virtual; abstract;
    procedure LoadRegisterContext(Root: IXMLNode); virtual; abstract;
    procedure SaveRegisterContext(Root: IXMLNode); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ColorsExport(const FilePath: string);
    procedure ColorsImport(const FilePath: string);

    procedure GetSessionFromAsmView(AAsmView: TAsmView);
    procedure GetSessionFromContext(AContext: TAbstractCPUContext);
    procedure GetSessionFromDumpView(ADumpView: TDumpView);
    procedure GetSessionFromRegView(ARegView: TRegView);
    procedure GetSessionFromStackView(AStackView: TStackView);

    procedure FillCustomShortCuts;
    procedure Reset(APart: TSettingPart = spAll);
    procedure Load(const FilePath: string);
    procedure Save(const FilePath: string);

    procedure SetSettingsToAsmView(AAsmView: TAsmView);
    procedure SetSettingsToContext(AContext: TAbstractCPUContext);
    procedure SetSettingsToDumpView(ADumpView: TDumpView);
    procedure SetSettingsToRegView(ARegView: TRegView);
    procedure SetSettingsToStackView(AStackView: TStackView);

    property ColorsMap: TList<TColorMapItem> read FColorsMap;
    property ColorMode: TColorMode read FColorMode write FColorMode;
    property Color[const Index: string]: TColor read GetColor write SetColor;
    property CpuViewDlgSettings: TCpuViewDlgSettings read FCpuViewDlgSettings write FCpuViewDlgSettings;
    property ExtendedHints: Boolean read FExtendedHints write FExtendedHints;
    property ExtendedHintPointerValues: TPointerValues read FPointerValues write FPointerValues;
    property ForceFindSymbols: Boolean read FForceFindSymbols write FForceFindSymbols;
    property HintInAsm: Boolean read FAsmSettings.Hints write FAsmSettings.Hints;
    property HintInDump: Boolean read FDumpSettings.Hints write FDumpSettings.Hints;
    property HintInRegForReg: Boolean read GetHintInRegForReg write SetHintInRegForReg;
    property HintInRegForFlag: Boolean read GetHintInRegForFlag write SetHintInRegForFlag;
    property HintInStack: Boolean read FStackSettings.Hints write FStackSettings.Hints;
    property FontName: string read FFontName write FFontName;
    property SaveFormSession: Boolean read FSaveFormSession write FSaveFormSession;
    property SaveViewersSession: Boolean read FSaveViewersSession write FSaveViewersSession;
    property ShotCutMode: TShortCutMode read FShotCutMode write FShotCutMode;
    property ShotCut[Index: TShortCutType]: TCpuViewShortCut read GetShotCut write SetShotCut;
    property ShowCallFuncName: Boolean read FAsmSettings.DisplayFunc write FAsmSettings.DisplayFunc;
    property ShowFullAddress: Boolean read FAsmSettings.ShowFullAddress write FAsmSettings.ShowFullAddress;
    property ShowJumps: Boolean read FAsmSettings.ShowJumps write FAsmSettings.ShowJumps;
    property ShowOpcodes: Boolean read FAsmSettings.ShowOpcodes write FAsmSettings.ShowOpcodes;
    property ShowSourceLines: Boolean read FAsmSettings.ShowSourceLines write FAsmSettings.ShowSourceLines;
    property UseDebugInfo: Boolean read FUseDebugInfo write FUseDebugInfo;
    property UseDebugLog: Boolean read FUseDebugLog write FUseDebugLog;
    property UseCrashDump: Boolean read FUseCrashDump write FUseCrashDump;
    property UseAddrValidation: Boolean read FUseAddrValidation write FUseAddrValidation;
    property ValidationDump[Index: TAddrValidationType]: Boolean read GetDumpValidation write SetDumpValidation;
    property ValidationReg[Index: TAddrValidationType]: Boolean read GetValidationReg write SetValidationReg;
    property ValidationStack[Index: TAddrValidationType]: Boolean read GetStackValidation write SeStackValidation;
  end;

  function KeyShiftToText(Key: Word; Shift: TShiftState): string;

implementation

type
  TViewAccess = class(TFWCustomHexView);

const
{$IFNDEF FPC}
  VK_UNKNOWN = 0;
  VK_C = $43;
  VK_N = $4E;
  VK_O = $4F;
{$ENDIF}

  DefaultShortCuts: array [TShortCutType] of TCpuViewShortCut = (
    (Key1: VK_ESCAPE; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_RETURN; Shift1: []; Key2: VK_ADD; Shift2: []),
    (Key1: VK_BACK; Shift1: []; Key2: VK_SUBTRACT; Shift2: []),
    (Key1: VK_F7; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    {$IFDEF MSWINDOWS}
    (Key1: VK_F7; Shift1: [ssCtrl]; Key2: VK_UNKNOWN; Shift2: []),
    {$ENDIF}
    {$IFDEF LINUX}
    (Key1: VK_F8; Shift1: [ssShift]; Key2: VK_UNKNOWN; Shift2: []),
    {$ENDIF}
    (Key1: VK_F8; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F2; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F4; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F9; Shift1: [ssAlt]; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_O; Shift1: [ssCtrl]; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_N; Shift1: [ssCtrl]; Key2: VK_UNKNOWN; Shift2: [])
  );

function KeyShiftToText(Key: Word; Shift: TShiftState): string;
begin
  Result := ShortCutToText(ShortCut(Key, Shift));
end;

procedure TextToKeyShift(const AText: string; var Key: Word; var Shift: TShiftState);
begin
  ShortCutToKey(TextToShortCut(AText), Key, Shift);
end;

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
    SaveToXML_Colors(Node);
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

constructor TCpuViewSettins.Create;
begin
  FRegSettings := GetContextSettingsClass.Create;
  FColors := TDictionary<string, TColor>.Create;
  FColorsMap := TList<TColorMapItem>.Create;
  InitColorMap;
  Reset;
end;

destructor TCpuViewSettins.Destroy;
begin
  FColorsMap.Free;
  FColors.Free;
  FRegSettings.Free;
  inherited;
end;

function TCpuViewSettins.DoubleToDpi(AValue: Double;
  AView: TFWCustomHexView): Integer;
begin
  Result := Round(AValue * AView.CurrentPPI / 96);
end;

function TCpuViewSettins.GetDumpValidation(Index: TAddrValidationType): Boolean;
begin
  Result := FDumpSettings.AddrValidation[Index];
end;

function TCpuViewSettins.GetStackValidation(Index: TAddrValidationType
  ): Boolean;
begin
  Result := FStackSettings.AddrValidation[Index];
end;

function TCpuViewSettins.GetValidationReg(Index: TAddrValidationType): Boolean;
begin
  Result := FRegSettings.AddrValidation[Index];
end;

function TCpuViewSettins.GetHintInRegForFlag: Boolean;
begin
  Result := FRegSettings.HintForFlag;
end;

function TCpuViewSettins.GetHintInRegForReg: Boolean;
begin
  Result := FRegSettings.HintForReg;
end;

procedure TCpuViewSettins.LoadFromAddrHightLightColorMap(
  Value: TAddressViewColorMap);
begin
  FColors.Add(xmlAddrValidateE, Value.AddrExecuteColor);
  FColors.Add(xmlAddrValidateR, Value.AddrReadColor);
  FColors.Add(xmlAddrValidateS, Value.AddrStackColor);
end;

function TCpuViewSettins.GetShotCut(Index: TShortCutType): TCpuViewShortCut;
const
  MSVCShortCuts: array [TShortCutType] of TCpuViewShortCut = (
    (Key1: VK_ESCAPE; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_RETURN; Shift1: []; Key2: VK_ADD; Shift2: []),
    (Key1: VK_BACK; Shift1: []; Key2: VK_SUBTRACT; Shift2: []),
    (Key1: VK_F11; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F11; Shift1: [ssShift]; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F10; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F9; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F10; Shift1: [ssCtrl]; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_UNKNOWN; Shift1: []; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_MULTIPLY; Shift1: [ssAlt]; Key2: VK_UNKNOWN; Shift2: []),
    (Key1: VK_F10; Shift1: [ssCtrl, ssShift]; Key2: VK_UNKNOWN; Shift2: [])
  );
begin
  case ShotCutMode of
    scmDefault: Result := DefaultShortCuts[Index];
    scmMSVC: Result := MSVCShortCuts[Index];
    scmCustom: Result := FShortCuts[Index];
  end;
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
  // от вьювера загружаются только сессионные настройки
  // остальные хранятся сами по себе

  // only session settings are loaded from the viewer
  // the rest are kept on their own
  FillChar(FAsmSettings.ColumnWidth, SizeOf(FAsmSettings.ColumnWidth), 0);
  for I := Low(TColumnType) to High(TColumnType) do
    if I in AAsmView.Header.Columns then
      FAsmSettings.ColumnWidth[I] := DpiToDouble(AAsmView.Header.ColumnWidth[I],AAsmView);
  FAsmSettings.FontHeight := DpiToDouble(AAsmView.Font.Height, AAsmView);
end;

procedure TCpuViewSettins.GetSessionFromContext(AContext: TAbstractCPUContext);
begin
  FRegSettings.LoadFromContext(AContext);
end;

procedure TCpuViewSettins.GetSessionFromDumpView(ADumpView: TDumpView);
begin
  FDumpSettings.ByteViewMode := ADumpView.ByteViewMode;
  FDumpSettings.CodePage := ADumpView.Encoder.CodePage;
  FDumpSettings.EncodeType := ADumpView.Encoder.EncodeType;
  FDumpSettings.EncodingName := ADumpView.Encoder.EncodingName;
  FDumpSettings.FontHeight := DpiToDouble(ADumpView.Font.Height, ADumpView);
end;

procedure TCpuViewSettins.GetSessionFromRegView(ARegView: TRegView);
begin
  FRegSettings.FontHeight := DpiToDouble(ARegView.Font.Height, ARegView);
end;

procedure TCpuViewSettins.GetSessionFromStackView(AStackView: TStackView);
begin
  FStackSettings.FontHeight := DpiToDouble(AStackView.Font.Height, AStackView);
end;

procedure TCpuViewSettins.FillCustomShortCuts;
var
  I: TShortCutType;
begin
  for I := Low(TShortCutType) to High(TShortCutType) do
    FShortCuts[I] := ShotCut[I];
  FShotCutMode := scmCustom;
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
    LoadFromAddrHightLightColorMap(StackColorMap);
  finally
    StackColorMap.Free;
  end;
end;

procedure TCpuViewSettins.InitDefaultSession;
const
  DefaultFontHeight = -12;
begin
  FAsmSettings := Default(TAsmSettings);
  FAsmSettings.ColumnWidth[ctWorkSpace] := 32;
  FAsmSettings.ColumnWidth[ctJmpLine] := 82;
  FAsmSettings.ColumnWidth[ctOpcode] := 170;
  FAsmSettings.ColumnWidth[ctDescription] := 250;
  FAsmSettings.ColumnWidth[ctComment] := 440;
  FAsmSettings.DisplayFunc := True;
  FAsmSettings.ShowFullAddress := True;
  FAsmSettings.ShowJumps := True;
  FAsmSettings.ShowOpcodes := True;
  FAsmSettings.ShowSourceLines := True;
  FAsmSettings.FontHeight := DefaultFontHeight;
  FAsmSettings.Hints := True;

  FCpuViewDlgSettings := Default(TCpuViewDlgSettings);
  FCpuViewDlgSettings.SplitterPos[spTopHorz] := 34;
  FCpuViewDlgSettings.SplitterPos[spBottomHorz] := 45;
  FCpuViewDlgSettings.SplitterPos[spCenterVert] := 38;

  FDumpSettings := Default(TDumpSettings);
  FDumpSettings.FontHeight := DefaultFontHeight;
  FDumpSettings.AddrValidation[avtExecutable] := True;
  FDumpSettings.AddrValidation[avtReadable] := True;
  FDumpSettings.AddrValidation[avtStack] := True;
  FDumpSettings.Hints := True;

  {$IFDEF MSWINDOWS}
  FFontName := 'Consolas';
  {$ENDIF}
  {$IFDEF UNIX}
  FFontName := 'DejaVu Sans Mono';
  {$ENDIF}

  FRegSettings.FontHeight := DefaultFontHeight;
  FRegSettings.InitDefault;

  FSaveFormSession := True;
  FSaveViewersSession := True;

  FStackSettings.FontHeight := DefaultFontHeight;
  FStackSettings.AddrValidation[avtExecutable] := True;
  FStackSettings.AddrValidation[avtReadable] := True;
  FStackSettings.AddrValidation[avtStack] := True;
  FStackSettings.Hints := True;

  FUseDebugInfo := True;
  FUseDebugLog := True;
  FUseCrashDump := True;
  FUseAddrValidation := True;
  FForceFindSymbols := True;
  FExtendedHints := True;
  FPointerValues := [bvmHex64..bvmFloat80];
end;

procedure TCpuViewSettins.InitDefaultShortCuts;
begin
  FShotCutMode := scmDefault;
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
    Reset;
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
  FColors.Add(xmlSelectTextContrastDarkColor, Value.SelectTextContrastDarkColor);
  FColors.Add(xmlSelectTextContrastLightColor, Value.SelectTextContrastLightColor);
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
  FAsmSettings.FontHeight := XMLReadDouble(Root, xmlFontSize);
  FAsmSettings.DisplayFunc := GetNodeAttr(Root, xmlShowFuncName);
  FAsmSettings.ShowJumps := GetNodeAttr(Root, xmlShowJumps);
  FAsmSettings.ShowFullAddress := GetNodeAttr(Root, xmlShowFullAddress);
  FAsmSettings.ShowOpcodes := GetNodeAttr(Root, xmlShowOpcodes);
  FAsmSettings.ShowSourceLines := GetNodeAttr(Root, xmlShowSrc);
  FAsmSettings.Hints := GetNodeAttr(Root, xmlHint);

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
  I, APointerValues: Integer;
  SplittersNode, ItemNode: IXMLNode;
  Splitter: TSplitters;
begin
  FSaveFormSession := GetNodeAttr(Root, xmlSaveFormSession);
  FSaveViewersSession := GetNodeAttr(Root, xmlSaveViewersSession);
  FFontName := GetNodeAttr(Root, xmlFont);
  FUseDebugInfo := GetNodeAttr(Root, xmlUseDebugInfo);
  FUseAddrValidation := GetNodeAttr(Root, xmlValidation);
  FUseDebugLog := GetNodeAttr(Root, xmlDbgLog);
  FUseCrashDump := GetNodeAttr(Root, xmlDbgDump);
  FForceFindSymbols := GetNodeAttr(Root, xmlForceFindSymbols);
  FExtendedHints := GetNodeAttr(Root, xmlExtendedHints);
  APointerValues := GetNodeAttr(Root, xmlHintPointerValues);
  FPointerValues := PPointerValues(@APointerValues)^;
  if FSaveFormSession then
  begin
    FCpuViewDlgSettings.BoundsRect.Left := GetNodeAttr(Root, xmlLeft);
    FCpuViewDlgSettings.BoundsRect.Top := GetNodeAttr(Root, xmlTop);
    FCpuViewDlgSettings.BoundsRect.Width := GetNodeAttr(Root, xmlWidth);
    FCpuViewDlgSettings.BoundsRect.Height := GetNodeAttr(Root, xmlHeight);
    FCpuViewDlgSettings.Maximized := GetNodeAttr(Root, xmlMaximized);
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
  FColorMode := TColorMode(
    GetEnumValue(TypeInfo(TColorMode), GetNodeAttr(Root, xmlMode)));
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
  FDumpSettings.FontHeight := XMLReadDouble(Root, xmlFontSize);
  FDumpSettings.AddrValidation[avtExecutable] := GetNodeAttr(Root, xmlAddrValidateE);
  FDumpSettings.AddrValidation[avtReadable] := GetNodeAttr(Root, xmlAddrValidateR);
  FDumpSettings.AddrValidation[avtStack] := GetNodeAttr(Root, xmlAddrValidateS);
  FDumpSettings.Hints := GetNodeAttr(Root, xmlHint);
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
  Node := FindNode(Root, xmlShortCuts);
  if Node = nil then Exit;
  LoadFromXML_ShortCuts(Node);
end;

procedure TCpuViewSettins.LoadFromXML_RegSettings(Root: IXMLNode);
begin
  FRegSettings.FontHeight := XMLReadDouble(Root, xmlFontSize);
  FRegSettings.AddrValidation[avtExecutable] := GetNodeAttr(Root, xmlAddrValidateE);
  FRegSettings.AddrValidation[avtReadable] := GetNodeAttr(Root, xmlAddrValidateR);
  FRegSettings.AddrValidation[avtStack] := GetNodeAttr(Root, xmlAddrValidateS);
  FRegSettings.HintForReg := GetNodeAttr(Root, xmlHint);
  FRegSettings.HintForFlag := GetNodeAttr(Root, xmlHintFlag);
  FRegSettings.LoadFromXML(Root);
end;

procedure TCpuViewSettins.LoadFromXML_ShortCuts(Root: IXMLNode);
var
  I: TShortCutType;
  ItemName: string;
  ShortCut: TCpuViewShortCut;
begin
  FShotCutMode := TShortCutMode(
    GetEnumValue(TypeInfo(TShortCutMode), GetNodeAttr(Root, xmlMode)));
  for I := Low(TShortCutType) to High(TShortCutType) do
  begin
    ItemName := GetEnumName(TypeInfo(TShortCutType), Integer(I));
    ShortCut := Default(TCpuViewShortCut);
    TextToKeyShift(GetNodeAttrString(Root, ItemName + '1'), ShortCut.Key1, ShortCut.Shift1);
    TextToKeyShift(GetNodeAttrString(Root, ItemName + '2'), ShortCut.Key2, ShortCut.Shift2);
    FShortCuts[I] := ShortCut;
  end;
end;

procedure TCpuViewSettins.LoadFromXML_StackSettings(Root: IXMLNode);
begin
  FStackSettings.FontHeight := XMLReadDouble(Root, xmlFontSize);
  FStackSettings.AddrValidation[avtExecutable] := GetNodeAttr(Root, xmlAddrValidateE);
  FStackSettings.AddrValidation[avtReadable] := GetNodeAttr(Root, xmlAddrValidateR);
  FStackSettings.AddrValidation[avtStack] := GetNodeAttr(Root, xmlAddrValidateS);
  FStackSettings.Hints := GetNodeAttr(Root, xmlHint);
end;

procedure TCpuViewSettins.Reset(APart: TSettingPart);
begin
  case APart of
    spAll:
    begin
      InitDefaultSession;
      InitDefaultColors;
      InitDefaultShortCuts;
    end;
    spSession: InitDefaultSession;
    spColors: InitDefaultColors;
    spShortCuts: InitDefaultShortCuts;
  end;
end;

procedure TCpuViewSettins.RestoreViewDefSettings(AView: TFWCustomHexView);
begin
  AView.ResetViewState;
  TViewAccess(AView).Font.Name := FontName;
  SaveToDefaultColorMap(TViewAccess(AView).ColorMap);
end;

procedure TCpuViewSettins.SaveToAddrHightLightColorMap(
  Value: TAddressViewColorMap);
begin
  if ColorMode <> cmCustom then Exit;
  Value.AddrExecuteColor := Color[xmlAddrValidateE];
  Value.AddrReadColor := Color[xmlAddrValidateR];
  Value.AddrStackColor := Color[xmlAddrValidateS];
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
  if ColorMode <> cmCustom then Exit;
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
  Value.ColorMode := ColorMode;
  if ColorMode <> cmCustom then Exit;
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
  Value.SelectTextContrastDarkColor := Color[xmlSelectTextContrastDarkColor];
  Value.SelectTextContrastLightColor := Color[xmlSelectTextContrastLightColor];
  Value.TextColor := Color[xmlTextColor];
  Value.TextCommentColor := Color[xmlTextCommentColor];
  Value.WorkSpaceTextColor := Color[xmlWorkSpaceTextColor];
end;

procedure TCpuViewSettins.SaveToRegColorMap(Value: TRegistersColorMap);
begin
  if ColorMode <> cmCustom then Exit;
  Value.HintColor := Color[xmlHintColor];
  Value.RegColor := Color[xmlRegColor];
  Value.ValueColor := Color[xmlValueColor];
  Value.ValueModifiedColor := Color[xmlValueModifiedColor];
end;

procedure TCpuViewSettins.SaveToStackColorMap(Value: TStackColorMap);
begin
  if ColorMode <> cmCustom then Exit;
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
  XMLWriteDouble(Root, xmlFontSize, FAsmSettings.FontHeight);
  SetNodeAttr(Root, xmlShowFuncName, FAsmSettings.DisplayFunc);
  SetNodeAttr(Root, xmlShowJumps, FAsmSettings.ShowJumps);
  SetNodeAttr(Root, xmlShowFullAddress, FAsmSettings.ShowFullAddress);
  SetNodeAttr(Root, xmlShowOpcodes, FAsmSettings.ShowOpcodes);
  SetNodeAttr(Root, xmlShowSrc, FAsmSettings.ShowSourceLines);
  SetNodeAttr(Root, xmlHint, FDumpSettings.Hints);

  ColNode := NewChild(Root, xmlColumns);
  Columns := [ctWorkSpace..ctComment];
  if not FAsmSettings.ShowJumps then
    Exclude(Columns, ctJmpLine);
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
  SetNodeAttr(Root, xmlSaveFormSession, FSaveFormSession);
  SetNodeAttr(Root, xmlSaveViewersSession, FSaveViewersSession);
  SetNodeAttr(Root, xmlFont, FFontName);
  SetNodeAttr(Root, xmlUseDebugInfo, FUseDebugInfo);
  SetNodeAttr(Root, xmlValidation, FUseAddrValidation);
  SetNodeAttr(Root, xmlDbgLog, FUseDebugLog);
  SetNodeAttr(Root, xmlDbgDump, FUseCrashDump);
  SetNodeAttr(Root, xmlForceFindSymbols, FForceFindSymbols);
  SetNodeAttr(Root, xmlExtendedHints, FExtendedHints);
  SetNodeAttr(Root, xmlHintPointerValues, PInteger(@FPointerValues)^);
  if FSaveFormSession then
  begin
    SetNodeAttr(Root, xmlLeft, FCpuViewDlgSettings.BoundsRect.Left);
    SetNodeAttr(Root, xmlTop, FCpuViewDlgSettings.BoundsRect.Top);
    SetNodeAttr(Root, xmlWidth, FCpuViewDlgSettings.BoundsRect.Width);
    SetNodeAttr(Root, xmlHeight, FCpuViewDlgSettings.BoundsRect.Height);
    SetNodeAttr(Root, xmlMaximized, FCpuViewDlgSettings.Maximized);
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
  XMLWriteDouble(Root, xmlFontSize, FAsmSettings.FontHeight);
  ByteView := NewChild(Root, xmlByteView);
  SetNodeAttr(ByteView, xmlAttrMode,
    GetEnumName(TypeInfo(TByteViewMode), Integer(FDumpSettings.ByteViewMode)));
  Encoder := NewChild(Root, xmlEncoder);
  SetNodeAttr(Encoder, xmlAttrMode,
    GetEnumName(TypeInfo(TCharEncoderType), Integer(FDumpSettings.EncodeType)));
  SetNodeAttr(Encoder, xmlEncoderName, FDumpSettings.EncodingName);
  SetNodeAttr(Encoder, xmlEncoderCP, FDumpSettings.CodePage);
  SetNodeAttr(Root, xmlAddrValidateE, FDumpSettings.AddrValidation[avtExecutable]);
  SetNodeAttr(Root, xmlAddrValidateR, FDumpSettings.AddrValidation[avtReadable]);
  SetNodeAttr(Root, xmlAddrValidateS, FDumpSettings.AddrValidation[avtStack]);
  SetNodeAttr(Root, xmlHint, FDumpSettings.Hints);
end;

procedure TCpuViewSettins.SaveToXML_Full(Root: IXMLNode);
begin
  {$IFDEF FPC}
  SaveToXML_BasicSettings(NewChild(Root, xmlBasic));
  SaveToXML_Colors(NewChild(Root, xmlColor));
  SaveToXML_AsmSettings(NewChild(Root, xmlAsmView));
  SaveToXML_DumpSettings(NewChild(Root, xmlDumpView));
  SaveToXML_RegSettings(NewChild(Root, xmlRegView));
  SaveToXML_ShortCuts(NewChild(Root, xmlShortCuts));
  SaveToXML_StackSettings(NewChild(Root, xmlStackView));
  {$ELSE}
  SaveToXML_BasicSettings(Root.AddChild(xmlBasic));
  SaveToXML_Colors(Root.AddChild(xmlColor));
  SaveToXML_AsmSettings(Root.AddChild(xmlAsmView));
  SaveToXML_DumpSettings(Root.AddChild(xmlDumpView));
  SaveToXML_RegSettings(Root.AddChild(xmlRegView));
  SaveToXML_ShortCuts(Root.AddChild(xmlShortCuts));
  SaveToXML_StackSettings(Root.AddChild(xmlStackView));
  {$ENDIF}
end;

procedure TCpuViewSettins.SaveToXML_RegSettings(Root: IXMLNode);
begin
  XMLWriteDouble(Root, xmlFontSize, FRegSettings.FontHeight);
  SetNodeAttr(Root, xmlAddrValidateE, FRegSettings.AddrValidation[avtExecutable]);
  SetNodeAttr(Root, xmlAddrValidateR, FRegSettings.AddrValidation[avtReadable]);
  SetNodeAttr(Root, xmlAddrValidateS, FRegSettings.AddrValidation[avtStack]);
  SetNodeAttr(Root, xmlHint, FRegSettings.HintForReg);
  SetNodeAttr(Root, xmlHintFlag, FRegSettings.HintForFlag);
  FRegSettings.SaveToXML(Root);
end;

procedure TCpuViewSettins.SaveToXML_ShortCuts(Root: IXMLNode);
var
  I: TShortCutType;
  ItemName: string;
  ShortCut: TCpuViewShortCut;
begin
  SetNodeAttr(Root, xmlMode, GetEnumName(TypeInfo(TShortCutMode), Integer(FShotCutMode)));
  for I := Low(TShortCutType) to High(TShortCutType) do
  begin
    ItemName := GetEnumName(TypeInfo(TShortCutType), Integer(I));
    ShortCut := FShortCuts[I];
    if ShortCut.Key1 <> VK_UNKNOWN then
      SetNodeAttr(Root, ItemName + '1', KeyShiftToText(ShortCut.Key1, ShortCut.Shift1));
    if ShortCut.Key2 <> VK_UNKNOWN then
      SetNodeAttr(Root, ItemName + '2', KeyShiftToText(ShortCut.Key2, ShortCut.Shift2));
  end;
end;

procedure TCpuViewSettins.SaveToXML_StackSettings(Root: IXMLNode);
begin
  XMLWriteDouble(Root, xmlFontSize, FStackSettings.FontHeight);
  SetNodeAttr(Root, xmlAddrValidateE, FStackSettings.AddrValidation[avtExecutable]);
  SetNodeAttr(Root, xmlAddrValidateR, FStackSettings.AddrValidation[avtReadable]);
  SetNodeAttr(Root, xmlAddrValidateS, FStackSettings.AddrValidation[avtStack]);
  SetNodeAttr(Root, xmlHint, FStackSettings.Hints);
end;

procedure TCpuViewSettins.SeStackValidation(Index: TAddrValidationType;
  AValue: Boolean);
begin
  FStackSettings.AddrValidation[Index] := AValue;
end;

procedure TCpuViewSettins.SetDumpValidation(Index: TAddrValidationType;
  AValue: Boolean);
begin
  FDumpSettings.AddrValidation[Index] := AValue;
end;

procedure TCpuViewSettins.SetValidationReg(Index: TAddrValidationType;
  AValue: Boolean);
begin
  FRegSettings.AddrValidation[Index] := AValue;
end;

procedure TCpuViewSettins.SetHintInRegForFlag(AValue: Boolean);
begin
  FRegSettings.HintForFlag := AValue;
end;

procedure TCpuViewSettins.SetHintInRegForReg(AValue: Boolean);
begin
  FRegSettings.HintForReg := AValue;
end;

procedure TCpuViewSettins.SetShotCut(Index: TShortCutType;
  const AValue: TCpuViewShortCut);
begin
  FShortCuts[Index] := AValue;
end;

procedure TCpuViewSettins.InitColorMap;

  procedure Add(const ID, Description: string);
  var
    Item: TColorMapItem;
  begin
    Item.Description := Description;
    Item.Id := Id;
    FColorsMap.Add(Item);
  end;

begin
  Add(xmlBackgroundColor, 'Background');
  Add(xmlBookmarkBackgroundColor, 'Bookmark Background');
  Add(xmlBookmarkBorderColor, 'Bookmark Border');
  Add(xmlBookmarkTextColor, 'Bookmark Text');
  Add(xmlCaretColor, 'Caret');
  Add(xmlCaretTextColor, 'Caret Text');
  Add(xmlGroupColor, 'Group');
  Add(xmlInfoBackgroundColor, 'Info Background');
  Add(xmlInfoBorderColor, 'Info Border');
  Add(xmlInfoTextColor, 'Info Text');
  Add(xmlHeaderBackgroundColor, 'Header Background');
  Add(xmlHeaderBorderColor, 'Header Border');
  Add(xmlHeaderColumnSeparatorColor, 'Header Column Separator');
  Add(xmlHeaderTextColor, 'Header Text');
  Add(xmlRowSeparatorColor, 'Row Separator');
  Add(xmlSelectColor, 'Select');
  Add(xmlSelectInactiveColor, 'Select Inactive');
  Add(xmlSelectTextContrastDarkColor, 'Select Text Contrast Dark');
  Add(xmlSelectTextContrastLightColor, 'Select Text Contrast Light');
  Add(xmlTextColor, 'Text');
  Add(xmlTextCommentColor, 'Comment');
  Add(xmlWorkSpaceTextColor, 'WorkSpace Text');

  Add(xmlActiveJumpColor, 'Asm: Active Jmp');
  Add(xmlArrowDownColor, 'Asm: Arrow Down');
  Add(xmlArrowDownSelectedColor, 'Asm: Arrow Down Selected');
  Add(xmlArrowUpColor, 'Asm: Arrow Up');
  Add(xmlArrowUpSelectedColor, 'Asm: Arrow Up Selected');
  Add(xmlBpActiveColor, 'Asm: BreakPoint Active');
  Add(xmlBpActiveFontColor, 'Asm: BreakPoint Active Font');
  Add(xmlBpColor, 'Asm: BreakPoint');
  Add(xmlBpDisabledColor, 'Asm: BreakPoint Disabled');
  Add(xmlBpDisabledFontColor, 'Asm: BreakPoint Disabled Font');
  Add(xmlBpFontColor, 'Asm: BreakPoint Font');
  Add(xmlJmpMarkColor, 'Asm: Jmp Mark');
  Add(xmlJmpMarkTextColor, 'Asm: Jmp Mark Text');
  Add(xmlSeparatorBackgroundColor, 'Asm: Separator Background');
  Add(xmlSeparatorBorderColor, 'Asm: Separator Border');
  Add(xmlSeparatorTextColor, 'Asm: Separator Text');
  Add(xmlNumberColor, 'Asm: Number');
  Add(xmlInstructionColor, 'Asm: Instruction');
  Add(xmlInsRegColor, 'Asm: Reg');
  Add(xmlPrefixColor, 'Asm: Prefix');
  Add(xmlJmpColor, 'Asm: Jmp');
  Add(xmlKernelColor, 'Asm: Kernel');
  Add(xmlNopColor, 'Asm: Nop');
  Add(xmlRegHighlightBackColor, 'Asm: Register Highlight Background');
  Add(xmlRegHighlightFontColor, 'Asm: Register Highlight Font');
  Add(xmlRIPBackgroundColor, 'Asm: RIP Background');
  Add(xmlRIPBackgroundFontColor, 'Asm: RIP Background Font');
  Add(xmlSizePfxColor, 'Asm: Size Prefix');
  Add(xmlSourceLineColor, 'Asm: Source Line');

  Add(xmlHintColor, 'Reg: Hint');
  Add(xmlRegColor, 'Reg: Register');
  Add(xmlValueColor, 'Reg: Value');
  Add(xmlValueModifiedColor, 'Reg: Modified Value');

  Add(xmlAddrPCColor, 'Stack: AddrPC');
  Add(xmlAddrPCFontColor, 'Stack: AddrPC Font');
  Add(xmlEmptyStackColor, 'Stack: Empty Stack');
  Add(xmlFrameColor, 'Stack: Frame');
  Add(xmlFrameActiveColor, 'Stack: Active Frame');
  Add(xmlStackPointColor, 'Stack: Stack Point');
  Add(xmlStackPointFontColor, 'Stack: Stack Point Font');

  Add(xmlAddrValidateE, 'Dump/Stack: Address is Executable');
  Add(xmlAddrValidateR, 'Dump/Stack: Address is Readable');
  Add(xmlAddrValidateS, 'Dump/Stack: Address is in Stack');
end;

procedure TCpuViewSettins.SetColor(const Index: string; Value: TColor);
begin
  FColors.AddOrSetValue(Index, Value);
end;

procedure TCpuViewSettins.SetSettingsToAsmView(AAsmView: TAsmView);
var
  I: TColumnType;
begin
  RestoreViewDefSettings(AAsmView);
  SaveToAsmColorMap(AAsmView.ColorMap);
  AAsmView.Font.Height := DoubleToDpi(FAsmSettings.FontHeight, AAsmView);
  for I := Low(TColumnType) to High(TColumnType) do
    if I in AAsmView.Header.Columns then
      FAsmSettings.ColumnWidth[I] := DoubleToDpi(AAsmView.Header.ColumnWidth[I], AAsmView);
  if not FAsmSettings.ShowJumps then
    AAsmView.Header.Columns := AAsmView.Header.Columns - [ctJmpLine];
  if not FAsmSettings.ShowOpcodes then
    AAsmView.Header.Columns := AAsmView.Header.Columns - [ctOpcode];
  AAsmView.ShowHint := HintInAsm;
end;

procedure TCpuViewSettins.SetSettingsToContext(AContext: TAbstractCPUContext);
begin
  FRegSettings.SaveToContext(AContext);
end;

procedure TCpuViewSettins.SetSettingsToDumpView(ADumpView: TDumpView);
begin
  RestoreViewDefSettings(ADumpView);
  SaveToAddrHightLightColorMap(ADumpView.ColorMap);
  ADumpView.Font.Height := DoubleToDpi(FDumpSettings.FontHeight, ADumpView);
  ADumpView.ByteViewMode := FDumpSettings.ByteViewMode;
  ADumpView.Encoder.EncodeType := FDumpSettings.EncodeType;
  ADumpView.Encoder.CodePage := FDumpSettings.CodePage;
  ADumpView.Encoder.EncodingName := FDumpSettings.EncodingName;
  // Without validation, the dump doesn't know about the addresses
  ADumpView.ShowHint := UseAddrValidation and HintInDump;
  ADumpView.ValidateAddress := UseAddrValidation;
  ADumpView.ValidateType[avtExecutable] := ValidationDump[avtExecutable];
  ADumpView.ValidateType[avtReadable] := ValidationDump[avtReadable];
  ADumpView.ValidateType[avtStack] := ValidationDump[avtStack];
end;

procedure TCpuViewSettins.SetSettingsToRegView(ARegView: TRegView);
begin
  RestoreViewDefSettings(ARegView);
  ARegView.Font.Height := DoubleToDpi(FRegSettings.FontHeight, ARegView);
  SaveToAddrHightLightColorMap(ARegView.ColorMap);
  SaveToRegColorMap(ARegView.ColorMap);
  ARegView.ShowHint := HintInRegForFlag or HintInRegForReg;
  ARegView.HintForFlag := HintInRegForFlag;
  ARegView.HintForReg := HintInRegForReg;
  ARegView.ValidateAddress := UseAddrValidation;
  ARegView.ValidateType[avtExecutable] := ValidationReg[avtExecutable];
  ARegView.ValidateType[avtReadable] := ValidationReg[avtReadable];
  ARegView.ValidateType[avtStack] := ValidationReg[avtStack];
end;

procedure TCpuViewSettins.SetSettingsToStackView(AStackView: TStackView);
begin
  RestoreViewDefSettings(AStackView);
  AStackView.Font.Height := DoubleToDpi(FStackSettings.FontHeight, AStackView);
  AStackView.ShowHint := HintInStack;
  AStackView.ValidateAddress := UseAddrValidation;
  AStackView.ValidateType[avtExecutable] := ValidationStack[avtExecutable];
  AStackView.ValidateType[avtReadable] := ValidationStack[avtReadable];
  AStackView.ValidateType[avtStack] := ValidationStack[avtStack];
  // ValidateAddress is involved in the calculation of column widths,
  // so you need to call recalculation
  AStackView.FitColumnsToBestSize;
  SaveToAddrHightLightColorMap(AStackView.ColorMap);
  SaveToStackColorMap(AStackView.ColorMap);
end;

end.

