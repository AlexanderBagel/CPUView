﻿////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Viewers.pas
//  * Purpose   : Set of Viewers to implement the CPU-View mode
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

unit CpuView.Viewers;

{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}

interface

{-$define debug_unlock_scroll}

uses
  {$IFDEF FPC}
  LCLType,
  LCLIntf,
  StdCtrls,
  {$ELSE}
  Windows,
  UITypes,
  {$ENDIF}
  Messages,
  Types,
  Classes,
  Graphics,
  SysUtils,
  Controls,
  Math,
  Clipbrd,
  Generics.Collections,
  Forms,
  FWHexView,
  FWHexView.Common,
  FWHexView.MappedView,
  FWHexView.AsmTokenizer,
  CpuView.Common,
  CpuView.CPUContext,
  CpuView.Actions;

type

  { TAsmColorMap }

  TAsmColorMap = class(TMapViewColors)
  private
    FActiveJmpColor: TColor;
    FBreakPointActiveColor: TColor;
    FBreakPointActiveFontColor: TColor;
    FBreakPointColor: TColor;
    FBreakPointDisabledColor: TColor;
    FBreakPointDisabledFontColor: TColor;
    FBreakPointFontColor: TColor;
    FInstructionColor: TColor;
    FJmpColor: TColor;
    FKernelColor: TColor;
    FNopColor: TColor;
    FNumberColor: TColor;
    FPrefixColor: TColor;
    FRegColor: TColor;
    FRegHighlightBackColor: TColor;
    FRegHighlightFontColor: TColor;
    FRIPBackgroundColor: TColor;
    FRIPBackgroundFontColor: TColor;
    FSourceLineColor: TColor;
    FSizePfxColor: TColor;
    procedure SetActiveJmpColor(const Value: TColor);
    procedure SetBreakPointActiveColor(AValue: TColor);
    procedure SetBreakPointActiveFontColor(AValue: TColor);
    procedure SetBreakPointColor(AValue: TColor);
    procedure SetBreakPointDisabledColor(AValue: TColor);
    procedure SetBreakPointDisabledFontColor(AValue: TColor);
    procedure SetBreakPointFontColor(AValue: TColor);
    procedure SetInstructionColor(const Value: TColor);
    procedure SetJmpColor(const Value: TColor);
    procedure SetKernelColor(const Value: TColor);
    procedure SetNopColor(const Value: TColor);
    procedure SetNumberColor(const Value: TColor);
    procedure SetPrefixColor(const Value: TColor);
    procedure SetRegColor(const Value: TColor);
    procedure SetRegHighlightBackColor(AValue: TColor);
    procedure SetRegHighlightFontColor(AValue: TColor);
    procedure SetRIPBackgroundColor(AValue: TColor);
    procedure SetRIPBackgroundFontColor(AValue: TColor);
    procedure SetSourceLineColor(AValue: TColor);
    procedure SetSizePfxColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InitLightMode; override;
    procedure InitDarkMode; override;
  published
    property ActiveJmpColor: TColor read FActiveJmpColor write SetActiveJmpColor stored IsColorStored;
    property BreakPointActiveColor: TColor read FBreakPointActiveColor write SetBreakPointActiveColor stored IsColorStored;
    property BreakPointActiveFontColor: TColor read FBreakPointActiveFontColor write SetBreakPointActiveFontColor stored IsColorStored;
    property BreakPointColor: TColor read FBreakPointColor write SetBreakPointColor stored IsColorStored;
    property BreakPointDisabledColor: TColor read FBreakPointDisabledColor write SetBreakPointDisabledColor stored IsColorStored;
    property BreakPointDisabledFontColor: TColor read FBreakPointDisabledFontColor write SetBreakPointDisabledFontColor stored IsColorStored;
    property BreakPointFontColor: TColor read FBreakPointFontColor write SetBreakPointFontColor stored IsColorStored;
    property InstructionColor: TColor read FInstructionColor write SetInstructionColor stored IsColorStored;
    property JmpColor: TColor read FJmpColor write SetJmpColor stored IsColorStored;
    property KernelColor: TColor read FKernelColor write SetKernelColor stored IsColorStored;
    property NopColor: TColor read FNopColor write SetNopColor stored IsColorStored;
    property NumberColor: TColor read FNumberColor write SetNumberColor stored IsColorStored;
    property PrefixColor: TColor read FPrefixColor write SetPrefixColor stored IsColorStored;
    property RegColor: TColor read FRegColor write SetRegColor stored IsColorStored;
    property RegHighlightBackColor: TColor read FRegHighlightBackColor write SetRegHighlightBackColor stored IsColorStored;
    property RegHighlightFontColor: TColor read FRegHighlightFontColor write SetRegHighlightFontColor stored IsColorStored;
    property RIPBackgroundColor: TColor read FRIPBackgroundColor write SetRIPBackgroundColor stored IsColorStored;
    property RIPBackgroundFontColor: TColor read FRIPBackgroundFontColor write SetRIPBackgroundFontColor stored IsColorStored;
    property SizePfxColor: TColor read FSizePfxColor write SetSizePfxColor stored IsColorStored;
    property SourceLineColor: TColor read FSourceLineColor write SetSourceLineColor stored IsColorStored;
  end;

  TAsmTextMetrics = class(TFixedHexByteTextMetric)
  protected
    procedure UpdateCharPosition(BytesInRow, CharWidth: Integer); override;
  end;

  TCustomAsmView = class;

  { TAsmViewDefRow }

  TAsmViewDefRow = class(TRowWithExDescription)
  private
    function AsmView: TCustomAsmView;
  protected
    procedure CorrectCanvasFont(ACanvas: TCanvas; AColumn: TColumnType); override;
    procedure DrawAddress(ACanvas: TCanvas; var ARect: TRect); override;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
  end;

  { TAsmViewSrcLineRow }

  TAsmViewSrcLineRow = class(TRowComment)
  protected
    procedure CorrectCanvasFont(ACanvas: TCanvas; {%H-}AColumn: TColumnType); override;
  end;

  { TExecutionPointPostPainter }

  TExecutionPointPostPainter = class(TLinesPostPainter)
  public
    procedure PostPaint(ACanvas: TCanvas; StartRow, EndRow: Int64;
      var Offset: TPoint); override;
  end;

  TScrollStepDirection = (ssdNone, ssdLineUp, ssdWheelUp, ssdPageUp, ssdLineDown, ssdWheelDown, ssdPageDown);

  TOnVerticalScrollEvent = procedure(Sender: TObject; AStep: TScrollStepDirection) of object;

  { TAsmViewHeader }

  TAsmViewHeader = class(TCustomHexViewHeader)
  protected
    procedure InitDefault; override;
  published
    property Columns default [ctWorkSpace..ctComment];
    property DrawColumnSeparator;
    property Visible default False;
  end;

  TOnQueryAddrType = procedure(Sender: TObject; AddrVA: Int64; var AddrType: TAddrType) of object;

  { TCustomAsmView }

  TCustomAsmView = class(TCustomMappedHexView)
  private
    FBreakPoints: TDictionary<Int64, Boolean>;
    FKeyScrollDirection: TScrollStepDirection;
    FLockKeyScroll: Boolean;
    FTokenizer: TAsmTokenizer;
    FOnScroll: TOnVerticalScrollEvent;
    FInstructionPoint: Int64;
    FCurrentIPIsActiveJmp: Boolean;
    FHighlightReg: string;
    FCacheEnd: TNotifyEvent;
    FOnQueryAddr: TOnQueryAddrType;
    procedure DoScrollStep(AStep: TScrollStepDirection);
    function GetColorMap: TAsmColorMap;
    procedure SetColorMap(const Value: TAsmColorMap);
    procedure SetCurrentIPIsActiveJmp(const Value: Boolean);
    procedure SetInstructionPoint(const Value: Int64);
    procedure SetHighlightReg(const Value: string);
  protected
    function CalculateColumnBestSize(Value: TColumnType): Integer; override;
    function CalculateJmpToRow(JmpFromRow: Int64): Int64; override;
    function CopyCommandEnabled(Value: TCopyStyle): Boolean; override;
    procedure DoBeforePaint(const ADiapason: TVisibleRowDiapason); override;
    procedure DoCacheEnd;
    procedure DoDrawToken(ACanvas: TCanvas; ATokenParam: TDrawParam;
      const ARect: TRect; AToken: PChar; var ATokenLen: Integer); override;
    procedure DoQueryAddrType(AddrVA: Int64; out AddrType: TAddrType);
    function GetCaretPreviosRowIndex(FromIndex: Int64;
      AColumn: TColumnType = ctNone): Int64; override;
    function GetCaretNextRowIndex(FromIndex: Int64;
      AColumn: TColumnType = ctNone): Int64; override;
    function GetColorMapClass: THexViewColorMapClass; override;
    function GetDefaultCaretChangeMode: TCaretChangeMode; override;
    function GetHeaderClass: THeaderClass; override;
    function GetOverloadPainterClass(Value: TPrimaryRowPainterClass): TPrimaryRowPainterClass; override;
    function IgnoreSelectionWhenCopyAddress: Boolean; override;
    procedure InitPainters; override;
    function IsJumpValid(AJmpToAddr: Int64): Boolean; override;
  protected
    // Lock Vertical Scroll
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      {%H-}MousePos: TPoint): Boolean; override;
    procedure UpdateScrollY({%H-}AOffset: Int64); override;
    procedure UpdateVerticalScrollPos; override;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SelectedInstructionAddr: Int64;
    property BreakPoints: TDictionary<Int64, Boolean> read FBreakPoints;
    property CurrentIPIsActiveJmp: Boolean read FCurrentIPIsActiveJmp write SetCurrentIPIsActiveJmp;
    property InstructionPoint: Int64 read FInstructionPoint write SetInstructionPoint;
    property HighlightReg: string read FHighlightReg write SetHighlightReg;
    property Tokenizer: TAsmTokenizer read FTokenizer;
  protected
    property ColorMap: TAsmColorMap read GetColorMap write SetColorMap stored IsColorMapStored;
    property OnCacheEnd: TNotifyEvent read FCacheEnd write FCacheEnd;
    property OnQueryAddressType: TOnQueryAddrType read FOnQueryAddr write FOnQueryAddr;
    property OnVerticalScroll: TOnVerticalScrollEvent read FOnScroll write FOnScroll;
  end;

  TAsmView = class(TCustomAsmView)
  published
    property AddressMode;
    property AddressView;
    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property ByteViewMode;
    property ColorMap;
    property Constraints;
    property Enabled;
    property Encoder;
    property Font;
    property Header;
    property HideSelection;
    property NoDataText;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShortCuts;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WheelMultiplier;
    property OnCacheEnd;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawToken;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnJmpTo;
    property OnHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnQueryAddressType;
    property OnQueryComment;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
    property OnVerticalScroll;
  end;

  { TAddressViewColorMap }

  TAddressViewColorMap = class(THexViewColorMap)
  private
    FExecuteColor: TColor;
    FReadColor: TColor;
    FStackColor: TColor;
    FDuplicateColor: TColor;
    FStringColor: TColor;
    procedure SetExecuteColor(AValue: TColor);
    procedure SetReadColor(AValue: TColor);
    procedure SetStackColor(AValue: TColor);
    procedure SetDuplicateColor(const Value: TColor);
    procedure SetStringColor(AValue: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InitLightMode; override;
    procedure InitDarkMode; override;
  published
    property AddrExecuteColor: TColor read FExecuteColor write SetExecuteColor stored IsColorStored;
    property AddrReadColor: TColor read FReadColor write SetReadColor stored IsColorStored;
    property AddrStackColor: TColor read FStackColor write SetStackColor stored IsColorStored;
    property AddrStringColor: TColor read FStringColor write SetStringColor stored IsColorStored;
    property DuplicateColor: TColor read FDuplicateColor write SetDuplicateColor stored IsColorStored;
  end;

  { TCustomAddressView }

  TCustomAddressView = class(TFWCustomHexView)
  private
    FHightLightSelected: Boolean;
    FLastInvalidAddrRect: TRect;
    FOnQueryAddr: TOnQueryAddrType;
    FValidateAddress: Boolean;
    FValidateType: array [TAddrValidationType] of Boolean;
    function GetColorMap: TAddressViewColorMap;
    function GetValidateType(Index: TAddrValidationType): Boolean;
    procedure SetColorMap(AValue: TAddressViewColorMap);
    procedure SetValidateAddress(AValue: Boolean);
    procedure SetValidateType(Index: TAddrValidationType; AValue: Boolean);
    procedure SetHightLightSelected(const Value: Boolean);
    procedure UpdateSelectionList;
  protected
    function CanDrawValidation(AAddrType: TAddrType): Boolean;
    procedure DoChange(ChangeCode: Integer); override;
    procedure DoSelectionChage(AStartAddr, AEndAddr: Int64); override;
    procedure DoQueryAddrType(AddrVA: Int64; out AddrType: TAddrType);
    procedure InitDefault; override;
    procedure InitPainters; override;
    function GetColorMapClass: THexViewColorMapClass; override;
    procedure RestoreViewParam; override;
    procedure UpdateDataMap; override;
    procedure UpdateVerticalScrollPos; override;
  protected
    property ColorMap: TAddressViewColorMap read GetColorMap write SetColorMap stored IsColorMapStored;
    property HightLightSelected: Boolean read FHightLightSelected write SetHightLightSelected default True;
    property ValidateAddress: Boolean read FValidateAddress write SetValidateAddress default True;
    property OnQueryAddressType: TOnQueryAddrType read FOnQueryAddr write FOnQueryAddr;
  public
    constructor Create(AOwner: TComponent); override;
    property ValidateType[Index: TAddrValidationType]: Boolean read GetValidateType write SetValidateType;
  published
    property Font;
    property PopupMenu;
  end;

  { TAddressViewPainter }

  TAddressViewPainter = class(TRowHexPainter)
  strict private
    FCacheAddrType: TAddrType;
    FCacheIndex, FCacheAddrIndex: Integer;
    FCacheData: TBytes;
  protected
    procedure CheckCache;
    function GetAddressAtCursor(const AMouseHitInfo: TMouseHitInfo;
      var AddrIndex: Integer): Int64;
    function GetAddrAtIndex(AIndex: Integer): Int64;
    function GetBounds(AIndex: Integer): TBoundaries;
    function GetSelBounds: TBoundaries;
    function QueryAddrType(AIndex: Integer): TAddrType;
    function View: TCustomAddressView;
  end;

  { TDumpPainter }

  TDumpPainter = class(TAddressViewPainter)
  protected
    function ColumnsDrawSupport: TFWHexViewColumnTypes; override;
    procedure DrawHeaderColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    procedure DrawHexPart(ACanvas: TCanvas; var ARect: TRect); override;
    function GetHeaderColumnCaption(AColumn: TColumnType): string; override;
    procedure GetHitInfo(var AHitInfo: TMouseHitInfo); override;
  end;

  { TCustomDumpView }

  TCustomDumpView = class(TCustomAddressView)
  protected
    procedure DoGetHint(var AHintParam: THintParam; var AHint: string); override;
    function DoLButtonDown(const AHitInfo: TMouseHitInfo): Boolean; override;
    function GetDefaultPainterClass: TPrimaryRowPainterClass; override;
    procedure InitDefault; override;
    procedure UpdateView; override;
  end;

  { TDumpView }

  TDumpView = class(TCustomDumpView)
  published
    property AddressMode;
    property AddressView;
    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property BytesInColorGroup;
    property BytesInGroup;
    property BytesInRow;
    property ByteViewMode;
    property ColorMap;
    property Constraints;
    property Enabled;
    property Encoder;
    property Font;
    property Header;
    property HideSelection;
    property HightLightSelected;
    property NoDataText;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SeparateGroupByColor;
    property ScrollBars;
    property ShortCuts;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValidateAddress;
    property Visible;
    property WheelMultiplier;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawToken;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHint;
    property OnJmpTo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnQueryAddressType;
    property OnQueryComment;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TStackColorMap }

  TStackColorMap = class(TAddressViewColorMap)
  private
    FAddrPCColor: TColor;
    FAddrPCFontColor: TColor;
    FEmptyStackColor: TColor;
    FFrameColor: TColor;
    FFrameActiveColor: TColor;
    FStackPointFontColor: TColor;
    FStackPointColor: TColor;
    procedure SetAddrPCColor(const Value: TColor);
    procedure SetAddrPCFontColor(const Value: TColor);
    procedure SetEmptyStackColor(const Value: TColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameActiveColor(const Value: TColor);
    procedure SetStackPointColor(const Value: TColor);
    procedure SetStackPointFontColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InitLightMode; override;
    procedure InitDarkMode; override;
  published
    property AddrPCColor: TColor read FAddrPCColor write SetAddrPCColor stored IsColorStored;
    property AddrPCFontColor: TColor read FAddrPCFontColor write SetAddrPCFontColor stored IsColorStored;
    property EmptyStackColor: TColor read FEmptyStackColor write SetEmptyStackColor stored IsColorStored;
    property FrameColor: TColor read FFrameColor write SetFrameColor stored IsColorStored;
    property FrameActiveColor: TColor read FFrameActiveColor write SetFrameActiveColor stored IsColorStored;
    property StackPointColor: TColor read FStackPointColor write SetStackPointColor stored IsColorStored;
    property StackPointFontColor: TColor read FStackPointFontColor write SetStackPointFontColor stored IsColorStored;
  end;

  TCustomStackView = class;

  { TStackPostPainter }

  TStackPostPainter = class(TLinesPostPainter)
  strict private
    function StackView: TCustomStackView;
  public
    procedure PostPaint(ACanvas: TCanvas; StartRow, EndRow: Int64;
      var Offset: TPoint); override;
  end;

  { TStackRowPainter }

  TStackRowPainter = class(TAddressViewPainter)
  strict private
    function StackView: TCustomStackView;
    function GetTopOfStackRowIndex(out AIndex: Int64): Boolean;
  protected
    procedure CorrectCanvasFont(ACanvas: TCanvas; AColumn: TColumnType); override;
    procedure DrawAddress(ACanvas: TCanvas; var ARect: TRect); override;
    procedure DrawHexPart(ACanvas: TCanvas; var ARect: TRect); override;
  end;

  { TCustomStackView }

  TCustomStackView = class(TCustomAddressView)
  strict private
    FFrames: TListEx<TStackFrame>;
    FAddrPCDict: TDictionary<Int64, Int64>;
    function GetColorMap: TStackColorMap;
    procedure SetColorMap(const Value: TStackColorMap);
    procedure UpdateFrameDescriptions;
  protected
    function ByteViewModeCommandEnabled(Value: TByteViewMode; var AChecked: Boolean): Boolean; override;
    function CalculateColumnBestSize(Value: TColumnType): Integer; override;
    function CopyCommandEnabled(Value: TCopyStyle): Boolean; override;
    procedure DoChange(ChangeCode: Integer); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoGetHint(var AHintParam: THintParam; var AHint: string); override;
    function GetDefaultCaretChangeMode: TCaretChangeMode; override;
    function GetDefaultPainterClass: TPrimaryRowPainterClass; override;
    procedure InitDefault; override;
    procedure InitPainters; override;
    function GetColorMapClass: THexViewColorMapClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopySelected(CopyStyle: TCopyStyle); override;
    procedure FramesUpdated;
    function IsAddrPCRow(AIndex: Int64; out AddrVA: Int64): Boolean;
    property Frames: TListEx<TStackFrame> read FFrames;
  protected
    property ColorMap: TStackColorMap read GetColorMap write SetColorMap stored IsColorMapStored;
  end;

  { TStackView }

  TStackView = class(TCustomStackView)
  published
    property AddressMode;
    property AddressView;
    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property ColorMap;
    property Constraints;
    property Enabled;
    property Encoder;
    property Font;
    property HideSelection;
    property HightLightSelected;
    property NoDataText;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShortCuts;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValidateAddress;
    property Visible;
    property WheelMultiplier;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawToken;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHint;
    property OnJmpTo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnQueryAddressType;
    property OnQueryComment;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TRegisterTextMetrics }

  TRegisterTextMetrics = class(TAbstractTextMetric)
  public
    procedure UpdateCharPosition(BytesInRow, CharWidth: Integer); override;
    function ValueMetric: TValueMetric; override;
  end;

  TCustomRegView = class;

  { TRegisterPainter }

  TRegisterPainter = class(TRowHexPainter)
  strict private
    FContext: TAbstractCPUContext;
    function View: TCustomRegView;
  protected
    function AcceptSelection: Boolean; override;
    function CaretEditMode(AColumn: TColumnType): TCaretEditMode; override;
    procedure CopyRowAsString(Builder: TSimplyStringBuilder); override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    function GetBounds(const AHitInfo: TMouseHitInfo; out ABounds: TBoundaries): Integer;
    procedure GetHitInfo(var AHitInfo: TMouseHitInfo); override;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
  end;

  { TRegistersRawData }

  TRegistersRawData = class(TRawData)
  strict private
    FContext: TAbstractCPUContext;
    function View: TCustomRegView;
  public
    function AddressToRowIndex(Value: Int64): Int64; override;
    function Count: Int64; override;
    function RawLength: Int64; override;
    function RowToAddress(ARowIndex: Int64; ValueOffset: Integer): Int64; override;
    procedure Update; override;
  end;

  { TRegistersColorMap }

  TRegistersColorMap = class(TAddressViewColorMap)
  private
    FHintColor: TColor;
    FRegColor: TColor;
    FValueColor: TColor;
    FValueModifiedColor: TColor;
    procedure SetHintColor(const Value: TColor);
    procedure SetRegColor(const Value: TColor);
    procedure SetValueColor(const Value: TColor);
    procedure SetValueModifiedColor(const Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure InitLightMode; override;
    procedure InitDarkMode; override;
  published
    property HintColor: TColor read FHintColor write SetHintColor stored IsColorStored;
    property RegColor: TColor read FRegColor write SetRegColor stored IsColorStored;
    property ValueColor: TColor read FValueColor write SetValueColor stored IsColorStored;
    property ValueModifiedColor: TColor read FValueModifiedColor write SetValueModifiedColor stored IsColorStored;
  end;

  TOnSelectedContextPopupEvent = procedure(Sender: TObject;
    MousePos: TPoint; RowIndex: Int64; ColIndex: Integer;
    var Handled: Boolean) of object;

  { TCustomRegView }

  TCustomRegView = class(TCustomAddressView, ICpuContextViewModeAction)
  private
    FContext: TAbstractCPUContext;
    FActiveRegParam: TRegParam;
    FHintFlag: Boolean;
    FHintsReg: Boolean;
    FPopup: TOnSelectedContextPopupEvent;
    FSelectedRegName: string;
    FSelectedRegValue: TRegValue;
    FSelectedRegister: TRegister;
    FQueryExternalHint: TContextQueryExternalRegHintEvent;
    procedure ContextUpdate(Sender: TObject; AChangeType: TContextChangeType);
    function GetColorMap: TRegistersColorMap;
    procedure SetColorMap(const Value: TRegistersColorMap);
    procedure SetContext(const Value: TAbstractCPUContext);
  protected
    function ByteViewModeCommandEnabled(Value: TByteViewMode; var AChecked: Boolean): Boolean; override;
    function CalculateColumnBestSize(Value: TColumnType): Integer; override;
    function ContextViewModeCommandEnabled(Value: TRegViewMode; var AChecked: Boolean): Boolean;
    function ContextViewModeCommandHandled(Value: TRegViewMode): Boolean;
    procedure ContextViewModeCommandExecute(Value: TRegViewMode);
    function CopyCommandEnabled(Value: TCopyStyle): Boolean; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoGetHint(var AHintParam: THintParam; var AHint: string); override;
    procedure DoSelectionChage(AStartAddr, AEndAddr: Int64); override;
    function GetCaretChangeMode(APainter: TAbstractPrimaryRowPainter;
      AColumn: TColumnType; Shift: TShiftState): TCaretChangeMode; override;
    function GetColorMapClass: THexViewColorMapClass; override;
    function GetDataStreamSize: Int64; override;
    function GetDefaultPainterClass: TPrimaryRowPainterClass; override;
    function GetRawDataClass: TRawDataClass; override;
    procedure InitDefault; override;
    function InternalGetRowPainter(ARowIndex: Int64): TAbstractPrimaryRowPainter; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CopySelected(CopyStyle: TCopyStyle); override;
    function RawData: TRegistersRawData;
    function ReadDataAtSelStart(var pBuffer; nSize: Integer): Integer; override;
    property Context: TAbstractCPUContext read FContext write SetContext;
    property SelectedRegister: TRegister read FSelectedRegister;
    property SelectedRegName: string read FSelectedRegName;
    property SelectedRegValue: TRegValue read FSelectedRegValue;
  protected
    property ColorMap: TRegistersColorMap read GetColorMap write SetColorMap stored IsColorMapStored;
    property HintForReg: Boolean read FHintsReg write FHintsReg default True;
    property HintForFlag: Boolean read FHintFlag write FHintFlag default True;
    property OnQueryExternalHint: TContextQueryExternalRegHintEvent read FQueryExternalHint write FQueryExternalHint;
    property OnSelectedContextPopup: TOnSelectedContextPopupEvent read FPopup write FPopup;
  end;

  { TRegView }

  TRegView = class(TCustomRegView)
  published
    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property ColorMap;
    property Constraints;
    property Cursor;
    property Enabled;
    property Encoder;
    property Font;
    property HideSelection;
    property HintForFlag;
    property HintForReg;
    property NoDataText;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ValidateAddress;
    property Visible;
    property WheelMultiplier;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawToken;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnQueryAddressType;
    property OnQueryComment;
    property OnQueryExternalHint;
    property OnSelectedContextPopup;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

  TCustomEditView = class;

  TEditTextMetric = class(TAbstractTextMetric)
  strict private
    FByteViewMode: TByteViewMode;
  protected
    procedure UpdateCharPosition(BytesInRow, CharWidth: Integer); override;
  public
    function ValueMetric: TValueMetric; override;
    property OverloadByteViewMode: TByteViewMode read FByteViewMode write FByteViewMode;
  end;

  TEditViewPainter = class(TRowHexPainter)
  strict private
    function View: TCustomEditView;
  protected
    function ByteViewMode: TByteViewMode; override;
    function CaretEditMode({%H-}AColumn: TColumnType): TCaretEditMode; override;
    function ColumnAsString(AColumn: TColumnType): string; override;
    function DrawRowColumnBackground(ACanvas: TCanvas;
      AColumn: TColumnType; const ARect: TRect): Boolean; override;
    function FormatMode: TFormatMode; override;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
  public
    function TextMetric: TAbstractTextMetric; override;
  end;

  TRegEditCommentPainter = class(TSecondaryRowPainter)
  strict private
    FFullRowRect: TRect;
    function View: TCustomEditView;
  protected
    function ColumnAsString(AColumn: TColumnType): string; override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
  end;

  TEditViewMode = (evmMM, evmX87, evmXMM, evmYMM);
  TPartViewType = (pvtHex, pvtSigned, pvtUnsigned);

  { TEditViewRawData }

  TEditViewRawData = class(TRawData)
  strict private
    FRowCount: Integer;
    FEditMode: TEditViewMode;
    FRegBuff: TBytes;
    FPartViewType: TPartViewType;
    function GetRegBuff(RowIndex: Integer): TBytes;
    procedure SetEditMode(const Value: TEditViewMode);
    procedure SetPartViewType(const Value: TPartViewType);
    function View: TCustomEditView;
  protected
    procedure OnEdit(Sender: TObject; const ACursor: TDrawParam;
      const AData: TEditParam; var Handled: Boolean);
  public
    constructor Create(AOwner: TFWCustomHexView); override;
    function Address: Int64; override;
    function AddressToRowIndex(Value: Int64): Int64; override;
    function Count: Int64; override;
    procedure InitRegBuff(const Buffer; ASize: Integer);
    procedure GetBuff(var Buffer; ASize: Integer);
    function RawLength: Int64; override;
    procedure Update; override;
    property EditMode: TEditViewMode read FEditMode write SetEditMode;
    property PartViewType: TPartViewType read FPartViewType write SetPartViewType;
    property RegBuff[RowIndex: Integer]: TBytes read GetRegBuff;
  end;

  TCustomEditView = class(TFWCustomHexView)
  private
    FExtended: TDictionary<TByteViewMode, TAbstractTextMetric>;
  protected
    function CalculateColumnBestSize(Value: TColumnType): Integer; override;
    function ColumnResetSelection(AColumn: TColumnType): Boolean; override;
    function GetDefaultPainterClass: TPrimaryRowPainterClass; override;
    function GetMetricAtByteView(AByteViewMode: TByteViewMode): TAbstractTextMetric;
    function GetRawDataClass: TRawDataClass; override;
    procedure GetRawBuff(ARowIndex: Int64; var Data: TBytes); override;
    procedure InitDefault; override;
    procedure InitPainters; override;
    function InternalGetRowPainter(ARowIndex: Int64): TAbstractPrimaryRowPainter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RawData: TEditViewRawData;
    property Header;
  end;

  TEditView = class(TCustomEditView)
  published
    property Align;
    property Anchors;
    property AutoSize;
    {$IFNDEF FPC}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property BorderStyle;
    property ColorMap;
    property Constraints;
    property Cursor;
    property Enabled;
    property Encoder;
    property Font;
    property HideSelection;
    property NoDataText;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WheelMultiplier;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawToken;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnQueryComment;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TAsmColorMap }

procedure TAsmColorMap.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAsmColorMap then
  begin
    TAsmColorMap(Dest).FActiveJmpColor := FActiveJmpColor;
    TAsmColorMap(Dest).FBreakPointActiveColor := FBreakPointActiveColor;
    TAsmColorMap(Dest).FBreakPointActiveFontColor := FBreakPointActiveFontColor;
    TAsmColorMap(Dest).FBreakPointColor := FBreakPointColor;
    TAsmColorMap(Dest).FBreakPointDisabledColor := FBreakPointDisabledColor;
    TAsmColorMap(Dest).FBreakPointDisabledFontColor := FBreakPointDisabledFontColor;
    TAsmColorMap(Dest).FBreakPointFontColor := FBreakPointFontColor;
    TAsmColorMap(Dest).FInstructionColor := FInstructionColor;
    TAsmColorMap(Dest).FJmpColor := FJmpColor;
    TAsmColorMap(Dest).FKernelColor := FKernelColor;
    TAsmColorMap(Dest).FNopColor := FNopColor;
    TAsmColorMap(Dest).FNumberColor := FNumberColor;
    TAsmColorMap(Dest).FPrefixColor := FPrefixColor;
    TAsmColorMap(Dest).FRegColor := FRegColor;
    TAsmColorMap(Dest).FRegHighlightBackColor := FRegHighlightBackColor;
    TAsmColorMap(Dest).FRegHighlightFontColor := FRegHighlightFontColor;
    TAsmColorMap(Dest).FRIPBackgroundColor := FRIPBackgroundColor;
    TAsmColorMap(Dest).FRIPBackgroundFontColor := FRIPBackgroundFontColor;
    TAsmColorMap(Dest).FSourceLineColor := FSourceLineColor;
    TAsmColorMap(Dest).FSizePfxColor := FSizePfxColor;
  end;
end;

procedure TAsmColorMap.InitDarkMode;
begin
  inherited;
  FActiveJmpColor := $7884F8;
  FBreakPointActiveColor := $AA;
  FBreakPointActiveFontColor := clWhite;
  FBreakPointColor := $ADB0FF;
  FBreakPointDisabledColor := $A3FCFF;
  FBreakPointDisabledFontColor := 0;
  FBreakPointFontColor := 0;
  FInstructionColor := $F6A289;
  FJmpColor := $7884F8;
  FKernelColor := $6658FF;
  FNopColor := $A8A8A8;
  FNumberColor := $7884F8;
  FPrefixColor := $F1F1F1;
  FRegColor := $F694B7;
  FRegHighlightBackColor := $AA;
  FRegHighlightFontColor := clWhite;
  FRIPBackgroundColor := $009D116C;
  FRIPBackgroundFontColor := clWhite;
  FSizePfxColor := $C9FFE8;
  FSourceLineColor := $A0A0A0;
end;

procedure TAsmColorMap.InitLightMode;
begin
  inherited;
  FActiveJmpColor := clRed;
  FBreakPointActiveColor := clRed;
  FBreakPointActiveFontColor := clWhite;
  FBreakPointColor := $ADB0FF;
  FBreakPointDisabledColor := $A3FCFF;
  FBreakPointDisabledFontColor := clWindowText;
  FBreakPointFontColor := clWindowText;
  FInstructionColor := clNavy;
  FJmpColor := $000080FF;
  FKernelColor := clRed;
  FNopColor := clDkGray;
  FNumberColor := clGreen;
  FPrefixColor := $5053EF;
  FRegColor := clMaroon;
  FRegHighlightBackColor := clRed;
  FRegHighlightFontColor := clWhite;
  FRIPBackgroundColor := $009D116C;
  FRIPBackgroundFontColor := clWhite;
  FSizePfxColor := clGreen;
  FSourceLineColor := clBlack;
end;

procedure TAsmColorMap.SetInstructionColor(const Value: TColor);
begin
  if InstructionColor <> Value then
  begin
    FInstructionColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetBreakPointColor(AValue: TColor);
begin
  if BreakPointColor <> AValue then
  begin
    FBreakPointColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetActiveJmpColor(const Value: TColor);
begin
  if ActiveJmpColor <> Value then
  begin
    FActiveJmpColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetBreakPointActiveColor(AValue: TColor);
begin
  if BreakPointActiveColor <> AValue then
  begin
    FBreakPointActiveColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetBreakPointActiveFontColor(AValue: TColor);
begin
  if BreakPointActiveFontColor <> AValue then
  begin
    FBreakPointActiveFontColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetBreakPointDisabledColor(AValue: TColor);
begin
  if BreakPointDisabledColor <> AValue then
  begin
    FBreakPointDisabledColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetBreakPointDisabledFontColor(AValue: TColor);
begin
  if BreakPointDisabledFontColor <> AValue then
  begin
    FBreakPointDisabledFontColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetBreakPointFontColor(AValue: TColor);
begin
  if BreakPointFontColor <> AValue then
  begin
    FBreakPointFontColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetJmpColor(const Value: TColor);
begin
  if JmpColor <> Value then
  begin
    FJmpColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetKernelColor(const Value: TColor);
begin
  if KernelColor <> Value then
  begin
    FKernelColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetNopColor(const Value: TColor);
begin
  if NopColor <> Value then
  begin
    FNopColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetNumberColor(const Value: TColor);
begin
  if NumberColor <> Value then
  begin
    FNumberColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetPrefixColor(const Value: TColor);
begin
  if PrefixColor <> Value then
  begin
    FPrefixColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetRegColor(const Value: TColor);
begin
  if RegColor <> Value then
  begin
    FRegColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetRegHighlightBackColor(AValue: TColor);
begin
  if RegHighlightBackColor <> AValue then
  begin
    FRegHighlightBackColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetRegHighlightFontColor(AValue: TColor);
begin
  if RegHighlightFontColor <> AValue then
  begin
    FRegHighlightFontColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetRIPBackgroundColor(AValue: TColor);
begin
  if RIPBackgroundColor <> AValue then
  begin
    FRIPBackgroundColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetRIPBackgroundFontColor(AValue: TColor);
begin
  if RIPBackgroundFontColor <> AValue then
  begin
    FRIPBackgroundFontColor := AValue;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetSizePfxColor(const Value: TColor);
begin
  if SizePfxColor <> Value then
  begin
    FSizePfxColor := Value;
    DoChange;
  end;
end;

procedure TAsmColorMap.SetSourceLineColor(AValue: TColor);
begin
  if SourceLineColor <> AValue then
  begin
    FSourceLineColor := AValue;
    DoChange;
  end;
end;

{ TAsmTextMetrics }

procedure TAsmTextMetrics.UpdateCharPosition(BytesInRow, CharWidth: Integer);
begin
  UpdateCharPositionText(BytesInRow, CharWidth);
end;

{ TAsmViewDefRow }

function TAsmViewDefRow.AsmView: TCustomAsmView;
begin
  Result := TCustomAsmView(Owner);
end;

procedure TAsmViewDefRow.CorrectCanvasFont(ACanvas: TCanvas;
  AColumn: TColumnType);
var
  IPIndex: Int64;
  IsBP, IsBPActive: Boolean;
begin
  if AColumn <> ctAddress then Exit;
  IPIndex := AddressToRowIndex(AsmView.InstructionPoint);
  IsBP := AsmView.BreakPoints.TryGetValue(RawData[RowIndex].Address, IsBPActive);
  if IPIndex = RowIndex then
  begin
    if IsBP and IsBPActive then
      ACanvas.Font.Color := AsmView.ColorMap.BreakPointActiveFontColor
    else
    begin
      ACanvas.Font.Color := AsmView.ColorMap.RIPBackgroundFontColor;
      ACanvas.Brush.Style := bsClear;
    end;
  end
  else
  begin
    if IsBP then
      if IsBPActive then
        ACanvas.Font.Color := AsmView.ColorMap.BreakPointFontColor
      else
        ACanvas.Font.Color := AsmView.ColorMap.BreakPointDisabledFontColor;
  end;
  if IsBP then
    ACanvas.Brush.Style := bsClear;
end;

procedure TAsmViewDefRow.DrawAddress(ACanvas: TCanvas; var ARect: TRect);
var
  IPIndex: Int64;
  IsBP, IsBPActive: Boolean;
  BackR: TRect;

  procedure FillBk;
  begin
    BackR := ARect;
    InflateRect(BackR, TextMargin, 0);
    Inc(BackR.Left);
    ACanvas.FillRect(BackR);
  end;

begin
  IPIndex := AddressToRowIndex(AsmView.InstructionPoint);
  IsBP := AsmView.BreakPoints.TryGetValue(RawData[RowIndex].Address, IsBPActive);
  if IPIndex = RowIndex then
  begin
    if IsBP and IsBPActive then
      ACanvas.Brush.Color := AsmView.ColorMap.BreakPointActiveColor
    else
      ACanvas.Brush.Color := AsmView.ColorMap.RIPBackgroundColor;
    FillBk;
  end
  else
    if IsBP then
    begin
      if IsBPActive then
        ACanvas.Brush.Color := AsmView.ColorMap.BreakPointColor
      else
        ACanvas.Brush.Color := AsmView.ColorMap.BreakPointDisabledColor;
      FillBk;
    end;
  inherited;
end;

function TAsmViewDefRow.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TAsmTextMetrics;
end;

{ TAsmViewSrcLineRow }

procedure TAsmViewSrcLineRow.CorrectCanvasFont(ACanvas: TCanvas;
  AColumn: TColumnType);
begin
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := TCustomAsmView(Owner).ColorMap.SourceLineColor;
end;

{ TExecutionPointPostPainter }

procedure TExecutionPointPostPainter.PostPaint(ACanvas: TCanvas; StartRow,
  EndRow: Int64; var Offset: TPoint);
var
  AsmView: TCustomAsmView;
  RowIndex: Int64;
  Param: TDrawLineParam;
  JmpLine: Int64;
begin
  AsmView := TCustomAsmView(Owner);
  RowIndex := AddressToRowIndex(AsmView.InstructionPoint);

  if RowIndex < StartRow then Exit;
  if RowIndex > EndRow then Exit;

  if AsmView.CurrentIPIsActiveJmp then
  begin
    PaintedLinesCount := 1;
    Param.DrawArrow := True;
    Param.DrawAlwais := False;
    Param.DrawOnlySelectedArrow := False;
    Param.SecondDraw := False;
    Param.LineIndent := TCustomAsmView(Owner).ToDpi(4);
    Param.LineVerticalMargin := DblSize(SplitMargin);
    Param.LineWidth := 1;
    Param.LineColor := TAsmColorMap(ColorMap).ActiveJmpColor;
    Param.Offset := Offset;
    if ctAddress in Columns then
      Inc(Param.Offset.X, ColumnWidth[ctAddress]);
    if ctOpcode in Columns then
      Inc(Param.Offset.X, ColumnWidth[ctOpcode]);
    JmpLine := AsmView.CalculateJmpToRow(RowIndex);
    Param.DirectionDown := JmpLine > RowIndex;
    Param.RowFrom := RowIndex;
    Param.RowTo := JmpLine;
    Param.SecondDraw := False;
    DrawLine(ACanvas, Param);
  end;
end;


{ TAsmViewHeader }

procedure TAsmViewHeader.InitDefault;
begin
  inherited;
  Columns := [ctWorkSpace..ctComment];
  Visible := False;
end;

{ TCustomAsmView }

function TCustomAsmView.CalculateColumnBestSize(Value: TColumnType): Integer;
begin
  if Value = ctOpcode then
    Result := ToDpi(170)
  else
    Result := inherited;
end;

function TCustomAsmView.CalculateJmpToRow(JmpFromRow: Int64): Int64;
begin
  Result := inherited;
  // TCustomAsmView работает с кэшем у которого первые и последние
  // несколько строк никогда не должны быть отображены и используются
  // как маркер для обновления данных. Поэтому если адрес прыжка
  // выходит за диапазон кэша, корректируем номер строчки на которую будет
  // ссылаться прыжок, используя для этого первую и последнюю строку.

  // TCustomAsmView works with a cache where the first and last few lines
  // should never be displayed and are used as a marker to update the data.
  // Therefore, if the jump address is out of the cache range,
  // adjust the line number to which the jump will be referenced
  // using the first and last lines.
  if Result < 0 then
  begin
    if RawData[JmpFromRow].JmpToAddr > RawData[JmpFromRow].Address then
      Result := RawData.Count - 1;
  end;
end;

function TCustomAsmView.CopyCommandEnabled(Value: TCopyStyle): Boolean;
begin
  Result := Focused and (Value in [csAsText, csAddress]);
end;

procedure TCustomAsmView.DoBeforePaint(const ADiapason: TVisibleRowDiapason);
var
  APainter: TAbstractPrimaryRowPainter;
  IsAsmPainter: Boolean;
begin
  APainter := InternalGetRowPainter(ADiapason.EndRow);
  IsAsmPainter := Assigned(APainter) and
    ((APainter is TAsmViewDefRow) or (APainter is TAsmViewSrcLineRow));
  if not IsAsmPainter then
    DoCacheEnd;
end;

procedure TCustomAsmView.DoCacheEnd;
begin
  if Assigned(FCacheEnd) then
    FCacheEnd(Self);
end;

constructor TCustomAsmView.Create(AOwner: TComponent);
begin
  inherited;
  ScrollBars :=  TScrollStyle.ssBoth;
  FTokenizer := TAsmTokenizer.Create;
  DrawIncomingJmp := True;
  FBreakPoints := TDictionary<Int64, Boolean>.Create;
  BorderStyle := bsNone;
end;

destructor TCustomAsmView.Destroy;
begin
  FTokenizer.Free;
  FBreakPoints.Free;
  inherited;
end;

procedure TCustomAsmView.DoDrawToken(ACanvas: TCanvas;
  ATokenParam: TDrawParam; const ARect: TRect; AToken: PChar;
  var ATokenLen: Integer);
var
  R: TRect;
begin
  if ATokenParam.Column <> ctDescription then Exit;
  if RowStyle(ATokenParam.RowIndex) <> rsAsm then Exit;
  if ACanvas.Font.Style <> [] then Exit;
  case FTokenizer.GetToken(AToken, ATokenLen) of
    ttNumber: ACanvas.Font.Color := ColorMap.NumberColor;
    ttInstruction: ACanvas.Font.Color := ColorMap.InstructionColor;
    ttSize: ACanvas.Font.Color := ColorMap.SizePfxColor;
    ttReg:
    begin
      ACanvas.Font.Color := ColorMap.RegColor;
      if HighlightReg <> '' then
        if CompareMem(AToken, @HighlightReg[1], ATokenLen * SizeOf(Char)) then
        begin
          R := ARect;
          R.Width := CharWidth * Length(HighlightReg);
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Brush.Color := ColorMap.RegHighlightBackColor;
          ACanvas.FillRect(R);
          ACanvas.Font.Color := ColorMap.RegHighlightFontColor;
          ACanvas.Brush.Style := bsClear;
        end;
    end;
    ttPrefix: ACanvas.Font.Color := ColorMap.PrefixColor;
    ttJmp: ACanvas.Font.Color := ColorMap.JmpColor;
    ttKernel: ACanvas.Font.Color := ColorMap.KernelColor;
    ttNop: ACanvas.Font.Color := ColorMap.NopColor;
  end;
end;

function TCustomAsmView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := False;
  {$ifdef debug_unlock_scroll}
  inherited; Exit;
  {$endif}
  if ssCtrl in Shift then
    Zoom(IfThen(WheelDelta > 0, 1, -1))
  else
    if WheelDelta > 0 then
      DoScrollStep(ssdWheelUp)
    else
      DoScrollStep(ssdWheelDown);
end;

procedure TCustomAsmView.DoQueryAddrType(AddrVA: Int64;
  out AddrType: TAddrType);
begin
  AddrType := atNone;
  if Assigned(FOnQueryAddr) then
    FOnQueryAddr(Self, AddrVA, AddrType);
end;

procedure TCustomAsmView.DoScrollStep(AStep: TScrollStepDirection);
var
  SavedColumn: TColumnType;
begin
  if FLockKeyScroll then Exit;
  SavedColumn := CaretPosData.Column;
  if Assigned(FOnScroll) then
    FOnScroll(Self, AStep);
  UpdateCaretColumn(SavedColumn);
end;

function TCustomAsmView.GetCaretNextRowIndex(FromIndex: Int64;
  AColumn: TColumnType): Int64;
begin
  if FKeyScrollDirection = ssdLineDown then
  begin
    if FromIndex >= VisibleRowCount then
    begin
      Result := FromIndex;
      DoScrollStep(ssdLineDown);
    end
    else
      Result := inherited;
    Exit;
  end;
  Result := CaretPosData.RowIndex;
  DoScrollStep(FKeyScrollDirection);
end;

function TCustomAsmView.GetCaretPreviosRowIndex(FromIndex: Int64;
  AColumn: TColumnType): Int64;
begin
  if FKeyScrollDirection = ssdLineUp then
  begin
    if FromIndex < 0 then
      FromIndex := VisibleRowCount + 1;
    if FromIndex = 0 then
    begin
      Result := FromIndex;
      DoScrollStep(ssdLineUp);
    end
    else
      Result := inherited;
    Exit;
  end;
  Result := CaretPosData.RowIndex;
  DoScrollStep(FKeyScrollDirection);
end;

function TCustomAsmView.GetColorMap: TAsmColorMap;
begin
  Result := TAsmColorMap(inherited ColorMap);
end;

function TCustomAsmView.GetColorMapClass: THexViewColorMapClass;
begin
  Result := TAsmColorMap;
end;

function TCustomAsmView.GetDefaultCaretChangeMode: TCaretChangeMode;
begin
  Result := ccmSelectRow;
end;

function TCustomAsmView.GetHeaderClass: THeaderClass;
begin
  Result := TAsmViewHeader;
end;

function TCustomAsmView.GetOverloadPainterClass(
  Value: TPrimaryRowPainterClass): TPrimaryRowPainterClass;
begin
  if Value = TRowAssembler then
    Result := TAsmViewDefRow
  else
    if Value = TRowComment then
      Result := TAsmViewSrcLineRow
    else
      Result := Value;
end;

function TCustomAsmView.IgnoreSelectionWhenCopyAddress: Boolean;
begin
  Result := True;
end;

procedure TCustomAsmView.InitPainters;
begin
  inherited;
  PostPainters.Add(TExecutionPointPostPainter.Create(Self));
end;

function TCustomAsmView.IsJumpValid(AJmpToAddr: Int64): Boolean;
var
  AddrType: TAddrType;
begin
  DoQueryAddrType(AJmpToAddr, AddrType);
  Result := AddrType = atExecute;
end;

procedure TCustomAsmView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FLockKeyScroll := ssShift in Shift;
  case Key of
    VK_LEFT, VK_UP: FKeyScrollDirection := ssdLineUp;
    VK_DOWN, VK_RIGHT: FKeyScrollDirection := ssdLineDown;
    VK_PRIOR: FKeyScrollDirection := ssdPageUp;
    VK_NEXT: FKeyScrollDirection := ssdPageDown;
    VK_HOME, VK_END: Exit;
  end;
  inherited;
  FKeyScrollDirection := ssdNone;
  FLockKeyScroll := False;
end;

function TCustomAsmView.SelectedInstructionAddr: Int64;
var
  RowIndex: Int64;
begin
  Result := 0;
  RowIndex := SelectedRowIndex;
  if RowIndex >= 0 then
    Result := RawData[RowIndex].Address;
end;

procedure TCustomAsmView.SetColorMap(const Value: TAsmColorMap);
begin
  ColorMap.Assign(Value);
end;

procedure TCustomAsmView.SetCurrentIPIsActiveJmp(const Value: Boolean);
begin
  if FCurrentIPIsActiveJmp <> Value then
  begin
    FCurrentIPIsActiveJmp := Value;
    Invalidate;
  end;
end;

procedure TCustomAsmView.SetHighlightReg(const Value: string);
begin
  if FHighlightReg <> Value then
  begin
    FHighlightReg := Value;
    Invalidate;
  end;
end;

procedure TCustomAsmView.SetInstructionPoint(const Value: Int64);
begin
  if InstructionPoint <> Value then
  begin
    FInstructionPoint := Value;
    Invalidate;
  end;
end;

procedure TCustomAsmView.UpdateScrollY(AOffset: Int64);
begin
  {$ifdef debug_unlock_scroll}
  inherited;
  {$endif}
end;

procedure TCustomAsmView.UpdateVerticalScrollPos;
var
  ScrollInfo: TScrollInfo;
begin
  {$ifdef debug_unlock_scroll}
  inherited; Exit;
  {$endif}
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nPos := MaxInt div 2;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.nMax := MaxInt;
  ShowScrollBar(Handle, SB_VERT, True);
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

procedure TCustomAsmView.WMVScroll(var Msg: TWMVScroll);
begin
  {$ifdef debug_unlock_scroll}
  inherited; Exit;
  {$endif}
  case Msg.ScrollCode of
    SB_LINEUP: DoScrollStep(ssdLineUp);
    SB_LINEDOWN: DoScrollStep(ssdLineDown);
    SB_PAGEUP: DoScrollStep(ssdPageUp);
    SB_PAGEDOWN: DoScrollStep(ssdPageDown);
  end;
end;

{ TAddressViewColorMap }

procedure TAddressViewColorMap.SetDuplicateColor(const Value: TColor);
begin
  if DuplicateColor <> Value then
  begin
    FDuplicateColor := Value;
    DoChange;
  end;
end;

procedure TAddressViewColorMap.SetStringColor(AValue: TColor);
begin
  if FStringColor <> AValue then
  begin
    FStringColor := AValue;
    DoChange;
  end;
end;

procedure TAddressViewColorMap.SetExecuteColor(AValue: TColor);
begin
  if FExecuteColor <> AValue then
  begin
    FExecuteColor := AValue;
    DoChange;
  end;
end;

procedure TAddressViewColorMap.SetReadColor(AValue: TColor);
begin
  if FReadColor <> AValue then
  begin
    FReadColor := AValue;
    DoChange;
  end;
end;

procedure TAddressViewColorMap.SetStackColor(AValue: TColor);
begin
  if FStackColor <> AValue then
  begin
    FStackColor := AValue;
    DoChange;
  end;
end;

procedure TAddressViewColorMap.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TAddressViewColorMap then
  begin
    TAddressViewColorMap(Dest).FExecuteColor := FExecuteColor;
    TAddressViewColorMap(Dest).FReadColor := FReadColor;
    TAddressViewColorMap(Dest).FStackColor := FStackColor;
    TAddressViewColorMap(Dest).FStringColor := FStringColor;
  end;
end;

procedure TAddressViewColorMap.InitLightMode;
begin
  inherited;
  FDuplicateColor := $82D5FF;
  FExecuteColor := $CC33FF;
  FReadColor := $CC66;
  FStackColor := $2197FF;
  FStringColor := $FF6380;
end;

procedure TAddressViewColorMap.InitDarkMode;
begin
  inherited;
  FDuplicateColor := $005B69;
  FExecuteColor := $CC33FF;
  FReadColor := $CC66;
  FStackColor := $2197FF;
  FStringColor := $C20041;
end;

{ TCustomAddressView }

function TCustomAddressView.GetColorMap: TAddressViewColorMap;
begin
  Result := TAddressViewColorMap(inherited ColorMap);
end;

function TCustomAddressView.GetValidateType(Index: TAddrValidationType
  ): Boolean;
begin
  Result := FValidateType[Index];
end;

procedure TCustomAddressView.DoChange(ChangeCode: Integer);
begin
  inherited;
  if ChangeCode = cmFont then
    FitColumnsToBestSize;
end;

procedure TCustomAddressView.DoQueryAddrType(AddrVA: Int64;
  out AddrType: TAddrType);
begin
  AddrType := atNone;
  if Assigned(FOnQueryAddr) then
    FOnQueryAddr(Self, AddrVA, AddrType);
end;

procedure TCustomAddressView.DoSelectionChage(AStartAddr, AEndAddr: Int64);
begin
  inherited;
  if HightLightSelected then
    UpdateSelectionList;
end;

procedure TCustomAddressView.InitDefault;
begin
  inherited;
  FHightLightSelected := True;
  FValidateAddress := True;
  FValidateType[avtExecutable] := True;
  FValidateType[avtReadable] := True;
  FValidateType[avtStack] := True;
  FValidateType[avtString] := True;
end;

procedure TCustomAddressView.InitPainters;
begin
  DefaultPainter := GetDefaultPainterClass.Create(Self);
  Painters.Add(DefaultPainter);
end;

function TCustomAddressView.GetColorMapClass: THexViewColorMapClass;
begin
  Result := TAddressViewColorMap;
end;

procedure TCustomAddressView.RestoreViewParam;
begin
  FitColumnsToBestSize;
end;

procedure TCustomAddressView.SetColorMap(AValue: TAddressViewColorMap);
begin
  ColorMap.Assign(AValue);
end;

procedure TCustomAddressView.SetHightLightSelected(const Value: Boolean);
begin
  if HightLightSelected <> Value then
  begin
    FHightLightSelected := Value;
    UpdateSelectionList;
  end;
end;

procedure TCustomAddressView.SetValidateAddress(AValue: Boolean);
begin
  if ValidateAddress <> AValue then
  begin
    FValidateAddress := AValue;
    Invalidate;
  end;
end;

procedure TCustomAddressView.SetValidateType(Index: TAddrValidationType;
  AValue: Boolean);
begin
  FValidateType[Index] := AValue;
end;

procedure TCustomAddressView.UpdateDataMap;
begin
  inherited;
  if HightLightSelected then
    UpdateSelectionList;
end;

procedure TCustomAddressView.UpdateSelectionList;
var
  SelBuf, CheckBuff, DisplayBuff: array of Byte;
  ASelStart, ASelEnd, ASelLength, ADisplayStart, ADisplayLength: Int64;
  I: Integer;
  CheckWindowAddrVA: Int64;
begin
  Selections.DropSelectionsAtTag(1);
  if not HightLightSelected then Exit;
  ASelStart := Min(SelStart, SelEnd);
  ASelEnd := Max(SelStart, SelEnd);
  ASelLength := ASelEnd - ASelStart + 1;
  SetLength(SelBuf{%H-}, ASelLength);
  ASelLength := ReadDataAtSelStart(SelBuf[0], ASelLength);
  if ASelLength = 0 then Exit;
  SetLength(CheckBuff{%H-}, ASelLength);
  if CompareMem(@CheckBuff[0], @SelBuf[0], ASelLength) then Exit;
  SetLength(SelBuf, ASelLength);
  SetLength(DisplayBuff{%H-}, (VisibleRowCount + 1) * BytesInRow);
  if DataStream = nil then Exit;
  ADisplayStart := RowToAddress(CurrentVisibleRow, 0);
  DataStream.Position := ADisplayStart - StartAddress;
  ADisplayLength := DataStream.Read(DisplayBuff[0], Length(DisplayBuff));
  I := 0;
  while I < ADisplayLength - ASelLength do
  begin
    CheckWindowAddrVA := ADisplayStart + I;
    if (CheckWindowAddrVA >= ASelStart) and (CheckWindowAddrVA <= ASelEnd) then
    begin
      Inc(I);
      Continue;
    end;
    Inc(CheckWindowAddrVA, ASelLength - 1);
    if (CheckWindowAddrVA >= ASelStart) and (CheckWindowAddrVA <= ASelEnd) then
    begin
      Inc(I);
      Continue;
    end;
    if CompareMem(@DisplayBuff[I], @SelBuf[0], ASelLength) then
    begin
      Selections.Add(1, ADisplayStart + I, ADisplayStart + I + ASelLength - 1, ColorMap.DuplicateColor);
      Inc(I, ASelLength);
      Continue;
    end;
    if ByteViewMode = bvmAddress then
      Inc(I, IfThen(AddressMode = am32bit, 4, 8))
    else
      Inc(I, DefValueMetric(ByteViewMode).ByteCount);
  end;
end;

procedure TCustomAddressView.UpdateVerticalScrollPos;
begin
  inherited;
  if HightLightSelected then
    UpdateSelectionList;
end;

constructor TCustomAddressView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
end;

function TCustomAddressView.CanDrawValidation(AAddrType: TAddrType): Boolean;
begin
  case AAddrType of
    atExecute: Result := ValidateType[avtExecutable];
    atRead, atReadLinked: Result := ValidateType[avtReadable];
    atStack: Result := ValidateType[avtStack];
    atString: Result := ValidateType[avtString];
  else
    Result := False;
  end;
end;

{ TAddressViewPainter }

procedure TAddressViewPainter.CheckCache;
begin
  if RowIndex = FCacheIndex then Exit;
  GetRawBuff(RowIndex, FCacheData);
  FCacheIndex := RowIndex;
  FCacheAddrIndex := -1;
end;

function TAddressViewPainter.GetAddrAtIndex(AIndex: Integer): Int64;
begin
  CheckCache;
  if FCacheData = nil then Exit(0);
  if AddressMode = am32bit then
    Result := PInteger(@FCacheData[AIndex shl 2])^
  else
    Result := PInt64(@FCacheData[AIndex shl 3])^;
end;

function TAddressViewPainter.GetAddressAtCursor(
  const AMouseHitInfo: TMouseHitInfo; var AddrIndex: Integer): Int64;
var
  I, L: Integer;
  ABounds: TBoundaries;
begin
  Result := 0;
  AddrIndex := -1;
  if AMouseHitInfo.SelectPoint.Column <> ctOpcode then Exit;
  CheckCache;
  for I := 0 to BytesInRow div IfThen(AddressMode = am32bit, 4, 8) - 1 do
  begin
    ABounds := GetBounds(I);
    L := AMouseHitInfo.ColumnStart + TextMargin + ABounds.LeftOffset;
    if L > AMouseHitInfo.ScrolledCursorPos.X then Exit;
    if L + ABounds.Width > AMouseHitInfo.ScrolledCursorPos.X then
    begin
      AddrIndex := I;
      Exit(GetAddrAtIndex(AddrIndex));
    end;
  end;
end;

function TAddressViewPainter.GetBounds(AIndex: Integer): TBoundaries;
var
  AByteCount, ASelStart, ASelEnd: Integer;
begin
  AByteCount := IfThen(AddressMode = am32bit, 8, 16);
  ASelStart := AByteCount * AIndex;
  ASelEnd := AByteCount * (AIndex + 1) - 2;
  Result.LeftOffset := TextMetric.CharLength(ctOpcode, 0, ASelStart) - CharWidth;
  Result.Width := TextMetric.CharLength(ctOpcode, ASelStart, ASelEnd) + CharWidth;
end;

function TAddressViewPainter.GetSelBounds: TBoundaries;
var
  ASelData: TSelectData;
  ASelStart, ASelEnd: Integer;
begin
  ASelData := GetSelectData(RowIndex);
  case ASelData.SelectStyle of
    ssAllSelected:
    begin
      ASelStart := 0;
      ASelEnd := RawData[RowIndex].RawLength - 1;
    end;
    ssLeftSelected:
    begin
      ASelStart := 0;
      ASelEnd := ASelData.FirstSelectIndex;
    end;
    ssCenterSelected:
    begin
      ASelStart := ASelData.FirstSelectIndex;
      ASelEnd := ASelData.SecondSelectIndex;
    end;
    ssRightSelected:
    begin
      ASelStart := ASelData.FirstSelectIndex;
      ASelEnd := RawData[RowIndex].RawLength - 1;
    end;
  else
    Result := Default(TBoundaries);
    Exit;
  end;
  ASelStart := ASelStart shl 1;
  ASelEnd := ASelEnd shl 1;
  Result.LeftOffset := TextMetric.CharLength(ctOpcode, 0, ASelStart) - CharWidth;
  Result.Width := TextMetric.CharLength(ctOpcode, ASelStart, ASelEnd) + CharWidth;
end;

function TAddressViewPainter.QueryAddrType(AIndex: Integer): TAddrType;
begin
  if FCacheAddrIndex <> AIndex then
  begin
    FCacheAddrIndex := AIndex;
    FCacheAddrType := atNone;
    View.DoQueryAddrType(GetAddrAtIndex(AIndex), FCacheAddrType);
  end;
  Result := FCacheAddrType
end;

function TAddressViewPainter.View: TCustomAddressView;
begin
  Result := TCustomAddressView(Owner);
end;

{ TDumpPainter }

function TDumpPainter.ColumnsDrawSupport: TFWHexViewColumnTypes;
begin
  Result := [ctOpcode, ctDescription];
end;

procedure TDumpPainter.DrawHeaderColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
begin
  case AColumn of
    ctDescription:
      DefaultDrawHeaderColumn(ACanvas, ARect,
        GetHeaderColumnCaption(AColumn), 0);
  else
    inherited;
  end;
end;

procedure TDumpPainter.DrawHexPart(ACanvas: TCanvas; var ARect: TRect);
var
  AddrType: TAddrType;
  I: Integer;
  ABounds: TBoundaries;
begin
  inherited;
  if not View.ValidateAddress then Exit;
  if ByteViewMode in [bvmFloat32..bvmText] then Exit;
  CheckCache;
  for I := 0 to BytesInRow div IfThen(AddressMode = am32bit, 4, 8) - 1 do
  begin
    AddrType := QueryAddrType(I);
    if View.CanDrawValidation(AddrType) then
    begin
      case AddrType of
        atExecute: ACanvas.Brush.Color := View.ColorMap.AddrExecuteColor;
        atRead, atReadLinked: ACanvas.Brush.Color := View.ColorMap.AddrReadColor;
        atStack: ACanvas.Brush.Color := View.ColorMap.AddrStackColor;
        atString: ACanvas.Brush.Color := View.ColorMap.AddrStringColor;
      end;
      ABounds := GetBounds(I);
      if AddrType in [atRead, atString] then
      begin
        {$IFDEF LINUX}
        ACanvas.Pen.Color := ACanvas.Brush.Color;
        ACanvas.Pen.Width := 2;
        {$ELSE}
        ACanvas.Pen.Color := View.ColorMap.BackgroundColor;
        {$ENDIF}
        ACanvas.Pen.Style := psDot;
        ACanvas.MoveTo(ARect.Left + ABounds.LeftOffset, ARect.Bottom - 2);
        ACanvas.LineTo(ARect.Left + ABounds.LeftOffset + ABounds.Width, ARect.Bottom - 2);
        ACanvas.Pen.Style := psSolid;
      end
      else
        PatBlt(ACanvas, ARect.Left + ABounds.LeftOffset,
          ARect.Bottom - 2, ABounds.Width, 2, PATCOPY);
    end;
  end;
end;

function TDumpPainter.GetHeaderColumnCaption(AColumn: TColumnType): string;
begin
  if AColumn = ctDescription then
    Result := Encoder.DisplayName
  else
    Result := inherited;
end;

procedure TDumpPainter.GetHitInfo(var AHitInfo: TMouseHitInfo);
var
  I, L: Integer;
  ABounds: TBoundaries;
begin
  inherited;
  if not View.ValidateAddress then Exit;
  if AHitInfo.SelectPoint.Column <> ctOpcode then Exit;
  if not (ssCtrl in AHitInfo.Shift) then Exit;
  CheckCache;
  for I := 0 to BytesInRow div IfThen(AddressMode = am32bit, 4, 8) - 1 do
  begin
    ABounds := GetBounds(I);
    L := AHitInfo.ColumnStart + TextMargin + ABounds.LeftOffset;
    if L > AHitInfo.ScrolledCursorPos.X then Exit;
    if (L + ABounds.Width > AHitInfo.ScrolledCursorPos.X) and (QueryAddrType(I) <> atNone) then
      AHitInfo.Cursor := crHandPoint;
  end;
end;

{ TCustomDumpView }

procedure TCustomDumpView.DoGetHint(var AHintParam: THintParam;
  var AHint: string);

  function PostFix(AAddrType: TAddrType): string;
  begin
    case AAddrType of
      atExecute: Result := 'in Assembly.';
      atStack: Result := 'in the Stack.';
      atRead, atReadLinked, atString: Result := 'in the Dump.';
    else
      Result := '';
    end;
  end;

var
  Painter: TAbstractPrimaryRowPainter;
  AddrType: TAddrType;
  AddrIndex: Integer;
  ABounds: TBoundaries;
begin
  if not ValidateAddress then Exit;
  if ByteViewMode in [bvmFloat32..bvmText] then Exit;
  if AHintParam.MouseHitInfo.SelectPoint.Column <> ctOpcode then Exit;
  if PtInRect(FLastInvalidAddrRect, AHintParam.MouseHitInfo.CursorPos) then Exit;
  Painter := GetRowPainter(AHintParam.MouseHitInfo.SelectPoint.RowIndex);
  if Assigned(Painter) then
  begin
    if keSelection in AHintParam.MouseHitInfo.Elements then
    begin
      AHintParam.AddrVA := 0;
      ReadDataAtSelStart(AHintParam.AddrVA, SizeOf(AHintParam.AddrVA));
      ABounds := TAddressViewPainter(Painter).GetSelBounds;
      AHintParam.HintInfo.CursorRect.Left :=
        AHintParam.MouseHitInfo.ColumnStart + TextMargin + ABounds.LeftOffset;
      AHintParam.HintInfo.CursorRect.Width := ABounds.Width;
      if not PtInRect(AHintParam.HintInfo.CursorRect, AHintParam.MouseHitInfo.CursorPos) then Exit;
      FLastInvalidAddrRect := TRect.Empty;
      inherited;
      Exit;
    end;
    AHintParam.AddrVA := TAddressViewPainter(Painter).GetAddressAtCursor(
      AHintParam.MouseHitInfo, AddrIndex{%H-});
    if (AddrIndex >= 0) and (AHintParam.AddrVA <> 0) then
    begin
      ABounds := TAddressViewPainter(Painter).GetBounds(AddrIndex);
      AHintParam.HintInfo.CursorRect.Left :=
        AHintParam.MouseHitInfo.ColumnStart + TextMargin + ABounds.LeftOffset;
      AHintParam.HintInfo.CursorRect.Width := ABounds.Width;
      DoQueryAddrType(AHintParam.AddrVA, AddrType{%H-});
      if AddrType = atNone then
      begin
        FLastInvalidAddrRect := AHintParam.HintInfo.CursorRect;
        Exit;
      end;
      FLastInvalidAddrRect := TRect.Empty;
      inherited;
      AHint := AHint + sLineBreak +
        'Press Ctrl+Click to jump to an address ' + PostFix(AddrType);
    end;
  end;
end;

function TCustomDumpView.DoLButtonDown(const AHitInfo: TMouseHitInfo): Boolean;
var
  Painter: TAbstractPrimaryRowPainter;
  AddrVA: Int64;
  AddrIndex: Integer;
begin
  Result := False;
  if not ValidateAddress then Exit;
  if not (ssCtrl in AHitInfo.Shift) then Exit;
  if AHitInfo.SelectPoint.Column <> ctOpcode then Exit;
  if AHitInfo.Cursor <> crHandPoint then Exit;
  Painter := GetRowPainter(AHitInfo.SelectPoint.RowIndex);
  if Assigned(Painter) then
  begin
    AddrVA := TDumpPainter(Painter).GetAddressAtCursor(AHitInfo, AddrIndex{%H-});
    DoJmpTo(AddrVA, jsQueryJump, Result);
  end;
end;

function TCustomDumpView.GetDefaultPainterClass: TPrimaryRowPainterClass;
begin
  Result := TDumpPainter;
end;

procedure TCustomDumpView.InitDefault;
begin
  inherited;
  ByteViewMode := bvmHex8;
  Encoder.EncodeType := cetDefault;
  Header.Columns := [ctAddress, ctOpcode, ctDescription];
  BytesInGroup := 4;
end;

procedure TCustomDumpView.UpdateView;
begin
  case ByteViewMode of
    bvmInt8, bvmUInt8: BytesInRow := 8;
    bvmFloat80: BytesInRow := 20;
    bvmText: BytesInRow := 64;
    bvmAddress:
      if AddressMode = am32bit then
        BytesInRow := 4
      else
        BytesInRow := 8;
  else
    BytesInRow := 16;
  end;
  case ByteViewMode of
    bvmText:
    begin
      Header.Columns := [ctAddress, ctOpcode];
      SeparateGroupByColor := False;
    end;
    bvmAddress:
    begin
      Header.Columns := [ctAddress, ctOpcode, ctDescription, ctComment];
      SeparateGroupByColor := False;
    end
  else
    SeparateGroupByColor := True;
    Header.Columns := [ctAddress, ctOpcode, ctDescription];
  end;

  inherited;

  FitColumnsToBestSize;
end;

{ TStackColorMap }

procedure TStackColorMap.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TStackColorMap then
  begin
    TStackColorMap(Dest).FAddrPCColor := FAddrPCColor;
    TStackColorMap(Dest).FAddrPCFontColor := FAddrPCFontColor;
    TStackColorMap(Dest).FEmptyStackColor := FEmptyStackColor;
    TStackColorMap(Dest).FFrameColor := FFrameColor;
    TStackColorMap(Dest).FFrameActiveColor := FFrameActiveColor;
    TStackColorMap(Dest).FStackPointFontColor := FStackPointFontColor;
    TStackColorMap(Dest).FStackPointColor := FStackPointColor;
  end;
end;

procedure TStackColorMap.InitDarkMode;
begin
  inherited;
  FAddrPCColor := $009BF18D;
  FAddrPCFontColor := clBlack;
  FEmptyStackColor := $999999;
  FFrameColor := $FF94A0;
  FFrameActiveColor := $7884F8;
  FStackPointColor := $009D116C;
  FStackPointFontColor := clWhite;
end;

procedure TStackColorMap.InitLightMode;
begin
  inherited;
  FAddrPCColor := $009BF18D;
  FAddrPCFontColor := clBlack;
  FEmptyStackColor := clGrayText;
  FFrameColor := clBlue;
  FFrameActiveColor := clRed;
  FStackPointColor := $009D116C;
  FStackPointFontColor := clWhite;
end;

procedure TStackColorMap.SetAddrPCColor(const Value: TColor);
begin
  if FAddrPCColor <> Value then
  begin
    FAddrPCColor := Value;
    DoChange;
  end;
end;

procedure TStackColorMap.SetAddrPCFontColor(const Value: TColor);
begin
  if FAddrPCFontColor <> Value then
  begin
    FAddrPCFontColor := Value;
    DoChange;
  end;
end;

procedure TStackColorMap.SetEmptyStackColor(const Value: TColor);
begin
  if FEmptyStackColor <> Value then
  begin
    FEmptyStackColor := Value;
    DoChange;
  end;
end;

procedure TStackColorMap.SetFrameActiveColor(const Value: TColor);
begin
  if FFrameActiveColor <> Value then
  begin
    FFrameActiveColor := Value;
    DoChange;
  end;
end;

procedure TStackColorMap.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    DoChange;
  end;
end;

procedure TStackColorMap.SetStackPointColor(const Value: TColor);
begin
  if FStackPointColor <> Value then
  begin
    FStackPointColor := Value;
    DoChange;
  end;
end;

procedure TStackColorMap.SetStackPointFontColor(const Value: TColor);
begin
  if FStackPointFontColor <> Value then
  begin
    FStackPointFontColor := Value;
    DoChange;
  end;
end;

{ TStackPostPainter }

procedure TStackPostPainter.PostPaint(ACanvas: TCanvas; StartRow,
  EndRow: Int64; var Offset: TPoint);
var
  I: Integer;
  AFrame: TStackFrame;
  Param: TDrawLineParam;
begin
  PaintedLinesCount := 1;
  Param.DrawArrow := False;
  Param.DrawAlwais := True;
  Param.DrawOnlySelectedArrow := False;
  Param.SecondDraw := False;
  Param.LineIndent := StackView.ToDpi(2);
  Param.LineVerticalMargin := 0;
  Param.LineWidth := 3;
  Param.Offset := Offset;
  Inc(Param.Offset.X, StackView.TextMargin - 2);
  for I := 0 to StackView.Frames.Count - 1 do
  begin
    AFrame := StackView.Frames[I];
    Param.RowFrom := AddressToRowIndex(AFrame.AddrStack);
    Param.RowTo := AddressToRowIndex(AFrame.AddrFrame);

    if I = 0 then
    begin
      // если фрейм еще не сформирован, пропускаем отрисовку нижней части.

      // if the frame is not formed yet, skip drawing of the bottom part.
      if Param.RowTo <= Param.RowFrom then
        Continue;
      Param.LineColor := TStackColorMap(StackView.ColorMap).FrameActiveColor
    end
    else
      Param.LineColor := TStackColorMap(StackView.ColorMap).FrameColor;

    DrawLine(ACanvas, Param);
  end;
end;

function TStackPostPainter.StackView: TCustomStackView;
begin
  Result := TCustomStackView(Owner);
end;

{ TStackRowPainter }

procedure TStackRowPainter.CorrectCanvasFont(ACanvas: TCanvas;
  AColumn: TColumnType);
var
  TopOfStackIndex: Int64;
  AddrPcVA: Int64;
begin
  if StackView.IsAddrPCRow(RowIndex, AddrPcVA) and (AColumn = ctAddress) then
  begin
    ACanvas.Font.Color := StackView.ColorMap.AddrPCFontColor;
    ACanvas.Brush.Style := bsClear;
  end
  else
    if not GetTopOfStackRowIndex(TopOfStackIndex) or (TopOfStackIndex > RowIndex) then
      ACanvas.Font.Color := StackView.ColorMap.EmptyStackColor;
  if (RowIndex = TopOfStackIndex) and (AColumn = ctAddress) then
  begin
    ACanvas.Font.Color := StackView.ColorMap.StackPointFontColor;
    ACanvas.Brush.Style := bsClear;
  end;
  inherited;
end;

procedure TStackRowPainter.DrawAddress(ACanvas: TCanvas; var ARect: TRect);
var
  TopOfStackIndex: Int64;
  BackR: TRect;
  AddrPcVA: Int64;
begin
  if StackView.IsAddrPCRow(RowIndex, AddrPcVA) then
  begin
    ACanvas.Brush.Color := StackView.ColorMap.AddrPCColor;
    BackR := ARect;
    InflateRect(BackR, TextMargin - ToDpi(2), 0);
    ACanvas.FillRect(BackR);
  end
  else
    if GetTopOfStackRowIndex(TopOfStackIndex) and (TopOfStackIndex = RowIndex) then
    begin
      ACanvas.Brush.Color := StackView.ColorMap.StackPointColor;
      BackR := ARect;
      InflateRect(BackR, TextMargin - ToDpi(2), 0);
      ACanvas.FillRect(BackR);
    end;
  inherited;
end;

procedure TStackRowPainter.DrawHexPart(ACanvas: TCanvas; var ARect: TRect);
var
  AddrType: TAddrType;
  ValidateRect: TRect;
  ABounds: TBoundaries;
  Offsets: Integer;
begin
  inherited;
  if not View.ValidateAddress then Exit;
  CheckCache;
  AddrType := QueryAddrType(0);
  if View.CanDrawValidation(AddrType) then
  begin
    case AddrType of
      atExecute: ACanvas.Brush.Color := View.ColorMap.AddrExecuteColor;
      atRead, atReadLinked: ACanvas.Brush.Color := View.ColorMap.AddrReadColor;
      atStack: ACanvas.Brush.Color := View.ColorMap.AddrStackColor;
      atString: ACanvas.Brush.Color := View.ColorMap.AddrStringColor;
    end;
    ValidateRect := ARect;
    ABounds := GetBounds(0);
    ValidateRect.Left := ARect.Left + ABounds.LeftOffset + ABounds.Width;
    ValidateRect.Width := ValidateRect.Height;
    ValidateRect.Right := Min(ValidateRect.Right, ARect.Right + TextMargin);
    Offsets := -Ceil(ARect.Height / 5);
    InflateRect(ValidateRect, Offsets, Offsets);
    if ValidateRect.IsEmpty then Exit;
    ACanvas.Pen.Color := ColorMap.TextColor;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.RoundRect(ValidateRect, 2, 2);
  end;
end;

function TStackRowPainter.GetTopOfStackRowIndex(out AIndex: Int64): Boolean;
begin
  Result := StackView.Frames.Count > 0;
  if Result then
    AIndex := AddressToRowIndex(StackView.Frames[0].AddrStack)
  else
    AIndex := -1;
end;

function TStackRowPainter.StackView: TCustomStackView;
begin
  Result := TCustomStackView(Owner);
end;

{ TCustomStackView }

function TCustomStackView.ByteViewModeCommandEnabled(
  Value: TByteViewMode; var AChecked: Boolean): Boolean;
begin
  Result := False;
end;

function TCustomStackView.CalculateColumnBestSize(Value: TColumnType): Integer;
begin
  case Value of
    ctOpcode:
    begin
      Result := inherited;
      if ValidateAddress then
        Inc(Result, RowHeight shr 1);
    end;
    ctComment: Result := ToDpi(350);
  else
    Result := inherited;
  end;
end;

function TCustomStackView.CopyCommandEnabled(Value: TCopyStyle): Boolean;
begin
  Result := Focused and (Value in [csAsText, csAddress, csBytes]);
end;

procedure TCustomStackView.CopySelected(CopyStyle: TCopyStyle);
var
  AValue: Int64;
begin
  case CopyStyle of
    csAsText, csAddress: inherited;
    csBytes:
    begin
      AValue := 0;
      ReadDataAtSelStart(AValue, IfThen(AddressMode = am32bit, 4, 8));
      Clipboard.AsText := IntToHex(AValue, 1);
    end;
  end;
end;

constructor TCustomStackView.Create(AOwner: TComponent);
begin
  inherited;
  FFrames := TListEx<TStackFrame>.Create;
  FAddrPCDict := TDictionary<Int64, Int64>.Create;
  UpdateFrameDescriptions;
end;

destructor TCustomStackView.Destroy;
begin
  FFrames.Free;
  FAddrPCDict.Free;
  inherited;
end;

procedure TCustomStackView.DoChange(ChangeCode: Integer);
begin
  inherited;
  if ChangeCode = cmAddressMode then
    UpdateFrameDescriptions;
end;

procedure TCustomStackView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  HitInfo: TMouseHitInfo;
begin
  HitInfo := GetHitInfo(MousePos.X, MousePos.Y, SavedShift);
  if (HitInfo.SelectPoint.ValueOffset >= 0) and
    not (keSelection in HitInfo.Elements) then
    UpdateSelection(HitInfo.SelectPoint, HitInfo.SelectPoint);
  inherited;
end;

procedure TCustomStackView.DoGetHint(var AHintParam: THintParam;
  var AHint: string);
var
  Painter: TAbstractPrimaryRowPainter;
  ABounds: TBoundaries;
begin
  if PtInRect(FLastInvalidAddrRect, AHintParam.MouseHitInfo.CursorPos) then Exit;
  if AHintParam.MouseHitInfo.SelectPoint.Column <> ctOpcode then Exit;
  Painter := GetRowPainter(AHintParam.MouseHitInfo.SelectPoint.RowIndex);
  if Assigned(Painter) then
  begin
    ABounds := TAddressViewPainter(Painter).GetBounds(0);
    AHintParam.HintInfo.CursorRect.Left :=
      AHintParam.MouseHitInfo.ColumnStart + TextMargin + ABounds.LeftOffset;
    AHintParam.HintInfo.CursorRect.Width := ABounds.Width + RowHeight;
    if not PtInRect(AHintParam.HintInfo.CursorRect, AHintParam.MouseHitInfo.CursorPos) then Exit;
    AHintParam.AddrVA := TAddressViewPainter(Painter).GetAddrAtIndex(0);
    if AHintParam.AddrVA <> 0 then
    begin
      FLastInvalidAddrRect := TRect.Empty;
      inherited;
    end
    else
      FLastInvalidAddrRect := AHintParam.HintInfo.CursorRect;
  end;
end;

function TCustomStackView.GetDefaultCaretChangeMode: TCaretChangeMode;
begin
  Result := ccmSelectRow;
end;

procedure TCustomStackView.FramesUpdated;
var
  I: Integer;
  AddrPC: Int64;
begin
  FAddrPCDict.Clear;
  for I := 0 to Frames.Count - 1 do
  begin
    AddrPC := Frames.List[I].AddrPC;
    FAddrPCDict.AddOrSetValue(RawData.AddressToRowIndex(AddrPC), AddrPC);
  end;
  Invalidate;
end;

function TCustomStackView.GetColorMap: TStackColorMap;
begin
  Result := TStackColorMap(inherited ColorMap);
end;

function TCustomStackView.GetColorMapClass: THexViewColorMapClass;
begin
  Result := TStackColorMap;
end;

function TCustomStackView.GetDefaultPainterClass: TPrimaryRowPainterClass;
begin
  Result := TStackRowPainter;
end;

procedure TCustomStackView.InitDefault;
begin
  inherited;
  ByteViewMode := bvmAddress;
  Header.Columns := [ctWorkSpace, ctAddress, ctOpcode, ctComment];
  Header.ColumnMinWidth[ctWorkSpace] := 42;
  Header.Visible := False;
  SeparateGroupByColor := False;
  // for automatic recalculation of column widths when font changes are made
  Header.ColumnCaption[ctWorkSpace] := 'REG';
end;

procedure TCustomStackView.InitPainters;
begin
  inherited;
  PostPainters.Add(TStackPostPainter.Create(Self));
end;

function TCustomStackView.IsAddrPCRow(AIndex: Int64; out AddrVA: Int64): Boolean;
begin
  Result := FAddrPCDict.TryGetValue(AIndex, AddrVA);
end;

procedure TCustomStackView.SetColorMap(const Value: TStackColorMap);
begin
  ColorMap.Assign(Value);
end;

procedure TCustomStackView.UpdateFrameDescriptions;
begin
  case AddressMode of
    am32bit: BytesInRow := 4;
    am64bit: BytesInRow := 8;
  else
    raise Exception.Create('Unsupported AddressMode');
  end;
end;

{ TRegisterTextMetrics }

procedure TRegisterTextMetrics.UpdateCharPosition(BytesInRow,
  CharWidth: Integer);
begin
  UpdateCharPositionText(BytesInRow, CharWidth);
end;

function TRegisterTextMetrics.ValueMetric: TValueMetric;
begin
  Result.CharCount := 1;
  Result.ByteCount := 1;
end;

{ TRegisterPainter }

function TRegisterPainter.AcceptSelection: Boolean;
begin
  Result := not View.Context.EmptyRow(RowIndex);
end;

function TRegisterPainter.CaretEditMode(AColumn: TColumnType): TCaretEditMode;
begin
  Result := cemDisabled;
end;

procedure TRegisterPainter.CopyRowAsString(Builder: TSimplyStringBuilder);

  function AlignLine(const Value: string; Len: Integer): string;
  var
    CurLen: Integer;
  begin
    if Len = 0 then Exit('');
    CurLen := Length(Value);
    Result := Value;
    if Len < CurLen then
      SetLength(Result, Len)
    else
      Result := Result + StringOfChar(' ', Len - CurLen);
  end;

var
  RowStr: string;
  I: Integer;
  Info: TRegister;
begin
  RowStr := '';
  // селекшен не учитывается - строка копируется целиком!
  // selections are not taken into account - the string is copied as a whole!
  if not FContext.EmptyRow(RowIndex) then
  begin
    for I := 0 to FContext.RegCount(RowIndex) - 1 do
    begin
      Info := FContext.RegInfo(RowIndex, I);
      RowStr := RowStr + AlignLine(FContext.RegQueryString(Info.RegID, rqstDisplayName), Info.RegNameSize);
      RowStr := RowStr + AlignLine(FContext.RegQueryString(Info.RegID, rqstValue), Info.ValueSize);
      RowStr := RowStr + AlignLine('', Info.ValueSeparatorSize);
    end;
  end;
  Builder.Append(RowStr + sLineBreak);
end;

procedure TRegisterPainter.DrawColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
var
  DrawR: TRect;
  I, Offsets: Integer;
  Info: TRegister;
  Selected: Boolean;
  ASelData: TSelectData;
  AParam: TRegParam;
  ARegValue: TRegValue;
  AddrType: TAddrType;
  ValidateRect: TRect;
begin
  if AColumn = ctNone then
  begin
    FContext := View.Context;
    Exit;
  end;

  DrawR := ARect;
  for I := 0 to FContext.RegCount(RowIndex) - 1 do
  begin
    Info := FContext.RegInfo(RowIndex, I);

    // Reg Name
    if Info.ValueType = crtSelectableHint then
      ACanvas.Font.Color := View.ColorMap.HintColor
    else
      ACanvas.Font.Color := View.ColorMap.RegColor;
    ACanvas.Brush.Style := bsClear;
    DrawTextBlock(ACanvas, AColumn, DrawR, FContext.RegQueryString(Info.RegID, rqstDisplayName),
      TextMetric.CharPointer(AColumn, 0));
    Inc(DrawR.Left, TextMetric.CharLength(AColumn, 1, Info.RegNameSize));

    // Value selection
    case SelData.SelectStyle of
      ssAllSelected: Selected := True;
      ssLeftSelected: Selected := I <= SelData.FirstSelectIndex;
      ssCenterSelected: Selected :=
        (I >= SelData.FirstSelectIndex) and (I <= SelData.FirstSelectIndex);
      ssRightSelected: Selected := I >= SelData.FirstSelectIndex;
    else
      Selected := False;
    end;
    if Selected then
    begin
      ASelData.SelectStyle := ssCenterSelected;
      ASelData.FirstSelectIndex := 0;
      ASelData.SecondSelectIndex := Info.ValueSize - 1;
      DrawSelectedBackround(ACanvas, AColumn, DrawR, ASelData);
    end;

    // Value data
    if Info.ValueType <> crtSelectableHint then
    begin
      if Info.Modifyed then
        ACanvas.Font.Color := View.ColorMap.ValueModifiedColor
      else
        ACanvas.Font.Color := View.ColorMap.ValueColor;
    end;
    ACanvas.Brush.Style := bsClear;
    DrawAlignedTextPart(ACanvas, AColumn, FContext.RegQueryString(Info.RegID, rqstValue), DrawR);
    Inc(DrawR.Left, TextMetric.CharLength(AColumn, 1,
      Info.ValueSize + Info.ValueSeparatorSize));

    // Value addr validation mark
    if not View.ValidateAddress then Continue;
    if Info.ValueType = crtValue then
    begin
      if not FContext.RegParam(Info.RegID, AParam) then Continue;
      if rfValidation in AParam.Flags then
      begin
        FContext.RegQueryValue(Info.RegID, ARegValue);
        View.DoQueryAddrType(ARegValue.QwordValue, AddrType);
        if not View.CanDrawValidation(AddrType) then Continue;
        case AddrType of
          atExecute: ACanvas.Brush.Color := View.ColorMap.AddrExecuteColor;
          atRead, atReadLinked: ACanvas.Brush.Color := View.ColorMap.AddrReadColor;
          atStack: ACanvas.Brush.Color := View.ColorMap.AddrStackColor;
          atString: ACanvas.Brush.Color := View.ColorMap.AddrStringColor;
        end;
        ValidateRect := DrawR;
        ValidateRect.Width := ValidateRect.Height;
        Offsets := -Ceil(ValidateRect.Height / 5);
        InflateRect(ValidateRect, Offsets, Offsets);
        ValidateRect.Right := Min(ValidateRect.Right, ARect.Right + TextMargin shl 1);
        if ValidateRect.IsEmpty then Exit;
        OffsetRect(ValidateRect, -1 -
          TextMetric.CharLength(AColumn, 1, Info.ValueSeparatorSize), 0);
        ACanvas.Pen.Color := ColorMap.TextColor;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.RoundRect(ValidateRect, 2, 2);
      end;
    end;
  end;
end;

function TRegisterPainter.GetBounds(const AHitInfo: TMouseHitInfo;
  out ABounds: TBoundaries): Integer;

  function GlyphLen(Index: Integer): Integer;
  begin
    Result := TextMetric.SelectionLength(AHitInfo.SelectPoint.Column, 0, Index - 1);
  end;

var
  I, RegCount, LeftOffset, FieldLength: Integer;
  Info: TRegister;
begin
  Result := -1;
  ABounds := Default(TBoundaries);
  if FContext = nil then Exit;
  LeftOffset := AHitInfo.ColumnStart;
  Inc(LeftOffset, TextMargin);
  RegCount := FContext.RegCount(AHitInfo.SelectPoint.RowIndex);
  for I := 0 to RegCount - 1 do
  begin
    Info := FContext.RegInfo(AHitInfo.SelectPoint.RowIndex, I);
    FieldLength := GlyphLen(Info.RegNameSize + Info.ValueSize);
    if (AHitInfo.ScrolledCursorPos.X >= LeftOffset) and
      (AHitInfo.ScrolledCursorPos.X < LeftOffset + FieldLength) then
    begin
      ABounds.LeftOffset := LeftOffset;
      ABounds.Width := FieldLength;
      Result := I;
      Break;
    end;
    Inc(LeftOffset, FieldLength + GlyphLen(Info.ValueSeparatorSize));
  end;
end;

procedure TRegisterPainter.GetHitInfo(var AHitInfo: TMouseHitInfo);
var
  ABounds: TBoundaries;
begin
  // контекста может еще не быть, а вот GetHitInfo уже может прийти!
  // The context may not be there yet, but GetHitInfo may already be called!
  if FContext = nil then Exit;
  if AHitInfo.SelectPoint.Column = ctNone then Exit;
  AHitInfo.SelectPoint.ValueOffset := GetBounds(AHitInfo, ABounds);
  AHitInfo.SelectPoint.CharIndex := AHitInfo.SelectPoint.ValueOffset;
  if FContext.EmptyRow(AHitInfo.SelectPoint.RowIndex) or
    (AHitInfo.SelectPoint.ValueOffset < 0) then
    AHitInfo.SelectPoint.Column := ctNone
  else
    AHitInfo.Cursor := crHandPoint;
end;

function TRegisterPainter.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TRegisterTextMetrics;
end;

function TRegisterPainter.View: TCustomRegView;
begin
  Result := TCustomRegView(Owner);
end;

{ TRegistersRawData }

function TRegistersRawData.AddressToRowIndex(Value: Int64): Int64;
var
  Descriptor: TRegDescriptor;
begin
  if FContext.RegDescriptor(Integer(Value), Descriptor) then
    Result := Descriptor.RowIndex
  else
    Result := 0;
end;

function TRegistersRawData.Count: Int64;
begin
  if FContext = nil then
    Result := 0
  else
    Result := FContext.Count;
end;

function TRegistersRawData.View: TCustomRegView;
begin
  Result := TCustomRegView(inherited Owner);
end;

function TRegistersRawData.RawLength: Int64;
begin
  if FContext = nil then
    Result := 0
  else
    Result := FContext.RegCount(RowIndex);
end;

function TRegistersRawData.RowToAddress(ARowIndex: Int64;
  ValueOffset: Integer): Int64;
begin
  if FContext.EmptyRow(ARowIndex) then
    Exit(0);
  if ValueOffset < 0 then
    Exit(0);
  Result := FContext.RegInfo(ARowIndex, ValueOffset).RegID;
end;

procedure TRegistersRawData.Update;
begin
  FContext := View.Context;
end;

{ TRegistersColorMap }

procedure TRegistersColorMap.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TRegistersColorMap then
  begin
    TRegistersColorMap(Dest).FHintColor := FHintColor;
    TRegistersColorMap(Dest).FRegColor := FRegColor;
    TRegistersColorMap(Dest).FValueColor := FValueColor;
    TRegistersColorMap(Dest).FValueModifiedColor := FValueModifiedColor;
  end;
end;

procedure TRegistersColorMap.InitDarkMode;
begin
  inherited;
  FHintColor := $999999;
  FRegColor := $F694B7;
  FValueColor := $E0E0E0;
  FValueModifiedColor := $7884F8;
end;

procedure TRegistersColorMap.InitLightMode;
begin
  inherited;
  FHintColor := clMaroon;
  FRegColor := clGrayText;
  FValueColor := clBlack;
  FValueModifiedColor := clRed;
end;

procedure TRegistersColorMap.SetHintColor(const Value: TColor);
begin
  if FHintColor <> Value then
  begin
    FHintColor := Value;
    DoChange;
  end;
end;

procedure TRegistersColorMap.SetRegColor(const Value: TColor);
begin
  if FRegColor <> Value then
  begin
    FRegColor := Value;
    DoChange;
  end;
end;

procedure TRegistersColorMap.SetValueColor(const Value: TColor);
begin
  if FValueColor <> Value then
  begin
    FValueColor := Value;
    DoChange;
  end;
end;

procedure TRegistersColorMap.SetValueModifiedColor(const Value: TColor);
begin
  if FValueModifiedColor <> Value then
  begin
    FValueModifiedColor := Value;
    DoChange;
  end;
end;

{ TCustomRegView }

function TCustomRegView.ByteViewModeCommandEnabled(
  Value: TByteViewMode; var AChecked: Boolean): Boolean;
begin
  Result := False;
end;

function TCustomRegView.CalculateColumnBestSize(Value: TColumnType): Integer;
var
  I, A, RowLen, MaxLen: Integer;
  R: TRegister;
begin
  if Context = nil then Exit(ClientWidth);
  MaxLen := 0;
  for I := 0 to Context.Count - 1 do
  begin
    if Context.EmptyRow(I) then Continue;
    RowLen := 0;
    for A := 0 to Context.RegCount(I) - 1 do
    begin
      R := Context.RegInfo(I, A);
      Inc(RowLen, R.RegNameSize + R.ValueSize + R.ValueSeparatorSize);
    end;
    MaxLen := Max(MaxLen, RowLen);
  end;
  MaxLen := MaxLen * CharWidth;
  Inc(MaxLen, TextMargin shl 1);
  Result := MaxLen;
end;

procedure TCustomRegView.ContextUpdate(Sender: TObject;
  AChangeType: TContextChangeType);
begin
  if AChangeType = cctRemaped then
  begin
    ClearSelection;
    RawData.Update;
    UpdateTextBoundary;
    FitColumnsToBestSize;
    UpdateScrollPos;
  end
  else
  begin
    DoSelectionChage(SelStart, SelEnd);
    Invalidate;
  end;
end;

function TCustomRegView.ContextViewModeCommandEnabled(
  Value: TRegViewMode; var AChecked: Boolean): Boolean;
var
  RegID: Integer;
begin
  Result := True;
  RegID := Context.RegInfo(FActiveRegParam.RowIndex, FActiveRegParam.ColIndex).RegID;
  AChecked := Value = Context.ViewMode[RegID];
end;

procedure TCustomRegView.ContextViewModeCommandExecute(Value: TRegViewMode);
var
  RegID: Integer;
begin
  RegID := Context.RegInfo(FActiveRegParam.RowIndex, FActiveRegParam.ColIndex).RegID;
  Context.ViewMode[RegID] := Value;
end;

function TCustomRegView.ContextViewModeCommandHandled(
  Value: TRegViewMode): Boolean;
begin
  Result := FActiveRegParam.Valid and (Value in FActiveRegParam.SupportedViewMode);
end;

function TCustomRegView.CopyCommandEnabled(Value: TCopyStyle): Boolean;
begin
  case Value of
    csAsText: Result := True;
    csBytes: Result := (SelStart >= 0) and (SelStart = SelEnd) and
      (FSelectedRegister.ValueType in [crtValue, crtExtra]);
  else
    Result := False;
  end;
  Result := Focused and Result;
end;

constructor TCustomRegView.Create(AOwner: TComponent);
begin
  inherited;
  FActiveRegParam.Reset;
end;

procedure TCustomRegView.CopySelected(CopyStyle: TCopyStyle);
var
  AParam: TRegParam;
  NeedCopyAsRawBuff: Boolean;
begin
  case CopyStyle of
    csAsText, csAddress: inherited;
    csBytes:
    begin
      if not (FSelectedRegister.ValueType in [crtValue, crtExtra]) then Exit;
      if not Context.RegParam(FSelectedRegister.RegID, AParam) then Exit;
      NeedCopyAsRawBuff := (FSelectedRegValue.ValueSize >= 8) and
        ((AParam.SupportedViewMode = vmX87Reg64) or
        (AParam.SupportedViewMode = vmX87Reg80) or
        (AParam.SupportedViewMode = vmSimdReg));
      if NeedCopyAsRawBuff then
        Clipboard.AsText := RawBufToHex(@FSelectedRegValue.Ext32[0], FSelectedRegValue.ValueSize, True)
      else
        Clipboard.AsText := IntToHex(FSelectedRegValue.QwordValue, 1);
    end;
  end;
end;

procedure TCustomRegView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  HitInfo: TMouseHitInfo;
  RegID: Integer;
begin
  if Context = nil then Exit;
  HitInfo := GetHitInfo(MousePos.X, MousePos.Y, SavedShift);
  if (HitInfo.SelectPoint.ValueOffset >= 0) and
    not (keSelection in HitInfo.Elements) then
    UpdateSelection(HitInfo.SelectPoint, HitInfo.SelectPoint);
  RegID := Context.RegInfo(HitInfo.SelectPoint.RowIndex,
    HitInfo.SelectPoint.ValueOffset).RegID;
  Context.RegParam(RegID, FActiveRegParam);
  if Assigned(FPopup) then
    FPopup(Self, MousePos, HitInfo.SelectPoint.RowIndex,
      HitInfo.SelectPoint.ValueOffset, Handled);
  inherited;
end;

procedure TCustomRegView.DoGetHint(var AHintParam: THintParam;
  var AHint: string);
var
  RegID: Integer;
  RegParam: TRegParam;
  RegValue: TRegValue;
  Painter: TAbstractPrimaryRowPainter;
  ABounds: TBoundaries;
begin
  RegID := Context.RegInfo(AHintParam.MouseHitInfo.SelectPoint.RowIndex,
    AHintParam.MouseHitInfo.SelectPoint.ValueOffset).RegID;
  Context.RegParam(RegID, RegParam);
  if HintForFlag and (rfHint in RegParam.Flags) then
    AHint := Context.RegQueryString(RegID, rqstHint);
  if HintForReg and (rfValidation in RegParam.Flags) then
  begin
    Context.RegQueryValue(RegID, RegValue);
    AHintParam.AddrVA := RegValue.QwordValue;
    inherited;
  end;
  if AHint = '' then Exit;
  Painter := GetRowPainter(AHintParam.MouseHitInfo.SelectPoint.RowIndex);
  if Assigned(Painter) then
  begin
    if TRegisterPainter(Painter).GetBounds(AHintParam.MouseHitInfo, ABounds) < 0 then Exit;
    AHintParam.HintInfo.CursorRect.Left := ABounds.LeftOffset;
    AHintParam.HintInfo.CursorRect.Width := ABounds.Width;
  end;
end;

procedure TCustomRegView.DoSelectionChage(AStartAddr, AEndAddr: Int64);
var
  RegID: Integer;
begin
  FillChar(FSelectedRegister, SizeOf(FSelectedRegister), 0);
  FSelectedRegister.RegID := -1;
  RegID := Min(AStartAddr, AEndAddr);
  if RegID >= 0 then
  begin
    // проверка типа поля для подсветки
    // работать можно только с реальными регистрами, и их флагами
    // field type check, for selection
    // you can work only with real registers and their flags
    FSelectedRegister := Context.RegInfo(RegID);
    if not (FSelectedRegister.ValueType in [crtValue..crtEnumValue]) then
      RegID := -1;
  end;
  if RegID >= 0 then
  begin
    FSelectedRegName := Context.RegQueryString(RegID, rqstName);
    if not Context.RegQueryValue(RegID, FSelectedRegValue) then
      FSelectedRegValue := Default(TRegValue);
  end
  else
  begin
    FSelectedRegValue := Default(TRegValue);
    FSelectedRegName := '';
  end;
  inherited;
end;

function TCustomRegView.GetCaretChangeMode(
  APainter: TAbstractPrimaryRowPainter; AColumn: TColumnType;
  Shift: TShiftState): TCaretChangeMode;
begin
  if ssShift in Shift then
    Result := ccmContinueSelection
  else
    Result := ccmSetNewSelection;
end;

function TCustomRegView.GetColorMap: TRegistersColorMap;
begin
  Result := TRegistersColorMap(inherited ColorMap);
end;

function TCustomRegView.GetColorMapClass: THexViewColorMapClass;
begin
  Result := TRegistersColorMap;
end;

function TCustomRegView.GetDataStreamSize: Int64;
begin
  Result := RawData.Count;
end;

function TCustomRegView.GetDefaultPainterClass: TPrimaryRowPainterClass;
begin
  Result := TRegisterPainter;
end;

function TCustomRegView.GetRawDataClass: TRawDataClass;
begin
  Result := TRegistersRawData;
end;

procedure TCustomRegView.InitDefault;
begin
  inherited;
  BytesInRow := 200;
  Header.Columns := [ctOpcode];
  Header.Visible := False;
  HightLightSelected := False;
  SeparateGroupByColor := False;
  HintForReg := True;
  HintForFlag := True;
  if Assigned(Context) then
    Context.InitDefault;
end;

function TCustomRegView.InternalGetRowPainter(
  ARowIndex: Int64): TAbstractPrimaryRowPainter;
begin
  if ARowIndex < 0 then
    Result := nil
  else
    Result := DefaultPainter;
end;

procedure TCustomRegView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Context) then
    Context := nil;
end;

function TCustomRegView.RawData: TRegistersRawData;
begin
  Result := TRegistersRawData(inherited RawData);
end;

function TCustomRegView.ReadDataAtSelStart(var pBuffer;
  nSize: Integer): Integer;
begin
  if SelectedRegName = '' then
    Result := 0
  else
  begin
    Result := Min(nSize, FSelectedRegValue.ValueSize);
    Move(FSelectedRegValue.ByteValue, pBuffer, Result);
  end;
end;

procedure TCustomRegView.SetColorMap(const Value: TRegistersColorMap);
begin
  ColorMap.Assign(Value);
end;

procedure TCustomRegView.SetContext(const Value: TAbstractCPUContext);
begin
  if Context <> Value then
  begin
    if Assigned(FContext) then
    begin
      FContext.UnRegisterChangeNotification(ContextUpdate);
      FContext.RemoveFreeNotification(Self);
    end;
    FContext := Value;
    if Assigned(FContext) then
    begin
      FContext.RegisterChangeNotification(ContextUpdate);
      FContext.OnQueryRegHint := OnQueryComment;
      FContext.OnQueryExternalHint := OnQueryExternalHint;
      FContext.FreeNotification(Self);
    end;
    ContextUpdate(Value, cctRemaped);
  end;
end;

{ TEditTextMetric }

procedure TEditTextMetric.UpdateCharPosition(BytesInRow, CharWidth: Integer);
begin
  if FByteViewMode = bvmHex8 then
    UpdateCharPositionText(BytesInRow, CharWidth)
  else
    UpdateCharPositionDef(BytesInRow, CharWidth);
end;

function TEditTextMetric.ValueMetric: TValueMetric;
begin
  Result := DefValueMetric(FByteViewMode);
  case FByteViewMode of
    bvmHex16, bvmInt16, bvmUInt16: Result.CharCount := 8;
    bvmHex32, bvmInt32, bvmUInt32: Result.CharCount := 17;
    bvmHex64, bvmInt64, bvmUInt64: Result.CharCount := 35;
    bvmFloat32: Result.CharCount := 17;
    bvmFloat64: Result.CharCount := 35;
    bvmFloat80: Result.CharCount := 20;
  end;
end;

{ TEditViewPainter }

function TEditViewPainter.ByteViewMode: TByteViewMode;
begin
  case View.RawData.EditMode of
    evmMM, evmXMM:
    begin
      case RowIndex of
        2:
        begin
          case View.RawData.PartViewType of
            pvtHex: Result := bvmHex16;
            pvtSigned: Result := bvmInt16;
          else
            Result := bvmUInt16;
          end;
        end;
        3:
        begin
          case View.RawData.PartViewType of
            pvtHex: Result := bvmHex32;
            pvtSigned: Result := bvmInt32;
          else
            Result := bvmUInt32;
          end;
        end;
        4: Result := bvmFloat32;
        5: Result := bvmFloat64;
        6:
        begin
          case View.RawData.PartViewType of
            pvtHex: Result := bvmHex64;
            pvtSigned: Result := bvmInt64;
          else
            Result := bvmUInt64;
          end;
        end;
      else
        Result := bvmHex8;
      end;
    end;
    evmX87:
    begin
      if RowIndex = 0 then
        Result := bvmHex8
      else
        Result := bvmFloat80;
    end;
    evmYMM:
    begin
      case RowIndex of
        3, 11:
        begin
          case View.RawData.PartViewType of
            pvtHex: Result := bvmHex16;
            pvtSigned: Result := bvmInt16;
          else
            Result := bvmUInt16;
          end;
        end;
        4, 12:
        begin
          case View.RawData.PartViewType of
            pvtHex: Result := bvmHex32;
            pvtSigned: Result := bvmInt32;
          else
            Result := bvmUInt32;
          end;
        end;
        5, 13: Result := bvmFloat32;
        6, 14: Result := bvmFloat64;
        7, 15:
        begin
          case View.RawData.PartViewType of
            pvtHex: Result := bvmHex64;
            pvtSigned: Result := bvmInt64;
          else
            Result := bvmUInt64;
          end;
        end;
      else
        Result := bvmHex8;
      end;
    end;
  else
    Result := inherited;
  end;
end;

function TEditViewPainter.CaretEditMode(AColumn: TColumnType): TCaretEditMode;
begin
  if ByteViewMode = bvmHex8 then
    Result := cemSingleChar
  else
    Result := cemValueEditor;
end;

function TEditViewPainter.ColumnAsString(AColumn: TColumnType): string;
const
  strRaw =    '   Raw:';
  strWord =   '  Word:';
  strDWord =  ' DWord:';
  strFloat =  'Single:';
  strDouble = 'Double:';
  strQWord =  ' QWord:';
  strExt =    ' Ext80:';
begin
  if AColumn = ctWorkSpace then
  begin
    Result := '';
    case View.RawData.EditMode of
      evmMM, evmXMM:
      begin
        case RowIndex of
          0: Result := strRaw;
          2: Result := strWord;
          3: Result := strDWord;
          4: Result := strFloat;
          5: Result := strDouble;
          6: Result := strQWord;
        end;
      end;
      evmX87:
      begin
        case RowIndex of
          0: Result := strRaw;
          1: Result := strExt;
        end;
      end;
      evmYMM:
      begin
        case RowIndex of
          1, 9: Result := strRaw;
          3, 11: Result := strWord;
          4, 12: Result := strDWord;
          5, 13: Result := strFloat;
          6, 14: Result := strDouble;
          7, 15: Result := strQWord;
        end;
      end;
    end;
  end
  else
    Result := inherited;
end;

function TEditViewPainter.DrawRowColumnBackground(ACanvas: TCanvas;
  AColumn: TColumnType; const ARect: TRect): Boolean;
const
  DefRowSet = [2, 4, 6, 9, 11, 13];
  YmmRowSet = [3, 5, 7, 11, 13, 15];
var
  R: TRect;
  RowSet: set of Byte;
begin
  Result := False;
  if AColumn <> ctOpcode then Exit;
  if View.RawData.EditMode = evmYMM then
    RowSet := YmmRowSet
  else
    RowSet := DefRowSet;
  if Byte(RowIndex) in RowSet then
  begin
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := ColorMap.GroupColor;
    R := ARect;
    R.Width := Min(ARect.Width, View.GetMetricAtByteView(
      bvmFloat32).SelectionLength(ctOpcode, 0, BytesInRow - 1));
    ACanvas.FillRect(R);
  end;
end;

function TEditViewPainter.FormatMode: TFormatMode;
const
  DefRowSet = [0];
  YmmRowSet = [1, 9];
var
  RowSet: set of Byte;
begin
  if View.RawData.EditMode = evmYMM then
    RowSet := YmmRowSet
  else
    RowSet := DefRowSet;
  if Byte(RowIndex) in RowSet then
    Result := RegFormatMode
  else
  begin
    Result := DefFormatMode;
    Result.AlignChar := ' ';
  end;
end;

function TEditViewPainter.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TEditTextMetric;
end;

function TEditViewPainter.TextMetric: TAbstractTextMetric;
begin
  Result := View.GetMetricAtByteView(ByteViewMode);
end;

function TEditViewPainter.View: TCustomEditView;
begin
  Result := TCustomEditView(Owner);
end;

{ TRegEditCommentPainter }

function TRegEditCommentPainter.ColumnAsString(AColumn: TColumnType): string;
const
  s07 =
    '     0-1      2-3      4-5      6-7';
  s0F =
    '     0-1      2-3      4-5      6-7       8-9      A-B      C-D      E-F';
  s1F =
    '   10-11    12-13    14-15    16-17     18-19    1A-1B    1C-1D    1E-1F';
  sLowPart =
    '                               LOW PART                                 ';
  sHiPart =
    '                              HIGH  PART                                ';
begin
  case AColumn of
    ctWorkSpace:
      if Byte(RowIndex) in [1, 2, 10] then
        Result := '  Byte:'
      else
        Result := '       ';
    ctComment:
    begin
      case View.RawData.EditMode of
        evmMM: Result := s07;
        evmXMM: Result := s0F;
        evmYMM:
        begin
          case RowIndex of
            0: Result := sHiPart;
            2: Result := s1F;
            8: Result := sLowPart;
            10: Result := s0F;
          end;
        end;
      end;
    end;
  end;
end;

procedure TRegEditCommentPainter.DrawColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
begin
  case AColumn of
    ctNone: FFullRowRect := ARect;
    ctWorkSpace: DrawWorkSpace(ACanvas, ARect);
    ctOpcode:
    begin
      DrawComment(ACanvas, ARect);
      if Byte(RowIndex) in [1, 2, 10] then
      begin
        ACanvas.Brush.Style := bsSolid;
        ACanvas.Brush.Color := clBlack;
        PatBlt(ACanvas, 0, FFullRowRect.Top, FFullRowRect.Right, 1, PATCOPY);
        PatBlt(ACanvas, 0, FFullRowRect.Bottom - 2, FFullRowRect.Right, 1, PATCOPY);
      end;
    end;
  end;
end;

function TRegEditCommentPainter.View: TCustomEditView;
begin
  Result := TCustomEditView(Owner);
end;

{ TEditViewRawData }

function TEditViewRawData.Address: Int64;
begin
  Result := 0;
  if (EditMode = evmYMM) and (RowIndex < 8 ) then
    Result := 16;
end;

function TEditViewRawData.AddressToRowIndex(Value: Int64): Int64;
begin
  if EditMode = evmYMM then
  begin
    if Value > 16 then
      Result := 1
    else
      Result := 9;
  end
  else
    Result := 0;
end;

function TEditViewRawData.Count: Int64;
begin
  Result := FRowCount;
end;

constructor TEditViewRawData.Create(AOwner: TFWCustomHexView);
begin
  inherited;
  View.OnEdit := OnEdit;
end;

function TEditViewRawData.GetRegBuff(RowIndex: Integer): TBytes;
begin
  Result := nil;
  SetLength(Result, View.BytesInRow);
  case EditMode of
    evmMM, evmX87, evmXMM:
      Move(FRegBuff[0], Result[0], View.BytesInRow);
    evmYMM:
    begin
      SetLength(Result, 16);
      if RowIndex in [0..7] then
        Move(FRegBuff[16], Result[0], 16)
      else
        Move(FRegBuff[0], Result[0], 16);
    end;
  end;
end;

procedure TEditViewRawData.InitRegBuff(const Buffer; ASize: Integer);
begin
  if ASize > 0 then
  begin
    SetLength(FRegBuff, ASize);
    Move(Buffer, FRegBuff[0], ASize);
    Update;
    Owner.Invalidate;
  end;
end;

procedure TEditViewRawData.GetBuff(var Buffer; ASize: Integer);
begin
  Move(FRegBuff[0], Buffer, ASize);
end;

procedure TEditViewRawData.OnEdit(Sender: TObject; const ACursor: TDrawParam;
  const AData: TEditParam; var Handled: Boolean);
begin
  if (AData.ValueSize > 0) and (AData.ValueSize + ACursor.AddrVA <= Length(FRegBuff)) then
  begin
    Move(AData.NewExtValue[0], FRegBuff[ACursor.AddrVA], AData.ValueSize);
    Owner.Invalidate;
    Handled := True;
  end;
end;

function TEditViewRawData.View: TCustomEditView;
begin
  Result := TCustomEditView(inherited Owner);
end;

function TEditViewRawData.RawLength: Int64;
begin
  Result := View.BytesInRow;
end;

procedure TEditViewRawData.SetEditMode(const Value: TEditViewMode);
begin
  if EditMode <> Value then
  begin
    FEditMode := Value;
    Update;
  end;
end;

procedure TEditViewRawData.SetPartViewType(const Value: TPartViewType);
begin
  if PartViewType <> Value then
  begin
    FPartViewType := Value;
    Owner.Invalidate;
  end;
end;

procedure TEditViewRawData.Update;
begin
  if RawLength = 0 then
  begin
    FRowCount := 0;
    Exit;
  end;
  case EditMode of
    evmMM: FRowCount := 7;
    evmX87: FRowCount := 2;
    evmXMM: FRowCount := 7;
    evmYMM: FRowCount := 16;
  end;
  case EditMode of
    evmMM: View.BytesInRow := 8;
    evmX87: View.BytesInRow := 10;
    evmXMM: View.BytesInRow := 16;
    evmYMM: View.BytesInRow := 16;
  end;
  if View.BytesInRow > RawLength then
    FRowCount := 0;
end;

{ TCustomEditView }

function TCustomEditView.CalculateColumnBestSize(Value: TColumnType): Integer;
begin
  case Value of
    ctWorkSpace: Result := MeasureCanvas.TextWidth('Double:');
    ctOpcode:
      if RawData.EditMode = evmX87 then
        Result :=
          GetMetricAtByteView(bvmFloat80).SelectionLength(Value, 0, BytesInRow - 1)
      else
        Result :=
          GetMetricAtByteView(bvmFloat32).SelectionLength(Value, 0, BytesInRow - 1);
  else
    Result := inherited;
  end;
  Inc(Result, TextMargin shl 1);
end;

function TCustomEditView.ColumnResetSelection(AColumn: TColumnType): Boolean;
begin
  Result := AColumn = ctNone;
end;

constructor TCustomEditView.Create(AOwner: TComponent);
var
  I: TByteViewMode;
  AMetric: TEditTextMetric;
begin
  inherited;
  FExtended := TDictionary<TByteViewMode, TAbstractTextMetric>.Create;
  for I in [bvmHex16..bvmHex64, bvmInt16..bvmInt64, bvmUInt16..bvmFloat80] do
  begin
    AMetric := TEditTextMetric.Create(Self);
    AMetric.OverloadByteViewMode := I;
    FExtended.Add(I, AMetric);
  end;
end;

destructor TCustomEditView.Destroy;
begin
  FExtended.Free;
  inherited;
end;

function TCustomEditView.GetDefaultPainterClass: TPrimaryRowPainterClass;
begin
  Result := TEditViewPainter;
end;

function TCustomEditView.GetMetricAtByteView(
  AByteViewMode: TByteViewMode): TAbstractTextMetric;
begin
  if AByteViewMode = bvmHex8 then
    Result := TextMetric
  else
    FExtended.TryGetValue(AByteViewMode, Result);
end;

procedure TCustomEditView.GetRawBuff(ARowIndex: Int64; var Data: TBytes);
begin
  Data := RawData.RegBuff[ARowIndex];
end;

function TCustomEditView.GetRawDataClass: TRawDataClass;
begin
  Result := TEditViewRawData;
end;

procedure TCustomEditView.InitDefault;
begin
  inherited;
  Header.Visible := False;
  Header.Columns := [ctWorkSpace, ctOpcode];
  SeparateGroupByColor := False;
  ReadOnly := False;
end;

procedure TCustomEditView.InitPainters;
begin
  inherited;
  Painters.Add(GetOverloadPainterClass(TRegEditCommentPainter).Create(Self));
end;

function TCustomEditView.InternalGetRowPainter(
  ARowIndex: Int64): TAbstractPrimaryRowPainter;
begin
  case RawData.EditMode of
    evmMM, evmXMM: if ARowIndex = 1 then Exit(Painters[1]);
    evmYMM: if Byte(ARowIndex) in [0, 2, 8, 10] then Exit(Painters[1]);
  end;
  Result := DefaultPainter;
end;

function TCustomEditView.RawData: TEditViewRawData;
begin
  Result := TEditViewRawData(inherited RawData);
end;

end.

