////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : FWHexView.CpuView.pas
//  * Purpose   : Набор классов для реализации режима CPU-View
//  * Author    : Александр (Rouse_) Багель
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 2.0.14
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//

unit CpuView.Viewers;

{$IFDEF FPC}
  {$MODE Delphi}
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
  Classes,
  Graphics,
  SysUtils,
  Controls,
  Math,
  Clipbrd,
  Generics.Collections,
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
    FRIPMarkColor: TColor;
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
    procedure SetRIPMarkColor(AValue: TColor);
    procedure SetSourceLineColor(AValue: TColor);
    procedure SetSizePfxColor(const Value: TColor);
  protected
    procedure InitLightMode; override;
    procedure InitDarkMode; override;
  published
    property ActiveJmpColor: TColor read FActiveJmpColor write SetActiveJmpColor;
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
    property RIPMarkColor: TColor read FRIPMarkColor write SetRIPMarkColor stored IsColorStored;
    property SizePfxColor: TColor read FSizePfxColor write SetSizePfxColor stored IsColorStored;
    property SourceLineColor: TColor read FSourceLineColor write SetSourceLineColor stored IsColorStored;
  end;

  TAsmTextMertics = class(TFixedHexByteTextMetric)
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
    procedure DoScrollStep(AStep: TScrollStepDirection);
    function GetColorMap: TAsmColorMap;
    procedure SetCurrentIPIsActiveJmp(const Value: Boolean);
    procedure SetInstructionPoint(const Value: Int64);
    procedure SetHighlightReg(const Value: string);
  protected
    function CalculateJmpToRow(JmpFromRow: Int64): Int64; override;
    function CopyCommandEnabled(Value: TCopyStyle): Boolean; override;
    procedure DoDrawToken(ACanvas: TCanvas; ATokenParam: TDrawParam;
      const ARect: TRect; AToken: PChar; var ATokenLen: Integer); override;
    function GetCaretPreviosRowIndex(FromIndex: Int64;
      AColumn: TColumnType = ctNone): Int64; override;
    function GetCaretNextRowIndex(FromIndex: Int64;
      AColumn: TColumnType = ctNone): Int64; override;
    function GetColorMapClass: THexViewColorMapClass; override;
    function GetOverloadPainterClass(Value: TPrimaryRowPainterClass): TPrimaryRowPainterClass; override;
    procedure InitPainters; override;
  protected
    // Lock Vertical Scroll
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure UpdateScrollY(AOffset: Int64); override;
    procedure UpdateVerticalScrollPos; override;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FitColumnToBestSize(Value: TColumnType); override;
    function SelectedInstructionAddr: Int64;
    property BreakPoints: TDictionary<Int64, Boolean> read FBreakPoints;
    property CurrentIPIsActiveJmp: Boolean read FCurrentIPIsActiveJmp write SetCurrentIPIsActiveJmp;
    property InstructionPoint: Int64 read FInstructionPoint write SetInstructionPoint;
    property HighlightReg: string read FHighlightReg write SetHighlightReg;
  protected
    property ColorMap: TAsmColorMap read GetColorMap;
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
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WheelMultiplyer;
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
    property OnVerticalScroll;
  end;

  { TFixedColumnView }

  TFixedColumnView = class(TFWCustomHexView)
  protected
    procedure DoChange(ChangeCode: Integer); override;
    procedure InitPainters; override;
    procedure RestoreViewParam; override;
  published
    property Font;
    property PopupMenu;
  end;

  TCustomDumpView = class;

  { TDumpPainter }

  TDumpPainter = class(TRowHexPainter)
  protected
    function ColumnsDrawSupport: TFWHexViewColumnTypes; override;
    function CpuView: TCustomDumpView;
    procedure DrawHeaderColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    function GetHeaderColumnCaption(AColumn: TColumnType): string; override;
  end;

  { TCustomDumpView }

  TCustomDumpView = class(TFixedColumnView)
  protected
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
    property SeparateGroupByColor;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WheelMultiplyer;
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

  { TStackColorMap }

  TStackColorMap = class(THexViewColorMap)
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

  TStackRowPainter = class(TRowHexPainter)
  strict private
    function StackView: TCustomStackView;
    function GetTopOfStackRowIndex(out AIndex: Int64): Boolean;
  protected
    procedure CorrectCanvasFont(ACanvas: TCanvas; AColumn: TColumnType); override;
    procedure DrawAddress(ACanvas: TCanvas; var ARect: TRect); override;
  end;

  { TCustomStackView }

  TCustomStackView = class(TFixedColumnView)
  strict private
    FFrames: TList<TStackFrame>;
    FAddrPCDict: TDictionary<Int64, UInt64>;
    FStartDescription, FEndDescription: string;
    function GetColorMap: TStackColorMap;
    procedure SetEndDescription(const Value: string);
    procedure SetStartDescription(const Value: string);
    procedure UpdateFrameDescriptions;
  protected
    function ByteViewModeCommandEnabled(Value: TByteViewMode; var AChecked: Boolean): Boolean; override;
    function CopyCommandEnabled(Value: TCopyStyle): Boolean; override;
    procedure DoChange(ChangeCode: Integer); override;
    function GetDefaultPainterClass: TPrimaryRowPainterClass; override;
    procedure InitDefault; override;
    procedure InitPainters; override;
    function GetColorMapClass: THexViewColorMapClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopySelected(CopyStyle: TCopyStyle); override;
    procedure FitColumnToBestSize(Value: TColumnType); override;
    procedure FramesUpdated;
    function IsAddrPCRow(AIndex: Int64; out AddrVA: UInt64): Boolean;
    property Frames: TList<TStackFrame> read FFrames;
  protected
    property ColorMap: TStackColorMap read GetColorMap;
    property EndDescription: string read FEndDescription write SetEndDescription;
    property StartDescription: string read FStartDescription write SetStartDescription;
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
    property NoDataText;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WheelMultiplyer;
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
  protected
    function AcceptEdit(AColumn: TColumnType): Boolean; override;
    function AcceptSelection: Boolean; override;
    procedure CopyRowAsString(Builder: TSimplyStringBuilder); override;
    procedure DrawColumn(ACanvas: TCanvas; AColumn: TColumnType;
      var ARect: TRect); override;
    procedure GetHitInfo(var AMouseHitInfo: TMouseHitInfo;
      XPos, YPos: Int64); override;
    function GetTextMetricClass: TAbstractTextMetricClass; override;
    function Owner: TCustomRegView;
  end;

  { TRegistersRawData }

  TRegistersRawData = class(TRawData)
  strict private
    FContext: TAbstractCPUContext;
  protected
    function Owner: TCustomRegView;
  public
    function AddressToRowIndex(Value: Int64): Int64; override;
    function Count: Int64; override;
    function RawLength: Int64; override;
    function RowToAddress(ARowIndex: Int64; ValueOffset: Integer): Int64; override;
    procedure Update; override;
  end;

  { TRegistersColorMap }

  TRegistersColorMap = class(THexViewColorMap)
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

  TCustomRegView = class(TFixedColumnView, ICpuContextViewModeAction)
  private
    FContext: TAbstractCPUContext;
    FActiveRegParam: TRegParam;
    FPopup: TOnSelectedContextPopupEvent;
    FSelectedRegName: string;
    FSelectedRegValue: UInt64;
    FSelectedRegister: TRegister;
    procedure ContextUpdate(Sender: TObject; AChangeType: TContextChangeType);
    function GetColorMap: TRegistersColorMap;
    procedure SetContext(const Value: TAbstractCPUContext);
  protected
    function ByteViewModeCommandEnabled(Value: TByteViewMode; var AChecked: Boolean): Boolean; override;
    function ContextViewModeCommandEnabled(Value: TRegViewMode; var AChecked: Boolean): Boolean;
    function ContextViewModeCommandHandled(Value: TRegViewMode): Boolean;
    procedure ContextViewModeCommandExecute(Value: TRegViewMode);
    function CopyCommandEnabled(Value: TCopyStyle): Boolean; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
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
    procedure UpdateCursor(const HitTest: TMouseHitInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure FitColumnToBestSize(Value: TColumnType); override;
    function RawData: TRegistersRawData;
    function ReadDataAtSelStart(var pBuffer; nSize: Integer): Integer; override;
    procedure RefreshSelected;
    property Context: TAbstractCPUContext read FContext write SetContext;
    property SelectedRegister: TRegister read FSelectedRegister;
    property SelectedRegName: string read FSelectedRegName;
  protected
    property ColorMap: TRegistersColorMap read GetColorMap;
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
    property Cursor default crIBeam;
    property Enabled;
    property Encoder;
    property Font;
    property HideSelection;
    property NoDataText;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WheelMultiplyer;
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
    property OnSelectedContextPopup;
    property OnSelectionChange;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TAsmColorMap }

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
  FRIPMarkColor := clWhite;
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
  FRIPMarkColor := clDkGray;
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

procedure TAsmColorMap.SetRIPMarkColor(AValue: TColor);
begin
  if RIPMarkColor <> AValue then
  begin
    FRIPMarkColor := AValue;
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

{ TAsmTextMertics }

procedure TAsmTextMertics.UpdateCharPosition(BytesInRow, CharWidth: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(FCharPositions[False]) - 1 do
    FCharPositions[False][I] := CharWidth;
  for I := 0 to Length(FCharPositions[True]) - 1 do
    FCharPositions[True][I] := CharWidth;
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
  Result := TAsmTextMertics;
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
  R: TRect;
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

  R.Left := SplitMargin + Offset.X;
  R.Top := GetRowOffset(RowIndex);
  R.Width := DblSize(TextMargin) + CharWidth * 3;
  R.Height := RowHeight;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := AsmView.ColorMap.RIPMarkColor;
  OffsetRect(R, 0, -1 + (R.Height - RowHeight) div 2);
  DrawText(ACanvas.Handle, PChar('IP'), -1, R, 0);
end;

{ TCustomAsmView }

function TCustomAsmView.CalculateJmpToRow(JmpFromRow: Int64): Int64;
begin
  Result := inherited;
  // TCustomAsmView работает с кэшем у которого первые и последние
  // несколько строк никогда не должны быть отображены и используются
  // как маркер для обновления данных. Поэтому если адрес прыжка
  // выходит за диапазон кэша, корректируем номер строчки на которую будет
  // ссылаться прыжок, используя для этого первую и последнюю строку.
  if Result < 0 then
  begin
    if RawData[JmpFromRow].JmpToAddr > RawData[JmpFromRow].Address then
      Result := RawData.Count - 1;
  end;
end;

function TCustomAsmView.CopyCommandEnabled(Value: TCopyStyle): Boolean;
begin
  Result := Value in [csAsText, csAddress];
end;

constructor TCustomAsmView.Create(AOwner: TComponent);
begin
  inherited;
  ScrollBars :=  TScrollStyle.ssBoth;
  Header.Columns := [ctWorkSpace..ctComment];
  Header.Visible := False;
  FTokenizer := TAsmTokenizer.Create;
  DrawIncomingJmp := True;
  FBreakPoints := TDictionary<Int64, Boolean>.Create;
end;

destructor TCustomAsmView.Destroy;
begin
  FTokenizer.Free;
  FBreakPoints.Free;
  inherited;
end;

procedure TCustomAsmView.FitColumnToBestSize(Value: TColumnType);
begin
  case Value of
    ctOpcode: Header.ColumnWidth[ctOpcode] := ToDpi(170);
    ctDescription: Header.ColumnWidth[ctDescription] := ToDpi(250);
  else
    inherited;
  end;
end;

procedure TCustomAsmView.DoDrawToken(ACanvas: TCanvas;
  ATokenParam: TDrawParam; const ARect: TRect; AToken: PChar;
  var ATokenLen: Integer);
var
  R: TRect;
begin
  if ATokenParam.Column <> ctDescription then Exit;
  if RowStyle(ATokenParam.RowIndex) <> rsAsm then Exit;
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
    DoFontResize(IfThen(WheelDelta > 0, 1, -1))
  else
    if WheelDelta > 0 then
      DoScrollStep(ssdWheelUp)
    else
      DoScrollStep(ssdWheelDown);
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

procedure TCustomAsmView.InitPainters;
begin
  inherited;
  PostPainters.Add(TExecutionPointPostPainter.Create(Self));
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

{ TFixedColumnView }

procedure TFixedColumnView.DoChange(ChangeCode: Integer);
begin
  inherited;
  if ChangeCode in [cmFont, cmData] then
    FitColumnsToBestSize;
end;

procedure TFixedColumnView.InitPainters;
begin
  DefaultPainter := GetDefaultPainterClass.Create(Self);
  Painters.Add(DefaultPainter);
end;

procedure TFixedColumnView.RestoreViewParam;
begin
  // колонки пересчитываются автоматически
end;

{ TDumpPainter }

function TDumpPainter.ColumnsDrawSupport: TFWHexViewColumnTypes;
begin
  Result := [ctOpcode, ctDescription];
end;

function TDumpPainter.CpuView: TCustomDumpView;
begin
  Result := TCustomDumpView(Owner);
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

function TDumpPainter.GetHeaderColumnCaption(AColumn: TColumnType): string;
begin
  if AColumn = ctDescription then
    Result := Encoder.DisplayName
  else
    Result := inherited;
end;

{ TCustomDumpView }

function TCustomDumpView.GetDefaultPainterClass: TPrimaryRowPainterClass;
begin
  Result := TDumpPainter;
end;

procedure TCustomDumpView.InitDefault;
begin
  inherited;
  ScrollBars := TScrollStyle.ssBoth;
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
  R: TRect;
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
      Param.LineColor := TStackColorMap(StackView.ColorMap).FrameActiveColor;
      if RowVisible(Param.RowFrom) then
      begin
        R := Bounds(SplitMargin, GetRowOffset(Param.RowFrom),
          ACanvas.TextWidth(StackView.StartDescription), RowHeight);
        DrawText(ACanvas.Handle, PChar(StackView.StartDescription), -1, R, 0);
      end;

      // если фрейм еще не сформирован, пропускаем отрисовку нижней части.
      if Param.RowTo <= Param.RowFrom then
        Continue;

      if RowVisible(Param.RowTo) then
      begin
        R := Bounds(SplitMargin, GetRowOffset(Param.RowTo),
          ACanvas.TextWidth(StackView.EndDescription), RowHeight);
        DrawText(ACanvas.Handle, PChar(StackView.EndDescription), -1, R, 0);
      end;
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
  AddrPcVA: UInt64;
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
  AddrPcVA: UInt64;
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

function TCustomStackView.CopyCommandEnabled(Value: TCopyStyle): Boolean;
begin
  Result := Value in [csAsText, csAddress, csBytes];
end;

procedure TCustomStackView.CopySelected(CopyStyle: TCopyStyle);
var
  AValue: UInt64;
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

procedure TCustomStackView.FitColumnToBestSize(Value: TColumnType);
begin
  case Value of
    ctComment: Header.ColumnWidth[Value] := ToDpi(350);
  else
    inherited;
  end;
end;

constructor TCustomStackView.Create(AOwner: TComponent);
begin
  inherited;
  ScrollBars := TScrollStyle.ssVertical;
  FFrames := TList<TStackFrame>.Create;
  FAddrPCDict := TDictionary<Int64, UInt64>.Create;
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

procedure TCustomStackView.FramesUpdated;
var
  I: Integer;
  AddrPC: UInt64;
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
  Header.Visible := False;
  SeparateGroupByColor := False;
  // для автоматического пересчета ширины колонки при изменении шрифта
  Header.ColumnCaption[ctWorkSpace] := 'REG';
end;

procedure TCustomStackView.InitPainters;
begin
  inherited;
  PostPainters.Add(TStackPostPainter.Create(Self));
end;

function TCustomStackView.IsAddrPCRow(AIndex: Int64; out AddrVA: UInt64): Boolean;
begin
  Result := FAddrPCDict.TryGetValue(AIndex, AddrVA);
end;

procedure TCustomStackView.SetEndDescription(const Value: string);
begin
  if EndDescription <> Value then
  begin
    FEndDescription := Value;
    Invalidate;
  end;
end;

procedure TCustomStackView.SetStartDescription(const Value: string);
begin
  if StartDescription <> Value then
  begin
    FStartDescription := Value;
    Invalidate;
  end;
end;

procedure TCustomStackView.UpdateFrameDescriptions;
begin
  case AddressMode of
    am32bit:
    begin
      StartDescription := 'ESP';
      EndDescription := 'EBP';
      BytesInRow := 4;
    end;
    am64bit:
    begin
      StartDescription := 'RSP';
      EndDescription := 'RBP';
      BytesInRow := 8;
    end;
  else
    raise Exception.Create('Unsupported AddressMode');
  end;
end;

{ TRegisterTextMetrics }

procedure TRegisterTextMetrics.UpdateCharPosition(BytesInRow,
  CharWidth: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(FCharPositions[False]) - 1 do
  begin
    FCharPositions[False][I] := CharWidth;
    FCharPositions[True][I] := CharWidth;
  end;
end;

function TRegisterTextMetrics.ValueMetric: TValueMetric;
begin
  Result.CharCount := 1;
  Result.ByteCount := 1;
end;

{ TRegisterPainter }

function TRegisterPainter.AcceptEdit(AColumn: TColumnType): Boolean;
begin
  Result := False;
end;

function TRegisterPainter.AcceptSelection: Boolean;
begin
  Result := not Owner.Context.EmptyRow(RowIndex);
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
  if not FContext.EmptyRow(RowIndex) then
  begin
    for I := 0 to FContext.RegCount(RowIndex) - 1 do
    begin
      Info := FContext.RegInfo(RowIndex, I);
      RowStr := RowStr + AlignLine(FContext.RegData(RowIndex, I, True), Info.RegNameSize);
      RowStr := RowStr + AlignLine(FContext.RegData(RowIndex, I, False), Info.ValueSize);
      RowStr := RowStr + AlignLine('', Info.ValueSeparatorSize);
    end;
  end;
  Builder.Append(RowStr + sLineBreak);
end;

procedure TRegisterPainter.DrawColumn(ACanvas: TCanvas;
  AColumn: TColumnType; var ARect: TRect);
var
  DrawR: TRect;
  I: Integer;
  Info: TRegister;
  Selected: Boolean;
  ASelData: TSelectData;
begin
  if AColumn = ctNone then
  begin
    FContext := Owner.Context;
    Exit;
  end;

  DrawR := ARect;
  for I := 0 to FContext.RegCount(RowIndex) - 1 do
  begin
    Info := FContext.RegInfo(RowIndex, I);

    // Reg Name
    if Info.ValueType = crtSelectableHint then
      ACanvas.Font.Color := TRegistersColorMap(Owner.ColorMap).HintColor
    else
      ACanvas.Font.Color := TRegistersColorMap(Owner.ColorMap).RegColor;
    ACanvas.Brush.Style := bsClear;
    DrawTextBlock(ACanvas, AColumn, DrawR, FContext.RegData(RowIndex, I, True),
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
        ACanvas.Font.Color := TRegistersColorMap(Owner.ColorMap).ValueModifiedColor
      else
        ACanvas.Font.Color := TRegistersColorMap(Owner.ColorMap).ValueColor;
    end;
    ACanvas.Brush.Style := bsClear;
    DrawTextBlock(ACanvas, AColumn, DrawR, FContext.RegData(RowIndex, I, False),
      TextMetric.CharPointer(AColumn, 0));
    Inc(DrawR.Left, TextMetric.CharLength(AColumn, 1,
      Info.ValueSize + Info.ValueSeparatorSize));
  end;
end;

procedure TRegisterPainter.GetHitInfo(var AMouseHitInfo: TMouseHitInfo;
  XPos, YPos: Int64);

  function GlyphLen(Index: Integer): Integer;
  begin
    Result := TextMetric.SelectionLength(AMouseHitInfo.SelectPoint.Column, 0, Index - 1);
  end;

var
  I, RegCount, LeftOffset, FieldLength: Integer;
  Info: TRegister;
begin
  // контекста может еще не быть, а вот GetHitInfo уже может прийти!
  if FContext = nil then Exit;
  LeftOffset := AMouseHitInfo.ColumnStart;
  Inc(LeftOffset, TextMargin);

  AMouseHitInfo.SelectPoint.ValueOffset := -1;
  RegCount := FContext.RegCount(AMouseHitInfo.SelectPoint.RowIndex);
  for I := 0 to RegCount - 1 do
  begin
    Info := FContext.RegInfo(AMouseHitInfo.SelectPoint.RowIndex, I);
    FieldLength := GlyphLen(Info.RegNameSize + Info.ValueSize);
    if (XPos >= LeftOffset) and (XPos < LeftOffset + FieldLength) then
    begin
      AMouseHitInfo.SelectPoint.ValueOffset := I;
      AMouseHitInfo.SelectPoint.CharIndex := I;
      Break;
    end;
    Inc(LeftOffset, FieldLength + GlyphLen(Info.ValueSeparatorSize));
  end;

  if FContext.EmptyRow(AMouseHitInfo.SelectPoint.RowIndex) or
    (AMouseHitInfo.SelectPoint.ValueOffset < 0) then
    AMouseHitInfo.SelectPoint.Column := ctNone;
end;

function TRegisterPainter.GetTextMetricClass: TAbstractTextMetricClass;
begin
  Result := TRegisterTextMetrics;
end;

function TRegisterPainter.Owner: TCustomRegView;
begin
  Result := TCustomRegView(inherited Owner);
end;

{ TRegistersRawData }

function TRegistersRawData.AddressToRowIndex(Value: Int64): Int64;
var
  ValueOffset: Integer;
  IntResult: Integer;
begin
  if FContext.RegQuery(Integer(Value), IntResult, ValueOffset) then
    Result := IntResult
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

function TRegistersRawData.Owner: TCustomRegView;
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
  FContext := Owner.Context;
end;

{ TRegistersColorMap }

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

procedure TCustomRegView.ContextUpdate(Sender: TObject;
  AChangeType: TContextChangeType);
begin
  if AChangeType = cctRemaped then
  begin
    ClearSelection;
    RawData.Update;
    UpdateTextBoundary;
    UpdateScrollPos;
    FitColumnsToBestSize;
  end
  else
    Invalidate;
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
    csBytes: Result := (SelStart > 0) and (SelStart = SelEnd);
  else
    Result := False;
  end;
end;

constructor TCustomRegView.Create(AOwner: TComponent);
begin
  inherited;
  ScrollBars := TScrollStyle.ssVertical;
  FActiveRegParam.Reset;
end;

procedure TCustomRegView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  HitInfo: TMouseHitInfo;
  RegID: Integer;
begin
  if Context = nil then Exit;
  HitInfo := GetHitInfo(MousePos.X, MousePos.Y);
  if (HitInfo.SelectPoint.ValueOffset >= 0) and
    not CheckSelected(HitInfo.SelectPoint) then
    UpdateSelection(HitInfo.SelectPoint, HitInfo.SelectPoint);
  RegID := Context.RegInfo(HitInfo.SelectPoint.RowIndex,
    HitInfo.SelectPoint.ValueOffset).RegID;
  Context.RegParam(RegID, FActiveRegParam);
  if Assigned(FPopup) then
    FPopup(Self, MousePos, HitInfo.SelectPoint.RowIndex,
      HitInfo.SelectPoint.ValueOffset, Handled);
  inherited;
end;

procedure TCustomRegView.DoSelectionChage(AStartAddr, AEndAddr: Int64);
var
  RegID, RowIndex, RegIndex: Integer;
begin
  FillChar(FSelectedRegister, SizeOf(FSelectedRegister), 0);
  FSelectedRegister.RegID := -1;
  RegID := Min(AStartAddr, AEndAddr);
  if RegID >= 0 then
  begin
    // проверка типа поля для подсветки
    // работать можно только с реальными регистрами, флаги и прочее игнорируются
    Context.RegQuery(RegID, RowIndex, RegIndex);
    FSelectedRegister := Context.RegInfo(RowIndex, RegIndex);
    if not (FSelectedRegister.ValueType in [crtValue..crtSetValue]) then
      RegID := -1;
  end;
  if RegID >= 0 then
  begin
    FSelectedRegName := Context.RegData(RowIndex, RegIndex, True);
    if not Context.RegQueryValue(RowIndex, RegIndex, FSelectedRegValue) then
      FSelectedRegValue := 0;
  end
  else
  begin
    FSelectedRegValue := 0;
    FSelectedRegName := '';
  end;
  inherited;
end;

procedure TCustomRegView.FitColumnToBestSize(Value: TColumnType);
var
  I, A, RowLen, MaxLen: Integer;
  R: TRegister;
begin
  if Context = nil then Exit;
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
  MaxLen := TextMetric.SelectionLength(Value, 0, MaxLen - 1);
  Inc(MaxLen, TextMargin shl 1);
  Header.ColumnWidth[Value] := MaxLen;
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
  SeparateGroupByColor := False;
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
    Result := Min(nSize, Context.PointerSize);
    Move(FSelectedRegValue, pBuffer, Result);
  end;
end;

procedure TCustomRegView.RefreshSelected;
begin
  DoSelectionChage(SelStart, SelEnd);
  Invalidate;
end;

procedure TCustomRegView.SetContext(const Value: TAbstractCPUContext);
begin
  if Context <> Value then
  begin
    if Assigned(FContext) then
      FContext.RemoveFreeNotification(Self);
    FContext := Value;
    if Assigned(FContext) then
    begin
      FContext.OnChange := ContextUpdate;
      FContext.OnQueryRegHint := OnQueryComment;
      FContext.FreeNotification(Self);
    end;
    ContextUpdate(Value, cctRemaped);
  end;
end;

procedure TCustomRegView.UpdateCursor(const HitTest: TMouseHitInfo);
begin
  if HitTest.OnHeader or HitTest.OnSplitter or
    (HitTest.SelectPoint.Column = ctNone) or
    (HitTest.SelectPoint.RowIndex < 0) then
    inherited
  else
    Cursor := crHandPoint;
end;

end.
