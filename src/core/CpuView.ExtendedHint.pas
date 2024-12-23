////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.ExtendedHint.pas
//  * Purpose   : Extended Hint Window for CPU-View
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

unit CpuView.ExtendedHint;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLType, LCLIntf, LCLProc,
  {$ELSE}
  Windows,
  {$ENDIF}
  Messages,
  Classes,
  SysUtils,
  Controls,
  Forms,
  Graphics,
  Generics.Collections,
  Math,

  FWHexView.AsmTokenizer,
  FWHexView.Common,
  CpuView.Common,
  CpuView.Viewers;

type

  TAccessToAddrType = function(AddrVA: Int64; AAccess: TRegionAccessSet): TAddrType of object;
  TFormatAccessString = function(const ARegionData: TRegionData): string of object;
  TQueryCacheItem = function(AddrVA: Int64; out AItem: TAddrCacheItem; InDeepCall: Boolean = False): Boolean of object;
  TQueryDisasmAtAddr = function(AddrVA: Int64; out AItem: TAddrCacheItem): Boolean of object;
  TQueryPointerValueAtAddr =  function(AddrVA: Int64; out AItem: TAddrCacheItem): Boolean of object;

  PExtendedHintData = ^TExtendedHintData;
  TExtendedHintData = record
    AddressType: TAddrType; // это лишнее
    AddrVA: Int64;
    BorderWidth: Integer;
    ChainList: array of Int64;
    CharWidth: Integer;
    ColorMap: TAsmColorMap;
    Colors: array [TAddrType] of TColor;
    Font: TFont;
    AccessToAddrType: TAccessToAddrType;
    FormatAccessString: TFormatAccessString;
    QueryCacheItem: TQueryCacheItem;
    QueryDisasmAtAddr: TQueryDisasmAtAddr;
    QueryPointerValueAtAddr: TQueryPointerValueAtAddr;
    RowHeight: Integer;
    Tokenizer: TAsmTokenizer;
  end;

  {$IFDEF FPC}
  TCustomData = Pointer;
  {$ENDIF}

  { TExtendedHintWindow }

  TExtendedHintWindow = class(THintWindow)
  private const
  {$IFDEF FPC}
    NcBorderWidth = 4;
  {$ELSE}
    NcBorderWidth = 2;
  {$ENDIF}
  private
    FItems: TList<TAddrCacheItem>;
    FAddrItem: TAddrCacheItem;
    FAddrStr: string;
    FAddrChain: TList<Int64>;
    FAsm, FHints: TStringList;
    FData: TExtendedHintData;
    FLastLine: string;
    FExtendedRect: TRect;
    FTextHeight, FMaxLine, FMaxHint: Integer;
    procedure CalcAddress(AddrVA: Int64);
    function CalculateAddrChain(MaxWidth: Integer): TRect;
    function CalculateExecute(MaxWidth: Integer): TRect;
    function CalculatePointerValue(MaxWidth: Integer): TRect;
    procedure DrawAddrChain;
    procedure DrawExecuteExtendedRect;
    procedure DrawReadMem;
    function GetAddrString(AddrVA: Int64): string;
  protected
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: TCustomData): TRect; override;
  end;

implementation

{ TExtendedHintWindow }

{ TExtendedHintWindow }

function TExtendedHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: TCustomData): TRect;
var
  I: Integer;
{$IFDEF FPC}
  Mon: TMonitor;
{$ENDIF}
begin
  if Screen.ActiveCustomForm <> nil then
  begin
    {$IFDEF FPC}
    Mon := Screen.MonitorFromPoint(Point(Left, Top));
    if Mon = nil then
      Mon := Screen.Monitors[0];
    if Assigned(Mon) then
      Canvas.Font.PixelsPerInch := Mon.PixelsPerInch;
    {$ENDIF}
    Canvas.Font := Screen.HintFont;
    {$IFNDEF FPC}
    Canvas.Font.Height := Muldiv(Canvas.Font.Height, Screen.ActiveCustomForm.CurrentPPI, Screen.PixelsPerInch);
    {$ENDIF}
  end;

  FData := PExtendedHintData(AData)^;
  FTextHeight := Canvas.TextHeight('J');

  CalcAddress(FData.AddrVA);
  for I := 0 to Length(FData.ChainList) - 1 do
    CalcAddress(FData.ChainList[I]);

  case FData.AddressType of
    atExecute: FData.QueryDisasmAtAddr(FData.AddrVA, FAddrItem);
    atRead: FData.QueryPointerValueAtAddr(FData.AddrVA, FAddrItem);
  else
    FData.QueryCacheItem(FData.AddrVA, FAddrItem);
  end;

  FHints.Text := AHint;
  if FHints.Count > 1 then
    FLastLine := FHints[FHints.Count - 1]
  else
    FLastLine := '';
  FAddrStr := Format('Addr: 0x%x (%s)',
    [FData.AddrVA, FData.FormatAccessString(FAddrItem.Region)]);

  if FData.AddressType = atExecute then
    Result := CalculateExecute(MaxWidth)
  else
    if FAddrItem.InDeepCount > 0 then
      Result := CalculateAddrChain(MaxWidth)
    else
      Result := CalculatePointerValue(MaxWidth);
end;

procedure TExtendedHintWindow.CalcAddress(AddrVA: Int64);
begin

end;

function TExtendedHintWindow.CalculateAddrChain(MaxWidth: Integer): TRect;
var
  TmpHint: string;
  I: Integer;
  AItem: TAddrCacheItem;
begin
  if FLastLine = '' then
    TmpHint := ' '
  else
    TmpHint := FLastLine;
  Result := inherited CalcHintRect(MaxWidth, ' ', nil); // for font setup
  FHints.Clear;
  FMaxLine := Canvas.TextWidth(GetAddrString(FData.AddrVA));
  if FLastLine <> '' then
  begin
    FMaxLine := Max(FMaxLine, Canvas.TextWidth(FLastLine));
    Inc(Result.Bottom, FTextHeight);
  end;
  for I := 0 to Length(FData.ChainList) - 1 do
    FMaxLine := Max(FMaxLine, Canvas.TextWidth(GetAddrString(FData.ChainList[I])));
  Inc(Result.Bottom, Length(FData.ChainList) * FTextHeight);
  Inc(Result.Right, FMaxLine + FTextHeight);
end;

function TExtendedHintWindow.CalculateExecute(MaxWidth: Integer): TRect;
const
  CorrectRectWidth = NcBorderWidth {$IFNDEF FPC} shl 1{$ENDIF};
var
  TmpHint: string;
  I, TopCount: Integer;
begin
  // basic calculate
  TmpHint := FAddrStr;
  TopCount := 1;
  if FAddrItem.Symbol <> '' then
  begin
    TmpHint := TmpHint + sLineBreak + FAddrItem.Symbol;
    Inc(TopCount);
  end;
  if FLastLine <> '' then
    TmpHint := TmpHint + sLineBreak + FLastLine;

  Result := inherited CalcHintRect(MaxWidth, TmpHint, nil); // for font setup

  if not FAddrItem.ExtendedDataPresent then
  begin
    FExtendedRect := Bounds(0, NcBorderWidth + TopCount * FData.RowHeight, 0, 0);
    Exit;
  end;

  // ExtendedRect calculate
  FAsm.Text := FAddrItem.AsmLines;
  FHints.Text := FAddrItem.HintLines;
  FMaxLine := 0;
  FMaxHint := 0;
  for I := 0 to FAsm.Count - 1 do
  begin
    FMaxLine := Max(FMaxLine, Length(FAsm[I]));
    FMaxHint := Max(FMaxHint, Length(FHints[I]));
  end;

  FExtendedRect := Bounds(
    NcBorderWidth,
    NcBorderWidth shl 1 + TopCount * FData.RowHeight,
    FData.BorderWidth shl 1 + (FMaxLine + FMaxHint) * FData.CharWidth,
    FData.BorderWidth shl 1 + FAsm.Count * FData.RowHeight);

  if FMaxHint > 0 then
    Inc(FExtendedRect.Right, FData.CharWidth);

  Result := Rect(0, 0,
    Max(Result.Width, FExtendedRect.Right + CorrectRectWidth),
    FExtendedRect.Bottom {$IFDEF FPC} + NcBorderWidth {$ENDIF});
  FExtendedRect.Right := Result.Right - CorrectRectWidth;

  if FLastLine <> '' then
    Inc(Result.Bottom, FTextHeight);
end;

function TExtendedHintWindow.CalculatePointerValue(MaxWidth: Integer): TRect;
begin
  Result := ClientRect.Empty;
end;

procedure TExtendedHintWindow.CMTextChanged(var Message: TMessage);
begin
  // lock ClientRect update
end;

constructor TExtendedHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  FAddrChain := TList<Int64>.Create;
  FAsm := TStringList.Create;
  FHints := TStringList.Create;
  FItems := TList<TAddrCacheItem>.Create;
end;

destructor TExtendedHintWindow.Destroy;
begin
  FAddrChain.Free;
  FAsm.Free;
  FHints.Free;
  FItems.Free;
  inherited;
end;

procedure TExtendedHintWindow.DrawAddrChain;
var
  TextPos: TPoint;
  MarkR, LineR: TRect;

  procedure DrawLine(AddrVA: Int64; WithLine: Boolean);
  var
    AItem: TAddrCacheItem;
    ArrowSize, Tripple: Integer;
  begin
    FData.QueryCacheItem(AddrVA, AItem);
    Canvas.Brush.Color := FData.Colors[FData.AccessToAddrType(AddrVA, AItem.Region.Access)];
    Canvas.Brush.Style := bsSolid;
    Canvas.RoundRect(MarkR, 2, 2);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(TextPos.X, TextPos.Y, GetAddrString(AddrVA));
    if WithLine then
    begin
      Canvas.Brush.Color := FData.ColorMap.ArrowDownSelectedColor;
      Canvas.Brush.Style := bsSolid;
      PatBlt(Canvas, LineR.Left, LineR.Top, LineR.Width, 1, PATCOPY);
      PatBlt(Canvas, LineR.Left, LineR.Top, 1, LineR.Height, PATCOPY);
      PatBlt(Canvas, LineR.Left, LineR.Bottom, LineR.Width, 1, PATCOPY);
      Canvas.Brush.Style := bsClear;
    end;
    OffsetRect(MarkR, 0, FTextHeight);
    OffsetRect(LineR, 0, FTextHeight);
    Inc(TextPos.Y, FTextHeight);
  end;

var
  I, Offsets: Integer;
begin
  MarkR := Bounds(NcBorderWidth, NcBorderWidth, FTextHeight, FTextHeight);
  Offsets := -Ceil(MarkR.Height / 5);
  InflateRect(MarkR, Offsets, Offsets);
  LineR := MarkR;
  OffsetRect(LineR, -FTextHeight shr 1, -FTextHeight shr 1);
  LineR.Right := MarkR.Left - 1;
  LineR.Left := NcBorderWidth;
  TextPos := Point(FTextHeight + NcBorderWidth, NcBorderWidth);
  DrawLine(FData.AddrVA, False);
  for I := 0 to Length(FData.ChainList) - 1 do
    DrawLine(FData.ChainList[I], True);
  if FLastLine <> '' then
    Canvas.TextOut(NcBorderWidth, TextPos.Y, FLastLine);
end;

procedure TExtendedHintWindow.DrawExecuteExtendedRect;
var
  R: TRect;
  I, nTokenLength, nTokenSize, nSize, nLength: Integer;
  Line: string;
  pData: PChar;
  Dx: array of Integer;
  DrawLinkStep: Byte;
begin
  SetLength(Dx, Max(FMaxLine, FMaxHint));
  for I := 0 to Length(Dx) - 1 do
    Dx[I] := FData.CharWidth;

  for I := 0 to FAsm.Count - 1 do
  begin
    Line := FAsm[I];
    pData := PChar(@Line[1]);
    nSize := Length(Line);
    nLength := UTF8StringLength(Line);
    R := Bounds(
      FExtendedRect.Left + FData.BorderWidth,
      FExtendedRect.Top + FData.BorderWidth + I * FData.RowHeight,
      FMaxLine * FData.CharWidth, FData.RowHeight);
    DrawLinkStep := 0;
    while nSize > 0 do
    begin
      nTokenLength := nLength;
      if DrawLinkStep = 1 then
        Inc(DrawLinkStep)
      else
      begin
        Canvas.Font.Style := [];
        case FData.Tokenizer.GetToken(pData, nTokenLength) of
          ttNumber: Canvas.Font.Color := FData.ColorMap.NumberColor;
          ttInstruction: Canvas.Font.Color := FData.ColorMap.InstructionColor;
          ttSize: Canvas.Font.Color := FData.ColorMap.SizePfxColor;
          ttReg: Canvas.Font.Color := FData.ColorMap.RegColor;
          ttPrefix: Canvas.Font.Color := FData.ColorMap.PrefixColor;
          ttJmp:
          begin
            Canvas.Font.Color := FData.ColorMap.JmpColor;
            if FAddrItem.Linked[I] then
              Inc(DrawLinkStep);
          end;
          ttKernel: Canvas.Font.Color := FData.ColorMap.KernelColor;
          ttNop: Canvas.Font.Color := FData.ColorMap.NopColor;
        else
          Canvas.Font.Color := FData.ColorMap.TextColor;
        end;
      end;
      if nTokenLength > nLength then
        nTokenLength := nLength;
      nTokenSize := UTF8ByteCount(pData, nTokenLength);
      if DrawLinkStep = 2 then
      begin
        // for a brighter color, first render with a normal color,
        // then the alpha blending areas will be clearer
        Canvas.Font.Color := FData.ColorMap.JmpMarkTextColor;
        Canvas.Font.Style := [TFontStyle.fsUnderline];
        ExtTextOut(Canvas, R.Left, R.Top, ETO_CLIPPED, @R, pData,
          {$IFDEF LINUX}nTokenSize{$ELSE}nTokenLength{$ENDIF}, @Dx[0]);
      end;
      ExtTextOut(Canvas, R.Left, R.Top, ETO_CLIPPED, @R, pData,
        {$IFDEF LINUX}nTokenSize{$ELSE}nTokenLength{$ENDIF}, @Dx[0]);
      Inc(pData, nTokenSize);
      Dec(nSize, nTokenSize);
      Dec(nLength, nTokenLength);
      Inc(R.Left, nTokenLength * FData.CharWidth);
    end;
    Line := FHints[I];
    if Line = '' then Continue;
    R.Left := FExtendedRect.Left + FData.BorderWidth +
      (FMaxLine + 1) * FData.CharWidth;
    R.Width := FMaxHint * FData.CharWidth;
    Canvas.Font.Color := FData.ColorMap.TextCommentColor;
    Canvas.Font.Style := [];
    ExtTextOut(Canvas, R.Left, R.Top, ETO_CLIPPED, @R, @Line[1],
      {$IFDEF LINUX}Length(Line){$ELSE}UTF8StringLength(Line){$ENDIF}, @Dx[0]);
  end;
end;

procedure TExtendedHintWindow.DrawReadMem;
begin

end;

function TExtendedHintWindow.GetAddrString(AddrVA: Int64): string;
var
  AItem: TAddrCacheItem;
begin
  FData.QueryCacheItem(AddrVA, AItem);
  if AItem.Symbol = '' then
    Result := Format('[0x%x] (%s)', [AddrVA, FData.FormatAccessString(AItem.Region)])
  else
    Result := Format('[0x%x] (%s) -> %s', [AddrVA, FData.FormatAccessString(AItem.Region), AItem.Symbol]);
end;

procedure TExtendedHintWindow.Paint;
var
  R: TRect;
  SavedFont: TFont;
begin
  // Fill default background without text
  Caption := '';
  inherited;

  if (FData.AddressType <> atExecute) and (FAddrItem.InDeepCount > 0) then
  begin
    DrawAddrChain;
    Exit;
  end;

  Canvas.Brush.Style := bsClear;
  R := Bounds(NcBorderWidth, NcBorderWidth, ClientWidth - NcBorderWidth shl 1, FTextHeight);
  DrawText(Canvas, FAddrStr, -1, R, DT_LEFT or DT_NOPREFIX or DT_SINGLELINE);
  OffsetRect(R, 0, FTextHeight);
  if FAddrItem.Symbol <> '' then
  begin
    DrawText(Canvas, FAddrItem.Symbol, -1, R, DT_LEFT or DT_NOPREFIX or DT_SINGLELINE);
    OffsetRect(R, 0, FTextHeight);
  end;

  if FLastLine <> '' then
  begin
    R.Top := FExtendedRect.Bottom;
    R.Height := FTextHeight;
    DrawText(Canvas, FLastLine, -1, R, DT_LEFT or DT_NOPREFIX or DT_SINGLELINE);
    OffsetRect(R, 0, FTextHeight);
  end;

  if FExtendedRect.IsEmpty then Exit;

  SavedFont := TFont.Create;
  try
    SavedFont.Assign(Canvas.Font);
    Canvas.Font := FData.Font;
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Color := FData.ColorMap.BackgroundColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(FExtendedRect);
    if FMaxHint > 0 then
    begin
      Canvas.Brush.Color := FData.ColorMap.HeaderColumnSeparatorColor;
      PatBlt(Canvas, FExtendedRect.Left + FData.BorderWidth +
        FMaxLine * FData.CharWidth + FData.CharWidth shr 1,
        FExtendedRect.Top + 1, 1, FExtendedRect.Height - 2, PATCOPY);
    end;
    Canvas.Brush.Style := bsClear;

    if FData.AddressType = atExecute then
      DrawExecuteExtendedRect
    else
      DrawReadMem;

    Canvas.Font.Assign(SavedFont);
  finally
    SavedFont.Free;
  end;

end;

end.

