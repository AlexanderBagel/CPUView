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
  PExtendedHintData = ^TExtendedHintData;
  TExtendedHintData = record
    AddrChain: array of TAddrCacheItem;
    BorderWidth: Integer;
    CharWidth: Integer;
    ColorMap: TAsmColorMap;
    Colors: array [TAddrType] of TColor;
    Font: TFont;
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
    FAsm, FBottomHint, FHints: TStringList;
    FData: TExtendedHintData;
    FExtendedRect: TRect;
    FDisplayedItem: TAddrCacheItem;
    FTextHeight, FMaxChainLine, FMaxLine, FMaxHint: Integer;
    procedure CalculateExecute(MaxWidth: Integer);
    function CalculatePointerValue(MaxWidth: Integer): TRect;
    procedure DrawAddrChain;
    procedure DrawExecuteExtendedRect;
    procedure DrawReadMem;
    function GetAddrString(const AItem: TAddrCacheItem): string;
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

function TExtendedHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: TCustomData): TRect;
const
  CorrectRectWidth = NcBorderWidth {$IFNDEF FPC} shl 1{$ENDIF};
var
  I, ChainLength: Integer;
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

  // Хинт состоит из трех частей.
  // 1. Цепочка адресов в верхней части (зависит от настроек глубокого поиска отладочной информации)
  // 2. Сами данные в виде Asm кода, или значения, на которое указывает самый последний адрес в цепочке
  // 3. Дополнительные данные выводимые текстом внизу подсказки

  // The hint has three parts.
  // 1. Address chain at the top (depends on the settings of deep search for debugging information)
  // 2. The data itself in the form of Asm code, or the value pointed to by the most recent address in the chain
  // 3. Additional data displayed in text at the bottom of the tooltip

  FAsm.Clear;
  FBottomHint.Text := AHint;
  FBottomHint.Delete(0);
  FHints.Clear;

  inherited CalcHintRect(MaxWidth, ' ', nil); // for font setup

  FMaxChainLine := 0;
  FMaxHint := 0;
  FMaxLine := 0;
  ChainLength := Length(FData.AddrChain);
  FDisplayedItem := FData.AddrChain[ChainLength - 1];

  for I := 0 to ChainLength - 1 do
    FMaxChainLine := Max(FMaxChainLine, Canvas.TextWidth(GetAddrString(FData.AddrChain[I])));
  Inc(FMaxChainLine, FTextHeight); // Reserve for address type labels

  for I := 0 to FBottomHint.Count - 1 do
    FMaxChainLine := Max(FMaxChainLine, Canvas.TextWidth(FBottomHint[I]));

  FExtendedRect := Bounds(
    NcBorderWidth,
    NcBorderWidth shl 1 + ChainLength * FData.RowHeight,
    FMaxChainLine, 0);

  case FDisplayedItem.AddrType of
    atExecute: CalculateExecute(MaxWidth);
    atString:; // not implemented yet...
  else
    CalculatePointerValue(MaxWidth);
  end;

  Result := Rect(0, 0,
    FExtendedRect.Right + NcBorderWidth,
    FExtendedRect.Bottom + NcBorderWidth + FBottomHint.Count * FTextHeight);
end;

procedure TExtendedHintWindow.CalculateExecute(MaxWidth: Integer);
var
  I, SeparatorWidth: Integer;
begin
  FAsm.Text := FDisplayedItem.AsmLines;
  FHints.Text := FDisplayedItem.HintLines;
  for I := 0 to FAsm.Count - 1 do
  begin
    FMaxLine := Max(FMaxLine, Length(FAsm[I]) * FData.CharWidth);
    FMaxHint := Max(FMaxHint, Length(FHints[I]) * FData.CharWidth);
  end;
  if FMaxHint > 0 then
    SeparatorWidth := FData.CharWidth
  else
    SeparatorWidth := 0;
  FExtendedRect := Bounds(
    FExtendedRect.Left,
    FExtendedRect.Top,
    Max(FMaxChainLine, FMaxLine + SeparatorWidth + FMaxHint) + FData.BorderWidth shl 1,
    FAsm.Count * FData.RowHeight + FData.BorderWidth shl 1);
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
  FAsm := TStringList.Create;
  FBottomHint := TStringList.Create;
  FHints := TStringList.Create;
end;

destructor TExtendedHintWindow.Destroy;
begin
  FAsm.Free;
  FBottomHint.Free;
  FHints.Free;
  inherited;
end;

procedure TExtendedHintWindow.DrawAddrChain;
var
  TextPos: TPoint;
  MarkR, LineR: TRect;

  procedure DrawLine(const AItem: TAddrCacheItem; WithLine: Boolean);
  var
    ArrowSize, Tripple: Integer;
  begin
    Canvas.Brush.Color := FData.Colors[AItem.AddrType];
    Canvas.Brush.Style := bsSolid;
    Canvas.RoundRect(MarkR, 2, 2);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(TextPos.X, TextPos.Y, GetAddrString(AItem));
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
  DrawLine(FData.AddrChain[0], False);
  for I := 1 to Length(FData.AddrChain) - 1 do
    DrawLine(FData.AddrChain[I], True);
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
            if FDisplayedItem.Linked[I] then
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
    R.Left := FExtendedRect.Left + FData.BorderWidth + FMaxLine + FData.CharWidth;
    R.Width := FMaxHint;
    Canvas.Font.Color := FData.ColorMap.TextCommentColor;
    Canvas.Font.Style := [];
    ExtTextOut(Canvas, R.Left, R.Top, ETO_CLIPPED, @R, @Line[1],
      {$IFDEF LINUX}Length(Line){$ELSE}UTF8StringLength(Line){$ENDIF}, @Dx[0]);
  end;
end;

procedure TExtendedHintWindow.DrawReadMem;
begin

end;

function TExtendedHintWindow.GetAddrString(const AItem: TAddrCacheItem): string;
begin
  if AItem.Symbol = '' then
    Result := Format('[0x%x] (%s)', [AItem.AddrVA, AItem.Region.ToString])
  else
    Result := Format('[0x%x] (%s) -> %s', [AItem.AddrVA, AItem.Region.ToString, AItem.Symbol]);
end;

procedure TExtendedHintWindow.Paint;
var
  P: TPoint;
  SavedFont: TFont;
  I: Integer;
begin
  // Fill default background without text
  Caption := '';
  inherited;

  DrawAddrChain;

  if FBottomHint.Count > 0 then
  begin
    Canvas.Brush.Style := bsClear;
    P := Point(NcBorderWidth, FExtendedRect.Bottom);
    for I := 0 to FBottomHint.Count - 1 do
    begin
      Canvas.TextOut(P.X, P.Y, FBottomHint[I]);
      Inc(P.Y, FTextHeight);
    end;
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
        FMaxLine + FData.CharWidth shr 1,
        FExtendedRect.Top + 1, 1, FExtendedRect.Height - 2, PATCOPY);
    end;
    Canvas.Brush.Style := bsClear;

    if FDisplayedItem.AddrType = atExecute then
      DrawExecuteExtendedRect
    else
      DrawReadMem;

    Canvas.Font.Assign(SavedFont);
  finally
    SavedFont.Free;
  end;

end;

end.

