////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Design.DpiFix.pas
//  * Purpose   : Fixes Dpi problems with some controls.
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

unit CpuView.Design.DpiFix;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, ImgList, Graphics, Controls, Forms,
  ComCtrls, Themes,
  {$IFDEF MSWINDOWS}
  Win32Themes, UxTheme,
  {$ENDIF}
  laz.VirtualTrees;

type

  { TLazVSTWithDPI }

  TLazVSTWithDPI = class(TLazVirtualStringTree)
  protected
    procedure AdjustImageBorder(AImages: TCustomImageList; ABidiMode: TBidiMode;
      VAlign: Integer; var R: TRect; var ImageInfo: TVTImageInfo); override;
    procedure AutoScale; override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const AText: string;
      CellRect: TRect; DrawFormat: Cardinal); override;
    procedure PaintCheckImage(ACanvas: TCanvas; const ImageInfo: TVTImageInfo;
      {%H-}ASelected: Boolean); override;
  end;

  { TScaledControlHelper }

  TScaledControlHelper = class Helper for TControl
  public
    function CurrentPPI: Integer;
  end;

  { TStatusBarWithDPI }

  TStatusBarWithDPI = class(TStatusBar)
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: Integer; WithThemeSpace: Boolean); override;
  end;

implementation

{ TLazVSTWithDPI }

procedure TLazVSTWithDPI.AdjustImageBorder(AImages: TCustomImageList;
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

procedure TLazVSTWithDPI.AutoScale;
var
  Enum: TVTVirtualNodeEnumerator;
begin
  BeginUpdate;
  try
    Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
    inherited AutoScale;
    DefaultNodeHeight := Scale96ToFont(DEFAULT_NODE_HEIGHT);
    Header.DefaultHeight := Scale96ToFont(DEFAULT_HEADER_HEIGHT);
    Header.Height := Header.DefaultHeight;
    Enum := Nodes.GetEnumerator;
    while Enum.MoveNext do
      NodeHeight[Enum.Current] := DefaultNodeHeight;
  finally
    EndUpdate;
  end;
end;

procedure TLazVSTWithDPI.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const AText: string; CellRect: TRect; DrawFormat: Cardinal);
begin
  PaintInfo.Canvas.Font.PixelsPerInch := Font.PixelsPerInch;
  inherited DoTextDrawing(PaintInfo, AText, CellRect, DrawFormat);
end;

procedure TLazVSTWithDPI.PaintCheckImage(ACanvas: TCanvas;
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

{ TScaledControlHelper }

function TScaledControlHelper.CurrentPPI: Integer;
var
  AForm: TCustomForm;
  AMonitor: TMonitor;
begin
  Result := Screen.PixelsPerInch;
  AForm := GetParentForm(Self);
  if AForm = nil then Exit;
  if not AForm.HandleAllocated then Exit;
  AMonitor := Screen.MonitorFromWindow(AForm.Handle);
  if AMonitor = nil then Exit;
  Result := AMonitor.PixelsPerInch;
end;

{ TStatusBarWithDPI }

procedure TStatusBarWithDPI.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: Integer; WithThemeSpace: Boolean);
var
  AScreenPPI, ACurrentPPI: Integer;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  AScreenPPI := Screen.PixelsPerInch;
  ACurrentPPI := CurrentPPI;
  if AScreenPPI <> ACurrentPPI then
    PreferredHeight := MulDiv(PreferredHeight, ACurrentPPI, AScreenPPI);
end;

end.
