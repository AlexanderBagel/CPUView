////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Stream.pas
//  * Purpose   : Auxiliary streams for reading data from a remote process.
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

unit CpuView.Stream;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  CpuView.Common;

type
  TOnCacheUpdated = procedure(pBuff: PByte; AAddrVA: Int64; ASize: Int64) of object;

  TRemoteStream = class(TStream)
  private
    FPosition, FSize: Int64;
    FUtils: TCommonAbstractUtils;
    FUpdated: TOnCacheUpdated;
  protected
    procedure DoUpdated(pBuff: PByte; AAddrVA, ASize: Int64);
    function GetSize: Int64; override;
    procedure SetSize(const Value: Int64); override;
  public
    constructor Create(AUtils: TCommonAbstractUtils);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property OnUpdated: TOnCacheUpdated read FUpdated write FUpdated;
  end;

  { TBufferedROStream }

  TBufferedROStream = class(TStream)
  private
    FStream: TRemoteStream;
    FOwnership: TStreamOwnership;
    FPosition, FWindowOffset: Int64;
    FBuff: array of Byte;
    FBuffStartPosition: Int64;
    FBuffSize: Integer;
    procedure InvalidateBuffer;
    function GetBuffer_EndPosition: Int64;
    function GetMemory: PByte;
    procedure SetBufferSize(Value: Integer);
  protected
    property Buffer_StartPosition: Int64 read FBuffStartPosition;
    property Buffer_EndPosition: Int64 read GetBuffer_EndPosition;
    function Buffer_Read(var Buffer; ASize: LongInt): Longint;
    function Buffer_Update: Boolean;
    function Buffer_Contains(APosition: Int64): Boolean;
  public
    constructor Create(AStream: TRemoteStream;
      AOwnership: TStreamOwnership = soReference);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    ///  HexView всегда отображает данные с нулевой позиции стрима,
    ///  т.е. от Position = 0, поэтому для стрима выставляется "окно"
    ///  в котором нулевая позиция будет означать какой-то конкретный адрес

    /// <summary>
    ///  HexView always displays data from the zero stream position,
    ///  i.e. from Position = 0, so a “window” is set up for the stream
    ///  in which zero position will mean some specific address
    /// </summary>
    procedure SetAddrWindow(AStartAddrVA, AEndAddrVA: Int64);
    property BufferSize: Integer read FBuffSize write SetBufferSize;
    property Memory: PByte read GetMemory;
    property Stream: TRemoteStream read FStream;
  end;

implementation

{ TRemoteStream }

constructor TRemoteStream.Create(AUtils: TCommonAbstractUtils);
begin
  FUtils := AUtils;
end;

procedure TRemoteStream.DoUpdated(pBuff: PByte; AAddrVA, ASize: Int64);
begin
  if Assigned(FUpdated) then
    FUpdated(pBuff, AAddrVA, ASize);
end;

function TRemoteStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TRemoteStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FUtils.ReadData({%H-}Pointer(FPosition), Buffer, Count);
  if (Result > 0) and FUtils.NeedUpdateReadData then
    DoUpdated(@Buffer, FPosition, Result);
end;

function TRemoteStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := GetSize + Offset;
  end;
  Result := FPosition
end;

procedure TRemoteStream.SetSize(const Value: Int64);
begin
  FSize := Value;
end;

function TRemoteStream.{%H-}Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('Stream is read-only');
end;

{ TBufferedROStream }

function TBufferedROStream.Buffer_Contains(APosition: Int64): Boolean;
begin
  Result := (FBuffStartPosition <= APosition) and (GetBuffer_EndPosition >= APosition);
end;

function TBufferedROStream.Buffer_Read(var Buffer; ASize: LongInt): Longint;
begin
  Result := 0;
  if not Buffer_Contains(FPosition) then
    Exit;
  Result := Buffer_EndPosition - FPosition + 1;
  Assert(Result > 0);
  if Result > ASize then
    Result := ASize;
  Move(FBuff[Integer(FPosition - Buffer_StartPosition)], Buffer, Result);
  Inc(FPosition, Result);
end;

function TBufferedROStream.Buffer_Update: Boolean;
begin
  FStream.Position := FPosition + FWindowOffset;
  FBuffStartPosition := FPosition;
  SetLength(FBuff, FBuffSize);
  SetLength(FBuff, FStream.Read(FBuff[0], FBuffSize));
  Result := Length(FBuff) > 0;
end;

constructor TBufferedROStream.Create(AStream: TRemoteStream;
  AOwnership: TStreamOwnership);
begin
  FStream := AStream;
  FOwnership := AOwnership;
  BufferSize := 4096;
end;

destructor TBufferedROStream.Destroy;
begin
  if FOwnership = soOwned then
    FreeAndNil(FStream);
  inherited;
end;

function TBufferedROStream.GetBuffer_EndPosition: Int64;
begin
  Result := Int64(Length(FBuff)) + FBuffStartPosition - Int64(1);
end;

procedure TBufferedROStream.InvalidateBuffer;
begin
  FBuff := nil;
end;

function TBufferedROStream.GetMemory: PByte;
begin
  if Length(FBuff) = 0 then
    Result := nil
  else
    Result := @FBuff[0];
end;

function TBufferedROStream.Read(var Buffer; Count: Longint): Longint;
var
  Readed: Integer;
begin
  Result := 0;
  while Result < Count do
  begin
    Readed := Buffer_Read(PAnsiChar(@Buffer)[Result], Count - Result);
    Inc(Result, Readed);
    if Readed = 0 then
      if not Buffer_Update then
        Exit;
  end;
end;

function TBufferedROStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FStream.Size + Offset;
  else
    Assert(False, 'Unknown TSeekOrigin'){%H-};
  end;
  Result := FPosition;
  if not Buffer_Contains(FPosition) then
    InvalidateBuffer;
end;

procedure TBufferedROStream.SetAddrWindow(AStartAddrVA, AEndAddrVA: Int64);
begin
  FPosition := 0;
  FWindowOffset := AStartAddrVA;
  FStream.SetSize(AEndAddrVA);
  InvalidateBuffer;
end;

procedure TBufferedROStream.SetBufferSize(Value: Integer);
begin
  if Value < 0 then
    Value := 4096;
  if FBuffSize <> Value then
  begin
    FBuffSize := Value;
    InvalidateBuffer;
  end;
end;

function TBufferedROStream.{%H-}Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('Stream is read-only');
end;

end.
