unit CpuView.Stream;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes,
  SysUtils;

type
  TRegionData = record
    AllocationBase,
    BaseAddr,
    RegionSize: Int64;
  end;

  TOnCacheUpdated = procedure(pBuff: PByte; AAddrVA: UInt64; ASize: Int64) of object;

  TRemoteAbstractStream = class(TStream)
  private
    FProcessID: Cardinal;
    FSize: Int64;
    FUpdated: TOnCacheUpdated;
  protected
    procedure DoUpdated(pBuff: PByte; AAddrVA, ASize: Int64);
    function GetSize: Int64; override;
    procedure SetProcessID(const Value: Cardinal); virtual;
    procedure SetSize(const Value: Int64); override;
  public
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; override;
    property ProcessID: Cardinal read FProcessID write SetProcessID;
    property OnUpdated: TOnCacheUpdated read FUpdated write FUpdated;
  end;

  TBufferedROStream = class(TStream)
  private
    FStream: TRemoteAbstractStream;
    FOwnership: TStreamOwnership;
    FPosition, FWindowOffset: Int64;
    FBuff: array of Byte;
    FBuffStartPosition: Int64;
    FBuffSize: Integer;
    procedure InvalidateBuffer;
    function GetBuffer_EndPosition: Int64;
    procedure SetBufferSize(Value: Integer);
  protected
    property Buffer_StartPosition: Int64 read FBuffStartPosition;
    property Buffer_EndPosition: Int64 read GetBuffer_EndPosition;
    function Buffer_Read(var Buffer; ASize: LongInt): Longint;
    function Buffer_Update: Boolean;
    function Buffer_Contains(APosition: Int64): Boolean;
  public
    constructor Create(AStream: TRemoteAbstractStream;
      AOwnership: TStreamOwnership = soReference);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    /// <summary>
    ///  HexView всегда отображает данные с нулевой позиции стрима,
    ///  т.е. от Position = 0, поэтому для стрима выставляется "окно"
    ///  в котором нулевая позиция будет означать какой-то конкретный адрес
    /// </summary>
    procedure SetAddrWindow(AStartAddrVA, AEndAddrVA: Int64);
    property BufferSize: Integer read FBuffSize write SetBufferSize;
    property Stream: TRemoteAbstractStream read FStream;
  end;

implementation

{ TRemoteAbstractStream }

procedure TRemoteAbstractStream.DoUpdated(pBuff: PByte; AAddrVA, ASize: Int64);
begin
  if Assigned(FUpdated) then
    FUpdated(pBuff, AAddrVA, ASize);
end;

function TRemoteAbstractStream.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TRemoteAbstractStream.SetProcessID(const Value: Cardinal);
begin
  FProcessID := Value;
end;

procedure TRemoteAbstractStream.SetSize(const Value: Int64);
begin
  FSize := Value;
end;

function TRemoteAbstractStream.Write(const Buffer; Count: Longint): Longint;
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

constructor TBufferedROStream.Create(AStream: TRemoteAbstractStream;
  AOwnership: TStreamOwnership);
begin
  FStream := AStream;
  FOwnership := AOwnership;
  BufferSize := 1024 * 1024;
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
    Assert(False, 'Unknown TSeekOrigin');
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
    Value := 16 * 1024;
  if FBuffSize <> Value then
  begin
    FBuffSize := Value;
    InvalidateBuffer;
  end;
end;

function TBufferedROStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('Stream is read-only');
end;

end.
