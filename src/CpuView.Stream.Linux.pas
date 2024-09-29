unit CpuView.Stream.Linux;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  baseunix,
  Classes,
  SysUtils,
  CpuView.Stream,
  CpuView.Linux;

// kernel >= 3.2
{$DEFINE HAVE_PROCESS_VM}

{$IFDEF HAVE_PROCESS_VM}

  function process_vm_readv(pid: pid_t; const local_iov: piovec;
    liovcnt: NativeUInt; const remote_iov: piovec; riovcnt: NativeUInt;
    flags: NativeUInt): ssize_t; cdecl; external 'libc.so';

{$ENDIF}

type
  TRemoteStream = class(TRemoteAbstractStream)
  private
    {$IFNDEF HAVE_PROCESS_VM}
    FProcessMem: TFileStream;
    {$ENDIF}
    FPosition: Int64;
  protected
    procedure SetProcessID(const Value: Cardinal); override;
  public
    destructor Destroy; override;
    function QueryRegion(AddrVA: Int64; out RegionData: TRegionData): Boolean; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

implementation

{ TRemoteStream }

destructor TRemoteStream.Destroy;
begin
  ProcessID := 0;
  inherited;
end;

function TRemoteStream.QueryRegion(AddrVA: Int64;
  out RegionData: TRegionData): Boolean;
var
  MBI: TMemoryBasicInformation;
begin
  Result := False;
  if ProcessID = 0 then Exit;
  {$IFNDEF HAVE_PROCESS_VM}
  if FProcessMem = nil then Exit;
  {$ENDIF}
  if not VirtualQueryMBI(ProcessID, AddrVA, MBI) then Exit;
  RegionData.AllocationBase := Int64(MBI.AllocationBase);
  RegionData.BaseAddr := Int64(MBI.BaseAddress);
  RegionData.RegionSize := Int64(MBI.RegionSize);
end;

function TRemoteStream.Read(var Buffer; Count: Longint): Longint;
var
  MBI: TMemoryBasicInformation;
  RegionSize: NativeInt;
  {$IFDEF HAVE_PROCESS_VM}
  Loc, Rem: iovec;
  {$ENDIF}
begin
  Result := 0;
  if ProcessID = 0 then Exit;
  {$IFNDEF HAVE_PROCESS_VM}
  if FProcessMem = nil then Exit;
  {$ENDIF}
  if not VirtualQueryMBI(ProcessID, FPosition, MBI) then Exit;

  if NativeInt(MBI.BaseAddress) > 0 then
  begin
    RegionSize := MBI.RegionSize - (FPosition - NativeInt(MBI.BaseAddress));
    if Count > NativeInt(RegionSize) then
      Count := NativeInt(RegionSize);
  end
  else
    // принудительно отключаем все что не можем прочитать
    MBI.Read := False;

  // если стоит запрет на чтение, или память не выделена,
  // то просто возвращаем нулевой буфер заданного размера
  if not MBI.Read then
  begin
    FillChar(Buffer, Count, 0);
    Exit(Count);
  end;

  {$IFDEF HAVE_PROCESS_VM}
  Loc.iov_len := Count;
  Loc.iov_base := Pointer(Buffer);
  Rem.iov_len := Count;
  Rem.iov_base := {%H-}Pointer(FPosition);
  Result := Longint(process_vm_readv(ProcessID, @Loc, 1, @Rem, 1, 0));
  {$ELSE}
  FProcessMem.Position := FPosition;
  Result := Longint(FProcessMem.Read(Buffer, Count));
  {$ENDIF}

  if Result > 0 then
    DoUpdated(@Buffer, FPosition, Result);
end;

function TRemoteStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := GetSize + Offset;
  end;
  Result := FPosition
end;

procedure TRemoteStream.SetProcessID(const Value: Cardinal);
begin
  if ProcessID = Value then Exit;
  {$IFDEF HAVE_PROCESS_VM}
  inherited
  {$ELSE}
  FreeAndNil(FProcessMem);
  inherited;
  if ProcessID <> 0 then
    FProcessMem := TFileStream.Create('/proc/' + IntToStr(ProcessID) + '/mem', fmOpenRead);
  {$ENDIF}
end;

end.
