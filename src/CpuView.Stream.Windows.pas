unit CpuView.Stream.Windows;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows,
  Classes,
  SysUtils,
  CpuView.Stream;

type
  TRemoteStream = class(TRemoteAbstractStream)
  private
    FProcessHandle: THandle;
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
  dwLength: Cardinal;
begin
  Result := False;
  if FProcessHandle = 0 then Exit;
  dwLength := SizeOf(TMemoryBasicInformation);
  Result := VirtualQueryEx(FProcessHandle, Pointer(AddrVA), MBI, dwLength) = dwLength;
  if Result then
  begin
    RegionData.AllocationBase := Int64(MBI.AllocationBase);
    RegionData.BaseAddr := Int64(MBI.BaseAddress);
    RegionData.RegionSize := Int64(MBI.RegionSize);
  end;
end;

function TRemoteStream.Read(var Buffer; Count: Longint): Longint;

  function CanRead(MBI: TMemoryBasicInformation): Boolean;
  begin
    Result := MBI.State = MEM_COMMIT;
    if Result then
      Result := MBI.Protect and (
        PAGE_EXECUTE_READ or
        PAGE_EXECUTE_READWRITE or
        PAGE_READONLY or
        PAGE_READWRITE) <> 0;
    if Result then
      Result := (MBI.Protect and PAGE_GUARD) = 0;
  end;

var
  MBI: TMemoryBasicInformation;
  dwLength: Cardinal;
  RegionSize: NativeInt;
  ReadCount: NativeUInt;
begin
  Result := 0;
  if FProcessHandle = 0 then Exit;
  dwLength := SizeOf(TMemoryBasicInformation);
  if VirtualQueryEx(FProcessHandle,
    Pointer(FPosition), MBI, dwLength) <> dwLength then Exit;

  if NativeInt(MBI.BaseAddress) > 0 then
  begin
    RegionSize := MBI.RegionSize - (FPosition - NativeInt(MBI.BaseAddress));
    if Count > NativeInt(RegionSize) then
      Count := NativeInt(RegionSize);
  end
  else
    // принудительно отключаем все что не можем прочитать
    MBI.Protect := MBI.Protect or PAGE_GUARD;

  // не будем вмешиваться в память удаленного процесса
  // меняя атрибуты страницы, поэтому если стоит запрет на чтение
  // то просто возвращаем нулевой буфер заданного размера
  if not CanRead(MBI) then
  begin
    FillChar(Buffer, Count, 0);
    Exit(Count);
  end;

  if ReadProcessMemory(FProcessHandle, Pointer(FPosition),
    @Buffer, Count, ReadCount) then
    Result := Longint(ReadCount);

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
  if FProcessHandle <> 0 then
  begin
    CloseHandle(FProcessHandle);
    FProcessHandle := 0;
  end;
  inherited;
  if ProcessID <> 0 then
    FProcessHandle := OpenProcess(
      PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
end;

end.
