unit CpuView.Linux;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLType,
  LCLIntf,
  Classes,
  SysUtils,
  Generics.Collections,
  CpuView.Common,
  CpuView.IntelContext.Types;

type
  TMemoryBasicInformation = record
    AllocationBase, BaseAddress, RegionSize, OffsetInFile: UInt64;
    Read, Write, Execute, Shared: Boolean;
    MappedFile, Hint: string;
  end;

  TMemoryBasicInformationList = class(TList<TMemoryBasicInformation>);

  function LoadVirtualMemoryInformation(AProcessID: Integer; AQueryAddr: UInt64 = 0): TMemoryBasicInformationList;
  function VirtualQueryMBI(AProcessID: Integer; AQueryAddr: UInt64; out MBI: TMemoryBasicInformation): Boolean;

  function GetIntelContext(ThreadID: DWORD): TIntelThreadContext;
  function SetIntelContext(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
  function GetIntelWow64Context(ThreadID: DWORD): TIntelThreadContext;
  function SetIntelWow64Context(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
  function GetThreadStackLimit(ProcessID, ThreadID: DWORD): TStackLimit;
  function GetThreadWow64StackLimit(ProcessID, ThreadID: DWORD): TStackLimit;

implementation

function ReadVirtualFile(const FilePath: string): TMemoryStream;
var
  F: TFileStream;
  LoadBuff: array of Byte;
  I: Integer;
begin
  Result := TMemoryStream.Create;
  if not FileExists(FilePath) then Exit;
  F := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LoadBuff{%H-}, 4096);
    repeat
      I := F.Read(LoadBuff, 4096);
      if I > 0 then
        Result.WriteBuffer(LoadBuff, I);
    until I = 0;
  finally
    F.Free;
  end;
  Result.Position := 0;
end;

function SkipChar(const buff: PByte): PByte;
begin
  Result := buff;
  if Result = nil then Exit;
  if Result^ = 0 then
    Result := nil
  else
    Inc(Result);
end;

function SkipWhiteSpace(const buff: PByte): PByte;
begin
  Result := buff;
  while Assigned(Result) and (Result^ in [9, 32]) do
    Inc(Result);
  if Result^ = 0 then
    Result := nil;
end;

function ScanDec(const buff: PByte; out Value: UInt64): PByte;
var
  Digit: Byte;
begin
  Value := 0;
  Digit := 0;
  Result := SkipWhiteSpace(buff);
  if Result = nil then Exit;
  while True do
  begin
    Digit := PByte(Result)^;
    if Byte(Digit - Byte('0')) <= 9 then
      Dec(Digit, Byte('0'))
    else
      Break;
    Value := (Value * 10) + Digit;
    Inc(Result);
  end;
end;

function ScanHex(const buff: PByte; out Value: UInt64): PByte;
var
  Digit: Byte;
begin
  Value := 0;
  Digit := 0;
  Result := SkipWhiteSpace(buff);
  if Result = nil then Exit;
  while True do
  begin
    Digit := PByte(Result)^;
    {$message 'Если не сделать каст к байту то Digit пойдет в RAX и вычитание с компарацией будет из RAX и все поплывет'}
    // т.к. должно быть в EAX который после MOVZX чистится
    if Byte(Digit - Byte('0')) <= 9 then
      Dec(Digit, Byte('0'))
    else if Byte(Digit - Byte('a')) < 6 then
      Dec(Digit, Byte('a') - 10)
    else if Byte(Digit - Byte('A')) < 6 then
      Dec(Digit, Byte('A') - 10)
    else
      Break;
    Value := (Value shl 4) or Digit;
    Inc(Result);
  end;
end;

function ScanChar(const buff: PByte; var AChar: Char): PByte;
begin
  Result := buff;
  AChar := #0;
  if Result = nil then Exit;
  AChar := Char(Result^);
  if AChar = #0 then
    Result := nil
  else
    Inc(Result);
end;

function ScanAccess(const buff: PByte; out Read, Write, Execute, Shared: Boolean): PByte;
var
  AccessChar: Char;
begin
  Read := False;
  Write := False;
  Execute := False;
  Shared := False;
  Result := SkipWhiteSpace(buff);
  if Result = nil then Exit;
  AccessChar := #0;
  Result := ScanChar(Result, AccessChar);
  Read := LowerCase(AccessChar) = 'r';
  Result := ScanChar(Result, AccessChar);
  Write := LowerCase(AccessChar) = 'w';
  Result := ScanChar(Result, AccessChar);
  Execute := LowerCase(AccessChar) = 'x';
  Result := ScanChar(Result, AccessChar);
  Shared := LowerCase(AccessChar) = 's';
end;

function LoadVirtualMemoryInformation(AProcessID: Integer; AQueryAddr: UInt64): TMemoryBasicInformationList;
var
  MapsPath, Line: string;
  VMData: TStringList;
  M: TMemoryStream;
  InumDict: TDictionary<UInt64, UInt64>;
  MBI: TMemoryBasicInformation;
  I: Integer;
  HighIdx, DummyIdx, InumIdx: UInt64;
  buff: PByte;
  Delimiter: Char;
begin
  Result := TMemoryBasicInformationList.Create;
  MapsPath := '/proc/' + IntToStr(AProcessID) + '/maps';
  if not FileExists(MapsPath) then Exit;

  VMData := TStringList.Create;
  try

    M := ReadVirtualFile(MapsPath);
    try
      VMData.LoadFromStream(M);
    finally
      M.Free;
    end;

    FillChar(MBI, SizeOf(MBI), 0);
    InumDict := TDictionary<UInt64, UInt64>.Create;
    try
      for I := 0 to VMData.Count - 1 do
      begin
        Line := VMData[I];
        if Line = '' then
          Continue;
        buff := PByte(@Line[1]);
        buff := ScanHex(buff, MBI.BaseAddress);
        buff := ScanChar(buff, Delimiter);
        buff := ScanHex(buff, HighIdx);
        MBI.RegionSize := HighIdx - MBI.BaseAddress;
        buff := ScanAccess(buff, MBI.Read, MBI.Write, MBI.Execute, MBI.Shared);
        buff := ScanHex(buff, MBI.OffsetInFile);

        buff := ScanHex(buff, DummyIdx);
        buff := ScanChar(buff, Delimiter);
        buff := ScanHex(buff, DummyIdx);

        buff := ScanHex(buff, InumIdx);
        if InumIdx = 0 then
          MBI.AllocationBase := MBI.BaseAddress
        else
        begin
          if not InumDict.TryGetValue(InumIdx, MBI.AllocationBase) then
          begin
            MBI.AllocationBase := MBI.BaseAddress;
            InumDict.Add(InumIdx, MBI.AllocationBase);
          end;
        end;

        buff := SkipWhiteSpace(buff);

        MBI.MappedFile := '';
        MBI.Hint := '';
        if buff <> nil then
        begin
          if buff^ = Byte('[') then
            MBI.Hint := PChar(buff)
          else
            MBI.MappedFile := PChar(buff);
        end;

        Result.Add(MBI);

        if (AQueryAddr <> 0) and (AQueryAddr >= MBI.BaseAddress) and (AQueryAddr < HighIdx) then
          Break;

      end;
    finally
      InumDict.Free;
    end;

  finally
    VMData.Free;
  end;
end;

function VirtualQueryMBI(AProcessID: Integer; AQueryAddr: UInt64; out
  MBI: TMemoryBasicInformation): Boolean;
var
  L: TMemoryBasicInformationList;
  Tmp: TMemoryBasicInformation;
begin
  FillChar(MBI, SizeOf(MBI), 0);
  L := LoadVirtualMemoryInformation(AProcessID, AQueryAddr);
  try
    Tmp := L.Last;
    Result := (Tmp.BaseAddress <= AQueryAddr) and (Tmp.BaseAddress + Tmp.RegionSize > AQueryAddr);
    if Result then
      MBI := Tmp;
  finally
    L.Free;
  end;
end;

function GetIntelContext(ThreadID: DWORD): TIntelThreadContext;
begin

end;

function SetIntelContext(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
begin

end;

function GetIntelWow64Context(ThreadID: DWORD): TIntelThreadContext;
begin

end;

function SetIntelWow64Context(ThreadID: DWORD; const AContext: TIntelThreadContext): Boolean;
begin

end;

function GetThreadStackLimit(ProcessID, ThreadID: DWORD): TStackLimit;
begin

end;

function GetThreadWow64StackLimit(ProcessID, ThreadID: DWORD): TStackLimit;
begin

end;

end.
