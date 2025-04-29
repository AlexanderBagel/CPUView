////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Windows.MMap.pas
//  * Purpose   : Windows process memory map
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

unit CpuView.Windows.MMap;

{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, Windows, SysUtils, Generics.Collections,

  FWHexView.Common,
  CpuView.Windows;

type
  TPageType = (ptFree, ptReserved, ptPrivate, ptMapped, ptImage, ptThread, ptHeap, ptSystem);

  TAdvancedInfo = record
    Description: string;
    Addr: NativeUInt;
    PageType: TPageType;
  end;

  PListItemData = ^TListItemData;
  TListItemData = record
    AddrVA, EndAddrVA, Size: Int64;
    PageType: TPageType;
    Image, Section, Contains, Access, IAccess, MapedFile: string;
    ImageIdx: Integer;
    Grayed: Boolean;
  end;

  TCoffSymbol = packed record
    Name: array [0..3] of Byte;
    StrOfs: LongInt;
    Value: LongInt;
    Section: SmallInt;
    Empty: Word;
    Typ: Byte;
    Aux: Byte;
  end;

  { TSimpleMemoryMap }

  TSimpleMemoryMap = class
  private
    FAdvData: array of TAdvancedInfo;
    FCurrentPid: Cardinal;
    FImageCount: Integer;
    FIs64Process: Boolean;
    FGrayed: Boolean;
    FLastPageType: TPageType;
    FList: TList<TListItemData>;
    procedure AddNewData(const Description: string; Addr: NativeUInt;
      APageType: TPageType = ptSystem); overload;
    procedure AddNewData(const Description: string; Addr: Pointer;
      APageType: TPageType = ptSystem); overload;
    function AlignedSectionSize(const ImageInfo: LOADED_IMAGE;
      const Value: DWORD): DWORD;
    function DisplayImageData(hProcess: THandle; const ImagePath: string;
      lpBuffer: TMemoryBasicInformation; var pSectionAddr: NativeUInt): Boolean;
    procedure DisplayStateAndProtect(var AItem: TListItemData;
      const lpBuffer: TMemoryBasicInformation);
    function ExtractAccessString(const Value: DWORD): string;
    function ExtractInitialAccessString(const Value: DWORD): string;
    procedure FillPEBInfo(const hProcess: THandle);
    procedure FillThreadsInfo(const hProcess: THandle);
    function GetContainsDirectory(const Code, Data: Boolean;
      const ImageInfo: LOADED_IMAGE; const SectionAddr, SectionSize: NativeUInt): string;
    function IsExecute(const Value: DWORD): Boolean;
    function IsWrite(const Value: DWORD): Boolean;
  public
    constructor Create(APid: Cardinal; AIs64Process: Boolean);
    procedure FillProcessMemoryMap(AList: TList<TListItemData>);
  end;

implementation

var
  _ItemsData: array of TListItemData;

//  Функция приводит пути с использованием символьных ссылок к нормальному виду
//
//  The function converts paths using symbolic links to normal form
// =============================================================================
function NormalizePath(const Value: string): string;
const
  OBJ_CASE_INSENSITIVE         = $00000040;
  STATUS_SUCCESS               = 0;
  FILE_SYNCHRONOUS_IO_NONALERT = $00000020;
  FILE_READ_DATA = 1;
  ObjectNameInformation = 1;
  DriveNameSize = 4;
  VolumeCount = 26;
  DriveTotalSize = DriveNameSize * VolumeCount;
  DosDeviceName = '\Device\HarddiskVolume';
var
  US: UNICODE_STRING;
  OA: OBJECT_ATTRIBUTES;
  IO: IO_STATUS_BLOCK;
  hFile: THandle;
  NTSTAT, dwReturn: DWORD;
  ObjectNameInfo: TOBJECT_NAME_INFORMATION;
  Buff, Volume: string;
  I, Count, dwQueryLength: Integer;
  lpQuery: array [0..MAX_PATH - 1] of Char;
  AnsiResult: AnsiString;
begin
  Result := Value;

  if not Value.StartsWith(DosDeviceName, True) then
  begin
    RtlInitUnicodeString(@US, StringToOleStr(Value));

    // Аналог макроса InitializeObjectAttributes

    // Analog of InitializeObjectAttributes macro
    OA := Default(OBJECT_ATTRIBUTES);
    OA.Length := SizeOf(OBJECT_ATTRIBUTES);
    OA.ObjectName := @US;
    OA.Attributes := OBJ_CASE_INSENSITIVE;

    // Функция ZwOpenFile спокойно открывает файлы, путь к которым представлен
    // с использованием символьных ссылок, например:
    // \SystemRoot\System32\ntdll.dll
    // \??\C:\Windows\System32\ntdll.dll
    // \Device\HarddiskVolume1\WINDOWS\system32\ntdll.dll
    // Поэтому будем использовать ее для получения хэндла

    // The ZwOpenFile function calmly opens files whose path is represented by
    // using character references, e.g:
    // \SystemRoot\System32\ntdll.dll
    // \??\C:\Windows\System32\ntdll.dll
    // \Device\HarddiskVolume1\WINDOWS\system32\ntdll.dll
    // So we will use it to get the handle
    NTSTAT := ZwOpenFile(@hFile, FILE_READ_DATA or SYNCHRONIZE, @OA, @IO,
      FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
      FILE_SYNCHRONOUS_IO_NONALERT);
    if NTSTAT = STATUS_SUCCESS then
    try
      // Файл открыт, теперь смотрим его формализованный путь

      // The file is opened, now let's see its formalized path
      NTSTAT := NtQueryObject(hFile, ObjectNameInformation,
        @ObjectNameInfo, MAX_PATH * 2, @dwReturn);
      if NTSTAT = STATUS_SUCCESS then
      begin
        SetLength(AnsiResult{%H-}, MAX_PATH);
        WideCharToMultiByte(CP_ACP, 0,
          @ObjectNameInfo.Name.Buffer[ObjectNameInfo.Name.MaximumLength -
          ObjectNameInfo.Name.Length {$IFDEF WIN64} + 4{$ENDIF}],
          ObjectNameInfo.Name.Length, @AnsiResult[1],
          MAX_PATH, nil, nil);
        Result := string(PAnsiChar(AnsiResult));
      end;
    finally
      ZwClose(hFile);
    end;
  end;

  // Путь на открытый через ZwOpenFile файл
  // возвращается в виде \Device\HarddiskVolumeХ\бла-бла
  // Осталось только его сопоставить с реальным диском

  // Path to the file opened via ZwOpenFile
  // is returned as \Device\HarddiskVolumeX\blah-blah.
  // The only thing left to do is to map it to a real disk.
  SetLength(Buff{%H-}, DriveTotalSize);
  Count := GetLogicalDriveStrings(DriveTotalSize, @Buff[1]) div DriveNameSize;
  for I := 0 to Count - 1 do
  begin
    Volume := PChar(@Buff[(I * DriveNameSize) + 1]);
    Volume[3] := #0;
    // Преобразуем имя каждого диска в символьную ссылку и
    // сравниваем с формализированным путем

    // Convert each disk name to a symbolic reference and
    // compare with the formalized path
    QueryDosDevice(PChar(Volume), @lpQuery[0], MAX_PATH);
    dwQueryLength := Length(string(lpQuery));
    if Copy(Result, 1, dwQueryLength) = string(lpQuery) then
    begin
      Volume[3] := '\';
      if lpQuery[dwQueryLength - 1] <> '\' then
        Inc(dwQueryLength);
      Delete(Result, 1, dwQueryLength);
      Result := Volume + Result;
      Break;
    end;
  end;
end;

{ TSimpleMemoryMap }

procedure TSimpleMemoryMap.AddNewData(const Description: string;
  Addr: NativeUInt; APageType: TPageType);
var
  AdvDataLen: Integer;
begin
  if Addr > 0 then
  begin
    AdvDataLen := Length(FAdvData);
    SetLength(FAdvData, AdvDataLen + 1);
    FAdvData[AdvDataLen].Description := Description;
    FAdvData[AdvDataLen].Addr := Addr;
    FAdvData[AdvDataLen].PageType := APageType;
  end;
end;

procedure TSimpleMemoryMap.AddNewData(const Description: string; Addr: Pointer;
  APageType: TPageType);
begin
  AddNewData(Description, NativeUInt(Addr), APageType);
end;

function TSimpleMemoryMap.AlignedSectionSize(const ImageInfo: LOADED_IMAGE;
  const Value: DWORD): DWORD;
begin
  if Value = 0 then
  begin
    Result := 0;
    Exit;
  end;
  with ImageInfo.FileHeader^.OptionalHeader do
  begin
    if SectionAlignment mod Value = 0 then
      Result := Value
    else
    begin
      Result := Value div SectionAlignment;
      Inc(Result);
      Result := Result * SectionAlignment;
    end;
  end;
end;

function TSimpleMemoryMap.DisplayImageData(hProcess: THandle;
  const ImagePath: string; lpBuffer: TMemoryBasicInformation;
  var pSectionAddr: NativeUInt): Boolean;
var
  ImageInfo: LOADED_IMAGE;
  MovedSectionCount: Integer;
  ImageSectionHeader: PImageSectionHeader;
  dwLength: NativeUInt;
  AItem: TListItemData;
  BaseAddr, SectionEnd: NativeUInt;
  NormalizedName: string;

  // Процедура проверяет, расположена ли секция образа в текущей странице,
  // и если да, то помещает данные о секции и сопоставленной с ней страницей

  // The procedure checks if the image section is located in the current page,
  // and if so, places data about the section and the page associated with it
  procedure MoveSectionsToPage;
  var
    COFFOffset, SectionNameOffset: NativeUInt;
    Index: Integer;
    SectionName: array [0..255] of AnsiChar;
  begin
    if ImageSectionHeader^.VirtualAddress < SectionEnd then
    begin

      COFFOffset := ImageInfo.FileHeader.FileHeader.PointerToSymbolTable +
        ImageInfo.FileHeader.FileHeader.NumberOfSymbols * SizeOf(TCoffSymbol);

      AItem := Default(TListItemData);
      with AItem do
      begin
        AddrVA := Int64(BaseAddr + ImageSectionHeader^.VirtualAddress);
        Size := AlignedSectionSize(
          ImageInfo, ImageSectionHeader^.Misc.VirtualSize);
        EndAddrVA := AddrVA + Size;
        Image := ExtractFileName(ImagePath);
        Section := PAnsiChar(@ImageSectionHeader^.Name[0]);
        Section := Copy(Section, 1, IMAGE_SIZEOF_SHORT_NAME);
        if (COFFOffset <> 0) and (Section[1] = '/') then
        begin
          if TryStrToInt(Copy(string(Section), 2, 7), Index) then
          begin
            SectionNameOffset := COFFOffset + NativeUInt(Index);
            SectionNameOffset := SectionNameOffset + NativeUint(ImageInfo.MappedAddress);
            SectionName[255] := #0;
            CopyMemory(@SectionName[0], Pointer(SectionNameOffset), 255);
            Section := string(PAnsiChar(@SectionName[0]));
          end;
        end;
        Contains := GetContainsDirectory(
          IsExecute(ImageSectionHeader^.Characteristics),
          IsWrite(ImageSectionHeader^.Characteristics),
          ImageInfo, ImageSectionHeader^.VirtualAddress,
          AlignedSectionSize(ImageInfo, ImageSectionHeader^.Misc.VirtualSize));
        DisplayStateAndProtect(AItem, lpBuffer);
        ImageIdx := FImageCount;
      end;

      FList.Add(AItem);
      Inc(ImageSectionHeader);
      Inc(MovedSectionCount);

      // Если еще есть секции, вызываем рекурсивно сами себя

      // If there are still sections, call recursively ourselves
      if DWORD(MovedSectionCount) < ImageInfo.NumberOfSections then
        MoveSectionsToPage;
    end;
  end;

  procedure ProcesssOverlay;
  var
    SecAddr: NativeUInt;
    MappedName: string;
  begin
    while GetMappedFileName(hProcess, lpBuffer.BaseAddress, MappedName) do
    begin
      if MappedName <> ImagePath then Break;

      AItem := Default(TListItemData);
      with AItem do
      begin
        AddrVA := Int64(pSectionAddr);
        Size := lpBuffer.RegionSize;
        EndAddrVA := AddrVA + Size;
        Image := ExtractFileName(ImagePath);
        SecAddr := NativeUInt(lpBuffer.AllocationBase) +
          ImageInfo.FileHeader.OptionalHeader.
          DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].VirtualAddress;
        if SecAddr = pSectionAddr then
          Contains := 'security';
        DisplayStateAndProtect(AItem, lpBuffer);
        ImageIdx := FImageCount;
      end;

      FList.Add(AItem);
      Inc(pSectionAddr, lpBuffer.RegionSize);
      if VirtualQueryEx(hProcess,
        Pointer(pSectionAddr), lpBuffer, dwLength) <> dwLength then
        RaiseLastOSError;

      Inc(SectionEnd, lpBuffer.RegionSize);
    end;
  end;

const
  MM_LOWEST_USER_ADDRESS = $10000;
begin
  Result := False;

  // Проверка аттрибутов страниц и адресов,
  // в которых не может распологаться образ РЕ файла

  // Check page attributes and addresses,
  // where the PE file image cannot be located
  if (lpBuffer.State = MEM_FREE) or (lpBuffer.State = MEM_RESERVE) or
    (NativeUInt(lpBuffer.BaseAddress) <= MM_LOWEST_USER_ADDRESS) then
    Exit;

  // Запоминаем адрес начальной страницы с которой размещен образ

  // Memorize the address of the initial page from which the image was placed
  BaseAddr := pSectionAddr;

  NormalizedName := NormalizePath(ImagePath);

  // Отображаем образ в наш процесс, для получения информации

  // Mapping the image into our process, to get the information
  if MapAndLoad(PAnsiChar(AnsiString(NormalizedName)), nil, @ImageInfo, True, True) then
  try
    // Проверка - является ли образ PE файлом?

    // Checking - is the image a PE file?
    if ImageInfo.FileHeader^.Signature = IMAGE_NT_SIGNATURE then
    begin
      Result := True;

      // Если все в порядке, добавляем информацию о найденном РЕ заголовке

      // If everything is OK, add the information about the found PE header
      AItem := Default(TListItemData);
      with AItem do
      begin
        AddrVA := Int64(lpBuffer.BaseAddress);

        // Размер РЕ секции равен либо оффсету следующей секции,
        // либо размеру текущей страницы

        // The size of the PE section is equal to either the offset of the next section,
        // or the size of the current page
        if lpBuffer.RegionSize < ImageInfo.Sections^.VirtualAddress then
          Size := lpBuffer.RegionSize
        else
          Size := ImageInfo.Sections^.VirtualAddress;
        EndAddrVA := AddrVA + Size;

        Image := ExtractFileName(ImagePath);
        Contains := 'PE Header';
        DisplayStateAndProtect(AItem, lpBuffer);
        MapedFile := NormalizedName;
        ImageIdx := FImageCount;
      end;
      FList.Add(AItem);
    end
    else
      Exit;

    // Перечисляем все секции образа и сопоставляем их с страницами памяти

    // Enum all image sections and map them to memory pages
    ImageSectionHeader := ImageInfo.Sections;
    MovedSectionCount := 0;
    SectionEnd := lpBuffer.RegionSize;
    dwLength := SizeOf(TMemoryBasicInformation);

    // Крутим цикл до тех пор, пока не кончаться секции

    // Cycle until we run out of sections.
    while DWORD(MovedSectionCount) < ImageInfo.NumberOfSections do
    begin

      MoveSectionsToPage;

      // Переключаемся на следующую страницу

      // Go to the next page
      Inc(pSectionAddr, lpBuffer.RegionSize);
      if VirtualQueryEx(hProcess,
        Pointer(pSectionAddr), lpBuffer, dwLength) <> dwLength then
        RaiseLastOSError;
      Inc(SectionEnd, lpBuffer.RegionSize);
    end;

    ProcesssOverlay;
  finally
    UnMapAndLoad(@ImageInfo);
  end;
end;

procedure TSimpleMemoryMap.DisplayStateAndProtect(var AItem: TListItemData;
  const lpBuffer: TMemoryBasicInformation);
begin
  with AItem do
  begin
    case lpBuffer.State of
      MEM_FREE: PageType := ptFree;
      MEM_RESERVE: PageType := ptReserved;
      MEM_COMMIT:
      case lpBuffer._Type of
        MEM_IMAGE: PageType := ptImage;
        MEM_MAPPED: PageType := ptMapped;
        MEM_PRIVATE: PageType := ptPrivate;
      end;
    end;
    Access := ExtractAccessString(lpBuffer.Protect);
    IAccess := ExtractInitialAccessString(lpBuffer.AllocationProtect);
  end;
  if FLastPageType <> AItem.PageType then
  begin
    FLastPageType := AItem.PageType;
    FGrayed := not FGrayed;
  end;
  AItem.Grayed := FGrayed;
end;

function TSimpleMemoryMap.ExtractAccessString(const Value: DWORD): string;
const
  PAGE_WRITECOMBINE = $400;
begin
  Result := ExtractInitialAccessString(Value);
  if (Value and PAGE_GUARD) = PAGE_GUARD then
    Result := Result + ', Guarded';
  if (Value and PAGE_NOCACHE) = PAGE_NOCACHE then
    Result := Result + ', No cache';
  if (Value and PAGE_WRITECOMBINE) = PAGE_WRITECOMBINE then
    Result := Result + ', Write Combine';
end;

function TSimpleMemoryMap.ExtractInitialAccessString(const Value: DWORD
  ): string;
begin
  Result := '';
  if (Value and PAGE_EXECUTE) = PAGE_EXECUTE then Result := 'E';
  if (Value and PAGE_EXECUTE_READ) = PAGE_EXECUTE_READ then Result := 'RE';
  if (Value and PAGE_EXECUTE_READWRITE) = PAGE_EXECUTE_READWRITE then
     Result := 'RWE';
  if (Value and PAGE_EXECUTE_WRITECOPY) = PAGE_EXECUTE_WRITECOPY then
    Result := 'RWE';
  if (Value and PAGE_NOACCESS) = PAGE_NOACCESS then Result := 'No access';
  if (Value and PAGE_READONLY) = PAGE_READONLY then Result := 'R';
  if (Value and PAGE_READWRITE) = PAGE_READWRITE then Result := 'RW';
  if (Value and PAGE_WRITECOPY) = PAGE_WRITECOPY then Result := 'W';
  if (Result = '') and (Value <> 0) then
    Result := 'Unknown: 0x' + IntToHex(Value, 8);
end;

procedure TSimpleMemoryMap.FillPEBInfo(const hProcess: THandle);
const
  ProcessBasicInformation = 0;
  ProcessWow64Information = 26;

var
  pProcBasicInfo: PROCESS_BASIC_INFORMATION;
  ReturnLength: NativeUInt;
  ProcessParameters: RTL_USER_PROCESS_PARAMETERS;
  SBI: TSystemInfo;
  PPointerData, PebWow64BaseAddress: Pointer;
  {$IFDEF WIN64}
  PebWow64: TWOW64_PEB;
  {$ENDIF}
  Peb: TPEB;
begin
  ReturnLength := 0;

  ReturnLength := 0;
  if NtQueryInformationProcess(hProcess, ProcessBasicInformation,
    @pProcBasicInfo, SizeOf(PROCESS_BASIC_INFORMATION),
    @ReturnLength) <> STATUS_SUCCESS then
    RaiseLastOSError;

  AddNewData('PEB', pProcBasicInfo.PebBaseAddress);

  if not ReadProcessMemory(hProcess, pProcBasicInfo.PebBaseAddress,
    @Peb, SizeOf(TPEB), ReturnLength) then
    RaiseLastOSError;

  {$IFDEF WIN64}
  if not FIs64Process then
  begin

    if NtQueryInformationProcess(hProcess, ProcessWow64Information,
      @PebWow64BaseAddress, SizeOf(ULONG_PTR),
      @ReturnLength) <> STATUS_SUCCESS then
      RaiseLastOSError;

    AddNewData('PEB (Wow64)', PebWow64BaseAddress);

    if not ReadProcessMemory(hProcess, PebWow64BaseAddress,
      @PebWow64, SizeOf(TWOW64_PEB), ReturnLength) then
      RaiseLastOSError;

    AddNewData('LoaderData (Wow64)', PebWow64.LoaderData);
    AddNewData('ProcessParameters (Wow64)', PebWow64.ProcessParameters);
    if PebWow64.ReadOnlySharedMemoryBase <> Cardinal(Peb.ReadOnlySharedMemoryBase) then
      AddNewData('ReadOnlySharedMemoryBase (Wow64)', PebWow64.ReadOnlySharedMemoryBase);
    if PebWow64.HotpatchInformation <> Cardinal(Peb.HotpatchInformation) then
      AddNewData('HotpatchInformation (Wow64)', PebWow64.HotpatchInformation);

    PPointerData := nil;
    if not ReadProcessMemory(hProcess, Pointer(PebWow64.ReadOnlyStaticServerData),
      @PPointerData, 4, ReturnLength) then
      RaiseLastOSError;

    AddNewData('ReadOnlyStaticServerData (Wow64)', PPointerData);

    if PebWow64.AnsiCodePageData <> Cardinal(Peb.AnsiCodePageData) then
      AddNewData('AnsiCodePageData (Wow64)', PebWow64.AnsiCodePageData);
    if PebWow64.OemCodePageData <> Cardinal(Peb.OemCodePageData) then
      AddNewData('OemCodePageData (Wow64)', PebWow64.OemCodePageData);
    if PebWow64.UnicodeCaseTableData <> Cardinal(Peb.UnicodeCaseTableData) then
      AddNewData('UnicodeCaseTableData (Wow64)', PebWow64.UnicodeCaseTableData);

    if PebWow64.GdiSharedHandleTable <> Cardinal(Peb.GdiSharedHandleTable) then
      AddNewData('GdiSharedHandleTable (Wow64)', PebWow64.GdiSharedHandleTable);
    if PebWow64.ProcessStarterHelper <> Cardinal(Peb.ProcessStarterHelper) then
      AddNewData('ProcessStarterHelper (Wow64)', PebWow64.ProcessStarterHelper);
    if PebWow64.PostProcessInitRoutine <> Cardinal(Peb.PostProcessInitRoutine) then
      AddNewData('PostProcessInitRoutine (Wow64)', PebWow64.PostProcessInitRoutine);
    if PebWow64.TlsExpansionBitmap <> Cardinal(Peb.TlsExpansionBitmap) then
      AddNewData('TlsExpansionBitmap (Wow64)', PebWow64.TlsExpansionBitmap);

    // Compatilibity
    if PebWow64.pShimData <> Cardinal(Peb.pShimData) then
      AddNewData('pShimData (Wow64)', PebWow64.pShimData);
    if PebWow64.AppCompatInfo <> Cardinal(Peb.AppCompatInfo) then
     AddNewData('AppCompatInfo (Wow64)', PebWow64.AppCompatInfo);

    if PebWow64.ActivationContextData <> Cardinal(Peb.ActivationContextData) then
      AddNewData('ActivationContextData (Wow64)', PebWow64.ActivationContextData);
    if PebWow64.ProcessAssemblyStorageMap <> Cardinal(Peb.ProcessAssemblyStorageMap) then
      AddNewData('ProcessAssemblyStorageMap (Wow64)', PebWow64.ProcessAssemblyStorageMap);
    if PebWow64.SystemDefaultActivationContextData <> Cardinal(Peb.SystemDefaultActivationContextData) then
      AddNewData('SystemDefaultActivationContextData (Wow64)', PebWow64.SystemDefaultActivationContextData);
    if PebWow64.SystemAssemblyStorageMap <> Cardinal(Peb.SystemAssemblyStorageMap) then
      AddNewData('SystemAssemblyStorageMap (Wow64)', PebWow64.SystemAssemblyStorageMap);

    if PebWow64.ApiSetMap <> Cardinal(Peb.ApiSetMap) then
      AddNewData('ApiSetMap (Wow64)', PebWow64.ApiSetMap);
  end;
  {$ENDIF}

  AddNewData('LoaderData', Peb.LoaderData);
  AddNewData('ProcessParameters', Peb.ProcessParameters);
  AddNewData('ReadOnlySharedMemoryBase', Peb.ReadOnlySharedMemoryBase);
  AddNewData('HotpatchInformation', Peb.HotpatchInformation);

  PPointerData := nil;
  if not ReadProcessMemory(hProcess, Peb.ReadOnlyStaticServerData,
    @PPointerData, 4, ReturnLength) then
    RaiseLastOSError;

  AddNewData('ReadOnlyStaticServerData', PPointerData);

  AddNewData('AnsiCodePageData', Peb.AnsiCodePageData);
  AddNewData('OemCodePageData', Peb.OemCodePageData);
  AddNewData('UnicodeCaseTableData', Peb.UnicodeCaseTableData);

  AddNewData('GdiSharedHandleTable', Peb.GdiSharedHandleTable);
  AddNewData('ProcessStarterHelper', Peb.ProcessStarterHelper);
  AddNewData('PostProcessInitRoutine', Peb.PostProcessInitRoutine);
  AddNewData('TlsExpansionBitmap', Peb.TlsExpansionBitmap);

  AddNewData('pShimData', Peb.pShimData);
  AddNewData('AppCompatInfo', Peb.AppCompatInfo);

  AddNewData('ActivationContextData', Peb.ActivationContextData);
  AddNewData('ProcessAssemblyStorageMap', Peb.ProcessAssemblyStorageMap);
  AddNewData('SystemDefaultActivationContextData', Peb.SystemDefaultActivationContextData);
  AddNewData('SystemAssemblyStorageMap', Peb.SystemAssemblyStorageMap);

  AddNewData('ApiSetMap', Peb.ApiSetMap);

  if not ReadProcessMemory(hProcess, Peb.ProcessParameters,
    @ProcessParameters, SizeOf(RTL_USER_PROCESS_PARAMETERS), ReturnLength) then
    RaiseLastOSError;
  AddNewData('Process Environments', ProcessParameters.Environment);

  SBI := Default(TSystemInfo);
  GetSystemInfo(SBI);
  AddNewData('KE_USER_SHARED_DATA',
    NativeUInt(SBI.lpMaximumApplicationAddress) and $7FFF0000);
end;

procedure TSimpleMemoryMap.FillProcessMemoryMap(AList: TList<TListItemData>);
const
  MM_HIGHEST_USER_ADDRESS32 = $7FFEFFFF;
  MM_HIGHEST_USER_ADDRESS64 = $7FFFFFFF0000;
var
  hProcess: THandle;
  pSectionAddr, pMaxAddrVA: NativeUInt;
  lpBuffer: TMemoryBasicInformation;
  dwLength: DWORD;
  OwnerName: string;
  AItem: TListItemData;
  I: Integer;
begin
  FList := AList;
  FList.Clear;
  SetLength(FAdvData, 0);
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
    False, FCurrentPid);
  if hProcess = 0 then
    RaiseLastOSError;
  try

    // Перечисляем информацию из блока окружения процесса

    // Fill the information from the process environment block
    FillPEBInfo(hProcess);

    // Перечисляем все нити процесса,
    // и запоминаем адреса их стэка и TLS секций

    // Enumerate all threads of the process,
    // and memorize the addresses of their stack and TLS sections
    FillThreadsInfo(hProcess);

    dwLength := SizeOf(TMemoryBasicInformation);
    lpBuffer := Default(TMemoryBasicInformation);

    // Перебираем в цикле все страницы памяти от нулевой,
    // до максимально доступной пользователю

    // Loop through all memory pages from zero,
    // to the maximum available to the user
    pSectionAddr := 0;
    if FIs64Process then
      pMaxAddrVA := MM_HIGHEST_USER_ADDRESS64
    else
      pMaxAddrVA := MM_HIGHEST_USER_ADDRESS32;
    while pSectionAddr < pMaxAddrVA do
    begin
      if VirtualQueryEx(hProcess,
        {%H-}Pointer(pSectionAddr), lpBuffer, dwLength) <> dwLength then
        RaiseLastOSError;

      // Проверка, принадлежит ли страница какому-либо образу?

      // Checking to see if the page belongs to any image?
      OwnerName := '';
      if GetMappedFileName(hProcess, lpBuffer.BaseAddress, OwnerName) then
      begin

        // на страницу отмаплен РЕ файл,
        // выводим данные по нему в отдельной процедуре

        // the PE file is mapped to the page,
        // output its data in a separate procedure
        if DisplayImageData(hProcess, OwnerName, lpBuffer, pSectionAddr) then
        begin
          Inc(FImageCount);
          Continue;
        end;
      end;

      Inc(pSectionAddr, lpBuffer.RegionSize);

      // Добавляем данные по странице в ListView

      // Add page data to ListView

      AItem := Default(TListItemData);
      with AItem do
      begin
        AddrVA := Int64(lpBuffer.BaseAddress);
        Size := lpBuffer.RegionSize;
        EndAddrVA := AddrVA + Size;
        DisplayStateAndProtect(AItem, lpBuffer);
        for I := 0 to Length(FAdvData) - 1 do
        begin
          if (FAdvData[I].Addr >= {%H-}NativeUInt(lpBuffer.BaseAddress)) and
            (FAdvData[I].Addr < {%H-}NativeUInt(lpBuffer.BaseAddress) +
            lpBuffer.RegionSize) then
          begin
            if Contains = '' then
            begin
              Contains := FAdvData[I].Description;
              PageType := FAdvData[I].PageType;
            end
            else
              Contains := Contains + ', ' + FAdvData[I].Description;
          end;
        end;
        if OwnerName <> '' then
          MapedFile := NormalizePath(OwnerName);
      end;
      FList.Add(AItem);
    end;

  finally
    CloseHandle(hProcess);
  end;
end;

procedure TSimpleMemoryMap.FillThreadsInfo(const hProcess: THandle);
const
  THREAD_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3FF;
  ThreadBasicInformation = 0;
var
  hSnap, hThread: THandle;
  ThreadEntry: TThreadEntry32;
  TBI: TThreadBasicInformation;
  TIB: NT_TIB;
  lpNumberOfBytesRead: PtrUInt;
  MainAdded: Boolean;
begin
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if hSnap <> INVALID_HANDLE_VALUE then
  try
    ThreadEntry.dwSize := SizeOf(TThreadEntry32);
    MainAdded := False;
    if Thread32First(hSnap, ThreadEntry) then
    repeat
      if ThreadEntry.th32OwnerProcessID = FCurrentPid then
      begin
        hThread := OpenThread(THREAD_ALL_ACCESS,
          False, ThreadEntry.th32ThreadID);
        if hThread <> 0 then
        try
          if NtQueryInformationThread(hThread, ThreadBasicInformation, @TBI,
            SizeOf(TThreadBasicInformation), nil) = 0 then
          begin
            lpNumberOfBytesRead := 0;
            if not ReadProcessMemory(hProcess,
              TBI.TebBaseAddress, @TIB, SizeOf(NT_TIB),
              lpNumberOfBytesRead) then
              RaiseLastOSError;
            if not MainAdded then
            begin
              AddNewData('Stack of main thread', TIB.StackLimit, ptThread);
              MainAdded := True;
            end
            else
              AddNewData('Stack of thread: ' +
                IntToStr(ThreadEntry.th32ThreadID), TIB.StackLimit, ptThread);
            AddNewData('TEB (' +
              IntToStr(ThreadEntry.th32ThreadID) + ')', TBI.TebBaseAddress, ptThread);
          end;

        finally
          CloseHandle(hThread);
        end;
      end;
    until not Thread32Next(hSnap, ThreadEntry);
  finally
     CloseHandle(hSnap);
  end;
end;

function TSimpleMemoryMap.GetContainsDirectory(const Code, Data: Boolean;
  const ImageInfo: LOADED_IMAGE; const SectionAddr, SectionSize: NativeUInt
  ): string;
const
  DirectoryStr: array [0..14] of String =
    ('export', 'import', 'resource', 'exception',
    'security', 'basereloc', 'debug', 'copyright',
    'globalptr', 'tls', 'load_config', 'bound_import',
    'iat', 'delay_import', 'com');
var
  dwDirSize: DWORD;
  ish: PImageSectionHeader;
  I: Integer;
  Done: Boolean;
begin
  if Code then
    Result := 'code'
  else
    Result := '';
  if Data then
    if Result = '' then
      Result := 'data'
    else
      Result := Result + ', data';
  for I := 0 to 14 do
  begin
    dwDirSize := 0;
    ish := nil;
    if (ImageDirectoryEntryToDataEx(ImageInfo.MappedAddress,
      False, I, dwDirSize, ish) <> nil) and (ish <> nil) then
    begin
      if I = IMAGE_DIRECTORY_ENTRY_TLS then
        Done := ish^.VirtualAddress = SectionAddr + SectionSize
      else
        Done := ish^.VirtualAddress = SectionAddr;
      if Done then
      begin
        if Result = '' then
          Result := DirectoryStr[I]
        else
          Result := Result + ', ' + DirectoryStr[I];
      end;
    end;
  end;
end;

function TSimpleMemoryMap.IsExecute(const Value: DWORD): Boolean;
const
  IMAGE_SCN_CNT_CODE = $00000020;
  IMAGE_SCN_MEM_EXECUTE = $20000000;
begin
  Result := False;
  if (Value and IMAGE_SCN_CNT_CODE) =
    IMAGE_SCN_CNT_CODE then Result := True;
  if (Value and IMAGE_SCN_MEM_EXECUTE) =
    IMAGE_SCN_MEM_EXECUTE then Result := True;
end;

function TSimpleMemoryMap.IsWrite(const Value: DWORD): Boolean;
const
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = $00000080;
  IMAGE_SCN_MEM_WRITE = $80000000;
begin
  Result := False;
  if (Value and IMAGE_SCN_CNT_UNINITIALIZED_DATA) =
    IMAGE_SCN_CNT_UNINITIALIZED_DATA then Result := True;
  if (Value and IMAGE_SCN_MEM_WRITE) = IMAGE_SCN_MEM_WRITE then
    Result := True;
end;

constructor TSimpleMemoryMap.Create(APid: Cardinal; AIs64Process: Boolean);
begin
  FCurrentPid := APid;
  FIs64Process := AIs64Process;
end;

end.
