////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Linux.MMap.pas
//  * Purpose   : Linux process memory map
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

unit CpuView.Linux.MMap;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LCLIntf, LCLType, SysUtils, Generics.Collections, Classes, LazLogger,
  Types, System.IOUtils,

  FWHexView.Common,
  CpuView.Common,
  CpuView.IntelContext.Types,
  CpuView.Linux;

const
  ELF_MAGIC       = $464C457F;
  EI_NIDENT       = 16;
  ELFCLASS32      = 1;
  ELFCLASS64      = 2;
  PN_XNUM         = $FFFF;
  PT_LOAD         = 1;
  SHN_LORESERVE	  = $FF00;
  SHF_ALLOC       = $2;

type
  Elf32_Addr = UInt32;
  Elf32_Off = UInt32;

  EIdent = record
    e_magic: UInt32;
    e_class: Byte;
    e_data: Byte;
    e_version: Byte;
    e_osabi: Byte;
    e_abiversion: Byte;
    e_padding: array [10..16] of Byte;
  end;

  PElf32_Ehdr = ^Elf32_Ehdr;
  Elf32_Ehdr = record
    e_ident: EIdent;
    e_type: UInt16;
    e_machine: UInt16;
    e_version: UInt32;
    e_entry: Elf32_Addr;
    e_phoff: Elf32_Off;
    e_shoff: Elf32_Off;
    e_flags: UInt32;
    e_ehsize: UInt16;
    e_phentsize: UInt16;
    e_phnum: UInt16;
    e_shentsize: UInt16;
    e_shnum: UInt16;
    e_shstrndx: UInt16;
  end;

  Elf32_Phdr = record
    p_type: UInt32;
    p_offset: Elf32_Off;
    p_vaddr: Elf32_Addr;
    p_paddr: Elf32_Addr;
    p_filesz: UInt32;
    p_memsz: UInt32;
    p_flags: UInt32;
    p_align: UInt32;
  end;

  Elf32_Shdr = record
    sh_name: UInt32;
    sh_type: UInt32;
    sh_flags: UInt32;
    sh_addr: Elf32_Addr;
    sh_offset: Elf32_Off;
    sh_size: UInt32;
    sh_link: UInt32;
    sh_info: UInt32;
    sh_addralign: UInt32;
    sh_entsize: UInt32;
  end;

  Elf64_Addr = Int64;
  Elf64_Off = UInt64;

  PElf64_Ehdr = ^Elf64_Ehdr;
  Elf64_Ehdr = record
    e_ident: EIdent;
    e_type: UInt16;
    e_machine: UInt16;
    e_version: UInt32;
    e_entry: Elf64_Addr;
    e_phoff: Elf64_Off;
    e_shoff: Elf64_Off;
    e_flags: UInt32;
    e_ehsize: UInt16;
    e_phentsize: UInt16;
    e_phnum: UInt16;
    e_shentsize: UInt16;
    e_shnum: UInt16;
    e_shstrndx: UInt16;
  end;

  Elf64_Phdr = record
    p_type: UInt32;
    p_flags: UInt32;
    p_offset: Elf64_Off;
    p_vaddr: Elf64_Addr;
    p_paddr: Elf64_Addr;
    p_filesz: UInt64;
    p_memsz: UInt64;
    p_align: UInt64;
  end;

  Elf64_Shdr = record
    sh_name: UInt32;
    sh_type: UInt32;
    sh_flags: UInt64;
    sh_addr: Elf64_Addr;
    sh_offset: Elf64_Off;
    sh_size: UInt64;
    sh_link: UInt32;
    sh_info: UInt32;
    sh_addralign: UInt64;
    sh_entsize: UInt64;
  end;

  TElfSectionHeader = record
    DisplayName: string;
    Hdr: Elf64_Shdr;
  end;

  TProgramHeaders = array of Elf64_Phdr;
  TSections = array of TElfSectionHeader;

  { TSimpleElf }

  TSimpleElf = class
  private
    FHeader: Elf64_Ehdr;
    FHeaderSize: Int64;
    FImageBase, FLoadedImageBase: Elf64_Addr;
    FImagePath: string;
    FImage64: Boolean;
    FProgramHeaders: TProgramHeaders;
    FSections: TSections;
    procedure LoadFromImage;
    function LoadHeader(Raw: TStream): Boolean;
    function LoadProgramHeaders(Raw: TStream): Boolean;
    function LoadSections(Raw: TMemoryStream): Boolean;
  public
    constructor Create(const ImagePath: string; ImageBase: Elf64_Addr);
    destructor Destroy; override;
    function Rebase(Value: Int64): Int64;
    property Header: Elf64_Ehdr read FHeader;
    property ProgramHeaders: TProgramHeaders read FProgramHeaders;
    property Sections: TSections read FSections;
  end;

  { TSimpleMemoryMap }

  TSimpleMemoryMap = class
  private
    FCurrentPid: Cardinal;
    FImageCount: Integer;
    FIs64Process: Boolean;
    FGrayed: Boolean;
    FLastPageType: TPageType;
    FPages: TList<TPageData>;
    procedure AddContains(var AItem: TPageData; const Value: string); overload;
    procedure AddContains(var AItem: TPageData; const ASection: TElfSectionHeader); overload;
    procedure CheckPage(const page: TPageData);
    function DisplayElf(mmap: TMemoryBasicInformationList; var Idx: Integer): Boolean;
    function ExtractAccess(const mitm: TMemoryBasicInformation): string;
    procedure GetPageType(const mitm: TMemoryBasicInformation;
      var AItem: TPageData; AThreadID: Cardinal);
    procedure FillMainThread;
    procedure FillThreads;
    procedure FillThread(AThreadID: Cardinal);
  public
    constructor Create(APid: Cardinal; AIs64Process: Boolean);
    procedure FillProcessMemoryMap(APages: TList<TPageData>);
  end;

implementation

{ TSimpleElf }

procedure TSimpleElf.LoadFromImage;
var
  Raw: TMemoryStream;
begin
  Raw := TMemoryStream.Create;
  try
    try
      Raw.LoadFromFile(FImagePath);
    except
      on E: Exception do
      begin
        DebugLn('Image load error "', FImagePath, '". ', E.ClassName, ': ', E.Message);
        Exit;
      end;
    end;

    if not LoadHeader(Raw) then Exit;
    LoadSections(Raw);
    LoadProgramHeaders(Raw);

  finally
    Raw.Free;
  end;
end;

function TSimpleElf.LoadHeader(Raw: TStream): Boolean;
var
  TmpHeader: Elf32_Ehdr;
begin
  Result := False;
  Raw.ReadBuffer(FHeader.e_ident, EI_NIDENT);
  if FHeader.e_ident.e_magic <> ELF_MAGIC then
    Exit;
  if not (FHeader.e_ident.e_class in [ELFCLASS32, ELFCLASS64]) then
  begin
    DebugLn(Format('File "%s" is an unknown class (%d)', [FImagePath, FHeader.e_ident.e_class]));
    Exit;
  end;
  FImage64 := FHeader.e_ident.e_class = ELFCLASS64;
  if FImage64 then
  begin
    Raw.ReadBuffer(FHeader.e_type, SizeOf(FHeader) - EI_NIDENT);
    FHeaderSize := SizeOf(Elf64_Ehdr);
  end
  else
  begin
    Raw.ReadBuffer(TmpHeader.e_type, SizeOf(Elf32_Ehdr) - EI_NIDENT);
    FHeader.e_type := TmpHeader.e_type;
    FHeader.e_machine := TmpHeader.e_machine;
    FHeader.e_version := TmpHeader.e_version;
    FHeader.e_entry := TmpHeader.e_entry;
    FHeader.e_phoff := TmpHeader.e_phoff;
    FHeader.e_shoff := TmpHeader.e_shoff;
    FHeader.e_flags := TmpHeader.e_flags;
    FHeader.e_ehsize := TmpHeader.e_ehsize;
    FHeader.e_phentsize := TmpHeader.e_phentsize;
    FHeader.e_phnum := TmpHeader.e_phnum;
    FHeader.e_shentsize := TmpHeader.e_shentsize;
    FHeader.e_shnum := TmpHeader.e_shnum;
    FHeader.e_shstrndx := TmpHeader.e_shstrndx;
    FHeaderSize := SizeOf(Elf32_Ehdr);
  end;
  Result := True;
end;

function TSimpleElf.LoadProgramHeaders(Raw: TStream): Boolean;
var
  ACount, Index: UInt32;
  Hdr32: Elf32_Phdr;
begin
  if FImage64 then
    Result := FHeader.e_phentsize = SizeOf(Elf64_Phdr)
  else
    Result := FHeader.e_phentsize = SizeOf(Elf32_Phdr);
  if not Result then Exit;
  Result := FHeader.e_phnum > 0;
  if not Result then Exit;
  Raw.Position := FHeader.e_phoff;
  ACount := FHeader.e_phnum;
  if ACount = PN_XNUM then
  begin
    if Length(FSections) > 0 then
      ACount := FSections[0].Hdr.sh_info
    else
      ACount := 0;
  end;
  if ACount = 0 then Exit(False);
  SetLength(FProgramHeaders, ACount);
  Index := 0;
  FLoadedImageBase := 0;
  while Index < ACount do
  begin
    if FImage64 then
      Raw.ReadBuffer(FProgramHeaders[Index], FHeader.e_phentsize)
    else
    begin
      Raw.ReadBuffer(Hdr32, SizeOf(Hdr32));
      FProgramHeaders[Index].p_type := Hdr32.p_type;
      FProgramHeaders[Index].p_flags := Hdr32.p_flags;
      FProgramHeaders[Index].p_offset := Hdr32.p_offset;
      FProgramHeaders[Index].p_vaddr := Hdr32.p_vaddr;
      FProgramHeaders[Index].p_paddr := Hdr32.p_paddr;
      FProgramHeaders[Index].p_filesz := Hdr32.p_filesz;
      FProgramHeaders[Index].p_memsz := Hdr32.p_memsz;
      FProgramHeaders[Index].p_align := Hdr32.p_align;
    end;
    if FProgramHeaders[Index].p_type = PT_LOAD then
      if FLoadedImageBase = 0 then
        FLoadedImageBase := FProgramHeaders[Index].p_vaddr;

    Inc(Index);
  end;
end;

function TSimpleElf.LoadSections(Raw: TMemoryStream): Boolean;
var
  I: Integer;
  ACount, Index: UInt32;
  StringsLen: UInt32;
  ACountInited: Boolean;
  Hdr32: Elf32_Shdr;
  Strings: PByte;
begin
  if FImage64 then
    Result := FHeader.e_shentsize = SizeOf(Elf64_Shdr)
  else
    Result := FHeader.e_shentsize = SizeOf(Elf32_Shdr);
  if not Result then Exit;
  Result := FHeader.e_shnum > 0;
  if not Result then Exit;
  Raw.Position := FHeader.e_shoff;
  ACount := FHeader.e_shnum;
  ACountInited := ACount < SHN_LORESERVE;
  if ACountInited then
    SetLength(FSections, ACount)
  else
    SetLength(FSections, 1);
  Index := 0;
  while Index < ACount do
  begin
    if FImage64 then
    begin
      Raw.ReadBuffer(FSections[Index].Hdr, FHeader.e_shentsize);
      if not ACountInited then
      begin
        ACountInited := True;
        ACount := FSections[Index].Hdr.sh_size;
        SetLength(FSections, ACount);
      end;
      Inc(Index);
    end
    else
    begin
      Raw.ReadBuffer(Hdr32, SizeOf(Hdr32));
      if not ACountInited then
      begin
        ACountInited := True;
        ACount := Hdr32.sh_size;
        SetLength(FSections, ACount);
      end;
      FSections[Index].Hdr.sh_name := Hdr32.sh_name;
      FSections[Index].Hdr.sh_type := Hdr32.sh_type;
      FSections[Index].Hdr.sh_flags := Hdr32.sh_flags;
      FSections[Index].Hdr.sh_addr := Hdr32.sh_addr;
      FSections[Index].Hdr.sh_offset := Hdr32.sh_offset;
      FSections[Index].Hdr.sh_size := Hdr32.sh_size;
      FSections[Index].Hdr.sh_link := Hdr32.sh_link;
      FSections[Index].Hdr.sh_info := Hdr32.sh_info;
      FSections[Index].Hdr.sh_addralign := Hdr32.sh_addralign;
      FSections[Index].Hdr.sh_entsize := Hdr32.sh_entsize;
      Inc(Index);
    end;
  end;
  StringsLen := Integer(FSections[FHeader.e_shstrndx].Hdr.sh_size);
  if StringsLen <= 0 then Exit(False);
  Strings := PByte(Raw.Memory) + FSections[FHeader.e_shstrndx].Hdr.sh_offset;
  for I := 0 to Index - 1 do
    if (FSections[I].Hdr.sh_name > 0) and (FSections[I].Hdr.sh_name < StringsLen) then
      FSections[I].DisplayName := string(PAnsiChar(Strings + FSections[I].Hdr.sh_name));
end;

constructor TSimpleElf.Create(const ImagePath: string; ImageBase: Elf64_Addr);
begin
  FImageBase := ImageBase;
  FImagePath := ImagePath;
  LoadFromImage;
end;

destructor TSimpleElf.Destroy;
begin
  inherited Destroy;
end;

function TSimpleElf.Rebase(Value: Int64): Int64;
begin
  if FImageBase <> FLoadedImageBase then
    Result := Value + FImageBase
  else
    Result := Value;
end;

{ TSimpleMemoryMap }

procedure TSimpleMemoryMap.AddContains(var AItem: TPageData; const Value: string
  );
begin
  if AItem.ContainsStr = '' then
    AItem.ContainsStr := Value
  else
    AItem.ContainsStr := AItem.ContainsStr + ', ' + Value;
end;

procedure TSimpleMemoryMap.AddContains(var AItem: TPageData;
  const ASection: TElfSectionHeader);
var
  L: Integer;
begin
  L := Length(AItem.Contains);
  if AItem.ContainsStr = '' then
    AItem.ContainsStr := ASection.DisplayName
  else
    AItem.ContainsStr := AItem.ContainsStr + ', ' + ASection.DisplayName;
  SetLength(AItem.Contains, L + 1);
  with AItem.Contains[L] do
  begin
    AddrVA := ASection.Hdr.sh_addr;
    Size := ASection.Hdr.sh_size;
    EndAddrVA := AddrVA + Size;
    Caption := ASection.DisplayName;
  end;
end;

procedure TSimpleMemoryMap.CheckPage(const page: TPageData);
var
  I: Integer;
begin
  if not (page.PageType in [ptHeap, ptThread, ptSystem]) then Exit;
  for I := 0 to FPages.Count - 1 do
  begin
    if (page.AddrVA = FPages[I].AddrVA) and (page.PageType <> FPages[I].PageType) then
    begin
      if page.EndAddrVA = FPages[I].EndAddrVA then
        FPages[I] := page;
    end;
  end;
end;

function TSimpleMemoryMap.DisplayElf(mmap: TMemoryBasicInformationList;
  var Idx: Integer): Boolean;
var
  mitm: TMemoryBasicInformation;
  Elf: TSimpleElf;
  page: TPageData;
  sh: TElfSectionHeader;
  RootIdx, SecVA: Int64;
  I: Integer;
begin
  Result := False;
  mitm := mmap[Idx];
  if not FileExists(mitm.MappedFile) then Exit;
  Elf := TSimpleElf.Create(mitm.MappedFile, mitm.AllocationBase);
  try
    if Length(Elf.ProgramHeaders) = 0 then Exit;
    Result := True;
    FLastPageType := ptImage;
    FGrayed := not FGrayed;
    RootIdx := mitm.InumIdx;
    while Idx < mmap.Count do
    begin
      mitm := mmap[Idx];
      if mitm.InumIdx <> RootIdx then
      begin
        Dec(Idx);
        Break;
      end;
      page := Default(TPageData);
      page.AddrVA := mitm.BaseAddress;
      page.Size := mitm.RegionSize;
      page.EndAddrVA := page.AddrVA + page.Size;
      page.Access := ExtractAccess(mitm);
      page.PageType := ptImage;
      page.Grayed := FGrayed;
      if mitm.AllocationBase = mitm.BaseAddress then
        page.MapedFile := mitm.MappedFile;
      page.Image := ExtractFileName(mitm.MappedFile);
      page.ImageIdx := mitm.InumIdx;
      for I := 0 to Length(Elf.Sections) - 1 do
      begin
        sh := Elf.Sections[I];
        if sh.Hdr.sh_flags and SHF_ALLOC = 0 then
          Continue;
        SecVA := Elf.Rebase(sh.Hdr.sh_addr);
        if (SecVA >= page.AddrVA) and (SecVA < page.EndAddrVA) then
        begin
          sh.Hdr.sh_addr := SecVA;
          AddContains(page, sh);
        end;
      end;
      FPages.Add(page);
      Inc(Idx);
    end;
  finally
    Elf.Free;
  end;
end;

function TSimpleMemoryMap.ExtractAccess(const mitm: TMemoryBasicInformation): string;
begin
  Result := '';
  if mitm.Read then
    Result := 'R';
  if mitm.Write then
    Result := Result + 'W';
  if mitm.Execute then
    Result := Result + 'E';
  if mitm.Shared then
    Result := Result + ', shared';
  if Result = '' then
    Result := 'No access';
end;

procedure TSimpleMemoryMap.GetPageType(const mitm: TMemoryBasicInformation;
  var AItem: TPageData; AThreadID: Cardinal);
var
  pt: TPageType;
begin
  try
    pt := ptFree;
    if mitm.MappedFile <> '' then
    begin
      pt := ptMapped;
      Exit;
    end;

    if mitm.Hint <> '' then
    begin
      case IndexOfString(mitm.Hint, ['[stack]', '[heap]', '[vvar]', '[vdso]']) of
        0:
        begin
          AddContains(AItem, 'Stack of main thread');
          pt := ptThread;
        end;
        1:
        begin
          AddContains(AItem, 'Heap');
          pt := ptHeap;
        end;
        2:
        begin
          AddContains(AItem, 'Virtual variables [vvar]');
          pt := ptSystem;
        end;
        3:
        begin
          AddContains(AItem, 'Virtual dynamic shared object [vdso]');
          pt := ptSystem;
        end;
      else
        AddContains(AItem, mitm.Hint);
        pt := ptSystem;
      end;
    end;
    if pt <> ptFree then Exit;
    if not (mitm.Read or mitm.Write or mitm.Execute or mitm.Shared) then
      pt := ptReserved
    else
      pt := ptPrivate;
  finally
    AItem.PageType := pt;
    if pt <> FLastPageType then
    begin
      FLastPageType := pt;
      FGrayed := not FGrayed;
    end;
    AItem.Grayed := FGrayed;
  end;
end;

procedure TSimpleMemoryMap.FillMainThread;
var
  I: Integer;
  mmap: TMemoryBasicInformationList;
  mitm: TMemoryBasicInformation;
  page, emptyPage: TPageData;
begin
  page := Default(TPageData);
  emptyPage := Default(TPageData);
  mmap := LoadVirtualMemoryInformation(FCurrentPid, FCurrentPid, False);
  try
    I := 0;
    while I < mmap.Count do
    begin
      mitm := mmap[I];
      if page.EndAddrVA < mitm.BaseAddress then
      begin
        emptyPage.AddrVA := page.EndAddrVA;
        emptyPage.EndAddrVA := mitm.BaseAddress;
        emptyPage.Size := emptyPage.EndAddrVA - emptyPage.AddrVA;
        FLastPageType := ptFree;
        FGrayed := not FGrayed;
        emptyPage.Grayed := FGrayed;
        FPages.Add(emptyPage);
      end;
      if not DisplayElf(mmap, I) then
      begin
        page := Default(TPageData);
        page.AddrVA := mitm.BaseAddress;
        page.Size := mitm.RegionSize;
        page.EndAddrVA := page.AddrVA + page.Size;
        page.Access := ExtractAccess(mitm);
        GetPageType(mitm, page, FCurrentPid);
        page.MapedFile := mitm.MappedFile;
        FPages.Add(page);
      end
      else
        page := FPages.Last;
      Inc(I);
    end;
  finally
    mmap.Free;
  end;
end;

procedure TSimpleMemoryMap.FillThreads;
var
  Path, TidPath: string;
  Dirs: TStringDynArray;
  Tid: Cardinal;
  I: Integer;
begin
  Path := Format('/proc/%d/task/', [FCurrentPid]);
  Dirs := TDirectory.GetDirectories(Path, '*', TSearchOption.soTopDirectoryOnly);
  for I := 0 to Length(Dirs) - 1 do
  begin
    TidPath := Dirs[I];
    Delete(TidPath, 1, Length(Path));
    if TryStrToUInt(TidPath, Tid) and (Tid <> FCurrentPid) then
      FillThread(Tid);
  end;
end;

procedure TSimpleMemoryMap.FillThread(AThreadID: Cardinal);
var
  Ctx: TIntelThreadContext;
  I: Integer;
  itm: TPageData;
begin
  if FIs64Process then
    Ctx := GetIntelContext(AThreadID)
  else
    Ctx := GetIntelWow64Context(AThreadID);
  for I := 0 to FPages.Count - 1 do
  begin
    itm := FPages[I];
    if (Ctx.Rsp >= itm.AddrVA) and (Ctx.Rsp < itm.EndAddrVA) then
    begin
      AddContains(itm, 'Stack of thread: ' + IntToStr(AThreadID));
      FPages[I] := itm;
    end;
  end;
end;

constructor TSimpleMemoryMap.Create(APid: Cardinal; AIs64Process: Boolean);
begin
  FCurrentPid := APid;
  FIs64Process := AIs64Process;
  FGrayed := True;
end;

procedure TSimpleMemoryMap.FillProcessMemoryMap(APages: TList<TPageData>);
begin
  FPages := APages;
  FPages.Clear;
  FillMainThread;
  FillThreads;
end;

end.
