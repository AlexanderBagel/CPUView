////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.Windows.Pdb.pas
//  * Purpose   : Classes for working directly with debug PDB files
//  *           : in RAW mode, replacing dbghlp.dll and symsrv.dll
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2026.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//  *
//  * SPDX-License-Identifier: MIT
//  * See LICENSE file in the project root for full license information.
//  *
//  ****************************************************************************
//

// https://github.com/microsoft/microsoft-pdb/
// https://github.com/luser/dump_syms
// https://llvm.org/docs/PDB/

unit CpuView.Windows.Pdb;

interface

{$MODE Delphi}
{$I CpuViewCfg.inc}

uses
  Windows,
  Classes,
  SysUtils,
  Math,
  StrUtils,
  DateUtils,
  bufstream,
  Generics.Collections,
  Generics.Defaults,
  syncobjs,
  WinInet,
  WinHTTP,
  ImageHlp,
  fpjson,
  FWHexView.Common;

  // [ru] выравнивание строго восемь, иначе поплывут размеры структур
  // [en] the alignment must be exactly eight, otherwise, the record sizes will be incorrect.

{$A8}

const
  // [ru] Фиксированные номера потоков
  // [en] Fixed stream numbers
  // https://llvm.org/docs/PDB/index.html#stream-layout
  PDB_STREAM_OLD_MSF_DIRECTORY  = 0;
  PDB_STREAM_PDB_INFO           = 1;
  PDB_STREAM_TPI                = 2;
  PDB_STREAM_DBI                = 3;
  PDB_STREAM_IPI                = 4;

  DBI_INVALID_STREAM_INDEX      = Word($FFFF);

  GSI_HASH_SIGNATURE = DWORD($FFFFFFFF);
  GSI_HASH_VERSION = DWORD($EFFE0000) + 19990810;

  // https://llvm.org/docs/PDB/CodeViewSymbols.html
  CV_SYMBOL_KIND_S_PUB32 = Word($110E);

  // https://github.com/microsoft/microsoft-pdb/blob/master/include/cvinfo.h#L3685
  CVPSF_CODE         = DWORD(1 shl 0);
  CVPSF_FUNCTION     = DWORD(1 shl 1);
  CVPSF_MANAGED_CODE = DWORD(1 shl 2);
  CVPSF_MANAGED_IL   = DWORD(1 shl 3);

  PdbInfoFile = '.pdb-info';

type
  L = UnicodeString;
  CB = Long;   // count of bytes
  UPN = Int32; // universal page no.

  // [ru] Структура MSF контейнера
  // [en] Structure of an MSF Container
  // https://github.com/microsoft/microsoft-pdb/

  // https://github.com/microsoft/microsoft-pdb/blob/master/PDB/msf/msf.cpp#L282
  SI_PERSIST = record
    cb: CB;
    mpspnpn: Int32;
  end;

  // https://github.com/microsoft/microsoft-pdb/blob/master/PDB/msf/msf.cpp#L946
  BIGMSF_HDR = record
    Magic: array [0..29] of Byte;
    cbPg: CB;           // page size
    pnFpm: UPN;         // page no. of valid FPM
    pnMac: UPN;         // current no. of pages
    siSt: SI_PERSIST;   // stream table stream info
    mpspnpnSt: array [0..0] of Int32;
  end;

  // https://llvm.org/docs/_sources/PDB/PdbStream.rst.txt
  PPdbStreamHeader = ^TPdbStreamHeader;
  TPdbStreamHeader = record
    Version: DWORD;

    // [ru] вот тут якобы должно лежать DWORD(-1), но его там нет
    // [en] a DWORD (-1) is supposed to be here, but it's not there
    Signature: DWORD;

    // [ru] у LLVM это Age, но по факту там лежит точно не Age
    // [en] in LLVM, it's called "Age", but in reality, it's definitely not "Age"
    Age: DWORD;

    // [ru] и только вот это поле действительно хранит GUID файла
    // [en] and this is the only field that actually stores the file's GUID
    UinqueId: TGUID;
  end;


  // https://llvm.org/docs/PDB/DbiStream.html
  // https://github.com/microsoft/microsoft-pdb/blob/master/PDB/dbi/dbi.h#L124
  PDbiStreamHeader = ^TDbiStreamHeader;
  TDbiStreamHeader = record
    VersionSignature: Integer;
    VersionHeader: DWORD;
    Age: DWORD;
    GlobalStreamIndex: SmallInt;
    BuildNumber: Word;
    PublicStreamIndex: SmallInt;
    PdbDllVersion: Word;
    SymbolRecordStreamIndex: SmallInt;
    PdbDllRbld: Word;
    ModuleInfoSize: Integer;
    SectionContributionSize: Integer;
    SectionMapSize: Integer;
    SourceInfoSize: Integer;
    TypeServerMapSize: Integer;
    MfcTypeServerIndex: DWORD;
    OptionalDebugHeaderSize: Integer;
    EcSubstreamSize: Integer;
    Flags: Word;
    Machine: Word;
    Padding: DWORD;
  end;

  // https://github.com/luser/dump_syms
  // https://llvm.org/docs/PDB/DbiStream.html#optional-debug-header-stream
  PDbiDebugHeader = ^TDbiDebugHeader;
  TDbiDebugHeader = record
    FpoDataStreamIndex: Word;
    ExceptionDataStreamIndex: Word;
    FixupDataStreamIndex: Word;
    OmapToSrcDataStreamIndex: Word;
    OmapFromSrcDataStreamIndex: Word;
    SectionHeaderStreamIndex: Word;
    TokenDataStreamIndex: Word;
    XdataStreamIndex: Word;
    PdataStreamIndex: Word;
    NewFpoDataStreamIndex: Word;
    OriginalSectionHeaderDataStreamIndex: Word;
  end;

  // Public Symbol Stream
  // https://llvm.org/docs/PDB/PublicStream.html
  PPublicStreamHeader = ^TPublicStreamHeader;
  TPublicStreamHeader = record
    SymHashSize: DWORD;
    AddrMapSize: DWORD;
    ThunkCount: DWORD;
    SizeOfThunk: DWORD;
    IsectThunkTable: Word;
    Padding: Word;
    OffsetThunkTable: DWORD;
    SectionCount: Word;
    Padding2: Word;
  end;

  PGsiHashTableHeader = ^TGsiHashTableHeader;
  TGsiHashTableHeader = record
    Signature: DWORD;
    Version: DWORD;
    HashRecordsSize: DWORD;
    BucketsSize: DWORD;
  end;

  PPdbHashRecord = ^TPdbHashRecord;
  TPdbHashRecord = record
    // [ru] смещение в Symbol Record Stream, хранится СО СДВИГОМ +1
    // [en] offset in the Symbol Record Stream, stored with an OFFSET OF +1
    Offset: DWORD;
    CRef: DWORD;
  end;

  // https://github.com/microsoft/microsoft-pdb/blob/master/include/cvinfo.h#L3696
  PPUBSYM32 = ^PUBSYM32;
  // [ru] тут packed, т.к. сразу за seg идут еще данные
  // [en] it's packed here, because there's more data right after the seg.
  PUBSYM32 = packed record
    reclen: Word;
    rectyp: Word;
    pubsymflags: DWORD; // CVPSF_xxx
    off: DWORD;
    // [ru] номера секций идут от единицы
    // [en] the section numbers start with one
    seg: Word;
    // [ru] Дальше следует Name переменной длины
    // [en] Next comes a variable-length “Name”
  end;

  EPdbException = class(Exception);

  TPages = array of UPN;
  TSectionsRva = array of DWORD;

  TPdbState = (
    // [ru] отладочные символы не найдены локально, но ещё не проверяли удалённо
    // [en] debug symbols were not found locally, but we haven't checked remotely yet
    psNotFoundLocally,

    // [ru] опросили все symsrv, символов нигде нет
    // [en] all symbol servers were queried, symbols were not found
    psNotFoundAnywhere,

    // [ru] символы локально отсутствуют, но их можно загрузить
    // [en] symbols are not available locally, but they can be downloaded
    psAvailableForLoading,

    // [ru] символы загружаются и еще не готовы к работе
    // [en] symbols are loading and are not yet ready for use
    psLoading,

    // [ru] символы загрузились, но секции PE файла отсутствуют
    // [en] symbols have been loaded, but the PE sections of the file are missing
    psNoSections,

    // [ru] ошибка чтения PDB файла
    // [en] failed to read the PDB file
    psBroken,

    // [ru] символы загружены и готовы к работе
    // [en] symbols have been loaded and are ready to use
    psReady
  );

  TPdbSymbol = record
    Name: string;
    Rva: DWORD;
    Section: Integer;
    IsFunc: Boolean;
  end;

  TPdbKey = packed record
    Uid: TGUID;
    Age: DWORD;
    Name: string;
  end;

  TPdbInfo = record
    DownloadedAt: TDateTime;
    DownloadedBy,
    RequestedForBinaryPath: string;
    BinaryGuid: TGUID;
    BinaryAge: DWORD;
  end;

  TPdb = class
  strict private
    FAge: DWORD;
    FHeader: BIGMSF_HDR;
    FMaxSectionIdx: Integer;
    FBinaryPath, FRelativePath, FPdbName, FLocalPath, FRemotePath: string;
    FSectionsRva: TSectionsRva;
    FState: TPdbState;
    FStreamPages: array of TPages;
    FStreamSizes: array of CB;
    FSymbols: TListEx<TPdbSymbol>;
    FUnicalId: TGUID;
    procedure CheckStream(AStream: TStream);
    function GetStream(AStream: TStream; Index: Integer): TBytes;
    function GetSymbol(Index: Integer): TPdbSymbol;
    procedure LoadSectionData(AStream: TStream; const Dbi: TBytes);
    procedure LoadStreams(AStream: TStream);
    procedure LoadSymbols(AStream: TStream);
    function PagesCount(Value: CB): Integer;
    function ReadPages(AStream: TStream; const PageList: TPages; TotalSize: Int64): TBytes;
    function ReadSymbolName(const Data: TBytes; NameStart, RecordEnd: Integer): string;
    procedure UpdateUnicalId(AStream: TStream);
  protected
    procedure SetLocalPath(const Value: string);
    procedure UpdateState(NewState: TPdbState);
    property RelativePath: string read FRelativePath write FRelativePath;
    property RemotePath: string read FRemotePath write FRemotePath;
  public
    constructor Create; overload;
    constructor Create(APdbKey: TPdbKey); overload;
    destructor Destroy; override;
    function Count: Integer;
    function GetRemotePath: string;
    function Key: TPdbKey;
    procedure LoadFromFile(const FilePath: string);
    procedure LoadFromStream(AStream: TStream);
    procedure Reset;
    procedure UpdateSections(const Value: TSectionsRva);
    property Age: DWORD read FAge;
    property BinaryPath: string read FBinaryPath write FBinaryPath;
    property LocalPath: string read FLocalPath;
    property PdbName: string read FPdbName;
    property State: TPdbState read FState;
    property Symbol[Index: Integer]: TPdbSymbol read GetSymbol; default;
    property UnicalId: TGUID read FUnicalId;
  end;

  TProxyKind = (pkNone, pkDefault, pkUserSettings, pkUserSettingsWithAuthority);
  TProxyAuthKind = (pakAuto, pakBasic, pakNegotiate, pakNTLM);

  TProxySettings = record
    Kind: TProxyKind;
    AuthKind: TProxyAuthKind;
    Host: string;
    Port: Integer;
    Login: string;
    Password: string;
  end;

  ESymSrvException = class(Exception);

  TSymSrvHandleChain = class
  private
    FSession, FConnect, FRequest: HINTERNET;
  public
    destructor Destroy; override;
    property Session: HINTERNET read FSession write FSession;
    property Connect: HINTERNET read FConnect write FConnect;
    property Request: HINTERNET read FRequest write FRequest;
  end;

  TSymSrvProgressState = (spsConnecting, spsDownloading, spsDone, spsError);
  TSymSrvContinueState = (scsRun, scsPause, scsCancel);
  TSymSrvProgressEvent = procedure(Sender: TObject;
    State: TSymSrvProgressState; Pdb: TPdb; BytesReceived, TotalBytes: Int64;
    var AContinueState: TSymSrvContinueState) of object;

  TSymSrv = class
  const
    UserAgent = 'RawScanner-PDB-Downloader/1.0';
  private
    FLastErrorCode, FLastHttpStatus: DWORD;
    FLastError: string;
    FProxySettings: TProxySettings;
    FState: TSymSrvProgressState;
    FOnProgress: TSymSrvProgressEvent;
    function AuthKindToScheme(Kind: TProxyAuthKind): DWORD;
    procedure DoProgress(State: TSymSrvProgressState; Pdb: TPdb;
      BytesReceived, TotalBytes: Int64; var AContinueState: TSymSrvContinueState);
    function OpenSession: HINTERNET;
    function PrepareRequest(const Url, Verb: string): TSymSrvHandleChain;
    function QueryStatusCode(hRequest: HINTERNET): DWORD;
    function QueryContentLength(hRequest: HINTERNET): DWORD;
    procedure ResetLastError;
    function SendAndReceive(hRequest: HINTERNET; out StatusCode: DWORD): Boolean;
    procedure SetLastErrorFrom(const Context: string);
    procedure SetLastErrorMsg(const Msg: string);
    procedure SetProxyCredentials(hRequest: HINTERNET; Scheme: DWORD);
    procedure SetProxySettings(const Value: TProxySettings);
    function WinHttpErrorMessage(ErrorCode: DWORD): string;
  public
    function CheckTwoTierStorage(const BaseUrl: string): Boolean;
    function CheckRemoteFilePresent(const BaseUrl: string): Boolean;
    function DownloadPdb(Pdb: TPdb): Boolean;
    function DownloadJson(const Url: string; out json: string): Boolean;
    property DownloadState: TSymSrvProgressState read FState;
    property LastErrorCode: DWORD read FLastErrorCode;
    property LastError: string read FLastError;
    property LastHttpStatus: DWORD read FLastHttpStatus;
    property ProxySettings: TProxySettings read FProxySettings write SetProxySettings;
    property OnProgress: TSymSrvProgressEvent read FOnProgress write FOnProgress;
  end;

  TAsyncSymSrv = class(TThread)
  private
    FBytesReceived, FTotalBytes: Int64;
    FContinueState: TSymSrvContinueState;
    FLastError: string;
    FLastErrorCode, FLastHttpStatus: DWORD;
    FPdb: TPdb;
    FProxySettings: TProxySettings;
    FState: TSymSrvProgressState;
    FSymSrv: TSymSrv;
    FOnProgress: TSymSrvProgressEvent;
    procedure InternalProgress(Sender: TObject; State: TSymSrvProgressState;
      {%H-}Pdb: TPdb; BytesReceived, TotalBytes: Int64;
      var AContinueState: TSymSrvContinueState);
    procedure NotifyProgress;
  protected
    procedure Execute; override;
  public
    property DownloadState: TSymSrvProgressState read FState;
    property LastError: string read FLastError;
    property LastErrorCode: DWORD read FLastErrorCode;
    property LastHttpStatus: DWORD read FLastHttpStatus;
    property Pdb: TPdb read FPdb write FPdb;
    property ProxySettings: TProxySettings read FProxySettings write FProxySettings;
    property OnProgress: TSymSrvProgressEvent read FOnProgress write FOnProgress;
  end;

    TPdbKeyComparer = class(TInterfacedObject, IEqualityComparer<TPdbKey>)
    public
      function Equals({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Left, Right: TPdbKey): Boolean; reintroduce;
      function GetHashCode({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TPdbKey): UInt32; reintroduce;
    end;

  TPdbDownloadMode = (
    // [ru] синхронно, просто ждем когда скачается, DownloadPDB возвращает nil
    // [en] synchronous, we just wait for it to finish downloading, DownloadPDB return nil
    pdmSync,

    // [ru] асинхронно в автоматическом режиме, DownloadPDB возвращает nil
    // [en] asynchronous in automatic mode, DownloadPDB return nil
    pdmAsyncAuto,

    // [ru] асинхронно в модальном режиме, DownloadPDB возвращает nil
    // [en] asynchronous in modal mode, DownloadPDB return nil
    pdmAsyncModal,

    // [ru] асинхронно с ручным контролем, DownloadPDB возвращает засуспенженый TAsyncSymSrv, который нужно запустить и самому освободить.
    // [en] asynchronous with manual control, DownloadPDB returns a suspended TAsyncSymSrv, which you must start and release yourself.
    pdmAsyncManual
  );

  TSymStorageType = (
    sstUnknown,
    sstOneTier,
    sstTwoTiear
  );

  TPdbStorage = class
  private const
    OneTierFileName = 'pingme.txt';
    TwoTierFileName = 'index2.txt';
  private
    FCacheFolders: TStringList;
    FDefaultTwoTierStorage: Boolean;
    FLock: TCriticalSection;
    FPdbList: TObjectList<TPdb>;
    FPdbDict: TDictionary<TPdbKey, Integer>;
    FProxySettings: TProxySettings;
    FSymConfig: string;
    FSymServers: TStringList;
    FOnProgress: TSymSrvProgressEvent;
    function CheckRelativeKeyName(const Value: string): Boolean;
    function GetRelativePath(const APdbKey: TPdbKey): string;
    function GetSymSrvIndex(Value: Pointer): Integer;
    function GetSymSrvStorageType(Value: Pointer): TSymStorageType;
    function FixUpPath(const Folder, PdbPath: string; CreateNew: Boolean): string;
    function MakeSymSrvData(StorageType: TSymStorageType; Index: Integer): Pointer;
    procedure RunThreadModal(AThread: TThread);
    procedure SetSymConfig(const Value: string);
    procedure SetProxySettings(const Value: TProxySettings);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function DownloadPDB(Pdb: TPdb; SyncMode: TPdbDownloadMode): TAsyncSymSrv;
    function FindLocalPdb(const APdbKey: TPdbKey): string;
    function QueryPDB(const APdbKey: TPdbKey): TPdb; overload;
    function QueryPDB(const Uid: TGUID; Age: DWORD; const PdbFileName: string): TPdb; overload;
    function QueryRemotePDB(Pdb: TPdb): TPdbState;
    // CacheFolders — READ-ONLY!!! Use SymConfig to make changes
    property CacheFolders: TStringList read FCacheFolders;
    property DefaultTwoTierStorage: Boolean read FDefaultTwoTierStorage write FDefaultTwoTierStorage;
    property Items: TObjectList<TPdb> read FPdbList;
    property ProxySettings: TProxySettings read FProxySettings write SetProxySettings;
    property SymConfig: string read FSymConfig write SetSymConfig;
    property OnProgress: TSymSrvProgressEvent read FOnProgress write FOnProgress;
  end;

  TSymJobKind = (jkQuery, jkDownload);

  TSymJob = record
    Pdb: TPdb;
    Kind: TSymJobKind;
  end;

  TSymQueue = class;

  TSymWorkerThread = class(TThread)
  strict private
    FOwner: TSymQueue;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TSymQueue);
  end;

  TSymJobQueue = class
  private
    FItems: TQueue<TSymJob>;
    FLock: TCriticalSection;
    FEvent: TEvent;
    FContinueState: TSymSrvContinueState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(const Job: TSymJob);
    function Pop(out Job: TSymJob): Boolean;
    procedure Pause;
    procedure Resume;
    procedure Stop;
  end;

  TSymQueueDoneEvent = procedure(Sender: TObject; const Job: TSymJob; const Error: string) of object;
  TSymQueueProgressEvent = procedure(Sender: TObject; State: TSymSrvProgressState;
    Pdb: TPdb; BytesReceived, TotalBytes: Int64) of object;

  TSymQueue = class
  strict private
    FActiveWorkers, FTotal, FDone: Integer;
    FStorage: TPdbStorage;
    FWorkers: TArray<TSymWorkerThread>;
    FJobs: TSymJobQueue;
    FContinueState: TSymSrvContinueState;
    FStopped: Boolean;
    FOnJobDone: TSymQueueDoneEvent;
    FOnBatchDone: TNotifyEvent;
    FOnProgress: TSymQueueProgressEvent;
    FOnStopped: TNotifyEvent;
    procedure HandleProgress(Sender: TObject; State: TSymSrvProgressState;
      Pdb: TPdb; BytesReceived, TotalBytes: Int64;
      var AContinueState: TSymSrvContinueState);
    procedure WorkerTerminated(Sender: TObject);
  protected
    function GetNextJob(out Job: TSymJob): Boolean;
    procedure RunJob(const Job: TSymJob);
    procedure NotifyJobDone(const Job: TSymJob; const Error: string);
    property Total: Integer read  FTotal;
  public
    constructor Create(AStorage: TPdbStorage; AWorkerCount: Integer);
    destructor Destroy; override;
    procedure QueryPdb(Pdb: TPdb);
    procedure DownloadPdb(Pdb: TPdb);
    procedure Pause;
    procedure Resume;
    procedure Stop;
    property ContinueState: TSymSrvContinueState read FContinueState;
    property OnJobDone: TSymQueueDoneEvent read FOnJobDone write FOnJobDone;
    property OnBatchDone: TNotifyEvent read FOnBatchDone write FOnBatchDone;
    property OnProgress: TSymQueueProgressEvent read FOnProgress write FOnProgress;
    property OnStopped: TNotifyEvent read FOnStopped write FOnStopped;
  end;

  { TQueuedProgressNotify }

  TQueuedProgressNotify = class
  private
    FOwner: TSymQueue;
    FState: TSymSrvProgressState;
    FPdb: TPdb;
    FBytesReceived, FTotalBytes: Int64;
    procedure Execute;
  public
    constructor Create(AOwner: TSymQueue; AState: TSymSrvProgressState;
      APdb: TPdb; ABytesReceived, ATotalBytes: Int64);
  end;

  { TQueuedJobDoneNotify }

  TQueuedJobDoneNotify = class
  private
    FOwner: TSymQueue;
    FJob: TSymJob;
    FError: string;
    FDone: Integer;
    procedure Execute;
  public
    constructor Create(AOwner: TSymQueue; const AJob: TSymJob;
      const AError: string; ADone: Integer);
  end;

  function GetImagePdbKey(const FilePath: string; out Key: TPdbKey): Boolean;
  function IsPdbKeyEqual(const A, B: TPdbKey): Boolean;
  function SavePdbInfo(Pdb: TPdb; const ImageFilePath: string): Boolean;
  function UnDecoratePDBSymbolName(const Value: string): string;

implementation

function GetImagePdbKey(const FilePath: string; out Key: TPdbKey): Boolean;
const
  CV_SIGNATURE_RSDS = DWORD($53445352);
type
  TCV_INFO_PDB70 = record
    Signature: DWORD;   // 'RSDS'
    Guid: TGUID;
    Age: DWORD;
    // char  PdbFileName[]; // null-terminated
  end;
var
  Stream: TBufferedFileStream;
  DosHeader: TImageDosHeader;
  NtSignature: DWORD;
  FileHeader: TImageFileHeader;
  OptHeaderStart: Int64;
  OptMagic: Word;
  Opt32: TImageOptionalHeader32;
  Opt64: TImageOptionalHeader64;
  DebugDataDir: TImageDataDirectory;
  SectionTableStart: Int64;
  DebugDir: TImageDebugDirectory;
  EntryCount, I, NameLen: Integer;
  DebugDirFileOffset: Int64;
  CvInfo: TCV_INFO_PDB70;
  NameBuff: array of Byte;

  function RvaToFileOffset(Rva: DWORD): Int64;
  var
    S: Integer;
    Sec: TImageSectionHeader;
  begin
    Result := -1;
    Stream.Position := SectionTableStart;
    for S := 0 to FileHeader.NumberOfSections - 1 do
    begin
      Stream.ReadBuffer(Sec{%H-}, SizeOf(Sec));
      if (Rva >= Sec.VirtualAddress) and (Rva < Sec.VirtualAddress + Sec.SizeOfRawData) then
      begin
        Result := Int64(Sec.PointerToRawData) + Int64(Rva - Sec.VirtualAddress);
        Exit;
      end;
    end;
  end;

begin
  Result := False;
  Key := Default(TPdbKey);
  if not FileExists(FilePath) then Exit;

  Stream := TBufferedFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    if Stream.Size < SizeOf(TImageDosHeader) then Exit;
    Stream.ReadBuffer(DosHeader{%H-}, SizeOf(DosHeader));
    if DosHeader.e_magic <> IMAGE_DOS_SIGNATURE then Exit;

    Stream.Position := DosHeader._lfanew;
    Stream.ReadBuffer(NtSignature{%H-}, SizeOf(NtSignature));
    if NtSignature <> IMAGE_NT_SIGNATURE then Exit;

    Stream.ReadBuffer(FileHeader{%H-}, SizeOf(FileHeader));
    OptHeaderStart := Stream.Position;
    SectionTableStart := OptHeaderStart + FileHeader.SizeOfOptionalHeader;

    if FileHeader.SizeOfOptionalHeader < SizeOf(Word) then Exit;

    Stream.ReadBuffer(OptMagic{%H-}, SizeOf(OptMagic));
    Stream.Position := OptHeaderStart;

    case OptMagic of
      IMAGE_NT_OPTIONAL_HDR32_MAGIC:
        begin
          if OptHeaderStart + SizeOf(Opt32) > Stream.Size then Exit;
          Stream.ReadBuffer(Opt32{%H-}, SizeOf(Opt32));
          DebugDataDir := Opt32.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
        end;
      IMAGE_NT_OPTIONAL_HDR64_MAGIC:
        begin
          if OptHeaderStart + SizeOf(Opt64) > Stream.Size then Exit;
          Stream.ReadBuffer(Opt64{%H-}, SizeOf(Opt64));
          DebugDataDir := Opt64.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
        end;
    else
      Exit;
    end;

    if (DebugDataDir.VirtualAddress = 0) or (DebugDataDir.Size = 0) then Exit;

    DebugDirFileOffset := RvaToFileOffset(DebugDataDir.VirtualAddress);
    if DebugDirFileOffset < 0 then Exit;

    EntryCount := DebugDataDir.Size div SizeOf(TImageDebugDirectory);
    Stream.Position := DebugDirFileOffset;

    for I := 0 to EntryCount - 1 do
    begin
      if Stream.Position + SizeOf(TImageDebugDirectory) > Stream.Size then Break;
      Stream.ReadBuffer(DebugDir{%H-}, SizeOf(DebugDir));

      if DebugDir.Type_ <> IMAGE_DEBUG_TYPE_CODEVIEW then Continue;
      if DebugDir.PointerToRawData = 0 then Continue;
      if Int64(DebugDir.PointerToRawData) + SizeOf(TCV_INFO_PDB70) > Stream.Size then Continue;

      Stream.Position := DebugDir.PointerToRawData;
      Stream.ReadBuffer(CvInfo{%H-}, SizeOf(CvInfo));
      if CvInfo.Signature <> CV_SIGNATURE_RSDS then Continue; // PDB 2.0 (NB10) is not supported

      Key.Uid := CvInfo.Guid;
      Key.Age := CvInfo.Age;

      SetLength(NameBuff{%H-}, Min(Int64(MAX_PATH), Stream.Size - Stream.Position));
      if Length(NameBuff) > 0 then
      begin
        Stream.ReadBuffer(NameBuff[0], Length(NameBuff));
        NameLen := 0;
        while (NameLen < Length(NameBuff)) and (NameBuff[NameLen] <> 0) do
          Inc(NameLen);
        SetString(Key.Name, PAnsiChar(@NameBuff[0]), NameLen);
      end;

      Result := True;
      Break;
    end;
  finally
    Stream.Free;
  end;
end;

function IsPdbKeyEqual(const A, B: TPdbKey): Boolean;
begin
  Result :=
    AnsiSameText(A.Name, B.Name) and
    (A.Age = B.Age) and
    IsEqualGUID(A.Uid, B.Uid);
end;

function SavePdbInfo(Pdb: TPdb; const ImageFilePath: string): Boolean;
var
  InfoFilePath: string;
  JsonObj: TJSONObject;
  SL: TStringList;
begin
  Result := True;
  InfoFilePath := ExtractFilePath(Pdb.LocalPath) + PdbInfoFile;
  JsonObj := TJSONObject.Create;
  try
    JsonObj.Add('downloadedAt',
      FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', LocalTimeToUniversal(Now)));
    JsonObj.Add('downloadedBy', ExtractFileName(ParamStr(0)));
    JsonObj.Add('requestedForBinaryPath', ImageFilePath);
    JsonObj.Add('binaryGuid', GUIDToString(Pdb.UnicalId));
    JsonObj.Add('binaryAge', Pdb.Age);

    SL := TStringList.Create;
    try
      SL.Text := JsonObj.FormatJSON();
      try
        SL.SaveToFile(InfoFilePath, TEncoding.UTF8);
      except
        Exit(False); // No write permissions
      end;
    finally
      SL.Free;
    end;

    {$WARN SYMBOL_PLATFORM OFF}
    FileSetAttr(InfoFilePath, faHidden);
    {$WARN SYMBOL_PLATFORM ON}
  finally
    JsonObj.Free;
  end;
end;

function UnDecoratePDBSymbolName(const Value: string): string;
const
  BuffLen = 4096;
var
  Index, Index2: Integer;
  UnDecName: string;
begin
  Result := Value;
  if Result = '' then Exit;
  if Result.StartsWith('??_C@') then
  begin
    Result := 'string';
    Index := PosEx('@', Value, 6);
    Index2 := PosEx('@', Value, Index + 1);
    if Index2 > 6 then
    begin
      if Index2 = Length(Value) then
        Index2 := Index;
      Result := Format('string: "%s"', [Copy(Value, Index2 + 1, Length(Value) - Index2 - 1)]);
    end;
    Exit;
  end;
  SetLength(UnDecName{%H-}, BuffLen);
  SetLength(UnDecName, ImageHlp.UnDecorateSymbolName(@Value[1],
    @UnDecName[1], BuffLen, UNDNAME_NAME_ONLY));
  Result := UnDecName;
end;

procedure ValidateProxy(Value: TProxySettings);
begin
  if Value.Kind in [pkUserSettings, pkUserSettingsWithAuthority] then
  begin
    if Value.Host = '' then
      raise ESymSrvException.Create('The proxy server address is not specified');
    if Value.Port = 0 then
      raise ESymSrvException.Create('The proxy server port is not specified');
  end;
  if Value.Kind = pkUserSettingsWithAuthority then
    if Value.Login = '' then
      raise ESymSrvException.Create('The proxy server username is not specified');
end;

{ TPdb }

procedure TPdb.CheckStream(AStream: TStream);
const
  MsfMagicPrefix: AnsiString = 'Microsoft C/C++ MSF 7.00';
begin
  if AStream.Size < SizeOf(BIGMSF_HDR) then
    raise EPdbException.Create('The PDB file is too small or corrupted');

  AStream.Position := 0;
  AStream.ReadBuffer(FHeader, SizeOf(BIGMSF_HDR));

  if not CompareMem(@FHeader.Magic[0], @MsfMagicPrefix[1], Length(MsfMagicPrefix)) then
    raise EPdbException.Create('Invalid MSF signature, this is not a PDB file');

  if (FHeader.cbPg = 0) or (FHeader.pnMac = 0) then
    raise EPdbException.Create('Invalid MSF header');
end;

function TPdb.Count: Integer;
begin
  Result := FSymbols.Count;
end;

constructor TPdb.Create(APdbKey: TPdbKey);
begin
  FSymbols := TListEx<TPdbSymbol>.Create;
  FAge := APdbKey.Age;
  FUnicalId := APdbKey.Uid;
  FPdbName := APdbKey.Name;
end;

constructor TPdb.Create;
begin
  Create(Default(TPdbKey));
end;

destructor TPdb.Destroy;
begin
  FSymbols.Free;
  inherited;
end;

function TPdb.GetRemotePath: string;
begin
  Result := RemotePath;
end;

function TPdb.GetStream(AStream: TStream; Index: Integer): TBytes;
begin
  if (Index < 0) or (Index >= Length(FStreamSizes)) then
    Result := nil
  else
    Result := ReadPages(AStream, FStreamPages[Index], FStreamSizes[Index]);
end;

function TPdb.GetSymbol(Index: Integer): TPdbSymbol;
begin
  if FState = psNoSections then
    raise EPdbException.Create('The PDB does not contain information about sections.' + sLineBreak +
      'This information must be loaded from an external source by calling `UpdateSections()`');

  Result := FSymbols[Index];
  if (Result.Section < 0) or (Result.Section >= Length(FSectionsRva)) then
    raise EPdbException.CreateFmt('"%s" contain invalid symbol (%d). Invalid section index (%d). Expected 0..%d',
      [PdbName, Index, Result.Section, Length(FSectionsRva) - 1]);

  Inc(Result.Rva, FSectionsRva[Result.Section]);
end;

function TPdb.Key: TPdbKey;
begin
  Result := Default(TPdbKey);
  Result.Uid := FUnicalId;
  Result.Age := Age;
  Result.Name := PdbName;
end;

procedure TPdb.LoadFromFile(const FilePath: string);
var
  F: TBufferedFileStream;
begin
  F := TBufferedFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    SetLocalPath(StringReplace(FilePath, '/', '\', [rfReplaceAll]));
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TPdb.LoadFromStream(AStream: TStream);
begin
  CheckStream(AStream);
  LoadStreams(AStream);
  LoadSymbols(AStream);
  UpdateUnicalId(AStream);
end;

procedure TPdb.LoadSectionData(AStream: TStream; const Dbi: TBytes);
var
  Sections: TBytes;
  DbiHeader: PDbiStreamHeader;
  DebugHeaderOffset: Int64;
  DebugHeader: PDbiDebugHeader;
  pSection: PImageSectionHeader;
  I: Integer;
begin
  DbiHeader := PDbiStreamHeader(@Dbi[0]);
  FAge := DbiHeader^.Age;
  if DbiHeader^.OptionalDebugHeaderSize = 0 then Exit;

  // https://llvm.org/docs/PDB/DbiStream.html#dbi-optional-dbg-stream
  // [ru] Дополнительный отладочный заголовок лежит после всех подпотоков
  // [ru] которые указаны в заголовке DBI
  // [en] An additional debug header follows all the substreams specified in the DBI header.
  DebugHeaderOffset :=
    SizeOf(TDbiStreamHeader) +
    DbiHeader.ModuleInfoSize +
    DbiHeader.SectionContributionSize +
    DbiHeader.SectionMapSize +
    DbiHeader.SourceInfoSize +
    DbiHeader.TypeServerMapSize +
    DbiHeader.EcSubstreamSize;

  if DebugHeaderOffset + SizeOf(TDbiDebugHeader) > Length(Dbi) then Exit;

  // [ru] и именно в нем может лежать номер стрима с заголовками
  // [en] and that's exactly where the stream number with titles might be found
  DebugHeader := PDbiDebugHeader(@Dbi[DebugHeaderOffset]);

  // [ru] а может и не лежать
  // [en] or maybe not
  if DebugHeader^.SectionHeaderStreamIndex = DBI_INVALID_STREAM_INDEX then Exit;
  Sections := GetStream(AStream, DebugHeader^.SectionHeaderStreamIndex);
  if Sections = nil then Exit;

  // [ru] Сам поток секций это буквально дамп секций PE файла один в один,
  // [ru] поэтому читаем штатной структурой. Нам нужно знать только VirtualAddress

  // [en] The section stream itself is literally a one-to-one dump of the PE file's sections,
  // [en] so we read it using the standard structure. We only need to know the VirtualAddress

  pSection := @Sections[0];
  SetLength(FSectionsRva, Length(Sections) div SizeOf(TImageSectionHeader));
  for I := 0 to Length(FSectionsRva) - 1 do
  begin
    FSectionsRva[I] := pSection^.VirtualAddress;
    Inc(pSection);
  end;
end;

procedure TPdb.LoadStreams(AStream: TStream);
var
  DirectoryPageCount: Integer;
  BlockMapEntryCount: Integer;
  BlockMapPages: TPages;
  DirectoryPagesRaw: TBytes;
  DirectoryPages: TPages;
  DirectoryData: TBytes;
  DirData: PIntegerArray;
  StreamCount: Integer;
  Idx, I, A: Integer;
begin

  // [ru] Для начала получаем список страниц где расположена информация о страницах
  // [ru] директории. Как правило она меньше одной страницы и для хранения ссылки на
  // [ru] неё достаточно первого элемента заголовка FHeader.mpspnpnSt[0]

  // [en] First, we retrieve a list of pages containing information about the pages
  // [en] in the directory. Typically, this list consists of fewer than one page, and to store a link to
  // [en] it, the first element of the FHeader.mpspnpnSt[0] header is sufficient

  DirectoryPageCount := PagesCount(FHeader.siSt.cb);
  BlockMapEntryCount := PagesCount(DirectoryPageCount * SizeOf(UPN));
  SetLength(BlockMapPages{%H-}, BlockMapEntryCount);
  if BlockMapEntryCount > 0 then
  begin
    BlockMapPages[0] := FHeader.mpspnpnSt[0];

    // [ru] Но если вдруг нужно больше (это должен быть гигантский PDB),
    // [ru] то дочитываем дополнительные данные

    // [en] But if more data is needed (it would have to be a massive PDB),
    // [en] then we read in the additional data

    if BlockMapEntryCount > 1 then
    begin
      AStream.Position := SizeOf(BIGMSF_HDR);
      AStream.ReadBuffer(BlockMapPages[1], (BlockMapEntryCount - 1) * SizeOf(UPN));
    end;
  end;

  // [ru] Теперь получаем список страниц, из которых состоит директория.
  // [en] Now we get a list of the pages that make up the directory.
  DirectoryPagesRaw := ReadPages(AStream, BlockMapPages, DirectoryPageCount * SizeOf(UPN));
  SetLength(DirectoryPages{%H-}, DirectoryPageCount);
  if DirectoryPageCount > 0 then
    Move(DirectoryPagesRaw[0], DirectoryPages[0], DirectoryPageCount * SizeOf(UPN));

  // [ru] Шаг 3: читаем данные директории
  // [en] Step 3: Read the directory data
  DirectoryData := ReadPages(AStream, DirectoryPages, FHeader.siSt.cb);

  // [ru] Шаг четыре: извлекаем из директории данные о стримах
  // [ru] Сама структура тривиальная (идет блоками по 4 байта):
  // [ru] Count (4 байта) стримов с данными
  // [ru] Массив (Count * 4) размеров каждого стрима
  // [ru] Массив страниц содержащий данные каждого стрима, где количетсво записей
  // [ru] страниц для каждого стрима рассчитывается от размера самого стрима.

  // [en] Step four: Retrieve stream data from the directory
  // [en] The structure itself is straightforward (processed in 4-byte blocks):
  // [en] Count (4 bytes) of streams containing data
  // [en] An array (Count * 4) of the sizes of each stream
  // [en] An array of pages containing the data for each stream, where the number of records
  // [en] per page for each stream is calculated based on the size of the stream itself.

  DirData := @DirectoryData[0];
  StreamCount := DirData[0];

  SetLength(FStreamPages, StreamCount);
  SetLength(FStreamSizes, StreamCount);

  // [ru] Читаем в два прохода, потому что сначала идут размеры
  // [en] We read it in two passes because the dimensions come first
  Idx := 1;
  for I := 0 to StreamCount - 1 do
  begin
    FStreamSizes[I] := DirData[Idx];
    // [ru] и количество данных по каждому стриму зависит от его размера
    // [en] and the amount of data for each stream depends on its size
    SetLength(FStreamPages[I], PagesCount(FStreamSizes[I]));
    Inc(Idx);
  end;

  // [ru] А уже после размеров идут сами данные, размер которых рассчитан выше
  // [en] And right after the dimensions come the data itself,
  // [en] the size of which was calculated above
  for I := 0 to StreamCount - 1 do
  begin
    for A := 0 to Length(FStreamPages[I]) - 1 do
    begin
      FStreamPages[I][A] := DirData[Idx];
      Inc(Idx);
    end;

    // [ru] как только собрали мини-FAT (т.е. номера страниц указывающих порядок
    // [ru] чтения данных чтобы собрать все воедино), данные можно зачитать ленивым вызовом

    // [en] Once the mini-FAT has been assembled (i.e. the page numbers specifying the order
    // [en] in which the data should be read to put everything together),
    // [en] the data can be read using a lazy load
  end;
end;

procedure TPdb.LoadSymbols(AStream: TStream);
var
  Dbi: TBytes;
  DbiHeader: PDbiStreamHeader;
  PublicsData, SymRecordData: TBytes;
  GsiHeader: PGsiHashTableHeader;
  HashRecordsOffset: Integer;
  NumRecords, I, SymCount, PublicsDataLen, SymDataLen, SecCount: Integer;
  HashRec: PPdbHashRecord;
  SymOffset: DWORD;
  PubSym: PPUBSYM32;
  NameStart, NameEnd: Integer;
  Sym: TPdbSymbol;
begin
  Dbi := GetStream(AStream, PDB_STREAM_DBI);
  if Length(Dbi) < SizeOf(TDbiStreamHeader) then Exit;

  LoadSectionData(AStream, Dbi);
  SecCount := Length(FSectionsRva);
  if SecCount > 0 then
    FMaxSectionIdx := SecCount - 1;

  DbiHeader := PDbiStreamHeader(@Dbi[0]);

  // [ru] Для загрузки нужен стрим с хэшами символов (PublicStreamIndex)
  // [ru] и стрим с самими символами (SymbolRecordStreamIndex)

  // [en] To load the data, you need a stream containing character hashes (PublicStreamIndex)
  // [en] and a stream containing the characters themselves (SymbolRecordStreamIndex)

  if (DbiHeader^.PublicStreamIndex < 0) or (DbiHeader^.SymbolRecordStreamIndex < 0) then Exit;

  PublicsData := GetStream(AStream, DbiHeader^.PublicStreamIndex);
  PublicsDataLen := Length(PublicsData);
  SymRecordData := GetStream(AStream, DbiHeader^.SymbolRecordStreamIndex);
  SymDataLen := Length(SymRecordData);
  if (PublicsDataLen < SizeOf(TPublicStreamHeader) + SizeOf(TGsiHashTableHeader)) or
     (SymDataLen = 0) then Exit;

  GsiHeader := PGsiHashTableHeader(@PublicsData[SizeOf(TPublicStreamHeader)]);

  // [ru] Такая же подстраховка как и в raw_pdb (HasValidPublicSymbolStream)
  // [en] The same safety check as in raw_pdb (HasValidPublicSymbolStream)
  if GsiHeader^.Signature <> GSI_HASH_SIGNATURE then Exit;
  if GsiHeader^.Version <> GSI_HASH_VERSION then Exit;

  HashRecordsOffset := SizeOf(TPublicStreamHeader) + SizeOf(TGsiHashTableHeader);
  if HashRecordsOffset + Integer(GsiHeader^.HashRecordsSize) > PublicsDataLen then Exit;

  NumRecords := GsiHeader^.HashRecordsSize div SizeOf(TPdbHashRecord);

  FSymbols.Count := NumRecords;
  SymCount := 0;
  for I := 0 to NumRecords - 1 do
  begin
    HashRec := PPdbHashRecord(@PublicsData[HashRecordsOffset + I * SizeOf(TPdbHashRecord)]);
    if HashRec^.Offset = 0 then Continue;

    // [ru] хранится со сдвигом +1
    // [en] is saved with an offset of +1
    SymOffset := HashRec^.Offset - 1;

    if Int64(SymOffset) + SizeOf(PUBSYM32) > SymDataLen then Continue;
    PubSym := PPUBSYM32(@SymRecordData[SymOffset]);
    if PubSym^.rectyp <> CV_SYMBOL_KIND_S_PUB32 then Continue;

    // [ru] Символы не привязанные к секции тоже пропускаются
    // [en] Symbols not associated with a section are skipped
    if PubSym^.seg = 0 then Continue;

    // [ru] Если секции есть в наличии, то выкидываем все что за диапазоном секций.
    // [en] If the sections are available, we discard everything outside the section range.
    if (SecCount > 0) and (PubSym^.seg >= SecCount) then Continue;

    NameStart := Integer(SymOffset) + SizeOf(PUBSYM32);

    // [ru] поле Size себя не содержит, нужно добавить
    // [en] the Size field does not contain itself, it needs to be added.
    NameEnd := Integer(SymOffset) + PubSym^.reclen + SizeOf(Word);
    if NameEnd > SymDataLen then Continue;

    Sym.Name := ReadSymbolName(SymRecordData, NameStart, NameEnd);
    Sym.Rva := PubSym^.off;
    // [ru] также со сдвидом в единицу
    // [en] also with a shift of one
    Sym.Section := PubSym^.seg - 1;
    Sym.IsFunc := (PubSym^.pubsymflags and CVPSF_FUNCTION) <> 0;

    if SecCount = 0 then
      FMaxSectionIdx := Max(FMaxSectionIdx, Sym.Section);

    FSymbols.List[SymCount] := Sym;
    Inc(SymCount);
  end;
  FSymbols.Count := SymCount;
end;

function TPdb.PagesCount(Value: CB): Integer;
begin
  if Value <= 0 then
    Result := 0
  else
    Result := Max(1, (Value + FHeader.cbPg - 1) div FHeader.cbPg);
end;

function TPdb.ReadPages(AStream: TStream; const PageList: TPages; TotalSize: Int64): TBytes;
var
  I: Integer;
  ChunkSize: CB;
  SrcOffset, DstOffset: Int64;
begin
  if TotalSize <= 0 then Exit(nil);
  SetLength(Result, TotalSize);
  DstOffset := 0;
  for I := 0 to Length(PageList) - 1 do
  begin
    if DstOffset >= TotalSize then Break;
    ChunkSize := FHeader.cbPg;
    if DstOffset + ChunkSize > TotalSize then
      ChunkSize := TotalSize - DstOffset;
    SrcOffset := Int64(PageList[I]) * FHeader.cbPg;
    if SrcOffset + ChunkSize > AStream.Size then
      ChunkSize := AStream.Size - SrcOffset;
    if ChunkSize <= 0 then Break;
    AStream.Position := SrcOffset;
    AStream.ReadBuffer(Result[DstOffset], ChunkSize);
    Inc(DstOffset, FHeader.cbPg);
  end;
end;

function TPdb.ReadSymbolName(const Data: TBytes; NameStart,
  RecordEnd: Integer): string;
const
  LINKER_INTERNAL_SYMBOL_PREFIX = $7F;
var
  NameEnd, DataEnd: Integer;
begin
  NameEnd := NameStart;
  DataEnd := Length(Data);
  while (NameEnd < RecordEnd) and (NameEnd < Length(Data)) and (Data[NameEnd] <> 0) do
  begin
    Inc(NameEnd);
    if NameEnd = DataEnd then
      Break;
  end;
  SetString(Result, PAnsiChar(@Data[NameStart]), NameEnd - NameStart);
  if Data[NameStart] = LINKER_INTERNAL_SYMBOL_PREFIX then
    Result[1] := '_';
end;

procedure TPdb.Reset;
begin
  FState := psNotFoundLocally;
  FSymbols.Clear;
  FMaxSectionIdx := 0;
  SetLength(FSectionsRva, 0);
  FHeader := Default(BIGMSF_HDR);
end;

procedure TPdb.SetLocalPath(const Value: string);
begin
  FLocalPath := Value;
  FPdbName := ExtractFileName(Value);
end;

procedure TPdb.UpdateSections(const Value: TSectionsRva);
begin
  FSectionsRva := Value;
  if (FState = psNoSections) and (FMaxSectionIdx < Length(Value)) then
    FState := psReady;
end;

procedure TPdb.UpdateState(NewState: TPdbState);
begin
  FState := NewState;
end;

procedure TPdb.UpdateUnicalId(AStream: TStream);
var
  Strm: TBytes;
begin
  FState := psBroken;

  Strm := GetStream(AStream, PDB_STREAM_PDB_INFO);
  if Length(Strm) >= SizeOf(TPdbStreamHeader) then
  begin
    FUnicalId := PPdbStreamHeader(@Strm[0]).UinqueId;
    if Count > 0 then
    begin
      if FMaxSectionIdx >= Length(FSectionsRva) then
        FState := psNoSections
      else
        FState := psReady;
    end;
  end;

  // [ru] FAT стримов больше не нужен, освобождаем память
  // [en] We no longer need the FAT streams, let's free up some memory
  SetLength(FStreamPages, 0);
  SetLength(FStreamSizes, 0);
end;

{ TSymSrvHandleChain }

destructor TSymSrvHandleChain.Destroy;
begin
  if FRequest <> nil then WinHttpCloseHandle(FRequest);
  if FConnect <> nil then WinHttpCloseHandle(FConnect);
  if FSession <> nil then WinHttpCloseHandle(FSession);
  inherited;
end;

{ TSymSrv }

function TSymSrv.AuthKindToScheme(Kind: TProxyAuthKind): DWORD;
begin
  case Kind of
    pakBasic: Result := WINHTTP_AUTH_SCHEME_BASIC;
    pakNegotiate: Result := WINHTTP_AUTH_SCHEME_NEGOTIATE;
    pakNTLM: Result := WINHTTP_AUTH_SCHEME_NTLM;
  else
    Result := 0;
  end;
end;

function TSymSrv.CheckRemoteFilePresent(const BaseUrl: string): Boolean;
var
  Chain: TSymSrvHandleChain;
  StatusCode: DWORD;
begin
  Result := False;
  ResetLastError;
  Chain := PrepareRequest(BaseUrl, 'HEAD');
  if Chain = nil then Exit;
  try
    if not SendAndReceive(Chain.Request, StatusCode) then Exit;
    Result := StatusCode = HTTP_STATUS_OK;
    if not Result then
      SetLastErrorMsg(Format('Server returned HTTP status %d', [StatusCode]));
  finally
    Chain.Free;
  end;
end;

function TSymSrv.CheckTwoTierStorage(const BaseUrl: string): Boolean;
begin
  // [ru] Тут нам нужен только статус-код, что файл присутствует в хранилище символов.
  // [ru] содержимое файла неважно: "The content of the file is of no importance"

  // [en] Here, we only need the status code to confirm that the file is present in the character storage.
  // [en] "The content of the file is of no importance"

  // https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/symbol-store-folder-tree
  Result := CheckRemoteFilePresent(BaseUrl + '/index2.txt');
end;

procedure TSymSrv.DoProgress(State: TSymSrvProgressState; Pdb: TPdb;
  BytesReceived, TotalBytes: Int64; var AContinueState: TSymSrvContinueState);
begin
  FState := State;
  if Assigned(FOnProgress) then
    FOnProgress(Self, State, Pdb, BytesReceived, TotalBytes, AContinueState);
end;

function TSymSrv.DownloadJson(const Url: string; out json: string): Boolean;
const
  BuffSize = $100000;
var
  Chain: TSymSrvHandleChain;
  StatusCode: DWORD;
  ContentLength: DWORD;
  BytesAvailable, BytesRead: DWORD;
  Buff: array of Byte;
  TmpStream: TMemoryStream;
  StartOffset: Integer;
  P: PByte;
begin
  Result := False;
  ResetLastError;
  SetLength(Buff{%H-}, BuffSize);
  Chain := PrepareRequest(Url, 'GET');
  if Chain = nil then Exit;
  try

    if not WinHttpAddRequestHeaders(Chain.Request,
      PWideChar(L('User-Agent: ' + UserAgent + sLineBreak +
      'Accept: application/vnd.github+json')),
      DWORD(-1), WINHTTP_ADDREQ_FLAG_ADD or WINHTTP_ADDREQ_FLAG_REPLACE) then
    begin
      SetLastErrorFrom('WinHttpAddRequestHeaders');
      Exit;
    end;

    if not SendAndReceive(Chain.Request, StatusCode) then Exit;

    if StatusCode <> HTTP_STATUS_OK then
    begin
      SetLastErrorMsg(Format('Server returned HTTP status %d', [StatusCode]));
      Exit;
    end;

    ContentLength := QueryContentLength(Chain.Request);

    TmpStream := TMemoryStream.Create;
    try
      repeat
        BytesAvailable := 0;
        if not WinHttpQueryDataAvailable(Chain.Request, @BytesAvailable) then
        begin
          SetLastErrorFrom('WinHttpQueryDataAvailable');
          Break;
        end;
        if BytesAvailable = 0 then Break;
        if BytesAvailable > BuffSize then
          BytesAvailable := BuffSize;
        if not WinHttpReadData(Chain.Request, @Buff[0], BytesAvailable, @BytesRead) then
        begin
          SetLastErrorFrom('WinHttpReadData');
          Break;
        end;
        if BytesRead = 0 then Break;
        TmpStream.WriteBuffer(Buff[0], BytesRead);
      until False;

      if (FLastErrorCode = 0) and (ContentLength > 0) and (TmpStream.Size <> ContentLength) then
      begin
        SetLastErrorMsg(Format(
          'Download interrupted: received %d of %d bytes', [TmpStream.Size, ContentLength]));
        Exit;
      end;

      P := TmpStream.Memory;
      if (TmpStream.Size >= 3) and (P[0] = $EF) and (P[1] = $BB) and (P[2] = $BF) then
        StartOffset := 3
      else
        StartOffset := 0;
      SetLength(Buff, TmpStream.Size - StartOffset);
      TmpStream.Position := StartOffset;
      TmpStream.ReadBuffer(Buff[0], Length(Buff));
      json := string(TEncoding.UTF8.GetString(Buff));

    finally
      TmpStream.Free;
    end;

    Result := True;

  finally
    Chain.Free;
  end;
end;

function TSymSrv.DownloadPdb(Pdb: TPdb): Boolean;
const
  BuffSize = $100000;
var
  Chain: TSymSrvHandleChain;
  StatusCode: DWORD;
  ContentLength: DWORD;
  BytesAvailable, BytesRead: DWORD;
  Buff: array of Byte;
  OutStream: TFileStream;
  TotalReceived: Int64;
  TmpFile: string;
  ContinueState: TSymSrvContinueState;
begin
  Result := False;
  ResetLastError;

  ContentLength := 0;
  TotalReceived := 0;
  SetLength(Buff{%H-}, BuffSize);
  ContinueState := scsRun;
  repeat
    DoProgress(spsConnecting, Pdb, TotalReceived, ContentLength, ContinueState);
    if ContinueState = scsCancel then Exit;
  until ContinueState = scsRun;
  Pdb.UpdateState(psLoading);

  Chain := PrepareRequest(Pdb.RemotePath, 'GET');
  if Chain = nil then Exit;
  try
    if not SendAndReceive(Chain.Request, StatusCode) then Exit;

    if StatusCode <> HTTP_STATUS_OK then
    begin
      SetLastErrorMsg(Format('Server returned HTTP status %d', [StatusCode]));
      Exit;
    end;

    ContentLength := QueryContentLength(Chain.Request);

    try
      TmpFile := Pdb.LocalPath + '.downloading';
      ForceDirectories(ExtractFilePath(TmpFile));
      OutStream := TFileStream.Create(TmpFile, fmCreate);
    except
      on E: Exception do
      begin
        SetLastErrorMsg('Cannot create temp file "' + TmpFile + '"' + sLineBreak +
          E.ClassName + ': ' + E.Message);
        Exit;
      end;
    end;
    try
      TotalReceived := 0;
      repeat
        BytesAvailable := 0;
        if not WinHttpQueryDataAvailable(Chain.Request, @BytesAvailable) then
        begin
          SetLastErrorFrom('WinHttpQueryDataAvailable');
          Break;
        end;
        if BytesAvailable = 0 then Break;
        if BytesAvailable > BuffSize then
          BytesAvailable := BuffSize;
        if not WinHttpReadData(Chain.Request, @Buff[0], BytesAvailable, @BytesRead) then
        begin
          SetLastErrorFrom('WinHttpReadData');
          Break;
        end;
        if BytesRead = 0 then Break;
        OutStream.WriteBuffer(Buff[0], BytesRead);
        Inc(TotalReceived, BytesRead);
        repeat
          DoProgress(spsDownloading, Pdb, TotalReceived, ContentLength, ContinueState);
          case ContinueState of
            scsRun: ;
            scsPause: Sleep(100);
            scsCancel: Break;
          end;
        until ContinueState = scsRun;
        if ContinueState = scsCancel then
        begin
          Pdb.UpdateState(psAvailableForLoading);
          Result := True;
          Break;
        end;
      until False;
    finally
      OutStream.Free;
    end;

    if (FLastErrorCode = 0) and (ContentLength > 0) and (TotalReceived <> ContentLength) then
      SetLastErrorMsg(Format(
        'Download interrupted: received %d of %d bytes', [TotalReceived, ContentLength]));

    if FLastError <> '' then
    begin
      DeleteFile(TmpFile);
      Exit;
    end;

    try
      if FileExists(Pdb.LocalPath) then
        DeleteFile(Pdb.LocalPath);
      MoveFile(PChar(TmpFile), PChar(Pdb.LocalPath));
    except
      on E: Exception do
      begin
        SetLastErrorMsg('Cannot move "' + TmpFile + '" to "' + Pdb.LocalPath + '"' +
          sLineBreak + E.ClassName + ': ' + E.Message);
        Exit;
      end;
    end;

    Pdb.LoadFromFile(Pdb.LocalPath);
    Pdb.RemotePath := '';

    Result := True;

  finally
    Chain.Free;
    if Result then
      DoProgress(spsDone, Pdb, TotalReceived, ContentLength, ContinueState)
    else
      DoProgress(spsError, Pdb, TotalReceived, ContentLength, ContinueState);
  end;
end;

function TSymSrv.OpenSession: HINTERNET;
var
  ProtocolFlags: DWORD;
begin
  case FProxySettings.Kind of
    pkNone:
      Result := WinHttpOpen(PWideChar(UserAgent),
        WINHTTP_ACCESS_TYPE_NO_PROXY, nil, nil, 0);
    pkUserSettings, pkUserSettingsWithAuthority:
      Result := WinHttpOpen(PWideChar(UserAgent),
        WINHTTP_ACCESS_TYPE_NAMED_PROXY,
        PWideChar(L(Format('%s:%d', [FProxySettings.Host, FProxySettings.Port]))),
        nil, 0);
  else
    Result := WinHttpOpen(PWideChar(UserAgent),
      WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, nil, nil, 0);
  end;
  if Result = nil then
    SetLastErrorFrom('WinHttpOpen')
  else
  begin

    // [ru] Принудительный откат на HTTP/1.1 — для обхода бага
    // [ru] зависания WinHTTP при работе по HTTP/2 на больших закачках

    // [en] Force a fallback to HTTP/1.1 — to work around a bug
    // [en] that causes WinHTTP to hang when using HTTP/2 for large downloads

    ProtocolFlags := 0;
    if not WinHttpSetOption(Result, WINHTTP_OPTION_ENABLE_HTTP_PROTOCOL,
      @ProtocolFlags, SizeOf(ProtocolFlags)) then
    begin
      SetLastErrorFrom('WinHttpSetOption');
      WinHttpCloseHandle(Result);
      Result := nil;
    end;
  end;
end;

function TSymSrv.PrepareRequest(const Url, Verb: string): TSymSrvHandleChain;
var
  Comp: TUrlComponents;
  HostBuf: array[0..INTERNET_MAX_HOST_NAME_LENGTH - 1] of WideChar;
  PathBuf: array[0..INTERNET_MAX_PATH_LENGTH - 1] of WideChar;
  Flags: DWORD;
begin
  Result := TSymSrvHandleChain.Create;
  try
    Comp := Default(TUrlComponents);
    Comp.dwStructSize := SizeOf(Comp);
    Comp.lpszHostName := @HostBuf[0];
    Comp.dwHostNameLength := Length(HostBuf);
    Comp.lpszUrlPath := @PathBuf[0];
    Comp.dwUrlPathLength := Length(PathBuf);
    if not WinHttpCrackUrl(PWideChar(L(Url)), Length(Url), 0, @Comp) then
    begin
      SetLastErrorFrom('WinHttpCrackUrl');
      FreeAndNil(Result);
      Exit;
    end;

    Result.Session := OpenSession;
    if Result.Session = nil then
    begin
      FreeAndNil(Result);
      Exit;
    end;

    Result.Connect := WinHttpConnect(Result.Session, PWideChar(@Comp.lpszHostName[0]), Comp.nPort, 0);
    if Result.Connect = nil then
    begin
      SetLastErrorFrom('WinHttpConnect');
      FreeAndNil(Result);
      Exit;
    end;

    Flags := 0;
    if Integer(Comp.nScheme) = WinHTTP.INTERNET_SCHEME_HTTPS then
      Flags := WINHTTP_FLAG_SECURE;

    Result.Request := WinHttpOpenRequest(Result.Connect, PWideChar(L(Verb)),
      PWideChar(@Comp.lpszUrlPath[0]), nil, nil, nil, Flags);
    if Result.Request = nil then
    begin
      SetLastErrorFrom('WinHttpOpenRequest');
      FreeAndNil(Result);
      Exit;
    end;

    // [ru] Если схема авторизации прокси задана явно,
    // [ru] то нужно выставить реквизиты сразу, чтобы не тратить лишний шаг на 407

    // [en] If the proxy's authorization scheme is specified explicitly,
    // [en] then you need to provide the credentials right away to avoid an extra step due to a 407 error

    if (FProxySettings.Kind = pkUserSettingsWithAuthority) and
       (FProxySettings.AuthKind <> pakAuto) then
      SetProxyCredentials(Result.Request, AuthKindToScheme(FProxySettings.AuthKind));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSymSrv.QueryContentLength(hRequest: HINTERNET): DWORD;
var
  Size: DWORD;
begin
  Result := 0;
  Size := SizeOf(Result);
  if not WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_CONTENT_LENGTH or
    WINHTTP_QUERY_FLAG_NUMBER, nil, @Result, @Size, nil) then
    Result := 0;
end;

function TSymSrv.QueryStatusCode(hRequest: HINTERNET): DWORD;
var
  Size: DWORD;
begin
  Result := 0;
  Size := SizeOf(Result);
  WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_STATUS_CODE or
    WINHTTP_QUERY_FLAG_NUMBER, nil, @Result, @Size, nil);
  FLastHttpStatus := Result;
end;

procedure TSymSrv.ResetLastError;
begin
  FLastErrorCode := NO_ERROR;
  FLastHttpStatus := 0;
  FLastError := '';
end;

function TSymSrv.SendAndReceive(hRequest: HINTERNET;
  out StatusCode: DWORD): Boolean;
var
  SupportedSchemes, FirstScheme, AuthTarget: DWORD;
begin
  Result := False;
  StatusCode := 0;

  if not WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) then
  begin
    SetLastErrorFrom('WinHttpSendRequest');
    Exit;
  end;
  if not WinHttpReceiveResponse(hRequest, nil) then
  begin
    SetLastErrorFrom('WinHttpReceiveResponse');
    Exit;
  end;
  StatusCode := QueryStatusCode(hRequest);

  // [ru] Автоопределение схемы авторизации прокси при 407 (не протестировано)
  // [en] Automatic detection of the proxy authorization scheme for a 407 error (not tested)
  if (StatusCode = HTTP_STATUS_PROXY_AUTH_REQ) and (FProxySettings.Kind =
    pkUserSettingsWithAuthority) and (FProxySettings.AuthKind = pakAuto) then
  begin
    if not WinHttpQueryAuthSchemes(hRequest, @SupportedSchemes, @FirstScheme, @AuthTarget) then
    begin
      SetLastErrorFrom('WinHttpQueryAuthSchemes');
      Exit;
    end;

    SetProxyCredentials(hRequest, FirstScheme);

    // [ru] После чего пробуем еще одну попытку
    // [en] Then we'll give it another try
    if not WinHttpSendRequest(hRequest, nil, 0, nil, 0, 0, 0) then
    begin
      SetLastErrorFrom('WinHttpSendRequest (proxy auth retry)');
      Exit;
    end;

    if not WinHttpReceiveResponse(hRequest, nil) then
    begin
      SetLastErrorFrom('WinHttpReceiveResponse (proxy auth retry)');
      Exit;
    end;

    StatusCode := QueryStatusCode(hRequest);
  end;

  Result := True;
end;

procedure TSymSrv.SetLastErrorFrom(const Context: string);
begin
  FLastErrorCode := GetLastError;
  FLastError := Format('%s: %s', [Context, WinHttpErrorMessage(FLastErrorCode)]);
end;

procedure TSymSrv.SetLastErrorMsg(const Msg: string);
begin
  FLastErrorCode := 0;
  FLastError := Msg;
end;

procedure TSymSrv.SetProxyCredentials(hRequest: HINTERNET; Scheme: DWORD);
begin
  if Scheme = 0 then Exit;
  if not WinHttpSetCredentials(hRequest, WINHTTP_AUTH_TARGET_PROXY, Scheme,
    PWideChar(L(FProxySettings.Login)), PWideChar(L(FProxySettings.Password)), nil) then
    SetLastErrorFrom('WinHttpSetCredentials');
end;

procedure TSymSrv.SetProxySettings(const Value: TProxySettings);
begin
  ValidateProxy(Value);
  FProxySettings := Value;
end;

function TSymSrv.WinHttpErrorMessage(ErrorCode: DWORD): string;
var
  Buf: PChar;
  Len: DWORD;
  hWinHttp: HMODULE;
begin
  Result := '';
  Buf := nil;

  Len := FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS, nil, ErrorCode, 0, @Buf, 0, nil);

  if (Len = 0) or (ErrorCode >= 12000) and (ErrorCode <= 12999) then
  begin
    if Buf <> nil then
      LocalFree({%H-}HLOCAL(Buf));
    Buf := nil;
    hWinHttp := GetModuleHandle('winhttp.dll');
    if hWinHttp <> 0 then
      Len := FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_HMODULE or
        FORMAT_MESSAGE_IGNORE_INSERTS, {%H-}Pointer(hWinHttp), ErrorCode, 0, @Buf, 0, nil);
  end;

  if (Len > 0) and (Buf <> nil) then
    Result := Trim(string(Buf))
  else
    Result := Format('WinHTTP error 0x%x (%d)', [ErrorCode, ErrorCode]);

  if Buf <> nil then
    LocalFree({%H-}HLOCAL(Buf));
end;

{ TAsyncSymSrv }

procedure TAsyncSymSrv.Execute;
begin
  TThread.NameThreadForDebugging('TAsyncSymSrv');
  FSymSrv := TSymSrv.Create;
  try
    FSymSrv.ProxySettings := ProxySettings;
    FSymSrv.OnProgress := InternalProgress;
    // [ru] это только для режима pdmAsyncManual (кому-то пригодится, но это не точно)
    // [en] this applies only to pdmAsyncManual mode (it might be useful to someone, but that's not certain)
    if not FSymSrv.DownloadPdb(Pdb) then
    begin
      FLastError := FSymSrv.LastError;
      FLastErrorCode := FSymSrv.LastErrorCode;
      FLastHttpStatus := FSymSrv.LastHttpStatus;
    end;
    FState := FSymSrv.DownloadState;
  finally
    FSymSrv.Free;
  end;
end;

procedure TAsyncSymSrv.InternalProgress(Sender: TObject;
  State: TSymSrvProgressState; Pdb: TPdb; BytesReceived, TotalBytes: Int64;
  var AContinueState: TSymSrvContinueState);
begin
  FBytesReceived := BytesReceived;
  FState := State;
  FTotalBytes := TotalBytes;
  Synchronize(NotifyProgress);
  AContinueState := FContinueState;
end;

procedure TAsyncSymSrv.NotifyProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(FSymSrv, FState, Pdb, FBytesReceived, FTotalBytes, FContinueState);
end;

{ TPdbKeyComparer }

function TPdbKeyComparer.Equals({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Left, Right: TPdbKey): Boolean;
begin
  Result :=
    AnsiSameText(Left.Name, Right.Name) and
    (Left.Age = Right.Age) and
    IsEqualGUID(Left.Uid, Right.Uid);
end;

function HashBobJenkinsGetHashValue(Data: Pointer; Len: Integer; InitVal: UInt32 = 0): UInt32;
var
  P: PByte;
  I: Integer;
  Hash: UInt32;
begin
  Hash := InitVal;
  P := PByte(Data);
  for I := 0 to Len - 1 do
  begin
    Hash := Hash + P^;
    Hash := Hash + (Hash shl 10);
    Hash := Hash xor (Hash shr 6);
    Inc(P);
  end;
  Hash := Hash + (Hash shl 3);
  Hash := Hash xor (Hash shr 11);
  Hash := Hash + (Hash shl 15);
  Result := Hash;
end;

function TPdbKeyComparer.GetHashCode({$IFDEF USE_CONSTREF}constref{$ELSE}const{$ENDIF} Value: TPdbKey): UInt32;
begin
  Result := HashBobJenkinsGetHashValue(PChar(Value.Name), Length(Value.Name) * SizeOf(Char), 0);
  Result := HashBobJenkinsGetHashValue(@Value.Uid, SizeOf(TGUID), Result);
  Result := HashBobJenkinsGetHashValue(@Value.Age, SizeOf(DWORD), Result);
end;

{ TPdbStorage }

function TPdbStorage.CheckRelativeKeyName(const Value: string): Boolean;
begin
  Result := (Pos('/', Value) = 0) and (Pos('\', Value) = 0);
end;

procedure TPdbStorage.Clear;
begin
  FPdbDict.Clear;
  FPdbList.Clear;
end;

constructor TPdbStorage.Create;
const
//  DefaultSymPath = 'd:\sym;srv*https://msdl.microsoft.com/download/symbols;C:\MyProject\pdb;cache*C:\Symbols*E:\sym;cache*D:\Symbols';
  DefaultSymPath = 'srv*c:\symbols*https://msdl.microsoft.com/download/symbols';
var
  Comparer: IEqualityComparer<TPdbKey>;
  SymPath: string;
begin
  FDefaultTwoTierStorage := True;
  FCacheFolders := TStringList.Create;
  FLock := TCriticalSection.Create;
  FPdbList := TObjectList<TPdb>.Create;
  Comparer := TPdbKeyComparer.Create;
  FPdbDict := TDictionary<TPdbKey, Integer>.Create(Comparer);
  FSymServers := TStringList.Create;
  SymPath := GetEnvironmentVariable('_NT_SYMBOL_PATH');
  if SymPath = '' then
    SymPath := DefaultSymPath;
  SetSymConfig(SymPath);
end;

destructor TPdbStorage.Destroy;
begin
  FCacheFolders.Free;
  FLock.Free;
  FSymServers.Free;
  FPdbList.Free;
  FPdbDict.Free;
  inherited;
end;

function TPdbStorage.DownloadPDB(Pdb: TPdb; SyncMode: TPdbDownloadMode): TAsyncSymSrv;
var
  SyncSymSrv: TSymSrv;
  AsyncSymSrv: TAsyncSymSrv;
begin
  Result := nil;
  if Pdb.State <> psAvailableForLoading then
    raise EPdbException.Create('The parameters for downloading the PDB file have not been specified.');
  if SyncMode = pdmSync then
  begin
    SyncSymSrv := TSymSrv.Create;
    try
      SyncSymSrv.ProxySettings := ProxySettings;
      SyncSymSrv.OnProgress := OnProgress;
      if not SyncSymSrv.DownloadPdb(Pdb) then
      begin
        Pdb.UpdateState(psAvailableForLoading);
        raise EPdbException.CreateFmt(
          'Error downloading a PDB file (HTTP %d, code %d): %s',
          [SyncSymSrv.LastHttpStatus, SyncSymSrv.LastErrorCode, SyncSymSrv.LastError]);
      end;
    finally
      SyncSymSrv.Free;
    end;
  end
  else
  begin
    AsyncSymSrv := TAsyncSymSrv.Create(True);
    AsyncSymSrv.Pdb := Pdb;
    AsyncSymSrv.ProxySettings := ProxySettings;
    AsyncSymSrv.OnProgress := OnProgress;
    case SyncMode of
      pdmAsyncAuto:
      begin
        AsyncSymSrv.FreeOnTerminate := True;
        AsyncSymSrv.Start;
      end;
      pdmAsyncModal: RunThreadModal(AsyncSymSrv);
      pdmAsyncManual: Result := AsyncSymSrv;
    end;
  end;
end;

function TPdbStorage.FindLocalPdb(const APdbKey: TPdbKey): string;
var
  I: Integer;
  RelativePath, LocalPath: string;
begin
  // [ru] Пробуем прямой путь на диске
  // [en] Let's try the direct path on the disk
  if not CheckRelativeKeyName(APdbKey.Name) then
  begin
    if FileExists(APdbKey.Name) then
      Exit(APdbKey.Name)
    else
      Exit('');
  end;

  if FCacheFolders.Count = 0 then
    EPdbException.Create('The symbols server configuration has not been specified');

  RelativePath := GetRelativePath(APdbKey);

  // [ru] перебираем локальные хэши
  // [en] We're going through the local caches
  for I := 0 to FCacheFolders.Count - 1 do
  begin
    if not DirectoryExists(FCacheFolders[I]) then Continue;

    // [ru] Проверка на двухуровневое хранилище символов
    // [en] Testing for a two-level character storage system
    // https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/symbol-store-folder-tree
    LocalPath := FixUpPath(FCacheFolders[I], RelativePath, False);

    if FileExists(LocalPath) then
      Exit(LocalPath);
  end;
end;

function TPdbStorage.FixUpPath(const Folder, PdbPath: string;
  CreateNew: Boolean): string;
begin
  if CreateNew and not DirectoryExists(Folder) then
  begin
    ForceDirectories(Folder);
    if DefaultTwoTierStorage then
      TFileStream.Create(Folder + TwoTierFileName, fmCreate).Free
    else
      TFileStream.Create(Folder + OneTierFileName, fmCreate).Free;
  end;
  if FileExists(Folder + TwoTierFileName) then
    Result := Folder + Copy(ExtractFileName(PdbPath), 1, 2) + '/' + PdbPath
  else
    Result := Folder + PdbPath;
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
end;

function TPdbStorage.GetRelativePath(const APdbKey: TPdbKey): string;
begin
  if CheckRelativeKeyName(APdbKey.Name) then
    Result := Format('%s/%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%d/%s',
      [APdbKey.Name, APdbKey.Uid.D1, APdbKey.Uid.D2, APdbKey.Uid.D3,
       APdbKey.Uid.D4[0], APdbKey.Uid.D4[1], APdbKey.Uid.D4[2],
       APdbKey.Uid.D4[3], APdbKey.Uid.D4[4], APdbKey.Uid.D4[5],
       APdbKey.Uid.D4[6], APdbKey.Uid.D4[7], APdbKey.Age, APdbKey.Name])
  else
    Result := '';
end;

function TPdbStorage.GetSymSrvIndex(Value: Pointer): Integer;
begin
  Result := {%H-}Integer(Value) and $3FFFFFFF;
end;

function TPdbStorage.GetSymSrvStorageType(Value: Pointer): TSymStorageType;
begin
  Result := TSymStorageType({%H-}Integer(Value) shr 30);
end;

function TPdbStorage.MakeSymSrvData(StorageType: TSymStorageType;
  Index: Integer): Pointer;
begin
  Result := {%H-}Pointer(Integer(StorageType) shl 30 + Index);
end;

function TPdbStorage.QueryPDB(const APdbKey: TPdbKey): TPdb;
var
  Idx: Integer;
  LocalPath: string;
begin
  if FPdbDict.TryGetValue(APdbKey, Idx) then
    Result := FPdbList[Idx]
  else
  begin
    Result := TPdb.Create(APdbKey);
    Idx := FPdbList.Add(Result);
    FPdbDict.Add(APdbKey, Idx);
    Result.RelativePath := GetRelativePath(APdbKey);
    LocalPath := FindLocalPdb(APdbKey);
    if FileExists(LocalPath) then
    begin
      try
        Result.LoadFromFile(LocalPath);
        // [ru] проверка, точно ли PDB соответствует указаным параметрам?
        // [ru] verify that the PDB matches the specified parameters?
        if Result.State in [psNoSections, psReady] then
        begin
          if (APdbKey.Age <> Result.Age) or not IsEqualGUID(APdbKey.Uid, Result.UnicalId) then
            Result.UpdateState(psBroken)
          else
            Exit;
        end;
      except
        Result.UpdateState(psBroken);
      end;
    end;
  end;
end;

function TPdbStorage.QueryPDB(const Uid: TGUID; Age: DWORD;
  const PdbFileName: string): TPdb;
var
  APdbKey: TPdbKey;
begin
  APdbKey.Uid := Uid;
  APdbKey.Age := Age;
  APdbKey.Name := PdbFileName;
  Result := QueryPDB(APdbKey);
end;

function TPdbStorage.QueryRemotePDB(Pdb: TPdb): TPdbState;
var
  SymSrv: TSymSrv;
  StorageType: TSymStorageType;
  LocalPath, RemotePath: string;
  I, A: Integer;
begin

  // [ru] Страхуемся от ошибки программиста, если символы УЖЕ доступны
  // [ru] незачем опрашивать серверы символов

  // [en] Guard against a programmer error if the symbols are ALREADY available —
  // [en] no need to query the symbol servers

  if Pdb.State in [psNoSections, psReady] then
    Exit(Pdb.State)
  else
    Result := psNotFoundAnywhere;

  if Pdb.RelativePath = '' then
  begin
    Pdb.UpdateState(Result);
    Exit;
  end;

  try
    SymSrv := TSymSrv.Create;
    try
      SymSrv.ProxySettings := ProxySettings;
      for I := 0 to FSymServers.Count - 1 do
      begin
        // [ru] Проверка на двухуровневое хранилище теперь у сервера символов
        // [en] The two-tier storage check is now at the character server
        FLock.Enter;
        try
          StorageType := GetSymSrvStorageType(FSymServers.Objects[I]);
          if StorageType = sstUnknown then
          begin
            A := GetSymSrvIndex(FSymServers.Objects[I]);
            if SymSrv.CheckTwoTierStorage(FSymServers[I]) then
              StorageType := sstTwoTiear
            else
              StorageType := sstOneTier;
            FSymServers.Objects[I] := MakeSymSrvData(StorageType, A);
          end;
        finally
          FLock.Leave;
        end;

        if StorageType = sstTwoTiear then
          RemotePath := FSymServers[I] + Copy(Pdb.PdbName, 1, 2) + '/' + Pdb.RelativePath
        else
          RemotePath := FSymServers[I] + Pdb.RelativePath;

        if not SymSrv.CheckRemoteFilePresent(RemotePath) then
          Continue;

        // [ru] Теперь нужно определить расположение кэша символов в который будет происходить скачивание
        // [en] Now we need to determine the location of the character cache where the download will take place
        for A := 0 to FCacheFolders.Count - 1 do
          if Integer(FCacheFolders.Objects[A]) = GetSymSrvIndex(FSymServers.Objects[I]) then
          begin
            // [ru] ... и опять не забыть проверку на двухуровневое хранилище
            // [en] ... and again, don't forget the check for the two-tier storage
            LocalPath := FixUpPath(FCacheFolders[A], Pdb.RelativePath, True);
            Break;
          end;

        Pdb.RemotePath := RemotePath;
        Pdb.SetLocalPath(LocalPath);
        Result := psAvailableForLoading;
        Break;
      end;
    finally
      SymSrv.Free;
    end;
  except
    Result := psNotFoundAnywhere;
  end;

  Pdb.UpdateState(Result);
end;

procedure TPdbStorage.RunThreadModal(AThread: TThread);
var
  AHandle: THandle;
  WaitResult: DWORD;
  Msg: TMsg;
begin
  AThread.FreeOnTerminate := False;
  AThread.Start;
  try
    AHandle := AThread.Handle;
    repeat
      WaitResult := MsgWaitForMultipleObjects(1, AHandle, False, INFINITE, QS_ALLINPUT);
      if WaitResult = WAIT_OBJECT_0 + 1 then
      begin
        CheckSynchronize;
        while PeekMessage(Msg{%H-}, 0, 0, 0, PM_REMOVE) do
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    until WaitResult = WAIT_OBJECT_0;
  finally
    AThread.Free;
  end;
end;

procedure TPdbStorage.SetProxySettings(const Value: TProxySettings);
begin
  ValidateProxy(Value);
  FProxySettings := Value;
end;

procedure TPdbStorage.SetSymConfig(const Value: string);

  procedure InsertCacheFolder(Value: string; ALevel: Integer);
  var
    I: Integer;
  begin
    Value := IncludeTrailingPathDelimiter(Value);
    for I := 0 to FCacheFolders.Count - 1 do
      if Integer(FCacheFolders.Objects[I]) > ALevel then
      begin
        FCacheFolders.InsertObject(I, Value, {%H-}Pointer(ALevel));
        Exit;
      end;
    FCacheFolders.AddObject(Value, {%H-}Pointer(ALevel));
  end;

var
  Parts, SubParts: TArray<string>;
  I, A, WaitCacheIdx: Integer;
begin
  if FSymConfig <> Value then
  begin
    FSymConfig := Value;
    FSymServers.Clear;
    FCacheFolders.Clear;
    Parts := Value.Split([';']);
    WaitCacheIdx := -1;
    for I := 0 to Length(Parts) - 1 do
    begin

      // [ru] основная конфигурация сервера загрузок символов
      // [en] main configuration of the character download server
      if Parts[I].StartsWith('SRV*', True) then
      begin
        SubParts := Parts[I].Split(['*']);
        WaitCacheIdx := I;
        for A := 1 to Length(SubParts) - 1 do
          if SubParts[A].StartsWith('HTTP', True) then
          begin
            if SubParts[A][Length(SubParts[A])] <> '/' then
              FSymServers.AddObject(SubParts[A] + '/', MakeSymSrvData(sstUnknown, I))
            else
              FSymServers.AddObject(SubParts[A], MakeSymSrvData(sstUnknown, I));
            Break;
          end
          else
          begin
            WaitCacheIdx := -1;
            InsertCacheFolder(SubParts[A], I);
          end;
        Continue;
      end;

      // [ru] кэш обрабатывается только для нестандартной ситуации
      // [ru] когда у SRV слева от кэша не указана своя папка для кэша
      // [ru] все остальные типы кэша игнорируются

      // [en] cache is processed only for non-standard situations
      // [en] when the SRV path to the left of the cache does not specify its own cache folder
      // [en] all other cache types are ignored

      if Parts[I].StartsWith('CACHE*', True) then
      begin
        if WaitCacheIdx >= 0 then
        begin
          SubParts := Parts[I].Split(['*']);
          for A := 1 to Length(SubParts) - 1 do
            InsertCacheFolder(SubParts[A], WaitCacheIdx);
          WaitCacheIdx := -1;
        end;
        Continue;
      end;

      // [ru] кастомная библиотека нас не интересует, у нас всё своё
      // [en] we don't care about the SYMSRV parameter
      if Parts[I].StartsWith('SYMSRV*', True) then
        Continue;

      // [ru] будем считать это папкой
      // [en] we will consider this a folder
      InsertCacheFolder(Parts[I], I);
    end;
  end;
end;

{ TSymWorkerThread }

constructor TSymWorkerThread.Create(AOwner: TSymQueue);
begin
  inherited Create(True);
  FOwner := AOwner;
  FreeOnTerminate := False;
end;

procedure TSymWorkerThread.Execute;
var
  Job: TSymJob;
begin
  TThread.NameThreadForDebugging('TSymWorkerThread');
  while not Terminated do
  begin
    if FOwner.GetNextJob(Job) then
      FOwner.RunJob(Job)
    else
      Break;
  end;
end;

{ TSymJobQueue }

constructor TSymJobQueue.Create;
begin
  inherited Create;
  FItems := TQueue<TSymJob>.Create;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, True, False, '');
  FContinueState := scsRun;
end;

destructor TSymJobQueue.Destroy;
begin
  FEvent.Free;
  FLock.Free;
  FItems.Free;
  inherited;
end;

procedure TSymJobQueue.Pause;
begin
  FContinueState := scsPause;
end;

function TSymJobQueue.Pop(out Job: TSymJob): Boolean;
begin
  while True do
  begin
    FLock.Enter;
    try
      case FContinueState of
        scsRun:
        begin
          if FItems.Count > 0 then
          begin
            Job := FItems.Dequeue;
            if FItems.Count = 0 then
              FEvent.ResetEvent;
            Exit(True);
          end;
        end;
        scsPause: FEvent.ResetEvent;
        scsCancel: Exit(False);
      end;
    finally
      FLock.Leave;
    end;
    FEvent.WaitFor(INFINITE);
  end;
end;

procedure TSymJobQueue.Push(const Job: TSymJob);
begin
  FLock.Enter;
  try
    FItems.Enqueue(Job);
  finally
    FLock.Leave;
  end;
  FEvent.SetEvent;
end;

procedure TSymJobQueue.Resume;
begin
  FLock.Enter;
  try
    FContinueState := scsRun;
  finally
    FLock.Leave;
  end;
  FEvent.SetEvent;
end;

procedure TSymJobQueue.Stop;
begin
  FLock.Enter;
  try
    FContinueState := scsCancel;
  finally
    FLock.Leave;
  end;
  FEvent.SetEvent;
end;

{ TSymQueue }

constructor TSymQueue.Create(AStorage: TPdbStorage; AWorkerCount: Integer);
var
  I: Integer;
begin
  inherited Create;
  FStorage := AStorage;
  FStorage.OnProgress := HandleProgress;
  FJobs := TSymJobQueue.Create;
  FActiveWorkers := AWorkerCount;
  SetLength(FWorkers, AWorkerCount);
  for I := 0 to AWorkerCount - 1 do
  begin
    FWorkers[I] := TSymWorkerThread.Create(Self);
    FWorkers[I].FreeOnTerminate := True;
    FWorkers[I].OnTerminate := WorkerTerminated;
  end;
  for I := 0 to AWorkerCount - 1 do
    FWorkers[I].Start;
end;

destructor TSymQueue.Destroy;
var
  Worker: TSymWorkerThread;
begin
  if not FStopped then
  begin
    for Worker in FWorkers do
      Worker.OnTerminate := nil;
    Stop;
  end;
  FJobs.Free;
  inherited;
end;

procedure TSymQueue.DownloadPdb(Pdb: TPdb);
var
  Job: TSymJob;
begin
  Job.Pdb := Pdb;
  Job.Kind := jkDownload;
  InterLockedIncrement(FTotal);
  FJobs.Push(Job);
end;

function TSymQueue.GetNextJob(out Job: TSymJob): Boolean;
begin
  Result := FJobs.Pop(Job);
end;

procedure TSymQueue.HandleProgress(Sender: TObject;
  State: TSymSrvProgressState; Pdb: TPdb; BytesReceived, TotalBytes: Int64;
  var AContinueState: TSymSrvContinueState);
var
  Notify: TQueuedProgressNotify;
begin
  AContinueState := FContinueState;
  Notify := TQueuedProgressNotify.Create(Self, State, Pdb, BytesReceived, TotalBytes);
  TThread.Queue(nil, Notify.Execute);
end;

procedure TSymQueue.NotifyJobDone(const Job: TSymJob;
  const Error: string);
var
  Done: Integer;
  Notify: TQueuedJobDoneNotify;
begin
  Done := InterLockedIncrement(FDone);
  Notify := TQueuedJobDoneNotify.Create(Self, Job, Error, Done);
  TThread.Queue(nil, Notify.Execute);
end;

procedure TSymQueue.Pause;
begin
  if FStopped then Exit;
  FContinueState := scsPause;
  FJobs.Pause;
end;

procedure TSymQueue.QueryPdb(Pdb: TPdb);
var
  Job: TSymJob;
begin
  Job.Pdb := Pdb;
  Job.Kind := jkQuery;
  InterlockedIncrement(FTotal);
  FJobs.Push(Job);
end;

procedure TSymQueue.Resume;
begin
  if FStopped then Exit;
  FContinueState := scsRun;
  FJobs.Resume;
end;

procedure TSymQueue.RunJob(const Job: TSymJob);
var
  Error: string;
begin
  Error := '';
  try
    case Job.Kind of
      jkQuery:
        FStorage.QueryRemotePDB(Job.Pdb);
      jkDownload:
        FStorage.DownloadPDB(Job.Pdb, pdmSync);
    end;
  except
    on E: Exception do
      Error := E.ClassName + ': ' + E.Message;
  end;
  NotifyJobDone(Job, Error);
end;

procedure TSymQueue.Stop;
begin
  if FStopped then Exit;
  FStopped := True;
  FContinueState := scsCancel;
  FJobs.Stop;
end;

procedure TSymQueue.WorkerTerminated(Sender: TObject);
begin
  if InterlockedDecrement(FActiveWorkers) = 0 then
    if Assigned(FOnStopped) then
      FOnStopped(Self);
end;

{ TQueuedProgressNotify }

constructor TQueuedProgressNotify.Create(AOwner: TSymQueue;
  AState: TSymSrvProgressState; APdb: TPdb; ABytesReceived, ATotalBytes: Int64);
begin
  inherited Create;
  FOwner := AOwner;
  FState := AState;
  FPdb := APdb;
  FBytesReceived := ABytesReceived;
  FTotalBytes := ATotalBytes;
end;

procedure TQueuedProgressNotify.Execute;
begin
  try
    if Assigned(FOwner.OnProgress) then
      FOwner.OnProgress(FOwner, FState, FPdb, FBytesReceived, FTotalBytes);
  finally
    Free;
  end;
end;

{ TQueuedJobDoneNotify }

constructor TQueuedJobDoneNotify.Create(AOwner: TSymQueue; const AJob: TSymJob;
  const AError: string; ADone: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FJob := AJob;
  FError := AError;
  FDone := ADone;
end;

procedure TQueuedJobDoneNotify.Execute;
begin
  try
    if Assigned(FOwner.OnJobDone) then
      FOwner.OnJobDone(FOwner, FJob, FError);
    if (FDone >= FOwner.Total) and Assigned(FOwner.OnBatchDone) then
      FOwner.OnBatchDone(FOwner);
  finally
    Free;
  end;
end;

end.
