////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.DebugerGate.pas
//  * Purpose   : An abstract gateway to bind CPU-View with any external debuggers.
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

unit CpuView.DebugerGate;

{$IFDEF FPC}
  {$MODE Delphi}
  {$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}

{$I CpuViewCfg.inc}

interface

uses
  Classes,
  Forms,
  Generics.Collections,
  FWHexView.Common,
  CpuView.Common,
  CpuView.CPUContext,
  CpuView.Stream;

const
  UserCodeAddrVANotFound = -1;
  UserCodeAddrVAFound = 0;

type

  // минимальный абстрактный интерфейс под разные отладчики
  // minimal abstract interface for different debuggers

  TAbstractDebugState = (adsError, adsStoped, adsStart, adsPaused, adsRunning, adsFinished);
  TInterfaceDebugCommand = (idcRun, idcRunTo, idcPause, idcStepInto, idcStepOver, idcStepOut, idcBreakPoint, idcRunToUserCode);

  TQuerySymbol = (qsName, qsSourceLine, qsAddrVA);

  TQuerySymbolValue = record
    AddrVA: Int64;
    Description: string;
  end;

  TInstruction = record
    AddrVA: Int64;
    AsString, Hint: string;
    Len: Integer;
    JmpTo: Int64;
  end;

  TBasicBreakPoint = record
    Active: Boolean;
    AddrVA: Int64;
  end;

  {$IFNDEF FPC}
  TLibHandle = THandle;
  {$ENDIF}

  TRemoteModule = record
    hInstance: TLibHandle;
    ImageBase: Int64;
    LibraryPath: string;
  end;

  TRemoteProc = record
    AddrVA: Int64;
    FuncName: string;
  end;

  { TAbstractDebugger }

  TAbstractDebugger = class(TComponent)
  private
    FBreakPointList: TListEx<TBasicBreakPoint>;
    FCtx: TCommonCpuContext;
    FUtils: TCommonAbstractUtils;
    FChange: TNotifyEvent;
    FErrorMessage: string;
    FShowFullAddress, FShowSourceLines, FUseDebugInfo, FUseCacheFoExternalSymbols: Boolean;
    FBreakPointsChange, FCtxChange, FStateChange, FThreadChange: TNotifyEvent;
    procedure SetCtx(AValue: TCommonCpuContext);
  protected
    procedure ContextUpdate(Sender: TObject; AChangeType: TContextChangeType);
    procedure DoBreakPointsChange;
    procedure DoChange;
    procedure DoError(const AMessage: string);
    procedure DoStateChange;
    procedure DoThreadChange;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateContext; virtual;
  public
    constructor Create(AOwner: TComponent; AUtils: TCommonAbstractUtils); reintroduce; virtual;
    destructor Destroy; override;
    function CommandAvailable(ACommand: TInterfaceDebugCommand): Boolean; virtual; abstract;
    function CurrentInstructionPoint: Int64; virtual; abstract;
    function DebugState: TAbstractDebugState; virtual; abstract;
    function Disassembly(AddrVA: Int64; pBuff: PByte; nSize: Integer;
      AShowSourceLines: Boolean): TList<TInstruction>; virtual; abstract;
    function IsActive: Boolean; virtual; abstract;
    function IsActiveJmp: Boolean; virtual; abstract;
    function IsUserCode(AAddrVA: Int64): Boolean; virtual; abstract;
    procedure FillThreadStackFrames(ALimit: TStackLimit;
      AddrStack, AddrFrame: Int64; AStream: TRemoteStream;
      AFrames: TList<TStackFrame>); virtual;
    function GetRemoteModuleHandle(const ALibraryName: string): TRemoteModule; virtual; abstract;
    function GetRemoteModules: TList<TRemoteModule>; virtual; abstract;
    function GetRemoteProcList(const AModule: TRemoteModule): TList<TRemoteProc>; virtual; abstract;
    function GetRemoteProcAddress(ALibHandle: TLibHandle; const AProcName: string): Int64; virtual; abstract;
    function GetReturnAddrVA: Int64; virtual; abstract;
    function GetSourceLine(AddrVA: Int64; out ASourcePath: string;
      out ASourceLine: Integer): Boolean; virtual; abstract;
    function GetUserCodeAddrVA: Int64; virtual; abstract;
    procedure Pause; virtual; abstract;
    function PointerSize: Integer; virtual; abstract;
    function ProcessID: Cardinal; virtual; abstract;
    function QuerySymbolAtAddr(AddrVA: Int64; AParam: TQuerySymbol): TQuerySymbolValue; virtual; abstract;
    function ReadMemory(AddrVA: Int64; var Buff; Size: Integer): Boolean; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure SetNewIP(AddrVA: Int64); virtual; abstract;
    function ThreadID: Cardinal; virtual; abstract;
    function ThreadStackLimit: TStackLimit; virtual; abstract;
    procedure ToggleBreakPoint(AddrVA: Int64); virtual; abstract;
    procedure TraceIn; virtual; abstract;
    procedure TraceOut; virtual; abstract;
    procedure TraceTilReturn; virtual; abstract;
    procedure TraceTo(AddrVA: Int64); virtual; abstract;
    procedure TraceToList(AddrVA: array of Int64); virtual; abstract;
    function UpdateRegValue(RegID: Integer; ANewRegValue: TRegValue): Boolean; virtual; abstract;
    procedure UpdateRemoteStream(pBuff: PByte; AAddrVA: Int64; ASize: Int64); virtual; abstract;
    property BreakPointList: TListEx<TBasicBreakPoint> read FBreakPointList;
    property Context: TCommonCpuContext read FCtx write SetCtx;
    property ErrorMessage: string read FErrorMessage;
    property ShowFullAddress: Boolean read FShowFullAddress write FShowFullAddress;
    property ShowSourceLines: Boolean read FShowSourceLines write FShowSourceLines;
    property UseDebugInfo: Boolean read FUseDebugInfo write FUseDebugInfo;
    property UseCacheFoExternalSymbols: Boolean read FUseCacheFoExternalSymbols write FUseCacheFoExternalSymbols;
    property Utils: TCommonAbstractUtils read FUtils;
    property OnChange: TNotifyEvent read FChange write FChange;
    property OnContextChange: TNotifyEvent read FCtxChange write FCtxChange;
    property OnStateChange: TNotifyEvent read FStateChange write FStateChange;
    property OnThreadChange: TNotifyEvent read FThreadChange write FThreadChange;
    property OnBreakPointsChange: TNotifyEvent read FBreakPointsChange write FBreakPointsChange;
  end;

  TAbstractDebuggerClass = class of TAbstractDebugger;

implementation

{ TAbstractDebugger }

procedure TAbstractDebugger.SetCtx(AValue: TCommonCpuContext);
begin
  if FCtx = AValue then Exit;
  if Assigned(FCtx) then
  begin
    FCtx.UnRegisterChangeNotification(ContextUpdate);
    FCtx.RemoveFreeNotification(Self);
  end;
  FCtx := AValue;
  if Assigned(FCtx) then
  begin
    FCtx.RegisterChangeNotification(ContextUpdate);
    FCtx.FreeNotification(Self);
  end;
  UpdateContext;
end;

procedure TAbstractDebugger.ContextUpdate(Sender: TObject;
  AChangeType: TContextChangeType);
begin
  if Assigned(FCtxChange) and (AChangeType = cctDataChange) then
    FCtxChange(Self);
end;

destructor TAbstractDebugger.Destroy;
begin
  FBreakPointList.Free;
  inherited;
end;

procedure TAbstractDebugger.DoBreakPointsChange;
begin
  if Assigned(FBreakPointsChange) then
    FBreakPointsChange(Self);
end;

procedure TAbstractDebugger.DoChange;
begin
  if Assigned(FChange) then
    FChange(Self);
end;

procedure TAbstractDebugger.DoError(const AMessage: string);
begin
  FErrorMessage := AMessage;
end;

procedure TAbstractDebugger.DoStateChange;
begin
  if Assigned(FStateChange) then
    FStateChange(Self);
end;

procedure TAbstractDebugger.DoThreadChange;
begin
  if Assigned(FThreadChange) then
    FThreadChange(Self);
end;

procedure TAbstractDebugger.FillThreadStackFrames(ALimit: TStackLimit;
  AddrStack, AddrFrame: Int64; AStream: TRemoteStream;
  AFrames: TList<TStackFrame>);

  function InStack(AddrVA: Int64): Boolean;
  begin
    Result := (AddrVA <= ALimit.Base) and (AddrVA >= ALimit.Limit);
  end;

var
  AFrame: TStackFrame;
  NewAddrFrame: Int64;
begin
  AFrames.Clear;
  Dec(ALimit.Base, PointerSize);
  AFrame.AddrStack := AddrStack;
  AFrame.AddrFrame := AddrFrame;
  {$IFDEF USE_INTEL_CTX}
  repeat
    AFrame.AddrPC := AFrame.AddrFrame + Int64(PointerSize);
    AFrames.Add(AFrame);
    AFrame.AddrStack := AFrame.AddrPC + Int64(PointerSize);
    AStream.Position := AFrame.AddrFrame;
    AStream.ReadBuffer(NewAddrFrame{%H-}, PointerSize);
    if AFrame.AddrFrame >= NewAddrFrame then
      Break;
    AFrame.AddrFrame := NewAddrFrame;
  until not InStack(AFrame.AddrFrame);
  {$ELSE}
    {$message 'not implemented'}
    AFrames.Add(AFrame);
  {$ENDIF}
end;

procedure TAbstractDebugger.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Context) then
    Context := nil;
end;

procedure TAbstractDebugger.UpdateContext;
begin
  if Assigned(Context) then
  begin
    Context.ThreadID := ThreadID;
    Utils.ThreadID := ThreadID;
    Context.Utils := Utils;
    case PointerSize of
      4: Context.AddressMode := am32bit;
      8: Context.AddressMode := am64bit;
    else
      Exit;
    end;
    Context.Update(CurrentInstructionPoint);
  end;
end;

constructor TAbstractDebugger.Create(AOwner: TComponent;
  AUtils: TCommonAbstractUtils);
begin
  inherited Create(AOwner);
  FUtils := AUtils;
  FBreakPointList := TListEx<TBasicBreakPoint>.Create;
end;

end.
