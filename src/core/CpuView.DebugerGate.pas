unit CpuView.DebugerGate;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I CpuViewCfg.inc}

interface

uses
  Classes,
  Forms,
  Generics.Collections,
  CpuView.Common,
  CpuView.CPUContext,
  CpuView.Stream,
  CpuView.Viewers;

type
  // минимальный абстрактный интерфейс под разные отладчики
  TAbstractDebugState = (adsStoped, adsStart, adsPaused, adsRunning, adsFinished);
  TInterfaceDebugCommand = (idcRun, idcRunTo, idcPause, idcStepInto, idcStepOver, idcStepOut, idcBreakPoint);

  TQuerySymbol = (qsName, qsSourceLine);

  {$message 'Адреса не могут быть Int64 - переделать на UInt64'}
  TInstruction = record
    AddrVA: Int64;
    AsString, Hint: string;
    Len: Integer;
    JmpTo: Int64;
  end;

  TBasicBreakPoint = record
    Active: Boolean;
    AddrVA: UInt64;
  end;

  { TAbstractDebugger }

  TAbstractDebugger = class(TComponent)
  private
    FBreakPointList: TList<TBasicBreakPoint>;
    FCpuViewForm: TCustomForm;
    FCtx: TCommonCpuContext;
    FUtils: TCommonAbstractUtils;
    FChange: TNotifyEvent;
    FBreakPointsChange, FCtxChange, FStateChange: TNotifyEvent;
    procedure SetCtx(AValue: TCommonCpuContext);
  protected
    procedure ContextUpdate(Sender: TObject; AChangeType: TContextChangeType);
    procedure DoBreakPointsChange;
    procedure DoChange;
    procedure DoStateChange;
    function GetUtilsClass: TCommonAbstractUtilsClass; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateContext; virtual;
  public
    constructor Create(ACpuViewForm: TCustomForm); reintroduce; virtual;
    destructor Destroy; override;
    function CommandAvailable(ACommand: TInterfaceDebugCommand): Boolean; virtual; abstract;
    function CurrentInstructionPoint: UInt64; virtual; abstract;
    function DebugState: TAbstractDebugState; virtual; abstract;
    function Disassembly(AddrVA: Int64; pBuff: PByte; nSize: Integer): TList<TInstruction>; virtual; abstract;
    function IsActive: Boolean; virtual; abstract;
    function IsActiveJmp: Boolean; virtual; abstract;
    procedure FillThreadStackFrames(ALimit: TStackLimit;
      AddrStack, AddrFrame: UInt64; AStream: TRemoteStream;
      AFrames: TList<TStackFrame>); virtual;
    function GetSourceLine(AddrVA: Int64; out ASourcePath: string;
      out ASourceLine: Integer): Boolean; virtual; abstract;
    procedure Pause; virtual; abstract;
    function PointerSize: Integer; virtual; abstract;
    function ProcessID: Cardinal; virtual; abstract;
    function QuerySymbolAtAddr(AddrVA: Int64; AParam: TQuerySymbol): string; virtual; abstract;
    function ReadMemory(AddrVA: Int64; var Buff; Size: Integer): Boolean; virtual; abstract;
    procedure Run; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure SetNewIP(AddrVA: UInt64); virtual; abstract;
    function ThreadID: Cardinal; virtual; abstract;
    function ThreadStackLimit: TStackLimit; virtual; abstract;
    procedure ToggleBreakPoint(AddrVA: UInt64); virtual; abstract;
    procedure TraceIn; virtual; abstract;
    procedure TraceOut; virtual; abstract;
    procedure TraceTilReturn; virtual; abstract;
    procedure TraceTo(AddrVA: Int64); virtual; abstract;
    function UpdateRegValue(RegID: Integer; ANewRegValue: UInt64): Boolean; virtual; abstract;
    procedure UpdateRemoteStream(pBuff: PByte; AAddrVA: UInt64; ASize: Int64); virtual; abstract;
    property BreakPointList: TList<TBasicBreakPoint> read FBreakPointList;
    property Context: TCommonCpuContext read FCtx write SetCtx;
    property CpuViewForm: TCustomForm read FCpuViewForm;
    property Utils: TCommonAbstractUtils read FUtils;
    property OnChange: TNotifyEvent read FChange write FChange;
    property OnContextChange: TNotifyEvent read FCtxChange write FCtxChange;
    property OnStateChange: TNotifyEvent read FStateChange write FStateChange;
    property OnBreakPointsChange: TNotifyEvent read FBreakPointsChange write FBreakPointsChange;
  end;

implementation

uses
  FWHexView.Common;

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
  FUtils.Free;
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

procedure TAbstractDebugger.DoStateChange;
begin
  if Assigned(FStateChange) then
    FStateChange(Self);
end;

procedure TAbstractDebugger.FillThreadStackFrames(ALimit: TStackLimit;
  AddrStack, AddrFrame: UInt64; AStream: TRemoteStream;
  AFrames: TList<TStackFrame>);

  function InStack(AddrVA: Uint64): Boolean;
  begin
    Result := (AddrVA <= ALimit.Base) and (AddrVA >= ALimit.Limit);
  end;

var
  AFrame: TStackFrame;
  NewAddrFrame: UInt64;
begin
  AFrames.Clear;
  Dec(ALimit.Base, PointerSize);
  AFrame.AddrStack := AddrStack;
  AFrame.AddrFrame := AddrFrame;
  {$IFDEF USE_INTEL_CTX}
  repeat
    AFrame.AddrPC := AFrame.AddrFrame + UInt64(PointerSize);
    AFrames.Add(AFrame);
    AFrame.AddrStack := AFrame.AddrPC + UInt64(PointerSize);
    AStream.Position := AFrame.AddrFrame;
    AStream.ReadBuffer(NewAddrFrame, PointerSize);
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

constructor TAbstractDebugger.Create(ACpuViewForm: TCustomForm);
begin
  inherited Create(ACpuViewForm);
  FUtils := GetUtilsClass.Create;
  FCpuViewForm := ACpuViewForm;
  FBreakPointList := TList<TBasicBreakPoint>.Create;
end;

end.
