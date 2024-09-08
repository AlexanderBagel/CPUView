unit CpuView.Actions;

interface

uses
  SysUtils,
  Classes,
  FWHexView.Actions,
  CpuView.Common;

const
  IID_CpuContextViewModeAction = '{38F0077D-9691-4C81-8E0B-BD9907A77991}';

type
  ICpuContextViewModeAction = interface
    [IID_CpuContextViewModeAction]
    function ContextViewModeCommandEnabled(Value: TRegViewMode; var AChecked: Boolean): Boolean;
    function ContextViewModeCommandHandled(Value: TRegViewMode): Boolean;
    procedure ContextViewModeCommandExecute(Value: TRegViewMode);
  end;

  TCpuContextCustomRegViewModeAction = class(THexViewBasicAction)
  private const
    stCaptions: array [TRegViewMode] of string = (
      'Hex', 'Hex Word', 'Hex Dword', 'Hex Qword',
      'Octal', 'Binary',
      'Signed Byte', 'Unsigned Byte',
      'Signed Word', 'Signed Dword', 'Signed Qword',
      'Unsigned Word', 'Unsigned Dword', 'Unsigned Qword',
      'Single', 'Double', 'Extended 80-bit');
  private
    FAutoHide: Boolean;
    FRegViewMode: TRegViewMode;
    procedure UpdateRegViewMode(const Value: TRegViewMode);
    procedure SetRegViewMode(const Value: TRegViewMode);
    procedure SetAutoHide(const Value: Boolean);
  protected
    function IsCaptionStored: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  protected
    property AutoHide: Boolean read FAutoHide write SetAutoHide default True;
    property Caption stored IsCaptionStored;
    property RegViewMode: TRegViewMode read FRegViewMode write SetRegViewMode default rvmHex;
  end;

  TCpuContextRegViewModeAction = class(TCpuContextCustomRegViewModeAction)
  published
    property AutoHide;
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property ImageIndex;
    property RegViewMode;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnExecuteTarget;
    property OnHandlesTarget;
    property OnHint;
    property OnUpdateTarget;
  end;

implementation

{ TCpuContextCustomRegViewModeAction }

constructor TCpuContextCustomRegViewModeAction.Create(AOwner: TComponent);
begin
  inherited;
  FAutoHide := True;
  UpdateRegViewMode(RegViewMode);
end;

procedure TCpuContextCustomRegViewModeAction.ExecuteTarget(Target: TObject);
var
  Intf: ICpuContextViewModeAction;
begin
  if csDesigning in ComponentState then Exit;
  if Assigned(Target) and Supports(Target, ICpuContextViewModeAction, Intf) then
    Intf.ContextViewModeCommandExecute(RegViewMode);
  DoExecuteTarget(Target);
end;

function TCpuContextCustomRegViewModeAction.HandlesTarget(
  Target: TObject): Boolean;
var
  Intf: ICpuContextViewModeAction;
begin
  if csDesigning in ComponentState then Exit(True);
  Result :=
    Assigned(Target) and
    Supports(Target, ICpuContextViewModeAction, Intf) and
    Intf.ContextViewModeCommandHandled(RegViewMode);
  DoHandlesTarget(Target, Result);
  if AutoHide then
    Visible := Result;
end;

function TCpuContextCustomRegViewModeAction.IsCaptionStored: Boolean;
begin
  Result := Caption <> stCaptions[RegViewMode];
end;

procedure TCpuContextCustomRegViewModeAction.SetAutoHide(const Value: Boolean);
begin
  if AutoHide <> Value then
  begin
    FAutoHide := Value;
    Update;
  end;
end;

procedure TCpuContextCustomRegViewModeAction.SetRegViewMode(
  const Value: TRegViewMode);
begin
  if RegViewMode <> Value then
  begin
    FRegViewMode := Value;
    UpdateRegViewMode(RegViewMode);
  end;
end;

procedure TCpuContextCustomRegViewModeAction.UpdateRegViewMode(
  const Value: TRegViewMode);
begin
  Caption := stCaptions[Value];
end;

procedure TCpuContextCustomRegViewModeAction.UpdateTarget(Target: TObject);
var
  Intf: ICpuContextViewModeAction;
  AEnabled, AChecked: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    Enabled := True;
    Exit;
  end;
  AChecked := False;
  AEnabled :=
    Assigned(Target) and
    Supports(Target, ICpuContextViewModeAction, Intf) and
    Intf.ContextViewModeCommandEnabled(RegViewMode, AChecked);
  DoUpdateTarget(Target, AEnabled, AChecked);
  Enabled := AEnabled;
  Checked := AChecked;
end;

end.
