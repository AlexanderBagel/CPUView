unit dlgCpuView.TemporaryLocker;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

var
  InterceptorActive: Boolean;
  InterceptorOwner: THandle;

  procedure SetHooks;
  procedure ResetHooks;

implementation

{$IFDEF LINUX}
procedure SetHooks;
begin

end;

procedure ResetHooks;
begin

end;
{$ENDIF}

{$IFDEF MSWINDOWS}

uses
  Windows;

// обвес под блокирование переключения на другие окна отладки

function FindIATEntry(AddrVA: Pointer): Pointer;
begin
  if AddrVA = nil then Exit(nil);
  Result := Pointer(
    {$IFDEF CPUX64}
    // RIP
    PByte(AddrVA) + 6 +
    {$ENDIF}
    // IAT gate
    PCardinal(PByte(AddrVA) + 2)^);
end;

function ReplaceFunction(pOldFunction: PPointer; pNewFunction: Pointer): Boolean;
var
  dwOldProtect: DWORD = 0;
begin
  Result := False;
  if VirtualProtect(pOldFunction, SizeOf(Pointer), PAGE_READWRITE, dwOldProtect) then
  begin
    pOldFunction^ := pNewFunction;
    Result := VirtualProtect(pOldFunction, SizeOf(Pointer), dwOldProtect, dwOldProtect);
  end;
  if Result then
    FlushInstructionCache(GetCurrentProcess, pOldFunction, SizeOf(Pointer));
end;

type
  TSetWindowPos = function (hWnd: THandle; hWndInsertAfter: THandle;
    X, Y, cx, cy: Integer; uFlags: UINT): BOOL; stdcall;

var
  SetWindowPosIAT: PPointer;
  OriginalSetWindowPos: TSetWindowPos;

function InterceptedSetWindowPos(hWnd: THandle; hWndInsertAfter: THandle;
  X, Y, cx, cy: Integer; uFlags: UINT): BOOL; stdcall;
begin
  if InterceptorActive and (hWnd <> InterceptorOwner) then
  begin
    uFlags := uFlags or SWP_NOACTIVATE or SWP_NOZORDER;
    if hWndInsertAfter = HWND_TOP then
      hWndInsertAfter := HWND_BOTTOM;
    if hWndInsertAfter = HWND_TOPMOST then
      hWndInsertAfter := HWND_BOTTOM;
  end;
  Result := OriginalSetWindowPos(hWnd, hWndInsertAfter, X, Y, cx, cy, uFlags);
end;

type
  TSetFocus = function (hWnd: THandle): THandle; stdcall;

var
  SetFocusIAT: PPointer;
  OriginalSetFocus: TSetFocus;

function InterceptedSetFocus(hWnd: THandle): THandle; stdcall;
begin
  if InterceptorActive and (hWnd <> InterceptorOwner) then Exit(0);
  Result := OriginalSetFocus(hWnd);
end;

type
  TSetForegroundWindow = function (hWnd: THandle): BOOL; stdcall;

var
  SetForegroundWindowIAT: PPointer;
  OriginalSetForegroundWindow: TSetForegroundWindow;

function InterceptedSetForegroundWindow(hWnd: HWND): BOOL; stdcall;
begin
  if InterceptorActive and (hWnd <> InterceptorOwner) then Exit(True);
  Result := OriginalSetForegroundWindow(hWnd);
end;

procedure SetHooks;
var
  Tmp: Pointer;
begin
  SetWindowPosIAT := FindIATEntry(@Windows.SetWindowPos);
  Tmp := SetWindowPosIAT^;
  if Assigned(SetWindowPosIAT) and ReplaceFunction(SetWindowPosIAT, @InterceptedSetWindowPos) then
    OriginalSetWindowPos := TSetWindowPos(Tmp);

  SetFocusIAT := FindIATEntry(@Windows.SetFocus);
  Tmp := SetFocusIAT^;
  if Assigned(SetFocusIAT) and ReplaceFunction(SetFocusIAT, @InterceptedSetFocus) then
    OriginalSetFocus := TSetFocus(Tmp);

  SetForegroundWindowIAT := FindIATEntry(@Windows.SetForegroundWindow);
  Tmp := SetForegroundWindowIAT^;
  if Assigned(SetForegroundWindowIAT) and ReplaceFunction(SetForegroundWindowIAT, @InterceptedSetForegroundWindow) then
    OriginalSetForegroundWindow := TSetForegroundWindow(Tmp);
end;

procedure ResetHooks;
begin
  if Assigned(SetWindowPosIAT) and Assigned(OriginalSetWindowPos) then
  begin
    ReplaceFunction(SetWindowPosIAT, @OriginalSetWindowPos);
    OriginalSetWindowPos := nil;
    SetWindowPosIAT := nil;
  end;

  if Assigned(SetFocusIAT) and Assigned(OriginalSetFocus) then
  begin
    ReplaceFunction(SetFocusIAT, @OriginalSetFocus);
    OriginalSetFocus := nil;
    SetFocusIAT := nil;
  end;

  if Assigned(SetForegroundWindowIAT) and Assigned(OriginalSetForegroundWindow) then
  begin
    ReplaceFunction(SetForegroundWindowIAT, @OriginalSetForegroundWindow);
    OriginalSetForegroundWindow := nil;
    SetForegroundWindowIAT := nil;
  end;
end;

{$ENDIF}

end.
