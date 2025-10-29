////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgTraceLog.pas
//  * Purpose   : Tracer log dialog
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

unit dlgTraceLog;

{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  LCLType, LCLIntf, Messages, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus,

  CpuView.TraceLog;

type

  { TfrmTraceLog }

  TfrmTraceLog = class(TForm)
    memLog: TMemo;
    miSelectAll: TMenuItem;
    miSave: TMenuItem;
    miClear: TMenuItem;
    mnuCopy: TMenuItem;
    pmTraceLog: TPopupMenu;
    SaveDialog: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miClearClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
  private
    procedure TraceLogChanged(Sender: TObject);
  public
    procedure UpdateTraceLog(Value: TTraceLog);
  end;

var
  frmTraceLog: TfrmTraceLog;

implementation

{$R *.lfm}

{ TfrmTraceLog }

procedure TfrmTraceLog.mnuCopyClick(Sender: TObject);
begin
  memLog.CopyToClipboard;
end;

procedure TfrmTraceLog.TraceLogChanged(Sender: TObject);
begin
  memLog.Lines.Assign(TraceLog.Data);
  SendMessage(frmTraceLog.memLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TfrmTraceLog.UpdateTraceLog(Value: TTraceLog);
begin
  TraceLog.OnChange := TraceLogChanged;
  TraceLogChanged(Value);
end;

procedure TfrmTraceLog.miClearClick(Sender: TObject);
begin
  TraceLog.Clear;
end;

procedure TfrmTraceLog.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caFree;
end;

procedure TfrmTraceLog.FormDestroy(Sender: TObject);
begin
  TraceLog.OnChange := nil;
  frmTraceLog := nil;
end;

procedure TfrmTraceLog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

procedure TfrmTraceLog.miSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    memLog.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmTraceLog.miSelectAllClick(Sender: TObject);
begin
  memLog.SelectAll;
end;

end.

