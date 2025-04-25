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

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  LCLType, LCLIntf, Messages, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus;

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
    FTraceLog: TStringList;
  public
    procedure UpdateTraceLog(Value: TStringList);
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

procedure TfrmTraceLog.UpdateTraceLog(Value: TStringList);
begin
  FTraceLog := Value;
  memLog.Lines.Assign(Value);
  SendMessage(frmTraceLog.memLog.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TfrmTraceLog.miClearClick(Sender: TObject);
begin
  memLog.Clear;
  FTraceLog.Clear;
end;

procedure TfrmTraceLog.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction := caFree;
end;

procedure TfrmTraceLog.FormDestroy(Sender: TObject);
begin
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

