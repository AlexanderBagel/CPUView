////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : dlgCpuView.pas
//  * Purpose   : GUI debugger with implementation for ARM AArch64 processor.
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

unit dlgCpuViewImplementation;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$WARN 6060 off : Case statement does not handle all possible cases}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls,

  dlgCpuView,

  CpuView.CPUContext,
  CpuView.Context.Aarch64,
  CpuView.ScriptExecutor,
  CpuView.ScriptExecutor.Aarch64;

type

  { TfrmCpuViewImpl }

  TfrmCpuViewImpl = class(TfrmCpuView)
    acRegSingle: TAction;
    acRegDouble: TAction;
    acRegNeon: TAction;
    miRegAarchSingle: TMenuItem;
    miRegAarchNeon: TMenuItem;
    miRegAarchDouble: TMenuItem;
    miRegAarchFit: TMenuItem;
    miRegAarchCopy: TMenuItem;
    pmAarch64Reg: TPopupMenu;
    miRegAarchSep2: TMenuItem;
    miRegAarchSep3: TMenuItem;
    procedure acRegDoubleExecute(Sender: TObject);
    procedure acRegDoubleUpdate(Sender: TObject);
    procedure acRegNeonExecute(Sender: TObject);
    procedure acRegNeonUpdate(Sender: TObject);
    procedure acRegSingleExecute(Sender: TObject);
    procedure acRegSingleUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FContext: TAarch64CpuContext;
    FScript: TAarch64ScriptExecutor;
  protected
    function GetContext: TCommonCpuContext; override;
    function ScriptExecutor: TAbstractScriptExecutor; override;
  public

  end;

implementation

{$R *.lfm}

{ TfrmCpuViewImpl }

procedure TfrmCpuViewImpl.FormCreate(Sender: TObject);
begin
  FContext := TAarch64CpuContext.Create(Self);
  FScript := TAarch64ScriptExecutor.Create;
  inherited;
end;

procedure TfrmCpuViewImpl.acRegNeonUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowVectored;
end;

procedure TfrmCpuViewImpl.acRegSingleExecute(Sender: TObject);
begin
  FContext.ShowSingle := not FContext.ShowSingle;
end;

procedure TfrmCpuViewImpl.acRegSingleUpdate(Sender: TObject);
begin
    TAction(Sender).Checked := FContext.ShowSingle;
end;

procedure TfrmCpuViewImpl.acRegNeonExecute(Sender: TObject);
begin
  FContext.ShowVectored := not FContext.ShowVectored;
end;

procedure TfrmCpuViewImpl.acRegDoubleUpdate(Sender: TObject);
begin
  TAction(Sender).Checked := FContext.ShowDouble;
end;

procedure TfrmCpuViewImpl.acRegDoubleExecute(Sender: TObject);
begin
  FContext.ShowDouble := not FContext.ShowDouble;
end;

procedure TfrmCpuViewImpl.FormDestroy(Sender: TObject);
begin
  inherited;
  FContext.Free;
  FScript.Free;
end;

function TfrmCpuViewImpl.GetContext: TCommonCpuContext;
begin
  Result := FContext;
end;

function TfrmCpuViewImpl.ScriptExecutor: TAbstractScriptExecutor;
begin
  Result := FScript;
end;

end.

