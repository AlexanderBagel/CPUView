////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewShortCuts.pas
//  * Purpose   : ShordCut settings for CPU-View dialog and all viewers.
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
//  * Version   : 1.0
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/CPUView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/CPUView
//  ****************************************************************************
//

unit frmCpuViewShortCuts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  IDEOptEditorIntf, frmCpuViewBaseOptions;

type

  { TCpuViewShortCutsFrame }

  TCpuViewShortCutsFrame = class(TCpuViewBaseOptionsFrame)
  private

  protected
    procedure DoReadSettings; override;
    procedure DoResetSettings; override;
    procedure DoWriteSettings; override;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
  end;

implementation

{$R *.lfm}

{ TCpuViewShortCutsFrame }

procedure TCpuViewShortCutsFrame.DoReadSettings;
begin

end;

procedure TCpuViewShortCutsFrame.DoResetSettings;
begin

end;

procedure TCpuViewShortCutsFrame.DoWriteSettings;
begin

end;

function TCpuViewShortCutsFrame.GetTitle: string;
begin
  Result := 'ShortCuts';
end;

procedure TCpuViewShortCutsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

end.

