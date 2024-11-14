////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewColors.pas
//  * Purpose   : Color settings for all viewers.
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

unit frmCpuViewColors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  IDEOptEditorIntf, frmCpuViewBaseOptions;

type

  { TCpuViewColorsFrame }

  TCpuViewColorsFrame = class(TCpuViewBaseOptionsFrame)
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

{ TCpuViewColorsFrame }

procedure TCpuViewColorsFrame.DoReadSettings;
begin

end;

procedure TCpuViewColorsFrame.DoResetSettings;
begin

end;

procedure TCpuViewColorsFrame.DoWriteSettings;
begin

end;

function TCpuViewColorsFrame.GetTitle: string;
begin
  Result := 'Colors';
end;

procedure TCpuViewColorsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

end.

