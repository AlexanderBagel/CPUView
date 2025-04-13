////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : frmCpuViewBaseOptions.pas
//  * Purpose   : A common class for all customization frames.
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

unit frmCpuViewBaseOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Generics.Collections,
  IDEOptionsIntf, IDEOptEditorIntf,

  CpuView.Settings,
  CpuView.Design.Common,

  dlgCpuView;

type

  { TCpuViewBaseOptionsFrame }

  TCpuViewBaseOptionsFrame = class(TAbstractIDEOptionsEditor)
  private class var
    FSettings: TCpuViewSettins;
    FOptionFrames: specialize TList<TCpuViewBaseOptionsFrame>;
  protected
    procedure DoReadSettings; virtual; abstract;
    procedure DoWriteSettings; virtual; abstract;
    function IsMainFrame: Boolean; virtual;
    class constructor CreateCpuViewOptions;
    class destructor DestroyCpuViewOptions;
    class property Settings: TCpuViewSettins read FSettings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCpuViewBaseOptionsFrame }

function TCpuViewBaseOptionsFrame.IsMainFrame: Boolean;
begin
  Result := False;
end;

class constructor TCpuViewBaseOptionsFrame.CreateCpuViewOptions;
begin
  FSettings := TCpuViewSettins.Create;
  FOptionFrames := specialize TList<TCpuViewBaseOptionsFrame>.Create;
end;

class destructor TCpuViewBaseOptionsFrame.DestroyCpuViewOptions;
begin
  FreeAndNil(FOptionFrames);
  FreeAndNil(FSettings);
end;

constructor TCpuViewBaseOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptionFrames.Add(Self);
end;

destructor TCpuViewBaseOptionsFrame.Destroy;
var
  Index: Integer;
begin
  Index := FOptionFrames.IndexOf(Self);
  if Index >= 0 then
    FOptionFrames.Delete(Index);
  inherited Destroy;
end;

procedure TCpuViewBaseOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  I: Integer;
begin
  if not IsMainFrame then Exit;
  if frmCpuView <> nil then
    frmCpuView.SaveSettings;
  FSettings.Load(ConfigPath);
  for I := 0 to FOptionFrames.Count - 1 do
    FOptionFrames[I].DoReadSettings;
end;

procedure TCpuViewBaseOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // do nothing...
end;

procedure TCpuViewBaseOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  I: Integer;
begin
  if not IsMainFrame then Exit;
  for I := 0 to FOptionFrames.Count - 1 do
    FOptionFrames[I].DoWriteSettings;
  FSettings.Save(ConfigPath);
  if frmCpuView <> nil then
    frmCpuView.LoadSettings;
end;

class function TCpuViewBaseOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

