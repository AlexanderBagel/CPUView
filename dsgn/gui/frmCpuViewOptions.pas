unit frmCpuViewOptions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, ColorBox,
  IDEOptionsIntf, IDEOptEditorIntf,CpuView.Settings;

type

  { TCpuViewMainOptionsFrame }

  TCpuViewMainOptionsFrame = class(TAbstractIDEOptionsEditor)
    btnFontBrowse: TButton;
    btnReset: TButton;
    btnImport: TButton;
    btnExport: TButton;
    cbFont: TComboBox;
    cbDisplayFuncName: TCheckBox;
    cbShowOpcodes: TCheckBox;
    cbShowSourceLines: TCheckBox;
    cbSymbols: TCheckBox;
    cbForm: TCheckBox;
    cbViewers: TCheckBox;
    cbDbgLog: TCheckBox;
    cbDbgCrash: TCheckBox;
    cbColor: TColorBox;
    clbColors: TColorListBox;
    ColorDialog: TColorDialog;
    cbColorMode: TComboBox;
    FontDialog: TFontDialog;
    gbAsmView: TGroupBox;
    gbSessions: TGroupBox;
    gbColors: TGroupBox;
    lblColorMode: TLabel;
    lblFont: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure btnExportClick(Sender: TObject);
    procedure btnFontBrowseClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure cbColorChange(Sender: TObject);
    procedure clbColorsSelectionChange(Sender: TObject; User: boolean);
  private
    FLockColorChange: Boolean;
    FSettings: TCpuViewSettins;
    procedure UpdateCurrentFont(const AFontName: string);
    procedure UpdateFrameControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

uses
  CpuView.Design.Common,
  dlgCpuView,
  FWHexView;

{$R *.lfm}

procedure TCpuViewMainOptionsFrame.btnFontBrowseClick(Sender: TObject);
begin
  FontDialog.Font.Name := cbFont.Text;
  if FontDialog.Execute then
    UpdateCurrentFont(FontDialog.Font.Name);
end;

procedure TCpuViewMainOptionsFrame.btnExportClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FSettings.ColorsExport(SaveDialog.FileName);
end;

procedure TCpuViewMainOptionsFrame.btnImportClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FSettings.ColorsImport(OpenDialog.FileName);
    UpdateFrameControl;
  end;
end;

procedure TCpuViewMainOptionsFrame.btnResetClick(Sender: TObject);
begin
  FSettings.Reset;
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.cbColorChange(Sender: TObject);
var
  Index: Integer;
begin
  if FLockColorChange then Exit;
  Index := clbColors.ItemIndex;
  if Index < 0 then Exit;
  clbColors.Colors[Index] := cbColor.Selected;
  cbColorMode.ItemIndex := Integer(cmCustom);
end;

procedure TCpuViewMainOptionsFrame.clbColorsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if User then
  begin
    FLockColorChange := True;
    try
      cbColor.Selected := clbColors.Selected;
    finally
      FLockColorChange := False;
    end;
  end;
end;

procedure TCpuViewMainOptionsFrame.UpdateCurrentFont(const AFontName: string);
var
  Index: Integer;
begin
  Index := cbFont.Items.IndexOf(AFontName);
  if Index < 0 then
    Index := cbFont.Items.Add(AFontName);
  cbFont.ItemIndex := Index;
end;

procedure TCpuViewMainOptionsFrame.UpdateFrameControl;
var
  I: Integer;
begin
  UpdateCurrentFont(FSettings.FontName);
  cbDisplayFuncName.Checked := FSettings.DisplayFunc;
  cbShowOpcodes.Checked :=  FSettings.ShowOpcodes;
  cbShowSourceLines.Checked := FSettings.ShowSourceLines;
  cbSymbols.Checked := FSettings.UseDebugInfo;
  cbForm.Checked := FSettings.SaveFormSession;
  cbViewers.Checked := FSettings.SaveViewersSession;
  cbDbgLog.Checked := FSettings.UseDebugLog;
  cbDbgCrash.Checked := FSettings.UseCrashDump;
  cbColorMode.ItemIndex := Integer(FSettings.ColorMode);
  clbColors.Items.Clear;
  for I := 0 to FSettings.ColorsMap.Count - 1 do
  begin
    clbColors.Items.Add(FSettings.ColorsMap[I].Description);
    clbColors.Colors[I] := FSettings.Color[FSettings.ColorsMap[I].Id];
  end;
end;

constructor TCpuViewMainOptionsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FSettings := TCpuViewSettins.Create;
end;

destructor TCpuViewMainOptionsFrame.Destroy;
begin
  FSettings.Free;
  inherited Destroy;
end;

function TCpuViewMainOptionsFrame.GetTitle: string;
begin
  Result := 'CPU-View';
end;

procedure TCpuViewMainOptionsFrame.Setup({%H-}ADialog: TAbstractOptionsEditorDialog);
begin
  cbFont.Clear;
  {$IFDEF MSWINDOWS}
  cbFont.Items.Add('Consolas');
  {$ENDIF}
  {$IFDEF LINUX}
  cbFont.Items.Add('DejaVu Sans Mono');
  cbFont.Items.Add('Monospace');
  {$ENDIF}
  cbFont.ItemIndex := 0;
  if frmCpuView <> nil then
    frmCpuView.SaveSettings;
end;

procedure TCpuViewMainOptionsFrame.ReadSettings({%H-}AOptions: TAbstractIDEOptions);
begin
  FSettings.Load(ConfigPath);
  UpdateFrameControl;
end;

procedure TCpuViewMainOptionsFrame.WriteSettings({%H-}AOptions: TAbstractIDEOptions);
var
  I: Integer;
begin
  FSettings.FontName := cbFont.Text;
  FSettings.DisplayFunc := cbDisplayFuncName.Checked;
  FSettings.ShowOpcodes := cbShowOpcodes.Checked;
  FSettings.ShowSourceLines := cbShowSourceLines.Checked;
  FSettings.UseDebugInfo := cbSymbols.Checked;
  FSettings.SaveFormSession := cbForm.Checked;
  FSettings.SaveViewersSession := cbViewers.Checked;
  FSettings.UseDebugLog := cbDbgLog.Checked;
  FSettings.UseCrashDump := cbDbgCrash.Checked;
  FSettings.ColorMode := TColorMode(cbColorMode.ItemIndex);
  for I := 0 to FSettings.ColorsMap.Count - 1 do
    FSettings.Color[FSettings.ColorsMap[I].Id] := clbColors.Colors[I];
  FSettings.Save(ConfigPath);
  if frmCpuView <> nil then
    frmCpuView.LoadSettings;
end;

procedure TCpuViewMainOptionsFrame.RestoreSettings({%H-}AOptions: TAbstractIDEOptions);
begin
  // do nothing...
end;

class function TCpuViewMainOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

