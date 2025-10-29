////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : CPU-View
//  * Unit Name : CpuView.DBase.pas
//  * Purpose   : Work with external information databases for CPU-View.
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

unit CpuView.DBase;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  {$IFDEF FPC}
  LazIDEIntf, Laz2_XMLRead, Laz2_DOM,
  {$ENDIF}
  Generics.Collections;

  {$I CpuViewCfg.inc}

type

  { TCpuViewDBase }

  TCpuViewDBase = class
  private
    FLastError: TDictionary<Integer, string>;
    FLastStatus: TDictionary<Cardinal, string>;
    FRegHint: TDictionary<Integer, string>;
    {$IFDEF FPC}
    procedure LazarusInit;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function GetLastErrorStr(AErrorCode: Integer): string;
    function GetLastStatusStr(AStatusCode: Cardinal): string;
    function GetRegHintStr(ARegID: Integer): string;
    procedure LoadFromFolder(const AFolderPath: string);
  end;

implementation

{ TCpuViewDBase }

{$IFDEF FPC}
procedure TCpuViewDBase.LazarusInit;
var
  Path, Value: string;
  Doc: TXMLDocument;
  Links, Node, NameNode, FileNameNode, Attribute: TDOMNode;
begin
  Path := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + 'packagefiles.xml';
  if not FileExists(Path) then Exit;
  ReadXMLFile(Doc, Path);
  try
    Links := Doc.DocumentElement.FindNode('UserPkgLinks');
    if Links = nil then Exit;
    Node := Links.FirstChild;
    while Assigned(Node) do
    begin
      NameNode := Node.FindNode('Name');
      if Assigned(NameNode) then
      begin
        Attribute := NameNode.Attributes.GetNamedItem('Value');
        if Assigned(Attribute) then
        begin
          Value := Attribute.NodeValue;
          if Value.StartsWith('CPUView_') then
          begin
            FileNameNode := Node.FindNode('Filename');
            if Assigned(FileNameNode) then
            begin
              Attribute := FileNameNode.Attributes.GetNamedItem('Value');
              if Assigned(Attribute) then
              begin
                Path := ExtractFilePath(Attribute.NodeValue) + 'db' + PathDelim;
                LoadFromFolder(Path);
              end;
            end;
            Break;
          end;
        end;
      end;
      Node := Node.NextSibling;
    end;
  finally
    Doc.Free;
  end;
end;
{$ENDIF}

procedure TCpuViewDBase.LoadFromFolder(const AFolderPath: string);
const
  {$IFDEF MSWINDOWS}
  LastError = 'WinLastError.db';
  LastStatus = 'WinLastStatus.db';
  {$ENDIF}
  {$IFDEF LINUX}
  LastError = 'LinLastError.db';
  LastStatus = 'LinLastStatus.db';
  {$ENDIF}
  {$IFDEF USE_INTEL_CTX}
  RegHint = 'IntelRegHint.db';
  {$ENDIF}
  {$IFDEF CPUAARCH64}
  RegHint = 'ArmRegHint.db';
  {$ENDIF}
var
  APath, ALine: string;
  AError, Idx: Integer;
  AStatus: Cardinal;
  S: TStringList;
begin
  FLastError.Clear;
  FLastStatus.Clear;
  FRegHint.Clear;
  if not DirectoryExists(AFolderPath) then Exit;
  APath := IncludeTrailingPathDelimiter(AFolderPath) + LastError;
  if FileExists(APath) then
  begin
    S := TStringList.Create;
    try
      S.LoadFromFile(APath);
      for ALine in S do
      begin
        Idx := Pos(' ', ALine);
        if TryStrToInt(Copy(ALine, 1, Idx - 1), AError) then
          FLastError.TryAdd(AError, Copy(ALine, Idx + 1, Length(ALine)));
      end;
    finally
      S.Free;
    end;
  end;
  APath := IncludeTrailingPathDelimiter(AFolderPath) + LastStatus;
  if FileExists(APath) then
  begin
    S := TStringList.Create;
    try
      S.LoadFromFile(APath);
      for ALine in S do
      begin
        Idx := Pos(' ', ALine);
        if TryStrToUInt('$' + Copy(ALine, 1, Idx - 1), AStatus) then
          FLastStatus.TryAdd(AStatus, Copy(ALine, Idx + 1, Length(ALine)));
      end;
    finally
      S.Free;
    end;
  end;
  APath := IncludeTrailingPathDelimiter(AFolderPath) + RegHint;
  if FileExists(APath) then
  begin
    S := TStringList.Create;
    try
      S.LoadFromFile(APath);
      for ALine in S do
      begin
        Idx := Pos(' ', ALine);
        if TryStrToInt(Copy(ALine, 1, Idx - 1), AError) then
          FRegHint.TryAdd(AError, Copy(ALine, Idx + 1, Length(ALine)));
      end;
    finally
      S.Free;
    end;
  end;
end;

constructor TCpuViewDBase.Create;
begin
  FLastError := TDictionary<Integer, string>.Create;
  FLastStatus := TDictionary<Cardinal, string>.Create;
  FRegHint := TDictionary<Integer, string>.Create;
  {$IFDEF FPC}
  LazarusInit;
  {$ENDIF}
end;

destructor TCpuViewDBase.Destroy;
begin
  FLastError.Free;
  FLastStatus.Free;
  FRegHint.Free;
  inherited Destroy;
end;

function TCpuViewDBase.GetLastErrorStr(AErrorCode: Integer): string;
begin
  Result := '';
  FLastError.TryGetValue(AErrorCode, Result);
end;

function TCpuViewDBase.GetLastStatusStr(AStatusCode: Cardinal): string;
begin
  Result := '';
  FLastStatus.TryGetValue(AStatusCode, Result);
end;

function TCpuViewDBase.GetRegHintStr(ARegID: Integer): string;
begin
  Result := '';
  FRegHint.TryGetValue(ARegID, Result);
end;

end.
