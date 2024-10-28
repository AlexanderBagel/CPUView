unit CpuView.Design.Common;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  LazIDEIntf;

  function ConfigPath: string;

implementation

function ConfigPath: string;
begin
  Result := IncludeTrailingPathDelimiter(LazarusIDE.GetPrimaryConfigPath) + 'cpuview.xml';
end;

end.
