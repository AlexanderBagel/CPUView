unit CpuView.Design.DbgLog;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, LazLoggerBase;

type

  { TCpuViewDebugLog }

  TCpuViewDebugLog = class
  public
    class procedure Log(const AMessage: string);
  end;

implementation

{ TCpuViewDebugLog }

class procedure TCpuViewDebugLog.Log(const AMessage: string);
begin
  DebugLn(AMessage);
end;

end.
