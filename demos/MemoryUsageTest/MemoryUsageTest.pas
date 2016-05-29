unit MemoryUsageTest;

interface

uses
  Types, Windows, PSApi, SysUtils;
  
  
procedure RUN;  
  
implementation


function GetMemoryUsage: NativeUInt;
var
  MemCounters: TProcessMemoryCounters;
begin
  MemCounters.cb := SizeOf(MemCounters);
  if (not GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters))) then
    RaiseLastOSError;

  Result := MemCounters.WorkingSetSize
end;

procedure RUN;
const
  MB = 1024 * 1024;
var
  Name: string;
  Size, i: NativeUInt;
  MemoryUsage: NativeUInt;
begin
  Name := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Size := StrToInt(Copy(Name, LastDelimiter('_', Name) + 1, MaxInt));

  MemoryUsage := GetMemoryUsage;
  for i := 1 to (100 * MB) div Size do
  begin
    GetMemory(Size);
  end;
  MemoryUsage := GetMemoryUsage - MemoryUsage;

  Writeln(Format('%s: %0.2fMb', [Name, MemoryUsage / MB]));
  {$ifdef DEBUG}
    Readln;
  {$endif}
end;

 
end. 