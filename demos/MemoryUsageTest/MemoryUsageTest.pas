unit MemoryUsageTest;

interface

uses
  Types, Windows, SysUtils;
  
  
procedure RUN;  
  
implementation


procedure RUN;
begin
  Writeln(ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
end;

 
end. 