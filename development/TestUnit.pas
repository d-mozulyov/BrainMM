unit TestUnit;

{$i compiler_directives.inc}

interface
  uses {$ifdef UNITSCOPENAMES}System.SysUtils{$else}SysUtils{$endif},
       {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif},{$endif}
       BrainMM;



procedure RUN;
procedure ShowMessage(const S: string); overload;
procedure ShowMessage(const StrFmt: string; const Args: array of const); overload;

implementation


procedure RUN;
begin

  ShowMessage('Test');
end;

procedure ShowMessage(const S: string);
var
  BreakPoint: string;
begin
  BreakPoint := S;

  {$ifdef MSWINDOWS}
    {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.MessageBox(0, PChar(BreakPoint), 'Message:', 0);
  {$endif}

  Halt;
end;

procedure ShowMessage(const StrFmt: string; const Args: array of const);
begin
  ShowMessage(Format(StrFmt, Args));
end;

initialization


end.
