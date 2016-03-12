unit UTBrainMM;

{$i compiler_directives.inc}

  (*
     BrainMM library's unit test
  *)

interface
  uses BrainMM,
       {$ifdef UNITSCOPENAMES}System.Types{$else}Types{$endif},
       // {$ifdef UNITSCOPENAMES}System.SysUtils{$else}SysUtils{$endif},
       {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif}{$endif};

implementation

end.
