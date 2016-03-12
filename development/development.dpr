program development;

uses
  {$ifdef BRAINMM_UNITTEST}UTBrainMM in 'UTBrainMM.pas',{$endif}
  BrainMM in '..\sources\BrainMM.pas',
  TestUnit in 'TestUnit.pas';

begin
  RUN;
end.
