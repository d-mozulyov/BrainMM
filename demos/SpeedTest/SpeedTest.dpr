program SpeedTest;

{
   Copyright (c) Steve Maughan
   http://www.stevemaughan.com/delphi/delphi-parallel-programming-library-memory-managers/

   Modyfied by Dmitry Mozulyov
   https://github.com/d-mozulyov/BrainMM
}

{$ifdef FASTMM}
  {$ifdef FASTMM_NEVERSLEEP}
    {$ifdef CPUX64}
      {$EXTENSION 'FastMM-NSOT-64.exe'}
    {$else}
      {$EXTENSION 'FastMM-NSOT-32.exe'}
    {$endif}
  {$else}
    {$ifdef CPUX64}
      {$EXTENSION 'FastMM-64.exe'}
    {$else}
      {$EXTENSION 'FastMM-32.exe'}
    {$endif}
  {$endif}
{$endif}

{$ifdef SCALEMM}
  {$ifdef CPUX64}
    {$EXTENSION 'ScaleMM-64.exe'}
  {$else}
    {$EXTENSION 'ScaleMM-32.exe'}
  {$endif}
{$endif}

{$ifdef SAPMM}
  {$ifdef CPUX64}
    {$MESSAGE ERROR 'THERE IS NO 64bit SAPMM'}
  {$else}
    {$EXTENSION 'SAPMM-32.exe'}
  {$endif}
{$endif}

{$ifdef NEXUSDB}
  {$ifdef CPUX64}
    {$EXTENSION 'NexusDB-64.exe'}
  {$else}
    {$EXTENSION 'NexusDB-32.exe'}
  {$endif}
{$endif}

{$ifdef BRAINMM}
  {$ifdef CPUX64}
    {$EXTENSION 'BrainMM-64.exe'}
  {$else}
    {$EXTENSION 'BrainMM-32.exe'}
  {$endif}
{$endif}


uses
  {$ifdef FASTMM}
    FastMM4,
  {$endif}
  {$ifdef SCALEMM}
    ScaleMM2,
  {$endif}
  {$ifdef SAPMM}
    SAPInstall,
  {$endif}
  {$ifdef NEXUSDB}
    nxReplacementMemoryManager,
    // http://www.nexusdb.com/support/index.php?q=node/522
  {$endif}
  {$ifdef BRAINMM}
    BrainMM,
  {$endif}
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
