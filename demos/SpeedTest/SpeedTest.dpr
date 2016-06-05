program SpeedTest;

{
   Copyright (c) Steve Maughan:
   http://www.stevemaughan.com/delphi/delphi-parallel-programming-library-memory-managers/

   Modyfied by Dmitry Mozulyov: https://github.com/d-mozulyov/BrainMM
   Binaries: http://dmozulyov.ucoz.net/BrainMM/Demo.rar
}

{$ifdef FASTMM}
  {$ifNdef FASTMM_NEVERSLEEP}
    {$ifdef CPUX64}
      {$EXTENSION 'FastMM-64.exe'}
    {$else}
      {$EXTENSION 'FastMM-32.exe'}
    {$endif}
  {$else}
    {$ifdef CPUX64}
      {$EXTENSION 'FastMM-NSOT-64.exe'}
    {$else}
      {$EXTENSION 'FastMM-NSOT-32.exe'}
    {$endif}
  {$endif}
{$endif}

{$ifdef SCALEMM2}
  {$ifdef CPUX64}
    {$EXTENSION 'ScaleMM2-64.exe'}
  {$else}
    {$EXTENSION 'ScaleMM2-32.exe'}
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

{$ifdef INTELTBB}
  {$ifdef CPUX64}
    {$EXTENSION 'IntelTBB-64.exe'}
  {$else}
    {$EXTENSION 'IntelTBB-32.exe'}
  {$endif}
{$endif}

{$ifdef TCMALLOC}
  {$ifdef CPUX64}
    {$EXTENSION 'TCMalloc-64.exe'}
  {$else}
    {$EXTENSION 'TCMalloc-32.exe'}
  {$endif}
{$endif}

{$ifdef MSHEAP}
  {$ifdef CPUX64}
    {$EXTENSION 'MSHeap-64.exe'}
  {$else}
    {$EXTENSION 'MSHeap-32.exe'}
  {$endif}
{$endif}


uses
  {$ifdef DEBUG}
    BrainMM,
  {$endif}

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
  {$ifdef INTELTBB}
    IntelTBB,
  {$endif}
  {$ifdef TCMALLOC}
    TCMalloc,
  {$endif}
  {$ifdef MSHEAP}
    MSHeap,
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
