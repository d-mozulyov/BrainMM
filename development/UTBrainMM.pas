unit UTBrainMM;

{$i compiler_directives.inc}

  (*
     BrainMM library's unit test
  *)

interface
  uses BrainMM,
       {$ifdef UNITSCOPENAMES}System.Types{$else}Types{$endif}
       {$ifdef MSWINDOWS},{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif}{$endif};

const
  BREAKPOINT = 0;

var
  Done: Boolean = True;
  TEST_NUMBER: Integer = 1;

type
  TCountSize = object
    Count: Integer;
    Size: Integer;

    procedure Clear;
    procedure Add(const ASize: Integer); overload;
    procedure Add(const ACountSize: TCountSize); overload;
  end;
  PCountSize = ^TCountSize;

  TBitSetInfo = object
    Value: TBitSet8;

    Empty, Full: Boolean;
    LowBit, HighBit: Integer;
    Bits: array[0..63] of Boolean;
    Count: Integer;

    procedure Init(const V: TBitSet8);
  end;
  PBitSetInfo = ^TBitSetInfo;

  TK1LineCompactInfo = object
    Value: PK1LineSmall;
    ValueIndex: Integer;
    ValuePool: PK64PoolSmall;

    Size, Index: Integer;
    InFullQueue: Boolean;
    Items: TBitSetInfo;
    Empties: TCountSize;
    Allocated: TCountSize;

    procedure Init(const V: PK1LineSmall);
  end;
  PK1LineCompactInfo = ^TK1LineCompactInfo;

  TK1LineInfo = object(TK1LineCompactInfo)
    Prev, Next: PK1LineCompactInfo;
    _: array[0..2 * SizeOf(TK1LineCompactInfo) - 1] of Byte;

    procedure Init(const V: PK1LineSmall);
  end;
  PK1LineInfo = ^TK1LineInfo;

  TPoolSmallCompactInfo = object
    Value: PK64PoolSmall;
    ValueThreadHeap: PThreadHeap;

    InFullQueue: Boolean;
    Lines: TBitSetInfo;
    Empties: TCountSize;
    Allocated: TCountSize;

    procedure Init(const V: PK64PoolSmall);
  end;
  PPoolSmallCompactInfo = ^TPoolSmallCompactInfo;

  TPoolSmallInfo = object(TPoolSmallCompactInfo)
    Prev, Next: PPoolSmallCompactInfo;
    _: array[0..2 * SizeOf(TPoolSmallCompactInfo) - 1] of Byte;

    procedure Init(const V: PK64PoolSmall);
  end;
  PPoolSmallInfo = ^TPoolSmallInfo;

  TPoolMediumCompactInfo = object
    Value: PK64PoolMedium;
    ValueThreadHeap: PThreadHeap;

    Allocated: TCountSize;
    Empties: array[0..31] of TCountSize;
    EmptiesTotal: TCountSize;

    procedure Init(const V: PK64PoolMedium);
    procedure CheckEmpty(const VEmpty: PHeaderMediumEmpty; const AIndex: Integer);
  end;
  PPoolMediumCompactInfo = ^TPoolMediumCompactInfo;

  TPoolMediumInfo = object(TPoolMediumCompactInfo)
    Prev, Next: PPoolMediumCompactInfo;
    _: array[0..2 * SizeOf(TPoolMediumCompactInfo) - 1] of Byte;

    procedure Init(const V: PK64PoolMedium);
  end;
  PPoolMediumInfo = ^TPoolMediumInfo;

  TK1LineSmallCounts = record
    Count: Integer;
    AvailableNonFull: Integer;
    AvailableFull: Integer;
    FullQueue: Integer;
  end;
  PK1LineSmallCounts = ^TK1LineSmallCounts;

  TThreadHeapInfo = object
    Value: PThreadHeap;

    K1LineSmalls: array[1..8] of TK1LineSmallCounts;
    PoolSmalls: record
      Count: Integer;
      AvailableNonFull: Integer;
      AvailableFull: Integer;
      FullQueue: Integer;
      Empties: TCountSize;
      Allocated: TCountSize;
    end;
    PoolMediums: record
      Count: Integer;
      CountNonFull: Integer;
      CountFull: Integer;
      Allocated: TCountSize;
      Empties: array[0..31] of TCountSize;
      EmptiesTotal: TCountSize;
    end;

    procedure Init(const V: PThreadHeap);
    procedure InitMain;
  end;
  PThreadHeapInfo = ^TThreadHeapInfo;

  TPointerSmallInfo = object
    Pool: TPoolSmallInfo;

    Line: TK1LineInfo;
    Index: Integer;

    procedure Init(const V: Pointer);
  end;
  PPointerSmallInfo = ^TPointerSmallInfo;

  THeaderMediumInfo = object
    Value: PHeaderMedium;

    Size: Cardinal;
    Align: TMemoryAlign;
    Allocated: Boolean;
    ValueEmpty: PHeaderMediumEmpty;

    procedure Init(const V: PHeaderMedium);
  end;
  PHeaderMediumInfo = ^THeaderMediumInfo;

  TPointerMediumInfo = object(THeaderMediumInfo)
    Pool: TPoolMediumInfo;
    Left, Right: THeaderMediumInfo;

    procedure Init(const V: Pointer);
  end;
  PPointerMediumInfo = ^TPointerMediumInfo;

  TPointerBigInfo = object
    ToDo: Integer;

    procedure Init(const V: Pointer);
  end;
  PPointerBigInfo = ^TPointerBigInfo;

  TPointerLargeInfo = object
    ToDo: Integer;

    procedure Init(const V: Pointer);
  end;
  PPointerLargeInfo = ^TPointerLargeInfo;

  _PointersInfo_ = packed record
  case Integer of
    0: (Small: TPointerSmallInfo);
    1: (Medium: TPointerMediumInfo);
    2: (Big: TPointerBigInfo);
    3: (Large: TPointerLargeInfo);
  end;

  TPointerAlign = (align16B, align32B, align64B, align128B, align256B,
    align512B, align1K, align2K, align4K, align16K, align64K, align256K,
    align1M, align4M, align16M, align64M, align256M);
  PPointerAlign = ^TPointerAlign;

  TPointerInfo = object
    Value: Pointer;
    Align: TPointerAlign;
    Size: Integer;
    AsSmall: PPointerSmallInfo;
    AsMedium: PPointerMediumInfo;
    AsBig: PPointerBigInfo;
    AsLarge: PPointerLargeInfo;
    _: array[0..SizeOf(_PointersInfo_) - 1] of Byte;

    procedure Init(const V: Pointer);
  end;
  PPointerInfo = ^TPointerInfo;

  TJitHeapInfo = object
    Value: TJitHeap;
    ToDo: Integer;

    procedure Init(const V: TJitHeap);
  end;
  PJitHeapInfo = ^TJitHeapInfo;


type
  PPointerList = ^TPointerList;
  TPointerList = array[0..(Maxint div 16) - 1] of Pointer;

  TSmallBytes = array[0..MAX_SMALL_SIZE - 1 + 1] of Byte;
  PSmallBytes = ^TSmallBytes;

  TMediumBytes = array[0..MAX_MEDIUM_SIZE - 1] of Byte;
  PMediumBytes = ^TMediumBytes;


var
  HeapInfo: TThreadHeapInfo;
  Info: TPointerInfo;

implementation



procedure INC_TEST;
begin
  Inc(TEST_NUMBER);

  // breakpoint
  if (TEST_NUMBER = BREAKPOINT) then
    TEST_NUMBER := TEST_NUMBER;
end;

procedure GetMem(var P: Pointer; Size: NativeInt);
begin
  INC_TEST;
  System.GetMem(P, Size);
end;

procedure GetMemAligned(var P: Pointer; Align: TMemoryAlign; Size: NativeInt);
begin
  INC_TEST;
  BrainMM.GetMemAligned(P, Align, Size);
end;

function AllocMem(Size: NativeInt): Pointer;
begin
  INC_TEST;
  Result := System.AllocMem(Size);
end;

procedure FreeMem(var P: Pointer);
begin
  INC_TEST;
  System.FreeMem(P);
end;

procedure FreeMemNil(var P: Pointer);
begin
  FreeMem(P);
  P := nil;
end;

procedure ReallocMem(var P: Pointer; NewSize: NativeInt);
begin
  INC_TEST;
  System.ReallocMem(P, NewSize);
end;

procedure RegetMem(var P: Pointer; NewSize: NativeInt);
begin
  INC_TEST;
  BrainMM.RegetMem(P, NewSize);
end;


const
  PTR_FAILURE = Pointer(High(NativeInt));

type
  TFormatBuffer = array[0..1024] of Char;

{$ifdef MSWINDOWS}
function vsprintf(S: PChar; const Format: PChar; va_list: Pointer): Integer; cdecl;
  external 'msvcrt.dll' name {$ifdef UNICODE}'vswprintf'{$else}'vsprintf'{$endif};

function FormatBuffer(const FmtStr: PChar; const Args: array of const): TFormatBuffer;
var
  i: Integer;
  va_list: array[0..63] of Integer;
  Dest: PInteger;
begin
  Dest := @va_list[0];

  for i := Low(Args) to High(Args) do
  with TVarRec(Args[i]) do
  begin
    case VType of
      vtInteger:
      begin
        Dest^ := VInteger;
        Inc(NativeUInt(Dest), SizeOf(NativeInt));
      end;
      vtInt64:
      begin
        Dest^ := VInt64^;
        Inc(NativeUInt(Dest), SizeOf(NativeInt));
      end;
      vtExtended:
      begin
        PDouble(Dest)^ := VExtended^;
        Inc(NativeUInt(Dest), SizeOf(Double));
      end
    else
      PPointer(Dest)^ := VPointer;
      Inc(NativeUInt(Dest), SizeOf(Pointer));
    end;
  end;

  Result[vsprintf(Result, FmtStr, @va_list)] := #0;
end;
{$endif}

// make console?
function Log(const Text: PChar; ModeException: Boolean = False;
  CancelAvailable: Boolean = False): Boolean;
{$ifdef MSWINDOWS}
const
  DLG_MODES: array[Boolean] of Integer = (MB_ICONINFORMATION, MB_ICONERROR);
  DLG_CAPTIONS: array[Boolean] of PChar = ('Information:', 'Exception:');
  DLG_BUTTONS: array[Boolean] of Integer = (MB_OK, MB_OKCANCEL);
begin
  Result := (ID_OK = MessageBox(GetForegroundWindow, Text,
    DLG_CAPTIONS[ModeException], DLG_MODES[ModeException] or DLG_BUTTONS[CancelAvailable]));
end;
{$else .POSIX}
begin
  Result := False;
end;
{$endif}

procedure ErrorHandler(ErrorCode: TRuntimeError; ErrorAddr: Pointer);
var
  Error, S: PChar;
  TextBuffer: TFormatBuffer;
begin
  // error code to string
  case (ErrorCode) of
    reOutOfMemory: Error := 'reOutOfMemory';
     reInvalidPtr: Error := 'reInvalidPtr';
      reDivByZero: Error := 'reDivByZero';
     reRangeError: Error := 'reRangeError';
    reIntOverflow: Error := 'reIntOverflow';
      reInvalidOp: Error := 'reInvalidOp';
     reZeroDivide: Error := 'reZeroDivide';
       reOverflow: Error := 'reOverflow';
    reInvalidCast: Error := 'reInvalidCast';
reAccessViolation: Error := 'reAccessViolation';
rePrivInstruction: Error := 'rePrivInstruction';
  reStackOverflow: Error := 'reStackOverflow';
  else
    Error := @TextBuffer[High(TextBuffer) - 4 - 5];
    Error[0] := 'c';
    Error[1] := 'o';
    Error[2] := 'd';
    Error[3] := 'e';
    Error[4] := ' ';
    S := Pointer(@Error[5]);

    if (Byte(ErrorCode) > 99) then
    begin
      S^ := Char(Ord('0') + Byte(ErrorCode) div 100);
      Inc(S);
    end;
    if (Byte(ErrorCode) > 9) then
    begin
      S^ := Char(Ord('0') + (Byte(ErrorCode) div 10) mod 10);
      Inc(S);
    end;

    S^ := Char(Ord('0') + Byte(ErrorCode) mod 10);
    Inc(S);
    S^ := #0;
  end;

  {$ifdef MSWINDOWS}
    TextBuffer := FormatBuffer('TEST_NUMBER: %d'#13'Exception %s at %p',
      [TEST_NUMBER, Error, ErrorAddr]);

    // show exception message
    Log(TextBuffer, True);
  {$else .POSIX}
    Log(Error, True);
  {$endif}
  Halt;
end;

procedure ExceptionHandler(P: PExceptionRecord);
const
  STATUS_ACCESS_VIOLATION         = $C0000005;
  STATUS_ARRAY_BOUNDS_EXCEEDED    = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND   = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO     = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT     = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION  = $C0000090;
  STATUS_FLOAT_OVERFLOW           = $C0000091;
  STATUS_FLOAT_STACK_CHECK        = $C0000092;
  STATUS_FLOAT_UNDERFLOW          = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO   = $C0000094;
  STATUS_INTEGER_OVERFLOW         = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION   = $C0000096;
  STATUS_STACK_OVERFLOW           = $C00000FD;
  STATUS_CONTROL_C_EXIT           = $C000013A;
var
  ErrorCode: TRuntimeError;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:  ErrorCode := reDivByZero;
    STATUS_ARRAY_BOUNDS_EXCEEDED:   ErrorCode := reRangeError;
    STATUS_FLOAT_OVERFLOW:          ErrorCode := reOverflow;
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:       ErrorCode := reInvalidOp;
    STATUS_FLOAT_DIVIDE_BY_ZERO:    ErrorCode := reZeroDivide;
    STATUS_INTEGER_OVERFLOW:        ErrorCode := reIntOverflow;
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:  ErrorCode := reUnderflow;
    STATUS_ACCESS_VIOLATION:        ErrorCode := reAccessViolation;
    STATUS_PRIVILEGED_INSTRUCTION:  ErrorCode := rePrivInstruction;
    STATUS_CONTROL_C_EXIT:          ErrorCode := reControlBreak;
    STATUS_STACK_OVERFLOW:          ErrorCode := reStackOverflow;
  else
    ErrorCode := TRuntimeError(255);
  end;

  ErrorHandler(ErrorCode, P.ExceptionAddress);
end;

procedure SystemError; {$ifNdef CPUINTEL} inline;
begin
  System.Error(reInvalidCast);
end;
{$else .CPUINTEL}
asm
  {$ifdef CPUX86}
    mov al, reInvalidCast
  {$else .CPUX64}
    mov cl, reInvalidCast
  {$endif}
  jmp System.Error
end;
{$endif}

const
  MEDIUM_TEST_SIZE = 256 - 16;

type
  TResizeMem = procedure(var P: Pointer; NewSize: NativeInt);
  PResizeMem = ^TResizeMem;

var
  RESIZEMEM_PROCS: array[Boolean{Realloc}] of TResizeMem = (
    RegetMem,
    ReallocMem
  );

procedure Mix2kPtrs(const List: PPointerList; const Count: Integer);
type
  TPointerIndex = record
    Index: Integer;
    Value: Pointer;
  end;
var
  Buffer: array[0..2000 - 1] of TPointerIndex;
  Temp: TPointerIndex;
  i, j: Integer;
begin
  for i := 0 to Count - 1 do
  with Buffer[i] do
  begin
    Index := Random(High(Integer));
    Value := List[i];
  end;

  for i := 0 to Count - 2 do
  for j := i + 1 to Count - 1 do
  if (Buffer[i].Index > Buffer[j].Index) then
  begin
    Temp := Buffer[i];
    Buffer[i] := Buffer[j];
    Buffer[j] := Temp;
  end;

  for i := 0 to Count - 1 do
    List[i] := Buffer[i].Value;
end;

procedure MixPointers(var Pointers: array of Pointer);
var
  Current: PPointer;
  Count: Integer;
begin
  Current := @Pointers[Low(Pointers)];
  Count := Length(Pointers);

  while (Count > 2000) do
  begin
    Mix2kPtrs(PPointerList(Current), 2000);
    Inc(Current, 2000);
    Dec(Count, 2000);
  end;

  Mix2kPtrs(PPointerList(Current), Count);
end;

procedure Check(const CorrectValue: Pointer; var Value: Pointer); overload;
begin
  if (CorrectValue <> Value) then
    SystemError;
end;

procedure Check(const CorrectValue: Integer; var Value: Integer); overload;
begin
  if (CorrectValue <> Value) then
    SystemError;
end;

procedure Check(const CorrectValue: Boolean; var Value: Boolean); overload;
begin
  if (CorrectValue <> Value) then
    SystemError;
end;

procedure Check(const CorrectValue: TMemoryAlign; var Value: TMemoryAlign); overload;
begin
  if (CorrectValue <> Value) then
    SystemError;
end;

procedure CheckEmptyHeap;
var
  ThreadHeap: PThreadHeap;
  HeapInfo: TThreadHeapInfo;
begin
  ThreadHeap := ThreadHeapList;
  while (ThreadHeap <> nil) do
  begin
    HeapInfo.Init(ThreadHeap);

    Check(0, HeapInfo.PoolSmalls.Count);
    Check(0, HeapInfo.PoolMediums.Count);

    if (ThreadHeap.Deferreds.Assigned)
      then SystemError;

    ThreadHeap := ThreadHeap.FNextHeap;
  end;

  // todo
end;

procedure InitPBytes(P: Pointer; Size: Integer; XorByte: Byte);
var
  j: Integer;
begin
  for j := 0 to Size - 1 do
   PMediumBytes(P)[j] := Byte(j) xor XorByte;
end;

procedure CheckPBytes(P: Pointer; Size: Integer; XorByte: Byte);
var
  j: Integer;
begin
  for j := 0 to Size - 1 do
  if (PMediumBytes(P)[j] <> Byte(j) xor XorByte) then SystemError;
end;

procedure CheckPBytesAlign(P: Pointer; Size: Integer; XorByte: Byte; Align: TMemoryAlign);
begin
  Info.Init(P);
  if (Info.Size < Size) then SystemError;
  if (Info.AsMedium.Align <> Align) then SystemError;
  CheckPBytes(P, Size, XorByte);
end;

procedure GetThreeMediumP(var P1, P2, P3: Pointer; const Size: Integer; const InitHeapInfo: Boolean);
begin
  GetMemAligned(P1, ma32Bytes, Size);
  GetMemAligned(P2, ma32Bytes, Size);
  GetMemAligned(P3, ma32Bytes, Size);

  InitPBytes(P1, Size, 1);
  InitPBytes(P2, Size, 2);
  InitPBytes(P3, Size, 3);

  if (InitHeapInfo) then
    HeapInfo.InitMain;
end;

procedure FreeThreeP(var P1, P2, P3: Pointer; const CheckEmpty: Boolean);
begin
  FreeMemNil(P1);
  FreeMemNil(P2);
  FreeMemNil(P3);

  if (CheckEmpty) then
    CheckEmptyHeap;
end;



procedure TestSizes;

  procedure CheckSize(const Size, Value: Integer);
  begin
    if (Size <> Value) then SystemError;
  end;
begin
  CheckSize(SizeOf(TBitSet8), 8);
  CheckSize(SizeOf(B16), 16);
  CheckSize(SizeOf(TQueuePrevNext), 16);
  CheckSize(SizeOf(TK1LineSmallHeader), 32);
  CheckSize(SizeOf(TK1LineSmall), 1024);
  CheckSize(SizeOf(TK64PoolSmall), 64 * 1024);
  CheckSize(SizeOf(THeaderMedium), 16);
  CheckSize(SizeOf(TK64PoolMedium), 64 * 1024);
  CheckSize(SizeOf(TSyncStack64), 64);
  CheckSize(SizeOf(TThreadHeap), {$ifdef CPUX64}7{$else}4{$endif} * 64);
end;

const
  BASIC_SMALL_SIZES: array[0..11] of Integer = (1, 2, 15, 16, 17, 31, 32, 33,
    48, 64, 127, 128);
  BASIC_MEDIUM_SIZES: array[0..6] of Integer = (129, 130, 1024, 4 * 1024,
    MAX_MEDIUM_SIZE - 16, MAX_MEDIUM_SIZE - 1, MAX_MEDIUM_SIZE);
  // todo big/large
  BASIC_BIG_SIZES: array[0..4] of Integer = (MAX_MEDIUM_SIZE + 1,
    MAX_MEDIUM_SIZE + 16, MAX_MEDIUM_SIZE + 17, MAX_MEDIUM_SIZE + 32, MAX_BIG_SIZE);

procedure TestGetMem;
var
  P: Pointer;
  P2: Pointer;

  i, Size: Integer;
begin
  P := PTR_FAILURE;
  GetMem(P, 0);
  Check(nil, P);

  P := PTR_FAILURE;
  GetMem(P, -1);
  Check(nil, P);

  P := PTR_FAILURE;
  GetMem(P, -100500);
  Check(nil, P);

  // small
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    Size := BASIC_SMALL_SIZES[i];

    P := PTR_FAILURE;
    GetMem(P, Size);
    Info.Init(P);

    if (Info.AsSmall = nil) then SystemError;
    if (Info.Size <> ((Size + 15) and -16)) then SystemError;

    FreeMem(P);
    CheckEmptyHeap;
  end;

  // medium
  for i := Low(BASIC_MEDIUM_SIZES) to High(BASIC_MEDIUM_SIZES) do
  begin
    Size := BASIC_MEDIUM_SIZES[i];

    P := PTR_FAILURE;
    GetMem(P, Size);
    Info.Init(P);

    if (Info.AsMedium = nil) then SystemError;
    if (Info.Size <> ((Size + 15) and -16)) then SystemError;

    FreeMem(P);
    CheckEmptyHeap;
  end;

  // 4kb-aligned medium
  begin
    GetMem(P, (4 * 1024) - 3 * 16 - 2 * SizeOf(THeaderMedium));
    GetMem(P2, MAX_SMALL_SIZE + 1);
    if (NativeInt(P2) and MASK_K4_TEST = 0) then SystemError;
    Info.Init(P);
    if (Info.AsMedium = nil) or (Info.Size <> (4 * 1024) - 4 * 16) then SystemError;
    FreeMem(P);
    FreeMem(P2);
    CheckEmptyHeap;
  end;

  // todo big/large

  for i := Low(BASIC_BIG_SIZES) to High(BASIC_BIG_SIZES) do
  begin
    Size := BASIC_BIG_SIZES[i];

    P := PTR_FAILURE;
    GetMem(P, Size);
    Info.Init(P);

    if (Info.AsSmall <> nil) or (Info.AsMedium <> nil) then
      SystemError;

    FreeMem(P);
    CheckEmptyHeap;
  end;
end;

procedure TestGetMemAligned;
var
  P: Pointer;

  i, Size: Integer;
  Align: TMemoryAlign;
begin
  P := PTR_FAILURE;
  GetMemAligned(P, ma16Bytes, 0);
  Check(nil, P);

  P := PTR_FAILURE;
  GetMemAligned(P, ma512Bytes,  -1);
  Check(nil, P);

  P := PTR_FAILURE;
  GetMemAligned(P, ma1024Bytes, -100500);
  Check(nil, P);

  // small
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  for Align := Low(TMemoryAlign) to High(TMemoryAlign) do
  begin
    Size := BASIC_SMALL_SIZES[i];

    P := PTR_FAILURE;
    GetMemAligned(P, Align, Size);
    Info.Init(P);

    if (Align = ma16Bytes) then
    begin
      if (Info.AsSmall = nil) then SystemError;
    end else
    begin
      if (Info.AsMedium = nil) then SystemError;
    end;
    if (Info.Size <> ((Size + 15) and -16)) then SystemError;
    if (Info.Align < TPointerAlign(Align)) then SystemError;

    FreeMem(P);
    CheckEmptyHeap;
  end;

  // medium
  for i := Low(BASIC_MEDIUM_SIZES) to High(BASIC_MEDIUM_SIZES) do
  for Align := Low(TMemoryAlign) to High(TMemoryAlign) do
  begin
    Size := BASIC_MEDIUM_SIZES[i];

    P := PTR_FAILURE;
    GetMemAligned(P, Align, Size);
    Info.Init(P);

    if (Info.AsMedium = nil) then SystemError;
    if (Info.Size <> ((Size + 15) and -16)) then SystemError;
    if (Info.Align < TPointerAlign(Align)) then SystemError;

    FreeMem(P);
    CheckEmptyHeap;
  end;

  // todo big/large

  for i := Low(BASIC_BIG_SIZES) to High(BASIC_BIG_SIZES) do
  for Align := Low(TMemoryAlign) to High(TMemoryAlign) do
  begin
    Size := BASIC_BIG_SIZES[i];

    P := PTR_FAILURE;
    GetMemAligned(P, Align, Size);
    Info.Init(P);

    if (Info.AsSmall <> nil) or (Info.AsMedium <> nil) then
      SystemError;
    if (Info.Align < TPointerAlign(Align)) then SystemError;

    FreeMem(P);
    CheckEmptyHeap;
  end;
end;

procedure TestAllocMem;
var
  P: Pointer;
  PN, P2: Pointer;

  i, j, Size: Integer;
begin
  P := AllocMem(0);
  Check(nil, P);

  {$if (not Defined(PUREPASCAL)) and (not Defined(BRAINMM_NOREDIRECT))}
  P := AllocMem(-1);
  Check(nil, P);
  {$ifend}

  {$if (not Defined(PUREPASCAL)) and (not Defined(BRAINMM_NOREDIRECT))}
  P := AllocMem(-100500);
  Check(nil, P);
  {$ifend}

  // small(zero)
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    Size := BASIC_SMALL_SIZES[i];

    P := AllocMem(Size);
    Info.Init(P);
    if (Info.AsSmall = nil) then SystemError;
    if (Info.Size <> ((Size + 15) and -16)) then SystemError;

    PN := Pointer(NativeInt(P) + ((Size + 15) and -16));
    FillChar(PN^, 200, $ff);
    P2 := AllocMem(Size);
    if (P2 <> PN) then SystemError;
    Info.Init(P2);
    if (Info.AsSmall = nil) then SystemError;
    if (Info.Size <> ((Size + 15) and -16)) then SystemError;

    for j := 0 to Size - 1 do
    if (PSmallBytes(P2)[i] <> 0) then SystemError;

    if (PSmallBytes(P2)[(Size + 15) and -16] <> $ff) then SystemError;

    FreeMem(P);
    FreeMem(P2);
    CheckEmptyHeap;
  end;
end;

procedure TestFreeMem;
var
  P: Pointer;
  PoolInfo: TPoolMediumCompactInfo;

  SmallPtrs: array[Low(BASIC_SMALL_SIZES)..High(BASIC_SMALL_SIZES)] of Pointer;
  MediumPtrs: array[Low(BASIC_MEDIUM_SIZES)..High(BASIC_MEDIUM_SIZES)] of Pointer;
  i, Size: Integer;

  procedure InitPool;
  var
    i: Integer;
    P: Pointer;
  begin
    for i := Low(MediumPtrs) to High(MediumPtrs) do
    begin
      P := MediumPtrs[i];
      if (P <> nil) then Break;
    end;

    if (P = nil) then
    begin
      FillChar(PoolInfo, SizeOf(PoolInfo), #0);
    end else
    begin
      PoolInfo.Init(Pointer(NativeInt(P) and MASK_K64_CLEAR));
    end;
  end;

  procedure GetMediums(const Count: Integer; const Size: Integer = MEDIUM_TEST_SIZE);
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
    begin
      GetMemAligned(MediumPtrs[i], ma32Bytes, Size);
      InitPBytes(MediumPtrs[i], Size, i);
    end;
  end;

  procedure FreeMediums(const Count: Integer; const CheckEmpty: Boolean = True);
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      FreeMemNil(MediumPtrs[i]);

    if (CheckEmpty) then
      CheckEmptyHeap;
  end;
begin
  INC_TEST;
  if (MainThreadHeap = nil) then SystemError;

  P := nil;
  FreeMem(P);
  CheckEmptyHeap;
  FillChar(MediumPtrs, SizeOf(MediumPtrs), #0);

  // current + right: free pool
  begin
    GetMediums(1);
    FreeMediums(1);
  end;

  // current + right (index not changed)
  begin
    GetMediums(2);
    InitPool;
    Check(1, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(MediumPtrs[1]);
    InitPool;
    Check(1, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMediums(1);
  end;

  // current, left + current + right: free pool
  begin
    GetMediums(2);
    FreeMemNil(MediumPtrs[0]);
    InitPool;
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(MediumPtrs[1]);
    CheckEmptyHeap;
  end;

  // current + right (index changed)
  begin
    GetMediums(3);

    FreeMemNil(MediumPtrs[1]);
    InitPool;
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(MediumPtrs[0]);
    InitPool;
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[12].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMediums(3);
  end;

  // left + current: free pool
  begin
    GetMem(MediumPtrs[0], MAX_MEDIUM_SIZE);
    GetMem(MediumPtrs[1], MAX_MEDIUM_SIZE);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(0, PoolInfo.EmptiesTotal.Count);

    FreeMemNil(MediumPtrs[0]);
    FreeMemNil(MediumPtrs[1]);
    CheckEmptyHeap;
  end;

  // left + current: exclude/include
  begin
    GetMediums(3);

    FreeMemNil(MediumPtrs[0]);
    InitPool;
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(MediumPtrs[1]);
    InitPool;
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[12].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMediums(3);
  end;

  // left + current + right: exclude/include
  begin
    GetMediums(4);

    FreeMemNil(MediumPtrs[0]);
    FreeMemNil(MediumPtrs[2]);
    InitPool;
    Check(3, PoolInfo.EmptiesTotal.Count);
    Check(2, PoolInfo.Empties[8].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(MediumPtrs[1]);
    InitPool;
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[15].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMediums(4);
  end;

  // alloc small (deferred)
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    Size := BASIC_SMALL_SIZES[i];
    GetMem(SmallPtrs[i], Size);
  end;

  // alloc medium (deferred)
  for i := Low(BASIC_MEDIUM_SIZES) to High(BASIC_MEDIUM_SIZES) do
  begin
    Size := BASIC_MEDIUM_SIZES[i];
    GetMem(MediumPtrs[i], Size);
  end;

  // big
  Size := BASIC_BIG_SIZES[Low(BASIC_BIG_SIZES)];
  GetMem(P, Size);

  // free small deferred
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    INC_TEST;
    MainThreadHeap.PushThreadDeferred(SmallPtrs[i], @TestFreeMem, True);
  end;

  // free medium deferred
  for i := Low(BASIC_MEDIUM_SIZES) to High(BASIC_MEDIUM_SIZES) do
  begin
    INC_TEST;
    MainThreadHeap.PushThreadDeferred(MediumPtrs[i], @TestFreeMem, False);
  end;

  // each free
  FreeMem(P);
  CheckEmptyHeap;
end;

procedure TestSmall;
var
  i: Integer;

  P: Pointer;
  PTRS_16_FIRST: array[4..63] of Pointer;
  PTRS_16_SECOND: array[2..63] of Pointer;
  PTRS_128: array[1..7] of Pointer;
  PTRS_128_MANY: array[0..7 * 64 - 1] of Pointer;

  Index, Size, j: Integer;
  PTRS: array[0..2000 - 1] of Pointer;
begin
  INC_TEST;
  if (MainThreadHeap = nil) then SystemError;

  // first line
  for i := Low(PTRS_16_FIRST) to High(PTRS_16_FIRST) do
  begin
    GetMem(PTRS_16_FIRST[i], 16);
    if ((NativeInt(PTRS_16_FIRST[i]) shr 4) and 63 <> i) then
      SystemError;
  end;

  HeapInfo.InitMain;
  Check(1, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableFull);

  i := Low(PTRS_16_SECOND);
  GetMem(PTRS_16_SECOND[i], 16);
  HeapInfo.InitMain;
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[1].FullQueue);

  // second line
  for i := Low(PTRS_16_SECOND) + 1 to High(PTRS_16_SECOND) do
  begin
    GetMem(PTRS_16_SECOND[i], 16);
    if ((NativeInt(PTRS_16_SECOND[i]) shr 4) and 63 <> i) then
      SystemError;
  end;

  HeapInfo.InitMain;
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[1].FullQueue);

  // free: available full
  FreeMem(PTRS_16_SECOND[Low(PTRS_16_SECOND)]);
  HeapInfo.InitMain;
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[1].FullQueue);

  // free: full queue
  FreeMem(PTRS_16_FIRST[Low(PTRS_16_FIRST)]);
  HeapInfo.InitMain;
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(2, HeapInfo.K1LineSmalls[1].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(0, HeapInfo.K1LineSmalls[1].FullQueue);

  // free: 2 lines
  for i := Low(PTRS_16_FIRST) + 1 to High(PTRS_16_FIRST) do
    FreeMem(PTRS_16_FIRST[i]);
  for i := Low(PTRS_16_SECOND) + 1 to High(PTRS_16_SECOND) do
    FreeMem(PTRS_16_SECOND[i]);
  CheckEmptyHeap;

  // another size: 128 bytes
  for i := Low(PTRS_128) to High(PTRS_128) do
  begin
    GetMem(PTRS_128[i], 128);
    if ((NativeInt(PTRS_128[i]) shr 7) and 7 <> i) then
      SystemError;
  end;

  HeapInfo.InitMain;
  Check(1, HeapInfo.K1LineSmalls[8].Count);
  Check(1, HeapInfo.K1LineSmalls[8].AvailableFull);

  GetMem(P, 128);
  HeapInfo.InitMain;
  Check(2, HeapInfo.K1LineSmalls[8].Count);
  Check(1, HeapInfo.K1LineSmalls[8].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[8].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[8].FullQueue);

  // free 128
  FreeMem(P);
  for i := Low(PTRS_128) to High(PTRS_128) do FreeMem(PTRS_128[i]);
  CheckEmptyHeap;

  // small pool check (128)
  begin
    for i := Low(PTRS_128_MANY) to High(PTRS_128_MANY) do
    begin
      GetMem(PTRS_128_MANY[i], 128);
    end;
    HeapInfo.InitMain;
    Check(1, HeapInfo.PoolSmalls.Count);
    Check(1, HeapInfo.PoolSmalls.AvailableFull);
    Check(64, HeapInfo.K1LineSmalls[8].Count);
    Check(1, HeapInfo.K1LineSmalls[8].AvailableFull);
    Check(63, HeapInfo.K1LineSmalls[8].FullQueue);

    GetMem(P, 128);
    HeapInfo.InitMain;
    Check(2, HeapInfo.PoolSmalls.Count);
    Check(1, HeapInfo.PoolSmalls.AvailableNonFull);
    Check(1, HeapInfo.PoolSmalls.FullQueue);
    Check(64 + 1, HeapInfo.K1LineSmalls[8].Count);
    Check(1, HeapInfo.K1LineSmalls[8].AvailableNonFull);
    Check(64, HeapInfo.K1LineSmalls[8].FullQueue);

    FreeMem(P);
    for i := Low(PTRS_128_MANY) to High(PTRS_128_MANY) do FreeMem(PTRS_128_MANY[i]);
    CheckEmptyHeap;
  end;

  // allocate random
  for i := Low(PTRS) to High(PTRS) do
  begin
    Index := Random(8);
    Size := (Index + 1) shl 4;
    GetMem(P, Size);
    PTRS[i] := P;

    Index := Index + (i and (255 and -8));
    FillChar(P^, Size, Byte(Index));
  end;

  // try heap analize
  HeapInfo.InitMain;

  // mixup
  MixPointers(PTRS);

  // free random
  for i := Low(PTRS) to High(PTRS) do
  begin
    P := PTRS[i];
    Info.Init(P);
    Index := PByte(P)^;
    Size := ((Index and 7) + 1) shl 4;

    if (Info.AsSmall = nil) or (Info.Size <> Size) then
      SystemError;

    for j := 0 to Size - 1 do
    if (PSmallBytes(P)[j] <> Byte(Index)) then
      SystemError;

    FreeMem(P);
  end;

  // check empty
  CheckEmptyHeap;
end;

procedure TestMedium;
var
  PoolInfo: TPoolMediumCompactInfo;
  P1, P2, P3, P4, StoredP: Pointer;
  Size, i, j: Integer;
  Align: TMemoryAlign;
  PTRS: array[0..2000 - 1] of Pointer;

  procedure InitPool;
  var
    P: Pointer;
  begin
    P := P1;
    if (P = nil) then P := P2;
    if (P = nil) then P := P3;

    PoolInfo.Init(Pointer(NativeInt(P) and MASK_K64_CLEAR));
  end;

  procedure GetThreeP;
  begin
    UTBrainMM.GetThreeMediumP(P1, P2, P3, MEDIUM_TEST_SIZE, True);
    InitPool;
  end;

  procedure FreeThreeP;
  begin
    UTBrainMM.FreeThreeP(P1, P2, P3, True);
  end;

begin
  INC_TEST;
  if (MainThreadHeap = nil) then SystemError;

  // free: current
  begin
    GetThreeP;
    Check(1, HeapInfo.PoolMediums.Count);
    Check(3, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.EmptiesTotal.Count);
    Check(@PoolInfo.Value.Items[1 + $00], P1);
    Check(@PoolInfo.Value.Items[1 + $10], P2);
    Check(@PoolInfo.Value.Items[1 + $20], P3);

    FreeMemNil(P2);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);

    CheckPBytesAlign(P1, MEDIUM_TEST_SIZE, 1, ma32Bytes);
    CheckPBytesAlign(P3, MEDIUM_TEST_SIZE, 3, ma32Bytes);

    FreeThreeP;
  end;

  // free: current + right (not full, index not changed)
  begin
    GetThreeP;
    FreeMemNil(P3);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[31].Count);

    CheckPBytesAlign(P1, MEDIUM_TEST_SIZE, 1, ma32Bytes);
    CheckPBytesAlign(P2, MEDIUM_TEST_SIZE, 2, ma32Bytes);

    FreeThreeP;
  end;

  // free: current + right (not full, index changed)
  begin
    GetThreeP;
    FreeMemNil(P2);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(MEDIUM_TEST_SIZE, PoolInfo.Empties[8].Size);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(P1);
    InitPool;
    Check(1, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[12].Count);
    Check(MEDIUM_TEST_SIZE * 2 + 16, PoolInfo.Empties[12].Size);
    Check(1, PoolInfo.Empties[31].Count);
    CheckPBytesAlign(P3, MEDIUM_TEST_SIZE, 3, ma32Bytes);
  end;

  // free: left + current + right (full)
  begin
    FreeMem(P3);
    CheckEmptyHeap;
  end;

  // free: left + current (not full)
  begin
    GetThreeP;
    FreeMemNil(P1);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(MEDIUM_TEST_SIZE, PoolInfo.Empties[8].Size);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(P2);
    InitPool;
    Check(1, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[12].Count);
    Check(MEDIUM_TEST_SIZE * 2 + 16, PoolInfo.Empties[12].Size);
    Check(1, PoolInfo.Empties[31].Count);
    CheckPBytesAlign(P3, MEDIUM_TEST_SIZE, 3, ma32Bytes);

    // clear
    FreeMem(P3);
    CheckEmptyHeap;
  end;

  // get: (margin <= 32) --> take full piece
  begin
    GetThreeP;

    FreeMemNil(P2);
    GetMem(P2, MEDIUM_TEST_SIZE - 16);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeMemNil(P2);
    GetMem(P2, MEDIUM_TEST_SIZE - 32);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // get: (margin > 32) --> leave empty (index changed)
  begin
    GetThreeP;

    FreeMemNil(P2);
    GetMem(P2, MEDIUM_TEST_SIZE - 48);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[0].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // get: (margin > 32) --> leave empty (index not changed)
  begin
    GetMemAligned(P1, ma32Bytes, MEDIUM_TEST_SIZE);
    GetMem(P2, 1328);
    GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);

    FreeMemNil(P2);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[17].Count);
    Check(1, PoolInfo.Empties[31].Count);

    GetMem(P2, (MAX_SMALL_B16COUNT + 1) * 16);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[17].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // get advanced: (align offset) skip available
  begin
    GetMemAligned(P1, ma32Bytes, MEDIUM_TEST_SIZE - 64);
    {aligned 256}GetMemAligned(P2, ma32Bytes, MEDIUM_TEST_SIZE);
    {aligned 256}GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE - 64);
    {aligned 256}GetMemAligned(P4, ma32Bytes, MEDIUM_TEST_SIZE);

    // Empties[6] = (P1, P3)
    StoredP := P3;
    FreeMemNil(P3);
    FreeMemNil(P1);

    GetMemAligned(P3, ma128Bytes, MEDIUM_TEST_SIZE - 64);
    Check(StoredP, P3);
    Info.Init(P3);
    Check(MEDIUM_TEST_SIZE - 64, Info.Size);

    FreeMem(P4);
    FreeThreeP;
  end;

  // get advanced: (align offset) skip all Empties[Index] availables
  begin
    GetMemAligned(P1, ma32Bytes, MEDIUM_TEST_SIZE - 64);
    {aligned 256}GetMemAligned(P2, ma32Bytes, MEDIUM_TEST_SIZE);
    {aligned 256}GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE - 64);
    {aligned 256}GetMemAligned(P4, ma32Bytes, MEDIUM_TEST_SIZE + 256 - $C0);

    // Empties[6] = (P1, P3)
    StoredP := Pointer(NativeUInt(P4) + MEDIUM_TEST_SIZE + 256 - $C0 + 16);
    FreeMemNil(P3);
    FreeMemNil(P1);

    GetMemAligned(P3, ma1024Bytes, MEDIUM_TEST_SIZE - 64);
    Check(StoredP, P3);
    Info.Init(P3);
    Check(MEDIUM_TEST_SIZE - 64, Info.Size);

    FreeMem(P4);
    FreeThreeP;
  end;

  // left allocated AlignOffsetEmpty: (margin = 16) --> grow allocated
  begin
    GetMemAligned(P1, ma32Bytes, MEDIUM_TEST_SIZE - 16);
    Info.Init(P1);
    Check(MEDIUM_TEST_SIZE - 16, Info.Size);
    InitPBytes(P1, MEDIUM_TEST_SIZE - 16, 1);

    GetMemAligned(P2, ma32Bytes, MEDIUM_TEST_SIZE);
    Info.Init(P1);
    Check(MEDIUM_TEST_SIZE, Info.Size);
    CheckPBytes(P1, MEDIUM_TEST_SIZE - 16, 1);

    HeapInfo.InitMain;
    Check(1, HeapInfo.PoolMediums.EmptiesTotal.Count);
    Check(2, HeapInfo.PoolMediums.Allocated.Count);

    FreeThreeP;
  end;

  // left allocated AlignOffsetEmpty: (margin >= 32) --> new empty
  begin
    GetMemAligned(P1, ma32Bytes, MEDIUM_TEST_SIZE - 32);
    Info.Init(P1);
    Check(MEDIUM_TEST_SIZE - 32, Info.Size);
    InitPBytes(P1, MEDIUM_TEST_SIZE - 32, 1);

    GetMemAligned(P2, ma64Bytes, MEDIUM_TEST_SIZE);
    Info.Init(P1);
    Check(MEDIUM_TEST_SIZE - 32, Info.Size);
    CheckPBytes(P1, MEDIUM_TEST_SIZE - 32, 1);

    HeapInfo.InitMain;
    Check(16, HeapInfo.PoolMediums.Empties[0].Size);
    Check(2, HeapInfo.PoolMediums.EmptiesTotal.Count);
    Check(2, HeapInfo.PoolMediums.Allocated.Count);

    FreeThreeP;
  end;

  // get advanced: (margin <= 32) --> take full piece (exclude empty)
  begin
    // magin = 0
    begin
      GetMem(P1, MEDIUM_TEST_SIZE - 16);
      {[8]}GetMem(P2, 224);
      GetMem(P3, MEDIUM_TEST_SIZE);

      Info.Init(P2);
      Check(224, Info.Size);
      FreeMemNil(P2);
      InitPool;
      Check(2, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[8].Count);
      Check(1, PoolInfo.Empties[31].Count);

      GetMemAligned(P2, ma32Bytes, 224 - 16 - 0);
      Info.Init(P1);
      Check(MEDIUM_TEST_SIZE, Info.Size);
      Info.Init(P2);
      Check(224 - 16, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[31].Count);
      FreeThreeP;
    end;

    // magin = 16
    begin
      GetMem(P1, MEDIUM_TEST_SIZE - 16);
      {[8]}GetMem(P2, 224);
      GetMem(P3, MEDIUM_TEST_SIZE);

      Info.Init(P2);
      Check(224, Info.Size);
      FreeMemNil(P2);
      InitPool;
      Check(2, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[8].Count);
      Check(1, PoolInfo.Empties[31].Count);

      GetMemAligned(P2, ma32Bytes, 224 - 16 - 16);
      Info.Init(P1);
      Check(MEDIUM_TEST_SIZE, Info.Size);
      Info.Init(P2);
      Check(224 - 16, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[31].Count);
      FreeThreeP;
    end;

    // magin = 32
    begin
      GetMem(P1, MEDIUM_TEST_SIZE - 16);
      {[8]}GetMem(P2, 224);
      GetMem(P3, MEDIUM_TEST_SIZE);

      Info.Init(P2);
      Check(224, Info.Size);
      FreeMemNil(P2);
      InitPool;
      Check(2, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[8].Count);
      Check(1, PoolInfo.Empties[31].Count);

      GetMemAligned(P2, ma32Bytes, 224 - 16 - 32);
      Info.Init(P1);
      Check(MEDIUM_TEST_SIZE, Info.Size);
      Info.Init(P2);
      Check(224 - 16, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[31].Count);
      FreeThreeP;
    end;
  end;

  // get advanced: (margin > 32) --> leave empty (index changed)
  begin
    GetMem(P1, MEDIUM_TEST_SIZE - 16);
    {[14]}GetMem(P2, 576);
    GetMem(P3, MEDIUM_TEST_SIZE);

    FreeMemNil(P2);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[14].Count);
    Check(1, PoolInfo.Empties[31].Count);

    GetMemAligned(P2, ma32Bytes, 16);
    Info.Init(P1);
    Check(MEDIUM_TEST_SIZE, Info.Size);
    Info.Init(P2);
    Check(16, Info.Size);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[13].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // get advanced: (margin > 32) --> leave empty (index not changed)
  begin
    GetMem(P1, MEDIUM_TEST_SIZE - 16);
    {[13]}GetMem(P2, 576 - 16);
    GetMem(P3, MEDIUM_TEST_SIZE);

    FreeMemNil(P2);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[13].Count);
    Check(1, PoolInfo.Empties[31].Count);

    GetMemAligned(P2, ma32Bytes, 16);
    Info.Init(P1);
    Check(MEDIUM_TEST_SIZE, Info.Size);
    Info.Init(P2);
    Check(16, Info.Size);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[13].Count);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // allocate random
  for i := Low(PTRS) to High(PTRS) do
  begin
    repeat
      Align := TMemoryAlign(Random(Ord(High(TMemoryAlign)) + 1));
      Size := Random(MAX_MEDIUM_SIZE) + 1;
    until (Align <> ma16Bytes) or (Size > MAX_SMALL_SIZE);

    if (Align <> ma16Bytes) or (Random(10) < 3) then
    begin
      GetMemAligned(P1, Align, Size);
    end else
    begin
      GetMem(P1, Size);
    end;
    PTRS[i] := P1;

    j := PHeaderMedium(NativeInt(P1) - SizeOf(THeaderMedium)).WSize;
    if (j > (Size + 15) and -16) then
    begin
      if (Cardinal(j - ((Size + 15) and -16)) > 32) then SystemError;
      Size := j;
    end;
    FillChar(P1^, Size, Byte(i));
    PInteger(P1)^ := Size;
  end;

  // try heap analize
  HeapInfo.InitMain;

  // mixup
  MixPointers(PTRS);

  // free random
  for i := Low(PTRS) to High(PTRS) do
  begin
    P1 := PTRS[i];
    Info.Init(P1);

    Size := PInteger(P1)^;
    if (Info.AsMedium = nil) then SystemError;
    if (Info.Size <> (Size + 15) and -16) and
      (Info.Size <> (Size + 15 + 16) and -16) then SystemError;

    for j := 4 to Size - 1 do
    if (PMediumBytes(P1)[j] <> PMediumBytes(P1)[4]) then
      SystemError;

    FreeMem(P1);
  end;

  // check empty
  CheckEmptyHeap;
end;

procedure TestSmallResize(const Realloc: Boolean);
var
  ResizeMem: TResizeMem;
  i, j, k, Size: Integer;
  P, LastP: Pointer;

  LineP, LineLastP: Integer;
  PTRS: array[0..2000 - 1] of Pointer;
begin
  INC_TEST;
  if (MainThreadHeap = nil) then SystemError;

  // initialize
  ResizeMem := RESIZEMEM_PROCS[Realloc];
  GetMem(P, MAX_SMALL_SIZE);
  Info.Init(P);
  Check(MAX_SMALL_SIZE, Info.Size);
  LastP := P;
  for j := 0 to MAX_SMALL_SIZE - 1 do PSmallBytes(P)[j] := j;

  // reduce
  for i := MAX_SMALL_SIZE downto 1 do
  begin
    ResizeMem(P, i);
    Info.Init(P);
    Check(MAX_SMALL_SIZE, Info.Size);
    Check(LastP, P);

    for j := 0 to MAX_SMALL_SIZE - 1 do
    if (PSmallBytes(P)[j] <> j) then SystemError;
  end;

  // grow (maximum)
  for i := 1 to MAX_SMALL_SIZE do
  begin
    ResizeMem(P, i);
    Info.Init(P);
    Check(MAX_SMALL_SIZE, Info.Size);
    Check(LastP, P);

    for j := 0 to MAX_SMALL_SIZE - 1 do
    if (PSmallBytes(P)[j] <> j) then SystemError;
  end;

  // clear
  FreeMem(P);
  CheckEmptyHeap;

  // grow
  for k := 1 to MAX_SMALL_B16COUNT do
  begin
    Size := k shl 4;
    GetMem(P, Size);
    LineLastP := (NativeInt(P) and MASK_K64_TEST) shr 10;
    for j := 0 to Size - 1 do PSmallBytes(P)[j] := j;

    for i := Size + 1 to MAX_SMALL_SIZE do
    begin
      ResizeMem(P, i);
      Info.Init(P);
      Check((i + 15) and -16, Info.Size);
      LineP := (NativeInt(P) and MASK_K64_TEST) shr 10;

      if (Realloc) then
      begin
        for j := 0 to Size - 1 do
        if (PSmallBytes(P)[j] <> j) then SystemError;

        if (((i - 1 - Size) shr 4) and 1 = 0) then
        begin
          Check(LineLastP + 1, LineP);
        end else
        begin
          Check(LineLastP, LineP);
        end;
      end else
      begin
        Check(LineLastP, LineP);
      end;
    end;

    FreeMem(P);
    CheckEmptyHeap;
  end;

  // allocate random
  for i := Low(PTRS) to High(PTRS) do
  begin
    GetMem(PTRS[i], Random(MAX_SMALL_SIZE) + 1);
  end;

  // try heap analize
  HeapInfo.InitMain;

  // mixup, realloc random
  MixPointers(PTRS);
  for i := Low(PTRS) to High(PTRS) do
  begin
    ResizeMem(PTRS[i], Random(MAX_SMALL_SIZE) + 1);
  end;

  // try heap analize
  HeapInfo.InitMain;

  // mixup, free random
  MixPointers(PTRS);
  for i := Low(PTRS) to High(PTRS) do
  begin
    FreeMem(PTRS[i]);
  end;

  // check empty
  CheckEmptyHeap;
end;


procedure TestMediumResize(const Realloc: Boolean);
var
  ResizeMem: TResizeMem;
  PoolInfo: TPoolMediumCompactInfo;
  P1, P2, P3, StoredP: Pointer;
  i, j, Size: Integer;
  Align: TMemoryAlign;
  PTRS: array[0..2000 - 1] of Pointer;

  procedure InitPool;
  var
    P: Pointer;
  begin
    P := P1;
    if (P = nil) then P := P2;
    if (P = nil) then P := P3;

    PoolInfo.Init(Pointer(NativeInt(P) and MASK_K64_CLEAR));
  end;

  procedure CheckPBytes(P: Pointer; Size: Integer; XorByte: Byte);
  begin
    if (Realloc) then
      UTBrainMM.CheckPBytes(P, Size, XorByte);
  end;

  procedure GetThreeP;
  begin
    UTBrainMM.GetThreeMediumP(P1, P2, P3, MEDIUM_TEST_SIZE, True);
    StoredP := P1;
  end;

  procedure FreeThreeP;
  begin
    UTBrainMM.FreeThreeP(P1, P2, P3, True);
  end;

  function ResizeOneP(var P: Pointer; const NewSize: NativeInt): Integer;
  begin
    ResizeMem(P, NewSize);
    Info.Init(P);
    HeapInfo.InitMain;
    Result := HeapInfo.PoolMediums.EmptiesTotal.Count;
  end;

  function FreeOneP(var P: Pointer): Integer;
  begin
    Info.Init(P);
    FreeMem(P);
    P := nil;
    HeapInfo.InitMain;
    Result := HeapInfo.PoolMediums.EmptiesTotal.Count;
  end;

begin
  // initialize
  INC_TEST;
  if (MainThreadHeap = nil) then SystemError;
  ResizeMem := RESIZEMEM_PROCS[Realloc];

  // reduce: ptr/data not changed
  begin
    GetMem(P1, MEDIUM_TEST_SIZE);
    StoredP := P1;
    InitPBytes(P1, MEDIUM_TEST_SIZE, 1);

    for i := MEDIUM_TEST_SIZE downto 1 do
    begin
      ResizeMem(P1, i);
      Check(StoredP, P1);
      Info.Init(P1);
      if (Cardinal(Info.Size - ((i + 15) and -16)) > 16) then
        SystemError;

      UTBrainMM.CheckPBytes(P1, Info.Size, 1);
      HeapInfo.InitMain;
    end;

    FreeMem(P1);
    CheckEmptyHeap;
  end;

  // reduce: right is empty, index not changed
  begin
    GetMem(P1, MEDIUM_TEST_SIZE);
    {[8]}GetMem(P2, 224);
    GetMem(P3, MEDIUM_TEST_SIZE);

    FreeMemNil(P2);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(224, PoolInfo.Empties[8].Size);
    Check(1, PoolInfo.Empties[31].Count);

    StoredP := P1;
    ResizeMem(P1, MEDIUM_TEST_SIZE - 32);
    Check(StoredP, P1);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(224 + 32, PoolInfo.Empties[8].Size);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // reduce: right is empty, index changed
  begin
    GetMem(P1, MEDIUM_TEST_SIZE);
    {[8]}GetMem(P2, 224);
    GetMem(P3, MEDIUM_TEST_SIZE);

    FreeMemNil(P2);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(224, PoolInfo.Empties[8].Size);
    Check(1, PoolInfo.Empties[31].Count);

    StoredP := P1;
    ResizeMem(P1, MEDIUM_TEST_SIZE - 48);
    Check(StoredP, P1);
    InitPool;
    Check(2, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.Empties[9].Count);
    Check(224 + 48, PoolInfo.Empties[9].Size);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // reduce: right is allocated, new empty
  begin
    GetThreeP;

    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(1, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[31].Count);

    StoredP := P1;
    ResizeMem(P1, MEDIUM_TEST_SIZE - 32);
    Check(StoredP, P1);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[0].Count);
    Check(16, PoolInfo.Empties[0].Size);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // grow right: (margin <= 32) --> take full piece (exclude empty)
  begin
    // margin = 0
    begin
      GetThreeP;
      StoredP := P2;

      ResizeMem(P2, 16);
      Check(StoredP, P2);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[7].Count);
      Check(208, PoolInfo.Empties[7].Size);

      ResizeMem(P2, MEDIUM_TEST_SIZE);
      Check(StoredP, P2);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE, Info.Size);

      FreeThreeP;
    end;

    // margin = 16
    begin
      GetThreeP;
      StoredP := P2;

      ResizeMem(P2, 16);
      Check(StoredP, P2);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[7].Count);
      Check(208, PoolInfo.Empties[7].Size);

      ResizeMem(P2, MEDIUM_TEST_SIZE - 16);
      Check(StoredP, P2);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE, Info.Size);

      FreeThreeP;
    end;

    // margin = 32
    begin
      GetThreeP;
      StoredP := P2;

      ResizeMem(P2, 16);
      Check(StoredP, P2);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[7].Count);
      Check(208, PoolInfo.Empties[7].Size);

      ResizeMem(P2, MEDIUM_TEST_SIZE - 32);
      Check(StoredP, P2);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE, Info.Size);

      FreeThreeP;
    end;
  end;

  // grow right: (margin > 32) --> empty resize (index not changed)
  begin
    GetThreeP;
    StoredP := P2;

    ResizeMem(P2, 16);
    Check(StoredP, P2);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[7].Count);
    Check(208, PoolInfo.Empties[7].Size);

    ResizeMem(P2, 32);
    Check(StoredP, P2);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[7].Count);
    Check(192, PoolInfo.Empties[7].Size);
    Info.Init(P2);
    Check(32, Info.Size);

    FreeThreeP;
  end;

  // grow right: (margin > 32) --> empty resize (index changed)
  begin
    GetThreeP;
    StoredP := P2;

    ResizeMem(P2, 16);
    Check(StoredP, P2);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[7].Count);
    Check(208, PoolInfo.Empties[7].Size);

    ResizeMem(P2, 48);
    Check(StoredP, P2);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[6].Count);
    Check(176, PoolInfo.Empties[6].Size);
    Info.Init(P2);
    Check(48, Info.Size);

    FreeThreeP;
  end;

  // grow penalty: (left is allocated) --> new place
  begin
    GetThreeP;

    StoredP := Pointer(NativeUInt(P3) + MEDIUM_TEST_SIZE + SizeOf(THeaderMedium));
    ResizeMem(P2, MEDIUM_TEST_SIZE + 16);
    Check(StoredP, P2);
    Info.Init(P2);
    Check(ma32Bytes, Info.AsMedium.Align);
    Check(MEDIUM_TEST_SIZE + 16, Info.Size);
    CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[8].Count);
    Check(MEDIUM_TEST_SIZE, PoolInfo.Empties[8].Size);

    FreeThreeP;
  end;

  // grow penalty: (left is empty, align offset = 0, size is not suitable) --> new place
  begin
    GetThreeP;

    StoredP := Pointer(NativeUInt(P3) + MEDIUM_TEST_SIZE + SizeOf(THeaderMedium));
    ResizeMem(P1, MEDIUM_TEST_SIZE - 32);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[0].Count);
    Check(16, PoolInfo.Empties[0].Size);

    ResizeMem(P2, MEDIUM_TEST_SIZE + 48);
    Check(StoredP, P2);
    Info.Init(P2);
    Check(ma32Bytes, Info.AsMedium.Align);
    Check(MEDIUM_TEST_SIZE + 48, Info.Size);
    CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[9].Count);
    Check(MEDIUM_TEST_SIZE + 32, PoolInfo.Empties[9].Size);

    FreeThreeP;
  end;

  // grow penalty: (left is empty, align offset = 16, size is not suitable) --> new place
  begin
    GetThreeP;

    StoredP := Pointer(NativeUInt(P3) + MEDIUM_TEST_SIZE + SizeOf(THeaderMedium));
    ResizeMem(P1, MEDIUM_TEST_SIZE - 48);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[0].Count);
    Check(32, PoolInfo.Empties[0].Size);

    ResizeMem(P2, MEDIUM_TEST_SIZE + 48);
    Check(StoredP, P2);
    Info.Init(P2);
    Check(ma32Bytes, Info.AsMedium.Align);
    Check(MEDIUM_TEST_SIZE + 48, Info.Size);
    CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[9].Count);
    Check(MEDIUM_TEST_SIZE + 48, PoolInfo.Empties[9].Size);

    FreeThreeP;
  end;

  // grow penalty: (left is empty, align offset = 0, margin <= 32) --> take full piece
  begin
    // magin = 0
    begin
      GetThreeP;

      StoredP := Pointer(NativeUInt(P2) - 32);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 32);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[0].Count);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + 32);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + 32, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[31].Count);

      FreeThreeP;
    end;

    // magin = 16
    begin
      GetThreeP;

      StoredP := Pointer(NativeUInt(P2) - 32);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 32);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[0].Count);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + 16);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + 32, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[31].Count);

      FreeThreeP;
    end;

    // magin = 32
    begin
      GetThreeP;

      StoredP := Pointer(NativeUInt(P2) - 64);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 64);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(2, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[1].Count);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + 32);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + 64, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[31].Count);

      FreeThreeP;
    end;
  end;

  // grow penalty: (left is empty, align offset = 0, margin > 32) --> include empty
  begin
    GetThreeP;

    StoredP := Pointer(NativeUInt(P2) - 64);
    ResizeMem(P1, MEDIUM_TEST_SIZE - 64);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[1].Count);
    Check(1, PoolInfo.Empties[31].Count);

    ResizeMem(P2, MEDIUM_TEST_SIZE + 16);
    Check(StoredP, P2);
    CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
    Info.Init(P2);
    Check(MEDIUM_TEST_SIZE + 16, Info.Size);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[0].Count);
    Check(32, PoolInfo.Empties[0].Size);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // grow penalty: (left/right are empty, align offset = 0, margin <= 32) --> take full piece
  begin
    // magin = 0
    begin
      GetThreeP;
      FreeMem(P3);
      GetMemAligned(StoredP, ma32Bytes, 16);
      GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
      FreeMem(StoredP);
      StoredP := Pointer(NativeUInt(P2) - 32);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 32);

      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(3, PoolInfo.EmptiesTotal.Count);
      Check(2, PoolInfo.Empties[0].Count);
      Check(32, PoolInfo.Empties[0].Size);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + 64);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + 64, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);

      FreeThreeP;
    end;

    // magin = 16
    begin
      GetThreeP;
      FreeMem(P3);
      GetMemAligned(StoredP, ma32Bytes, 16);
      GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
      FreeMem(StoredP);
      StoredP := Pointer(NativeUInt(P2) - 32);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 32);

      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(3, PoolInfo.EmptiesTotal.Count);
      Check(2, PoolInfo.Empties[0].Count);
      Check(32, PoolInfo.Empties[0].Size);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + 48);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + 64, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);

      FreeThreeP;
    end;

    // magin = 32
    begin
      GetThreeP;
      FreeMem(P3);
      GetMemAligned(StoredP, ma32Bytes, 16);
      GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
      FreeMem(StoredP);
      StoredP := Pointer(NativeUInt(P2) - 64);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 64);

      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(3, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[0].Count);
      Check(16, PoolInfo.Empties[0].Size);
      Check(1, PoolInfo.Empties[1].Count);
      Check(48, PoolInfo.Empties[1].Size);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + 64);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + 64 + 32, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);

      FreeThreeP;
    end;
  end;

  // grow penalty: (left/right are empty, align offset = 0, margin > 32) --> exclude+include empty
  begin
    GetThreeP;
    FreeMem(P3);
    GetMemAligned(StoredP, ma32Bytes, 16);
    GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
    FreeMem(StoredP);
    StoredP := Pointer(NativeUInt(P2) - 64);
    ResizeMem(P1, MEDIUM_TEST_SIZE - 64);

    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(3, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[0].Count);
    Check(16, PoolInfo.Empties[0].Size);
    Check(1, PoolInfo.Empties[1].Count);
    Check(48, PoolInfo.Empties[1].Size);
    Check(1, PoolInfo.Empties[31].Count);

    ResizeMem(P2, MEDIUM_TEST_SIZE + 48);
    Check(StoredP, P2);
    CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
    Info.Init(P2);
    Check(MEDIUM_TEST_SIZE + 48, Info.Size);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(2, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[0].Count);
    Check(32, PoolInfo.Empties[0].Size);
    Check(1, PoolInfo.Empties[31].Count);

    FreeThreeP;
  end;

  // grow penalty: (left/right are empty, align offset = 16, margin <= 32) --> grow allocated, take full empty piece
  begin
    // magin = 0
    begin
      GetThreeP;
      FreeMem(P3);
      GetMemAligned(StoredP, ma32Bytes, 16);
      GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
      FreeMem(StoredP);
      StoredP := Pointer(NativeUInt(P2) - 32);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 48);

      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(3, PoolInfo.EmptiesTotal.Count);
      Check(2, PoolInfo.Empties[0].Count);
      Check(48, PoolInfo.Empties[0].Size);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + (48 - 16) + 32);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + (48 - 16) + 32, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Info.Init(P1);
      Check(MEDIUM_TEST_SIZE - 32, Info.Size);

      FreeThreeP;
    end;

    // magin = 16
    begin
      GetThreeP;
      FreeMem(P3);
      GetMemAligned(StoredP, ma32Bytes, 16);
      GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
      FreeMem(StoredP);
      StoredP := Pointer(NativeUInt(P2) - 32);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 48);

      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(3, PoolInfo.EmptiesTotal.Count);
      Check(2, PoolInfo.Empties[0].Count);
      Check(48, PoolInfo.Empties[0].Size);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + (48 - 16) + 32 - 16);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + (48 - 16) + 32, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Info.Init(P1);
      Check(MEDIUM_TEST_SIZE - 32, Info.Size);

      FreeThreeP;
    end;

    // magin = 32
    begin
      GetThreeP;
      FreeMem(P3);
      GetMemAligned(StoredP, ma32Bytes, 16);
      GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
      FreeMem(StoredP);
      StoredP := Pointer(NativeUInt(P2) - 64);
      ResizeMem(P1, MEDIUM_TEST_SIZE - 80);

      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(3, PoolInfo.EmptiesTotal.Count);
      Check(1, PoolInfo.Empties[0].Count);
      Check(16, PoolInfo.Empties[0].Size);
      Check(1, PoolInfo.Empties[2].Count);
      Check(64, PoolInfo.Empties[2].Size);
      Check(1, PoolInfo.Empties[31].Count);

      ResizeMem(P2, MEDIUM_TEST_SIZE + (80 - 16) + 32 - 32);
      Check(StoredP, P2);
      CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
      Info.Init(P2);
      Check(MEDIUM_TEST_SIZE + (80 - 16) + 32, Info.Size);
      InitPool;
      Check(3, PoolInfo.Allocated.Count);
      Check(1, PoolInfo.EmptiesTotal.Count);
      Info.Init(P1);
      Check(MEDIUM_TEST_SIZE - 80 + 16, Info.Size);

      FreeThreeP;
    end;
  end;

  // grow penalty: (left/right are empty, align offset > 16, margin > 32) --> exclude modified left/right + include modified left/right
  begin
    GetThreeP;
    StoredP := P2;
    FreeMemNil(P2);
    GetMemAligned(P2, ma64Bytes, MEDIUM_TEST_SIZE);
    Check(StoredP, P2);
    Info.Init(P2);
    Check(MEDIUM_TEST_SIZE, Info.Size);
    Check(ma64Bytes, Info.AsMedium.Align);
    InitPBytes(P2, MEDIUM_TEST_SIZE, 2);

    FreeMem(P3);
    GetMemAligned(StoredP, ma32Bytes, 48);
    GetMemAligned(P3, ma32Bytes, MEDIUM_TEST_SIZE);
    FreeMem(StoredP);
    StoredP := Pointer(NativeUInt(P2) - 128);
    ResizeMem(P1, MEDIUM_TEST_SIZE - 128 - 32);

    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(3, PoolInfo.EmptiesTotal.Count);
    Check(1, PoolInfo.Empties[1].Count);
    Check(48, PoolInfo.Empties[1].Size);
    Check(1, PoolInfo.Empties[5].Count);
    Check(144, PoolInfo.Empties[5].Size);
    Check(1, PoolInfo.Empties[31].Count);

    ResizeMem(P2, 128 + MEDIUM_TEST_SIZE + 16);
    Check(StoredP, P2);
    CheckPBytes(P2, MEDIUM_TEST_SIZE, 2);
    Info.Init(P2);
    Check(128 + MEDIUM_TEST_SIZE + 16, Info.Size);
    InitPool;
    Check(3, PoolInfo.Allocated.Count);
    Check(3, PoolInfo.EmptiesTotal.Count);
    Check(2, PoolInfo.Empties[0].Count);
    Check(16 + 32, PoolInfo.Empties[0].Size);
    Check(1, PoolInfo.Empties[31].Count);
    Info.Init(P1);
    Check(MEDIUM_TEST_SIZE - 128 - 32, Info.Size);

    FreeThreeP;
  end;

  // allocate random
  for i := Low(PTRS) to High(PTRS) do
  begin
    repeat
      Align := TMemoryAlign(Random(Ord(High(TMemoryAlign)) + 1));
      Size := Random(MAX_MEDIUM_SIZE) + 1;
    until (Align <> ma16Bytes) or (Size > MAX_SMALL_SIZE);

    repeat
      Align := TMemoryAlign(Random(Ord(High(TMemoryAlign)) + 1));
      Size := Random(MAX_MEDIUM_SIZE) + 1;
    until (Align <> ma16Bytes) or (Size > MAX_SMALL_SIZE);

    if (Align <> ma16Bytes) or (Random(10) < 3) then
    begin
      GetMemAligned(P1, Align, Size);
    end else
    begin
      GetMem(P1, Size);
    end;
    PTRS[i] := P1;
  end;

  // realloc random
  MixPointers(PTRS);
  for i := Low(PTRS) to High(PTRS) do
  begin
    Size := Random(MAX_MEDIUM_SIZE) + 1;
    ResizeMem(PTRS[i], Size);

    P1 := PTRS[i];
    FillChar(P1^, Size, Byte(i));
    PInteger(P1)^ := Size;
  end;

  // free random
  MixPointers(PTRS);
  for i := Low(PTRS) to High(PTRS) do
  begin
    P1 := PTRS[i];
    Info.Init(P1);

    Size := PInteger(P1)^;
    if (Info.AsMedium = nil) then SystemError;
    if (Cardinal(Info.Size - ((Size + 15) and -16)) > 32) then SystemError;

    if (Realloc) then
    begin
      for j := 4 to Size - 1 do
      if (PMediumBytes(P1)[j] <> PMediumBytes(P1)[4]) then
        SystemError;
    end;

    FreeMem(P1);
  end;

  // check empty
  CheckEmptyHeap;
end;


procedure TestDifficults;
var
  P: Pointer;
  Realloc: Boolean;

  procedure CheckPBytes(P: Pointer; Size: Integer; XorByte: Byte);
  begin
    if (Realloc) then
      UTBrainMM.CheckPBytes(P, Size, XorByte);
  end;

  function ResizeDifficult(P: Pointer; NewB16Count: NativeUInt): Pointer;
  begin
    Result := MainThreadHeap.ResizeDifficult(NativeInt(P) + Ord(Realloc), NewB16Count, @TestDifficults);
  end;

begin
  INC_TEST;
  if (MainThreadHeap = nil) then SystemError;
  MainThreadHeap.ErrorAddr := @TestDifficults;

  // free difficult: small
  begin
    GetMem(P, MAX_SMALL_SIZE);

    if (MainThreadHeap.FreeDifficult(P, @TestDifficults) <> FREEMEM_DONE) then SystemError;
    if (not MainThreadHeap.Deferreds.Assigned) then SystemError;
    MainThreadHeap.ProcessThreadDeferred;

    CheckEmptyHeap;
  end;

  // free difficult: medium
  begin
    GetMem(P, MAX_MEDIUM_SIZE);

    if (MainThreadHeap.FreeDifficult(P, @TestDifficults) <> FREEMEM_DONE) then SystemError;
    if (not MainThreadHeap.Deferreds.Assigned) then SystemError;
    MainThreadHeap.ProcessThreadDeferred;

    CheckEmptyHeap;
  end;

  // realloc/reget
  for Realloc := True downto False do
  begin
    // small --> medium
    begin
      GetMem(P, MAX_SMALL_SIZE);
      InitPBytes(P, MAX_SMALL_SIZE, 1);

      P := ResizeDifficult(P, MAX_SMALL_B16COUNT + 1);
      if (MainThreadHeap.Deferreds.Assigned) then SystemError;
      CheckPBytes(P, MAX_SMALL_SIZE, 1);
      HeapInfo.InitMain;
      Check(0, HeapInfo.PoolSmalls.Count);
      Check(1, HeapInfo.PoolMediums.Count);
      Info.Init(P);
      Check(MAX_SMALL_SIZE + 16, Info.Size);
      if (Info.AsMedium.Align <> ma16Bytes) then SystemError;

      FreeMem(P);
      CheckEmptyHeap;
    end;

    // medium --> big/large
    begin
      GetMem(P, MAX_MEDIUM_SIZE);
      InitPBytes(P, MAX_MEDIUM_SIZE, 2);

      P := ResizeDifficult(P, MAX_MEDIUM_B16COUNT + 1);
      if (MainThreadHeap.Deferreds.Assigned) then SystemError;
      CheckPBytes(P, MAX_MEDIUM_SIZE, 2);
      HeapInfo.InitMain;
      Check(0, HeapInfo.PoolSmalls.Count);
      Check(0, HeapInfo.PoolMediums.Count);
      Info.Init(P);
      if (Info.AsSmall <> nil) or (Info.AsMedium <> nil) then SystemError;

      FreeMem(P);
      CheckEmptyHeap;
    end;

    // small --> big/large
    begin
      GetMem(P, MAX_SMALL_SIZE);
      InitPBytes(P, MAX_SMALL_SIZE, 3);

      P := ResizeDifficult(P, MAX_MEDIUM_B16COUNT + 1);
      if (MainThreadHeap.Deferreds.Assigned) then SystemError;
      CheckPBytes(P, MAX_SMALL_SIZE, 3);
      HeapInfo.InitMain;
      Check(0, HeapInfo.PoolSmalls.Count);
      Check(0, HeapInfo.PoolMediums.Count);
      Info.Init(P);
      if (Info.AsSmall <> nil) or (Info.AsMedium <> nil) then SystemError;

      FreeMem(P);
      CheckEmptyHeap;
    end;
  end;

  // check empty
  CheckEmptyHeap;
end;

procedure RUN_TESTS;
begin
  // basic
  TestSizes;
  TestGetMem;
  TestGetMemAligned;
  TestAllocMem;
  TestFreeMem;

  // small
  TestSmall;

  // medium
  TestMedium;

  // small realloc/reget
  TestSmallResize(True);
  TestSmallResize(False);

  // medium realloc/reget
  TestMediumResize(True);
  TestMediumResize(False);

  // todo big/large

  // free/realloc/reget difficult
  TestDifficults;

  // done
  if (Done) then Log('Done.');
  Halt;
end;


{ TCountSize }

procedure TCountSize.Clear;
begin
  Self.Count := 0;
  Self.Size := 0;
end;

procedure TCountSize.Add(const ASize: Integer);
begin
  Inc(Self.Count);
  Inc(Self.Size, ASize);
end;

procedure TCountSize.Add(const ACountSize: TCountSize);
begin
  Inc(Self.Count, ACountSize.Count);
  Inc(Self.Size, ACountSize.Size);
end;


{ TBitSetInfo }

procedure TBitSetInfo.Init(const V: TBitSet8);
var
  i: Integer;
begin
  Value := V;

  Empty := (V.V64 = -1);
  Full := (V.V64 = 0);
  LowBit := -1;
  HighBit := -1;
  FillChar(Bits, SizeOf(Bits), False);
  Count := 0;

  for i := 0 to 63 do
  if (V.V64 and (Int64(1) shl i) <> 0) then
  begin
    Bits[i] := True;
    Inc(Count);

    if (LowBit < 0) then LowBit := i;
    HighBit := i;
  end;
end;

{ TK1LineCompactInfo }

procedure TK1LineCompactInfo.Init(const V: PK1LineSmall);
var
  i: Integer;
  ItemSet: TBitSet8;
begin
  Value := V;
  ValueIndex := Integer((NativeUInt(V) div 1024) and 63);
  if (V = nil) or (NativeInt(V) and MASK_K1_TEST <> 0) then
    SystemError;

  ValuePool := Pointer(NativeInt(V) and MASK_K64_CLEAR);
  if (ValuePool = nil) or (ValuePool.ThreadHeap = nil) or
    (Pointer(not ValuePool.ThreadHeap.FMarkerNotSelf) <> ValuePool.ThreadHeap) or
    (ValuePool.LineSet.V64 and (Int64(1) shl ValueIndex) <> 0) then
    SystemError;

  Size := V.Header.ModeSize and $f0;
  Index := V.Header.ModeSize and 15;
  if (Size <> (16 * (Index and 7 + 1))) then
    SystemError;
  if (Ord(NativeInt(V) and MASK_K64_TEST = 0) <> Ord(Index >= 8)) then
    SystemError;
  if (V.Header.InQK64PoolSmallFull) and (Index <= 7) then
    SystemError;

  ItemSet.V64 := V.Header.ItemSet.V64 and -4;
  case (V.Header.ItemSet.VLow32 and 3) of
    0: InFullQueue := False;
    3: InFullQueue := True;
  else
    SystemError;
  end;
  if (ItemSet.V64 and (not DEFAULT_BITSETS_SMALL[Index]) <> 0) then
    SystemError;
  Items.Init(ItemSet);

  Empties.Clear;
  Allocated.Clear;
  for i := 0 to 63 do
  if (DEFAULT_BITSETS_SMALL[Index] and (Int64(1) shl i) <> 0) then
  begin
    if (Items.Bits[i]) then
    begin
      Empties.Add(Size);
    end else
    begin
      Allocated.Add(Size);
    end;
  end;
end;

{ TK1LineInfo }

procedure TK1LineInfo.Init(const V: PK1LineSmall);
var
  Found: Boolean;
  Current: PK1LineSmall;
  Left, Right: PK1LineSmall;
begin
  inherited Init(V);

  if (InFullQueue) then
  begin
    Current := ValuePool.ThreadHeap.QK1LineFull;
  end else
  begin
    Current := ValuePool.ThreadHeap.FK1LineSmalls[(Self.Index and 7) + 1];
  end;
  if (Current = nil) then SystemError;
  if (Current.Header.Queue.Prev <> nil) then SystemError;

  Found := (Current = V);
  while (True) do
  begin
    Left := Current;
    Current := Current.Header.Queue.Next;
    if (Current = nil) then Break;

    if (Current = V) then
      Found := True;

    if (Current.Header.Queue.Prev <> Left) then
      SystemError;
  end;
  if (not Found) then
    SystemError;

  Left := V.Header.Queue.Prev;
  if (Left = nil) then
  begin
    Prev := nil;
  end else
  begin
    Prev := Pointer(@_[0]);
    Prev.Init(Left);
  end;

  Right := V.Header.Queue.Next;
  if (Right = nil) then
  begin
    Next := nil;
  end else
  begin
    Next := Pointer(@_[SizeOf(TK1LineCompactInfo)]);
    Next.Init(Right);
  end;
end;

{ TPoolSmallCompactInfo }

procedure TPoolSmallCompactInfo.Init(const V: PK64PoolSmall);
var
  i: Integer;
  LineInfo: TK1LineCompactInfo;
begin
  Value := V;
  ValueThreadHeap := V.ThreadHeap;
  if (ValueThreadHeap = nil) or (Pointer(not ValueThreadHeap.FMarkerNotSelf) <> ValueThreadHeap) then
    SystemError;

  InFullQueue := V.Header.InQK64PoolSmallFull;
  Lines.Init(V.LineSet);

  Empties.Clear;
  Allocated.Clear;
  for i := Low(Lines.Bits) to High(Lines.Bits) do
  if (not Lines.Bits[i]) then
  begin
    LineInfo.Init(@V.Lines[i]);

    Empties.Add(LineInfo.Empties);
    Allocated.Add(LineInfo.Allocated);
  end;
end;

{ TPoolSmallInfo }

procedure TPoolSmallInfo.Init(const V: PK64PoolSmall);
var
  Found: Boolean;
  Current: PK64PoolSmall;
  Left, Right: PK64PoolSmall;
begin
  inherited Init(V);

  if (InFullQueue) then
  begin
    Current := ValueThreadHeap.QK64PoolSmallFull;
  end else
  begin
    Current := ValueThreadHeap.QK64PoolSmall;
  end;
  if (Current = nil) then SystemError;
  if (Current.Queue.Prev <> nil) then SystemError;

  Found := (Current = V);
  while (True) do
  begin
    Left := Current;
    Current := Current.Queue.Next;
    if (Current = nil) then Break;

    if (Current = V) then
      Found := True;

    if (Current.Queue.Prev <> Left) then
      SystemError;
  end;
  if (not Found) then
    SystemError;

  Left := V.Queue.Prev;
  if (Left = nil) then
  begin
    Prev := nil;
  end else
  begin
    Prev := Pointer(@_[0]);
    Prev.Init(Left);

    if (Prev.InFullQueue <> Self.InFullQueue) or
      (Prev.ValueThreadHeap <> Self.ValueThreadHeap)  then
      SystemError;
  end;

  Right := V.Queue.Next;
  if (Right = nil) then
  begin
    Next := nil;
  end else
  begin
    Next := Pointer(@_[SizeOf(TPoolSmallCompactInfo)]);
    Next.Init(Right);

    if (Next.InFullQueue <> Self.InFullQueue) or
      (Next.ValueThreadHeap <> Self.ValueThreadHeap)  then
      SystemError;
  end;
end;

{ TPoolMediumCompactInfo }

procedure TPoolMediumCompactInfo.Init(const V: PK64PoolMedium);
var
  i, Count: Integer;
  Header, Next, Finish: PHeaderMedium;
  Empty: PHeaderMediumEmpty;
begin
  Value := V;
  ValueThreadHeap := V.ThreadHeap;
  if (V.MarkerNil <> nil) or (ValueThreadHeap = nil) or
    (Pointer(not ValueThreadHeap.FMarkerNotSelf) <> ValueThreadHeap) then
    SystemError;

  Allocated.Clear;
  for i := Low(Empties) to High(Empties) do Empties[i].Clear;
  EmptiesTotal.Clear;

  if (Value.FlagsFakeAllocated <> MASK_MEDIUM_ALLOCATED) then
    SystemError;

  Header := @Value.Items[Low(Value.Items)];
  if (Header.PreviousSize <> 0) then
    SystemError;

  Finish := @Value.Items[High(Value.Items)];
  if (Finish.Flags <> MASK_MEDIUM_ALLOCATED) then
    SystemError;

  while (Header <> Finish) do
  begin
    Next := Pointer(NativeUInt(Header) + Header.WSize + SizeOf(THeaderMedium));
    if (Next.PreviousSize <> Header.WSize) then
      SystemError;

    if (Header.Allocated) then
    begin
      if (Header.Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then
        SystemError;

      Allocated.Add(Header.WSize);
    end else
    begin
      if (Header.Flags and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then
        SystemError;

      i := MEDIUM_INDEXES[Header.WSize shr 4];
      Empties[i].Add(Header.WSize);
      EmptiesTotal.Add(Header.WSize);

      Empty := Pointer(NativeUInt(Header) + Header.WSize);
      CheckEmpty(Empty, i);
    end;

    Header := Next;
  end;

  for i := Low(Empties) to High(Empties) do
  begin
    Count := 0;
    Empty := ValueThreadHeap.FMedium.FEmpties[i];

    while (Empty <> nil) do
    begin
      if (Pointer(NativeInt(Empty) and MASK_K64_CLEAR) = Pointer(V)) then
      begin
        Header := Pointer(NativeUInt(Empty) - Empty.Size);

        if (Empty.Size <> Header.WSize) then
          SystemError;

        if (Header.Flags and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then
          SystemError;

        if (i <> MEDIUM_INDEXES[Header.WSize shr 4]) then
          SystemError;

        Inc(Count);
      end;

      Empty := Empty.Next;
    end;

    if (Empties[i].Count <> Count) then
      SystemError;
  end;
end;

procedure TPoolMediumCompactInfo.CheckEmpty(const VEmpty: PHeaderMediumEmpty; const AIndex: Integer);
var
  Found: Boolean;
  S: NativeUInt;
  Current, Next: PHeaderMediumEmpty;
begin
  Current := ValueThreadHeap.FMedium.FEmpties[AIndex];
  if (Current = nil) then
    SystemError;

  Found := False;
  while (Current <> nil) do
  begin
    S := Current.Size;
    if (S and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then
      SystemError;

    if (PHeaderMedium(NativeUInt(Current) - S).Flags <> S) then
      SystemError;

    if (Current = VEmpty) then
      Found := True;

    Next := Current.Next;
    if (Next <> nil) and (Next.Prev <> Current) then
      SystemError;

    Current := Next;
  end;

  if (not Found) then
    SystemError;
end;

{ TPoolMediumInfo }

procedure TPoolMediumInfo.Init(const V: PK64PoolMedium);
var
  Found: Boolean;
  Current: PK64PoolMedium;
  Left, Right: PK64PoolMedium;
begin
  inherited Init(V);

  Current := ValueThreadHeap.FMedium.QK64PoolMedium;
  if (Current = nil) then SystemError;
  if (Current.Queue.Prev <> nil) then SystemError;

  Found := (Current = V);
  while (True) do
  begin
    Left := Current;
    Current := Current.Queue.Next;
    if (Current = nil) then Break;

    if (Current = V) then
      Found := True;

    if (Current.Queue.Prev <> Left) then
      SystemError;
  end;
  if (not Found) then
    SystemError;

  Left := V.Queue.Prev;
  if (Left = nil) then
  begin
    Prev := nil;
  end else
  begin
    Prev := Pointer(@_[0]);
    Prev.Init(Left);

    if (Prev.ValueThreadHeap <> Self.ValueThreadHeap)  then
      SystemError;
  end;

  Right := V.Queue.Next;
  if (Right = nil) then
  begin
    Next := nil;
  end else
  begin
    Next := Pointer(@_[SizeOf(TPoolMediumCompactInfo)]);
    Next.Init(Right);

    if (Next.ValueThreadHeap <> Self.ValueThreadHeap)  then
      SystemError;
  end;
end;

{ TThreadHeapInfo }

procedure TThreadHeapInfo.Init(const V: PThreadHeap);
var
  i, Index, Count: Integer;
  Line: PK1LineSmall;
  LineCounts: PK1LineSmallCounts;
  LineCompactInfo: TK1LineCompactInfo;
  LineInfo: TK1LineInfo;
  PoolSmall: PK64PoolSmall;
  PoolSmallInfo: TPoolSmallInfo;
  PoolMedium: PK64PoolMedium;
  PoolMediumInfo: TPoolMediumInfo;
  Current, Next: PHeaderMediumEmpty;
begin
  FillChar(Self, SizeOf(Self), #0);
  Value := V;
  if (PThreadHeap(not V.FMarkerNotSelf) <> V) then
    SystemError;

  for i := Low(V.FK1LineSmalls) to High(V.FK1LineSmalls) do
  begin
    Line := V.FK1LineSmalls[i];
    LineCounts := @Self.K1LineSmalls[i];

    while (Line <> nil) do
    begin
      LineCompactInfo.Init(Line);
      Inc(LineCounts.Count);

      if (LineCompactInfo.InFullQueue) then
        SystemError;

      if (LineCompactInfo.Items.Full) then
      begin
        Inc(LineCounts.AvailableFull);
      end else
      begin
        Inc(LineCounts.AvailableNonFull);
      end;

      Line := Line.Header.Queue.Next;
    end;
  end;

  Line := V.QK1LineFull;
  while (Line <> nil) do
  begin
    LineInfo.Init(Line);

    if (not LineInfo.InFullQueue) then
      SystemError;

    Index := (LineInfo.Index and 7) + 1;
    Inc(Self.K1LineSmalls[Index].Count);
    Inc(Self.K1LineSmalls[Index].FullQueue);

    if (LineInfo.Next = nil) then
    begin
      Line := nil;
    end else
    begin
      Line := LineInfo.Next.Value;
    end;
  end;

  for i := Low(Self.K1LineSmalls) to High(Self.K1LineSmalls) do
  with Self.K1LineSmalls[i] do
  begin
    if (Count <> AvailableNonFull + AvailableFull + FullQueue) then
      SystemError;
  end;

  PoolSmall := V.QK64PoolSmall;
  while (PoolSmall <> nil) do
  begin
    PoolSmallInfo.Init(PoolSmall);
    if (PoolSmallInfo.InFullQueue) then
      SystemError;

    Inc(Self.PoolSmalls.Count);
    if (PoolSmallInfo.Lines.Full) then
    begin
      Inc(Self.PoolSmalls.AvailableFull);
    end else
    begin
      Inc(Self.PoolSmalls.AvailableNonFull);
    end;

    Self.PoolSmalls.Empties.Add(PoolSmallInfo.Empties);
    Self.PoolSmalls.Allocated.Add(PoolSmallInfo.Allocated);

    if (PoolSmallInfo.Next = nil) then
    begin
      PoolSmall := nil;
    end else
    begin
      PoolSmall := PoolSmallInfo.Next.Value;
    end;
  end;

  PoolSmall := V.QK64PoolSmallFull;
  while (PoolSmall <> nil) do
  begin
    PoolSmallInfo.Init(PoolSmall);
    if (not PoolSmallInfo.InFullQueue) then
      SystemError;

    Inc(Self.PoolSmalls.Count);
    Inc(Self.PoolSmalls.FullQueue);

    Self.PoolSmalls.Empties.Add(PoolSmallInfo.Empties);
    Self.PoolSmalls.Allocated.Add(PoolSmallInfo.Allocated);

    if (PoolSmallInfo.Next = nil) then
    begin
      PoolSmall := nil;
    end else
    begin
      PoolSmall := PoolSmallInfo.Next.Value;
    end;
  end;

  with Self.PoolSmalls do
  begin
    if (Count <> AvailableNonFull + AvailableFull + FullQueue) then
      SystemError;

    if (Empties.Size + Allocated.Size > Count * SizeOf(TK64PoolSmall)) then
      SystemError;
  end;

  PoolMedium := V.FMedium.QK64PoolMedium;
  while (PoolMedium <> nil) do
  begin
    PoolMediumInfo.Init(PoolMedium);

    Inc(Self.PoolMediums.Count);
    if (PoolMediumInfo.EmptiesTotal.Size = 0) then
    begin
      Inc(Self.PoolMediums.CountFull);
    end else
    begin
      Inc(Self.PoolMediums.CountNonFull);
    end;

    Self.PoolMediums.Allocated.Add(PoolMediumInfo.Allocated);
    Self.PoolMediums.EmptiesTotal.Add(PoolMediumInfo.EmptiesTotal);
    for i := Low(Self.PoolMediums.Empties) to High(Self.PoolMediums.Empties) do
      Self.PoolMediums.Empties[i].Add(PoolMediumInfo.Empties[i]);

    if (PoolMediumInfo.Next = nil) then
    begin
      PoolMedium := nil;
    end else
    begin
      PoolMedium := PoolMediumInfo.Next.Value;
    end;
  end;

  with Self.PoolMediums do
  if (EmptiesTotal.Size + Allocated.Size > Count * SizeOf(THeaderMediumList)) then
    SystemError;

  for i := Low(Self.PoolMediums.Empties) to High(Self.PoolMediums.Empties) do
  begin
    Count := 0;

    Current := Value.FMedium.FEmpties[i];
    while (Current <> nil) do
    begin
      Inc(Count);

      Next := Current.Next;
      if (Next <> nil) and (Next.Prev <> Current) then
        SystemError;

      Current := Next;
    end;

    if (Self.PoolMediums.Empties[i].Count <> Count) then
      SystemError;
  end;
end;

procedure TThreadHeapInfo.InitMain;
begin
  Self.Init(MainThreadHeap);
end;

{ TPointerSmallInfo }

procedure TPointerSmallInfo.Init(const V: Pointer);
begin
  Pool.Init(Pointer(NativeInt(V) and MASK_K64_CLEAR));

  Line.Init(Pointer(NativeInt(V) and MASK_K1_CLEAR));
  Index := (NativeInt(V) and MASK_K1_TEST) shr 4;
  if (Line.Items.Bits[Index]) then
    SystemError;

  if (DEFAULT_BITSETS_SMALL[Line.Index] and (Int64(1) shl Index) = 0) then
    SystemError;
end;

{ THeaderMediumInfo }

procedure THeaderMediumInfo.Init(const V: PHeaderMedium);
const
  MASK_MEDIUM_ALIGNS: array[TMemoryAlign] of NativeInt = (
    {ma16Bytes}   16 - 1,
    {ma32Bytes}   32 - 1,
    {ma64Bytes}   64 - 1,
    {ma128Bytes}  128 - 1,
    {ma256Bytes}  256 - 1,
    {ma512Bytes}  512 - 1,
    {ma1024Bytes} 1024 - 1,
    {ma2048Bytes} 2048 - 1
  );
begin
  Value := V;

  Size := V.WSize;
  Align := V.Align;
  Allocated := V.Allocated;

  if (V.Allocated) then
  begin
    ValueEmpty := nil;
    if (V.Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then
      SystemError;

    if ((NativeInt(V) + SizeOf(THeaderMedium)) and MASK_MEDIUM_ALIGNS[Align] <> 0) then
      SystemError;
  end else
  begin
    ValueEmpty := Pointer(NativeUInt(V) + Size);
    if (V.Flags and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then
      SystemError;
  end;

  if (PHeaderMedium(NativeUInt(V) + SizeOf(THeaderMedium) + Size).PreviousSize <> Size) then
    SystemError;
end;

{ TPointerMediumInfo }

procedure TPointerMediumInfo.Init(const V: Pointer);
var
  RightHeader: PHeaderMedium;
begin
  Pool.Init(Pointer(NativeInt(V) and MASK_K64_CLEAR));
  inherited Init(PHeaderMedium(NativeInt(V) - SizeOf(THeaderMedium)));

  // left
  if (V = @Pool.Value.Items[Low(Pool.Value.Items)]) then
  begin
    FillChar(Left, SizeOf(Left), #0);
  end else
  begin
    Left.Init(PHeaderMedium(NativeUInt(Value) - Value.PreviousSize - SizeOf(THeaderMedium)));

    if (not Allocated) and (not Left.Allocated) then
      SystemError;
  end;

  // right
  RightHeader := PHeaderMedium(NativeUInt(Value) + Size + SizeOf(THeaderMedium));
  if (RightHeader = @Pool.Value.Items[High(Pool.Value.Items)]) then
  begin
    FillChar(Right, SizeOf(Right), #0);
  end else
  begin
    Right.Init(RightHeader);

    if (not Allocated) and (not Right.Allocated) then
      SystemError;
  end;

  // empties
  if (Self.ValueEmpty <> nil) then Pool.CheckEmpty(Self.ValueEmpty, MEDIUM_INDEXES[Self.ValueEmpty.Size shr 4]);
  if (Left.ValueEmpty <> nil) then Pool.CheckEmpty(Left.ValueEmpty, MEDIUM_INDEXES[Left.ValueEmpty.Size shr 4]);
  if (Right.ValueEmpty <> nil) then Pool.CheckEmpty(Right.ValueEmpty, MEDIUM_INDEXES[Right.ValueEmpty.Size shr 4]);
end;

{ TPointerBigInfo }

procedure TPointerBigInfo.Init(const V: Pointer);
begin

  // todo
end;

{ TPointerLargeInfo }

procedure TPointerLargeInfo.Init(const V: Pointer);
begin

  // todo
end;

{ TPointerInfo }

procedure TPointerInfo.Init(const V: Pointer);
const
  K = 1024;
  M = K * K;
var
  X: NativeInt;
  Options: TMemoryOptions;
begin
  Value := V;
  X := NativeInt(V);
  if (V = nil) or (X and 15 <> 0) then
    SystemError;

  if (not GetMemoryOptions(V, Options)) then
    SystemError;

  if (X and (256 * M - 1) = 0) then Align := align256M
  else
  if (X and (64 * M - 1) = 0) then Align := align64M
  else
  if (X and (16 * M - 1) = 0) then Align := align16M
  else
  if (X and (4 * M - 1) = 0) then Align := align4M
  else
  if (X and (1 * M - 1) = 0) then Align := align1M
  else
  if (X and (256 * K - 1) = 0) then Align := align256K
  else
  if (X and (64 * K - 1) = 0) then Align := align64K
  else
  if (X and (16 * K - 1) = 0) then Align := align16K
  else
  if (X and (4 * K - 1) = 0) then Align := align4K
  else
  if (X and (2 * K - 1) = 0) then Align := align2K
  else
  if (X and (1 * K - 1) = 0) then Align := align1K
  else
  if (X and (512 - 1) = 0) then Align := align512B
  else
  if (X and (256 - 1) = 0) then Align := align256B
  else
  if (X and (128 - 1) = 0) then Align := align128B
  else
  if (X and (64 - 1) = 0) then Align := align64B
  else
  if (X and (32 - 1) = 0) then Align := align32B
  else
  Align := align16B;

  // info
  AsSmall := nil;
  AsMedium := nil;
  AsBig := nil;
  AsLarge := nil;

  if (X and MASK_K4_TEST <> 0) then
  begin
    if (PK64PoolMedium(X and MASK_K64_CLEAR).MarkerNil <> nil) then
    begin
      AsSmall := Pointer(@_[0]);
      AsSmall.Init(V);
      Size := AsSmall.Line.Size;

      if (Options.Kind <> mkSmall) or (Options.Align <> ma16Bytes) then
        SystemError;
    end else
    begin
      AsMedium := Pointer(@_[0]);
      AsMedium.Init(V);
      Size := AsMedium.Size;

      if (Options.Kind <> mkMedium) or (Options.Align <> AsMedium.Align) then
        SystemError;
    end;

    if (Options.ThreadId <> MainThreadID) then
      SystemError;
  end else
  begin
    // big or large
    // todo
    AsLarge := Pointer(@_[0]);
    Size := PNativeUInt(X - SizeOf(NativeUInt))^ * SIZE_K4;

    if (Options.Kind <> mkLarge) or (Options.ThreadId <> 0) then
      SystemError;
  end;

  if (Options.Size <> NativeUInt(Size)) then
    SystemError;
end;

{ TJitHeapInfo }

procedure TJitHeapInfo.Init(const V: TJitHeap);
begin
  Value := V;

  // todo
end;


initialization
  System.ErrorProc := Pointer(@ErrorHandler);
  {$ifdef MSWINDOWS}
  System.ExceptObjProc := Pointer(@ExceptionHandler);
  {$endif}
  RUN_TESTS;

end.
