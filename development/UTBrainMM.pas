unit UTBrainMM;

{$i compiler_directives.inc}

  (*
     BrainMM library's unit test
  *)

interface
  uses BrainMM,
       {$ifdef UNITSCOPENAMES}System.Types{$else}Types{$endif},
       {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif}{$endif};

const
  BREAKPOINT = 459716;

var
  Done: Boolean = True;
  TEST_NUMBER: Integer = 1;

type
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
    EmptiesCount, EmptiesSize: Integer;
    AllocatedCount, AllocatedSize: Integer;

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
    EmptiesCount, EmptiesSize: Integer;
    AllocatedCount, AllocatedSize: Integer;

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

    EmptiesCount, EmptiesSize: Integer;
    AllocatedCount, AllocatedSize: Integer;

    procedure Init(const V: PK64PoolMedium);
    procedure CheckEmpty(const VEmpty: PHeaderMediumEmpty);
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
      EmptiesCount, EmptiesSize: Integer;
      AllocatedCount, AllocatedSize: Integer;
    end;
    PoolMediums: record
      Count: Integer;
      CountNonFull: Integer;
      CountFull: Integer;
      EmptiesCount, EmptiesSize: Integer;
      AllocatedCount, AllocatedSize: Integer;
    end;

    procedure Init(const V: PThreadHeap);
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

implementation


procedure INC_TEST;
begin
  Inc(TEST_NUMBER);

  // breakpoint
  if (TEST_NUMBER = BREAKPOINT) then
    TEST_NUMBER := TEST_NUMBER;
end;

const
  PTR_FAILURE = Pointer(High(NativeInt));

type
  TFormatBuffer = array[0..1024] of Char;

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

// make console?
function Log(const Text: PChar; ModeException: Boolean = False;
  CancelAvailable: Boolean = False): Boolean;
const
  DLG_MODES: array[Boolean] of Integer = (MB_ICONINFORMATION, MB_ICONERROR);
  DLG_CAPTIONS: array[Boolean] of PChar = ('Information:', 'Exception:');
  DLG_BUTTONS: array[Boolean] of Integer = (MB_OK, MB_OKCANCEL);
begin
  Result := (ID_OK = MessageBox(GetForegroundWindow, Text,
    DLG_CAPTIONS[ModeException], DLG_MODES[ModeException] or DLG_BUTTONS[CancelAvailable]));
end;

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

  TextBuffer := FormatBuffer('TEST_NUMBER: %d'#13'Exception %s at 0x%p',
    [TEST_NUMBER, Error, ErrorAddr]);

  // show exception message
  Log(TextBuffer, True);
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
  Info: TThreadHeapInfo;
begin
  ThreadHeap := ThreadHeapList;
  while (ThreadHeap <> nil) do
  begin
    Info.Init(ThreadHeap);

    Check(0, Info.PoolSmalls.Count);
    Check(0, Info.PoolMediums.Count);

    if (ThreadHeap.Deferreds.Assigned)
      then SystemError;

    ThreadHeap := ThreadHeap.FNextHeap;
  end;

  // todo
end;


procedure TestSizes;

  procedure CheckSize(const Size, Value: Integer);
  begin
    if (Size <> Value) then SystemError;
  end;
begin
  CheckSize(SizeOf(TBitSet8), 8);
  CheckSize(SizeOf(B16), 16);
  CheckSize(SizeOf(TK1LineSmall), 1024);
  CheckSize(SizeOf(TK64PoolSmall), 64 * 1024);
  CheckSize(SizeOf(THeaderMedium), 16);
  CheckSize(SizeOf(THeaderMediumEmpty), 16);
  CheckSize(SizeOf(TK64PoolMedium), 64 * 1024);
  CheckSize(SizeOf(TSyncStack64), 64);
  CheckSize(SizeOf(TThreadHeap), {$ifdef CPUX64}3{$else}2{$endif} * 64);
  CheckSize(SizeOf(TK4Page), 4 * 1024);
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
  Info: TPointerInfo;
begin
  INC_TEST;
  P := PTR_FAILURE;
  GetMem(P, 0);
  Check(nil, P);

  INC_TEST;
  P := PTR_FAILURE;
  GetMem(P, -1);
  Check(nil, P);

  INC_TEST;
  P := PTR_FAILURE;
  GetMem(P, -100500);
  Check(nil, P);

  // small
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    INC_TEST;
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
    INC_TEST;
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
    GetMem(P, (4 * 1024) - 6 * 16 - SizeOf(THeaderMedium));
    GetMem(P2, MAX_SMALL_SIZE + 1);
    if (NativeInt(P2) and MASK_K4_TEST = 0) then SystemError;
    Info.Init(P);
    if (Info.AsMedium = nil) or (Info.Size <> (4 * 1024) - 6 * 16) then SystemError;
    FreeMem(P);
    FreeMem(P2);
    CheckEmptyHeap;
  end;

  // todo big/large

  for i := Low(BASIC_BIG_SIZES) to High(BASIC_BIG_SIZES) do
  begin
    INC_TEST;
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
  Info: TPointerInfo;
begin
  INC_TEST;
  P := PTR_FAILURE;
  GetMemAligned(P, ma16Bytes, 0);
  Check(nil, P);

  INC_TEST;
  P := PTR_FAILURE;
  GetMemAligned(P, ma512Bytes,  -1);
  Check(nil, P);

  INC_TEST;
  P := PTR_FAILURE;
  GetMemAligned(P, ma1024Bytes, -100500);
  Check(nil, P);

  // small
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  for Align := Low(TMemoryAlign) to High(TMemoryAlign) do
  begin
    INC_TEST;
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
    INC_TEST;
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
    INC_TEST;
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
  Info: TPointerInfo;
begin
  INC_TEST;
  P := AllocMem(0);
  Check(nil, P);

  INC_TEST;
  {$if (not Defined(PUREPASCAL)) and (not Defined(BRAINMM_NOREDIRECT))}
  P := AllocMem(-1);
  Check(nil, P);
  {$ifend}

  INC_TEST;
  {$if (not Defined(PUREPASCAL)) and (not Defined(BRAINMM_NOREDIRECT))}
  P := AllocMem(-100500);
  Check(nil, P);
  {$ifend}

  // small(zero)
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    INC_TEST;
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
  ThreadHeap: PThreadHeap;

  SmallPtrs: array[Low(BASIC_SMALL_SIZES)..High(BASIC_SMALL_SIZES)] of Pointer;
  MediumPtrs: array[Low(BASIC_MEDIUM_SIZES)..High(BASIC_MEDIUM_SIZES)] of Pointer;
  i, Size: Integer;
begin
  INC_TEST;
  P := nil;
  FreeMem(P);
  CheckEmptyHeap;

  INC_TEST;
  ThreadHeap := CurrentThreadHeap;
  if (ThreadHeap = nil) then SystemError;

  // small
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    INC_TEST;
    Size := BASIC_SMALL_SIZES[i];
    GetMem(SmallPtrs[i], Size);
  end;

  // medium
  for i := Low(BASIC_MEDIUM_SIZES) to High(BASIC_MEDIUM_SIZES) do
  begin
    INC_TEST;
    Size := BASIC_MEDIUM_SIZES[i];
    GetMem(MediumPtrs[i], Size);
  end;

  // big
  INC_TEST;
  Size := BASIC_BIG_SIZES[Low(BASIC_BIG_SIZES)];
  GetMem(P, Size);

  // small deferred
  for i := Low(BASIC_SMALL_SIZES) to High(BASIC_SMALL_SIZES) do
  begin
    INC_TEST;
    ThreadHeap.PushThreadDeferred(SmallPtrs[i], @TestFreeMem, True);
  end;

  // medium deferred
  for i := Low(BASIC_MEDIUM_SIZES) to High(BASIC_MEDIUM_SIZES) do
  begin
    INC_TEST;
    ThreadHeap.PushThreadDeferred(MediumPtrs[i], @TestFreeMem, False);
  end;

  // each free
  INC_TEST;
  FreeMem(P);
  CheckEmptyHeap;
end;

procedure TestSmall;
var
  i: Integer;
  ThreadHeap: PThreadHeap;
  HeapInfo: TThreadHeapInfo;

  P: Pointer;
  PTRS_16_FIRST: array[3..63] of Pointer;
  PTRS_16_SECOND: array[1..63] of Pointer;
  PTRS_128: array[1..7] of Pointer;
  PTRS_128_MANY: array[0..7 * 64 - 1] of Pointer;

  Index, Size, j: Integer;
  Info: TPointerInfo;
  PTRS: array[0..2000 - 1] of Pointer;
begin
  INC_TEST;
  ThreadHeap := CurrentThreadHeap;
  if (ThreadHeap = nil) then SystemError;

  // first line
  for i := Low(PTRS_16_FIRST) to High(PTRS_16_FIRST) do
  begin
    INC_TEST;
    GetMem(PTRS_16_FIRST[i], 16);
    if ((NativeInt(PTRS_16_FIRST[i]) shr 4) and 63 <> i) then
      SystemError;
  end;

  INC_TEST;
  HeapInfo.Init(ThreadHeap);
  Check(1, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableFull);

  INC_TEST;
  i := Low(PTRS_16_SECOND);
  GetMem(PTRS_16_SECOND[i], 16);
  HeapInfo.Init(ThreadHeap);
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[1].FullQueue);

  // second line
  for i := Low(PTRS_16_SECOND) + 1 to High(PTRS_16_SECOND) do
  begin
    INC_TEST;
    GetMem(PTRS_16_SECOND[i], 16);
    if ((NativeInt(PTRS_16_SECOND[i]) shr 4) and 63 <> i) then
      SystemError;
  end;

  INC_TEST;
  HeapInfo.Init(ThreadHeap);
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[1].FullQueue);

  // free: available full
  INC_TEST;
  FreeMem(PTRS_16_SECOND[Low(PTRS_16_SECOND)]);
  HeapInfo.Init(ThreadHeap);
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(1, HeapInfo.K1LineSmalls[1].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[1].FullQueue);

  // free: full queue
  INC_TEST;
  FreeMem(PTRS_16_FIRST[Low(PTRS_16_FIRST)]);
  HeapInfo.Init(ThreadHeap);
  Check(2, HeapInfo.K1LineSmalls[1].Count);
  Check(2, HeapInfo.K1LineSmalls[1].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[1].AvailableFull);
  Check(0, HeapInfo.K1LineSmalls[1].FullQueue);

  // free: 2 lines
  INC_TEST;
  for i := Low(PTRS_16_FIRST) + 1 to High(PTRS_16_FIRST) do
    FreeMem(PTRS_16_FIRST[i]);
  for i := Low(PTRS_16_SECOND) + 1 to High(PTRS_16_SECOND) do
    FreeMem(PTRS_16_SECOND[i]);
  CheckEmptyHeap;

  // another size: 128 bytes
  for i := Low(PTRS_128) to High(PTRS_128) do
  begin
    INC_TEST;
    GetMem(PTRS_128[i], 128);
    if ((NativeInt(PTRS_128[i]) shr 7) and 7 <> i) then
      SystemError;
  end;

  INC_TEST;
  HeapInfo.Init(ThreadHeap);
  Check(1, HeapInfo.K1LineSmalls[8].Count);
  Check(1, HeapInfo.K1LineSmalls[8].AvailableFull);

  INC_TEST;
  GetMem(P, 128);
  HeapInfo.Init(ThreadHeap);
  Check(2, HeapInfo.K1LineSmalls[8].Count);
  Check(1, HeapInfo.K1LineSmalls[8].AvailableNonFull);
  Check(0, HeapInfo.K1LineSmalls[8].AvailableFull);
  Check(1, HeapInfo.K1LineSmalls[8].FullQueue);

  // free 128
  INC_TEST;
  FreeMem(P);
  for i := Low(PTRS_128) to High(PTRS_128) do FreeMem(PTRS_128[i]);
  CheckEmptyHeap;

  // small pool check (128)
  begin
    for i := Low(PTRS_128_MANY) to High(PTRS_128_MANY) do
    begin
      INC_TEST;
      GetMem(PTRS_128_MANY[i], 128);
    end;
    HeapInfo.Init(ThreadHeap);
    Check(1, HeapInfo.PoolSmalls.Count);
    Check(1, HeapInfo.PoolSmalls.AvailableFull);
    Check(64, HeapInfo.K1LineSmalls[8].Count);
    Check(1, HeapInfo.K1LineSmalls[8].AvailableFull);
    Check(63, HeapInfo.K1LineSmalls[8].FullQueue);

    INC_TEST;
    GetMem(P, 128);
    HeapInfo.Init(ThreadHeap);
    Check(2, HeapInfo.PoolSmalls.Count);
    Check(1, HeapInfo.PoolSmalls.AvailableNonFull);
    Check(1, HeapInfo.PoolSmalls.FullQueue);
    Check(64 + 1, HeapInfo.K1LineSmalls[8].Count);
    Check(1, HeapInfo.K1LineSmalls[8].AvailableNonFull);
    Check(64, HeapInfo.K1LineSmalls[8].FullQueue);

    INC_TEST;
    FreeMem(P);
    for i := Low(PTRS_128_MANY) to High(PTRS_128_MANY) do FreeMem(PTRS_128_MANY[i]);
    CheckEmptyHeap;
  end;

  // allocate random
  for i := Low(PTRS) to High(PTRS) do
  begin
    INC_TEST;

    Index := Random(8);
    Size := (Index + 1) shl 4;
    GetMem(P, Size);
    PTRS[i] := P;

    Index := Index + (i and (255 and -8));
    FillChar(P^, Size, Byte(Index));
  end;

  // try heap analize
  INC_TEST;
  HeapInfo.Init(ThreadHeap);

  // mixup
  MixPointers(PTRS);

  // free random
  for i := Low(PTRS) to High(PTRS) do
  begin
    INC_TEST;
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
  INC_TEST;
  CheckEmptyHeap;
end;

procedure TestMedium;
var
  ThreadHeap: PThreadHeap;
  HeapInfo: TThreadHeapInfo;

  P1, P2, P3, P4, P5, P6, P7, P8, P9: Pointer;
  Info: TPointerInfo;

  Size, i, j: Integer;
  Align: TMemoryAlign;
  PTRS: array[0..2000 - 1] of Pointer;
begin
  // push back
  INC_TEST;
  ThreadHeap := CurrentThreadHeap;
  if (ThreadHeap = nil) then SystemError;

  INC_TEST;
  GetMemAligned(P1, ma32Bytes, 16);
  Info.Init(P1);
  Check(@Info.AsMedium.Pool.Value.Items[0], P1);

  INC_TEST;
  GetMemAligned(P2, ma32Bytes, 16);
  Info.Init(P2);
  Check(@Info.AsMedium.Pool.Value.Items[2], P2);

  INC_TEST;
  GetMemAligned(P3, ma2048Bytes, MAX_MEDIUM_SIZE - 10 * 16);
  Info.Init(P3);
  if (Info.Align < align2K) then SystemError;
  Check(@Info.AsMedium.Pool.Value.Items[$80 - 6{122}], P3);

  INC_TEST;
  GetMemAligned(P4, ma512Bytes, 28 * 1024 + 26 * 16);
  Info.Init(P4);
  if (Info.Align < align512B) then SystemError;
  Check(@Info.AsMedium.Pool.Value.Items[$880 - 6{2170}], P4);

  INC_TEST;
  GetMemAligned(P5, ma128Bytes, 117 * 16);
  Info.Init(P5);
  if (Info.Align < align128B) then SystemError;
  Check(@Info.AsMedium.Pool.Value.Items[$8 - 6], P5);

  INC_TEST;
  HeapInfo.Init(ThreadHeap);
  Check(2, HeapInfo.PoolMediums.Count);
  Check(2, HeapInfo.PoolMediums.CountNonFull);

  // push current
  INC_TEST;
  GetMemAligned(P6, ma16Bytes, MAX_SMALL_SIZE + 16);
  Info.Init(P6);
  Check(@Info.AsMedium.Pool.Value.Items[($8 - 6) + 117 + 1], P6);

  // single pool again
  INC_TEST;
  FreeMem(P5);
  FreeMem(P6);
  HeapInfo.Init(ThreadHeap);
  Check(1, HeapInfo.PoolMediums.Count);
  Check(1, HeapInfo.PoolMediums.CountNonFull);

  // alloc 116 --> 117
  INC_TEST;
  GetMemAligned(P7, ma16Bytes, 116 * 16);
  Info.Init(P7);
  Check(@Info.AsMedium.Pool.Value.Items[$A - 6], P7);
  Check(117 * 16, Info.Size);

  // push back (clear top)
  INC_TEST;
  GetMemAligned(P8, ma16Bytes, 100 * 16);
  Info.Init(P8);
  Check(@Info.AsMedium.Pool.Value.Items[$F9B - 6], P8);

  // alloc middle (full pool)
  INC_TEST;
  GetMemAligned(P9, ma16Bytes, 9 * 16);
  Info.Init(P9);
  Check(@Info.AsMedium.Pool.Value.Items[$876 - 6], P9);

  HeapInfo.Init(ThreadHeap);
  Check(1, HeapInfo.PoolMediums.Count);
  Check(0, HeapInfo.PoolMediums.CountNonFull);
  Check(1, HeapInfo.PoolMediums.CountFull);

  // make empty
  INC_TEST;
  Info.Init(P1);
  FreeMem(P1);
  Info.Init(P2);
  FreeMem(P2);
  Info.Init(P3);
  FreeMem(P3);
  Info.Init(P4);
  FreeMem(P4);
  Info.Init(P7);
  FreeMem(P7);
  Info.Init(P8);
  FreeMem(P8);
  HeapInfo.Init(ThreadHeap);
  Info.Init(P9);
  FreeMem(P9);
  CheckEmptyHeap;

  // left allocated grow
  INC_TEST;
  GetMemAligned(P1, ma32Bytes, 32);
  Info.Init(P1);
  Check(32, Info.Size);
  INC_TEST;
  GetMemAligned(P2, ma32Bytes, 32);
  Info.Init(P1);
  Check(32 + 16, Info.Size);

  FreeMem(P2);
  FreeMem(P1);
  CheckEmptyHeap;

  // left empty grow
  INC_TEST;
  GetMemAligned(P1, ma32Bytes, 16);
  GetMemAligned(P2, ma32Bytes, 16);
  GetMemAligned(P3, ma32Bytes, 16);
  FreeMem(P1);
  FreeMem(P2);
  HeapInfo.Init(ThreadHeap);
  Check(2, HeapInfo.PoolMediums.EmptiesCount);
  Check(1, HeapInfo.PoolMediums.AllocatedCount);
  FreeMem(P3);
  CheckEmptyHeap;

  // right empty grow
  INC_TEST;
  GetMemAligned(P1, ma32Bytes, 16);
  GetMemAligned(P2, ma32Bytes, 16);
  FreeMem(P2);
  HeapInfo.Init(ThreadHeap);
  Check(1, HeapInfo.PoolMediums.EmptiesCount);
  Check(1, HeapInfo.PoolMediums.AllocatedCount);
  FreeMem(P1);
  CheckEmptyHeap;

  // allocate random
  for i := Low(PTRS) to High(PTRS) do
  begin
    INC_TEST;

    repeat
      Align := TMemoryAlign(Random(Ord(High(TMemoryAlign)) + 1));
      Size := Random(MAX_MEDIUM_SIZE) + 1;
    until (Align <> ma16Bytes) or (Size > MAX_SMALL_SIZE);

    GetMemAligned(P1, Align, Size);
    PTRS[i] := P1;

    FillChar(P1^, Size, Byte(i));
    PInteger(P1)^ := Size;
  end;

  // try heap analize
  INC_TEST;
  HeapInfo.Init(ThreadHeap);

  // mixup
  MixPointers(PTRS);

  // free random
  for i := Low(PTRS) to High(PTRS) do
  begin
    INC_TEST;
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
  INC_TEST;
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

  // todo

  // done
  if (Done) then Log('Done.');
  Halt;
end;


{ TBitSetInfo }

procedure TBitSetInfo.Init(const V: TBitSet8);
var
  i: Integer;
begin
  INC_TEST;
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
  INC_TEST;
  Value := V;
  ValueIndex := Integer((NativeUInt(V) div 1024) and 63);
  if (V = nil) or (NativeInt(V) and MASK_K1_TEST <> 0) then
    SystemError;

  ValuePool := Pointer(NativeInt(V) and MASK_K64_CLEAR);
  if (ValuePool = nil) or (ValuePool.ThreadHeap = nil) or
    (Pointer(not ValuePool.ThreadHeap.FMarkerNotSelf) <> ValuePool.ThreadHeap) or
    (ValuePool.LineSet.V64 and (Int64(1) shl ValueIndex) <> 0) then
    SystemError;

  Size := V.ModeSize and $f0;
  Index := V.ModeSize and 15;
  if (Size <> (16 * (Index and 7 + 1))) then
    SystemError;

  ItemSet := V.ItemSet;
  InFullQueue := (ItemSet.V64 and 1 <> 0);
  if (InFullQueue) then
  begin
    ItemSet.V64 := 0;
  end else
  begin
    if (ItemSet.V64 and (not DEFAULT_BITSETS_SMALL[Index]) <> 0) then
      SystemError;
  end;
  Items.Init(ItemSet);

  EmptiesCount := 0;
  AllocatedCount := 0;
  for i := 0 to 63 do
  if (DEFAULT_BITSETS_SMALL[Index] and (Int64(1) shl i) <> 0) then
  begin
    if (Items.Bits[i]) then
    begin
      Inc(EmptiesCount);
    end else
    begin
      Inc(AllocatedCount);
    end;
  end;
  EmptiesSize := EmptiesCount * Size;
  AllocatedSize := AllocatedCount * Size;
end;

{ TK1LineInfo }

procedure TK1LineInfo.Init(const V: PK1LineSmall);
var
  Left, Right: PK1LineSmall;
begin
  inherited Init(V);

  if (InFullQueue) then
  begin
    Left := Pointer(V.FullPrevNext.Prev and MASK_K1_CLEAR);
    Right := Pointer(V.FullPrevNext.Next and MASK_K1_CLEAR);
  end else
  begin
    Left := nil;
    Right := ValuePool.ThreadHeap.FK1LineSmalls[(Self.Index and 7) + 1];
    repeat
      if (Right = nil) then
        SystemError;

      if (Right = V) then
        Break;

      Left := Right;
      Right := Pointer(Right.Next and MASK_K1_CLEAR);
    until (False);

    Right := Pointer(Right.Next and MASK_K1_CLEAR);
  end;

  if (Left = nil) then
  begin
    Prev := nil;
  end else
  begin
    Prev := Pointer(@_[0]);
    Prev.Init(Left);
  end;

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
  INC_TEST;
  Value := V;
  ValueThreadHeap := V.ThreadHeap;
  if (ValueThreadHeap = nil) or (Pointer(not ValueThreadHeap.FMarkerNotSelf) <> ValueThreadHeap) then
    SystemError;

  InFullQueue := (V.PrevNext.Prev and 1 <> 0);
  Lines.Init(V.LineSet);

  EmptiesCount := 0;
  EmptiesSize := 0;
  AllocatedCount := 0;
  AllocatedSize := 0;
  for i := Low(Lines.Bits) to High(Lines.Bits) do
  if (not Lines.Bits[i]) then
  begin
    LineInfo.Init(@V.Lines[i]);

    Inc(EmptiesCount, LineInfo.EmptiesCount);
    Inc(EmptiesSize, LineInfo.EmptiesSize);
    Inc(AllocatedCount, LineInfo.AllocatedCount);
    Inc(AllocatedSize, LineInfo.AllocatedSize);
  end;
end;

{ TPoolSmallInfo }

procedure TPoolSmallInfo.Init(const V: PK64PoolSmall);
var
  Left, Right: PK64PoolSmall;
begin
  inherited Init(V);

  Left := Pointer(V.PrevNext.Prev and MASK_64_CLEAR);
  Right := Pointer(V.PrevNext.Next and MASK_64_CLEAR);

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
  Header, Next: PHeaderMedium;
  Count: Integer;
  Empty: PHeaderMediumEmpty;
begin
  INC_TEST;
  Value := V;
  ValueThreadHeap := V.ThreadHeap;
  if (V.MarkerNil <> nil) or (ValueThreadHeap = nil) or
    (Pointer(not ValueThreadHeap.FMarkerNotSelf) <> ValueThreadHeap) then
    SystemError;

  EmptiesCount := 0;
  EmptiesSize := 0;
  AllocatedCount := 0;
  AllocatedSize := 0;

  Header := @Value.Items[Low(Value.Items)];
  if (Header.PreviousSize <> 0) then
    SystemError;

  if (not Value.Finish.Allocated) or (Value.Finish.B16Count <> 0) then
    SystemError;

  while (Header <> @Value.Finish) do
  begin
    if (Header.Allocated) then
    begin
      if (Header.Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then
        SystemError;

      Inc(AllocatedCount);
      Inc(AllocatedSize, Header.B16Count shl 4);
    end else
    begin
      if (Header.Flags and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then
        SystemError;

      Inc(EmptiesCount);
      Inc(EmptiesSize, Header.B16Count shl 4);
      CheckEmpty(PHeaderMediumEmpty(@PHeaderMediumList(Header)[Header.B16Count - 1]));
    end;

    Next := @PHeaderMediumList(Header)[Header.B16Count];
    if (Next.PreviousSize <> Header.B16Count shl 4) then
      SystemError;
    Header := Next;
  end;

  Count := 0;
  Empty := Value.Empties.First.Next;
  while (Empty <> @Value.Empties.Last) do
  begin
    Inc(Count);
    Empty := Empty.Next;
  end;

  if (Count <> EmptiesCount) then
    SystemError;
end;

procedure TPoolMediumCompactInfo.CheckEmpty(const VEmpty: PHeaderMediumEmpty);
var
  Found: Boolean;
  S: NativeUInt;
  Current, Next: PHeaderMediumEmpty;
begin
  INC_TEST;

  if (Value.Empties.First.Prev <> nil) then
    SystemError;
  if (Value.Empties.Last.Next <> nil) then
    SystemError;

  Found := False;
  Current := Value.Empties.First.Next;
  while (Current <> @Value.Empties.Last) do
  begin
    S := PHeaderMediumEmptyEx(Current).Size;
    if (S and MASK_MEDIUM_SIZE_TEST <> MASK_MEDIUM_SIZE_VALUE) then
      SystemError;

    if (PHeaderMedium(NativeUInt(Current) - S).Flags and MASK_MEDIUM_EMPTY_TEST
      <> MASK_MEDIUM_EMPTY_VALUE) then
      SystemError;

    if (Current = VEmpty) then
      Found := True;

    Next := Current.Next;
    if (Next.Prev <> Current) then
      SystemError;

    Current := Next;
  end;

  if (not Found) then
    SystemError;
end;

{ TPoolMediumInfo }

procedure TPoolMediumInfo.Init(const V: PK64PoolMedium);
var
  Left, Right: PK64PoolMedium;
begin
  inherited Init(V);

  Left := Pointer(V.PrevNext.Prev and MASK_64_CLEAR);
  Right := Pointer(V.PrevNext.Next and MASK_64_CLEAR);

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
  i, Index: Integer;
  Line: PK1LineSmall;
  LineCounts: PK1LineSmallCounts;
  LineCompactInfo: TK1LineCompactInfo;
  LineInfo: TK1LineInfo;
  PoolSmall: PK64PoolSmall;
  PoolSmallInfo: TPoolSmallInfo;
  PoolMedium: PK64PoolMedium;
  PoolMediumInfo: TPoolMediumInfo;
begin
  INC_TEST;
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

      Line := Pointer(Line.Next and MASK_K1_CLEAR);
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
    INC_TEST;

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

    Inc(Self.PoolSmalls.EmptiesCount, PoolSmallInfo.EmptiesCount);
    Inc(Self.PoolSmalls.EmptiesSize, PoolSmallInfo.EmptiesSize);
    Inc(Self.PoolSmalls.AllocatedCount, PoolSmallInfo.AllocatedCount);
    Inc(Self.PoolSmalls.AllocatedSize, PoolSmallInfo.AllocatedSize);

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

    Inc(Self.PoolSmalls.EmptiesCount, PoolSmallInfo.EmptiesCount);
    Inc(Self.PoolSmalls.EmptiesSize, PoolSmallInfo.EmptiesSize);
    Inc(Self.PoolSmalls.AllocatedCount, PoolSmallInfo.AllocatedCount);
    Inc(Self.PoolSmalls.AllocatedSize, PoolSmallInfo.AllocatedSize);

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

    if (EmptiesSize + AllocatedSize > Count * SizeOf(TK64PoolSmall)) then
      SystemError;
  end;

  PoolMedium := V.QK64PoolMedium;
  while (PoolMedium <> nil) do
  begin
    PoolMediumInfo.Init(PoolMedium);

    Inc(Self.PoolMediums.Count);
    if (PoolMediumInfo.EmptiesSize = 0) then
    begin
      Inc(Self.PoolMediums.CountFull);
    end else
    begin
      Inc(Self.PoolMediums.CountNonFull);
    end;

    Inc(Self.PoolMediums.EmptiesCount, PoolMediumInfo.EmptiesCount);
    Inc(Self.PoolMediums.EmptiesSize, PoolMediumInfo.EmptiesSize);
    Inc(Self.PoolMediums.AllocatedCount, PoolMediumInfo.AllocatedCount);
    Inc(Self.PoolMediums.AllocatedSize, PoolMediumInfo.AllocatedSize);

    if (PoolMediumInfo.Next = nil) then
    begin
      PoolMedium := nil;
    end else
    begin
      PoolMedium := PoolMediumInfo.Next.Value;
    end;
  end;

  with Self.PoolMediums do
  if (EmptiesSize + AllocatedSize > Count * SizeOf(THeaderMediumList)) then
    SystemError;
end;

{ TPointerSmallInfo }

procedure TPointerSmallInfo.Init(const V: Pointer);
begin
  INC_TEST;
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
  INC_TEST;
  Value := V;

  Size := V.B16Count * 16;
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
    ValueEmpty := Pointer(@PHeaderMediumList(V)[Size shr 4 - 1]);
    if (V.Flags and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then
      SystemError;
  end;

  if (PHeaderMediumList(V)[Size shr 4].PreviousSize <> Size) then
    SystemError;
end;

{ TPointerMediumInfo }

procedure TPointerMediumInfo.Init(const V: Pointer);
var
  RightHeader: PHeaderMedium;
begin
  INC_TEST;
  Pool.Init(Pointer(NativeInt(V) and MASK_K64_CLEAR));
  inherited Init(PHeaderMedium(NativeInt(V) - SizeOf(THeaderMedium)));

  // left
  if (V = @Pool.Value.Items[0]) then
  begin
    FillChar(Left, SizeOf(Left), #0);
  end else
  begin
    Left.Init(PHeaderMedium(NativeUInt(Value) - Value.PreviousSize - SizeOf(THeaderMedium)));

    if (not Allocated) and (not Left.Allocated) then
      SystemError;
  end;

  // right
  RightHeader := @PHeaderMediumList(Value)[Size shr 4];
  if (RightHeader = @Pool.Value.Finish) then
  begin
    FillChar(Right, SizeOf(Right), #0);
  end else
  begin
    Right.Init(RightHeader);

    if (not Allocated) and (not Right.Allocated) then
      SystemError;
  end;

  // empties
  if (Self.ValueEmpty <> nil) then Pool.CheckEmpty(Self.ValueEmpty);
  if (Left.ValueEmpty <> nil) then Pool.CheckEmpty(Left.ValueEmpty);
  if (Right.ValueEmpty <> nil) then Pool.CheckEmpty(Right.ValueEmpty);
end;

{ TPointerBigInfo }

procedure TPointerBigInfo.Init(const V: Pointer);
begin
  INC_TEST;

  // todo
end;

{ TPointerLargeInfo }

procedure TPointerLargeInfo.Init(const V: Pointer);
begin
  INC_TEST;

  // todo
end;

{ TPointerInfo }

procedure TPointerInfo.Init(const V: Pointer);
const
  K = 1024;
  M = K * K;
var
  X: NativeInt;
begin
  INC_TEST;
  Value := V;
  X := NativeInt(V);
  if (V = nil) or (X and 15 <> 0) then
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
    end else
    begin
      AsMedium := Pointer(@_[0]);
      AsMedium.Init(V);
      Size := AsMedium.Size;
    end;
  end else
  begin
    // big or large
    // todo
    Size := 0;
  end;

  // todo
end;

{ TJitHeapInfo }

procedure TJitHeapInfo.Init(const V: TJitHeap);
begin
  INC_TEST;
  Value := V;

  // todo
end;


initialization
  System.ErrorProc := Pointer(@ErrorHandler);
  System.ExceptObjProc := Pointer(@ExceptionHandler);
  RUN_TESTS;

end.
