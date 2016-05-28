unit BrainMM;

{******************************************************************************}
{ Copyright (c) 2016 Dmitry Mozulyov                                           }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    }
{ THE SOFTWARE.                                                                }
{                                                                              }
{ email: softforyou@inbox.ru                                                   }
{ skype: dimandevil                                                            }
{ repository: https://github.com/d-mozulyov/BrainMM                            }
{******************************************************************************}


// compiler directives
{$ifdef FPC}
  {$mode delphi}
  {$asmmode intel}
  {$define INLINESUPPORT}
  {$define INLINESUPPORTSIMPLE}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}
{$else}
  {$ifdef CONDITIONALEXPRESSIONS}
    {$if CompilerVersion >= 24}
      {$LEGACYIFEND ON}
    {$ifend}
    {$if CompilerVersion >= 15}
      {$WARN UNSAFE_CODE OFF}
      {$WARN UNSAFE_TYPE OFF}
      {$WARN UNSAFE_CAST OFF}
    {$ifend}
    {$if CompilerVersion >= 20}
      {$define INLINESUPPORT}
    {$ifend}
    {$if CompilerVersion >= 17}
      {$define INLINESUPPORTSIMPLE}
    {$ifend}
    {$if CompilerVersion >= 18}
      {$define MEMORYMANAGEREX}
    {$ifend}
    {$if CompilerVersion < 23}
      {$define CPUX86}
    {$ifend}
    {$if CompilerVersion >= 23}
      {$define UNITSCOPENAMES}
      {$define RETURNADDRESS}
    {$ifend}
    {$if CompilerVersion >= 21}
      {$WEAKLINKRTTI ON}
      {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
    {$ifend}
    {$if (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
      {$define INTERNALCODEPAGE}
    {$ifend}
  {$else}
    {$define CPUX86}
  {$endif}
{$endif}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}
{$ifdef CONDITIONALEXPRESSIONS}{$A4}{$else}{$A+}{$endif}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$ifdef CONDITIONALEXPRESSIONS}
  {$if Defined(CPUX86) or Defined(CPUX64)}
    {$define CPUINTEL}
  {$ifend}
  {$if Defined(CPUX64) or Defined(CPUARM64)}
    {$define LARGEINT}
  {$ifend}
  {$if (not Defined(CPUX64)) and (not Defined(CPUARM64))}
    {$define SMALLINT}
  {$ifend}
  {$if Defined(FPC) or (CompilerVersion >= 18)}
    {$define OPERATORSUPPORT}
  {$ifend}
  {$ifdef MSWINDOWS}
    {$SETPEFLAGS $20}
  {$endif}
{$else}
  {$define CPUINTEL}
  {$define SMALLINT}
  {$define MSWINDOWS}
{$endif}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}

{$ifNdef MSWINDOWS}
  {$define PUREPASCAL}
{$endif}

{$ifNdef BRAINMM_NOREDIRECT}
  {$ifNdef PUREPASCAL}
    {$ifdef CONDITIONALEXPRESSIONS}
      {$if Defined(CPUINTEL) and not Defined(FPC)}
        {$define BRAINMM_REDIRECT}
      {$ifend}
    {$else}
      {$define BRAINMM_REDIRECT}
    {$endif}
  {$endif}
{$endif}

interface
  uses {$ifdef CONDITIONALEXPRESSIONS}
         {$ifdef UNITSCOPENAMES}System.Types{$else}Types{$endif},
         {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif}{$endif}
         {$ifdef POSIX}Posix.Base, Posix.String_, Posix.Unistd, Posix.SysTypes, Posix.PThread{$endif};
       {$else}
         Windows;
       {$endif}

type
  // standard types
  {$ifdef CONDITIONALEXPRESSIONS}
    {$ifdef FPC}
      PUInt64 = ^UInt64;
    {$else}
      {$if CompilerVersion < 16}
        UInt64 = Int64;
        PUInt64 = ^UInt64;
      {$ifend}
      {$if CompilerVersion < 21}
        NativeInt = Integer;
        NativeUInt = Cardinal;
      {$ifend}
      {$if CompilerVersion < 22}
        PNativeInt = ^NativeInt;
        PNativeUInt = ^NativeUInt;
      {$ifend}
    {$endif}
  {$else}
     UInt64 = Int64;
     PUInt64 = ^UInt64;
     NativeInt = Integer;
     NativeUInt = Cardinal;
     PNativeInt = ^NativeInt;
     PNativeUInt = ^NativeUInt;
  {$endif}


  (* Extended memory API *)

  TMemoryAlign = (ma16Bytes, ma32Bytes, ma64Bytes, ma128Bytes, ma256Bytes,
    ma512Bytes, ma1024Bytes, ma2048Bytes);
  PMemoryAlign = ^TMemoryAlign;

  MemoryBlock = type Pointer;
  PMemoryBlock = ^MemoryBlock;

  TMemoryBlockSize = (BLOCK_4K, BLOCK_16K, BLOCK_64K, BLOCK_256K,
    BLOCK_1MB, BLOCK_4MB, BLOCK_16MB, BLOCK_64MB, BLOCK_256MB);
  PMemoryBlockSize = ^TMemoryBlockSize;

  MemoryPages = type Pointer;
  PMemoryPages = ^MemoryPages;

  TMemoryAccessRight = (marRead, marWrite, marExecute);
  PMemoryAccessRight = ^TMemoryAccessRight;
  TMemoryAccessRights = set of TMemoryAccessRight;
  PMemoryAccessRights = ^TMemoryAccessRights;

  // additional memory functions
  procedure GetMemAligned(var P: Pointer; Align: TMemoryAlign; Size: NativeInt);
  procedure RegetMem(var P: Pointer; NewSize: NativeInt);
  {$ifNdef MEMORYMANAGEREX}
  function AllocMem(Size: NativeInt): Pointer;
  {$endif}

  // block routine
  procedure GetMemoryBlock(var Block: MemoryBlock; BlockSize: TMemoryBlockSize);
  procedure FreeMemoryBlock(Block: MemoryBlock);

  // 4kb-pages routine
  procedure GetMemoryPages(var Pages: MemoryPages; Count: NativeInt);
  procedure RegetMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
  procedure ReallocMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
  procedure FreeMemoryPages(Pages: MemoryPages);

  // any 4kb-aligned memory
  procedure ChangeMemoryAccessRights(Pages: MemoryPages; Count: NativeInt; Rights: TMemoryAccessRights);

  // fast SSE-based 16-aligned memory move
  procedure MoveB16Aligned(const Source; var Dest; const B16Count: NativeInt); {$ifNdef CPUINTEL}inline;{$endif}


type

{ TJITHeap class }
{ Just-In-Time memory heap: READ | WRITE | EXECUTE }

  IJITHeap = interface
    procedure Clear;
    function GetMemory(Size: NativeInt): Pointer;
    procedure FreeMemory(P: Pointer);
    function SyncGetMemory(Size: NativeInt): Pointer;
    procedure SyncFreeMemory(P: Pointer);
  end;

  TJITHeap = class(TInterfacedObject, IJITHeap)
  protected
    FSpin: NativeUInt;
    FHeapBuffer: array[0..64 * {$ifdef LARGEINT}3{$else}2{$endif} - 1 + 64] of Byte;
    FBigOrLargeHash: array[0..63] of Pointer;

    function HeapInstance: Pointer; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure EnqueueBigOrLarge(Pages: MemoryPages);
    function DequeueBigOrLarge(Pages: MemoryPages): Boolean;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create;
    procedure Clear;

    // memory management
    function GetMemory(Size: NativeInt): Pointer;
    procedure FreeMemory(P: Pointer);

    // synchronization (spin lock) + memory management
    function SyncGetMemory(Size: NativeInt): Pointer;
    procedure SyncFreeMemory(P: Pointer);
  end;

{$ifNdef BRAINMM_UNITTEST}
implementation
{$endif}


 (* Header types and contants *)

const
  SIZE_16 = 16;
  MASK_16_CLEAR = -SIZE_16;
  MASK_16_TEST = SIZE_16 - 1;
  SIZE_64 = 64;
  MASK_64_CLEAR = -SIZE_64;
  MASK_64_TEST = SIZE_64 - 1;
  SIZE_K1 = 1024;
  MASK_K1_CLEAR = -SIZE_K1;
  MASK_K1_TEST = SIZE_K1 - 1;
  SIZE_K4 = 4 * 1024;
  MASK_K4_CLEAR = -SIZE_K4;
  MASK_K4_TEST = SIZE_K4 - 1;
  SIZE_K16 = 16 * 1024;
  MASK_K16_CLEAR = -SIZE_K16;
  MASK_K16_TEST = SIZE_K16 - 1;
  SIZE_K32 = 32 * 1024;
  MASK_K32_CLEAR = -SIZE_K32;
  MASK_K32_TEST = SIZE_K32 - 1;
  SIZE_K64 = 64 * 1024;
  MASK_K64_CLEAR = -SIZE_K64;
  MASK_K64_TEST = SIZE_K64 - 1;

  B16_PER_PAGE = SIZE_K4 div SIZE_16;
  B16_PER_PAGE_SHIFT = 12 - 4;

  MAX_SMALL_SIZE = 128;
  MAX_SMALL_B16COUNT = MAX_SMALL_SIZE div 16;
  MAX_MEDIUM_SIZE = 8 * (4 * 1024) - 16;
  MAX_MEDIUM_B16COUNT = MAX_MEDIUM_SIZE div 16;
  MAX_BIG_SIZE = 8 * (64 * 1024);
  MAX_BIG_B16COUNT = MAX_BIG_SIZE div 16;

  PAGESMODE_USER = 0;
  PAGESMODE_SYSTEM = 1;
  PAGESMODE_JIT = 2;

  THREAD_HEAP_LOCKABLE = 2;
  THREAD_HEAP_LOCKED_BIT = 1;
  THREAD_HEAP_LOCKED = THREAD_HEAP_LOCKABLE or THREAD_HEAP_LOCKED_BIT;

  PTR_INVALID = Pointer(1);
  FREEMEM_INVALID = Integer({$ifdef FPC}0{$else}-1{$endif});
  FREEMEM_DONE = Integer({$ifdef FPC}-1{$else}0{$endif});
  JITHEAP_MARKER = {PThreadHeap}Pointer(NativeInt(-1));

  {$ifdef LARGEINT}
    HIGH_NATIVE_BIT = 63;
  {$else}
    HIGH_NATIVE_BIT = 31;
  {$endif}
  MASK_HIGH_NATIVE_BIT = NativeInt(1) shl HIGH_NATIVE_BIT;
  MASK_HIGH_NATIVE_TEST = NativeInt(-1) shr 1;

  DEFAULT_BITSETS_SMALL: array[0..15] of Int64 = (
    {16}   Int64($FFFFFFFFFFFFFFFC),
          {32}   $5555555555555554,
          {48}   $2492492492492490,
          {64}   $1111111111111110,
          {80}   $0842108421084210,
          {96}   $0410410410410410,
          {112}  $0204081020408100,
          {128}  $0101010101010100,
    {16-}  Int64($FFFFFFFFFFFFFFF0),
          {32-}  $5555555555555550,
          {48-}  $2492492492492490,
          {64-}  $1111111111111110,
          {80-}  $0842108421084210,
          {96-}  $0410410410410410,
          {112-} $0204081020408100,
          {128-} $0101010101010100
  );

  FIRST_BITSETS_SMALL: array[0..15] of Cardinal = (
  (*
      BitMask: Word;
      PtrOffset: Word(Byte);
  *)
      $00200004,
      $00200004,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00800100,
      $00800100,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00800100,
      $00800100
  );

  OFFSET_MEDIUM_ALIGN = 16;
  MASK_MEDIUM_ALIGN = {7}NativeUInt(High(TMemoryAlign)) shl OFFSET_MEDIUM_ALIGN;
  MASK_MEDIUM_ALLOCATED = NativeUInt(1 shl 24);

  MASK_MEDIUM_SIZE_TEST = not NativeUInt(High(Word) and -16);
  MASK_MEDIUM_SIZE_VALUE = 0;
  MASK_MEDIUM_EMPTY_TEST = not NativeUInt(High(Word) div 16);
  MASK_MEDIUM_EMPTY_VALUE = 0;
  MASK_MEDIUM_ALLOCATED_TEST = (not NativeUInt(MAX_MEDIUM_B16COUNT or MASK_MEDIUM_ALIGN)) or MASK_MEDIUM_ALLOCATED;
  MASK_MEDIUM_ALLOCATED_VALUE = MASK_MEDIUM_ALLOCATED;
  MASK_MEDIUM_TEST = MASK_MEDIUM_EMPTY_TEST xor MASK_MEDIUM_ALLOCATED xor MASK_MEDIUM_ALIGN;
  MASK_MEDIUM_VALUE = 0;

type
  PThreadHeap = ^TThreadHeap;
  SupposedPtr = type NativeUInt;
  PSupposedPtr = ^SupposedPtr;

  TBitSet8 = packed record
  case Integer of
    0: (V64: Int64);
    1: (VLow32, VHigh32: Integer);
    2: (VIntegers: array[0..1] of Integer);
    3: (VBytes: array[0..7] of Byte);
  end;
  PBitSet8 = ^TBitSet8;

  B16 = array[0..15] of Byte;
  P16 = ^B16;

  TQueuePrevNext = packed record
    Prev: Pointer;
    Next: Pointer;
    {$ifdef SMALLINT}_Padding: array[0..1] of Cardinal;{$endif}
  end;
  PQueuePrevNext = ^TQueuePrevNext;

  TK1LineSmallHeader = packed record
    ItemSet: TBitSet8;
    (*
       FullQueue: Boolean:1;
       Reserved: Boolean:1;
    *)
    case Integer of
      0: (_Padding: Int64; Queue: TQueuePrevNext);
      1: (ModeSize{+0..15}: Byte; Reserved: Word; {FirstLine}InQK64PoolSmallFull: Boolean);
      2: (Flags: NativeUInt);
      (*
         Index: 0..15:3+1{FirstLine};
         ModeSizeBits: Byte:4;
         Reserved: Word;
         InQK64PoolSmallFull: Boolean;
      *)
  end;
  PK1LineSmallHeader = ^TK1LineSmallHeader;

  TK1LineSmall = packed record
  case Integer of
    0: (Header: TK1LineSmallHeader);
    1: (Items: array[0{2}..63] of B16);
  end;
  PK1LineSmall = ^TK1LineSmall;

  TK64PoolSmall = packed record
  case Boolean of
    True: (
             Header: TK1LineSmallHeader;
             ThreadHeap: PThreadHeap;
             {$ifdef SMALLINT}_Padding1: Cardinal;{$endif}
             LineSet: TBitSet8;
             Queue: TQueuePrevNext;
          );
   False: (Lines: array[0..63] of TK1LineSmall);
  end;
  PK64PoolSmall = ^TK64PoolSmall;

  THeaderMedium = packed record
    PreviousSize: NativeUInt;
    {$ifdef SMALLINT}_Padding: array[0..1] of Cardinal;{$endif}
    case Integer of
     0: (B16Count: Word; Align: TMemoryAlign; Allocated: Boolean);
     1: (Flags: NativeUInt);
  end;
  PHeaderMedium = ^THeaderMedium;

  PHeaderMediumEmpty = ^THeaderMediumEmpty;
  THeaderMediumEmpty = object
    Prev, Next: PHeaderMediumEmpty;
    {$ifdef SMALLINT}_Padding: array[0..1] of Cardinal;{$endif}
  end;

  THeaderMediumEmptyEx = object(THeaderMediumEmpty)
    Size: NativeUInt{Next.Header.PreviousSize};
  end;
  PHeaderMediumEmptyEx = ^THeaderMediumEmptyEx;

  THeaderMediumList = array[-1..(64 * 1024) div 16 - 8] of THeaderMedium;
  PHeaderMediumList = ^THeaderMediumList;

  TK64PoolMedium = packed record
    Empties: packed record
      First: THeaderMediumEmpty;
      Last: THeaderMediumEmpty;
    end;
    MarkerNil: Pointer;
    {$ifdef SMALLINT}_Padding2: Cardinal;{$endif}
    ThreadHeap: PThreadHeap;
    {$ifdef SMALLINT}_Padding3: Cardinal;{$endif}
    Queue: TQueuePrevNext;
    FakeAllocated: THeaderMedium;
    Items: {Start + }THeaderMediumList;
    Finish: THeaderMedium;
  end;
  PK64PoolMedium = ^TK64PoolMedium;

  TSyncStack = object
  private
    F: packed record
    case Integer of
      0: (Handle: SupposedPtr; x86RefCounter: NativeUInt);
      1: (Assigned4Bytes: LongBool);
    end;
    {$ifNdef SMALLINT}
    function GetAssigned: Boolean; inline;
    {$endif}
  public
    procedure Push(const Value: Pointer); {$ifdef INLINESUPPORT}inline;{$endif}
    function Pop: Pointer;

    procedure PushList(const First, Last: Pointer);
    function PopList: Pointer;

    {$ifdef SMALLINT}
    property Assigned: LongBool read F.Assigned4Bytes;
    {$else .LARGEINT}
    property Assigned: Boolean read GetAssigned;
    {$endif}
  end;
  PSyncStack = ^TSyncStack;

  TSyncStack64 = object(TSyncStack)
  protected
    _Padding: array[1..64 - SizeOf(TSyncStack)] of Byte;
  end;
  PSyncStack64 = ^TSyncStack64;

  TThreadHeap = {$ifdef OPERATORSUPPORT}packed record{$else}object{$endif}
  public
    FNextHeap: PThreadHeap;
    FK1LineSmalls: array[1..8] of PK1LineSmall;
    FMarkerNotSelf: SupposedPtr;
    
  public
    // 1kb-lines (small) + 64kb pool (small) routine
    QK1LineFull: PK1LineSmall;
    QK64PoolSmall: PK64PoolSmall;
    QK64PoolSmallFull: PK64PoolSmall;

    function NewK64PoolSmall: PK64PoolSmall;
    function DisposeK64PoolSmall(PoolSmall: PK64PoolSmall): Integer;
    function GetNewK1LineSmall(B16Count: NativeUInt): Pointer;
    function DisposeK1LineSmall(Line: PK1LineSmall): Integer;
    {$ifNdef PUREPASCAL}function PenaltyGrowSmallToSmall(P: Pointer; Dest: Pointer): Pointer;{$endif}
  public
    // 64kb pool (medium) routine
    QK64PoolMedium: PK64PoolMedium;
    FReserved: Pointer;

    function NewK64PoolMedium: PK64PoolMedium;
    function DisposeK64PoolMedium(PoolMedium: PK64PoolMedium): Integer;

    function PenaltyReduceMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;
    function PenaltyGetMedium(B16Count: NativeUInt; Align: NativeUInt{TMemoryAlign}): Pointer;
    function PenaltyFreeMedium(P: Pointer): Integer;
    function PenaltyRegetMediumToMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;
    function PenaltyReallocMediumToMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;
  public
    // errors
    ErrorAddr: Pointer {address/nil};

    function ErrorOutOfMemory: Pointer;
    function ErrorInvalidPtr: Integer;
    function RaiseOutOfMemory: Pointer;
    function RaiseInvalidPtr: Integer;
  public
    // thread deffered (free) memory pieces: small or medium
    Deferreds: TSyncStack;
    LockFlags: SupposedPtr;
    _Padding: array[1..64 - SizeOf(TSyncStack) - SizeOf(SupposedPtr)] of Byte;

    procedure PushThreadDeferred(P: Pointer; ReturnAddress: Pointer; IsSmall: Boolean);
    procedure ProcessThreadDeferred;
  public
    // most frequently used
    function GetSmall(B16Count: NativeUInt): Pointer;
    function FreeSmall(P: Pointer): Integer;
    function RegrowSmallToSmall(P: Pointer; NewB16Count: NativeUInt): Pointer;
    function GrowSmallToSmall(P: Pointer; NewB16Count: NativeUInt): Pointer;

    // medium routine
    function ReduceMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;
    function GetMedium(B16Count: NativeUInt; Align: NativeUInt{TMemoryAlign}): Pointer;
    function FreeMedium(P: Pointer): Integer;
    function RegetMediumToMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;
    function ReallocMediumToMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;

    // difficult routine
    function FreeDifficult(P: Pointer; ReturnAddress: Pointer): Integer;
    function RegetDifficult(P: Pointer; NewB16Count: NativeUInt; ReturnAddress: Pointer): Pointer;
    function ReallocDifficult(P: Pointer; NewB16Count: NativeUInt; ReturnAddress: Pointer): Pointer;
  end;


 (* Extended memory API *)

type
  TBrainMemoryManager = packed record
  case Integer of
    0: (
        BrainMM: packed record
          {$ifdef MSWINDOWS}
          ThreadFuncEvent: function(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
          {$else .POSIX}
          ThreadFuncEvent: function(Attribute: PThreadAttr; ThreadFunc: TThreadFunc; Parameter: Pointer; var ThreadId: NativeUInt): Integer;
          {$endif}
          EndThreadEvent: procedure(ExitCode: Integer);

          GetMemoryBlock: function(BlockSize: TMemoryBlockSize; PagesMode: NativeUInt): MemoryBlock;
          FreeMemoryBlock: function(Block: MemoryBlock; PagesMode: NativeUInt): Boolean;
          GetMemoryPages: function(Count: NativeUInt; PagesMode: NativeUInt): MemoryPages;
          RegetMemoryPages: function(Pages: MemoryPages; NewCount: NativeUInt; PagesMode: NativeUInt): MemoryPages;
          ReallocMemoryPages: function(Pages: MemoryPages; NewCount: NativeUInt; PagesMode: NativeUInt): MemoryPages;
          FreeMemoryPages: function(Pages: MemoryPages; PagesMode: NativeUInt): Boolean;
          Reserved: array[1..4] of Pointer;
          GetMemAligned: function(Align: TMemoryAlign; Size: NativeUInt): Pointer;
          RegetMem: function(P: Pointer; NewSize: NativeUInt): Pointer;
        end;
        Standard: packed record
          GetMem: function(Size: NativeUInt): Pointer;
          FreeMem: function(P: Pointer): Integer;
          ReallocMem: function(P: Pointer; NewSize: NativeUInt): Pointer;
          AllocMem: function(Size: NativeUInt): Pointer;
          RegisterExpectedMemoryLeak: function(P: Pointer): Boolean;
          UnregisterExpectedMemoryLeak: function(P: Pointer): Boolean;
          Reserved: array[1..4] of Pointer;
        end;
       );
    1: (POINTERS: array[1..24] of Pointer);
  end;

  TK4Page = array[0..4 * 1024 - 1] of Byte;
  PK4Page = TK4Page;

  PInternalRecord = ^TInternalRecord;
  TInternalRecord = packed record
    Next: PInternalRecord;
    case Integer of
      0: (SupposedStart: SupposedPtr;
          (*
             PagesMode: Byte:2;
             IsBlock: Boolean:1;
             IsLarge: Boolean:1;
             Reserved: Byte:8;
             Pages: Pointer:High(Pointer)-12;
          *)
          PagesCount: NativeUInt;
          case Integer of
            // Big
            0: (BigPool: PInternalRecord);
            // Large
            1: (LastPart: PInternalRecord);
            // Block
            // todo
          );
    // 1: BigPool
      2: (ThreadHeap: PThreadHeap);
  end;

  TPagesHashItem = packed record
    Spin: SupposedPtr;
    List: PInternalRecord;
  end;
  PPagesHashItem = ^TPagesHashItem;

  // 64 bytes aligned global storage
  TGlobalStorage = packed record
    PagesHash: array[0..1023] of TPagesHashItem;

    CoreThreadHeaps: TSyncStack64;
    CoreInternalRecords: TSyncStack64;
    DeferredThreadHeaps: TSyncStack64;

    K64BlockCache: packed record
      Stack: TSyncStack;
      Count: NativeUInt;
      _Padding: array[1..64 - SizeOf(TSyncStack) - SizeOf(NativeUInt)] of Byte;
    end;
  end;
  PGlobalStorage = ^TGlobalStorage;


var
  MemoryManager: TBrainMemoryManager;

  UnknownThreadHeap: PThreadHeap;
  MainThreadHeap: PThreadHeap;
  ThreadHeapList: PThreadHeap;
  GLOBAL_STORAGE: array[0..SizeOf(TGlobalStorage) + 63] of Byte;

{$ifdef BRAINMM_UNITTEST}
  function CurrentThreadHeap: PThreadHeap;
implementation
{$endif}


procedure GetMemAligned(var P: Pointer; Align: TMemoryAlign; Size: NativeInt);
var
  Value: Pointer;
begin
  if (Byte(Align) <= Byte(High(TMemoryAlign))) then
  begin
    if (Size > 0) then
    begin
      Value := MemoryManager.BrainMM.GetMemAligned(Align, NativeUInt(Size));
      P := Value;
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end else
    begin
      P := nil;
    end;
  end else
  begin
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  end;
end;

function RecallGetMem(None: Pointer; Size: NativeInt; var P: Pointer): Pointer;
begin
  if (Size > 0) then
  begin
    Result := MemoryManager.Standard.GetMem(NativeUInt(Size));
    if (Result = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    Result := nil;
  end;

  P := Result;
end;

function RecallFreeMem(Value: Pointer; None: NativeInt; var P: Pointer): {nil}Pointer;
begin
  if (Value <> nil) then
  begin
    if (MemoryManager.Standard.FreeMem(Value) {$ifdef FPC}={$else}<>{$endif} 0) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;

  P := nil;
  Result := nil;
end;

procedure RegetMem(var P: Pointer; NewSize: NativeInt);
var
  Value: Pointer;
begin
  Value := P;
  if (NewSize > 0) then
  begin
    if (Value <> nil) then
    begin
      Value := MemoryManager.BrainMM.RegetMem(Value, NativeUInt(NewSize));
      P := Value;
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end else
    begin
      RecallGetMem(nil, NewSize, P);
    end;
  end else
  begin
    RecallFreeMem(Value, 0, P);
  end;
end;

{$ifNdef MEMORYMANAGEREX}
function AllocMem(Size: NativeInt): Pointer;
begin
  if (Size > 0) then
  begin
    Result := MemoryManager.Standard.AllocMem(Size);
    if (Result = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    Result := nil;
  end;
end;
{$endif}

procedure GetMemoryBlock(var Block: MemoryBlock; BlockSize: TMemoryBlockSize);
var
  Value: MemoryBlock;
begin
  if (Byte(BlockSize) <= Byte(High(TMemoryBlockSize))) then
  begin
    Value := MemoryManager.BrainMM.GetMemoryBlock(BlockSize, PAGESMODE_USER);
    Block := Value;
    if (Value = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  end;
end;

procedure FreeMemoryBlock(Block: MemoryBlock);
begin
  if (Block <> nil) then
  begin
    if (not MemoryManager.BrainMM.FreeMemoryBlock(Block, PAGESMODE_USER)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;
end;

procedure GetMemoryPages(var Pages: MemoryPages; Count: NativeInt);
var
  Value: MemoryPages;
begin
  if (Count > 0) then
  begin
    Value := MemoryManager.BrainMM.GetMemoryPages(NativeUInt(Count), PAGESMODE_USER);
    Pages := Value;
    if (Value = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    Pages := nil;
  end;
end;

procedure RegetMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
var
  Value: Pointer;
begin
  Value := Pages;
  if (NewCount > 0) then
  begin
    if (Value <> nil) then
    begin
      Value := MemoryManager.BrainMM.RegetMemoryPages(Value, NativeUInt(NewCount), PAGESMODE_USER);
      if (Value = PTR_INVALID) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif}
      else
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif}
      else
        Pages := Value;
    end else
    begin
      GetMemoryPages(Pages, NewCount);
    end;
  end else
  begin
    Pages := nil;
    FreeMemoryPages(Value);
  end;
end;

procedure ReallocMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
var
  Value: Pointer;
begin
  Value := Pages;
  if (NewCount > 0) then
  begin
    if (Value <> nil) then
    begin
      Value := MemoryManager.BrainMM.ReallocMemoryPages(Value, NativeUInt(NewCount), PAGESMODE_USER);
      if (Value = PTR_INVALID) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif}
      else
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif}
      else
        Pages := Value;
    end else
    begin
      GetMemoryPages(Pages, NewCount);
    end;
  end else
  begin
    Pages := nil;
    FreeMemoryPages(Value);
  end;
end;

procedure FreeMemoryPages(Pages: MemoryPages);
begin
  if (Pages <> nil) then
  begin
    if (NativeInt(Pages) and (4 * 1024 - 1) <> 0) or
      (not MemoryManager.BrainMM.FreeMemoryPages(Pages, PAGESMODE_USER)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;
end;


 (* POSIX API *)

{$ifdef POSIX}
const
  PROT_READ       = $1;         { Page can be read.  }
  PROT_WRITE      = $2;         { Page can be written.  }
  PROT_EXEC       = $4;         { Page can be executed.  }
  PROT_NONE       = $0;         { Page can not be accessed.  }
  MAP_SHARED      = $01;        { Share changes.  }
  MAP_PRIVATE     = $02;        { Changes are private.  }
  MAP_TYPE        = $0F;        { Mask for type of mapping.  }
  MAP_FIXED       = $10;        { Interpret addr exactly.  }
  MAP_FILE        = $00;
  MAP_ANONYMOUS   = $20;        { Don't use a file.  }
  MAP_GROWSDOWN   = $0100;      { Stack-like segment.  }
  MAP_DENYWRITE   = $0800;      { ETXTBSY }
  MAP_EXECUTABLE  = $1000;      { Mark it as an executable.  }
  MAP_LOCKED      = $2000;      { Lock the mapping.  }
  MAP_NORESERVE   = $4000;      { Don't check for reservations.  }

  MAP_FAILED = Pointer(-1);

function memalign(alignment, size: NativeUInt): Pointer; cdecl;
  external libc name _PU + 'memalign';

procedure free(P: Pointer); cdecl;
  external libc name _PU + 'free';

function mmap(address: Pointer; length: NativeUInt; protect, flags, filedes: Integer;
  offset: NativeUInt): Pointer; cdecl;
  external libc name _PU + 'mmap';

function munmap(address: Pointer; length: NativeUInt): Integer; cdecl;
  external libc name _PU + 'munmap';

function mprotect(address: Pointer; length: NativeUInt; protect: Integer): Integer; cdecl;
  external libc name _PU + 'mprotect';

function mlock(address: Pointer; length: NativeUInt): Integer; cdecl;
  external libc name _PU + 'mlock';

function munlock(address: Pointer; length: NativeUInt): Integer; cdecl;
  external libc name _PU + 'munlock';

function mremap(address: Pointer; length, new_length: NativeUInt;
  flag: Integer): Pointer; cdecl;
  external libc name _PU + 'mremap';
{$endif}


 (* Helpfull routine *)

{$ifdef CONDITIONALEXPRESSIONS}
{$if Defined(FPC) or (CompilerVersion < 24)}
{$ifdef FPC}
function AtomicCmpExchange(var Target: SupposedPtr; NewValue: SupposedPtr; Comparand: SupposedPtr): SupposedPtr; inline;
begin
  Result := InterlockedCompareExchange(Target, NewValue, Comparand);
end;
function AtomicIncrement(var Target: NativeInt; Increment: NativeInt = 1): NativeInt; inline;
begin
  Result := InterlLockedExchangeAdd(Target, Increment);
end;
{$else}
function AtomicCmpExchange(var Target: SupposedPtr; NewValue: SupposedPtr; Comparand: SupposedPtr): SupposedPtr;
asm
  {$ifdef CPUX86}
    xchg eax, ecx
    lock cmpxchg [ecx], edx
  {$else .CPUX64}
    mov rax, r8
    lock cmpxchg [rcx], rdx
  {$endif}
end;
function AtomicIncrement(var Target: NativeInt; Increment: NativeInt = 1): NativeInt;
asm
  {$ifdef CPUX86}
    mov ecx, edx
    lock xadd [eax], edx
    lea eax, [edx + ecx]
  {$else .CPUX64}
    mov rax, rdx
    lock xadd [rcx], rdx
    add rax, rdx
  {$endif}
end;
{$endif}
{$ifend}
{$else}
function AtomicCmpExchange(var Target: SupposedPtr; NewValue: SupposedPtr; Comparand: SupposedPtr): SupposedPtr;
asm
  xchg eax, ecx
  lock cmpxchg [ecx], edx
end;
function AtomicIncrement(var Target: NativeInt; Increment: NativeInt = 1): NativeInt;
asm
  mov ecx, edx
  lock xadd [eax], edx
  lea eax, [edx + ecx]
end;
{$endif}

procedure ChangeMemoryAccessRights(Pages: MemoryPages; Count: NativeInt;
  Rights: TMemoryAccessRights);
const
  HIGH_ACCESS_RIGHT = Ord(High(TMemoryAccessRight));
{$ifdef MSWINDOWS}
  ACCESS_RIGHTS: array[0.. 1 shl (HIGH_ACCESS_RIGHT + 1) - 1] of Cardinal = (
  { Execute | Write | Read }
    {000} PAGE_NOACCESS,
    {001} PAGE_READONLY,
    {010} PAGE_WRITECOPY,
    {011} PAGE_READWRITE,
    {100} PAGE_EXECUTE,
    {101} PAGE_EXECUTE_READ,
    {110} PAGE_EXECUTE_WRITECOPY,
    {111} PAGE_EXECUTE_READWRITE
  );
var
  Protect: Cardinal;
{$endif}
begin
  if (Byte(Rights) < (1 shl (HIGH_ACCESS_RIGHT + 1))) then
  begin
    if (Pages <> nil) and (NativeInt(Pages) and MASK_K4_TEST = 0) then
    begin
      if (Count > 0) then
      begin
        {$ifdef MSWINDOWS}
          if (not VirtualProtect(Pages, SIZE_K4 * Count, ACCESS_RIGHTS[Byte(Rights)], Protect)) then
            {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
        {$else .POSIX}
          // Prot = Byte(Rights)
          {$MESSAGE 'ToDo'}
        {$endif}
      end;
    end else
    begin
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
    end;
  end else
  begin
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  end;
end;


{$ifdef MSWINDOWS}
var
  SwitchToThreadFunc: function: BOOL; stdcall;
{$endif}

procedure ThreadSleepLong;
begin
  {$ifdef MSWINDOWS}
    Sleep(0);
  {$else .POSIX}
    usleep(0 shl 10);
  {$endif}
end;

procedure ThreadSleep;
begin
  {$ifdef MSWINDOWS}
    Sleep(1);
  {$else .POSIX}
    usleep(1 shl 10);
  {$endif}
end;

procedure ThreadSwitch;
begin
  {$ifdef MSWINDOWS}
  if (Assigned(SwitchToThreadFunc)) then
  begin
    SwitchToThreadFunc;
    Exit;
  end;
  {$endif}

  ThreadSleep;
end;

procedure ThreadPause;
{$ifdef CPUINTEL}
asm
  DB $F3, $90 // pause
end;
{$else}
begin
  ThreadSwitch;
end;
{$endif}

function SpinWait(var Spin: SupposedPtr; const Mask: NativeUInt): SupposedPtr;
var
  K: NativeUInt;
begin
  K := NativeUInt(-1);
  repeat
    Result := Spin;
    if (Result and Mask = 0) then Exit;
    
    K := (K + 1) and 7;
    case (K) of
   3..5: ThreadSwitch;
      6: ThreadSleep;
      7: ThreadSleepLong;
    else
      ThreadPause;
    end;
  until (False);
end;

(* procedure SpinLock(var Spin: SupposedPtr);
begin
  repeat
    // wait locked
    if (Spin <> 0) then SpinWait(Spin, High(NativeUInt));

    // make locked
  until (0 = AtomicCmpExchange(Spin, 1, 0));
end;

procedure SpinUnlock(var Spin: SupposedPtr);
begin
  Spin := 0;
end; *)

const
  SPINEX_LOCK_INCREMENT = 1 shl 16;
  SPINEX_LOCK_MASK = NativeUInt(High(NativeUInt) shl 16);
  SPINEX_LOCK_OVERFLOW = 2 * SPINEX_LOCK_INCREMENT;
  SPINEX_ENTER_MASK = SPINEX_LOCK_INCREMENT - 1;

procedure SpinExLock(var SpinEx: SupposedPtr);
var
  Item: SupposedPtr;
begin
  Item := SpinEx;
  repeat
    // wait locked
    if (Item and SPINEX_LOCK_MASK <> 0) then SpinWait(SpinEx, SPINEX_LOCK_MASK);

    // make locked
    Item := AtomicIncrement(NativeInt(SpinEx), SPINEX_LOCK_INCREMENT);
    if (Item >= SPINEX_LOCK_OVERFLOW) then
    begin
      // retrieve lockeds, wait locked
      Item := AtomicIncrement(NativeInt(SpinEx), -SPINEX_LOCK_INCREMENT);
    end else
    begin
      // done
      Break;
    end;
  until (False);

  // wait entered (readonly)
  if (Item and SPINEX_ENTER_MASK <> 0) then SpinWait(SpinEx, SPINEX_ENTER_MASK);
end;

procedure SpinExUnlock(var SpinEx: SupposedPtr);
begin
  AtomicIncrement(NativeInt(SpinEx), -SPINEX_LOCK_INCREMENT);
end;

procedure SpinExReadEnter{Readonly}(var SpinEx: SupposedPtr);
var
  Item: SupposedPtr;
begin
  Item := SpinEx;
  repeat
    // wait locked
    if (Item and SPINEX_LOCK_MASK <> 0) then SpinWait(SpinEx, SPINEX_LOCK_MASK);

    // make entered
    Item := AtomicIncrement(NativeInt(SpinEx));
    if (Item and SPINEX_LOCK_MASK <> 0) then
    begin
      // retrieve counter, wait locked
      Item := AtomicIncrement(NativeInt(SpinEx), -1);
    end else
    begin
      // done
      Break;
    end;
  until (False);
end;

procedure SpinExReadLeave{Readonly}(var SpinEx: SupposedPtr);
begin
  AtomicIncrement(NativeInt(SpinEx), -1);
end;


{$ifdef CPUX86}
var
  SSE_SUPPORT: Integer{Boolean};

procedure CheckSSESupport;
asm
  push ebx
  mov eax, 1
  DB $0F, $A2 // cpuid
  test edx, 02000000h
  setnz byte ptr [SSE_SUPPORT]
  pop ebx
end;
{$endif}

{$ifdef CPUINTEL}
procedure BackwardSSEMove(const Source; var Dest; const B16Count: NativeInt);
const
  COUNT_8 = 8 * 8;
asm
  // x86: check SSE availability
  // eax/rax = Src + Size, edx/rdx = Dest + Size, ecx/rcx = -(8 * B16Count)
  {$ifdef CPUX86}
    cmp SSE_SUPPORT, 0
    jz @x86_non_SSE
    lea ecx, [ecx * 8]
    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    neg ecx
  {$else .CPUX64}
    lea r8, [r8 * 8]
    lea rax, [rcx + r8 * 2]
    lea rdx, [rdx + r8 * 2]
    xor rcx, rcx
    sub rcx, r8
  {$endif}

@move_next_16128:
  {$ifdef CPUX86}
    cmp ecx, -COUNT_8
    jle @move_128

    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    lea ecx, [ecx + @done]
    jmp ecx
  @move_128:
    lea eax, [eax - 128]
    lea edx, [edx - 128]
    lea ecx, [ecx + COUNT_8]
  {$else .CPUX64}
    cmp rcx, -COUNT_8
    jle @move_128

    lea rax, [rax + rcx * 2]
    lea rdx, [rdx + rcx * 2]
    lea r9, @done
    lea r9, [r9 + rcx]
    jmp r9
    nop
    nop
  @move_128:
    lea rax, [rax - 128]
    lea rdx, [rdx - 128]
    lea rcx, [rcx + COUNT_8]
  {$endif}

  {$ifdef CPUX86}
    DB $0F, $28, $78, $70 // movaps xmm7, [eax + 7*16]
    DB $0F, $29, $7A, $70 // movaps [edx + 7*16], xmm7
    DB $0F, $28, $70, $60 // movaps xmm6, [eax + 6*16]
    DB $0F, $29, $72, $60 // movaps [edx + 6*16], xmm6
    DB $0F, $28, $68, $50 // movaps xmm5, [eax + 5*16]
    DB $0F, $29, $6A, $50 // movaps [edx + 5*16], xmm5
    DB $0F, $28, $60, $40 // movaps xmm4, [eax + 4*16]
    DB $0F, $29, $62, $40 // movaps [edx + 4*16], xmm4
    DB $0F, $28, $58, $30 // movaps xmm3, [eax + 3*16]
    DB $0F, $29, $5A, $30 // movaps [edx + 3*16], xmm3
    DB $0F, $28, $50, $20 // movaps xmm2, [eax + 2*16]
    DB $0F, $29, $52, $20 // movaps [edx + 2*16], xmm2
    DB $0F, $28, $48, $10 // movaps xmm1, [eax + 1*16]
    DB $0F, $29, $4A, $10 // movaps [edx + 1*16], xmm1
    DB $0F, $28, $00      // movaps xmm0, [eax]
    DB $90                // nop
    DB $0F, $29, $02      // movaps [edx], xmm0
    DB $90                // nop
  {$else .CPUX64}
    movaps xmm7, [rax + 7*16]
    movaps [rdx + 7*16], xmm7
    movaps xmm6, [rax + 6*16]
    movaps [rdx + 6*16], xmm6
    movaps xmm5, [rax + 5*16]
    movaps [rdx + 5*16], xmm5
    movaps xmm4, [rax + 4*16]
    movaps [rdx + 4*16], xmm4
    movaps xmm3, [rax + 3*16]
    movaps [rdx + 3*16], xmm3
    movaps xmm2, [rax + 2*16]
    movaps [rdx + 2*16], xmm2
    movaps xmm1, [rax + 1*16]
    movaps [rdx + 1*16], xmm1
    movaps xmm0, [rax]
    nop
    movaps [rdx], xmm0
    nop
  {$endif}
@done:
  jl @move_next_16128
  ret

  {$ifdef CPUX86}
  @x86_non_SSE:
    shl ecx, 4
    jmp Move
  {$endif}
end;
{$endif}

procedure NcMoveB16(var Source, Dest: B16; B16Count: NativeUInt); forward;

procedure MoveB16Aligned(const Source; var Dest; const B16Count: NativeInt);
{$ifNdef CPUINTEL}
begin
  Move(Source, Dest, B16Count shl 4);
end;
{$else}
asm
  // if (B16Count <= 0) then Exit
  {$ifdef CPUX86}
    test ecx, ecx
  {$else .CPUX64}
    test r8, r8
  {$endif}
  jle @none

  // if (Dest = Source) then Exit
  // if (Dest < Source) then NcMoveB16(Source, Dest, B16Count)
  {$ifdef CPUX86}
    cmp edx, eax
  {$else .CPUX64}
    cmp rdx, rcx
  {$endif}
  je @none
  jb NcMoveB16

  // if (Dest >= Source + 16 * B16Count) then NcMoveB16(Source, Dest, B16Count)
  {$ifdef CPUX86}
    push eax
    lea eax, [eax + 8 * ecx]
    lea eax, [eax + 8 * ecx]
    cmp edx, eax
    pop eax
  {$else .CPUX64}
    lea r9, [rax + 8 * r8]
    lea r9, [r9 + 8 * r8]
    cmp rdx, r9
  {$endif}
  jae NcMoveB16

  // BackwardSSEMove(Source, Dest, B16Count)
  jmp BackwardSSEMove

@none:
end;
{$endif}

procedure NcMoveB16(var Source, Dest: B16; B16Count: NativeUInt);
{$ifNdef CPUINTEL} inline;
begin
  {$ifdef POSIX}
     memcpy(Dest, Source, B16Count shl 4);
  {$else}
     Move(Source, Dest, B16Count shl 4);
  {$endif}
end;
{$else}
asm
  {$ifdef CPUX86}
    // check SSE availability
    cmp SSE_SUPPORT, 0
    jz @x86_non_SSE
  {$else .CPUX64}
    // rax = Src, rdx = Dest, rcx = B16Count
    mov rax, rcx
    mov rcx, r8
  {$endif}

@move_next_16128:
  {$ifdef CPUX86}
    cmp ecx, 8
    jae @move_128

    lea ecx, [ecx * 8]
    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    neg ecx
    lea ecx, [ecx + @done]
    jmp ecx
  @move_128:
    lea eax, [eax + 128]
    lea edx, [edx + 128]
    lea ecx, [ecx - 8]
  {$else .CPUX64}
    cmp rcx, 8
    jae @move_128

    lea rcx, [rcx * 8]
    lea rax, [rax + rcx * 2]
    lea rdx, [rdx + rcx * 2]
    lea r9, @done
    neg rcx
    lea r9, [r9 + rcx]
    jmp r9
    nop
  @move_128:
    lea rax, [rax + 128]
    lea rdx, [rdx + 128]
    lea rcx, [rcx - 8]
  {$endif}

  {$ifdef CPUX86}
    DB $0F, $28, $78, $80 // movaps xmm7, [eax - 7*16 - 16]
    DB $0F, $29, $7A, $80 // movaps [edx - 7*16 - 16], xmm7
    DB $0F, $28, $70, $90 // movaps xmm6, [eax - 6*16 - 16]
    DB $0F, $29, $72, $90 // movaps [edx - 6*16 - 16], xmm6
    DB $0F, $28, $68, $A0 // movaps xmm5, [eax - 5*16 - 16]
    DB $0F, $29, $6A, $A0 // movaps [edx - 5*16 - 16], xmm5
    DB $0F, $28, $60, $B0 // movaps xmm4, [eax - 4*16 - 16]
    DB $0F, $29, $62, $B0 // movaps [edx - 4*16 - 16], xmm4
    DB $0F, $28, $58, $C0 // movaps xmm3, [eax - 3*16 - 16]
    DB $0F, $29, $5A, $C0 // movaps [edx - 3*16 - 16], xmm3
    DB $0F, $28, $50, $D0 // movaps xmm2, [eax - 2*16 - 16]
    DB $0F, $29, $52, $D0 // movaps [edx - 2*16 - 16], xmm2
    DB $0F, $28, $48, $E0 // movaps xmm1, [eax - 1*16 - 16]
    DB $0F, $29, $4A, $E0 // movaps [edx - 1*16 - 16], xmm1
    DB $0F, $28, $40, $F0 // movaps xmm0, [eax - 0*16 - 16]
    DB $0F, $29, $42, $F0 // movaps [edx - 0*16 - 16], xmm0
  {$else .CPUX64}
    movaps xmm7, [rax - 7*16 - 16]
    movaps [rdx - 7*16 - 16], xmm7
    movaps xmm6, [rax - 6*16 - 16]
    movaps [rdx - 6*16 - 16], xmm6
    movaps xmm5, [rax - 5*16 - 16]
    movaps [rdx - 5*16 - 16], xmm5
    movaps xmm4, [rax - 4*16 - 16]
    movaps [rdx - 4*16 - 16], xmm4
    movaps xmm3, [rax - 3*16 - 16]
    movaps [rdx - 3*16 - 16], xmm3
    movaps xmm2, [rax - 2*16 - 16]
    movaps [rdx - 2*16 - 16], xmm2
    movaps xmm1, [rax - 1*16 - 16]
    movaps [rdx - 1*16 - 16], xmm1
    movaps xmm0, [rax - 0*16 - 16]
    movaps [rdx - 0*16 - 16], xmm0
  {$endif}
@done:
  ja @move_next_16128
  ret

  {$ifdef CPUX86}
  @x86_non_SSE:
    shl ecx, 4
    jmp Move
  {$endif}
end;
{$endif}

function NcMoveB16Small(var Source, Dest: B16; B16Count{0..8}: NativeUInt): {Dest}P16;
{$ifNdef CPUINTEL} inline;
begin
  {$ifdef POSIX}
     memcpy(Dest, Source, B16Count shl 4);
  {$else}
     Move(Source, Dest, B16Count shl 4);
  {$endif}

  Result := @Dest;
end;
{$else}
asm
  {$ifdef CPUX86}
    // check SSE availability
    cmp SSE_SUPPORT, 0
    jz @x86_non_SSE
  {$else .CPUX64}
    // rax = Src, rdx = Dest, rcx = B16Count
    mov rax, rcx
    mov rcx, r8
  {$endif}

  {$ifdef CPUX86}
    lea ecx, [ecx * 8]
    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    neg ecx
    lea ecx, [ecx + @done]
    jmp ecx
  {$else .CPUX64}
    lea rcx, [rcx * 8]
    lea rax, [rax + rcx * 2]
    lea rdx, [rdx + rcx * 2]
    lea r9, @done
    neg rcx
    lea r9, [r9 + rcx]
    jmp r9
    nop
  {$endif}

  {$ifdef CPUX86}
    DB $0F, $28, $78, $80 // movaps xmm7, [eax - 7*16 - 16]
    DB $0F, $29, $7A, $80 // movaps [edx - 7*16 - 16], xmm7
    DB $0F, $28, $70, $90 // movaps xmm6, [eax - 6*16 - 16]
    DB $0F, $29, $72, $90 // movaps [edx - 6*16 - 16], xmm6
    DB $0F, $28, $68, $A0 // movaps xmm5, [eax - 5*16 - 16]
    DB $0F, $29, $6A, $A0 // movaps [edx - 5*16 - 16], xmm5
    DB $0F, $28, $60, $B0 // movaps xmm4, [eax - 4*16 - 16]
    DB $0F, $29, $62, $B0 // movaps [edx - 4*16 - 16], xmm4
    DB $0F, $28, $58, $C0 // movaps xmm3, [eax - 3*16 - 16]
    DB $0F, $29, $5A, $C0 // movaps [edx - 3*16 - 16], xmm3
    DB $0F, $28, $50, $D0 // movaps xmm2, [eax - 2*16 - 16]
    DB $0F, $29, $52, $D0 // movaps [edx - 2*16 - 16], xmm2
    DB $0F, $28, $48, $E0 // movaps xmm1, [eax - 1*16 - 16]
    DB $0F, $29, $4A, $E0 // movaps [edx - 1*16 - 16], xmm1
    DB $0F, $28, $40, $F0 // movaps xmm0, [eax - 0*16 - 16]
    DB $0F, $29, $42, $F0 // movaps [edx - 0*16 - 16], xmm0
  {$else .CPUX64}
    movaps xmm7, [rax - 7*16 - 16]
    movaps [rdx - 7*16 - 16], xmm7
    movaps xmm6, [rax - 6*16 - 16]
    movaps [rdx - 6*16 - 16], xmm6
    movaps xmm5, [rax - 5*16 - 16]
    movaps [rdx - 5*16 - 16], xmm5
    movaps xmm4, [rax - 4*16 - 16]
    movaps [rdx - 4*16 - 16], xmm4
    movaps xmm3, [rax - 3*16 - 16]
    movaps [rdx - 3*16 - 16], xmm3
    movaps xmm2, [rax - 2*16 - 16]
    movaps [rdx - 2*16 - 16], xmm2
    movaps xmm1, [rax - 1*16 - 16]
    movaps [rdx - 1*16 - 16], xmm1
    movaps xmm0, [rax - 0*16 - 16]
    movaps [rdx - 0*16 - 16], xmm0
  {$endif}
@done:
  {$ifdef CPUX86}
    sub ecx, offset @done
    lea eax, [edx + ecx * 2]
  {$else .CPUX64}
    lea rax, [rdx + rcx * 2]
  {$endif}
  ret

  {$ifdef CPUX86}
  @x86_non_SSE:
  push eax
    shl ecx, 4
    call Move
  pop eax
  {$endif}
end;
{$endif}


const
  BIT_SCANS: array[Byte] of Byte = ({failure}8, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0,
    2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2,
    0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0,
    1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1,
    0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
    4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 7, 0, 1, 0, 2, 0, 1, 0, 3,
    0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0,
    1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1,
    0, 2, 0, 1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0,
    2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2,
    0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0
  );

function BitScan(var BitSet: TBitSet8): NativeInt;
{$ifdef PUREPASCAL}
var
  P: PByte;
begin
  P := @BitSet.VBytes[0];
  Inc(PInteger(P), Byte(PInteger(P)^ = 0));
  Inc(PWord(P), Byte(PWord(P)^ = 0));
  Inc(P, Byte(P^ = 0));

  Result := (NativeUInt(P) - NativeUInt(@BitSet)) shl 3 + BIT_SCANS[P^];
  Result := Result or -(Result shr 6);
end;
{$else}
asm
  {$ifdef CPUX86}
    mov edx, [eax]
    lea ecx, [eax + 4]
    test edx, edx
    DB $0F, $44, $C1 // cmovz eax, ecx
    lea ecx, [ecx - 4]
    mov edx, [eax]
    sub ecx, eax
    and ecx, 32
    or eax, -1
    bsf eax, edx
    or eax, ecx
  {$else .CPUX64}
    mov rdx, [rcx]
    or rax, -1
    bsf rax, rdx
  {$endif}
end;
{$endif}

function BitReserve(var BitSet: TBitSet8): NativeInt;
{$ifdef PUREPASCAL}
var
  P: PByte;
  VInteger: Integer;
  PVInteger: PInteger;
begin
  P := @BitSet.VBytes[0];
  Inc(PInteger(P), Byte(PInteger(P)^ = 0));

  PVInteger := Pointer(P);
  VInteger := PVInteger^;
  if (VInteger <> 0) then
  begin
    Inc(PWord(P), Byte(VInteger and $ffff = 0));
    Inc(P, Byte(P^ = 0));
    Result := (NativeUInt(P) - NativeUInt(@BitSet)) shl 3 + BIT_SCANS[P^];
    PVInteger^ := VInteger xor (1 shl (Result and 31));
    Exit;
  end else
  begin
    Result := -1;
  end;
end;
{$else}
asm
  {$ifdef CPUX86}
    mov edx, [eax]
    lea ecx, [eax + 4]
    test edx, edx
    DB $0F, $44, $C1 // cmovz eax, ecx
    lea ecx, [ecx - 4]
    mov edx, [eax]
    push ecx
    bsf ecx, edx
    jz @failure
    btr edx, ecx
    mov [eax], edx
    pop edx
    sub edx, eax
    and edx, 32
    lea eax, [edx + ecx]
  {$else .CPUX64}
    mov rdx, [rcx]
    bsf rax, rdx
    jz @failure
    btr rdx, rax
    mov [rcx], rdx
  {$endif}

  ret
@failure:
  {$ifdef CPUX86}
    pop ecx
    or eax, -1
  {$else .CPUX64}
    or rax, -1
  {$endif}
end;
{$endif}

function BitUnreserve(var BitSet: TBitSet8; const Index: NativeUInt): Boolean;
{$ifdef PUREPASCAL}
var
  Mask: NativeInt;
  Value: NativeInt;
  {$ifdef SMALLINT}
  PVInteger: PInteger;
  {$endif}
begin
  Mask := 1;
  {$ifdef LARGEINT}
    Mask := Mask shl Index;
    Value := BitSet.V64;
  {$else .SMALLINT}
    Mask := Mask shl (Index and 31);
    PVInteger := @BitSet.VIntegers[Byte(Index > 31)];
    Value := PVInteger^;
  {$endif}
  if (Value and Mask = 0) then
  begin
    Inc(Value, Mask);
    {$ifdef LARGEINT}
      BitSet.V64 := Value;
    {$else .SMALLINT}
      PVInteger^ := Value;
    {$endif}
    Result := True;
    Exit;
  end else
  begin
    Result := False;
  end;
end;
{$else}
asm
  {$ifdef CPUX86}
    lea ecx, [eax + 4]
    test edx, 32
    DB $0F, $45, $C1 // cmovnz eax, ecx
    and edx, 31
    mov ecx, [eax]
    bts ecx, edx
    mov [eax], ecx
    setnc al
  {$else .CPUX64}
    mov rax, [rcx]
    bts rax, rdx
    mov [rcx], rax
    setnc al
  {$endif}
end;
{$endif}


{ TSyncStack }

{$ifdef CPUX64}
const
  X64_SYNCSTACK_MASK = ((NativeUInt(1) shl 48) - 1) and -16;
  X64_SYNCSTACK_CLEAR = not X64_SYNCSTACK_MASK;
{$endif}

{$ifNdef SMALLINT}
function TSyncStack.GetAssigned: Boolean;
begin
  {$ifdef CPUINTEL}
    Result := (F.Handle and (((NativeUInt(1) shl 48) - 1) and -16){X64_SYNCSTACK_MASK} <> 0);
  {$else .CPUARM64}
    Result := (F.Handle <> 0);
  {$endif}
end;
{$endif}

procedure TSyncStack.Push(const Value: Pointer);
{$ifdef INLINESUPPORT}
begin
  Self.PushList(Value, Value);
end;
{$else .CPUX86}
asm
  mov ecx, edx
  jmp TSyncStack.PushList
end;
{$endif}

procedure TSyncStack.PushList(const First, Last: Pointer);
{$ifNdef CPUX86}
var
  Item, NewItem: SupposedPtr;
begin
  repeat
    Item := F.Handle;
    PSupposedPtr(Last)^ := Item {$ifdef CPUX64}and X64_SYNCSTACK_MASK{$endif};
    NewItem := SupposedPtr(First) {$ifdef CPUX64}+ (((Item or X64_SYNCSTACK_MASK) + 1) and X64_SYNCSTACK_CLEAR){$endif};
  until (Item = AtomicCmpExchange(F.Handle, NewItem, Item));
end;
{$else .CPUX86}
asm
  push esi
  push edi
  push ebp
  push ebx
  mov esi, eax // Self
  mov edi, edx // First
  mov ebp, ecx // Last

  // Item := F.Handle
  fild qword ptr [esi]
  sub esp, 8
  fistp qword ptr [esp]
  pop eax
  pop edx

  // lock-free loop
  @repeat:
    // PStackItem(Last).Next := Item
    mov [ebp], eax

    // NewItem := First | Counter++
    mov ebx, edi
    lea ecx, [edx + 1]

    // Item := AtomicCmpExchangeInt64(F.Handle, NewItem, Item)
    DB $F0, $0F, $C7, $0E // lock cmpxchg8b [esi]
  jnz @repeat

@done:
  pop ebx
  pop ebp
  pop edi
  pop esi
end;
{$endif}

function TSyncStack.Pop: Pointer;
{$ifNdef CPUX86}
var
  Item, NewItem: SupposedPtr;
begin
  repeat
    Item := F.Handle;
    Result := Pointer(Item {$ifdef CPUX64}and X64_SYNCSTACK_MASK{$endif});
    if (Result = nil) then Exit;

    NewItem := PSupposedPtr(Result)^ {$ifdef CPUX64}+ (((Item or X64_SYNCSTACK_MASK) + 1) and X64_SYNCSTACK_CLEAR){$endif};
  until (Item = AtomicCmpExchange(F.Handle, NewItem, Item));
end;
{$else .CPUX86}
asm
  push esi
  push ebx
  mov esi, eax // Self

  // Item := F.Handle
  fild qword ptr [esi]
  sub esp, 8
  fistp qword ptr [esp]
  pop eax
  pop edx

  // lock-free loop
  @repeat:
    // if (Result(Item) = nil) then Exit;
    test eax, eax
    jz @done

    // NewItem := PStackItem(Item).Next | Counter++
    mov ebx, [eax]
    lea ecx, [edx + 1]

    // Item := AtomicCmpExchangeInt64(F.Handle, NewItem, Item)
    DB $F0, $0F, $C7, $0E // lock cmpxchg8b [esi]
  jnz @repeat

@done:
  pop ebx
  pop esi
end;
{$endif}

function TSyncStack.PopList: Pointer;
{$ifNdef CPUX86}
var
  Item, NewItem: SupposedPtr;
begin
  repeat
    Item := F.Handle;
    Result := Pointer(Item {$ifdef CPUX64}and X64_SYNCSTACK_MASK{$endif});
    if (Result = nil) then Exit;

    NewItem := {nil}0 {$ifdef CPUX64}+ (((Item or X64_SYNCSTACK_MASK) + 1) and X64_SYNCSTACK_CLEAR){$endif};
  until (Item = AtomicCmpExchange(F.Handle, NewItem, Item));
end;
{$else .CPUX86}
asm
  push esi
  push ebx
  mov esi, eax // Self

  // Item := F.Handle
  fild qword ptr [esi]
  sub esp, 8
  fistp qword ptr [esp]
  pop eax
  pop edx

  // lock-free loop
  @repeat:
    // if (Result(Item) = nil) then Exit;
    test eax, eax
    jz @done

    // NewItem := 0 | Counter++
    xor ebx, ebx
    lea ecx, [edx + 1]

    // Item := AtomicCmpExchangeInt64(F.Handle, NewItem, Item)
    DB $F0, $0F, $C7, $0E // lock cmpxchg8b [esi]
  jnz @repeat

@done:
  pop ebx
  pop esi
end;
{$endif}


 (* Global storage routine *)

function GrowCoreThreadHeaps(Storage: PGlobalStorage): PThreadHeap;
type
  TCoreThreadHeapList = array[0..SIZE_K64 div SizeOf(TThreadHeap) - 1] of TThreadHeap;
  PCoreThreadHeapList = ^TCoreThreadHeapList;
var
  i: Integer;
  CoreThreadHeapList: PCoreThreadHeapList;
begin
  {$ifdef MSWINDOWS}
    CoreThreadHeapList := VirtualAlloc(nil, SIZE_K64, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
  {$else .POSIX}
    CoreThreadHeapList := memalign(SIZE_K64, SIZE_K64);
  {$endif}
  if (CoreThreadHeapList = nil) then
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};

  for i := 1 to High(TCoreThreadHeapList) - 1 do
    CoreThreadHeapList[i].FNextHeap := @CoreThreadHeapList[i + 1];

  Storage.CoreThreadHeaps.PushList(@CoreThreadHeapList[1],
    @CoreThreadHeapList[High(TCoreThreadHeapList)]);

  Result := @CoreThreadHeapList[0];
end;

function GrowCoreInternalRecords(Storage: PGlobalStorage): PInternalRecord;
type
  TCoreInternalRecordList = array[0..SizeOf(TThreadHeap) div SizeOf(TInternalRecord) - 1] of TInternalRecord;
  PCoreInternalRecordList = ^TCoreInternalRecordList;
var
  i: Integer;
  CoreInternalRecordList: PCoreInternalRecordList;
begin
  CoreInternalRecordList := Storage.CoreThreadHeaps.Pop;
  if (CoreInternalRecordList = nil) then
  begin
    CoreInternalRecordList := Pointer(GrowCoreThreadHeaps(Storage));
  end;

  for i := 1 to High(TCoreInternalRecordList) - 1 do
    CoreInternalRecordList[i].Next := @CoreInternalRecordList[i + 1];

  Storage.CoreInternalRecords.PushList(@CoreInternalRecordList[1],
    @CoreInternalRecordList[High(TCoreInternalRecordList)]);

  Result := @CoreInternalRecordList[0];
end;

function CoreInternalRecordPop: PInternalRecord;
var
  GlobalStorage: PGlobalStorage;
begin
  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);

  Result := GlobalStorage.CoreInternalRecords.Pop;
  if (Result = nil) then
    Result := GrowCoreInternalRecords(GlobalStorage);
end;

procedure CoreInternalRecordPush(const InternalRecord: PInternalRecord);
var
  GlobalStorage: PGlobalStorage;
begin
  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  GlobalStorage.CoreInternalRecords.Push(InternalRecord);
end;


 (* Local thread heap initialization *)

{$ifdef PUREPASCAL}
threadvar
  ThreadHeapInstance: PThreadHeap;
{$endif}

function CreateThreadHeap(Store: Boolean): PThreadHeap;
var
  GlobalStorage: PGlobalStorage;
  InternalRecord: PInternalRecord;
  Flags: SupposedPtr;
  NextHeap: PThreadHeap;
begin
  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);

  InternalRecord := GlobalStorage.DeferredThreadHeaps.Pop;
  if (InternalRecord <> nil) then
  begin
    Result := InternalRecord.ThreadHeap;

    // make unlokable
    repeat
      Flags := Result.LockFlags;
      if (Flags <> THREAD_HEAP_LOCKABLE) then
      begin
        if (Flags = THREAD_HEAP_LOCKED) then
        begin
          SpinWait(Result.LockFlags, THREAD_HEAP_LOCKED_BIT);
        end else
        begin
          {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
        end;
      end;
    until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(Result.LockFlags, 0, THREAD_HEAP_LOCKABLE));

    // deferred
    Result.ProcessThreadDeferred;

    {$ifdef PUREPASCAL}
    if (Store) then
      ThreadHeapInstance := Result;
    {$endif}

    GlobalStorage.CoreInternalRecords.Push(InternalRecord);
    Exit;
  end;

  Result := GlobalStorage.CoreThreadHeaps.Pop;
  if (Result = nil) then
  begin
    Result := GrowCoreThreadHeaps(GlobalStorage);
  end;

  FillChar(Result^, SizeOf(TThreadHeap), #0);
  Result.FMarkerNotSelf := not SupposedPtr(Result);

  repeat
    NextHeap := ThreadHeapList;
    Result.FNextHeap := NextHeap;
  until (SupposedPtr(NextHeap) = AtomicCmpExchange(SupposedPtr(ThreadHeapList),
    SupposedPtr(Result), SupposedPtr(NextHeap)));

  {$ifdef PUREPASCAL}
  if (Store) then
    ThreadHeapInstance := Result;
  {$endif}

  // hints off
  Result.FReserved := nil;
end;

{$ifNdef PUREPASCAL}
const
  THREAD_HEAP = {$ifdef CPUX86}$14{$else}$28{$endif};

function ThreadHeapInstance: PThreadHeap;
asm
  // Get
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rax, gs:[abs THREAD_HEAP]
    test rax, rax
  {$endif}
  jz @create_thread_heap
  ret

@create_thread_heap:
  {$ifdef CPUX86}
    xor eax, eax
    call CreateThreadHeap
  {$else .CPUX64}
    {stack align} push rcx
    xor rcx, rcx
    call CreateThreadHeap
    {stack align} pop rcx
  {$endif}

  // Set
  {$ifdef CPUX86}
    mov fs:[THREAD_HEAP], eax
  {$else .CPUX64}
    mov gs:[abs THREAD_HEAP], rax
  {$endif}
end;

function SafeProcessThreadDeferred(Self: PThreadHeap
  {, v2, v3, ReturnAddress, x64_v5: Pointer}): PThreadHeap;
asm
  {$ifdef CPUX86}
  push eax
    push edx
    push ecx
      call TThreadHeap.ProcessThreadDeferred
    pop ecx
    pop edx
  pop eax
  jmp ebx
  {$else .CPUX64}
  push rcx
  push r9
    push rdx
    push r8
    push r10
      call TThreadHeap.ProcessThreadDeferred
    pop r10
    pop r8
    pop rdx
  pop r9
  pop rcx
  jmp r9
  {$endif}
end;
{$endif}

function CurrentThreadHeap: PThreadHeap;
{$ifdef PUREPASCAL}
begin
  Result := ThreadHeapInstance;
end;
{$else}
asm
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    mov rax, gs:[abs THREAD_HEAP]
  {$endif}
end;
{$endif}


type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

procedure ThreadHeapLock(ThreadHeap: PThreadHeap);
var
  GlobalStorage: PGlobalStorage;
  InternalRecord: PInternalRecord;
begin
  ThreadHeap.ProcessThreadDeferred;
  if (ThreadHeap.LockFlags <> 0) then
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;

  InternalRecord := CoreInternalRecordPop;
  InternalRecord.ThreadHeap := ThreadHeap;
  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  GlobalStorage.DeferredThreadHeaps.Push(InternalRecord);
end;

function BrainMMThreadProxy(Parameter: Pointer): Integer;
var
  ThreadRec: TThreadRec;
  ThreadHeap: PThreadHeap;
begin
  {$ifdef PUREPASCAL}
    CreateThreadHeap(True);
  {$else}
    ThreadHeapInstance;
  {$endif}

  ThreadRec := PThreadRec(Parameter)^;
  CoreInternalRecordPush(Parameter);
  Result := ThreadRec.Func(ThreadRec.Parameter);

  ThreadHeap := ThreadHeapInstance;
  {$ifdef PUREPASCAL}
    ThreadHeapInstance := nil;
  {$endif}
  if (ThreadHeap <> nil) then
  begin
    if (ThreadHeap.LockFlags = 0) then
    begin
      ThreadHeapLock(ThreadHeap);
    end else
    if (ThreadHeap.LockFlags > THREAD_HEAP_LOCKED) then
    begin
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
    end;
  end;
end;

procedure BrainMMEndThreadEvent(ExitCode: Integer);
var
  ThreadHeap: PThreadHeap;
begin
  ThreadHeap := ThreadHeapInstance;
  {$ifdef PUREPASCAL}
    ThreadHeapInstance := nil;
  {$endif}

  if (ThreadHeap <> nil) then
  begin
    if (ThreadHeap.LockFlags = 0) then
    begin
      ThreadHeapLock(ThreadHeap);
    end else
    if (ThreadHeap.LockFlags > THREAD_HEAP_LOCKED) then
    begin
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
    end;
  end;
end;

{$ifdef FPC}
  {$MESSAGE ERROR 'THREAD ROUTINE NOT YET REALIZED'}
{$else}
  {$ifdef MSWINDOWS}
    {$ifdef CONDITIONALEXPRESSIONS}
       {$if CompilerVersion < 18}
         {$define THREAD_FUNCS_EMULATE}
         {$define THREAD_FUNCS_WRAPPER}
       {$ifend}
    {$else}
       {$define THREAD_FUNCS_EMULATE}
       {$define THREAD_FUNCS_WRAPPER}
    {$endif}
  {$endif}
{$endif}

{$ifdef POSIX}
  {$define THREAD_FUNCS_WRAPPER}
{$endif}

{$ifdef THREAD_FUNCS_WRAPPER}
procedure _FpuInit;
{$WARNINGS OFF}
{$ifdef CPUX86}
  {$ifdef IOS}
  begin
    FSetExceptMask((DefaultMXCSR and $1F80) shr 7);
    Set8087CW(Default8087CW);
    SetMXCSR(DefaultMXCSR);
  end;
  {$else}
  const
    Default8087CW: Word = $1332;
  asm
    FNINIT
    FWAIT
    FLDCW Default8087CW
  end;
  {$endif}
{$else}
  {$ifdef CPUARM}
    const
      ValidMask = $07C0009F;
    begin
      SetFPSCR(DefaultFPSCR and ValidMask);
      FSetExceptMask(DefaultFPSCR);
    end;
  {$else .CPUX64}
     {$ifdef MSWINDOWS}
     asm
        LDMXCSR DefaultMXCSR
     end;
     {$endif}
  {$endif}
{$endif}
{$WARNINGS ON}

procedure RunErrorAt(ErrCode: Integer; ErrorAtAddr: Pointer);
begin
  ErrorAddr := ErrorAtAddr;
  Halt(ErrCode);
end;

procedure _UnhandledException;
type TExceptProc = procedure (Obj: TObject; Addr: Pointer);
begin
  {$ifdef CONDITIONALEXPRESSIONS}
  if Assigned(ExceptProc) then
    TExceptProc(ExceptProc)(ExceptObject, ExceptAddr)
  else
    RunErrorAt(230, ExceptAddr);
  {$else}
    RunErrorAt(230, ErrorAddr);
  {$endif}
end;

function __ThreadWrapper(Parameter: Pointer):
  {$ifdef MSWINDOWS}Integer; stdcall;{$else .POSIX}NativeInt; cdecl;{$endif}
var
  ThreadRec: TThreadRec;
begin
  Result := 0;
  try
    _FpuInit;
    ThreadRec := PThreadRec(Parameter)^;
    FreeMem(PThreadRec(Parameter));
    Result := ThreadRec.Func(ThreadRec.Parameter);
  except
    _UnhandledException;
  end;
end;
{$endif}


{$ifdef MSWINDOWS}
function BrainMMThreadFuncEvent(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
var
  Proxy, P: PThreadRec;
begin
  Proxy := Pointer(CoreInternalRecordPop);
  Proxy.Func := ThreadFunc;
  Proxy.Parameter := Parameter;

  New(P);
  P.Func := BrainMMThreadProxy;
  P.Parameter := Proxy;

  Result := P;
end;
{$else .POSIX}
function posix_pthread_create(var Thread: pthread_t; Attr: Ppthread_attr_t;
  TFunc: Pointer; Arg: Pointer): Integer; cdecl;
  external libpthread name _PU + 'pthread_create';

function BrainMMThreadFuncEvent(Attribute: PThreadAttr;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    var ThreadId: NativeUInt): Integer;
var
  Proxy, P: PThreadRec;
begin
  Proxy := Pointer(CoreInternalRecordPop);
  Proxy.Func := ThreadFunc;
  Proxy.Parameter := Parameter;

  New(P);
  P.Func := BrainMMThreadProxy;
  P.Parameter := Proxy;

  IsMultiThread := True;
  Result := posix_pthread_create(pthread_t(ThreadID), Ppthread_attr_t(Attribute),
    @__ThreadWrapper, P);

  if Result <> 0 then
    Dispose(P);
end;
{$endif}

{$ifdef THREAD_FUNCS_EMULATE} // CompilerVersion <= Delphi 2005
var
  SystemThreadFuncProc: function(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
  SystemThreadEndProc: procedure(ExitCode: Integer);

function __BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: NativeUInt): THandle;
var
  P: PThreadRec;
begin
  if Assigned(SystemThreadFuncProc) then
    P := PThreadRec(SystemThreadFuncProc(ThreadFunc, Parameter))
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
  end;

  IsMultiThread := TRUE;

  Result := CreateThread(SecurityAttributes, StackSize, @__ThreadWrapper, P,
    CreationFlags, ThreadID);

  if Result = 0 then
    Dispose(P);
end;

procedure __EndThread(ExitCode: Integer);
begin
  if Assigned(SystemThreadEndProc) then
    SystemThreadEndProc(ExitCode);
  ExitThread(ExitCode);
end;
{$endif}


 (* Global pages/blocks routine *)

function BrainMMGetMemoryBlock(BlockSize: TMemoryBlockSize;
  PagesMode: NativeUInt): MemoryBlock;
var
  GlobalStorage: PGlobalStorage;
begin
  // todo
  if (BlockSize <> BLOCK_64K) or (PagesMode = PAGESMODE_USER) then
  begin
    Result := nil;
    Exit;
  end;

  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  Result := GlobalStorage.K64BlockCache.Stack.Pop;
  if (Result <> nil) then
  begin
    AtomicIncrement(NativeInt(GlobalStorage.K64BlockCache.Count), -1);
  end else
  begin
    {$ifdef MSWINDOWS}
      Result := VirtualAlloc(nil, SIZE_K64, MEM_COMMIT, PAGE_READWRITE);
    {$else .POSIX}
      Result := memalign(SIZE_K64, SIZE_K64);
    {$endif}
  end;

  if (Result <> nil) and (PagesMode = PAGESMODE_JIT) then
    ChangeMemoryAccessRights(Result, 64 div 4, [marRead, marWrite, marExecute]);
end;

function BrainMMFreeMemoryBlock(Block: MemoryBlock; PagesMode: NativeUInt): Boolean;
var
  GlobalStorage: PGlobalStorage;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := False;
    Exit;
  end;

  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  if (GlobalStorage.K64BlockCache.Count < 16) then
  begin
    AtomicIncrement(NativeInt(GlobalStorage.K64BlockCache.Count));
    GlobalStorage.K64BlockCache.Stack.Push(Block);
    Result := True;
  end else
  begin
    {$ifdef MSWINDOWS}
      Result := VirtualFree(Block, 0, MEM_RELEASE);
    {$else .POSIX}
      free(Block);
      Result := True;
    {$endif}
  end;
end;

function BrainMMGetMemoryPages(Count: NativeUInt; PagesMode: NativeUInt): MemoryPages;
var
  K64Count: NativeUInt;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := nil;
    Exit;
  end;

  K64Count := ((Count + 1) + 15) shr 4;

  {$ifdef MSWINDOWS}
    Result := VirtualAlloc(nil, K64Count * SIZE_K64, MEM_COMMIT, PAGE_READWRITE);
  {$else .POSIX}
    Result := memalign(SIZE_K4, K64Count * SIZE_K64);
  {$endif}

  if (Result <> nil) then
  begin
    Inc(NativeInt(Result), SIZE_K4);
    PNativeUInt(NativeInt(Result) - SizeOf(NativeUInt))^ := Count;

    if (PagesMode = PAGESMODE_JIT) then
      ChangeMemoryAccessRights(Result, Count, [marRead, marWrite, marExecute])
  end;
end;

function BrainMMRegetMemoryPages(Pages: MemoryPages; NewCount: NativeUInt;
  PagesMode: NativeUInt): MemoryPages;
var
  K64Count: NativeUInt;
  LastCount, LastK64Count: NativeUInt;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := nil;
    Exit;
  end;

  K64Count := ((NewCount + 1) + 15) shr 4;
  LastCount := PNativeUInt(NativeInt(Pages) - SizeOf(NativeUInt))^;
  LastK64Count := ((LastCount + 1) + 15) shr 4;

  if (LastK64Count = K64Count) then
  begin
    PNativeUInt(NativeInt(Pages) - SizeOf(NativeUInt))^ := NewCount;
    Result := Pages;
  end else
  begin
    if (not MemoryManager.BrainMM.FreeMemoryPages(Pages, PagesMode)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};

    Result := MemoryManager.BrainMM.GetMemoryPages(NewCount, PagesMode);
  end;
end;

function BrainMMReallocMemoryPages(Pages: MemoryPages; NewCount: NativeUInt;
  PagesMode: NativeUInt): MemoryPages;
var
  K64Count: NativeUInt;
  LastCount, LastK64Count: NativeUInt;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := nil;
    Exit;
  end;

  K64Count := ((NewCount + 1) + 15) shr 4;
  LastCount := PNativeUInt(NativeInt(Pages) - SizeOf(NativeUInt))^;
  LastK64Count := ((LastCount + 1) + 15) shr 4;

  if (LastK64Count = K64Count) then
  begin
    PNativeUInt(NativeInt(Pages) - SizeOf(NativeUInt))^ := NewCount;
    Result := Pages;
  end else
  begin
    Result := MemoryManager.BrainMM.GetMemoryPages(NewCount, PagesMode);
    if (Result <> nil) then
    begin
      if (LastCount > NewCount) then LastCount := NewCount;
      NcMoveB16(P16(Pages)^, P16(Result)^, LastCount * B16_PER_PAGE);
    end;  

    if (not MemoryManager.BrainMM.FreeMemoryPages(Pages, PagesMode)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;
end;

function BrainMMFreeMemoryPages(Pages: MemoryPages; PagesMode: NativeUInt): Boolean;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := False;
    Exit;
  end;

  Dec(NativeInt(Pages), SIZE_K4);

  {$ifdef MSWINDOWS}
    Result := VirtualFree(Pages, 0, MEM_RELEASE);
  {$else .POSIX}
    free(Pages);
    Result := True;
  {$endif}
end;


{ TThreadHeap }

function TThreadHeap.ErrorOutOfMemory: Pointer;
{$ifNdef CONDITIONALEXPRESSIONS}
type
  TErrorProc = procedure(ErrorCode: Integer; ErrorAddr: Pointer);
{$endif}
begin
  if (ErrorAddr <> nil) then
  begin
    if Assigned(System.ErrorProc) then
    {$ifdef CONDITIONALEXPRESSIONS}
      System.ErrorProc(Byte(reOutOfMemory), ErrorAddr);
    {$else}
      TErrorProc(System.ErrorProc)(1, ErrorAddr);
    {$endif}
      
    System.ErrorAddr := ErrorAddr;
    if (System.ExitCode = 0) then System.ExitCode := 203{reOutOfMemory};
    System.Halt;
  end;

  Result := {failure}nil;
end;

function TThreadHeap.ErrorInvalidPtr: Integer;
{$ifNdef CONDITIONALEXPRESSIONS}
type
  TErrorProc = procedure(ErrorCode: Integer; ErrorAddr: Pointer);
{$endif}
begin
  if (ErrorAddr <> nil) then
  begin
    if Assigned(System.ErrorProc) then
    {$ifdef CONDITIONALEXPRESSIONS}
      System.ErrorProc(Byte(reInvalidPtr), ErrorAddr);
    {$else}
      TErrorProc(System.ErrorProc)(2, ErrorAddr);
    {$endif}      

    System.ErrorAddr := ErrorAddr;
    if (System.ExitCode = 0) then System.ExitCode := 204{reInvalidPtr};
    System.Halt;
  end;

  Result := {failure}FREEMEM_INVALID;
end;

function TThreadHeap.RaiseOutOfMemory: Pointer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
     mov edx, [esp]
     cmp [EAX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorOutOfMemory
     mov [EAX].TThreadHeap.ErrorAddr, edx
  {$else .CPUX64}
     mov rdx, [rsp]
     cmp [RCX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorOutOfMemory
     mov [RCX].TThreadHeap.ErrorAddr, rdx
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory
end;
{$else}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @TThreadHeap.RaiseOutOfMemory;
{$endif}
begin
  if (Self.ErrorAddr = nil) then Self.ErrorAddr := ReturnAddress;
  Result := Self.ErrorOutOfMemory;
end;
{$endif}

function TThreadHeap.RaiseInvalidPtr: Integer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
     mov edx, [esp]
     cmp [EAX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorInvalidPtr
     mov [EAX].TThreadHeap.ErrorAddr, edx
  {$else .CPUX64}
     mov rdx, [rsp]
     cmp [RCX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorInvalidPtr
     mov [RCX].TThreadHeap.ErrorAddr, rdx
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr
end;
{$else}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @TThreadHeap.RaiseInvalidPtr;
{$endif}
begin
  if (Self.ErrorAddr = nil) then Self.ErrorAddr := ReturnAddress;
  Result := Self.ErrorInvalidPtr;
end;
{$endif}

type
  PThreadDeferred = ^TThreadDeferred;
  TThreadDeferred = packed record
    Next: PThreadDeferred;
    ReturnAddress: SupposedPtr;
    {
      Address: Pointer:31/63;
      IsSmall: Boolean:1;
    }
  end;

procedure TThreadHeap.PushThreadDeferred(P: Pointer; ReturnAddress: Pointer;
  IsSmall: Boolean);
label
  lock_free;
var
  Flags: SupposedPtr;
  LastErrorAddr: Pointer;
begin
  if (Self.LockFlags = 0) then
  begin
  lock_free:
    PThreadDeferred(P).ReturnAddress := SupposedPtr(ReturnAddress) +
      (SupposedPtr(IsSmall) shl HIGH_NATIVE_BIT);
    Deferreds.Push(P);
  end else
  begin
    // inline SpinLock
    repeat
      Flags := Self.LockFlags;
      if (Flags = 0) then goto lock_free;
      if (Flags <> THREAD_HEAP_LOCKABLE) then
      begin
        if (Flags = THREAD_HEAP_LOCKED) then
        begin
          SpinWait(Self.LockFlags, THREAD_HEAP_LOCKED_BIT);
        end else
        begin
          {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
        end;
      end;
    until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(Self.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
    LastErrorAddr := Self.ErrorAddr;
    try
      Self.ErrorAddr := ReturnAddress;
      if (IsSmall) then
      begin
        Self.FreeSmall(P);
      end else
      begin
        Self.FreeMedium(P);
      end;
    finally
      Self.ErrorAddr := LastErrorAddr;
      Self.LockFlags := THREAD_HEAP_LOCKABLE; // inline SpinUnlock
    end;
  end;
end;

procedure TThreadHeap.ProcessThreadDeferred;
var
  LastErrorAddr: Pointer;
  ReturnAddress: SupposedPtr;
  Counter: NativeUInt;
  ThreadDeferred, Next: PThreadDeferred;
begin
  ThreadDeferred := Deferreds.PopList;
  if (ThreadDeferred <> nil) then
  begin
    LastErrorAddr := Self.ErrorAddr;
    try
      // check duplicates
      Next := ThreadDeferred.Next;
      if (Next <> nil) then
      begin
        Counter := 0;
        repeat
          if (NativeInt(Next) and MASK_16_TEST <> 0) or (ThreadDeferred = Next) then
          begin
            Self.ErrorAddr := Pointer(NativeInt(ThreadDeferred.ReturnAddress) and MASK_HIGH_NATIVE_TEST);
            Self.RaiseInvalidPtr;
          end;
          Next := Next.Next;
          Inc(Counter);
        until ((Next = nil) or (Counter = 16));
      end;

      // free small/medium
      repeat
        Next := ThreadDeferred.Next;
        ReturnAddress := ThreadDeferred.ReturnAddress;

        Self.ErrorAddr := Pointer(NativeInt(ReturnAddress) and MASK_HIGH_NATIVE_TEST);
        if (NativeInt(ReturnAddress) < 0) then
        begin
          Self.FreeSmall(ThreadDeferred);
        end else
        begin
          Self.FreeMedium(ThreadDeferred);
        end;

        ThreadDeferred := Next;
      until (ThreadDeferred = nil);
    finally
      Self.ErrorAddr := LastErrorAddr;
    end;
  end;
end;


function BrainMMUnknownGetMem(None: Pointer; B16Count: NativeUInt;
  ErrorAddr: Pointer): Pointer;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // allocation
    ThreadHeap.ErrorAddr := ErrorAddr;
    case (B16Count) of
      0..MAX_SMALL_B16COUNT: Result := ThreadHeap.GetSmall(B16Count);
      MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
        Result := ThreadHeap.GetMedium(B16Count, Ord(ma16Bytes));
    else
      Result := MemoryManager.BrainMM.GetMemoryPages(
        (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
        PAGESMODE_SYSTEM);
      if (Result = nil) then
        Result := ThreadHeap.ErrorOutOfMemory;
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMGetMem(Size: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
label
  thread_deferreds_done;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  B16Count: NativeUInt;
begin
  B16Count := (Size + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      case (B16Count) of
        0..MAX_SMALL_B16COUNT: Result := ThreadHeap.GetSmall(B16Count);
        MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
          Result := ThreadHeap.GetMedium(B16Count, Ord(ma16Bytes));
      else
        Result := MemoryManager.BrainMM.GetMemoryPages(
          (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
          PAGESMODE_SYSTEM);
        { if (Result = nil) then
          Result := ThreadHeap.ErrorOutOfMemory; }
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownGetMem(nil, B16Count, ErrorAddr);
  end;
end;
{$else}
asm
  // ErrorAddr(v3) := nil
  {$ifdef CPUX86}
    xor ecx, ecx
    nop
    nop
  {$else .CPUX64}
    xor r8, r8
    nop
  {$endif}
@redirect:
  // v2 := (Size(v1) + 15) div 16
  {$ifdef CPUX86}
    lea edx, [eax + 15]
    shr edx, 4
  {$else .CPUX64}
    lea rdx, [rcx + 15]
    shr rdx, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz BrainMMUnknownGetMem
  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v3)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ecx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r8
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r8, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r8
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // case (B16Count) of
  //   Exit ThreadHeap.GetSmall(B16Count)
  //   Exit ThreadHeap.GetMedium(B16Count, Ord(ma16Bytes))
  //   Exit MemoryManager.BrainMM.GetMemoryPages(PagesOf(B16Count), PAGESMODE_SYSTEM)
  {$ifdef CPUX86}
    xor ecx, ecx
    cmp edx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    cmp edx, MAX_MEDIUM_B16COUNT
    jbe TThreadHeap.GetMedium
  {$else .CPUX64}
    xor r8, r8
    cmp rdx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    cmp rdx, MAX_MEDIUM_B16COUNT
    jbe TThreadHeap.GetMedium
  {$endif}

  {$ifdef CPUX86}
    lea eax, [edx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr eax, B16_PER_PAGE_SHIFT
  {$else .CPUX64}
    lea rcx, [rdx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr rcx, B16_PER_PAGE_SHIFT
  {$endif}
  call MemoryManager.BrainMM.GetMemoryPages
  {$ifdef CPUX86}
    test eax, eax
  {$else .CPUX64}
    test rax, rax
  {$endif}
  jz @error_outof_memory
  ret

@error_outof_memory:
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory

@penalty_thread_deferreds:
  {$ifdef CPUX86}
    push ebx
    lea ebx, @x86_thread_deferreds_ret
    jmp SafeProcessThreadDeferred
  @x86_thread_deferreds_ret:
    pop ebx
    jmp @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
    jmp SafeProcessThreadDeferred
  {$endif}
end;
{$endif}

function TThreadHeap.GetSmall(B16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
label
  item_reserve, new_k1line;
var
  Line, Next, N: PK1LineSmall;
  Index: NativeInt;
begin
  Line := Self.FK1LineSmalls[B16Count];

  if (Line <> nil) then
  begin
    if (Line.Header.ItemSet.VLow32 and 3 = 0{not FullQueue}) then
    begin
    item_reserve:
      Index := BitReserve(Line.Header.ItemSet);
      if (Index > 0) then
      begin
        Result := @Line.Items[Index];
        Exit;
      end else
      begin
        repeat
          Line.Header.ItemSet.VLow32 := 3{FullQueue := True};

          // Dequeue
          Next := Line.Header.Queue.Next;
          Self.FK1LineSmalls[B16Count] := Next;

          // Enqueue
          N := Self.QK1LineFull;
          Self.QK1LineFull := Line;
          Line.Header.Queue.Prev := nil;
          Line.Header.Queue.Next := N;
          if (N <> nil) then N.Header.Queue.Prev := Line;

          Line := Next;
          if (Next = nil) then goto new_k1line;
          Next.Header.Queue.Prev := nil;
        until (
          {$ifdef SMALLINT}
            Line.Header.ItemSet.VLow32 or Line.Header.ItemSet.VHigh32 <> 0
          {$else .LARGEINT}
            Line.Header.ItemSet.V64 <> 0
          {$endif}
          );
        goto item_reserve;
      end;
    end else
    begin
      Result := Pointer(Self.RaiseInvalidPtr);
      Exit;
    end;
  end else
  begin
  new_k1line:
    Result := Self.GetNewK1LineSmall(B16Count);
  end;
end;
{$else}
asm
  // Line := FK1LineSmalls[B16Count]
  // if (Line = nil) then Exit Self.GetNewK1LineSmall(B16Count)
  {$ifdef CPUX86}
    mov ecx, [EAX + edx * 4]
    test ecx, ecx
  {$else .CPUX64}
    mov r8, [RCX + rdx * 8]
    test r8, r8
  {$endif}
  jz TThreadHeap.GetNewK1LineSmall

  // if (Line.FullQueue) then Exit RaiseInvalidPtr;
  {$ifdef CPUX86}
    mov edx, [ECX].TK1LineSmall.Header.ItemSet.VLow32
    test edx, 3
  {$else .CPUX64}
    mov r9, [R8].TK1LineSmall.Header.ItemSet.V64
    test r9, 3
  {$endif}
  jnz TThreadHeap.RaiseInvalidPtr

  // if (Line.Header.ItemSet.VInt64 <> 0{deferred Full}) then Index := BitReserve(Line.ItemSet)
  // else RequeueLine(FK1LineSmalls, QK1LineFull), FindNextLine;
@item_reserve:
  {$ifdef CPUX86}
    lea eax, [ECX].TK1LineSmall.Header.ItemSet.VHigh32
    test edx, edx
    DB $0F, $44, $C8 // cmovz ecx, eax
    mov edx, [ecx]
    bsf eax, edx
    jz @requeue_line_loop_prefix
    btr edx, eax
    mov [ecx], edx
    lea edx, [eax + 32]
    test ecx, MASK_K1_TEST
    DB $0F, $45, $C2 // cmovnz eax, edx
  {$else .CPUX64}
    bsf rax, r9
    jz @requeue_line_loop_prefix
    btr r9, rax
    mov [R8].TK1LineSmall.Header.ItemSet.V64, r9
  {$endif}

  // Result := @Line.Items[Index]
  {$ifdef CPUX86}
    add eax, eax
    and ecx, MASK_K1_CLEAR
    lea eax, [ecx + eax * 8]
  {$else .CPUX64}
    add rax, rax
    lea rax, [r8 + rax * 8]
  {$endif}
  ret
@requeue_line_loop_prefix:
{$ifdef CPUX86}
  // store ebx/esi, retrieve Line, ThreadHeap, B16Count
  mov [esp - 8], ebx
  mov [esp - 4], esi
  and ecx, MASK_K1_CLEAR
  movzx edx, byte ptr [ECX].TK1LineSmall.Header.Flags
  shr edx, 4
  mov eax, MASK_K64_CLEAR
  and eax, ecx
  mov eax, [EAX].TK64PoolSmall.ThreadHeap
{$endif}
@requeue_line_loop:
  // mark as FullQueue
  {$ifdef CPUX86}
    mov [ECX].TK1LineSmall.Header.ItemSet.VLow32, 3
  {$else .CPUX64}
    mov [R8].TK1LineSmall.Header.ItemSet.VLow32, 3
  {$endif}

  // dequeue, enqueue (during optional fake next LineFull)
  {$ifdef CPUX86}
    mov ebx, [ECX].TK1LineSmall.Header.Queue.Next
    mov esi, [EAX].TThreadHeap.QK1LineFull
    mov [EAX + edx * 4], ebx
    mov [EAX].TThreadHeap.QK1LineFull, ecx

    mov [ECX].TK1LineSmall.Header.Queue.Prev, 0
    mov [ECX].TK1LineSmall.Header.Queue.Next, esi
    lea esp, [esp - 64]
    test esi, esi
    DB $0F, $44, $F4 // cmovz esi, esp
    lea esp, [esp + 64]
    mov [ESI].TK1LineSmall.Header.Queue.Prev, ecx
  {$else .CPUX64}
    mov r9, [R8].TK1LineSmall.Header.Queue.Next
    mov r10, [RCX].TThreadHeap.QK1LineFull
    mov [RCX + rdx * 8], r9
    mov [RCX].TThreadHeap.QK1LineFull, R8

    xor rax, rax
    mov [R8].TK1LineSmall.Header.Queue.Prev, rax
    mov [R8].TK1LineSmall.Header.Queue.Next, r10
    lea rax, [rsp - 64]
    test r10, r10
    cmovz r10, rax
    mov [R10].TK1LineSmall.Header.Queue.Prev, r8
  {$endif}

  // Line := Next;
  // if (Line = nil) then Exit Self.GetNewK1LineSmall(B16Count);
  {$ifdef CPUX86}
    test ebx, ebx
    xchg ecx, ebx
    mov ebx, [esp - 8]
    mov esi, [esp - 4]
  {$else .CPUX64}
    test r9, r9
    xchg r8, r9
  {$endif}
  jz TThreadHeap.GetNewK1LineSmall

  // Line.Header.Queue.Prev := nil;
  // if (Line.Header.ItemSet.V64 = 0) then Continue;
  {$ifdef CPUX86}
    mov [ECX].TK1LineSmall.Header.Queue.Prev, 0
    mov ebx, [ECX].TK1LineSmall.Header.ItemSet.VLow32
    mov esi, [ECX].TK1LineSmall.Header.ItemSet.VHigh32
    or esi, ebx
  {$else .CPUX64}
    xor rax, rax
    mov [R8].TK1LineSmall.Header.Queue.Prev, rax
    mov r9, [R8].TK1LineSmall.Header.ItemSet.V64
    test r9, r9
  {$endif}
  jz @requeue_line_loop

  // if (Line.FullQueue) then Exit RaiseInvalidPtr;
  // goto @item_reserve;
  {$ifdef CPUX86}
    test ebx, 3
    xchg edx, ebx
    mov ebx, [esp - 8]
    mov esi, [esp - 4]
  {$else .CPUX64}
    test r9, 3
  {$endif}
  jz @item_reserve
  jmp TThreadHeap.RaiseInvalidPtr
end;
{$endif}

function BrainMMUnknownAllocMem(None: Pointer; B16Count: NativeUInt;
  ErrorAddr: Pointer): Pointer;
begin
  Result := BrainMMUnknownGetMem(None, B16Count, ErrorAddr);
  if (Result <> nil) then
    System.FillChar(Result^, B16Count shl 4, #0);
end;

function BrainMMAllocMem(Size: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
label
  thread_deferreds_done;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  B16Count: NativeUInt;
begin
  B16Count := (Size + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      case (B16Count) of
        0..MAX_SMALL_B16COUNT: Result := ThreadHeap.GetSmall(B16Count);
        MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
          Result := ThreadHeap.GetMedium(B16Count, Ord(ma16Bytes));
      else
        Result := MemoryManager.BrainMM.GetMemoryPages(
          (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
          PAGESMODE_SYSTEM);
        { if (Result = nil) then
          Result := ThreadHeap.ErrorOutOfMemory; }
      end;

      if (Result <> nil) then
        System.FillChar(Result^, B16Count shl 4, #0);
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownAllocMem(nil, B16Count, ErrorAddr);
  end;
end;
{$else}
asm
  // ErrorAddr(v3) := nil
  {$ifdef CPUX86}
    xor ecx, ecx
    nop
    nop
  {$else .CPUX64}
    xor r8, r8
    nop
  {$endif}
@redirect:
  // v2 := (Size(v1) + 15) div 16
  {$ifdef CPUX86}
    lea edx, [eax + 15]
    shr edx, 4
  {$else .CPUX64}
    lea rdx, [rcx + 15]
    shr rdx, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz BrainMMUnknownAllocMem

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v3)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ecx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r8
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r8, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r8
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // store B16Count
  // case (B16Count) of
  //   call ThreadHeap.GetSmall(B16Count)
  //   call ThreadHeap.GetMedium(B16Count, Ord(ma16Bytes))
  //   call MemoryManager.BrainMM.GetMemoryPages(PagesOf(B16Count), PAGESMODE_SYSTEM)
  {$ifdef CPUX86}
    push edx
    push offset @fill_zero
    xor ecx, ecx
    cmp edx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    cmp edx, MAX_MEDIUM_B16COUNT
    jbe TThreadHeap.GetMedium
  {$else .CPUX64}
    push rdx
    lea r9, @fill_zero
    push r9
    xor r8, r8
    cmp rdx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    cmp rdx, MAX_MEDIUM_B16COUNT
    jbe TThreadHeap.GetMedium
  {$endif}

  {$ifdef CPUX86}
    lea eax, [edx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr eax, B16_PER_PAGE_SHIFT
    pop ecx
  {$else .CPUX64}
    lea rcx, [rdx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr rcx, B16_PER_PAGE_SHIFT
    pop r8
  {$endif}
  call MemoryManager.BrainMM.GetMemoryPages
  {$ifdef CPUX86}
    test eax, eax
  {$else .CPUX64}
    test rax, rax
  {$endif}
  jnz @fill_zero
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  call TThreadHeap.ErrorOutOfMemory

@fill_zero:
  // retrieve B16Count
  // if (Result = nil) then Exit
  {$ifdef CPUX86}
    pop ecx
    test eax, eax
  {$else .CPUX64}
    pop rcx
    test rax, rax
  {$endif}
  jz @done

  // FillChar(Result^, B16Count * 16, #0);
  {$ifdef CPUX86}
    mov edx, edi
    xchg eax, edi
    shl ecx, 2
    xor eax, eax
    rep STOSD
    mov ecx, [esp - 4]
  {$else .CPUX64}
    mov r8, rcx
    mov rdx, rdi
    xchg rax, rdi
    add rcx, rcx
    xor rax, rax
    rep STOSQ
  {$endif}

  // Result := edi/rdi - B16Count * 16, restore edi/rdi (from edx/rdx)
  {$ifdef CPUX86}
    shl ecx, 4
    xchg eax, edx
    sub edi, ecx
    xchg eax, edi
  {$else .CPUX64}
    shl r8, 4
    xchg rax, rdx
    sub rdi, r8
    xchg rax, rdi
  {$endif}

@done:
  ret

@penalty_thread_deferreds:
  {$ifdef CPUX86}
    push ebx
    lea ebx, @x86_thread_deferreds_ret
    jmp SafeProcessThreadDeferred
  @x86_thread_deferreds_ret:
    pop ebx
    jmp @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
    jmp SafeProcessThreadDeferred
  {$endif}
end;
{$endif}

function BrainMMUnknownGetMemAligned(Align: TMemoryAlign; B16Count: NativeUInt;
  ErrorAddr: Pointer): Pointer;
label
  medium;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // allocation
    ThreadHeap.ErrorAddr := ErrorAddr;
    case (B16Count) of
      0..MAX_SMALL_B16COUNT:
      begin
        if (Align <> ma16Bytes) then goto medium;
        Result := ThreadHeap.GetSmall(B16Count);
      end;
      MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
      begin
      medium:
        Result := ThreadHeap.GetMedium(B16Count, Ord(Align));
      end
    else
      Result := MemoryManager.BrainMM.GetMemoryPages(
        (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
        PAGESMODE_SYSTEM);
      if (Result = nil) then
        Result := ThreadHeap.ErrorOutOfMemory;
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMGetMemAligned(Align: TMemoryAlign; Size: NativeUInt): Pointer;
label
  thread_deferreds_done, medium;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  B16Count: NativeUInt;
begin
  B16Count := (Size + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      case (B16Count) of
        0..MAX_SMALL_B16COUNT:
        begin
          if (Align <> ma16Bytes) then goto medium;
          Result := ThreadHeap.GetSmall(B16Count);
        end;
        MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
        begin
        medium:
          Result := ThreadHeap.GetMedium(B16Count, Ord(Align));
        end
      else
        Result := MemoryManager.BrainMM.GetMemoryPages(
          (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
          PAGESMODE_SYSTEM);
        { if (Result = nil) then
          Result := ThreadHeap.ErrorOutOfMemory; }
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownGetMemAligned(Align, B16Count, ErrorAddr);
  end;
end;

function BrainMMUnknownFreeMem(None: Pointer; P: Pointer;
  ErrorAddr: Pointer): Integer;
label
  medium, error_invalid_ptr;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // dispose
    ThreadHeap.ErrorAddr := ErrorAddr;
    if (NativeInt(P) and MASK_16_TEST = 0) then
    begin
      Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

      if (NativeInt(P) and MASK_K1_TEST <> 0) then
      begin
        // pool: small or medium
        if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
        begin
          // pool small
          Result := ThreadHeap.FreeSmall(P);
          Exit;
        end else
        if (PK64PoolSmall(Pool).ThreadHeap = nil) then
        begin
          // pool medium
        medium:
          if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
          begin
            Result := ThreadHeap.FreeMedium(P);
            Exit;
          end;
        end;
      end else
      begin
        if (NativeInt(P) and MASK_K4_TEST <> 0) then
        begin
          // medium or invalid pointer
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
          goto error_invalid_ptr;
        end else
        begin
          // big or large
          if (not MemoryManager.BrainMM.FreeMemoryPages(MemoryPages(P), PAGESMODE_SYSTEM)) then
            goto error_invalid_ptr;

          Result := FREEMEM_DONE;
          Exit;
        end;
      end;

      // "default" method
      Result := ThreadHeap.FreeDifficult(P, ErrorAddr);
      Exit;
    end else
    begin
    error_invalid_ptr:
      Result := ThreadHeap.ErrorInvalidPtr;
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMFreeMem(P: Pointer): Integer;
{$ifdef PUREPASCAL}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @BrainMMFreeMem;
{$endif}
label
  thread_deferreds_done, medium, error_invalid_ptr;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
begin
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      if (NativeInt(P) and MASK_16_TEST = 0) then
      begin
        Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

        if (NativeInt(P) and MASK_K1_TEST <> 0) then
        begin
          // pool: small or medium
          if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
          begin
            // pool small
            Result := ThreadHeap.FreeSmall(P);
            Exit;
          end else
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then
          begin
            // pool medium
          medium:
            if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
            begin
              Result := ThreadHeap.FreeMedium(P);
              Exit;
            end;
          end;
        end else
        begin
          if (NativeInt(P) and MASK_K4_TEST <> 0) then
          begin
            // medium or invalid pointer
            if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
            goto error_invalid_ptr;
          end else
          begin
            // big or large
            if (not MemoryManager.BrainMM.FreeMemoryPages(MemoryPages(P), PAGESMODE_SYSTEM)) then
              goto error_invalid_ptr;

            Result := FREEMEM_DONE;
            Exit;
          end;
        end;

        // "default" method
        Result := ThreadHeap.FreeDifficult(P, ReturnAddress);
        Exit;
      end else
      begin
      error_invalid_ptr:
        Result := ThreadHeap.ErrorInvalidPtr;
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownFreeMem(nil, P, ErrorAddr);
  end;
end;
{$else}
asm
  // ErrorAddr(v3) := nil
  {$ifdef CPUX86}
    xor ecx, ecx
    nop
    nop
  {$else .CPUX64}
    xor r8, r8
    nop
  {$endif}
@redirect:
  // v2 := P (v1)
  {$ifdef CPUX86}
    xchg eax, edx
  {$else .CPUX64}
    xchg rcx, rdx
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz BrainMMUnknownFreeMem

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v3)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ecx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r8
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r8, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r8
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // if (P (v2) and MASK_16_TEST <> 0) then Exit ThreadHeap.ErrorInvalidPtr
  {$ifdef CPUX86}
    test edx, MASK_16_TEST
  {$else .CPUX64}
    test rdx, MASK_16_TEST
  {$endif}
  jnz TThreadHeap.ErrorInvalidPtr

  // v3 := PK64PoolSmall/PK64PoolMedium(P)
  {$ifdef CPUX86}
    mov ecx, edx
    and ecx, MASK_K64_CLEAR
  {$else .CPUX64}
    mov r8, rdx
    and r8, MASK_K64_CLEAR
  {$endif}

  // if (P(v2) and MASK_K1_TEST = 0) then Not Small
  {$ifdef CPUX86}
    test edx, MASK_K1_TEST
  {$else .CPUX64}
    test rdx, MASK_K1_TEST
  {$endif}
  jz @not_small

  // if (PK64PoolSmall(P).ThreadHeap = @Self) then Exit ThreadHeap.FreeSmall(P)
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolSmall.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R8].TK64PoolSmall.ThreadHeap, rcx
  {$endif}
  je TThreadHeap.FreeSmall

  // if (PK64PoolSmall(P).ThreadHeap <> nil) then Exit ThreadHeap.FreeDifficult(P, ReturnAddress)
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolSmall.ThreadHeap, 0
    jne @free_difficult
  {$else .CPUX64}
    cmp [R8].TK64PoolSmall.ThreadHeap, 0
    jne @free_difficult
  {$endif}

@medium:
  // if (PK64PoolMedium(P).ThreadHeap = @Self) then Exit ThreadHeap.FreeMedium(P)
  // else Exit ThreadHeap.FreeDifficult
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolMedium.ThreadHeap, eax
    je TThreadHeap.FreeMedium
  {$else .CPUX64}
    cmp [R8].TK64PoolMedium.ThreadHeap, rcx
    je TThreadHeap.FreeMedium
  {$endif}
  jmp @free_difficult

@not_small:
  // if (P(v2) and MASK_K4_TEST = 0) then
  //   MemoryManager.BrainMM.FreeMemoryPages(P, PAGESMODE_SYSTEM)
  {$ifdef CPUX86}
    test edx, MASK_K4_TEST
  {$else .CPUX64}
    test rdx, MASK_K4_TEST
  {$endif}
  jz @free_big_large

  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else ThreadHeap.ErrorInvalidPtr
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R8].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  je @medium
  jmp TThreadHeap.ErrorInvalidPtr

@free_big_large:
  {$ifdef CPUX86}
    mov eax, edx
  {$else .CPUX64}
    mov rcx, rdx
  {$endif}
  mov edx, PAGESMODE_SYSTEM
  call MemoryManager.BrainMM.FreeMemoryPages
  test al, al
  jz @error_invalid_ptr
  {$ifdef CPUX86}
    mov eax, FREEMEM_DONE
  {$else .CPUX64}
    mov rax, FREEMEM_DONE
  {$endif}
  ret

@error_invalid_ptr:
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr

@free_difficult:
  {$ifdef CPUX86}
    mov ecx, [esp]
  {$else .CPUX64}
    mov r8, [rsp]
  {$endif}
  jmp TThreadHeap.FreeDifficult
@penalty_thread_deferreds:
  {$ifdef CPUX86}
    push ebx
    lea ebx, @x86_thread_deferreds_ret
    jmp SafeProcessThreadDeferred
  @x86_thread_deferreds_ret:
    pop ebx
    jmp @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
    jmp SafeProcessThreadDeferred
  {$endif}
end;
{$endif}

function TThreadHeap.FreeSmall(P: Pointer): Integer;
{$ifdef PUREPASCAL}
var
  Line, Prev, Next: PK1LineSmall;
  Index, B16Count: NativeInt;

  Mask: NativeInt;
  {$ifdef SMALLINT}
  PVInteger: PInteger;
  {$endif}
begin
  Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
  Index := (NativeInt(P) and MASK_K1_TEST) shr 4;

  if (Line.Header.ItemSet.VLow32 and 3 = 0{not FullQueue}) then
  begin
    if (BitUnreserve(Line.Header.ItemSet, Index)) then
    begin
      if (Line.Header.ItemSet.V64 <> DEFAULT_BITSETS_SMALL[Line.Header.Flags and 15]) then
      begin
        Result := FREEMEM_DONE;
        Exit;
      end else
      begin
        Result := Self.DisposeK1LineSmall(Line);
        Exit;
      end;
    end else
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;
  end else
  begin
    // Dequeue QK1LineFull
    Prev := Line.Header.Queue.Prev;
    Next := Line.Header.Queue.Next;
    if (Prev = nil) then
    begin
      Self.QK1LineFull := Next;
    end else
    begin
      Prev.Header.Queue.Next := Next;
    end;
    if (Next <> nil) then
    begin
      Next.Header.Queue.Prev := Prev;
    end;

    // Enqueue
    B16Count := NativeUInt(Line.Header.ModeSize) shr 4;
    Next := Self.FK1LineSmalls[B16Count];
    Self.FK1LineSmalls[B16Count] := Line;
    Line.Header.Queue.Prev := nil;
    Line.Header.Queue.Next := Next;
    if (Next <> nil) then Next.Header.Queue.Prev := Line;

    // full bitset, unreserve
    Mask := 1;
    {$ifdef LARGEINT}
      Mask := Mask shl Index;
      Line.Header.ItemSet.V64 := Mask;
    {$else .SMALLINT}
      Mask := Mask shl (Index and 31);
      PVInteger := @Line.Header.ItemSet.VIntegers[Byte(Index > 31)];
      Line.Header.ItemSet.VLow32{V64} := 0;
      PVInteger^ := Mask;
    {$endif}
    Result := FREEMEM_DONE;
  end;
end;
{$else}
asm
  // Line := LineOf(P)
  // Index := IndexOf(Line, P)
  {$ifdef CPUX86}
    mov ecx, edx
    and edx, MASK_K1_CLEAR
    and ecx, MASK_K1_TEST
    shr ecx, 4
  {$else .CPUX64}
    mov r8, rdx
    and rdx, MASK_K1_CLEAR
    and r8, MASK_K1_TEST
    shr r8, 4
  {$endif}

  // if (Line.FullQueue) then Exit Self.PenaltyFreeSmall(@Line.Items[Index])
  // if (not BitUnreserve(Index)) then Exit Self.ErrorInvalidPtr
  {$ifdef CPUX86}
    test [EDX].TK1LineSmall.Header.ItemSet.VLow32, 3
    jnz @penalty_requeue
    lea eax, [EDX].TK1LineSmall.Header.ItemSet.VHigh32
    test ecx, 32
    DB $0F, $45, $D0 // cmovnz edx, eax
    and ecx, 31
    mov eax, [edx]
    bts eax, ecx
    mov [edx], eax
    jc @penalty_error
  {$else .CPUX64}
    mov r9, [RDX].TK1LineSmall.Header.ItemSet.V64
    test r9, 3
    jnz @penalty_requeue
    bts r9, r8
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, r9
    jc @penalty_error
  {$endif}

  // if (Line.Header.ItemSet.V64 = DEFAULT_BITSETS_SMALL[BitSetKind(Line)]) then
  // Exit Self.DisposeK1LineSmall(Line)
  {$ifdef CPUX86}
    and edx, MASK_K1_CLEAR
    mov ecx, 15
    and ecx, [EDX].TK1LineSmall.Header.Flags
    mov eax, [offset DEFAULT_BITSETS_SMALL + ecx * 8]
    mov ecx, [offset DEFAULT_BITSETS_SMALL + ecx * 8 + 4]
    sub eax, [edx]
    sub ecx, [edx + 4]
    or eax, ecx
    jz @penalty_dispose_line
  {$else .CPUX64}
    mov rax, [RDX].TK1LineSmall.Header.Flags
    and rax, 15
    lea r8, DEFAULT_BITSETS_SMALL
    cmp r9, [r8 + rax * 8]
    je @penalty_dispose_line
  {$endif}

  // Result
  mov eax, FREEMEM_DONE
  ret

@penalty_error:
  {$ifdef CPUX86}
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr
@penalty_dispose_line:
  {$ifdef CPUX86}
    mov eax, edx
    and eax, MASK_K64_CLEAR
    mov eax, [EAX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.DisposeK1LineSmall
@penalty_requeue:
  // store Self, x86: store esi/edi
  // dequeue Self.QK1LineFull
  {$ifdef CPUX86}
    push esi
    push edi
    push eax

    mov esi, [EDX].TK1LineSmall.Header.Queue.Prev
    mov edi, [EDX].TK1LineSmall.Header.Queue.Next
    lea eax, [EAX + TThreadHeap.QK1LineFull - TK1LineSmall.Header.Queue.Next]
    test esi, esi
    DB $0F, $45, $C6 // cmovnz eax, esi
    mov [EAX].TK1LineSmall.Header.Queue.Next, edi
    lea eax, [esp - 64]
    test edi, edi
    DB $0F, $45, $C7 // cmovnz eax, edi
    mov [EAX].TK1LineSmall.Header.Queue.Prev, esi
  {$else .CPUX64}
    mov rax, rcx

    mov r9, [RDX].TK1LineSmall.Header.Queue.Prev
    mov r10, [RDX].TK1LineSmall.Header.Queue.Next
    lea rcx, [RCX + TThreadHeap.QK1LineFull - TK1LineSmall.Header.Queue.Next]
    test r9, r9
    cmovnz rcx, r9
    mov [RCX].TK1LineSmall.Header.Queue.Next, r10
    lea rcx, [rsp - 64]
    test r10, r10
    cmovnz rcx, r10
    mov [RCX].TK1LineSmall.Header.Queue.Prev, r9
  {$endif}

  // restore Self
  // enqueue Self.FK1LineSmalls
  // v4 := 0
  {$ifdef CPUX86}
    pop eax
    add eax, 4
    mov esi, [EDX].TK1LineSmall.Header.Flags
    and esi, 7
    mov edi, [EAX + esi * 4]
    mov [EAX + esi * 4], edx
    xor esi, esi
    mov [EDX].TK1LineSmall.Header.Queue.Prev, esi
    mov [EDX].TK1LineSmall.Header.Queue.Next, edi
    lea eax, [esp - 64]
    test edi, edi
    DB $0F, $45, $C7 // cmovnz eax, edi
    mov [EAX].TK1LineSmall.Header.Queue.Prev, edx
  {$else .CPUX64}
    lea rcx, [rax + 8]
    mov r9, [RDX].TK1LineSmall.Header.Flags
    and r9, 7
    mov r10, [RCX + r9 * 8]
    mov [RCX + r9 * 8], rdx
    xor r9, r9
    mov [RDX].TK1LineSmall.Header.Queue.Prev, r9
    mov [RDX].TK1LineSmall.Header.Queue.Next, r10
    lea rcx, [rsp - 64]
    test r10, r10
    cmovnz rcx, r10
    mov [RCX].TK1LineSmall.Header.Queue.Prev, rdx
  {$endif}

  // full bitset, unreserve (v4 = 0)
  {$ifdef CPUX86}
    mov [EDX].TK1LineSmall.Header.ItemSet.VLow32, esi
    lea edi, [EDX].TK1LineSmall.Header.ItemSet.VHigh32
    { lea edx, [EDX].TK1LineSmall.Header.ItemSet.VLow32 }
    test ecx, 32
    DB $0F, $45, $D7 // cmovnz edx, edi
    and ecx, 31
    mov edi, [edx]
    bts edi, ecx
    mov [edx], edi
    pop edi
    pop esi
  {$else .CPUX64}
    bts r9, r8
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, r9
  {$endif}

  // Result
  mov eax, FREEMEM_DONE
end;
{$endif}

function BrainMMUnknownRegetMem(ErrorAddr: Pointer; P: Pointer;
  NewB16Count: NativeUInt; ReturnAddress: Pointer): Pointer;
label
  return_made_none, medium, raise_invalid_ptr;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
  LastB16Count: NativeUInt;
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // reget
    ThreadHeap.ErrorAddr := ErrorAddr;
    if (NativeInt(P) and MASK_16_TEST = 0) then
    begin
      Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

      if (NativeInt(P) and MASK_K1_TEST <> 0) then
      begin
        // pool: small or medium
        if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
        begin
          // pool small
          NewB16Count := {NewSize}NewB16Count shl 4;
          if (PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize >= {NewSize}NewB16Count) then
          begin
          return_made_none:
            Result := P;
            Exit;
          end else
          begin
            NewB16Count := {retrieve}NewB16Count shr 4;
            if (NewB16Count <= MAX_SMALL_B16COUNT) then
            begin
              Result := ThreadHeap.RegrowSmallToSmall(P, NewB16Count);
              Exit;
            end;
          end;
        end else
        if (PK64PoolSmall(Pool).ThreadHeap = nil) then
        begin
          // pool medium
        medium:
          if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
          begin
            LastB16Count := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).B16Count;
            if (NewB16Count <= LastB16Count) then
            begin
              if (NewB16Count = LastB16Count) then goto return_made_none;
              Result := ThreadHeap.ReduceMedium(P, NewB16Count);
              Exit;
            end;
            if (NewB16Count <= MAX_MEDIUM_B16COUNT) then
            begin
              Result := ThreadHeap.RegetMediumToMedium(P, NewB16Count);
              Exit;
            end;
          end;
        end;
      end else
      begin
        if (NativeInt(P) and MASK_K4_TEST <> 0) then
        begin
          // medium or invalid pointer
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
          goto raise_invalid_ptr;
        end else
        begin
          // big or large
          Result := MemoryManager.BrainMM.RegetMemoryPages(MemoryPages(P),
            (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, PAGESMODE_SYSTEM);
          if (NativeUInt(Result) <= NativeUInt(PTR_INVALID)) then
          begin
            if (Result = PTR_INVALID) then goto raise_invalid_ptr;
            Result := ThreadHeap.ErrorOutOfMemory;
          end;

          Exit;
        end;
      end;

      // "default" method
      Result := ThreadHeap.RegetDifficult(P, NewB16Count, ReturnAddress);
      Exit;
    end else
    begin
    raise_invalid_ptr:
      Result := Pointer(ThreadHeap.RaiseInvalidPtr);
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMRegetMem(P: Pointer; NewSize: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @BrainMMRegetMem;
{$endif}
label
  thread_deferreds_done, return_made_none,
  medium, raise_invalid_ptr;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
  LastB16Count, NewB16Count: NativeUInt;
begin
  NewB16Count := (NewSize + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      if (NativeInt(P) and MASK_16_TEST = 0) then
      begin
        Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

        if (NativeInt(P) and MASK_K1_TEST <> 0) then
        begin
          // pool: small or medium
          if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
          begin
            // pool small
            NewB16Count := {NewSize}NewB16Count shl 4;
            if (PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize >= {NewSize}NewB16Count) then
            begin
            return_made_none:
              Result := P;
              Exit;
            end else
            begin
              NewB16Count := {retrieve}NewB16Count shr 4;
              if (NewB16Count <= MAX_SMALL_B16COUNT) then
              begin
                Result := ThreadHeap.RegrowSmallToSmall(P, NewB16Count);
                Exit;
              end;
            end;
          end else
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then
          begin
            // pool medium
          medium:
            if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
            begin
              LastB16Count := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).B16Count;
              if (NewB16Count <= LastB16Count) then
              begin
                if (NewB16Count = LastB16Count) then goto return_made_none;
                Result := ThreadHeap.ReduceMedium(P, NewB16Count);
                Exit;
              end;
              if (NewB16Count <= MAX_MEDIUM_B16COUNT) then
              begin
                Result := ThreadHeap.RegetMediumToMedium(P, NewB16Count);
                Exit;
              end;
            end;
          end;
        end else
        begin
          if (NativeInt(P) and MASK_K4_TEST <> 0) then
          begin
            // medium or invalid pointer
            if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
            goto raise_invalid_ptr;
          end else
          begin
            // big or large
            Result := MemoryManager.BrainMM.RegetMemoryPages(MemoryPages(P),
              (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, PAGESMODE_SYSTEM);
            if (NativeUInt(Result) <= NativeUInt(PTR_INVALID)) then
            begin
              if (Result = PTR_INVALID) then goto raise_invalid_ptr;
              Result := ThreadHeap.ErrorOutOfMemory;
            end;

            Exit;
          end;
        end;

        // "default" method
        Result := ThreadHeap.RegetDifficult(P, NewB16Count, ReturnAddress);
        Exit;
      end else
      begin
      raise_invalid_ptr:
        Result := Pointer(ThreadHeap.RaiseInvalidPtr);
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownRegetMem(ErrorAddr, P, NewB16Count, ReturnAddress);
  end;
end;
{$else}
asm
  // var P(v3) := fake
  // ErrorAddr(v4) := nil
  {$ifdef CPUX86}
    lea ecx, [esp - 8]
    push ebx
    xor ebx, ebx
    nop
  {$else .CPUX64}
    lea r8, [rsp - 8]
    xor r9, r9
  {$endif}
@redirect:
  // store var P (v3), v3 := (NewSize(v2) + 15) div 16, v2 := const P (v1)
  {$ifdef CPUX86}
    // stack := var P
    push ecx
    lea ecx, [edx + 15]
    xchg edx, eax
    shr ecx, 4
  {$else .CPUX64}
    // v5 := var P
    mov r10, r8
    lea r8, [rdx + 15]
    xchg rdx, rcx
    shr r8, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz @return_unknown

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v4)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ebx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r9
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r9, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r9
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // if (P (v2) and MASK_16_TEST <> 0) then ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    test edx, MASK_16_TEST
  {$else .CPUX64}
    test rdx, MASK_16_TEST
  {$endif}
  jnz @raise_invalid_ptr

  // v4 := PK64PoolSmall/PK64PoolMedium(P)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K64_CLEAR
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K64_CLEAR
  {$endif}

  // if (P(v2) and MASK_K1_TEST = 0) then Not Small
  {$ifdef CPUX86}
    test edx, MASK_K1_TEST
  {$else .CPUX64}
    test rdx, MASK_K1_TEST
  {$endif}
  jz @not_small

  // if (PK64PoolSmall(P).ThreadHeap <> @Self) then Not Fast
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, rcx
  {$endif}
  jne @not_fast

  // if (PK1LineSmall(P).ModeSize >= {NewSize}NewB16Count * 16) then Exit P (return made none)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K1_CLEAR
    shl ecx, 4
    movzx ebx, byte ptr TK1LineSmall[EBX].Header.ModeSize
    cmp ebx, ecx
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K1_CLEAR
    shl r8, 4
    movzx r9, byte ptr TK1LineSmall[R9].Header.ModeSize
    cmp r9, r8
  {$endif}
  jb @small_grow
@return_made_none:
  // Result := const P (v2)
  {$ifdef CPUX86}
    mov eax, edx
    pop ecx
    pop ebx
  {$else .CPUX64}
    mov rax, rdx
  {$endif}
  ret

@small_grow:
  // retrieve NewB16Count
  // if (NewSize > MAX_SMALL_SIZE) then Exit ThreadHeap.RegetDifficult
  {$ifdef CPUX86}
    mov ebx, ecx
    shr ecx, 4
    cmp ebx, MAX_SMALL_SIZE
  {$else .CPUX64}
    mov r9, r8
    shr r8, 4
    cmp r9, MAX_SMALL_SIZE
  {$endif}
  ja @reget_difficult

  // Exit (recall) ThreadHeap.RegrowSmallToSmall
  {$ifdef CPUX86}
    cmp esp, [esp]
    mov ebx, [esp + 4]
    lea esp, [esp + 8]
  {$else .CPUX64}
    lea r9, [rsp - 8]
    cmp r9, r10
  {$endif}
  je TThreadHeap.RegrowSmallToSmall
  {$ifdef CPUX86}
    lea esp, [esp - 8]
    push offset @return_var_p
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
  {$endif}
  jmp TThreadHeap.RegrowSmallToSmall

@not_fast:
  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else Exit RegetDifficult
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  jnz @reget_difficult

@medium:
  // if (PK64PoolMedium(P).ThreadHeap <> @Self) then Exit ThreadHeap.RegetDifficult
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolMedium.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolMedium.ThreadHeap, rcx
  {$endif}
  jne @reget_difficult

  // if (NewB16Count > MAX_MEDIUM_B16COUNT) then Exit ThreadHeap.RegetDifficult
  {$ifdef CPUX86}
    cmp ecx, MAX_MEDIUM_B16COUNT
  {$else .CPUX64}
    cmp r8, MAX_MEDIUM_B16COUNT
  {$endif}  
  ja @reget_difficult
  
  // Compare: PHeaderMedium(P).B16Count ~ NewB16Count
  // if = then Exit P (return made none)
  // if < then Exit ThreadHeap.RegetMediumToMedium
  // if > then Exit ThreadHeap.ReduceMedium
  {$ifdef CPUX86}
    cmp [EDX - 16].THeaderMedium.B16Count, cx
  {$else .CPUX64}
    cmp [RDX - 16].THeaderMedium.B16Count, r8w
  {$endif}
  je @return_made_none
  {$ifdef CPUX86}
    push offset @return_var_p
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
  {$endif}
  jb TThreadHeap.RegetMediumToMedium
  jmp TThreadHeap.ReduceMedium

@not_small:
  // if (P(v2) and MASK_K4_TEST = 0) then BigOrLarge
  {$ifdef CPUX86}
    test edx, MASK_K4_TEST
  {$else .CPUX64}
    test rdx, MASK_K4_TEST
  {$endif}
  jz @reget_big_large

  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  je @medium
  jmp @raise_invalid_ptr

@reget_big_large:
  // Exit MemoryManager.BrainMM.RegetMemoryPages(P, PagesOf(NewB16Count), PAGESMODE_SYSTEM);
  {$ifdef CPUX86}
    mov eax, edx
    lea edx, [ecx + B16_PER_PAGE - 1]
    mov ecx, PAGESMODE_SYSTEM
    shr edx, B16_PER_PAGE_SHIFT
  {$else .CPUX64}
    mov rcx, rdx
    lea rdx, [r8 + B16_PER_PAGE - 1]
    mov r8d, PAGESMODE_SYSTEM
    shr rdx, B16_PER_PAGE_SHIFT
    push r10
  {$endif}
  call MemoryManager.BrainMM.RegetMemoryPages
  {$ifdef CPUX86}
    cmp eax, 1 // PTR_INVALID
  {$else .CPUX64}
    cmp rax, 1 // PTR_INVALID
  {$endif}
  ja @return_var_p
  je @raise_invalid_pages
  {$ifdef CPUX86}
    push offset @return_var_p
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    lea r9, @return_var_p
    push r9
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory

@reget_difficult:
  {$ifdef CPUX86}
    push dword ptr [esp + 8]
  {$else .CPUX64}
    mov r9, [rsp]
    push r10
  {$endif}
  call TThreadHeap.RegetDifficult
@return_var_p:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
    mov [ecx], eax
  {$else .CPUX64}
    pop rcx
    mov [rcx], rax
  {$endif}
  ret
@return_unknown:
  {$ifdef CPUX86}
    mov eax, ebx
    push dword ptr [esp + 8]
    push offset @return_var_p
  {$else .CPUX64}
    mov rcx, r9
    mov r9, [rsp]
    push r10
    lea r10, @return_var_p
    push r10
  {$endif}
  jmp BrainMMUnknownRegetMem
@raise_invalid_pages:
  {$ifdef CPUX64}
    pop r10
  {$endif}
@raise_invalid_ptr:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
  {$endif}
  jmp TThreadHeap.RaiseInvalidPtr
@penalty_thread_deferreds:
  {$ifdef CPUX86}
    lea ebx, @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
  {$endif}
  jmp SafeProcessThreadDeferred
end;
{$endif}

function TThreadHeap.RegrowSmallToSmall(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
begin
  if (Self.FreeSmall(P) {$ifdef FPC}={$else}<>{$endif} 0) then
  begin
    Result := Pointer(RaiseInvalidPtr);
  end else
  begin
    Result := Self.GetSmall(NewB16Count);
  end;
end;
{$else}
asm
  {$ifdef CPUX86}
    push eax
    push ecx
      call TThreadHeap.FreeSmall
    test eax, eax
    pop edx
    pop eax
  {$else .CPUX64}
    push rcx
    push r8
    {stack align} push rcx
       call TThreadHeap.FreeSmall
    {stack align} pop rcx
    test eax, eax
    pop rdx
    pop rcx
  {$endif}

  {$ifNdef FPC}
    jz TThreadHeap.GetSmall
  {$else .FPC}
    jnz TThreadHeap.GetSmall
  {$endif}

  jmp TThreadHeap.RaiseInvalidPtr
end;
{$endif}

function BrainMMUnknownReallocMem(ErrorAddr: Pointer; P: Pointer;
  NewB16Count: NativeUInt; ReturnAddress: Pointer): Pointer;
label
  return_made_none, medium, raise_invalid_ptr;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
  LastB16Count: NativeUInt;
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // realloc
    ThreadHeap.ErrorAddr := ErrorAddr;
    if (NativeInt(P) and MASK_16_TEST = 0) then
    begin
      Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

      if (NativeInt(P) and MASK_K1_TEST <> 0) then
      begin
        // pool: small or medium
        if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
        begin
          // pool small
          NewB16Count := {NewSize}NewB16Count shl 4;
          if (PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize >= {NewSize}NewB16Count) then
          begin
          return_made_none:
            Result := P;
            Exit;
          end else
          begin
            NewB16Count := {retrieve}NewB16Count shr 4;
            if (NewB16Count <= MAX_SMALL_B16COUNT) then
            begin
              Result := ThreadHeap.GrowSmallToSmall(P, NewB16Count);
              Exit;
            end;
          end;
        end else
        if (PK64PoolSmall(Pool).ThreadHeap = nil) then
        begin
          // pool medium
        medium:
          if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
          begin
            LastB16Count := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).B16Count;
            if (NewB16Count <= LastB16Count) then
            begin
              if (NewB16Count = LastB16Count) then goto return_made_none;
              Result := ThreadHeap.ReduceMedium(P, NewB16Count);
              Exit;
            end;
            if (NewB16Count <= MAX_MEDIUM_B16COUNT) then
            begin
              Result := ThreadHeap.ReallocMediumToMedium(P, NewB16Count);
              Exit;
            end;
          end;
        end;
      end else
      begin
        if (NativeInt(P) and MASK_K4_TEST <> 0) then
        begin
          // medium or invalid pointer
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
          goto raise_invalid_ptr;
        end else
        begin
          // big or large
          Result := MemoryManager.BrainMM.ReallocMemoryPages(MemoryPages(P),
            (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, PAGESMODE_SYSTEM);
          if (NativeUInt(Result) <= NativeUInt(PTR_INVALID)) then
          begin
            if (Result = PTR_INVALID) then goto raise_invalid_ptr;
            Result := ThreadHeap.ErrorOutOfMemory;
          end;

          Exit;
        end;
      end;

      // "default" method
      Result := ThreadHeap.ReallocDifficult(P, NewB16Count, ReturnAddress);
      Exit;
    end else
    begin
    raise_invalid_ptr:
      Result := Pointer(ThreadHeap.RaiseInvalidPtr);
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMReallocMem(P: Pointer; NewSize: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @BrainMMReallocMem;
{$endif}
label
  thread_deferreds_done, return_made_none,
  medium, raise_invalid_ptr;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
  LastB16Count, NewB16Count: NativeUInt;
begin
  NewB16Count := (NewSize + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      if (NativeInt(P) and MASK_16_TEST = 0) then
      begin
        Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

        if (NativeInt(P) and MASK_K1_TEST <> 0) then
        begin
          // pool: small or medium
          if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
          begin
            // pool small
            NewB16Count := {NewSize}NewB16Count shl 4;
            if (PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize >= {NewSize}NewB16Count) then
            begin
            return_made_none:
              Result := P;
              Exit;
            end else
            begin
              NewB16Count := {retrieve}NewB16Count shr 4;
              if (NewB16Count <= MAX_SMALL_B16COUNT) then
              begin
                Result := ThreadHeap.GrowSmallToSmall(P, NewB16Count);
                Exit;
              end;
            end;
          end else
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then
          begin
            // pool medium
          medium:
            if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
            begin
              LastB16Count := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).B16Count;
              if (NewB16Count <= LastB16Count) then
              begin
                if (NewB16Count = LastB16Count) then goto return_made_none;
                Result := ThreadHeap.ReduceMedium(P, NewB16Count);
                Exit;
              end;
              if (NewB16Count <= MAX_MEDIUM_B16COUNT) then
              begin
                Result := ThreadHeap.ReallocMediumToMedium(P, NewB16Count);
                Exit;
              end;
            end;
          end;
        end else
        begin
          if (NativeInt(P) and MASK_K4_TEST <> 0) then
          begin
            // medium or invalid pointer
            if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
            goto raise_invalid_ptr;
          end else
          begin
            // big or large
            Result := MemoryManager.BrainMM.ReallocMemoryPages(MemoryPages(P),
              (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, PAGESMODE_SYSTEM);
            if (NativeUInt(Result) <= NativeUInt(PTR_INVALID)) then
            begin
              if (Result = PTR_INVALID) then goto raise_invalid_ptr;
              Result := ThreadHeap.ErrorOutOfMemory;
            end;

            Exit;
          end;
        end;

        // "default" method
        Result := ThreadHeap.ReallocDifficult(P, NewB16Count, ReturnAddress);
        Exit;
      end else
      begin
      raise_invalid_ptr:
        Result := Pointer(ThreadHeap.RaiseInvalidPtr);
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownReallocMem(ErrorAddr, P, NewB16Count, ReturnAddress);
  end;
end;
{$else}
asm
  // var P(v3) := fake
  // ErrorAddr(v4) := nil
  {$ifdef CPUX86}
    lea ecx, [esp - 8]
    push ebx
    xor ebx, ebx
    nop
  {$else .CPUX64}
    lea r8, [rsp - 8]
    xor r9, r9
  {$endif}
@redirect:
  // store var P (v3), v3 := (NewSize(v2) + 15) div 16, v2 := const P (v1)
  {$ifdef CPUX86}
    // stack := var P
    push ecx
    lea ecx, [edx + 15]
    xchg edx, eax
    shr ecx, 4
  {$else .CPUX64}
    // v5 := var P
    mov r10, r8
    lea r8, [rdx + 15]
    xchg rdx, rcx
    shr r8, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz @return_unknown

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v4)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ebx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r9
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r9, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r9
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // if (P (v2) and MASK_16_TEST <> 0) then ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    test edx, MASK_16_TEST
  {$else .CPUX64}
    test rdx, MASK_16_TEST
  {$endif}
  jnz @raise_invalid_ptr

  // v4 := PK64PoolSmall/PK64PoolMedium(P)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K64_CLEAR
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K64_CLEAR
  {$endif}

  // if (P(v2) and MASK_K1_TEST = 0) then Not Small
  {$ifdef CPUX86}
    test edx, MASK_K1_TEST
  {$else .CPUX64}
    test rdx, MASK_K1_TEST
  {$endif}
  jz @not_small

  // if (PK64PoolSmall(P).ThreadHeap <> @Self) then Not Fast
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, rcx
  {$endif}
  jne @not_fast

  // if (PK1LineSmall(P).ModeSize >= {NewSize}NewB16Count * 16) then Exit P (return made none)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K1_CLEAR
    shl ecx, 4
    movzx ebx, byte ptr TK1LineSmall[EBX].Header.ModeSize
    cmp ebx, ecx
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K1_CLEAR
    shl r8, 4
    movzx r9, byte ptr TK1LineSmall[R9].Header.ModeSize
    cmp r9, r8
  {$endif}
  jb @small_grow
@return_made_none:
  // Result := const P (v2)
  {$ifdef CPUX86}
    mov eax, edx
    pop ecx
    pop ebx
  {$else .CPUX64}
    mov rax, rdx
  {$endif}
  ret

@small_grow:
  // retrieve NewB16Count
  // if (NewSize > MAX_SMALL_SIZE) then Exit ThreadHeap.ReallocDifficult
  {$ifdef CPUX86}
    mov ebx, ecx
    shr ecx, 4
    cmp ebx, MAX_SMALL_SIZE
  {$else .CPUX64}
    mov r9, r8
    shr r8, 4
    cmp r9, MAX_SMALL_SIZE
  {$endif}
  ja @realloc_difficult

  // Exit (recall) ThreadHeap.GrowSmallToSmall
  {$ifdef CPUX86}
    cmp esp, [esp]
    mov ebx, [esp + 4]
    lea esp, [esp + 8]
  {$else .CPUX64}
    lea r9, [rsp - 8]
    cmp r9, r10
  {$endif}
  je TThreadHeap.GrowSmallToSmall
  {$ifdef CPUX86}
    lea esp, [esp - 8]
    push offset @return_var_p
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
  {$endif}
  jmp TThreadHeap.GrowSmallToSmall

@not_fast:
  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else Exit ReallocDifficult
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  jnz @realloc_difficult

@medium:
  // if (PK64PoolMedium(P).ThreadHeap <> @Self) then Exit ThreadHeap.ReallocDifficult
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolMedium.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolMedium.ThreadHeap, rcx
  {$endif}
  jne @realloc_difficult

  // if (NewB16Count > MAX_MEDIUM_B16COUNT) then Exit ThreadHeap.ReallocDifficult
  {$ifdef CPUX86}
    cmp ecx, MAX_MEDIUM_B16COUNT
  {$else .CPUX64}
    cmp r8, MAX_MEDIUM_B16COUNT
  {$endif}  
  ja @realloc_difficult
  
  // Compare: PHeaderMedium(P).B16Count ~ NewB16Count
  // if = then Exit P (return made none)
  // if < then Exit ThreadHeap.ReallocMediumToMedium
  // if > then Exit ThreadHeap.ReduceMedium
  {$ifdef CPUX86}
    cmp [EDX - 16].THeaderMedium.B16Count, cx
  {$else .CPUX64}
    cmp [RDX - 16].THeaderMedium.B16Count, r8w
  {$endif}
  je @return_made_none
  {$ifdef CPUX86}
    push offset @return_var_p
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
  {$endif}
  jb TThreadHeap.ReallocMediumToMedium
  jmp TThreadHeap.ReduceMedium

@not_small:
  // if (P(v2) and MASK_K4_TEST = 0) then BigOrLarge
  {$ifdef CPUX86}
    test edx, MASK_K4_TEST
  {$else .CPUX64}
    test rdx, MASK_K4_TEST
  {$endif}
  jz @realloc_big_large

  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  je @medium
  jmp @raise_invalid_ptr

@realloc_big_large:
  // Exit MemoryManager.BrainMM.ReallocMemoryPages(P, PagesOf(NewB16Count), PAGESMODE_SYSTEM);
  {$ifdef CPUX86}
    mov eax, edx
    lea edx, [ecx + B16_PER_PAGE - 1]
    mov ecx, PAGESMODE_SYSTEM
    shr edx, B16_PER_PAGE_SHIFT
  {$else .CPUX64}
    mov rcx, rdx
    lea rdx, [r8 + B16_PER_PAGE - 1]
    mov r8d, PAGESMODE_SYSTEM
    shr rdx, B16_PER_PAGE_SHIFT
    push r10
  {$endif}
  call MemoryManager.BrainMM.ReallocMemoryPages
  {$ifdef CPUX86}
    cmp eax, 1 // PTR_INVALID
  {$else .CPUX64}
    cmp rax, 1 // PTR_INVALID
  {$endif}
  ja @return_var_p
  je @raise_invalid_pages
  {$ifdef CPUX86}
    push offset @return_var_p
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    lea r9, @return_var_p
    push r9
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory

@realloc_difficult:
  {$ifdef CPUX86}
    push dword ptr [esp + 8]
  {$else .CPUX64}
    mov r9, [rsp]
    push r10
  {$endif}
  call TThreadHeap.ReallocDifficult
@return_var_p:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
    mov [ecx], eax
  {$else .CPUX64}
    pop rcx
    mov [rcx], rax
  {$endif}
  ret
@return_unknown:
  {$ifdef CPUX86}
    mov eax, ebx
    push dword ptr [esp + 8]
    push offset @return_var_p
  {$else .CPUX64}
    mov rcx, r9
    mov r9, [rsp]
    push r10
    lea r10, @return_var_p
    push r10
  {$endif}
  jmp BrainMMUnknownReallocMem
@raise_invalid_pages:
  {$ifdef CPUX64}
    pop r10
  {$endif}
@raise_invalid_ptr:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
  {$endif}
  jmp TThreadHeap.RaiseInvalidPtr
@penalty_thread_deferreds:
  {$ifdef CPUX86}
    lea ebx, @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
  {$endif}
  jmp SafeProcessThreadDeferred
end;
{$endif}

function TThreadHeap.GrowSmallToSmall(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
var
  LastB16Count: NativeUInt;
begin
  Result := Self.GetSmall(NewB16Count);
  if (Result <> nil) then
  begin
    LastB16Count := NativeUInt(PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize) shr 4;
    Result := NcMoveB16Small(P16(P)^, P16(Result)^, LastB16Count);
  end;

  if (Self.FreeSmall(P) = FREEMEM_INVALID) then
    Self.RaiseInvalidPtr;
end;
{$else}
asm
  // store P
  // Result := Self.GetSmall(NewB16Count);
  // if (Result = nil) then Exit
  {$ifdef CPUX86}
    push ebx
    mov ebx, edx
    mov edx, ecx
    call TThreadHeap.GetSmall
    test eax, eax
  {$else .CPUX64}
    push rdx
    mov rdx, r8
    call TThreadHeap.GetSmall
    test rax, rax
  {$endif}
  jz @none_allocated

  // store Result
  // Line := LineOf(stored P)
  // Index := IndexOf(Line, stored P)
  {$ifdef CPUX86}
    push eax
    mov edx, ebx
    mov ecx, MASK_K1_TEST
    and ecx, ebx
    and edx, MASK_K1_CLEAR
    shr ecx, 4
  {$else .CPUX64}
    mov r10, rax
    mov rdx, [rsp]
    mov eax, MASK_K1_TEST
    and rax, rdx
    and rdx, MASK_K1_CLEAR
    shr rax, 4
  {$endif}

  // if (Line.FullQueue) then Exit Self.PenaltyGrowSmallToSmall(mode FullQueue)
  // if (not BitUnreserve(Index)) then Exit Self.ErrorInvalidPtr
  {$ifdef CPUX86}
    test [EDX].TK1LineSmall.Header.ItemSet.VLow32, 3
    jnz @penalty_copy_freemem
    lea eax, [EDX].TK1LineSmall.Header.ItemSet.VHigh32
    test ecx, 32
    DB $0F, $45, $D0 // cmovnz edx, eax
    and ecx, 31
    mov eax, [edx]
    bts eax, ecx
    mov [edx], eax
    jc @penalty_error
  {$else .CPUX64}
    mov r8, [RDX].TK1LineSmall.Header.ItemSet.V64
    test r8, 3
    jnz @penalty_copy_freemem
    bts r8, rax
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, r8
    jc @penalty_error
  {$endif}

  // if (Line.Header.ItemSet.V64 = DEFAULT_BITSETS_SMALL[BitSetKind(Line)]) then
  // Exit Self.PenaltyGrowSmallToSmall(mode DisposeK1LineSmall)
  {$ifdef CPUX86}
    and edx, MASK_K1_CLEAR
    mov ecx, 15
    and ecx, [EDX].TK1LineSmall.Header.Flags
    mov eax, [offset DEFAULT_BITSETS_SMALL + ecx * 8]
    mov ecx, [offset DEFAULT_BITSETS_SMALL + ecx * 8 + 4]
    sub eax, [edx]
    sub ecx, [edx + 4]
    or eax, ecx
    jz @penalty_copy_freemem
  {$else .CPUX64}
    mov rax, [RDX].TK1LineSmall.Header.Flags
    and rax, 15
    lea r9, DEFAULT_BITSETS_SMALL
    cmp r8, [r9 + rax * 8]
    je @penalty_copy_freemem
  {$endif}

  // Result := NcMoveB16Small(P16(P)^, P16(Result)^, LineOf(P).B16Count)
  {$ifdef CPUX86}
    pop edx
    mov eax, ebx
    and ebx, MASK_K1_CLEAR
    movzx ecx, [EBX].TK1LineSmall.Header.ModeSize
    shr ecx, 4
    pop ebx
  {$else .CPUX64}
    pop r8
    mov rdx, r10
    mov rcx, r8
    and r8, MASK_K1_CLEAR
    movzx r8, [R8].TK1LineSmall.Header.ModeSize
    shr r8, 4
  {$endif}
  jmp NcMoveB16Small

@penalty_copy_freemem:
  {$ifdef CPUX86}
    pop ecx
    mov eax, ebx
    mov edx, ebx
    pop ebx
    and eax, MASK_K64_CLEAR
    mov eax, [EAX].TK64PoolSmall.ThreadHeap
  {$else .CPUX64}
    pop rdx
    mov r8, r10
    mov rcx, rdx
    and rcx, MASK_K64_CLEAR
    mov rcx, [RCX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.PenaltyGrowSmallToSmall
@penalty_error:
  {$ifdef CPUX86}
    mov edx, ebx
    pop ebx
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolSmall.ThreadHeap
  {$else .CPUX64}
    pop rdx
    and rdx, MASK_K64_CLEAR
    mov rcx, [RDX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.RaiseInvalidPtr
@none_allocated:
  // Self.FreeSmall(P)
  {$ifdef CPUX86}
    mov eax, ebx
    mov edx, ebx
    pop ebx
    and eax, MASK_K64_CLEAR
    mov eax, [EAX].TK64PoolSmall.ThreadHeap
    push eax
  {$else .CPUX64}
    pop rdx
    mov rcx, rdx
    and rcx, MASK_K64_CLEAR
    mov rcx, [RCX].TK64PoolSmall.ThreadHeap
    push rcx
  {$endif}
  call TThreadHeap.FreeSmall

  // if (Ret = FREEMEM_INVALID) then RaiseInvalidPtr
  cmp eax, FREEMEM_INVALID
  {$ifdef CPUX86}
    pop eax
  {$else .CPUX64}
    pop rcx
  {$endif}
  je TThreadHeap.RaiseInvalidPtr

  // Result := nil
  {$ifdef CPUX86}
    xor eax, eax
  {$else .CPUX64}
    xor rax, rax
  {$endif}
end;
{$endif}

{$ifNdef PUREPASCAL}
function TThreadHeap.PenaltyGrowSmallToSmall(P: Pointer; Dest: Pointer): Pointer;
var
  Line, Prev, Next: PK1LineSmall;
  Index, B16Count: NativeInt;

  Mask: NativeInt;
  {$ifdef SMALLINT}
  PVInteger: PInteger;
  {$endif}
begin
  Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
  Result := NcMoveB16Small(P16(P)^, P16(Dest)^, {LastB16Count}NativeUInt(Line.Header.ModeSize) shr 4);

  if (Line.Header.ItemSet.VLow32 and 3 <> 0{FullQueue}) then
  begin
    // Dequeue QK1LineFull
    Prev := Line.Header.Queue.Prev;
    Next := Line.Header.Queue.Next;
    if (Prev = nil) then
    begin
      Self.QK1LineFull := Next;
    end else
    begin
      Prev.Header.Queue.Next := Next;
    end;
    if (Next <> nil) then
    begin
      Next.Header.Queue.Prev := Prev;
    end;

    // Enqueue
    B16Count := NativeUInt(Line.Header.ModeSize) shr 4;
    Next := FK1LineSmalls[B16Count];
    FK1LineSmalls[B16Count] := Line;
    Line.Header.Queue.Prev := nil;
    Line.Header.Queue.Next := Next;
    if (Next <> nil) then Next.Header.Queue.Prev := Line;

    // full bitset, unreserve
    Index := (NativeInt(P) and MASK_K1_TEST) shr 4;
    Mask := 1;
    {$ifdef LARGEINT}
      Mask := Mask shl Index;
      Line.Header.ItemSet.V64 := Mask;
    {$else .SMALLINT}
      Mask := Mask shl (Index and 31);
      PVInteger := @Line.Header.ItemSet.VIntegers[Byte(Index > 31)];
      Line.Header.ItemSet.VLow32{V64} := 0;
      PVInteger^ := Mask;
    {$endif}
  end else
  begin
    if (Self.DisposeK1LineSmall(Line) = FREEMEM_INVALID) then
      Result := Pointer(Self.RaiseInvalidPtr);
  end;
end;
{$endif}


type
  TMediumAlignOffsets = packed record
    Values: array[0..255] of Byte;
    AllocatedFlags: Cardinal;
  end;
  PMediumAlignOffsets = ^TMediumAlignOffsets;

var
  MEDIUM_ALIGN_OFFSETS{from Header}: array[0..Ord(High(TMemoryAlign))] of TMediumAlignOffsets;

procedure InitializeOffsetsMedium;
const
  MASK_MEDIUM_ALIGNS: array[0..Ord(High(TMemoryAlign))] of NativeUInt = (
    {ma16Bytes}   16 - 1,
    {ma32Bytes}   32 - 1,
    {ma64Bytes}   64 - 1,
    {ma128Bytes}  128 - 1,
    {ma256Bytes}  256 - 1,
    {ma512Bytes}  512 - 1,
    {ma1024Bytes} 1024 - 1,
    {ma2048Bytes} 2048 - 1
  );
var
  Align, AlignMaskTest, AlignMaskClear: NativeUInt;
  AlignOffsets: PMediumAlignOffsets;
  X, Value: NativeUInt;
begin
  for Align := 0 to Ord(High(TMemoryAlign)) do
  begin
    AlignOffsets := @MEDIUM_ALIGN_OFFSETS[Align];
    AlignOffsets.AllocatedFlags := (Align shl 16) + MASK_MEDIUM_ALLOCATED;
    AlignMaskTest := MASK_MEDIUM_ALIGNS[Align];
    AlignMaskClear := (not AlignMaskTest);

    for X := 0 to 255 do
    begin
      Value := (X + 1) shl 4;
      Value := (Value + AlignMaskTest) and AlignMaskClear;
      Value := (Value + Byte(Value and MASK_K4_TEST = 0) + AlignMaskTest) and AlignMaskClear;
      AlignOffsets.Values[X] := (Value shr 4) - X - 1;
    end;
  end;
end;

function TThreadHeap.ReduceMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
var
  Header: PHeaderMedium;
  Flags, EmptyFlags: NativeUInt;
begin
  // header flags
  Header := P;
  Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;

  // if (Next.Empty) then Grow(Next)
  EmptyFlags := Flags and MASK_MEDIUM_ALLOCATED_TEST;
  if (EmptyFlags = MASK_MEDIUM_ALLOCATED_VALUE) then
  begin
    Flags := (Flags shl 4) and ($ffff shl 4);
    Inc(NativeUInt(Header), Flags);
    EmptyFlags := Header.Flags;
    if (EmptyFlags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE) and
      (Header.PreviousSize = Flags) then
    begin
      // MediumEmpty.Size := MediumEmpty.Size + (NewSize - LastSize)
      EmptyFlags := EmptyFlags shl 4;
      NewB16Count := NewB16Count shl 4;
      Inc(NativeUInt(Header), EmptyFlags);
      Dec(Flags, NewB16Count);
      Inc(Flags, EmptyFlags);
      PHeaderMediumEmptyEx(Header{Empty}).Size := Flags;

      // new empty flags
      Dec(NativeUInt(Header), Flags);
      Flags := Flags shr 4;
      Header.PreviousSize := NewB16Count;
      Header.Flags := Flags;

      // result
      Dec(NativeUInt(Header), NewB16Count);
      NewB16Count := NewB16Count shr 4;
      Dec(Header);
      Header.B16Count := NewB16Count;
      Result := Pointer(@PHeaderMediumList(Header)[0]);
      Exit;
    end else
    begin
      Dec(NativeUInt(Header), Flags);
    end;
  end;

  Result := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.PenaltyReduceMedium(Pointer(Header), NewB16Count);
end;
{$else}
asm
  // if (not Allocated(P)) then PenaltyReduceMedium
  // Size := SizeOf(P)
  {$ifdef CPUX86}
    mov eax, [EDX - 16].THeaderMedium.Flags
    push ebx
    mov ebx, MASK_MEDIUM_ALLOCATED_TEST
    and ebx, eax
    movzx eax, ax
    shl eax, 4
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
  {$else .CPUX64}
    mov rcx, [RDX - 16].THeaderMedium.Flags
    mov r9, MASK_MEDIUM_ALLOCATED_TEST
    and r9, rcx
    movzx rcx, cx
    shl rcx, 4
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
  {$endif}
  jne @penalty_reducemem

  // if (not Next.Empty) then PenaltyReduceMedium
  {$ifdef CPUX86}
    add edx, eax
    mov ebx, [EDX].THeaderMedium.Flags
    cmp [EDX].THeaderMedium.PreviousSize, eax
    jne @penalty_reducemem_restore
    test ebx, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_reducemem_restore
  {$else .CPUX64}
    add rdx, rcx
    mov r9, [RDX].THeaderMedium.Flags
    cmp [RDX].THeaderMedium.PreviousSize, rcx
    jne @penalty_reducemem_restore
    test r9, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_reducemem_restore
  {$endif}

  // empty grow routine, result
  {$ifdef CPUX86}
    shl ebx, 4
    shl ecx, 4
    add edx, ebx
    sub eax, ecx
    add eax, ebx
    mov [EDX].THeaderMediumEmptyEx.Size, eax

    sub edx, eax
    shr eax, 4
    mov [EDX].THeaderMedium.PreviousSize, ecx
    mov [EDX].THeaderMedium.Flags, eax

    sub edx, ecx
    shr ecx, 4
    pop ebx
    mov [EDX - 16].THeaderMedium.B16Count, cx
    xchg eax, edx
  {$else .CPUX64}
    shl r9, 4
    shl r8, 4
    add rdx, r9
    sub rcx, r8
    add rcx, r9
    mov [RDX].THeaderMediumEmptyEx.Size, rcx

    sub rdx, rcx
    shr rcx, 4
    mov [RDX].THeaderMedium.PreviousSize, r8
    mov [RDX].THeaderMedium.Flags, rcx

    sub rdx, r8
    shr r8, 4
    mov [RDX - 16].THeaderMedium.B16Count, r8w
    xchg rax, rdx
  {$endif}

  // Exit
  ret
@penalty_reducemem_restore:
  {$ifdef CPUX86}
    sub edx, eax
  {$else .CPUX64}
    sub rdx, rcx
  {$endif}
@penalty_reducemem:
  {$ifdef CPUX86}
    mov eax, MASK_K64_CLEAR
    and eax, edx
    mov eax, [EAX].TK64PoolMedium.ThreadHeap
    pop ebx
  {$else .CPUX64}
    mov rcx, MASK_K64_CLEAR
    and rcx, rdx
    mov rcx, [RCX].TK64PoolMedium.ThreadHeap
  {$endif}
  jmp TThreadHeap.PenaltyReduceMedium
end;
{$endif}

function TThreadHeap.PenaltyReduceMedium(P: Pointer; NewB16Count: NativeUInt): Pointer;
label
  ptr_invalid, done;
var
  Header, NextHeader: PHeaderMedium;
  Flags, EmptySize: NativeUInt;
  CurrentB16Count, EmptyB16Count: NativeUInt;
  Pool: PK64PoolMedium;
  Empty, Left: PHeaderMediumEmpty;
begin
  // header, B16Count, next header
  Header := Pointer(NativeInt(P) - SizeOf(THeaderMedium));
  Flags := Header.Flags;
  if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then goto ptr_invalid;
  CurrentB16Count := Word(Flags);
  Flags := {CurrentSize}(Flags shl 4) and ($ffff shl 4);
  NextHeader := P;
  Inc(NativeUInt(NextHeader), {CurrentSize}Flags);
  if (NextHeader.PreviousSize <> {CurrentSize}Flags) then goto ptr_invalid;

  // optional reduce piece + new/grow empty piece
  Flags := NextHeader.Flags;
  if (Flags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE) then
  begin
    // grow empty piece
    EmptySize := NativeUInt(Word(Flags)) shl 4;
    Inc(NextHeader);
    Inc(NativeUInt(NextHeader), EmptySize);
    if (NextHeader.PreviousSize <> EmptySize) then goto ptr_invalid;
    EmptySize := EmptySize + (CurrentB16Count - NewB16Count) shl 4;
    NextHeader.PreviousSize := EmptySize;
    Dec(NextHeader);
    Dec(NativeUInt(NextHeader), EmptySize);
    NextHeader.Flags := {EmptyB16Count, Align: ma16Bytes, Allocated: False}EmptySize shr 4;
  end else
  if (Flags and MASK_MEDIUM_ALLOCATED_TEST = MASK_MEDIUM_ALLOCATED_VALUE) then
  begin
    // optional make empty piece
    EmptyB16Count := CurrentB16Count - NewB16Count;
    if (EmptyB16Count = 1) then goto done;

    EmptyB16Count := EmptyB16Count - 1;
    NextHeader := @PHeaderMediumList(Header)[NewB16Count];
    NextHeader.Flags := {EmptyB16Count, Align: ma16Bytes, Allocated: False}EmptyB16Count;
    Empty := Pointer(@PHeaderMediumList(NextHeader)[EmptyB16Count - 1]);
    PHeaderMediumEmptyEx(Empty).Size := EmptyB16Count shl 4;

    // push back
    Pool := PK64PoolMedium(NativeInt(Empty) and MASK_K64_CLEAR);
    Left := Pool.Empties.Last.Prev;
    Left.Next := Empty;
    Empty.Prev := Left;
    Empty.Next := @Pool.Empties.Last;
    Pool.Empties.Last.Prev := Empty;
  end else
  begin
  ptr_invalid:
    Result := Pointer({Self}PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.RaiseInvalidPtr);
    Exit;
  end;

  // new header B16Count/size
  Header.B16Count := NewB16Count;
  PHeaderMediumList(Header)[NewB16Count].PreviousSize := NewB16Count shl 4;
done:
  // result
  Result := Pointer(@PHeaderMediumList(Header)[0]);
end;

function OffsetHeaderMediumEmpty(Header: PHeaderMedium;
  AlignOffset: NativeUInt): PHeaderMedium;
label
  ptr_invalid;
var
  Pool: PK64PoolMedium;
  Size, Flags, CurrentB16Count: NativeUInt;
  Empty, Left: PHeaderMediumEmpty;
begin
  if (AlignOffset > 1) then
  begin
    // new empty piece
    AlignOffset := AlignOffset - 1;
    Header.Flags := {B16Count, Align: ma16Bytes, Allocated: False} AlignOffset;
    Empty := Pointer(@PHeaderMediumList(Header)[AlignOffset - 1]);
    PHeaderMediumEmptyEx(Empty).Size := AlignOffset shl 4;

    // push back
    Pool := PK64PoolMedium(NativeInt(Empty) and MASK_K64_CLEAR);
    Left := Pool.Empties.Last.Prev;
    Left.Next := Empty;
    Empty.Prev := Left;
    Empty.Next := @Pool.Empties.Last;
    Pool.Empties.Last.Prev := Empty;

    // after empty header
    Result := @PHeaderMediumList(Empty)[0];
    Exit;
  end else
  begin
    // grow allocated left
    Size := Header.PreviousSize;
    Dec(Header);
    Dec(NativeUInt(Header), Size);
    if (Size and MASK_MEDIUM_SIZE_TEST <> MASK_MEDIUM_SIZE_VALUE) then goto ptr_invalid;
    Flags := Header.Flags;
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then goto ptr_invalid;
    CurrentB16Count := Word(Flags);
    if (CurrentB16Count <> Size shr 4) then goto ptr_invalid;

    Inc(CurrentB16Count);
    Inc(Size, SizeOf(B16));
    Header.B16Count := CurrentB16Count;
    Inc(Header);
    Inc(NativeUInt(Header), Size);
    Header.PreviousSize := Size;
    Result := Header;
    Exit;
  end;

ptr_invalid:
  Result := nil;
end;

function TThreadHeap.GetMedium(B16Count: NativeUInt; Align: NativeUInt{TMemoryAlign}): Pointer;
{$ifdef PUREPASCAL}
var
  Pool: PK64PoolMedium;
  Header: PHeaderMedium;
  Flags: NativeUInt;
begin
  if (Align = 0) then
  begin
    Pool := Self.QK64PoolMedium;
    if (Pool <> nil) then
    begin
      Header{Empty} := Pointer(Pool.Empties.First.Next);
      if (Header{Empty} <> Pointer(@Pool.Empties.Last)) then
      begin
        Flags{Size} := PHeaderMediumEmptyEx(Header{Empty}).Size;
        Inc(Header);
        Dec(NativeUInt(Header), Flags{Size});
        if (Flags{Size} and MASK_MEDIUM_SIZE_TEST = MASK_MEDIUM_SIZE_VALUE) and
          (NativeInt(Header) and MASK_K4_TEST <> 0) then
        begin
          Dec(Header);
          Flags := Flags{Size} shr 4;
          Inc(B16Count, 2);
          if (Flags = Header.Flags) and (Flags >= B16Count{ + 2}) then
          begin
            // Flags := Flags - B16Count - 1
            // (B16Count * 16)
            Dec(Flags, B16Count);
            Dec(B16Count, 2);
            Inc(Flags);
            B16Count := B16Count shl 4;

            // make empty shorter
            // (Flags * 16)
            Inc(Header);
            Inc(NativeUInt(Header), B16Count);
            Header.PreviousSize := B16Count;
            Header.Flags := Flags{Empty B16Count};
            Flags := Flags shl 4;
            Inc(Header);
            Inc(NativeUInt(Header), Flags);
            Header.PreviousSize := Flags;

            // result
            Dec(Header, 2);
            Dec(NativeUInt(Header), B16Count);
            B16Count := B16Count shr 4;
            Dec(NativeUInt(Header), Flags);
            Header.Flags := B16Count + {AllocatedFlags}MASK_MEDIUM_ALLOCATED;
            Result := Pointer(@PHeaderMediumList(Header)[0]);
            Exit;
          end;
          Dec(B16Count, 2);
        end;
      end;

      Result := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.PenaltyGetMedium(B16Count, 0);
    end else
    begin
      Result := Self.PenaltyGetMedium(B16Count, 0);
    end;
  end else
  begin
    Result := Self.PenaltyGetMedium(B16Count, Align);
  end;
end;
{$else}
asm
  // if (Align <> ma16Bytes) then Self.PenaltyGetMedium(..., ...)
  {$ifdef CPUX86}
    test ecx, ecx
  {$else .CPUX64}
    test r8, r8
  {$endif}
  jnz TThreadHeap.PenaltyGetMedium

  // if (Self.QK64PoolMedium = nil) then Self.PenaltyGetMedium(..., 0)
  {$ifdef CPUX86}
    mov ecx, [EAX].TThreadHeap.QK64PoolMedium
    test ecx, ecx
  {$else .CPUX64}
    mov r8, [RCX].TThreadHeap.QK64PoolMedium
    test r8, r8
  {$endif}
  jz TThreadHeap.PenaltyGetMedium

  // if (not Contains(Empty)) then Self.PenaltyGetMedium(..., 0)
  {$ifdef CPUX86}
    mov eax, [ECX].TK64PoolMedium.Empties.First.Next
    lea ecx, [ECX].TK64PoolMedium.Empties.Last
    cmp eax, ecx
  {$else .CPUX64}
    mov rcx, [R8].TK64PoolMedium.Empties.First.Next
    lea r8, [R8].TK64PoolMedium.Empties.Last
    cmp rcx, r8
  {$endif}
  je @penalty_getmem_thread

  // Size := SizeOf(Empty), check bits
  // Header := HeaderOf(Empty), check 4kb align
  {$ifdef CPUX86}
    mov ecx, [EAX].THeaderMediumEmptyEx.Size
    add eax, 16
    sub eax, ecx
    test ecx, MASK_MEDIUM_SIZE_TEST
    jnz @penalty_getmem_thread
    test eax, MASK_K4_TEST
    jz @penalty_getmem_thread
  {$else .CPUX64}
    mov r8, [RCX].THeaderMediumEmptyEx.Size
    add rcx, 16
    sub rcx, r8
    test r8, MASK_MEDIUM_SIZE_TEST
    jnz @penalty_getmem_thread
    test rcx, MASK_K4_TEST
    jz @penalty_getmem_thread
  {$endif}

  // if (Size shr 4 <> Header.Flags) or (Size shr 4 >= B16Count + 2) then
  //   Self.PenaltyGetMedium(..., 0)
  {$ifdef CPUX86}
    shr ecx, 4
    add edx, 2
    sub eax, 16
    cmp ecx, edx
    jb @penalty_getmem_b16count
    cmp ecx, [EAX].THeaderMedium.Flags
    jne @penalty_getmem_b16count
  {$else .CPUX64}
    shr r8, 4
    add rdx, 2
    sub rcx, 16
    cmp r8, rdx
    jb @penalty_getmem_b16count
    cmp r8, [RCX].THeaderMedium.Flags
    jne @penalty_getmem_b16count
  {$endif}

  // allocate and empty reduce routine
  {$ifdef CPUX86}
    sub ecx, edx
    sub edx, 2
    add ecx, 1
    shl edx, 4

    add eax, edx
    mov [EAX + 16].THeaderMedium.PreviousSize, edx
    mov [EAX + 16].THeaderMedium.Flags, ecx
    shl ecx, 4
    add eax, ecx
    mov [EAX + 32].THeaderMedium.PreviousSize, ecx

    add ecx, edx
    shr edx, 4
    sub eax, ecx
    add edx, MASK_MEDIUM_ALLOCATED
    mov [EAX].THeaderMedium.Flags, edx
    lea eax, [EAX + 16]
  {$else .CPUX64}
    sub r8, rdx
    sub rdx, 2
    add r8, 1
    shl rdx, 4

    add rcx, rdx
    mov [RCX + 16].THeaderMedium.PreviousSize, rdx
    mov [RCX + 16].THeaderMedium.Flags, r8
    shl r8, 4
    add rcx, r8
    mov [RCX + 32].THeaderMedium.PreviousSize, r8

    add r8, rdx
    shr rdx, 4
    sub rcx, r8
    add rdx, MASK_MEDIUM_ALLOCATED
    mov [RCX].THeaderMedium.Flags, rdx
    lea rax, [RCX + 16]
  {$endif}

  // Exit
  ret
@penalty_getmem_b16count:
  // Dec(B16Count, 2)
  {$ifdef CPUX86}
    sub edx, 2
  {$else .CPUX64}
    sub rdx, 2
  {$endif}
@penalty_getmem_thread:
  // Self := ThreadHeapOf(Header/Empty)
  {$ifdef CPUX86}
    and eax, MASK_K64_CLEAR
    mov eax, [EAX].TK64PoolMedium.ThreadHeap
  {$else .CPUX64}
    and rcx, MASK_K64_CLEAR
    mov rcx, [RCX].TK64PoolMedium.ThreadHeap
  {$endif}
@penalty_getmem:
  {$ifdef CPUX86}
    xor ecx, ecx
  {$else .CPUX64}
    xor r8, r8
  {$endif}
  jmp TThreadHeap.PenaltyGetMedium
end;
{$endif}

function TThreadHeap.PenaltyGetMedium(B16Count: NativeUInt; Align: NativeUInt{TMemoryAlign}): Pointer;
label
  done, ptr_invalid;
var
  ALIGN_OFFSETS: PMediumAlignOffsets;
  Pool: PK64PoolMedium;
  Empty, Left, Right: PHeaderMediumEmpty;
  Size, Flags, AlignOffset: NativeUInt;
  Header: PHeaderMedium;
  RequirementB16Count: NativeUInt;
  CurrentB16Count: NativeUInt;
  VacantB16Count: NativeUInt;
begin
  // align offsets
  ALIGN_OFFSETS := @MEDIUM_ALIGN_OFFSETS[Align];

  // allocation loops
  Pool := Self.QK64PoolMedium;
  repeat
    if (Pool <> nil) then
    repeat
      Empty := Pool.Empties.First.Next;
      if (Empty <> @Pool.Empties.Last) then
      repeat
        // check bits
        Size := PHeaderMediumEmptyEx(Empty).Size;
        Header := Pointer(NativeUInt(Empty) - Size);
        if (Size and MASK_MEDIUM_SIZE_TEST <> MASK_MEDIUM_SIZE_VALUE) then goto ptr_invalid;
        Flags := Header.Flags;
        if (Flags and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then goto ptr_invalid;
        if (Flags and $ffff <> Size shr 4) then goto ptr_invalid;

        // align offset, piece size
        AlignOffset := ALIGN_OFFSETS.Values[(NativeInt(Header) and MASK_K4_TEST) shr 4];
        RequirementB16Count := AlignOffset + B16Count;
        if (Header.B16Count >= RequirementB16Count) then goto done;

        Empty := Empty.Next;
      until (Empty = @Pool.Empties.Last);

      Pool := Pool.Queue.Next;
    until (Pool = nil);

    Pool := Self.NewK64PoolMedium;
    if (Pool = nil) then
    begin
      Result := nil{Self.ErrorOutOfMemory};
      Exit;
    ptr_invalid:
      Result := Pointer(Self.RaiseInvalidPtr);
      Exit;
    end;
  until (False);

done:
  // make empty piece shorter (or remove)
  CurrentB16Count := Header.B16Count;
  VacantB16Count := CurrentB16Count - RequirementB16Count;
  if (VacantB16Count > 1) then
  begin
    VacantB16Count := VacantB16Count - 1;
    PHeaderMediumList(Header)[CurrentB16Count].PreviousSize := VacantB16Count shl 4;
    PHeaderMediumList(Header)[RequirementB16Count].Flags := {NewB16Count, Align: ma16Bytes, Allocated: False}VacantB16Count;
  end else
  begin
    Inc(B16Count, VacantB16Count);
    Empty := Pointer(@PHeaderMediumList(Header)[CurrentB16Count - 1]);
    Left := Empty.Prev;
    Right := Empty.Next;
    Left.Next := Right;
    Right.Prev := Left;
  end;

  // offset
  if (AlignOffset <> 0) then
  begin
    Header := OffsetHeaderMediumEmpty(Header, AlignOffset);
    if (Header = nil) then goto ptr_invalid;
  end;

  // result
  CurrentB16Count := B16Count;
  Header.Flags := CurrentB16Count + ALIGN_OFFSETS.AllocatedFlags;
  PHeaderMediumList(Header)[CurrentB16Count].PreviousSize := CurrentB16Count shl 4;
  Result := Pointer(@PHeaderMediumList(Header)[0]);
end;

{
  TAllocatedMediumInformation = packed record
    SelfRightB16Count: Word:12;
    Reserved: Byte:4;
    Align: TMemoryAlign:3;
    RightEmpty: Boolean:1;
    LeftEmptyB16Count: Word:12;
  end;
}

const
  OFFSET_MEDIUM_LEFT_B16COUNT = (32 - 12);
  FLAG_MEDIUM_RIGHT_EMPTY = NativeUInt(1 shl (OFFSET_MEDIUM_ALIGN + 3));

function InspectAllocatedMedium(P: Pointer): NativeUInt;
label
  ptr_invalid;
var
  StoredP: Pointer;
  Header: PHeaderMedium;
  Size, Flags: NativeUInt;
begin
  // flags
  StoredP := P;
  Header := P;
  Dec(Header);
  Result := Header.Flags;
  if (Result and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then
    goto ptr_invalid;
  Result := Result and (MASK_MEDIUM_ALIGN or MAX_MEDIUM_B16COUNT);

  // previous
  Size := Header.PreviousSize;
  if (Size <> 0) then
  begin
    Dec(Header);
    Dec(NativeUInt(Header), Size);
    if (Size and MASK_MEDIUM_SIZE_TEST <> MASK_MEDIUM_SIZE_VALUE) then goto ptr_invalid;
    Flags := Header.Flags;
    if (Flags and MASK_MEDIUM_TEST <> MASK_MEDIUM_VALUE) then goto ptr_invalid;
    if (Word(Flags) = Word(Size shr 4)) then
    begin
      if (Flags and MASK_MEDIUM_ALLOCATED = 0) then
      begin
        Result := Result + Cardinal((Flags + 1) shl OFFSET_MEDIUM_LEFT_B16COUNT);
      end;
    end else
    begin
    ptr_invalid:
      Result := High(NativeUInt);
      Exit;
    end;
  end else
  begin
    if (Header <> Pointer(@PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).Items)) then
      goto ptr_invalid;
  end;

  // right
  Size := NativeUInt(Word(Result)) shl 4;
  Header := StoredP;
  Inc(NativeUInt(Header), Size);
  if (Size <> Header.PreviousSize) then goto ptr_invalid;
  StoredP := Pointer(@PHeaderMediumList(Header)[0]);
  Flags := Header.Flags;
  if (Flags and MASK_MEDIUM_TEST <> MASK_MEDIUM_VALUE) then goto ptr_invalid;
  if (Flags and MASK_MEDIUM_ALLOCATED = 0) then
  begin
    Size := {B16Count}Word(Flags);
    Inc(Result, FLAG_MEDIUM_RIGHT_EMPTY + 1);
    Inc(Result, {B16Count}Size);
    Size := Size shl 4;
    Header := StoredP;
    Inc(NativeUInt(Header), Size);
    if (Size <> Header.PreviousSize) then goto ptr_invalid;
  end;
end;

function TThreadHeap.FreeMedium(P: Pointer): Integer;
const
  MAX_MEDIUMEMPTY_SIZE = SizeOf(THeaderMediumList) - {Start}SizeOf(THeaderMedium);
{$ifdef PUREPASCAL}
var
  Header: PHeaderMedium;
  Flags, EmptyFlags: NativeUInt;
  Pool: PK64PoolMedium;
begin
  // header flags
  Header := P;
  Dec(Header);
  Flags := Header.Flags;

  // if (Prev.Allocated) and (Next.Empty) then Grow(Next)
  EmptyFlags := Flags and MASK_MEDIUM_ALLOCATED_TEST;
  if (EmptyFlags = MASK_MEDIUM_ALLOCATED_VALUE) then
  begin
    EmptyFlags := PHeaderMedium(NativeUInt(Header) - Header.PreviousSize - SizeOf(THeaderMedium)).Flags;
    EmptyFlags := EmptyFlags and MASK_MEDIUM_ALLOCATED_TEST;
    if (EmptyFlags = MASK_MEDIUM_ALLOCATED_VALUE) then
    begin
      Inc(Header);
      Flags := (Flags shl 4) and ($ffff shl 4);
      Inc(NativeUInt(Header), Flags);
      EmptyFlags := Header.Flags;
      if (EmptyFlags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE) and
        (Header.PreviousSize = Flags) then
      begin
        // MediumEmpty.Size := MediumEmpty.Size + Size + 16
        EmptyFlags := EmptyFlags shl 4;
        Inc(Flags, 16);
        Inc(NativeUInt(Header{Empty}), EmptyFlags);
        Inc(Flags, EmptyFlags);
        if (Flags <> MAX_MEDIUMEMPTY_SIZE) then
        begin
          PHeaderMediumEmptyEx(Header{Empty}).Size := Flags;

          // new empty flags
          Dec(NativeUInt(Header), Flags);
          Flags := Flags shr 4;
          Header.Flags := Flags;

          // result
          Result := FREEMEM_DONE;
          Exit;
        end else
        begin
          Pool := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR);
          Result := Pool.ThreadHeap.DisposeK64PoolMedium(Pool);
          Exit;
        end;
      end else
      begin
        Dec(Header);
        Dec(NativeUInt(Header), Flags);
      end;
    end;
  end;

  Inc(Header);
  Result := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.PenaltyFreeMedium(Pointer(Header));
end;
{$else}
asm
  // if (not Self.Allocated) or (not Prev.Allocated) or
  //  (not Next.Allocated) then PenaltyFreeMedium
  {$ifdef CPUX86}
    mov eax, [EDX - 16].THeaderMedium.Flags
    mov ecx, MASK_MEDIUM_ALLOCATED_TEST
    and ecx, eax
    cmp ecx, MASK_MEDIUM_ALLOCATED_VALUE
    jne @penalty_freemem

    lea ecx, [edx - 32]
    sub ecx, [EDX - 16].THeaderMedium.PreviousSize
    movzx eax, ax
    mov ecx, [ECX].THeaderMedium.Flags
    shl eax, 4
    and ecx, MASK_MEDIUM_ALLOCATED_TEST
    add edx, eax
    cmp ecx, MASK_MEDIUM_ALLOCATED_VALUE
    jne @penalty_freemem_restore

    mov ecx, [EDX].THeaderMedium.Flags
    test ecx, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_freemem_restore
    shl ecx, 4
    cmp eax, [EDX].THeaderMedium.PreviousSize
    jne @penalty_freemem_restore
  {$else .CPUX64}
    mov rcx, [RDX - 16].THeaderMedium.Flags
    mov r8, MASK_MEDIUM_ALLOCATED_TEST
    and r8, rcx
    cmp r8, MASK_MEDIUM_ALLOCATED_VALUE
    jne @penalty_freemem

    lea r8, [rdx - 32]
    sub r8, [RDX - 16].THeaderMedium.PreviousSize
    movzx rcx, cx
    mov r8, [R8].THeaderMedium.Flags
    shl rcx, 4
    and r8, MASK_MEDIUM_ALLOCATED_TEST
    add rdx, rcx
    cmp r8, MASK_MEDIUM_ALLOCATED_VALUE
    jne @penalty_freemem_restore

    mov r8, [RDX].THeaderMedium.Flags
    test r8, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_freemem_restore
    shl r8, 4
    cmp rcx, [RDX].THeaderMedium.PreviousSize
    jne @penalty_freemem_restore
  {$endif}

  // if (MediumEmpty.Size + Size + 16 = MAX_MEDIUMEMPTY_SIZE) then
  //   Pool.ThreadHeap.DisposeK64PoolMedium(Pool)
  {$ifdef CPUX86}
    add eax, ecx
    add edx, ecx
    cmp eax, MAX_MEDIUMEMPTY_SIZE - 16
    lea eax, [eax + 16]
  {$else .CPUX64}
    add rcx, r8
    add rdx, r8
    cmp rcx, MAX_MEDIUMEMPTY_SIZE - 16
    lea rcx, [rcx + 16]
  {$endif}
  je @dispose_pool

  // result
  {$ifdef CPUX86}
    mov [EDX].THeaderMediumEmptyEx.Size, eax
    sub edx, eax
    shr eax, 4
    mov [EDX].THeaderMedium.Flags, eax

    mov eax, FREEMEM_DONE
  {$else .CPUX64}
    mov [RDX].THeaderMediumEmptyEx.Size, rcx
    sub rdx, rcx
    shr rcx, 4
    mov [RDX].THeaderMedium.Flags, rcx

    mov rax, FREEMEM_DONE
  {$endif}

  // Exit
  ret
@dispose_pool:
  {$ifdef CPUX86}
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolMedium.ThreadHeap
  {$else .CPUX64}
    and rdx, MASK_K64_CLEAR
    mov rcx, [RDX].TK64PoolMedium.ThreadHeap
  {$endif}
  jmp TThreadHeap.DisposeK64PoolMedium
@penalty_freemem_restore:
  {$ifdef CPUX86}
    sub edx, eax
  {$else .CPUX64}
    sub rdx, rcx
  {$endif}
@penalty_freemem:
  {$ifdef CPUX86}
    mov eax, MASK_K64_CLEAR
    and eax, edx
    mov eax, [EAX].TK64PoolMedium.ThreadHeap
  {$else .CPUX64}
    mov rcx, MASK_K64_CLEAR
    and rcx, rdx
    mov rcx, [RCX].TK64PoolMedium.ThreadHeap
  {$endif}
  jmp TThreadHeap.PenaltyFreeMedium
end;
{$endif}

function TThreadHeap.PenaltyFreeMedium(P: Pointer): Integer;
var
  Information: NativeUInt;
  Pool: PK64PoolMedium;
  Empty, Left, Right: PHeaderMediumEmpty;
  Size: NativeUInt;
begin
  // inspect
  Empty := P;
  Information := InspectAllocatedMedium(P);
  if (Information > ((1 shl OFFSET_MEDIUM_LEFT_B16COUNT) - 1)) then
  begin
    if (Information = High(NativeUInt)) then
    begin
      Result := {Self}PK64PoolMedium(NativeInt(Empty) and MASK_K64_CLEAR).ThreadHeap.ErrorInvalidPtr;
      Exit;
    end else
    begin
      // left empty: remove from pool
      Dec(Empty, 2);
      Left := Empty.Prev;
      Right := Empty.Next;
      Left.Next := Right;
      Right.Prev := Left;
      Inc(Empty, 2);
    end;
  end;

  // mark empty header
  // optional dispose medium pool
  Size := NativeUInt(Word(Information)) shl 4;
  Dec(Empty);
  Inc(NativeUInt(Empty), Size);
  Inc(Size, (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4);
  PHeaderMediumEmptyEx(Empty).Size := Size;
  if (Size <> SizeOf(THeaderMediumList) - {Start}SizeOf(THeaderMedium)) then
  begin
    Dec(NativeUInt(Empty), Size);
    PHeaderMedium(Empty).Flags := Size shr 4;

    // right not empty: push back
    if (Information and FLAG_MEDIUM_RIGHT_EMPTY = 0) then
    begin
      Pool := PK64PoolMedium(NativeInt(Empty) and MASK_K64_CLEAR);
      Inc(NativeUInt(Empty), Size);
      Left := Pool.Empties.Last.Prev;
      Left.Next := Empty;
      Empty.Prev := Left;
      Empty.Next := @Pool.Empties.Last;
      Pool.Empties.Last.Prev := Empty;
    end;
  end else
  begin
    Pool := PK64PoolMedium(NativeInt(Empty) and MASK_K64_CLEAR);
    Result := Pool.ThreadHeap.DisposeK64PoolMedium(Pool);
    Exit;
  end;

  // result
  Result := FREEMEM_DONE;
end;

function TThreadHeap.RegetMediumToMedium(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
var
  Header: PHeaderMedium;
  Flags, EmptyFlags: NativeUInt;
begin
  // header flags
  Header := P;
  Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;

  // if (Next.Empty) then try Reduce (Next)
  EmptyFlags := Flags and MASK_MEDIUM_ALLOCATED_TEST;
  Flags := Word(Flags);
  if (EmptyFlags = MASK_MEDIUM_ALLOCATED_VALUE) then
  begin
    Dec(NewB16Count{GrowCount}, Flags);
    Flags := Flags shl 4;
    Inc(NativeUInt(Header), Flags);
    EmptyFlags := Header.Flags;
    if (EmptyFlags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE) and
      (Header.PreviousSize = Flags) then
    begin
      Inc(NewB16Count, 2);
      if (EmptyFlags >= NewB16Count{ + 2}) then
      begin
        Dec(NewB16Count, 2);
        EmptyFlags := EmptyFlags shl 4;
        NewB16Count := NewB16Count shl 4;
        Inc(NativeUInt(Header), EmptyFlags);

        Dec(EmptyFlags, NewB16Count);
        Inc(NewB16Count, Flags);
        PHeaderMediumEmptyEx(Header{Empty}).Size := EmptyFlags;

        Dec(NativeUInt(Header), EmptyFlags);
        EmptyFlags := EmptyFlags shr 4;
        Header.Flags := EmptyFlags;
        Header.PreviousSize := NewB16Count;

        // result
        Dec(Header);
        Dec(NativeUInt(Header), NewB16Count);
        NewB16Count := NewB16Count shr 4;
        Header.B16Count := NewB16Count;
        Result := Pointer(@PHeaderMediumList(Header)[0]);
        Exit;
      end;
      Dec(NewB16Count, 2);
    end;

    Dec(NativeUInt(Header), Flags);
    Flags := Flags shr 4;
    Inc(NewB16Count, Flags);
  end;

  Result := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.PenaltyRegetMediumToMedium(Pointer(Header), NewB16Count);
end;
{$else}
asm
  // if (not Allocated(P)) then PenaltyRegetMediumToMedium
  {$ifdef CPUX86}
    mov eax, [EDX - 16].THeaderMedium.Flags
    push ebx
    mov ebx, MASK_MEDIUM_ALLOCATED_TEST
    and ebx, eax
    movzx eax, ax
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
  {$else .CPUX64}
    mov rcx, [RDX - 16].THeaderMedium.Flags
    mov r9, MASK_MEDIUM_ALLOCATED_TEST
    and r9, rcx
    movzx rcx, cx
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
  {$endif}
  jne @penalty_regetmem

  // GrowCount := NewB16Count - Header(P).P16Count
  // if (not Next.Empty) then PenaltyRegetMediumToMedium
  {$ifdef CPUX86}
    sub ecx, eax
    shl eax, 4
    cmp [EDX + eax].THeaderMedium.PreviousSize, eax
    lea edx, [edx + eax]
    jne @penalty_regetmem_restore
    mov ebx, [EDX].THeaderMedium.Flags
    test ebx, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_regetmem_restore
  {$else .CPUX64}
    sub r8, rcx
    shl rcx, 4
    cmp [RDX + rcx].THeaderMedium.PreviousSize, rcx
    lea rdx, [rdx + rcx]
    jne @penalty_regetmem_restore
    mov r9, [RDX].THeaderMedium.Flags
    test r9, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_regetmem_restore
  {$endif}

  // if (EmptyFlags < NewB16Count + 2) then PenaltyRegetMediumToMedium
  {$ifdef CPUX86}
    add ecx, 2
    cmp ebx, ecx
    lea ecx, [ecx - 2]
  {$else .CPUX64}
    add r8, 2
    cmp r9, r8
    lea r8, [r8 - 2]
  {$endif}
  jb @penalty_regetmem_restore

  // empty reduce routine, result
  {$ifdef CPUX86}
    shl ebx, 4
    shl ecx, 4
    add edx, ebx

    sub ebx, ecx
    add ecx, eax
    mov [EDX].THeaderMediumEmptyEx.Size, ebx

    sub edx, ebx
    shr ebx, 4
    mov [EDX].THeaderMedium.PreviousSize, ecx
    mov [EDX].THeaderMedium.Flags, ebx

    sub edx, ecx
    shr ecx, 4
    pop ebx
    mov [EDX - 16].THeaderMedium.B16Count, cx
    xchg eax, edx
  {$else .CPUX64}
    shl r9, 4
    shl r8, 4
    add rdx, r9

    sub r9, r8
    add r8, rcx
    mov [RDX].THeaderMediumEmptyEx.Size, r9

    sub rdx, r9
    shr r9, 4
    mov [RDX].THeaderMedium.PreviousSize, r8
    mov [RDX].THeaderMedium.Flags, r9

    sub rdx, r8
    shr r8, 4
    mov [RDX - 16].THeaderMedium.B16Count, r8w
    xchg rax, rdx
  {$endif}

  // Exit
  ret
@penalty_regetmem_restore:
  {$ifdef CPUX86}
    sub edx, eax
    shr eax, 4
    add ecx, eax
  {$else .CPUX64}
    sub rdx, rcx
    shr rcx, 4
    add r8, rcx
  {$endif}
@penalty_regetmem:
  {$ifdef CPUX86}
    mov eax, MASK_K64_CLEAR
    and eax, edx
    mov eax, [EAX].TK64PoolMedium.ThreadHeap
    pop ebx
  {$else .CPUX64}
    mov rcx, MASK_K64_CLEAR
    and rcx, rdx
    mov rcx, [RCX].TK64PoolMedium.ThreadHeap
  {$endif}
  jmp TThreadHeap.PenaltyRegetMediumToMedium
end;
{$endif}

function TThreadHeap.PenaltyRegetMediumToMedium(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
label
  free_and_get, ptr_invalid;
var
  ALIGN_OFFSETS: PMediumAlignOffsets;
  Header, NextHeader: PHeaderMedium;
  Information, Size: NativeUInt;
  VacantB16Count, AlignOffset: NativeUInt;
  Pool: PK64PoolMedium;
  Empty, Left, Right: PHeaderMediumEmpty;
  ThreadHeap: PThreadHeap;
begin
  // inspect
  Header := P;
  Information := InspectAllocatedMedium(P);

  Dec(Header);
  if (Information = High(NativeUInt)) then goto ptr_invalid;
  if (Word(Information) >= Word(NewB16Count)) then
  begin
    // grow right, make empty piece shorter (or remove)
    VacantB16Count := NativeUInt(Word(Information)) - NewB16Count;
    if (VacantB16Count > 1) then
    begin
      VacantB16Count := VacantB16Count - 1;
      PHeaderMediumList(Header)[NativeUInt(Word(Information))].PreviousSize := VacantB16Count shl 4;
      PHeaderMediumList(Header)[NewB16Count].Flags := {NewB16Count, Align: ma16Bytes, Allocated: False}VacantB16Count;
    end else
    begin
      Inc(NewB16Count, VacantB16Count);
      Empty := Pointer(@PHeaderMediumList(Header)[NativeUInt(Word(Information)) - 1]);
      Left := Empty.Prev;
      Right := Empty.Next;
      Left.Next := Right;
      Right.Prev := Left;
    end;
  end else
  begin
    if (Information > ((1 shl OFFSET_MEDIUM_LEFT_B16COUNT) - 1)) then
    begin
      // try grow left
      ALIGN_OFFSETS := @MEDIUM_ALIGN_OFFSETS[(Information shr OFFSET_MEDIUM_ALIGN) and NativeUInt(High(TMemoryAlign))];
      Size := (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4;
      AlignOffset := ALIGN_OFFSETS.Values[((NativeUInt(Header) - Size) and MASK_K4_TEST) shr 4];
      if ((Size shr 4) + NativeUInt(Word(Information)) < AlignOffset + NewB16Count) then goto free_and_get;

      // remove left empty
      Empty := Pointer(NativeUInt(Header) - SizeOf(THeaderMediumEmpty));
      Left := Empty.Prev;
      Right := Empty.Next;
      Left.Next := Right;
      Right.Prev := Left;

      // left align offset
      if (AlignOffset <> 0) then
      begin
        Size := (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4;
        NextHeader := Header;
        Header := OffsetHeaderMediumEmpty(Pointer(NativeUInt(Header) - Size), AlignOffset);
        if (Header = nil) then
        begin
          Header := NextHeader;
          goto ptr_invalid;
        end;
      end else
      begin
        Dec(NativeUInt(Header), (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4);
      end;

      // create, resize of remove right empty piece
      VacantB16Count := NativeUInt(Word(Information)) +
        (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) - AlignOffset - NewB16Count;
      NextHeader := @PHeaderMediumList(Header)[NewB16Count];
      if (VacantB16Count > 1) then
      begin
        VacantB16Count := VacantB16Count - 1;
        NextHeader.Flags := {NewB16Count, Align: ma16Bytes, Allocated: False}VacantB16Count;
        Empty := Pointer(@PHeaderMediumList(NextHeader)[VacantB16Count - 1]);
        PHeaderMediumEmptyEx(Empty).Size := VacantB16Count shl 4;

        if (Information and FLAG_MEDIUM_RIGHT_EMPTY = 0) then
        begin
          Pool := PK64PoolMedium(NativeInt(Empty) and MASK_K64_CLEAR);
          Left := Pool.Empties.Last.Prev;
          Left.Next := Empty;
          Empty.Prev := Left;
          Empty.Next := @Pool.Empties.Last;
          Pool.Empties.Last.Prev := Empty;
        end;
      end else
      begin
        Inc(NewB16Count, VacantB16Count);

        if (Information and FLAG_MEDIUM_RIGHT_EMPTY <> 0) then
        begin
          Empty := Pointer(@PHeaderMediumList(Header)[NewB16Count - 1]);
          Left := Empty.Prev;
          Right := Empty.Next;
          Left.Next := Right;
          Right.Prev := Left;
        end;
      end;
    end else
    begin
    free_and_get:
      ThreadHeap := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap;
      if (ThreadHeap.FreeMedium(@PHeaderMediumList(Header)[0]) <> FREEMEM_DONE) then
      begin
      ptr_invalid:
        ThreadHeap := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap;
        Result := Pointer(ThreadHeap.RaiseInvalidPtr);
        Exit;
      end;

      Result := ThreadHeap.GetMedium(NewB16Count, (Information shr OFFSET_MEDIUM_ALIGN) and NativeUInt(High(TMemoryAlign)));
      Exit;
    end;
  end;

  // result
  Header.Flags := (Information and MASK_MEDIUM_ALIGN) + MASK_MEDIUM_ALLOCATED + NewB16Count;
  Size := NewB16Count shl 4;
  Inc(Header);
  Inc(NativeUInt(Header), Size);
  Header.PreviousSize := Size;
  Dec(NativeUInt(Header), Size);
  Result := Header;
end;

function TThreadHeap.ReallocMediumToMedium(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
var
  Header: PHeaderMedium;
  Flags, EmptyFlags: NativeUInt;
begin
  // header flags
  Header := P;
  Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;

  // if (Next.Empty) then try Reduce (Next)
  EmptyFlags := Flags and MASK_MEDIUM_ALLOCATED_TEST;
  Flags := Word(Flags);
  if (EmptyFlags = MASK_MEDIUM_ALLOCATED_VALUE) then
  begin
    Dec(NewB16Count{GrowCount}, Flags);
    Flags := Flags shl 4;
    Inc(NativeUInt(Header), Flags);
    EmptyFlags := Header.Flags;
    if (EmptyFlags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE) and
      (Header.PreviousSize = Flags) then
    begin
      Inc(NewB16Count, 2);
      if (EmptyFlags >= NewB16Count{ + 2}) then
      begin
        Dec(NewB16Count, 2);
        EmptyFlags := EmptyFlags shl 4;
        NewB16Count := NewB16Count shl 4;
        Inc(NativeUInt(Header), EmptyFlags);

        Dec(EmptyFlags, NewB16Count);
        Inc(NewB16Count, Flags);
        PHeaderMediumEmptyEx(Header{Empty}).Size := EmptyFlags;

        Dec(NativeUInt(Header), EmptyFlags);
        EmptyFlags := EmptyFlags shr 4;
        Header.Flags := EmptyFlags;
        Header.PreviousSize := NewB16Count;

        // result
        Dec(Header);
        Dec(NativeUInt(Header), NewB16Count);
        NewB16Count := NewB16Count shr 4;
        Header.B16Count := NewB16Count;
        Result := Pointer(@PHeaderMediumList(Header)[0]);
        Exit;
      end;
      Dec(NewB16Count, 2);
    end;

    Dec(NativeUInt(Header), Flags);
    Flags := Flags shr 4;
    Inc(NewB16Count, Flags);
  end;

  Result := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.PenaltyReallocMediumToMedium(Pointer(Header), NewB16Count);
end;
{$else}
asm
  // if (not Allocated(P)) then PenaltyReallocMediumToMedium
  {$ifdef CPUX86}
    mov eax, [EDX - 16].THeaderMedium.Flags
    push ebx
    mov ebx, MASK_MEDIUM_ALLOCATED_TEST
    and ebx, eax
    movzx eax, ax
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
  {$else .CPUX64}
    mov rcx, [RDX - 16].THeaderMedium.Flags
    mov r9, MASK_MEDIUM_ALLOCATED_TEST
    and r9, rcx
    movzx rcx, cx
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
  {$endif}
  jne @penalty_reallocmem

  // GrowCount := NewB16Count - Header(P).P16Count
  // if (not Next.Empty) then PenaltyReallocMediumToMedium
  {$ifdef CPUX86}
    sub ecx, eax
    shl eax, 4
    cmp [EDX + eax].THeaderMedium.PreviousSize, eax
    lea edx, [edx + eax]
    jne @penalty_reallocmem_restore
    mov ebx, [EDX].THeaderMedium.Flags
    test ebx, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_reallocmem_restore
  {$else .CPUX64}
    sub r8, rcx
    shl rcx, 4
    cmp [RDX + rcx].THeaderMedium.PreviousSize, rcx
    lea rdx, [rdx + rcx]
    jne @penalty_reallocmem_restore
    mov r9, [RDX].THeaderMedium.Flags
    test r9, MASK_MEDIUM_EMPTY_TEST
    jnz @penalty_reallocmem_restore
  {$endif}

  // if (EmptyFlags < NewB16Count + 2) then PenaltyReallocMediumToMedium
  {$ifdef CPUX86}
    add ecx, 2
    cmp ebx, ecx
    lea ecx, [ecx - 2]
  {$else .CPUX64}
    add r8, 2
    cmp r9, r8
    lea r8, [r8 - 2]
  {$endif}
  jb @penalty_reallocmem_restore

  // empty reduce routine, result
  {$ifdef CPUX86}
    shl ebx, 4
    shl ecx, 4
    add edx, ebx

    sub ebx, ecx
    add ecx, eax
    mov [EDX].THeaderMediumEmptyEx.Size, ebx

    sub edx, ebx
    shr ebx, 4
    mov [EDX].THeaderMedium.PreviousSize, ecx
    mov [EDX].THeaderMedium.Flags, ebx

    sub edx, ecx
    shr ecx, 4
    pop ebx
    mov [EDX - 16].THeaderMedium.B16Count, cx
    xchg eax, edx
  {$else .CPUX64}
    shl r9, 4
    shl r8, 4
    add rdx, r9

    sub r9, r8
    add r8, rcx
    mov [RDX].THeaderMediumEmptyEx.Size, r9

    sub rdx, r9
    shr r9, 4
    mov [RDX].THeaderMedium.PreviousSize, r8
    mov [RDX].THeaderMedium.Flags, r9

    sub rdx, r8
    shr r8, 4
    mov [RDX - 16].THeaderMedium.B16Count, r8w
    xchg rax, rdx
  {$endif}

  // Exit
  ret
@penalty_reallocmem_restore:
  {$ifdef CPUX86}
    sub edx, eax
    shr eax, 4
    add ecx, eax
  {$else .CPUX64}
    sub rdx, rcx
    shr rcx, 4
    add r8, rcx
  {$endif}
@penalty_reallocmem:
  {$ifdef CPUX86}
    mov eax, MASK_K64_CLEAR
    and eax, edx
    mov eax, [EAX].TK64PoolMedium.ThreadHeap
    pop ebx
  {$else .CPUX64}
    mov rcx, MASK_K64_CLEAR
    and rcx, rdx
    mov rcx, [RCX].TK64PoolMedium.ThreadHeap
  {$endif}
  jmp TThreadHeap.PenaltyReallocMediumToMedium
end;
{$endif}

function TThreadHeap.PenaltyReallocMediumToMedium(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
label
  get_copy_free, ptr_invalid;
var
  ALIGN_OFFSETS: PMediumAlignOffsets;
  Header, NextHeader: PHeaderMedium;
  Information, Size: NativeUInt;
  VacantB16Count, AlignOffset: NativeUInt;
  Pool: PK64PoolMedium;
  Empty, Left, Right: PHeaderMediumEmpty;
  ThreadHeap: PThreadHeap;
begin
  // inspect
  Header := P;
  Information := InspectAllocatedMedium(P);

  Dec(Header);
  if (Information = High(NativeUInt)) then goto ptr_invalid;
  if (Word(Information) >= Word(NewB16Count)) then
  begin
    // grow right, make empty piece shorter (or remove)
    VacantB16Count := NativeUInt(Word(Information)) - NewB16Count;
    if (VacantB16Count > 1) then
    begin
      VacantB16Count := VacantB16Count - 1;
      PHeaderMediumList(Header)[NativeUInt(Word(Information))].PreviousSize := VacantB16Count shl 4;
      PHeaderMediumList(Header)[NewB16Count].Flags := {NewB16Count, Align: ma16Bytes, Allocated: False}VacantB16Count;
    end else
    begin
      Inc(NewB16Count, VacantB16Count);
      Empty := Pointer(@PHeaderMediumList(Header)[NativeUInt(Word(Information)) - 1]);
      Left := Empty.Prev;
      Right := Empty.Next;
      Left.Next := Right;
      Right.Prev := Left;
    end;
  end else
  begin
    if (Information > ((1 shl OFFSET_MEDIUM_LEFT_B16COUNT) - 1)) then
    begin
      // try grow left
      ALIGN_OFFSETS := @MEDIUM_ALIGN_OFFSETS[(Information shr OFFSET_MEDIUM_ALIGN) and NativeUInt(High(TMemoryAlign))];
      Size := (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4;
      AlignOffset := ALIGN_OFFSETS.Values[((NativeUInt(Header) - Size) and MASK_K4_TEST) shr 4];
      if ((Size shr 4) + NativeUInt(Word(Information)) < AlignOffset + NewB16Count) then goto get_copy_free;

      // remove left empty
      Empty := Pointer(NativeUInt(Header) - SizeOf(THeaderMediumEmpty));
      Left := Empty.Prev;
      Right := Empty.Next;
      Left.Next := Right;
      Right.Prev := Left;

      // copying
      NcMoveB16(P16(@PHeaderMediumList(Header)[0])^,
        P16(NativeUInt(Header) + SizeOf(THeaderMedium) -
         {Size}((Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4) +
         (AlignOffset shl 4))^,
        Header.B16Count);

      // left align offset
      if (AlignOffset <> 0) then
      begin
        Size := (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4;
        NextHeader := Header;
        Header := OffsetHeaderMediumEmpty(Pointer(NativeUInt(Header) - Size), AlignOffset);
        if (Header = nil) then
        begin
          Header := NextHeader;
          goto ptr_invalid;
        end;
      end else
      begin
        Dec(NativeUInt(Header), (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) shl 4);
      end;

      // create, resize of remove right empty piece
      VacantB16Count := NativeUInt(Word(Information)) +
        (Information shr OFFSET_MEDIUM_LEFT_B16COUNT) - AlignOffset - NewB16Count;
      NextHeader := @PHeaderMediumList(Header)[NewB16Count];
      if (VacantB16Count > 1) then
      begin
        VacantB16Count := VacantB16Count - 1;
        NextHeader.Flags := {NewB16Count, Align: ma16Bytes, Allocated: False}VacantB16Count;
        Empty := Pointer(@PHeaderMediumList(NextHeader)[VacantB16Count - 1]);
        PHeaderMediumEmptyEx(Empty).Size := VacantB16Count shl 4;

        if (Information and FLAG_MEDIUM_RIGHT_EMPTY = 0) then
        begin
          Pool := PK64PoolMedium(NativeInt(Empty) and MASK_K64_CLEAR);
          Left := Pool.Empties.Last.Prev;
          Left.Next := Empty;
          Empty.Prev := Left;
          Empty.Next := @Pool.Empties.Last;
          Pool.Empties.Last.Prev := Empty;
        end;
      end else
      begin
        Inc(NewB16Count, VacantB16Count);

        if (Information and FLAG_MEDIUM_RIGHT_EMPTY <> 0) then
        begin
          Empty := Pointer(@PHeaderMediumList(Header)[NewB16Count - 1]);
          Left := Empty.Prev;
          Right := Empty.Next;
          Left.Next := Right;
          Right.Prev := Left;
        end;
      end;
    end else
    begin
    get_copy_free:
      ThreadHeap := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap;
      Result := ThreadHeap.GetMedium(NewB16Count, (Information shr OFFSET_MEDIUM_ALIGN) and NativeUInt(High(TMemoryAlign)));
      if (Result <> nil) then
        NcMoveB16(P16(@PHeaderMediumList(Header)[0])^, P16(Result)^, Header.B16Count);

      if (ThreadHeap.FreeMedium(@PHeaderMediumList(Header)[0]) <> FREEMEM_DONE) then
      begin
      ptr_invalid:
        ThreadHeap := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap;
        Result := Pointer(ThreadHeap.RaiseInvalidPtr);
      end;
      Exit;
    end;
  end;

  // result
  Header.Flags := (Information and MASK_MEDIUM_ALIGN) + MASK_MEDIUM_ALLOCATED + NewB16Count;
  Size := NewB16Count shl 4;
  Inc(Header);
  Inc(NativeUInt(Header), Size);
  Header.PreviousSize := Size;
  Dec(NativeUInt(Header), Size);
  Result := Header;
end;

function TThreadHeap.FreeDifficult(P: Pointer; ReturnAddress: Pointer): Integer;
var
  Pool: Pointer;
  PoolThreadHeap: PThreadHeap;
  Line: PK1LineSmall;
  ItemSet: TBitSet8;
  Index: NativeInt;
  Flags, B16Count: NativeUInt;
begin
  Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);
  PoolThreadHeap := TK64PoolSmall(Pool^).ThreadHeap;
  if (PoolThreadHeap <> nil) then
  begin
    // pool small
    if (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // check pointer
    Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
    repeat
      ItemSet := Line.Header.ItemSet;
    until (ItemSet.V64 = Line.Header.ItemSet.V64);
    Index := (NativeInt(P) and MASK_K1_TEST) shr 4;
    if (ItemSet.VLow32 and 3 = 0{not FullQueue}) and
      (ItemSet.VIntegers[Index shr 5] and (1 shl (Index and 31)) <> 0{not Allocated}) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // thread deffered
    PoolThreadHeap.PushThreadDeferred(P, ReturnAddress, True);
  end else
  begin
    // pool medium
    PoolThreadHeap := TK64PoolMedium(Pool^).ThreadHeap;
    if (PoolThreadHeap = nil) or (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // check pointer
    Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;
    B16Count := Word(Flags);
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) or
      (PHeaderMediumList(P)[B16Count - 1].PreviousSize <> B16Count shl 4) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // thread deffered
    PoolThreadHeap.PushThreadDeferred(P, ReturnAddress, False);
  end;

  // after thread deffered result
  Result := FREEMEM_DONE;
end;

function TThreadHeap.RegetDifficult(P: Pointer; NewB16Count: NativeUInt;
  ReturnAddress: Pointer): Pointer;
var
  Pool: Pointer;
  PoolThreadHeap: PThreadHeap;
  Line: PK1LineSmall;
  ItemSet: TBitSet8;
  Index: NativeInt;
  IsSmall: Boolean;
  Align: TMemoryAlign;
  R: Integer;
  Flags, B16Count: NativeUInt;
begin
  Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);
  PoolThreadHeap := TK64PoolSmall(Pool^).ThreadHeap;
  if (PoolThreadHeap <> nil) then
  begin
    // pool small
    if (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
      Self.RaiseInvalidPtr;

    // check pointer
    Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
    repeat
      ItemSet := Line.Header.ItemSet;
    until (ItemSet.V64 = Line.Header.ItemSet.V64);
    Index := (NativeInt(P) and MASK_K1_TEST) shr 4;
    if (ItemSet.VLow32 and 3 = 0{not FullQueue}) and
      (ItemSet.VIntegers[Index shr 5] and (1 shl (Index and 31)) <> 0{not Allocated}) then
      Self.RaiseInvalidPtr;

    // small flags
    IsSmall := True;
    Align := ma16Bytes;
  end else
  begin
    // pool medium
    PoolThreadHeap := TK64PoolMedium(Pool^).ThreadHeap;
    if (PoolThreadHeap = nil) or (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
      Self.RaiseInvalidPtr;

    // check pointer
    Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;
    B16Count := Word(Flags);
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) or
      (PHeaderMediumList(P)[B16Count - 1].PreviousSize <> B16Count shl 4) then
      Self.RaiseInvalidPtr;

    // medium flags
    IsSmall := False;
    Align := TMemoryAlign((Flags shr OFFSET_MEDIUM_ALIGN) and NativeUInt(High(TMemoryAlign)));
  end;

  // free small/medium
  if (PoolThreadHeap = @Self) then
  begin
    if (IsSmall) then R := Self.FreeSmall(P)
    else R := Self.FreeMedium(P);

    if (R = FREEMEM_INVALID) then
      Self.RaiseInvalidPtr;
  end else
  begin
    PoolThreadHeap.PushThreadDeferred(P, ReturnAddress, IsSmall);
  end;

  // allocate
  case (NewB16Count) of
    0..MAX_SMALL_B16COUNT:
    begin
      if (Align = ma16Bytes) then Result := Self.GetSmall(NewB16Count)
      else Result := Self.GetMedium(NewB16Count, Ord(Align));
    end;
    MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
      Result := Self.GetMedium(NewB16Count, Ord(Align));
  else
    Result := MemoryManager.BrainMM.GetMemoryPages(
      (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
      PAGESMODE_SYSTEM);
    if (Result = nil) then
      Result := Self.ErrorOutOfMemory;
  end;
end;

function TThreadHeap.ReallocDifficult(P: Pointer; NewB16Count: NativeUInt;
  ReturnAddress: Pointer): Pointer;
var
  LastB16Count: NativeUInt;
  Pool: Pointer;
  PoolThreadHeap: PThreadHeap;
  Line: PK1LineSmall;
  ItemSet: TBitSet8;
  Index: NativeInt;
  IsSmall: Boolean;
  Align: TMemoryAlign;
  R: Integer;
  Flags, B16Count: NativeUInt;
begin
  Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);
  PoolThreadHeap := TK64PoolSmall(Pool^).ThreadHeap;
  if (PoolThreadHeap <> nil) then
  begin
    // pool small
    if (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
      Self.RaiseInvalidPtr;

    // check pointer
    Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
    repeat
      ItemSet := Line.Header.ItemSet;
    until (ItemSet.V64 = Line.Header.ItemSet.V64);
    Index := (NativeInt(P) and MASK_K1_TEST) shr 4;
    if (ItemSet.VLow32 and 3 = 0{not FullQueue}) and
      (ItemSet.VIntegers[Index shr 5] and (1 shl (Index and 31)) <> 0{not Allocated}) then
      Self.RaiseInvalidPtr;

    // small size/flags
    LastB16Count := NativeUInt(Line.Header.ModeSize) shr 4;
    Align := ma16Bytes;
    IsSmall := True;
  end else
  begin
    // pool medium
    PoolThreadHeap := TK64PoolMedium(Pool^).ThreadHeap;
    if (PoolThreadHeap = nil) or (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
      Self.RaiseInvalidPtr;

    // check pointer
    Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;
    B16Count := Word(Flags);
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) or
      (PHeaderMediumList(P)[B16Count - 1].PreviousSize <> B16Count shl 4) then
      Self.RaiseInvalidPtr;

    // medium size/flags
    LastB16Count := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).B16Count;
    Align := TMemoryAlign((Flags shr OFFSET_MEDIUM_ALIGN) and NativeUInt(High(TMemoryAlign)));
    IsSmall := False;
  end;

  // allocate
  case (NewB16Count) of
    0..MAX_SMALL_B16COUNT:
    begin
      if (Align = ma16Bytes) then Result := Self.GetSmall(NewB16Count)
      else Result := Self.GetMedium(NewB16Count, Ord(Align));
    end;
    MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
      Result := Self.GetMedium(NewB16Count, Ord(Align));
  else
    Result := MemoryManager.BrainMM.GetMemoryPages(
      (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
      PAGESMODE_SYSTEM);
    if (Result = nil) then
      Result := Self.ErrorOutOfMemory;
  end;

  // copy (minimum)
  if (Result <> nil) then
  begin
    if (LastB16Count > NewB16Count) then LastB16Count := NewB16Count;
    NcMoveB16(P16(P)^, P16(Result)^, LastB16Count);
  end;

  // free small/medium
  if (PoolThreadHeap = @Self) then
  begin
    if (IsSmall) then R := Self.FreeSmall(P)
    else R := Self.FreeMedium(P);

    if (R = FREEMEM_INVALID) then
      Self.RaiseInvalidPtr;
  end else
  begin
    PoolThreadHeap.PushThreadDeferred(P, ReturnAddress, IsSmall);
  end;
end;

function TThreadHeap.NewK64PoolSmall: PK64PoolSmall;
var
  PagesMode: NativeUInt;
  Next: PK64PoolSmall;
begin
  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  Result := MemoryManager.BrainMM.GetMemoryBlock(BLOCK_64K, PagesMode);
  if (Result = nil) then
  begin
    Result := Self.ErrorOutOfMemory;
    Exit;
  end;

  Result.Header.Flags := 0{not FullQueue};
  Result.ThreadHeap := @Self;
  Result.LineSet.V64 := -1{Empty};

  // Enqueue
  Next := QK64PoolSmall;
  QK64PoolSmall := Result;
  Result.Queue.Prev := nil;
  Result.Queue.Next := Next;
  if (Next <> nil) then Next.Queue.Prev := Result;
end;

function TThreadHeap.DisposeK64PoolSmall(PoolSmall: PK64PoolSmall): Integer;
var
  PagesMode: NativeUInt;
  Prev, Next: PK64PoolSmall;
begin
  // Dequeue
  Prev := PoolSmall.Queue.Prev;
  Next := PoolSmall.Queue.Next;
  if (Prev = nil) then
  begin
    QK64PoolSmall := Next;
  end else
  begin
    Prev.Queue.Next := Next;
  end;
  if (Next <> nil) then
  begin
    Next.Queue.Prev := Prev;
  end;

  PoolSmall.LineSet.V64 := 0{none available lines};
  PoolSmall.ThreadHeap := nil;
  PK64PoolMedium(PoolSmall).ThreadHeap := nil;

  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  if (MemoryManager.BrainMM.FreeMemoryBlock(PoolSmall, PagesMode)) then
  begin
    Result := FREEMEM_DONE;
  end else
  begin
    Result := Self.ErrorInvalidPtr;
  end;
end;

function TThreadHeap.GetNewK1LineSmall(B16Count: NativeUInt): Pointer;
label
  allocated_pool, new_k64pool;
var
  Index: NativeUInt;
  Line: PK1LineSmall;
  PoolSmall, Next, NextFull: PK64PoolSmall;

  P: PInteger;
  VInteger: Integer;
  BitSet: PBitSet8;

  {$ifdef CPUX86}
  ThreadHeap: PThreadHeap;
  {$endif}
begin
  // take small pool, reserve line, get first
  Next := Self.QK64PoolSmall;
  if (Next <> nil) then
  begin
    repeat
      PoolSmall := Next;
    allocated_pool:
      P := Pointer(@PoolSmall.LineSet);
      P := @PBitSet8(P).VIntegers[Byte(PoolSmall.LineSet.VLow32 = 0)];
      VInteger := P^;
      if ({has available lines}VInteger <> 0) then
      begin
        // reserve line
        Inc(PWord(P), Byte(VInteger and $ffff = 0));
        Inc(NativeUInt(P), Byte(PByte(P)^ = 0));
        Index := (NativeUInt(P) - NativeUInt(@PoolSmall.LineSet)) shl 3 + BIT_SCANS[PByte(P)^];
        PInteger(NativeInt(P) and -4)^ := VInteger xor (1 shl (Index and 31));

        // store line
        {$ifdef CPUX86}
        ThreadHeap := PoolSmall.ThreadHeap;
        {$endif}
        Index := Index shl 10;
        Line := Pointer(PoolSmall);
        Inc(NativeUInt(Line), Index);
        {$ifdef CPUX86}
          ThreadHeap.FK1LineSmalls[B16Count] := Line;
        {$else}
          Self.FK1LineSmalls[B16Count] := Line;
        {$endif}
        Line.Header.Queue.Prev := nil;
        Line.Header.Queue.Next := nil;

        // parameters
        Index := {8 * IsFirst: Boolean}((Index - 1) shr ({$ifdef LARGEINT}63{$else}31{$endif} - 3)) and 8;
        Index := Index + {BaseIndex}(B16Count - 1);
        Line.Header.Flags := {ModeSizeBits}(B16Count shl 4) + Index;

        // item set, get first
        BitSet := Pointer(@DEFAULT_BITSETS_SMALL[Index]);
        Index := FIRST_BITSETS_SMALL[Index];
        {$ifdef LARGEINT}
          Line.Header.ItemSet.V64 := BitSet.V64 xor NativeInt(Word(Index));
        {$else .SMALLINT}
          Line.Header.ItemSet.VHigh32 := BitSet.VHigh32;
          Line.Header.ItemSet.VLow32 := BitSet.VLow32 xor NativeInt(Word(Index));
        {$endif}
        Inc(NativeUInt(Line), Index shr 16);
        Result := Line;

        Exit;
      end;

      // Full (QK64PoolSmall) --> (QK64PoolSmallFull)
      begin
        PoolSmall.Header.InQK64PoolSmallFull := True;
        // Dequeue
        Next := {Self.QK64}PoolSmall.Queue.Next;
        Self.QK64PoolSmall := Next;
        // Enqueue
        NextFull := Self.QK64PoolSmallFull;
        Self.QK64PoolSmallFull := PoolSmall;
        { PoolSmall.Queue.Prev := already nil; }
        PoolSmall.Queue.Next := NextFull;
        if (Next <> nil) then
        begin
          PoolSmall.Queue.Prev := nil;
          if (NextFull <> nil) then NextFull.Queue.Prev := PoolSmall;
          // Continue;
        end else
        begin
          if (NextFull <> nil) then NextFull.Queue.Prev := PoolSmall;
          goto new_k64pool;
        end;
      end;
    until (False)
  end else
  begin
  new_k64pool:
    PoolSmall := Self.NewK64PoolSmall;
    if (PoolSmall <> nil) then goto allocated_pool;
  end;

  // failure
  Result := nil;
end;

function TThreadHeap.DisposeK1LineSmall(Line: PK1LineSmall): Integer;
{$ifdef PUREPASCAL}
var
  Index: NativeUInt;
  PoolSmall: PK64PoolSmall;
  Prev, Next: Pointer;
begin
  // Dequeue
  Prev := Line.Header.Queue.Prev;
  Next := Line.Header.Queue.Next;
  if (Prev = nil) then
  begin
    Self.FK1LineSmalls[(Line.Header.Flags and 7) + 1] := Next;
  end else
  begin
    PK1LineSmall(Prev).Header.Queue.Next := Next;
  end;
  if (Next <> nil) then
  begin
    PK1LineSmall(Next).Header.Queue.Prev := Prev;
  end;

  PoolSmall := Pointer(NativeUInt(Line) and MASK_K64_CLEAR);
  if (PoolSmall.Header.InQK64PoolSmallFull) then
  begin
    // QK64PoolSmallFull --> QK64PoolSmall
    PoolSmall.Header.InQK64PoolSmallFull := False;
    // Dequeue
    Prev := PoolSmall.Queue.Prev;
    Next := PoolSmall.Queue.Next;
    if (Prev = nil) then
    begin
      Self.QK64PoolSmallFull := Next;
    end else
    begin
      PK64PoolSmall(Prev).Queue.Next := Next;
    end;
    if (Next <> nil) then
    begin
      PK64PoolSmall(Next).Queue.Prev := Prev;
    end;
    // Enqueue
    Next := Self.QK64PoolSmall;
    Self.QK64PoolSmall := PoolSmall;
    PoolSmall.Queue.Prev := nil;
    PoolSmall.Queue.Next := Next;
    if (Next <> nil) then PK64PoolSmall(Next).Queue.Prev := PoolSmall;
  end;

  Line.Header.ItemSet.V64 := 0{non available items};
  Line.Header.Flags := 0;
  Index := (NativeUInt(Line) and MASK_K64_TEST) shr 10;

  if (not BitUnreserve(PoolSmall.LineSet, Index)) then
  begin
    Result := Self.ErrorInvalidPtr;
    Exit;
  end;

  if (PoolSmall.LineSet.V64 = -1{Empty}) then
  begin
    Result := DisposeK64PoolSmall(PoolSmall);
  end else
  begin
    Result := FREEMEM_DONE;
  end;
end;
{$else}
asm
  // v1 := @Self.FK1LineSmalls[(Line.Header.Flags and 7) + 1] - TK1LineSmall.Header.Queue.Next
  // x86: store (v4)
  {$ifdef CPUX86}
    mov ecx, [EDX].TK1LineSmall.Header.Flags
    and ecx, 7
    lea eax, [eax + TThreadHeap.FK1LineSmalls - TK1LineSmall.Header.Queue.Next]
    push ebx
    lea eax, [eax + 4 * ecx]
  {$else .CPUX64}
    mov r8, [RDX].TK1LineSmall.Header.Flags
    and r8, 7
    lea rcx, [rcx + TThreadHeap.FK1LineSmalls - TK1LineSmall.Header.Queue.Next]
    lea rcx, [rcx + 8 * r8]
  {$endif}

  // dequeue
  {$ifdef CPUX86}
    mov ecx, [EDX].TK1LineSmall.Header.Queue.Prev
    mov ebx, [EDX].TK1LineSmall.Header.Queue.Next
    test ecx, ecx
    DB $0F, $45, $C1 // cmovnz eax, ecx
    mov [EAX].TK1LineSmall.Header.Queue.Next, ebx
    lea eax, [esp - 64]
    test ebx, ebx
    DB $0F, $45, $C3 // cmovnz eax, ebx
    mov [EAX].TK1LineSmall.Header.Queue.Prev, ecx
  {$else .CPUX64}
    mov r8, [RDX].TK1LineSmall.Header.Queue.Prev
    mov r9, [RDX].TK1LineSmall.Header.Queue.Next
    test r8, r8
    cmovnz rcx, r8
    mov [RCX].TK1LineSmall.Header.Queue.Next, r9
    lea rcx, [rsp - 64]
    test r9, r9
    cmovnz rcx, r9
    mov [RCX].TK1LineSmall.Header.Queue.Prev, r8
  {$endif}

  // v3 := PoolSmalOf(Line)
  // if PoolSmall(v3).Header.InQK64PoolSmallFull then goto Requeue(QK64PoolSmallFull, QK64PoolSmall);
  {$ifdef CPUX86}
    mov ecx, MASK_K64_CLEAR
    and ecx, edx
    cmp [ECX].TK1LineSmall.Header.Flags, $00ffffff
  {$else .CPUX64}
    mov r8, MASK_K64_CLEAR
    and r8, rdx
    cmp [R8].TK1LineSmall.Header.Flags, $00ffffff
  {$endif}
  ja @pool_small_requeue

  // clear line flags
  // Index (v2) := IndexOf(Line)
@dispose_line:
  {$ifdef CPUX86}
    xor eax, eax
    mov [EDX].TK1LineSmall.Header.ItemSet.VLow32, eax
    mov [EDX].TK1LineSmall.Header.ItemSet.VHigh32, eax
    mov [EDX].TK1LineSmall.Header.Flags, eax
    and edx, MASK_K64_TEST
    shr edx, 10
  {$else .CPUX64}
    xor rcx, rcx
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, rcx
    mov [RDX].TK1LineSmall.Header.Flags, rcx
    and rdx, MASK_K64_TEST
    shr rdx, 10
  {$endif}

  // if (not BitUnreserve(PoolSmall.LineSet, Index)) then Exit Self.ErrorInvalidPtr;
  {$ifdef CPUX86}
    lea eax, [ECX].TK64PoolSmall.LineSet.VLow32
    lea ebx, [ECX].TK64PoolSmall.LineSet.VHigh32
    test edx, 32
    DB $0F, $45, $C3 // cmovnz eax, ebx
    and edx, 31
    mov ebx, [eax]
    bts ebx, edx
    mov [eax], ebx
    pop ebx
  {$else .CPUX64}
    mov rax, [R8].TK64PoolSmall.LineSet.V64
    bts rax, rdx
    mov [R8].TK64PoolSmall.LineSet.V64, rax
  {$endif}
  jc @error_invalid_ptr

  // if (PoolSmall(v3).LineSet.V64 = -1{Empty}) then Exit Self.DisposeK64PoolSmall(PoolSmall);
  {$ifdef CPUX86}
    mov edx, [ECX].TK64PoolSmall.LineSet.VLow32
    mov eax, [ECX].TK64PoolSmall.LineSet.VHigh32
    add edx, 1
    add eax, 1
    or edx, eax
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
    xchg edx, ecx
  {$else .CPUX64}
    cmp rax, -1
    mov rcx, [R8].TK64PoolSmall.ThreadHeap
    xchg rdx, r8
  {$endif}
  je TThreadHeap.DisposeK64PoolSmall

  // result
  mov eax, FREEMEM_DONE
  ret

@pool_small_requeue:
  // PoolSmall.Header.InQK64PoolSmallFull := False
  {$ifdef CPUX86}
    mov [ECX].TK64PoolSmall.Header.InQK64PoolSmallFull, 0
  {$else .CPUX64}
    mov [R8].TK64PoolSmall.Header.InQK64PoolSmallFull, 0
  {$endif}

  // retrieve Self, store line
  // x64: store Self
  {$ifdef CPUX86}
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
    push edx
  {$else .CPUX64}
    mov rcx, [R8].TK64PoolSmall.ThreadHeap
    xchg rax, rdx
    mov r11, rcx
  {$endif}

  // dequeue
  {$ifdef CPUX86}
    lea eax, [EAX + TK64PoolSmall.QK64PoolSmallFull - TK64PoolSmall.Queue.Next]
    mov edx, [ECX].TK64PoolSmall.Queue.Prev
    mov ebx, [ECX].TK64PoolSmall.Queue.Next
    test edx, edx
    DB $0F, $45, $C2 // cmovnz eax, edx
    mov [EAX].TK64PoolSmall.Queue.Next, ebx
    lea eax, [esp - 64]
    test ebx, ebx
    DB $0F, $45, $C3 // cmovnz eax, ebx
    mov [EAX].TK64PoolSmall.Queue.Prev, edx
  {$else .CPUX64}
    lea rcx, [RCX + TK64PoolSmall.QK64PoolSmallFull - TK64PoolSmall.Queue.Next]
    mov rdx, [R8].TK64PoolSmall.Queue.Prev
    mov r9, [R8].TK64PoolSmall.Queue.Next
    test rdx, rdx
    cmovnz rcx, rdx
    mov [RCX].TK64PoolSmall.Queue.Next, r9
    lea rcx, [rsp - 64]
    test r9, r9
    cmovnz rcx, r9
    mov [RCX].TK64PoolSmall.Queue.Prev, rdx
  {$endif}

  // retrieve Self, enqueue, retrieve Line
  {$ifdef CPUX86}
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
    mov edx, [EAX].TThreadHeap.QK64PoolSmall
    mov [EAX].TThreadHeap.QK64PoolSmall, ecx
    mov [ECX].TK64PoolSmall.Queue.Prev, 0
    mov [ECX].TK64PoolSmall.Queue.Next, edx
    lea ebx, [esp - 64]
    test edx, edx
    DB $0F, $45, $DA // cmovnz ebx, edx
    mov [EBX].TK64PoolSmall.Queue.Prev, ecx
    pop edx
  {$else .CPUX64}
    xchg rcx, r11
    mov rdx, [RCX].TThreadHeap.QK64PoolSmall
    mov [RCX].TThreadHeap.QK64PoolSmall, r8
    xor r11, r11
    mov [R8].TK64PoolSmall.Queue.Prev, r11
    mov [R8].TK64PoolSmall.Queue.Next, rdx
    lea r9, [rsp - 64]
    test rdx, rdx
    cmovnz r9, rdx
    mov [R9].TK64PoolSmall.Queue.Prev, r8
    xchg rdx, rax
  {$endif}

  jmp @dispose_line
@error_invalid_ptr:
  {$ifdef CPUX86}
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
  {$else .CPUX64}
    mov rcx, [R8].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr
end;
{$endif}

function TThreadHeap.NewK64PoolMedium: PK64PoolMedium;
var
  PagesMode: NativeUInt;
  Start: PHeaderMedium;
  Empty: PHeaderMediumEmpty;
  Next: PK64PoolMedium;
begin
  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  Result := MemoryManager.BrainMM.GetMemoryBlock(BLOCK_64K, PagesMode);
  if (Result = nil) then
  begin
    Result := Self.ErrorOutOfMemory;
    Exit;
  end;

  Result.MarkerNil := nil;
  Result.ThreadHeap := @Self;

  Result.FakeAllocated.PreviousSize := NativeUInt(-1);
  Result.FakeAllocated.Flags := MASK_MEDIUM_ALLOCATED;

  Start := @Result.Items[Low(Result.Items)];
  Start.PreviousSize := 0;
  Start.Flags := {B16Count}(High(Result.Items) + 1) + {Align: ma16Bytes, Allocated: False}0;
  Result.Finish.PreviousSize := (High(Result.Items) + 1) * SizeOf(THeaderMedium);
  Result.Finish.Flags := {failure B16Count}0 + (Ord(ma16Bytes) shl 16) + MASK_MEDIUM_ALLOCATED;

  Empty := Pointer(@Result.Items[High(Result.Items)]);
  Result.Empties.First.Prev := nil;
  Result.Empties.First.Next := Empty;
  Empty.Prev := @Result.Empties.First;
  Empty.Next := @Result.Empties.Last;
  Result.Empties.Last.Prev := Empty;
  Result.Empties.Last.Next := nil;

  // Enqueue
  Next := QK64PoolMedium;
  QK64PoolMedium := Result;
  Result.Queue.Prev := nil;
  Result.Queue.Next := Next;
  if (Next <> nil) then Next.Queue.Prev := Result;
end;

function TThreadHeap.DisposeK64PoolMedium(PoolMedium: PK64PoolMedium): Integer;
var
  PagesMode: NativeUInt;
  Prev, Next: PK64PoolMedium;
begin
  // Dequeue
  Prev := PoolMedium.Queue.Prev;
  Next := PoolMedium.Queue.Next;
  if (Prev = nil) then
  begin
    QK64PoolMedium := Next;
  end else
  begin
    Prev.Queue.Next := Next;
  end;
  if (Next <> nil) then
  begin
    Next.Queue.Prev := Prev;
  end;

  PoolMedium.ThreadHeap := nil;
  PK64PoolSmall(PoolMedium).ThreadHeap := nil;

  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  if (MemoryManager.BrainMM.FreeMemoryBlock(PoolMedium, PagesMode)) then
  begin
    Result := FREEMEM_DONE;
  end else
  begin
    Result := Self.ErrorInvalidPtr;
  end;
end;


{ TJITHeap }

type
  PJITHashItem = ^TJITHashItem;
  TJITHashItem = packed record
    Next: PJITHashItem;
    Pages: MemoryPages;
  end;

function TJITHeap.HeapInstance: {PThreadHeap}Pointer;
begin
  Result := Pointer(NativeInt(@FHeapBuffer[63]) and MASK_64_CLEAR);
end;

constructor TJITHeap.Create;
begin
  inherited;
  PThreadHeap(HeapInstance).FNextHeap := JITHEAP_MARKER;
end;

destructor TJITHeap.Destroy;
begin
  Clear;
  inherited;
end;

procedure TJITHeap.EnqueueBigOrLarge(Pages: MemoryPages);
var
  Heap: PThreadHeap;
  Index: NativeUInt;
  HashItem: PJITHashItem;
begin
  Heap := HeapInstance;
  Index := (NativeUInt(Pages) shr (10 + 2)) and High(FBigOrLargeHash);

  HashItem := Heap.GetSmall(1);
  HashItem.Next := FBigOrLargeHash[Index];
  HashItem.Pages := Pages;
  FBigOrLargeHash[Index] := HashItem;
end;

function TJITHeap.DequeueBigOrLarge(Pages: MemoryPages): Boolean;
var
  Heap: PThreadHeap;
  Index: NativeUInt;
  HashItem, Next: PJITHashItem;
begin
  Heap := HeapInstance;
  Index := (NativeUInt(Pages) shr (10 + 2)) and High(FBigOrLargeHash);

  HashItem := FBigOrLargeHash[Index];
  if (HashItem <> nil) then
  begin
    Next := HashItem.Next;
    if (HashItem.Pages = Pages) then
    begin
      FBigOrLargeHash[Index] := Next;
      Heap.FreeSmall(HashItem);
      Result := True;
      Exit;
    end else
    begin
      repeat
        if (Next = nil) then Break;

        if (Next.Pages = Pages) then
        begin
          HashItem.Next := Next.Next;
          Heap.FreeSmall(Next);
          Result := True;
          Exit;
        end;

        HashItem := Next;
        Next := Next.Next;
      until (False);
    end;
  end;

  Result := False;
end;

{$ifdef CPUINTEL}
procedure JITHeapClear(Self: TJITHeap; ReturnAddress: Pointer); forward;
procedure TJITHeap.Clear;
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
  {$else .CPUX64}
    mov r8, [rsp]
  {$endif}
  jmp JITHeapClear
end;
{$endif}

{$ifdef CPUINTEL}
procedure JITHeapClear(Self: TJITHeap; ReturnAddress: Pointer);
{$else}
procedure TJITHeap.Clear;
{$endif}
var
  Heap: PThreadHeap;
  Index: NativeUInt;
  HashItem: PJITHashItem;
  PoolSmall: PK64PoolSmall;
  PoolMedium: PK64PoolMedium;
  Next: Pointer;
begin
  Heap := {HeapInstance}Pointer(NativeInt(@Self.FHeapBuffer[63]) and MASK_64_CLEAR);
  Heap.ErrorAddr := ReturnAddress;

  // clear big or larges
  for Index := Low(Self.FBigOrLargeHash) to High(Self.FBigOrLargeHash) do
  begin
    HashItem := Self.FBigOrLargeHash[Index];
    if (HashItem <> nil) then
    begin
      Self.FBigOrLargeHash[Index] := nil;
      repeat
        if (not MemoryManager.BrainMM.FreeMemoryPages(HashItem.Pages, PAGESMODE_JIT)) then
        begin
          Heap.RaiseInvalidPtr;
        end;

        HashItem := HashItem.Next;
      until (HashItem = nil);
    end;
  end;

  // 1kb-lines (small)
  Heap.QK1LineFull := nil;
  for Index := Low(Heap.FK1LineSmalls) to High(Heap.FK1LineSmalls) do
    Heap.FK1LineSmalls[Index] := nil;

  // pool small
  PoolSmall := Heap.QK64PoolSmall;
  if (PoolSmall <> nil) then
  begin
    Heap.QK64PoolSmall := nil;
    repeat
      Next := PoolSmall.Queue.Next;

      if (not MemoryManager.BrainMM.FreeMemoryBlock(PoolSmall, PAGESMODE_JIT)) then
      begin
        Heap.RaiseInvalidPtr;
      end;

      PoolSmall := Next;
    until (PoolSmall = nil);
  end;

  // pool small (full)
  PoolSmall := Heap.QK64PoolSmallFull;
  if (PoolSmall <> nil) then
  begin
    Heap.QK64PoolSmallFull := nil;
    repeat
      Next := PoolSmall.Queue.Next;

      if (not MemoryManager.BrainMM.FreeMemoryBlock(PoolSmall, PAGESMODE_JIT)) then
      begin
        Heap.RaiseInvalidPtr;
      end;

      PoolSmall := Next;
    until (PoolSmall = nil);
  end;

  // pool medium
  PoolMedium := Heap.QK64PoolMedium;
  if (PoolMedium <> nil) then
  begin
    Heap.QK64PoolMedium := nil;
    repeat
      Next := PoolMedium.Queue.Next;

      if (not MemoryManager.BrainMM.FreeMemoryBlock(PoolMedium, PAGESMODE_JIT)) then
      begin
        Heap.RaiseInvalidPtr;
      end;

      PoolMedium := Next;
    until (PoolMedium = nil);
  end;
end;

{$ifdef CPUINTEL}
function JITHeapGetMemory(Self: TJITHeap; Size: NativeInt; ReturnAddress: Pointer): Pointer; forward;
function TJITHeap.GetMemory(Size: NativeInt): Pointer;
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
  {$else .CPUX64}
    mov r8, [rsp]
  {$endif}
  jmp JITHeapGetMemory
end;
{$endif}

{$ifdef CPUINTEL}
function JITHeapGetMemory(Self: TJITHeap; Size: NativeInt; ReturnAddress: Pointer): Pointer;
{$else}
function TJITHeap.GetMemory(Size: NativeInt): Pointer;
{$endif}
var
  Heap: PThreadHeap;
  B16Count: NativeUInt;
begin
  if (Size > 0) then
  begin
    Heap := {HeapInstance}Pointer(NativeInt(@Self.FHeapBuffer[63]) and MASK_64_CLEAR);
    Heap.ErrorAddr := ReturnAddress;

    B16Count := (Size + 15) shr 4;
    case (B16Count) of
      0..MAX_SMALL_B16COUNT: Result := Heap.GetSmall(B16Count);
      MAX_SMALL_B16COUNT+1..MAX_MEDIUM_B16COUNT:
        Result := Heap.GetMedium(B16Count, Ord(ma16Bytes));
    else
      Result := MemoryManager.BrainMM.GetMemoryPages(
        (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, PAGESMODE_JIT);
      if (Result = nil) then
      begin
        Heap.RaiseOutOfMemory;
      end;

      Self.EnqueueBigOrLarge(MemoryPages(Result));
    end;
  end else
  begin
    Result := nil;
  end;
end;

{$ifdef CPUINTEL}
procedure JITHeapFreeMemory(Self: TJITHeap; P: Pointer; ReturnAddress: Pointer); forward;
procedure TJITHeap.FreeMemory(P: Pointer);
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
  {$else .CPUX64}
    mov r8, [rsp]
  {$endif}
  jmp JITHeapFreeMemory
end;
{$endif}

{$ifdef CPUINTEL}
procedure JITHeapFreeMemory(Self: TJITHeap; P: Pointer; ReturnAddress: Pointer);
{$else}
procedure TJITHeap.FreeMemory(P: Pointer);
{$endif}
label
  medium;
var
  Heap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
begin
  if (P <> nil) then
  begin
    Heap := {HeapInstance}Pointer(NativeInt(@Self.FHeapBuffer[63]) and MASK_64_CLEAR);
    Heap.ErrorAddr := ReturnAddress;

    if (NativeInt(P) and MASK_16_TEST = 0) then
    begin
      Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

      if (NativeInt(P) and MASK_K1_TEST <> 0) then
      begin
        // pool: small or medium
        if (PK64PoolSmall(Pool).ThreadHeap = Heap) then
        begin
          // pool small
          Heap.FreeSmall(P);
          Exit;
        end else
        if (PK64PoolSmall(Pool).ThreadHeap = nil) then
        begin
          // pool medium
        medium:
          if (PK64PoolMedium(Pool).ThreadHeap = Heap) then
          begin
            Heap.FreeMedium(P);
            Exit;
          end;
        end;
      end else
      begin
        if (NativeInt(P) and MASK_K4_TEST = 0) then
        begin
          // big or large
          if (Self.DequeueBigOrLarge(MemoryPages(P))) then
          begin
            if (MemoryManager.BrainMM.FreeMemoryPages(MemoryPages(P), PAGESMODE_JIT)) then
              Exit;
          end;
        end else
        begin
          // medium or invalid pointer
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
        end;
      end;
    end;

    // "default" method
    Heap.RaiseInvalidPtr;
  end;
end;

function TJITHeap.SyncGetMemory(Size: NativeInt): Pointer;
begin
  if (Size > 0) then
  begin
    // inline SpinLock
    repeat
      if (Self.FSpin <> 0) then SpinWait(SupposedPtr(Self.FSpin), High(NativeUInt));
    until (0 = AtomicCmpExchange(SupposedPtr(Self.FSpin), 1, 0));
    try
      Result := Self.GetMemory(Size);
    finally
      FSpin := 0; // inline SpinUnlock
    end;
  end else
  begin
    Result := nil;
  end;
end;

procedure TJITHeap.SyncFreeMemory(P: Pointer);
begin
  if (P <> nil) then
  begin
    // inline SpinLock
    repeat
      if (Self.FSpin <> 0) then SpinWait(SupposedPtr(Self.FSpin), High(NativeUInt));
    until (0 = AtomicCmpExchange(SupposedPtr(Self.FSpin), 1, 0));
    try
      Self.FreeMemory(P);
    finally
      FSpin := 0; // inline SpinUnlock
    end;
  end;
end;


 (* Debug memory routine  *)

function BrainMMRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // todo
  Result := False;
end;

function BrainMMUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // todo
  Result := False;
end;


{$ifdef MSWINDOWS}
type
  TJumpInfo = packed record
    CodeOffset: Cardinal;
    Jump: Pointer;
    JumpOffset: Cardinal;
  end;

procedure PatchCode(const AddrProc: Pointer; const CodeSize: NativeUInt;
  const Code: Pointer);
var
  OldProtect: Cardinal;
begin
  VirtualProtect(AddrProc, CodeSize, PAGE_EXECUTE_READWRITE, OldProtect);
  Move(Code^, AddrProc^, CodeSize);
  VirtualProtect(AddrProc, CodeSize, OldProtect, OldProtect);
end;

procedure PatchRedirect(const AddrProc: Pointer; const CodeSize: NativeUInt;
  const Code: Pointer; const Jumps: array of TJumpInfo);
var
  i: Integer;
  Buffer: array[0..33] of Byte;
begin
  Move(Code^, Buffer, CodeSize);

  for i := Low(Jumps) to High(Jumps) do
  with Jumps[i] do
  begin
    PInteger(@Buffer[CodeOffset])^ := NativeInt(Jump) -
      (NativeInt(AddrProc) + NativeInt(CodeOffset) + 4) + NativeInt(JumpOffset);
  end;

  PatchCode(AddrProc, CodeSize, @Buffer);
end;
{$endif}

{$ifdef BRAINMM_REDIRECT}
// Size: 14/17
// BrainMMGetMem: 7/9
function RedirectGetMem(Size: NativeInt): Pointer;
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
    test eax, eax
  {$else .CPUX64}
    mov r8, [rsp]
    test rcx, rcx
  {$endif}
  jg BrainMMGetMem // + 4
  {$ifdef CPUX86}
    xor eax, eax
  {$else .CPUX64}
    xor rax, rax
  {$endif}
end;

// Size: 14/17
// BrainMMAllocMem: 7/9
function RedirectAllocMem(Size: NativeInt): Pointer;
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
    test eax, eax
  {$else .CPUX64}
    mov r8, [rsp]
    test rcx, rcx
  {$endif}
  jg BrainMMAllocMem // + 4
  {$ifdef CPUX86}
    xor eax, eax
  {$else .CPUX64}
    xor rax, rax
  {$endif}
end;

// Size: 14/15
// BrainMMFreeMem: 4/5
function RedirectFreeMem(P: Pointer): Integer;
asm
  {$ifdef CPUX86}
    test eax, eax
  {$else .CPUX64}
    test rcx, rcx
  {$endif}
  jnz BrainMMFreeMem // + 4
  mov eax, FREEMEM_DONE
end;

// Size: 30/33
// RecallFreeMem: 8/11
// RecallGetMem: 16/20
// BrainMMReallocMem(BrainMMRegetMem): 26/29
function RedirectReallocRegetMem(var P: Pointer; NewSize: NativeInt): Pointer;
asm
  // Value := P
  // if (NewSize <= 0) then Exit RecallFreeMem(Value, 0, P)
  {$ifdef CPUX86}
    mov ecx, eax
    mov eax, [eax]
    test edx, edx
  {$else .CPUX64}
    mov r8, rcx
    mov rcx, [rcx]
    test rdx, rdx
  {$endif}
  jle RecallFreeMem

  // if (Value = nil) Exit RecallGetMem(nil, NewSize, P)
  {$ifdef CPUX86}
    test eax, eax
  {$else .CPUX64}
    test rcx, rcx
  {$endif}
  jz RecallGetMem

  // Exit BrainMMReallocMem+8(Value, NewSize, P, ReturnAddress)
  {$ifdef CPUX86}
    push ebx
    mov ebx, [esp + 4]
  {$else .CPUX64}
    mov r9, [rsp]
  {$endif}
  jmp BrainMMReallocMem // + 8
end;

function AddrGetMem: Pointer;
asm
  {$ifdef CPUX86}
    lea eax, System.@GetMem
  {$else .CPUX64}
    lea rax, System.@GetMem
  {$endif}
end;

function AddrAllocMem: Pointer;
asm
  {$ifdef MEMORYMANAGEREX}
    {$ifdef CPUX86}
      lea eax, System.AllocMem
    {$else .CPUX64}
      lea rax, System.AllocMem
    {$endif}
  {$else}
    {$ifdef CPUX86}
      lea eax, BrainMM.AllocMem
    {$else .CPUX64}
      lea rax, BrainMM.AllocMem
    {$endif}
  {$endif}
end;

function AddrFreeMem: Pointer;
asm
  {$ifdef CPUX86}
    lea eax, System.@FreeMem
  {$else .CPUX64}
    lea rax, System.@FreeMem
  {$endif}
end;

function AddrReallocMem: Pointer;
asm
  {$ifdef CPUX86}
    lea eax, System.@ReallocMem
  {$else .CPUX64}
    lea rax, System.@ReallocMem
  {$endif}
end;

procedure BrainMMRedirectInitialize;
const
  JUMPS_GETMEM: array[0..0] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}7{$else .CPUX64}9{$endif};
      Jump: @BrainMMGetMem; JumpOffset: 4)
  );
  JUMPS_ALLOCMEM: array[0..0] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}7{$else .CPUX64}9{$endif};
      Jump: @BrainMMAllocMem; JumpOffset: 4)
  );
  JUMPS_FREEMEM: array[0..0] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}4{$else .CPUX64}5{$endif};
      Jump: @BrainMMFreeMem; JumpOffset: 4)
  );
  JUMPS_REALLOCMEM: array[0..2] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}8{$else .CPUX64}11{$endif};
      Jump: @RecallFreeMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}16{$else .CPUX64}20{$endif};
      Jump: @RecallGetMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}26{$else .CPUX64}29{$endif};
      Jump: @BrainMMReallocMem; JumpOffset: 8)
  );
  JUMPS_REGETMEM: array[0..2] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}8{$else .CPUX64}11{$endif};
      Jump: @RecallFreeMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}16{$else .CPUX64}20{$endif};
      Jump: @RecallGetMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}26{$else .CPUX64}29{$endif};
      Jump: @BrainMMRegetMem; JumpOffset: 8)
  );
begin
  PatchRedirect(AddrGetMem, {$ifdef CPUX86}14{$else .CPUX64}17{$endif},
    @RedirectGetMem, JUMPS_GETMEM);
  PatchRedirect(AddrAllocMem, {$ifdef CPUX86}14{$else .CPUX64}17{$endif},
    @RedirectAllocMem, JUMPS_ALLOCMEM);
  PatchRedirect(AddrFreeMem, {$ifdef CPUX86}14{$else .CPUX64}15{$endif},
    @RedirectFreeMem, JUMPS_FREEMEM);
  PatchRedirect(AddrReallocMem, {$ifdef CPUX86}30{$else .CPUX64}33{$endif},
    @RedirectReallocRegetMem, JUMPS_REALLOCMEM);
  PatchRedirect(@RegetMem, {$ifdef CPUX86}30{$else .CPUX64}33{$endif},
    @RedirectReallocRegetMem, JUMPS_REGETMEM);
end;
{$endif}

procedure BrainMMInitialize;
{$ifdef THREAD_FUNCS_EMULATE}
const
  JUMP_BYTES: array[1..5] of Byte = ($E9, 0, 0, 0, 0);
  JUMP_BEGIN_THREAD: array[0..0] of TJumpInfo = (
    (CodeOffset: 1; Jump: @__BeginThread; JumpOffset: 0)
  );
  JUMP_END_THREAD: array[0..0] of TJumpInfo = (
    (CodeOffset: 1; Jump: @__EndThread; JumpOffset: 0)
  );
{$endif}
{$ifdef BRAINMM_REDIRECT}{$ifdef CPUX86}
const
  NOP_9BYTES: array[1..9] of Byte = ($66, $0F, $1F, $84, $00, $00, $00, $00, $00);
{$endif}{$endif}
type
  PMemoryMgr = {$ifdef MEMORYMANAGEREX}^TMemoryManagerEx{$else}^TMemoryManager{$endif};
{$ifdef MSWINDOWS}
const
  BRAINMM_MARKER: array[1..25] of AnsiChar = 'BRAIN_MEMORY_MANAGER_PID_';
  HEX_CHARS: array[0..15] of AnsiChar = '0123456789ABCDEF';
var
  i: Integer;
  ProcessId: Cardinal;
  Buffer: array[0..SizeOf(BRAINMM_MARKER) + 8] of AnsiChar;
  Handle: HWND;
  BrainMMRegistered: ^TBrainMemoryManager;
{$endif}
begin
  InitializeOffsetsMedium;

  MemoryManager.BrainMM.ThreadFuncEvent := BrainMMThreadFuncEvent;
  MemoryManager.BrainMM.EndThreadEvent := BrainMMEndThreadEvent;
  MemoryManager.BrainMM.GetMemoryBlock := BrainMMGetMemoryBlock;
  MemoryManager.BrainMM.FreeMemoryBlock := BrainMMFreeMemoryBlock;
  MemoryManager.BrainMM.GetMemoryPages := BrainMMGetMemoryPages;
  MemoryManager.BrainMM.RegetMemoryPages := BrainMMRegetMemoryPages;
  MemoryManager.BrainMM.ReallocMemoryPages := BrainMMReallocMemoryPages;
  MemoryManager.BrainMM.FreeMemoryPages := BrainMMFreeMemoryPages;
  MemoryManager.BrainMM.GetMemAligned := BrainMMGetMemAligned;
  MemoryManager.BrainMM.RegetMem := BrainMMRegetMem;
  MemoryManager.Standard.GetMem := BrainMMGetMem;
  MemoryManager.Standard.FreeMem := BrainMMFreeMem;
  MemoryManager.Standard.ReallocMem := BrainMMReallocMem;
  MemoryManager.Standard.AllocMem := BrainMMAllocMem;
  MemoryManager.Standard.RegisterExpectedMemoryLeak := BrainMMRegisterExpectedMemoryLeak;
  MemoryManager.Standard.UnregisterExpectedMemoryLeak := BrainMMUnregisterExpectedMemoryLeak;

  {$ifdef MSWINDOWS}
  begin
    Move(BRAINMM_MARKER, Buffer, SizeOf(BRAINMM_MARKER));
    ProcessId := GetCurrentProcessId;
    for i := 0 to 7 do
    begin
      Buffer[High(Buffer) - 1 - i] := HEX_CHARS[ProcessId and $f];
      ProcessId := ProcessId shr 4;
    end;
    Buffer[High(Buffer)] := #0;

    Handle := FindWindowA('STATIC', Buffer);
    if (Handle = 0) then
    begin
      Handle := CreateWindowA('STATIC', Buffer, WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
      SetWindowLong(Handle, GWL_USERDATA, NativeInt(@MemoryManager));

      UnknownThreadHeap := CreateThreadHeap(False);
      UnknownThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
      {$ifdef PUREPASCAL}
        MainThreadHeap := CreateThreadHeap(True);
      {$else}
        MainThreadHeap := ThreadHeapInstance;
      {$endif}

      {$ifdef BRAINMM_REDIRECT}
        BrainMMRedirectInitialize;
      {$endif}
    end else
    begin
      BrainMMRegistered := Pointer(GetWindowLong(Handle, GWL_USERDATA));

      for i := Low(BrainMMRegistered.POINTERS) to High(BrainMMRegistered.POINTERS) do
      if (BrainMMRegistered.POINTERS[i] <> nil) then
        MemoryManager.POINTERS[i] := BrainMMRegistered.POINTERS[i];
    end;
  end;
  {$endif}

  {$ifdef MSWINDOWS}
    {$ifdef THREAD_FUNCS_EMULATE}
      PatchRedirect(@System.BeginThread, SizeOf(JUMP_BYTES), @JUMP_BYTES, JUMP_BEGIN_THREAD);
      PatchRedirect(@System.EndThread, SizeOf(JUMP_BYTES), @JUMP_BYTES, JUMP_END_THREAD);
    {$endif}
    SystemThreadFuncProc := MemoryManager.BrainMM.ThreadFuncEvent;
    SystemThreadEndProc := MemoryManager.BrainMM.EndThreadEvent;
  {$else .POSIX}
    BeginThreadProc := MemoryManager.BrainMM.ThreadFuncEvent;
    EndThreadProc := MemoryManager.BrainMM.EndThreadEvent;
  {$endif}

  {$ifdef BRAINMM_REDIRECT}{$ifdef CPUX86}
  if (SSE_SUPPORT <> 0) then
  begin
    PatchCode(@BackwardSSEMove, SizeOf(NOP_9BYTES), @NOP_9BYTES);
    PatchCode(@NcMoveB16, SizeOf(NOP_9BYTES), @NOP_9BYTES);
    PatchCode(@NcMoveB16Small, SizeOf(NOP_9BYTES), @NOP_9BYTES);
  end;
  {$endif}{$endif}

  SetMemoryManager(PMemoryMgr(@MemoryManager.Standard)^);
end;


initialization
  {$ifdef CPUX86}
  CheckSSESupport;
  {$endif}
  {$ifdef MSWINDOWS}
    @SwitchToThreadFunc := GetProcAddress(LoadLibrary(kernel32), 'SwitchToThread');
  {$endif}
  BrainMMInitialize;

finalization

end.
