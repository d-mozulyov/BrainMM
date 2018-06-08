# BrainMM
![](http://dmozulyov.ucoz.net/BrainMM/Logo/Logo.png)

Several years ago, I started studying garbage collection algorithms and memory management principles in such systems. Rare defragmentation and fast memory allocation at the end of the pool are considered to be performed by the memory manager with a more effective principle of garbage collection than the one used in traditional approaches, when allocation and release of memory require complicated slow manipulations. However, as a matter of fact, memory managers with garbage collection principle can hardly be called effective due to the following:
* Realloc always leads to the new memory piece allocation and data copying in it, which does not always take place in traditional-approach managers
* Memory overconsumption takes place, as the pool also contains the memory pieces that have been previously allocated, but not used (released)
* Defragmentation leads to the locking of all threads, long analysis and data copying operations, often leading to an unpleasant application "freezing" effect
* Additional resources are required for the alignment and maintenance of pointers, as after defragmentation the data may be situated at another physical address
* Fast memory allocation principle in the end of the pool is, with rare exception, also applied in traditional memory managers
 
On the other side, as the time goes by, popular memory manager FastMM fails to comply with the requirements of modern applications as it works with locks on multi-thread applications, does not contain API to aligned data, and, depending on the platform, uses minimum 8/16 bytes of service information, even on small memory pieces.

That is why I started working on project BrainMM, i.e. the memory manager designed on the basis of modern application requirements. I sincerely believe that with time the project will grow to the level of standard Delphi/C++ Builder supply, along with other great libraries. BrainMM memory manager features:
* Extremely high performance (*not fully implemented*)
* No locks for memory pieces up to 32Kb
* Support of all operating systems envisaged by Delphi and C++ Builder
* DLL-shared memory (Delphi/FPC compatibility)
* Guaranteed aligned address for 16 bytes. It is useful for lock-free algorithms, SEE operations and in general at memory reading/writing/copying
* RegetMem function. Functionally similar to ReallocMem, but does not guarantee data safety, that is why in some cases it can work faster
* GetMemAligned function. Allows allocating memory with specific alignment. This memory is released in a standard way, via FreeMem. When the size is changed using ReallocMem/RegetMem, the alignment is preserved. The exception is when NewSize equals zero, in this case, FreeMem shall be induced and alignment information will be lost
* API for memory block allocation (*not fully implemented*). BrainMM memory blocks are memory pieces of specific granularity, the size of which is unchangeable. Memory blocks are useful for highly specialized performance-demanding memory management. Service information can be stored at the beginning of the block, access to this information may be received by applying the logical multiplication (`and`) operation to the pointer. The management of small (up to 128 bytes) and medium (up to 32Kb) memory pieces in BrainMM is performed, for example, with the help of blocks of 64Kb
* API for work with memory pages (*not fully implemented*)
* Memory leaks reporting (*not fully implemented*), standard `ReportMemoryLeaksOnShutdow` flag
* Finalizations fixes ([QC #103616](http://qc.embarcadero.com/wc/qcmain.aspx?d=103616))


##### Performance
This test was conducted on the basis of [Steve Maughan's article](http://www.stevemaughan.com/delphi/delphi-parallel-programming-library-memory-managers/). Source codes are in the repository, but you can also [download binary files]( http://dmozulyov.ucoz.net/BrainMM/Demo.rar).

![](http://dmozulyov.ucoz.net/BrainMM/SpeedTest.png)
##### System memory consumption
System memory consumption is always larger than the developer tried to allocate as it is necessary to store service information on each allocated piece. Besides service information, the notion of granularity also influences memory consumption. For example, if you allocate 20 bytes and the granularity equals 16, the size shall be rounded off to 32 bytes.

Source codes of this test are in the repository. The results are presented in megabytes at the rate of 100 Mb of useful data. BrainMM was compared to two popular managers: FastMM, ScaleMM2. The difference of system memory consumption is explained by architecture peculiarities of each manager. Default FastMM x86 granularity is 8 bytes, optionally 16 bytes can be adjusted, for x64 - always 16 bytes. The approximate size of service information is 8 bytes for each piece. In BrainMM, for small pieces (up to 128 bytes), compact bit masks are used, that is why the average system memory consumption is lower. However, BrainMM granularity is always 16 bytes, that is why, for example, for pieces of 8 and 24 bytes, FastMM steals a march, although insignificantly. Large consumption by ScaleMM2 manager is most probably explained by the less economical service information storage. 

![](http://dmozulyov.ucoz.net/BrainMM/MemoryUsageTest.png)

##### What shall be improved
* Management of large memory pieces (more than 32Kb)
* Memory leakage and debugging search methods
* Effective interconnection with operating systems

As an example, I will outline one of the most important tasks to be solved. Let's say, our memory piece consists of 2 pages, on the left 4 pages are free, on the right - 3 pages. If we need to increase memory piece up to 5 pages, right pages shall be tagged as occupied and associated with this piece. If the size should be increased to 6 pages and the number of right pages is not enough, 4 pages on the left can be borrowed and 2 available pages of data can be copied. Both approaches are successfully implemented in FastMM memory manager, but there is no approach that would allow to occupy both 4 pages on the left and 3 pages on the right. Besides, as far as I am aware, operating systems possess APIs allowing to avoid data copying by changing the page address.
```
---0000XX000---
```

##### Extended memory API
```pascal
type
  TMemoryAlign = (ma16Bytes, ma32Bytes, ma64Bytes, ma128Bytes, ma256Bytes,
    ma512Bytes, ma1024Bytes, ma2048Bytes);
  PMemoryAlign = ^TMemoryAlign;

  MemoryBlock = type Pointer;
  PMemoryBlock = ^MemoryBlock;

  TMemoryBlockSize = (BLOCK_4K, BLOCK_16K, BLOCK_64K, BLOCK_256K, BLOCK_1MB,
    BLOCK_4MB, BLOCK_16MB, BLOCK_64MB, BLOCK_256MB);
  PMemoryBlockSize = ^TMemoryBlockSize;

  MemoryPages = type Pointer;
  PMemoryPages = ^MemoryPages;

  TMemoryAccessRight = (marRead, marWrite, marExecute);
  PMemoryAccessRight = ^TMemoryAccessRight;
  TMemoryAccessRights = set of TMemoryAccessRight;
  PMemoryAccessRights = ^TMemoryAccessRights;

  TMemoryKind = (mkSmall, mkMedium, mkBig, mkLarge, mkPages, mkBlock, mkJIT);
  PMemoryKind = ^TMemoryKind;

  TMemoryOptions = packed record
    Kind: TMemoryKind;
    Align: TMemoryAlign;
    BlockSize: TMemoryBlockSize;
    AccessRights: TMemoryAccessRights;
    ThreadId: NativeUInt;
    Size: NativeUInt;
  end;
  PMemoryOptions = ^TMemoryOptions;

  // additional memory functions
  procedure GetMemAligned(var P: Pointer; Align: TMemoryAlign; Size: NativeInt);
  procedure RegetMem(var P: Pointer; NewSize: NativeInt);

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
  function GetMemoryAccessRights(Pages: MemoryPages): TMemoryAccessRights;

  // low level routine
  function GetMemoryOptions(const P: Pointer; var Options: TMemoryOptions): Boolean;
  function ThreadHeapMinimize: Boolean;

  // fast SSE-based 16-aligned memory move
  procedure MoveB16Aligned(const Source; var Dest; const B16Count: NativeInt);
  
type
{ TJITHeap class }
{ Just-In-Time memory heap: READ | WRITE | EXECUTE }

  IJITHeap = interface
    procedure Clear; stdcall;
    function GetMemory(Size: NativeInt): Pointer; stdcall;
    procedure FreeMemory(P: Pointer); stdcall;
    function SyncGetMemory(Size: NativeInt): Pointer; stdcall;
    procedure SyncFreeMemory(P: Pointer); stdcall;
  end;

  TJITHeap = class(TInterfacedObject, IJITHeap)
  public
    constructor Create;
    destructor Destroy; override;    
    procedure Clear; stdcall;

    // memory management
    function GetMemory(Size: NativeInt): Pointer; stdcall;
    procedure FreeMemory(P: Pointer); stdcall;

    // synchronization (spin lock) + memory management
    function SyncGetMemory(Size: NativeInt): Pointer; stdcall;
    procedure SyncFreeMemory(P: Pointer); stdcall;
  end;


{ TMalloc class }

  TMalloc = class(TInterfacedObject, IMalloc)
    function Alloc(cb: Longint): Pointer; stdcall;
    function Realloc(pv: Pointer; cb: Longint): Pointer; stdcall;
    procedure Free(pv: Pointer); stdcall;
    function GetSize(pv: Pointer): Longint; stdcall;
    function DidAlloc(pv: Pointer): Integer; stdcall;
    procedure HeapMinimize; stdcall;
  end;
```

## Development was temporarily suspended
I want to say thank you for your anticipation and patience! However, at the moment there are more priority tasks. The next possible release date is winter (2019). Or if someone succeeds in attracting donations/investments of $5000-$10000 - the development will continue immediately. Thank you again for your attention to the project!

[Donate Link](https://www.paypal.me/BrainMM/100usd)

USD VISA 4779 6426 1574 7797

RUB VISA 4154 8120 7035 2715