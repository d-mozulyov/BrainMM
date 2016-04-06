# BrainMM
![](http://dmozulyov.ucoz.net/BrainMM/Logo.png)
Несколько лет назад я начал изучать алгоритмы сборки мусора и принципы менеджмента памяти в таких системах. Считается, что редкая дефрагментация и быстрое выделение памяти в конце пула - делают менеджеры памяти с принципом сборки мусора эффективнее традиционных подходов, когда выделение и освобождение памяти требует сложных медленных манипуляций. Однако на самом деле менеджеры памяти с принципом сборки мусора сложно назвать эффективными, потому что:
* Realloc памяти гарантированно приводит к выделению нового куска памяти и копированию данных в него, что не обязательно происходит в менеджерах с традиционным подходом
* Происходит перерасход памяти, т.к. в пуле содержатся так же ранее выделенные, но не используемые (освобождённые) куски памяти
* Дефрагментация приводит к блокировке всех потоков, длительным операциям анализа и копирования данных - что нередко приводит к неприятному эффекту "зависания" приложения
* Требуются дополнительные ресурсы на синхронизацию и обслуживание указателей, т.к. после дефрагментации данные могут находиться по другому физическому адресу
* Принцип быстрого выделения памяти в конце пула за редким исключением применим и в традиционных менеджерах памяти
 
С другой стороны популярный менеджер памяти FastMM со временем перестаёт удовлетворять требованиям современных приложений, т.к. работает с блокировками на многопоточных приложениях, не содержит API для выровненных данных и расходует минимум, в зависимости от платформы, 8/16 байт служебной информации даже на малых кусках памяти.

Поэтому я начал работу над проектом BrainMM - менеджером памяти, спроектированным под современные требования приложений. Искренне верю, что со временем проект вырастет до такого уровня, что войдёт в стандартную поставку Delphi/C++ Builder наряду с другими замечательными библиотеками. Для менеджера памяти BrainMM характерно следующее:
* Экстремально высокая производительность (*реализовано не полностью*)
* Без блокировок для кусков памяти до 32Кб
* Поддержка всех операционных систем, предусмотренных в Delphi/C++ Builder
* Разделяемая с DLL память
* Гарантированно выровненный адрес на 16 байт. Это полезно для lock-free алгоритмов, SSE-команд и в целом при чтении/записи/копировании памяти
* Функция RegetMem. По функциональности похожа на ReallocMem, но не гарантирует сохранности данных, поэтому в некоторых случаях может работать быстрее
* Функция GetMemAligned. Позволяет выделить память с определённым выравниванием. Освобождается такая память стандартно по FreeMem. При изменении размера с помощью ReallocMem/RegetMem выравнивание сохраняется. Исключение составляют ситуации, когда NewSize равен нулю, в этом случае вызывается FreeMem и информация о выравнивании теряется
* API для выделения блоков памяти (*реализовано не польностью*). Под блоками памяти BrainMM подразумеваются куски памяти определённой гранулярности, чей размер изменить нельзя. Блоки памяти полезны для узкоспециализированного требовательного к производительности менеджмента памяти. Служебную информацию можно хранить в начале блока и получать доступ к ней, применяя к указателю операцию логического умножения. Менеджмент малыми (до 128 байт) и средними (до 32Кб) кусками памяти в BrainMM, например, осуществляется с помощью блоков размером 64Кб
* API для работы со страницами памяти (*реализовано не польностью*)
 
Несмотря на то, что библиотека успешно прошла ряд юнит-тестов, на данный момент она находится в состоянии **неофициального релиза** и её **не рекомендуется** использовать в коммерческих приложениях.

##### Что необходимо доработать
* Менеджмент больших кусков памяти (больше 32Кб)
* Средства поиска утечек памяти и отладки
* Эффективное взаимодействие с операционными системами

В качестве примера приведу одну из важных задач, которую предстоит решить. Допустим наш кусок памяти состоит из 2 страниц, слева свободно 4 страницы, справа свободно 3. Если необходимо увеличить размер куска памяти до 5 страниц - то страницы справа помечаются как занятые, и ассоциируются с этим куском. Если необходимо увеличить размер до 6 страниц - то количества свободных страниц справа не хватает, но можно занять 4 страницы слева и скопировать 2 страницы данных, которые имеются на данный момент. Оба подхода успешно реализованы в менеджере памяти FastMM, но нет подхода, позволяющего задействовать как 4 страницы слева, так и 3 страницы справа. Кроме того, насколько я осведомлён, операционные системы содержат API, позволяющие избежать копирования данных путём изменения адреса страниц.
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
  procedure MoveB16Aligned(const Source; var Dest; const B16Count: NativeInt);
  
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
  public
    constructor Create;
    destructor Destroy; override;    
    procedure Clear;

    // memory management
    function GetMemory(Size: NativeInt): Pointer;
    procedure FreeMemory(P: Pointer);

    // synchronization (spin lock) + memory management
    function SyncGetMemory(Size: NativeInt): Pointer;
    procedure SyncFreeMemory(P: Pointer);
  end;
```