unit uSpeedTest;

{
   Copyright (c) Steve Maughan:
   http://www.stevemaughan.com/delphi/delphi-parallel-programming-library-memory-managers/
}

interface

uses
  Generics.Collections;

type

  TFoo = class
  private
    fValue: Double;
  public
    constructor create;
    property Value: double read fValue;
  end;

  TFooList = class
  private
    fValue: double;
  public
    constructor create;
    destructor Destroy; override;

    function Execute(Size: integer = 3500; Capacity: integer = 7): double;

    property Value: double read fValue;
  end;

  TSpeedTest = class
  private
    fList: TObjectList<TFooList>;
    fIterations: integer;
    fMulticore: boolean;
  public
    constructor create(aIterations: integer);
    destructor Destroy; override;

    function Execute: integer;

    property Iterations: integer read fIterations write fIterations;
    property Multicore: boolean read fMulticore write fMulticore;
  end;

implementation

uses
  System.Diagnostics, System.Threading;

{ TSpeedTest }

constructor TSpeedTest.create(aIterations: integer);
var
  aFooList: TFooList;
  i: Integer;
begin
  inherited create;

  fList := TObjectList<TFooList>.Create;
  fMulticore := false;

  fIterations := aIterations;
  for i := 0 to fIterations - 1 do
  begin
    aFooList := TFooList.create;
    fList.Add(aFooList)
  end;
end;

destructor TSpeedTest.Destroy;
begin
  fList.Free;
  inherited;
end;

function TSpeedTest.Execute: integer;
var
  i: Integer;
  StopWatch: TStopwatch;
begin

  Stopwatch := TStopwatch.Create;
  Stopwatch.Reset;
  Stopwatch.Start;

  if fMultiCore then

    TParallel.&For(0, fList.Count - 1, procedure(i: integer)
    begin
      fList[i].Execute;
    end)

  else
    for i := 0 to fList.Count - 1 do
      fList[i].Execute;

  Stopwatch.Stop;
  result := Stopwatch.ElapsedMilliseconds;

end;

{ TFooList }

constructor TFooList.create;
begin
  inherited create;

end;

destructor TFooList.destroy;
begin

  inherited;
end;

function TFooList.Execute(Size, Capacity: integer): double;
var
  List: TObjectList<TFoo>;
  aFoo: TFoo;
  i: Integer;
begin
  List := TObjectList<TFoo>.Create;
  List.Capacity := Capacity;
  try

    for i := 0 to Size - 1 do
    begin
      aFoo := TFoo.create;
      List.Add(aFoo);
    end;

    result := 0;
    for i := List.Count - 1 downto 0 do
    begin
      fValue := List[i].Value;
      result := List[i].Value;
      List.Delete(i);
    end;

  finally
    List.Free;
  end;
end;

{ TFoo }

constructor TFoo.create;
begin
  inherited create;
  fValue := Random;
end;

end.
