unit uSpeedTest;

{
   Copyright (c) Steve Maughan
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
    fList: TObjectList<TFoo>;
    fSize: integer;
    fValue: double;
  public
    constructor create;
    destructor Destroy; override;

    function Execute: double;

    property Size: integer read fSize;
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
  System.Diagnostics;//, System.Threading;

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
  fSize := 12000;
  fList := TObjectList<TFoo>.Create;
  fList.Capacity := 10;
end;

destructor TFooList.destroy;
begin
  fList.Free;
  inherited;
end;

function TFooList.Execute: double;
var
  aFoo: TFoo;
  i: Integer;
begin

  for i := 0 to Size - 1 do
  begin
    aFoo := TFoo.create;
    fList.Add(aFoo);
  end;

  result := 0;
  while fList.Count > 0 do
  begin
    fValue := fList[0].Value;
    result := fList[0].Value;
    fList.Delete(0);
  end;

end;

{ TFoo }

constructor TFoo.create;
begin
  inherited create;
  fValue := Random;
end;

end.
