unit ConsoleClasses;

interface

uses
  SysUtils,Windows;

type
  TShiftState=set of (ssShift,ssAlt,ssCtrl,ssLeft,ssRight,ssMiddle);

  TPointers=array[0..$FFFFF] of Pointer;
  PPointers=^TPointers;

  TList=class
  private
    FData:PPointers;
    FCapacity,FCount:Integer;

    function GetItem(Index:Integer):Pointer;
    procedure SetItem(Index:Integer;const Value:Pointer);
  protected
    procedure SetCapacity(const ACapacity:Integer);
    procedure SetCount(const Value:Integer);

    procedure RaiseRangeError(x:Integer);

    procedure CheckRange(x:Integer);overload;
    procedure CheckRange(x,Max:Integer);overload;
  public
    property Item[Index:Integer]:Pointer read GetItem write SetItem;default;

    property Capacity:Integer read FCapacity;
    property Count:Integer read FCount;

    function Add(APointer:Pointer):Integer;
    procedure Insert(AIndex:Integer;APointer:Pointer);
    procedure Exchange(Index1,Index2:Integer);
    function IndexOf(APointer:Pointer):Integer;
    procedure Remove(APointer:Pointer);
    procedure Delete(AIndex:Integer);
    procedure Clear;

    destructor Destroy;override;
  end;

  TOperation=(opInsert,opRemove);

  TComponent=class
  private
    FComponents,FNotifiers:TList;
    FOwner:TComponent;
  protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);virtual;
  public
    constructor Create(AOwner:TComponent);virtual;

    property Owner:TComponent read FOwner;

    procedure FreeNotification(AComponent:TComponent);
    procedure RemoveFreeNotification(AComponent:TComponent);

    destructor Destroy;override;
  end;

  TThreadPriority=(tpIdle,tpLowest,tpLower,tpNormal,tpHigher,tpHighest,tpTimeCritical);

  TThread=class
  private
    FHandle:THandle;
    FThreadID:Cardinal;
    FPriority:TThreadPriority;
    FReturnValue:Cardinal;
    FTerminated:Boolean;

    procedure SetPriority(const Value:TThreadPriority);
  protected
    procedure Execute;virtual;abstract;

    property Terminated:Boolean read FTerminated;
    property ReturnValue:Cardinal write FReturnValue;
  public
    constructor Create(CreateSuspended:Boolean;Priority:TThreadPriority=tpNormal);

    property Handle:THandle read FHandle;
    property ThreadID:Cardinal read FThreadID;
    property Priority:TThreadPriority read FPriority write SetPriority;

    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor:Cardinal;

    destructor Destroy;override;
  end;

const
  SListIndexOutOfRange='List index out of range (%d)';

implementation

{ TList }

function TList.Add(APointer: Pointer): Integer;
begin
  Result:=FCount;
  SetCount(FCount+1);
  FData[FCount-1]:=APointer;
end;

procedure TList.CheckRange(x: Integer);
begin
  CheckRange(x,Count-1);
end;

procedure TList.CheckRange(x, Max: Integer);
begin
  if (x<0) or (x>Max) then
    RaiseRangeError(x);
end;

procedure TList.Clear;
begin
  SetCount(0);
end;

procedure TList.Delete(AIndex: Integer);
var
  a:Integer;
begin
  CheckRange(AIndex);
  for a:=AIndex to FCount-2 do
    FData[a]:=FData[a+1];
  SetCount(FCount-1);
end;

destructor TList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TList.Exchange(Index1, Index2: Integer);
var
  p:Pointer;
begin
  CheckRange(Index1);
  CheckRange(Index2);
  p:=FData[Index1];
  FData[Index1]:=FData[Index2];
  FData[Index2]:=p;
end;

function TList.GetItem(Index: Integer): Pointer;
begin
  CheckRange(Index);
  Result:=FData[Index];
end;

function TList.IndexOf(APointer: Pointer): Integer;
var
  a:Integer;
begin
  for a:=0 to FCount-1 do
    if FData[a]=APointer then begin
      Result:=a;
      Exit;
    end;
  Result:=-1;
end;

procedure TList.Insert(AIndex: Integer; APointer: Pointer);
var
  a:Integer;
begin
  CheckRange(AIndex,Count);
  SetCount(Count+1);
  for a:=FCount-1 downto AIndex+1 do
    FData[a]:=FData[a-1];
  FData[AIndex]:=APointer;
end;

procedure TList.RaiseRangeError(x: Integer);
begin
  raise Exception.Create(Format(SListIndexOutOfRange,[x]));
end;

procedure TList.Remove(APointer: Pointer);
var
  a:Integer;
begin
  for a:=FCount-1 downto 0 do
    if FData[a]=APointer then
      Delete(a);
end;

procedure TList.SetCapacity(const ACapacity: Integer);
begin
  ReallocMem(FData,ACapacity*SizeOf(Pointer));
  FCapacity:=ACapacity;
end;

procedure TList.SetCount(const Value: Integer);
begin
  if Value>FCapacity then
    SetCapacity((Value*3) div 2)
  else
    if 2*Value<FCapacity then
      SetCapacity(Value);
  FCount:=Value;
end;

procedure TList.SetItem(Index: Integer; const Value: Pointer);
begin
  CheckRange(Index);
  FData[Index]:=Value;
end;

{ TComponent }

constructor TComponent.Create(AOwner: TComponent);
begin
  FComponents:=TList.Create;
  FNotifiers:=TList.Create;
  FOwner:=AOwner;
  if Assigned(FOwner) then
    FOwner.FComponents.Add(Self);
end;

destructor TComponent.Destroy;
var
  a:Integer;
begin
  for a:=FNotifiers.Count-1 downto 0 do
    TComponent(FNotifiers[a]).Notification(Self,opRemove);
  for a:=FComponents.Count-1 downto 0 do
    TComponent(FComponents[a]).Destroy;
  if Assigned(FOwner) then
    FOwner.FComponents.Remove(Self);
  FreeAndNil(FNotifiers);
  FreeAndNil(FComponents);
  inherited;
end;

procedure TComponent.FreeNotification(AComponent: TComponent);
begin
  if FNotifiers.IndexOf(AComponent)=-1 then
    FNotifiers.Insert(0,AComponent);
end;

procedure TComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin

end;

procedure TComponent.RemoveFreeNotification(AComponent: TComponent);
begin
  FNotifiers.Remove(AComponent);
end;

{ TThread }

function StartThread(Thread:TThread):Cardinal;stdcall;
begin
  Thread.Execute;
  Result:=Thread.FReturnValue;
end;

constructor TThread.Create(CreateSuspended: Boolean;
  Priority: TThreadPriority);
const
  T:array[False..True] of Cardinal=(0,CREATE_SUSPENDED);
begin
  FHandle:=CreateThread(nil,0,@StartThread,Self,T[CreateSuspended],FThreadID);
  if Priority<>tpNormal then
    Self.Priority:=Priority;
end;

destructor TThread.Destroy;
begin
  try
    if not CloseHandle(FHandle) then
      RaiseLastOSError;
  finally
    inherited;
  end;
end;

procedure TThread.Resume;
begin
  if ResumeThread(FHandle)=Cardinal(-1) then
    RaiseLastOSError;
end;

procedure TThread.SetPriority(const Value: TThreadPriority);
const
  T:array[tpIdle..tpTimeCritical] of Integer=(
    THREAD_PRIORITY_IDLE,
    THREAD_PRIORITY_LOWEST,
    THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL,
    THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST,
    THREAD_PRIORITY_TIME_CRITICAL
  );
begin
  if FPriority<>Value then begin
    if not SetThreadPriority(FHandle,T[Value]) then
      RaiseLastOSError;
    FPriority:=Value;
  end;
end;

procedure TThread.Suspend;
begin
  if SuspendThread(FHandle)=Cardinal(-1) then
    RaiseLastOSError;
end;

procedure TThread.Terminate;
begin
  FTerminated:=True;
end;

function TThread.WaitFor: Cardinal;
begin
  WaitForSingleObject(FHandle,INFINITE);
  Result:=FReturnValue;
end;

end.
