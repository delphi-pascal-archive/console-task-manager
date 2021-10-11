unit ConsoleTimers;

interface

uses
  SysUtils,Windows,Classes,ConsoleForms,SyncObjs;

type
  TTimer=class(TComponent)
  private
    FThread:TThread;
    FEnabled:Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;

    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
  protected
    procedure Update;
  public
    constructor Create(AOwner:TComponent);override;

    destructor Destroy;override;

    procedure Timer;

    property Enabled:Boolean read FEnabled write SetEnabled;
    property Interval:Cardinal read FInterval write SetInterval;

    property OnTimer:TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

const
  CM_TIMER=$0001;

type
  TTimerThread=class(TThread)
  private
    FTimer:TTimer;
    FEvent:TEvent;
    FInterval:Cardinal;
    FQueue:Pointer;

    procedure CMTimer(var Message:TPostMessage);message CM_TIMER;
  protected
    procedure Execute;override;
  public
    constructor Create(ATimer:TTimer);

    procedure Queue;
    procedure Stop;

    destructor Destroy;override;
  end;

{ TTimerThread }

procedure TTimerThread.CMTimer(var Message: TPostMessage);
begin
  InterlockedDecrement(Integer(FQueue));
  if Terminated then begin
    WaitFor;
    Destroy;
  end else
    FTimer.Timer;
end;

constructor TTimerThread.Create(ATimer: TTimer);
begin
  inherited Create(True);
  FTimer:=ATimer;
  FInterval:=ATimer.FInterval;
  FEvent:=TEvent.Create(nil,False,False,'');
  Resume;
end;

destructor TTimerThread.Destroy;
begin
  FreeAndnil(FEvent);
  inherited;
end;

procedure TTimerThread.Execute;
begin
  while not Terminated do begin
    FEvent.WaitFor(FInterval);
    Queue;
  end;
end;

procedure TTimerThread.Queue;
begin
  if InterlockedCompareExchange(FQueue,Pointer(1),nil)=nil then
    Application.PostMessage(CM_TIMER,Self,0,0);
end;

procedure TTimerThread.Stop;
begin
  Terminate;
  FTimer:=nil;
  Queue;
  FEvent.SetEvent;
end;

{ TTimer }

constructor TTimer.Create(AOwner: TComponent);
begin
  inherited;
  FInterval:=1000;
end;

destructor TTimer.Destroy;
begin
  SetEnabled(False);
  inherited;
end;

procedure TTimer.SetEnabled(const Value: Boolean);
begin
  if Assigned(FThread) xor Value then begin
    FEnabled := Value;
    Update;
  end;
end;

procedure TTimer.SetInterval(const Value: Cardinal);
begin
  if Value<>FInterval then begin
    FInterval := Value;
    Update;
  end;
end;

procedure TTimer.Timer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TTimer.Update;
begin
  if Assigned(FThread) then
    TTimerThread(FThread).Stop;
  FThread:=nil;
  if FEnabled then
    FThread:=TTimerThread.Create(Self);
end;

end.

