unit ConsoleForms;

interface

uses
  SysUtils,Windows,SyncObjs,Classes,Contnrs,
  ConsoleUtils,ConsoleMessages,ConsoleGraphics,ConsoleControls,ConsoleMenus;

const
  GMaxMessageQueueLength=$100;
  GMaxStateStackLength=$100;

type
  TApplication=class;
  TForm=class;

  TWindowState=(wsNormal,wsMaximized);
  TBorderIcon=(biSystemMenu,biHelp,biZoom,biClose);
  TBorderIcons=set of TBorderIcon;
  TBorderStyle=(bsNone,bsSingle,bsSizeable,bsDialog,bsToolWindow,bsSizeToolWin);
  TModalResult=(mrNone,mrOk,mrYes,mrNo,mrYesToAll,mrNoToAll,mrRetry,mrAbort,mrCancel,mrIgnore,mrAll);

  TForm=class(TControl)
  private
    FWindowState:TWindowState;
    FBorderIcons:TBorderIcons;
    FBorderStyle:TBorderStyle;
    FDefaultArea:TArea;

    FModalResult:TModalResult;

    FDefault:TControl;
    FFocus:TControl;

    procedure SetWindowState(const Value: TWindowState);
    procedure SetModalResult(const Value: TModalResult);

    procedure CMChar(var Message:TCMChar);message CM_CHAR;
    procedure CMKeyDown(var Message:TCMKey);message CM_KEY_DOWN;
    procedure CMKeyUp(var Message:TCMKey);message CM_KEY_UP;

    procedure CMMouseMove(var Message:TCMMouseMove);message CM_MOUSE_MOVE;
    procedure CMMouseUp(var Message:TCMMouseButton);message CM_MOUSE_UP;
    procedure CMMouseClick(var Message:TCMMouseButton);message CM_MOUSE_CLICK;
    procedure CMMouseDoubleClick(var Message:TCMMouseButton);message CM_MOUSE_DOUBLE_CLICK;

    procedure CMPaint(var Message:TCMPaint);message CM_PAINT;
    procedure SetBorderIcons(const Value: TBorderIcons);
    procedure SetBorderStyle(const Value: TBorderStyle);

    procedure SetDefault(const Value: TControl);
    procedure SetFocus(const Value: TControl);

    procedure FocusFirst;
    procedure FocusNext;
    procedure FocusPrevious;
  protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;

    procedure ShowSystemMenu(p:TCoord);

    procedure PaintBorder(Canvas:TCanvas);override;

    procedure UpdateBounds(var Bounds:TArea);override;
    procedure UpdateClientArea(var ClientArea:TArea);override;

    function HitArea(Zone:THitZone):TArea;override;
    function HasBorder(Edge:TEdgeBorder):Boolean;override;
    function HasBorderIcon(Icon:TBorderIcon):Boolean;virtual;
    function HasTitle:Boolean;virtual;
  public
    constructor Create(AOwner:TComponent);override;

    function IsVisible:Boolean;override;
    function IsEnabled:Boolean;override;
    function IsActive:Boolean;override;

    procedure Center;  
    procedure Close;

    function ShowModal:TModalResult;
    property ModalResult:TModalResult read FModalResult write SetModalResult;

    property WindowState:TWindowState read FWindowState write SetWindowState;
    property BorderIcons:TBorderIcons read FBorderIcons write SetBorderIcons;
    property BorderStyle:TBorderStyle read FBorderStyle write SetBorderStyle;

    property Default:TControl read FDefault write SetDefault;
    property Focus:TControl read FFocus write SetFocus;
  end;

  TFormClass=class of TForm;

  TModalState=record
    Form:TForm;
    Terminated:Boolean;
  end;

  TModalStateStack=class
  private
    FLength:Cardinal;
    FStates:array[1..GMaxStateStackLength] of TModalState;

    function GetForm: TForm;
    function GetTerminated: Boolean;
    function GetEmpty: Boolean;
  public
    procedure Push(AForm:TForm);
    procedure Pop;

    procedure Remove(AControl:TControl);

    property Form:TForm read GetForm;
    property Terminated:Boolean read GetTerminated;
    property Empty:Boolean read GetEmpty;
  end;

  TControlStateStack=class
  private
    FControl:TControl;
    FLength:Cardinal;
    FConsistency:array[1..GMaxStateStackLength] of Boolean;

    procedure SetControl(const Value: TControl);
    function GetConsistent: Boolean;
    procedure Reset;
  public
    procedure Remove(AControl:TControl);

    procedure Push;
    procedure Pop;

    property Control:TControl read FControl write SetControl;
    property Consistent:Boolean read GetConsistent;
  end;

  TInputRecords=array[1..GMaxMessageQueueLength] of TInputRecord;
  PInputRecords=^TInputRecords;

  TPostMessage=record
    Message:Cardinal;
    Sender:TObject;
    wParam,lParam:Integer;
  end;
  TPostMessages=array[1..GMaxMessageQueueLength] of TPostMessage;

  TMessageQueue=class(TThread)
  private
    FSection:TCriticalSection;
    FResumeEvent,FTerminateEvent:TEvent;
    FInputBuffers:array[False..True] of TInputRecords;
    FInputQueue:PInputRecords;
    FPostQueue:TPostMessages;
    FInvalidateFlag:Boolean;
    FInputQueueLength,FInputQueueIndex,FPostQueueLength,FPostQueueIndex:Cardinal;
    FEmptyInputQueue:Boolean;
  protected
    procedure Execute;override;
  public
    constructor Create;

    procedure Terminate;

    destructor Destroy;override;
  end;

  TApplication=class(TComponent)
  private
    FStdIn,FStdOut:THandle;
    FQueueEvent:TEvent;
    FMessageQueue:TMessageQueue;
    FBufferSizeUpdateCount:Integer;
    FCanvas:TCanvas;

    FMainForm:TForm;

    FForms:TList;
    FModalStack:TModalStateStack;
    FActiveStack,FCaptureStack,FFocusStack:TControlStateStack;

    FMouseState:TShiftState;
    FLastMouseButton:TMouseButton;

    function GetScreenSize: TCoord;
  protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;

    function TranslateShiftState(const State:Cardinal):TShiftState;
    function TranslateMouseState(const State:Cardinal):TShiftState;
    function CalcMouseDiff(State1,State2:TShiftState;var Button:TMouseButton;var Pressed:Boolean):Boolean;

    procedure RunModal(AForm:TForm);
    procedure FlushMessageQueue;
    procedure ProcessKeyEvent(KeyEvent:TKeyEventRecord);
    procedure ProcessMouseEvent(MouseEvent:TMouseEventRecord);
    procedure ProcessWindowEvent;
    procedure ProcessPostMessage(PostMessage:TPostMessage);
    procedure ProcessInvalidateFlag;

    procedure PushBufferSize;
    procedure PopBufferSize;
    procedure AdjustScreenBuffer(KeepWindowSize:Boolean=True);

    procedure SetActiveCapture(AControl:TControl);
    procedure SetActiveForm(AForm:TForm);
    procedure SetCapture(AControl:TControl);
    procedure SetFocus(AControl:TControl);
  public
    constructor Create(AOwner:TComponent);override;

    procedure Run;
    procedure Terminate;
    procedure PostMessage(Message:Cardinal;Sender:TObject;wParam,lParam:Integer);

    procedure Invalidate;
    procedure Update;

    function ControlFromPoint(p:TCoord;VisibleOnly:Boolean=True):TControl;
    function FormFromPoint(p:TCoord;VisibleOnly:Boolean=True):TForm;

    procedure CreateForm(FormClass:TFormClass;var Form);

    destructor Destroy;override;

    property ScreenSize:TCoord read GetScreenSize;
  end;

var
  Application:TApplication=nil;

const
{$ifndef MOUSE_WHEELED}
  MOUSE_WHEELED=$00000004;
{$endif}

implementation

uses
  ConsoleDialogs;

{ TForm }

procedure TForm.Center;
begin
  with Application.ScreenSize do begin
    Left:=(X-Width) div 2;
    Top:=(Y-Height) div 2;
  end;
end;

procedure TForm.Close;
begin
  if csModal in ControlState then
    SetModalResult(mrNone)
  else
    Destroy;
end;

procedure TForm.CMChar(var Message: TCMChar);
begin
  if Assigned(FFocus) then
    FFocus.Dispatch(Message)
  else
    inherited;
end;

procedure TForm.CMKeyDown(var Message: TCMKey);

  procedure Translate(AControl:TControl);
  var
    a:Integer;
  begin
    Message.Canceled:=AControl.Action(Message.ShiftState,Message.Key);
    a:=0;
    while not Message.Canceled and (a<AControl.ControlCount) do begin
      with AControl.Control[a] do
        if Visible and Enabled then
          Translate(AControl.Control[a]);
      Inc(a);
    end;
  end;

  procedure Next(Tab:Boolean);
  begin
    if not Assigned(FFocus) or (Tab and not (csTabStop in FFocus.ControlStyle)) or (not Tab and not (csArrowStop in FFocus.ControlStyle)) then begin
      FocusNext;
      Message.Canceled:=True;
    end;
  end;

  procedure Previous(Tab:Boolean);
  begin
    if not Assigned(FFocus) or (Tab and not (csTabStop in FFocus.ControlStyle)) or (not Tab and not (csArrowStop in FFocus.ControlStyle)) then begin
      FocusPrevious;
      Message.Canceled:=True;
    end;
  end;

begin
  with Message do
    case Key of
      VK_RIGHT,VK_DOWN:Next(False);
      VK_LEFT,VK_UP:Previous(False);
      VK_TAB:begin
        if ShiftState=[] then
          Next(True);
        if ShiftState=[ssShift] then
          Previous(True);
      end;
    end;
  if not Message.Canceled and Assigned(FFocus) then
    FFocus.Dispatch(Message);
  if not Message.Canceled then
    Translate(Self);
  if not Message.Canceled then
    inherited;
end;

procedure TForm.CMKeyUp(var Message: TCMKey);
begin
  if Assigned(FFocus) then
    FFocus.Dispatch(Message);
  if not Message.Canceled then
    inherited;
end;

procedure TForm.CMMouseClick(var Message: TCMMouseButton);
begin
  inherited;
end;

procedure TForm.CMMouseDoubleClick(var Message: TCMMouseButton);
begin
  with Message do
    if Button=mbLeft then begin
      case HitTest(Position) of
        hzTitle:if HasBorderIcon(biZoom) then begin
          if FWindowState=wsMaximized then
            SetWindowState(wsNormal)
          else
            SetWindowState(wsMaximized);
        end;
        hzSystemMenu:Close;
        else
          inherited;
      end
    end else
      inherited;
end;

procedure TForm.CMMouseMove(var Message: TCMMouseMove);
begin
  with Message do
    if (ShiftState=[ssLeft]) and (LastHitZone[mbLeft]=hzTitle) then begin
      Left:=Left+Position.X-LastHitPosition[mbLeft].X;
      Top:=Top+Position.Y-LastHitPosition[mbLeft].Y;
    end else
      inherited;
end;

procedure TForm.CMMouseUp(var Message: TCMMouseButton);
var
  z:THitZone;
begin
  with Message do begin
    z:=HitTest(Position);
    if z=LastHitZone[Button] then begin
      case z of
        hzTitle:if Button=mbRight then
          ShowSystemMenu(Position);//TODO: System menu
        hzZoom:if HasBordericon(biZoom) and (Button=mbLeft) then begin
          if FWindowState=wsMaximized then
            SetWindowState(wsNormal)
          else
            SetWindowState(wsMaximized);
        end;
        hzClose:if Button=mbLeft then
          Close;
      else
        inherited;
      end;
    end else
      inherited;
  end;
end;

procedure TForm.CMPaint(var Message: TCMPaint);
begin
  with Message do begin
    Canvas.PushClipArea(Bounds,Bounds.Origin);
    try
      Paint(Canvas);
    finally
      Canvas.PopClipArea;
    end;
  end;
end;

constructor TForm.Create(AOwner: TComponent);
begin
  inherited;
  FBorderIcons:=[biSystemMenu,biZoom,biClose];
  FBorderStyle:=bsSizeable;
  Visible:=False;
end;

procedure TForm.FocusFirst;

  procedure Rec(Control:TControl);
  var
    a:Integer;
  begin
    if Control.IsFocusable then
      SetFocus(Control);
    a:=0;
    while (a<Control.ControlCount) and not Assigned(FFocus) do begin
      Rec(Control.Control[a]);
      Inc(a);
    end;
  end;

begin
  SetFocus(FDefault);
  if not Assigned(FFocus) then
    Rec(Self);
end;

procedure TForm.FocusNext;
var
  c:TControl;
  t:Boolean;

  procedure Rec(Control:TControl);
  var
    a:Integer;
  begin
    if t and Control.IsFocusable then
      c:=Control;
    if Control=FFocus then
      t:=not t;
    a:=0;
    while (a<Control.ControlCount) and not Assigned(c) do begin
      Rec(Control.Control[a]);
      Inc(a);
    end;
  end;
  
begin
  if not Assigned(FFocus) then
    FocusFirst
  else begin
    c:=nil;
    t:=False;
    Rec(Self);
    t:=not Assigned(c);
    Rec(Self);
    SetFocus(c);
  end;
end;

procedure TForm.FocusPrevious;
var
  c:TControl;
  t:Boolean;

  procedure Rec(Control:TControl);
  var
    a:Integer;
  begin
    if t and Control.IsFocusable then
      c:=Control;
    if Control=FFocus then
      t:=not t;
    a:=Control.ControlCount-1;
    while (a>=0) and not Assigned(c) do begin
      Rec(Control.Control[a]);
      Dec(a);
    end;
  end;
  
begin
  if not Assigned(FFocus) then
    FocusFirst
  else begin
    c:=nil;
    t:=False;
    Rec(Self);
    t:=not Assigned(c);
    Rec(Self);
    SetFocus(c);
  end;
end;

function TForm.HasBorder(Edge: TEdgeBorder): Boolean;
begin
  Result:=(FWindowState=wsNormal) and (FBorderStyle<>bsNone) and inherited HasBorder(Edge);
end;

function TForm.HasBorderIcon(Icon: TBorderIcon): Boolean;
begin
  Result:=HasTitle and (Icon in FBorderIcons) and not ((Icon=biZoom) and (FBorderStyle<>bsSizeable));
end;

function TForm.HasTitle: Boolean;
begin
  Result:=FBorderStyle<>bsNone;
end;

function TForm.HitArea(Zone: THitZone): TArea;
var
  i:TBorderIcon;
  t:Boolean;
begin
  case Zone of
    hzTitle:if HasTitle then begin
      with Bounds do
        Result:=Area(0,0,Width,1);
      if HasBorderIcon(biSystemMenu) then begin
        Inc(Result.X,2);
        Dec(Result.Width,2);
      end;
      t:=True;
      for i:=biHelp to biClose do
        if HasBorderIcon(i) then begin
          Dec(Result.Width);
          if t then begin
            Dec(Result.Width);
            t:=False;
          end;
        end;
      if HasBorder(ebTop) then
        Inc(Result.Y);
      if HasBorder(ebLeft) then begin
        Inc(Result.X);
        Dec(Result.Width);
      end;
      if HasBorder(ebRight) then
        Dec(Result.Width);
    end;
    hzSystemMenu:if HasBorderIcon(biSystemMenu) then begin
      Result:=Area(0,0,1,1);
      if HasBorder(ebTop) then
        Inc(Result.Y);
      if HasBorder(ebLeft) then 
        Inc(Result.X);
    end;
    hzHelp:if HasBorderIcon(biZoom) then begin
      with Bounds do
        Result:=Area(Width-1,0,1,1);
      if HasBorderIcon(biClose) then
        Dec(Result.X);
      if HasBorderIcon(biZoom) then
        Dec(Result.X);
      if HasBorder(ebTop) then
        Inc(Result.Y);
      if HasBorder(ebRight) then
        Dec(Result.X);
    end;
    hzZoom:if HasBorderIcon(biZoom) then begin
      with Bounds do
        Result:=Area(Width-1,0,1,1);
      if HasBorderIcon(biClose) then
        Dec(Result.X);
      if HasBorder(ebTop) then
        Inc(Result.Y);
      if HasBorder(ebRight) then
        Dec(Result.X);
    end;
    hzClose:if HasBorderIcon(biClose) then begin
      with Bounds do
        Result:=Area(Width-1,0,1,1);
      if HasBorder(ebTop) then
        Inc(Result.Y);
      if HasBorder(ebRight) then
        Dec(Result.X);
    end;
  else
    Result:=inherited HitArea(Zone);
  end;
end;

function TForm.IsActive: Boolean;
begin
  Result:=Application.FActiveStack.FControl=Self;
end;

function TForm.IsEnabled: Boolean;
begin
  Result:=Enabled;
end;

function TForm.IsVisible: Boolean;
begin
  Result:=Visible or (Application.FMainForm=Self);
end;

procedure TForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then begin
    if FFocus=AComponent then
      FFocus:=nil;
    if FDefault=AComponent then
      FDefault:=nil;
  end;
end;

procedure TForm.PaintBorder(Canvas: TCanvas);
begin
  if FWindowState=wsNormal then 
    inherited;
  if HasTitle then
    case inherited BorderStyle of
      ConsoleControls.bsSingle:Canvas.Pen.Style:=psSingle;
      ConsoleControls.bsDouble:Canvas.Pen.Style:=psDouble;
      ConsoleControls.bsNone:Canvas.Pen.Style:=psClear;
    end;
    with HitArea(hzTitle) do begin
      Canvas.Brush.Style:=bsClear;
      Canvas.Pen.Color:=GTheme[BorderColor];
      Canvas.HorizontalLine(0,Bounds.Width-1,Y+1);
      Canvas.VerticalLine(Y-1,Y+1,X-1);
      Canvas.VerticalLine(Y-1,Y+1,X+Width);
      if HasBorderIcon(biSystemMenu) then
        with HitArea(hzSystemMenu) do
          Canvas.TextOut(X,Y,#4);
      if IsActive then
        Canvas.Pen.Color:=GTheme[tcActiveText]
      else
        Canvas.Pen.Color:=GTheme[tcInactiveText];
      if HasBorderIcon(biHelp) then
        with HitArea(hzHelp) do
          Canvas.TextOut(X,Y,'?');
      if HasBorderIcon(biZoom) then
        with HitArea(hzZoom) do
          Canvas.TextOut(X,Y,'þ');
      if HasBorderIcon(biClose) then
        with HitArea(hzClose) do
          Canvas.TextOut(X,Y,'ž');
      Canvas.DrawCaption(X,Y,Width,Caption,taCenter);
    end;
end;

procedure TForm.SetBorderIcons(const Value: TBorderIcons);
begin
  if FBorderIcons<>Value then begin
    FBorderIcons:=Value;
    Update;
  end;
end;

procedure TForm.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle<>Value then begin
    FBorderStyle:=Value;
    Update;
  end;
end;

procedure TForm.SetDefault(const Value: TControl);
begin
  if Assigned(FDefault) then
    FDefault.Invalidate;
  FDefault:=Value;
  if Assigned(FDefault) then begin
    FDefault.Invalidate;
    FDefault.FreeNotification(Self);
    if Assigned(FDefault) and ((Application.FForms.IndexOf(Self)=-1) or FDefault.IsFocusable) then
      SetFocus(FDefault);
  end;
end;

procedure TForm.SetFocus(const Value: TControl);
begin
  if Application.FActiveStack.Control=Self then
    Application.SetFocus(Value)
  else begin
    FFocus:=Value;
    if Assigned(FFocus) then
      FFocus.FreeNotification(Self);
  end;
end;

procedure TForm.SetModalResult(const Value: TModalResult);
var
  a:Cardinal;
begin
  if csModal in ControlState then begin
    FModalResult:=Value;
    for a:=Application.FModalStack.FLength downto 1 do
      if Self=Application.FModalStack.FStates[a].Form then
        Application.FModalStack.FStates[a].Terminated:=True;
  end;
end;

procedure TForm.SetWindowState(const Value: TWindowState);
begin
  if FWindowState<>Value then begin
    FWindowState:=Value;
    case FWindowState of
      wsNormal:Bounds:=FDefaultArea;
      wsMaximized:begin
        FDefaultArea:=Bounds;
        Update;
      end;
    end;
  end;
end;

function TForm.ShowModal: TModalResult;
begin
  Assert(not Visible,'Cannot make a visible window modal');
  FModalResult:=mrNone;
  Visible:=True;
  SetDefault(FDefault);
  ControlState:=ControlState+[csModal];
  try
    Application.RunModal(Self);
    Result:=FModalResult;
    FModalResult:=mrNone;
  finally
    ControlState:=ControlState-[csModal];
    Visible:=False;
  end;
end;

procedure TForm.ShowSystemMenu(p: TCoord);
begin
  ConsoleMenus.ShowSystemMenu;
end;

procedure TForm.UpdateBounds(var Bounds: TArea);
begin
  if FWindowState=wsMaximized then
    with Application.ScreenSize do
      Bounds:=Area(0,0,X,Y);
  with Application.ScreenSize do begin
    if Bounds.Width>Width then
      Bounds.Width:=Width;
    if Bounds.Height>Height then
      Bounds.Height:=Height;
  end;
end;

procedure TForm.UpdateClientArea(var ClientArea: TArea);
begin
  if FBorderStyle<>bsNone then begin
    Inc(ClientArea.Y,2);
    Dec(ClientArea.Height,2);
  end;
  inherited;
end;

{ TMessageQueue }

constructor TMessageQueue.Create;
begin
  FSection:=TCriticalSection.Create;
  FResumeEvent:=TEvent.Create(nil,False,False,'');
  FTerminateEvent:=TEvent.Create(nil,False,False,'');
  FInputQueue:=@FInputBuffers[True];
  inherited Create(False);
end;

destructor TMessageQueue.Destroy;
begin
  inherited;
  FreeAndNil(FTerminateEvent);
  FreeAndNil(FResumeEvent);
  FreeAndNil(FSection);
end;

procedure TMessageQueue.Execute;
var
  l:Cardinal;
  t:Boolean;
  h:array[0..1] of THandle;
begin
  h[0]:=Application.FStdIn;
  h[1]:=FTerminateEvent.Handle;
  t:=False;
  while not Terminated do begin
    if (WaitForMultipleObjects(2,@h,False,INFINITE)=WAIT_OBJECT_0) and not Terminated and ReadConsoleInput(Application.FStdIn,TInputRecord(FInputBuffers[t][1]),GMaxMessageQueueLength,l) and (l>0) then begin
      FSection.Enter;
      try
        FInputQueue:=@FInputBuffers[t];
        FInputQueueLength:=l;
        t:=not t;
      finally
        FSection.Leave;
      end;
      Application.FQueueEvent.SetEvent;
      FResumeEvent.WaitFor(INFINITE);
    end;
  end;
end;

procedure TMessageQueue.Terminate;
begin
  inherited Terminate;
  FTerminateEvent.SetEvent;
end;

{ TApplication }

procedure TApplication.AdjustScreenBuffer(KeepWindowSize: Boolean);
var
  SBI:TConsoleScreenBufferInfo;
  s:TCoord;
  r:TSMallRect;
begin
  PushBufferSize;
  try
//    s:=GetLargestConsoleWindowSize(FStdOut);
//    if Cardinal(s)=0 then
//      RaiseLastOSError;
//    if not SetConsoleScreenBufferSize(FStdOut,s) then
//      RaiseLastOSError;
    if not GetConsoleScreenBufferInfo(FStdOut,SBI) then
      RaiseLastOSError;
    if KeepWindowSize then begin
      s.X:=SBI.srWindow.Right-SBI.srWindow.Left+1;
      s.Y:=SBI.srWindow.Bottom-SBI.srWindow.Top+1;
    end else
      s:=SBI.dwSize;
    if s.X>SBI.dwMaximumWindowSize.X then
      s.X:=SBI.dwMaximumWindowSize.X;
    if s.Y>SBI.dwMaximumWindowSize.Y then
      s.Y:=SBI.dwMaximumWindowSize.Y;
    if not SetConsoleScreenBufferSize(FStdOut,s) then
      RaiseLastOSError;
    r.Left:=0;
    r.Top:=0;
    r.Right:=s.X-1;
    r.Bottom:=s.Y-1;
    if not SetConsoleWindowInfo(FStdOut,True,r) then
      RaiseLastOSError;
  finally
    PopBufferSize;
  end;
end;

function TApplication.CalcMouseDiff(State1, State2: TShiftState;
  var Button: TMouseButton; var Pressed: Boolean): Boolean;
const
  T:array[0..2] of TShiftState=([ssLeft],[ssMiddle],[ssRight]);
  U:array[0..2] of TMouseButton=(mbLeft,mbMiddle,mbRight);
var
  a:Integer;
begin
  Result:=False;
  for a:=Low(T) to High(T) do
    if T[a]*State1<>T[a]*State2 then begin
      Pressed:=T[a]*State1=[];
      Button:=U[a];
      Result:=True;
      Exit;
    end;
end;

function TApplication.ControlFromPoint(p: TCoord; VisibleOnly: Boolean): TControl;
var
  f:TForm;
begin
  Result:=nil;
  f:=FormFromPoint(p,VisibleOnly);
  if Assigned(f) then begin
    p.X:=p.X-f.Left;
    p.Y:=p.Y-f.Top;
    Result:=f.ControlFromPoint(p,VisibleOnly);
    if not Assigned(Result) then
      Result:=f;
  end;
end;

constructor TApplication.Create(AOwner: TComponent);
begin
  inherited;
  //AllocConsole;
  if not SetConsoleFullScreenMode(True) then
    RaiseLastOSError;
  FStdIn:=GetStdHandle(STD_INPUT_HANDLE);
  if FStdIn=0 then
    RaiseLastOSError;
  FStdOut:=GetStdHandle(STD_OUTPUT_HANDLE);
  if FStdOut=0 then
    RaiseLastOSError;
  if not SetConsoleMode(FStdIn,ENABLE_WINDOW_INPUT or ENABLE_MOUSE_INPUT) then
    RaiseLastOSError;
  AdjustScreenBuffer(True);
  FQueueEvent:=TEvent.Create(nil,False,False,'');
  FMessageQueue:=TMessageQueue.Create;
  FCanvas:=TCanvas.Create;
  FForms:=TList.Create;
  FModalStack:=TModalStateStack.Create;
  FActiveStack:=TControlStateStack.Create;
  FCaptureStack:=TControlStateStack.Create;
  FFocusStack:=TControlStateStack.Create;
end;

procedure TApplication.CreateForm(FormClass: TFormClass; var Form);
begin
  TForm(Form):=FormClass.Create(Self);
  TForm(Form).FreeNotification(Self);
  FForms.Add(TForm(Form));
  if not Assigned(FMainForm) then
    FMainForm:=TForm(Form);
  if not Assigned(FActiveStack.Control) then
    SetActiveForm(TForm(Form));
  Update;
end;

destructor TApplication.Destroy;
begin
  FMessageQueue.Terminate;
  WaitForSingleObject(FMessageQueue.Handle,INFINITE);
  inherited;
  FreeAndNil(FFocusStack);
  FreeAndNil(FCaptureStack);
  FreeAndNil(FActiveStack);
  FreeAndNil(FModalStack);
  FreeAndNil(FForms);
  FreeAndNil(FCanvas);
  FreeAndNil(FMessageQueue);
  FreeAndNil(FQueueEvent);
end;

procedure TApplication.FlushMessageQueue;
begin
  while (FMessageQueue.FInputQueueIndex<FMessageQueue.FInputQueueLength) and not FMessageQueue.Terminated and not FModalStack.Terminated do begin
    Inc(FMessageQueue.FInputQueueIndex);
    FMessageQueue.FEmptyInputQueue:=FMessageQueue.FInputQueueIndex=FMessageQueue.FInputQueueLength;
    with FMessageQueue.FInputQueue[FMessageQueue.FInputQueueIndex] do
      case EventType of
        KEY_EVENT:ProcessKeyEvent(Event.KeyEvent);
        _MOUSE_EVENT:ProcessMouseEvent(Event.MouseEvent);
      else
        ProcessWindowEvent;
      end;
  end;
  while (FMessageQueue.FPostQueueIndex<FMessageQueue.FPostQueueLength) do begin
    Inc(FMessageQueue.FPostQueueIndex);
    ProcessPostMessage(FMessageQueue.FPostQueue[FMessageQueue.FPostQueueIndex]);
  end;
  FMessageQueue.FPostQueueIndex:=0;
  FMessageQueue.FPostQueueLength:=0;
  if FMessageQueue.FInvalidateFlag and not FMessageQueue.Terminated and not FModalStack.Terminated then begin
    FMessageQueue.FInvalidateFlag:=False;
    ProcessInvalidateFlag;
  end;
  if FMessageQueue.FEmptyInputQueue then begin
    FMessageQueue.FResumeEvent.SetEvent;
    FMessageQueue.FInputQueueIndex:=0;
    FMessageQueue.FInputQueueLength:=0;
  end;            
end;

function TApplication.FormFromPoint(p: TCoord; VisibleOnly: Boolean): TForm;
var
  a:Integer;
begin
  Result:=nil;
  for a:=FForms.Count-1 downto 0 do
    with TForm(FForms[a]) do
      if CoordInArea(p,Bounds) and (not VisibleOnly or Visible or (FForms[a]=FMainForm)) then begin
        Result:=TForm(FForms[a]);
        Exit;
      end;
end;

function TApplication.GetScreenSize: TCoord;
var
  CSBI:TConsoleScreenBufferInfo;
begin
  if not GetConsoleScreenBufferInfo(FStdOut,CSBI) then
    RaiseLastOSError;
  Result:=CSBI.dwSize;
end;

procedure TApplication.Invalidate;
begin
  if not (csDestroying in ComponentState) then begin
    FMessageQueue.FSection.Enter;
    try
      FMessageQueue.FInvalidateFlag:=True;
      FQueueEvent.SetEvent;
    finally
      FMessageQueue.FSection.Leave;
    end;
  end;
end;

procedure TApplication.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  a:Integer;
begin
  inherited;
  if (Operation=opRemove) and (AComponent is TControl) and not (csDestroying in ComponentState) then begin
    FForms.Remove(AComponent);
    FModalStack.Remove(TControl(AComponent));
    FActiveStack.Remove(TControl(AComponent));
    FCaptureStack.Remove(TControl(AComponent));
    FFocusStack.Remove(TControl(AComponent));
    if not Assigned(FActiveStack.Control) then
      for a:=FForms.Count-1 downto 0 do
        if TForm(FForms[a]).Visible or (FForms[a]=FMainForm) then begin
          SetActiveForm(TForm(FForms[a]));
          Break;
        end;
    if AComponent=FMainForm then begin
      Terminate;
      FMainForm:=nil;
    end;
    Invalidate;
  end;
end;

procedure TApplication.PopBufferSize;
begin
  InterlockedDecrement(FBufferSizeUpdateCount);
end;

procedure TApplication.PostMessage(Message: Cardinal;
  Sender: TObject; wParam, lParam: Integer);
var
  m:TPostMessage;
begin
  m.Message:=Message;
  m.Sender:=Sender;
  m.wParam:=wParam;
  m.lParam:=lParam;
  FMessageQueue.FSection.Enter;
  try
    Assert(FMessageQueue.FPostQueueLength<GMaxMessageQueueLength,'Stack overflow');
    Inc(FMessageQueue.FPostQueueLength);
    FMessageQueue.FPostQueue[FMessageQueue.FPostQueueLength]:=m;
    FQueueEvent.SetEvent;
  finally
    FMessageQueue.FSection.Leave;
  end;
end;

procedure TApplication.ProcessInvalidateFlag;
var
  a:Integer;
  mPaint:TCMPaint;
  CSBI:TConsoleScreenBufferInfo;
begin
  mPaint.Message:=CM_PAINT;
  mPaint.Canvas:=FCanvas;
  GetConsoleScreenBufferInfo(FStdOut,CSBI);
  FCanvas.BeginPaint(CSBI.dwSize);
  try
    for a:=0 to FForms.Count-1 do
      with TForm(FForms[a]) do
        if Visible or (FForms[a]=FMainForm) then
          Dispatch(mPaint);
  finally
    FCanvas.EndPaint(FStdOut);
  end;
end;

procedure TApplication.ProcessKeyEvent(KeyEvent: TKeyEventRecord);
var
  mKey:TCMKey;
  mChar:TCMChar;
begin
  if Assigned(FActiveStack.Control) then begin
    FActiveStack.Push;
    try
      mKey.Key:=KeyEvent.wVirtualKeyCode;
      mKey.ShiftState:=TranslateShiftState(KeyEvent.dwControlKeyState);
      mKey.Canceled:=False;
      if KeyEvent.bKeyDown then begin
        mKey.Message:=CM_KEY_DOWN;
        mChar.AsciiChar:=KeyEvent.AsciiChar;
        mChar.Canceled:=False;
        mChar.Message:=CM_CHAR;
      end else
        mKey.Message:=CM_KEY_UP;
      FActiveStack.Control.Dispatch(mKey);
      if KeyEvent.bKeyDown and not mKey.Canceled then begin
        if FActiveStack.Consistent then
          FActiveStack.Control.Dispatch(mChar);
        if mKey.Key=VK_ESCAPE then
          SetCapture(nil);
      end;
    finally
      FActiveStack.Pop;
    end;
  end;
end;

procedure TApplication.ProcessMouseEvent(MouseEvent: TMouseEventRecord);
var
  s:TShiftState;
  t:Boolean;
  mMouseButton:TCMMouseButton;
  mMouseMove:TCMMouseMove;
  mMouseWheel:TCMMouseWheel;
begin
  mMouseButton.Position:=MouseEvent.dwMousePosition;
  mMouseButton.ShiftState:=TranslateShiftState(MouseEvent.dwControlKeyState);
  case MouseEvent.dwEventFlags of
    0:begin
      s:=FMouseState;
      FMouseState:=TranslateMouseState(MouseEvent.dwButtonState);
      if CalcMouseDiff(s,FMouseState,mMouseButton.Button,t) then begin
        FLastMouseButton:=mMouseButton.Button;
        if t then begin
          if s=[] then
            SetActiveCapture(ControlFromPoint(MouseEvent.dwMousePosition));
          mMouseButton.Message:=CM_MOUSE_DOWN;
        end else
          mMouseButton.Message:=CM_MOUSE_UP;
        if Assigned(FCaptureStack.Control) then begin
          FCaptureStack.Push;
          try
            with FCaptureStack.Control.AbsoluteOrigin do
              OffsetCoord(mMouseButton.Position,-X,-Y);
            FCaptureStack.Control.Dispatch(mMouseButton);
            if not t and (mMouseButton.Button=mbLeft) and FCaptureStack.Consistent and CoordInArea(MouseEvent.dwMousePosition,FCaptureStack.Control.AbsoluteBounds) then begin
              mMouseButton.Message:=CM_MOUSE_CLICK;
              FCaptureStack.Control.Dispatch(mMouseButton);
            end;
          finally
            FCaptureStack.Pop;
          end;
        end;
      end;
    end;
    DOUBLE_CLICK:begin
      mMouseButton.Button:=FLastMouseButton;
      mMouseButton.Position:=MouseEvent.dwMousePosition;
      SetCapture(ControlFromPoint(MouseEvent.dwMousePosition));
      if Assigned(FCaptureStack.Control) then begin
        FCaptureStack.Push;
        try
          with FCaptureStack.Control.AbsoluteOrigin do
            OffsetCoord(mMouseButton.Position,-X,-Y);
          mMouseButton.Message:=CM_MOUSE_DOWN;
          FCaptureStack.Control.Dispatch(mMouseButton);
          mMouseButton.Message:=CM_MOUSE_UP;
          if FCaptureStack.Consistent then
            FCaptureStack.Control.Dispatch(mMouseButton);
          mMouseButton.Message:=CM_MOUSE_DOUBLE_CLICK;
          if FCaptureStack.Consistent then
            FCaptureStack.Control.Dispatch(mMouseButton);
        finally
          FCaptureStack.Pop;
        end;
      end;
    end;
    MOUSE_MOVED:begin
      if FMouseState=[] then
        SetCapture(ControlFromPoint(MouseEvent.dwMousePosition));
      if Assigned(FCaptureStack.Control) then begin
        mMouseMove.Message:=CM_MOUSE_MOVE;
        mMouseMove.Position:=MouseEvent.dwMousePosition;
        with FCaptureStack.Control.AbsoluteOrigin do
          OffsetCoord(mMouseMove.Position,-X,-Y);
        mMouseMove.ShiftState:=TranslateShiftState(MouseEvent.dwControlKeyState);
        FCaptureStack.Control.Dispatch(mMouseMove);
      end;
    end;
    MOUSE_WHEELED:begin
      if FMouseState=[] then
        SetCapture(ControlFromPoint(MouseEvent.dwMousePosition));
      if Assigned(FCaptureStack.Control) then begin
        mMouseWheel.Message:=CM_MOUSE_WHEEL;
        mMouseWheel.Position:=MouseEvent.dwMousePosition;
        with FCaptureStack.Control.AbsoluteOrigin do
          OffsetCoord(mMouseWheel.Position,-X,-Y);
        mMouseWheel.Delta:=0;
        mMouseWheel.ShiftState:=TranslateShiftState(MouseEvent.dwControlKeyState);
        FCaptureStack.Control.Dispatch(mMouseMove);
      end;
    end;
  end;
end;

procedure TApplication.ProcessPostMessage(PostMessage: TPostMessage);
begin
  PostMessage.Sender.Dispatch(PostMessage);
end;

procedure TApplication.ProcessWindowEvent;
begin
  if FBufferSizeUpdateCount=0 then begin
    AdjustScreenBuffer(False);
    Invalidate;
  end;
end;

procedure TApplication.PushBufferSize;
begin
  InterlockedIncrement(FBufferSizeUpdateCount);
end;

function CtrlHandler(CtrlType:Cardinal):BOOL;stdcall;
begin
  Result:=False;
end;

procedure TApplication.Run;
var
  i:TConsoleCursorInfo;
begin
  i.dwSize:=50;
  i.bVisible:=False;
  SetConsoleCursorInfo(FStdOut,i);
  Update;
  if not SetConsoleCtrlHandler(@CtrlHandler,True) then
    RaiseLastOSError;
  try
    FMessageQueue.FResumeEvent.SetEvent;
    repeat
      if FMessageQueue.FEmptyInputQueue then
        FQueueEvent.WaitFor(INFINITE);
      FMessageQueue.FSection.Enter;
      try
        FlushMessageQueue;
      finally
        FMessageQueue.FSection.Leave;
      end;
    until FMessageQueue.Terminated;
  finally
    SetConsoleCtrlHandler(@CtrlHandler,False);
  end;
end;

procedure TApplication.RunModal(AForm: TForm);
begin
  SetCapture(nil);
  SetActiveForm(nil);
//  SetFocus(nil);
  FModalStack.Push(AForm);
  SetActiveForm(AForm);
  FMessageQueue.FSection.Leave;
  try
    repeat
      if FMessageQueue.FEmptyInputQueue then
        FQueueEvent.WaitFor(INFINITE);
      FMessageQueue.FSection.Enter;
      try
        FlushMessageQueue;
      finally
        FMessageQueue.FSection.Leave;
      end;
    until FMessageQueue.Terminated or FModalStack.Terminated;
  finally
    FMessageQueue.FSection.Enter;
    FModalStack.Pop;
  end;
end;

procedure TApplication.SetActiveCapture(AControl: TControl);
var
  c:TControl;
begin
  if Assigned(AControl) then begin
    c:=AControl.AbsoluteParent;
    if c is TForm then
      SetActiveForm(c as TForm);
  end;
  SetCapture(AControl);
  SetFocus(AControl);
end;

procedure TApplication.SetActiveForm(AForm: TForm);
var
  a:Integer;
begin
  if (AForm<>FActiveStack.Control) and (FModalStack.Empty or (FModalStack.Form=AForm)) then begin
    SetCapture(nil);
    if Assigned(AForm) and not AForm.Enabled then
      AForm:=nil;
    a:=FForms.IndexOf(AForm);
    if a>-1 then
      FForms.Move(a,FForms.Count-1)
    else begin
      for a:=FForms.Count-1 downto 0 do
        with TForm(FForms[a]) do
          if IsVisible and IsEnabled then begin
            AForm:=TForm(FForms[a]);
            FForms.Move(a,FForms.Count-1);
            Break;
          end;
    end;
    FActiveStack.Control:=AForm;
    if Assigned(AForm) then
      SetFocus(AForm.FFocus);
    Invalidate;
  end;
end;

procedure TApplication.SetCapture(AControl: TControl);
begin
  if (AControl<>FCaptureStack.Control) and (not Assigned(AControl) or FModalStack.Empty or (FModalStack.Form=AControl.AbsoluteParent)) then begin
    if Assigned(AControl) and not AControl.IsEnabled then
      AControl:=nil;
    FCaptureStack.Control:=AControl;
    if Assigned(AControl) then
      AControl.FreeNotification(Self);
  end;
end;

procedure TApplication.SetFocus(AControl: TControl);
begin
  if (AControl<>FFocusStack.Control) and (not Assigned(AControl) or Assigned(FActiveStack.Control) and AControl.HasForParent(FActiveStack.Control)) then begin
    if Assigned(FFocusStack.Control) then
      FFocusStack.Control.ControlState:=FFocusStack.Control.ControlState-[csFocus];
    if Assigned(AControl) and not AControl.IsFocusable then
      AControl:=nil;
    FFocusStack.Control:=AControl;
    if Assigned(FActiveStack.Control) then begin
      TForm(FActiveStack.Control).FFocus:=AControl;
      if Assigned(AControl) then
        AControl.FreeNotification(FActiveStack.Control);
    end;
    if Assigned(AControl) then begin
      AControl.FreeNotification(Self);
      AControl.ControlState:=AControl.ControlState+[csFocus];
      AControl.Invalidate;
    end;
  end;
end;

procedure TApplication.Terminate;
begin
  FMessageQueue.Terminate;
  FQueueEvent.SetEvent;
end;

function TApplication.TranslateMouseState(const State:Cardinal):TShiftState;
const
  T:array[0..2] of TShiftState=([ssLeft],[ssMiddle],[ssRight]);
  U:array[0..2] of Cardinal=(FROM_LEFT_1ST_BUTTON_PRESSED,FROM_LEFT_2ND_BUTTON_PRESSED,RIGHTMOST_BUTTON_PRESSED);
var
  a:Integer;
begin
  Result:=[];
  for a:=Low(T) to High(T) do
    if (U[a] and State)=U[a] then
      Result:=Result+T[a];
end;

function TApplication.TranslateShiftState(const State:Cardinal):TShiftState;
const
  T:array[0..4] of TShiftState=([ssAlt],[ssCtrl],[ssAlt],[ssCtrl],[ssShift]);
  U:array[0..4] of Cardinal=(LEFT_ALT_PRESSED,LEFT_CTRL_PRESSED,RIGHT_ALT_PRESSED,RIGHT_CTRL_PRESSED,SHIFT_PRESSED);
var
  a:Integer;
begin
  Result:=FMouseState;
  for a:=Low(T) to High(T) do
    if (U[a] and State)=U[a] then
      Result:=Result+T[a];
end;

procedure TApplication.Update;
begin
  if Assigned(FActiveStack.Control) and not (FActiveStack.Control.IsVisible and FActiveStack.Control.IsEnabled) then
    SetActiveForm(nil);
  if Assigned(FCaptureStack.Control) and not (FCaptureStack.Control.IsVisible and FActiveStack.Control.IsEnabled) then
    SetCapture(nil);
  if Assigned(FFocusStack.Control) and not (Assigned(FActiveStack.Control) and FFocusStack.Control.HasForParent(FActiveStack.Control) and FFocusStack.Control.IsFocusable) then
    SetFocus(nil);
  Invalidate;
end;

{ TModalStateStack }

function TModalStateStack.GetEmpty: Boolean;
begin
  Result:=FLength=0;
end;

function TModalStateStack.GetForm: TForm;
begin
  if FLength=0 then
    Result:=nil
  else
    Result:=FStates[FLength].Form;
end;

function TModalStateStack.GetTerminated: Boolean;
begin
  Result:=(FLength>0) and FStates[FLength].Terminated;
end;

procedure TModalStateStack.Pop;
begin
  Assert(FLength>0,'Stack underflow');
  Dec(FLength);
end;

procedure TModalStateStack.Push(AForm: TForm);
begin
  Assert(FLength<GMaxStateStackLength,'Stack overflow');
  Inc(FLength);
  with FStates[FLength] do begin
    Form:=AForm;
    Terminated:=False;
  end;
end;

procedure TModalStateStack.Remove(AControl: TControl);
var
  a:Cardinal;
begin
  for a:=FLength downto 1 do
    with FStates[a] do
      if AControl=Form then begin
        Terminated:=True;
        Form:=nil;
      end;
end;

{ TControlStateStack }

function TControlStateStack.GetConsistent: Boolean;
begin
  Result:=(FLength=0) or FConsistency[FLength];
end;

procedure TControlStateStack.Pop;
begin
  Assert(FLength>0,'Stack underflow');
  Dec(FLength);
end;

procedure TControlStateStack.Push;
begin
  Assert(FLength<GMaxStateStackLength,'Stack overflow');
  Inc(FLength);
  FConsistency[FLength]:=True;
end;

procedure TControlStateStack.Remove(AControl: TControl);
begin
  if AControl=FControl then begin
    FControl:=nil;
    Reset;
  end;
end;

procedure TControlStateStack.Reset;
var
  a:Cardinal;
begin
  for a:=FLength downto 1 do
    FConsistency[a]:=False;
end;

procedure TControlStateStack.SetControl(const Value: TControl);
begin
  if FControl<>Value then begin
    FControl:=Value;
    Reset;
  end;
end;

initialization
  Application:=TApplication.Create(nil);
finalization
  try
    Application.Free;
  finally
    Application:=nil;
  end;
end.
