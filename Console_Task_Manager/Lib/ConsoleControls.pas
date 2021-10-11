unit ConsoleControls;

interface

uses
  Windows,SysUtils,Classes,ConsoleUtils,ConsoleGraphics,ConsoleMessages;

type
  TControlState=set of (csUpdate,csUpdateControls,csHorizontalScroll,csVerticalScroll,csCapture,csFocus,csModal);
  TControlStyle=set of (csFocusable,csTabStop,csArrowStop,csDefaultOverride,csShadow);

  TAlign=(alNone,alTop,alBottom,alLeft,alRight,alClient);

  TBorderStyle=(bsNone,bsSingle,bsDouble);

  TEdgeBorder=(ebTop,ebBottom,ebLeft,ebRight);
  TEdgeBorders=set of TEdgeBorder;

  THitZone=(hzNone,hzBorder,hzClient,
            hzScrollVertical,hzScrollHorizontal,hzScrollUp,hzScrollDown,hzScrollLeft,hzScrollRight,
            hzTitle,hzSystemMenu,hzHelp,hzZoom,hzClose,
            hzSizeN,hzSizeS,hzSizeW,hzSizeE,hzSizeNW,hzSizeSW,hzSizeNE,hzSizeSE);

  TControlAspect=(caFocused,caActive,caInactive,caDisabled,caInvisible);

  TKeyEvent=procedure(Sender:TObject;var Key:Word;Shift:TShiftState) of object;
  TKeyPressEvent=procedure(Sender:TObject;var Key:Char) of object;

  TMouseEvent=procedure(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer) of object;
  TMouseMoveEvent=procedure(Sender:TObject;Shift:TShiftState;X,Y:Integer) of object;

  TControl=class(TComponent)
  private
    FControlState:TControlState;
    FControlStyle:TControlStyle;
    FParent:TControl;
    FCaption:string;
    FBounds,FClientArea,FScrollArea:TArea;
    FAlign:TAlign;
    FAlignState:Boolean;
    FVisible,FEnabled:Boolean;
    FBorderStyle:TBorderStyle;
    FEdgeBorders:TEdgeBorders;

    FLastHitZone:array[mbLeft..mbRight] of THitZone;
    FLastHitPosition:array[mbLeft..mbRight] of TCoord;

    FControls:TList;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnDblClick: TNotifyEvent;
    FOnClick: TNotifyEvent;

    procedure SetParent(const Value: TControl);virtual;
    function GetAbsoluteParent: TControl;

    procedure SetCaption(const Value: string);

    procedure SetBounds(const Value: TArea);
    procedure SetLeft(const Value: Smallint);
    procedure SetTop(const Value: Smallint);
    procedure SetWidth(const Value: Smallint);
    procedure SetHeight(const Value: Smallint);
    procedure SetAlign(const Value: TAlign);

    procedure SetVisible(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    function GetDefault:Boolean;
    procedure SetDefault(const Value:Boolean);

    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetEdgeBorders(const Value: TEdgeBorders);

    function GetControl(const Index: Integer): TControl;
    function GetControlCount: Integer;

    function GetControlAspect: TControlAspect;

    function GetLastHitZone(const Button: TMouseButton): THitZone;
    function GetLastHitPosition(const Button: TMouseButton): TCoord;

    procedure CMKeyDown(var Message:TCMKey);message CM_KEY_DOWN;
    procedure CMKeyUp(var Message:TCMKey);message CM_KEY_UP;
    procedure CMChar(var Message:TCMChar);message CM_CHAR;

    procedure CMMouseMove(var Message:TCMMouseMove);message CM_MOUSE_MOVE;
    procedure CMMouseDown(var Message:TCMMouseButton);message CM_MOUSE_DOWN;
    procedure CMMouseUp(var Message:TCMMouseButton);message CM_MOUSE_UP;
    procedure CMMouseClick(var Message:TCMMouseButton);message CM_MOUSE_CLICK;
    procedure CMMouseDoubleClick(var Message:TCMMouseButton);message CM_MOUSE_DOUBLE_CLICK;
  protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;

    procedure SetControlState(const Value: TControlState);virtual;

    procedure AddControl(Control:TControl);
    procedure RemoveControl(Control:TControl);

    function GetBorderColor:TThemeColor;virtual;
    function GetBackgroundColor:TThemeColor;virtual;
    function GetForegroundColor:TThemeColor;virtual;

    procedure Paint(Canvas:TCanvas);virtual;
    procedure PaintBackground(Canvas:TCanvas);virtual;
    procedure PaintBorder(Canvas:TCanvas);virtual;
    procedure PaintClient(Canvas:TCanvas);virtual;
    procedure PaintControls(Canvas:TCanvas);virtual;

    procedure Update;
    procedure UpdateBounds(var Bounds:TArea);virtual;
    procedure UpdateClientArea(var ClientArea:TArea);virtual;
    procedure UpdateScrollArea(var ScrollArea:TArea);virtual;
    procedure UpdateControls;

    function GetAbsoluteOrigin: TCoord;
    function GetAbsoluteBounds: TArea;

    property ClientArea:TArea read FClientArea;
    property ScrollArea:TArea read FScrollArea write FScrollArea;

    function HitTest(Position:TCoord):THitZone;virtual;
    function HitArea(Zone:THitZone):TArea;virtual;
    property LastHitZone[const Button:TMouseButton]:THitZone read GetLastHitZone;
    property LastHitPosition[const Button:TMouseButton]:TCoord read GetLastHitPosition;
    function HasBorder(Edge:TEdgeBorder):Boolean;virtual;

    property ControlAspect:TControlAspect read GetControlAspect;

    property BorderStyle:TBorderStyle read FBorderStyle write SetBorderStyle;
    property EdgeBorders:TEdgeBorders read FEdgeBorders write SetEdgeBorders;

    property BorderColor:TThemeColor read GetBorderColor;
    property BackgroundColor:TThemeColor read GetBackgroundColor;
    property ForegroundColor:TThemeColor read GetForegroundColor;
  public
    constructor Create(AOwner:TComponent);override;
    procedure AfterConstruction;override;
    destructor Destroy;override;

    function IsVisible:Boolean;virtual;
    function IsEnabled:Boolean;virtual;
    function IsActive:Boolean;virtual;
    function IsFocusable:Boolean;virtual;
    function IsFocused:Boolean;virtual;
    function IsDefault:Boolean;virtual;
    function HasForParent(AControl:TControl):Boolean;virtual;

    function Action(ShiftState:TShiftState;Key:Word):Boolean;virtual;

    procedure KeyDown(var Key:Word;Shift:TShiftState);virtual;
    procedure KeyUp(var Key:Word;Shift:TShiftState);virtual;
    procedure KeyPress(var Key:Char);virtual;

    procedure MouseMove(Shift:TShiftState;X,Y:Integer);virtual;
    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);virtual;
    procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);virtual;
    procedure Click;virtual;
    procedure DblClick;virtual;

    property ControlState:TControlState read FControlState write SetControlState;
    property ControlStyle:TControlStyle read FControlStyle write FControlStyle;

    procedure Invalidate;

    procedure Resize(ALeft,ATop,AWidth,AHeight:Smallint);overload;

    property Parent:TControl read FParent write SetParent;
    property AbsoluteParent:TControl read GetAbsoluteParent;

    property Caption:string read FCaption write SetCaption;

    property Bounds:TArea read FBounds write SetBounds;
    property Left:Smallint read FBounds.X write SetLeft;
    property Top:Smallint read FBounds.Y write SetTop;
    property Width:Smallint read FBounds.Width write SetWidth;
    property Height:Smallint read FBounds.Height write SetHeight;
    property Align:TAlign read FAlign write SetAlign;
    property AbsoluteOrigin:TCoord read GetAbsoluteOrigin;
    property AbsoluteBounds:TArea read GetAbsoluteBounds;
    property ClientWidth:Smallint read FClientArea.Width;
    property ClientHeight:Smallint read FClientArea.Height;
    property ClientOrigin:TCoord read FClientArea.Origin;

    property Visible:Boolean read FVisible write SetVisible;
    property Enabled:Boolean read FEnabled write SetEnabled;
    property Default:Boolean read GetDefault write SetDefault;

    property ControlCount:Integer read GetControlCount;
    property Control[const Index:Integer]:TControl read GetControl;
    function ControlFromPoint(p:TCoord;VisibleOnly:Boolean=True):TControl;
    function ControlIndex(Control:TControl):Integer;

    property OnKeyDown:TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp:TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyPress:TKeyPressEvent read FOnKeyPress write FOnKeyPress;

    property OnMouseMove:TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown:TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp:TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnClick:TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick:TNotifyEvent read FOnDblClick write FOnDblClick;
  end;

implementation

uses
  ConsoleForms;

{ TControl }

function TControl.Action(ShiftState: TShiftState; Key: Word): Boolean;
begin
  Result:=False;
end;

procedure TControl.AddControl(Control: TControl);
begin
  FControls.Add(Control);
  Control.FreeNotification(Self);
  UpdateControls;
end;

procedure TControl.AfterConstruction;
begin
  inherited;
  Update;
end;

procedure TControl.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TControl.CMChar(var Message: TCMChar);
begin
  with Message do
    KeyPress(AsciiChar);
end;

procedure TControl.CMKeyDown(var Message: TCMKey);
begin
  with Message do begin
    KeyDown(Key,ShiftState);
    if Key=0 then
      Canceled:=True;
  end;
end;

procedure TControl.CMKeyUp(var Message: TCMKey);
begin
  with Message do begin
    KeyUp(Key,ShiftState);
    if Key=0 then
      Canceled:=True;
  end;
end;

procedure TControl.CMMouseClick(var Message: TCMMouseButton);
begin
  with Message do
    if (Button=mbLeft) and (FLasthitZone[Button]=HitTest(Position)) then
      case FLasthitZone[Button] of
        hzClient:Click;
      end;
end;

procedure TControl.CMMouseDoubleClick(var Message: TCMMouseButton);
begin
  with Message do
    if Button=mbLeft then
      case HitTest(Position) of
        hzClient:DblClick;
      end;
end;

procedure TControl.CMMouseDown(var Message: TCMMouseButton);
begin
  with Message do begin
    FLastHitZone[Button]:=HitTest(Position);
    FLastHitPosition[Button]:=Position;
    case FLastHitZone[Button] of
      hzClient:MouseDown(Button,ShiftState,Position.X-FClientArea.X-FScrollArea.X,Position.Y-FClientArea.Y-FScrollArea.Y);
      hzScrollVertical:if Button=mbLeft then begin
        if FClientArea.Height<>3 then
          FScrollArea.Y:=-((Position.Y-FClientArea.Y-1)*(FScrollArea.Height-FClientArea.Height)) div (FClientArea.Height-3);
        Update;
      end;
      hzScrollHorizontal:if Button=mbLeft then begin
        if FClientArea.Width<>3 then
          FScrollArea.X:=-((Position.X-FClientArea.X-1)*(FScrollArea.Width-FClientArea.Width)) div (FClientArea.Width-3);
        Update;
      end;
      hzScrollUp:if Button=mbLeft then begin
        Inc(FScrollArea.Y);
        Update;
      end;
      hzScrollDown:if Button=mbLeft then begin
        Dec(FScrollArea.Y);
        Update;
      end;
      hzScrollLeft:if Button=mbLeft then begin
        Inc(FScrollArea.X);
        Update;
      end;
      hzScrollRight:if Button=mbLeft then begin
        Dec(FScrollArea.X);
        Update;
      end;
    end;
  end;
end;

procedure TControl.CMMouseMove(var Message: TCMMouseMove);
begin
  with Message do
    if [ssLeft]<=ShiftState then begin
      case FLastHitZone[mbLeft] of
        hzClient:MouseMove(ShiftState,Position.X-FClientArea.X-FScrollArea.X,Position.Y-FClientArea.Y-FScrollArea.Y);
        hzScrollVertical:if ShiftState=[ssLeft] then begin
          if FClientArea.Height<>3 then
            FScrollArea.Y:=-((Position.Y-FClientArea.Y-1)*(FScrollArea.Height-FClientArea.Height)) div (FClientArea.Height-3);
          Update;
        end;
        hzScrollHorizontal:if ShiftState=[ssLeft] then begin
          if FClientArea.Width<>3 then
            FScrollArea.X:=-((Position.X-FClientArea.X-1)*(FScrollArea.Width-FClientArea.Width)) div (FClientArea.Width-3);
          Update;
        end;
      end;
    end else begin
      case HitTest(Position) of
        hzClient:MouseMove(ShiftState,Position.X-FClientArea.X-FScrollArea.X,Position.Y-FClientArea.Y-FScrollArea.Y);
      end;
    end;
end;

procedure TControl.CMMouseUp(var Message: TCMMouseButton);
begin
  with Message do begin
    case FLasthitZone[Button] of
      hzClient:MouseUp(Button,ShiftState,Position.X-FClientArea.X-FScrollArea.X,Position.Y-FClientArea.Y-FScrollArea.Y);
    end;
    //FLasthitZone[Button]:=hzNone;
  end;
end;

function TControl.ControlFromPoint(p: TCoord;
  VisibleOnly: Boolean): TControl;
var
  a:Integer;
begin
  Result:=nil;
  if CoordInArea(p,FClientArea) then begin
    p.X:=p.X-FClientArea.X-FScrollArea.X;
    p.Y:=p.Y-FClientArea.Y-FScrollArea.Y;
    for a:=FControls.Count-1 downto 0 do
      with TControl(FControls[a]) do
        if CoordInArea(p,FBounds) and (not VisibleOnly or FVisible) then begin
          p.X:=p.X-FBounds.X;
          p.Y:=p.Y-FBounds.Y;
          Result:=ControlFromPoint(p,VisibleOnly);
          if not Assigned(Result) then
            Result:=TControl(Self.FControls[a]);
          Exit;
        end;
  end;
end;

function TControl.ControlIndex(Control: TControl): Integer;
begin
  Result:=FControls.IndexOf(Control);
end;

constructor TControl.Create(AOwner: TComponent);
begin
  FBounds:=Area(5,5,25,12);

  FVisible:=True;
  FEnabled:=True;

  FBorderStyle:=bsDouble;
  FEdgeBorders:=[ebTop..ebRight];

  FControls:=TList.Create;
  inherited;
end;

procedure TControl.DblClick;
begin
  if Assigned(FOnDblCLick) then
    FOnDblClick(Self);
end;

destructor TControl.Destroy;
var
  a:Integer;
begin
  inherited;
  for a:=FControls.Count-1 downto 0 do
    TControl(FControls[a]).SetParent(nil);
  SetParent(nil);
  FreeAndNil(FControls);
end;

function TControl.GetAbsoluteBounds: TArea;
begin
  Result.Size:=FBounds.Size;
  Result.Origin:=GetAbsoluteOrigin;
end;

function TControl.GetAbsoluteOrigin: TCoord;
begin
  Result:=FBounds.Origin;
  if Assigned(FParent) then begin
    OffsetCoord(Result,FParent.GetAbsoluteOrigin);
    OffsetCoord(Result,FParent.FClientArea.Origin);
    OffsetCoord(Result,FParent.FScrollArea.Origin);
  end;
end;

function TControl.GetAbsoluteParent: TControl;
begin
  if Assigned(FParent) then
    Result:=FParent.GetAbsoluteParent
  else
    Result:=Self;
end;

function TControl.GetBackgroundColor: TThemeColor;
begin
  if IsEnabled then begin
    if IsActive then
      Result:=tcActiveBack
    else
      Result:=tcInactiveBack
  end else
    Result:=tcDisabledBack;
end;

function TControl.GetBorderColor: TThemeColor;
begin
  if IsEnabled then begin
    if IsActive then
      Result:=tcActiveBorder
    else
      Result:=tcInactiveBorder
  end else
    Result:=tcDisabledBorder;
end;

function TControl.GetControl(const Index: Integer): TControl;
begin
  Result:=TControl(FControls[Index]);
end;

function TControl.GetControlAspect: TControlAspect;
begin
  if IsVisible then begin
    if IsEnabled then begin
      if IsActive then begin
        if IsFocused then
          Result:=caFocused
        else
          Result:=caActive;
      end else
        Result:=caInactive;
    end else
      Result:=caDisabled;
  end else
    Result:=caInvisible;
end;

function TControl.GetControlCount: Integer;
begin
  Result:=FControls.Count;
end;

function TControl.GetDefault: Boolean;
var
  c:TControl;
begin
  c:=GetAbsoluteParent;
  Result:=(c is TForm) and (TForm(c).Default=Self);
end;

function TControl.GetForegroundColor: TThemeColor;
begin
  if IsEnabled then begin
    if IsActive then
      Result:=tcActiveText
    else
      Result:=tcInactiveText
  end else
    Result:=tcDisabledText;
end;

function TControl.GetLastHitPosition(const Button: TMouseButton): TCoord;
begin
  Result:=FLastHitPosition[Button];
end;

function TControl.GetLastHitZone(const Button: TMouseButton): THitZone;
begin
  Result:=FLastHitZone[Button];
end;

function TControl.HasBorder(Edge: TEdgeBorder): Boolean;
begin
  Result:=Edge in FEdgeBorders;
end;

function TControl.HasForParent(AControl: TControl): Boolean;
begin
  Result:=(FParent=AControl) or Assigned(FParent) and FParent.HasForParent(AControl);
end;

function TControl.HitArea(Zone: THitZone): TArea;
begin
  Result:=GNullArea;
  case Zone of
    hzBorder:Result:=Area(0,0,FBounds.Width,FBounds.Height);
    hzClient:Result:=FClientArea;
    hzScrollVertical:if csVerticalScroll in FControlState then
      with FClientArea do
        Result:=Area(X+Width,Y,1,Height);
    hzScrollHorizontal:if csHorizontalScroll in FControlState then
      with FClientArea do
        Result:=Area(X,Y+Height,Width,1);
    hzScrollUp:with FClientArea do
      if (csVerticalScroll in FControlState) and (Height>1) then
        Result:=Area(X+Width,Y,1,1);
    hzScrollDown:with FClientArea do
      if (csVerticalScroll in FControlState) and (Height>1) then
        Result:=Area(X+Width,Y+Height-1,1,1);
    hzScrollLeft:with FClientArea do
      if (csHorizontalScroll in FControlState) and (Width>1) then
        Result:=Area(X,Y+Height,1,1);
    hzScrollRight:with FClientArea do
      if (csHorizontalScroll in FControlState) and (Width>1) then
        Result:=Area(X+Width-1,Y+Height,1,1);
  end;
end;

function TControl.HitTest(Position: TCoord): THitZone;
var
  z:THitZone;
begin
  Result:=hzNone;
  for z:=hzBorder to High(ThitZone) do
    if CoordInArea(Position,HitArea(z)) then
      Result:=z;
end;

procedure TControl.Invalidate;
begin
  Application.Invalidate;
end;

function TControl.IsActive: Boolean;
begin
  Result:=Assigned(FParent) and FParent.IsActive;
end;

function TControl.IsDefault: Boolean;
var
  c:TControl;
begin
  c:=GetAbsoluteParent;
  Result:=(c is TForm) and (TForm(c).Default=Self) and (not Assigned(TForm(c).Focus) or not (csDefaultOverride in TForm(c).Focus.FControlStyle));
end;

function TControl.IsEnabled: Boolean;
begin
  Result:=FEnabled and Assigned(FParent) and FParent.IsEnabled;
end;

function TControl.IsFocusable: Boolean;
begin
  Result:=(csFocusable in ControlStyle) and IsEnabled and IsVisible;
end;

function TControl.IsFocused: Boolean;
begin
  Result:=csFocus in FControlState;
end;

function TControl.IsVisible: Boolean;
begin
  Result:=FVisible and Assigned(FParent) and FParent.IsVisible;
end;

procedure TControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self,Key,Shift);
end;

procedure TControl.KeyPress(var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self,Key);
end;

procedure TControl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self,Key,Shift);
end;

procedure TControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self,Button,Shift,X,Y);
end;

procedure TControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self,Shift,X,Y);
end;

procedure TControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self,Button,Shift,X,Y);
end;

procedure TControl.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  a:Integer;
begin
  inherited;
  if Operation=opRemove then begin
    a:=FControls.IndexOf(AComponent);
    if a>-1 then begin
      FControls.Remove(AComponent);
      UpdateControls;
    end;
  end;
end;

procedure TControl.Paint(Canvas: TCanvas);
begin
  PaintBackground(Canvas);
  PaintBorder(Canvas);
  Canvas.PushClipArea(FClientArea,AddCoord(FClientArea.Origin,FScrollArea.Origin));
  try
    PaintClient(Canvas);
    PaintControls(Canvas);
  finally
    Canvas.PopClipArea;
  end;
end;

procedure TControl.PaintBackground(Canvas: TCanvas);
begin
  Canvas.Brush.Style:=bsFill;
  Canvas.Brush.Color:=GTheme[BackgroundColor];
  Canvas.Pen.Color:=GTheme[ForegroundColor];
  Canvas.FillRect(0,0,FBounds.Width,FBounds.Height);
end;

procedure TControl.PaintBorder(Canvas: TCanvas);
begin
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Color:=GTheme[BorderColor];
  case FBorderStyle of
    bsSingle:Canvas.Pen.Style:=psSingle;
    bsDouble:Canvas.Pen.Style:=psDouble;
    bsNone:Canvas.Pen.Style:=psClear;
  end;
  if HasBorder(ebTop) then
    Canvas.HorizontalLine(0,FBounds.Width-1,0);
  if HasBorder(ebBottom) then
    Canvas.HorizontalLine(0,FBounds.Width-1,FBounds.Height-1);
  if HasBorder(ebLeft) then
    Canvas.VerticalLine(0,FBounds.Height-1,0);
  if HasBorder(ebRight) then
    Canvas.VerticalLine(0,FBounds.Height-1,FBounds.Width-1);
  if csVerticalScroll in FControlState then
    Canvas.DrawVerticalScrollbar(FClientArea.X+FClientArea.Width,FClientArea.Y,FClientArea.Height,-FScrollArea.Y,FScrollArea.Height-FClientArea.Height);
  if csHorizontalScroll in FControlState then
    Canvas.DrawHorizontalScrollbar(FClientArea.X,FClientArea.Y+FClientArea.Height,FClientArea.Width,-FScrollArea.X,FScrollArea.Width-FClientArea.Width);
end;

procedure TControl.PaintClient(Canvas: TCanvas);
begin

end;

procedure TControl.PaintControls(Canvas: TCanvas);
var
  a:Integer;
begin
  for a:=0 to FControls.Count-1 do
    with TControl(FControls[a]) do
      if Visible then begin
        Canvas.PushClipArea(FBounds,FBounds.Origin);
        try
          Paint(Canvas);
        finally
          Canvas.PopClipArea;
        end;
      end;
end;

procedure TControl.RemoveControl(Control: TControl);
begin
  FControls.Remove(Control);
  UpdateControls;
end;

procedure TControl.Resize(ALeft, ATop, AWidth, AHeight: Smallint);
begin
  SetBounds(Area(ALeft,ATop,AWidth,AHeight));
end;

procedure TControl.SetAlign(const Value: TAlign);
begin
  FAlign:=Value;
  if Assigned(FParent) then
    FParent.Update;
end;

procedure TControl.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle<>Value then begin
    FBorderStyle:=Value;
    Update;
  end;
end;

procedure TControl.SetBounds(const Value: TArea);
begin
  if Int64(FBounds)<>Int64(Value) then begin
    FBounds:=Value;
    Update;
  end;
end;

procedure TControl.SetCaption(const Value: string);
begin
  if FCaption<>Value then begin
    FCaption:=Value;
    Update;
  end;
end;

procedure TControl.SetControlState(const Value: TControlState);
begin
  FControlState:=Value;
end;

procedure TControl.SetDefault(const Value: Boolean);
var
  c:TControl;
begin
  c:=AbsoluteParent;
  if (c is TForm) and ((TForm(c).Default=Self) xor Value) then begin
    if Value then
      TForm(c).Default:=Self
    else
      TForm(c).Default:=nil;
  end;
  Invalidate;
end;

procedure TControl.SetEdgeBorders(const Value: TEdgeBorders);
begin
  if FEdgeBorders<>Value then begin
    FEdgeBorders:=Value;
    Update;
  end;
end;

procedure TControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled xor Value then begin
    FEnabled:=Value;
    Invalidate;
    Application.Update;
  end;
end;

procedure TControl.SetHeight(const Value: Smallint);
var
  a:TArea;
begin
  a:=FBounds;
  a.Height:=Value;
  SetBounds(a);
end;

procedure TControl.SetLeft(const Value: Smallint);
var
  a:TArea;
begin
  a:=FBounds;
  a.X:=Value;
  SetBounds(a);
end;

procedure TControl.SetParent(const Value: TControl);
begin
  if Value<>FParent then begin
    if Assigned(FParent) then
      FParent.RemoveControl(Self);
    FParent:=Value;
    if Assigned(FParent) then
      FParent.AddControl(Self);
    Application.Update;
  end;
end;

procedure TControl.SetTop(const Value: Smallint);
var
  a:TArea;
begin
  a:=FBounds;
  a.Y:=Value;
  SetBounds(a);
end;

procedure TControl.SetVisible(const Value: Boolean);
begin
  if FVisible xor Value then begin
    FVisible:=Value;
    if Assigned(FParent) then
      FParent.Update;
    Application.Update;
  end;
end;

procedure TControl.SetWidth(const Value: Smallint);
var
  a:TArea;
begin
  a:=FBounds;
  a.Width:=Value;
  SetBounds(a);
end;

procedure TControl.Update;
begin
  if not (csUpdate in FControlState) and not (csDestroying in ComponentState) then begin
    Include(FControlState,csUpdate);
    try
      UpdateBounds(FBounds);
      Integer(FClientArea.Origin):=0;
      FClientArea.Size:=FBounds.Size;
      UpdateClientArea(FClientArea);
      FScrollArea.Size:=FClientArea.Size;
      UpdateScrollArea(FScrollArea);
      UpdateControls;
      Invalidate;
    finally
      Exclude(FControlState,csUpdate);
    end;
  end;
end;

procedure TControl.UpdateBounds(var Bounds: TArea);
begin

end;

procedure TControl.UpdateClientArea(var ClientArea: TArea);
begin
  if FBorderStyle<>bsNone then begin
    if HasBorder(ebTop) then begin
      Inc(ClientArea.Y);
      Dec(ClientArea.Height);
    end;
    if HasBorder(ebBottom) then
      Dec(ClientArea.Height);
    if HasBorder(ebLeft) then begin
      Inc(ClientArea.X);
      Dec(ClientArea.Width);
    end;
    if HasBorder(ebRight) then
      Dec(ClientArea.Width);
  end;
end;

procedure TControl.UpdateControls;
var
  a:TAlign;
  b,d:Integer;
  c,e:Smallint;
  s:TArea;

  function GetAreaOffset(s:TArea;a:TAlign):Integer;
  begin
    case a of
      alTop:Result:=s.Y;
      alBottom:Result:=-(s.Y+s.Height-1);
      alLeft:Result:=s.Y;
      alRight:Result:=-(s.X+s.Width-1);
    else
      Result:=0;
    end;
  end;

  function CalcAreaAlign(var s:TArea;t:TArea;a:TAlign):TArea;
  begin
    case a of
      alTop:begin
        Result.Origin:=s.Origin;
        Result.Width:=s.Width;
        Result.Height:=t.Height;
        Inc(s.Y,t.Height);
        Dec(s.Height,t.Height);
      end;
      alBottom:begin
        Result.X:=s.X;
        Result.Y:=s.Y+s.Height-t.Height;
        Result.Width:=s.Width;
        Result.Height:=t.Height;
        Dec(s.Height,t.Height);
      end;
      alLeft:begin
        Result.Origin:=s.Origin;
        Result.Width:=t.Width;
        Result.Height:=s.Height;
        Inc(s.X,t.Width);
        Dec(s.Width,t.Width);
      end;
      alRight:begin
        Result.X:=s.X+s.Width-t.Width;
        Result.Y:=s.Y;
        Result.Width:=t.Width;
        Result.Height:=s.Height;
        Dec(s.Width,t.Width);
      end;
    end;
  end;

begin
  if not (csUpdateControls in FControlState) then begin
    Include(FControlState,csUpdateControls);
    try
      for b:=0 to FControls.Count-1 do
        TControl(FControls[b]).FAlignState:=False;
      s.X:=0;
      s.Y:=0;
      s.Size:=FScrollArea.Size;
      for a:=alTop to alRight do begin
        repeat
          d:=-1;
          c:=High(c);
          for b:=0 to FControls.Count-1 do
            with TControl(FControls[b]) do
              if FAlign=a then begin
                e:=GetAreaOffset(FBounds,a);
                if (e<c) and not FAlignState then begin
                  d:=b;
                  c:=e;
                end;
              end;
          if d>-1 then
            with TControl(FControls[d]) do begin
              FAlignState:=True;
              Bounds:=CalcAreaAlign(s,FBounds,a);
            end;
        until d=-1;
        for b:=0 to FControls.Count-1 do
          with TControl(FControls[b]) do
            if FAlign=alClient then
              Bounds:=s;
      end;
    finally
      Exclude(FControlState,csUpdateControls);
    end;
  end;
end;

procedure TControl.UpdateScrollArea(var ScrollArea: TArea);

  procedure AdjustToControls;
  var
    a:Integer;
  begin
    for a:=FControls.Count-1 downto 0 do
      if TControl(FControls[a]).FAlign=alNone then
        with TControl(FControls[a]).FBounds do begin
          if X+Width>ScrollArea.Width then
            ScrollArea.Width:=X+Width;
          if Y+Height>ScrollArea.Height then
            ScrollArea.Height:=Y+Height;
        end;
  end;

begin
  AdjustToControls;
  if ScrollArea.Width>FClientArea.Width then begin
    Dec(FClientArea.Height);
    Dec(ScrollArea.Height);
    Include(FControlState,csHorizontalScroll);
  end else
    Exclude(FControlState,csHorizontalScroll);
  if ScrollArea.Height>FClientArea.Height then begin
    Dec(FClientArea.Width);
    Dec(ScrollArea.Width);
    Include(FControlState,csVerticalScroll);
  end else
    Exclude(FControlState,csVerticalScroll);
  AdjustToControls;
  if (ScrollArea.Width>FClientArea.Width) and not (csHorizontalScroll in FControlState) then begin
    Dec(FClientArea.Height);
    Include(FControlState,csHorizontalScroll);
  end;
  if (ScrollArea.Height>FClientArea.Height) and not (csVerticalScroll in FControlState) then begin
    Dec(FClientArea.Width);
    Include(FControlState,csVerticalScroll);
  end;
  if not (csHorizontalScroll in FControlState) or (ScrollArea.X>0) then
    ScrollArea.X:=0;
  if -ScrollArea.X>ScrollArea.Width-FClientArea.Width then
    ScrollArea.X:=-ScrollArea.Width+FClientArea.Width;
  if not (csVerticalScroll in FControlState) or (ScrollArea.Y>0) then
    ScrollArea.Y:=0;
  if -ScrollArea.Y>ScrollArea.Height-FClientArea.Height then
    ScrollArea.Y:=-ScrollArea.Height+FClientArea.Height;
end;

end.
