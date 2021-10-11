unit ConsoleButtons;

interface

uses
  SysUtils,Windows,Classes,ConsoleMessages,ConsoleUtils,ConsoleForms,ConsoleControls,ConsoleGraphics;

type
  TCustomButton=class(TControl)
  private
    FCancel: Boolean;
    FAutosize: Boolean;
    FDown: Boolean;

    procedure CMMouseDown(var Message:TCMMouseButton);message CM_MOUSE_DOWN;
    procedure CMMouseUp(var Message:TCMMouseButton);message CM_MOUSE_UP;
    procedure CMMouseMove(var Message:TCMMouseMove);message CM_MOUSE_MOVE;
    procedure CMClick(var Message:TCMMouseButton);message CM_MOUSE_CLICK;

    procedure SetCancel(const Value: Boolean);
    procedure SetAutosize(const Value: Boolean);
  protected
    procedure SetDown(const Value: Boolean);virtual;

    property Down:Boolean read FDown write SetDown;
  public
    procedure KeyDown(var Key:Word;Shift:TShiftState);override;
    procedure KeyUp(var Key:Word;Shift:TShiftState);override;

    function Action(ShiftState:TShiftState;Key:Word):Boolean;override;

    property Cancel:Boolean read FCancel write SetCancel;

    property Autosize:Boolean read FAutosize write SetAutosize;
  end;

  TBevelButton=class(TCustomButton)
  private
  protected
    procedure SetControlState(const Value:TControlState);override;

    procedure UpdateBounds(var Bounds:TArea);override;

    function GetBackgroundColor:TThemeColor;override;
    function GetBorderColor:TThemeColor;override;
    function GetForegroundColor:TThemeColor;override;

    procedure PaintClient(Canvas:TCanvas);override;
  public
    constructor Create(AOwner:TComponent);override;
  end;

  TButton=class(TBevelButton)
  public
    constructor Create(AOwner:TComponent);override;
  end;

  TBitBtnKind=(bkCustom,bkOk,bkYes,bkNo,bkYesToAll,bkNoToAll,bkRetry,bkAbort,bkCancel,bkIgnore,bkAll);

  TBitBtn=class(TBevelButton)
  private
    FKind: TBitBtnKind;
    FModalResult: TModalResult;
    
    procedure SetKind(const Value: TBitBtnKind);
    procedure SetModalResult(const Value: TModalResult);
  protected
  public
    constructor Create(AOwner:TComponent);override;

    procedure Click;override;

    property Kind:TBitBtnKind read FKind write SetKind;
    property ModalResult:TModalResult read FModalResult write SetModalResult;
  end;

  TCheckBox=class(TBevelButton)
  private
    FChecked: Boolean;
    procedure SetChecked(const Value: Boolean);
  protected
    procedure PaintClient(Canvas:TCanvas);override;
    procedure UpdateBounds(var Bounds:TArea);override;
  public
    constructor Create(AOwner:TComponent);override;

    procedure Click;override;

    property Checked:Boolean read FChecked write SetChecked;
  end;

implementation

{ TCustomButton }

function TCustomButton.Action(ShiftState: TShiftState; Key: Word): Boolean;
begin
  Result:=(ShiftState=[]) and (CaptionAction(Caption,Key) or Default and (Key=VK_RETURN) or FCancel and (Key=VK_ESCAPE));
  if Result then
    Click;
end;

procedure TCustomButton.CMClick(var Message: TCMMouseButton);
begin
  Click;
end;

procedure TCustomButton.CMMouseDown(var Message: TCMMouseButton);
begin
  with Message do
    if Button=mbLeft then
      Down:=True;
end;

procedure TCustomButton.CMMouseMove(var Message: TCMMouseMove);
begin
  with Message do
    if ssLeft in ShiftState then
      SetDown(CoordInArea(Position,Area(0,0,Width,Height)));
end;

procedure TCustomButton.CMMouseUp(var Message: TCMMouseButton);
begin
  SetDown(False);
end;

procedure TCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:Click;
    VK_SPACE:Down:=True;
    VK_ESCAPE:Down:=False;
  end;
end;

procedure TCustomButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE:if Down then Click;
  end;
end;

procedure TCustomButton.SetAutosize(const Value: Boolean);
begin
  if Value xor FAutosize then begin
    FAutosize := Value;
    if FAutoSize then
      Update;
  end;
end;

procedure TCustomButton.SetCancel(const Value: Boolean);
begin
  FCancel := Value;
  Invalidate;
end;

procedure TCustomButton.SetDown(const Value: Boolean);
begin
  if FDown xor Value then begin
    FDown := Value;
    Invalidate;
  end;
end;

{ TBevelButton }

constructor TBevelButton.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle:=bsSingle;
  Height:=3;
  Width:=5;
end;

function TBevelButton.GetBackgroundColor: TThemeColor;
begin
  if IsEnabled then begin
    if IsActive then begin
      if FDown then
        Result:=tcDownButtonBack
      else begin
        if IsFocused then
          Result:=tcFocusBack
        else
          if IsDefault then
            Result:=tcDefaultButtonBack
          else
            Result:=tcButtonBack;
      end;
    end else
      Result:=tcInactiveBack
  end else
    Result:=tcDisabledBack;
end;

function TBevelButton.GetBorderColor: TThemeColor;
begin
  if IsEnabled then begin
    if IsActive then begin
      if FDown then
        Result:=tcDownButtonBorder
      else begin
        if IsFocused then
          Result:=tcFocusBorder
        else begin
          if IsDefault then
            Result:=tcDefaultButtonBorder
          else
            Result:=tcButtonBorder;
        end;
      end;
    end else
      Result:=tcInactiveBorder
  end else
    Result:=tcDisabledBorder;
end;

function TBevelButton.GetForegroundColor: TThemeColor;
begin
  if IsEnabled then begin
    if IsActive then begin
      if FDown then
        Result:=tcDownButtonText
      else begin
        if IsFocused then
          Result:=tcFocusText
        else
          if IsDefault then
            Result:=tcDefaultButtonText
          else
            Result:=tcButtonText;
      end;
    end else
      Result:=tcInactiveText
  end else
    Result:=tcDisabledText;
end;

procedure TBevelButton.PaintClient(Canvas: TCanvas);
begin
  Canvas.Pen.Color:=GTheme[GetForegroundColor];
  Canvas.Brush.Color:=GTheme[GetBackgroundColor];
  Canvas.DrawCaption(0,0,ClientArea.Width,Caption);
end;

procedure TBevelButton.SetControlState(const Value: TControlState);
begin
  if (csFocus in Value) xor (csFocus in ControlState) then begin
    if (csFocus in Value) then
      BorderStyle:=bsDouble
    else
      BorderStyle:=bsSingle;
  end;
  inherited;
end;

procedure TBevelButton.UpdateBounds(var Bounds: TArea);
begin
  inherited;
  if FAutosize then
    with Bounds do begin
      Height:=3;
      Width:=2+CaptionLength(Caption);
    end;
end;

{ TButton }

constructor TButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:=ControlStyle+[csFocusable,csDefaultOverride];
end;

{ TBitBtn }

procedure TBitBtn.Click;
var
  c:TControl;
begin
  inherited;
  if FModalResult<>mrNone then begin
    c:=AbsoluteParent;
    if c is TForm then
      TForm(c).ModalResult:=FModalResult;
  end;
end;

constructor TBitBtn.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:=ControlStyle+[csFocusable,csDefaultOverride];
end;

procedure TBitBtn.SetKind(const Value: TBitBtnKind);
const
  Captions:array[bkOK..bkAll] of string=(
    '_Ok',
    '_Yes',
    '_No',
    'Yes to all',
    'No to all',
    '_Retry',
    'A_bort',
    '_Cancel',
    '_Ignore',
    '_All'
  );
begin
  if Value<>bkCustom then begin
    Default:=Value in [bkOk,bkYes];
    SetCancel(Value in [bkCancel]);
    Caption:=Captions[Value];
    FModalResult:=TModalResult(Value);
  end;
  FKind := Value;
end;

procedure TBitBtn.SetModalResult(const Value: TModalResult);
begin
  if FModalResult<>Value then begin
    FModalResult := Value;
    FKind:=bkCustom;
  end;
end;

{ TCheckBox }

procedure TCheckBox.Click;
begin
  Checked:=not Checked;
end;

constructor TCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle:=bsNone;
  ControlStyle:=ControlStyle+[csFocusable];
end;

procedure TCheckBox.PaintClient(Canvas: TCanvas);
begin
  if FChecked then
    Canvas.TextOut(0,0,'ž')
  else
    Canvas.TextOut(0,0,'o');
  Canvas.DrawCaption(2,0,Bounds.Width-2,Caption);
end;

procedure TCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked xor Value then begin
    FChecked := Value;
    Invalidate;
  end;
end;

procedure TCheckBox.UpdateBounds(var Bounds: TArea);
begin
  inherited;
  Height:=1;
  if FAutosize then
    Bounds.Width:=2+CaptionLength(Caption);
end;

end.

