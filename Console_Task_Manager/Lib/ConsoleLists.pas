unit ConsoleLists;

interface

uses
  Windows,SysUtils,Classes,ConsoleUtils,ConsoleMessages,ConsoleGraphics,ConsoleControls;

type
  TBooleans=array[0..$FFFF] of Boolean;
  PBooleans=^TBooleans;

  TItemState=set of (isSelected,isFocus);

  TCustomListControl=class(TControl)
  private
    FItemIndex:Integer;
    FMultiSelect,FDefaultToggle:Boolean;
    FSelection:PBooleans;
    FSelectionLength:Integer;
    FOnSelectionChanged: TNotifyEvent;

    procedure ReallocSelection;

    function GetSelected(const Index: Integer): Boolean;
    procedure SetSelected(const Index: Integer; const Value: Boolean);

    procedure SetItemIndex(Value:Integer);
    procedure SetMultiSelect(const Value: Boolean);
  protected
    function GetItemCount:Integer;virtual;abstract;
    procedure ScrollToIndex;virtual;abstract;
    function IndexFromPoint(X,Y:Integer):Integer;virtual;abstract;

    procedure ItemsChanged;virtual;
    procedure SelectionChanged;virtual;

    property MultiSelect:Boolean read FMultiSelect write SetMultiSelect;
    property DefaultToggle:Boolean read FDefaultToggle write FDefaultToggle;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;

    procedure KeyDown(var Key:Word;Shift:TShiftState);override;

    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;

    procedure ClearSelection;
    procedure SelectAll;

    property Selected[const Index:Integer]:Boolean read GetSelected write SetSelected;

    property ItemCount:Integer read GetItemCount;
    property ItemIndex:Integer read FItemIndex write SetItemIndex;

    property EdgeBorders;

    property OnSelectionChanged:TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

  TVerticalListControl=class(TCustomListControl)
  private
  protected
    procedure SetControlState(const Value:TControlState);override;

    procedure PaintClient(Canvas:TCanvas);override;
    procedure PaintItem(Canvas:TCanvas;Index:Integer;State:TItemState);virtual;abstract;

    procedure UpdateScrollArea(var ScrollArea:TArea);override;

    function IndexFromPoint(X,Y:Integer):Integer;override;

    procedure ScrollToIndex;override;

    procedure ItemsChanged;override;
    procedure SelectionChanged;override;
  public
  end;

  TStringListControl=class(TVerticalListControl)
  private
    FItems:TStringList;

    procedure _Change(Sender:TObject);
    function GetItems: TStrings;
  protected
    function GetItemCount:Integer;override;
  public
    constructor Create(AOwner:TComponent);override;

    destructor Destroy;override;

    property Items:TStrings read GetItems;
  end;

  TListBox=class(TStringListControl)
  private
    FAutoScroll: Boolean;

    procedure SetAutoScroll(const Value: Boolean);
  protected
    procedure UpdateScrollArea(var ScrollArea:TArea);override;

    procedure PaintItem(Canvas:TCanvas;Index:Integer;State:TItemState);override;
  public
    property AutoScroll:Boolean read FAutoScroll write SetAutoScroll;

    property MultiSelect;
    property DefaultToggle;
  end;

  TListView=class;

  TListColumn=class(TCollectionItem)
  private
    FCaption:string;
    FAlignment:TAlignment;
    FWidth,FActualWidth:Integer;
    FAutoSize,FAutoExpand:Boolean;

    procedure SetCaption(const Value: string);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetWidth(const Value: Integer);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetAutoExpand(const Value: Boolean);
  protected
    procedure UpdateOwner;

    property ActualWidth:Integer read FActualWidth;
  public
    constructor Create(Collection:TCollection);override;

    property Caption:string read FCaption write SetCaption;
    property Alignment:TAlignment read FAlignment write SetAlignment;
    property Width:Integer read FWidth write SetWidth;
    property AutoSize:Boolean read FAutoSize write SetAutoSize;
    property AutoExpand:Boolean read FAutoExpand write SetAutoExpand;
  end;

  TListColumns=class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TListColumn;
  protected
    procedure Update(Item:TCollectionItem);override;
  public
    constructor Create(AOwner:TListView);

    property Items[const Index:Integer]:TListColumn read GetItems;default;
    function Add:TListColumn;
  end;

  TListItem=class(TCollectionItem)
  private
    FCaption:string;
    FSubItems:TStringList;
    FData: Pointer;

    procedure _SubItemsChange(Sender:TObject);

    procedure SetCaption(const Value: string);

    function GetSelected: Boolean;
    procedure SetSelected(const Value: Boolean);

    function GetSubItems: TStrings;
    function GetSubItemText(const Index: Integer): string;
  protected
    procedure UpdateOwner;

    property SubItemText[const Index:Integer]:string read GetSubItemText;
  public
    constructor Create(Collection:TCollection);override;
    destructor Destroy;override;

    property Caption:string read FCaption write SetCaption;
    property SubItems:TStrings read GetSubItems;

    property Selected:Boolean read GetSelected write SetSelected;

    property Data:Pointer read FData write FData;
  end;

  TListItems=class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TListItem;
  protected
    procedure Update(Item:TCollectionItem);override;
  public
    constructor Create(AOwner:TListView);

    property Items[const Index:Integer]:TListItem read GetItems;default;
    function Add:TListItem;
  end;

  TListView=class(TVerticalListControl)
  private
    FColumns:TListColumns;
    FItems:TListItems;
  protected
    procedure PaintBorder(Canvas:TCanvas);override;
    procedure PaintClient(Canvas:TCanvas);override;
    procedure PaintItem(Canvas:TCanvas;_Index:Integer;State:TItemState);override;

    procedure UpdateClientArea(var ClientArea:TArea);override;
    procedure UpdateScrollArea(var ScrollArea:TArea);override;

    function GetItemCount:Integer;override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;

    property MultiSelect;

    property Columns:TListColumns read FColumns;
    property Items:TListItems read FItems;
  end;


implementation

{ TCustomListControl }

procedure TCustomListControl.ClearSelection;
var
  a:Integer;
begin
  if FMultiSelect then
    for a:=0 to FSelectionLength-1 do
      FSelection[a]:=False
  else
    FItemIndex:=-1;
  SelectionChanged;
end;

constructor TCustomListControl.Create(AOwner: TComponent);
begin
  inherited;
  FItemIndex:=-1;
  ControlStyle:=ControlStyle+[csArrowStop,csFocusable];
end;

destructor TCustomListControl.Destroy;
begin
  inherited;
  FMultiSelect:=False;
  ReallocSelection;
end;

function TCustomListControl.GetSelected(const Index: Integer): Boolean;
begin
  if FMultiSelect then
    Result:=(Index>=0) and (Index<GetItemCount) and FSelection[Index]
  else
    Result:=Index=FItemIndex;
end;

procedure TCustomListControl.ItemsChanged;
begin
  ReallocSelection;
  //ClearSelection;
  if FItemIndex>=GetItemCount then
    FItemIndex:=GetItemCount-1;
  Invalidate;
end;

procedure TCustomListControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Shift=[] then begin
    case Key of
      VK_LEFT,VK_UP:if FItemIndex>0 then SetItemIndex(FItemIndex-1) else SetItemIndex(0);
      VK_RIGHT,VK_DOWN:if FItemIndex<GetItemCount-1 then SetItemIndex(FItemIndex+1) else SetItemIndex(GetItemCount-1);
      VK_PRIOR:if FItemIndex>=10 then SetItemIndex(FItemIndex-10) else SetItemIndex(0);
      VK_NEXT:if FItemIndex+10<GetItemCount then SetItemIndex(FItemIndex+10) else SetItemIndex(GetItemCount-1);
      VK_SPACE:Selected[FItemIndex]:=not FMultiSelect or not Selected[FItemIndex];
    end;
  end;
{  if Shift=[ssShift] then begin
    case Key of
      VK_LEFT,VK_UP:if FItemIndex>0 then SetItemIndex(FItemIndex-1) else SetItemIndex(0);
      VK_RIGHT,VK_DOWN:if FItemIndex<GetItemCount-1 then SetItemIndex(FItemIndex+1) else SetItemIndex(GetItemCount-1);
      VK_PRIOR:if FItemIndex>=10 then SetItemIndex(FItemIndex-10) else SetItemIndex(0);
      VK_NEXT:if FItemIndex+10<GetItemCount then SetItemIndex(FItemIndex+10) else SetItemIndex(GetItemCount-1);
    end;
  end;}
end;

procedure TCustomListControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i:Integer;
begin
  if Button=mbLeft then begin
    i:=IndexFromPoint(X,Y);
    SetItemIndex(i);
    if FMultiSelect then begin
      if Shift=[] then begin
        if FDefaultToggle then
          Selected[i]:=not Selected[i]
        else begin
          ClearSelection;
          Selected[i]:=True;
        end;
      end;
      if Shift=[ssShift] then begin

      end;
      if Shift=[ssCtrl] then begin
        Selected[IndexFromPoint(X,Y)]:=not Selected[IndexFromPoint(X,Y)];
      end;
    end;
  end;
end;

procedure TCustomListControl.ReallocSelection;
begin
  if FMultiSelect then begin
    if Assigned(FSelection) then begin
      if FSelectionLength<>GetItemCount then begin
        ReallocMem(FSelection,GetItemCount*SizeOf(Boolean));
        FSelectionLength:=GetItemCount;
        ClearSelection;
      end
    end else begin
      GetMem(FSelection,GetItemCount*SizeOf(Boolean));
      FSelectionLength:=GetItemCount;
      ClearSelection;
    end;
  end else begin
    if Assigned(FSelection) then begin
      FreeMem(FSelection);
      FSelection:=nil;
      ClearSelection;
    end;
  end;
end;

procedure TCustomListControl.SelectAll;
var
  a:Integer;
begin
  if FMultiSelect then
    for a:=0 to FSelectionLength-1 do
      FSelection[a]:=True;
  SelectionChanged;
end;

procedure TCustomListControl.SelectionChanged;
begin
  Invalidate;
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
end;

procedure TCustomListControl.SetItemIndex(Value: Integer);
begin
  if Value<-1 then
    Value:=-1;
  if Value>=GetItemCount then
    Value:=GetItemCount-1;
  if Value<>FItemIndex then begin
    FItemIndex:=Value;
    SelectionChanged;
    if FItemIndex>-1 then
      ScrollToIndex;
  end;
end;

procedure TCustomListControl.SetMultiSelect(const Value: Boolean);
begin
  if Value xor FMultiSelect then begin
    FMultiSelect:=Value;
    ReallocSelection;
  end;
end;

procedure TCustomListControl.SetSelected(const Index: Integer;
  const Value: Boolean);
begin
  if FMultiSelect then begin
    if (Index>=0) and (Index<GetItemCount) and (FSelection[Index] xor Value) then begin
      FSelection[Index]:=Value;
      SelectionChanged;
    end;
  end else
    SetItemIndex(Index);
end;

{ TVerticalListControl }

function TVerticalListControl.IndexFromPoint(X, Y: Integer): Integer;
begin
  Result:=Y;
end;

procedure TVerticalListControl.ItemsChanged;
begin
  inherited;
  Update;
end;

procedure TVerticalListControl.PaintClient(Canvas: TCanvas);
var
  a,b:Integer;
  C:array[0..7] of TColor;
const
  // []B, []T, [H]B, [H]T, [S]B, [S]T, [HS]B, [HS]T
  T:array[caFocused..caDisabled,0..7] of TThemeColor=(
    (tcButtonBack,tcButtonText,tcFocusHighlightBack,tcFocusHighlightText,tcFocusSelectionBack,tcFocusSelectionText,tcFocusHighlightSelectionBack,tcFocusHighlightSelectionText),
    (tcButtonBack,tcButtonText,tcHighlightBack,tcHighlightText,tcSelectionBack,tcSelectionText,tcHighlightSelectionBack,tcHighlightSelectionText),
    (tcInactiveBack,tcInactiveText,tcInactiveText,tcInactiveBack,tcInactiveBack,tcInactiveText,tcInactiveText,tcInactiveBack),
    (tcDisabledBack,tcDisabledText,tcDisabledText,tcDisabledBack,tcDisabledBack,tcDisabledText,tcDisabledText,tcDisabledBack)
  );
  U:array[0..3] of TItemState=(
    [],
    [isSelected],
    [isFocus],
    [isSelected,isFocus]
  );
begin
  for a:=Low(C) to High(C) do
    C[a]:=GTheme[T[ControlAspect,a]];
  with ScrollArea do begin
    Canvas.Brush.Style:=bsFill;
    Canvas.Brush.Color:=C[0];
    Canvas.FillRect(-X,-Y,ClientWidth,ClientHeight);
    for a:=-Y+ClientHeight-1 downto -Y do
      if (a>=0) and (a<GetItemCount) then begin
        b:=0;
        if a=ItemIndex then
          Inc(b,2);
        if Selected[a] then
          Inc(b);
        Canvas.Brush.Color:=C[2*b];
        Canvas.Pen.Color:=C[2*b+1];
        PaintItem(Canvas,a,U[b]);
      end;
  end;  
end;

procedure TVerticalListControl.ScrollToIndex;
begin
  with ScrollArea do begin
    if FItemIndex<-Y then begin
      ScrollArea:=Area(X,-FItemIndex,Width,Height);
      Invalidate;
    end;
    if FItemIndex>=-Y+ClientHeight then begin
      ScrollArea:=Area(X,-FItemIndex+ClientHeight-1,Width,Height);
      Invalidate;
    end;
  end;
end;

procedure TVerticalListControl.SelectionChanged;
begin
  inherited;
  Update;
end;

procedure TVerticalListControl.SetControlState(const Value: TControlState);
begin
  if (csFocus in Value) xor (csFocus in ControlState) then
    Invalidate;
  inherited;
end;

procedure TVerticalListControl.UpdateScrollArea(var ScrollArea: TArea);
begin
  if GetItemCount>ScrollArea.Height then
    ScrollArea.Height:=GetItemCount;
  inherited;
end;

{ TStringListControl }

procedure TStringListControl._Change(Sender: TObject);
begin
  ItemsChanged;
end;

constructor TStringListControl.Create(AOwner: TComponent);
begin
  FItems:=TStringList.Create;
  FItems.OnChange:=_Change;
  inherited;
end;

destructor TStringListControl.Destroy;
begin
//  FItems.Clear;
  inherited;
  FItems.OnChange:=nil;
  FreeAndNil(FItems);
end;

function TStringListControl.GetItemCount: Integer;
begin
  Result:=FItems.Count;
end;

function TStringListControl.GetItems: TStrings;
begin
  Result:=FItems;
end;

{ TListBox }

procedure TListBox.PaintItem(Canvas: TCanvas; Index: Integer;
  State: TItemState);
begin
  if isSelected in State then
    with ScrollArea do
      Canvas.FillRect(-X,Index,ClientWidth,1);
  Canvas.TextOut(0,Index,FItems[Index]);
end;

procedure TListBox.SetAutoScroll(const Value: Boolean);
begin
  if FAutoScroll xor Value then begin
    FAutoScroll:=Value;
    Update;
  end;
end;

procedure TListBox.UpdateScrollArea(var ScrollArea: TArea);
var
  a,b,c:Integer;
begin
  if FAutoScroll then begin
    b:=0;
    for a:=FItems.Count-1 downto 0 do begin
      c:=Length(FItems[a]);
      if c>b then
        b:=c;
    end;
    if b>ScrollArea.Width then begin
      if GetItemCount+1>ScrollArea.Height then begin
        ScrollArea.Height:=GetItemCount+1;
        ScrollArea.Width:=b+1;
      end else
        ScrollArea.Width:=b;
    end else begin
      if GetItemCount+1>ScrollArea.Height then begin
        ScrollArea.Height:=GetItemCount+1;
        if b+1>ScrollArea.Width then 
          ScrollArea.Width:=b+1;
      end;
    end;
    inherited;
  end else
    inherited;
end;

{ TListColumn }

constructor TListColumn.Create(Collection: TCollection);
begin
  FWidth:=8;
  inherited;
end;

procedure TListColumn.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  UpdateOwner;
end;

procedure TListColumn.SetAutoExpand(const Value: Boolean);
begin
  FAutoExpand := Value;
  UpdateOwner;
end;

procedure TListColumn.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  UpdateOwner;
end;

procedure TListColumn.SetCaption(const Value: string);
begin
  FCaption := Value;
  UpdateOwner;
end;

procedure TListColumn.SetWidth(const Value: Integer);
begin
  if Value>1 then
    FWidth:=Value
  else
    FWidth:=2;
  UpdateOwner;
end;

procedure TListColumn.UpdateOwner;
begin
  TListColumns(Collection).Changed;
end;

{ TListColumns }

function TListColumns.Add: TListColumn;
begin
  Result:=TListColumn(inherited Add);
end;

constructor TListColumns.Create(AOwner: TListView);
begin
  inherited Create(AOwner,TListColumn);
end;

function TListColumns.GetItems(const Index: Integer): TListColumn;
begin
  Result:=TListColumn(inherited Items[Index]);
end;

procedure TListColumns.Update(Item: TCollectionItem);
begin
  inherited;
  TListView(Owner).Update;
end;

{ TListItem }

constructor TListItem.Create(Collection: TCollection);
begin
  FSubItems:=TStringList.Create;
  FSubItems.OnChange:=_SubItemsChange;
  inherited;
end;

destructor TListItem.Destroy;
begin
  inherited;
  FSubItems.OnChange:=nil;
  FreeAndNil(FSubItems);
end;

function TListItem.GetSelected: Boolean;
begin
  Result:=TListView(TListItems(Collection).Owner).Selected[Index];
end;

function TListItem.GetSubItems: TStrings;
begin
  Result:=FSubItems;
end;

function TListItem.GetSubItemText(const Index: Integer): string;
begin
  if Index=0 then
    Result:=FCaption
  else begin
    if (Index>0) and (Index<=FSubItems.Count) then
      Result:=FSubItems[Index-1]
    else
      Result:='';
  end;
end;

procedure TListItem.SetCaption(const Value: string);
begin
  FCaption := Value;
  UpdateOwner;
end;

procedure TListItem.SetSelected(const Value: Boolean);
begin
  TListView(TListItems(Collection).Owner).Selected[Index]:=Value;
end;

procedure TListItem.UpdateOwner;
begin
  TListItems(Collection).Changed;
end;

procedure TListItem._SubItemsChange(Sender: TObject);
begin
  UpdateOwner;
end;

{ TListItems }

function TListItems.Add: TListItem;
begin
  Result:=TListItem(inherited Add);
end;

constructor TListItems.Create(AOwner: TListView);
begin
  inherited Create(AOwner,TListItem);
end;

function TListItems.GetItems(const Index: Integer): TListItem;
begin
  Result:=TListItem(inherited Items[Index]);
end;

procedure TListItems.Update(Item: TCollectionItem);
begin
  inherited;
  TListView(Owner).Update;
end;

{ TListView }

constructor TListView.Create(AOwner: TComponent);
begin
  FColumns:=TListColumns.Create(Self);
  FItems:=TListItems.Create(Self);
  inherited;
end;

destructor TListView.Destroy;
begin
  inherited;
  FreeAndNil(FItems);
  FreeAndNil(FColumns);
end;

function TListView.GetItemCount: Integer;
begin
  Result:=FItems.Count;
end;

procedure TListView.PaintBorder(Canvas: TCanvas);
var
  a,b,c:Integer;
begin
  inherited;
  b:=ScrollArea.X;
  c:=ClientOrigin.Y-2;
  Canvas.Pen.Style:=psSingle;
  for a:=0 to FColumns.Count-1 do
    with FColumns[a] do begin
      Canvas.DrawCaption(b,c,FActualWidth,Caption,Alignment);
      Inc(b,FActualWidth);
      if a<FColumns.Count then begin
        Canvas.VerticalLine(c-1,c+1,b);
        Inc(b);
      end;
    end;
  Canvas.HorizontalLine(0,ClientWidth,c+1);
end;

procedure TListView.PaintClient(Canvas: TCanvas);
var
  a,b:Integer;
const
  T:array[caFocused..caDisabled] of TThemeColor=(
    tcButtonText,tcButtonText,tcInactiveText,tcDisabledText
  );
begin
  inherited;
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Color:=GTheme[T[ControlAspect]];
  b:=0;
  for a:=0 to FColumns.Count-2 do
    with FColumns[a] do begin
      Inc(b,FActualWidth);
      Canvas.VerticalLine(0,ScrollArea.Height,b);
      Inc(b);
    end;
end;

procedure TListView.PaintItem(Canvas: TCanvas; _Index: Integer;
  State: TItemState);
var
  a,b:Integer;
begin
  if isSelected in State then
    Canvas.FillRect(-ScrollArea.X,_Index,ClientWidth,1);
  b:=0;
  for a:=0 to FColumns.Count-1 do
    with FColumns[a] do begin
      Canvas.DrawCaption(b,_Index,FActualWidth,FItems[_Index].SubItemText[a],Alignment);
      Inc(b,FActualWidth+1);
    end;
end;

procedure TListView.UpdateClientArea(var ClientArea: TArea);
begin
  inherited;
  if FColumns.Count>0 then begin
    Inc(ClientArea.Y,2);
    Dec(ClientArea.Height,2);
  end;
end;

procedure TListView.UpdateScrollArea(var ScrollArea: TArea);
var
  a,b,c,d,e:Integer;
begin
  d:=0;
  e:=0;
  for a:=0 to FColumns.Count-1 do begin
    with FColumns[a] do begin
      if FAutoSize then begin
        FActualWidth:=Length(Caption);
        if FActualWidth<1 then
          FActualWidth:=1;
        Inc(e,FWidth);
        if FAutoExpand then begin
          for b:=0 to FItems.Count-1 do begin
            c:=Length(FItems[b].SubItemText[a]);
            if c>FActualWidth then
              FActualWidth:=c;
          end;
        end;
      end else
        FActualWidth:=FWidth;
      Inc(d,FActualWidth);
    end;
    if a>0 then
      Inc(d);
  end;
  if (FItems.Count>ClientHeight) or (FItems.Count=ClientHeight) and (ScrollArea.Width<d) then
    Inc(d);
  c:=ScrollArea.Width-d;
  if ScrollArea.Width<d then
    ScrollArea.Width:=d;
  if c<0 then
    c:=0;
  for a:=0 to FColumns.Count-1 do 
    with FColumns[a] do
      if FAutoSize then begin
        b:=(c*FWidth) div e;
        Inc(FActualWidth,b);
        Dec(c,b);
        Dec(e,FWidth);
      end;
  inherited;
end;

end.

