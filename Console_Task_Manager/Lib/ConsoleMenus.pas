unit ConsoleMenus;

interface

uses
  SysUtils,Windows,Classes,ConsoleMessages,ConsoleControls,Contnrs;

type
  TMenu=class;
  TMenuItem=class;

  TMenuItem=class(TComponent)
  private
    FItems:TList;
    FVisible: Boolean;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TMenuItem;

    procedure SetVisible(const Value: Boolean);
  protected
    procedure Notification(AComponent:TComponent;Operation:TOperation);override;
  public
    constructor Create(AOwner:TComponent);override;

    destructor Destroy;override;

    property Count:Integer read GetCount;
    property Item[const Index:Integer]:TMenuItem read GetItem;

    property Visible:Boolean read FVisible write SetVisible;
  end;

  TMenu=class(TComponent)
  private
    FItems: TMenuItem;
  protected
  public
    constructor Create(AOwner:TComponent);override;

    property Items:TMenuItem read FItems;
  end;

  TPopupMenu=class(TMenu)

  end;

procedure ShowSystemMenu;

implementation

uses
  ConsoleForms;

type
  TMenuForm=class(TForm)
  private
//    FMenuItem:TMenuItem;

    procedure CMMouseClick(var Message:TCMMouseButton);message CM_MOUSE_CLICK;
  protected
    function HasBorder(Edge:TEdgeBorder):Boolean;override;
    function HasTitle:Boolean;override;
  public
  end;

procedure ShowSystemMenu;
var
  f:TMenuForm;
begin
  Application.CreateForm(TMenuForm,TForm(f));
  try
    f.ShowModal;
  finally
    f.Destroy;
  end;
end;

{ TMenuForm }

procedure TMenuForm.CMMouseClick(var Message: TCMMouseButton);
begin
  ModalResult:=mrOk;
end;

function TMenuForm.HasBorder(Edge: TEdgeBorder): Boolean;
begin
  Result:=True;
end;

function TMenuForm.HasTitle: Boolean;
begin
  Result:=False;
end;

{ TMenuItem }

constructor TMenuItem.Create(AOwner: TComponent);
begin
  FItems:=TList.Create;
  FVisible:=True;
  inherited;
end;

destructor TMenuItem.Destroy;
begin               
  inherited;
  FreeAndNil(FItems);
end;

function TMenuItem.GetCount: Integer;
begin
  Result:=FItems.Count;
end;

function TMenuItem.GetItem(const Index: Integer): TMenuItem;
begin
  Result:=TMenuItem(FItems[Index]);
end;

procedure TMenuItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then
    FItems.Remove(AComponent);
end;

procedure TMenuItem.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TMenu }

constructor TMenu.Create(AOwner: TComponent);
begin
  inherited;
  FItems:=TMenuItem.Create(Self);
end;

end.

