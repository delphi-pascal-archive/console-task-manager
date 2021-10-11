unit PriorityFormUnit;

interface

uses
  SysUtils,Windows,TLHelp32,Classes,ConsoleControls,ConsoleForms,ConsoleLists,ConsoleButtons;

type
  TPriorityForm=class(TForm)
    ListBoxPriority:TListBox;
    ButtonOk,ButtonCancel:TBitBtn;

    procedure ListBoxPrioritySelectionChanged(Sender:TObject);
  public
    constructor Create(AOwner:TComponent);override;

    procedure Execute(ProcessEntry:TProcessEntry32);
  end;

var
  PriorityForm:TPriorityForm;

implementation

const
   ABOVE_NORMAL_PRIORITY_CLASS=$00008000;
   BELOW_NORMAL_PRIORITY_CLASS=$00004000;

{ TPriorityForm }

constructor TPriorityForm.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle:=bsDialog;
  Width:=40;
  Height:=14;
  ListBoxPriority:=TListBox.Create(Self);
  with ListBoxPriority do begin
    Parent:=Self;
    Align:=alTop;
    Height:=Self.ClientHeight-3;
    EdgeBorders:=[ebBottom];
    Items.Add('Low');
    Items.Add('Lower');
    Items.Add('Normal');
    Items.Add('Higher');
    Items.Add('Highest');
    Items.Add('Critical');
    OnSelectionChanged:=ListBoxPrioritySelectionChanged;
  end;
  ButtonOk:=TBitBtn.Create(Self);
  with ButtonOk do begin
    Parent:=Self;
    Kind:=bkOk;
    Autosize:=True;
    Top:=Self.ClientHeight-Height;
    Left:=Self.ClientWidth div 2-Width-1;
  end;
  ButtonCancel:=TBitBtn.Create(Self);
  with ButtonCancel do begin
    Parent:=Self;
    Kind:=bkCancel;
    Autosize:=True;
    Top:=Self.ClientHeight-Height;
    Left:=Self.ClientWidth div 2;
  end;
end;

procedure TPriorityForm.Execute(ProcessEntry: TProcessEntry32);
var
  h:THandle;
  a:Integer;
  b:Cardinal;
const
  T:array[0..5] of Cardinal=(
    IDLE_PRIORITY_CLASS,
    BELOW_NORMAL_PRIORITY_CLASS,
    NORMAL_PRIORITY_CLASS,
    ABOVE_NORMAL_PRIORITY_CLASS,
    HIGH_PRIORITY_CLASS,
    REALTIME_PRIORITY_CLASS
  );
begin
  Center;
  Caption:='Priority for '+ProcessEntry.szExeFile;
  h:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_SET_INFORMATION,False,ProcessEntry.th32ProcessID);
  if h=0 then
    RaiseLastOSError;
  try
    b:=GetPriorityClass(h);
    ListBoxPriority.ItemIndex:=-1;
    for a:=low(T) to High(T) do
      if T[a]=b then
        ListBoxPriority.ItemIndex:=a;
    ListBoxPrioritySelectionChanged(nil);
    if (ShowModal=mrOk) and not SetPriorityClass(h,T[ListBoxPriority.ItemIndex]) then
      RaiseLastOSError;
  finally
    CloseHandle(h);
  end;
end;

procedure TPriorityForm.ListBoxPrioritySelectionChanged(Sender: TObject);
begin
  ButtonOk.Enabled:=ListBoxPriority.ItemIndex>-1;
end;

end.

