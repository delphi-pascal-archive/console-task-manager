unit AffinityFormUnit;

interface

uses
  SysUtils,Windows,TLHelp32,Classes,ConsoleControls,ConsoleForms,ConsoleLists,ConsoleButtons;

type
  TAffinityForm=class(TForm)
    ListBoxAffinity:TListBox;
    ButtonOk,ButtonCancel:TBitBtn;

    procedure ListBoxAffinitySelectionChanged(Sender:TObject);
  public
    constructor Create(AOwner:TComponent);override;

    procedure Execute(ProcessEntry:TProcessEntry32);
  end;

var
  AffinityForm:TAffinityForm;

implementation

{ TAffinityForm }

constructor TAffinityForm.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle:=bsDialog;
  Width:=40;
  Height:=20;
  ListBoxAffinity:=TListBox.Create(Self);
  with ListBoxAffinity do begin
    Parent:=Self;
    Align:=alTop;
    Height:=Self.ClientHeight-3;
    EdgeBorders:=[ebBottom];
    MultiSelect:=True;
    DefaultToggle:=True;
    OnSelectionChanged:=ListBoxAffinitySelectionChanged;
  end;
  ButtonOk:=TBitBtn.Create(Self);
  with ButtonOk do begin
    Parent:=Self;
    Kind:=bkOk;
    Autosize:=True;
    Top:=Self.ClientHeight-Height;
    Left:=Self.ClientWidth div 2-Width-3;
  end;
  ButtonCancel:=TBitBtn.Create(Self);
  with ButtonCancel do begin
    Parent:=Self;
    Kind:=bkCancel;
    Autosize:=True;
    Top:=Self.ClientHeight-Height;
    Left:=Self.ClientWidth div 2-2;
  end;
end;

procedure TAffinityForm.Execute(ProcessEntry: TProcessEntry32);
var
  a,b:Integer;
  h:THandle;
  mProcess,mSystem:Cardinal;
begin
  Center;
  Caption:='CPU affinity for '+ProcessEntry.szExeFile;
  h:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_SET_INFORMATION,False,ProcessEntry.th32ProcessID);
  if h=0 then
    RaiseLastOSError;
  try
    if not GetProcessAffinityMask(h,mProcess,mSystem) then
      RaiseLastOSError;
    ListBoxAffinity.Items.Clear;
    for a:=0 to 31 do
      if (mSystem and (1 shl a))<>0 then
        ListBoxAffinity.Items.Add('CPU '+IntToStr(a));
    b:=0;
    for a:=0 to 31 do
      if (mSystem and (1 shl a))<>0 then begin
        if (mProcess and (1 shl a))<>0 then
          ListBoxAffinity.Selected[b]:=True;
        Inc(b);
      end;
    ListBoxAffinitySelectionChanged(nil);
    if ShowModal=mrOk then begin
      mProcess:=0;
      b:=0;
      for a:=0 to 31 do
        if (mSystem and (1 shl a))<>0 then begin
          if ListBoxAffinity.Selected[b] then
            mProcess:=mProcess or (1 shl a);
          Inc(b);
        end;
      if not SetProcessAffinityMask(h,mProcess) then
        RaiseLastOSError;
    end;
  finally
    CloseHandle(h);
  end;
end;

procedure TAffinityForm.ListBoxAffinitySelectionChanged(Sender: TObject);
var
  a:Integer;
begin
  for a:=ListBoxAffinity.Items.Count-1 downto 0 do
    if ListBoxAffinity.Selected[a] then begin
      ButtonOk.Enabled:=True;
      Exit;
    end;
  ButtonOk.Enabled:=False;
end;

end.

