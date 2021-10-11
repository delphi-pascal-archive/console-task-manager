unit MainFormUnit;

interface

uses
  SysUtils,Classes,Windows,TLHelp32,
  ConsoleControls,ConsoleLists,ConsoleButtons,ConsoleTimers,ConsoleForms,ConsoleDialogs,
  InfoFormUnit,PriorityFormUnit,AffinityFormUnit;

type
  TMainForm=class(TForm)
    ButtonInfo,ButtonTerminate,ButtonPriority,ButtonAffinity,ButtonExit:TButton;
    ListViewProcess:TListView;
    TimerUpdate:TTimer;

    procedure TimerUpdateTimer(Sender:TObject);
    procedure ListViewProcessSelectionChanged(Sender:TObject);
    procedure ListViewProcessDblClick(Sender:TObject);
    procedure ButtonInfoClick(Sender:TObject);
    procedure ButtonTerminateClick(Sender:TObject);
    procedure ButtonPriorityClick(Sender:TObject);
    procedure ButtonAffinityClick(Sender:TObject);
    procedure ButtonExitClick(Sender:TObject);
  public
    constructor Create(AOwner:TComponent);override;
  end;

var
  MainForm:TMainForm;

implementation

{ TMainForm }

procedure TMainForm.ButtonAffinityClick(Sender: TObject);
begin
  try
    with ListViewProcess.Items[ListViewProcess.ItemIndex] do
      AffinityForm.Execute(PProcessEntry32(Data)^);
  except
    on e:Exception do
      MessageDlg(e.Message,mtError,[mrOk]);
  end;
end;

procedure TMainForm.ButtonExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.ButtonInfoClick(Sender: TObject);
begin
  try
    with ListViewProcess.Items[ListViewProcess.ItemIndex] do
      InfoForm.Execute(PProcessEntry32(Data)^);
  except
    on e:Exception do
      MessageDlg(e.Message,mtError,[mrOk]);
  end;
end;

procedure TMainForm.ButtonPriorityClick(Sender: TObject);
begin
  try
    with ListViewProcess.Items[ListViewProcess.ItemIndex] do
      PriorityForm.Execute(PProcessEntry32(Data)^);
  except
    on e:Exception do
      MessageDlg(e.Message,mtError,[mrOk]);
  end;
end;

procedure TMainForm.ButtonTerminateClick(Sender: TObject);
var
  h:THandle;
begin
  try
    with ListViewProcess.Items[ListViewProcess.ItemIndex] do begin
      h:=OpenProcess(PROCESS_TERMINATE,False,PProcessEntry32(Data)^.th32ProcessID);
      if h=0 then
        RaiseLastOSError;
      try
        if MessageDlg('Terminate process "'+PProcessEntry32(Data)^.szExeFile+'"?',mtConfirmation,mrYesNo,mrNo)=mrYes then begin
          if not TerminateProcess(h,0) then
            RaiseLastOSError;
        end;
      finally
        CloseHandle(h);
      end;
    end;
  except
    on e:Exception do
      MessageDlg(e.Message,mtError,[mrOk]);
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  Caption:='Console TaskManager Version 1.0';
  WindowState:=wsMaximized;
  BorderIcons:=[biClose];
  ButtonInfo:=TButton.Create(Self);
  with ButtonInfo do begin
    Parent:=Self;
    Autosize:=True;
    Caption:='_Info';
    Top:=Self.ClientHeight-Height-1;
    Left:=1;
    OnClick:=ButtonInfoClick;
  end;
  ButtonTerminate:=TButton.Create(Self);
  with ButtonTerminate do begin
    Parent:=Self;
    Autosize:=True;
    Caption:='_Terminate';
    Top:=Self.ClientHeight-Height-1;
    Left:=ButtonInfo.Left+ButtonInfo.Width+1;
    OnClick:=ButtonTerminateClick;
  end;
  ButtonPriority:=TButton.Create(Self);
  with ButtonPriority do begin
    Parent:=Self;
    Autosize:=True;
    Caption:='_Priority';
    Top:=Self.ClientHeight-Height-1;
    Left:=ButtonTerminate.Left+ButtonTerminate.Width+1;
    OnClick:=ButtonPriorityClick;
  end;
  ButtonAffinity:=TButton.Create(Self);
  with ButtonAffinity do begin
    Parent:=Self;
    Autosize:=True;
    Caption:='_Affinity';
    Top:=Self.ClientHeight-Height-1;
    Left:=ButtonPriority.Left+ButtonPriority.Width+1;
    OnClick:=ButtonAffinityClick;
  end;
  ButtonExit:=TButton.Create(Self);
  with ButtonExit do begin
    Parent:=Self;
    Autosize:=True;
    Caption:='E_xit';
    Top:=Self.ClientHeight-Height-1;
    Left:=ButtonAffinity.Left+ButtonAffinity.Width+1;
    OnClick:=ButtonExitClick;
  end;
  ListViewProcess:=TListView.Create(Self);
  with ListViewProcess do begin
    Parent:=Self;
    Default:=True;
    Align:=alTop;
    Height:=Self.ClientHeight-4;
    EdgeBorders:=[ebBottom];
    OnSelectionChanged:=ListViewProcessSelectionChanged;
    OnDblClick:=ListViewProcessDblClick;
    with Columns.Add do begin
      Caption:='Image';
      AutoSize:=True;
      AutoExpand:=True;
    end;
    with Columns.Add do begin
      Caption:='Priority';
      AutoSize:=True;
      AutoExpand:=True;
      Alignment:=taCenter;
    end;
    with Columns.Add do begin
      Caption:='Threads';
      AutoSize:=True;
      AutoExpand:=True;
      Alignment:=taCenter;
    end;
    with Columns.Add do begin
      Caption:='Process ID';
      AutoSize:=True;
      AutoExpand:=True;
      Alignment:=taRightJustify;
    end;
    with Columns.Add do begin
      Caption:='Parent ID';
      AutoSize:=True;
      AutoExpand:=True;
      Alignment:=taRightJustify;
    end;
  end;
  TimerUpdate:=TTimer.Create(Self);
  with TimerUpdate do begin
    Enabled:=True;
    OnTimer:=TimerUpdateTimer;
    Interval:=1000;
  end;
  ListViewProcessSelectionChanged(nil);
end;

procedure TMainForm.ListViewProcessDblClick(Sender: TObject);
begin
  if ButtonInfo.Enabled then
    ButtonInfo.Click;
end;

procedure TMainForm.ListViewProcessSelectionChanged(Sender: TObject);
var
  t:Boolean;
begin
  t:=(ListViewProcess.ItemIndex>-1) and (PProcessEntry32(ListViewProcess.Items[ListViewProcess.ItemIndex].Data)^.th32ProcessID<>0);
  ButtonInfo.Enabled:=t;
  ButtonTerminate.Enabled:=t;
  ButtonPriority.Enabled:=t;
  ButtonAffinity.Enabled:=t;
end;

procedure TMainForm.TimerUpdateTimer(Sender: TObject);
var
  PE:TProcessEntry32;
  p:PProcessEntry32;
  h:THandle;
  a:Integer;
begin
  for a:=ListViewProcess.Items.Count-1 downto 0 do
    Dispose(PProcessEntry32(ListViewProcess.Items[a].Data));
  ZeroMemory(@PE,SizeOf(PE));
  PE.dwSize:=SizeOf(PE);
  ListViewProcess.Items.BeginUpdate;
  try
    ListViewProcess.Items.Clear;
    h:=CreateToolhelp32Snapshot(TH32CS_SNAPALL,0);
    if h=0 then
      RaiseLastOSError;
    try
      if Process32First(h,PE) then
        repeat
          New(p);
          p^:=PE;
          with ListViewProcess.Items.Add do begin
            Caption:=PE.szExeFile;
            SubItems.Add(IntToStr(PE.pcPriClassBase));
            SubItems.Add(IntToStr(PE.cntThreads));
            SubItems.Add(IntToStr(PE.th32ProcessID));
            SubItems.Add(IntToStr(PE.th32ParentProcessID));
            Data:=p;
          end;
        until not Process32Next(h,PE);
    finally
      CloseHandle(h);
    end;
  finally
    ListViewProcess.Items.EndUpdate;
  end;
  if ListViewProcess.ItemIndex=-1 then
    ListViewProcess.ItemIndex:=0;
end;

end.

