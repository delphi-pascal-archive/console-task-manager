unit InfoFormUnit;

interface

uses
  SysUtils,Windows,TLHelp32,psAPI,Classes,ConsoleControls,ConsoleForms,ConsoleLists,ConsoleButtons;

type
  TInfoForm=class(TForm)
    ListBoxInfo:TListBox;
    ButtonOk:TBitBtn;
  public
    constructor Create(AOwner:TComponent);override;

    procedure Execute(ProcessEntry:TProcessEntry32);
  end;

var
  InfoForm:TInfoForm;

implementation

{ TInfoForm }

constructor TInfoForm.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle:=bsDialog;
  Width:=46;
  Height:=25;
  ListBoxInfo:=TListBox.Create(Self);
  with ListBoxInfo do begin
    Parent:=Self;
    Align:=alTop;
    Height:=Self.ClientHeight-3;
    EdgeBorders:=[ebBottom];
    AutoScroll:=True;
  end;
  ButtonOk:=TBitBtn.Create(Self);
  with ButtonOk do begin
    Parent:=Self;
    Kind:=bkOk;
    Autosize:=True;
    Top:=Self.ClientHeight-Height;
    Left:=(Self.ClientWidth-Width) div 2;
  end;
end;

procedure TInfoForm.Execute(ProcessEntry: TProcessEntry32);
var
  h:THandle;
  n:Cardinal;
  s:string;
  t:array[0..MAX_PATH] of Char;
  tCreation,tExit,tKernel,tUser:TFileTime;
  PMC:TProcessMemoryCounters;

  function FileTimeToStr(t:TFileTime;MS:Boolean):string;
  var
    i:Int64;
  begin
    i:=Int64(t) div 10000;
    if MS then
      Result:=Format('%.2dh%.2dm%.2ds%.3dms',[(i div 3600000) mod 60,(i div 60000) mod 60,(i div 1000) mod 60,i mod 1000])
    else
      Result:=Format('%.2dh%.2dm%.2ds',[(i div 3600000) mod 60,(i div 60000) mod 60,(i div 1000) mod 60]);
  end;

  function FileTimeToDuration(t:TFileTime;MS:Boolean):string;
  var
    tNow:TFileTime;
  begin
    GetSystemTimeAsFileTime(tNow);
    Result:=FileTimeToStr(TFileTime(Int64(tNow)-Int64(t)),MS);
  end;

  function IntToMemory(i:Integer):string;
  var
    a:Integer;
  begin
    Result:=' Ko';
    i:=i div 1024;
    for a:=0 to 2 do begin
      case i of
        0:Result:='   '+Result;
        1..9:Result:='  '+IntToStr(i)+Result;
        10..99:Result:=' '+IntToStr(i)+Result;
        100..999:Result:=IntToStr(i)+Result;
      else
        Result:=Format('%.3d',[i mod 1000])+Result;
      end;
      i:=i div 1000;
      if a<2 then
        Result:=' '+Result;
    end;
  end;
  
begin
  Caption:='Information for '+ProcessEntry.szExeFile;
  h:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,ProcessEntry.th32ProcessID);
  if h=0 then
    h:=OpenProcess(PROCESS_QUERY_INFORMATION,False,ProcessEntry.th32ProcessID);
  if h=0 then
    RaiseLastOSError;
  try
    ListBoxInfo.Items.BeginUpdate;
    try
      ListBoxInfo.Items.Clear;
      ListBoxInfo.Items.Add('[Basic information]');
      ListBoxInfo.Items.Add('  Identifier       : '+IntToStr(ProcessEntry.th32ProcessID)+' (0x'+IntToHex(ProcessEntry.th32ProcessID,8)+')');
      ListBoxInfo.Items.Add('  Parent process   : '+IntToStr(ProcessEntry.th32ParentProcessID)+' (0x'+IntToHex(ProcessEntry.th32ParentProcessID,8)+')');
      ListBoxInfo.Items.Add('  Executable name  : '+ProcessEntry.szExeFile);
      s:='  Executable path  : ';
      if GetModuleFileNameEx(h,0,t,Length(t))<>0 then
        s:=s+t
      else
        s:=s+'N/A';
      ListBoxInfo.Items.Add(s);
      s:='  System version   : ';
      n:=GetProcessVersion(ProcessEntry.th32ProcessID);
      if n<>0 then
        s:=s+IntToStr(n div $10000)+'.'+IntToStr(n and $FFFF)
      else
        s:=s+'N/A';
      ListBoxInfo.Items.Add(s);
      ListBoxInfo.Items.Add('');
      ListBoxInfo.Items.Add('[Resources]');
      s:=IntToStr(ProcessEntry.cntThreads);
      ListBoxInfo.Items.Add(Copy('  Threads          :                      ',1,35-Length(s))+s);
      if GetProcessTimes(h,tCreation,tExit,tKernel,tUser) then begin
        ListBoxInfo.Items.Add('  Running time     : '+FileTimeToDuration(tCreation,False));
        ListBoxInfo.Items.Add('  Kernel time      : '+FileTimeToStr(tKernel,True));
        ListBoxInfo.Items.Add('  User time        : '+FileTimeToStr(tUser,True));
      end else
        ListBoxInfo.Items.Add('  Time information :            N/A');
      ListBoxInfo.Items.Add('');
      ListBoxInfo.Items.Add('[Memory]');
      if GetProcessMemoryInfo(h,@PMC,SizeOf(PMC)) then begin
        s:=IntToStr(PMC.PageFaultCount);
        ListBoxInfo.Items.Add(Copy('  Page fault count :               ',1,35-Length(s))+s);
        ListBoxInfo.Items.Add('  Working set size : '+IntToMemory(PMC.WorkingSetSize));
        ListBoxInfo.Items.Add('  Peak             : '+IntToMemory(PMC.PeakWorkingSetSize));
        ListBoxInfo.Items.Add('  Paged pool usage : '+IntToMemory(PMC.QuotaPagedPoolUsage));
        ListBoxInfo.Items.Add('  Peak             : '+IntToMemory(PMC.QuotaPeakPagedPoolUsage));
        ListBoxInfo.Items.Add('  Non paged pool   : '+IntToMemory(PMC.QuotaNonPagedPoolUsage));
        ListBoxInfo.Items.Add('  Peak             : '+IntToMemory(PMC.QuotaPeakNonPagedPoolUsage));
        ListBoxInfo.Items.Add('  Page file usage  : '+IntToMemory(PMC.PagefileUsage));
        ListBoxInfo.Items.Add('  Peak             : '+IntToMemory(PMC.PeakPagefileUsage));
      end else
        ListBoxInfo.Items.Add('  Memory usage     :            N/A');
    finally
      ListBoxInfo.Items.EndUpdate;
    end;
  finally
    CloseHandle(h);
  end;
  Center;
  ShowModal;
end;

end.

