program taskmgr;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  Classes,
  ConsoleForms,
  MainFormUnit in 'MainFormUnit.pas',
  InfoFormUnit in 'InfoFormUnit.pas',
  PriorityFormUnit in 'PriorityFormUnit.pas',
  AffinityFormUnit in 'AffinityFormUnit.pas';

begin
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TPriorityForm, PriorityForm);
  Application.CreateForm(TAffinityForm, AffinityForm);
  Application.CreateForm(TInfoForm, InfoForm);
  Application.Run;
end.
