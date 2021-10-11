unit ConsoleDialogs;

interface

uses
  SysUtils,Windows,Classes,ConsoleGraphics,ConsoleControls,ConsoleForms,ConsoleButtons;

type
  TMsgDlgType=(mtWarning,mtError,mtInformation,mtConfirmation,mtCustom);
  TMsgDlgButtons=set of TModalResult;

const
  mrYesNo:TMsgDlgButtons=[mrYes,mrNo];
  mrYesNoCancel:TMsgDlgButtons=[mrYes,mrNo,mrCancel];
  mrYesAllNoAllCancel:TMsgDlgButtons=[mrYes,mrYesToAll,mrNo,mrNoToAll,mrCancel];
  mrOKCancel:TMsgDlgButtons=[mrOK,mrCancel];
  mrAbortRetryIgnore:TMsgDlgButtons=[mrAbort,mrRetry,mrIgnore];
  mrAbortIgnore:TMsgDlgButtons=[mrAbort,mrIgnore];

function MessageDlg(const Msg:string;DlgType:TMsgDlgType;Buttons:TMsgDlgButtons;DefaultBtn:TModalResult=mrNone):TModalResult;overload;
function MessageDlg(const Caption,Msg:string;DlgType:TMsgDlgType;Buttons:TMsgDlgButtons;DefaultBtn:TModalResult=mrNone):TModalResult;overload;
function MessageDlg(const Caption,Msg:string;DlgType:TMsgDlgType;Buttons:TMsgDlgButtons;X,Y:Integer;DefaultBtn:TModalResult=mrNone):TModalResult;overload;

implementation

type
  TDialogForm=class(TForm)
  private
    FText:TStringList;
  protected
    procedure PaintClient(Canvas:TCanvas);override;
  public
    constructor Create(AOwner:TComponent);override;

    destructor Destroy;override;
  end;

{ TDialogForm }

constructor TDialogForm.Create(AOwner: TComponent);
begin
  FText:=TStringList.Create;
  inherited;
end;

destructor TDialogForm.Destroy;
begin
  inherited;
  FreeAndNil(FText);
end;

procedure TDialogForm.PaintClient(Canvas: TCanvas);
var
  a:Integer;
begin
  inherited;
  for a:=FText.Count-1 downto 0 do
    Canvas.TextOut(0,a,FText[a]);
end;

function MessageDlg(const Msg:string;DlgType:TMsgDlgType;Buttons:TMsgDlgButtons;DefaultBtn:TModalResult=mrNone):TModalResult;
const
  T:array[mtWarning..mtCustom] of string=(
    'Warning',
    'Error',
    'Information',
    'Confirmation',
    ''
  );
begin
  Result:=MessageDlg(T[DlgType],Msg,DlgType,Buttons,-1,-1,DefaultBtn);
end;

function MessageDlg(const Caption,Msg:string;DlgType:TMsgDlgType;Buttons:TMsgDlgButtons;DefaultBtn:TModalResult=mrNone):TModalResult;
begin
  Result:=MessageDlg(Caption,Msg,DlgType,Buttons,-1,-1,DefaultBtn);
end;

function MessageDlg(const Caption,Msg:string;DlgType:TMsgDlgType;Buttons:TMsgDlgButtons;X,Y:Integer;DefaultBtn:TModalResult=mrNone):TModalResult;
var
  f:TDialogForm;
  r:TModalResult;
  a,b,c,l:Integer;
  p:PChar;
  s:TCoord;
begin
  CharToOem(PChar(Msg),PChar(Msg));
  Application.CreateForm(TDialogForm,f);
  try
    f.BorderStyle:=bsDialog;
    f.Caption:=Caption;
    s:=Application.ScreenSize;
    s.X:=(2*s.X) div 3;
    p:=PChar(Msg);
    l:=Length(Msg);
    a:=0;
    b:=1;
    c:=-1;
    while a<=l do begin
      case (p+a)^ of
        #9,' ':begin
          c:=a;
          while (p+c)^ in [#9,' '] do
            Inc(c);
          a:=c;
          Continue;
        end;
        #10,#13:begin
          f.FText.Add(Copy(Msg,b,a-b+1));
          Inc(a);
          b:=a+1;
          c:=-1;
          Continue;
        end;
      end;
      if (a-b>s.X) or ((p+a)^=#0) then begin
        if (c=-1) or ((p+a)^=#0) then
          c:=a;
        f.FText.Add(Copy(Msg,b,c-b+1));
        b:=c+1;
        c:=-1;
      end;
      Inc(a);
    end;
    f.Height:=8+f.FText.Count;
    if Length(Caption)>s.X then
      c:=s.X-2
    else
      c:=6+Length(Caption);
    for a:=f.FText.Count-1 downto 0 do begin
      b:=Length(f.FText[a]);
      if b>c then
        c:=b;
    end;
    b:=0;
    for r:=mrOk to mrAll do
      if r in Buttons then
        with TBitBtn.Create(f) do begin
          Parent:=f;
          Kind:=TBitBtnKind(r);
          Autosize:=True;
          Top:=f.ClientHeight-Height;
          b:=b+Width;
        end;
    Inc(b,f.ControlCount-1);
    if b>c then begin
      if b>s.X then
        c:=s.X-2
      else
        c:=b;
    end;
    f.Width:=c+2;
    c:=(f.ClientWidth-b) div 2;
    if c<0 then
      c:=0;
    for a:=0 to f.ControlCount-1 do
      with f.Control[a] as TBitBtn do begin
        Left:=c;
        Inc(c,Width+1);
        if ModalResult=DefaultBtn then
          Default:=True;
      end;
    f.Center;
    Result:=f.ShowModal;
  finally
    f.Destroy;
  end;
end;

end.

