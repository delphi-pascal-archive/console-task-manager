unit ConsoleGraphics;

interface

uses
  Windows,SysUtils,Classes,ConsoleUtils;

type
  TCharSet=set of Char;

  TCharInfo=packed record
    case Integer of
      0:(UnicodeChar:WCHAR;Attributes:Word);
      1:(AsciiChar:CHAR;Dummy:Byte);
  end;

  TCharInfos=array[0..$FFFFF] of TCharInfo;
  PCharInfos=^TCharInfos;

  TColor=(clBlack,clWhite,clGray,clRed,clGreen,clBlue,clCyan,clMagenta,clYellow,
          clDarkGray,clDarkRed,clDarkGreen,clDarkBlue,clDarkCyan,clDarkMagenta,clDarkYellow);

  TBrushStyle=(bsFill,bsClear);
  TPenStyle=(psSingle,psDouble,psClear);

  TBrush=class
  private
    FColor: TColor;
    FStyle: TBrushStyle;
  public
    constructor Create;

    property Color:TColor read FColor write FColor;
    property Style:TBrushStyle read FStyle write FStyle;
  end;

  TPen=class
  private
    FColor: TColor;
    FStyle: TPenStyle;
  public
    constructor Create;

    property Color:TColor read FColor write FColor;
    property Style:TPenStyle read FStyle write FStyle;
  end;

  TClipStack=record
    Area:array[0..$FF] of TArea;
    Origin:array[0..$FF] of TCoord;
    Index:Integer;
  end;

  TCanvas=class
  private
    FBorderChars:array[0..255] of Char;
    FSize:TCoord;
    FData:PCharInfos;
    FClipStack:TClipStack;
    FBrush: TBrush;
    FPen: TPen;

    function GetCell(const X, Y: Smallint): TCharInfo;
    function GetAsciiChar(const X, Y: Smallint): Char;
    function GetAttributes(const X, Y: Smallint): Word;
    procedure SetCell(const X, Y: Smallint; const Value: TCharInfo);
    procedure SetAsciiChar(const X, Y: Smallint; const Value: Char);
    procedure SetAttributes(const X, Y: Smallint; const Value: Word);
  protected
    procedure Clear;

    function GetColorAttributes:Word;overload;
    function GetColorAttributes(const Background,Foreground:TColor):Word;overload;

    property Cell[const X,Y:Smallint]:TCharInfo read GetCell write SetCell;
    property AsciiChar[const X,Y:Smallint]:Char read GetAsciiChar write SetAsciiChar;
    property Attributes[const X,Y:Smallint]:Word read GetAttributes write SetAttributes;
  public
    constructor Create;

    procedure BeginPaint(ASize:TCoord);
    procedure EndPaint(Handle:THandle);

    procedure PushClipArea(Area:TArea;Origin:TCoord);overload;
    procedure PushClipArea(Area:TArea);overload;
    procedure PopClipArea;

    procedure ResetCurves;

    property Brush:TBrush read FBrush;
    property Pen:TPen read FPen;

    procedure HorizontalLine(X1,X2,Y:Smallint);
    procedure VerticalLine(Y1,Y2,X:Smallint);

    procedure DrawHorizontalScrollbar(X,Y,Width,Position,Max:Smallint);
    procedure DrawVerticalScrollbar(X,Y,Height,Position,Max:Smallint);

    procedure FillRect(X,Y,Width,Height:Smallint);overload;
    procedure FillRect(Area:TArea);overload;
    procedure Rectangle(X,Y,Width,Height:Smallint);overload;
    procedure Rectangle(Area:TArea);overload;

    procedure TextOut(const X,Y:Smallint;const Text:string);overload;
    procedure TextOut(Position:TCoord;const Text:string);overload;

    procedure DrawCaption(X,Y:Smallint;const Text:string;ShortcutBackground:TColor=clWhite;ShortcutForeground:TColor=clDarkRed);overload;
    procedure DrawCaption(X,Y,MaxWidth:Smallint;const Text:string;Alignment:TAlignment=taLeftJustify;ShortcutBackground:TColor=clWhite;ShortcutForeground:TColor=clDarkRed);overload;

    destructor Destroy;override;
  end;

  TThemeColor=(
    tcBackgroundBack,tcBackgroundFront,
    tcFocusBorder,tcFocusBack,tcFocusText,
    tcActiveBorder,tcActiveBack,tcActiveText,
//    tcActiveBorder,tcActiveBack,tcActiveText,
    tcInactiveBorder,tcInactiveBack,tcInactiveText,
    tcDisabledBorder,tcDisabledBack,tcDisabledText,

    tcButtonBorder,tcButtonBack,tcButtonText,
    tcDefaultButtonBorder,tcDefaultButtonBack,tcDefaultButtonText,
    tcDownButtonBorder,tcDownButtonBack,tcDownButtonText,

    tcFocusHighlightBack,tcFocusHighlightText,
    tcFocusSelectionBack,tcFocusSelectionText,
    tcFocusHighlightSelectionBack,tcFocusHighlightSelectionText,
    tcHighlightBack,tcHighlightText,
    tcSelectionBack,tcSelectionText,
    tcHighlightSelectionBack,tcHighlightSelectionText,

    tcShortcutBack,tcShortcutText
  );

  TTheme=class
  protected
    function GetColor(const Index: TThemeColor): TColor;virtual;abstract;
  public
    constructor Create;virtual;

    property Color[const Index:TThemeColor]:TColor read GetColor;default;
  end;

  TThemeClass=class of TTheme;

var
  GTheme:TTheme=nil;

procedure SetThemeClass(AClass:TThemeClass);

const
  GForegroundColors:array[clBlack..clDarkYellow] of Word=(
    FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE,
    FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY,
    0,
    FOREGROUND_RED or FOREGROUND_INTENSITY,
    FOREGROUND_GREEN or FOREGROUND_INTENSITY,
    FOREGROUND_BLUE or FOREGROUND_INTENSITY,
    FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY,
    FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_INTENSITY,
    FOREGROUND_RED or FOREGROUND_GREEN or FOREGROUND_INTENSITY,
    FOREGROUND_INTENSITY,
    FOREGROUND_RED,
    FOREGROUND_GREEN,
    FOREGROUND_BLUE,
    FOREGROUND_GREEN or FOREGROUND_BLUE,
    FOREGROUND_RED or FOREGROUND_BLUE,
    FOREGROUND_RED or FOREGROUND_GREEN
  );

  GBackgroundColors:array[clBlack..clDarkYellow] of Word=(
    BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE,
    BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY,
    0,
    BACKGROUND_RED or BACKGROUND_INTENSITY,
    BACKGROUND_GREEN or BACKGROUND_INTENSITY,
    BACKGROUND_BLUE or BACKGROUND_INTENSITY,
    BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY,
    BACKGROUND_RED or BACKGROUND_BLUE or BACKGROUND_INTENSITY,
    BACKGROUND_RED or BACKGROUND_GREEN or BACKGROUND_INTENSITY,
    BACKGROUND_INTENSITY,
    BACKGROUND_RED,
    BACKGROUND_GREEN,
    BACKGROUND_BLUE,
    BACKGROUND_GREEN or BACKGROUND_BLUE,
    BACKGROUND_RED or BACKGROUND_BLUE,
    BACKGROUND_RED or BACKGROUND_GREEN
  );

// ...  .@.  ...  .@.  ...  .@.  ...  .@.  ...  .@.  ...  .@.  ...  .@.  ...  .@.
// ...  .@.  .@@  .@@  .@.  .@.  .@@  .@@  @@.  @@.  @@@  @@@  @@.  @@.  @@@  @@@
// ...  ...  ...  ...  .@.  .@.  .@.  .@.  ...  ...  ...  ...  .@.  .@.  .@.  .@.

  GBorderChars:array[0..31] of Char=(
    ' ','³','Ä','À','³','³','Ú','Ã','Ä','Ù','Ä','Á','¿','´','Â','Å',
    ' ','º','Í','È','º','º','É','Ì','Í','¼','Í','Ê','»','¹','Ë','Î'
  );

// .^.  ...  ...  ...
// ...  ..>  ...  <..
// ...  ...  .v.  ...
//
// .^.  ...  .|.  ...
// .|.  -->  .|.  <--
// .|.  ...  .v.  ...

  GArrowsChars:array[0..1,0..3] of Char=(
    (#30,#16,#31,#17),
    (#24,#26,#25,#27)
  );

  GArrowUpDownChar:Char=#18;
  GArrowLeftRightChar:Char=#29;

  GEllipsisChar:Char='ù';

implementation

procedure SetThemeClass(AClass:TThemeClass);
begin
  FreeAndNil(GTheme);
  GTheme:=AClass.Create;
end;

function CharInfo(Attributes:Word;AsciiChar:Char):TCharInfo;
begin
  Result.AsciiChar:=AsciiChar;
  Result.Attributes:=Attributes;
end;

type
  TDefaultTheme=class(TTheme)
  protected
    function GetColor(const Index: TThemeColor): TColor;override;
  end;

{ TDefaultTheme }

function TDefaultTheme.GetColor(const Index: TThemeColor): TColor;
const
  T:array[tcBackgroundBack..tcShortcutText] of TColor=(
    clWhite,                //tcBackgroundBack
    clRed,                  //tcBackgroundFront
    clDarkGreen,            //tcFocusBorder
    clWhite,                //tcFocusBack
    clRed,                  //tcFocusText
    clCyan,                 //tcActiveBorder
    clBlue,                 //tcActiveBack
    clYellow,               //tcActiveText
    clDarkGray,             //tcInactiveBorder
    clDarkBlue,             //tcInactiveBack
    clDarkGray,             //tcInactiveText
    clGray,                 //tcDisabledBorder
    clDarkGray,             //tcDisabledBack
    clGray,                 //tcDisabledText
    clBlack,                //tcButtonBorder
    clGray,                 //tcButtonBack
    clBlack,                //tcButtonText
    clDarkGreen,            //tcDefaultButtonBorder
    clDarkYellow,           //tcDefaultButtonBack
    clBlack,                //tcDefaultButtonText
    clBlack,                //tcDownButtonBorder
    clDarkGreen,            //tcDownButtonBack
    clWhite,                //tcDownButtonText

    clDarkRed,              //tcFocusHighlightBack
    clWhite,                //tcFocusHighlightText
    clWhite,                //tcFocusSelectionBack
    clDarkYellow,           //tcFocusSelectionText
    clDarkRed,              //tcFocusHighlightSelectionBack
    clYellow,               //tcFocusHighlightSelectionText
    clDarkCyan,             //tcHighlightBack
    clWhite,                //tcHighlightText
    clWhite,                //tcSelectionBack
    clDarkGray,             //tcSelectionText
    clDarkCyan,             //tcHighlightSelectionBack
    clDarkMagenta,          //tcHighlightSelectionText

    clGreen,                //tcShortcutBack
    clRed                   //tcShortcutText
  );
begin
  Result:=T[Index];
end;

{ TBrush }

constructor TBrush.Create;
begin
  FColor:=clDarkBlue;//Black;
end;

{ TPen }

constructor TPen.Create;
begin
  FColor:=clYellow;
end;

{ TCanvas }

procedure TCanvas.BeginPaint(ASize: TCoord);
begin
  ReallocMem(FData,ASize.X*ASize.Y*SizeOf(TCharInfo));
  FSize:=ASize;
  Clear;
  FClipStack.Index:=0;
  FClipStack.Area[0].Origin:=FClipStack.Origin[0];
  FClipStack.Area[0].Size:=FSize;
  FClipStack.Origin[0].X:=0;
  FClipStack.Origin[0].Y:=0;
end;

procedure TCanvas.Clear;
var
  a:Integer;
  i:TCharInfo;
begin
  i.Attributes:=GetColorAttributes(clWhite,clRed);
  i.UnicodeChar:=' ';
  for a:=FSize.X*FSize.Y-1 downto 0 do
    FData[a]:=i;
end;

constructor TCanvas.Create;
var
  a:Integer;
begin
  ZeroMemory(@FBorderChars,SizeOf(FBorderChars));
  for a:=0 to 15 do begin
    FBorderChars[a]:=GBorderChars[a];
    FBorderChars[a shl 4]:=GBorderChars[a+16];
  end;
  FBrush:=TBrush.Create;
  FPen:=TPen.Create;
end;

destructor TCanvas.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FBrush);
  ReallocMem(FData,0);
  inherited;
end;

procedure TCanvas.DrawCaption(X, Y: Smallint; const Text: string;
  ShortcutBackground, ShortcutForeground: TColor);
begin
  DrawCaption(X,Y,$FFF,Text);
end;

procedure TCanvas.DrawCaption(X, Y, MaxWidth: Smallint;
  const Text: string; Alignment: TAlignment; ShortcutBackground,
  ShortcutForeground: TColor);
var
  a:Integer;
  p:PChar;
  t:Boolean;

  procedure Flush;
  begin
    if t then
      Cell[X+a,Y]:=CharInfo(GetColorAttributes(ShortcutBackground,ShortcutForeground),p^)
    else
      Cell[X+a,Y]:=CharInfo(GetColorAttributes,p^);
    t:=False;
    Inc(a);
  end;

begin
  X:=X+FClipStack.Origin[FClipStack.Index].X;
  Y:=Y+FClipStack.Origin[FClipStack.Index].Y;
  case Alignment of
    taLeftJustify:a:=0;
    taRightJustify:begin
      a:=MaxWidth-CaptionLength(Text);
      if a>=0 then begin
        X:=X+a;
        a:=0;
      end else
        a:=-a;
    end;
    taCenter:begin
      a:=MaxWidth-CaptionLength(Text);
      if a>=0 then begin
        X:=X+a div 2;
        a:=0;
      end else
        a:=0;
    end;
  end;
  p:=PChar(Text)+a;
  a:=0;
  while (p^<>#0) and (a<MaxWidth) do begin
    if p^='_' then
      t:=True
    else
      Flush;
    Inc(p);
  end;
end;

procedure TCanvas.DrawHorizontalScrollbar(X, Y, Width, Position,
  Max: Smallint);
var
  a,Z,L:Smallint;
  i:Word;
begin
  X:=X+FClipStack.Origin[FClipStack.Index].X;
  Y:=Y+FClipStack.Origin[FClipStack.Index].Y;
  if (Y<0) or (Y>=FClipStack.Area[FClipStack.Index].Y+FClipStack.Area[FClipStack.Index].Height) then
    Exit;
  IntersectRange(X,Width,FClipStack.Area[FClipStack.Index].X,FClipStack.Area[FClipStack.Index].Width,Z,L);
  i:=GetColorAttributes;
  for a:=0 to L-1 do
    with FData[Y*FSize.X+Z+a] do begin
      Attributes:=i;
      AsciiChar:='±';
      Dummy:=0;
    end;
  with FData[Y*FSize.X+Z] do begin
    Attributes:=i;
    AsciiChar:=GArrowsChars[0][3];
    Dummy:=0;
  end;
  with FData[Y*FSize.X+Z+L-1] do begin
    Attributes:=i;
    AsciiChar:=GArrowsChars[0][1];
    Dummy:=0;
  end;
  if Width>2 then
    with FData[Y*FSize.X+Z+1+((L-3)*Position) div Max] do begin
      Attributes:=i;
      AsciiChar:='²';//'Û';
      Dummy:=0;
    end;
end;

procedure TCanvas.DrawVerticalScrollbar(X, Y, Height, Position,
  Max: Smallint);
var
  a,Z,L:Smallint;
  i:Word;
begin
  X:=X+FClipStack.Origin[FClipStack.Index].X;
  Y:=Y+FClipStack.Origin[FClipStack.Index].Y;
  if (X<0) or (X>=FClipStack.Area[FClipStack.Index].X+FClipStack.Area[FClipStack.Index].Width) then
    Exit;
  IntersectRange(Y,Height,FClipStack.Area[FClipStack.Index].Y,FClipStack.Area[FClipStack.Index].Height,Z,L);
  i:=GetColorAttributes;
  for a:=0 to L-1 do
    with FData[(Z+a)*FSize.X+X] do begin
      Attributes:=i;
      AsciiChar:='±';    //'°';//
      Dummy:=0;
    end;
  with FData[Z*FSize.X+X] do begin
    Attributes:=i;
    AsciiChar:=GArrowsChars[0][0];
    Dummy:=0;
  end;
  with FData[(Z+L-1)*FSize.X+X] do begin
    Attributes:=i;
    AsciiChar:=GArrowsChars[0][2];
    Dummy:=0;
  end;
  if Height>2 then
    with FData[(Z+1+((L-3)*Position) div Max)*FSize.X+X] do begin
      Attributes:=i;
      AsciiChar:='²';//'Û';
      Dummy:=0;
    end;
end;

procedure TCanvas.EndPaint(Handle: THandle);
const
  s:TCoord=(X:0;Y:0);
var
  r:TSmallRect;
//  a:Integer;
begin
  r.Left:=0;
  r.Top:=0;
  r.Right:=FSize.X-1;
  r.Bottom:=FSize.Y-1;
  ResetCurves;
  if not WriteConsoleOutput(Handle,FData,FSize,s,r) then
    RaiseLastOSError;
//  for a:=0 to 255 do
//    Write(string(chr(a)));
end;

procedure TCanvas.FillRect(X, Y, Width, Height: Smallint);
begin
  FillRect(Area(X,Y,Width,Height));
end;

procedure TCanvas.FillRect(Area: TArea);
var
  a,b:Integer;
  i:Word;
begin
  if FBrush.FStyle<>bsClear then begin
    OffsetArea(Area,FClipStack.Origin[FClipStack.Index]);
    Area:=IntersectArea(Area,FClipStack.Area[FClipStack.Index]);
    i:=GetColorAttributes;
    for a:=0 to Area.Width-1 do
      for b:=0 to Area.Height-1 do
        with FData[(b+Area.Y)*FSize.X+a+Area.X] do begin
          Attributes:=i;
          Dummy:=0;
          AsciiChar:=' ';
        end;
  end;
end;

function TCanvas.GetAsciiChar(const X, Y: Smallint): Char;
begin
  if (X>=0) and (Y>=0) and (X<FSize.X) and (Y<FSize.Y) then
    Result:=FData[X+Y*FSize.X].AsciiChar
  else
    Result:=#0;
end;

function TCanvas.GetAttributes(const X, Y: Smallint): Word;
begin
  if (X>=0) and (Y>=0) and (X<FSize.X) and (Y<FSize.Y) then
    Result:=FData[X+Y*FSize.X].Attributes
  else
    Result:=0;
end;

function TCanvas.GetCell(const X, Y: Smallint): TCharInfo;
begin
  if (X>=0) and (Y>=0) and (X<FSize.X) and (Y<FSize.Y) then
    Result:=FData[X+Y*FSize.X]
  else
    Integer(Result):=0;
end;

function TCanvas.GetColorAttributes: Word;
begin
  Result:=GBackgroundColors[FBrush.FColor] or GForegroundColors[FPen.FColor];
end;

function TCanvas.GetColorAttributes(const Background,
  Foreground: TColor): Word;
begin
  Result:=GBackgroundColors[Background] or GForegroundColors[Foreground];
end;

procedure TCanvas.HorizontalLine(X1, X2, Y: Smallint);
var
  a:Integer;
  u,v:Byte;
  X,L:Smallint;
  i:Word;
begin
  i:=GetColorAttributes;
  Y:=Y+FClipStack.Origin[FClipStack.Index].Y;
  if (Y<FClipStack.Area[FClipStack.Index].Y) or (Y>=FClipStack.Area[FClipStack.Index].Y+FClipStack.Area[FClipStack.Index].Height) then
    Exit;
  if X1>X2 then begin
    a:=X1;
    X1:=X2;
    X2:=a;
  end;
  IntersectRange(FClipStack.Area[FClipStack.Index].X,FClipStack.Area[FClipStack.Index].Width,X1+FClipStack.Origin[FClipStack.Index].X,X2-X1+1,X,L);
  u:=10 shl (4*Ord(FPen.Style));
  v:=15 shl (4*Ord(FPen.Style));
  for a:=1 to L-2 do
    with FData[Y*FSize.X+a+X] do begin
      Dummy:=(Dummy or u) and v;
      AsciiChar:=FBorderChars[Dummy];
      Attributes:=i;
    end;
  if L>0 then begin
    with FData[Y*FSize.X+X] do begin
      Dummy:=(Dummy or (2 shl (4*Ord(FPen.Style)))) and v;
      AsciiChar:=FBorderChars[Dummy];
      Attributes:=i;
    end;
    with FData[Y*FSize.X+X+L-1] do begin
      Dummy:=(Dummy or (8 shl (4*Ord(FPen.Style)))) and v;
      AsciiChar:=FBorderChars[Dummy];
      Attributes:=i;
    end;
  end;
end;

procedure TCanvas.PopClipArea;
begin
  Assert(FClipStack.Index>0,'Clip stack underflow');
  Dec(FClipStack.Index);
end;

procedure TCanvas.PushClipArea(Area: TArea; Origin: TCoord);
begin
  Assert(FClipStack.Index<High(FClipStack.Area),'Clip stack overflow');
  OffsetArea(Area,FClipStack.Origin[FClipStack.Index]);
  OffsetCoord(Origin,FClipStack.Origin[FClipStack.Index]);
  FClipStack.Area[FClipStack.Index+1]:=IntersectArea(FClipStack.Area[FClipStack.Index],Area);
  FClipStack.Origin[FClipStack.Index+1]:=Origin;
  Inc(FClipStack.Index);
end;

procedure TCanvas.PushClipArea(Area: TArea);
const
  p:TCoord=(X:0;Y:0);
begin
  PushClipArea(Area,p);
end;

procedure TCanvas.Rectangle(X, Y, Width, Height: Smallint);
begin
  Rectangle(Area(X,Y,Width,Height));
end;

procedure TCanvas.Rectangle(Area: TArea);
begin
  FillRect(Area);
  if FPen.FStyle<>psClear then
    with Area do begin
      HorizontalLine(X,X+Width-1,Y);
      HorizontalLine(X,X+Width-1,Y+Height-1);
      VerticalLine(Y,Y+Height-1,X);
      VerticalLine(Y,Y+Height-1,X+Width-1);
    end;
end;

procedure TCanvas.ResetCurves;
var
  a:Integer;
begin
  for a:=FSize.X*FSize.Y-1 downto 0 do
    FData[a].Dummy:=0;
end;

procedure TCanvas.SetAsciiChar(const X, Y: Smallint; const Value: Char);
begin
  if CoordInArea(X,Y,FClipStack.Area[FClipStack.Index]) then
    FData[X+Y*FSize.X].AsciiChar:=Value;
end;

procedure TCanvas.SetAttributes(const X, Y: Smallint; const Value: Word);
begin
  if CoordInArea(X,Y,FClipStack.Area[FClipStack.Index]) then
    FData[X+Y*FSize.X].Attributes:=Value;
end;

procedure TCanvas.SetCell(const X, Y: Smallint; const Value: TCharInfo);
begin
  if CoordInArea(X,Y,FClipStack.Area[FClipStack.Index]) then
    FData[X+Y*FSize.X]:=Value;
end;

procedure TCanvas.TextOut(const X, Y: Smallint; const Text: string);
begin
  TextOut(Coord(X,Y),Text);
end;

procedure TCanvas.TextOut(Position: TCoord; const Text: string);
var
  a:Integer;
  i:Word;
begin
  OffsetCoord(Position,FClipStack.Origin[FClipStack.Index]);
  i:=GetColorAttributes;
  if FBrush.FStyle=bsClear then begin
    i:=i and $F;
    for a:=Length(Text) downto 1 do begin
      AsciiChar[Position.X+a-1,Position.Y]:=Text[a];
      Attributes[Position.X+a-1,Position.Y]:=(Attributes[Position.X+a-1,Position.Y] and $F0) or i;
    end;
  end else
    for a:=Length(Text) downto 1 do
      Cell[Position.X+a-1,Position.Y]:=CharInfo(i,Text[a]);
end;

procedure TCanvas.VerticalLine(Y1, Y2, X: Smallint);
var
  a:Integer;
  u,v:Byte;
  Y,L:Smallint;
  i:Word;
begin
  i:=GetColorAttributes;
  X:=X+FClipStack.Origin[FClipStack.Index].X;
  if (X<FClipStack.Area[FClipStack.Index].X) or (X>=FClipStack.Area[FClipStack.Index].X+FClipStack.Area[FClipStack.Index].Width) then
    Exit;
  if Y1>Y2 then begin
    a:=Y1;
    Y1:=Y2;
    Y2:=a;
  end;
  IntersectRange(FClipStack.Area[FClipStack.Index].Y,FClipStack.Area[FClipStack.Index].Height,Y1+FClipStack.Origin[FClipStack.Index].Y,Y2-Y1+1,Y,L);
  u:=5 shl (4*Ord(FPen.Style));
  v:=15 shl (4*Ord(FPen.Style));
  if FBrush.FStyle=bsClear then begin
    i:=i and $F;
    for a:=1 to L-2 do
      with FData[(a+Y)*FSize.X+X] do begin
        Dummy:=(Dummy or u) and v;
        AsciiChar:=FBorderChars[Dummy];
        Attributes:=(Attributes and $F0) or i;
      end;
  end else
    for a:=1 to L-2 do
      with FData[(a+Y)*FSize.X+X] do begin
        Dummy:=(Dummy or u) and v;
        AsciiChar:=FBorderChars[Dummy];
        Attributes:=i;
      end;
  if L>0 then begin
    with FData[Y*FSize.X+X] do begin
      Dummy:=(Dummy or (4 shl (4*Ord(FPen.Style)))) and v;
      AsciiChar:=FBorderChars[Dummy];
      if FBrush.FStyle=bsClear then
        Attributes:=(Attributes and $F0) or i
      else
        Attributes:=i;
    end;
    with FData[(Y+L-1)*FSize.X+X] do begin
      Dummy:=(Dummy or (1 shl (4*Ord(FPen.Style)))) and v;
      AsciiChar:=FBorderChars[Dummy];
      if FBrush.FStyle=bsClear then
        Attributes:=(Attributes and $F0) or i
      else
        Attributes:=i;
    end;
  end;
end;

{ TTheme }

constructor TTheme.Create;
begin

end;

initialization
  SetThemeClass(TDefaultTheme);
finalization
  FreeAndNil(GTheme);
end.

