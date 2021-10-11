unit ConsoleUtils;

interface

uses
  Windows;

type
  TArea=packed record
    case Integer of
      0:(X,Y,Width,Height:Smallint);
      1:(Origin,Size:TCoord);
  end;

const
  GNullArea:TArea=(
    X:0;
    Y:0;
    Width:0;
    Height:0;
  );

  CONSOLE_FULLSCREEN_MODE = $00000001;
  CONSOLE_WINDOWED_MODE   = $00000002;

function SetConsoleDisplayMode(ConsoleOutput:THandle;Flags:DWORD;NewScreenBufferDimensions:PCoord):BOOL;stdcall;external kernel32;

function SetConsoleFullScreenMode(const FullScreen:Boolean):Boolean;

function Coord(const X,Y:Smallint):TCoord;
procedure OffsetCoord(var Coord:TCoord;const DeltaX,DeltaY:Smallint);overload;
procedure OffsetCoord(var Coord:TCoord;const Delta:TCoord);overload;
function AddCoord(const p,q:TCoord):TCoord;

procedure IntersectRange(const Z1,L1,Z2,L2:SMallInt;var Z,L:SmallInt);

function Area(const X,Y:Smallint;const Width,Height:Word):TArea;
procedure OffsetArea(var Area:TArea;const DeltaX,DeltaY:Smallint);overload;
procedure OffsetArea(var Area:TArea;const Delta:TCoord);overload;
procedure InflateArea(var Area:TArea;const DeltaX,DeltaY:Smallint);
function CoordInArea(const X,Y:Smallint;const Area:TArea):Boolean;overload;
function CoordInArea(const Coord:TCoord;const Area:TArea):Boolean;overload;
function IntersectArea(const A1,A2:TArea):TArea;

function CaptionLength(const Caption:string):Integer;
function CaptionAction(const Caption:string;Key:Word):Boolean;

implementation

function SetConsoleFullScreenMode(const FullScreen:Boolean):Boolean;
var
  h:THandle;
const
  T:array[False..True] of Cardinal=(CONSOLE_WINDOWED_MODE,CONSOLE_FULLSCREEN_MODE);
begin
  h:=GetStdHandle(STD_OUTPUT_HANDLE);
  Result:=(h<>INVALID_HANDLE_VALUE) and not SetConsoleDisplayMode(h,T[FullScreen],nil);
end;

function Coord(const X,Y:Smallint):TCoord;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

procedure OffsetCoord(var Coord:TCoord;const DeltaX,DeltaY:Smallint);
begin
  Coord.X:=Coord.X+DeltaX;
  Coord.Y:=Coord.Y+DeltaY;
end;

procedure OffsetCoord(var Coord:TCoord;const Delta:TCoord);
begin
  Coord.X:=Coord.X+Delta.X;
  Coord.Y:=Coord.Y+Delta.Y;
end;

function AddCoord(const p,q:TCoord):TCoord;
begin
  Result.X:=p.X+q.X;
  Result.Y:=p.Y+q.Y;
end;

procedure IntersectRange(const Z1,L1,Z2,L2:SMallInt;var Z,L:SmallInt);
begin
  if Z1>Z2 then
    Z:=Z1
  else
    Z:=Z2;
  if Z1+L1<Z2+L2 then
    L:=Z1+L1-Z
  else
    L:=Z2+L2-Z;
  if L<0 then
    L:=0;
end;

function Area(const X,Y:Smallint;const Width,Height:Word):TArea;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Width:=Width;
  Result.Height:=Height;
end;

procedure OffsetArea(var Area:TArea;const DeltaX,DeltaY:Smallint);
begin
  Area.X:=Area.X+DeltaX;
  Area.Y:=Area.Y+DeltaY;
end;

procedure OffsetArea(var Area:TArea;const Delta:TCoord);
begin
  Area.X:=Area.X+Delta.X;
  Area.Y:=Area.Y+Delta.Y;
end;

procedure InflateArea(var Area:TArea;const DeltaX,DeltaY:Smallint);
begin
  Area.X:=Area.X-DeltaX;
  Area.Width:=Area.Width+2*DeltaX;
  Area.Y:=Area.Y-DeltaY;
  Area.Height:=Area.Height+2*DeltaY;
end;

function CoordInArea(const X,Y:Smallint;const Area:TArea):Boolean;
begin
  Result:=(X>=Area.X) and (Y>=Area.Y) and (X<Area.X+Area.Width) and (Y<Area.Y+Area.Height);
end;

function CoordInArea(const Coord:TCoord;const Area:TArea):Boolean;
begin
  Result:=(Coord.X>=Area.X) and (Coord.Y>=Area.Y) and (Coord.X<Area.X+Area.Width) and (Coord.Y<Area.Y+Area.Height);
end;

function IntersectArea(const A1,A2:TArea):TArea;
begin
  IntersectRange(A1.X,A1.Width,A2.X,A2.Width,Result.X,Result.Width);
  IntersectRange(A1.Y,A1.Height,A2.Y,A2.Height,Result.Y,Result.Height);
end;

function CaptionLength(const Caption:string):Integer;
var
  p:PChar;
begin
  Result:=0;
  p:=PChar(Caption);
  while p^<>#0 do begin
    if p^<>'_' then
      Inc(Result);
    Inc(p);
  end;
end;

function CaptionAction(const Caption:string;Key:Word):Boolean;
var
  p:PChar;
  t:Boolean;
begin
  Result:=False;
  t:=False;
  p:=PChar(Caption);
  while p^<>#0 do begin
    if t and (Byte(UpCase(p^))=Key) then begin
      Result:=True;
      Exit;
    end;
    t:=p^='_';
    Inc(p);
  end;
end;

end.
