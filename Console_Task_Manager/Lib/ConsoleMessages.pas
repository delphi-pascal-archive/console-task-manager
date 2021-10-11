unit ConsoleMessages;

interface

uses
  Windows,Classes,ConsoleGraphics;

type
  TCMFocus=packed record
    Message:Cardinal;
    case Integer of
      0:(Allow:Boolean);
      1:(Acquired:Boolean);
  end;

  TCMKey=packed record
    Message:Cardinal;
    Key:Word;
    Canceled:Boolean;
    ShiftState:TShiftState;
  end;

  TCMChar=packed record
    Message:Cardinal;
    AsciiChar:Char;
    Canceled:Boolean;
//    ShiftState:TShiftState;
  end;

  TMouseButton=(mbLeft,mbMiddle,mbRight);

  TCMMouseMove=packed record
    Message:Cardinal;
    Position:TCoord;
    ShiftState:TShiftState;
  end;

  TCMMouseButton=packed record
    Message:Cardinal;
    Position:TCoord;
    Button:TMousebutton;
    ShiftState:TShiftState;
  end;

  TCMMouseWheel=packed record
    Message:Cardinal;
    Position:TCoord;
    Delta:Integer;
    ShiftState:TShiftState;
  end;

  TCMPaint=packed record
    Message:Cardinal;
    Canvas:TCanvas;
  end;

const
  CM_REDRAW              = $0001;

  CM_SETFOCUS            = $1001;
  CM_LOSEFOCUS           = $1002;
  CM_FOCUSCHANGED        = $1003;

  CM_KEY_DOWN            = $2001;
  CM_KEY_UP              = $2002;
  CM_CHAR                = $2003;

  CM_MOUSE_MOVE          = $3001;
  CM_MOUSE_DOWN          = $3002;
  CM_MOUSE_UP            = $3003;
  CM_MOUSE_CLICK         = $3004;
  CM_MOUSE_DOUBLE_CLICK  = $3005;
  CM_MOUSE_WHEEL         = $3006;

  CM_PAINT               = $4001;

implementation

end.

