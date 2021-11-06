unit ImageViewer;

interface

uses
  Windows, GDIPAPI, GDIPOBJ, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TMouseHandler = class;

  TImageViewer = class(TCustomControl)
  private
    { Private declarations }
    FOnPaint: TNotifyEvent;
    FMouseHandler: TMouseHandler;
    FEraseBackground: Boolean;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    //procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    procedure FillBorders(Inside: TRect);
    procedure DrawAttributes(attrStr: string);
    property Canvas;
    property MouseHandler: TMouseHandler read FMouseHandler write FMouseHandler;
  published
    { Published declarations }
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    // Erfenis
    property Align;
    //property DoubleBuffered;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
    property EraseBackground: boolean read FEraseBackground write FEraseBackground default False;
  end;

  TMouseHandler = class
  private
    FIsDown: boolean;
    FOnReady: TNotifyEvent;
    FOnMove: TNotifyEvent;
  public
    procedure MouseDown(Sender: TImageViewer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Sender: TImageViewer; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseUp(Sender: TImageViewer; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    property OnReady: TNotifyEvent read FOnReady write FOnReady;
    property IsDown: Boolean read FIsDown;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;
  end;

procedure Register;

implementation
//----------------------------------------------------------------------
// TMouseHandler
//----------------------------------------------------------------------

procedure TMouseHandler.MouseDown;
begin
  FIsDown := true;
end;

procedure TMouseHandler.MouseMove;
begin
  if assigned(OnMove) then
    OnMove(Self);
end;

procedure TMouseHandler.MouseUp;
begin
  FIsDown := false;
  if assigned(OnReady) then
    OnReady(Self);
end;

//----------------------------------------------------------------------
// TImageViewer
//----------------------------------------------------------------------
constructor TImageViewer.Create;
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
end;

procedure TImageViewer.WMEraseBkgnd;
begin
  if FEraseBackground or (csDesigning in ComponentState) then
    inherited
  else
    // Gewoon negeren!
    Message.Result := 1;
end;

procedure TImageViewer.FillBorders;
begin
  Canvas.FillRect(Rect(0, 0, Width, Inside.Y));
  Canvas.FillRect(Rect(0, Inside.Y, Inside.X, Inside.Height + Inside.Y));
  Canvas.FillRect(Rect(Inside.Width + Inside.X, Inside.Y, Width, Inside.Height + Inside.Y));
  Canvas.FillRect(Rect(0, Inside.Height + Inside.Y, Width, Height));
end;

procedure TImageViewer.DrawAttributes;
var
  graph: TGPGraphics;
  font: TGPFont;
  brushBack, brushText: TGPSolidBrush;
  stringFormat: TGPStringFormat;
  textRect: TRectF;
begin
  font := TGPFont.Create('Arial', 16, FontStyleBold);
  brushBack := TGPSolidBrush.Create(aclBlack);
  brushText := TGPSolidBrush.Create(aclYellow);
  stringFormat := TGPStringFormat.Create;
  stringFormat.SetAlignment(StringAlignmentNear);
  graph := TGPGraphics.Create(Canvas.Handle);
  graph.MeasureString(attrStr, -1, font, MakePoint(1.0, 1.0), textRect);
  textRect.Height := 0;
  graph.DrawString(attrStr, -1, font, textRect, stringFormat, brushBack);
  textRect.X := textRect.X - 1;
  textRect.Y := textRect.Y - 1;
  graph.DrawString(attrStr, -1, font, textRect, stringFormat, brushText);
  font.Free;
  brushBack.Free;
  brushText.Free;
  stringFormat.Free;
  graph.Free;
end;


procedure TImageViewer.Paint;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.FillRect(Rect(2,2, Width-4, Height-4));
  end else
    if assigned(OnPaint) then OnPaint(self);
end;

procedure TImageViewer.MouseDown;
begin
  if assigned(MouseHandler) then
  begin
    MouseHandler.MouseDown(self, Button, Shift, X, Y);
  end else
    inherited;
end;

procedure TImageViewer.MouseUp;
begin
  if assigned(MouseHandler) then
  begin
    MouseHandler.MouseUp(self, Button, Shift, X, Y);
  end else
    inherited;
end;

procedure TImageViewer.MouseMove;
begin
  if assigned(MouseHandler) then
  begin
    //OutputDebugString(PChar('Mouse move: ' + IntToStr(X) + ', ' + IntToStr(Y)));
    MouseHandler.MouseMove(self, Shift, X, Y);
  end else
    inherited;
end;

procedure Register;
begin
  RegisterComponents('MiLo', [TImageViewer]);
end;


end.
