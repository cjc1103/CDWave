unit VolumeMeter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TVolumeMeter = class(TGraphicControl)
  private
    { Private declarations }
    FBarColor: TColor;
    FPeakColor: TColor;
    FBackColor: TColor;
    FVolume:   SmallInt;
    FPeak:     SmallInt;
    FBorderSize:   Integer;
  protected
    { Protected declarations }
    procedure SetVolume(value: SmallInt);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetBorderSize(Value: Integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure ResetPeak;
  published
    { Published declarations }
    property BackColor: TColor read FBackColor write FBackColor;
    property BarColor: TColor read FBarColor write FBarColor;
    property PeakColor: TColor read FPeakColor write FPeakColor;
    property Volume: SmallInt read FVolume write SetVolume;
    property BorderSize: Integer read FBorderSize write SetBorderSize;
    property Align;
    property Anchors;
  end;

procedure Register;

implementation

constructor TVolumeMeter.Create;
begin
  inherited;
  FBarColor := clGreen;
  FPeakColor := clLime;
  FBackColor := clBlack;
  Height := 16;
  Width := 64;
end;

procedure TVolumeMeter.SetVolume;
var
  NewPos, OldPos: Integer;
  DrawTop, DrawBottom: Integer;
begin
  DrawTop := BorderSize;
  DrawBottom := Height - BorderSize;
  if Value > FPeak then FPeak := Value;
  with Canvas do
  begin
    OldPos := (Width * FVolume) shr 15;
    NewPos := (Width * value) shr 15;
    if NewPos < OldPos then
    begin
      Brush.Color := BackColor;
    end else begin
      Brush.Color := FBarColor;
    end;
    FillRect(Rect(OldPos, DrawTop, NewPos, DrawBottom));
    NewPos := (Width * FPeak) shr 15;
    Pen.Color := FPeakColor;
    MoveTo(NewPos, DrawTop);
    LineTo(NewPos, DrawBottom);
  end;
  FVolume := Value;
end;

procedure TVolumeMeter.SetBorderSize;
begin
  FBorderSize := Value;
  Invalidate;
end;

procedure TVolumeMeter.ResetPeak;
begin
  FPeak := 0;
  Invalidate;
end;

procedure TVolumeMeter.MouseUp;
begin
  if Button = mbLeft then
  begin
    ResetPeak;
  end;
end;

procedure TVolumeMeter.Paint;
var
  VolPos: Integer;
  DrawTop, DrawBottom: Integer;
begin
  DrawTop := BorderSize;
  DrawBottom := Height - BorderSize;
  with Canvas do
  begin
    VolPos := (Width * FVolume) shr 15;
    Brush.Color := BackColor;
    FillRect(Rect(0, 0, Width, Height));
    Brush.Color := FBarColor;
    FillRect(Rect(0, DrawTop, VolPos, DrawBottom));
    VolPos := (Width * FPeak) shr 15;
    Pen.Color := FPeakColor;
    MoveTo(VolPos, DrawTop);
    LineTo(VolPos, DrawBottom);
  end;
end;

procedure Register;
begin
  RegisterComponents('MiLo', [TVolumeMeter]);
end;

end.
