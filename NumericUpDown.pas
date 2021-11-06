unit NumericUpDown;

interface

uses
  controls, Comctrls, stdctrls, classes, Sysutils, Messages, Windows, ExtCtrls;

type

  TNumericUpDown = class(TPanel)
  private
    FMin: LongInt;
    FMax: LongInt;
    FUpDown: TUpDown;
    FEdit: TEdit;
    FEditorEnabled: Boolean;
    FTimeToChange: Integer;
    FTimerChange: TTimer;
    FIncrement: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMinValue(value: Integer);
    procedure SetMaxValue(value: Integer);
    procedure SetIncrement(value: Integer);
    function GetMinHeight: Integer;
    function GetValue: LongInt;
    procedure SetValue(NewValue: LongInt);
    procedure ChangeText(Sender: TObject);
    procedure ChangeDelayed();
    procedure Timer(Sender: TObject);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property UpDown: TUpDown read FUpDown;
    property Edit: TEdit read FEdit;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property Height;
    property HelpContext;
    property Hint;
    property Left;
    property Locked;
    property Name;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property UseDockManager;
    property Visible;
    property Width;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: LongInt read FIncrement write SetIncrement;
    property Max: LongInt read FMax write SetMaxValue;
    property Min: LongInt read FMin write SetMinValue;
    property Value: LongInt read GetValue write SetValue;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation
//----------------------------------------------------------------------
// TNumericUpDown
//----------------------------------------------------------------------
constructor TNumericUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  ControlStyle := ControlStyle - [csSetCaption];
  FEditorEnabled := True;
  FIncrement := 1;
  FMin := 0;
  FMax := 100;
  // UpDown
  FUpDown := TUpDown.Create(Self);
  FUpDown.Width := 15;
  FUpDown.Height := 17;
  FUpDown.Visible := True;
  FUpDown.Parent := Self;
  // Edit
  FEdit := TEdit.Create(Self);
  FEdit.Visible := True;
  FEdit.Parent := Self;
  FEdit.Text := '0';
  FEdit.Width := 50;
  FEdit.OnChange := ChangeText;
  FUpDown.Associate := FEdit;
  Width := 75;
  Height := 25;
end;

destructor TNumericUpDown.Destroy;
begin
  FUpdown.Free;
  FEdit.Free;
  FTimerChange.Free;
  inherited Destroy;
end;

procedure TNumericUpDown.SetMinValue;
begin
  FMin := value;
  if FUpDown <> nil then
    FUpDown.Min := FMin;
  if value > GetValue then SetValue(value);
end;

procedure TNumericUpDown.SetMaxValue;
begin
  FMax := value;
  if FUpDown <> nil then
  begin
    FUpDown.Max := FMax;
  end;
  if value < GetValue then SetValue(value);
end;

procedure TNumericUpDown.SetIncrement;
begin
  FIncrement := value;
  FUpDown.Increment := value;
end;

function TNumericUpDown.GetValue;
begin
  Result := FUpDown.Position;
end;

procedure TNumericUpDown.SetValue;
begin
  FUpDown.Position := NewValue;
end;

procedure TNumericUpDown.ChangeText;
begin
  ChangeDelayed;
  if assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNumericUpDown.ChangeDelayed;
begin
  FTimeToChange := 0;
  if FTimerChange = nil then
      FTimerChange := TTimer.Create(Self);
  FTimerChange.OnTimer := Timer;
  FTimerChange.Enabled := True;
end;

procedure TNumericUpDown.Timer;
begin
  if FTimeToChange = 0 then
  begin
    FTimerChange.Enabled := False;
    try
      FUpDown.Position := StrToInt(FEdit.Text);
    except
      FUpDown.Position := FMin;
      FEdit.Text := IntToStr(FMin);
    end;
  end else
    dec(FTimeToChange);
end;

procedure TNumericUpDown.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{  Params.Style := Params.Style and not WS_BORDER;  }
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TNumericUpDown.WMSize;
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  if Height < MinHeight then
    Height := MinHeight
  else if FUpDown <> nil then
  begin
    if NewStyleControls and Ctl3D then
    begin
      FUpDown.SetBounds(Width - FUpDown.Width - 5, 2, FUpDown.Width, Height - 5);
      FEdit.SetBounds(2, 2, Width - FUpDown.Width - 5, Height - 5);
    end else
    begin
      FUpDown.SetBounds(Width - FUpDown.Width, 1, FUpDown.Width, Height - 3);
      FEdit.SetBounds(1, 1, Width - FUpDown.Width, Height - 3);
    end;
  end;
end;

function TNumericUpDown.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(FEdit.Handle);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, FEdit.Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TNumericUpDown.WMPaste(var Message: TWMPaste);
begin
  if not FEditorEnabled or FEdit.ReadOnly then Exit;
  inherited;
end;

procedure TNumericUpDown.WMCut(var Message: TWMPaste);
begin
  if not FEditorEnabled or FEdit.ReadOnly then Exit;
  inherited;
end;

procedure TNumericUpDown.CMExit(var Message: TCMExit);
begin
  inherited;
  SetValue(Value);
end;

procedure TNumericUpDown.CMEnter(var Message: TCMGotFocus);
begin
  if FEdit.AutoSelect and not (csLButtonDown in ControlState) then
    FEdit.SelectAll;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('MiLo', [TNumericUpDown]);
end;

end.
