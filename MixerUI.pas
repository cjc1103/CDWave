unit MixerUI;

interface

uses Controls, StdCtrls, ExtCtrls, ComCtrls, SysUtils, Mixer;

procedure CreatePanel(dest: TPanel; line: TAudioMixerLine);

implementation

function CreateFader(dest: TPanel; audiocontrol: TAudioFaderControl; line: TAudioMixerLine): Tpanel;
var
  c: Integer;
  control: TTrackbar;
  lbl: TLabel;
  pos: Integer;
begin
  audiocontrol.GetValues;
  Result := TPanel.Create(dest);
  Result.Parent := dest;
  pos := 0;
  lbl := TLabel.Create(Result);
  lbl.Parent := Result;
  lbl.Caption := audioControl.Name;
  inc(pos, lbl.Height);
  for c := 0 to line.Channels-1 do
  begin
    control := TTrackBar.Create(Result);
    control.Top := pos;
    control.Parent := Result;
    control.TickMarks := tmBoth;
    control.Min := 0;
    control.Max := 65535;
    control.PageSize := 2048;
    control.LineSize := 65536 div audiocontrol.Steps;
    control.Frequency := 4096;
    control.Position := audiocontrol.FValues[c].Signed;
    lbl := TLabel.Create(Result);
    lbl.Parent := Result;
    lbl.Top := pos;
    lbl.Left := control.Left + control.Width;
    lbl.Caption := IntToStr(audiocontrol.FValues[c].Signed);
    inc(pos, control.Height);
  end;
  Result.Height := pos;
end;

procedure CreatePanel(dest: TPanel; line: TAudioMixerLine);
var
  audioControl: TAudioMixerControl;
  i: Integer;
begin
  for i := 0 to line.Count-1 do
  begin
    audiocontrol := line[i];
    if audiocontrol is TAudioFaderControl then
    begin
      CreateFader(dest, TAudioFaderControl(audiocontrol), line);
    end;
  end;

end;


end.
