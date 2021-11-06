unit mmutil;

interface

uses MMSystem;

function GetWaveInName(Device: Integer): string;
function GetWaveOutName(Device: Integer): string;

resourcestring
  WaveNameError = '(Unknown device or multimedia error)';

implementation

function GetWaveInName(Device: Integer): string;
var Caps: TWaveInCaps;
begin
  if waveInGetDevCaps(Device, @Caps, sizeof(Caps)) = 0
  then Result := Caps.szPname
  else Result := WaveNameError;
end;


function GetWaveOutName(Device: Integer): string;
var Caps: TWaveOutCaps;
begin
  if waveOutGetDevCaps(Device, @Caps, sizeof(Caps)) = 0
  then Result := Caps.szPname
  else Result := WaveNameError;
end;

end.

