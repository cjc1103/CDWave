unit FLAC_Enc;

interface
uses MMSystem, FLAC;

const WAVE_FORMAT_FLAC = $FFF3;

type
  TFlacWaveFormat = packed record
    WaveFormat: TWaveFormatEx;
    Level: Integer;
  end;
  PFlacWaveFormat = ^TFlacWaveFormat;



implementation

end.
