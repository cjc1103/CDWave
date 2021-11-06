unit lame_fmt;

interface

uses mmsystem, lame;

type
  PLameWaveFormat = ^TLameWaveFormat;
  TLameWaveFormat = packed record
     WaveFormat: TWaveformatEx;
     preset: preset_mode;
  end;

const
  WAVE_FORMAT_LAME_MP3 = $FFFF;

implementation

end.

