{*******************************************************}
{                                                       }
{       Delphi Runtime Library                          }
{       Windows 32bit API Interface Unit                }
{       Multimedia Registration Interface unit          }
{       programmed by M.yanagisawa.                     }
{                                                       }
{*******************************************************}

unit MMReg;

interface

uses Windows,MMsystem;

{  Define the following to skip definitions

   NOMMIDS      Multimedia IDs are not defined
   NONEWWAVE    No new waveform types are defined except WAVEFORMATEX
   NONEWRIFF    No new RIFF forms are defined
   NOJPEGDIB    No JPEG DIB definitions
   NONEWIC      No new Image Compressor types are defined
   NOBITMAP     No extended bitmap info header definition		}

{ manufacturer IDs }
const
	MM_MICROSOFT		= 1;	     {  Microsoft Corporation				  	}
	MM_CREATIVE         = 2;         {  Creative Labs, Inc 						}
    MM_MEDIAVISION      = 3;         {  Media Vision, Inc.  					}
    MM_FUJITSU          = 4;         {  Fujitsu Corp.  							}
    MM_ARTISOFT         = 20;        {  Artisoft, Inc.  						}
    MM_TURTLE_BEACH     = 21;        {  Turtle Beach, Inc.  					}
    MM_IBM              = 22;        {  IBM Corporation  						}
    MM_VOCALTEC         = 23;        {  Vocaltec LTD.  							}
    MM_ROLAND           = 24;        {  Roland  								}
    MM_DSP_SOLUTIONS    = 25;        {  DSP Solutions, Inc.  					}
    MM_NEC              = 26;        {  NEC  									}
    MM_ATI              = 27;        {  ATI  									}
    MM_WANGLABS         = 28;        {  Wang Laboratories, Inc  				}
    MM_TANDY            = 29;        {  Tandy Corporation  						}
    MM_VOYETRA          = 30;        {  Voyetra  								}
    MM_ANTEX            = 31;        {  Antex Electronics Corporation  			}
    MM_ICL_PS           = 32;        {  ICL Personal Systems  					}
    MM_INTEL            = 33;        {  Intel Corporation  						}
    MM_GRAVIS           = 34;        {  Advanced Gravis  						}
    MM_VAL              = 35;        {  Video Associates Labs, Inc.  			}
    MM_INTERACTIVE      = 36;        {  InterActive Inc  						}
    MM_YAMAHA           = 37;        {  Yamaha Corporation of America  			}
    MM_EVEREX           = 38;        {  Everex Systems, Inc  					}
    MM_ECHO             = 39;        {  Echo Speech Corporation  				}
    MM_SIERRA           = 40;        {  Sierra Semiconductor Corp  				}
    MM_CAT              = 41;        {  Computer Aided Technologies  			}
    MM_APPS             = 42;        {  APPS Software International  			}
    MM_DSP_GROUP        = 43;        {  DSP Group, Inc  						}
    MM_MELABS           = 44;        {  microEngineering Labs  					}
    MM_COMPUTER_FRIENDS = 45;        {  Computer Friends, Inc.  				}
    MM_ESS              = 46;        {  ESS Technology  						}
    MM_AUDIOFILE        = 47;        {  Audio, Inc.  							}
    MM_MOTOROLA         = 48;        {  Motorola, Inc.  						}
    MM_CANOPUS          = 49;        {  Canopus, co., Ltd.  					}
    MM_EPSON            = 50;        {  Seiko Epson Corporation  				}
    MM_TRUEVISION       = 51;        {  Truevision  							}
    MM_AZTECH           = 52;        {  Aztech Labs, Inc.  						}
    MM_VIDEOLOGIC       = 53;        {  Videologic  							}
    MM_SCALACS          = 54;        {  SCALACS  								}
    MM_KORG             = 55;        {  Toshihiko Okuhura, Korg Inc.  			}
    MM_APT              = 56;        {  Audio Processing Technology  			}
    MM_ICS              = 57;        {  Integrated Circuit Systems, Inc. 		}
    MM_ITERATEDSYS      = 58;        {  Iterated Systems, Inc.  				}
    MM_METHEUS          = 59;        {  Metheus  								}
    MM_LOGITECH         = 60;        {  Logitech, Inc.  						}
    MM_WINNOV           = 61;        {  Winnov, Inc.  							}
    MM_NCR              = 62;        {  NCR Corporation  						}
    MM_EXAN             = 63;        {  EXAN  									}
    MM_AST              = 64;        {  AST Research Inc.  						}
    MM_WILLOWPOND       = 65;        {  Willow Pond Corporation  				}
    MM_SONICFOUNDRY     = 66;        {  Sonic Foundry  							}
    MM_VITEC            = 67;        {  Vitec Multimedia  						}
    MM_MOSCOM           = 68;        {  MOSCOM Corporation  					}
    MM_SILICONSOFT      = 69;        {  Silicon Soft, Inc.  					}
    MM_SUPERMAC         = 73;        {  Supermac  								}
    MM_AUDIOPT          = 74;        {  Audio Processing Technology  			}
    MM_SPEECHCOMP       = 76;        {  Speech Compression  					}
    MM_DOLBY            = 78;        {  Dolby Laboratories  					}
    MM_OKI              = 79;        {  OKI  									}
    MM_AURAVISION       = 80;        {  AuraVision Corporation  				}
    MM_OLIVETTI         = 81;        {  Olivetti  								}
    MM_IOMAGIC          = 82;        {  I/O Magic Corporation  					}
    MM_MATSUSHITA       = 83;        {  Matsushita Electric Industrial Co., LTD.}
    MM_CONTROLRES       = 84;        {  Control Resources Limited  				}
    MM_XEBEC            = 85;        {  Xebec Multimedia Solutions Limited  	}
    MM_NEWMEDIA         = 86;        {  New Media Corporation  					}
    MM_NMS              = 87;        {  Natural MicroSystems  					}
    MM_LYRRUS           = 88;        {  Lyrrus Inc.  							}
    MM_COMPUSIC         = 89;        {  Compusic  								}
    MM_OPTI             = 90;        {  OPTi Computers Inc.  					}
    MM_DIALOGIC         = 93;        {  Dialogic Corporation  					}


{ MM_MICROSOFT product IDs }
const
    MM_PCSPEAKER_WAVEOUT            = 13;  {  PC speaker waveform output  		  }
    MM_MSFT_WSS_WAVEIN              = 14;  {  MS Audio Board waveform input  	  }
    MM_MSFT_WSS_WAVEOUT             = 15;  {  MS Audio Board waveform output  	  }
    MM_MSFT_WSS_FMSYNTH_STEREO      = 16;  {  MS Audio Board  Stereo FM synth  	  }
    MM_MSFT_WSS_MIXER               = 17;  {  MS Audio Board Mixer Driver  		  }
    MM_MSFT_WSS_OEM_WAVEIN          = 18;  {  MS OEM Audio Board waveform input   }
    MM_MSFT_WSS_OEM_WAVEOUT         = 19;  {  MS OEM Audio Board waveform output  }
    MM_MSFT_WSS_OEM_FMSYNTH_STEREO  = 20;  {  MS OEM Audio Board Stereo FM Synth  }
    MM_MSFT_WSS_AUX                 = 21;  {  MS Audio Board Aux. Port  		  }
    MM_MSFT_WSS_OEM_AUX             = 22;  {  MS OEM Audio Aux Port  			  }
    MM_MSFT_GENERIC_WAVEIN          = 23;  {  MS Vanilla driver waveform input    }
    MM_MSFT_GENERIC_WAVEOUT         = 24;  {  MS Vanilla driver wavefrom output   }
    MM_MSFT_GENERIC_MIDIIN          = 25;  {  MS Vanilla driver MIDI in  		  }
    MM_MSFT_GENERIC_MIDIOUT         = 26;  {  MS Vanilla driver MIDI external out }
    MM_MSFT_GENERIC_MIDISYNTH       = 27;  {  MS Vanilla driver MIDI synthesizer  }
    MM_MSFT_GENERIC_AUX_LINE        = 28;  {  MS Vanilla driver aux (line in)     }
    MM_MSFT_GENERIC_AUX_MIC         = 29;  {  MS Vanilla driver aux (mic)  		  }
    MM_MSFT_GENERIC_AUX_CD          = 30;  {  MS Vanilla driver aux (CD)  		  }
    MM_MSFT_WSS_OEM_MIXER           = 31;  {  MS OEM Audio Board Mixer Driver  	  }
    MM_MSFT_MSACM                   = 32;  {  MS Audio Compression Manager  	  }
    MM_MSFT_ACM_MSADPCM             = 33;  {  MS ADPCM Codec  					  }
    MM_MSFT_ACM_IMAADPCM            = 34;  {  IMA ADPCM Codec  					  }
    MM_MSFT_ACM_MSFILTER            = 35;  {  MS Filter  						  }
    MM_MSFT_ACM_GSM610              = 36;  {  GSM 610 codec  					  }
    MM_MSFT_ACM_G711                = 37;  {  G.711 codec  						  }
    MM_MSFT_ACM_PCM                 = 38;  {  PCM converter  					  }

{  Microsoft Windows Sound System drivers }

    MM_WSS_SB16_WAVEIN              = 39;  {  Sound Blaster 16 waveform input     }
    MM_WSS_SB16_WAVEOUT             = 40;  {  Sound Blaster 16  waveform output   }
    MM_WSS_SB16_MIDIIN              = 41;  {  Sound Blaster 16 midi-in  		  }
    MM_WSS_SB16_MIDIOUT             = 42;  {  Sound Blaster 16 midi out  		  }
    MM_WSS_SB16_SYNTH               = 43;  {  Sound Blaster 16 FM Synthesis  	  }
    MM_WSS_SB16_AUX_LINE            = 44;  {  Sound Blaster 16 aux (line in)	  }
    MM_WSS_SB16_AUX_CD              = 45;  {  Sound Blaster 16 aux (CD)  		  }
    MM_WSS_SB16_MIXER               = 46;  {  Sound Blaster 16 mixer device  	  }
    MM_WSS_SBPRO_WAVEIN             = 47;  {  Sound Blaster Pro waveform input    }
    MM_WSS_SBPRO_WAVEOUT            = 48;  {  Sound Blaster Pro waveform output   }
    MM_WSS_SBPRO_MIDIIN             = 49;  {  Sound Blaster Pro midi in  		  }
    MM_WSS_SBPRO_MIDIOUT            = 50;  {  Sound Blaster Pro midi out  		  }
    MM_WSS_SBPRO_SYNTH              = 51;  {  Sound Blaster Pro FM synthesis   	  }
    MM_WSS_SBPRO_AUX_LINE           = 52;  {  Sound Blaster Pro aux (line in )    }
    MM_WSS_SBPRO_AUX_CD             = 53;  {  Sound Blaster Pro aux (CD)  		  }
    MM_WSS_SBPRO_MIXER              = 54;  {  Sound Blaster Pro mixer  			  }

    MM_MSFT_WSS_NT_WAVEIN           = 55;  {  WSS NT wave in  					  }
    MM_MSFT_WSS_NT_WAVEOUT          = 56;  {  WSS NT wave out  					  }
    MM_MSFT_WSS_NT_FMSYNTH_STEREO   = 57;  {  WSS NT FM synth  					  }
    MM_MSFT_WSS_NT_MIXER            = 58;  {  WSS NT mixer  					  }
    MM_MSFT_WSS_NT_AUX              = 59;  {  WSS NT aux  						  }

    MM_MSFT_SB16_WAVEIN    = 60;     {  Sound Blaster 16 waveform input  		  }
    MM_MSFT_SB16_WAVEOUT   = 61;     {  Sound Blaster 16  waveform output  		  }
    MM_MSFT_SB16_MIDIIN    = 62;     {  Sound Blaster 16 midi-in  				  }
    MM_MSFT_SB16_MIDIOUT   = 63;     {  Sound Blaster 16 midi out  				  }
    MM_MSFT_SB16_SYNTH     = 64;     {  Sound Blaster 16 FM Synthesis  			  }
    MM_MSFT_SB16_AUX_LINE  = 65;     {  Sound Blaster 16 aux (line in)  		  }
    MM_MSFT_SB16_AUX_CD    = 66;     {  Sound Blaster 16 aux (CD)  				  }
    MM_MSFT_SB16_MIXER     = 67;     {  Sound Blaster 16 mixer device  			  }
    MM_MSFT_SBPRO_WAVEIN   = 68;     {  Sound Blaster Pro waveform input  		  }
    MM_MSFT_SBPRO_WAVEOUT  = 69;     {  Sound Blaster Pro waveform output  		  }
    MM_MSFT_SBPRO_MIDIIN   = 70;     {  Sound Blaster Pro midi in  				  }
    MM_MSFT_SBPRO_MIDIOUT  = 71;     {  Sound Blaster Pro midi out  			  }
    MM_MSFT_SBPRO_SYNTH    = 72;     {  Sound Blaster Pro FM synthesis  		  }
    MM_MSFT_SBPRO_AUX_LINE = 73;     {  Sound Blaster Pro aux (line in )  		  }
    MM_MSFT_SBPRO_AUX_CD   = 74;     {  Sound Blaster Pro aux (CD)  			  }
    MM_MSFT_SBPRO_MIXER    = 75;     {  Sound Blaster Pro mixer  				  }
    MM_MSFT_MSOPL_SYNTH    = 76;	 {  Yamaha OPL2/OPL3 compatible FM synthesis  }

{ MM_CREATIVE product IDs }
    MM_CREATIVE_SB15_WAVEIN      = 1;       {  SB (r) 1.5 waveform input  }
    MM_CREATIVE_SB20_WAVEIN      = 2;
    MM_CREATIVE_SBPRO_WAVEIN     = 3;
    MM_CREATIVE_SBP16_WAVEIN     = 4;
    MM_CREATIVE_SB15_WAVEOUT     = 101;
    MM_CREATIVE_SB20_WAVEOUT     = 102;
    MM_CREATIVE_SBPRO_WAVEOUT    = 103;
    MM_CREATIVE_SBP16_WAVEOUT    = 104;
    MM_CREATIVE_MIDIOUT          = 201;     {  SB (r)  }
    MM_CREATIVE_MIDIIN           = 202;     {  SB (r)  }
    MM_CREATIVE_FMSYNTH_MONO     = 301;     {  SB (r)  }
    MM_CREATIVE_FMSYNTH_STEREO   = 302;     {  SB Pro (r) stereo synthesizer  }
    MM_CREATIVE_MIDI_AWE32       = 303;
    MM_CREATIVE_AUX_CD           = 401;     {  SB Pro (r) aux (CD)  }
    MM_CREATIVE_AUX_LINE         = 402;     {  SB Pro (r) aux (Line in )  }
    MM_CREATIVE_AUX_MIC          = 403;     {  SB Pro (r) aux (mic)  }
    MM_CREATIVE_AUX_MASTER       = 404;
    MM_CREATIVE_AUX_PCSPK        = 405;
    MM_CREATIVE_AUX_WAVE         = 406;
    MM_CREATIVE_AUX_MIDI         = 407;
    MM_CREATIVE_SBPRO_MIXER      = 408;
    MM_CREATIVE_SB16_MIXER       = 409;

{ MM_MEDIAVISION product IDs }

{ Pro Audio Spectrum }
    MM_MEDIAVISION_PROAUDIO      = $10;
    MM_PROAUD_MIDIOUT            = (MM_MEDIAVISION_PROAUDIO+1);
    MM_PROAUD_MIDIIN             = (MM_MEDIAVISION_PROAUDIO+2);
    MM_PROAUD_SYNTH              = (MM_MEDIAVISION_PROAUDIO+3);
    MM_PROAUD_WAVEOUT            = (MM_MEDIAVISION_PROAUDIO+4);
    MM_PROAUD_WAVEIN             = (MM_MEDIAVISION_PROAUDIO+5);
    MM_PROAUD_MIXER              = (MM_MEDIAVISION_PROAUDIO+6);
    MM_PROAUD_AUX                = (MM_MEDIAVISION_PROAUDIO+7);

{ Thunder Board }
    MM_MEDIAVISION_THUNDER       = $20;
    MM_THUNDER_SYNTH             = (MM_MEDIAVISION_THUNDER+3);
    MM_THUNDER_WAVEOUT           = (MM_MEDIAVISION_THUNDER+4);
    MM_THUNDER_WAVEIN            = (MM_MEDIAVISION_THUNDER+5);
    MM_THUNDER_AUX               = (MM_MEDIAVISION_THUNDER+7);

{ Audio Port  }
    MM_MEDIAVISION_TPORT         = $40;
    MM_TPORT_WAVEOUT             = (MM_MEDIAVISION_TPORT+1);
    MM_TPORT_WAVEIN              = (MM_MEDIAVISION_TPORT+2);
    MM_TPORT_SYNTH               = (MM_MEDIAVISION_TPORT+3);

{ Pro Audio Spectrum Plus }
    MM_MEDIAVISION_PROAUDIO_PLUS = $50;
    MM_PROAUD_PLUS_MIDIOUT       = (MM_MEDIAVISION_PROAUDIO_PLUS+1);
    MM_PROAUD_PLUS_MIDIIN        = (MM_MEDIAVISION_PROAUDIO_PLUS+2);
    MM_PROAUD_PLUS_SYNTH         = (MM_MEDIAVISION_PROAUDIO_PLUS+3);
    MM_PROAUD_PLUS_WAVEOUT       = (MM_MEDIAVISION_PROAUDIO_PLUS+4);
    MM_PROAUD_PLUS_WAVEIN        = (MM_MEDIAVISION_PROAUDIO_PLUS+5);
    MM_PROAUD_PLUS_MIXER         = (MM_MEDIAVISION_PROAUDIO_PLUS+6);
    MM_PROAUD_PLUS_AUX           = (MM_MEDIAVISION_PROAUDIO_PLUS+7);

{ Pro Audio Spectrum 16 }
    MM_MEDIAVISION_PROAUDIO_16   = $60;
    MM_PROAUD_16_MIDIOUT         = (MM_MEDIAVISION_PROAUDIO_16+1);
    MM_PROAUD_16_MIDIIN          = (MM_MEDIAVISION_PROAUDIO_16+2);
    MM_PROAUD_16_SYNTH           = (MM_MEDIAVISION_PROAUDIO_16+3);
    MM_PROAUD_16_WAVEOUT         = (MM_MEDIAVISION_PROAUDIO_16+4);
    MM_PROAUD_16_WAVEIN          = (MM_MEDIAVISION_PROAUDIO_16+5);
    MM_PROAUD_16_MIXER           = (MM_MEDIAVISION_PROAUDIO_16+6);
    MM_PROAUD_16_AUX             = (MM_MEDIAVISION_PROAUDIO_16+7);

{ Pro Audio Studio 16 }
    MM_MEDIAVISION_PROSTUDIO_16  = $60;
    MM_STUDIO_16_MIDIOUT         = (MM_MEDIAVISION_PROSTUDIO_16+1);
    MM_STUDIO_16_MIDIIN          = (MM_MEDIAVISION_PROSTUDIO_16+2);
    MM_STUDIO_16_SYNTH           = (MM_MEDIAVISION_PROSTUDIO_16+3);
    MM_STUDIO_16_WAVEOUT         = (MM_MEDIAVISION_PROSTUDIO_16+4);
    MM_STUDIO_16_WAVEIN          = (MM_MEDIAVISION_PROSTUDIO_16+5);
    MM_STUDIO_16_MIXER           = (MM_MEDIAVISION_PROSTUDIO_16+6);
    MM_STUDIO_16_AUX             = (MM_MEDIAVISION_PROSTUDIO_16+7);

{ CDPC }
    MM_MEDIAVISION_CDPC          = $70;
    MM_CDPC_MIDIOUT              = (MM_MEDIAVISION_CDPC+1);
    MM_CDPC_MIDIIN               = (MM_MEDIAVISION_CDPC+2);
    MM_CDPC_SYNTH                = (MM_MEDIAVISION_CDPC+3);
    MM_CDPC_WAVEOUT              = (MM_MEDIAVISION_CDPC+4);
    MM_CDPC_WAVEIN               = (MM_MEDIAVISION_CDPC+5);
    MM_CDPC_MIXER                = (MM_MEDIAVISION_CDPC+6);
    MM_CDPC_AUX                  = (MM_MEDIAVISION_CDPC+7);

{ Opus MV 1208 Chipsent }
    MM_MEDIAVISION_OPUS1208      = $80;
    MM_OPUS401_MIDIOUT           = (MM_MEDIAVISION_OPUS1208+1);
    MM_OPUS401_MIDIIN            = (MM_MEDIAVISION_OPUS1208+2);
    MM_OPUS1208_SYNTH            = (MM_MEDIAVISION_OPUS1208+3);
    MM_OPUS1208_WAVEOUT          = (MM_MEDIAVISION_OPUS1208+4);
    MM_OPUS1208_WAVEIN           = (MM_MEDIAVISION_OPUS1208+5);
    MM_OPUS1208_MIXER            = (MM_MEDIAVISION_OPUS1208+6);
    MM_OPUS1208_AUX              = (MM_MEDIAVISION_OPUS1208+7);

{ Opus MV 1216 chipset }
    MM_MEDIAVISION_OPUS1216      = $90;
    MM_OPUS1216_MIDIOUT          = (MM_MEDIAVISION_OPUS1216+1);
    MM_OPUS1216_MIDIIN           = (MM_MEDIAVISION_OPUS1216+2);
    MM_OPUS1216_SYNTH            = (MM_MEDIAVISION_OPUS1216+3);
    MM_OPUS1216_WAVEOUT          = (MM_MEDIAVISION_OPUS1216+4);
    MM_OPUS1216_WAVEIN           = (MM_MEDIAVISION_OPUS1216+5);
    MM_OPUS1216_MIXER            = (MM_MEDIAVISION_OPUS1216+6);
    MM_OPUS1216_AUX              = (MM_MEDIAVISION_OPUS1216+7);

{ MM_ARTISOFT product IDs }
    MM_ARTISOFT_SBWAVEIN       = 1;   { Artisoft sounding Board waveform input  }
    MM_ARTISOFT_SBWAVEOUT      = 2;   { Artisoft sounding Board waveform output }

{ MM_IBM product IDs }
    MM_MMOTION_WAVEAUX         = 1;   {  IBM M-Motion Auxiliary Device	}
    MM_MMOTION_WAVEOUT         = 2;   {  IBM M-Motion Waveform output  	}
    MM_MMOTION_WAVEIN          = 3;   {  IBM M-Motion  Waveform Input  	}
    MM_IBM_PCMCIA_WAVEIN       = 11;  {  IBM waveform input  			}
    MM_IBM_PCMCIA_WAVEOUT      = 12;  {  IBM Waveform output  			}
    MM_IBM_PCMCIA_SYNTH        = 13;  {  IBM Midi Synthesis  			}
    MM_IBM_PCMCIA_MIDIIN       = 14;  {  IBM external MIDI in  			}
    MM_IBM_PCMCIA_MIDIOUT      = 15;  {  IBM external MIDI out  		}
    MM_IBM_PCMCIA_AUX          = 16;  {  IBM auxiliary control  		}

{ MM_VOCALTEC product IDs }
    MM_VOCALTEC_WAVEOUT        = 1;
    MM_VOCALTEC_WAVEIN         = 2;

{ MM_ROLAND product IDs }
    MM_ROLAND_MPU401_MIDIOUT   = 15;
    MM_ROLAND_MPU401_MIDIIN    = 16;
    MM_ROLAND_SMPU_MIDIOUTA    = 17;
    MM_ROLAND_SMPU_MIDIOUTB    = 18;
    MM_ROLAND_SMPU_MIDIINA     = 19;
    MM_ROLAND_SMPU_MIDIINB     = 20;
    MM_ROLAND_SC7_MIDIOUT      = 21;
    MM_ROLAND_SC7_MIDIIN       = 22;
    MM_ROLAND_SERIAL_MIDIOUT   = 23;
    MM_ROLAND_SERIAL_MIDIIN    = 24;

{ MM_DSP_SOLUTIONS product IDs }
    MM_DSP_SOLUTIONS_WAVEOUT   = 1;
    MM_DSP_SOLUTIONS_WAVEIN    = 2;
    MM_DSP_SOLUTIONS_SYNTH     = 3;
    MM_DSP_SOLUTIONS_AUX       = 4;

{ MM_WANGLABS product IDs }
    MM_WANGLABS_WAVEIN1        = 1;  {  Input audio wave on CPU board models: Exec 4010, 4030, 3450; PC 251/25c, pc 461/25s , pc 461/33c  }
    MM_WANGLABS_WAVEOUT1       = 2;

{ MM_TANDY product IDs }
    MM_TANDY_VISWAVEIN         = 1;
    MM_TANDY_VISWAVEOUT        = 2;
    MM_TANDY_VISBIOSSYNTH      = 3;
    MM_TANDY_SENS_MMAWAVEIN    = 4;
    MM_TANDY_SENS_MMAWAVEOUT   = 5;
    MM_TANDY_SENS_MMAMIDIIN    = 6;
    MM_TANDY_SENS_MMAMIDIOUT   = 7;
    MM_TANDY_SENS_VISWAVEOUT   = 8;
    MM_TANDY_PSSJWAVEIN        = 9;
    MM_TANDY_PSSJWAVEOUT       = 10;

{ product IDs }
    MM_INTELOPD_WAVEIN         = 1;       {  HID2 WaveAudio Driver  }
    MM_INTELOPD_WAVEOUT        = 101;     {  HID2					}
    MM_INTELOPD_AUX            = 401;     {  HID2 for mixing  		}

{ MM_INTERACTIVE product IDs }
    MM_INTERACTIVE_WAVEIN      = $45;
    MM_INTERACTIVE_WAVEOUT     = $45;

{ MM_YAMAHA product IDs }
    MM_YAMAHA_GSS_SYNTH        = $01;
    MM_YAMAHA_GSS_WAVEOUT      = $02;
    MM_YAMAHA_GSS_WAVEIN       = $03;
    MM_YAMAHA_GSS_MIDIOUT      = $04;
    MM_YAMAHA_GSS_MIDIIN       = $05;
    MM_YAMAHA_GSS_AUX          = $06;

{ MM_EVEREX product IDs }
    MM_EVEREX_CARRIER          = $01;

{ MM_ECHO product IDs }
    MM_ECHO_SYNTH              = $01;
    MM_ECHO_WAVEOUT            = $02;
    MM_ECHO_WAVEIN             = $03;
    MM_ECHO_MIDIOUT            = $04;
    MM_ECHO_MIDIIN             = $05;
    MM_ECHO_AUX                = $06;

{ MM_SIERRA product IDs }
    MM_SIERRA_ARIA_MIDIOUT     = $14;
    MM_SIERRA_ARIA_MIDIIN      = $15;
    MM_SIERRA_ARIA_SYNTH       = $16;
    MM_SIERRA_ARIA_WAVEOUT     = $17;
    MM_SIERRA_ARIA_WAVEIN      = $18;
    MM_SIERRA_ARIA_AUX         = $19;
    MM_SIERRA_ARIA_AUX2        = $20;

{ MM_CAT product IDs }
    MM_CAT_WAVEOUT             = 1;

{ MM_DSP_GROUP product IDs }
    MM_DSP_GROUP_TRUESPEECH    = $01;

{ MM_MELABS product IDs }
    MM_MELABS_MIDI2GO          = $01;

{ MM_ESS product IDs }
    MM_ESS_AMWAVEOUT           = $01;
    MM_ESS_AMWAVEIN            = $02;
    MM_ESS_AMAUX               = $03;
    MM_ESS_AMSYNTH             = $04;
    MM_ESS_AMMIDIOUT           = $05;
    MM_ESS_AMMIDIIN            = $06;
    MM_ESS_MIXER               = $07;
    MM_ESS_AUX_CD              = $08;
    MM_ESS_MPU401_MIDIOUT      = $09;
    MM_ESS_MPU401_MIDIIN       = $0A;
    MM_ESS_ES488_WAVEOUT       = $10;
    MM_ESS_ES488_WAVEIN        = $11;
    MM_ESS_ES488_MIXER         = $12;
    MM_ESS_ES688_WAVEOUT       = $13;
    MM_ESS_ES688_WAVEIN        = $14;
    MM_ESS_ES688_MIXER         = $15;
    MM_ESS_ES1488_WAVEOUT      = $16;
    MM_ESS_ES1488_WAVEIN       = $17;
    MM_ESS_ES1488_MIXER        = $18;
    MM_ESS_ES1688_WAVEOUT      = $19;
    MM_ESS_ES1688_WAVEIN       = $1A;
    MM_ESS_ES1688_MIXER        = $1B;

{ product IDs }
    MM_EPS_FMSND               = 1;

{ MM_TRUEVISION product IDs }
    MM_TRUEVISION_WAVEIN1      = 1;
    MM_TRUEVISION_WAVEOUT1     = 2;

{ MM_AZTECH product IDs }
    MM_AZTECH_MIDIOUT          = 3;
    MM_AZTECH_MIDIIN           = 4;
    MM_AZTECH_WAVEIN           = 17;
    MM_AZTECH_WAVEOUT          = 18;
    MM_AZTECH_FMSYNTH          = 20;
    MM_AZTECH_MIXER            = 21;
    MM_AZTECH_PRO16_WAVEIN     = 33;
    MM_AZTECH_PRO16_WAVEOUT    = 34;
    MM_AZTECH_PRO16_FMSYNTH    = 38;
    MM_AZTECH_DSP16_WAVEIN     = 65;
    MM_AZTECH_DSP16_WAVEOUT    = 66;
    MM_AZTECH_DSP16_FMSYNTH    = 68;
    MM_AZTECH_DSP16_WAVESYNTH  = 70;
    MM_AZTECH_AUX_CD           = 401;
    MM_AZTECH_AUX_LINE         = 402;
    MM_AZTECH_AUX_MIC          = 403;
    MM_AZTECH_AUX              = 404;
    MM_AZTECH_NOVA16_WAVEIN    = 71;
    MM_AZTECH_NOVA16_WAVEOUT   = 72;
    MM_AZTECH_NOVA16_MIXER     = 73;
    MM_AZTECH_WASH16_WAVEIN    = 74;
    MM_AZTECH_WASH16_WAVEOUT   = 75;
    MM_AZTECH_WASH16_MIXER     = 76;

{ MM_VIDEOLOGIC product IDs }
    MM_VIDEOLOGIC_MSWAVEIN     = 1;
    MM_VIDEOLOGIC_MSWAVEOUT    = 2;

{ MM_KORG product IDs }
    MM_KORG_PCIF_MIDIOUT       = 1;
    MM_KORG_PCIF_MIDIIN        = 2;

{ MM_APT product IDs }
    MM_APT_ACE100CD            = 1;

{ MM_ICS product IDs }
    MM_ICS_WAVEDECK_WAVEOUT    = 1;      {  MS WSS compatible card and driver  }
    MM_ICS_WAVEDECK_WAVEIN     = 2;
    MM_ICS_WAVEDECK_MIXER      = 3;
    MM_ICS_WAVEDECK_AUX        = 4;
    MM_ICS_WAVEDECK_SYNTH      = 5;

{ MM_ITERATEDSYS product IDs }
    MM_ITERATEDSYS_FUFCODEC    = 1;

{ MM_METHEUS product IDs }
    MM_METHEUS_ZIPPER          = 1;

{ MM_WINNOV product IDs }
    MM_WINNOV_CAVIAR_WAVEIN    = 1;
    MM_WINNOV_CAVIAR_WAVEOUT   = 2;
    MM_WINNOV_CAVIAR_VIDC      = 3;
    MM_WINNOV_CAVIAR_CHAMPAGNE = 4;       {  Fourcc is CHAM  }
    MM_WINNOV_CAVIAR_YUV8      = 5;       {  Fourcc is YUV8  }

{ MM_NCR product IDs }
    MM_NCR_BA_WAVEIN           = 1;
    MM_NCR_BA_WAVEOUT          = 2;
    MM_NCR_BA_SYNTH            = 3;
    MM_NCR_BA_AUX              = 4;
    MM_NCR_BA_MIXER            = 5;

{ MM_VITEC product IDs }
    MM_VITEC_VMAKER            = 1;
    MM_VITEC_VMPRO             = 2;

{ MM_MOSCOM product IDs }
    MM_MOSCOM_VPC2400          = 1;       {  Four Port Voice Processing / Voice Recognition Board  }

{ MM_SILICONSOFT product IDs }
    MM_SILICONSOFT_SC1_WAVEIN  = 1;  { Waveform in , high sample rate  			 }
    MM_SILICONSOFT_SC1_WAVEOUT = 2;  { Waveform out , high sample rate  		 }
    MM_SILICONSOFT_SC2_WAVEIN  = 3;  { Waveform in 2 channels, high sample rate  }
    MM_SILICONSOFT_SC2_WAVEOUT = 4;  { Waveform out 2 channels, high sample rate }
    MM_SILICONSOFT_SOUNDJR2_WAVEOUT   = 5; {  Waveform out, self powered, efficient  }
    MM_SILICONSOFT_SOUNDJR2PR_WAVEIN  = 6; {  Waveform in, self powered, efficient  }
    MM_SILICONSOFT_SOUNDJR2PR_WAVEOUT = 7; {  Waveform out 2 channels, self powered, efficient  }
    MM_SILICONSOFT_SOUNDJR3_WAVEOUT   = 8; {  Waveform in 2 channels, self powered, efficient  }

{ MM_OLIVETTI product IDs }
    MM_OLIVETTI_WAVEIN          = 1;
    MM_OLIVETTI_WAVEOUT         = 2;
    MM_OLIVETTI_MIXER           = 3;
    MM_OLIVETTI_AUX             = 4;
    MM_OLIVETTI_MIDIIN          = 5;
    MM_OLIVETTI_MIDIOUT         = 6;
    MM_OLIVETTI_SYNTH           = 7;
    MM_OLIVETTI_JOYSTICK        = 8;
    MM_OLIVETTI_ACM_GSM         = 9;
    MM_OLIVETTI_ACM_ADPCM       = 10;
    MM_OLIVETTI_ACM_CELP        = 11;
    MM_OLIVETTI_ACM_SBC         = 12;
    MM_OLIVETTI_ACM_OPR         = 13;

{ MM_IOMAGIC product IDs }

{  The I/O Magic Tempo is a PCMCIA Type 2 audio card featuring wave audio
    record and playback, FM synthesizer, and MIDI output.  The I/O Magic
    Tempo WaveOut device supports mono and stereo PCM playback at rates
    of 7350, 11025, 22050, and  44100 samples }

    MM_IOMAGIC_TEMPO_WAVEOUT           = 1;
    MM_IOMAGIC_TEMPO_WAVEIN            = 2;
    MM_IOMAGIC_TEMPO_SYNTH             = 3;
    MM_IOMAGIC_TEMPO_MIDIOUT           = 4;
    MM_IOMAGIC_TEMPO_MXDOUT            = 5;
    MM_IOMAGIC_TEMPO_AUXOUT            = 6;

{ MM_MATSUSHITA product IDs }
    MM_MATSUSHITA_WAVEIN               = 1;
    MM_MATSUSHITA_WAVEOUT              = 2;
    MM_MATSUSHITA_FMSYNTH_STEREO       = 3;
    MM_MATSUSHITA_MIXER                = 4;
    MM_MATSUSHITA_AUX                  = 5;

{ MM_NEWMEDIA product IDs }
    MM_NEWMEDIA_WAVJAMMER              = 1;       {  WSS Compatible sound card.  }

{ MM_LYRRUS product IDs }

{  Bridge is a MIDI driver that allows the the Lyrrus G-VOX hardware to
    communicate with Windows base transcription and sequencer applications.
    The driver also provides a mechanism for the user to configure the system
    to their personal playing style. }

    MM_LYRRUS_BRIDGE_GUITAR            = 1;

{ MM_OPTI product IDs }
    MM_OPTI_M16_FMSYNTH_STEREO         = $0001;
    MM_OPTI_M16_MIDIIN                 = $0002;
    MM_OPTI_M16_MIDIOUT                = $0003;
    MM_OPTI_M16_WAVEIN                 = $0004;
    MM_OPTI_M16_WAVEOUT                = $0005;
    MM_OPTI_M16_MIXER                  = $0006;
    MM_OPTI_M16_AUX                    = $0007;
    MM_OPTI_P16_FMSYNTH_STEREO         = $0010;
    MM_OPTI_P16_MIDIIN                 = $0011;
    MM_OPTI_P16_MIDIOUT                = $0012;
    MM_OPTI_P16_WAVEIN                 = $0013;
    MM_OPTI_P16_WAVEOUT                = $0014;
    MM_OPTI_P16_MIXER                  = $0015;
    MM_OPTI_P16_AUX                    = $0016;
    MM_OPTI_M32_WAVEIN                 = $0020;
    MM_OPTI_M32_WAVEOUT                = $0021;
    MM_OPTI_M32_MIDIIN                 = $0022;
    MM_OPTI_M32_MIDIOUT                = $0023;
    MM_OPTI_M32_SYNTH_STEREO           = $0024;
    MM_OPTI_M32_MIXER                  = $0025;
    MM_OPTI_M32_AUX                    = $0026;

{////////////////////////////////////////////////////////////////////////// }

{              INFO LIST CHUNKS (from the Multimedia Programmer's Reference
                                        plus new ones)
}
   FOURCC_RIFFINFO_IARL = $4C524149;   	{Archival location }
   FOURCC_RIFFINFO_IART = $54524149;	{Artist }
   FOURCC_RIFFINFO_ICMS = $534D4349;	{Commissioned }
   FOURCC_RIFFINFO_ICMT = $544D4349;	{Comments }
   FOURCC_RIFFINFO_ICOP = $504F4349;	{Copyright }
   FOURCC_RIFFINFO_ICRD = $44524349;   	{Creation date of subject }
   FOURCC_RIFFINFO_ICRP = $50524349;	{Cropped }
   FOURCC_RIFFINFO_IDIM = $4D494449;	{Dimensions }
   FOURCC_RIFFINFO_IDPI = $49504449;	{Dots per inch }
   FOURCC_RIFFINFO_IENG = $474E4549;   	{Engineer }
   FOURCC_RIFFINFO_IGNR = $524E4749;   	{Genre }
   FOURCC_RIFFINFO_IKEY = $59454B49;   	{Keywords }
   FOURCC_RIFFINFO_ILGT = $54474C49;	{Lightness settings }
   FOURCC_RIFFINFO_IMED = $44454D49;	{Medium }
   FOURCC_RIFFINFO_INAM = $4D414E49;   	{Name of subject }
   FOURCC_RIFFINFO_IPLT = $544C5049;   	{Palette Settings. No. of colors requested. }
   FOURCC_RIFFINFO_IPRD = $44525049;	{Product }
   FOURCC_RIFFINFO_ISBJ = $4A425349;	{Subject description  }
   FOURCC_RIFFINFO_ISFT = $54465349;   	{Software. Name of package used to create file.  }
   FOURCC_RIFFINFO_ISHP = $50485349;   	{Sharpness.  }
   FOURCC_RIFFINFO_ISRC = $43525349;	{Source.   }
   FOURCC_RIFFINFO_ISRF = $46525349;   	{Source Form. ie slide, paper  }
   FOURCC_RIFFINFO_ITCH = $48435449;	{Technician who digitized the subject.  }

{ New INFO Chunks as of August 30, 1993: }
   FOURCC_RIFFINFO_ISMP = $504D5349;	{SMPTE time code  }
{ ISMP: SMPTE time code of digitization start point expressed as a NULL terminated
	    text string "HH:MM:SS:FF". If performing MCI capture in AVICAP, this
        chunk will be automatically set based on the MCI start time.
}
   FOURCC_RIFFINFO_IDIT = $54494449;     {Digitization Time  }
{ IDIT: "Digitization Time" Specifies the time and date that the digitization commenced.
     The digitization time is contained in an ASCII string which
     contains exactly 26 characters and is in the format
     "Wed Jan 02 02:03:55 1990\n\0".
     The ctime(), asctime(), functions can be used to create strings
     in this format. This chunk is automatically added to the capture
     file based on the current system time at the moment capture is initiated.
}

{Template line for new additions}
{   RIFFINFO_I      mmioFOURCC ('I', '', '', '')        }

{///////////////////////////////////////////////////////////////////////////}

{ WAVE form wFormatTag IDs }
    WAVE_FORMAT_UNKNOWN    = $0000; {  Microsoft Corporation  }
    WAVE_FORMAT_ADPCM      = $0002; {  Microsoft Corporation  }
    WAVE_FORMAT_IBM_CVSD   = $0005; {  IBM Corporation  }
    WAVE_FORMAT_ALAW       = $0006; {  Microsoft Corporation  }
    WAVE_FORMAT_MULAW      = $0007; {  Microsoft Corporation  }
    WAVE_FORMAT_OKI_ADPCM  = $0010; {  OKI  }
    WAVE_FORMAT_DVI_ADPCM  = $0011; {  Intel Corporation  }
    WAVE_FORMAT_IMA_ADPCM  = (WAVE_FORMAT_DVI_ADPCM); {  Intel Corporation  }
    WAVE_FORMAT_MEDIASPACE_ADPCM   = $0012;  {  Videologic  }
    WAVE_FORMAT_SIERRA_ADPCM       = $0013;  {  Sierra Semiconductor Corp  }
    WAVE_FORMAT_G723_ADPCM = $0014;  {  Antex Electronics Corporation  }
    WAVE_FORMAT_DIGISTD    = $0015;  {  DSP Solutions, Inc.  }
    WAVE_FORMAT_DIGIFIX    = $0016;  {  DSP Solutions, Inc.  }
    WAVE_FORMAT_DIALOGIC_OKI_ADPCM = $0017;  {  Dialogic Corporation  }
    WAVE_FORMAT_YAMAHA_ADPCM       = $0020;  {  Yamaha Corporation of America  }
    WAVE_FORMAT_SONARC     = $0021;  {  Speech Compression  }
    WAVE_FORMAT_DSPGROUP_TRUESPEECH  = $0022;  {  DSP Group, Inc  }
    WAVE_FORMAT_ECHOSC1    = $0023;  {  Echo Speech Corporation  }
    WAVE_FORMAT_AUDIOFILE_AF36  = $0024;  {    }
    WAVE_FORMAT_APTX       = $0025;  {  Audio Processing Technology  }
    WAVE_FORMAT_AUDIOFILE_AF10  = $0026;  {    }
    WAVE_FORMAT_DOLBY_AC2  = $0030;  {  Dolby Laboratories  }
    WAVE_FORMAT_GSM610     = $0031;  {  Microsoft Corporation  }
    WAVE_FORMAT_ANTEX_ADPCME       = $0033;  {  Antex Electronics Corporation  }
    WAVE_FORMAT_CONTROL_RES_VQLPC  = $0034;  {  Control Resources Limited  }
    WAVE_FORMAT_DIGIREAL   = $0035;  {  DSP Solutions, Inc.  }
    WAVE_FORMAT_DIGIADPCM  = $0036;  {  DSP Solutions, Inc.  }
    WAVE_FORMAT_CONTROL_RES_CR10   = $0037;  {  Control Resources Limited  }
    WAVE_FORMAT_NMS_VBXADPCM       = $0038;  {  Natural MicroSystems  }
    WAVE_FORMAT_CS_IMAADPCM = $0039; { Crystal Semiconductor IMA ADPCM }
    WAVE_FORMAT_G721_ADPCM = $0040;  {  Antex Electronics Corporation  }
    WAVE_FORMAT_MPEG       = $0050;  {  Microsoft Corporation  }
    WAVE_FORMAT_CREATIVE_ADPCM     = $0200;  {  Creative Labs, Inc  }
    WAVE_FORMAT_CREATIVE_FASTSPEECH8   = $0202;  {  Creative Labs, Inc  }
    WAVE_FORMAT_CREATIVE_FASTSPEECH10  = $0203;  {  Creative Labs, Inc  }
    WAVE_FORMAT_FM_TOWNS_SND       = $0300;  {  Fujitsu Corp.  }
    WAVE_FORMAT_OLIGSM     = $1000;  {  Ing C. Olivetti & C., S.p.A.  }
    WAVE_FORMAT_OLIADPCM   = $1001;  {  Ing C. Olivetti & C., S.p.A.  }
    WAVE_FORMAT_OLICELP    = $1002;  {  Ing C. Olivetti & C., S.p.A.  }
    WAVE_FORMAT_OLISBC     = $1003;  {  Ing C. Olivetti & C., S.p.A.  }
    WAVE_FORMAT_OLIOPR     = $1004;  {  Ing C. Olivetti & C., S.p.A.  }

{
    the WAVE_FORMAT_DEVELOPMENT format tag can be used during the
    development phase of a new wave format.  Before shipping, you MUST
    acquire an official format tag from Microsoft.
}
   WAVE_FORMAT_DEVELOPMENT  = ($FFFF);

{ Define data for MS ADPCM }
type
  PADPCMCoefset = ^TADPCMCoefset;
  TADPCMCoefset = packed record
      iCoef1 : word;
      iCoef2 : word;
   end;

{
 *  this pragma disables the warning issued by the Microsoft C compiler
 *  when using a zero size array as place holder when compiling for
 *  C++ or with -W4.
 *
 }

type
  PADPCMWaveFormat = ^TADPCMWaveFormat;
  TADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
      wSamplePerBlock : WORD;
      wNumCoef : WORD;
      aCoef : array [0..6] of TADPCMCoefSet;
   end;

{
    Intel's DVI ADPCM structure definitions

       for WAVE_FORMAT_DVI_ADPCM   ($0011)

}

type
  PDVI_ADPCMWaveFormat = ^TDVI_ADPCMWaveFormat;
  TDVI_ADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
      wSamplePerBlock : WORD;
   end;

{
    IMA endorsed ADPCM structure definitions--note that this is exactly
    the same format as Intel's DVI ADPCM.

        for WAVE_FORMAT_IMA_ADPCM   ($0011)

}

type
  PIMA_ADPCMWaveFormat = ^TIMA_ADPCMWaveFormat;
  TIMA_ADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
      wSamplePerBlock : WORD;
   end;

{
   VideoLogic's Media Space ADPCM Structure definitions
   for  WAVE_FORMAT_MEDIASPACE_ADPCM    ($0012)

}
type
  PMediaSpace_ADPCMWaveFormat = ^TMediaSpace_ADPCMWaveFormat;
  TMediaSpace_ADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
      wRevision : WORD;
   end;

{
    Sierra Semiconductor

        for WAVE_FORMAT_SIERRA_ADPCM   ($0013)

}
type
  PSierra_ADPCMWaveFormat = ^TSierra_ADPCMWaveFormat;
  TSierra_ADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
      wRevision : WORD;
   end;


{
    Antex Electronics  structure definitions

        for WAVE_FORMAT_G723_ADPCM   ($0014)

}
type
  PG723_ADPCMWaveFormat = ^TG723_ADPCMWaveFormat;
  TG723_ADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
      cbExtraSize : WORD;
      nAuxBlockSize : WORD;
   end;


{
    DSP Solutions (formerly DIGISPEECH) structure definitions

       for WAVE_FORMAT_DIGISTD   ($0015)

}

type
  PDigistdWaveWaveFormat = ^TDigistdWaveFormat;
  TDigistdWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;

{
    DSP Solutions (formerly DIGISPEECH) structure definitions

        for WAVE_FORMAT_DIGIFIX   ($0016)

}
type
  PDigifixWaveWaveFormat = ^TDigifixWaveFormat;
  TDigifixWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
     Dialogic Corporation
   WAVEFORMAT_DIALOGIC_OKI_ADPCM   ($0017)
}
type
  PDialogicOkiADPCMWaveFormat = ^TDialogicOkiADPCMWaveFormat;
  TDialogicOkiADPCMWaveFormat = packed record
      ewf : TWaveFormatEx;
   end;

{
    Yamaha Compression's ADPCM structure definitions

        for WAVE_FORMAT_YAMAHA_ADPCM   ($0020)

}
type
  PYAMAHA_ADPCMWaveFormat = ^TYAMAHA_ADPCMWaveFormat;
  TYAMAHA_ADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
    Speech Compression's Sonarc structure definitions

        for WAVE_FORMAT_SONARC   ($0021)

}
type
  PSonarcWaveFormat = ^TSonarcWaveFormat;
  TSonarcWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wCompType : WORD;
   end;


{
    DSP Groups's TRUESPEECH structure definitions

        for WAVE_FORMAT_DSPGROUP_TRUESPEECH   ($0022)

}
type
  PTrueSpeechWaveFormat = ^TTrueSpeechWaveFormat;
  TTrueSpeechWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wRevision : WORD;
	  nSamplesPerBlock : WORD;
	  abReserved : array [0..27] of BYTE;
   end;

{
    Echo Speech Corp structure definitions

        for WAVE_FORMAT_ECHOSC1   ($0023)

}
type
  PEchosc1WaveFormat = ^TEchosc1WaveFormat;
  TEchosc1WaveFormat = packed record
      wfx : TWaveFormatEx;
   end;

{
    Audiofile Inc.structure definitions

        for WAVE_FORMAT_AUDIOFILE_AF36   ($0024)

}
type
  PAudioFile_Af36WaveFormat = ^TAudioFile_Af36WaveFormat;
  TAudioFile_Af36WaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
    Audio Processing Technology structure definitions

        for WAVE_FORMAT_APTX   ($0025)

}
type
  PAptWaveFormat = ^TAptWaveFormat;
  TAptWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;

{
    Audiofile Inc.structure definitions

        for WAVE_FORMAT_AUDIOFILE_AF10   ($0026)

}
type
  PAudioFile_Af10WaveFormat = ^TAudioFile_Af10WaveFormat;
  TAudioFile_Af10WaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
  Dolby's AC-2 wave format structure definition
           WAVE_FORMAT_DOLBY_AC2    ($0030)
}
type
  PDolbyAc2WaveFormat = ^TDolbyAc2WaveFormat;
  TDolbyAc2WaveFormat = packed record
      wfx : TWaveFormatEx;
	  nAuxBitsCode : WORD;
   end;


{
  Microsoft's
    WAVE_FORMAT_GSM 610           $0031
}
type
  PGSM610WaveFormat = ^TGSM610WaveFormat;
  TGSM610WaveFormat = packed record
      wfx : TWaveFormatEx;
	  wSamplePerBlock : WORD;
   end;


{
        Antex Electronics Corp

        for WAVE_FORMAT_ADPCME                  ($0033)

}
type
  PADPCMeWaveFormat = ^TADPCMeWaveFormat;
  TADPCMeWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wSamplePerBlock : WORD;
   end;


{
     Control Resources Limited
  	  WAVE_FORMAT_CONTROL_RES_VQLPC                 $0034
}
type
  PContresVqlpcWaveFormat = ^TContresVqlpcWaveFormat;
  TContresVqlpcWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wSamplePerBlock : WORD;
   end;


{


        for WAVE_FORMAT_DIGIREAL                   ($0035)

}
type
  PDigiRealWaveFormat = ^TDigiRealWaveFormat;
  TDigiRealWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wSamplePerBlock : WORD;
   end;

{
    DSP Solutions

        for WAVE_FORMAT_DIGIADPCM   ($0036)

}
type
  PDigiADPCMWaveFormat = ^TDigiADPCMWaveFormat;
  TDigiADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wSamplePerBlock : WORD;
   end;


{
     Control Resources Limited
     WAVE_FORMAT_CONTROL_RES_CR10          $0037
}
type
  PContrescr10WaveFormat = ^TContrescr10WaveFormat;
  TContrescr10WaveFormat = packed record
      wfx : TWaveFormatEx;
	  wSamplePerBlock : WORD;
   end;

{
    Natural Microsystems

        for WAVE_FORMAT_NMS_VBXADPCM   ($0038)

}
type
  PNMS_VbxADPCMWaveFormat = ^TNMS_VbxADPCMWaveFormat;
  TNMS_VbxADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wSamplePerBlock : WORD;
   end;

{
    Antex Electronics  structure definitions

        for WAVE_FORMAT_G721_ADPCM   ($0040)

}
type
  PG721_ADPCMWaveFormat = ^TG721_ADPCMWaveFormat;
  TG721_ADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
	  nAuxBlockSize : WORD;
   end;

{

   Microsoft MPEG audio WAV definition

   MPEG-1 audio wave format (audio layer only).   ($0050)

}
type
  PMPEG1WaveFormat = ^TMPEG1WaveFormat;
  TMPEG1WaveFormat = packed record
      wfx : TWaveFormatEx;
      fwHeadLayer : WORD;
      dwHeadBitrate : DWORD;
      fwHeadMode : WORD;
      fwHeadModeExt : WORD;
      wHeadEmphasis : WORD;
      fwHeadFlags : WORD;
      dwPTSLow : DWORD;
      dwPTSHigh : DWORD;
  end;

const
  ACM_MPEG_LAYER1       	= $0001;
  ACM_MPEG_LAYER2           = $0002;
  ACM_MPEG_LAYER3           = $0004;
  ACM_MPEG_STEREO           = $0001;
  ACM_MPEG_JOINTSTEREO      = $0002;
  ACM_MPEG_DUALCHANNEL      = $0004;
  ACM_MPEG_SINGLECHANNEL    = $0008;
  ACM_MPEG_PRIVATEBIT       = $0001;
  ACM_MPEG_COPYRIGHT        = $0002;
  ACM_MPEG_ORIGINALHOME     = $0004;
  ACM_MPEG_PROTECTIONBIT    = $0008;
  ACM_MPEG_ID_MPEG1         = $0010;

{
    Creative's ADPCM structure definitions

        for WAVE_FORMAT_CREATIVE_ADPCM   ($0200)

}
type
  PCreativeADPCMWaveFormat = ^TCreativeADPCMWaveFormat;
  TCreativeADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
	  wRevision : WORD;
   end;

{
      Creative FASTSPEECH
   WAVEFORMAT_CREATIVE_FASTSPEECH8   ($0202)
}
type
  PCreativeFastSpeech8WaveFormat = ^TCreativeFastSpeech8WaveFormat;
  TCreativeFastSpeech8WaveFormat = packed record
      wfx : TWaveFormatEx;
	  wRevision : WORD;
   end;


{
      Creative FASTSPEECH
   WAVEFORMAT_CREATIVE_FASTSPEECH10   ($0203)
}
type
  PCreativeFastSpeech10WaveFormat = ^TCreativeFastSpeech10WaveFormat;
  TCreativeFastSpeech10WaveFormat = packed record
      wfx : TWaveFormatEx;
	  wRevision : WORD;
   end;

{
    Fujitsu FM Towns 'SND' structure

        for WAVE_FORMAT_FMMTOWNS_SND   ($0300)

}
type
  PFMTOWNS_SND_WaveFormat = ^TFMTOWNS_SND_WaveFormat;
  TFMTOWNS_SND_WaveFormat = packed record
      wfx : TWaveFormatEx;
	  wRevision : WORD;
   end;


{
    Olivetti structure

        for WAVE_FORMAT_OLIGSM   ($1000)

}
type
  POliGSMWaveFormat = ^TOliGSMWaveFormat;
  TOliGSMWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
    Olivetti structure

        for WAVE_FORMAT_OLIADPCM   ($1001)

}
type
  POliADPCMWaveFormat = ^TOliADPCMWaveFormat;
  TOliADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
    Olivetti structure

        for WAVE_FORMAT_OLICELP   ($1002)

}
type
  POliCELPWaveFormat = ^TOliCELPWaveFormat;
  TOliCELPWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
    Olivetti structure

        for WAVE_FORMAT_OLISBC   ($1003)

}
type
  POliSBCWaveFormat = ^TOliSBCWaveFormat;
  TOliSBCWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;


{
    Olivetti structure

        for WAVE_FORMAT_OLIOPR   ($1004)

}
type
  POliOPRWaveFormat = ^TOliOPRWaveFormat;
  TOliOPRWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;

{
    Crystal Semiconductor IMA ADPCM format

        for WAVE_FORMAT_CS_IMAADPCM   ($0039)

}
type
  PCsIMAADPCMWaveFormat = ^TCsIMAADPCMWaveFormat;
  TCsIMAADPCMWaveFormat = packed record
      wfx : TWaveFormatEx;
   end;

{=========================================================================

   ACM Wave Filters

========================================================================== }

const
  WAVE_FILTER_UNKNOWN     = $0000;
  WAVE_FILTER_DEVELOPMENT = $FFFF;

type
  PWaveFilter = ^TWaveFilter;
  TWaveFilter = packed record
    cbStruct	: DWORD;           { Size of the filter in bytes }
    dwFilterTag : DWORD;           { filter type }
    fdwFilter	: DWORD;           { Flags for the filter (Universal Dfns) }
    dwReserved	: array [0..4] of DWORD;    	{ Reserved for system use }
   end;

const
  WAVE_FILTER_VOLUME = $0001;

type
  PVolumeWaveFilter = ^TVolumeWaveFilter;
  TVolumeWaveFilter = packed record
        wfltr	: TWaveFilter;
        dwVolume : DWORD;
   end;

const
  WAVE_FILTER_ECHO = $0002;

type
  PEchoWaveFilter = ^TEchoWaveFilter;
  TEchoWaveFilter = packed record
        wfltr	: TWaveFilter;
        dwVolume : DWORD;
		dwDelay : DWORD;
   end;

{

   New RIFF WAVE Chunks

}

const
  FOURCC_RIFFWAVE_inst = $74736E69;

type
	s_RIFFWAVE_inst = packed record
		bUnshiftedNote : BYTE;
    	chFineTune : shortint;
    	chGain : shortint;
    	bLowNote : BYTE;
    	bHighNote : BYTE;
    	bLowVelocity : BYTE;
    	bHighVelocity : BYTE;
	end;
	
{

   New RIFF Forms

}
{ RIFF AVI }

{
   AVI file format is specified in a seperate file (AVIFMT.H),
   which is available in the VfW and Win 32 SDK
}

{ RIFF CPPO }
const
  FOURCC_RIFFCPPO      = $4F505043;

  FOURCC_RIFFCPPO_objr = $726A626F;
  FOURCC_RIFFCPPO_obji = $696A626F;

  FOURCC_RIFFCPPO_clsr = $72736C63;
  FOURCC_RIFFCPPO_clsi = $69736C63;

  FOURCC_RIFFCPPO_mbr  = $2072626D;

  FOURCC_RIFFCPPO_char = $72616863;

  FOURCC_RIFFCPPO_byte = $65747962;
  FOURCC_RIFFCPPO_int  = $20746E69;
  FOURCC_RIFFCPPO_word = $64726F77;
  FOURCC_RIFFCPPO_long = $676E6F6C;
  FOURCC_RIFFCPPO_dwrd = $64727764;
  FOURCC_RIFFCPPO_flt  = $20746C66;
  FOURCC_RIFFCPPO_dbl  = $206C6264;
  FOURCC_RIFFCPPO_str  = $20727473;

{

   DIB Compression Defines

}
const
  BI_BITFIELDS = 3;

  QUERYDIBSUPPORT = 3073;
  QDI_SETDIBITS   = $0001;
  QDI_GETDIBITS   = $0002;
  QDI_DIBTOSCREEN = $0004;
  QDI_STRETCHDIB  = $0008;

{ Structure definitions }

type
  TExBmInfoHeader = packed record
        bmi : TBitMapInfoHeader;
        { extended BITMAPINFOHEADER fields }
        biExtDataOffset : DWORD;
        
        { Other stuff will go here }

        { ... }

        { Format-specific information }
        { biExtDataOffset points here }
  end;

{ New DIB Compression Defines }
const
  FOURCC_BICOMP_IBMULTIMOTION  = $49544C55; { 'ULTI' }
  FOURCC_BICOMP_IBMPHOTOMOTION = $4F4D4850; { 'PHMO' }
  FOURCC_BICOMP_CREATIVEYUV    = $76757963;	{ 'cyuv' }

{ New DIB Compression Defines }
  FOURCC_JPEG_DIB = $4745504A;		{ Still image JPEG DIB biCompression 'JPEG' }
  FOURCC_MJPG_DIB = $47504A4D;		{ Motion JPEG DIB biCompression 'MJPG' }

{ JPEGProcess Definitions }
  JPEG_PROCESS_BASELINE = 0;       	{ Baseline DCT }

{ AVI File format extensions }
  AVIIF_CONTROLFRAME	= $00000200;     { This is a control frame }

    { JIF Marker byte pairs in JPEG Interchange Format sequence }
const
  JIFMK_SOF0    = $FFC0;   { SOF Huff  - Baseline DCT}
  JIFMK_SOF1    = $FFC1;   { SOF Huff  - Extended sequential DCT}
  JIFMK_SOF2    = $FFC2;   { SOF Huff  - Progressive DCT}
  JIFMK_SOF3    = $FFC3;   { SOF Huff  - Spatial (sequential) lossless}
  JIFMK_SOF5    = $FFC5;   { SOF Huff  - Differential sequential DCT}
  JIFMK_SOF6    = $FFC6;   { SOF Huff  - Differential progressive DCT}
  JIFMK_SOF7    = $FFC7;   { SOF Huff  - Differential spatial}
  JIFMK_JPG     = $FFC8;   { SOF Arith - Reserved for JPEG extensions}
  JIFMK_SOF9    = $FFC9;   { SOF Arith - Extended sequential DCT}
  JIFMK_SOF10   = $FFCA;   { SOF Arith - Progressive DCT}
  JIFMK_SOF11   = $FFCB;   { SOF Arith - Spatial (sequential) lossless}
  JIFMK_SOF13   = $FFCD;   { SOF Arith - Differential sequential DCT}
  JIFMK_SOF14   = $FFCE;   { SOF Arith - Differential progressive DCT}
  JIFMK_SOF15   = $FFCF;   { SOF Arith - Differential spatial}
  JIFMK_DHT     = $FFC4;   { Define Huffman Table(s) }
  JIFMK_DAC     = $FFCC;   { Define Arithmetic coding conditioning(s) }
  JIFMK_RST0    = $FFD0;   { Restart with modulo 8 count 0 }
  JIFMK_RST1    = $FFD1;   { Restart with modulo 8 count 1 }
  JIFMK_RST2    = $FFD2;   { Restart with modulo 8 count 2 }
  JIFMK_RST3    = $FFD3;   { Restart with modulo 8 count 3 }
  JIFMK_RST4    = $FFD4;   { Restart with modulo 8 count 4 }
  JIFMK_RST5    = $FFD5;   { Restart with modulo 8 count 5 }
  JIFMK_RST6    = $FFD6;   { Restart with modulo 8 count 6 }
  JIFMK_RST7    = $FFD7;   { Restart with modulo 8 count 7 }
  JIFMK_SOI     = $FFD8;   { Start of Image }
  JIFMK_EOI     = $FFD9;   { End of Image }
  JIFMK_SOS     = $FFDA;   { Start of Scan }
  JIFMK_DQT     = $FFDB;   { Define quantization Table(s) }
  JIFMK_DNL     = $FFDC;   { Define Number of Lines }
  JIFMK_DRI     = $FFDD;   { Define Restart Interval }
  JIFMK_DHP     = $FFDE;   { Define Hierarchical progression }
  JIFMK_EXP     = $FFDF;   { Expand Reference Component(s) }
  JIFMK_APP0    = $FFE0;   { Application Field 0}
  JIFMK_APP1    = $FFE1;   { Application Field 1}
  JIFMK_APP2    = $FFE2;   { Application Field 2}
  JIFMK_APP3    = $FFE3;   { Application Field 3}
  JIFMK_APP4    = $FFE4;   { Application Field 4}
  JIFMK_APP5    = $FFE5;   { Application Field 5}
  JIFMK_APP6    = $FFE6;   { Application Field 6}
  JIFMK_APP7    = $FFE7;   { Application Field 7}
  JIFMK_JPG0    = $FFF0;   { Reserved for JPEG extensions }
  JIFMK_JPG1    = $FFF1;   { Reserved for JPEG extensions }
  JIFMK_JPG2    = $FFF2;   { Reserved for JPEG extensions }
  JIFMK_JPG3    = $FFF3;   { Reserved for JPEG extensions }
  JIFMK_JPG4    = $FFF4;   { Reserved for JPEG extensions }
  JIFMK_JPG5    = $FFF5;   { Reserved for JPEG extensions }
  JIFMK_JPG6    = $FFF6;   { Reserved for JPEG extensions }
  JIFMK_JPG7    = $FFF7;   { Reserved for JPEG extensions }
  JIFMK_JPG8    = $FFF8;   { Reserved for JPEG extensions }
  JIFMK_JPG9    = $FFF9;   { Reserved for JPEG extensions }
  JIFMK_JPG10   = $FFFA;   { Reserved for JPEG extensions }
  JIFMK_JPG11   = $FFFB;   { Reserved for JPEG extensions }
  JIFMK_JPG12   = $FFFC;   { Reserved for JPEG extensions }
  JIFMK_JPG13   = $FFFD;   { Reserved for JPEG extensions }
  JIFMK_COM     = $FFFE;   { Comment }
  JIFMK_TEM     = $FF01;   { for temp private use arith code }
  JIFMK_RES     = $FF02;   { Reserved }
  JIFMK_00      = $FF00;   { Zero stuffed byte - entropy data }
  JIFMK_FF      = $FFFF;   { Fill byte }

{ JPEGColorSpaceID Definitions }
  JPEG_Y        = 1;       { Y only component of YCbCr }
  JPEG_YCbCr    = 2;       { YCbCr as define by CCIR 601 }
  JPEG_RGB      = 3;       { 3 component RGB }

{ Structure definitions }
type
	JPEGInfoHeader = packed record
    	{ compression-specific fields }
    	{ these fields are defined for 'JPEG' and 'MJPG' }
    	JPEGSize : DWORD;
    	JPEGProcess : DWORD;

    	{ Process specific fields }
    	JPEGColorSpaceID : DWORD;
    	JPEGBitsPerSample : DWORD;
    	JPEGHSubSampling : DWORD;
    	JPEGVSubSampling : DWORD;
	end;

{ Default DHT Segment }

const
  MJPGHDTSEG_STORAGE : array [0..$1A3] of byte = (
 { JPEG DHT Segment for YCrCb omitted from MJPG data }
$FF,$C4,$01,$A2,
$00,$00,$01,$05,$01,$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$00,$00,
$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$01,$00,$03,$01,$01,$01,$01,
$01,$01,$01,$01,$01,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$05,$06,$07,
$08,$09,$0A,$0B,$10,$00,$02,$01,$03,$03,$02,$04,$03,$05,$05,$04,$04,$00,
$00,$01,$7D,$01,$02,$03,$00,$04,$11,$05,$12,$21,$31,$41,$06,$13,$51,$61,
$07,$22,$71,$14,$32,$81,$91,$A1,$08,$23,$42,$B1,$C1,$15,$52,$D1,$F0,$24,
$33,$62,$72,$82,$09,$0A,$16,$17,$18,$19,$1A,$25,$26,$27,$28,$29,$2A,$34,
$35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,$47,$48,$49,$4A,$53,$54,$55,$56,
$57,$58,$59,$5A,$63,$64,$65,$66,$67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,
$79,$7A,$83,$84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,$98,$99,
$9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,$B4,$B5,$B6,$B7,$B8,$B9,
$BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,$D9,
$DA,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$F1,$F2,$F3,$F4,$F5,$F6,$F7,
$F8,$F9,$FA,$11,$00,$02,$01,$02,$04,$04,$03,$04,$07,$05,$04,$04,$00,$01,
$02,$77,$00,$01,$02,$03,$11,$04,$05,$21,$31,$06,$12,$41,$51,$07,$61,$71,
$13,$22,$32,$81,$08,$14,$42,$91,$A1,$B1,$C1,$09,$23,$33,$52,$F0,$15,$62,
$72,$D1,$0A,$16,$24,$34,$E1,$25,$F1,$17,$18,$19,$1A,$26,$27,$28,$29,$2A,
$35,$36,$37,$38,$39,$3A,$43,$44,$45,$46,$47,$48,$49,$4A,$53,$54,$55,$56,
$57,$58,$59,$5A,$63,$64,$65,$66,$67,$68,$69,$6A,$73,$74,$75,$76,$77,$78,
$79,$7A,$82,$83,$84,$85,$86,$87,$88,$89,$8A,$92,$93,$94,$95,$96,$97,$98,
$99,$9A,$A2,$A3,$A4,$A5,$A6,$A7,$A8,$A9,$AA,$B2,$B3,$B4,$B5,$B6,$B7,$B8,
$B9,$BA,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$D2,$D3,$D4,$D5,$D6,$D7,$D8,
$D9,$DA,$E2,$E3,$E4,$E5,$E6,$E7,$E8,$E9,$EA,$F2,$F3,$F4,$F5,$F6,$F7,$F8,
$F9,$FA
);
{

 Defined IC types

}
const
  FOURCC_ICTYPE_VIDEO = $63646976;		{ 'vidc' }
  FOURCC_ICTYPE_AUDIO = $63647561;		{ 'audc' }

{
   Misc. FOURCC registration
}

{ Sierra Semiconductor: RDSP- Confidential RIFF file format
         for the storage and downloading of DSP
         code for Audio and communications devices.
}
const
  FOURCC_RDSP = $50534452;				{ 'RDSP' }

  MIXERCONTROL_CONTROLTYPE_SRS_MTS = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 6);
  MIXERCONTROL_CONTROLTYPE_SRS_ONOFF = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 7);
  MIXERCONTROL_CONTROLTYPE_SRS_SYNTHSELECT = (MIXERCONTROL_CONTROLTYPE_BOOLEAN + 8);

implementation

end.
