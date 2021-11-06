unit Properties;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TFilePropertiesForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lSamplesPerSec: TStaticText;
    lBitsPerSample: TStaticText;
    lChannels: TStaticText;
    lAvgBytesPerSec: TStaticText;
    BitBtn1: TBitBtn;
    Label5: TLabel;
    Label6: TLabel;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    lWaveSize: TStaticText;
    lTotalTime: TStaticText;
    GroupBox3: TGroupBox;
    Label9: TLabel;
    lMaxSampleValue: TStaticText;
    lMaxSamplePercent: TStaticText;
    Label10: TLabel;
    Label11: TLabel;
    lMinSampleValue: TStaticText;
    Label12: TLabel;
    lMinSamplePercent: TStaticText;
    Label13: TLabel;
    Label14: TLabel;
    lClippedSectors: TStaticText;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
