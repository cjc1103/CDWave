unit Mixer;

interface

uses SysUtils, Windows, MMSystem;

type
   TMixerControlDetailsUnion = record
     case integer of
     0: (Unsigned: DWORD);
     1: (Signed:   LongInt);
     2: (Bool:     LongBool);
   end;

   TAudioMixerLine = class;
   TAudioMixerControl = class;

   TAudioMixer = class
   private
     FHandle: HMIXER;
     FName: string;
     FCount: Cardinal;
     FDestinations: array of TAudioMixerLine;
    function GetIsOpen: Boolean;
   private
     function GetDestination(index: Integer): TAudioMixerLine;
   public
     destructor Destroy; override;
     procedure Open(MxId, dwCallback, fdwOpen: DWORD);
     procedure Close;
     property IsOpen: Boolean read GetIsOpen;
     property Handle: HMIXER read FHandle;
     property Name: string read FName;
     property Count: Cardinal read FCount;
     property Destination[index: Integer]: TAudioMixerLine read GetDestination; default;
     function FindControl(id: Cardinal): TAudioMixerControl;
   end;

   TAudioMixerLine = class
   private
     FOwner: TAudioMixer;
     FMixerLine: TMixerLine;
     FName: string;
     FControls: array of TAudioMixerControl;
     FMixerControlArray: array of TMIXERCONTROL;
     FConnections: array of TAudioMixerLine;
     function GetConnections(index: Integer): TAudioMixerLine;
   private
     function GetControls(index: Integer): TAudioMixerControl;
     procedure Initialize;
   public
     constructor Create(AOwner: TAudioMixer; index: Integer);
     constructor CreateTo(ADest: TAudioMixerLine; srcIndex, dstIndex: Cardinal);
     destructor Destroy; override;
     property Name: string read FName;
     property Channels: Cardinal read FMixerLine.cChannels;
     property Count: Cardinal read FMixerLine.cControls;
     property LineID: Cardinal read FMixerLine.dwLineID;
     property Controls[index: Integer]: TAudioMixerControl read GetControls; default;
     property ConnectionsCount: Cardinal read FMixerLine.cConnections;
     property Connections[index: Integer]: TAudioMixerLine read GetConnections;
     property Owner: TAudioMixer read FOwner;
     function FindControl(id: Cardinal): TAudioMixerControl;
   end;

   // Unknown control type (also base class)
   TAudioMixerControl = class
   private
     FOwner: TAudioMixerLine;
     FMixerControl: PMixerControl;
     FID: Cardinal;
     FName: string;
     function GetIsMultiple: boolean;
     function GetMultipleItems: Cardinal;
     function GetIsUniform: boolean;
     function GetValueCount: Cardinal;
   protected
     procedure Initialize; virtual;
   public
     FValues: array of TMixerControlDetailsUnion;
     constructor Create(AOwner: TAudioMixerLine; info: PMixerControl);
     property Name: string read FName;
     property ID: Cardinal read FID;
     property IsMultiple: boolean read GetIsMultiple;
     property IsUniform: boolean read GetIsUniform;
     property MultipleItems: Cardinal read GetMultipleItems;
     property ValueCount: Cardinal read GetValueCount;
     procedure GetValues;
     procedure SetValues;
   end;

   TAudioFaderControl = class(TAudioMixerControl)
  private
    function GetSteps: Cardinal;
   public
     property Steps: Cardinal read GetSteps;
   end;

   TAudioSwitchControl = class(TAudioMixerControl)
   end;

  TAudioListControl = class(TAudioMixerControl)
  private
    FNames: array of string;
    function GetName(index: integer): string;
    function GetIsMultiChoice: boolean;
  protected
    procedure Initialize; override;
  public
    property Name[index: integer]: string read GetName;
    property IsMultiChoice: boolean read GetIsMultiChoice;
  end;

implementation

type TMIXERCONTROLArray = array[0..0] of TMIXERCONTROL;
     PMIXERCONTROLArray = ^TMIXERCONTROLArray;

     TErrorRec = record
       No: DWORD;
       Msg: string;
     end;

const ErrorListCount = 7;
const ErrorList: array[0..ErrorListcount-1] of TErrorRec =
  ((No:MMSYSERR_ALLOCATED; Msg: 'The specified resource is already allocated by the maximum number of clients possible.'),
   (No:MMSYSERR_BADDEVICEID; Msg: 'The uMxId parameter specifies an invalid device identifier.'),
   (No:MMSYSERR_INVALFLAG; Msg: 'One or more flags are invalid.'),
   (No:MMSYSERR_INVALHANDLE; Msg: 'The uMxId parameter specifies an invalid handle.'),
   (No:MMSYSERR_INVALPARAM; Msg: 'One or more parameters are invalid.'),
   (No:MMSYSERR_NODRIVER; Msg: 'No mixer device is available for the object specified by uMxId. Note that the location referenced by uMxId will also contain the value  - 1.'),
   (No:MMSYSERR_NOMEM; Msg: 'Unable to allocate resources.'));
function MMMessage(code: MMRESULT): string;
var i: Integer;
begin
  for i := 0 to ErrorListCount-1 do
    if ErrorList[i].No = code then
      begin
        Result := ErrorList[i].Msg;
        Exit;
      end;
  Result := 'Multimedia error occured, Unknown MM Error code: ' + IntToStr(code);
end;

procedure MM(code: MMResult);
begin
  if (code <> 0) then
    raise Exception.Create(MMMessage(code));
end;

{ TAudioMixer }

procedure TAudioMixer.Close;
var i: Integer;
begin
  if IsOpen then
  begin
    for i := 0 to Count-1 do
    begin
      FDestinations[i].Free;
    end;
    mixerClose(FHandle);
    FHandle := 0;
    FName := '';
    FCount := 0;
  end;
end;

destructor TAudioMixer.Destroy;
begin
  if IsOpen then Close;
  inherited;
end;

function TAudioMixer.FindControl(id: Cardinal): TAudioMixerControl;
var i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := self[i].FindControl(id);
    if (Result <> nil) then Exit;
  end;
  Result := nil;
end;

function TAudioMixer.GetDestination(index: Integer): TAudioMixerLine;
begin
  Result := FDestinations[index];
end;

function TAudioMixer.GetIsOpen: Boolean;
begin
  Result := Handle <> 0;
end;

procedure TAudioMixer.Open(MxId, dwCallback, fdwOpen: DWORD);
var FCaps: TMixerCAPS;
    i: Integer;
begin
  if IsOpen then Close;
  MM(mixerOpen(@FHandle, MxId, dwCallback, DWORD(@self), fdwOpen));
  MM(mixerGetDevCaps(FHandle, @FCaps, sizeof(TMIXERCAPS)));
  FName := FCaps.szPname;
  FCount := FCaps.cDestinations;
  SetLength(FDestinations, FCaps.cDestinations);
  for i := 0 to Count-1 do
  begin
    FDestinations[i] := TAudioMixerLine.Create(self, i);
  end;
end;

{ TAudioMixerLine }

constructor TAudioMixerLine.Create(AOwner: TAudioMixer; index: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FMixerLine.cbStruct := sizeof(FMixerLine);
  FMixerLine.dwDestination := index;
  MM(mixerGetLineInfo(AOwner.Handle, @FMixerLine, MIXER_GETLINEINFOF_DESTINATION));
  Initialize;
end;

constructor TAudioMixerLine.CreateTo(ADest: TAudioMixerLine; srcIndex,
  dstIndex: Cardinal);
begin
  inherited;
  FOwner := ADest.FOwner;
  FMixerLine.cbStruct := sizeof(FMixerLine);
  FMixerLine.dwDestination := dstIndex;
  FMixerLine.dwSource := srcIndex;
  MM(mixerGetLineInfo(FOwner.Handle, @FMixerLine, MIXER_GETLINEINFOF_SOURCE));
  Initialize;
end;

procedure TAudioMixerLine.Initialize;
var controls: TMixerLineControls;
    i: Integer;
    control: TAudioMixerControl;
    info: PMixerControl;
begin
  FName := FMixerLine.szName;
  if (FMixerLine.cControls <> 0) then
  begin
    SetLength(FMixerControlArray, FMixerLine.cControls);
    SetLength(FControls, FMixerLine.cControls);
    controls.cbStruct := sizeof(controls);
    controls.dwLineID := LineID;
    controls.cControls := Count;
    controls.cbmxctrl := sizeof(TMIXERCONTROL);
    controls.pamxctrl := @FMixerControlArray[0];
    MM(mixerGetLineControls(FOwner.Handle, @controls, MIXER_GETLINECONTROLSF_ALL));
    for i := 0 to Count-1 do
    begin
      info := @FMixerControlArray[i];
      case (FMixerControlArray[i].dwControlType and MIXERCONTROL_CT_CLASS_MASK) of
        MIXERCONTROL_CT_CLASS_FADER:
          control := TAudioFaderControl.Create(self, info);
        MIXERCONTROL_CT_CLASS_SWITCH:
          control := TAudioSwitchControl.Create(self, info);
        MIXERCONTROL_CT_CLASS_LIST:
          control := TAudioListControl.Create(self, info);
      else
        control := TAudioMixerControl.Create(self, info);
      end;
      FControls[i] := control;
    end;
  end;
  if (ConnectionsCount <> 0) then
  begin
    SetLength(FConnections, ConnectionsCount);
    for i := 0 to ConnectionsCount-1 do
    begin
      FConnections[i] := TAudioMixerLine.CreateTo(self, i, FMixerLine.dwDestination);
    end;
  end;
end;


function TAudioMixerLine.GetControls(index: Integer): TAudioMixerControl;
begin
  Result := FControls[index];
end;

function TAudioMixerLine.GetConnections(index: Integer): TAudioMixerLine;
begin
  result := FConnections[index];
end;


function TAudioMixerLine.FindControl(id: Cardinal): TAudioMixerControl;
var i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    if Controls[i].ID = id then
    begin
      Result := Controls[i];
      Exit;
    end;
  end;
  for i := 0 to ConnectionsCount-1 do
  begin
    Result := Connections[i].FindControl(id);
    if (Result <> nil) then Exit;
  end;
  Result := nil;
end;

destructor TAudioMixerLine.Destroy;
var i: Integer;
begin
  for i := 0 to Count-1 do
    Controls[i].Free;
  for i := 0 to ConnectionsCount-1 do
    Connections[i].Free;
  inherited;
end;

{ TAudioMixerControl }

constructor TAudioMixerControl.Create;
begin
  inherited Create;
  FOwner := AOwner;
  FMixerControl := info;
  FName := info.szName;
  FID := info.dwControlID;
  SetLength(FValues, ValueCount);
  Initialize;
end;

function TAudioMixerControl.GetIsMultiple: boolean;
begin
  Result :=
   (FMixerControl.fdwControl and MIXERCONTROL_CONTROLF_MULTIPLE) <> 0;
end;

function TAudioMixerControl.GetIsUniform: boolean;
begin
  Result :=
   (FMixerControl.fdwControl and MIXERCONTROL_CONTROLF_UNIFORM) <> 0;
end;

function TAudioMixerControl.GetMultipleItems: Cardinal;
begin
  Result := FMixerControl.cMultipleItems;
end;

function TAudioMixerControl.GetValueCount: Cardinal;
begin
  if IsMultiple then
  begin
    if IsUniform then
      Result := MultipleItems
    else
      Result := FOwner.Channels * MultipleItems;
  end else begin
    if IsUniform then
      Result := 1
    else
      Result := FOwner.Channels;
  end;
end;

procedure TAudioMixerControl.GetValues;
var
    details: TMIXERCONTROLDETAILS;
begin
    details.cbStruct := sizeof(details);
    details.dwControlID := ID;
    if IsUniform then
      details.cChannels := 1
    else
      details.cChannels := FOwner.Channels;
    details.cMultipleItems := MultipleItems;
    details.cbDetails := sizeof(TMixerControlDetailsUnion);
    details.paDetails := @FValues[0];
    MM(mixerGetControlDetails(FOwner.Owner.Handle, @details,
         MIXER_GETCONTROLDETAILSF_VALUE));
end;

procedure TAudioMixerControl.Initialize;
begin
 // doesn't do anything...
end;

procedure TAudioMixerControl.SetValues;
var
    details: TMIXERCONTROLDETAILS;
begin
    details.cbStruct := sizeof(details);
    details.dwControlID := ID;
    if IsUniform then
      details.cChannels := 1
    else
      details.cChannels := FOwner.Channels;
    details.cMultipleItems := MultipleItems;
    details.cbDetails := sizeof(TMixerControlDetailsUnion);
    details.paDetails := @FValues[0];
    MM(mixerSetControlDetails(FOwner.Owner.Handle, @details,
         MIXER_SETCONTROLDETAILSF_VALUE));
end;

{ TAudioListControl }

function TAudioListControl.GetIsMultiChoice: boolean;
begin
  Result :=
    (FMixerControl.dwControlType and MIXERCONTROL_CT_SC_LIST_MULTIPLE) <> 0;
end;

function TAudioListControl.GetName(index: integer): string;
begin
  Result := FNames[index];
end;

procedure TAudioListControl.Initialize;
var
    size, i: Cardinal;
    details: TMIXERCONTROLDETAILS;
    list: array of MIXERCONTROLDETAILS_LISTTEXT;
begin
  inherited;
  size := ValueCount;
  SetLength(list, size);
  try
    details.cbStruct := sizeof(details);
    details.dwControlID := ID;
    if IsUniform then
      details.cChannels := 1
    else
      details.cChannels := FOwner.Channels;
    details.cMultipleItems := MultipleItems;
    details.cbDetails := sizeof(MIXERCONTROLDETAILS_LISTTEXT);
    details.paDetails := @list[0];
    MM(mixerGetControlDetails(FOwner.Owner.Handle, @details,
         MIXER_GETCONTROLDETAILSF_LISTTEXT));
    SetLength(FNames, size);
    for i := 0 to size-1 do
      FNames[i] := list[i].szName;
  finally
    SetLength(list, 0);
  end;
end;

{ TAudioFaderControl }

function TAudioFaderControl.GetSteps: Cardinal;
begin
  Result := FMixerControl.Metrics.cSteps;
end;

end.
