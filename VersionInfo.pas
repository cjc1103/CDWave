unit VersionInfo;

interface

uses Windows;

type
  TVersionInfo = class
    constructor Create;
    destructor Destroy; override;
  private
    size: Cardinal;
    buffer: Pointer;
    function GetItem(index: string): string;
    function GetProductVersion: string;
  public
    FixedFileInfo: ^VS_FIXEDFILEINFO;
    property Item[index: string]: string read GetItem; default;
    property ProductVersion: string read GetProductVersion;
  end;

implementation

constructor TVersionInfo.Create;
var handle: Cardinal;
    filename: PChar;
begin
  filename := PChar(paramStr(0));
  size := GetFileVersionInfoSize(filename, handle);
  GetMem(Buffer, size);
  GetFileVersionInfo(filename, handle, size, Buffer);
  VerQueryValue(buffer, '\', pointer(FixedFileInfo), size);
  inherited;
end;

destructor TVersionInfo.Destroy;
begin
  FreeMem(Buffer, size);
  inherited;
end;

function TVersionInfo.GetItem;
var
  Data: PChar;
begin
  VerQueryValue(buffer, PChar('\StringFileInfo\040904E4\' + index), pointer(Data), size);
  Result := Data;
end;

function TVersionInfo.GetProductVersion;
begin
  Result := Item['ProductVersion'];
  if (FixedFileInfo.dwFileFlags and VS_FF_PRERELEASE) <> 0 then
    Result := Result + ' beta';
end;

end.



