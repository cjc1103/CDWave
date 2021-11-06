program APETest;

{$APPTYPE CONSOLE}

uses
  SysUtils, APE;

const
    filename = 'F:\Hawaii 5-0.ape';

var r: Integer;
    hAPEDecompress: APE_DECOMPRESS_HANDLE;

begin
try
  if LibAPELoaded then
  begin
    r := APEGetVersionNumber;
    WriteLn('APE Library loaded, version: ', r);
    if @APEDecompress_Create <> nil then WriteLn('APEDecompress_Create Loaded OK');
    hAPEDecompress := APEDecompress_Create(PChar(FileName), @r);
    if (hAPEDecompress = nil) then
        raise Exception.CreateFmt('APEDecompress_Create failed, code: %d.', [r]);
    try
        WriteLn('Opened file: ', FileName);
        r := APEDecompress_GetInfo(hAPEDecompress, APE_DECOMPRESS_TOTAL_BLOCKS, 0, 0);
        WriteLn('APE_DECOMPRESS_TOTAL_BLOCKS: ', r);
    finally
        APEDecompress_Destroy(hAPEDecompress);
    end;
  end else
    WriteLn('APE Library not loaded.');
finally
  ReadLn;
end;
end.
