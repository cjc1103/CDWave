unit buffers;

interface

uses WaveUtil, MMSystem;

type
  TWaveBuffers = array[0..maxint div sizeof(Integer)-1] of PWaveHdr;

  TWaveBufferList = class(TObject)

  end;

implementation

end.
 