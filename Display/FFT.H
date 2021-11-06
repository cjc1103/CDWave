#include <math.h>

#define      PI          3.14159265358979                         

#define FLOAT double

#define MINFFTSIZE         64
#define MAXFFTSIZE      16384
#define LOGMAXFFTSIZE      14

#define WI_BOX      0       // box window
#define WI_TRIANG   1       // triangular window
#define WI_HANN     2       // hanning window
// ... more to follow

#define ASC_LIN     0       // amplitude scaling: linear
#define ASC_SQRT    1       // amplitude scaling: sqrt
#define ASC_LOG     2       // amplitude scaling: log.

#define FSC_LIN     0       // frequency scaling: linear
#define FSC_LOG     1       // frequency scaling: log.

typedef short   SValue;

void     fft( long windowtype, long scaletype, long size, SValue *in, unsigned char *out );

//////////////////////////////////////////////////////////////
