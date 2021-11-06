// PLUGIN DISPLAY-DLL FOR 'CDWAVE'
//

#include "stdafx.h"

#include "display.h"
#include "fft.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define SPS         44100           // samples per second
#define PACKETSIZE  (SPS * 1)       // # of samples to be used for computing 1 statistical packet
#define BANDS_PP    256             // # of frequency bands per packet (each of size 1 byte)


typedef unsigned char   uchar;


typedef struct
{
    long    totalsamples;       // total size of sound file
    long    seen;               // # of samples already seen by ANALYZE()
    long    numpackets;         // # of statistical packets
    uchar   *data;              // stat. data
    short   *mixbuffer;         // stereo to mono downmix buffer
    uchar   *fftout;            // frequ. ampl. output of FFT (half the number of input values!)
} DLLSTATE;


typedef struct
{
    long    ascale;         // scaling for amplitude (ASC_...)
    long    fscale;         // scaling for frequency (FSC_...)
    long    wtype;          // window function (WI_...)
    long    fftsize;        // # of values to do a FFT over
} DISPLAYPARAMS;

static DISPLAYPARAMS DspPar;
static DISPLAYPARAMS DefaultPar = { ASC_LOG, FSC_LIN, WI_HANN, 512 };

///////////////////////////////////////////////////////////////////////////////
/*
numsamples (IN):            total size of sound file just opened
preferredchunksize (OUT):
ref (OUT):                  ptr. to internal state
The DLL tells you how many samples it wants each time you call ANALYZE(). 
My favourite value is now 44100.
At a later stage you could also supply the format of the WAV-data and I 
could be more flexible with my preferredchunksize.
*/

extern "C" long WINAPI  START( long numsamples, long *preferredchunksize, void **ref )
{
    DLLSTATE    *ps;

    ps = (DLLSTATE*) malloc(sizeof(DLLSTATE));
    if( ps==NULL )
        return FAIL;

    memset(ps,0,sizeof(DLLSTATE));

    ps->totalsamples = numsamples;
    ps->seen = 0;
    ps->numpackets = numsamples / PACKETSIZE;

    ps->data = (uchar*) malloc(ps->numpackets*BANDS_PP);    // allocate stat. data
    if( ps->data==NULL )
        goto fail;

    ps->mixbuffer = (short*) malloc(PACKETSIZE*sizeof(short));
    ps->fftout = (uchar*) malloc((MAXFFTSIZE/2)*sizeof(uchar));
    if( ps->mixbuffer==NULL || ps->fftout==NULL )
        goto fail;

    *preferredchunksize = PACKETSIZE;
    *ref = (void*) ps;
    return SUCCESS;

fail:
    free(ps->data);
    free(ps->mixbuffer);
    free(ps->fftout);

    return FAIL;
}

///////////////////////////////////////////////////////////////////////////////
/*
parameters (IN/OUT):
    If *paramsize==0, the DLL knows that no parameters have been retrieved by CDWAV
    from long-time storage. So the DLL just creates a default parameter set and 
    returns a pointer to them in *parameters; *paramsize receives the size of them.

    If *paramsize!=0,the DLL knows that CDWAV retrieved a parameter set. 
    So I'm able to copy this set from *parameters.

    CDWAV should call this function once BEFORE any call to START() and every time 
    the user chooses to load a stored parameter set from disk.
    As you pointed already out, those parameters are used globally for all views of CDWAV.
*/

extern "C" long WINAPI  INITPARAMS( void **parameters, long *paramsize )
{
    if( *paramsize==0 )
    {
        DspPar = DefaultPar;
        *parameters = (void*) &DspPar;
        *paramsize = sizeof(DspPar);
    }
    else
        memcpy(&DspPar,*parameters,sizeof(DspPar));

    return SUCCESS;
}

///////////////////////////////////////////////////////////////////////////////
/*
ref (INPUT):        void-pointer to my internal state.
numsamples (INPUT): number of STEREO samples which are pointed to by <data>.
                    if for example numsamples==44100, <data> should point to 44100*2*2
                    bytes of data. Left and right channel data are interleaved like in 
                    a normal WAV-file. I assume that each time I get called, <data>
                    holds the next sequential chunk of data from the WAV-file.
data (INPUT):       pointer to data.
RETURN-value:       SUCCESS, if everthing went fine;
                    FAIL, if something went wrong
*/

extern "C" long WINAPI  ANALYZE( void *ref, long numsamples, short *data )
{
    long        total, seen, i, j, skip, idx, noutvalues;
    uchar       *dataptr, val;
    short       *pmix;
    DLLSTATE    *ps;

    ps = (DLLSTATE*) ref;

    total = ps->totalsamples;   // # of samples in sound file
    seen = ps->seen;            // # of samples already seen

    if( seen+PACKETSIZE>total )
        return SUCCESS;       // ignore last data packet if it has 'odd' size

    if( numsamples!=PACKETSIZE )
        return FAIL;      // wrong packet size

    pmix = ps->mixbuffer;
    for( i=0; i<PACKETSIZE; i++,data+=2 )   // stereo --> mono downmix
        *pmix++ = (data[0]+data[1])/2;

    // later we may do many FFT's per PACKETSIZE, depending on some display precision parameter
    fft(DspPar.wtype,DspPar.ascale,DspPar.fftsize, ps->mixbuffer,ps->fftout);

    dataptr = ps->data + BANDS_PP*(seen/PACKETSIZE);    // ptr. to current stat. packet (contains BANDS_PP values)
    noutvalues = DspPar.fftsize/2;
    // later: do LOG. scaling of frequ. domain
    if( noutvalues<BANDS_PP )
    {
        skip = BANDS_PP / noutvalues;
        for( idx=0,i=0; i<noutvalues; i++ )
        {
            val = ps->fftout[i];
            for( j=0; j<skip; j++ )         // later we may do some smoothing
                dataptr[idx++] = val;
        }
    }
    else
    {
        skip = noutvalues / BANDS_PP;
        for( idx=0,i=0; i<noutvalues; i+=skip )   // later we may do some smoothing
            dataptr[idx++] = ps->fftout[i];
    }

    ps->seen += PACKETSIZE;

    return SUCCESS;		// assume everything went fine
}

///////////////////////////////////////////////////////////////////////////////
/*
ref (INPUT):        void-pointer to my internal state.
parameters (INPUT/OUTPUT):
                    pointer to parameter values (their size agreed upon in INITPARAMS())
                    representing the current display parameters
parent (INPUT):     handle of parent window (CDWAV's main window)
RETURN-value:       SUCCESS, if everthing went fine;
                    FAIL, if something went wrong
*/

extern "C" long WINAPI   USERUI( void *ref, char *parameters, HWND parent )
{
    ::MessageBox(parent,"still not finished","Display Parameters",MB_OK);
    return SUCCESS;
}


/////////////////////////////////////////////////////////////////////////////

static void *_pbmdata = NULL;
static int  _w, _h, _depth, _bpl;

static int  CreateBitmapData( int w, int h, int depth )
{
    int     size;

    switch(depth)
    {
        case 8:  _bpl = w + (w&1); break;
        case 16: _bpl = 2*w; break;
        case 24: _bpl = 3*w + (w&1); break;
    }

    size = _bpl*h;
    _pbmdata = (void*) malloc(size);
    _w = w; _h = h; _depth = depth;

    return( _pbmdata!=NULL );
}

/////////////////////////////////////////////////////////////////////////////

static void DeleteBitmapData( void )
{
    free(_pbmdata);
}

/////////////////////////////////////////////////////////////////////////////

static void DrawBitmapPoint( int x, int y, unsigned char b, unsigned char g, unsigned char r )
{
    unsigned char   *p;

    if( x<0 || x>=_w || y<0 || y>=_h )
        return;

    switch(_depth)
    {
        case 8:
            p = ((unsigned char*)_pbmdata) + y*_bpl + x;
            r = (r>127) ? 1 : 0;
            g = (g>127) ? 2 : 0;
            b = (b>127) ? 4 : 0;
            *p = r|g|b;
            break;

        case 16:
            p = ((unsigned char*)_pbmdata) + y*_bpl + 2*x;
            r >>= 3;
            g >>= 3;
            b >>= 3;
            *((unsigned short*)p) = (r<<10) | (g<<5) | b;
            break;

        case 24:
            p = ((unsigned char*)_pbmdata) + y*_bpl + 3*x;
            *p++ = r;
            *p++ = g;
            *p++ = b;
            break;
    }
}

/////////////////////////////////////////////////////////////////////////////

static void DrawTestimage( void )
{
    int     x, y, xmid, ymid, dist;
    unsigned char   r, g, b;

    xmid = _w/2;
    ymid = _h/2;

    for( y=0; y<_h; y++ )
        for( x=0; x<_w; x++ )
        {
            r = ((_w-x)*255) / _w;
            g = (y<ymid-20 || y>ymid+20) ? 0 : x&0xFF;
            b = (y*255) / _h;
            dist = (x-xmid)*(x-xmid) + (y-ymid)*(y-ymid);
            if( dist<256 )
                DrawBitmapPoint(x,y, r,g,dist);  // blue circle
            else
                DrawBitmapPoint(x,y, r,g,b);
        }
}

///////////////////////////////////////////////////////////////////////////////
/*
ref (INPUT):        void-pointer to my internal state.
paintDC (INPUT):    a DC ready for painting in the display area
startsample (INPUT): 'logical' number of STEREO sample which is at extreme left of
                    display region
endsample (INPUT):  dito. for the sample at the extreme right
dspRegion (INPUT):  region to paint into
updRegion (INPUT):  sub-region to update (maybe is used later)
RETURN-value:       SUCCESS, if everthing went fine;
                    FAIL, if something went wrong
*/

extern "C" long WINAPI  DISPLAY( void *ref, HDC paintDC, long startsample, long endsample,
                                 RECT dspRegion, RECT updRegion )
{
    long    x, y, maxpacket, total, idx1, idx2, sondisplay;
    unsigned long val;
    int     w, h, nbits;
    uchar   r,g,b, *dataptr, *ptr1, *ptr2, ifrac1, ifrac2;
    double  scale, fidx, frac1;
    HBITMAP hbm;
    HDC     hMemDC;
    DLLSTATE    *ps;

    ps = (DLLSTATE*) ref;

    if( startsample>=endsample || endsample>=ps->totalsamples )
        return FAIL;

    w = dspRegion.right - dspRegion.left + 1;
    h = dspRegion.bottom - dspRegion.top + 1;
    nbits = GetDeviceCaps(paintDC,BITSPIXEL);

    if( !CreateBitmapData(w,h,nbits) )
        return FAIL;

    total = ps->totalsamples;
    maxpacket = ps->numpackets;

    sondisplay = endsample - startsample + 1;

    if( sondisplay < w )
    {
        DeleteBitmapData();
        return FAIL;        // scale too large
    }

    if( sondisplay < w*PACKETSIZE )  
    {
        // upscale: less than PACKETSIZE data per pixel
        scale = ((double)sondisplay) / ((double)w);     // samples per pixel
        dataptr = ps->data;

        fidx = (double) startsample;
        for( x=0; x<w; x++ )
        {
            idx1 = (long) (fidx/PACKETSIZE);
            if( idx1>=maxpacket )
                idx1 = maxpacket - 1;

            idx2 = idx1 + 1;
            if( idx2>=maxpacket )
                idx2 = maxpacket - 1;

            ptr1 = dataptr + idx1*BANDS_PP;
            ptr2 = dataptr + idx2*BANDS_PP;

            frac1 = (((double)fidx/PACKETSIZE)-idx1);
            if( frac1<0. )
                ifrac1 = 0;
            else
                if( frac1>=1. )
                    ifrac1 = 255;
                else
                    ifrac1 = (uchar) (256.*frac1);

            ifrac2 = 255 - ifrac1;

            for( y=0; y<256 && y<h; y++ )
            {
                val = ifrac1*ptr1[y]/256 + ifrac2*ptr2[y]/256;
                if( val>=256 )
                    val = 255;

                r = (uchar) (y*val/256);
                g = (uchar) ((256-y)*val/256);
                b = 0;
                DrawBitmapPoint(x,y, r,g,b);
            }

            fidx += scale;
        }
    }
    else
    {
        // downscale: more than PACKETSIZE data per pixel
        dataptr = ps->data;
        int firstpacket = startsample/PACKETSIZE;
        for( x=0; x<w && (x+firstpacket)<maxpacket; x++ )
        {
            for( y=0; y<256 && y<h; y++ )
            {
                val = dataptr[y];
                r = (uchar) (y*val/256);
                g = (uchar) ((256-y)*val/256);
                b = 0;
                DrawBitmapPoint(x,y, r,g,b);
            }
            dataptr += 256;
        }
    }


    hbm = ::CreateBitmap(w,h, 1,nbits, _pbmdata);

    hMemDC = ::CreateCompatibleDC(paintDC);
    HBITMAP hOldBitmap;
    hOldBitmap = (HBITMAP) ::SelectObject(hMemDC,(HGDIOBJ)hbm);
      
    ::BitBlt(paintDC, 0,0, w,h, hMemDC, 0,0, SRCCOPY);
      
    ::SelectObject(hMemDC,hOldBitmap);
    ::DeleteDC(hMemDC);
    ::DeleteObject(hbm);

    DeleteBitmapData();

    return SUCCESS;
}

/////////////////////////////////////////////////////////////////////////////
// CDWAV tells my DLL that work on the current file ends.
// So I free all resources for this special file.
// ref (INPUT):        void-pointer to my internal state.
// RETURN-value:       SUCCESS, if everthing went fine;
//                     FAIL, if something went wrong

extern "C" long WINAPI  END( void *ref )
{
    DLLSTATE    *ps;

    ps = (DLLSTATE*) ref;
    free(ps->data);
    free(ps->mixbuffer);
    free(ps->fftout);
    free(ps);

    return SUCCESS;
}

/////////////////////////////////////////////////////////////////////////////

