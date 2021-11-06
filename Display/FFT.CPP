/**********************************************************************
 * ISO MPEG Audio Subgroup Software Simulation Group (1996)
 * ISO 13818-3 MPEG-2 Audio Encoder - Lower Sampling Frequency Extension
 *
 * $Id: subs.c,v 1.1 1996/02/14 04:04:23 rowlands Exp $
 *
 * $Log: subs.c,v $
 * Revision 1.1  1996/02/14 04:04:23  rowlands
 * Initial revision
 *
 * Received from Mike Coleman
 **********************************************************************/
/**********************************************************************
 *   date   programmers         comment                               *
 * 2/25/91  Davis Pan           start of version 1.0 records          *
 * 5/10/91  W. Joseph Carter    Ported to Macintosh and Unix.         *
 * 7/10/91  Earle Jennings      Ported to MsDos from Macintosh        *
 *                              Replacement of one float with FLOAT   *
 * 2/11/92  W. Joseph Carter    Added type casting to memset() args.  *
 * 4/27/92  Masahiro Iwadare    Added 256 point version for Layer III *
 **********************************************************************/

#include <memory.h>
#include "fft.h"

static void    do_fft( FLOAT x_real[], FLOAT x_imag[], FLOAT energy[], FLOAT phi[], int N );


/////////////////////////////////////////////////////////////////////////////////
// perform a FFT:
// windowtype:  type of windowing function (WI_...)
// scaletype:   type of scaling function for frequency amplitude values (ASC_...)
// size:        # of input values to perform FFT on (in range 64,128,256,...,16384 only powers of 2)
// in:          array of input values <0 .. size-1>
// out:         array of output values <0 .. size/2-1>

void     fft( long windowtype, long scaletype, long size, SValue *in, unsigned char *out )
{
    int             i;
    FLOAT           maxv;
    static long     lastwtype = -1, lastsize = -1;
    static FLOAT    wsamp_r[MAXFFTSIZE], wsamp_i[MAXFFTSIZE];
    static FLOAT    energy[MAXFFTSIZE], phi[MAXFFTSIZE];
    static FLOAT    window[MAXFFTSIZE];

    if( lastwtype!=windowtype || lastsize!=size )
    {
        switch( windowtype )
        {
        case WI_BOX:
            for( i=0; i<size; i++ )
                window[i] = 1.0;
            break;

        case WI_TRIANG:
            for( i=0; i<size/2; i++ )
            {
                window[i] = ((FLOAT)i+0.5)/(size/2);
                window[size-i-1] = window[i];
            }
            break;

        case WI_HANN:
            for( i=0; i<size; i++ )
                window[i]  = 0.5*(1-cos(2.0*PI*(i-0.5)/size));
            break;
        }

        lastwtype = windowtype;
        lastsize = size;
    }

    for( i=0; i<size; i++ )
    {
        wsamp_r[i] = window[i]*((FLOAT) in[i]); 
        wsamp_i[i] = 0.;
    }

    //  Compute FFT
    do_fft(wsamp_r,wsamp_i,energy,phi, size);

    maxv = 0.;
    switch( scaletype )
    {
    case ASC_LIN:
        for( i=0; i<size/2; i++ )
            if( energy[i]>maxv )
                maxv = energy[i];
        break;

    case ASC_SQRT:
        for( i=0; i<size/2; i++ )
        {
            energy[i] = (energy[i]>0.01) ? sqrt(energy[i]) : sqrt(0.01);
            if( energy[i]>maxv )
                maxv = energy[i];
        }
        break;

    case ASC_LOG:
        for( i=0; i<size/2; i++ )
        {
            energy[i] = (energy[i]>0.01) ? log(energy[i]) : log(0.01);
            if( energy[i]>maxv )
                maxv = energy[i];
        }
        break;
    }

    for( i=0; i<size/2; i++ )
    {
        if( energy[i]<0. )
            out[i] = 0;
        else
            out[i] = (unsigned char) (255.*energy[i]/maxv+0.499);
    }
}

/*****************************************************************************
 * FFT computes fast fourier transform of BLKSIZE samples of data            *
 *   uses decimation-in-frequency algorithm described in "Digital            *
 *   Signal Processing" by Oppenheim and Schafer, refer to pages 304         *
 *   (flow graph) and 330-332 (Fortran program in problem 5)                 *
 *   to get the inverse fft, change line 20 from                             *
 *                 w_imag[L] = -sin(PI/le1);                                 *
 *                          to                                               *
 *                 w_imag[L] = sin(PI/le1);                                  *
 *                                                                           *
 *   required constants:                                                     *
 *         #define      PI          3.14159265358979                         *
 *         #define      BLKSIZE     1024                                     *
 *         #define      LOGBLKSIZE  10                                       *
 *         #define      BLKSIZE_S   256                                      *
 *         #define      LOGBLKSIZE_S 8                                       *
 *                                                                           *
 *****************************************************************************/

static void    do_fft( FLOAT x_real[], FLOAT x_imag[], FLOAT energy[], FLOAT phi[], int N )
{
    int            MM1;
    int            NV2, NM1;
    int            i,j,k,L;
    int            ip, le,le1;
    double         t_real, t_imag, u_real, u_imag;
    static int     lastN = 0, M;
    static double  w_real[LOGMAXFFTSIZE], w_imag[LOGMAXFFTSIZE];

    if( lastN!=N )
    {
        switch( N )
        {
            case 64:    M = 6; break;
            case 128:   M = 7; break;
            case 256:   M = 8; break;
            case 512:   M = 9; break;
            case 1024:  M = 10; break;
            case 2048:  M = 11; break;
            case 4096:  M = 12; break;
            case 8192:  M = 13; break;
            case 16384: M = 14; break;
            default:    return;
        }

        memset((char *) w_real, 0, sizeof(w_real));  /* preset statics to 0 */
        memset((char *) w_imag, 0, sizeof(w_imag));  /* preset statics to 0 */

        for(L=0; L<M; L++)
        {
            le = 1 << (M-L);
            le1 = le >> 1;
            w_real[L] = cos(PI/le1);
            w_imag[L] = -sin(PI/le1);
        }
    
        lastN = N;
    }

    MM1 = M-1;
    NV2 = N >> 1;
    NM1 = N - 1;
    for(L=0; L<MM1; L++)
    {
        le = 1 << (M-L);
        le1 = le >> 1;
        u_real = 1;
        u_imag = 0;
        for(j=0; j<le1; j++)
        {
            for(i=j; i<N; i+=le)
            {
                ip = i + le1;
                t_real = x_real[i] + x_real[ip];
                t_imag = x_imag[i] + x_imag[ip];
                x_real[ip] = x_real[i] - x_real[ip];
                x_imag[ip] = x_imag[i] - x_imag[ip];
                x_real[i] = t_real;
                x_imag[i] = t_imag;
                t_real = x_real[ip];
                x_real[ip] = x_real[ip]*u_real - x_imag[ip]*u_imag;
                x_imag[ip] = x_imag[ip]*u_real + t_real*u_imag;
            }
            t_real = u_real;
            u_real = u_real*w_real[L] - u_imag*w_imag[L];
            u_imag = u_imag*w_real[L] + t_real*w_imag[L];
        }
    }

    /* special case: L = M-1; all Wn = 1 */
    for(i=0; i<N; i+=2)
    {
        ip = i + 1;
        t_real = x_real[i] + x_real[ip];
        t_imag = x_imag[i] + x_imag[ip];
        x_real[ip] = x_real[i] - x_real[ip];
        x_imag[ip] = x_imag[i] - x_imag[ip];
        x_real[i] = t_real;
        x_imag[i] = t_imag;
        energy[i] = x_real[i]*x_real[i] + x_imag[i]*x_imag[i];
        if(energy[i] <= 0.0005)
        {
            phi[i] = 0;
            energy[i] = 0.0005;
        }
        else 
            phi[i] = atan2((double) x_imag[i],(double) x_real[i]);

        energy[ip] = x_real[ip]*x_real[ip] + x_imag[ip]*x_imag[ip];
        if(energy[ip] == 0)
            phi[ip] = 0;
        else 
            phi[ip] = atan2((double) x_imag[ip],(double) x_real[ip]);
    }

    /* this section reorders the data to the correct ordering */
    j = 0;
    for(i=0; i<NM1; i++)
    {
        if(i<j)
        {
            /* use this section only if you need the FFT in complex number form *
             * (and in the correct ordering)                                    */
            t_real = x_real[j];
            t_imag = x_imag[j];
            x_real[j] = x_real[i];
            x_imag[j] = x_imag[i];
            x_real[i] = t_real;
            x_imag[i] = t_imag;
            /* reorder the energy and phase, phi                                        */
            t_real = energy[j];
            energy[j] = energy[i];
            energy[i] = t_real;
            t_real = phi[j];
            phi[j] = phi[i];
            phi[i] = t_real;
        }

        k=NV2;
        while(k<=j)
        {
            j = j-k;
            k = k >> 1;
        }
        j = j+k;
    }
}


