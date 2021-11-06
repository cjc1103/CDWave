#define  SUCCESS    0
#define  FAIL       -1


extern "C" long WINAPI  START( long numsamples, long *preferredchunksize, void **ref );
extern "C" long WINAPI  INITPARAMS( void **parameters, long *paramsize );
extern "C" long WINAPI  ANALYZE( void *ref, long numsamples, short *data );
extern "C" long WINAPI  DISPLAY( void *ref, HDC paintDC, long startsample, long endsample,
                                 RECT dspRegion, RECT updRegion );
extern "C" long WINAPI  USERUI( void *ref, char *parameters, HWND parent );
extern "C" long WINAPI  END( void *ref );


typedef long (FAR WINAPI *START_PROC)( long numsamples, long *preferredchunksize, void **ref );
typedef long (FAR WINAPI *INITPARAMS_PROC)( void **parameters, long *paramsize );
typedef long (FAR WINAPI *ANALYZE_PROC)( void *ref, long numsamples, short *data );
typedef long (FAR WINAPI *DISPLAY_PROC)( void *ref, HDC paintDC, long startsample, long endsample,
                                         RECT dspRegion, RECT updRegion );
typedef long (FAR WINAPI *USERUI_PROC)( void *ref, char *parameters, HWND parent );
typedef long (FAR WINAPI *END_PROC)( void *ref );

