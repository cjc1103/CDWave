#define  SUCCESS    0
#define  FAIL       -1

#define DllExport   __declspec( dllexport )
#define DSP_DLL_API int 


DllExport DSP_DLL_API  START( long numsamples, long *preferredchunksize, void **ref );
DllExport DSP_DLL_API  INITPARAMS( void **parameters, long *paramsize );
DllExport DSP_DLL_API  ANALYZE( void *ref, long numsamples, short *data );
DllExport DSP_DLL_API  DISPLAY( void *ref, HDC paintDC, long startsample, long endsample,
                                 RECT dspRegion, RECT updRegion );
extern DllExport DSP_DLL_API  USERUI( void *ref, char *parameters, HWND parent );
DllExport DSP_DLL_API  END( void *ref );

/*
typedef long (FAR WINAPI *START_PROC)( long numsamples, long *preferredchunksize, void **ref );
typedef long (FAR WINAPI *INITPARAMS_PROC)( void **parameters, long *paramsize );
typedef long (FAR WINAPI *ANALYZE_PROC)( void *ref, long numsamples, short *data );
typedef long (FAR WINAPI *DISPLAY_PROC)( void *ref, HDC paintDC, long startsample, long endsample,
                                         RECT dspRegion, RECT updRegion );
typedef long (FAR WINAPI *USERUI_PROC)( void *ref, char *parameters, HWND parent );
typedef long (FAR WINAPI *END_PROC)( void *ref );
*/
