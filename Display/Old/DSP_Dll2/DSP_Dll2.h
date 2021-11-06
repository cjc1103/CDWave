
// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the DSP_DLL2_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// DSP_DLL2_API functions as being imported from a DLL, wheras this DLL sees symbols
// defined with this macro as being exported.
#ifdef DSP_DLL2_EXPORTS
#define DSP_DLL2_API __declspec(dllexport)
#else
#define DSP_DLL2_API __declspec(dllimport)
#endif

// This class is exported from the DSP_Dll2.dll
class DSP_DLL2_API CDSP_Dll2 {
public:
	CDSP_Dll2(void);
	// TODO: add your methods here.
};

extern DSP_DLL2_API int nDSP_Dll2;

DSP_DLL2_API int fnDSP_Dll2(void);

