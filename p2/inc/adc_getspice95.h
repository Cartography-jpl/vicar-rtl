// adc_getspice95.h
//
// Apr 18, 2002
// Michael Brady

#if !defined ADC_GETSPICE95_H_INCLUDED
#define ADC_GETSPICE95_H_INCLUDED


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "ms_defines.h"

/**
 *  Obtains the getspice information from 
 *  the Anciliary Data Server (ads) system.
 */
int adc_getspice(msUserRequestStruct*    req,
		msCkStruct*             ckdata,
		msSpkStruct*            spkdata);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif // !defined ADC_GETSPICE95_H_INCLUDED
