#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void   zhiscale(hist,npts,scale,ohist,lsat,hsat)
void   *hist,*ohist;	
int    npts;
double scale;	
float  *lsat,*hsat;	

{
float tscale;

tscale = (float) scale;
F77_FUNC(hiscale, HISCALE)
(hist,&npts,&tscale,ohist,lsat,hsat);
}
