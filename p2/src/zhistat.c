#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void   zhistat(ohist,npts,mean,sigma,maxfreq)
void   *ohist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *maxfreq;	

{
F77_FUNC(histat, HISTAT)
(ohist, &npts, mean, sigma, maxfreq);
}
void   zhistat2(hist,npts,mean,sigma,mindn,maxdn,maxfreq)
void   *hist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *mindn,*maxdn,*maxfreq;	

{
F77_FUNC(histat2, HISTAT2)
(hist, &npts, mean, sigma,mindn,maxdn, maxfreq);
}
