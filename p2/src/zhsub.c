#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


void zhsub( dcode, ns, buf, hist, ilow, ihigh)
int dcode;	
int ns;		
void *buf;	
void *hist;	
int ilow;	
int ihigh;	

{
F77_FUNC(hsub, HSUB)
( &dcode, &ns, buf, hist, &ilow, &ihigh);
}
