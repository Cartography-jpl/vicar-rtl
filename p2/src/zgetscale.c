#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void   zgetscale(itype,labuf,maxdn,iscale,oscale,ind)
void   *labuf;	
int    itype,*ind,maxdn;
float  *iscale,*oscale;	

{
F77_FUNC(getscale, GETSCALE)
(&itype,labuf,&maxdn,iscale,oscale,ind);
}
