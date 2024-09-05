#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void   zcomphist(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    iunit,sl,ss,nl,ns;
{
F77_FUNC(comphist, COMPHIST)
(&iunit,&sl,&ss,&nl,&ns,ohist,ibuf);
}
void   zcomphist2(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    iunit,sl,ss,nl,ns;
{
F77_FUNC(comphist2, COMPHIST2)
(&iunit,&sl,&ss,&nl,&ns,ohist,ibuf);
}
