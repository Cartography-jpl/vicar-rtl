#include  "xvmaininc.h"  
#include  "ftnbridge.h"

/************************************************************************/
/*  C-Callable Version ZABLE77V2  (See Fortran Source ABLE77V2)             */
/************************************************************************/

void  zable77v2(ind,unit,arra)  
int   *ind;          /*  returned status (output) */
int   unit;          /* VICAR  unit # (input)     */
void   *arra;          /* array containing the extracted data (output)  */
{
F77_FUNC(able77v2, ABLE77V2) 
(ind, &unit, arra) ;
}
