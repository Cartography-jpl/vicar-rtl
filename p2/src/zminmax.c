#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zminmax - find min and max of array 		*/
/************************************************************************/

void zminmax( dcode, n, buf, min, max, imin, imax) 
int dcode, n, *min, *max, *imin, *imax;
void *buf;

{
F77_FUNC(minmax, MINMAX)
( &dcode, &n, buf, min, max, imin, imax);
}

/************************************************************************/
/* C-Callable Version: zminmaxe - find min and max of array excluding value*/
/************************************************************************/

void zminmaxe( dcode, n, buf, exclude, min, max, imin, imax) 
int dcode, n, *min, *max, *imin, *imax, *exclude;
void *buf;

{
  F77_FUNC(minmaxe, MINMAXE)
( &dcode, &n, buf, exclude, min, max, imin, imax);
}
