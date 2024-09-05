#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zinside - test if given point inside specified polygon*/
/************************************************************************/

int zinside( point, corner, n)
float point[2];
float *corner;
int n;				/* number of corners in polygon */

{
return F77_FUNC(inside, INSIDE) ( point, corner, &n);
}
