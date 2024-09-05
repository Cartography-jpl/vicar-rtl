/************************************************************************
 * zia.c 
 ************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "p1proto.h"
#include <string.h>

/************************************************************************/
/* Fortran-Callable Version (no C-version needed -- use memset)         */
/************************************************************************/

void F77_FUNC(zia, ZIA)
(char *cbuf,int *n)
{
  memset(cbuf,0,4*(*n));
  return;
}



