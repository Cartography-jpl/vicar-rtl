#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/* Position tape to different file number */

/* Pretty stupid routine; why not just call zvadd? */

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int F77_FUNC(xvsfile, XVSFILE)
(int *unit, int *file)
{
   return zvsfile(*unit, *file);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvsfile(int unit, int file)
{

   return zvadd(unit, "U_FILE", file, NULL);

}
