#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Initialize the run-time library */

int zv_rtl_init()
{

#if RTL_USE_TAPE
   i_init(parb);			/* Initialize tapes if available */
#endif

   applic_lang = C_LANG;		/* overridden in xvzinit if fortran */

   return SUCCESS;
}

