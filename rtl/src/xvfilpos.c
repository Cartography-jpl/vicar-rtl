#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "errdefs.h"
#include "ftnbridge.h"

/* Return the current tape position (file number).  The unit must be	*/
/* open.  Returns -1 if unit is not a tape.				*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int F77_FUNC(xvfilpos, XVFILPOS)
(int *unit)
{
   return zvfilpos(*unit);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvfilpos(int UNUSED(unit))
{

   current_call = VFILPOS;

#if RTL_USE_TAPE

   if (valid_unit(unit) != SUCCESS) {
      error_handler(unit, NO_SUCH_UNIT);
      return 0;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      error_handler(unit, FILE_NOT_OPEN);
      return 0;
   }

   if (!(CURRENT_I_VALUE(FLAGS) & UNIT_IS_TAPE))
      return -1;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   if (bufstate->devstate.device_type != DEV_TAPE)
      return -1;

   return i_file[bufstate->devstate.dev.tape.tindex];

#else

   return -1;

#endif

}
