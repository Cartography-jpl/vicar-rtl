#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* In the course of the processing for opening a file an error may occur, */
/* in which case, the 'partially' opened file must be 'closed down'.      */
/* That is the function of this routine.				  */

void v2_close_down(unit)
int unit;
{

   struct bufstate *bufstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   close_os_file(unit);

   if (bufstate->buffer != NULL)
      free(bufstate->buffer);

   close_unit(unit);		/* close the control blocks for this unit. */

   return;
}
