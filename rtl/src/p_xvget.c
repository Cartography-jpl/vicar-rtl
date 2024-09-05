#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvget(unit)
int unit;
{

   if (first_call)
      general_initialize();		/* one-time setup */

   current_access = G;			/* Set up the current access */
   current_call = VGET;

   if (valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   return SUCCESS;

}
