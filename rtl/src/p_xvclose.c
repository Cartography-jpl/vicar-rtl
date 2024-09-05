#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvclose(unit)
int unit;
{

   if (first_call)
      general_initialize();		/* one-time setup */

   current_access = C;			/* Set up the current access */
   current_call = VCLO;

   if (valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN))
      return FILE_NOT_OPEN;

   return SUCCESS;

}
