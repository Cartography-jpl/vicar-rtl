#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvadd(unit)
int unit;
{

   if (first_call)
      general_initialize();		/* one-time setup */

   current_access = A;			/* Set up the current access */
   current_call = VADD;

   if (valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   initialize_value_table(unit_table, N_UNIT_TABLE_ENTRIES, current_table[unit],
			default_table);

   if (CURRENT_I_VALUE(FLAGS) & OPEN)
      return CANNOT_MOD_OPEN_UNIT;

   return SUCCESS;

}
