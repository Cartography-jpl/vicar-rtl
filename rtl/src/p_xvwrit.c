#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvwrit(unit)
int unit;
{

   if (first_call)
      general_initialize();		/* one-time setup */

   current_access = W;			/* Set up the current access */
   current_call = VWRI;

   if (valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   initialize_value_table(unit_table, N_UNIT_TABLE_ENTRIES,
			current_table[unit], default_table);

   if (!(CURRENT_I_VALUE(FLAGS) & OPEN))
      return FILE_NOT_OPEN;

   if (!EQUAL(CURRENT_S_VALUE(OP), "WRITE") &&
       !EQUAL(CURRENT_S_VALUE(OP), "UPDATE"))

      return IMPROPER_OPERATION;

   return SUCCESS;

}
