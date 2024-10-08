#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xvopen(unit)
int unit;
{

   if (first_call)
      general_initialize();		/* one-time setup */

   current_access = O;			/* Set up the current access */
   current_call = VOPE;

   if (valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

   initialize_value_table(unit_table, N_UNIT_TABLE_ENTRIES, current_table[unit],
							 default_table);

   if (CURRENT_I_VALUE(FLAGS) & OPEN) 
      return FILE_IS_ALREADY_OPEN;

   return SUCCESS;

}
