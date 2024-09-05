#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Preprocessing (before process_optionals)			*/
/************************************************************************/

int p_xlninfo(unit)
int unit;
{

   if (first_call)
      general_initialize();		/* one-time setup */

   current_access = NI;			/* Set up the current access */
   current_call = LNIN;

   if (valid_unit(unit) != SUCCESS)
      return NO_SUCH_UNIT;

/* The rest of the checks were moved to c_xlninfo so err_act would work */

   initialize_value_table(label_options,N_LABEL_TABLE_ENTRIES,label_table[unit],
			label_default_table);
   initialize_value_table(unit_table, N_UNIT_TABLE_ENTRIES, current_table[unit],
			default_table);

   return SUCCESS;

}
