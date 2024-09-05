#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* Copies the value from the unit_table or value_table associated with	*/
/* 'from_unit' to the corresponding structure associated with 'to_unit';*/
/* 'item' is the number of the unit table value to be copied.		*/

int copy_value_block_item(from_unit, to_unit, item)
int from_unit, to_unit, item;
{
   int status;

   if (unit_table[item].type == STRING)
       status = add_str_current_table(
				  current_table[from_unit][item].pvalue,
                                  item, current_table[to_unit], default_table);
   else
   if (unit_table[item].type == MESSAGE)
       status = add_msg_current_table(
				current_table[from_unit][item].pvalue,
                                item, current_table[to_unit], default_table);
   else {
      current_table[to_unit][item].value =
	   current_table[from_unit][item].value;
      status = SUCCESS;
   }

   return status;
}
