#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/*$$*/  /* That symbol marks things in the template which	*/
	/* change for different routines			*/

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvopen(int unit, ...)			/*$$*/
{
   va_list params;
   int nargs;
   int status;

   va_start(params, unit);		/*$$*/

   if (valid_unit(unit) != SUCCESS) {
      va_end(params);
      error_handler(unit, NO_SUCH_UNIT);
      return NO_SUCH_UNIT;
   }

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 1; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Preset the OPEN_ACT and OPEN_MES parameters in case there's an early error */

   current_access = O;			/* Set up the current access */
   current_call = VOPE;

   status = open_act_preset_c(unit, nargs, &params);
   va_end(params);
   if (status != SUCCESS) {
      error_handler(unit, status);
      return status;
   }

   va_start(params, unit);		/* start the optional list over again */

/* Do the common preprocessing */

   status = p_xvopen(unit);		/*$$*/
   if (status != SUCCESS) {
      error_handler(unit, status);
      va_end(params);
      return status;
   }

   status = process_optionals_c(unit, unit_table, N_UNIT_TABLE_ENTRIES,
			     current_table[unit], default_table,
			     nargs, &params);
   if (status != SUCCESS) {
      error_handler(unit, status);
      va_end(params);
      return status;
   }

/* Call the common routine */

   status = c_xvopen(unit);		/*$$*/
   va_end(params);

   if (status != SUCCESS)
      error_handler(unit, status);

   return status;

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void F77_FUNC(xvopen, XVOPEN)
(int *unitp, int *status, ...)		/*$$*/
{
   va_list params, str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 2; /*$$*/	/* argument number of last const arg */
   int strno = 0; /*$$*/	/* string number of last const arg */
   int which = 0;
   int unit = *unitp;

   va_start(params, status);		/*$$*/

   if (valid_unit(unit) != SUCCESS) {
      va_end(params);
      *status = NO_SUCH_UNIT;
      error_handler(unit, NO_SUCH_UNIT);
      return;
   }

/* Figure out the number of arguments */

/* Preset the OPEN_ACT and OPEN_MES params in case there's an early error  */
/* Also, fill in nopts if we don't know yet...takes place of get_nopts().  */

   current_access = O;			/* Set up the current access */
   current_call = VOPE;

   *status = open_act_preset_for(unit, &nopts, &nargs, argno, &params);
   if (*status != SUCCESS) {
      va_end(params);
      error_handler(unit, *status);
      return;
   }

/* Get constant strings (params points correctly) */

   va_copy(str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

/* Do the common preprocessing */

   *status = p_xvopen(unit);		/*$$*/
   if (*status != SUCCESS) {
      error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Process the optional arguments */

   *status = process_optionals_for(unit, unit_table, N_UNIT_TABLE_ENTRIES,
				current_table[unit], default_table,
				nopts, &params, (char *) &unitp,		/*$$*/
				nargs, argno, strno, &str_params, &which);
   if (*status != SUCCESS) {
      error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Call the common routine */

   *status = c_xvopen(unit);		/*$$*/
   va_end(params);

   if (*status != SUCCESS)
      error_handler(unit, *status);

   return;

}
