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

int zlinfo(int unit, char *type, char *key, char *format, int *maxlength,
				int *nelement, ...)		/*$$*/
{
   va_list params;
   int nargs;
   int status;

   char c_type[MAX_SHORT_STRING_SIZE+1];
   char c_key[MAX_LABEL_KEY_SIZE+1];

   va_start(params, nelement);

   make_upper_case_max(c_type, type, MAX_SHORT_STRING_SIZE);
   make_upper_case_max(c_key, key, MAX_LABEL_KEY_SIZE);

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 6; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xlinfo(unit);		/*$$*/
   if (status != SUCCESS) {
      error_handler(unit, status);
      va_end(params);
      return status;
   }

   status = process_optionals_c(unit, label_options, N_LABEL_TABLE_ENTRIES,
			     label_table[unit], label_default_table,
			     nargs, &params);
   if (status != SUCCESS) {
      error_handler(unit, status);
      va_end(params);
      return status;
   }

/* Call the common routine */

   status = c_xlinfo(unit, c_type, c_key, format, maxlength, nelement);	/*$$*/
   va_end(params);
   if (status != SUCCESS) {
      error_handler(unit, status);
      return status;
   }

   return SUCCESS;

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void F77_FUNC(xlinfo, XLINFO)
(int *unitp, char *for_type, char *for_key,
	char *for_format, int *maxlength, int *nelement, int *status, ...)/*$$*/
{
   va_list params, str_params, save_str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 7; /*$$*/	/* argument number of last const arg */
   int strno = 3; /*$$*/	/* string number of last const arg */
   int which = 0;
   int unit = *unitp;

   char c_type[MAX_SHORT_STRING_SIZE+1];
   char c_key[MAX_LABEL_KEY_SIZE+1];
   char c_format[MAX_SHORT_STRING_SIZE+1];

   va_start(params, status);		/*$$*/

/* Figure out the number of arguments */

   get_nopts(&nopts, &nargs, argno, &params);

   /* Get constant strings (param points correctly) */

   va_copy(str_params, params);
   va_copy(save_str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

   v2_sfor2c(c_type, MAX_SHORT_STRING_SIZE, for_type, &unitp, nargs, 2, 1,
			&str_params, &which);
   make_upper_case(c_type, c_type);
   v2_sfor2c(c_key, MAX_LABEL_KEY_SIZE, for_key, &unitp, nargs, 3, 2,
			&str_params, &which);
   make_upper_case(c_key, c_key);

/* Do the common preprocessing */

   *status = p_xlinfo(unit);		/*$$*/
   if (*status != SUCCESS) {
      error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Process the optional arguments */

   *status = process_optionals_for(unit, label_options, N_LABEL_TABLE_ENTRIES,
				label_table[unit], label_default_table,
				nopts, &params, (char *)&unitp,		/*$$*/
				nargs, argno, strno, &str_params, &which);
   if (*status != SUCCESS) {
      error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Call the common routine */

   *status = c_xlinfo(unit, c_type, c_key, c_format, maxlength, nelement);/*$$*/
   va_end(params);
   if (*status != SUCCESS) {
      error_handler(unit, *status);
      return;
   }

   which = 0;		/* reset str lens */
   v2_sc2for(c_format, 0, for_format, &unitp, nargs, 4, 3,
					&save_str_params, &which);

   *status = SUCCESS;
   return;

}
