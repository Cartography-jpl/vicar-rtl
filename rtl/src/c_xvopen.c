#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xvopen(unit)
int unit;
{
   int status;


   if (CURRENT_I_VALUE(FLAGS) & OPEN)
      return SUCCESS;

#if VMS_OS
   /* This prevents the user from unwittingly using his terminal */
   /* as a file and getting ghastly VMS errors.			 */

   make_upper_case_max(tempname,CURRENT_S_VALUE(NAME), MAX_FILE_NAME_SIZE);
   if (EQUAL(tempname,"TT"))
      return NO_IO_TO_TERMINAL;
#endif /* VMS_OS */

   /* COND is made upper case by process_optionals */

   if (substr(CURRENT_S_VALUE(COND),"NOLABELS")) /* No label processing */
      CURRENT_I_VALUE(FLAGS) |= NO_LABELS;

   if (substr(CURRENT_S_VALUE(COND),"NOBLOCK"))	/* Tapes won't be blocked */
      CURRENT_I_VALUE(FLAGS) |= NOBLOCK;

   if (substr(CURRENT_S_VALUE(COND),"BINARY"))	/* Access binary area of image*/
      CURRENT_I_VALUE(FLAGS) |= BINARY;

   if (substr(CURRENT_S_VALUE(COND),"VARREC"))	{/* Tape w/variable len records*/
      if ((CURRENT_I_VALUE(FLAGS) & NO_LABELS) &&
	  (CURRENT_I_VALUE(FLAGS) & NOBLOCK))
         CURRENT_I_VALUE(FLAGS) |= VARREC;
      else
         return VARREC_ERROR;
   }

   /* If one of the host type labels is given as "NATIVE" or "LOCAL", */
   /* replace it with the correct type for the native machine.        */

   if (EQUAL(CURRENT_S_VALUE(HOST),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(HOST),"LOCAL"))
      add_lbl_item_value_tbl(unit,HOST,NATIVE_HOST_LABEL);
   if (EQUAL(CURRENT_S_VALUE(INTFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(INTFMT),"LOCAL"))
      add_lbl_item_value_tbl(unit,INTFMT,NATIVE_INTFMT);
   if (EQUAL(CURRENT_S_VALUE(REALFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(REALFMT),"LOCAL"))
      add_lbl_item_value_tbl(unit,REALFMT,NATIVE_REALFMT);
   if (EQUAL(CURRENT_S_VALUE(BHOST),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(BHOST),"LOCAL"))
      add_lbl_item_value_tbl(unit,BHOST,NATIVE_HOST_LABEL);
   if (EQUAL(CURRENT_S_VALUE(BINTFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(BINTFMT),"LOCAL"))
      add_lbl_item_value_tbl(unit,BINTFMT,NATIVE_INTFMT);
   if (EQUAL(CURRENT_S_VALUE(BREALFMT),"NATIVE") ||
       EQUAL(CURRENT_S_VALUE(BREALFMT),"LOCAL"))
      add_lbl_item_value_tbl(unit,BREALFMT,NATIVE_REALFMT);

   /* Process the file, either as input (READ, UPDATE) or output (WRITE) */
   if (EQUAL(CURRENT_S_VALUE(OP),"WRITE"))
      status = v2_process_output_file(unit);
   else
      status = v2_process_input_file(unit);

   if (status != SUCCESS)
      return status;

   status = determine_translation(unit);
   if (status != SUCCESS)
      return status;

   if (CURRENT_PP_VALUE(ADDRESS) != NULL) {
      *(CURRENT_PP_VALUE(ADDRESS)) = v2_array_io_location(unit);
      if (*CURRENT_PP_VALUE(ADDRESS) == NULL)
         return ARRAY_IO_NOT_ALLOWED;
   }

   /* If compressed, perform addition opening operations. */
   if(COMPRESSED)
   {
      status = compress_preprocess(unit);

      if (status != SUCCESS)
         error_handler(unit, status);
   }

   return SUCCESS;

}
