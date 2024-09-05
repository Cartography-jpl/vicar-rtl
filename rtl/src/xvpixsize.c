#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/* Returns the pixel size for a given data type and host representation.*/
/* Can get the host either from string parameters (xvpixsize) or from	*/
/* an open file (xvpixsizeu).  For xvpixsize, the host can be "NATIVE"	*/
/* or "LOCAL" for the native host (the one you're running on).  The	*/
/* size of data types in the binary label of an open file is returned	*/
/* by xvpixsizeb.							*/
/* If an input string is bad, then 0 is returned for the pixel size and	*/
/* the error handler is called.						*/

/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

/************************************************************************/
/* Get host from open unit						*/
/************************************************************************/

int F77_FUNC(xvpixsizeu, XVPIXSIZEU)
(int *pixsize, char *type, int *unit, ZFORSTR_PARAM)
/* int *pixsize;		Out: pixel size */
/* char *type;			In: data type */
/* int *unit;			In: unit number */
{
   ZFORSTR_BLOCK
   char c_type[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_type, 29, type, &pixsize, 3, 2, 1, unit);

   return zvpixsizeu(pixsize, c_type, *unit);
}

/************************************************************************/
/* Get host from open unit's binary label				*/
/************************************************************************/

int F77_FUNC(xvpixsizeb, XVPIXSIZEB)

(int *pixsize, char *type, int *unit, ZFORSTR_PARAM)
/* int *pixsize;		Out: pixel size */
/* char *type;			In: data type */
/* int *unit;			In: unit number */
{
   ZFORSTR_BLOCK
   char c_type[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_type, 29, type, &pixsize, 3, 2, 1, unit);

   return zvpixsizeb(pixsize, c_type, *unit);
}

/************************************************************************/
/* Get host from parameters						*/
/************************************************************************/

int F77_FUNC(xvpixsize, XVPIXSIZE)
(int *pixsize, char *type, char *ihost, char *rhost,
							ZFORSTR_PARAM)
/* int *pixsize;		Out: pixel size */
/* char *type;			In: data type */
/* char *ihost, *rhost;		In: int, real host representations */
{
   ZFORSTR_BLOCK
   char c_type[30], c_ihost[30], c_rhost[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_type,  29, type,  &pixsize, 4, 2, 1, rhost);
   zsfor2c(c_ihost, 29, ihost, &pixsize, 4, 3, 2, rhost);
   zsfor2c(c_rhost, 29, rhost, &pixsize, 4, 4, 3, rhost);

   return zvpixsize(pixsize, c_type, c_ihost, c_rhost);
}

/************************************************************************/
/* C-Callable Versions							*/
/************************************************************************/

/************************************************************************/
/* Get host from open unit						*/
/************************************************************************/

int zvpixsizeu(pixsize, type, unit)
int *pixsize;			/* Out: pixel size */
char *type;			/* In: data type */
int unit;			/* In: unit number */
{
   int status;

   current_call = VPIXSIZEU;
   *pixsize = 0;			/* in case of error */

   if (valid_unit(unit) != SUCCESS) {
      error_handler(unit, NO_SUCH_UNIT);
      return NO_SUCH_UNIT;
   }
   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      error_handler(unit, FILE_NOT_OPEN);
      return FILE_NOT_OPEN;
   }

/* Strings are converted to upper case in the low-level routines */

   *pixsize = bytes_per_pixel(type, CURRENT_S_VALUE(INTFMT),
				    CURRENT_S_VALUE(REALFMT), &status);
   if (status != SUCCESS) {
      *pixsize = 0;
      error_handler(unit, status);
      return status;
   }

   return SUCCESS;
}

/************************************************************************/
/* Get host from open unit's binary label				*/
/************************************************************************/

int zvpixsizeb(pixsize, type, unit)
int *pixsize;			/* Out: pixel size */
char *type;			/* In: data type */
int unit;			/* In: unit number */
{
   int status;

   current_call = VPIXSIZEB;
   *pixsize = 0;			/* in case of error */

   if (valid_unit(unit) != SUCCESS) {
      error_handler(unit, NO_SUCH_UNIT);
      return NO_SUCH_UNIT;
   }
   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      error_handler(unit, FILE_NOT_OPEN);
      return FILE_NOT_OPEN;
   }

/* Strings are converted to upper case in the low-level routines */

   *pixsize = bytes_per_pixel(type, CURRENT_S_VALUE(BINTFMT),
				    CURRENT_S_VALUE(BREALFMT), &status);
   if (status != SUCCESS) {
      *pixsize = 0;
      error_handler(unit, status);
      return status;
   }

   return SUCCESS;
}

/************************************************************************/
/* Get host from parameters						*/
/************************************************************************/

int zvpixsize(pixsize, type, ihost, rhost)
int *pixsize;			/* Out: pixel size */
char *type;			/* In: data type */
char *ihost, *rhost;		/* In: int, real host representations */
{
   int status;

   current_call = VPIXSIZE;
   *pixsize = 0;		/* in case of error */

/* Strings are converted to upper case in the low-level routines */

   *pixsize = bytes_per_pixel(type, ihost, rhost, &status);

   if (status != SUCCESS) {
      *pixsize = 0;
      error_handler(NO_UNIT, status);
      return status;
   }

   return SUCCESS;
}
