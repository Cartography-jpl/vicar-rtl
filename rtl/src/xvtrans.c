#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/* Data type and host translation functions.  xvtrans() translates an	*/
/* array from one data type/representation to another.  One of the	*/
/* following three routines must be called first to set up the		*/
/* translation.  xvtrans_set() sets up the conversion for data types in	*/
/* the native host machine's representation.  xvtrans_in() sets up the	*/
/* conversion for input from a foreign host, and xvtrans_out() sets up	*/
/* the conversion for output to a foreign host.  Host types (both	*/
/* the integer and real representation are required) can come from	*/
/* the INTFMT and REALFMT labels, or they can be "NATIVE" or "LOCAL"	*/
/* for the native host (the one you're running on).  xvtrans_inu()	*/
/* is provided as a convenience where the host type is obtained via a	*/
/* unit number (the file must be open).  xvtrans_inb() is provided as	*/
/* a convenience where the host type for binary labels is obtained via	*/
/* a unit number (the file must be open).  Note that the corresponding	*/
/* xvtrans_outu() and xvtrans_outb() are not needed because most files	*/
/* are output in the native format and xvtrans_out() provides the	*/
/* cabability in a slightly harder-to-use format.			*/
/*									*/
/* These routines require a buffer to be supplied that is at least	*/
/* 12 integers (48 bytes) long that will describe the translation.  The	*/
/* internals of this buffer are unknown to the calling program... just	*/
/* call one of the three setup routines to fill in the buffer, then	*/
/* call xvtrans with the buffer to perform the translation.  One thing	*/
/* only is known:  if the first integer in the buffer is NULL (0), then	*/
/* no translation is needed.  xvtrans() will check for this and just do	*/
/* a v2_move(), but the caller can save time in some cases by knowing	*/
/* this.								*/

/************************************************************************/
/* Fortran-Callable Versions						*/
/************************************************************************/

/************************************************************************/
/* Setup the buffer for native data-type conversion			*/
/************************************************************************/

void F77_FUNC_(xvtrans_set, XVTRANS_SET)
(struct trans *buf, char *stype, char *dtype,
						int *status, ZFORSTR_PARAM)
/* struct trans *buf;		Out: buffer to describe translation */
/* char *stype, *dtype;		In: source, dest data types */
/* int *status;			Out: status return */
{
   ZFORSTR_BLOCK
   char c_stype[30];
   char c_dtype[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_stype, 29, stype, &buf, 4, 2, 1, status);
   zsfor2c(c_dtype, 29, dtype, &buf, 4, 3, 2, status);

   *status = zvtrans_set((struct trans *) buf, c_stype, c_dtype);

   return;
}

/************************************************************************/
/* Setup the buffer for input data-type conversion from a foreign host	*/
/************************************************************************/

void F77_FUNC_(xvtrans_in, XVTRANS_IN)
(struct trans *buf, char *stype, char *dtype,
			char *sihost, char *srhost, int *status, ZFORSTR_PARAM)
/* struct trans *buf;		Out: buffer to describe translation */
/* char *stype, *dtype;		In: source, dest data types */
/* char *sihost, *srhost;	In: int, real host representations */
/* int *status;			Out: status return */
{
   ZFORSTR_BLOCK
   char c_stype[30];
   char c_dtype[30];
   char c_sihost[30], c_srhost[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_stype,  29, stype,  &buf, 6, 2, 1, status);
   zsfor2c(c_dtype,  29, dtype,  &buf, 6, 3, 2, status);
   zsfor2c(c_sihost, 29, sihost, &buf, 6, 4, 3, status);
   zsfor2c(c_srhost, 29, srhost, &buf, 6, 5, 4, status);

   *status = zvtrans_in((struct trans *) buf, c_stype, c_dtype, c_sihost, c_srhost);

   return;
}

/************************************************************************/
/* Setup the buffer for output data-type conversion to a foreign host	*/
/************************************************************************/

void F77_FUNC_(xvtrans_out, XVTRANS_OUT)
(struct trans *buf, char *stype, char *dtype,
			char *dihost, char *drhost, int *status, ZFORSTR_PARAM)
/* struct trans *buf;		Out: buffer to describe translation */
/* char *stype, *dtype;		In: source, dest data types */
/* char *dihost, *drhost;	In: int, real host representations */
/* int *status;			Out: status return */
{
   ZFORSTR_BLOCK
   char c_stype[30];
   char c_dtype[30];
   char c_dihost[30], c_drhost[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_stype,  29, stype,  &buf, 6, 2, 1, status);
   zsfor2c(c_dtype,  29, dtype,  &buf, 6, 3, 2, status);
   zsfor2c(c_dihost, 29, dihost, &buf, 6, 4, 3, status);
   zsfor2c(c_drhost, 29, drhost, &buf, 6, 5, 4, status);

   *status = zvtrans_out((struct trans *) buf, c_stype, c_dtype, c_dihost, c_drhost);

   return;
}

/************************************************************************/
/* Setup the buffer for input data-type conversion from a foreign host	*/
/* given a unit number to get host info from.				*/
/************************************************************************/

void F77_FUNC_(xvtrans_inu, XVTRANS_INU)
(struct trans *buf, char *stype, char *dtype,
			int *unit, int *status, ZFORSTR_PARAM)
/* struct trans *buf;		Out: buffer to describe translation */
/* char *stype, *dtype;		In: source, dest data types */
/* int *unit;			In: unit number for getting host */
/* int *status;			Out: status return */
{
   ZFORSTR_BLOCK
   char c_stype[30];
   char c_dtype[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_stype, 29, stype, &buf, 5, 2, 1, status);
   zsfor2c(c_dtype, 29, dtype, &buf, 5, 3, 2, status);

   *status = zvtrans_inu((struct trans *) buf, c_stype, c_dtype, *unit);

   return;
}

/************************************************************************/
/* Setup the buffer for input data-type conversion for binary labels	*/
/* from a foreign host given a unit number to get host info from.	*/
/************************************************************************/

void F77_FUNC_(xvtrans_inb, XVTRANS_INB)
(struct trans *buf, char *stype, char *dtype,
				int *unit, int *status, ZFORSTR_PARAM)
/* struct trans *buf;		Out: buffer to describe translation */
/* char *stype, *dtype;		In: source, dest data types */
/* int *unit;			In: unit number for getting host */
/* int *status;			Out: status return */
{
   ZFORSTR_BLOCK
   char c_stype[30];
   char c_dtype[30];

/* Strings are converted to upper case in the low-level routines */

   zsfor2c(c_stype, 29, stype, &buf, 5, 2, 1, status);
   zsfor2c(c_dtype, 29, dtype, &buf, 5, 3, 2, status);

   *status = zvtrans_inb((struct trans *) buf, c_stype, c_dtype, *unit);

   return;
}

/************************************************************************/
/* Use the buffer (translation structure) to do a translation.		*/
/************************************************************************/

void F77_FUNC(xvtrans, XVTRANS)
(struct trans *buf, void *source, void *dest, int *npix)
/* struct trans *buf;		In: buffer to describe translation */
/* void *source, *dest;		In: pointers to source & dest buffers */
/* int *npix;			In: number of pixels to translate */
{

/* Note: source and dest should *not* overlap */

   if (buf->transfn == NULL)		/* No translation required */
      v2_move(dest, source, *npix * buf->spixsize);
   else					/* Do the translation */
      (*buf->transfn)(source, dest, *npix, buf);

   return;
}


/************************************************************************/
/* C-Callable Versions							*/
/************************************************************************/

/************************************************************************/
/* Setup the buffer for native data-type conversion			*/
/************************************************************************/

int zvtrans_set(buf, stype, dtype)
struct trans *buf;		/* Out: buffer to describe translation */
char *stype, *dtype;		/* In: source, dest data types */
{
   int status;

   current_call = VTRANS_SET;

/* Strings are converted to upper case in the low-level routines */

   status = v2_translate_input(stype, dtype, "NATIVE", "NATIVE", buf);

   if (status != SUCCESS) {
      error_handler(NO_UNIT, status);
      return status;
   }

   return SUCCESS;
}

/************************************************************************/
/* Setup the buffer for input data-type conversion from a foreign host	*/
/************************************************************************/

int zvtrans_in(buf, stype, dtype, sihost, srhost)
struct trans *buf;		/* Out: buffer to describe translation */
char *stype, *dtype;		/* In: source, dest data types */
char *sihost, *srhost;		/* In: int, real host representations */
{
   int status;

   current_call = VTRANS_IN;

/* Strings are converted to upper case in the low-level routines */

   status = v2_translate_input(stype, dtype, sihost, srhost, buf);

   if (status != SUCCESS) {
      error_handler(NO_UNIT, status);
      return status;
   }

   return SUCCESS;
}

/************************************************************************/
/* Setup the buffer for output data-type conversion to a foreign host	*/
/************************************************************************/

int zvtrans_out(buf, stype, dtype, dihost, drhost)
struct trans *buf;		/* Out: buffer to describe translation */
char *stype, *dtype;		/* In: source, dest data types */
char *dihost, *drhost;		/* In: int, real host representations */
{
   int status;

   current_call = VTRANS_OUT;

/* Strings are converted to upper case in the low-level routines */

   status = v2_translate_output(stype, dtype, dihost, drhost, buf);

   if (status != SUCCESS) {
      error_handler(NO_UNIT, status);
      return status;
   }

   return SUCCESS;
}

/************************************************************************/
/* Setup the buffer for input data-type conversion from a foreign host	*/
/* given a unit number to get host info from.				*/
/************************************************************************/

int zvtrans_inu(buf, stype, dtype, unit)
struct trans *buf;		/* Out: buffer to describe translation */
char *stype, *dtype;		/* In: source, dest data types */
int unit;			/* In: unit number for getting host */
{
   int status;

   current_call = VTRANS_INU;

   if (valid_unit(unit) != SUCCESS) {
      error_handler(unit, NO_SUCH_UNIT);
      return NO_SUCH_UNIT;
   }
   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      error_handler(unit, FILE_NOT_OPEN);
      return FILE_NOT_OPEN;
   }

/* Strings are converted to upper case in the low-level routines */

   status = v2_translate_input(stype, dtype, CURRENT_S_VALUE(INTFMT),
					  CURRENT_S_VALUE(REALFMT), buf);

   if (status != SUCCESS) {
      error_handler(NO_UNIT, status);
      return status;
   }

   return SUCCESS;
}

/************************************************************************/
/* Setup the buffer for input data-type conversion for binary labels	*/
/* from a foreign host given a unit number to get host info from.	*/
/************************************************************************/

int zvtrans_inb(buf, stype, dtype, unit)
struct trans *buf;		/* Out: buffer to describe translation */
char *stype, *dtype;		/* In: source, dest data types */
int unit;			/* In: unit number for getting host */
{
   int status;

   current_call = VTRANS_INB;

   if (valid_unit(unit) != SUCCESS) {
      error_handler(unit, NO_SUCH_UNIT);
      return NO_SUCH_UNIT;
   }
   if (!(CURRENT_I_VALUE(FLAGS) & OPEN)) {
      error_handler(unit, FILE_NOT_OPEN);
      return FILE_NOT_OPEN;
   }

/* Strings are converted to upper case in the low-level routines */

   status = v2_translate_input(stype, dtype, CURRENT_S_VALUE(BINTFMT),
					  CURRENT_S_VALUE(BREALFMT), buf);

   if (status != SUCCESS) {
      error_handler(NO_UNIT, status);
      return status;
   }

   return SUCCESS;
}

/************************************************************************/
/* Use the buffer (translation structure) to do a translation.		*/
/************************************************************************/

void zvtrans(buf, source, dest, npix)
struct trans *buf;		/* In: buffer to describe translation */
void *source, *dest;		/* In: pointers to source & dest buffers */
int npix;			/* In: number of pixels to translate */
{

/* Note: source and dest should *not* overlap */

   if (buf->transfn == NULL)		/* No translation required */
      v2_move(dest, source, npix * buf->spixsize);
   else					/* Do the translation */
      (*buf->transfn)(source, dest, npix, buf);

   return;
}
