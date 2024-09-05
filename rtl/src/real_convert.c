#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Host format floating-point translation functions.  Functions are	*/
/* supplied for real, double, and complex data types to convert between	*/
/* vax, ieee, and rieee (reverse ieee) formats.  It is a good idea to	*/
/* re-write at least some of these in assembler code, in a host-	*/
/* dependent file (like "real_convert_vms.mar"), then use #if's to	*/
/* remove the functions here that are not needed.			*/

static void swap4(char *from, char *to);
static void swap8(char *from, char *to);

/************************************************************************/
/* Single-precision floats  (REAL)					*/
/************************************************************************/

int r_vax2ieee(void *fromi, void* toi, int len, struct trans* UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
     vax_ieee_r((unsigned char*) from, (unsigned char*) to);
      from += 4;
      to += 4;
   }
   return SUCCESS;
}

/************************************************************************/

int r_vax2rieee(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      vax_ieee_r((unsigned char*)from, (unsigned char*)to);
      swap4(to, to);
      from+=4;
      to+=4;
   }
   return SUCCESS;
}

/************************************************************************/

int r_ieee2vax(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      ieee_vax_r((unsigned char*)from, (unsigned char*)to);
      from += 4;
      to += 4;
   }
   return SUCCESS;
}

/************************************************************************/

int r_rieee2vax(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;
   char temp[4];			/* Assume 4 chars per float */

   for (i=0; i<len; i++) {
      swap4(from, temp);
      ieee_vax_r((unsigned char*)temp, (unsigned char*)to);
      from+=4;
      to+=4;
   }
   return SUCCESS;
}

/************************************************************************/
/* Double-precision floats (DOUB)					*/
/************************************************************************/

int d_vax2ieee(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      vax_ieee_d((unsigned char*)from, (unsigned char*)to);
      from += 8;
      to += 8;
   }
   return SUCCESS;
}

/************************************************************************/

int d_vax2rieee(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      vax_ieee_d((unsigned char*)from, (unsigned char*)to);
      swap8(to, to);
      from+=8;
      to+=8;
   }
   return SUCCESS;
}

/************************************************************************/

int d_ieee2vax(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      ieee_vax_d((unsigned char*)from, (unsigned char*)to);
      from += 8;
      to += 8;
   }
   return SUCCESS;
}

/************************************************************************/

int d_rieee2vax(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;
   char temp[8];			/* Assume 8 chars per float */

   for (i=0; i<len; i++) {
      swap8(from, temp);
      ieee_vax_d((unsigned char*)temp, (unsigned char*)to);
      from+=8;
      to+=8;
   }
   return SUCCESS;
}

/************************************************************************/
/* Complex numbers (COMP) - just two REAL's in a row			*/
/************************************************************************/

int c_vax2ieee(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      vax_ieee_r((unsigned char*)from, (unsigned char*)to);		/* Real */
      from += 4;
      to += 4;
      vax_ieee_r((unsigned char*)from, (unsigned char*)to);		/* Imaginary */
      from += 4;
      to += 4;
   }
   return SUCCESS;
}

/************************************************************************/

int c_vax2rieee(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      vax_ieee_r((unsigned char*)from, (unsigned char*)to);		/* Real */
      swap4(to, to);
      from+=4;
      to+=4;
      vax_ieee_r((unsigned char*)from, (unsigned char*)to);		/* Imaginary */
      swap4(to, to);
      from+=4;
      to+=4;
   }
   return SUCCESS;
}

/************************************************************************/

int c_ieee2vax(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      ieee_vax_r((unsigned char*)from, (unsigned char*)to);		/* Real */
      from += 4;
      to += 4;
      ieee_vax_r((unsigned char*)from, (unsigned char*)to);		/* Imaginary */
      from += 4;
      to += 4;
   }
   return SUCCESS;
}

/************************************************************************/

int c_rieee2vax(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;
   char temp[4];			/* Assume 4 chars per float */

   for (i=0; i<len; i++) {
      swap4(from, temp);			/* Real */
      ieee_vax_r((unsigned char*)temp, (unsigned char*)to);
      from+=4;
      to+=4;
      swap4(from, temp);			/* Imaginary */
      ieee_vax_r((unsigned char*)temp, (unsigned char*)to);
      from+=4;
      to+=4;
   }
   return SUCCESS;
}

/************************************************************************/

int c_ieee2rieee(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      swap4(from, to);			/* Real */
      from+=4;
      to+=4;
      swap4(from, to);			/* Imaginary */
      from+=4;
      to+=4;
   }
   return SUCCESS;
}

/************************************************************************/

int c_rieee2ieee(void *fromi, void* toi, int len, struct trans *UNUSED(trans))
{
   char *from = (char *) fromi;
   char *to = (char *) toi;
   register int i;

   for (i=0; i<len; i++) {
      v2_move(to, from, 4);			/* Real */
      swap4(to, to);
      from+=4;
      to+=4;
      v2_move(to, from, 4);			/* Imaginary */
      swap4(to, to);
      from+=4;
      to+=4;
   }
   return SUCCESS;
}
/************************************************************************/
/* Miscellaneous swap routines for the above.				*/
/************************************************************************/

/* Swaps four bytes, moving them in the process.  From may = to	*/
/* (i.e., you can swap in place).				*/

static void swap4(char *from, char *to)
{
   char temp;

   temp = from[0];
   to[0] = from[3];
   to[3] = temp;
   temp = from[1];
   to[1] = from[2];
   to[2] = temp;
}

/* Swaps eight bytes, moving them in the process.  From may = to*/
/* (i.e., you can swap in place).				*/

static void swap8(char *from, char *to)
{
   char temp;

   temp = from[0];
   to[0] = from[7];
   to[7] = temp;

   temp = from[1];
   to[1] = from[6];
   to[6] = temp;

   temp = from[2];
   to[2] = from[5];
   to[5] = temp;

   temp = from[3];
   to[3] = from[4];
   to[4] = temp;
}

