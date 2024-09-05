#include "zvproto.h"
#include "rtlintproto.h"
#include <ctype.h>

/* Copies a string from "in" to "out", converting it to upper case on the way */

void make_upper_case(out,in)
char *in, *out;
{
   int i,l;

   l = strlen(in);
   if (l==0) {
      *out='\0';
      return;
   }

   for (i=0; i<l; i++)
      out[i] = islower(in[i]) ? toupper(in[i]) : in[i];
   out[l] = '\0';

   return;
}

/* Same as make_upper_case(), but include a max length for the output.	*/
/* Output should be dimensioned [max+1] for the terminator.		*/
void make_upper_case_max(out,in,max)
char *in, *out;
int max;
{
   int i,l;

   l = strlen(in);
   if (l > max)
      l = max;
   if (l==0) {
      *out='\0';
      return;
   }

   for (i=0; i<l; i++)
      out[i] = islower(in[i]) ? toupper(in[i]) : in[i];
   out[l] = '\0';

   return;
}
