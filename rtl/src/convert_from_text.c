#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* Convert the ascii item in 'text' to its real, double, integer, or	*/
/* string form.								*/
/* NOTE:  The converted value is kept in an internal static buffer	*/
/* (*converted_item is set to it), so it better be used or copied	*/
/* elsewhere before this routine is called again.			*/

int v2_convert_from_text(converted_item, text, length, format)
char **converted_item;		/* out: ptr to resultant value */
char *text;			/* in: text to convert */
int length;			/* in: max length of an element */
char *format;			/* in: format of item */
{
   static char tempc[MAX_LABEL_VALUE_SIZE+1];
   static int tempi;
   static float tempf;
   static double tempd;

   if(EQUAL(format, "STRING")) {
     strcpy(tempc, dequoted(text, MIN((int) strlen(text),length)));
     *converted_item = (char *)tempc;
   }

   else if (EQUAL(format, "INT")) {
      tempi = atoi(dequoted(text, strlen(text)));
      *converted_item = (char*) &tempi;
   }

   else if (EQUAL(format, "REAL")) {
      tempf = (float)atof(dequoted(text, strlen(text)));
      *converted_item = (char*) &tempf;
   }
   else if (EQUAL(format, "DOUB")) {
      tempd = atof(dequoted(text, strlen(text)));
      *converted_item = (char*) &tempd;
   }

   else
      return ILLEGAL_FORMAT_REQUEST;

   return SUCCESS;
}
