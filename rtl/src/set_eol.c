#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* When labels are modified they may be expanded; as a result they may	*/
/* not fit in the old label space in the file. So the labels are split	*/
/* into two areas. When this occurs the EOL keyword in the first label	*/
/* area must be set to 1; this routine does that; 'p' points at the	*/
/* label.								*/

void v2_set_eol(p) 
char *p;
{
   char *place;
   char *value;
   int vallen;

   if (find_entry(p, "EOL", &value, &vallen, &place) != NULL)
      *value = '1';

   return;
}
