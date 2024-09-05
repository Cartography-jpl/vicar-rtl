#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* This function acts upon a substring of a label string, deleting the	*/
/* substring between 'start' and 'stop' (not including *stop) and	*/
/* inserting in its place the string 'label_item'.			*/

int cut_label_string(start,stop,label_item)
char *start,*stop,*label_item;
{
   int len;

   len = strlen(label_item);

   /* If label_item is too big to fit between start and stop, return	*/
   /* failure, since this routine does no re-allocation.		*/

   if (stop - start < len)
      return FAILURE;

   strncpy(start, label_item, len);	/* insert 'label_item' */
   // This actually is a subtle bug. strcpy is *not* allowed to
   // overlap source and destination. Instead of strcpy, we just do
   // an actual copy
   // strcpy(start+len, stop);		/* delete the rest     */
   start += len;
   for(; *stop != '\0'; ++stop, ++start)
     *start = *stop;
   *start = '\0';

   return SUCCESS;
}
