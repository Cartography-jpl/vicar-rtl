#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* Given the buffer 'buf', this routine finds the first history	*/
/* label in 'buf' by searching for the history label delimiter,	*/
/* TASK_KEY; the location is returned in 'loc' and a TRUE	*/
/* indicator if a history label is found.			*/

int find_first_hist_item(buf, loc) 
char *buf;			/* In: buffer to search */
char **loc;			/* Out: location of history label */
{
   char *value;
   int vallen;

   buf = find_entry(buf,TASK_KEY,&value,&vallen,loc);

   if (buf == NULL)
      return FALSE;

   return TRUE;
}
