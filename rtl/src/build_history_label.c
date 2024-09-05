#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* Appends the standard history label for this task to 'ps'.	*/
/* 'ps' is assumed to be big enough to hold the label.		*/

void build_history_label(ps)
char *ps;
{
   int i;

   for (i=0; i<N_HISTORY_ITEMS; i++) {
      strcat(ps, history[i].name);
      strcat(ps, "='");
      strcat(ps, history[i].value);
      strcat(ps, "'  ");
   }

   return;
}
