/*

  Written by Thomas Roatsch, DLR             2-Sep-1993
  Prototypes added by Vadim Parizher, JPL   27-Jun-1997

*/

/* Function file_no_path returns the filename without path */

#include "xvmaininc.h"
#include "p1proto.h"
#include <stdio.h>
#include <string.h>

void file_no_path(char *filename)
{
   char *value;
   
#ifdef __VMS
   value = strrchr(filename,':');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
   value = strrchr(filename,']');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
#else
   value = strrchr(filename,'/');
   if (value != NULL) {
      strcpy(filename,value);
      value = &filename[1];
      strcpy(filename,value);
   }
#endif   
}
