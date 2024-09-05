#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <errno.h>
#include <stdio.h>

/* Delete the file */

int delete_file(unit)
int unit;
{
   int status;

   status = remove(CURRENT_S_VALUE(NAME));
   if (status != 0)
      return errno;

   return SUCCESS;
}
