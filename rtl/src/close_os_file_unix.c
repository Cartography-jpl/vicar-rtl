#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <errno.h>
#include "zvproto.h"
#include "rtlintproto.h"
#include <unistd.h>

/* Perform the actual OS close of the file */

int close_os_file(unit)
int unit;
{
int status;
struct bufstate *bufstate;
struct devstate *devstate;

bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
devstate = &bufstate->devstate;

status = SUCCESS;

switch (devstate->device_type) {
   case DEV_ARRAY:
      status = free_mapped_sect(&devstate->dev.array);
      bufstate->buffer = NULL;
      /* drop down to DEV_DISK */

   case DEV_DISK:
      if (close(devstate->dev.disk.channel) == -1 && status == SUCCESS)
         status = errno;		/* Let unmap error take precedence */
      break;

   default:
      break;

}

return status;
}
