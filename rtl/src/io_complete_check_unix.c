#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Wait for and return the status of a pending io on the file associated */
/* with 'state'.							 */

int io_complete_check(struct devstate * UNUSED(devstate))
{
   return SUCCESS;			/* No async I/O on Unix (yet) */
}
