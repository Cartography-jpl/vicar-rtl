#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/************************************************************************/
/* Close the parameter file opened by xvpopen().			*/
/************************************************************************/

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void F77_FUNC(xvpclose, XVPCLOSE)
(int *status)
{
   *status = zvpclose();
   return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

int zvpclose()
{
   struct PARM_LINE header;
   int status;

   header.p_type = PARM_V_EOF;			/* write EOF mark */
   header.version = 0;
   header.name_len = 0;
   status = parm_write((char *)&header, sizeof(header));
   if (status != SUCCESS)
      return status;

   status = parm_close();			/* flush buffers */
   if (status != SUCCESS)
      return status;

   status = zvclose(parm_file_unit, NULL);		/* Close parameter file	*/

   return status;
}
