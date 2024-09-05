#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zcylpatch - Compute Line intercepting equator 	*/
/************************************************************************/
struct data
  {
  float rdata[38];
  int idata;
  float rdata40;
  };

void zcylpatch (rdata)
struct data  *rdata;			/* input structure of data */

{

F77_FUNC(cylpatch, CYLPATCH)
( rdata); /* invoke cylpatch */

}
