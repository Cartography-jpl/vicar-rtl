#include "xvmaininc.h"
#include "ftnbridge.h"

static void zbswap(unsigned char *in, int n);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/
void F77_FUNC(bswap, BSWAP)
( in , n )
unsigned char *in;
int *n;
{
     zbswap(in,*n);
     return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

static void zbswap(unsigned char *in, int n)
{
  register int i, indx;
  register unsigned char temp;

  for (i=0; i<n;i++) {
    indx = i * 2;
    temp = in[indx];
    in[indx] = in[indx+1];
    in[indx+1] = temp;
  }
}
    
