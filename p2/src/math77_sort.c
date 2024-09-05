/************************************************************************/
/* C-callable version of MATH77 sort routines.                          */
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"

void zisort(buf,m,n)
int *buf;
int m,n;
{
F77_FUNC(isort, ISORT)
(buf,&m,&n);
}

void zssort(buf,m,n)
float *buf;
int m,n;
{
F77_FUNC(ssort, SSORT)
(buf,&m,&n);
}

void zdsort(buf,m,n)
double *buf;
int m,n;
{
F77_FUNC(dsort, DSORT)
(buf,&m,&n);
}

void zisortp(buf,m,n,ip)
int *buf;
int m,n;
int *ip;
{
F77_FUNC(isortp, ISORTP)
(buf,&m,&n,ip);
}

void zssortp(buf,m,n,ip)
float *buf;
int m,n;
int *ip;
{
F77_FUNC(ssortp, SSORTP)
(buf,&m,&n,ip);
}

void zdsortp(buf,m,n,ip)
double *buf;
int m,n;
int *ip;
{
F77_FUNC(dsortp, DSORTP)
(buf,&m,&n,ip);
}
