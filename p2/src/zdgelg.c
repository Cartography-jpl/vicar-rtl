#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of DGELG                                         */
/************************************************************************/

void zdgelg(r,a,m,n,eps,ifail)
double *r,
       *a;
int     m,
        n,
        eps,
        ifail;
{
F77_FUNC(dgelg, DGELG)
(r,a,m,n,eps,ifail);
}
