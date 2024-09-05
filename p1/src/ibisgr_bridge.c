/*
 *  ibisgr_bridge.c -- FORTRAN bridges to IBIS Graphics-1 code
 *
 *  NOTE!!!: Ported C code should call the "z" routines rather
 *   than the fortran bridges below. Only FORTRAN programs can
 *   call these routines.
 */

#include "xvmaininc.h"
#include "ftnbridge.h"
int zrdgr(int instance,int num,int dimension);
int zrdgr_unit(int unit,int num,int dimension);
int zwrgr(int instance,int num,int dimension);
int zwrgr_unit(int unit,int num,int dimension);
int zupdategr(int unit,int num,int dimension);
int zgetgr(int num,int *zero1,int *eof,float* first_c,float* second_c,float* other_c);
int znextgr(int num,int *eof,float* first_c,float* second_c,float* other_c);
int zputgr(int num,float first_c,float second_c,float* other_c);
int zendgr(int num);
int zsetgr(int num, int row);
int zclgr(int num);
void zsignalgr(int num,int status,int abendflag);

int F77_FUNC(rdgr, RDGR)
(instance,num,dimension)
int *instance;
int *num;
int *dimension;
{
	return zrdgr(*instance,*num,*dimension);
}

int F77_FUNC_(rdgr_unit, RDGR_UNIT)
(unit,num,dimension)
int *unit;
int *num;
int *dimension;
{
	return zrdgr_unit(*unit,*num,*dimension);
}

int F77_FUNC(wrgr, WRGR)
(instance,num,dimension)
int *instance;
int *num;
int *dimension;
{
	return zwrgr(*instance,*num,*dimension);
}

int F77_FUNC_(wrgr_unit, WRGR_UNIT)
(unit,num,dimension)
int *unit;
int *num;
int *dimension;
{
	return zwrgr_unit(*unit,*num,*dimension);
}

int F77_FUNC(updategr, UPDATEGR)
(unit,num,dimension)
int *unit;
int *num;
int *dimension;
{
	return zupdategr(*unit,*num,*dimension);
}

int F77_FUNC(getgr, GETGR)
(num,zero,eof,first_c,second_c,other_c)
int *num;
int *zero;
int *eof;
float *first_c;
float *second_c;
float *other_c;
{
	return zgetgr(*num,zero,eof,first_c,second_c,other_c);
}

int F77_FUNC(nextgr, NEXTGR)
(num,eof,first_c,second_c,other_c)
int *num;
int *eof;
float *first_c;
float *second_c;
float *other_c;
{
	return znextgr(*num,eof,first_c,second_c,other_c);
}

int F77_FUNC(putgr, PUTGR)
(num,first_c,second_c,other_c)
int *num;
float *first_c;
float *second_c;
float *other_c;
{
	return zputgr(*num,*first_c,*second_c,other_c);
}

int F77_FUNC(endgr, ENDGR)
(num)
int *num;
{
	return zendgr(*num);
}

int F77_FUNC(setgr, SETGR)
(num, row)
int *num;
int *row;
{
	return zsetgr(*num, *row);
}

int F77_FUNC(clgr, CLGR)
(num)
int *num;
{
	return zclgr(*num);
}

void F77_FUNC(signalgr, SIGNALGR)
(num,status,abortflag)
int *num;
int *status;
int *abortflag;
{
	(void) zsignalgr(*num,*status,*abortflag);
}



