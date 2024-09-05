/************************************************************************/
/* Amoeba algorithm for finding a minimum of the given multidimensional	*/
/* function.  Implements the "downhill simplex method" of Nelder and	*/
/* Mead.								*/
/* Taken by Jean Lorre from Numerical Recipes.				*/
/* Genericized by Bob Deen.						*/
/*									*/
/* P is an array of NDIM+1 rows, where each row is a potential solution.*/
/* The algorithm operates by moving the solutions around in		*/
/* multidimensional space using one of 4 defined transforms, "flowing"	*/
/* the highest values "downhill", in an amoeba-like manner.  See the	*/
/* book for more details.						*/
/*									*/
/* NDIM is the number of dimensions (independent variables).		*/
/*									*/
/* P must be initialized as follows:  the first row is an initial guess	*/
/* at the solution.  Each successive row is equal to that first row,	*/
/* with one of the variables perturbed by an amount "which is your	*/
/* guess of the problem's characteristic length scale", so each variable*/
/* is perterbed in one and only one row.  Upon completion, any row will	*/
/* hold a valid solution, although P[0] is normally used.  See amoeba2	*/
/* or amoeba3 for wrapper functions which do this initialization for	*/
/* you.									*/
/*									*/
/* Physically, P must be an array[][WIDTH], where WIDTH is the physical	*/
/* size of the second dimension.  Only NDIM elements are used from the	*/
/* second dimension.  The first dimension must be big enough to hold	*/
/* NDIM+1 rows.  Note that P must be a true two-dimensional array, not	*/
/* an array of pointers to arrays.  The code internally is complicated	*/
/* somewhat in order to support a variable WIDTH dimension.		*/
/*									*/
/* Y is an array of NDIM+1 values, which hold the results of the	*/
/* function evaluation for each trial in P.  It must be pre-initialized	*/
/* based on the initial P.						*/
/*									*/
/* FTOL is the fractional convergence tolerance to be achieved in the	*/
/* function. 0.00000001 (1e-8) is a good value.				*/
/*									*/
/* The number of iterations is returned in ITER.  ITMAX is the maximum	*/
/* number of iterations allowed (suggestion: 5000).			*/
/*									*/
/* "func" is a pointer to the actual function to be minimized.  It	*/
/* must have the signature:						*/
/*   double func(double p[], int ndim, void *func_args);		*/
/* AmoebaFunc is a typedef for this pointer defined in the include	*/
/* file.  If you're using C++, this function must be declared		*/
/* extern "C".								*/
/*									*/
/* "func_args" is an opaque pointer that is passed into the function.	*/
/* It will normally be a pointer to a structure that contains all the	*/
/* additional arguments that the function needs.			*/
/*									*/
/* Sample call:								*/
/*	struct CostParams {						*/
/*	    int a, b;  double c;					*/
/*	};								*/
/*	double cost(double p[], int ndim, void *func_args)		*/
/*	    struct CostParams *params = (struct CostParams *)func_args;	*/
/*	    return params->a + p[0] * params->c - p[1];	    (whatever)	*/
/*	}								*/
/*      ... in the caller ...						*/
/*	struct CostParams parm;						*/
/*	double P[11][10], Y[11];					*/
/*	parm.a = 5;  parm.b = 10;  parm.c = 3.14159;			*/
/*	... fill up P[0][*] with initial solution ...			*/
/*	... copy P[0][*] to P[1:10][*] ...				*/
/*	... add labmda to P[i+1][i] ...					*/
/*	for (i=0; i<11; i++)						*/
/*	    Y[i] = cost(P[i], 10, &parm);				*/
/*	amoeba(P, Y, 10, 10, 1e-8, 1000, &iter, cost, &parm);		*/
/*	... P[0][*] contains solution and Y[0] the function value ...	*/
/*									*/
/* Note that this is callable only from C; due to the function pointers	*/
/* and such a Fortran bridge is not feasible.  Use the original Fortran	*/
/* code from the book if you need to.					*/
/************************************************************************/

#include "amoeba.h"
#include <zvproto.h>
#include <math.h>
#include <stdlib.h>

/* Access to P as P(row,col) */
#define P(row,col) (Parg[(row)*WIDTH + (col)])

void amoeba(double *Parg, double Y[], int NDIM, int WIDTH,
	double FTOL, int ITMAX, int *ITER, AmoebaFunc func, void *func_args)
{
    int I, J, MPTS, ILO, IHI, INHI;
    double RTOL, ALPHA, BETA, GAMMA, YPR, YPRR;
    double *PR, *PRR, *PBAR;
    double denom;

    PR = (double *)malloc(NDIM * sizeof(double));
    PRR = (double *)malloc(NDIM * sizeof(double));
    PBAR = (double *)malloc(NDIM * sizeof(double));
    if (!PR  || !PRR || !PBAR) {
	zvmessage("Amoeba unable to allocate memory!", "");
	free(PR); free(PRR); free(PBAR);
	return;
    }

    ALPHA=1.0;	/* Three params which define the expansions and contractions */
    BETA=0.5;
    GAMMA=2.0;
    MPTS=NDIM+1;
    *ITER=0;

    while (1) {					/* returns from middle */

	/* First we must determine which point is the highest (worst),	*/
	/* next-highest, and lowest (best),				*/

	ILO=0;
	if (Y[0] > Y[1]) {
	    IHI=0;
	    INHI=1;
	}
	else {
	    IHI=1;
	    INHI=0;
	}

	/* by looping over the points in the simplex */

	for (I=0; I < MPTS; I++) {
	    if (Y[I] < Y[ILO]) ILO=I;
	    if (Y[I] > Y[IHI]) {
		INHI=IHI;
		IHI=I;
	    }
	    else if (Y[I] > Y[INHI]) {
		if (I != IHI) INHI=I;
	    }
	}

	/* Compute the fractional range from highest to lowest and	*/
	/* return if satisfactory.					*/

	denom = fabs(Y[IHI])+fabs(Y[ILO]);
	if (denom == 0.0) {
	    free(PR); free(PRR); free(PBAR);
	    return;			/* degenerate case, avoid divide by 0 */
	}
	RTOL=2.*fabs(Y[IHI]-Y[ILO])/denom;
	if (RTOL < FTOL) {		/* Success! */
	    free(PR); free(PRR); free(PBAR);
	    return;
	}
	if (*ITER == ITMAX) {		/* Oops! */
	    zvmessage("Amoeba exceeding maximum iterations", "");
	    free(PR); free(PRR); free(PBAR);
	    return; 
	}

	*ITER= *ITER+1;
	for (J=0; J < NDIM; J++) {
	    PBAR[J]=0.;
	}

	/* Begin a new iteration.  Compute the vector average of all	*/
	/* points except the highest, i.e. the center of the "face"	*/
	/* of the simplex across from the high point.  We will		*/
	/* subsequently explore along the ray from the high point	*/
	/* through that center.						*/

	for (I=0; I < MPTS; I++) {
	    if (I != IHI) {
		for (J=0; J < NDIM; J++) {
		    PBAR[J]=PBAR[J]+P(I,J);
		}
	    }
	}

	/* Extrapolate by a factor ALPHA through the face, i.e. reflect	*/
	/* the simplex from the high point.				*/

	for (J=0; J < NDIM; J++) {
	    PBAR[J]=PBAR[J]/NDIM;
	    PR[J]=(1.+ALPHA)*PBAR[J]-ALPHA*P(IHI,J);
	}

	/* Evaluate the function at the reflected point */

	YPR = (*func)(PR, NDIM, func_args);
	if (YPR <= Y[ILO]) {

	    /* Gives a result better than the best point, so try an	*/
	    /* additional extrapolation by a factor GAMMA,		*/

	    for (J=0; J < NDIM; J++) {
		PRR[J]=GAMMA*PR[J]+(1.-GAMMA)*PBAR[J];
	    }

	    /* and check out the function there. */

	    YPRR = (*func)(PRR, NDIM, func_args);
	    if (YPRR < Y[ILO]) {

		/* The additional extrapolation succeeded, and replaces	*/
		/* the high point.					*/

		for (J=0; J < NDIM; J++) {
		    P(IHI,J)=PRR[J];
		}
		Y[IHI]=YPRR;
	    }
	    else {

		/* The additional extrapolation failed, but we can	*/
		/* still use the reflected point.			*/

		for (J=0; J < NDIM; J++) {
		    P(IHI,J)=PR[J];
		}
		Y[IHI]=YPR;
	    }
	}
	else if (YPR >= Y[INHI]) {

	    /* The reflected point is worse than the second-highest.	*/

	    if (YPR < Y[IHI]) {

		/* If it's better than the highest, then replace the highest, */

		for (J=0; J < NDIM; J++) {
		    P(IHI,J)=PR[J];
		}
		Y[IHI]=YPR;
	    }

	    /* but look for an intermediate lower point, in other words	*/
	    /* perform a contraction of the simplex along one dimension.*/
	    /* Then evaluate the function.				*/

	    for (J=0; J < NDIM; J++) {
		PRR[J]=BETA*P(IHI,J)+(1.-BETA)*PBAR[J];
	    }
	    YPRR = (*func)(PRR, NDIM, func_args);

	    if (YPRR < Y[IHI]) {

		/* Contraction gives an improvement, so accept it.	*/

		for (J=0; J < NDIM; J++) {
		    P(IHI,J)=PRR[J];
		}
		Y[IHI]=YPRR;
	    }
	    else {

		/* Can't seem to get rid of that high point.  Better	*/
		/* contract around the lowest (best) point.		*/

		for (I=0; I < MPTS; I++) {
		    if (I != ILO) {
			for (J=0; J < NDIM; J++) {
			    PR[J]=0.5*(P(I,J)+P(ILO,J));
			    P(I,J)=PR[J];
			}
			Y[I] = (*func)(PR, NDIM, func_args);
		    }
		}
	    }
	}
	else {

	    /* We arrive here if the original reflection gives a	*/
	    /* middling point.  Replace the old high point and continue.*/

	    for(J=0; J < NDIM; J++) {
		P(IHI,J)=PR[J];
	    }
	    Y[IHI]=YPR;
	}
    }	/* Go back to the test of doneness and the next iteration. */
}

/************************************************************************/
/* This is a wrapper around the amoeba algorithm which does the		*/
/* initialization for you.  You provide only the initial solution,	*/
/* Pzero, as a simple vector of size NDIM, plus the "length scale"	*/
/* constant, lambda.  This wrapper will allocate P and Y, fill them	*/
/* up appropriately, call amoeba, and return the P[0] solution in	*/
/* Pzero an the Y[0] value as the function return.			*/
/*									*/
/* This should handle all uses of amoeba except when you want lambda	*/
/* to be a vector, e.g. a different "length scale" for each variable.	*/
/* See amoeba3 for this case.						*/
/*									*/
/* Note that the P macro from above is used here as well.		*/
/*									*/
/* Sample call:								*/
/*	struct CostParams {						*/
/*	    int a, b;  double c;					*/
/*	};								*/
/*	double cost(double p[], int ndim, void *func_args)		*/
/*	    struct CostParams *params = (struct CostParams *)func_args;	*/
/*	    return params->a + p[0] * params->c - p[1];	    (whatever)	*/
/*	}								*/
/*      ... in the caller ...						*/
/*	struct CostParams parm;						*/
/*	double Pzero[10], value;					*/
/*	parm.a = 5;  parm.b = 10;  parm.c = 3.14159;			*/
/*	... fill up Pzero[*] with initial solution ...			*/
/*	value = amoeba2(Pzero, .1, 10, 1e-8, 1000, &iter, cost, &parm);	*/
/*	... Pzero contains solution and "value" the function value ...	*/
/*									*/
/************************************************************************/

double amoeba2(double *Pzero, double lambda,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args)
{
    int i, j;
    double *Parg, *Y;
    double result;
    int WIDTH = NDIM;

    Parg = (double *)malloc(NDIM * (NDIM+1) * sizeof(double));
    Y = (double *)malloc((NDIM+1) * sizeof(double));

    if (!Parg  || !Y) {
	zvmessage("Amoeba2 unable to allocate memory!", "");
	free(Parg); free(Y);
	return 0;
    }

    /* Fill in the initial solution */

    for (j=0; j < NDIM; j++) {
	for (i=0; i < NDIM+1; i++) {
	    P(i,j) = Pzero[j];
	}
    }

    /* Perturb the initial solutions */

    for (i=0; i < NDIM; i++) {
	P(i+1,i) += lambda;		/* diagonal elements, skipping row 0 */
    }

    /* Calculate the initial Y's */

    for (i=0; i < NDIM+1; i++) {
	Y[i] = (*func)(&P(i,0), NDIM, func_args);
    }

    /* Call amoeba */

    amoeba(Parg, Y, NDIM, WIDTH, FTOL, ITMAX, ITER, func, func_args);

    /* Return results */

    for (j=0; j < NDIM; j++) {
	Pzero[j] = P(0,j);
    }
    result = Y[0];

    free(Parg); free(Y);

    return result;
}

/************************************************************************/
/* This is another wrapper around the amoeba algorithm which does the	*/
/* initialization for you.  It is exactly like amoeba2, except that the	*/
/* "length scale" constant, lambda, is an array of doubles (of size	*/
/* NDIM) instead of a single value.  This allows you to have a		*/
/* different length scale for each variable.				*/
/************************************************************************/

double amoeba3(double *Pzero, double *lambda_vec,
		int NDIM, double FTOL, int ITMAX, int *ITER,
		AmoebaFunc func, void *func_args)
{
    int i, j;
    double *Parg, *Y;
    double result;
    int WIDTH = NDIM;

    Parg = (double *)malloc(NDIM * (NDIM+1) * sizeof(double));
    Y = (double *)malloc((NDIM+1) * sizeof(double));

    if (!Parg  || !Y) {
	zvmessage("Amoeba2 unable to allocate memory!", "");
	free(Parg); free(Y);
	return 0;
    }

    /* Fill in the initial solution */

    for (j=0; j < NDIM; j++) {
	for (i=0; i < NDIM+1; i++) {
	    P(i,j) = Pzero[j];
	}
    }

    /* Perturb the initial solutions */

    for (i=0; i < NDIM; i++) {
	P(i+1,i) += lambda_vec[i];	/* diagonal elements, skipping row 0 */
    }

    /* Calculate the initial Y's */

    for (i=0; i < NDIM+1; i++) {
	Y[i] = (*func)(&P(i,0), NDIM, func_args);
    }

    /* Call amoeba */

    amoeba(Parg, Y, NDIM, WIDTH, FTOL, ITMAX, ITER, func, func_args);

    /* Return results */

    for (j=0; j < NDIM; j++) {
	Pzero[j] = P(0,j);
    }
    result = Y[0];

    free(Parg); free(Y);

    return result;
}

