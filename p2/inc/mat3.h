/******************************************************************************
*                                                                             *
*                                     M A T 3                                 *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 12 Sep 1991                  *
*                                       Updated: 17 Mar 2000                  *
*                                                                             *
*      This file has miscellaneous mathematic functions to operate on         *
*      3-dimensional double-precision vectors and matrices.                   *
******************************************************************************/
#ifndef _MAT3_H
#define _MAT3_H

#include "xvmaininc.h"	       /* ONLY for _NO_PROTO definition */
			       /* (can be removed for non-VICAR environment) */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _NO_PROTO

double *add3(		/* vector addition: A+B -> C */
    double a[3],
    double b[3],
    double c[3]
    );
double (*add33(		/* matrix addition: A+B -> C */
    double a[3][3],
    double b[3][3],
    double c[3][3]
    ))[3];
double *copy3(		/* vector copy: A -> B */
    double a[3],
    double b[3]
    );
double (*copy33(	/* matrix copy: A -> B */
    double a[3][3],
    double b[3][3]
    ))[3];
double *copyq(		/* quaternion copy: A -> B */
    double a[4],
    double b[4]
    );
double *cross3(		/* vector cross product: AxB -> C */
    double a[3],
    double b[3],
    double c[3]
    );
double det33(		/* matrix determinant: |A| */
    double a[3][3]
    );
double dot3(		/* vector inner (dot) product: A.B */
    double a[3],
    double b[3]
    );
double (*ident33(	/* matrix identity: I -> A */
    double a[3][3]
    ))[3];
double (*inv33(		/* matrix inversion (general): 1/A -> B */
    double a[3][3],
    double b[3][3]
    ))[3];
double (*inv33pd(	/* matrix inversion (positive-definite): 1/A -> B */
    double a[3][3],
    double b[3][3]
    ))[3];
double mag3(		/* vector magnitude: |A| */
    double a[3]
    );
double *mult133(	/* vector-matrix multiply: AB -> C */
    double a[3],
    double b[3][3],
    double c[3]
    );
double (*mult313(	/* vector-vector multiply: AB -> C */
    double a[3],
    double b[3],
    double c[3][3]
    ))[3];
double *mult331(	/* matrix-vector multiply: MV -> U */
    double m[3][3],
    double v[3],
    double u[3]
    );
double (*mult333(	/* matrix-matrix multiply: AB -> C */
    double a[3][3],
    double b[3][3],
    double c[3][3]
    ))[3];
double *multq(		/* quaternion-quaternion multiply: AB -> C */
    double a[4],
    double b[4],
    double c[4]
    );
double *quatr(		/* quaternion from rotation matrix: f(R) -> Q */
    double r[3][3],
    double q[4]
    );
double *quatva(		/* quaternion from vector & angle: f(V,a) -> Q */
    double v[3],
    double a,
    double q[4]
    );
double (*rotq(		/* rotation matrix from quaternion: f(Q) -> R */
    double q[4],
    double r[3][3]
    ))[3];
double (*rotxyz(	/* rotation matrix from Euler XYZ: f(a,b,c) -> R */
    double a,
    double b,
    double c,
    double r[3][3]
    ))[3];
double (*rotzxz(	/* rotation matrix from Euler ZXZ: f(a,b,c) -> R */
    double a,
    double b,
    double c,
    double r[3][3]
    ))[3];
double (*rotzyz(	/* rotation matrix from Euler ZYZ: f(a,b,c) -> R */
    double a,
    double b,
    double c,
    double r[3][3]
    ))[3];
double *scale3(		/* vector scaling: sA -> B */
    double s,
    double a[3],
    double b[3]
    );
double (*scale33(	/* matrix scaling: sA -> B */
    double s,
    double a[3][3],
    double b[3][3]
    ))[3];
double *sub3(		/* vector subtraction: A-B -> C */
    double a[3],
    double b[3],
    double c[3]
    );
double (*sub33(		/* matrix subtraction: A-B -> C */
    double a[3][3],
    double b[3][3],
    double c[3][3]
    ))[3];
double (*trans33(	/* matrix transpose: At -> B */
    double a[3][3],
    double b[3][3]
    ))[3];
double *unit3(		/* vector normalize: A/|A| -> B */
    double a[3],
    double b[3]
    );
double *unitq(		/* quaternion normalize: f(A) -> B */
    double a[4],
    double b[4]
    );
double vaquat(		/* vector & angle from quaternion: f(Q) -> V,a */
    double q[4],
    double v[3],
    double *a
    );
double (*xyzrot(	/* Euler XYZ from rotation matrix: r -> a,b,c */
    double r[3][3],
    double *a,
    double *b,
    double *c
    ))[3];
double *zero3(		/* vector zero: 0 -> A */
    double a[3]
    );
double (*zero33(	/* matrix zero: 0 -> A */
    double a[3][3]
    ))[3];

#else

double  *add3();
double (*add33())[3];
double  *copy3();
double (*copy33())[3];
double  *copyq();
double  *cross3();
double   det33();
double   dot3();
double (*ident33())[3];
double (*inv33())[3];
double (*inv33pd())[3];
double   mag3();
double  *mult133();
double (*mult313())[3];
double  *mult331();
double (*mult333())[3];
double  *multq();
double  *quatr();
double  *quatva();
double (*rotq())[3];
double (*rotxyz())[3];
double (*rotzxz())[3];
double (*rotzyz())[3];
double  *scale3();
double (*scale33())[3];
double  *sub3();
double (*sub33())[3];
double (*trans33())[3];
double  *unit3();
double  *unitq();
double   vaquat();
double (*xyzrot())[3];
double  *zero3();
double (*zero33())[3];

#endif /* _NO_PROTO */

#ifdef __cplusplus
}	/* end extern "C" */
#endif

#endif /* _MAT3_H */
