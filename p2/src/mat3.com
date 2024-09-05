$!****************************************************************************
$!
$! Build proc for MIPL module mat3
$! VPACK Version 1.9, Friday, August 09, 2002, 15:16:04
$!
$! Execute by entering:		$ @mat3
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module mat3 ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mat3.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("mat3.imake") .nes. ""
$   then
$      vimake mat3
$      purge mat3.bld
$   else
$      if F$SEARCH("mat3.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mat3
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mat3.bld "STD"
$   else
$      @mat3.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mat3.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mat3.com -mixed -
	-s mat3.c -
	-i mat3.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mat3.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/******************************************************************************
*                                                                             *
*                                     M A T 3                                 *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 12 Sep 1991                  *
*                                       Updated: 23 Jun 1997                  *
*                                                                             *
*******************************************************************************


	This file has miscellaneous mathematic functions to operate on
	3-dimensional double-precision vectors and matrices. */


#include <math.h>

#include "mat3.h"

#ifndef NULL
#define NULL 0
#endif

#ifndef EPSILON
#define EPSILON 1e-15
#endif


/******************************************************************************
********************************   ADD3   *************************************
*******************************************************************************

    This function adds two 3-vectors. */

double *add3(a, b, c)
double a[3];		/* input addend vector */
double b[3];		/* input addend vector */
double c[3];		/* output sum vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Add the two vectors */
    c[0] = a[0] + b[0];
    c[1] = a[1] + b[1];
    c[2] = a[2] + b[2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   ADD33   ************************************
*******************************************************************************

    This function adds together two 3x3 matrices. */

double (*add33(a, b, c))[3]
double a[3][3];		/* input addend matrix */
double b[3][3];		/* input addend matrix */
double c[3][3];		/* output sum matrix */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Calculate the sum */
    c[0][0] = a[0][0] + b[0][0];
    c[0][1] = a[0][1] + b[0][1];
    c[0][2] = a[0][2] + b[0][2];
    c[1][0] = a[1][0] + b[1][0];
    c[1][1] = a[1][1] + b[1][1];
    c[1][2] = a[1][2] + b[1][2];
    c[2][0] = a[2][0] + b[2][0];
    c[2][1] = a[2][1] + b[2][1];
    c[2][2] = a[2][2] + b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   COPY3   ************************************
*******************************************************************************

    This function copies a 3-vector. */

double *copy3(a, b)
double a[3];		/* input vector */
double b[3];		/* output vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Copy the two vectors */
    b[0] = a[0];
    b[1] = a[1];
    b[2] = a[2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   COPY33   ***********************************
*******************************************************************************

    This function adds copies a 3x3 matrix. */

double (*copy33(a, b))[3]
double a[3][3];		/* input matrix */
double b[3][3];		/* output matrix */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Copy all of the elements */
    b[0][0] = a[0][0];
    b[0][1] = a[0][1];
    b[0][2] = a[0][2];
    b[1][0] = a[1][0];
    b[1][1] = a[1][1];
    b[1][2] = a[1][2];
    b[2][0] = a[2][0];
    b[2][1] = a[2][1];
    b[2][2] = a[2][2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   COPYQ   ************************************
*******************************************************************************

    This function copies a quaternion (a 4-vector). */

double *copyq(a, b)
double a[4];		/* input vector */
double b[4];		/* output vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Copy the two vectors */
    b[0] = a[0];
    b[1] = a[1];
    b[2] = a[2];
    b[3] = a[3];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   CROSS3   ***********************************
*******************************************************************************

    This function performs the cross product of two 3-vectors. */

double *cross3(a, b, c)
double a[3];		/* input vector */
double b[3];		/* input vector */
double c[3];		/* output vector */
{
    double d[3];

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Perform the cross product */
    d[0]  =  a[1] * b[2]  -  a[2] * b[1];
    d[1]  =  a[2] * b[0]  -  a[0] * b[2];
    d[2]  =  a[0] * b[1]  -  a[1] * b[0];

    /* Return a pointer to the result */
    c[0] = d[0];
    c[1] = d[1];
    c[2] = d[2];
    return c;
    }


/******************************************************************************
********************************   DET33   ************************************
*******************************************************************************

    This function takes the determinant of a 3x3 matrix. */

double det33(a)
double a[3][3];		/* input matrix */
{
    double d;

    /* Check for NULL inputs */
    if (a == NULL)
	return 0.0;

    /* Calculate the determinant */
    d = a[0][0] * (a[1][1] * a[2][2]  -  a[1][2] * a[2][1]) -
	a[0][1] * (a[1][0] * a[2][2]  -  a[1][2] * a[2][0]) +
	a[0][2] * (a[1][0] * a[2][1]  -  a[1][1] * a[2][0]);

    /* Return determinant */
    return d;
    }


/******************************************************************************
********************************   DOT3   *************************************
*******************************************************************************

    This function performs the inner product of two 3-vectors. */

double dot3(a, b)
double a[3];		/* input vector */
double b[3];		/* input vector */
{
    double f;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return 0.0;

    /* Dot the two vectors */
    f = a[0] * b[0] +
	a[1] * b[1] +
	a[2] * b[2];

    /* Return the dot product */
    return f;
    }


/******************************************************************************
********************************   IDENT33   **********************************
*******************************************************************************

    This function creates a 3x3 identity matrix. */

double (*ident33(a))[3]
double a[3][3];		/* output matrix */
{
    /* Check for NULL inputs */
    if (a == NULL)
	return NULL;

    /* Define the elements of the identity matrix */
    a[0][0] = 1;
    a[0][1] = 0;
    a[0][2] = 0;
    a[1][0] = 0;
    a[1][1] = 1;
    a[1][2] = 0;
    a[2][0] = 0;
    a[2][1] = 0;
    a[2][2] = 1;

    /* Return a pointer to the result */
    return a;
    }


/******************************************************************************
********************************   INV33   ************************************
*******************************************************************************

    This function inverts any invertible 3x3 matrix. */

double (*inv33(a, b))[3]
double a[3][3];		/* input matrix */
double b[3][3];		/* output matrix */
{
    double det;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Check for non-distinct output */
    if (a == b)
	return NULL;

    /* Compute the determinant */
    det = a[0][0] * (a[1][1] * a[2][2] - a[2][1] * a[1][2])
	- a[1][0] * (a[0][1] * a[2][2] - a[2][1] * a[0][2])
	+ a[2][0] * (a[0][1] * a[1][2] - a[1][1] * a[0][2]);
    if ((det < EPSILON) && (det > -EPSILON))
	return NULL;

    b[0][0] =  (a[1][1] * a[2][2] - a[2][1] * a[1][2]) / det;
    b[0][1] = -(a[0][1] * a[2][2] - a[2][1] * a[0][2]) / det;
    b[0][2] =  (a[0][1] * a[1][2] - a[1][1] * a[0][2]) / det;

    b[1][0] = -(a[1][0] * a[2][2] - a[2][0] * a[1][2]) / det;
    b[1][1] =  (a[0][0] * a[2][2] - a[2][0] * a[0][2]) / det;
    b[1][2] = -(a[0][0] * a[1][2] - a[1][0] * a[0][2]) / det;

    b[2][0] =  (a[1][0] * a[2][1] - a[2][0] * a[1][1]) / det;
    b[2][1] = -(a[0][0] * a[2][1] - a[2][0] * a[0][1]) / det;
    b[2][2] =  (a[0][0] * a[1][1] - a[1][0] * a[0][1]) / det;

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   INV33PD   **********************************
*******************************************************************************

    This function inverts a positive-definite 3x3 matrix. Since the function
    inv33() is faster and is not restricted to positive-definite matrices,
    it is recommended over this one. The only reason for putting inv33pd()
    here is as an example of how to invert larger matrices. The recursive
    method used by inv33() does not scale nicely, and quickly becomes too
    slow. But this function scales up quite well. */

double (*inv33pd(a, b))[3]
double a[3][3];		/* input matrix */
double b[3][3];		/* output matrix */
{
    int i, j, k;
    double f;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Start with a copy of the input in the output */
    if (a != b) {
	b[0][0] = a[0][0];
	b[0][1] = a[0][1];
	b[0][2] = a[0][2];
	b[1][0] = a[1][0];
	b[1][1] = a[1][1];
	b[1][2] = a[1][2];
	b[2][0] = a[2][0];
	b[2][1] = a[2][1];
	b[2][2] = a[2][2];
	}

    /* Perform the matrix inversion */
    for (i=0; i<3; i++) {
	if (b[i][i] == 0.0)
	    return NULL;
	f = 1.0 / b[i][i];
	b[i][i] = 1.0;
	for (j=0; j<3; j++)
	    b[i][j] *= f;
	for (k=0; k<3; k++) {
	    if (i != k) {
		f = b[k][i];
		b[k][i] = 0.0;
		for (j=0; j<3; j++)
                    b[k][j] = b[k][j]  -  f * b[i][j];
		}
	    }
	}

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   MAG3   *************************************
*******************************************************************************

    This function calculates the magnitude of a 3-vector. */

double mag3(a)
double a[3];		/* input vector */
{
    double mag;

    /* Check for NULL inputs */
    if (a == NULL)
	return 0.0;

    /* Calculate the magnitude */
    mag = sqrt(a[0] * a[0]  +  a[1] * a[1]  +  a[2] * a[2]);

    /* Return the result */
    return mag;
    }


/******************************************************************************
********************************   MULT133   **********************************
*******************************************************************************

    This function multiplies a 3-vector (1x3 matrix) by a 3x3 matrix. */

double *mult133(a, b, c)
double a[3];		/* input vector */
double b[3][3];		/* input matrix */
double c[3];		/* output vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Check for non-distinct output */
    if ((a == c) || (b[0] == c))
	return NULL;

    /* Perform the matrix multiply */
    c[0] = a[0] * b[0][0]  +  a[1] * b[1][0]  +  a[2] * b[2][0];
    c[1] = a[0] * b[0][1]  +  a[1] * b[1][1]  +  a[2] * b[2][1];
    c[2] = a[0] * b[0][2]  +  a[1] * b[1][2]  +  a[2] * b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   MULT313   **********************************
*******************************************************************************

    This function multiplies a 3-vector (3x1 matrix) by a 3-vector
    (1x3 matrix). */

double (*mult313(a, b, c))[3]
double a[3];		/* input vector */
double b[3];		/* input vector */
double c[3][3];		/* output matrix */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Check for non-distinct output */
    if ((a == c[0]) || (b == c[0]))
	return NULL;

    /* Perform the matrix multiply */
    c[0][0] = a[0] * b[0];
    c[0][1] = a[0] * b[1];
    c[0][2] = a[0] * b[2];
    c[1][0] = a[1] * b[0];
    c[1][1] = a[1] * b[1];
    c[1][2] = a[1] * b[2];
    c[2][0] = a[2] * b[0];
    c[2][1] = a[2] * b[1];
    c[2][2] = a[2] * b[2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   MULT331   **********************************
*******************************************************************************

    This function multiplies a 3x3 matrix by a 3-vector (3x1 matrix). */

double *mult331(m, v, u)
double m[3][3];		/* input matrix */
double v[3];		/* input vector */
double u[3];		/* output vector */
{
    /* Check for NULL inputs */
    if ((m == NULL) || (v == NULL) || (u == NULL))
	return NULL;

    /* Check for non-distinct output */
    if (v == u)
	return NULL;

    /* Perform the matrix multiply */
    u[0] = m[0][0] * v[0]  +  m[0][1] * v[1]  +  m[0][2] * v[2];
    u[1] = m[1][0] * v[0]  +  m[1][1] * v[1]  +  m[1][2] * v[2];
    u[2] = m[2][0] * v[0]  +  m[2][1] * v[1]  +  m[2][2] * v[2];

    /* Return a pointer to the result */
    return u;
    }


/******************************************************************************
********************************   MULT333   **********************************
*******************************************************************************

    This function multiplies two 3x3 matrices. */

double (*mult333(a, b, c))[3]
double a[3][3];		/* input matrix */
double b[3][3];		/* input matrix */
double c[3][3];		/* output matrix */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Check for non-distinct output */
    if ((a == c) || (b == c))
	return NULL;

    /* Perform the matrix multiply */
    c[0][0] = a[0][0] * b[0][0]  +  a[0][1] * b[1][0]  +  a[0][2] * b[2][0];
    c[0][1] = a[0][0] * b[0][1]  +  a[0][1] * b[1][1]  +  a[0][2] * b[2][1];
    c[0][2] = a[0][0] * b[0][2]  +  a[0][1] * b[1][2]  +  a[0][2] * b[2][2];
    c[1][0] = a[1][0] * b[0][0]  +  a[1][1] * b[1][0]  +  a[1][2] * b[2][0];
    c[1][1] = a[1][0] * b[0][1]  +  a[1][1] * b[1][1]  +  a[1][2] * b[2][1];
    c[1][2] = a[1][0] * b[0][2]  +  a[1][1] * b[1][2]  +  a[1][2] * b[2][2];
    c[2][0] = a[2][0] * b[0][0]  +  a[2][1] * b[1][0]  +  a[2][2] * b[2][0];
    c[2][1] = a[2][0] * b[0][1]  +  a[2][1] * b[1][1]  +  a[2][2] * b[2][1];
    c[2][2] = a[2][0] * b[0][2]  +  a[2][1] * b[1][2]  +  a[2][2] * b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   MULTQ   ************************************
*******************************************************************************

    This function multiplies two quaternions together. */

double *multq(a, b, c)
double a[4];		/* input rotation quaternion */
double b[4];		/* input orientation quaternion */
double c[4];		/* output orientation quaternion */
{
    double a0, a1, a2, a3;
    double b0, b1, b2, b3;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Perform the conversion */
    a0 = a[0];
    a1 = a[1];
    a2 = a[2];
    a3 = a[3];
    b0 = b[0];
    b1 = b[1];
    b2 = b[2];
    b3 = b[3];
    c[0] = a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3;
    c[1] = a0 * b1 + a1 * b0 + a2 * b3 - a3 * b2;
    c[2] = a0 * b2 + a2 * b0 + a3 * b1 - a1 * b3;
    c[3] = a0 * b3 + a3 * b0 + a1 * b2 - a2 * b1;

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   QUATR   ************************************
*******************************************************************************

    This function converts a rotation matrix to an equivalent unit quaternion
    which represents rotation. */

double *quatr(r, q)
double r[3][3];		/* input rotation matrix */
double q[4];		/* output quaternion */
{
    int i, j, k;
    double t, fourq0, s, den, vj, vk, ui, uj, uk, b;

    /* Check for NULL inputs */
    if ((q == NULL) || (r == NULL))
	return NULL;

    /* Perform the conversion */
    t = r[0][0] + r[1][1] + r[2][2];
    if (t >= 1.0) {
	fourq0 = 2.0 * sqrt(1.0 + t);
	q[0] = 0.25 * fourq0;
	q[1] = (r[2][1] - r[1][2]) / fourq0;
	q[2] = (r[0][2] - r[2][0]) / fourq0;
	q[3] = (r[1][0] - r[0][1]) / fourq0;
	}
    else {
	i = 0;	/* we need to point i to largest diagonal element */
	s = r[0][0];
	if (r[1][1] > s) {
	    i = 1;
	    s = r[1][1];
	    }
	if (r[2][2] > s)
	    i = 2;
	j = (i + 1) % 3;
	k = (j + 1) % 3;
	s = 0.5 * sqrt(3.0 - t);
	den =  1.0 / ((1.0 - r[k][k]) * (1.0 - r[j][j]) - r[j][k] * r[k][j]);
	vj = ((1.0 - r[k][k]) * r[j][i] + r[j][k] * r[k][i]) * den;
	vk = ((1.0 - r[j][j]) * r[k][i] + r[k][j] * r[j][i]) * den;
	ui = 1.0 / sqrt(1.0 + vj * vj + vk * vk);
	uj = vj * ui;
	uk = vk * ui;
	b = r[k][j] - r[j][k];
	if (b < 0.0)
	    s = -s;
	q[0]   = b / (4.0 * s * ui);
	q[i+1] = s * ui;
	q[j+1] = s * uj;
	q[k+1] = s * uk;
	}

    /* Return a pointer to the result */
    return q;
    }


/******************************************************************************
********************************   QUATVA   ***********************************
*******************************************************************************

    This function converts a vector and an angle of rotation about that vector
    to an equivalent unit quaternion which represents rotation of an object
    in a fixed coordinate system. It is constructed for pre-multiplication. */

double *quatva(v, a, q)
double v[3];		/* input vector */
double a;		/* input angle of rotation */
double q[4];		/* output quaternion */
{
    double vmag, c, s;

    /* Check for NULL inputs */
    if ((q == NULL) || (v == NULL))
	return NULL;

    /* Precompute some needed quantities */
    vmag = sqrt(v[0] * v[0]  +  v[1] * v[1]  +  v[2] * v[2]);
    if (vmag < EPSILON)
	return NULL;
    c = cos(a/2);
    s = sin(a/2);

    /* Construct the quaternion */
    q[0] = c;
    q[1] = s * v[0] / vmag;
    q[2] = s * v[1] / vmag;
    q[3] = s * v[2] / vmag;

    /* Return a pointer to the result */
    return q;
    }


/******************************************************************************
********************************   ROTQ   *************************************
*******************************************************************************

    This function converts a unit quaternion which represents rotation to
    an equivalent rotation matrix. */

double (*rotq(q, r))[3]
double q[4];		/* input quaternion */
double r[3][3];		/* output rotation matrix */
{
    double q0, q1, q2, q3;
    double q0q0, q0q1, q0q2, q0q3;
    double q1q1, q1q2, q1q3;
    double q2q2, q2q3, q3q3;

    /* Check for NULL inputs */
    if ((q == NULL) || (r == NULL))
	return NULL;

    /* Perform the conversion */
    q0 = q[0];
    q1 = q[1];
    q2 = q[2];
    q3 = q[3];
    q0q0 = q0 * q0;
    q0q1 = q0 * q1;
    q0q2 = q0 * q2;
    q0q3 = q0 * q3;
    q1q1 = q1 * q1;
    q1q2 = q1 * q2;
    q1q3 = q1 * q3;
    q2q2 = q2 * q2;
    q2q3 = q2 * q3;
    q3q3 = q3 * q3;
    r[0][0] = q0q0 + q1q1 - q2q2 - q3q3;
    r[0][1] = 2.0 * (q1q2 - q0q3);
    r[0][2] = 2.0 * (q1q3 + q0q2);
    r[1][0] = 2.0 * (q1q2 + q0q3);
    r[1][1] = q0q0 + q2q2 - q1q1 - q3q3;
    r[1][2] = 2.0 * (q2q3 - q0q1);
    r[2][0] = 2.0 * (q1q3 - q0q2);
    r[2][1] = 2.0 * (q2q3 + q0q1);
    r[2][2] = q0q0 + q3q3 - q1q1 - q2q2;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   ROTXYZ   ***********************************
*******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. This function was constructed from the
    transpose, which is the inverse in this case, of the equations found
    in Herbert Goldstein, "Classical Mechanics," second edition, p. 609,
    called there the "xyz convention." */

double (*rotxyz(a, b, c, r))[3]
double a;		/* input angle about X axis (roll) */
double b;		/* input angle about Y axis (pitch) */
double c;		/* input angle about Z axis (yaw) */
double r[3][3];		/* output rotation matrix */
{
    double sin_phi, sin_theta, sin_psi;
    double cos_phi, cos_theta, cos_psi;

    /* Check for NULL inputs */
    if (r == NULL)
	return NULL;

    /* Precompute sine and cosine values */
    sin_phi   = sin(c);
    cos_phi   = cos(c);
    sin_theta = sin(b);
    cos_theta = cos(b);
    sin_psi   = sin(a);
    cos_psi   = cos(a);

    /* Compute rotation matrix */
    r[0][0] =  cos_theta * cos_phi;
    r[1][0] =  cos_theta * sin_phi;
    r[2][0] = -sin_theta;
    r[0][1] =  sin_psi * sin_theta * cos_phi  -  cos_psi * sin_phi;
    r[1][1] =  sin_psi * sin_theta * sin_phi  +  cos_psi * cos_phi;
    r[2][1] =  cos_theta * sin_psi;
    r[0][2] =  cos_psi * sin_theta * cos_phi  +  sin_psi * sin_phi;
    r[1][2] =  cos_psi * sin_theta * sin_phi  -  sin_psi * cos_phi;
    r[2][2] =  cos_theta * cos_psi;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   ROTZXZ   ***********************************
*******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are as performed on an object about the static world Z, then
    X, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated X axis, followed by the rotated Z axis.
    This function was constructed from the transpose, which is the inverse in
    this case, of the equations found in Herbert Goldstein, "Classical
    Mechanics," second edition, p. 147, called there the "x convention." */

double (*rotzxz(a, b, c, r))[3]
double a;		/* input angle about original Z axis */
double b;		/* input angle about rotated  X axis */
double c;		/* input angle about rotated  Z axis */
double r[3][3];		/* output rotation matrix */
{
    double sin_phi, sin_theta, sin_psi;
    double cos_phi, cos_theta, cos_psi;

    /* Check for NULL inputs */
    if (r == NULL)
	return NULL;

    /* Precompute sine and cosine values */
    sin_phi   = sin(a);
    cos_phi   = cos(a);
    sin_theta = sin(b);
    cos_theta = cos(b);
    sin_psi   = sin(c);
    cos_psi   = cos(c);

    /* Compute rotation matrix */
    r[0][0] =  cos_psi * cos_phi  -  cos_theta * sin_phi * sin_psi;
    r[1][0] =  cos_psi * sin_phi  +  cos_theta * cos_phi * sin_psi;
    r[2][0] =  sin_psi * sin_theta;
    r[0][1] = -sin_psi * cos_phi  -  cos_theta * sin_phi * cos_psi;
    r[1][1] = -sin_psi * sin_phi  +  cos_theta * cos_phi * cos_psi;
    r[2][1] =  cos_psi * sin_theta;
    r[0][2] =  sin_theta * sin_phi;
    r[1][2] = -sin_theta * cos_phi;
    r[2][2] =  cos_theta;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   ROTZYZ   ***********************************
*******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are as performed on an object about the static world Z, then
    Y, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated Y axis, followed by the rotated Z axis.
    This function was constructed from the transpose, which is the inverse in
    this case, of the equations found in Herbert Goldstein, "Classical
    Mechanics," second edition, p. 147, called there the "y convention." */

double (*rotzyz(a, b, c, r))[3]
double a;		/* input angle about original Z axis */
double b;		/* input angle about rotated  Y axis */
double c;		/* input angle about rotated  Z axis */
double r[3][3];		/* output rotation matrix */
{
    double sin_phi, sin_theta, sin_psi;
    double cos_phi, cos_theta, cos_psi;

    /* Check for NULL inputs */
    if (r == NULL)
	return NULL;

    /* Precompute sine and cosine values */
    sin_phi   = sin(a);
    cos_phi   = cos(a);
    sin_theta = sin(b);
    cos_theta = cos(b);
    sin_psi   = sin(c);
    cos_psi   = cos(c);

    /* Compute rotation matrix */
    r[0][0] = -sin_psi * sin_phi  +  cos_theta * cos_phi * cos_psi;
    r[1][0] =  sin_psi * cos_phi  +  cos_theta * cos_phi * cos_psi;
    r[2][0] = -cos_psi * sin_theta;
    r[0][1] = -cos_psi * sin_phi  -  cos_theta * cos_phi * sin_psi;
    r[1][1] =  cos_psi * cos_phi  -  cos_theta * sin_phi * sin_psi;
    r[2][1] =  sin_psi * sin_theta;
    r[0][2] =  sin_theta * cos_psi;
    r[1][2] =  sin_theta * sin_psi;
    r[2][2] =  cos_theta;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   SCALE3   ***********************************
*******************************************************************************

    This function multipies a 3-vector by a scalar. */

double *scale3(s, a, b)
double s;		/* input scalar */
double a[3];		/* input vector */
double b[3];		/* output vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Perform the scalar multiplication */
    b[0]  =  s * a[0];
    b[1]  =  s * a[1];
    b[2]  =  s * a[2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   SCALE33   **********************************
*******************************************************************************

    This function multipies a 3x3 matrix by a scalar. */

double (*scale33(s, a, b))[3]
double s;		/* input scalar */
double a[3][3];		/* input matrix */
double b[3][3];		/* output matrix */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Perform the scalar multiplication */
    b[0][0]  =  s * a[0][0];
    b[0][1]  =  s * a[0][1];
    b[0][2]  =  s * a[0][2];
    b[1][0]  =  s * a[1][0];
    b[1][1]  =  s * a[1][1];
    b[1][2]  =  s * a[1][2];
    b[2][0]  =  s * a[2][0];
    b[2][1]  =  s * a[2][1];
    b[2][2]  =  s * a[2][2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   SUB3   *************************************
*******************************************************************************

    This function subtracts two 3-vectors. */

double *sub3(a, b, c)
double a[3];		/* input minuend vector */
double b[3];		/* input subtrahend vector */
double c[3];		/* output difference vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Subtract the two vectors */
    c[0] = a[0] - b[0];
    c[1] = a[1] - b[1];
    c[2] = a[2] - b[2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   SUB33   ************************************
*******************************************************************************

    This function subtracts two 3x3 matrices. */

double (*sub33(a, b, c))[3]
double a[3][3];		/* input minuend matrix */
double b[3][3];		/* input subtrahend matrix */
double c[3][3];		/* output difference matrix */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Calculate the difference */
    c[0][0] = a[0][0] - b[0][0];
    c[0][1] = a[0][1] - b[0][1];
    c[0][2] = a[0][2] - b[0][2];
    c[1][0] = a[1][0] - b[1][0];
    c[1][1] = a[1][1] - b[1][1];
    c[1][2] = a[1][2] - b[1][2];
    c[2][0] = a[2][0] - b[2][0];
    c[2][1] = a[2][1] - b[2][1];
    c[2][2] = a[2][2] - b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   TRANS33   **********************************
*******************************************************************************

    This function transposes a 3x3 matrix. */

double (*trans33(a, b))[3]
double a[3][3];		/* input matrix */
double b[3][3];		/* output matrix */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Check for non-distinct output */
    if (a == b)
	return NULL;

    /* Perform the matrix transpose */
    b[0][0] = a[0][0];
    b[1][0] = a[0][1];
    b[2][0] = a[0][2];
    b[0][1] = a[1][0];
    b[1][1] = a[1][1];
    b[2][1] = a[1][2];
    b[0][2] = a[2][0];
    b[1][2] = a[2][1];
    b[2][2] = a[2][2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   UNIT3   ************************************
*******************************************************************************

    This function normalizes a 3-vector, producing a unit 3-vector. It returns
    a pointer to the result, or NULL on error. */

double *unit3(a, b)
double a[3];		/* input vector */
double b[3];		/* output vector */
{
    double mag;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Calculate the magnitude of the vector */
    mag = sqrt(a[0] * a[0]  +  a[1] * a[1]  +  a[2] * a[2]);
    if (mag < EPSILON)
	return NULL;

    /* Convert to a unit vector */
    b[0] = a[0] / mag;
    b[1] = a[1] / mag;
    b[2] = a[2] / mag;

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   UNITQ   ************************************
*******************************************************************************

    This function normalizes a quaternion, producing a unit quaternion. It
    returns a pointer to the result, or NULL on error. */

double *unitq(a, b)
double a[4];		/* input quaternion */
double b[4];		/* output quaternion */
{
    double mag;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Calculate the magnitude of the quaternion */
    mag = sqrt(a[0] * a[0]  +  a[1] * a[1]  +  a[2] * a[2]  +  a[3] * a[3]);
    if (mag < EPSILON)
	return NULL;

    /* Convert to a unit quaternion */
    b[0] = a[0] / mag;
    b[1] = a[1] / mag;
    b[2] = a[2] / mag;
    b[3] = a[3] / mag;

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   VAQUAT   ***********************************
*******************************************************************************

    This function converts a quaternion into a vector and an angle or rotation
    about that vector. If the quaternion is a unit quaternion, then the output
    vector will be a unit vector. See quatva() for comments on representation.
    */

double vaquat(q, v, a)
double q[4];		/* input quaternion */
double v[3];		/* output vector */
double *a;		/* output angle of rotation */
{
    double mag, s, aa;

    /* Check for NULL inputs */
    if ((q == NULL) || (v == NULL) || (a == NULL))
	return 0.0;

    /* Default values */
    v[0] = 1.0;
    v[1] = 0.0;
    v[2] = 0.0;
    *a   = 0;

    /* Compute the angle */
    mag = sqrt(q[0] * q[0]  +  q[1] * q[1]  +  q[2] * q[2]  +  q[3] * q[3]);
    if (mag < EPSILON)
	return 0.0;
    *a = aa = 2 * acos(q[0]/mag);

    /* Isolate the vector rotation axis */
    s = sin(aa/2);
    if ((s < EPSILON) && (s > -EPSILON))
	return 0.0;
    v[0] = q[1] / s;
    v[1] = q[2] / s;
    v[2] = q[3] / s;

    /* Return the angle */
    return aa;
    }


/******************************************************************************
********************************   XYZROT   ***********************************
*******************************************************************************

    This function converts a rotation matrix into three axis rotations. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. See rotxyz(). */

double (*xyzrot(r, a, b, c))[3]
double r[3][3];		/* input rotation matrix */
double *a;		/* output angle about X axis (roll) */
double *b;		/* output angle about Y axis (pitch) */
double *c;		/* output angle about Z axis (yaw) */
{
    double r00, r10, r20, r21, r22;

    /* Check for NULL inputs */
    if ((r == NULL) || (a == NULL) || (b == NULL) || (c == NULL))
	return NULL;

    /* Compute rotation angles */
    r00 = r[0][0];
    r10 = r[1][0];
    r20 = r[2][0];
    r21 = r[2][1];
    r22 = r[2][2];
    *a = atan2(r21, r22);
    *b = atan2(-r20, sqrt(r00*r00 + r10*r10));
    *c = atan2(r10, r00);

    /* Return a pointer to the input */
    return r;
    }


/******************************************************************************
********************************   ZERO3   ************************************
*******************************************************************************

    This function creates a zero 3-vector. */

double *zero3(a)
double a[3];		/* output vector */
{
    /* Check for NULL inputs */
    if (a == NULL)
	return NULL;

    /* Zero out the entire vector */
    a[0] = 0;
    a[1] = 0;
    a[2] = 0;

    /* Return a pointer to the result */
    return a;
    }


/******************************************************************************
********************************   ZERO33   ***********************************
*******************************************************************************

    This function creates a zero 3x3 matrix. */

double (*zero33(a))[3]
double a[3][3];		/* output matrix */
{
    /* Check for NULL inputs */
    if (a == NULL)
	return NULL;

    /* Zero out the entire matrix */
    a[0][0] = 0;
    a[0][1] = 0;
    a[0][2] = 0;
    a[1][0] = 0;
    a[1][1] = 0;
    a[1][2] = 0;
    a[2][0] = 0;
    a[2][1] = 0;
    a[2][2] = 0;

    /* Return a pointer to the result */
    return a;
    }
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mat3.imake
#define SUBROUTINE mat3

#define MODULE_LIST mat3.c

#define P2_SUBLIB

#define USES_ANSI_C

$ Return
$!#############################################################################
