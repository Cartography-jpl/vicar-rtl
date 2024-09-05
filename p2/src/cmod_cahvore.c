/******************************************************************************
*                                                                             *
*                                  C A H V O R E                              *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 11 May 1998                  *
*                                       Updated:  9 Aug 2002                  *
*                                                                             *
*                                       Copyright (C) 1998, 2000, 2001, 2002  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for using the camera model known
	locally as CAHVORE. This model is an extension by Yalin Xiong
	and Donald Gennery of the earlier CAHVOR (Gennery) and CAHV
	(Yakimovsky & Cunningham) models. */


#include <stdio.h>
#include <mat3.h>

#define SUCCESS 0
#define FAILURE (-1)

#define TRUE 1
#define FALSE 0

#define	PI (3.14159265358979323846)

#ifndef EPSILON
#define EPSILON 1e-15
#endif

#define MAX_NEWTON 100

#define TYPE_PERSPECTIVE 1
#define TYPE_FISHEYE     2
#define TYPE_GENERAL     3

typedef int bool_t;

extern double atan2();
extern double atan();
extern double tan();
extern double cos();
extern double acos();
extern double fabs();
extern double sin();
extern double asin();
extern double sqrt();

void cmod_cahvore_3d_to_2d();
static void cmod_cahvore_2d_to_3d_general();
static void cmod_cahvore_3d_to_2d_general();
static int cmod_read_scanstr();
void cmod_cahv_2d_to_3d(double pos2[2], double c[3], double a[3], double h[3], double v[3], double pos3[3], double uvec3[3],
			double  par[3][2]);
void cmod_cahv_internal(double c[3], double a[3], double h[3], double v[3], double s[12][12], 
			double* hs, double* hc, double* vs, double* vc, double* theta, double s_int[5][5]);
void cmod_cahv_3d_to_2d(double pos3[3], double c[3], double a[3], double h[3], double v[3], double* range, 
			double pos2[2], double par[2][3]);
void cmod_cahv_reflect(double c_i[3], double a_i[3], double h_i[3], double v_i[3], 
		       double p[3], double n[3],
		       double c_f[3], double a_f[3], double h_f[3], double v_f[3], bool_t *parallel, bool_t *behind);
void cmod_cahvor_reflect(double c_i[3], double a_i[3], double h_i[3], double v_i[3], double o_i[3], 
			 double r_i[3], double p[3], double n[3],
			 double c_f[3], double a_f[3], double h_f[3], double v_f[3], 
			 double o_f[3], double r_f[3] , bool_t* parallel, bool_t* behind);

#define scanstr_(fp, str) \
	if (cmod_read_scanstr(fp, str) == FAILURE) { \
	    fprintf(stderr, "Error looking for '%s' in file %s\n", \
		str, filename); \
	    fclose(fp); \
	    return FAILURE; \
	    }

#define scan_(fp, num, args) \
	if (fscanf args != num) { \
	    fprintf(stderr, "Error reading input data in file %s\n", \
		filename); \
	    fclose(fp); \
	    return FAILURE; \
	    }


/******************************************************************************
********************************   CMOD_CAHVORE_2D_TO_3D   ********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs the partial-derivative matrixes of the projection point
    and the unit vector with respect to the 2D image-plane point. If the
    parameter for either output partial matrix is passed as (double *)NULL,
    then it will not be calculated, note that it is necessary to calculate
    the partial for the unit vector if the one for the projection point
    is desired. */

void cmod_cahvore_2d_to_3d(
			pos2, mtype, mparm, c, a, h, v, o, r, e, approx,
			pos3, uvec3, ppar, upar)
double pos2[2];		/* input 2D position */
int mtype;		/* input type of model */
double mparm;		/* input model parameter */
double c[3];		/* input model center position vector   C */
double a[3];		/* input model orthog. axis unit vector A */
double h[3];		/* input model horizontal vector        H */
double v[3];		/* input model vertical vector          V */
double o[3];		/* input model optical axis unit vector O */
double r[3];		/* input model radial-distortion terms  R */
double e[3];		/* input model entrance-pupil    terms  E */
bool_t approx;		/* input flag to use fast approximation */
double pos3[3];		/* output 3D origin of projection */
double uvec3[3];	/* output unit vector ray of projection */
double ppar[3][2];	/* output partial-derivative matrix of pos3  to pos2 */
double upar[3][2];	/* output partial-derivative matrix of uvec3 to pos2 */
{
    double linearity;

    switch (mtype) {

	case TYPE_PERSPECTIVE:		/* perspective projection */
	    linearity = 1;
	    break;

	case TYPE_FISHEYE:		/* fisheye */
	    linearity = 0;
	    break;

	case TYPE_GENERAL:		/* parametric */
	    linearity = mparm;
	    break;

	default:
	    printf("cmod_cahvore_2d_to_3d(): Bad model type (%d)\n", mtype);
	    return;
	}

    cmod_cahvore_2d_to_3d_general(pos2, linearity, c, a, h, v, o, r, e, approx,
					pos3, uvec3, ppar, upar);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_2D_TO_3D_GENERAL   ************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs the partial-derivative matrixes of the projection point
    and the unit vector with respect to the 2D image-plane point. If the
    parameter for either output partial matrix is passed as (double *)NULL,
    then it will not be calculated, note that it is necessary to calculate
    the partial for the unit vector if the one for the projection point
    is desired. */

static void cmod_cahvore_2d_to_3d_general(pos2, linearity, c, a, h, v, o, r, e,
			approx, pos3, uvec3, ppar, upar)
double pos2[2];		/* input 2D position */
double linearity;	/* input linearity parameter */
double c[3];		/* input model center position vector   C */
double a[3];		/* input model orthog. axis unit vector A */
double h[3];		/* input model horizontal vector        H */
double v[3];		/* input model vertical vector          V */
double o[3];		/* input model optical axis unit vector O */
double r[3];		/* input model radial-distortion terms  R */
double e[3];		/* input model entrance-pupil    terms  E */
bool_t approx;		/* input flag to use fast approximation */
double pos3[3];		/* output 3D origin of projection */
double uvec3[3];	/* output unit vector ray of projection */
double ppar[3][2];	/* output partial-derivative matrix of pos3  to pos2 */
double upar[3][2];	/* output partial-derivative matrix of uvec3 to pos2 */
{
    double rp[3], zetap, lambdap, lambdap3[3], chip, cp[3], ri[3];
    double u3[3], v3[3], w3[3], avh1;
    double dcpdrp[3][3], drdrp[3][3], n33[3][3], m33[3][3];
    double chi, chi2, chi3, chi4, chi5;
    double linchi, theta, theta2, theta3, theta4;
    double drpdx[3], drpdy[3], dcpdx[3], dcpdy[3], drdx[3], drdy[3];

    /* In the following there is a mixture of nomenclature from several */
    /* versions of Gennery's write-ups and Litwin's software. Beware!   */

    /* Calculate initial terms */

    scale3(pos2[1], a, u3);
    sub3(v, u3, u3);
    scale3(pos2[0], a, v3);
    sub3(h, v3, v3);
    cross3(u3, v3, w3);
    cross3(v, h, u3);
    avh1 = 1/dot3(a, u3);
    scale3(avh1, w3, rp);

    zetap = dot3(rp, o);

    scale3(zetap, o, u3);
    sub3(rp, u3, lambdap3);

    lambdap = mag3(lambdap3);

    chip = lambdap / zetap;

    /* Approximations for small angles */
    if (chip < 1e-8) {
	copy3(c, cp);
	copy3(o, ri);
	}

    /* Full calculations */
    else {
	int n;
	double dchi, s;

	/* Calculate chi using Newton's Method */
	n = 0;
	chi = chip;
	dchi = 1;
	for (;;) {
	    double deriv;

	    /* Make sure we don't iterate forever */
	    if (++n > MAX_NEWTON) {
		printf("cahvore_2d_to_3d(): too many iterations\n");
		break;
		}

	    /* Compute terms from the current value of chi */
	    chi2 = chi * chi;
	    chi3 = chi * chi2;
	    chi4 = chi * chi3;
	    chi5 = chi * chi4;

	    /* Check exit criterion from last update */
	    if (fabs(dchi) < 1e-8)
		break;

	    /* Update chi */
	    deriv = (1 + r[0]) + 3*r[1]*chi2 + 5*r[2]*chi4;
	    dchi = ((1 + r[0])*chi + r[1]*chi3 + r[2]*chi5 - chip) / deriv;
	    chi -= dchi;
	    }

	/* Compute the incoming ray's angle */
	linchi = linearity * chi;
	if (linearity < -EPSILON)
	    theta = asin(linchi) / linearity;
	else if (linearity > EPSILON)
	    theta = atan(linchi) / linearity;
	else
	    theta = chi;

	theta2 = theta * theta;
	theta3 = theta * theta2;
	theta4 = theta * theta3;

	/* Compute the shift of the entrance pupil */
	s = (theta/sin(theta) - 1) * (e[0] + e[1]*theta2 + e[2]*theta4);

	/* The position of the entrance pupil */
	scale3(s, o, cp);
	add3(c, cp, cp);

	/* The unit vector along the ray */
	unit3(lambdap3, u3);
	scale3(sin(theta), u3, u3);
	scale3(cos(theta), o, v3);
	add3(u3, v3, ri);
	}

    copy3(cp, pos3);
    copy3(ri, uvec3);

    /* Optionally calculate the partial of pos3 & unit3 with respect to pos2 */
    if ((ppar == NULL) || (upar == NULL))
	return;

    cross3(v, a, u3);
    scale3(-avh1, u3, drpdx);

    cross3(h, a, u3);
    scale3(avh1, u3, drpdy);

    /* Approximations for small angles */
    if (chip < 1e-8) {

	zero33(dcpdrp);

	ident33(n33);
	mult313(o, o, m33);
	sub33(n33, m33, n33);
	scale33(1/(mag3(rp)*(1 + r[0])), n33, drdrp);
	}

    /* Full calculations */
    else {
	double nu, omega, t, sinth, costh, linth, psi;

	sinth = sin(theta);
	costh = cos(theta);

	nu = 2*r[1]*chi + 4*r[2]*chi3;

	omega = nu*chi + chip/chi;

	t = (1/sinth - theta*costh/(sinth*sinth))
				* (e[0] +   e[1]*theta2 +   e[2]*theta4)
	  + (theta/sinth - 1)	* (       2*e[1]*theta  + 4*e[2]*theta3);

	linth = linearity * theta;
	if (linearity < -EPSILON)
	    psi = cos(linth);
	else if (linearity > EPSILON) {
	    psi = 1/cos(linth);
	    psi *= psi;
	    }
	else
	    psi = 1;

	mult313(o, lambdap3, n33);
	scale33(t/(psi*zetap*lambdap*omega), n33, n33);
	mult313(o, o, m33);
	scale33(t*chip/(psi*zetap*omega), m33, m33);
	sub33(n33, m33, dcpdrp);

	ident33(n33);
	scale33(sinth/lambdap, n33, n33);
	mult313(o, o, m33);
	scale33(lambdap*sinth/(psi*zetap*zetap*omega) - sinth/lambdap,
			m33, m33);
	add33(n33, m33, n33);
	mult313(lambdap3, o, m33);
	scale33(costh/(psi*zetap*zetap*omega), m33, m33);
	sub33(n33, m33, n33);
	mult313(o, lambdap3, m33);
	scale33(sinth/(psi*zetap*lambdap*omega), m33, m33);
	sub33(n33, m33, n33);
	mult313(lambdap3, lambdap3, m33);
	scale33(costh/(psi*zetap*lambdap*lambdap*omega)
			- sinth/(lambdap*lambdap*lambdap), m33, m33);
	add33(n33, m33, drdrp);
	}

    /* Complete the partial derivatives */

    mult331(dcpdrp, drpdx, dcpdx);
    mult331(dcpdrp, drpdy, dcpdy);
    mult331(drdrp,  drpdx, drdx);
    mult331(drdrp,  drpdy, drdy);

    ppar[0][0] = dcpdx[0];
    ppar[1][0] = dcpdx[1];
    ppar[2][0] = dcpdx[2];
    ppar[0][1] = dcpdy[0];
    ppar[1][1] = dcpdy[1];
    ppar[2][1] = dcpdy[2];

    upar[0][0] = drdx[0];
    upar[1][0] = drdx[1];
    upar[2][0] = drdx[2];
    upar[0][1] = drdy[0];
    upar[1][1] = drdy[1];
    upar[2][1] = drdy[2];

    /* If requested, just use the approximations for speed */
    if (approx)
	return;

    /* Some day we might consider having approximate calculations */
    /*????*/
    }


/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D   ********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (double (*)[3])NULL, then it will not be calculated. */

void cmod_cahvore_3d_to_2d(pos3, mtype, mparm, c, a, h, v, o, r, e, approx,
			range, pos2, par)
double pos3[3];		/* input 3D position */
int mtype;		/* input type of model */
double mparm;		/* input model parameter */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis  O */
double r[3];		/* input model radial-distortion terms R */
double e[3];		/* input model entrance-pupil    terms E */
bool_t approx;		/* input flag to use fast approximation */
double *range;		/* output range along A (same units as C) */
double pos2[2];		/* output 2D image-plane projection */
double par[2][3];	/* output partial-derivative matrix of pos2 to pos3 */
{
    double linearity;

    switch (mtype) {

	case TYPE_PERSPECTIVE:		/* perspective projection */
	    linearity = 1;
	    break;

	case TYPE_FISHEYE:		/* fisheye */
	    linearity = 0;
	    break;

	case TYPE_GENERAL:		/* parametric */
	    linearity = mparm;
	    break;

	default:
	    printf("cmod_cahvore_3d_to_2d(): Bad model type (%d)\n", mtype);
	    return;
	}

    cmod_cahvore_3d_to_2d_general(pos3, linearity, c, a, h, v, o, r, e, approx,
					range, pos2, par);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D_GENERAL   ************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (double (*)[3])NULL, then it will not be calculated. */

static void cmod_cahvore_3d_to_2d_general(
			pos3, linearity, c, a, h, v, o, r, e, approx,
			range, pos2, par)
double pos3[3];		/* input 3D position */
double linearity;	/* input linearity parameter */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis  O */
double r[3];		/* input model radial-distortion terms R */
double e[3];		/* input model entrance-pupil    terms E */
bool_t approx;		/* input flag to use fast approximation */
double *range;		/* output range along A (same units as C) */
double pos2[2];		/* output 2D image-plane projection */
double par[2][3];	/* output partial-derivative matrix of pos2 to pos3 */
{
    int n;
    double p_c[3], zeta, lambda, lambda3[3];
    double dtheta, theta, theta2, theta3, theta4;
    double upsilon, costh, sinth;
    double rp[3], u3[3], v3[3], m33[3][3], n33[3][3];
    double drpdp[3][3];
    double alpha, beta, gamma, xh, yh;
    double dxhdrp[3], dyhdrp[3];

    /* In the following there is a mixture of nomenclature from several */
    /* versions of Gennery's write-ups and Litwin's software. Beware!   */

    /* Basic Computations */

    /* Calculate initial terms */
    sub3(pos3, c, p_c);
    zeta = dot3(p_c, o);
    scale3(zeta, o, u3);
    sub3(p_c, u3, lambda3);
    lambda = mag3(lambda3);

    /* Calculate theta using Newton's Method */
    n = 0;
    theta = atan2(lambda, zeta);
    dtheta = 1;
    for (;;) {

	/* Make sure we don't iterate forever */
	if (++n > MAX_NEWTON) {
	    printf("cahvore_3d_to_2d(): too many iterations\n");
	    break;
	    }

	/* Compute terms from the current value of theta */
	costh = cos(theta);
	sinth = sin(theta);
	theta2 = theta * theta;
	theta3 = theta * theta2;
	theta4 = theta * theta3;
	upsilon = zeta*costh + lambda*sinth
		- (1     - costh) * (e[0] +  e[1]*theta2 +   e[2]*theta4)
		- (theta - sinth) * (      2*e[1]*theta  + 4*e[2]*theta3);

	/* Check exit criterion from last update */
	if (fabs(dtheta) < 1e-8)
	    break;

	/* Update theta */
	dtheta = (
		zeta*sinth - lambda*costh
		- (theta - sinth) * (e[0] + e[1]*theta2 + e[2]*theta4)
		) / upsilon;
	theta -= dtheta;
	}

    /* Check the value of theta */
    if ((theta * fabs(linearity)) > PI/2)
	printf("cahvore_3d_to_2d(): theta out of bounds\n");

    /* Approximations for small theta */
    if (theta < 1e-8) {

	copy3(p_c, rp);

	if ((par != NULL) && !approx) {
	    ident33(m33);
	    scale33((1+r[0]), m33, m33);
	    mult313(o, o, n33);
	    scale33(r[0], n33, n33);
	    sub33(m33, n33, drpdp);
	    }
	}

    /* Full calculations */
    else {
	double linth, chi, chi2, chi3, chi4, psi, zetap, mu, nu;

	linth = linearity * theta;
	if (linearity < -EPSILON)
	    chi = sin(linth) / linearity;
	else if (linearity > EPSILON)
	    chi = tan(linth) / linearity;
	else
	    chi = theta;

	chi2 = chi * chi;
	chi3 = chi * chi2;
	chi4 = chi * chi3;

	zetap = lambda / chi;

	mu = r[0] + r[1]*chi2 + r[2]*chi4;

	scale3(zetap, o, u3);
	scale3(1+mu, lambda3, v3);
	add3(u3, v3, rp);

	if ((par != NULL) && !approx) {

	    if (linearity < -EPSILON)
		psi = cos(linth);
	    else if (linearity > EPSILON) {
		psi = 1 / cos(linth);
		psi *= psi;
		}
	    else
		psi = 1;

	    nu = 2*r[1]*chi + 4*r[2]*chi3;

	    ident33(m33);
	    scale33(1+mu, m33, m33);
	    scale3(zetap*psi*sinth/(chi*upsilon) - (1+mu), o, u3);
	    mult313(u3, o, n33);
	    add33(m33, n33, m33);
	    scale3(nu*psi*sinth/upsilon, lambda3, u3);
	    mult313(u3, o, n33);
	    sub33(m33, n33, m33);
	    scale3(1/(chi*lambda) - psi*costh/(chi2*upsilon), o, u3);
	    mult313(u3, lambda3, n33);
	    add33(m33, n33, m33);
	    scale3(nu*psi*costh/(lambda*upsilon), lambda3, u3);
	    mult313(u3, lambda3, n33);
	    add33(m33, n33, drpdp);
	    }
	}

    /* Calculate the projection */
    alpha  = dot3(rp, a);
    beta   = dot3(rp, h);
    gamma  = dot3(rp, v);
    pos2[0] = xh = beta  / alpha;
    pos2[1] = yh = gamma / alpha;
    *range = alpha;

    /* Only calculate the partial derivatives upon request */
    if (par == NULL)
	return;

    /* Calculate the approximate partial derivatives */

    scale3(xh, a, u3);
    sub3(h, u3, u3);
    scale3(1/alpha, u3, (approx ? par[0] : dxhdrp));

    scale3(yh, a, u3);
    sub3(v, u3, u3);
    scale3(1/alpha, u3, (approx ? par[1] : dyhdrp));

    /* If requested, just use the approximations for speed */
    if (approx)
	return;

    /* Complete the calculations for accuracy */
    mult133(dxhdrp, drpdp, par[0]);
    mult133(dyhdrp, drpdp, par[1]);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D_POINT   **************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane. In addition it calculates the partial-derivative matrix of the 2D
    point with respect to the 3D vector of the input ray. If the parameter for
    the output partial matrix is passed as (double (*)[3])NULL, then it will
    not be calculated. */

void cmod_cahvore_3d_to_2d_point(mtype, mparm, c, a, h, v, o, r, e, approx,
		pos3, uvec3, pos2, par)
int mtype;		/* input type of model */
double mparm;		/* input model parameter */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis  O */
double r[3];		/* input model radial-distortion terms R */
double e[3];		/* input model entrance-pupil    terms E */
bool_t approx;		/* input flag to use fast approximation */
double pos3[3];		/* input 3D position of line */
double uvec3[3];	/* input 3D unit vector of line */
double pos2[2];		/* output 2D image-plane projection */
double par[2][3];	/* output derivative matrix of pos2 to uvec3 */
{
    double p[3], range;
    static double e0[3] = {0, 0, 0};

    /* The vanishing point's projection will be the same as that of */
    /* a point a unit distance away in the proper direction. Since  */
    /* a finite motion of the entrance pupil becomes insignificant  */
    /* when compared to the infinity of the vanishing point's 3D    */
    /* position, we can set the E terms to zero, something we must  */
    /* do in this case since we are not moving to infinity.         */

    if (dot3(a, uvec3) >= 0)
	copy3(uvec3, p);
    else
	scale3(-1.0, uvec3, p);
    add3(c, p, p);
    cmod_cahvore_3d_to_2d(p, mtype, mparm, c, a, h, v, o, r, e0, approx,
			&range, pos2, par);

    /* I believe that the partial computed there should be correct */
    }


/******************************************************************************
********************************   CMOD_CAHVORE_ALIGN_MODELS   ****************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models with have their original C vectors, but will share newly
    computed A, H, V, O, R, and E terms, as well as internal model parameters.
    Note that image warping will be necessary in order to use the new models.
    If the E terms are non-zero, then the results will include some amount
    of error from internal parallax. */

void cmod_cahvore_align_models(
		xdim1, ydim1, mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
		xdim2, ydim2, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
			a, h, v, o, r, e, hs, hc, vs, vc, theta)
int xdim1;		/* input number of columns */
int ydim1;		/* input number of rows */
int mtype1;		/* input type of model */
double mparm1;		/* input model parameter */
double c1[3];		/* input model 1 center vector C */
double a1[3];		/* input model 1 axis   vector A */
double h1[3];		/* input model 1 horiz. vector H */
double v1[3];		/* input model 1 vert.  vector V */
double o1[3];		/* input model 1 axis   vector O */
double r1[3];		/* input model 1 dist.  terms  R */
double e1[3];		/* input model 1 pupil  terms  E */
int xdim2;		/* input number of columns */
int ydim2;		/* input number of rows */
int mtype2;		/* input type of model */
double mparm2;		/* input model parameter */
double c2[3];		/* input model 2 center vector C */
double a2[3];		/* input model 2 axis   vector A */
double h2[3];		/* input model 2 horiz. vector H */
double v2[3];		/* input model 2 vert.  vector V */
double o2[3];		/* input model 2 axis   vector O */
double r2[3];		/* input model 2 dist.  terms  R */
double e2[3];		/* input model 2 pupil  terms  E */
double a[3];		/* output virtual model axis   vector A */
double h[3];		/* output virtual model horiz. vector H */
double v[3];		/* output virtual model vert.  vector V */
double o[3];		/* output virtual model axis   vector O */
double r[3];		/* output virtual model dist.  terms  R */
double e[3];		/* output virtual model pupil  terms  E */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
{
    double hs1, hc1, vs1, vc1, theta1;
    double hs2, hc2, vs2, vc2, theta2;
    double rt[3], dn[3], p3[3], u3[3], vec1[3], vec2[3], p2[2], pointing[3];

    /* Check input */
    if ((mtype1 != mtype2) || (mparm1 != mparm2)) {
	printf("cmod_cahvore_align_models(): different model types\n");
	return;
	}

    /* Due to moving entrance pupil, O must move as little as possible */
    add3(o1, o2, o);
    scale3(0.5, o, o);

    /* Set R to zero and compute mean E terms */
    zero3(r);
    add3(e1, e2, e);
    scale3(0.5, e, e);

    /* Maybe later we could try something more sophisticated. For  */
    /* example, should the E terms be the root mean of some higher */
    /* power than 1, different for each of the 3 terms?            */

    /* Compute pointing vector */
    p2[0] = (xdim1-1)/2.0;
    p2[1] = (ydim1-1)/2.0;
    cmod_cahvore_2d_to_3d(p2, mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1, FALSE,
		    p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
    p2[0] = (xdim2-1)/2.0;
    p2[1] = (ydim2-1)/2.0;
    cmod_cahvore_2d_to_3d(p2, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2, FALSE,
		    p3, pointing, (double (*)[2])NULL, (double (*)[2])NULL);
    add3(u3, pointing, pointing);
    unit3(pointing, pointing);

    /* Compute right and down vectors */
    sub3(c2, c1, rt);		/* right vector */

    if(dot3(rt, h1) < 0)
      scale3(-1.0, rt, rt);

    cross3(pointing, rt, dn);	/* down vector */
    unit3(dn, dn);
    unit3(rt, rt);

    /* Compute A so that both image planes are in the same plane:   */
    /* A must be perpendicular to the baseline between the cameras. */
    /* It would be nice if we could choose A so that it was very    */
    /* close to the input values, but this would cause problems for */
    /* looking at objects very close to the cameras.                */
    cross3(rt, dn, a);
    unit3(a, a);

    /* Extract internal parameters */
    cmod_cahv_internal(c1, a1, h1, v1, (double (*)[12])NULL,
			&hs1, &hc1, &vs1, &vc1, &theta1, (double (*)[5])NULL);
    cmod_cahv_internal(c2, a2, h2, v2, (double (*)[12])NULL,
			&hs2, &hc2, &vs2, &vc2, &theta2, (double (*)[5])NULL);

    /* Compute mean of the scales */
    *hs = (hs1 + hs2) / 2.0;
    *vs = (vs1 + vs2) / 2.0;

    /* Assign idealized image centers and coordinate angles */
    *hc = (xdim2 - 1) / 2.0;
    *vc = (ydim2 - 1) / 2.0;

    /* Adjust image centers so that they project   */
    /* out along the mean input pointing direction */
    *hc -= *hs * dot3(pointing, rt);
    *vc -= *vs * dot3(pointing, dn);

    /* Is the above calculation right? Shouldn't it be done so that    */
    /* the full CAHVORE projection, not just the CAHV one, is correct? */
    /* Maybe it's close enough, since its near the image center?       */

    /* Assign idealized coordinate angles, which was assumed */
    /* in the calculations above                             */
    *theta = -PI / 2.0;

    /* Construct H and V */
    scale3(*hs, rt, vec1);
    scale3(*hc,  a, vec2);
    add3(vec1, vec2, h);
    scale3(*vs, dn, vec1);
    scale3(*vc,  a, vec2);
    add3(vec1, vec2, v);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_MOVE   ************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahvore_move(p_i, q_i, c_i, a_i, h_i, v_i, o_i, r_i, e_i,
		p_f, q_f, c_f, a_f, h_f, v_f, o_f, r_f, e_f)
double p_i[3];		/* input initial pos of camera ref pt */
double q_i[4];		/* input initial orientation (quat) of camera ref pt */
double c_i[3];		/* input initial model center vector C */
double a_i[3];		/* input initial model axis   vector A */
double h_i[3];		/* input initial model horiz. vector H */
double v_i[3];		/* input initial model vert.  vector V */
double o_i[3];		/* input initial model optical axis unit vector O */
double r_i[3];		/* input initial model radial-distortion terms  R */
double e_i[3];		/* input initial model entrance-pupil    terms  E */
double p_f[3];		/* input final pos of camera ref pt */
double q_f[4];		/* input final orientation (quat) of camera ref pt */
double c_f[3];		/* output final model center vector C */
double a_f[3];		/* output final model axis   vector A */
double h_f[3];		/* output final model horiz. vector H */
double v_f[3];		/* output final model vert.  vector V */
double o_f[3];		/* output final model optical axis unit vector O */
double r_f[3];		/* output final model radial-distortion terms  R */
double e_f[3];		/* output final model entrance-pupil    terms  E */
{
    double rqf[3][3], rqi[3][3], rqit[3][3], r[3][3];
    double d[3];

    /* Calculate the rotation from the initial to the final orientation */
    rotq(q_f, rqf);
    rotq(q_i, rqi);
    trans33(rqi, rqit);
    mult333(rqf, rqit, r);

    /* Rotate and translate the C vector */
    sub3(c_i, p_i, d);		/* delta vector from P_i to C_i */
    mult331(r, d, c_f);		/* rotate delta vector */
    add3(c_f, p_f, c_f);	/* reposition C_f from P_f */

    /* Rotate the A, H, V, O vectors */
    mult331(r, a_i, a_f);
    mult331(r, h_i, h_f);
    mult331(r, v_i, v_f);
    mult331(r, o_i, o_f);

    /* Copy over the R & E "vectors" unchanged */
    copy3(r_i, r_f);
    copy3(e_i, e_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_READ   ************************
*******************************************************************************

    This function reads a CAHVORE model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahvore_read(filename, xdim, ydim, mtype, mparm,
			c, a, h, v, o, r, e, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
int *xdim;		/* output number of columns */
int *ydim;		/* output number of rows */
int *mtype;		/* output type of model */
double *mparm;		/* output model parameter */
double c[3];		/* output model center vector C */
double a[3];		/* output model axis   vector A */
double h[3];		/* output model horiz. vector H */
double v[3];		/* output model vert.  vector V */
double o[3];		/* output model optical axis unit vector O */
double r[3];		/* output model radial-distortion terms  R */
double e[3];		/* output model entrance-pupil    terms  E */
double s[21][21];	/* output covariance of CAHVORE */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
double s_int[5][5];	/* output covariance matrix */
{
    int i;
    FILE *fp;

    /* Open the CAHVORE file */
    if ((fp = fopen(filename, "r")) == NULL) {
	fprintf(stderr, "Error opening CAHVORE file: %s\n", filename);
	return FAILURE;
	}

    /* Read in model type and image dimensions */
    scanstr_(fp, "Model =");
    scan_(fp, 1, (fp, " CAHVORE%d ", mtype));
    if (fscanf(fp, ",%lf", mparm) != 1)
	*mparm = 0;
    scanstr_(fp, "Dimensions =");
    scan_(fp, 2, (fp, " %d %d ", xdim, ydim));

    /* Read C, A, H, V, O, R, E vectors */
    scanstr_(fp, "C =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &c[0], &c[1], &c[2]));
    scanstr_(fp, "A =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &a[0], &a[1], &a[2]));
    scanstr_(fp, "H =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &h[0], &h[1], &h[2]));
    scanstr_(fp, "V =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &v[0], &v[1], &v[2]));
    scanstr_(fp, "O =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &o[0], &o[1], &o[2]));
    scanstr_(fp, "R =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &r[0], &r[1], &r[2]));
    scanstr_(fp, "E =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &e[0], &e[1], &e[2]));

    /* Read covariance matrix for C, A, H, V, O, R, E */
    scanstr_(fp, "S =");
    for (i=0; i<21; i++) {
	scan_(fp, 21, (fp, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf\
			%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf\n",
	    &s[i][ 0], &s[i][ 1], &s[i][ 2],
	    &s[i][ 3], &s[i][ 4], &s[i][ 5],
	    &s[i][ 6], &s[i][ 7], &s[i][ 8],
	    &s[i][ 9], &s[i][10], &s[i][11],
	    &s[i][12], &s[i][13], &s[i][14],
	    &s[i][15], &s[i][16], &s[i][17],
	    &s[i][18], &s[i][19], &s[i][20]
	    ));
	}

    /* Read internal model parameters */
    scanstr_(fp, "Hs    =");
    scan_(fp, 1, (fp, "%lf \n", hs));
    scanstr_(fp, "Hc    =");
    scan_(fp, 1, (fp, "%lf \n", hc));
    scanstr_(fp, "Vs    =");
    scan_(fp, 1, (fp, "%lf \n", vs));
    scanstr_(fp, "Vc    =");
    scan_(fp, 1, (fp, "%lf \n", vc));
    scanstr_(fp, "Theta =");
    scan_(fp, 1, (fp, "%lf \n", theta));

    /* Read covariance matrix for internal model parameters */
    scanstr_(fp, "S internal =");
    for (i=0; i<5; i++) {
	scan_(fp, 5, (fp, "%lf %lf %lf %lf %lf\n",
	    &s_int[i][ 0], &s_int[i][ 1],
	    &s_int[i][ 2], &s_int[i][ 3],
	    &s_int[i][ 4]
	    ));
	}

    /* Close the CAHVORE file */
    fclose(fp);

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_CAHVORE_REFLECT_COV   *****************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahvore_reflect_cov(s_i, n, s_f)
double s_i[21][21];	/* input initial covariance */
double n[3];		/* input normal to the reflecting plane */
double s_f[21][21];	/* output final covariance */
{
    double nu0, nu1, nu2, nu[3], r[3][3];
    void cmod_cahvore_transform_cov();

    /* Make sure that the normal is a unit vector */
    unit3(n, nu);
    nu0 = nu[0];
    nu1 = nu[1];
    nu2 = nu[2];

    /* Construct the transformation matrix */
    r[0][0] = -2*nu0*nu0 + 1;
    r[0][1] = -2*nu0*nu1;
    r[0][2] = -2*nu0*nu2;
    r[1][0] = -2*nu1*nu0;
    r[1][1] = -2*nu1*nu1 + 1;
    r[1][2] = -2*nu1*nu2;
    r[2][0] = -2*nu2*nu0;
    r[2][1] = -2*nu2*nu1;
    r[2][2] = -2*nu2*nu2 + 1;

    /* Transform the covariance */
    cmod_cahvore_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_REFLECT   *********************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahvore_reflect(c_i, a_i, h_i, v_i, o_i, r_i, e_i, p, n,
		c_f, a_f, h_f, v_f, o_f, r_f, e_f, parallel, behind)
double c_i[3];		/* input initial model center vector C */
double a_i[3];		/* input initial model axis   vector A */
double h_i[3];		/* input initial model horiz. vector H */
double v_i[3];		/* input initial model vert.  vector V */
double o_i[3];		/* input initial model optical axis unit vector O */
double r_i[3];		/* input initial model radial-distortion terms  R */
double e_i[3];		/* input initial model entrance-pupil    terms  E */
double p[3];		/* input point on the reflecting plane */
double n[3];		/* input normal to the reflecting plane */
double c_f[3];		/* output final model center vector C */
double a_f[3];		/* output final model axis   vector A */
double h_f[3];		/* output final model horiz. vector H */
double v_f[3];		/* output final model vert.  vector V */
double o_f[3];		/* output final model optical axis unit vector O */
double r_f[3];		/* output final model radial-distortion terms  R */
double e_f[3];		/* output final model entrance-pupil    terms  E */
bool_t *parallel;	/* output if camera view and plane are parallel */
bool_t *behind;		/* output if camera is behind the reflecting plane */
{
    cmod_cahvor_reflect(c_i, a_i, h_i, v_i, o_i, r_i, p, n,
		c_f, a_f, h_f, v_f, o_f, r_f, parallel, behind);
    copy3(e_i, e_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_ROT_COV   *********************
*******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rot_cov(r_i, s_i, r_f, s_f)
double r_i[3][3];	/* input initial orientation (rot) of camera ref pt */
double s_i[21][21];	/* input initial covariance */
double r_f[3][3];	/* input final orientation (rot) of camera ref pt */
double s_f[21][21];	/* output final covariance */
{
    double r_it[3][3], r[3][3];
    void cmod_cahvore_transform_cov();

    /* Calculate the rotation, R, from the initial to the final orientation */
    trans33(r_i, r_it);
    mult333(r_f, r_it, r);
    cmod_cahvore_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_ROTATE_COV   ******************
*******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rotate_cov(q_i, s_i, q_f, s_f)
double q_i[4];		/* input initial orientation (quat) of camera ref pt */
double s_i[21][21];	/* input initial covariance */
double q_f[4];		/* input final orientation (quat) of camera ref pt */
double s_f[21][21];	/* output final covariance */
{
    double r_f[3][3], r_i[3][3];

    rotq(q_f, r_f);
    rotq(q_i, r_i);
    cmod_cahvore_rot_cov(r_i, s_i, r_f, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_SCALE   ***********************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvore_scale(hscale, vscale, h1, v1, s1, h2, v2, s2)
double hscale;		/* input horizontal scale factor */
double vscale;		/* input vertical   scale factor */
double h1[3];		/* input  model horiz. vector H */
double v1[3];		/* input  model vert.  vector V */
double s1[21][21];	/* input  covariance matrix, or NULL */
double h2[3];		/* output model horiz. vector H */
double v2[3];		/* output model vert.  vector V */
double s2[21][21];	/* output covariance matrix, or NULL */
{
    int i, j;

    /* Scale the model */
    h2[0] = hscale * h1[0];
    h2[1] = hscale * h1[1];
    h2[2] = hscale * h1[2];
    v2[0] = vscale * v1[0];
    v2[1] = vscale * v1[1];
    v2[2] = vscale * v1[2];

    /* Optionally scale the covariance */
    if ((s1 == NULL) || (s2 == NULL))
	return;
    if (s1 != s2) {
	for (i=0; i<21; i++)
	    for (j=0; j<21; j++)
		s2[i][j] = s1[i][j];
	}
    for (i=0; i<21; i++) {
	for (j=6; j<9; j++) {
	    s2[i][j] *= hscale;
	    s2[j][i] *= hscale;
	    }
	for (j=9; j<12; j++) {
	    s2[i][j] *= vscale;
	    s2[j][i] *= vscale;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVORE_SHIFT   ***********************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvore_shift(dx, dy, a1, h1, v1, h2, v2)
double dx;		/* input horizontal shift */
double dy;		/* input vertical   shift */
double a1[3];		/* input  model axis   vector A */
double h1[3];		/* input  model horiz. vector H */
double v1[3];		/* input  model vert.  vector V */
double h2[3];		/* output model horiz. vector H */
double v2[3];		/* output model vert.  vector V */
{
    h2[0] = h1[0]  -  dx * a1[0];
    h2[1] = h1[1]  -  dx * a1[1];
    h2[2] = h1[2]  -  dx * a1[2];
    v2[0] = v1[0]  -  dy * a1[0];
    v2[1] = v1[1]  -  dy * a1[1];
    v2[2] = v1[2]  -  dy * a1[2];
    }


/******************************************************************************
********************************   CMOD_CAHVORE_TRANSFORM_COV   ***************
*******************************************************************************

    This function transform a CAHVORE model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvore_transform_cov(s_i, r, s_f)
double s_i[21][21];	/* input initial covariance */
double r[3][3];		/* input transformation matrix of camera ref pt */
double s_f[21][21];	/* output final covariance */
{
    int i, j, k;
    double d;
    static double r21[21][21], r21t[21][21], stemp[21][21];

    /* Contruct a matrix, R21, (and its transpose) to rotate the covariance
	|R      |
	| R     |
	|  R    |
	|   R   |
	|    R  |
	|     I |
	|      I|
    */
    for (i=0; i<21; i++)
	for (j=0; j<21; j++)
	    r21[i][j] = 0;
    for (i=0; i<3; i++) {
	for (j=0; j<3; j++) {
	    r21[i+ 0][j+ 0] = r[i][j];
	    r21[i+ 3][j+ 3] = r[i][j];
	    r21[i+ 6][j+ 6] = r[i][j];
	    r21[i+ 9][j+ 9] = r[i][j];
	    r21[i+12][j+12] = r[i][j];
	    }
	}
    r21[15][15] = 1;
    r21[16][16] = 1;
    r21[17][17] = 1;
    r21[18][18] = 1;
    r21[19][19] = 1;
    r21[20][20] = 1;
    for (i=0; i<21; i++)
	for (j=0; j<21; j++)
	    r21t[i][j] = r21[j][i];

    /* Pre-multiply by the matrix */
    for (i=0; i<21; i++) {
	for (j=0; j<21; j++) {
	    d = 0;
	    for (k=0; k<21; k++)
		d += r21[i][k] * s_i[k][j];
	    stemp[i][j] = d;
	    }
	}

    /* Post-multiply by the transpose */
    for (i=0; i<21; i++) {
	for (j=0; j<21; j++) {
	    d = 0;
	    for (k=0; k<21; k++)
		d += stemp[i][k] * r21t[k][j];
	    s_f[i][j] = d;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_FROM_CAHV   **************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    CAHVORE. */

void cmod_cahvore_warp_from_cahv(c1, a1, h1, v1, pos1, rdist, approx,
			mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2, pos2)
double c1[3];		/* input initial model center position vector   C */
double a1[3];		/* input initial model orthog. axis unit vector A */
double h1[3];		/* input initial model horizontal vector        H */
double v1[3];		/* input initial model vertical vector          V */
double pos1[2];		/* input 2D position from CAHV */
double rdist;		/* input radial distance to project */
bool_t approx;		/* input flag to use fast approximation */
int mtype2;		/* input final model type */
double mparm2;		/* input model parameter */
double c2[3];		/* input final model center position vector   C */
double a2[3];		/* input final model orthog. axis unit vector A */
double h2[3];		/* input final model horizontal vector        H */
double v2[3];		/* input final model vertical vector          V */
double o2[3];		/* input final model optical axis unit vector O */
double r2[3];		/* input final model radial-distortion terms  R */
double e2[3];		/* input final model entrance-pupil    terms  E */
double pos2[2];		/* output 2D position for CAHVOR */
{
    double range;
    double p3[3], u3[3];

    cmod_cahv_2d_to_3d(pos1, c1, a1, h1, v1, p3, u3, (double (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_cahvore_3d_to_2d(p3, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	approx, &range, pos2, (double (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_MODEL   ******************
*******************************************************************************

    This function warps a camera model so that it is purely linear. The
    parameters C and A will not change. The parameters O (identical to A)
    and R and E (all terms zero) will not be output. Note that image warping
    will be necessary in order to use the new models. */

void cmod_cahvore_warp_model(xdim, ydim, mtype, mparm, c, a, h, v, o, r, e,
			limfov, minfov,
			xdim2, ydim2, a2, h2, v2, hs, hc, vs, vc, theta)
int xdim;		/* input number of columns */
int ydim;		/* input number of rows */
int mtype;		/* input type of model */
double mparm;		/* input model parameter */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model axis   vector O */
double r[3];		/* input model dist.  terms  R */
double e[3];		/* input model pupil  terms  E */
double limfov;		/* input limit field of view: must be < Pi rad */
int minfov;		/* input if to minimize to common field of view */
int xdim2;		/* input number of columns of output model */
int ydim2;		/* input number of rows    of output model */
double a2[3];		/* output virtual model axis   vector A */
double h2[3];		/* output virtual model horiz. vector H */
double v2[3];		/* output virtual model vert.  vector V */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
{
    int i;
    double rt[3], dn[3], p3[3], u3[3], vec1[3], vec2[3], p2[2];
    double cs, x, hmin, hmax, vmin, vmax, hpts[6][2], vpts[6][2];

    /* Check input */
    if (limfov >= (0.99 * PI)) {
	printf("cmod_cahvore_warp_model(): limfov too large: %f\n", limfov);
	return;
	}

    /* Record the landmark 2D coordinates around the perimeter of the image */
    hpts[0][0] = 0;
    hpts[0][1] = 0;
    hpts[1][0] = 0;
    hpts[1][1] = (ydim-1)/2.0;
    hpts[2][0] = 0;
    hpts[2][1] = ydim-1;
    hpts[3][0] = xdim-1;
    hpts[3][1] = 0;
    hpts[4][0] = xdim-1;
    hpts[4][1] = (ydim-1)/2.0;
    hpts[5][0] = xdim-1;
    hpts[5][1] = ydim-1;
    vpts[0][0] = 0;
    vpts[0][1] = 0;
    vpts[1][0] = (xdim-1)/2.0;
    vpts[1][1] = 0;
    vpts[2][0] = xdim-1;
    vpts[2][1] = 0;
    vpts[3][0] = 0;
    vpts[3][1] = ydim-1;
    vpts[4][0] = (xdim-1)/2.0;
    vpts[4][1] = ydim-1;
    vpts[5][0] = xdim-1;
    vpts[5][1] = ydim-1;

    /* Choose a camera axis in the middle of the image */
    p2[0] = (xdim-1)/2.0;
    p2[1] = (ydim-1)/2.0;
    cmod_cahvore_2d_to_3d(p2, mtype, mparm, c, a, h, v, o, r, e, FALSE,
		    p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
    copy3(u3, a2);

    /* Compute the original right and down vectors */
    cross3(a,  h, dn);	/* down vector */
    cross3(dn, a, rt);	/* right vector */
    unit3(dn, dn);
    unit3(rt, rt);

    /* Adjust the right and down vectors to be orthogonal to new axis */
    cross3(dn, a2, rt);
    cross3(a2, rt, dn);
    unit3(dn, dn);
    unit3(rt, rt);

    /* Find horizontal and vertical fields of view */
    hmin =  1;
    hmax = -1;
    for (i=0; i<6; i++) {
	cmod_cahvore_2d_to_3d(hpts[i], mtype, mparm, c, a, h, v, o, r, e, FALSE,
			p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
	x = dot3(dn, u3);
	scale3(x, dn, vec1);
	sub3(u3, vec1, vec2);
	unit3(vec2, vec2);
	cs = dot3(a2, vec2);
	if (hmin > cs)
	    hmin = cs;
	if (hmax < cs)
	    hmax = cs;
	}
    vmin =  1;
    vmax = -1;
    for (i=0; i<6; i++) {
	cmod_cahvore_2d_to_3d(vpts[i], mtype, mparm, c, a, h, v, o, r, e, FALSE,
			p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
	x = dot3(rt, u3);
	scale3(x, rt, vec1);
	sub3(u3, vec1, vec2);
	unit3(vec2, vec2);
	cs = dot3(a2, vec2);
	if (vmin > cs)
	    vmin = cs;
	if (vmax < cs)
	    vmax = cs;
	}

    /* Compute the all-encompassing scale factors */
    cs = (!minfov ? hmin : hmax);	/* logic is reverse for cos() */
    if (acos(cs) > limfov)
	cs = cos(limfov);
    x = xdim2 / 2.0;
    *hs = x * cs / sqrt(1.0 - cs*cs);
    cs = (!minfov ? vmin : vmax);	/* logic is reverse for cos() */
    if (acos(cs) > limfov)
	cs = cos(limfov);
    x = ydim2 / 2.0;
    *vs = x * cs / sqrt(1.0 - cs*cs);

    /* Assign idealized image centers and coordinate angles */
    *hc = (xdim2 - 1) / 2.0;
    *vc = (ydim2 - 1) / 2.0;
    *theta = -PI / 2.0;

    /* Construct H and V */
    scale3(*hs, rt, vec1);
    scale3(*hc, a2, vec2);
    add3(vec1, vec2, h2);
    scale3(*vs, dn, vec1);
    scale3(*vc, a2, vec2);
    add3(vec1, vec2, v2);
    }


/******************************************************************************
**********************************   CMOD_CAHVORE_WARP_MODELS   ***************
*******************************************************************************
 
    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models will have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Since the output models will be linear, the parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */
 
void cmod_cahvore_warp_models(
 		xdim1, ydim1, mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
 		xdim2, ydim2, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
 		limfov, minfov, xdim, ydim, a, h, v, hs, hc, vs, vc, theta)
 int xdim1;		/* input number of columns */
 int ydim1;		/* input number of rows */
 int mtype1;		/* input type of model */
 double mparm1;		/* input model parameter */
 double c1[3];		/* input model 1 center vector C */
 double a1[3];		/* input model 1 axis   vector A */
 double h1[3];		/* input model 1 horiz. vector H */
 double v1[3];		/* input model 1 vert.  vector V */
 double o1[3];		/* input model 1 axis   vector O */
 double r1[3];		/* input model 1 dist.  terms  R */
 double e1[3];		/* input model 1 pupil  terms  E */
 int xdim2;		/* input number of columns */
 int ydim2;		/* input number of rows */
 int mtype2;		/* input type of model */
 double mparm2;		/* input model parameter */
 double c2[3];		/* input model 2 center vector C */
 double a2[3];		/* input model 2 axis   vector A */
 double h2[3];		/* input model 2 horiz. vector H */
 double v2[3];		/* input model 2 vert.  vector V */
 double o2[3];		/* input model 2 axis   vector O */
 double r2[3];		/* input model 2 dist.  terms  R */
 double e2[3];		/* input model 2 pupil  terms  E */
 double limfov;		/* input limit field of view: must be < Pi rad */
 int minfov;		/* input if to minimize to common field of view */
 int xdim;		/* input number of columns of output model */
 int ydim;		/* input number of rows    of output model */
 double a[3];		/* output virtual model axis   vector A */
 double h[3];		/* output virtual model horiz. vector H */
 double v[3];		/* output virtual model vert.  vector V */
 double *hs;		/* output horizontal scale factor */
 double *hc;		/* output horizontal center */
 double *vs;		/* output vertical scale factor */
 double *vc;		/* output vertical center */
 double *theta;		/* output angle between axes */
 {
     int i;
     double fwd[3], rt[3], dn[3], p3[3], u3[3], vec1[3], vec2[3], p2[2];
     double cs, x, hmin, hmax, vmin, vmax;
     double hs1, hc1, vs1, vc1, theta1;
     double hs2, hc2, vs2, vc2, theta2;
     double hpts1[2][2], vpts1[2][2];
     double hpts2[2][2], vpts2[2][2];
 
     /* Check input */
     if (limfov >= (0.99 * PI)) {
 	printf("cmod_cahvore_warp_models(): limfov too large: %f\n", limfov);
 	return;
 	}
 
     /* Extract internal parameters */
     cmod_cahv_internal(c1, a1, h1, v1, (double (*)[12])NULL,
 			&hs1, &hc1, &vs1, &vc1, &theta1, (double (*)[5])NULL);
     cmod_cahv_internal(c2, a2, h2, v2, (double (*)[12])NULL,
 			&hs2, &hc2, &vs2, &vc2, &theta2, (double (*)[5])NULL);
 
     /* Record the landmark 2D coordinates */
     hpts1[0][0] = 0;
     hpts1[0][1] = vc1;	/* (ydim1-1)/2.0 */
     hpts1[1][0] = xdim1-1;
     hpts1[1][1] = vc1;	/* (ydim1-1)/2.0 */
     vpts1[0][0] = hc1;	/* (xdim1-1)/2.0 */
     vpts1[0][1] = 0;
     vpts1[1][0] = hc1;	/* (xdim1-1)/2.0 */
     vpts1[1][1] = ydim1-1;
     hpts2[0][0] = 0;
     hpts2[0][1] = vc2;	/* (ydim2-1)/2.0 */
     hpts2[1][0] = xdim2-1;
     hpts2[1][1] = vc2;	/* (ydim2-1)/2.0 */
     vpts2[0][0] = hc2;	/* (xdim2-1)/2.0 */
     vpts2[0][1] = 0;
     vpts2[1][0] = hc2;	/* (xdim2-1)/2.0 */
     vpts2[1][1] = ydim2-1;
 
     /* Compute forward pointing vector */
     p2[0] = (xdim1-1)/2.0;
     p2[1] = (ydim1-1)/2.0;
     cmod_cahvore_2d_to_3d(p2, mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1, 
			   FALSE, p3, u3, (double (*)[2])NULL, 
			   (double (*)[2])NULL);
     p2[0] = (xdim2-1)/2.0;
     p2[1] = (ydim2-1)/2.0;
     cmod_cahvore_2d_to_3d(p2, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2, 
			   FALSE, p3, fwd, (double (*)[2])NULL, 
			   (double (*)[2])NULL);
     add3(u3, fwd, fwd);
     unit3(fwd, fwd);
 
     /* Compute the A, right, and down vectors */
     sub3(c2, c1, rt);	/* right vector */
     if (dot3(rt, h1) < 0)
 	scale3(-1.0, rt, rt);
     cross3(fwd, rt, dn);/* down vector */
     cross3(rt, dn, a);	/* A is orthogonal to baseline */
     unit3(dn, dn);
     unit3(rt, rt);
     unit3( a,  a);
 
     /* Note that the forward and A vectors are not the same. It is necessary*/
     /* to make the A vector orthogonal to the baseline between the cameras  */
     /* in order for the two output models to be epipolar-aligned. But this  */
     /* direction will not necessary be the same as the desired pointing     */
     /* direction. It seems to me that it should be possible to compensate   */
     /* for this by adjusting the image scale and center, at least for small */
     /* deviations between the two vectors, but I cannot see how to do it. So*/
     /* I'm going to take the easy way out and use A, not the forward vector,*/
     /* to determine the field of view. This will result in a diminished     */
     /* field of view for the minfov case, and an enlarged field of view     */
     /* otherwise.                                                           */
 
     /* Find largest and smallest horizontal and vertical fields of view */
     hmin =  1;
     hmax = -1;
     for (i=0; i<2; i++) {
 	cmod_cahvore_2d_to_3d(hpts1[i],
 		mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
 		FALSE, p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
 	cs = dot3(a, u3);
 	if (hmin > cs)
 	    hmin = cs;
 	if (hmax < cs)
 	    hmax = cs;
 	cmod_cahvore_2d_to_3d(hpts2[i],
 		mtype2, mparm2, c2, a2, h2, v2, o2, r2, e1,
 		FALSE, p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
 	cs = dot3(a, u3);
 	if (hmin > cs)
 	    hmin = cs;
 	if (hmax < cs)
 	    hmax = cs;
 	}
     vmin =  1;
     vmax = -1;
     for (i=0; i<2; i++) {
 	cmod_cahvore_2d_to_3d(vpts1[i],
 		mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
 		FALSE, p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
 	cs = dot3(a, u3);
 	if (vmin > cs)
 	    vmin = cs;
 	if (vmax < cs)
 	    vmax = cs;
 	cmod_cahvore_2d_to_3d(vpts2[i],
 		mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
 		FALSE, p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
 	cs = dot3(a, u3);
 	if (vmin > cs)
 	    vmin = cs;
 	if (vmax < cs)
 	    vmax = cs;
 	}
 
     /* Compute the all-encompassing scale factors */
     cs = (!minfov ? hmin : hmax);	/* logic is reverse for cos() */
     if (acos(cs) > limfov)
 	cs = cos(limfov);
     x = xdim / 2.0;
     *hs = x * cs / sqrt(1.0 - cs*cs);
     cs = (!minfov ? vmin : vmax);	/* logic is reverse for cos() */
     if (acos(cs) > limfov)
 	cs = cos(limfov);
     x = ydim / 2.0;
     *vs = x * cs / sqrt(1.0 - cs*cs);
 
     /* Assign idealized image centers and coordinate angles */
     *hc = (xdim - 1) / 2.0;
     *vc = (ydim - 1) / 2.0;
     *theta = -PI / 2.0;
 
     /* Would adjust horizontal scale and center here for fwd/A misalignment */
 
     /* Construct H and V */
     scale3(*hs, rt, vec1);
     scale3(*hc,  a, vec2);
     add3(vec1, vec2, h);
     scale3(*vs, dn, vec1);
     scale3(*vc,  a, vec2);
     add3(vec1, vec2, v);
     }

/******************************************************************************
********************************   CMOD_CAHVORE_WARP_TO_CAHV   ****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVORE and warps it into an image coordinate modeled by
    CAHV. */

void cmod_cahvore_warp_to_cahv(mtype, mparm,
			c1, a1, h1, v1, o1, r1, e1, pos1, rdist, approx,
			c2, a2, h2, v2, pos2)
int mtype;		/* input type of model */
double mparm;		/* input model parameter */
double c1[3];		/* input initial model center position vector   C */
double a1[3];		/* input initial model orthog. axis unit vector A */
double h1[3];		/* input initial model horizontal vector        H */
double v1[3];		/* input initial model vertical vector          V */
double o1[3];		/* input initial model optical axis unit vector O */
double r1[3];		/* input initial model radial-distortion terms  R */
double e1[3];		/* input initial model entrance-pupil    terms  E */
double pos1[2];		/* input 2D position from CAHVORE */
double rdist;		/* input radial distance to project */
bool_t approx;		/* input flag to use fast approximation */
double c2[3];		/* input final model center position vector   C */
double a2[3];		/* input final model orthog. axis unit vector A */
double h2[3];		/* input final model horizontal vector        H */
double v2[3];		/* input final model vertical vector          V */
double pos2[2];		/* output 2D position for CAHV */
{
    double range;
    double p3[3], u3[3];

    cmod_cahvore_2d_to_3d(pos1, mtype, mparm, c1, a1, h1, v1, o1, r1, e1,
	approx, p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_cahv_3d_to_2d(p3, c2, a2, h2, v2, &range, pos2, (double (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_TO_CAHVORE   *************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVORE and warps it into an image coordinate modeled by
    a different CAHVORE. */

void cmod_cahvore_warp_to_cahvore(mtype, mparm,
			c1, a1, h1, v1, o1, r1, e1, pos1, rdist, approx,
			c2, a2, h2, v2, o2, r2, e2, pos2)
int mtype;		/* input type of model */
double mparm;		/* input model parameter */
double c1[3];		/* input initial model center position vector   C */
double a1[3];		/* input initial model orthog. axis unit vector A */
double h1[3];		/* input initial model horizontal vector        H */
double v1[3];		/* input initial model vertical vector          V */
double o1[3];		/* input initial model optical axis unit vector O */
double r1[3];		/* input initial model radial-distortion terms  R */
double e1[3];		/* input initial model entrance-pupil    terms  E */
double pos1[2];		/* input 2D position from CAHVORE */
double rdist;		/* input radial distance to project */
bool_t approx;		/* input flag to use fast approximation */
double c2[3];		/* input final model center position vector   C */
double a2[3];		/* input final model orthog. axis unit vector A */
double h2[3];		/* input final model horizontal vector        H */
double v2[3];		/* input final model vertical vector          V */
double o2[3];		/* input final model optical axis unit vector O */
double r2[3];		/* input final model radial-distortion terms  R */
double e2[3];		/* input final model entrance-pupil    terms  E */
double pos2[2];		/* output 2D position for CAHV */
{
    double range;
    double p3[3], u3[3];

    cmod_cahvore_2d_to_3d(pos1, mtype, mparm, c1, a1, h1, v1, o1, r1, e1,
	approx, p3, u3, (double (*)[2])NULL, (double (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_cahvore_3d_to_2d(p3, mtype, mparm, c2, a2, h2, v2, o2, r2, e2, approx,
	&range, pos2, (double (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WRITE   ***********************
*******************************************************************************

    This function writes a CAHVORE model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahvore_write(filename, comment, xdim, ydim, mtype, mparm,
			c, a, h, v, o, r, e, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
char *comment;		/* input one-line comment to record in file */
int xdim;		/* input number of columns */
int ydim;		/* input number of rows */
int mtype;		/* input type of model */
double mparm;		/* input model parameter */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis unit vector O */
double r[3];		/* input model radial-distortion terms  R */
double e[3];		/* input model entrance-pupil    terms  E */
double s[21][21];	/* input covariance of CAHVORE */
double hs;		/* input horizontal scale factor */
double hc;		/* input horizontal center */
double vs;		/* input vertical scale factor */
double vc;		/* input vertical center */
double theta;		/* input angle between axes */
double s_int[5][5];	/* input covariance matrix */
{
    int i, j;
    FILE *fp;

    /* Open the CAHVORE file */
    if ((fp = fopen(filename, "w")) == NULL) {
	fprintf(stderr, "Error creating CAHVORE file: %s\n", filename);
	return FAILURE;
	}

    /* Write out the comment */
    fprintf(fp, "# %s\n", comment);

    /* Write out model type */
    fprintf(fp, "\n");
    if (mtype == 1)
	fprintf(fp, "Model = CAHVORE1 = perspective, distortion, pupil\n");
    else if (mtype == 2)
	fprintf(fp, "Model = CAHVORE2 = fish-eye\n");
    else if (mtype == 3)
	fprintf(fp, "Model = CAHVORE3,%g = general\n", mparm);
    else
	fprintf(fp, "Model = CAHVORE%d\n", mtype);

    /* Write out image dimensions */
    fprintf(fp, "\n");
    fprintf(fp, "Dimensions = %d %d\n", xdim, ydim);

    /* Write C, A, H, V vectors */
    fprintf(fp, "\n");
    fprintf(fp, "C = %13f %13f %13f\n", c[0], c[1], c[2]);
    fprintf(fp, "A = %13f %13f %13f\n", a[0], a[1], a[2]);
    fprintf(fp, "H = %13f %13f %13f\n", h[0], h[1], h[2]);
    fprintf(fp, "V = %13f %13f %13f\n", v[0], v[1], v[2]);
    fprintf(fp, "O = %13f %13f %13f\n", o[0], o[1], o[2]);
    fprintf(fp, "R = %13f %13f %13f\n", r[0], r[1], r[2]);
    fprintf(fp, "E = %13f %13f %13f\n", e[0], e[1], e[2]);

    /* Write covariance matrix for C, A, H, V, O, R, E */
    fprintf(fp, "\n");
    fprintf(fp, "S =\n");
    for (i=0; i<21; i++) {
	for (j=0; j<21; j++)
	    fprintf(fp, " %14.7e", s[i][j]);
	fprintf(fp, "\n");
	}

    /* Write internal model parameters */
    fprintf(fp, "\n");
    fprintf(fp, "Hs    = %13f\n", hs);
    fprintf(fp, "Hc    = %13f\n", hc);
    fprintf(fp, "Vs    = %13f\n", vs);
    fprintf(fp, "Vc    = %13f\n", vc);
    fprintf(fp, "Theta = %13f (%f deg)\n", theta, (theta * 180 / PI));

    /* Write covariance matrix for internal model parameters */
    fprintf(fp, "\n");
    fprintf(fp, "S internal =\n");
    for (i=0; i<5; i++) {
	for (j=0; j<5; j++)
	    fprintf(fp, " %14.7e", s_int[i][j]);
	fprintf(fp, "\n");
	}
    fprintf(fp, "\n");

    /* Close file */
    fclose(fp);

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_READ_SCANSTR   ************************
*******************************************************************************

    This function scans the input for the given string. It return SUCCESS or
    FAILURE. */

static int cmod_read_scanstr(FILE *fp, char *str)
{
    char *s;
    int c;

    s = str;
    for (;;) {
	c = getc(fp);
	if (c == EOF)
	    return FAILURE;
	if (c == *s) {
	    s++;
	    if (*s == '\0')
		return SUCCESS;
	    }
	else
	    s = str;
	}
    }
