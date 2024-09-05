/******************************************************************************
*                                                                             *
*                                  C A H V O R                                *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  3 Aug 1993                  *
*                                       Updated:  7 Mar 2002                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998, 1999, 2000, *
*                                                     2001, 2002              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for using the camera model known
	locally as CAHVOR. This model is an extension by Donald Gennery
	into radial distortion of the linear model by Yakimovsky &
	Cunningham, known locally as CAHV. */


#include <stdio.h>
#include <mat3.h>

#define SUCCESS 0
#define FAILURE (-1)

#define TRUE 1
#define FALSE 0

#ifndef EPSILON
#define EPSILON 1e-15
#endif

#define	PI (3.14159265358979323846)

typedef int bool_t;

extern double atan2();
extern double cos();
extern double acos();
extern double fabs();
extern double sin();
extern double asin();
extern double sqrt();

void cmod_cahvor_3d_to_2d();
static int cmod_read_scanstr(FILE* fp, char *str);
void cmod_cahv_2d_to_3d(double pos2[2], double c[3], double a[3], double h[3], double v[3], double pos3[3], double uvec3[3],
			double  par[3][2]);
void cmod_cahv_internal(double c[3], double a[3], double h[3], double v[3], double s[12][12], 
			double* hs, double* hc, double* vs, double* vc, double* theta, double s_int[5][5]);
void cmod_cahv_3d_to_2d(double pos3[3], double c[3], double a[3], double h[3], double v[3], double* range, 
			double pos2[2], double par[2][3]);

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
********************************   CMOD_CAHVOR_2D_TO_3D   *********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point.  If the parameter
    for the output partial matrix is passed as (double (*)[2])NULL, then it
    will not be calculated. */

#define MAXITER  20     /* maximum number of iterations allowed */
#define CONV   1.0e-6   /* covergence tolerance */

void cmod_cahvor_2d_to_3d(pos2, c, a, h, v, o, r, approx, pos3, uvec3, par)
double pos2[2];		/* input 2D position */
double c[3];		/* input model center position vector   C */
double a[3];		/* input model orthog. axis unit vector A */
double h[3];		/* input model horizontal vector        H */
double v[3];		/* input model vertical vector          V */
double o[3];		/* input model optical axis unit vector O */
double r[3];		/* input model radial-distortion terms  R */
bool_t approx;		/* input flag to use fast approximation */
double pos3[3];		/* output 3D origin of projection */
double uvec3[3];	/* output unit vector ray of projection */
double par[3][2];	/* output partial-derivative matrix of uvec3 to pos2 */
{
    int i, j;
    double omega, omega_2, tau, mu, u, u_2, du, k1, k3, k5, poly, deriv;
    double f[3], g[3], rr[3], pp[3], wo[3], lambda[3];
    double m33[3][3], n33[3][3];
    double sgn, t[3], w[3], irrt[3][3], magv, magi, v3[3], u3[3];
    double drpdx[3], drpdy[3], dldr[3][3], dudt;
    double drpdr[3][3], drpdri[3][3], drdx[3], drdy[3];

    /* The projection point is merely the C of the camera model. */
    copy3(c, pos3);

    /* Calculate the projection ray assuming normal vector directions, */
    /* neglecting distortion.                                          */
    scale3(pos2[1], a, f);
    sub3(v, f, f);
    scale3(pos2[0], a, g);
    sub3(h, g, g);
    cross3(f, g, rr);
    magi = 1.0/mag3(rr);
    scale3(magi, rr, rr);

    /* Check and optionally correct for vector directions. */
    sgn = 1;
    cross3(v, h, t);
    if (dot3(t, a) < 0) {
	scale3(-1.0, rr, rr);
	sgn = -1;
	}

    /* Optionally compute partial for non-linear part of the model */
    if (par != NULL) {
	ident33(irrt);
	for (i=0; i<3; i++)
	    for (j=0; j<3; j++)
		irrt[i][j] -= rr[i] * rr[j];
	cross3(f, a, t);
	mult331(irrt, t, w);
	drpdx[0] = par[0][0] = -sgn * w[0] * magi;
	drpdx[1] = par[1][0] = -sgn * w[1] * magi;
	drpdx[2] = par[2][0] = -sgn * w[2] * magi;
	cross3(g, a, t);
	mult331(irrt, t, w);
	drpdy[0] = par[0][1] = sgn * w[0] * magi;
	drpdy[1] = par[1][1] = sgn * w[1] * magi;
	drpdy[2] = par[2][1] = sgn * w[2] * magi;
	}

    /* Remove the radial lens distortion.  Preliminary values of omega,  */
    /* lambda, and tau are computed from the rr vector including         */
    /* distortion, in order to obtain the coefficients of the equation   */
    /* k5*u^5 + k3*u^3 + k1*u = 1, which is solved for u by means of     */
    /* Newton's method.  This value is used to compute the corrected rr. */
    omega = dot3(rr, o);
    omega_2 = omega * omega;
    scale3(omega, o, wo);
    sub3(rr, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    k1 = 1 + r[0];		/*  1 + rho0 */
    k3 = r[1] * tau;		/*  rho1*tau  */
    k5 = r[2] * tau*tau;	/*  rho2*tau^2  */
    mu = r[0] + k3 + k5;
    u = 1.0 - mu;	/* initial approximation for iterations */
    for (i=0; i<MAXITER; i++) {
	u_2 = u*u;
	poly  =  ((k5*u_2  +  k3)*u_2 + k1)*u - 1;
	deriv = (5*k5*u_2 + 3*k3)*u_2 + k1;
	if (deriv <= 0) {
	    printf("cmod_cahvor_2d_to_3d(): Distortion is too negative\n");
	    break;
	    }
	else {
	    du = poly/deriv;
	    u -= du;
	    if (fabs(du) < CONV)
		break;
	    }
	}
    if (i >= MAXITER)
	printf("cmod_cahvor_2d_to_3d(): Too many iterations (%d)\n", i);
    mu = 1 - u;
    scale3(mu, lambda, pp);
    sub3(rr, pp, uvec3);
    magv = mag3(uvec3);
    scale3(1.0/magv, uvec3, uvec3);

    /* Note:  If partial derivatives are to be computed, corrected values */
    /* of omega, lambda, tau, and mu must be computed.                    */

    /* Optionally calculate the partial of uvec3 with respect to pos2 */
    if (par == NULL)
	return;

    /* Note that the approximate partial for non-linear part is above */

    /* If requested, just use the approximations for speed */
    if (approx)
	return;

    /* Recompute omega, lambda, tau, and mu */
    omega = dot3(uvec3, o);
    omega_2 = omega * omega;
    scale3(omega, o, wo);
    sub3(uvec3, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    mu = r[0] + r[1]*tau + r[2]*tau*tau;

    /* Compute the partial derivatives for distortion */

    ident33(dldr);
    mult313(o, o, m33);
    sub33(dldr, m33, dldr);

    /*...
    ident33(dldo);
    scale33(-omega, dldo, dldo);
    mult313(o, uvec3, m33);
    sub33(dldo, m33, dldo);
    ...*/

    dudt = r[1] + (2 * r[2] * tau);

    /*...
    mult133(lambda, dldo, v3);
    scale3(2/omega_2, v3, v3);
    scale3((2 * tau / omega), uvec3, u3);
    sub3(v3, u3, v3);
    mult313(lambda, v3, m33);
    scale33(dudt, m33, m33);
    scale33(mu, dldo, n33);
    add33(m33, n33, drpdo);
    ...*/

    mult133(lambda, dldr, v3);
    scale3(2/omega_2, v3, v3);
    scale3((2 * tau / omega), o, u3);
    sub3(v3, u3, v3);
    mult313(lambda, v3, m33);
    scale33(dudt, m33, m33);
    scale33(mu, dldr, n33);
    add33(m33, n33, drpdr);
    ident33(m33);
    add33(m33, drpdr, drpdr);
    scale33(magv, drpdr, drpdr);

    /* Apply these partials to get the final result */

    inv33(drpdr, drpdri);
    mult331(drpdri, drpdx, drdx);
    mult331(drpdri, drpdy, drdy);

    par[0][0] = drdx[0];
    par[1][0] = drdx[1];
    par[2][0] = drdx[2];
    par[0][1] = drdy[0];
    par[1][1] = drdy[1];
    par[2][1] = drdy[2];
    }


/******************************************************************************
********************************   CMOD_CAHVOR_3D_TO_2D   *********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (double (*)[3])NULL, then it will not be calculated. */

void cmod_cahvor_3d_to_2d(pos3, c, a, h, v, o, r, approx, range, pos2, par)
double pos3[3];		/* input 3D position */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis  O */
double r[3];		/* input model radial-distortion terms R */
bool_t approx;		/* input flag to use fast approximation */
double *range;		/* output range along A (same units as C) */
double pos2[2];		/* output 2D image-plane projection */
double par[2][3];	/* output partial-derivative matrix of pos2 to pos3 */
{
    double alpha, beta, gamma, xh, yh;
    double omega, omega_2, tau, mu;
    double p_c[3], pp[3], pp_c[3], wo[3], lambda[3];
    double dldp[3][3], dppdp[3][3], m33[3][3], n33[3][3];
    double dxhdpp[3], dyhdpp[3], v3[3], u3[3];
    double dudt;

    /* Calculate p' and other necessary quantities */
    sub3(pos3, c, p_c);
    omega = dot3(p_c, o);
    omega_2 = omega * omega;
    scale3(omega, o, wo);
    sub3(p_c, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    mu = r[0] + (r[1] * tau) + (r[2] * tau * tau);
    scale3(mu, lambda, pp);
    add3(pos3, pp, pp);

    /* Calculate alpha, beta, gamma, which are (p' - c) */
    /* dotted with a, h, v, respectively                */
    sub3(pp, c, pp_c);
    alpha  = dot3(pp_c, a);
    beta   = dot3(pp_c, h);
    gamma  = dot3(pp_c, v);

    /* Calculate the projection */
    pos2[0] = xh = beta  / alpha;
    pos2[1] = yh = gamma / alpha;
    *range = alpha;

    /* Only calculate the partial derivatives upon request */
    if (par == NULL)
	return;

    /* Calculate the approximate partial derivatives */

    scale3(xh, a, v3);
    sub3(h, v3, v3);
    scale3(1/alpha, v3, (approx ? par[0] : dxhdpp));

    scale3(yh, a, v3);
    sub3(v, v3, v3);
    scale3(1/alpha, v3, (approx ? par[1] : dyhdpp));

    /* If requested, just use the approximations for speed */
    if (approx)
	return;

    /* Complete the calculations for accuracy */

    ident33(dldp);
    mult313(o, o, m33);
    sub33(dldp, m33, dldp);

    dudt = r[1] + (2 * r[2] * tau);

    mult133(lambda, dldp, v3);
    scale3(2/omega_2, v3, v3);
    scale3((2 * tau / omega), o, u3);
    sub3(v3, u3, v3);
    mult313(lambda, v3, m33);
    scale33(dudt, m33, m33);
    scale33(mu, dldp, n33);
    add33(m33, n33, dppdp);
    ident33(m33);
    add33(m33, dppdp, dppdp);

    mult133(dxhdpp, dppdp, par[0]);
    mult133(dyhdpp, dppdp, par[1]);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_3D_TO_2D_POINT   ***************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane. In addition it calculates the partial-derivative matrix of the 2D
    point with respect to the 3D vector of the input ray. If the parameter for
    the output partial matrix is passed as (double (*)[3])NULL, then it will
    not be calculated. */

void cmod_cahvor_3d_to_2d_point(c, a, h, v, o, r, approx, pos3, uvec3,
		pos2, par)
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis  O */
double r[3];		/* input model radial-distortion terms R */
bool_t approx;		/* input flag to use fast approximation */
double pos3[3];		/* input 3D position of line */
double uvec3[3];	/* input 3D unit vector of line */
double pos2[2];		/* output 2D image-plane projection */
double par[2][3];	/* output derivative matrix of pos2 to uvec3 */
{
    double alpha, beta, gamma;
    double omega, omega_2, tau, mu;
    double pp_c[3], wo[3], lambda[3];

    /* Calculate (p' - c). Note: p' is never computed directly, */
    /* but is understood to be a unit distance from c in the    */
    /* direction of the vanishing point.                        */
    omega = dot3(uvec3, o);
    omega_2 = omega * omega;
    scale3(omega, o, wo);
    sub3(uvec3, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    mu = r[0] + (r[1] * tau) + (r[2] * tau * tau);
    scale3(mu, lambda, pp_c);
    add3(uvec3, pp_c, pp_c);

    /* Calculate alpha, beta, gamma, which are (p' - c) */
    /* dotted with a, h, v, respectively                */
    alpha  = dot3(pp_c, a);
    beta   = dot3(pp_c, h);
    gamma  = dot3(pp_c, v);

    /* Calculate the projection */
    pos2[0] = beta  / alpha;
    pos2[1] = gamma / alpha;

    /* Optionally calculate the partial-derivative matrix */
    if (par != NULL) {
	printf("cmod_cahvor_3d_to_2d_point(): par not yet calculated\n");
	}
    }


/******************************************************************************
********************************   CMOD_CAHVOR_MOVE   *************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahvor_move(p_i, q_i, c_i, a_i, h_i, v_i, o_i, r_i,
		p_f, q_f, c_f, a_f, h_f, v_f, o_f, r_f)
double p_i[3];		/* input initial pos of camera ref pt */
double q_i[4];		/* input initial orientation (quat) of camera ref pt */
double c_i[3];		/* input initial model center vector C */
double a_i[3];		/* input initial model axis   vector A */
double h_i[3];		/* input initial model horiz. vector H */
double v_i[3];		/* input initial model vert.  vector V */
double o_i[3];		/* input initial model optical axis unit vector O */
double r_i[3];		/* input initial model radial-distortion terms  R */
double p_f[3];		/* input final pos of camera ref pt */
double q_f[4];		/* input final orientation (quat) of camera ref pt */
double c_f[3];		/* output final model center vector C */
double a_f[3];		/* output final model axis   vector A */
double h_f[3];		/* output final model horiz. vector H */
double v_f[3];		/* output final model vert.  vector V */
double o_f[3];		/* output final model optical axis unit vector O */
double r_f[3];		/* output final model radial-distortion terms  R */
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

    /* Copy over the R "vector" unchanged */
    copy3(r_i, r_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_READ   *************************
*******************************************************************************

    This function reads a CAHVOR model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

/******************************************************************************
********************************   CMOD_CAHVOR_READ2   ************************
*******************************************************************************

    This function reads a CAHVOR model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

int cmod_cahvor_read2(filename, xdim, ydim,
			c, a, h, v, o, r, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
int *xdim;		/* output number of columns */
int *ydim;		/* output number of rows */
double c[3];		/* output model center vector C */
double a[3];		/* output model axis   vector A */
double h[3];		/* output model horiz. vector H */
double v[3];		/* output model vert.  vector V */
double o[3];		/* output model optical axis unit vector O */
double r[3];		/* output model radial-distortion terms  R */
double s[18][18];	/* output covariance of CAHVOR */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
double s_int[5][5];	/* output covariance matrix */
{
    int i;
    FILE *fp;

    /* Open the CAHVOR file */
    if ((fp = fopen(filename, "r")) == NULL) {
	fprintf(stderr, "Error opening CAHVOR file: %s\n", filename);
	return FAILURE;
	}

    /* Optionally look for image dimensions */
    if ((xdim != NULL) && (ydim != NULL)) {
	*xdim = -1;
	*ydim = -1;
	if (cmod_read_scanstr(fp, "Dimensions =") == FAILURE)
	    rewind(fp);
	else
	    scan_(fp, 2, (fp, " %d %d ", xdim, ydim));
	}

    /* Read C, A, H, V, O, R vectors */
    scanstr_(fp, "C =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n",&c[0], &c[1], &c[2]));
    scanstr_(fp, "A =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n",&a[0], &a[1], &a[2]));
    scanstr_(fp, "H =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n",&h[0], &h[1], &h[2]));
    scanstr_(fp, "V =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n",&v[0], &v[1], &v[2]));
    scanstr_(fp, "O =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n",&o[0], &o[1], &o[2]));
    scanstr_(fp, "R =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n",&r[0], &r[1], &r[2]));

    /* Read covariance matrix for C, A, H, V, O, R */
    scanstr_(fp, "S =");
    for (i=0; i<18; i++) {
	scan_(fp, 18, (fp, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf\
			%lf %lf %lf %lf %lf %lf %lf\n",
	    &s[i][ 0], &s[i][ 1], &s[i][ 2],
	    &s[i][ 3], &s[i][ 4], &s[i][ 5],
	    &s[i][ 6], &s[i][ 7], &s[i][ 8],
	    &s[i][ 9], &s[i][10], &s[i][11],
	    &s[i][12], &s[i][13], &s[i][14],
	    &s[i][15], &s[i][16], &s[i][17]
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

    /* Close the CAHVOR file */
    fclose(fp);

    return SUCCESS;
    }


int cmod_cahvor_read(filename, c, a, h, v, o, r, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
double c[3];		/* output model center vector C */
double a[3];		/* output model axis   vector A */
double h[3];		/* output model horiz. vector H */
double v[3];		/* output model vert.  vector V */
double o[3];		/* output model optical axis unit vector O */
double r[3];		/* output model radial-distortion terms  R */
double s[18][18];	/* output covariance of CAHVOR */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
double s_int[5][5];	/* output covariance matrix */
{
    return cmod_cahvor_read2(filename, (int *)NULL, (int *)NULL,
			c, a, h, v, o, r, s, hs, hc, vs, vc, theta, s_int);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_REFLECT   **********************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahvor_reflect(double c_i[3], double a_i[3], double h_i[3], double v_i[3], double o_i[3], 
			 double r_i[3], double p[3], double n[3],
			 double c_f[3], double a_f[3], double h_f[3], double v_f[3], 
			 double o_f[3], double r_f[3] , bool_t* parallel, bool_t* behind)
{
    double k, d, nu[3], u[3];
    double c[3], a[3], h[3], v[3], o[3];

    /* Check if the camera view and plane are parallel (or nearly so) */
    unit3(n, nu);
    k = dot3(a_i, nu);
    if (fabs(k) < EPSILON) {
	*parallel = TRUE;
	*behind   = FALSE;
	return;
	}
    *parallel = FALSE;

    /* Compute the reflected A vector */
    scale3(-2*k, nu, a);
    add3(a_i, a, a);

    /* Compute the reflected H vector */
    scale3(-2*dot3(h_i, nu), nu, h);
    add3(h_i, h, h);

    /* Compute the reflected V vector */
    scale3(-2*dot3(v_i, nu), nu, v);
    add3(v_i, v, v);

    /* Compute the reflected O vector */
    scale3(-2*dot3(o_i, nu), nu, o);
    add3(o_i, o, o);

    /* Calculate where A's extension intersects the plane */
    d = (dot3(p, nu) - dot3(c_i, nu)) / k;
    if (d < 0) {
	*behind = TRUE;
	return;
	}
    *behind = FALSE;
    scale3(d, a_i, u);
    add3(c_i, u, c);

    /* Compute the reflected model's position */
    scale3(-d, a, u);
    add3(u, c, c);

    /* Copy over the results */
    copy3(c,   c_f);
    copy3(a,   a_f);
    copy3(h,   h_f);
    copy3(v,   v_f);
    copy3(o,   o_f);
    copy3(r_i, r_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_REFLECT_COV   ******************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahvor_reflect_cov(s_i, n, s_f)
double s_i[18][18];	/* input initial covariance */
double n[3];		/* input normal to the reflecting plane */
double s_f[18][18];	/* output final covariance */
{
    double nu0, nu1, nu2, nu[3], r[3][3];
    void cmod_cahvor_transform_cov();

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
    cmod_cahvor_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_ROT_COV   **********************
*******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rot_cov(r_i, s_i, r_f, s_f)
double r_i[3][3];	/* input initial orientation (rot) of camera ref pt */
double s_i[18][18];	/* input initial covariance */
double r_f[3][3];	/* input final orientation (rot) of camera ref pt */
double s_f[18][18];	/* output final covariance */
{
    double r_it[3][3], r[3][3];
    void cmod_cahvor_transform_cov();

    /* Calculate the rotation, R, from the initial to the final orientation */
    trans33(r_i, r_it);
    mult333(r_f, r_it, r);
    cmod_cahvor_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_ROTATE_COV   *******************
*******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rotate_cov(q_i, s_i, q_f, s_f)
double q_i[4];		/* input initial orientation (quat) of camera ref pt */
double s_i[18][18];	/* input initial covariance */
double q_f[4];		/* input final orientation (quat) of camera ref pt */
double s_f[18][18];	/* output final covariance */
{
    double r_f[3][3], r_i[3][3];

    rotq(q_f, r_f);
    rotq(q_i, r_i);
    cmod_cahvor_rot_cov(r_i, s_i, r_f, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_SCALE   ************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvor_scale(hscale, vscale, h1, v1, s1, h2, v2, s2)
double hscale;		/* input horizontal scale factor */
double vscale;		/* input vertical   scale factor */
double h1[3];		/* input  model horiz. vector H */
double v1[3];		/* input  model vert.  vector V */
double s1[18][18];	/* input  covariance matrix, or NULL */
double h2[3];		/* output model horiz. vector H */
double v2[3];		/* output model vert.  vector V */
double s2[18][18];	/* output covariance matrix, or NULL */
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
	for (i=0; i<18; i++)
	    for (j=0; j<18; j++)
		s2[i][j] = s1[i][j];
	}
    for (i=0; i<18; i++) {
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
********************************   CMOD_CAHVOR_SHIFT   ************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvor_shift(dx, dy, a1, h1, v1, h2, v2)
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
********************************   CMOD_CAHVOR_TRANSFORM_COV   ****************
*******************************************************************************

    This function transform a CAHVOR model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvor_transform_cov(s_i, r, s_f)
double s_i[18][18];	/* input initial covariance */
double r[3][3];		/* input transformation matrix of camera ref pt */
double s_f[18][18];	/* output final covariance */
{
    int i, j, k;
    double d;
    static double r18[18][18], r18t[18][18], stemp[18][18];

    /* Contruct a matrix, R18, (and its transpose) to rotate the covariance
	|R     |
	| R    |
	|  R   |
	|   R  |
	|    R |
	|     I|
    */
    for (i=0; i<18; i++)
	for (j=0; j<18; j++)
	    r18[i][j] = 0;
    for (i=0; i<3; i++) {
	for (j=0; j<3; j++) {
	    r18[i+ 0][j+ 0] = r[i][j];
	    r18[i+ 3][j+ 3] = r[i][j];
	    r18[i+ 6][j+ 6] = r[i][j];
	    r18[i+ 9][j+ 9] = r[i][j];
	    r18[i+12][j+12] = r[i][j];
	    }
	}
    r18[15][15] = 1;
    r18[16][16] = 1;
    r18[17][17] = 1;
    for (i=0; i<18; i++)
	for (j=0; j<18; j++)
	    r18t[i][j] = r18[j][i];

    /* Pre-multiply by the matrix */
    for (i=0; i<18; i++) {
	for (j=0; j<18; j++) {
	    d = 0;
	    for (k=0; k<18; k++)
		d += r18[i][k] * s_i[k][j];
	    stemp[i][j] = d;
	    }
	}

    /* Post-multiply by the transpose */
    for (i=0; i<18; i++) {
	for (j=0; j<18; j++) {
	    d = 0;
	    for (k=0; k<18; k++)
		d += stemp[i][k] * r18t[k][j];
	    s_f[i][j] = d;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_FROM_CAHV   ***************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    CAHVOR. */

void cmod_cahvor_warp_from_cahv(c1, a1, h1, v1, pos1, approx,
				c2, a2, h2, v2, o2, r2, pos2)
double c1[3];		/* input initial model center position vector   C */
double a1[3];		/* input initial model orthog. axis unit vector A */
double h1[3];		/* input initial model horizontal vector        H */
double v1[3];		/* input initial model vertical vector          V */
double pos1[2];		/* input 2D position from CAHV */
bool_t approx;		/* input flag to use fast approximation */
double c2[3];		/* input final model center position vector   C */
double a2[3];		/* input final model orthog. axis unit vector A */
double h2[3];		/* input final model horizontal vector        H */
double v2[3];		/* input final model vertical vector          V */
double o2[3];		/* input final model optical axis unit vector O */
double r2[3];		/* input final model radial-distortion terms  R */
double pos2[2];		/* output 2D position for CAHVOR */
{
    double range;
    double p3[3], u3[3];

    cmod_cahv_2d_to_3d(pos1, c1, a1, h1, v1, p3, u3, (double (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahvor_3d_to_2d(p3, c2, a2, h2, v2, o2, r2, approx, &range, pos2,
	(double (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODEL   *******************
*******************************************************************************

    This function warps a camera model so that it is purely linear. The
    parameters C and A will not change. The parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvor_warp_model(c, a, h, v, o, r, minfov, idims, odims,
			a2, h2, v2, hs, hc, vs, vc, theta)
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model axis   vector O */
double r[3];		/* input model dist.  terms  R */
int minfov;		/* input if to minimize to common field of view */
int idims[2];		/* input image dimensions of input  model */
int odims[2];		/* input image dimensions of output model */
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
    double rt[3], dn[3], p3[3], u3[3], vec1[3], vec2[3];
    double sn, x, hmin, hmax, vmin, vmax, pts[12][2], hpts[6][2], vpts[6][2];

    /* Record the landmark 2D coordinates around the perimeter of the image */
    pts[ 0][0] = hpts[0][0] = 0;
    pts[ 0][1] = hpts[0][1] = 0;
    pts[ 1][0] = hpts[1][0] = 0;
    pts[ 1][1] = hpts[1][1] = (idims[1]-1)/2.0;
    pts[ 2][0] = hpts[2][0] = 0;
    pts[ 2][1] = hpts[2][1] = idims[1]-1;
    pts[ 3][0] = hpts[3][0] = idims[0]-1;
    pts[ 3][1] = hpts[3][1] = 0;
    pts[ 4][0] = hpts[4][0] = idims[0]-1;
    pts[ 4][1] = hpts[4][1] = (idims[1]-1)/2.0;
    pts[ 5][0] = hpts[5][0] = idims[0]-1;
    pts[ 5][1] = hpts[5][1] = idims[1]-1;
    pts[ 6][0] = vpts[0][0] = 0;
    pts[ 6][1] = vpts[0][1] = 0;
    pts[ 7][0] = vpts[1][0] = (idims[0]-1)/2.0;
    pts[ 7][1] = vpts[1][1] = 0;
    pts[ 8][0] = vpts[2][0] = idims[0]-1;
    pts[ 8][1] = vpts[2][1] = 0;
    pts[ 9][0] = vpts[3][0] = 0;
    pts[ 9][1] = vpts[3][1] = idims[1]-1;
    pts[10][0] = vpts[4][0] = (idims[0]-1)/2.0;
    pts[10][1] = vpts[4][1] = idims[1]-1;
    pts[11][0] = vpts[5][0] = idims[0]-1;
    pts[11][1] = vpts[5][1] = idims[1]-1;

    /* Choose a camera axis in the middle of the perimeter */
    zero3(a2);
    for (i=0; i<12; i++) {
	cmod_cahvor_2d_to_3d(pts[i], c, a, h, v, o, r, FALSE,
				p3, u3, (double (*)[2])NULL);
	add3(u3, a2, a2);
	}
    unit3(a2, a2);

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
	cmod_cahvor_2d_to_3d(hpts[i], c, a, h, v, o, r, FALSE,
				p3, u3, (double (*)[2])NULL);
	x = dot3(dn, u3);
	scale3(x, dn, vec1);
	sub3(u3, vec1, vec2);
	unit3(vec2, vec2);
	sn = mag3(cross3(a2, vec2, vec1));
	if (hmin > sn)
	    hmin = sn;
	if (hmax < sn)
	    hmax = sn;
	}
    vmin =  1;
    vmax = -1;
    for (i=0; i<6; i++) {
	cmod_cahvor_2d_to_3d(vpts[i], c, a, h, v, o, r, FALSE,
				p3, u3, (double (*)[2])NULL);
	x = dot3(rt, u3);
	scale3(x, rt, vec1);
	sub3(u3, vec1, vec2);
	unit3(vec2, vec2);
	sn = mag3(cross3(a2, vec2, vec1));
	if (vmin > sn)
	    vmin = sn;
	if (vmax < sn)
	    vmax = sn;
	}

    /* Compute the all-encompassing scale factors */
    sn = (minfov ? hmin : hmax);
    x = odims[0] / 2.0;
    *hs = sqrt((x*x)/(sn*sn) - x*x);
    sn = (minfov ? vmin : vmax);
    x = odims[1] / 2.0;
    *vs = sqrt((x*x)/(sn*sn) - x*x);

    /* Assign idealized image centers and coordinate angles */
    *hc = (odims[0] - 1) / 2.0;
    *vc = (odims[1] - 1) / 2.0;
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
********************************   CMOD_CAHVOR_WARP_MODELS   ******************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models will have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Since the output models will be linear, the parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvor_warp_models(c1, a1, h1, v1, o1, r1, c2, a2, h2, v2, o2, r2,
			minfov, idims, odims, a, h, v, hs, hc, vs, vc, theta)
double c1[3];		/* input model 1 center vector C */
double a1[3];		/* input model 1 axis   vector A */
double h1[3];		/* input model 1 horiz. vector H */
double v1[3];		/* input model 1 vert.  vector V */
double o1[3];		/* input model 1 axis   vector O */
double r1[3];		/* input model 1 dist.  terms  R */
double c2[3];		/* input model 2 center vector C */
double a2[3];		/* input model 2 axis   vector A */
double h2[3];		/* input model 2 horiz. vector H */
double v2[3];		/* input model 2 vert.  vector V */
double o2[3];		/* input model 2 axis   vector O */
double r2[3];		/* input model 2 dist.  terms  R */
int minfov;		/* input if to minimize to common field of view */
int idims[2];		/* input image dimensions of input  models */
int odims[2];		/* input image dimensions of output models */
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
    double rt[3], dn[3], p3[3], u3[3], vec1[3], vec2[3], p2[2];
    double sn, x, hmin, hmax, vmin, vmax, hpts[2][2], vpts[2][2];

    /* Record the landmark 2D coordinates */
    hpts[0][0] = 0;
    hpts[0][1] = (idims[1]-1)/2.0;
    hpts[1][0] = idims[0]-1;
    hpts[1][1] = (idims[1]-1)/2.0;
    vpts[0][0] = (idims[0]-1)/2.0;
    vpts[0][1] = 0;
    vpts[1][0] = (idims[0]-1)/2.0;
    vpts[1][1] = idims[1]-1;

    /* Compute pointing vector as the mean of the two input A vectors */
    /*...
    add3(a1, a2, a);	/+ average pointing vector +/
    unit3(a, a);
    ...*/

    /* Compute pointing vector as the mean of projections of the landmarks */
    /*...
    zero3(a);
    for (i=0; i<4; i++) {
	cmod_cahvor_2d_to_3d(pts[i], c1, a1, h1, v1, o1, r1, FALSE,
				p3, u3, (double (*)[2])NULL);
	add3(u3, a, a);
	cmod_cahvor_2d_to_3d(pts[i], c2, a2, h2, v2, o2, r2, FALSE,
				p3, u3, (double (*)[2])NULL);
	add3(u3, a, a);
	}
    unit3(a, a);
    ...*/

    /* Compute pointing vector as the mean of central projections */
    p2[0] = idims[0] / 2.0;
    p2[1] = idims[1] / 2.0;
    cmod_cahvor_2d_to_3d(p2, c1, a1, h1, v1, o1, r1, FALSE,
			    p3, a,  (double (*)[2])NULL);
    cmod_cahvor_2d_to_3d(p2, c2, a2, h2, v2, o2, r2, FALSE,
			    p3, u3, (double (*)[2])NULL);
    add3(u3, a, a);
    unit3(a, a);

    /* NOTE: The A vector computed above is not really right. It is being  */
    /* computed as a value close to the camera-pointing directions (with   */
    /* the commented out code being an attempt to use the original A's,    */
    /* which only works for models whose horizontal and vertical centers   */
    /* are near the geometrical center of the image, or as the means of    */
    /* boundary pointings, which is a clumsier way to get at what is being */
    /* done now, but which must have run into problems that I no longer    */
    /* remember). It really should be orthogonal to the baseline between   */
    /* the cameras (as is being done for the CAHVORE models), but that has */
    /* problems, too. The current method is an approximation that is fine  */
    /* as long as items of interest in the image are far from the camera.  */
    /* As they get close, it would be better to make A perpendicular to    */
    /* the camera baseline, and then adjust the image centers below so     */
    /* that the geometrical center of the images projects out in 3D in the */
    /* desired camera pointing direction.                                  */

    /* Compute right and down vectors */
    sub3(c2, c1, rt);	/* initial right vector */
    if (dot3(rt, h1) < 0)
	scale3(-1.0, rt, rt);
    cross3(a, rt, dn);	/* down vector */
    cross3(dn, a, rt);	/* corrected right to be orthogonal to pointing */
    unit3(dn, dn);
    unit3(rt, rt);

    /* Find largest horizontal and vertical fields of view */
    hmin =  1;
    hmax = -1;
    for (i=0; i<2; i++) {
	cmod_cahvor_2d_to_3d(hpts[i], c1, a1, h1, v1, o1, r1, FALSE,
				p3, u3, (double (*)[2])NULL);
	sn = mag3(cross3(a, u3, vec1));
	if (hmin > sn)
	    hmin = sn;
	if (hmax < sn)
	    hmax = sn;
	cmod_cahvor_2d_to_3d(hpts[i], c2, a2, h2, v2, o2, r2, FALSE,
				p3, u3, (double (*)[2])NULL);
	sn = mag3(cross3(a, u3, vec1));
	if (hmin > sn)
	    hmin = sn;
	if (hmax < sn)
	    hmax = sn;
	}
    vmin =  1;
    vmax = -1;
    for (i=0; i<2; i++) {
	cmod_cahvor_2d_to_3d(vpts[i], c1, a1, h1, v1, o1, r1, FALSE,
				p3, u3, (double (*)[2])NULL);
	sn = mag3(cross3(a, u3, vec1));
	if (vmin > sn)
	    vmin = sn;
	if (vmax < sn)
	    vmax = sn;
	cmod_cahvor_2d_to_3d(vpts[i], c2, a2, h2, v2, o2, r2, FALSE,
				p3, u3, (double (*)[2])NULL);
	sn = mag3(cross3(a, u3, vec1));
	if (vmin > sn)
	    vmin = sn;
	if (vmax < sn)
	    vmax = sn;
	}

    /* Compute the all-encompassing scale factors */
    sn = (minfov ? hmin : hmax);
    x = odims[0] / 2.0;
    *hs = sqrt((x*x)/(sn*sn) - x*x);
    sn = (minfov ? vmin : vmax);
    x = odims[1] / 2.0;
    *vs = sqrt((x*x)/(sn*sn) - x*x);

    /* Assign idealized image centers and coordinate angles */
    *hc = (odims[0] - 1) / 2.0;
    *vc = (odims[1] - 1) / 2.0;
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
********************************   CMOD_CAHVOR_WARP_MODELS_NODIMS   ***********
*******************************************************************************

    This function is the same as cmod_cahvor_warp_models_dims(), but for cases
    where the image dimensions are not known precisely. This function fills
    in those dimensions with approximate values generated from the horizontal
    and vertical centers of the input models themselves. */

void cmod_cahvor_warp_models_nodims(
			c1, a1, h1, v1, o1, r1, c2, a2, h2, v2, o2, r2,
			minfov, a, h, v, hs, hc, vs, vc, theta)
double c1[3];		/* input model 1 center vector C */
double a1[3];		/* input model 1 axis   vector A */
double h1[3];		/* input model 1 horiz. vector H */
double v1[3];		/* input model 1 vert.  vector V */
double o1[3];		/* input model 1 axis   vector O */
double r1[3];		/* input model 1 dist.  terms  R */
double c2[3];		/* input model 2 center vector C */
double a2[3];		/* input model 2 axis   vector A */
double h2[3];		/* input model 2 horiz. vector H */
double v2[3];		/* input model 2 vert.  vector V */
double o2[3];		/* input model 2 axis   vector O */
double r2[3];		/* input model 2 dist.  terms  R */
int minfov;		/* input if to minimize to common field of view */
double a[3];		/* output virtual model axis   vector A */
double h[3];		/* output virtual model horiz. vector H */
double v[3];		/* output virtual model vert.  vector V */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
{
    int dims[2];
    double hs1, hc1, vs1, vc1;
    double hs2, hc2, vs2, vc2;
    static double s12[12][12], s5[5][5];

    /* Compute a reasonable set of dimensions from the model centers */
    cmod_cahv_internal(c1, a1, h1, v1, s12, &hs1, &hc1, &vs1, &vc1, theta, s5);
    cmod_cahv_internal(c2, a2, h2, v2, s12, &hs2, &hc2, &vs2, &vc2, theta, s5);
    dims[0] = hc1 + hc2 + 1;
    dims[1] = vc1 + vc2 + 1;

    /* Call the more general function with these values */
    cmod_cahvor_warp_models(c1, a1, h1, v1, o1, r1, c2, a2, h2, v2, o2, r2,
				minfov, dims, dims,
				a, h, v, hs, hc, vs, vc, theta);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_TO_CAHV   *****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVOR and warps it into an image coordinate modeled by
    CAHV. */

void cmod_cahvor_warp_to_cahv(c1, a1, h1, v1, o1, r1, pos1, approx,
				c2, a2, h2, v2, pos2)
double c1[3];		/* input initial model center position vector   C */
double a1[3];		/* input initial model orthog. axis unit vector A */
double h1[3];		/* input initial model horizontal vector        H */
double v1[3];		/* input initial model vertical vector          V */
double o1[3];		/* input initial model optical axis unit vector O */
double r1[3];		/* input initial model radial-distortion terms  R */
double pos1[2];		/* input 2D position from CAHVOR */
bool_t approx;		/* input flag to use fast approximation */
double c2[3];		/* input final model center position vector   C */
double a2[3];		/* input final model orthog. axis unit vector A */
double h2[3];		/* input final model horizontal vector        H */
double v2[3];		/* input final model vertical vector          V */
double pos2[2];		/* output 2D position for CAHV */
{
    double range;
    double p3[3], u3[3];

    cmod_cahvor_2d_to_3d(pos1, c1, a1, h1, v1, o1, r1, approx, p3, u3,
	(double (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahv_3d_to_2d(p3, c2, a2, h2, v2, &range, pos2, (double (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_TO_CAHVOR   ***************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVOR and warps it into an image coordinate modeled by
    a different CAHVOR. */

void cmod_cahvor_warp_to_cahvor(c1, a1, h1, v1, o1, r1, pos1, approx,
				c2, a2, h2, v2, o2, r2, pos2)
double c1[3];		/* input initial model center position vector   C */
double a1[3];		/* input initial model orthog. axis unit vector A */
double h1[3];		/* input initial model horizontal vector        H */
double v1[3];		/* input initial model vertical vector          V */
double o1[3];		/* input initial model optical axis unit vector O */
double r1[3];		/* input initial model radial-distortion terms  R */
double pos1[2];		/* input 2D position from CAHVOR */
bool_t approx;		/* input flag to use fast approximation */
double c2[3];		/* input final model center position vector   C */
double a2[3];		/* input final model orthog. axis unit vector A */
double h2[3];		/* input final model horizontal vector        H */
double v2[3];		/* input final model vertical vector          V */
double o2[3];		/* input final model optical axis unit vector O */
double r2[3];		/* input final model radial-distortion terms  R */
double pos2[2];		/* output 2D position for CAHV */
{
    double range;
    double p3[3], u3[3];

    cmod_cahvor_2d_to_3d(pos1, c1, a1, h1, v1, o1, r1, approx, p3, u3,
	(double (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahvor_3d_to_2d(p3, c2, a2, h2, v2, o2, r2, approx, &range, pos2,
	(double (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WRITE2   ***********************
*******************************************************************************

    This function writes a CAHVOR model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahvor_write2(filename, comment, xdim, ydim,
			c, a, h, v, o, r, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
char *comment;		/* input one-line comment to record in file */
int xdim;		/* input number of columns */
int ydim;		/* input number of rows */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis unit vector O */
double r[3];		/* input model radial-distortion terms  R */
double s[18][18];	/* input covariance of CAHVOR */
double hs;		/* input horizontal scale factor */
double hc;		/* input horizontal center */
double vs;		/* input vertical scale factor */
double vc;		/* input vertical center */
double theta;		/* input angle between axes */
double s_int[5][5];	/* input covariance matrix */
{
    int i, j;
    FILE *fp;

    /* Open the CAHVOR file */
    if ((fp = fopen(filename, "w")) == NULL) {
	fprintf(stderr, "Error creating CAHVOR file: %s\n", filename);
	return FAILURE;
	}

    /* Write out the comment */
    fprintf(fp, "# %s\n", comment);

    /* Write out model type */
    fprintf(fp, "\n");
    fprintf(fp, "Model = CAHVOR = perspective, distortion\n");

    /* Write out image dimensions */
    if ((xdim >= 0) && (ydim >= 0)) {
	fprintf(fp, "\n");
	fprintf(fp, "Dimensions = %d %d\n", xdim, ydim);
	}

    /* Write C, A, H, V vectors */
    fprintf(fp, "\n");
    fprintf(fp, "C = %13f %13f %13f\n", c[0], c[1], c[2]);
    fprintf(fp, "A = %13f %13f %13f\n", a[0], a[1], a[2]);
    fprintf(fp, "H = %13f %13f %13f\n", h[0], h[1], h[2]);
    fprintf(fp, "V = %13f %13f %13f\n", v[0], v[1], v[2]);
    fprintf(fp, "O = %13f %13f %13f\n", o[0], o[1], o[2]);
    fprintf(fp, "R = %13f %13f %13f\n", r[0], r[1], r[2]);

    /* Write covariance matrix for C, A, H, V */
    fprintf(fp, "\n");
    fprintf(fp, "S =\n");
    for (i=0; i<18; i++) {
	for (j=0; j<18; j++)
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
********************************   CMOD_CAHVOR_WRITE   ************************
*******************************************************************************

    This function writes a CAHVOR model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahvor_write(filename, comment,
			c, a, h, v, o, r, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
char *comment;		/* input one-line comment to record in file */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double o[3];		/* input model optical axis unit vector O */
double r[3];		/* input model radial-distortion terms  R */
double s[18][18];	/* input covariance of CAHVOR */
double hs;		/* input horizontal scale factor */
double hc;		/* input horizontal center */
double vs;		/* input vertical scale factor */
double vc;		/* input vertical center */
double theta;		/* input angle between axes */
double s_int[5][5];	/* input covariance matrix */
{
    return cmod_cahvor_write2(filename, comment, -1, -1,
			c, a, h, v, o, r, s, hs, hc, vs, vc, theta, s_int);
    }


/******************************************************************************
********************************   CMOD_READ_SCANSTR   ************************
*******************************************************************************

    This function scans the input for the given string. It return SUCCESS or
    FAILURE. */

static int cmod_read_scanstr(FILE* fp, char *str)
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
