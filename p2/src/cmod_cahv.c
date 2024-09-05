/******************************************************************************
*                                                                             *
*                                    C A H V                                  *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  3 Aug 1993                  *
*                                       Updated:  7 Mar 2002                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998, 1999, 2000, *
*                                                     2002                    *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for using the Yakimovsky & Cunningham
	camera model, known locally as CAHV. */


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

static int cmod_read_scanstr(FILE *fp, char *str);

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
********************************   CMOD_CAHV_2D_TO_3D   ***********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs and the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point. If the parameter
    for the output partial matrix is passed as (double (*)[2])NULL, then it
    will not be calculated. */

void cmod_cahv_2d_to_3d(pos2, c, a, h, v, pos3, uvec3, par)
double pos2[2];		/* input 2D position */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double pos3[3];		/* output 3D origin of projection */
double uvec3[3];	/* output unit vector ray of projection */
double par[3][2];	/* output partial-derivative matrix of uvec3 to pos2 */
{
    int i, j;
    double f[3], g[3], sgn, t[3], u[3], irrt[3][3], magi;

    /* The projection point is merely the C of the camera model */
    copy3(c, pos3);

    /* Calculate the projection ray assuming normal vector directions */
    scale3(pos2[1], a, f);
    sub3(v, f, f);
    scale3(pos2[0], a, g);
    sub3(h, g, g);
    cross3(f, g, uvec3);
    magi = 1.0/mag3(uvec3);
    scale3(magi, uvec3, uvec3);

    /* Check and optionally correct for vector directions */
    sgn = 1;
    cross3(v, h, t);
    if (dot3(t, a) < 0) {
	scale3(-1.0, uvec3, uvec3);
	sgn = -1;
	}

    /* Optionally calculate the partial of uvec3 with respect to pos2 */
    if (par == NULL)
	return;
    ident33(irrt);
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    irrt[i][j] -= uvec3[i] * uvec3[j];
    cross3(f, a, t);
    mult331(irrt, t, u);
    par[0][0] = -sgn * u[0] * magi;
    par[1][0] = -sgn * u[1] * magi;
    par[2][0] = -sgn * u[2] * magi;
    cross3(g, a, t);
    mult331(irrt, t, u);
    par[0][1] = sgn * u[0] * magi;
    par[1][1] = sgn * u[1] * magi;
    par[2][1] = sgn * u[2] * magi;
    }


/******************************************************************************
********************************   CMOD_CAHV_3D_TO_2D   ***********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (double (*)[3])NULL, then it will not be calculated. */

void cmod_cahv_3d_to_2d(double pos3[3], double c[3], double a[3], double h[3], double v[3], double* range, 
			double pos2[2], double par[2][3])
{
    double d[3];
    double r_1;

    /* Calculate the projection */
    sub3(pos3, c, d);
    *range = dot3(d, a);
    r_1 = 1.0 / *range;
    pos2[0] = dot3(d, h) * r_1;
    pos2[1] = dot3(d, v) * r_1;

    /* Optionally calculate the partial of pos2 with respect to pos3 */
    if (par != NULL) {
	scale3(pos2[0], a, par[0]);
	sub3(h, par[0], par[0]);
	scale3(r_1, par[0], par[0]);
	scale3(pos2[1], a, par[1]);
	sub3(v, par[1], par[1]);
	scale3(r_1, par[1], par[1]);
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_3D_TO_2D_RAY   *******************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane; the 2D back projection of the line from the vanishing point is
    calculated as well. In addition it calculates the partial-derivative
    matrix of the 2D point and vector with respect to the 3D vector of the
    input ray. If the parameter for the output partial matrix is passed as
    (double (*)[3])NULL, then it will not be calculated. If the output unit
    vector is (double *)NULL, then it will not be calculated. Note that there
    is a degenerate case where the 2D output vector is undefined; it will be
    set to (0,0) in this case. */

void cmod_cahv_3d_to_2d_ray(c, a, h, v, pos3, uvec3, pos2, uvec2, par)
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double pos3[3];		/* input 3D position of line */
double uvec3[3];	/* input 3D unit vector of line */
double pos2[2];		/* output 2D image-plane projection */
double uvec2[2];	/* output 2D unit vector of back-projected line */
double par[4][3];	/* output derivative matrix of pos2,uvec2 to uvec3 */
{
    double x;
    double d[3], uh[3], uv[3];

    /* Calculate the projection of the vanishing point */
    x = dot3(uvec3, a);
    pos2[0] = dot3(uvec3, h) / x;
    pos2[1] = dot3(uvec3, v) / x;

    /* Calculate the back-projected ray in 3D */
    if (uvec2 == NULL)
	return;
    sub3(c, pos3, d);
    cross3(uvec3, d, d);
    cross3(a, d, d);
    if ((d[0] != 0) || (d[1] != 0) || (d[2] != 0))
	unit3(d, d);

    /* Calculate horizontal & vertical unit vectors in image plane */
    cross3(h, a, uh);
    cross3(a, uh, uh);
    unit3(uh, uh);
    cross3(v, a, uv);
    cross3(a, uv, uv);
    unit3(uv, uv);

    /* Use those unit vectors to convert back-projected ray to 2D */
    uvec2[0] = dot3(d, uh);
    uvec2[1] = dot3(d, uv);

    /* Optionally calculate the partial-derivative matrix */
    if (par != NULL) {
	printf("cmod_cahv_3d_to_2d_ray(): par not yet calculated\n");
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_INTERNAL   ***********************
*******************************************************************************

    This function calculates the internal camera model parameters for the
    CAHV camera model, deriving them from C, A, H, V, and their covariance
    matrix S. If either the input or output covariance matrices is NULL,
    then the covariance computation will be skipped. */

void cmod_cahv_internal(double c[3], double a[3], double h[3], double v[3], double s[12][12], 
			double* hs, double* hc, double* vs, double* vc, double* theta, double s_int[5][5])
{
    double jacobian[5][12], m_5_12[5][12];
    double jacobian_t[12][5];
    int i, j, k;
    double v_h_a, sin2th, sinth_costh;
    double cross[3], cross1[3], cross2[3];

    /* Calculate hs, hc, vs, vc, theta */
    *hs = mag3(cross3(a, h, cross));
    *hc = dot3(a, h);
    *vs = mag3(cross3(a, v, cross));
    *vc = dot3(a, v);
    *theta = atan2(
	dot3(cross3(v, h, cross), a),
	dot3(cross3(a, v, cross1), cross3(a, h, cross2))
	);

    /* Calculate the Jacobian of those 5 quantities */
    if ((s == NULL) || (s_int == NULL))
	return;
    for (i=0; i<5; i++)
	for (j=0; j<12; j++)
	    jacobian[i][j] = 0;
    jacobian[0][ 3] = - *hc * h[0] / *hs;
    jacobian[0][ 4] = - *hc * h[1] / *hs;
    jacobian[0][ 5] = - *hc * h[2] / *hs;
    jacobian[0][ 6] = (h[0] - (*hc * a[0])) /
				*hs;
    jacobian[0][ 7] = (h[1] - (*hc * a[1])) /
				*hs;
    jacobian[0][ 8] = (h[2] - (*hc * a[2])) /
				*hs;
    jacobian[1][ 3] = h[0];
    jacobian[1][ 4] = h[1];
    jacobian[1][ 5] = h[2];
    jacobian[1][ 6] = a[0];
    jacobian[1][ 7] = a[1];
    jacobian[1][ 8] = a[2];
    jacobian[2][ 3] = - *vc * v[0] / *vs;
    jacobian[2][ 4] = - *vc * v[1] / *vs;
    jacobian[2][ 5] = - *vc * v[2] / *vs;
    jacobian[2][ 9] = (v[0] - (*vc * a[0])) /
				*vs;
    jacobian[2][10] = (v[1] - (*vc * a[1])) /
				*vs;
    jacobian[2][11] = (v[2] - (*vc * a[2])) /
				*vs;
    jacobian[3][ 3] = v[0];
    jacobian[3][ 4] = v[1];
    jacobian[3][ 5] = v[2];
    jacobian[3][ 9] = a[0];
    jacobian[3][10] = a[1];
    jacobian[3][11] = a[2];
    v_h_a = dot3(cross3(v, h, cross), a);
    sin2th = sin(*theta);
    sin2th *= sin2th;
    sinth_costh = sin(*theta) * cos(*theta);
    cross3(v, h, cross);
    jacobian[4][ 3] = (cross[0] * sinth_costh
	+ (*hc * v[0] + *vc * h[0]) * sin2th)
				/ v_h_a;
    jacobian[4][ 4] = (cross[1] * sinth_costh
	+ (*hc * v[1] + *vc * h[1]) * sin2th)
				/ v_h_a;
    jacobian[4][ 5] = (cross[2] * sinth_costh
	+ (*hc * v[2] + *vc * h[2]) * sin2th)
				/ v_h_a;
    cross3(a, v, cross);
    jacobian[4][ 6] = (cross[0] * sinth_costh
	+ (*vc * a[0] - v[0]) * sin2th)
				/ v_h_a;
    jacobian[4][ 7] = (cross[1] * sinth_costh
	+ (*vc * a[1] - v[1]) * sin2th)
				/ v_h_a;
    jacobian[4][ 8] = (cross[2] * sinth_costh
	+ (*vc * a[2] - v[2]) * sin2th)
				/ v_h_a;
    cross3(a, h, cross);
    jacobian[4][ 9] = (-cross[0] * sinth_costh
	+ (*hc * a[0] - h[0]) * sin2th)
				/ v_h_a;
    jacobian[4][10] = (-cross[1] * sinth_costh
	+ (*hc * a[1] - h[1]) * sin2th)
				/ v_h_a;
    jacobian[4][11] = (-cross[2] * sinth_costh
	+ (*hc * a[2] - h[2]) * sin2th)
				/ v_h_a;

    /* Calculate the covariance matrix J S Jt */
    for (i=0; i<5; i++)				/* tranpose */
	for (j=0; j<12; j++)
	    jacobian_t[j][i] = jacobian[i][j];
    for (i=0; i<5; i++)				/* J S */
	for (k=0; k<12; k++) {
	    m_5_12[i][k] = 0;
	    for (j=0; j<12; j++)
		m_5_12[i][k] += jacobian[i][j] * s[j][k];
	    }
    for (i=0; i<5; i++)				/* (J S) Jt */
	for (k=0; k<5; k++) {
	    s_int[i][k] = 0;
	    for (j=0; j<12; j++)
		s_int[i][k] += m_5_12[i][j] * jacobian_t[j][k];
	    }
    }


/******************************************************************************
********************************   CMOD_CAHV_MOVE   ***************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahv_move(p_i, q_i, c_i, a_i, h_i, v_i,
		p_f, q_f, c_f, a_f, h_f, v_f)
double p_i[3];		/* input initial pos of camera ref pt */
double q_i[4];		/* input initial orientation (quat) of camera ref pt */
double c_i[3];		/* input initial model center vector C */
double a_i[3];		/* input initial model axis   vector A */
double h_i[3];		/* input initial model horiz. vector H */
double v_i[3];		/* input initial model vert.  vector V */
double p_f[3];		/* input final pos of camera ref pt */
double q_f[4];		/* input final orientation (quat) of camera ref pt */
double c_f[3];		/* output final model center vector C */
double a_f[3];		/* output final model axis   vector A */
double h_f[3];		/* output final model horiz. vector H */
double v_f[3];		/* output final model vert.  vector V */
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

    /* Rotate the A, H, V vectors */
    mult331(r, a_i, a_f);
    mult331(r, h_i, h_f);
    mult331(r, v_i, v_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_POSE   ***************************
*******************************************************************************

    This function returns the position and rotation matrix of orientation
    for the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    */

void cmod_cahv_pose(c, a, h, v, p, r)
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double p[3];		/* output position vector */
double r[3][3];		/* output rotation matrix */
{
    int i;
    double vc, vs, av[3], vp[3], vpa[3];
    /*...
    double hc, hs, ah[3], hp[3], hpa[3];
    ...*/

    /* Copy over the position */
    copy3(c, p);

    /* Compute the orientation, forcing vertical to dominate */
    vs = mag3(cross3(a, v, av));	/* vertical scale & center */
    vc = dot3(a, v);
    vp[0] = (v[0] - vc * a[0]) / vs;	/* unit projection of V in img plane */
    vp[1] = (v[1] - vc * a[1]) / vs;
    vp[2] = (v[2] - vc * a[2]) / vs;
    cross3(vp, a, vpa);			/* right vector */
    for (i=0; i<3; i++) {		/* rotation matrix from orthogonal */
	r[i][0] = vpa[i];		/*   unit vectors                  */
	r[i][1] = a[i];
	r[i][2] = -vp[i];
	}

    /* Compute the orientation, forcing horizontal to dominate */
    /*...
    hs = mag3(cross3(a, h, ah));	/+ horizontal scale & center +/
    hc = dot3(a, h);
    hp[0] = (h[0] - hc * a[0]) / hs;	/+ unit projection of H in img plane +/
    hp[1] = (h[1] - hc * a[1]) / hs;
    hp[2] = (h[2] - hc * a[2]) / hs;
    cross3(hp, a, hpa);			/+ up vector +/
    for (i=0; i<3; i++) {		/+ rotation matrix from orthogonal +/
	r[i][0] = hp[i];		/+   unit vectors                  +/
	r[i][1] = a[i];
	r[i][2] = hpa[i];
	}
    ...*/
    }


/******************************************************************************
********************************   CMOD_CAHV_POSTURE   ************************
*******************************************************************************

    This function returns the rotation matrix of orientation for the given
    model. The absolute orientation is based on a reference orientation of
    the camera pointing straight down the Y axis, with Z up. The left- or
    right-handedness of the vectors is preserved, with the normal case being
    considered right-handed. */

void cmod_cahv_posture(a, h, v, r)
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double r[3][3];		/* output rotation matrix */
{
    int i;
    double hc, hs, ah[3], hp[3], vc, vs, av[3], vp[3];

    /* Compute the orientation, preserved handedness of vectors */
    hs = mag3(cross3(a, h, ah));	/* horizontal scale & center */
    hc = dot3(a, h);
    vs = mag3(cross3(a, v, av));	/* vertical   scale & center */
    vc = dot3(a, v);
    hp[0] = (h[0] - hc * a[0]) / hs;	/* unit projection of H in img plane */
    hp[1] = (h[1] - hc * a[1]) / hs;
    hp[2] = (h[2] - hc * a[2]) / hs;
    vp[0] = (v[0] - vc * a[0]) / vs;	/* unit projection of V in img plane */
    vp[1] = (v[1] - vc * a[1]) / vs;
    vp[2] = (v[2] - vc * a[2]) / vs;
    for (i=0; i<3; i++) {		/* rotation matrix from orthogonal */
	r[i][0] = hp[i];		/*   unit vectors                  */
	r[i][1] = a[i];
	r[i][2] = -vp[i];
	}
    }



/******************************************************************************
********************************   CMOD_CAHV_READ2   **************************
*******************************************************************************

    This function reads a CAHV model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

int cmod_cahv_read2(filename, xdim, ydim,
			c, a, h, v, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
int *xdim;		/* output number of columns */
int *ydim;		/* output number of rows */
double c[3];		/* output model center vector C */
double a[3];		/* output model axis   vector A */
double h[3];		/* output model horiz. vector H */
double v[3];		/* output model vert.  vector V */
double s[12][12];	/* output covariance of CAHV */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
double s_int[5][5];	/* output covariance matrix */
{
    int i;
    FILE *fp;

    /* Open the CAHV file */
    if ((fp = fopen(filename, "r")) == NULL) {
	fprintf(stderr, "Error opening CAHV file: %s\n", filename);
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

    /* Read C, A, H, V vectors */
    scanstr_(fp, "C =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &c[0], &c[1], &c[2]));
    scanstr_(fp, "A =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &a[0], &a[1], &a[2]));
    scanstr_(fp, "H =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &h[0], &h[1], &h[2]));
    scanstr_(fp, "V =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &v[0], &v[1], &v[2]));

    /* Read covariance matrix for C, A, H, V */
    scanstr_(fp, "S =");
    for (i=0; i<12; i++) {
	scan_(fp, 12, (fp, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
	    &s[i][ 0], &s[i][ 1], &s[i][ 2], &s[i][ 3],
	    &s[i][ 4], &s[i][ 5], &s[i][ 6], &s[i][ 7],
	    &s[i][ 8], &s[i][ 9], &s[i][10], &s[i][11]
	    ));
	scanstr_(fp, "\n");	/* skip any extra elements if reading CAHVOR */
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

    /* Close the CAHV file */
    fclose(fp);

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_CAHV_READ   ***************************
*******************************************************************************

    This function reads a CAHV model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahv_read(filename, c, a, h, v, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
double c[3];		/* output model center vector C */
double a[3];		/* output model axis   vector A */
double h[3];		/* output model horiz. vector H */
double v[3];		/* output model vert.  vector V */
double s[12][12];	/* output covariance of CAHV */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
double s_int[5][5];	/* output covariance matrix */
{
    return cmod_cahv_read2(filename, (int *)NULL, (int *)NULL,
				c, a, h, v, s, hs, hc, vs, vc, theta, s_int);
    }

/******************************************************************************
********************************   CMOD_CAHV_REFLECT   ************************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahv_reflect(double c_i[3], double a_i[3], double h_i[3], double v_i[3], 
		       double p[3], double n[3],
		       double c_f[3], double a_f[3], double h_f[3], double v_f[3], bool_t *parallel, bool_t *behind)
{
    double k, d, nu[3], u[3];
    double c[3], a[3], h[3], v[3];

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
    copy3(c, c_f);
    copy3(a, a_f);
    copy3(h, h_f);
    copy3(v, v_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_REFLECT_COV   ********************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahv_reflect_cov(s_i, n, s_f)
double s_i[12][12];	/* input initial covariance */
double n[3];		/* input normal to the reflecting plane */
double s_f[12][12];	/* output final covariance */
{
    double nu0, nu1, nu2, nu[3], r[3][3];
    void cmod_cahv_transform_cov();

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
    cmod_cahv_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_ROT_COV   ************************
*******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rot_cov(r_i, s_i, r_f, s_f)
double r_i[3][3];	/* input initial orientation (rot) of camera ref pt */
double s_i[12][12];	/* input initial covariance */
double r_f[3][3];	/* input final orientation (rot) of camera ref pt */
double s_f[12][12];	/* output final covariance */
{
    double r_it[3][3], r[3][3];
    void cmod_cahv_transform_cov();

    /* Calculate the rotation, R, from the initial to the final orientation */
    trans33(r_i, r_it);
    mult333(r_f, r_it, r);
    cmod_cahv_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_ROTATE_COV   *********************
*******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rotate_cov(q_i, s_i, q_f, s_f)
double q_i[4];		/* input initial orientation (quat) of camera ref pt */
double s_i[12][12];	/* input initial covariance */
double q_f[4];		/* input final orientation (quat) of camera ref pt */
double s_f[12][12];	/* output final covariance */
{
    double r_f[3][3], r_i[3][3];

    rotq(q_f, r_f);
    rotq(q_i, r_i);
    cmod_cahv_rot_cov(r_i, s_i, r_f, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_SCALE   **************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahv_scale(hscale, vscale, h1, v1, s1, h2, v2, s2)
double hscale;		/* input horizontal scale factor */
double vscale;		/* input vertical   scale factor */
double h1[3];		/* input  model horiz. vector H */
double v1[3];		/* input  model vert.  vector V */
double s1[12][12];	/* input  covariance matrix, or NULL */
double h2[3];		/* output model horiz. vector H */
double v2[3];		/* output model vert.  vector V */
double s2[12][12];	/* output covariance matrix, or NULL */
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
	for (i=0; i<12; i++)
	    for (j=0; j<12; j++)
		s2[i][j] = s1[i][j];
	}
    for (i=0; i<12; i++) {
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
********************************   CMOD_CAHV_SHIFT   **************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahv_shift(dx, dy, a1, h1, v1, h2, v2)
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
********************************   CMOD_CAHV_TRANSFORM_COV   ******************
*******************************************************************************

    This function transform a CAHV model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahv_transform_cov(s_i, r, s_f)
double s_i[12][12];	/* input initial covariance */
double r[3][3];		/* input transformation matrix of camera ref pt */
double s_f[12][12];	/* output final covariance */
{
    int i, j, k;
    double d;
    static double r12[12][12], r12t[12][12], stemp[12][12];

    /* Contruct a matrix, R12, (and its transpose) to rotate the covariance
	|R   |
	| R  |
	|  R |
	|   R|
    */
    for (i=0; i<12; i++)
	for (j=0; j<12; j++)
	    r12[i][j] = 0;
    for (i=0; i<3; i++) {
	for (j=0; j<3; j++) {
	    r12[i+0][j+0] = r[i][j];
	    r12[i+3][j+3] = r[i][j];
	    r12[i+6][j+6] = r[i][j];
	    r12[i+9][j+9] = r[i][j];
	    }
	}
    for (i=0; i<12; i++)
	for (j=0; j<12; j++)
	    r12t[i][j] = r12[j][i];

    /* Pre-multiply by the matrix */
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++) {
	    d = 0;
	    for (k=0; k<12; k++)
		d += r12[i][k] * s_i[k][j];
	    stemp[i][j] = d;
	    }
	}

    /* Post-multiply by the transpose */
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++) {
	    d = 0;
	    for (k=0; k<12; k++)
		d += stemp[i][k] * r12t[k][j];
	    s_f[i][j] = d;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_WARP_MODELS   ********************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models with have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Note that image warping will be necessary in order to use the new models.
    This function is based on the paper "Stereo images warping and
    triangulation," by Pierrick Grandjean, 20 Nov 1992. */

void cmod_cahv_warp_models(c1, a1, h1, v1, c2, a2, h2, v2,
			a, h, v, hs, hc, vs, vc, theta)
double c1[3];		/* input model 1 center vector C */
double a1[3];		/* input model 1 axis   vector A */
double h1[3];		/* input model 1 horiz. vector H */
double v1[3];		/* input model 1 vert.  vector V */
double c2[3];		/* input model 2 center vector C */
double a2[3];		/* input model 2 axis   vector A */
double h2[3];		/* input model 2 horiz. vector H */
double v2[3];		/* input model 2 vert.  vector V */
double a[3];		/* output virtual model axis   vector A */
double h[3];		/* output virtual model horiz. vector H */
double v[3];		/* output virtual model vert.  vector V */
double *hs;		/* output horizontal scale factor */
double *hc;		/* output horizontal center */
double *vs;		/* output vertical scale factor */
double *vc;		/* output vertical center */
double *theta;		/* output angle between axes */
{
    double f[3], g[3], hp[3], ap[3], app[3], vp[3];

    /* Compute a common image center and scale for the two models */
    *hc = (dot3(h1, a1) + dot3(h2, a2)) / 2.0;
    *vc = (dot3(v1, a1) + dot3(v2, a2)) / 2.0;
    *hs = (mag3(cross3(a1, h1, f))		/* Pierrick's eqn        */
	+ mag3(cross3(a2, h2, g))) / 2.0;	/*  was unclear:         */
    *vs = (mag3(cross3(a1, v1, f))		/*  clarified from       */
	+ mag3(cross3(a2, v2, g))) / 2.0;	/*  cmod_cahv_internal() */
    *theta = -PI / 2.0;

    /* Use common center and scale to construct common A, H, V */
    add3(a1, a2, app);
    sub3(c2, c1, f);
    cross3(app, f, g);	/* alter f (CxCy) to be         */
    cross3(g, app, f);	/*   perpendicular to average A */
    if (dot3(f, h1) > 0)
	scale3((*hs)/mag3(f), f, hp);
    else
	scale3(-(*hs)/mag3(f), f, hp);
    scale3(0.5, app, app);
    scale3(dot3(app, hp)/((*hs) * (*hs)), hp, g);
    sub3(app, g, ap);
    unit3(ap, a);
    cross3(a, hp, f);
    scale3((*vs)/(*hs), f, vp);
    scale3(*hc, a, f);
    add3(hp, f, h);
    scale3(*vc, a, f);
    add3(vp, f, v);
    }


/******************************************************************************
********************************   CMOD_CAHV_WARP_TO_CAHV   *******************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    different CAHV. */

void cmod_cahv_warp_to_cahv(c1, a1, h1, v1, pos1, c2, a2, h2, v2, pos2)
double c1[3];		/* input initial model center position vector   C */
double a1[3];		/* input initial model orthog. axis unit vector A */
double h1[3];		/* input initial model horizontal vector        H */
double v1[3];		/* input initial model vertical vector          V */
double pos1[2];		/* input 2D position from CAHV */
double c2[3];		/* input final model center position vector   C */
double a2[3];		/* input final model orthog. axis unit vector A */
double h2[3];		/* input final model horizontal vector        H */
double v2[3];		/* input final model vertical vector          V */
double pos2[2];		/* output 2D position for CAHV */
{
    double range;
    double p3[3], u3[3];

    cmod_cahv_2d_to_3d(pos1, c1, a1, h1, v1, p3, u3, (double (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahv_3d_to_2d(p3, c2, a2, h2, v2, &range, pos2, (double (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHV_WRITE2   *************************
*******************************************************************************

    This function writes a CAHV model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahv_write2(filename, comment, xdim, ydim,
			c, a, h, v, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
char *comment;		/* input one-line comment to record in file */
int xdim;		/* input number of columns */
int ydim;		/* input number of rows */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double s[12][12];	/* input covariance of CAHV */
double hs;		/* input horizontal scale factor */
double hc;		/* input horizontal center */
double vs;		/* input vertical scale factor */
double vc;		/* input vertical center */
double theta;		/* input angle between axes */
double s_int[5][5];	/* input covariance matrix */
{
    int i, j;
    FILE *fp;

    /* Open the CAHV file */
    if ((fp = fopen(filename, "w")) == NULL) {
	fprintf(stderr, "Error creating CAHV file: %s\n", filename);
	return FAILURE;
	}

    /* Write out the comment */
    fprintf(fp, "# %s\n", comment);

    /* Write out model type */
    fprintf(fp, "\n");
    fprintf(fp, "Model = CAHV = perspective, linear\n");

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

    /* Write covariance matrix for C, A, H, V */
    fprintf(fp, "\n");
    fprintf(fp, "S =\n");
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++)
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
********************************   CMOD_CAHV_WRITE   **************************
*******************************************************************************

    This function writes a CAHV model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahv_write(filename, comment, c, a, h, v, s, hs, hc, vs, vc, theta, s_int)
char *filename;		/* input filename */
char *comment;		/* input one-line comment to record in file */
double c[3];		/* input model center vector C */
double a[3];		/* input model axis   vector A */
double h[3];		/* input model horiz. vector H */
double v[3];		/* input model vert.  vector V */
double s[12][12];	/* input covariance of CAHV */
double hs;		/* input horizontal scale factor */
double hc;		/* input horizontal center */
double vs;		/* input vertical scale factor */
double vc;		/* input vertical center */
double theta;		/* input angle between axes */
double s_int[5][5];	/* input covariance matrix */
{
    return cmod_cahv_write2(filename, comment, -1, -1,
			c, a, h, v, s, hs, hc, vs, vc, theta, s_int);
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
