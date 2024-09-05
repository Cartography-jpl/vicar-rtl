/************************************************************************/
/******************************************************************************
*                                                                             *
*                    C A H V / C A H V O R / C A H V O R E                    *
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


	This file contains functions for using the Yakimovsky & Cunningham
	camera model, known locally as CAHV, and its extension into radial
	distortion by Donald Gennery, known as CAHVOR. */

#ifndef _CAHVOR_H
#define _CAHVOR_H

#include "xvmaininc.h"	       /* ONLY for _NO_PROTO definition */
			       /* (can be removed for non-VICAR environment) */

#ifdef __cplusplus
#define CONST const
#else
#define CONST
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef int bool_t;

#define CAHVOR_SUCCESS 0

#ifndef _NO_PROTO

/******************************************************************************
********************************   CMOD_CAHV_2D_TO_3D   ***********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs and the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point. If the parameter
    for the output partial matrix is passed as (double (*)[2])NULL, then it
    will not be calculated. */

void cmod_cahv_2d_to_3d(
	CONST double pos2[2],	/* input 2D position */
	CONST double c[3],	/* input model center vector C */
	CONST double a[3],	/* input model axis   vector A */
	CONST double h[3],	/* input model horiz. vector H */
	CONST double v[3],	/* input model vert.  vector V */
	double pos3[3],		/* output 3D origin of projection */
	double uvec3[3],	/* output unit vector ray of projection */
	double par[3][2]/* output partial-derivative matrix of uvec3 to pos2 */
    );

/******************************************************************************
********************************   CMOD_CAHV_3D_TO_2D   ***********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (double (*)[3])NULL, then it will not be calculated. */

void cmod_cahv_3d_to_2d(
	CONST double pos3[3],	/* input 3D position */
	CONST double c[3],	/* input model center vector C */
	CONST double a[3],	/* input model axis   vector A */
	CONST double h[3],	/* input model horiz. vector H */
	CONST double v[3],	/* input model vert.  vector V */
	double *range,		/* output range (same units as C) */
	double pos2[2],		/* output 2D image-plane projection */
	double par[2][3] /* output partial-derivative matrix of pos2 to pos3 */
    );

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

void cmod_cahv_3d_to_2d_ray(
	CONST double c[3],	/* input model center vector C */
	CONST double a[3],	/* input model axis   vector A */
	CONST double h[3],	/* input model horiz. vector H */
	CONST double v[3],	/* input model vert.  vector V */
	CONST double pos3[3],	/* input 3D position of line */
	CONST double uvec3[3],	/* input 3D unit vector of line */
	double pos2[2],		/* output 2D image-plane projection */
	double uvec2[2],   /* output 2D unit vector of back-projected line */
	double par[4][3]   /* output derivative matrix of pos2,uvec2 to pos3 */
    );

/******************************************************************************
********************************   CMOD_CAHV_INTERNAL   ***********************
*******************************************************************************

    This function calculates the internal camera model parameters for the
    CAHV camera model, deriving them from C, A, H, V, and their covariance
    matrix S. If either the input or output covariance matrices is NULL,
    then the covariance computation will be skipped. */

void cmod_cahv_internal(
	CONST double c[3],	/* input model center vector C */
	CONST double a[3],	/* input model axis   vector A */
	CONST double h[3],	/* input model horiz. vector H */
	CONST double v[3],	/* input model vert.  vector V */
	CONST double s[12][12],	/* input covariance of CAHV */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta,		/* output angle between axes */
	double s_int[5][5]	/* output covariance matrix */
    );

/******************************************************************************
********************************   CMOD_CAHV_MOVE   ***************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahv_move(
	CONST double p_i[3],	/* input initial pos of camera ref pt */
	CONST double q_i[4],/*input initial orientation(quat)of camera ref pt*/
	CONST double c_i[3],	/* input initial model center vector C */
	CONST double a_i[3],	/* input initial model axis   vector A */
	CONST double h_i[3],	/* input initial model horiz. vector H */
	CONST double v_i[3],	/* input initial model vert.  vector V */
	CONST double p_f[3],	/* input final pos of camera ref pt */
	CONST double q_f[4],/*input final orientation (quat) of camera ref pt*/
	double c_f[3],		/* output final model center vector C */
	double a_f[3],		/* output final model axis   vector A */
	double h_f[3],		/* output final model horiz. vector H */
	double v_f[3]		/* output final model vert.  vector V */
    );
/******************************************************************************
********************************   CMOD_CAHV_POSE   ***************************
*******************************************************************************

    This function returns the position and rotation matrix of orientation
    for the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    */
void cmod_cahv_pose(
	CONST double c[3],       /* input model center vector C */
	CONST double a[3],	 /* input model axis   vector A */
	CONST double h[3],	 /* input model horiz. vector H */
	CONST double v[3],	 /* input model vert.  vector V */
	double p[3],		 /* output position vector */
	double r[3][3]		 /* output rotation matrix */
     );
/******************************************************************************
********************************   CMOD_CAHV_POSTURE   ************************
*******************************************************************************

    This function returns the rotation matrix of orientation for the given
    model. The absolute orientation is based on a reference orientation of
    the camera pointing straight down the Y axis, with Z up. The left- or
    right-handedness of the vectors is preserved, with the normal case being
    considered right-handed. */
void cmod_cahv_posture(
        CONST double a[3],	 /* input model axis   vector A */
	CONST double h[3],	 /* input model horiz. vector H */
	CONST double v[3],	 /* input model vert.  vector V */
	double r[3][3] 		 /* output rotation matrix */
     );
/******************************************************************************
********************************   CMOD_CAHV_READ   ***************************
*******************************************************************************

    This function reads a CAHV model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahv_read(
	CONST char *filename,	/* input filename */
	double c[3],		/* output model center vector C */
	double a[3],		/* output model axis   vector A */
	double h[3],		/* output model horiz. vector H */
	double v[3],		/* output model vert.  vector V */
	double s[12][12],	/* output covariance of CAHV */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta,		/* output angle between axes */
	double s_int[5][5]	/* output covariance matrix */
    );

/******************************************************************************
********************************   CMOD_CAHV_READ2   **************************
*******************************************************************************

    This function reads a CAHV model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */
int  cmod_cahv_read2(
        CONST char *filename,  /* input filename */
	int *xdim,	       /* output number of columns */
	int *ydim,	       /* output number of rows */
	double c[3],	       /* output model center vector C */
	double a[3],	       /* output model axis   vector A */
	double h[3],	       /* output model horiz. vector H */
	double v[3],	       /* output model vert.  vector V */
	double s[12][12],      /* output covariance of CAHV */
	double *hs,	       /* output horizontal scale factor */
	double *hc,	       /* output horizontal center */
	double *vs,	       /* output vertical scale factor */
	double *vc,	       /* output vertical center */
	double *theta,	       /* output angle between axes */
	double s_int[5][5]     /* output covariance matrix */
     );
/******************************************************************************
********************************   CMOD_CAHV_REFLECT   ************************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahv_reflect(
        CONST double c_i[3],	/* input initial model center vector C */
	CONST double a_i[3],	/* input initial model axis   vector A */
	CONST double h_i[3],	/* input initial model horiz. vector H */
	CONST double v_i[3],	/* input initial model vert.  vector V */
	CONST double p[3],	/* input point on the reflecting plane */
	CONST double n[3],	/* input normal to the reflecting plane */
	double c_f[3],		/* output final model center vector C */
	double a_f[3],		/* output final model axis   vector A */
	double h_f[3],		/* output final model horiz. vector H */
	double v_f[3],		/* output final model vert.  vector V */
	bool_t *parallel,    /* output if camera view and plane are parallel */
	bool_t *behind	  /* output if camera is behind the reflecting plane */
    );

/******************************************************************************
********************************   CMOD_CAHV_REFLECT_COV   ********************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */
void cmod_cahv_reflect_cov(
        CONST double s_i[12][12], /* input initial covariance */
	double n[3],		  /* input normal to the reflecting plane */
	double s_f[12][12]	  /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHV_ROT_COV   ************************
*******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rot_cov(
      CONST double r_i[3][3],/*input initial orientation(rot)of camera ref pt*/
      CONST double s_i[12][12],/* input initial covariance */
      CONST double r_f[3][3],  /*input final orientation(rot)of camera ref pt*/
      double s_f[12][12]       /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHV_ROTATE_COV   *********************
*******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rotate_cov(
        CONST double q_i[4],/*input initial orientation(quat)of camera ref pt*/
        CONST double s_i[12][12],/* input initial covariance */
        CONST double q_f[4],  /*input final orientation(quat)of camera ref pt*/
	double s_f[12][12]   /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHV_SCALE   **************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahv_scale(
        CONST double hscale,     /* input horizontal scale factor */
	CONST double vscale,     /* input vertical   scale factor */
	CONST double h1[3],      /* input  model horiz. vector H */
	CONST double v1[3],      /* input  model vert.  vector V */
	CONST double s1[12][12], /* input  covariance matrix, or NULL */
	double h2[3],		 /* output model horiz. vector H */
	double v2[3],		 /* output model vert.  vector V */
	double s2[12][12]	 /* output covariance matrix, or NULL */
     );

/******************************************************************************
********************************   CMOD_CAHV_SHIFT   **************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahv_shift(
        CONST double dx,		/* input horizontal shift */
        CONST double dy,		/* input vertical   shift */
	CONST double a1[3],		/* input  model axis   vector A */
	CONST double h1[3],		/* input  model horiz. vector H */
	CONST double v1[3],		/* input  model vert.  vector V */
	double h2[3],		/* output model horiz. vector H */
	double v2[3]		/* output model vert.  vector V */
     );

/******************************************************************************
********************************   CMOD_CAHV_TRANSFORM_COV   ******************
*******************************************************************************

    This function transform a CAHV model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahv_transform_cov(
        CONST double s_i[12][12], /* input initial covariance */
        CONST double r[3][3],  /*input transformation matrix of camera ref pt*/
	double s_f[12][12]	  /* output final covariance */
     );

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
void cmod_cahv_warp_models(
	CONST double c1[3],	/* input model 1 center vector C */
	CONST double a1[3],	/* input model 1 axis   vector A */
	CONST double h1[3],	/* input model 1 horiz. vector H */
	CONST double v1[3],	/* input model 1 vert.  vector V */
	CONST double c2[3],	/* input model 2 center vector C */
	CONST double a2[3],	/* input model 2 axis   vector A */
	CONST double h2[3],	/* input model 2 horiz. vector H */
	CONST double v2[3],	/* input model 2 vert.  vector V */
	double a[3],		/* output virtual model axis   vector A */
	double h[3],		/* output virtual model horiz. vector H */
	double v[3],		/* output virtual model vert.  vector V */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta		/* output angle between axes */
    );

/******************************************************************************
********************************   CMOD_CAHV_WARP_TO_CAHV   *******************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    different CAHV. */

void cmod_cahv_warp_to_cahv(
        CONST double c1[3],/* input initial model center position vector   C */
	CONST double a1[3],/* input initial model orthog. axis unit vector A */
	CONST double h1[3],/* input initial model horizontal vector        H */
	CONST double v1[3],/* input initial model vertical vector          V */
	CONST double pos1[2],/* input 2D position from CAHV */
	CONST double c2[3],  /* input final model center position vector   C */
	CONST double a2[3],  /* input final model orthog. axis unit vector A */
	CONST double h2[3],  /* input final model horizontal vector        H */
	CONST double v2[3],  /* input final model vertical vector          V */
	double pos2[2]       /* output 2D position for CAHV */
     );

/******************************************************************************
********************************   CMOD_CAHV_WRITE   **************************
*******************************************************************************

    This function writes a CAHV model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int  cmod_cahv_write(
        CONST char *filename,	 /* input filename */
	CONST char *comment,	 /* input one-line comment to record in file */
	CONST double c[3],	 /* input model center vector C */
	CONST double a[3],	 /* input model axis   vector A */
	CONST double h[3],	 /* input model horiz. vector H */
	CONST double v[3],	 /* input model vert.  vector V */
	CONST double s[12][12],	 /* input covariance of CAHV */
	CONST double hs,	 /* input horizontal scale factor */
	CONST double hc,	 /* input horizontal center */
	CONST double vs,	 /* input vertical scale factor */
	CONST double vc,	 /* input vertical center */
	CONST double theta,	 /* input angle between axes */
	CONST double s_int[5][5] /* input covariance matrix */
     );

/******************************************************************************
********************************   CMOD_CAHV_WRITE2   *************************
*******************************************************************************

    This function writes a CAHV model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int  cmod_cahv_write2(
        CONST char *filename,	 /* input filename */
	CONST char *comment,	 /* input one-line comment to record in file */
	CONST int xdim,		 /* input number of columns */
	CONST int ydim,		 /* input number of rows */
	CONST double c[3],	 /* input model center vector C */
	CONST double a[3],	 /* input model axis   vector A */
	CONST double h[3],	 /* input model horiz. vector H */
	CONST double v[3],	 /* input model vert.  vector V */
	CONST double s[12][12],	 /* input covariance of CAHV */
	CONST double hs,	 /* input horizontal scale factor */
	CONST double hc,	 /* input horizontal center */
	CONST double vs,	 /* input vertical scale factor */
	CONST double vc,	 /* input vertical center */
	CONST double theta,	 /* input angle between axes */
	CONST double s_int[5][5] /* input covariance matrix */
    );

/******************************************************************************
********************************   CMOD_CAHVOR_WARP_TO_CAHV   *****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVOR and warps it into an image coordinate modeled by
    CAHV. */

void cmod_cahv_warp_to_cahvor(
	CONST double c1[3], /* input initial model center position vector  C */
	CONST double a1[3], /* input initial model orthog. axis unit vect. A */
	CONST double h1[3], /* input initial model horizontal vector       H */
	CONST double v1[3], /* input initial model vertical vector         V */
	CONST double pos1[2],/* input 2D position from CAHV */
	CONST bool_t approx, /* input flag to use fast approximation */
	CONST double c2[3],  /* input final model center position vector   C */
	CONST double a2[3],  /* input final model orthog. axis unit vector A */
	CONST double h2[3],  /* input final model horizontal vector        H */
	CONST double v2[3],  /* input final model vertical vector          V */
	CONST double o2[3],  /* input final model optical axis unit vector O */
	CONST double r2[3],  /* input final model radial-distortion terms  R */
	double pos2[2]	     /* output 2D position for CAHVOR */
    );

/******************************************************************************
********************************   CMOD_CAHVOR_2D_TO_3D   *********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point.  If the parameter
    for the output partial matrix is passed as (double (*)[2])NULL, then it
    will not be calculated. */

void cmod_cahvor_2d_to_3d(
	CONST double pos2[2],	/* input 2D position */
	CONST double c[3],	/* input model center position vector   C */
	CONST double a[3],	/* input model orthog. axis unit vector A */
	CONST double h[3],	/* input model horizontal vector        H */
	CONST double v[3],	/* input model vertical vector          V */
	CONST double o[3],	/* input model optical axis unit vector O */
	CONST double r[3],	/* input model radial-distortion terms  R */
	CONST bool_t approx,	/* input flag to use fast approximation */
	double pos3[3],		/* output 3D origin of projection */
	double uvec3[3],	/* output unit vector ray of projection */
	double par[3][2]/* output partial-derivative matrix of uvec3 to pos2 */
    );

/******************************************************************************
********************************   CMOD_CAHVOR_3D_TO_2D   *********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (double (*)[3])NULL, then it will not be calculated. */

void cmod_cahvor_3d_to_2d(
	CONST double pos3[3],	/* input 3D position */
	CONST double c[3],	/* input model center vector C */
	CONST double a[3],	/* input model axis   vector A */
	CONST double h[3],	/* input model horiz. vector H */
	CONST double v[3],	/* input model vert.  vector V */
	CONST double o[3],	/* input model optical axis  O */
	CONST double r[3],	/* input model radial-distortion terms R */
	CONST bool_t approx,	/* input flag to use fast approximation */
	double *range,		/* output range (same units as C) */
	double pos2[2],		/* output 2D image-plane projection */
	double par[2][3] /* output partial-derivative matrix of pos2 to pos3 */
    );
/******************************************************************************
********************************   CMOD_CAHVOR_3D_TO_2D_POINT   ***************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane. In addition it calculates the partial-derivative matrix of the 2D
    point with respect to the 3D vector of the input ray. If the parameter for
    the output partial matrix is passed as (double (*)[3])NULL, then it will
    not be calculated. */

void cmod_cahvor_3d_to_2d_point(
        CONST double c[3],	/* input model center vector C */
	CONST double a[3],	/* input model axis   vector A */
	CONST double h[3],	/* input model horiz. vector H */
	CONST double v[3],	/* input model vert.  vector V */
	CONST double o[3],	/* input model optical axis  O */
	CONST double r[3],	/* input model radial-distortion terms R */
	CONST bool_t approx,	/* input flag to use fast approximation */
	CONST double pos3[3],	/* input 3D position of line */
	CONST double uvec3[3],	/* input 3D unit vector of line */
	double pos2[2],		/* output 2D image-plane projection */
	double par[2][3]	/* output derivative matrix of pos2 to uvec3 */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_MOVE   *************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahvor_move(
	CONST double p_i[3],/* input initial pos of camera ref pt */
	CONST double q_i[4],/*input initial orientation(quat)of camera ref pt*/
	CONST double c_i[3],/* input initial model center vector C */
	CONST double a_i[3],/* input initial model axis   vector A */
	CONST double h_i[3],/* input initial model horiz. vector H */
	CONST double v_i[3],/* input initial model vert.  vector V */
	CONST double o_i[3],/* input initial model optical axis unit vector O*/
	CONST double r_i[3],/* input initial model radial-distortion terms  R*/
	CONST double p_f[3],/* input final pos of camera ref pt */
	CONST double q_f[4],/* input final orientation(quat) of camera ref pt*/
	double c_f[3],	    /* output final model center vector C */
	double a_f[3],	    /* output final model axis   vector A */
	double h_f[3],	    /* output final model horiz. vector H */
	double v_f[3],	    /* output final model vert.  vector V */
	double o_f[3],	    /* output final model optical axis unit vector O */
	double r_f[3]	    /* output final model radial-distortion terms  R */
    );

/******************************************************************************
********************************   CMOD_CAHVOR_READ   *************************
*******************************************************************************

    This function reads a CAHVOR model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahvor_read(
	CONST char *filename,	/* input filename */
	double c[3],		/* output model center vector C */
	double a[3],		/* output model axis   vector A */
	double h[3],		/* output model horiz. vector H */
	double v[3],		/* output model vert.  vector V */
	double o[3],		/* output model optical axis unit vector O */
	double r[3],		/* output model radial-distortion terms  R */
	double s[18][18],	/* output covariance of CAHVOR */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta,		/* output angle between axes */
	double s_int[5][5]	/* output covariance matrix */
    );

/******************************************************************************
********************************   CMOD_CAHVOR_READ2   ************************
*******************************************************************************

    This function reads a CAHVOR model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

int  cmod_cahvor_read2(
        CONST char *filename,	/* input filename */
	int *xdim,		/* output number of columns */
	int *ydim,		/* output number of rows */
	double c[3],		/* output model center vector C */
	double a[3],		/* output model axis   vector A */
	double h[3],		/* output model horiz. vector H */
	double v[3],		/* output model vert.  vector V */
	double o[3],		/* output model optical axis unit vector O */
	double r[3],		/* output model radial-distortion terms  R */
	double s[18][18],	/* output covariance of CAHVOR */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta,		/* output angle between axes */
	double s_int[5][5]	/* output covariance matrix */
    );


/******************************************************************************
********************************   CMOD_CAHVOR_REFLECT   **********************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahvor_reflect(
        CONST double c_i[3],/* input initial model           center vector C */
	CONST double a_i[3],/* input initial model           axis   vector A */
	CONST double h_i[3],/* input initial model           horiz. vector H */
	CONST double v_i[3],/* input initial model           vert.  vector V */
	CONST double o_i[3],/* input initial model optical axis unit vectorO */
	CONST double r_i[3],/* input initial modelradial-distortion terms  R */
	CONST double p[3],  /* input point on the reflecting plane */
	CONST double n[3],  /* input normal to the reflecting plane */
	double c_f[3],	    /* output final model            center vector C */
	double a_f[3],	    /* output final model            axis   vector A */
	double h_f[3],	    /* output final model            horiz. vector H */
	double v_f[3],	    /* output final model            vert.  vector V */
	double o_f[3],	    /* output final model optical axis unit vector O */
	double r_f[3],	    /* output final model radial-distortion terms  R */
	bool_t *parallel,   /* output if camera view and plane are parallel  */
	bool_t *behind	    /*output if camera is behind the reflecting plane*/
    );

/******************************************************************************
********************************   CMOD_CAHVOR_REFLECT_COV   ******************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */
  
void cmod_cahvor_reflect_cov(
        CONST double s_i[18][18], /* input initial covariance */
        CONST double n[3],	  /* input normal to the reflecting plane */
	double s_f[18][18]	  /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_ROT_COV   **********************
*******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rot_cov(
       CONST double r_i[3][3],/*input initial orientation(rot)ofCamera ref pt*/
       CONST double s_i[18][18],/* input initial covariance */
       CONST double r_f[3][3],/*input final orientation(rot) of camera ref pt*/
       double s_f[18][18]     /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_ROTATE_COV   *******************
*******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rotate_cov(
        CONST double q_i[4],/*input initial orientation(quat)of camera ref pt*/
        CONST double s_i[18][18],  /* input initial covariance */
        CONST double q_f[4],/* input final orientation(quat) of camera ref pt*/
	double s_f[18][18]	   /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_SCALE   ************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvor_scale(
        CONST double hscale,		/* input horizontal scale factor */
	CONST double vscale,		/* input vertical   scale factor */
	CONST double h1[3],		/* input  model horiz. vector H */
	CONST double v1[3],		/* input  model vert.  vector V */
	CONST double s1[18][18],	/* input  covariance matrix, or NULL */
	double h2[3],		/* output model horiz. vector H */
	double v2[3],		/* output model vert.  vector V */
	double s2[18][18]	/* output covariance matrix, or NULL */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_SHIFT   ************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvor_shift(
        CONST double dx,	/* input horizontal shift */
	CONST double dy,	/* input vertical   shift */
	CONST double a1[3],	/* input  model axis   vector A */
	CONST double h1[3],	/* input  model horiz. vector H */
	CONST double v1[3],	/* input  model vert.  vector V */
	double h2[3],		/* output model horiz. vector H */
	double v2[3]		/* output model vert.  vector V */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_TRANSFORM_COV   ****************
*******************************************************************************

    This function transform a CAHVOR model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvor_transform_cov(
        CONST double s_i[18][18],/* input initial covariance */
	CONST double r[3][3],/* input transformation matrix of camera ref pt */
	double s_f[18][18]   /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_WARP_FROM_CAHV   ***************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    CAHVOR. */

void cmod_cahvor_warp_from_cahv(
        CONST double c1[3],/* input initial model center position vector   C */
	CONST double a1[3],/* input initial model orthog. axis unit vector A */
	CONST double h1[3],/* input initial model horizontal vector        H */
	CONST double v1[3],/* input initial model vertical vector          V */
	CONST double pos1[2],/* input 2D position from CAHV */
	CONST bool_t approx,/* input flag to use fast approximation */
	CONST double c2[3],/* input final model center position vector   C */
	CONST double a2[3],/* input final model orthog. axis unit vector A */
	CONST double h2[3],/* input final model horizontal vector        H */
	CONST double v2[3],/* input final model vertical vector          V */
	CONST double o2[3],/* input final model optical axis unit vector O */
	CONST double r2[3],/* input final model radial-distortion terms  R */
	double pos2[2]     /* output 2D position for CAHVOR */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODEL   *******************
*******************************************************************************

    This function warps a camera model so that it is purely linear. The
    parameters C and A will not change. The parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvor_warp_model(
        CONST double c[3],   /* input model center vector C */
	CONST double a[3],   /* input model axis   vector A */
	CONST double h[3],   /* input model horiz. vector H */
	CONST double v[3],   /* input model vert.  vector V */
	CONST double o[3],   /* input model axis   vector O */
	CONST double r[3],   /* input model dist.  terms  R */
	CONST int minfov,    /* input if to minimize to common field of view */
	CONST int idims[2],  /* input image dimensions of input  model */
	CONST int odims[2],	   /* input image dimensions of output model */
	double a2[3],		/* output virtual model axis   vector A */
	double h2[3],		/* output virtual model horiz. vector H */
	double v2[3],		/* output virtual model vert.  vector V */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta		/* output angle between axes */
    );

/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODELS   ******************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models with have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Since the output models will be linear, the parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvor_warp_models(
        CONST double c1[3],  /* input model 1 center vector C */
	CONST double a1[3],  /* input model 1 axis   vector A */
	CONST double h1[3],  /* input model 1 horiz. vector H */
	CONST double v1[3],  /* input model 1 vert.  vector V */
	CONST double o1[3],  /* input model 1 axis   vector O */
	CONST double r1[3],  /* input model 1 dist.  terms  R */
	CONST double c2[3],  /* input model 2 center vector C */
	CONST double a2[3],  /* input model 2 axis   vector A */
	CONST double h2[3],  /* input model 2 horiz. vector H */
	CONST double v2[3],  /* input model 2 vert.  vector V */
	CONST double o2[3],  /* input model 2 axis   vector O */
	CONST double r2[3],  /* input model 2 dist.  terms  R */
	CONST int minfov,    /* input if to minimize to common field of view */
	CONST int idims[2],  /* input image dimensions of input  models */
	CONST int odims[2],  /* input image dimensions of output models */
	double a[3],	     /* output virtual model axis   vector A */
	double h[3],	     /* output virtual model horiz. vector H */
	double v[3],	     /* output virtual model vert.  vector V */
	double *hs,	     /* output horizontal scale factor */
	double *hc,	     /* output horizontal center */
	double *vs,	     /* output vertical scale factor */
	double *vc,	     /* output vertical center */
	double *theta	     /* output angle between axes */
    );

/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODELS_NODIMS   ***********
*******************************************************************************

    This function is the same as cmod_cahvor_warp_models_dims(), but for cases
    where the image dimensions are not known precisely. This function fills
    in those dimensions with approximate values generated from the horizontal
    and vertical centers of the input models themselves. */

void cmod_cahvor_warp_models_nodims(
        CONST double c1[3],  /* input model 1 center vector C */
	CONST double a1[3],  /* input model 1 axis   vector A */
	CONST double h1[3],  /* input model 1 horiz. vector H */
	CONST double v1[3],  /* input model 1 vert.  vector V */
	CONST double o1[3],  /* input model 1 axis   vector O */
	CONST double r1[3],  /* input model 1 dist.  terms  R */
	CONST double c2[3],  /* input model 2 center vector C */
	CONST double a2[3],  /* input model 2 axis   vector A */
	CONST double h2[3],  /* input model 2 horiz. vector H */
	CONST double v2[3],  /* input model 2 vert.  vector V */
	CONST double o2[3],  /* input model 2 axis   vector O */
	CONST double r2[3],  /* input model 2 dist.  terms  R */
	CONST int minfov,    /* input if to minimize to common field of view */
	double a[3],	     /* output virtual model axis   vector A */
	double h[3],	     /* output virtual model horiz. vector H */
	double v[3],	     /* output virtual model vert.  vector V */
	double *hs,	     /* output horizontal scale factor */
	double *hc,	     /* output horizontal center */
	double *vs,	     /* output vertical scale factor */
	double *vc,	     /* output vertical center */
	double *theta	     /* output angle between axes */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_WARP_TO_CAHV   *****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVOR and warps it into an image coordinate modeled by
    CAHV. */

void cmod_cahvor_warp_to_cahv(
	CONST double c1[3],/* input initial model center position vector   C */
	CONST double a1[3],/* input initial model orthog. axis unit vector A */
	CONST double h1[3],/* input initial model horizontal vector        H */
	CONST double v1[3],/* input initial model vertical vector          V */
	CONST double o1[3],/* input initial model optical axis unit vector O */
	CONST double r1[3],/* input initial model radial-distortion terms  R */
	CONST double pos1[2],/* input 2D position from CAHVOR */
	CONST bool_t approx,/* input flag to use fast approximation */
	CONST double c2[3],/* input final model center position vector   C */
	CONST double a2[3],/* input final model orthog. axis unit vector A */
	CONST double h2[3],/* input final model horizontal vector        H */
	CONST double v2[3],/* input final model vertical vector          V */
	double pos2[2]	   /* output 2D position for CAHV */
     );

/******************************************************************************
********************************   CMOD_CAHVOR_WARP_TO_CAHVOR   ***************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVOR and warps it into an image coordinate modeled by
    a different CAHVOR. */

void cmod_cahvor_warp_to_cahvor(
        CONST double c1[3],/* input initial model center position vector   C */
	CONST double a1[3],/* input initial model orthog. axis unit vector A */
	CONST double h1[3],/* input initial model horizontal vector        H */
	CONST double v1[3],/* input initial model vertical vector          V */
	CONST double o1[3],/* input initial model optical axis unit vector O */
	CONST double r1[3],/* input initial model radial-distortion terms  R */
	CONST double pos1[2],    /* input 2D position from CAHVOR */
	CONST bool_t approx,     /* input flag to use fast approximation */
	CONST double c2[3],  /* input final model center position vector   C */
	CONST double a2[3],  /* input final model orthog. axis unit vector A */
	CONST double h2[3],  /* input final model horizontal vector        H */
	CONST double v2[3],  /* input final model vertical vector          V */
	CONST double o2[3],  /* input final model optical axis unit vector O */
	CONST double r2[3],  /* input final model radial-distortion terms  R */
	double pos2[2]       /* output 2D position for CAHV */
     );
/******************************************************************************
********************************   CMOD_CAHVOR_WRITE   ************************
*******************************************************************************

    This function writes a CAHVOR model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahvor_write(
        CONST char *filename,	 /* input filename */
	CONST char *comment,	 /* input one-line comment to record in file */
	CONST double c[3],	 /* input model center vector C */
	CONST double a[3],	 /* input model axis   vector A */
	CONST double h[3],	 /* input model horiz. vector H */
	CONST double v[3],	 /* input model vert.  vector V */
	CONST double o[3],	 /* input model optical axis unit vector O */
	CONST double r[3],	 /* input model radial-distortion terms  R */
	CONST double s[18][18],	 /* input covariance of CAHVOR */
	CONST double hs,	 /* input horizontal scale factor */
	CONST double hc,	 /* input horizontal center */
	CONST double vs,	 /* input vertical scale factor */
	CONST double vc,	 /* input vertical center */
	CONST double theta,	 /* input angle between axes */
	CONST double s_int[5][5] /* input covariance matrix */
	);

/******************************************************************************
********************************   CMOD_CAHVOR_WRITE2   ***********************
*******************************************************************************

    This function writes a CAHVOR model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int cmod_cahvor_write2(
      CONST char *filename,   /* input filename */
      CONST char *comment,    /* input one-line comment to record in file */
      CONST int xdim,	      /* input number of columns */
      CONST int ydim,	      /* input number of rows */
      CONST double c[3],      /* input model center vector C */
      CONST double a[3],      /* input model axis   vector A */
      CONST double h[3],      /* input model horiz. vector H */
      CONST double v[3],      /* input model vert.  vector V */
      CONST double o[3],      /* input model optical axis unit vector O */
      CONST double r[3],      /* input model radial-distortion terms  R */
      CONST double s[18][18], /* input covariance of CAHVOR */
      CONST double hs,	      /* input horizontal scale factor */
      CONST double hc,	      /* input horizontal center */
      CONST double vs,	      /* input vertical scale factor */
      CONST double vc,	      /* input vertical center */
      CONST double theta,     /* input angle between axes */
      CONST double s_int[5][5]/* input covariance matrix */
      );

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
        CONST double pos2[2],	   /* input 2D position */
	CONST int mtype,	   /* input type of model */
	CONST double mparm,	   /* input model parameter */
	CONST double c[3],	   /* input model center position vector   C */
	CONST double a[3],	   /* input model orthog. axis unit vector A */
	CONST double h[3],	   /* input model horizontal vector        H */
	CONST double v[3],	   /* input model vertical vector          V */
	CONST double o[3],	   /* input model optical axis unit vector O */
	CONST double r[3],	   /* input model radial-distortion terms  R */
	CONST double e[3],	   /* input model entrance-pupil    terms  E */
	CONST bool_t approx,	   /* input flag to use fast approximation */
	double pos3[3],		   /* output 3D origin of projection */
	double uvec3[3],	   /* output unit vector ray of projection */
	double ppar[3][2],/*output partial-derivative matrix of pos3 to pos2*/
	double upar[3][2] /*output partial-derivative matrix of uvec3 to pos2*/
     );

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

static void cmod_cahvore_2d_to_3d_general(
       CONST double pos2[2],		/* input 2D position */
       CONST double linearity, /* input linearity parameter */
       CONST double c[3],      /* input model center position vector   C */
       CONST double a[3],      /* input model orthog. axis unit vector A */
       CONST double h[3],      /* input model horizontal vector        H */
       CONST double v[3],      /* input model vertical vector          V */
       CONST double o[3],      /* input model optical axis unit vector O */
       CONST double r[3],      /* input model radial-distortion terms  R */
       CONST double e[3],      /* input model entrance-pupil    terms  E */
       CONST bool_t approx,    /* input flag to use fast approximation */
       double pos3[3],	       /* output 3D origin of projection */
       double uvec3[3],	       /* output unit vector ray of projection */
       double ppar[3][2],/* output partial-derivative matrix of pos3  to pos2*/
       double upar[3][2] /* output partial-derivative matrix of uvec3 to pos2*/
       );

/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D   ********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (double (*)[3])NULL, then it will not be calculated. */

void cmod_cahvore_3d_to_2d(
        CONST double pos3[3],	    /* input 3D position */
	CONST int mtype,	    /* input type of model */
	CONST double mparm,	    /* input model parameter */
	CONST double c[3],	    /* input model center vector C */
	CONST double a[3],	    /* input model axis   vector A */
	CONST double h[3],	    /* input model horiz. vector H */
	CONST double v[3],	    /* input model vert.  vector V */
	CONST double o[3],	    /* input model optical axis  O */
	CONST double r[3],	    /* input model radial-distortion terms R */
	CONST double e[3],	    /* input model entrance-pupil    terms E */
	CONST bool_t approx,	    /* input flag to use fast approximation */
	double *range,	 /* output range along A (same units as C) */
	double pos2[2],	 /* output 2D image-plane projection */
	double par[2][3] /* output partial-derivative matrix of pos2 to pos3 */
	);

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
       CONST double pos3[3],		/* input 3D position */
       CONST double linearity,	/* input linearity parameter */
       CONST double c[3],	    /* input model center vector C */
       CONST double a[3],	    /* input model axis   vector A */
       CONST double h[3],	    /* input model horiz. vector H */
       CONST double v[3],	    /* input model vert.  vector V */
       CONST double o[3],	    /* input model optical axis  O */
       CONST double r[3],	    /* input model radial-distortion terms R */
       CONST double e[3],	    /* input model entrance-pupil    terms E */
       CONST bool_t approx,	    /* input flag to use fast approximation */
       double *range,		/* output range along A (same units as C) */
       double pos2[2],		/* output 2D image-plane projection */
       double par[2][3]	/* output partial-derivative matrix of pos2 to pos3 */
       );

/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D_POINT   **************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane. In addition it calculates the partial-derivative matrix of the 2D
    point with respect to the 3D vector of the input ray. If the parameter for
    the output partial matrix is passed as (double (*)[3])NULL, then it will
    not be calculated. */

void cmod_cahvore_3d_to_2d_point(
        CONST int mtype,	    /* input type of model */
	CONST double mparm,	    /* input model parameter */
	CONST double c[3],	    /* input model center vector C */
	CONST double a[3],	    /* input model axis   vector A */
	CONST double h[3],	    /* input model horiz. vector H */
	CONST double v[3],	    /* input model vert.  vector V */
	CONST double o[3],	    /* input model optical axis  O */
	CONST double r[3],	    /* input model radial-distortion terms R */
	CONST double e[3],	    /* input model entrance-pupil    terms E */
	CONST bool_t approx,	    /* input flag to use fast approximation */
	CONST double pos3[3],	    /* input 3D position of line */
	CONST double uvec3[3],	    /* input 3D unit vector of line */
	double pos2[2],		/* output 2D image-plane projection */
	double par[2][3]	/* output derivative matrix of pos2 to uvec3 */
     );

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
        CONST int xdim1,		/* input number of columns */
	CONST int ydim1,		/* input number of rows */
	CONST int mtype1,		/* input type of model */
	CONST double mparm1,		/* input model parameter */
	CONST double c1[3],		/* input model 1 center vector C */
	CONST double a1[3],		/* input model 1 axis   vector A */
	CONST double h1[3],		/* input model 1 horiz. vector H */
	CONST double v1[3],		/* input model 1 vert.  vector V */
	CONST double o1[3],		/* input model 1 axis   vector O */
	CONST double r1[3],		/* input model 1 dist.  terms  R */
	CONST double e1[3],		/* input model 1 pupil  terms  E */
	CONST int xdim2,		/* input number of columns */
	CONST int ydim2,		/* input number of rows */
	CONST int mtype2,		/* input type of model */
	CONST double mparm2,		/* input model parameter */
	CONST double c2[3],		/* input model 2 center vector C */
	CONST double a2[3],		/* input model 2 axis   vector A */
	CONST double h2[3],		/* input model 2 horiz. vector H */
	CONST double v2[3],		/* input model 2 vert.  vector V */
	CONST double o2[3],		/* input model 2 axis   vector O */
	CONST double r2[3],		/* input model 2 dist.  terms  R */
	CONST double e2[3],		/* input model 2 pupil  terms  E */
	double a[3],		/* output virtual model axis   vector A */
	double h[3],		/* output virtual model horiz. vector H */
	double v[3],		/* output virtual model vert.  vector V */
	double o[3],		/* output virtual model axis   vector O */
	double r[3],		/* output virtual model dist.  terms  R */
	double e[3],		/* output virtual model pupil  terms  E */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta		/* output angle between axes */
     );

/******************************************************************************
********************************   CMOD_CAHVORE_MOVE   ************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahvore_move(
        CONST double p_i[3],	      /* input initial pos of camera ref pt */
        CONST double q_i[4],/*input initial orientation(quat)of camera ref pt*/
	CONST double c_i[3],          /* input initial model center vector C */
	CONST double a_i[3],	      /* input initial model axis   vector A */
	CONST double h_i[3],	      /* input initial model horiz. vector H */
	CONST double v_i[3],	      /* input initial model vert.  vector V */
	CONST double o_i[3],/*input initial model optical axis unit vector O */
	CONST double r_i[3],/* input initial model radial-distortion terms R */
	CONST double e_i[3], /* input initial model entrance-pupil  terms  E */
	CONST double p_f[3],	      /* input final pos of camera ref pt */
	CONST double q_f[4],/*input final orientation (quat) of camera ref pt*/
	double c_f[3],	               /* output final model center vector C */
	double a_f[3],	               /* output final model axis   vector A */
	double h_f[3],	               /* output final model horiz. vector H */
	double v_f[3],	               /* output final model vert.  vector V */
	double o_f[3],	    /* output final model optical axis unit vector O */
	double r_f[3],	    /* output final model radial-distortion terms  R */
	double e_f[3]	    /* output final model entrance-pupil    terms  E */
     );

/******************************************************************************
********************************   CMOD_CAHVORE_READ   ************************
*******************************************************************************

    This function reads a CAHVORE model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int  cmod_cahvore_read(
        CONST char *filename,	/* input filename */
	int *xdim,		/* output number of columns */
	int *ydim,		/* output number of rows */
	int *mtype,		/* output type of model */
	double *mparm,		/* output model parameter */
	double c[3],		/* output model center vector C */
	double a[3],		/* output model axis   vector A */
	double h[3],		/* output model horiz. vector H */
	double v[3],		/* output model vert.  vector V */
	double o[3],		/* output model optical axis unit vector O */
	double r[3],		/* output model radial-distortion terms  R */
	double e[3],		/* output model entrance-pupil    terms  E */
	double s[21][21],	/* output covariance of CAHVORE */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta,		/* output angle between axes */
	double s_int[5][5]	/* output covariance matrix */
     );

/******************************************************************************
********************************   CMOD_CAHVORE_REFLECT   *********************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahvore_reflect(
        CONST double c_i[3],/* input initial model           center vector C */
	CONST double a_i[3],/* input initial model           axis   vector A */
	CONST double h_i[3],/* input initial model           horiz. vector H */
	CONST double v_i[3],/* input initial model           vert.  vector V */
	CONST double o_i[3],/*input initial model optical axis unit vector O */
	CONST double r_i[3],/* input initial model radial-distortion terms R */
	CONST double e_i[3],/* input initial model entrance-pupil   terms  E */
	CONST double p[3],  /* input point on the reflecting plane */
	CONST double n[3],  /* input normal to the reflecting plane */
	double c_f[3],	    /* output final model            center vector C */
	double a_f[3],	    /* output final model            axis   vector A */
	double h_f[3],	    /* output final model            horiz. vector H */
	double v_f[3],	    /* output final model            vert.  vector V */
	double o_f[3],	    /* output final model optical axis unit vector O */
	double r_f[3],	    /* output final model radial-distortion terms  R */
	double e_f[3],	    /* output final model entrance-pupil    terms  E */
	bool_t *parallel,   /* output if camera view and plane are parallel */
	bool_t *behind	    /*output if camera is behind the reflecting plane*/
     );

/******************************************************************************
********************************   CMOD_CAHVORE_REFLECT_COV   *****************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahvore_reflect_cov(
        CONST double s_i[21][21],    /* input initial covariance */
	CONST double n[3],	     /* input normal to the reflecting plane */
	double s_f[21][21]	     /* output final covariance */
     );
/******************************************************************************
********************************   CMOD_CAHVORE_ROT_COV   *********************
*******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rot_cov(
        CONST double r_i[3][3],/*input init orientation(rot)of camera ref pt*/
	CONST double s_i[21][21],/* input initial covariance */
	CONST double r_f[3][3],/*input final orientation(rot)of camera ref pt*/
	double s_f[21][21]     /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHVORE_ROTATE_COV   ******************
*******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rotate_cov(
        CONST double q_i[4],/*input initial orientation(quat)of camera ref pt*/
	CONST double s_i[21][21],/* input initial covariance */
	CONST double q_f[4],/* input final orientation(quat)of camera ref pt */
	double s_f[21][21]  /* output final covariance */
     );
/******************************************************************************
********************************   CMOD_CAHVORE_SCALE   ***********************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvore_scale(
        CONST double hscale,		/* input horizontal scale factor */
	CONST double vscale,		/* input vertical   scale factor */
	CONST double h1[3],		/* input  model horiz. vector H */
	CONST double v1[3],		/* input  model vert.  vector V */
	CONST double s1[21][21],	/* input  covariance matrix, or NULL */
	double h2[3],		/* output model horiz. vector H */
	double v2[3],		/* output model vert.  vector V */
	double s2[21][21]	/* output covariance matrix, or NULL */
     );

/******************************************************************************
********************************   CMOD_CAHVORE_SHIFT   ***********************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvore_shift(
        CONST double dx,		/* input horizontal shift */
	CONST double dy,		/* input vertical   shift */
	CONST double a1[3],		/* input  model axis   vector A */
	CONST double h1[3],		/* input  model horiz. vector H */
	CONST double v1[3],		/* input  model vert.  vector V */
	double h2[3],		/* output model horiz. vector H */
	double v2[3]		/* output model vert.  vector V */
    );

/******************************************************************************
********************************   CMOD_CAHVORE_TRANSFORM_COV   ***************
*******************************************************************************

    This function transform a CAHVORE model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvore_transform_cov(
	CONST double s_i[21][21],/* input initial covariance */
	CONST double r[3][3],/* input transformation matrix of camera ref pt */
	double s_f[21][21]   /* output final covariance */
     );

/******************************************************************************
********************************   CMOD_CAHVORE_WARP_FROM_CAHV   **************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    CAHVORE. */

void cmod_cahvore_warp_from_cahv(
        CONST double c1[3],/* input initial model center position vector   C */
	CONST double a1[3],/* input initial model orthog. axis unit vector A */
	CONST double h1[3],/* input initial model horizontal vector        H */
	CONST double v1[3],/* input initial model vertical vector          V */
	CONST double pos1[2],/* input 2D position from CAHV */
	CONST double rdist,
	CONST bool_t approx, /* input flag to use fast approximation */
	CONST int mtype2,    /* input final model type */
	CONST double mparm2, /* input model parameter */
	CONST double c2[3],  /* input final model center position vector   C */
	CONST double a2[3],  /* input final model orthog. axis unit vector A */
	CONST double h2[3],  /* input final model horizontal vector        H */
	CONST double v2[3],  /* input final model vertical vector          V */
	CONST double o2[3],  /* input final model optical axis unit vector O */
	CONST double r2[3],  /* input final model radial-distortion terms  R */
	CONST double e2[3],  /* input final model entrance-pupil    terms  E */
	double pos2[2]	     /* output 2D position for CAHVOR */
    );

/******************************************************************************
********************************   CMOD_CAHVORE_WARP_MODEL   ******************
*******************************************************************************

    This function warps a camera model so that it is purely linear. The
    parameters C and A will not change. The parameters O (identical to A)
    and R and E (all terms zero) will not be output. Note that image warping
    will be necessary in order to use the new models. */

void cmod_cahvore_warp_model(
        CONST int xdim,	     /* input number of columns */
	CONST int ydim,	     /* input number of rows */
	CONST int mtype,     /* input type of model */
	CONST double mparm,  /* input model parameter */
	CONST double c[3],   /* input model center vector C */
	CONST double a[3],   /* input model axis   vector A */
	CONST double h[3],   /* input model horiz. vector H */
	CONST double v[3],   /* input model vert.  vector V */
	CONST double o[3],   /* input model axis   vector O */
	CONST double r[3],   /* input model dist.  terms  R */
	CONST double e[3],   /* input model pupil  terms  E */
	CONST double limfov, /* input limit field of view: must be < Pi rad */
	CONST int minfov,    /* input if to minimize to common field of view */
	CONST int xdim2,     /* input number of columns of output model */
	CONST int ydim2,     /* input number of rows    of output model */
	double a2[3],	     /* output virtual model axis   vector A */
	double h2[3],	     /* output virtual model horiz. vector H */
	double v2[3],	     /* output virtual model vert.  vector V */
	double *hs,	     /* output horizontal scale factor */
	double *hc,	     /* output horizontal center */
	double *vs,	     /* output vertical scale factor */
	double *vc,	     /* output vertical center */
	double *theta	     /* output angle between axes */
     );

/******************************************************************************
********************************   CMOD_CAHVORE_WARP_MODELS   *****************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models with have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Since the output models will be linear, the parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvore_warp_models(
        CONST int xdim1,     /* input number of columns */
	CONST int ydim1,     /* input number of rows */
	CONST int mtype1,    /* input type of model */
	CONST double mparm1, /* input model parameter */
	CONST double c1[3],  /* input model 1 center vector C */
	CONST double a1[3],  /* input model 1 axis   vector A */
	CONST double h1[3],  /* input model 1 horiz. vector H */
	CONST double v1[3],  /* input model 1 vert.  vector V */
	CONST double o1[3],  /* input model 1 axis   vector O */
	CONST double r1[3],  /* input model 1 dist.  terms  R */
	CONST double e1[3],  /* input model 1 pupil  terms  E */
	CONST int xdim2,     /* input number of columns */
	CONST int ydim2,     /* input number of rows */
	CONST int mtype2,    /* input type of model */
	CONST double mparm2, /* input model parameter */
	CONST double c2[3],  /* input model 2 center vector C */
	CONST double a2[3],  /* input model 2 axis   vector A */
	CONST double h2[3],  /* input model 2 horiz. vector H */
	CONST double v2[3],  /* input model 2 vert.  vector V */
	CONST double o2[3],  /* input model 2 axis   vector O */
	CONST double r2[3],  /* input model 2 dist.  terms  R */
	CONST double e2[3],  /* input model 2 pupil  terms  E */
	CONST double limfov, /* input limit field of view: must be < Pi rad */
	CONST int minfov,    /* input if to minimize to common field of view */
	CONST int xdim,		/* input number of columns of output model */
	CONST int ydim,		/* input number of rows    of output model */
	double a[3],		/* output virtual model axis   vector A */
	double h[3],		/* output virtual model horiz. vector H */
	double v[3],		/* output virtual model vert.  vector V */
	double *hs,		/* output horizontal scale factor */
	double *hc,		/* output horizontal center */
	double *vs,		/* output vertical scale factor */
	double *vc,		/* output vertical center */
	double *theta		/* output angle between axes */
    );

/******************************************************************************
********************************   CMOD_CAHVORE_WARP_TO_CAHV   ****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVORE and warps it into an image coordinate modeled by
    CAHV. */

void cmod_cahvore_warp_to_cahv(
        CONST int mtype,   /* input type of model */
	CONST double mparm,/* input model parameter */
	CONST double c1[3],/* input initial model center position vector   C */
	CONST double a1[3],/* input initial model orthog. axis unit vector A */
	CONST double h1[3],/* input initial model horizontal vector        H */
	CONST double v1[3],/* input initial model vertical vector          V */
	CONST double o1[3],/* input initial model optical axis unit vector O */
	CONST double r1[3],/* input initial model radial-distortion terms  R */
	CONST double e1[3],/* input initial model entrance-pupil    terms  E */
	CONST double pos1[2],/* input 2D position from CAHVORE */
	CONST double rdist,  /* input radial distance to project */
	CONST bool_t approx, /* input flag to use fast approximation */
	CONST double c2[3],  /* input final model center position vector   C */
	CONST double a2[3],  /* input final model orthog. axis unit vector A */
	CONST double h2[3],  /* input final model horizontal vector        H */
	CONST double v2[3],  /* input final model vertical vector          V */
	double pos2[2]	     /* output 2D position for CAHV */
    );

/******************************************************************************
********************************   CMOD_CAHVORE_WARP_TO_CAHVORE   *************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVORE and warps it into an image coordinate modeled by
    a different CAHVORE. */

void cmod_cahvore_warp_to_cahvore(
        CONST int mtype,   /* input type of model */
	CONST double mparm,/* input model parameter */
	CONST double c1[3],/* input initial model center position vector   C */
	CONST double a1[3],/* input initial model orthog. axis unit vector A */
	CONST double h1[3],/* input initial model horizontal vector        H */
	CONST double v1[3],/* input initial model vertical vector          V */
	CONST double o1[3],/* input initial model optical axis unit vector O */
	CONST double r1[3],/* input initial model radial-distortion terms  R */
	CONST double e1[3],/* input initial model entrance-pupil    terms  E */
	CONST double pos1[2],/* input 2D position from CAHVORE */
	CONST double rdist,/* input radial distance to project */
	CONST bool_t approx,/* input flag to use fast approximation */
	CONST double c2[3], /* input final model center position vector   C */
	CONST double a2[3], /* input final model orthog. axis unit vector A */
	CONST double h2[3], /* input final model horizontal vector        H */
	CONST double v2[3], /* input final model vertical vector          V */
	CONST double o2[3], /* input final model optical axis unit vector O */
	CONST double r2[3], /* input final model radial-distortion terms  R */
	CONST double e2[3], /* input final model entrance-pupil    terms  E */
	double pos2[2]	    /* output 2D position for CAHV */
    );

/******************************************************************************
********************************   CMOD_CAHVORE_WRITE   ***********************
*******************************************************************************

    This function writes a CAHVORE model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

int  cmod_cahvore_write(
        CONST char *filename,	/* input filename */
	CONST char *comment,	/* input one-line comment to record in file */
	CONST int xdim,		/* input number of columns */
	CONST int ydim,		/* input number of rows */
	CONST int mtype,	/* input type of model */
	CONST double mparm,	/* input model parameter */
	CONST double c[3],	/* input model center vector C */
	CONST double a[3],	/* input model axis   vector A */
	CONST double h[3],	/* input model horiz. vector H */
	CONST double v[3],	/* input model vert.  vector V */
	CONST double o[3],	/* input model optical axis unit vector O */
	CONST double r[3],	/* input model radial-distortion terms  R */
	CONST double e[3],	/* input model entrance-pupil    terms  E */
	CONST double s[21][21],	/* input covariance of CAHVORE */
	CONST double hs,	/* input horizontal scale factor */
	CONST double hc,	/* input horizontal center */
	CONST double vs,	/* input vertical scale factor */
	CONST double vc,	/* input vertical center */
	CONST double theta,	/* input angle between axes */
	CONST double s_int[5][5]/* input covariance matrix */
    );

#else /* _NO_PROTO */

void cmod_cahv_2d_to_3d();
void cmod_cahv_3d_to_2d();
void cmod_cahv_3d_to_2d_ray();
void cmod_cahv_internal(); 
void cmod_cahv_move();
void cmod_cahv_pose();
void cmod_cahv_posture();
int  cmod_cahv_read();
int  cmod_cahv_read2();
void cmod_cahv_reflect();
void cmod_cahv_reflect_cov();
void cmod_cahv_rot_cov( )
void cmod_cahv_rotate_cov();
void cmod_cahv_scale();
void cmod_cahv_shift();
void cmod_cahv_transform_cov();
void cmod_cahv_warp_models();
void cmod_cahv_warp_to_cahv(); 
int  cmod_cahv_write(); 
int  cmod_cahv_write2();
 
void cmod_cahv_warp_to_cahvor();

void cmod_cahvor_2d_to_3d();
void cmod_cahvor_3d_to_2d();
void cmod_cahvor_3d_to_2d_point(); 
void cmod_cahvor_move(); 
int  cmod_cahvor_read(); 
int  cmod_cahvor_read2(); 
void cmod_cahvor_reflect();
void cmod_cahvor_reflect_cov();
void cmod_cahvor_rot_cov();
void cmod_cahvor_rotate_cov();
void cmod_cahvor_scale();
void cmod_cahvor_shift();
void cmod_cahvor_transform_cov();
void cmod_cahvor_warp_from_cahv();
void cmod_cahvor_warp_model();
void cmod_cahvor_warp_models();
void cmod_cahvor_warp_models_nodims();
void cmod_cahvor_warp_to_cahv();
void cmod_cahvor_warp_to_cahvor();
int  cmod_cahvor_write();
int  cmod_cahvor_write2();

void cmod_cahvore_2d_to_3d();
static void cmod_cahvore_2d_to_3d_general();
void cmod_cahvore_3d_to_2d();
static void cmod_cahvore_3d_to_2d_general();
void cmod_cahvore_3d_to_2d_point();
void cmod_cahvore_align_models);
void cmod_cahvore_move();
int  cmod_cahvore_read();
void cmod_cahvore_reflect();
void cmod_cahvore_reflect_cov();
void cmod_cahvore_rot_cov();
void cmod_cahvore_rotate_cov();
void cmod_cahvore_scale();
void cmod_cahvore_shift();
void cmod_cahvore_transform_cov();
void cmod_cahvore_warp_from_cahv();
void cmod_cahvore_warp_model);
void cmod_cahvore_warp_models();
void cmod_cahvore_warp_to_cahv();
void cmod_cahvore_warp_to_cahvore();
int  cmod_cahvore_write();

#endif /* _NO_PROTO */

#ifdef __cplusplus
}	/* end extern "C" */
#endif

#endif /* _CAHVOR_H */

