/************************************************************************/
/* Gruen correlation algorithm.  See gruen.c file for meanings of	*/
/* all the arguments.							*/
/* gruen() is the original API; gruen2() has several new enhancements.  */
/************************************************************************/

#ifndef _GRUEN_H
#define _GRUEN_H

#include "xvmaininc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Main gruen algorithm */

#ifdef _NO_PROTO
int gruen();
int gruen2();
#else
int gruen(double *left, int nlw, int nsw, int max_left_area,
	   double *right, int nlw2, int nsw2, int max_right_area,
	   double *correl,
	   double *line_offset, double *samp_offset,
	   double line_coef[3], double samp_coef[3],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits, double *quality, int mode);

int gruen2(double *left, int nlw, int nsw, int max_left_area,
	   double *right, int nlw2, int nsw2, int max_right_area,
	   double *correl,
	   double *line_offset, double *samp_offset,
	   double line_coef[4], double samp_coef[4],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits, double *quality, int mode,
	   double ftol, int inv_flag);
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _GRUEN_H */

