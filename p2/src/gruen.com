$!****************************************************************************
$!
$! Build proc for MIPL module gruen
$! VPACK Version 1.9, Saturday, October 07, 2006, 12:48:56
$!
$! Execute by entering:		$ @gruen
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
$ write sys$output "*** module gruen ***"
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
$ write sys$output "Invalid argument given to gruen.com file -- ", primary
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
$   if F$SEARCH("gruen.imake") .nes. ""
$   then
$      vimake gruen
$      purge gruen.bld
$   else
$      if F$SEARCH("gruen.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gruen
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gruen.bld "STD"
$   else
$      @gruen.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gruen.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gruen.com -mixed -
	-s gruen.c -
	-i gruen.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gruen.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* Gruen correlation algorithm.						*/
/************************************************************************/
/* The differences between gruen() and gruen2() are minor.  gruen() is
 * maintained for backwards compatibility; apps should use gruen2().
 * gruen() takes 3-element arrays for line_coef and samp_coef, while
 * gruen2() takes 4-element arrays.  Also, gruen2() adds the ftol parameter.
 *
 * Note that the extra parameter (an xy term which allows for perspective
 * transforms, not just affines) is NOT supported in the annealing modes
 * at this time.
 */
/*
   ARGUMENTS:

left = left image area template,[max_left_area][max_left_area]		double

nlw = # lines left area. odd number					int

nsw = # samples left area. odd number					int

max_left_area = inner physical dimension of "left" array		int

right = right image search area, [max_right_area][max_right_area]	double

nlw2 = # lines right area. odd number					int

nsw2 = # samples right area. odd number					int

max_right_area = inner physical dimension of "right" and "correl" arrays int

correl = correlation matrix, [max_right_area][max_right_area]		double
         Returned. Only used in mode=0.

line_offset = line shift to correct initial registration, returned	double
              Negative means shift upwards.

samp_offset = sample shift to correct initial registration, returned	double
              Negative means shift to the left.

line_coef = line equation coefficients,[4], returned			double
            Mapping polynomial from template to search area.
            rightline=line_coef[0]*leftsamp+line_coef[1]*leftline+line_coef[2]
			+ line_coef[3]*leftsamp*leftline
            On entry contains best estimate of line polynomial solution.
            Try: 0., 1., 0., 0.
            For gruen(), this is a 3-element array.

samp_coef = sample equation coefficients,[4], returned			double
            Mapping polynomial from template to search area.
            rightsamp=samp_coef[0]*leftsamp+samp_coef[1]*leftline+samp_coef[2]
			+ samp_coef[3]*leftsamp*leftline
            On entry contains best estimate of sample polynomial solution.
            Try: 1., 0., 0., 0.
            For gruen(), this is a 3-element array.

line_coef_limits = initial boundary conditions, [3][2]			double
                   Only used in modes 1 and 4.
                   [][0] is negative limit
                   [][1] is positive limit
                   These limit the possible range of mapping polynomial values.

samp_coef_limits = initial boundary conditions, [3][2]			double
                   Only used in modes 1 and 4.
                   [][0] is negative limit
                   [][1] is positive limit
                   These limit the possible range of mapping polynomial values.

line_temp = line equation coefficients temperature,[3]			double
            Only used in modes 1 and 4.
            The initial guessing range for solutions of polynomial values.
            Try: (line_coef_limits[][1]-line_coef_limits[][0])/12.

samp_temp = sample equation coefficients temperature,[3]		double
            Only used in modes 1 and 4.
            The initial guessing range for solutions of polynomial values.
            Try: (samp_coef_limits[][1]-samp_coef_limits[][0])/12.

percent = Percentage of the template to use in correlations. If percent=100
          then one correlation is computed with the entire area, otherwise
          five correlations are computed, beginning with the entire area. 
          The other four correlations utilize only:
          "percent" lines at the bottom,
          "percent" lines at the top,
          "percent" columns at the right,
          "percent" columns at the left.				double
          The purpose of this is to permit the exclusion of border points
          which for some reason are incompatible in intensity with the
          data being correlated. 
          For example if percent=80 then one of the correlation attempts
          will only use nlw*0.80 lines either at the top or bottom of the
          window.

limits = The number of iterations permitted in the annealing scheme.	int
         This should be several thousand at the least.

quality = correlation quality. Zero to one (if inv_flag is true), or -1 to 1
          (if inv_flag is false).  1=perfect.			      returned

mode = is the type of correlation desired. An integer from 0 to 8.	int
       (all modes are sub-pixel).
	If mode is negative, linear (mode 0) is performed before the
	otherwise specified -mode.  This avoids getting too many extra modes
	to support linear followed by all the amoebas.
        mode=0 linear least squares fit. 
             Fastest, on integral pixel borders. Translation only.
             Accuracy limited to 1/10 pixel.
        mode=1 annealing fit. Very slow. Able to search entire area.
             Accuracy adjustable but around 1/30 pixel.
             Handles rotation,skew,flip,scale,offset.
        mode=2 amoeba fit. deterministic simplex search method.
             Accuracy as good as it is possible to achieve(1/100 pixel).
             Initial estimate must be within 2 pixels or else the
             resulting tiepoint will be in error.
             Speed intermediate between modes 0 and 1.
             Handles rotation,skew,flip,scale,offset.
        mode=3 linear (mode=0) followed by amoeba (mode=2) fit
             Linear fit locates minimum and amoeba fit refines it.
             Handles rotation,skew,flip,scale,offset.
        mode=4 annealing (mode=1) followed by amoeba (mode=2) fit.
             Annealing locates minimum and amoeba refines it.
             Handles rotation,skew,flip,scale,offset.
        mode=5 amoeba fit with 2 degrees of freedom.  Like mode=2 except
             only two degrees of freedom (x/y translation) are allowed.
             line_coef[0] and [1], and samp_coef[0] and [1] are returned
             unchanged.
        mode=6 linear (mode=0) followed by amoeba w/2 degrees of freedom
             (mode=5) fit.  Similar to mode=3 except the amoeba step is
             limited to 2 degrees of freedom (x/y translation).  The other
             parameters are fixed by mode=0:  line_coef[0]=0, line_coef[1]=1,
             samp_coef[0]=1, samp_coef[1]=0.
        mode=7 amoeba fit with 8 degrees of freedom.  Like mode 2 but adds
             two additional degrees of freedom that handle perspective or
             trapezoidal (non-affine) transforms.  Only available in gruen2().
             The equations are thus:
             y' = ax + by + c + gxy
             x' = dx + ey + f + hxy
             where (a,b,c,g) maps to line_coef and (d,e,f,h) maps to samp_coef.
        mode=8 amoeba fit with 4 degrees of freedom.  This is a good model
             for in-situ cameras looking out at a plane.  Allows translation
             only in y, while translation, trapezoid, and shear are allowed
             in x.  So in the mode=7 equation, a==0, b==1, g==0, d==1, while
             c, e, f, h are allowed to vary.
        mode=9 amoeba fit with 5 degrees of freedom.  This is a better model
             for in-situ cameras looking out at a plane with epipolar-aligned
             cameras.  Allows translation only in y, while translation,
             trapezoid, scale, and shear are allowed in x.  So in the mode=7
             equation, a==0, b==1, g==0, while c, d, e, f, h are allowed to
             vary.
        Note: The annealing/amoeba-2 DOF mode (mode=1 followed by mode=5)
             is not implemented at this time.  Annealing will generate 6
             degrees of freedom, which are then ignored by amoeba-2 DOF.
             This can easily be implemented if necessary, however.
        Note: Combinations of mode=7 and mode=8 with linear (analogous to
             modes 3 and 6) are not implemented at this time.  They could
             easily be added if needed, however.

ftol = tolerance value for amoeba.  This tells amoeba when to stop.  When
       the difference in correlation quality obtained by varying the
       parameters becomes less than this value, it is assumed that the
       minimum as been found.  This value relates directly (but not simply)
       to the accuracy of the obtained result, and also has a tremendous
       impact on execution speed.  Smaller values mean more precision in
       the results, at the cost of increased run time.  Historically, this
       value has been .000001 (the value still used by gruen()) but for
       most cases, a value around .001 or slightly higher will be sufficient.
       input double.

inv_flag = flag indicating whether to accept inverse correlations or not.
           Setting this to true (non-0), which is the default for gruen(),
           and the traditional value) means that inverse correlations are
	   allowed as well as normal correlations, and the quality value
	   will always be non-negative.  Qualities < 0 get mapped to the
	   negative of the quality (thus -.98 is the same as +.98).  This
	   allows e.g. different spectral bands to be correlated, where
	   the features are the same but intensities get reversed.
	   Setting it to false (0) means that inverse correlations are NOT
	   allowed, and the returned quality value will range from -1 to +1.

Function return = return status, 0=OK, 1=unable to obtain a correlation. int
*/

/************************************************************************/

#include "gruen.h"
#include "amoeba.h"

#include <math.h>

/* Defines for accessing arrays */

#define left(row,col) (params->left_p[(row)*params->max_left_area + (col)])
#define right(row,col) (params->right_p[(row)*params->max_right_area + (col)])
#define correl(row,col) (params->correl_p[(row)*params->max_right_area + (col)])

/* Parameter structure for "cost" function. */

struct CostParams {
	double coef_limits[8][2];
	double *left_p;
	int nlw;
	int nsw;
	int max_left_area;
	double *right_p;
	int nlw2;
	int nsw2;
	int max_right_area;
	double *correl_p;	/* used only by search_area */
	double sumx[10];
	double left_sum[10];
	double percent;
	int inv_flag;
	int ind;		/* returned */
};

/* Function prototypes for this module */

static double cost(double p[], int ndim, void *func_args);
static void search_area(struct CostParams *params, double answer[6],
	double *quality, int inv_flag);
static void metropolis(struct CostParams *params, double answer[6],
	double range[6], int numten, int limits, int norm, double *quality);

/************************************************************************/
/* The gruen code itself */
/************************************************************************/

int gruen(double *left_p, int nlw, int nsw, int max_left_area,
	   double *right_p, int nlw2, int nsw2, int max_right_area,
	   double *correl_p,
	   double *line_offset, double *samp_offset,
	   double line_coef[3], double samp_coef[3],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits,
	   double *quality,
	   int mode)
{
    double line_coef4[4], samp_coef4[4];
    int i, status;
 
    if ((mode < -6) || (mode > 6)) {  /* modes 7,8,9 not supported in gruen() */
        return 1;
    }

    /* Transfer to 4-element arrays... */

    for (i=0; i<3; i++) {
	line_coef4[i] = line_coef[i];
	samp_coef4[i] = samp_coef[i];
    }
    line_coef4[3] = 0.0;
    samp_coef4[3] = 0.0;

    status = gruen2(left_p, nlw, nsw, max_left_area, right_p, nlw2, nsw2,
		max_right_area, correl_p, line_offset, samp_offset,
		line_coef4, samp_coef4, line_coef_limits, samp_coef_limits,
		line_temp, samp_temp, percent, limits,
		quality, mode, 0.00001, 1);

    /* and back again... */
    for (i=0; i<3; i++) {
	line_coef[i] = line_coef4[i];
	samp_coef[i] = samp_coef4[i];
    }
    return status;

}

int gruen2(double *left_p, int nlw, int nsw, int max_left_area,
	   double *right_p, int nlw2, int nsw2, int max_right_area,
	   double *correl_p,
	   double *line_offset, double *samp_offset,
	   double line_coef[4], double samp_coef[4],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits,
	   double *quality,
	   int mode, double ftol,
	   int inv_flag)
{

    double range[8],answer[8];
    double P[9][8],Y[9],scr[8];
    double sumx2,left_dn,rn;
    int numten,norm,i,j,ITER,kl,ks,num_areas,k,kll,kss;
    int num_DOF;
    double min;
    int ilo;

    struct CostParams params_struct;
    struct CostParams *params = &params_struct;

    params->left_p = left_p;
    params->nlw = nlw;
    params->nsw = nsw;
    params->max_left_area = max_left_area;
    params->right_p = right_p;
    params->nlw2 = nlw2;
    params->nsw2 = nsw2;
    params->correl_p = correl_p;
    params->max_right_area = max_right_area;
    params->percent = percent;
    params->inv_flag = inv_flag;

    params->ind = 0;

    /* compute sub area limits if percent < 100 */
    num_areas=1;
    if (params->percent < 99.99) {
	num_areas=5;
	kl=(params->nlw*(params->percent/100.)) + .5;
	if (kl > params->nlw) kl=params->nlw;
        if (kl < 1) kl=1;
        ks=(params->nsw*(params->percent/100.)) + .5;
        if (ks > nlw) ks=nlw;
        if (ks < 1) ks=1;
        kll=nlw-kl-1;
        kss=nsw-ks-1;
    }

    /* loop on all sub areas */
    for (k=0; k < num_areas; k++) {

        /* compute area of sub area */
	if (k == 0) rn=params->nlw*params->nsw;
	if (k == 1) rn=kl*params->nsw;
	if (k == 2) rn=kl*params->nsw;
	if (k == 3) rn=ks*params->nlw;
	if (k == 4) rn=ks*params->nlw;

        /* pre-compute left (template) sums for COST function */
	sumx2=0.0;
	params->sumx[k]=0.0;
	for (j=0; j < nlw; j++) {
	    for (i=0; i < nsw; i++) {
                if (k == 1) {
                    if (j <= kll) continue;
                }
                if (k == 2) {
                    if (j >= kl) continue;
                }
                if (k == 3) {
                    if (i <= kss) continue;
                }
                if (k == 4) {
                    if (i >= ks) continue;
                }
                left_dn=left(j,i);
                params->sumx[k] += left_dn;
                sumx2 += left_dn*left_dn;
             }
	}
	params->left_sum[k]=sumx2-params->sumx[k]*params->sumx[k]/rn;
    }

    if ((mode == 0) || (mode == 3) || (mode == 6) || (mode < 0)) {

        /* Use conventional linear correlation scheme only */

        search_area(params, answer, quality, inv_flag);

	if (mode < 0)
	    mode = -mode;
    }

    if ((mode == 1) || (mode == 4)) {

        /* use Annealing correlation */

        answer[0]=line_coef[0];
        answer[1]=line_coef[1];
        answer[2]=line_coef[2];
        answer[3]=samp_coef[0];
        answer[4]=samp_coef[1];
        answer[5]=samp_coef[2];
        range[0]=line_temp[0];
        range[1]=line_temp[1];
        range[2]=line_temp[2];
        range[3]=samp_temp[0];
        range[4]=samp_temp[1];
        range[5]=samp_temp[2];
        params->coef_limits[0][0]=line_coef_limits[0][0];
        params->coef_limits[0][1]=line_coef_limits[0][1];
        params->coef_limits[1][0]=line_coef_limits[1][0];
        params->coef_limits[1][1]=line_coef_limits[1][1];
        params->coef_limits[2][0]=line_coef_limits[2][0];
        params->coef_limits[2][1]=line_coef_limits[2][1];
        params->coef_limits[3][0]=samp_coef_limits[0][0];
        params->coef_limits[3][1]=samp_coef_limits[0][1];
        params->coef_limits[4][0]=samp_coef_limits[1][0];
        params->coef_limits[4][1]=samp_coef_limits[1][1];
        params->coef_limits[5][0]=samp_coef_limits[2][0];
        params->coef_limits[5][1]=samp_coef_limits[2][1];

        /* set iteration control */
        numten=limits/2;
        norm=numten/7;

        metropolis(params, answer, range,numten,limits,norm,quality);
    }

    if (mode == 2) {

        /* use Amoeba correlation only, so set it up (modes 3-6 already set) */

        answer[0]=line_coef[0];
        answer[1]=line_coef[1];
        answer[2]=line_coef[2];
        answer[3]=samp_coef[0];
        answer[4]=samp_coef[1];
        answer[5]=samp_coef[2];
    }

    /* Modes 5/6 (2-DOF amoeba) use answer[0-1] only. */

    num_DOF = 6;

    if (mode == 5) {			/* amoeba only */
	answer[0] = line_coef[2];
	answer[1] = samp_coef[2];
	num_DOF = 2;
    }
    if (mode == 6) {		/* must shift results of mode=0 down */
	answer[0] = answer[2];
	answer[1] = answer[5];
	num_DOF = 2;
    }

    if (mode == 7) {			/* amoeba only, 8-DOF */
        answer[0]=line_coef[0];
        answer[1]=line_coef[1];
        answer[2]=line_coef[2];
        answer[3]=samp_coef[0];
        answer[4]=samp_coef[1];
        answer[5]=samp_coef[2];
	answer[6]=line_coef[3];
	answer[7]=samp_coef[3];
	num_DOF = 8;
    }

    if (mode == 8) {
	answer[0] = line_coef[2];
	answer[1] = samp_coef[2];
	answer[2] = samp_coef[1];
	answer[3] = samp_coef[3];
	num_DOF = 4;
    }

    if (mode == 9) {
	answer[0] = line_coef[2];
	answer[1] = samp_coef[2];
	answer[2] = samp_coef[0];
	answer[3] = samp_coef[1];
	answer[4] = samp_coef[3];
	num_DOF = 5;
    }

    if (mode >= 2 && mode <= 9) {

	/* Amoeba correlation */
	/* amoeba is used rather than amoeba2 because the "length	*/
	/* scales" are different for each parameter.			*/
	/* Only the first 4 lines are used for DOF=2.  Note that their	*/
	/* meaning changes in this case; coef_limits[0][*] is the same	*/
	/* as coef_limits[2][*] for DOF=6, and [1] is the same as [5].	*/
	/* Since all are set the same here, it doesn't matter, but	*/
	/* watch out if you change these.				*/

        params->coef_limits[0][0]= -1000.;		/* [2][0] for DOF=2 */
        params->coef_limits[0][1]=  1000.;		/* [2][1] for DOF=2 */
        params->coef_limits[1][0]= -1000.;		/* [5][0] for DOF=2 */
        params->coef_limits[1][1]=  1000.;		/* [5][1] for DOF=2 */
        params->coef_limits[2][0]= -1000.;
        params->coef_limits[2][1]=  1000.;
        params->coef_limits[3][0]= -1000.;
        params->coef_limits[3][1]=  1000.;
        params->coef_limits[4][0]= -1000.;
        params->coef_limits[4][1]=  1000.;
        params->coef_limits[5][0]= -1000.;
        params->coef_limits[5][1]=  1000.;
	params->coef_limits[6][0]= -1000.;		/* only DOF=8*/
	params->coef_limits[6][1]=  1000.;		/* only DOF=8*/
	params->coef_limits[7][0]= -1000.;		/* only DOF=8*/
	params->coef_limits[7][1]=  1000.;		/* only DOF=8*/

        /* precompute the matrix P of DOF+1 simplex starting points */
        /* and array Y of corresponding errors */

        for (j=0; j < num_DOF+1; j++) {
            for (i=0; i < num_DOF; i++) {
                P[j][i]=answer[i];
            }
	    if (num_DOF == 2) {
		if (j == 0) P[j][j]=P[j][j]-0.3;	/* was for j==2 */
		if (j == 1) P[j][j]=P[j][j]-0.3;	/* was for j==5 */
	    }
	    else if (num_DOF == 4) {
		if (j == 0) P[j][j]=P[j][j]-0.3;	/* was for j==2 */
		if (j == 1) P[j][j]=P[j][j]-0.3;	/* was for j==5 */
		if (j == 2) P[j][j]=P[j][j]-.07;	/* was for j==4 */
		if (j == 3) P[j][j]=P[j][j]-.01;	/* was for j==7 */
	    }
	    else if (num_DOF == 5) {
		if (j == 0) P[j][j]=P[j][j]-0.3;	/* was for j==2 */
		if (j == 1) P[j][j]=P[j][j]-0.3;	/* was for j==5 */
		if (j == 2) P[j][j]=P[j][j]-.07;	/* was for j==3 */
		if (j == 3) P[j][j]=P[j][j]-.07;	/* was for j==4 */
		if (j == 4) P[j][j]=P[j][j]-.01;	/* was for j==7 */
	    }
	    else {
        	if (j == 0) P[j][j]=P[j][j]-.07;
        	if (j == 1) P[j][j]=P[j][j]-.07;
        	if (j == 2) P[j][j]=P[j][j]-0.3;
        	if (j == 3) P[j][j]=P[j][j]-.07;
        	if (j == 4) P[j][j]=P[j][j]-.07;
        	if (j == 5) P[j][j]=P[j][j]-0.3;
		if (j == 6) P[j][j]=P[j][j]-.01;
		if (j == 7) P[j][j]=P[j][j]-.01;
	    }
            for (i=0; i < num_DOF; i++) {
                scr[i]=P[j][i];
            }
	    Y[j] = cost(scr, num_DOF, params);
        }

        /* solve for the minimum using the simplex method */

        amoeba((double *)P, Y, num_DOF, 8, ftol, 5000, &ITER, cost, params);

	/* We used to pick the first answer arbitrarily, since all are	*/
	/* within FTOL.  However, with larger FTOL values the spread	*/
	/* gets larger.  So, we might as well pick the best of all the	*/
	/* possible answers.  It's cheap, and marginally improves the	*/
	/* results.							*/

	min = Y[0];
	ilo=0;
	for (i=1; i<num_DOF+1; i++) {
	   if (Y[i] < min)
	      ilo = i;
	}

        *quality = 1.0 - (Y[ilo] - 1.0);
	for (i=0; i < num_DOF; i++) {
            answer[i]=P[ilo][i];
        }

	if (mode == 5 || mode == 6) {	/* Reshuffle the answer to match 0-5 */
	    answer[2] = answer[0];	/* shift over good answers */
	    answer[5] = answer[1];
	    answer[0] = line_coef[0];	/* and fill in the rest */
	    answer[1] = line_coef[1];
	    answer[3] = samp_coef[0];
	    answer[4] = samp_coef[1];
	}
	if (mode == 8) {		/* Reshuffle the answer to match 0-5 */
	    answer[5] = answer[1];	/* shift over good answers */
	    answer[4] = answer[2];
	    answer[7] = answer[3];
	    answer[2] = answer[0];
	    answer[0] = line_coef[0];	/* and fill in the rest */
	    answer[1] = line_coef[1];
	    answer[3] = samp_coef[0];
	    answer[6] = line_coef[3];
	}
	if (mode == 9) {		/* Reshuffle the answer to match 0-5 */
	    answer[5] = answer[1];	/* shift over good answers */
	    answer[7] = answer[4];
	    answer[4] = answer[3];
	    answer[3] = answer[2];
	    answer[2] = answer[0];
	    answer[0] = line_coef[0];	/* and fill in the rest */
	    answer[1] = line_coef[1];
	    answer[6] = line_coef[3];
	}
    }

    if ((mode < 0) || (mode > 9)) {		/* bad mode */
        return 1;
    }

    /* restore & return new polynomial coefficients */
    line_coef[0]=answer[0];
    line_coef[1]=answer[1];
    line_coef[2]=answer[2];
    samp_coef[0]=answer[3];
    samp_coef[1]=answer[4];
    samp_coef[2]=answer[5];
    line_coef[3]=answer[6];
    samp_coef[3]=answer[7];

    /* return offset from initial estimate, Note other polynomial terms are
       zero because multiply by x=y=0 at template origin */
    *line_offset=line_coef[2];
    *samp_offset=samp_coef[2];
 
    return params->ind;
}


/************************************************************************/
/* Objective function to be minimized.					*/
/* Polynomial is of the form:						*/
/* rightline = answer[0]*leftsamp + answer[1]*leftline + answer[2]	*/
/*           + answer[6]*leftsamp*leftline				*/
/* rightsamp = answer[3]*leftsamp + answer[4]*leftline + answer[5]	*/
/*           + answer[7]*leftsamp*leftline				*/
/*									*/
/* Note:  1.0 is added to the result before returning, to avoid some	*/
/* instabilities in the amoeba algorithm when the values are very close	*/
/* to 0.  Subtract 1.0 from the result to get a correlation quality.	*/
/* Also, the result is inverted so lower is better (since amoeba	*/
/* minimizes).  So, quality = 1.0 - (result - 1.0);			*/
/************************************************************************/

static double cost(double ans[], int ndim, void *func_args)
{
    struct CostParams *params = (struct CostParams *)func_args;

    int j,i,m,n,kl,ks,num_areas,k,kll,kss;
    double left_center_line,left_center_samp;
    double right_center_line,right_center_samp;
    double r1,r2,x,y,right_line,right_samp,wl,wr,wt;
    double top,bot,rnlw2,rnsw2,best_r2;
    double sumy,sumy2,sumxy,rn,right_dn,denom,numer;
    double bad_return;
    double *answer, ans_2[8];

    bad_return = (1.0 - 0.0) + 1.0; /* worst return value possible */

    /* check if solution limits are violated */
    for (i=0; i < ndim; i++) {
        if ((ans[i] < params->coef_limits[i][0]) || 
	   (ans[i] > params->coef_limits[i][1])) {
            params->ind=1;
            return bad_return;
        }
    }

    if (ndim == 2) {	/* 2 degrees of freedom, shuffle answer around */
	ans_2[0] = 0.0;
	ans_2[1] = 1.0;
	ans_2[2] = ans[0];
	ans_2[3] = 1.0;
	ans_2[4] = 0.0;
	ans_2[5] = ans[1];
	ans_2[6] = 0.0;
	ans_2[7] = 0.0;
	answer = ans_2;
    }
    else if (ndim == 4) {     /* 4 degrees of freedom, shuffle answer around */
	ans_2[0] = 0.0;
	ans_2[1] = 1.0;
	ans_2[2] = ans[0];
	ans_2[3] = 1.0;
	ans_2[4] = ans[2];
	ans_2[5] = ans[1];
	ans_2[6] = 0.0;
	ans_2[7] = ans[3];
	answer = ans_2;
    }
    else if (ndim == 5) {     /* 5 degrees of freedom, shuffle answer around */
	ans_2[0] = 0.0;
	ans_2[1] = 1.0;
	ans_2[2] = ans[0];
	ans_2[3] = ans[2];
	ans_2[4] = ans[3];
	ans_2[5] = ans[1];
	ans_2[6] = 0.0;
	ans_2[7] = ans[4];
	answer = ans_2;
    }
    else if (ndim == 6) {	/* 6 traditional degrees of freedom */
	ans_2[0] = ans[0];	/* (no xy terms) */
	ans_2[1] = ans[1];
	ans_2[2] = ans[2];
	ans_2[3] = ans[3];
	ans_2[4] = ans[4];
	ans_2[5] = ans[5];
	ans_2[6] = 0.0;
	ans_2[7] = 0.0;
	answer = ans_2;
    }
    else			/* use the full answer */
	answer = ans;

    /* constants */
    left_center_line=(params->nlw-1)/2.0;
    left_center_samp=(params->nsw-1)/2.0;
    right_center_line=(params->nlw2-1)/2.0;
    right_center_samp=(params->nsw2-1)/2.0;

    /* check if corners of left area fall within right area */
    rnlw2=params->nlw2-1;
    rnsw2=params->nsw2-1;
    y= -left_center_line;
    x= -left_center_samp;
    right_line = x*answer[0] + y*answer[1] + answer[2] + right_center_line +
		x*y*answer[6];
    right_samp = x*answer[3] + y*answer[4] + answer[5] + right_center_samp +
		x*y*answer[7];
    if ((right_line < 0.0) || (right_line > rnlw2) ||
       (right_samp < 0.0) || (right_samp > rnsw2)) {
	params->ind=1;
        return bad_return;
    }
    y= -left_center_line;
    x= (params->nsw-1)-left_center_samp;
    right_line = x*answer[0] + y*answer[1] + answer[2] + right_center_line +
		x*y*answer[6];
    right_samp = x*answer[3] + y*answer[4] + answer[5] + right_center_samp +
		x*y*answer[7];
    if ((right_line < 0.0) || (right_line > rnlw2) ||
       (right_samp < 0.0) || (right_samp > rnsw2)) {
        params->ind=1;
        return bad_return;
    }
    y= (params->nlw-1)-left_center_line;
    x= -left_center_samp;
    right_line = x*answer[0] + y*answer[1] + answer[2] + right_center_line +
		x*y*answer[6];
    right_samp = x*answer[3] + y*answer[4] + answer[5] + right_center_samp +
		x*y*answer[7];
    if ((right_line < 0.0) || (right_line > rnlw2) ||
       (right_samp < 0.0) || (right_samp > rnsw2)) {
	params->ind=1;
	return bad_return;
    }
    y=(params->nlw-1)-left_center_line;
    x=(params->nsw-1)-left_center_samp;
    right_line = x*answer[0] + y*answer[1] + answer[2] + right_center_line +
		x*y*answer[6];
    right_samp = x*answer[3] + y*answer[4] + answer[5] + right_center_samp +
		x*y*answer[7];
    if ((right_line < 0.0) || (right_line > rnlw2) ||
       (right_samp < 0.0) || (right_samp > rnsw2)) {
        params->ind=1;
        return bad_return;
    }

    /* compute sub area limits if percent < 100 */
    num_areas=1;
    if (params->percent < 99.99) {
        num_areas=5;
        kl=(params->nlw*(params->percent/100.)) + .5;
        if (kl > params->nlw) kl=params->nlw;
        if (kl < 1) kl=1;
        ks=(params->nsw*(params->percent/100.)) + .5;
        if (ks > params->nlw) ks=params->nlw;
        if (ks < 1) ks=1;
        kll=params->nlw-kl-1;
        kss=params->nsw-ks-1;
    }

    /* loop on all sub areas */
    best_r2=-1.0;
    for (k=0; k < num_areas; k++) {

	/* compute area of sub area */
        if (k == 0) rn=params->nlw * params->nsw;
        if (k == 1) rn=kl * params->nsw;
        if (k == 2) rn=kl * params->nsw;
        if (k == 3) rn=ks * params->nlw;
        if (k == 4) rn=ks * params->nlw;

	/* compute coefficient of determination r2 */
        sumy=0.0;
        sumy2=0.0;
        sumxy=0.0;
        for (j=0; j < params->nlw; j++) {
            y=j-left_center_line;
            /* store redundant terms */
	    r1=y*answer[1]+answer[2]+right_center_line;
	    r2=y*answer[4]+answer[5]+right_center_samp;
            for (i=0; i < params->nsw; i++) {
                if (k == 1) {
                    if (j <= kll) continue;
                }
                if (k == 2) {
                    if (j >= kl) continue;
                }
                if (k == 3) {
                    if (i <= kss) continue;
                }
                if (k == 4) {
                    if (i >= ks) continue;
                }
                x=i-left_center_samp;
                right_line=x*answer[0]+x*y*answer[6]+r1;
                right_samp=x*answer[3]+x*y*answer[7]+r2;

		/* Bilinear interpolation */

                m=right_samp;
                n=right_line;
                wl=right_samp-m;
                wr=1.0-wl;
                top=wl*right(n,m+1)+wr*right(n,m); /* bilinear interpolation */
                bot=wl*right(n+1,m+1)+wr*right(n+1,m);
                wt=right_line-n;
                right_dn=bot*wt+top*(1.0-wt); /* got right DN value by interpolation */

                sumy += right_dn; /* compute sums for least squares fit */
                sumy2 += right_dn*right_dn;
                sumxy += right_dn*left(j,i);
            }
        }
        denom=params->left_sum[k]*(sumy2-sumy*sumy/rn);
        if (denom == 0.0) {
            params->ind=1;
            return bad_return;
        }
	numer = sumxy-params->sumx[k]*sumy/rn;
        r2=(numer*numer)/denom;
	if (numer < 0 && !params->inv_flag)  // Negative correlation... allowed?
	    r2 = - r2;			// nope... so q _is_ negative
        if (r2 > best_r2) best_r2=r2;
    }/* end of areas loop */

    params->ind = 0;
    return (1.0 - best_r2) + 1.0;	/* invert so we can minimize */
}

/************************************************************************/
/* Routine to perform conventional correlation only.			*/
/* Correl is filled with the matrix of coefficient of determination values. */
/* Then the peak value is interpolated to sub pixel.			*/
/* Answer is returned with the equivalent polynomial mapping from	*/
/* left to right areas centered on the central pixel.			*/
/************************************************************************/

static void search_area(struct CostParams *params, double answer[6],
	double *quality, int inv_flag)
{
    double rn,right_dn,sumy,sumxy,sumy2;
    double r2,a,b,c,line_val[10],samp_val[10],qual[10];
    double numer,denom,sample,line;
    int i,j,m,n,kl,ks,num_areas,k,kll,kss;

    /* compute sub area limits if percent < 100 */
    num_areas=1;
    if (params->percent < 99.99) {
        num_areas=5;
        kl=(params->nlw*(params->percent/100.)) + .5;
        if (kl > params->nlw) kl=params->nlw;
        if (kl < 1) kl=1;
        ks=(params->nsw*(params->percent/100.)) + .5;
        if (ks > params->nlw) ks=params->nlw;
        if (ks < 1) ks=1;
        kll=params->nlw-kl-1;
        kss=params->nsw-ks-1;
    }

    /* loop on all sub areas */
    for (k=0; k < num_areas; k++) {

	/* compute area of sub area */
        if (k == 0) rn=params->nlw * params->nsw;
        if (k == 1) rn=kl * params->nsw;
        if (k == 2) rn=kl * params->nsw;
        if (k == 3) rn=ks * params->nlw;
        if (k == 4) rn=ks * params->nlw;

	/* fill correlation matrix */
        for (n=0; n < params->nlw2-params->nlw+1; n++) {
            for (m=0; m < params->nsw2-params->nsw+1; m++) {
                sumy=0.0;
                sumy2=0.0;
                sumxy=0.0;
                for (j=0; j < params->nlw; j++) {
                    for (i=0; i < params->nsw; i++) {
                        if (k == 1) {
                            if (j <= kll) continue;
                        }
                        if (k == 2) {
                            if (j >= kl) continue;
                        }
                        if (k == 3) {
                            if (i <= kss) continue;
                        }
                        if (k == 4) {
                            if (i >= ks) continue;
                        }
                        right_dn=right(j+n,i+m);
                        sumy += right_dn;
                        sumy2 += right_dn*right_dn;
                        sumxy += right_dn*left(j,i);
                    }
                }
		numer = sumxy-params->sumx[k]*sumy/rn;
		correl(n,m) = numer*numer / 
   		         (params->left_sum[k]*(sumy2-sumy*sumy/rn));
		if (numer < 0 && !inv_flag)	// allow negative correlation?
		    correl(n,m) = - correl(n,m); // nope (so q _is_ negative)
            }
        }
  
	/* locate highest value */
        r2= -1.0;
        for (n=0; n < params->nlw2-params->nlw+1; n++) {
            for (m=0; m < params->nsw2-params->nsw+1; m++) {
                if (r2 < correl(n,m)) {
                    r2=correl(n,m);
                    j=n;
                    i=m;
                }
            }
        }

	/* reject point if on border */
	if ((j == 0) || (j == params->nlw2-params->nlw+1) ||
	   (i == 0) || (i == params->nsw2-params->nsw+1)) {
            if (num_areas == 1) {
		params->ind = 1;
		return;
            }
            else{
                qual[k]=0.;
                break;			/* exit this loop */
            }
        }

	/* compute sub-pixel location of best correlation.
	   See Numerical Recipes, eqn: 10.2.1, note b+1/2 should read b-1/2   */
        a=correl(j,i-1);
        b=2.0*correl(j,i);
        c=correl(j,i+1);
        denom=2.0*(b-c-a);
        if (denom != 0.0) {
            sample=(c-a)/denom+i+(params->nsw-1)/2;}
        else{
            sample=i+(params->nsw-1)/2;
        }
        a=correl(j-1,i);
        c=correl(j+1,i);
        denom=2.0*(b-c-a);
        if (denom != 0.0) {
            line=(c-a)/denom+j+(params->nlw-1)/2;}
        else{
            line=j+(params->nlw-1)/2;
        }

        line_val[k]=line;
        samp_val[k]=sample;
        qual[k]=r2;
    } /* end of areas loop */

    /* locate best area fit */
    if (num_areas > 1) {
        i= -1;
        r2=-1.0;
        for (k=0; k < num_areas; k++) {
            if (qual[k] > r2) {
                r2=qual[k];
                i=k;
            }
        }
        if (i == -1) {
            params->ind=1;
            return;
        }
        line=line_val[i];
        sample=samp_val[i];
        *quality=qual[i];
    }
    else
        *quality=correl(j,i);

#if 0
    /* assemble polynomial terms mapping left to right */
    rightline=answer[0]*leftsamp+answer[1]*leftline+answer[2]
    rightsamp=answer[3]*leftsamp+answer[4]*leftline+answer[5]
#endif

    answer[0]=0.0;
    answer[1]=1.0;
    answer[2]=line-(params->nlw2-1)/2;
    answer[3]=1.0;
    answer[4]=0.0;
    answer[5]=sample-(params->nsw2-1)/2;

    params->ind = 0;
}

/************************************************************************/
/* Metropolis function to minimize by simulated annealing.		*/
/* This routine is a modified version of what's in r2lib (it's in C too)*/
/************************************************************************/

static void metropolis(struct CostParams *params, double answer[6],
	double range[6], int numten, int limits, int norm, double *quality)
{
      int fail1,fail2,success1,success2,limit,numreset,j,k,loop,narg,ind;
      unsigned int iseed;
      double temp[6],x[6],minx[6],mincost,pi,pi2,c1,c2,c3,scale,numtenf;
      double costsum,energy,boltzman,rand_max,ran,prob;

      pi=3.14159;
      pi2=pi/2.0;
      limit=limits;
      iseed=1;		/*!!!! p2 ones use zrangen() and a seed of 10109854 */
      srand(iseed);
      narg=6;
      numtenf=numten;
      fail1=0;
      fail2=0;
      success1=0;
      success2=0;
      numreset=numten/10;
      scale=exp((log(0.1))/numtenf);
      loop=0;
      rand_max=pow(2.0,31.0)-1.0;	/*!!!! Unix man page says 0..2^15-1! */

/*  Compute the cost at position ANSWER and assign to variable C1. */
      c1 = cost(answer, 6, params) - 1.0;
      if (params->ind != 0) {
         return;
      }

/*  Save the cost in case the user had good reason to inspect this
    solution position. */
      mincost=c1;
      for (j=0; j < narg; j++) {
         minx[j]=answer[j];
      }

/*  Set initial temperatures to the range estimates. */
      for (j=0; j < narg; j++) {
         temp[j]=range[j];
      }

/*   MAIN LOOP: loop on number of successful changes in solution space. */
      while(loop < limit) {

/*   Compute the delta_cost/temperature ratio for
     normalization of probabilities.
     Note that this is the Boltzmann constant for this 'system'.*/

        k=loop/norm;
        if (loop-k*norm == 0) {
           costsum=0.0;
           k=0;
           for (j=0; j < narg; j++) {
              x[j]=answer[j];
           }
           for (j=0; j < narg; j++) {
              x[j]=answer[j]-temp[j];
              c2 = cost(x, 6, params) - 1.0;
              if (params->ind == 0) {
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j]+temp[j];
              c2 = cost(x, 6, params) - 1.0;
              if (ind == 0) {
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j];
           }
           if (k == 0) {
              params->ind=2;
              return;
           }
           boltzman=5.0*(costsum/k);
        }

/*      Decrement the temperature according to the multiplicative
        cooling schedule.  */
        for (j=0; j < narg; j++) {
           temp[j]=temp[j]*scale;
        }
        energy=boltzman*(temp[0]+temp[1]+temp[2]+temp[3]+temp[4]
                        +temp[5])/6.0;

/*      Compute a solution space guess using a Cauchy-Lorentzian
        random probability distribution function. */
	do {
            for (j=0; j < narg; j++) {
               ran=rand()/rand_max;
               x[j]=temp[j]*tan(pi*ran+pi2)+answer[j];
            }
            c2 = cost(x, 6, params) - 1.0;
            if (params->ind != 0) {
               fail1 += 1;
            }
	} while (params->ind != 0);

        if (c2 < c1) {

/*          Accept lower cost position.
            We always accept a downhill cost route if offered.*/

            success1 += 1;
            c1=c2;
            for (j=0; j < narg; j++) {
               answer[j]=x[j];
            }
        }
        else{
/*          Compute probability of accepting higher cost position.
            This comes from the Boltzmann probability of our system 
            transitioning from energy state c1 to energy state c2.*/

            c3=(c2-c1)/energy;
            if (c3 > 50.) {
               goto A92;
            }
            prob=1.0/exp((double)c3);

/*          Evaluate the probability by comparing it against chance.*/

            ran=rand()/rand_max;
            if (prob > ran) {
/*              Accept higher cost position.*/
                success2 += 1;
                c1=c2;
                for (j=0; j < narg; j++) {
                   answer[j]=x[j];
                }
            }
            else{
/*              Reject higher cost position.*/
                fail2 += 1;
                goto A92;
            }
        }

/*       Save the minimum cost and associated solution as we go.*/

        if (c1 < mincost) {
            mincost=c1;
            for (j=0; j < narg; j++) {
               minx[j]=answer[j];
            }
        }
A92:
        loop=loop+1;

/*       Reset the solution pointer to the minimum cost
         location every numreset successful iterations. */

        k=loop/numreset;
        if (loop-k*numreset == 0) {
            c1=mincost;
            for (j=0; j < narg; j++) {
               answer[j]=minx[j];
            }
        }

      }  /*   END of MAIN WHILE LOOP  */

/*     Put minimum solution into ANSWER & it's cost into quality  */

      for (j=0; j < narg; j++) {
         answer[j]=minx[j];
      }
      *quality=1.0-mincost;

      params->ind = 0;
 /*printf("Initial ok=%d bad=%d, Probability ok=%d bad=%d\n",success1,
         fail1,success2,fail2);*/
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gruen.imake
#define SUBROUTINE gruen

#define MODULE_LIST gruen.c

#define P2_SUBLIB

#define USES_ANSI_C

$ Return
$!#############################################################################
