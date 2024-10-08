$!****************************************************************************
$!
$! Build proc for MIPL module gllgcor
$! VPACK Version 1.8, Tuesday, September 03, 1996, 14:10:14
$!
$! Execute by entering:		$ @gllgcor
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module gllgcor ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to gllgcor.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("gllgcor.imake") .nes. ""
$   then
$      vimake gllgcor
$      purge gllgcor.bld
$   else
$      if F$SEARCH("gllgcor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake gllgcor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @gllgcor.bld "STD"
$   else
$      @gllgcor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create gllgcor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack gllgcor.com -
	-s gllgcor.c -
	-i gllgcor.imake -
	-t tgllgcor.f tcgllgcor.c tgllgcor.imake tgllgcor.pdf tstgllgcor.pdf -
	-o gllgcor.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create gllgcor.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"

/* The distortion constant must be a positive number. If it is smaller than  */
/* 6.58 E-11; G_FLOAT type doubles have to be used for the algorithm to work */
#define  DEFAULT_A			0.00000000658

#define  DEFAULT_OA_LINE		400.0
#define  DEFAULT_OA_SAMP		400.0

double cubic_root();

/*---------------------------------------------------------------------------*/
/* Fortran-Callable Version                                                  */
/*---------------------------------------------------------------------------*/

void FTN_NAME(gllgcor)(status,is_line,is_samp,os_line, os_samp,mode,icam)
  int   *status;        /*  return status, currently information-less */
  float	*is_line,	/*  Image-space line coordinate             */
	*is_samp,	/*  Image-space sample coordinate           */
	*os_line,	/*  Object-space line coordinate            */
	*os_samp;	/*  Object-space sample coordinate          */
  int	*mode;		/*  Conversion mode: 1=IS to OS, 0=OS to IS */
  int   *icam;		/*  1=full-frame, 2=summation mode          */
{
  *status = zgllgcor(is_line,is_samp,os_line,os_samp,*mode,*icam);
}

/*---------------------------------------------------------------------------*/
/* C-Callable Version                                                        */
/*---------------------------------------------------------------------------*/

int zgllgcor(is_line,is_samp,os_line,os_samp,mode,icam)
  float	*is_line,	/*  Image-space line coordinate             */
	*is_samp,	/*  Image-space sample coordinate           */
	*os_line,	/*  Object-space line coordinate            */
	*os_samp;	/*  Object-space sample coordinate          */
  int	mode;		/*  Conversion mode: 1=IS to OS, 0=OS to IS */
  int   icam;		/*  1=full frame, 2=summation mode          */
{
  double	oa_x, oa_y,	/*  Optical axis location                    */
	        x, y,		/*  Optical axis corrected location          */
		A_const,	/*  Theorical distortion constant            */
		os_rad,		/*  Object-space radius from optical axis    */
		is_rad,		/*  Image-space radius from optical axis     */
	        A_3,		/*  Intermediate value in IS->OS computation */
	        B_3,		/*  Intermediate value in IS->OS computation */
	        common_term1,	/*  Intermediate value in computations       */
	        common_term2;	/*  Intermediate value in computations       */
  /***  Substitute defaults for missing optional parameters  ---------***/
  oa_y = DEFAULT_OA_LINE;
  oa_x = DEFAULT_OA_SAMP;
  A_const = DEFAULT_A;

  /***  Image Space to Object Space  ---------------------------------***/
  if (mode) {
     if (icam==2) {			/* If summation mode, */
        x = (*is_samp)*2. - oa_x;	/* convert to full-frame */
        y = (*is_line)*2. - oa_y;	/* coordinates */
     }
     else {
        x = (*is_samp) - oa_x;
        y = (*is_line) - oa_y;
     }
     is_rad = sqrt((double)(x*x + y*y));

     if (is_rad == 0.0) {
        *os_line = *is_line;
        *os_samp = *is_samp;
     }
     else {
        common_term1 = is_rad/(2.0 * A_const);
        common_term2 = sqrt(pow(common_term1,(double)(2)) +
                          pow((double)(1.0/(3.0 * A_const)),(double)(3)));
        A_3 = common_term1 + common_term2;
        B_3 = common_term1 - common_term2;
        os_rad = cubic_root(A_3) + cubic_root(B_3);
        *os_line = (os_rad*y)/is_rad + oa_y;
        *os_samp = (os_rad*x)/is_rad + oa_x;
        if (icam==2) {
           *os_line = *os_line/2.;
           *os_samp = *os_samp/2.;
        }
     }
  }

  /***  Object Space to Image Space  ---------------------------------***/
  else {
     if (icam == 2) {
        x = (*os_samp)*2 - oa_x;
        y = (*os_line)*2 - oa_y;
     }
     else {
        x = (*os_samp) - oa_x;
        y = (*os_line) - oa_y;
     }
     common_term1 = 1.0 + (A_const*(x*x + y*y));

     *is_line = y*common_term1 + oa_y;
     *is_samp = x*common_term1 + oa_x;
     if (icam==2) {
        *is_line = *is_line/2.;
        *is_samp = *is_samp/2.;
     }
  }
  return (0);
}

/***  Determines the cubic root of x  -------------------------------------***/
double cubic_root(x)
  double x;
{
  if     (x == 0) return ((double)(0));
  else if (x < 0) return ( -exp(log(fabs(x)) / 3.0) );
  else            return ( exp(log(x) / 3.0) );
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create gllgcor.imake
#define SUBROUTINE gllgcor

#define MODULE_LIST gllgcor.c

#define USES_C

#define P2_SUBLIB
$ Return
$!#############################################################################
$Test_File:
$ create tgllgcor.f
      include 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      IMPLICIT NONE
      CHARACTER*80 MSG
      INTEGER STATUS,MODE,I,N,ICAM
      REAL*4 IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,R,ERR
      REAL*4 L(4)/1.,  1.,400.,800./
      REAL*4 S(4)/1.,400.,400.,800./
  100 FORMAT('IS to OS: (',F10.6,',',F10.6,
     &       ')   (',F10.6,',',F10.6,')  R=',F10.6)
  101 FORMAT('OS to IS: (',F10.6,',',F10.6,
     &       ')   (',F10.6,',',F10.6,')  ERR=',F10.6)

      CALL XVMESSAGE('Test FORTRAN bridge to GLLGCOR', ' ')

      DO 10 ICAM=1,2
      CALL XVMESSAGE(' ',' ')
      IF (ICAM.EQ.1) CALL XVMESSAGE('Full Frame test',' ')
      IF (ICAM.EQ.2) CALL XVMESSAGE('Summation mode test',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('            IS (LINE,SAMP)          OS (LINE,SAMP)
     & ',' ')
      N = 5 - ICAM

      DO 10 I=1,N
      IS_LINE = L(I)
      IS_SAMP = S(I)
      MODE = 1
      CALL GLLGCOR(status,IS_LINE,IS_SAMP,os_line,os_samp,MODE,ICAM)
      R = SQRT((OS_LINE-IS_LINE)**2 + (OS_SAMP-IS_SAMP)**2)
      WRITE (MSG,100) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,R
      CALL XVMESSAGE(MSG,' ')
      MODE = 0
      CALL GLLGCOR(status,is_line,is_samp,OS_LINE,OS_SAMP,MODE,ICAM)
      ERR = SQRT((IS_LINE-L(I))**2 + (IS_SAMP-S(I))**2)
      WRITE (MSG,101) IS_LINE,IS_SAMP,OS_LINE,OS_SAMP,ERR
      CALL XVMESSAGE(MSG,' ')
   10 CALL XVMESSAGE(' ',' ')

      CALL XVMESSAGE('Test zgllgcor', ' ')
      CALL TCGLLGCOR
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tcgllgcor.c
#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"

#define ERROR_TOLERANCE 0.00005

FTN_NAME(tcgllgcor)()
{

  int lc,lc1;
  float is_line,
	is_samp,
	os_line,
	os_samp,
        line_dif,
        samp_dif;
  char	output[128];


  sprintf(output,"Tolerance is set to %10.8f",ERROR_TOLERANCE);
  zvmessage(output,"");

  for (lc=(-100);lc<=900;lc+=100)
     for (lc1=(-100);lc1<=900;lc1+=100) {
        is_line = lc;
        is_samp = lc1;

        zgllgcor(&is_line,&is_samp,&os_line,&os_samp,1,0);
        zgllgcor(&is_line,&is_samp,&os_line,&os_samp,0,0);

        line_dif = fabs(is_line-(float)lc);
        samp_dif = fabs(is_samp-(float)lc1);

        if (line_dif > ERROR_TOLERANCE || samp_dif > ERROR_TOLERANCE) {

            sprintf(output,"line: %d %10.6f %10.6f samp: %d %10.6f %10.6f",
                       lc, is_line,line_dif,lc1,is_samp,samp_dif);
            zvmessage(output,"");
        }
  }
}
$!-----------------------------------------------------------------------------
$ create tgllgcor.imake
#define PROGRAM tgllgcor
#define MODULE_LIST tgllgcor.f tcgllgcor.c
#define TEST
#define MAIN_LANG_FORTRAN
#define USES_C
#define USES_FORTRAN
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create tgllgcor.pdf
process help=*
end-proc

.TITLE
Vicar2 Routine TGLLCGOR
.HELP
PURPOSE:

	Verify operation of GLLGCOR routine.

EXECUTION:

	TGLLGCOR   ...  There are no parameters

OPERATION:

	TGLLGCOR runs though a loop of the more common line/sample
combinations converting from image space to object space and back.
the original vlaue is then compared to the processed value.  If the
two differ by an amount greater than an internally defined tolerance,
the line & sample pairs are displayed.

PROGRAM HISTORY:
 Original Programmer: Allan Runkle
 Cognizant programmer:  Allan Runkle
.end
$!-----------------------------------------------------------------------------
$ create tstgllgcor.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"

TGLLGCOR

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create gllgcor.hlp
procedure help=*
body
help GLLGCOR
end-proc
.title
VICAR routine GLLGCOR
.help
Purpose:

GLLGCOR converts image line,sample values from raw (image space) coordinates to
geometrically corrected (object space) coordinates or vice-versa.  This routine
is specific to the Galileo SSI camera.

FORTRAN Calling Sequence:

      INTEGER*4 STATUS			!Always 0 (success)
      INTEGER*4 IS_LINE,IS_SAMPLE	!Image space line,sample
      INTEGER*4 OS_LINE,OS_SAMPLE	!Object space line,sample
      INTEGER*4 MODE			!0=OS to IS, 1=IS to OS
      INTEGER*4 ICAM			!1=full frame, 2=summation mode
      CALL GLLGCOR(status,is_line,is_sample,os_line,os_sample,MODE,ICAM)

C Calling Sequence:

      int status;			/* always 0 (success) */
      int is_line,is_sample;		/* Image space line,sample */
      int os_line,os_sample;		/* Object space line,sample */
      int mode;				/* 0=OS to IS, 1=IS to OS */
      int icam;				/* 1=full frame, 2=summation mode */
      status=zgllgcor(&is_line,&is_sample,&os_line,&os_sample,mode,icam);

Reference: D-5880, "Solid-State Imaging Subsystem Calibration Report: Part 2",
  page 133, K. Klaasen, March 22, 1993.

Operation:

GLLGCOR uses an optical distortion model of the SSI camera where the distortion
is a function of the radial distance from the point where the optical axis
intercepts the focal plane:

	R_is = A*R_os**3 + R_os
where
        R_is is the radial distances from the optical center in Image Space,
        R_os is the radial distances from the optical center in Object Space,
        A (=6.58*10**(-9)) is the optical distortion constant

The optical center is at location line=400, sample=400.  The magnitude of the
distortion is 1.2 pixels at the corners.  All units refer to full-frame images.

The solution of this equation for R_os is from the CRC math tables and is
tailored to this problem (i.e., only one real root is expected and therefore
only one calculated).  The solution is accurate to .0005 pixels for radial
distances varying between 0 and 707 pixels.

History:

Original Programmer:	A. Runkle	March 3, 1989
Cognizant Programmer:	Gary Yagi
Source Language: C
Revision History:

02 Sep 96  GMY  Add ICAM argument to handle summation mode.
.end
$ Return
$!#############################################################################
