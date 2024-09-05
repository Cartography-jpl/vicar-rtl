$!****************************************************************************
$!
$! Build proc for MIPL module mp_pconstants
$! VPACK Version 1.9, Wednesday, October 07, 1998, 09:46:41
$!
$! Execute by entering:		$ @mp_pconstants
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
$ write sys$output "*** module mp_pconstants ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Create_Other .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to mp_pconstants.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("mp_pconstants.imake") .nes. ""
$   then
$      vimake mp_pconstants
$      purge mp_pconstants.bld
$   else
$      if F$SEARCH("mp_pconstants.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mp_pconstants
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mp_pconstants.bld "STD"
$   else
$      @mp_pconstants.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mp_pconstants.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mp_pconstants.com -
	-s mp_pconstants.c -
	-i mp_pconstants.imake -
	-o mp_pconstants.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mp_pconstants.c
$ DECK/DOLLARS="$ VOKAGLEVE"
				/*					*/
				/* INCLUDE FILES NEEDED FOR ROUTINES 	*/
				/*					*/
#include "xvmaininc.h"		/* Standard VICAR Include File		*/
#include "applic.h"
#include "errdefs.h"
#include "ftnbridge.h"		/* FORTRAN bridge Include FIle 		*/
#include <math.h>		/* Standard math library include	*/
#include <stdio.h>		/* Standard C I/O Include File		*/
#include <stdlib.h>		/* C Memory Management Include File	*/
#include <time.h>
#include "mp_routines.h"
/*

VICAR SUBROUTINE		mpPConstants

Purpose				To get planetary constants values and place
				them in the MP data object.

Function			Takes as input the file name of a NAIF SPICE
				planetary constants kernel and a target body
				name and fills the planetary constants portion
				of the MP data object with the respective
				kernel values.

Libraries and subroutines
required to run routine:	NAIF SPICELIB 

Calling Sequence:		

	calling from C		status = mpPConstants( MP_DATA, 
					char *file_name, 
					char *target_body_name );

	calling from FORTRAN	call mp_pconstants( MP_DATA 
			       .	file_name,
			       .	target_body_name ) 


Necessary include files
from calling routine 
or program:			mp.h


Arguments:
	
Name			Type		In/Out		Description

MP_DATA			MP		Input/Output	Address of
							Map Projection 
							Data Object

file_name		char		input 		Full file name of
							P_CONSTANTS.KER

target_body_name	char		Input 		Full target body
							name

Background and References:	Map Projection Software Set 
				Software Specification Document,
				JPL, April 28, 1993.

Software Platform:		VICAR 11.0

Hardware Platforms:		No particular hardware required; tested on 
				VAX 8650 and Sun Sparcstation.

Programming Language:		ANSI C

Specification by:		Justin McNeill, JPL.

Cognizant Programmer:		Justin McNeill, JPL
				(jfm059@ipl.jpl.nasa.gov)

Date:				October 1993

Revision History:		June 1994	JFM

				Function check_body_id added to source
				code file to remove dependence of the
				general MP routines on the NAIF SPICE
				library. (FR 85074)


				September 1998  msm
				
				Moved mppconstants.c from mp_routines.com to
				mp_pconstants.com; to eliminate SPICE
				dependencies, mp_routines.com moved to P1 while
				mp_pconstants.com remained in P2
				
*/

#define		MAX_ELEMENTS	10

/*************************************************************************

FORTRAN Callable Version

*************************************************************************/

int FTN_NAME(mp_pconstants)( MP *mp, char *file_name, char *target_body,
				ZFORSTR_PARAM )
{
ZFORSTR_BLOCK
int	status;
int	string_length;
char 	*file;
char	*target;
MP	*mp_obj;

/*

Fortran-to-C bridge for strings.

*/
zsfor2len(string_length, file_name, &mp, 3, 2, 1, target_body);
file 		= (char *)calloc(string_length++,sizeof(char));
zsfor2c(file,string_length,file_name, &mp, 3, 2, 1, target_body);

zsfor2len(string_length, target_body, &mp, 3, 3, 2, target_body);
target		= (char *)calloc(string_length++,sizeof(char));
zsfor2c(target,string_length,target_body, &mp, 3, 3, 2, target_body);

/*

Call C version of mp_pconstants.

*/
status = mpPConstants( *mp, file, target );

return status;

}
/*************************************************************************

C Callable Version

*************************************************************************/

int mpPConstants( MP mp_obj, char file_name[], char target_body_name[] )
{
int	status;
int	body_id, temp_body_id;
int	dimensions;
double	values[MAX_ELEMENTS], value;

/*

Set SPICELIB error handling to send messages without aborting
mpPConstants routine and load P_contants kernel.

*/
zerract("SET","RETURN");	
zldpool(file_name);

/*

Determine NAIF body id for target body name.

*/

zbodn2c(target_body_name, &body_id, &status);
if ( status == FALSE )
	{
	status = mpGetValues(mp_obj,mpTARGET_BODY,target_body_name);
	CHECKif( status < mpSUCCESS );

	zbodn2c(target_body_name, &body_id, &status);
	CHECKif( status == FALSE );
	}

/*

Determine if keyword RADII exists in the P_constants.ker for specified
NAIF body and get measures from the kernel.

*/
temp_body_id = body_id;

status = check_body_id(&temp_body_id,"RADII");
CHECKif( status != mpSUCCESS );

dimensions = 3;
zbodvar(temp_body_id, "RADII", &dimensions, values);

/*

Set body axes measures in data object.

*/
status = mpSetValues(mp_obj,mpA_AXIS_RADIUS,values[0],NULL);
CHECKif( status < mpSUCCESS );

status = mpSetValues(mp_obj,mpB_AXIS_RADIUS,values[1],NULL);
CHECKif( status < mpSUCCESS );

status = mpSetValues(mp_obj,mpC_AXIS_RADIUS,values[2],NULL);
CHECKif( status < mpSUCCESS );

/*

Determine if keyword POLE_RA exists in the P_constants.ker for specified
NAIF body and get values from the kernel.


temp_body_id = body_id;
status = check_body_id(&temp_body_id,"POLE_RA");
if ( status != 0 ) return -2;

dimensions = 3;
zbodvar(temp_body_id, "POLE_RA", &dimensions, values);

Set body pole right ascension coefficients in data object.

status = mpSetValues(mp_obj,mpBODY_POLE_RA_1,values[0],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_POLE_RA_2,values[1],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_POLE_RA_3,values[2],NULL);
if ( status < 0 ) return -1;
*/

/*

Determine if keyword POLE_DEC exists in the P_constants.ker for specified
NAIF body and get values from the kernel.


temp_body_id = body_id;
status = check_body_id(&temp_body_id,"POLE_DEC");
if ( status != 0 ) return -2;

dimensions = 3;
zbodvar(temp_body_id, "POLE_DEC", &dimensions, values);

Set body pole declination coefficients in data object.

status = mpSetValues(mp_obj,mpBODY_POLE_DEC_1,values[0],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_POLE_DEC_2,values[1],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_POLE_DEC_3,values[2],NULL);
if ( status < 0 ) return -1;
*/

/*

Determine if keyword POLE_PM exists in the P_constants.ker for specified
NAIF body and get values from the kernel.

temp_body_id = body_id;
status = check_body_id(&temp_body_id,"PM");
if ( status != 0 ) return -2;

dimensions = 3;
zbodvar(temp_body_id, "PM", &dimensions, values);

Set body prime meridian coefficients in data object.

status = mpSetValues(mp_obj,mpBODY_PM,values,dimensions,NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_PM_2,values[1],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_PM_3,values[2],NULL);
if ( status < 0 ) return -1;
*/


/*
Determine if keyword LONG_AXIS exists in the P_constants.ker for specified
NAIF body and get values from the kernel.
*/

temp_body_id = body_id;
status = check_body_id(&temp_body_id,"LONG_AXIS");
if ( status != 0 ) return -2;

dimensions = 1;
zbodvar(temp_body_id, "LONG_AXIS", &dimensions, values);

status = mpSetValues(mp_obj,mpBODY_LONG_AXIS,values[0],NULL);
if (status < 0) return -1;

/*
dimensions = 3;
zbodvar(temp_body_id, "LONG_AXIS", &dimensions, values);

Set body long axis measures in data object.

status = mpSetValues(mp_obj,mpBODY_LONG_AXIS_1,values[0],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_LONG_AXIS_2,values[1],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_LONG_AXIS_3,values[2],NULL);
if ( status < 0 ) return -1;
*/


/*

Determine if keyword NUT_PREC_RA exists in the P_constants.ker for specified
NAIF body and get values from the kernel.

temp_body_id = body_id;
status = check_body_id(&temp_body_id,"NUT_PREC_RA");
if ( status != 0 ) return -2;

dimensions = 3;
zbodvar(temp_body_id, "NUT_PREC_RA", &dimensions, values);

Set nutation precision right ascension coefficients in data object.


status = mpSetValues(mp_obj,mpBODY_NUT_PREC_RA_1,values[0],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_RA_2,values[1],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_RA_3,values[2],NULL);
if ( status < 0 ) return -1;
*/

/*

Determine if keyword NUT_PREC_DEC exists in the P_constants.ker for specified
NAIF body and get values from the kernel.

temp_body_id = body_id;
status = check_body_id(&temp_body_id,"NUT_PREC_DEC");
if ( status != 0 ) return -2;

dimensions = 3;
zbodvar(temp_body_id, "NUT_PREC_RA", &dimensions, values);

Set nutation precision declination coefficients in data object.

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_DEC_1,values[0],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_DEC_2,values[1],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_DEC_3,values[2],NULL);
if ( status < 0 ) return -1;
*/

/*

Determine if keyword NUT_PREC_PM exists in the P_constants.ker for specified
NAIF body and get values from the kernel.

temp_body_id = body_id;
status = check_body_id(&temp_body_id,"NUT_PREC_PM");
if ( status != 0 ) return -2;

dimensions = 3;
zbodvar(temp_body_id, "NUT_PREC_PM", &dimensions, values);

Set nutation precisions prime meridian coefficients in data object.

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_PM_1,values[0],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_PM_2,values[1],NULL);
if ( status < 0 ) return -1;

status = mpSetValues(mp_obj,mpBODY_NUT_PREC_PM_3,values[2],NULL);
if ( status < 0 ) return -1;
*/

return mpSUCCESS;
}

/***********************************************************************

check_body_id

Checks for a given NAIF body id if item is found.
If not, replace body_id with barycenter body_id when
body_id is x99. (See NAIF PCK values for target bodies.)

*/
int	check_body_id( int *body_id, char *item )
{
int 	status;
int	temp_body_id;
double  double_argument,fractional_part,integer_part;

status = zbodfnd(*body_id,item);
if ( status == FALSE )
	{
	double_argument = (double)*body_id / 100.0;
	fractional_part = modf(double_argument,&integer_part);
	if ( fractional_part > 0.989 )			/* A planet is always */
		{					/* the 99th satellite */
		temp_body_id = (int)integer_part;	/* of its own baryctr */
							/* Use baryctr to get */
							/* keyword value.     */
		status = zbodfnd(temp_body_id,item);
		CHECKif( status == FALSE );

		*body_id = temp_body_id;
		}
	else
		return mpFAILURE;
	}

return mpSUCCESS;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mp_pconstants.imake
/* Imake file for MIPS subroutines MP_PCONSTANTS   */

#define SUBROUTINE  	mp_pconstants

#define MODULE_LIST 	mp_pconstants.c

#define P2_SUBLIB

/*************************

LOCAL LIBRARY for development

#define DEBUG
#define LIB_LOCAL
*************************/

/*#define FTN_STRING */	/* comment this out for Debug on Unix! */
#define USES_ANSI_C
#define USES_FORTRAN
#define LIB_FORTRAN
$ Return
$!#############################################################################
$Other_File:
$ create mp_pconstants.hlp
1 VICAR SUBROUTINE mpPConstants

PURPOSE

The function mpPConstants retrieves planetary constants 
values from a designated NAIF/SPICE PCK file and places
them in the MP data object.

2 OPERATION

This function takes as input the file name of a NAIF SPICE
planetary constants kernel and a target body name and fills 
the planetary constants portion of the MP data object with 
the respective kernel values.  This NAIF SPICE Toolkit and
its special collection of Galileo functions are required for
this function.


CALLING SEQUENCE		

from C		status = mpPConstants( MP mp, char *file_name, 
				char *target_body_name );

from FORTRAN	status = mp_pconstants( mp, file_name, 
				target_body_name ) 

Necessary include files
from calling routine 
or program:			mp_routines.h


ARGUMENTS

INPUT 	

mp			(MP type)

A pointer to the MP data structure as returned mpInit. 
In FORTRAN, this variable is of type REAL*8.  MP type
is defined in include file mp_routines.h.

file_name		(character string)

Full file pathname of NAIF/SPICE PCK file (planetary constants 
kernel - P_CONSTANTS.KER)

target_body_name	(character string)

Full NAIF standard target body name.


RETURN

status 			(integer)

Status flag for the operation of the routine. 
Zero (0) signifies success and -1 failure.

3 ENVIRONMENT

Programming Language:		ANSI C
Software Platform:		VICAR 11.0
Hardware Platforms:		No particular hardware required; 
				tested on DEC Alpha, DEC VAX-VMS,
				Sun Sparcstation (Solaris and SunOS),
				Silicon Graphics (SGI), and HP 700.

3 BACKGROUND

Author:				Justin McNeill, JPL.
Cognizant Engineer:		Justin McNeill, JPL
				(jfm059@ipl.jpl.nasa.gov)
Date written:			October 1993
Revision History:		Original
Background and References:	

1. Map Projection Software Set Software Specification Document,
	JPL, April 28, 1993;
2. "Conformal Mapping of the Triaxial Ellipsoid," Survey Review,
	vol. 28, July 1985.
3. MIPS Map Projection Software Users' Guide, D-11810, version 1.0,
	May 1994.
$ Return
$!#############################################################################
