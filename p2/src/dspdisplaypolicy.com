$!****************************************************************************
$!
$! Build proc for MIPL module dspdisplaypolicy
$! VPACK Version 1.8, Thursday, October 17, 1996, 01:18:46
$!
$! Execute by entering:		$ @dspdisplaypolicy
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
$ write sys$output "*** module dspdisplaypolicy ***"
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
$ write sys$output "Invalid argument given to dspdisplaypolicy.com file -- ", primary
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
$   if F$SEARCH("dspdisplaypolicy.imake") .nes. ""
$   then
$      vimake dspdisplaypolicy
$      purge dspdisplaypolicy.bld
$   else
$      if F$SEARCH("dspdisplaypolicy.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspdisplaypolicy
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspdisplaypolicy.bld "STD"
$   else
$      @dspdisplaypolicy.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspdisplaypolicy.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspdisplaypolicy.com -mixed -
	-s DspDisplayPolicy.cc -
	-i dspdisplaypolicy.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspDisplayPolicy.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//       DspDisplayPolicy.cc: Contains Pseudo color Luts.
//
//      FUTURE:  
//		(1) may need more pixel size code in the future
//
//		(2) change stretchImage to 3 stretch lutarrays
//		for handling color. Then correct getStretchLut()
//
//		(3) change in GUI design caused change in this
//		module.  Now it doesn't need to be created for 
//		every image.  It can be shared umong images/wedges.
//
///////////////////////////////////////////////////////////////////
#include "DspDisplayPolicy.h"
#include "DspInstrBasic.h"
#include "UIComponent.h"
#include <iostream.h>



///////////////////////////////////////////////////////////////////
//	CONSTRUCTOR 
///////////////////////////////////////////////////////////////////
DspDisplayPolicy::DspDisplayPolicy(DspInstrBasic * , char *   )
{
//	INIT VARIABLES
	_pixelSize = 8;  // default

//	CREATE 3 LUTS FOR PSEUDO COLOR (init in dspinstrument)
	_lutPseudoColor[__red]   = new DspLut();
	_lutPseudoColor[__green] = new DspLut();
	_lutPseudoColor[__blue]  = new DspLut();
	
//	CREATE 3 LUTS FOR STRETCHING
	_lutStretch[__red]   = new DspLut();
	_lutStretch[__green] = new DspLut();		// temp
	_lutStretch[__blue]  = new DspLut();		// temp
	
//	INIT STRETCH LUTS
	_lutStretch[__red]->ramp();
	_lutStretch[__green]->ramp();
	_lutStretch[__blue]->ramp();			
}

///////////////////////////////////////////////////////////////////
//	UPDATE (or change) MODE IN IW (wedge or image)
//		called internally by addWidget()
//		and may be called externally for updating widget
///////////////////////////////////////////////////////////////////
void DspDisplayPolicy::updateMode(Widget w, 
					void (*fp_getStretch)(int *, int *, int *),
					Boolean pseudoColored,
					Boolean isDialogStretch)
{
//	GET COLOR MODE (IE BLACK-WHITE OR COLOR)
	unsigned char	colorMode;
	XtVaGetValues(w, XvicNimageMode, &colorMode, NULL );
	
//	SET PseudoColor LUTS IN IMAGE
        if (pseudoColored && colorMode == XvicBW) 
	     XvicImageSetColorLUT(w, _lutPseudoColor[__red]->getAsArray(),
				_lutPseudoColor[__green]->getAsArray(),
				_lutPseudoColor[__blue]->getAsArray());
						
//	First STRETCH IMAGE (if stretch dialog not enabled)
	stretchImage(w,fp_getStretch,isDialogStretch);

//	SET TO PSEUDO MODE OR NON PSEUDOMODE (but always stretched-may be raw)			
	if ((colorMode == XvicBW ) && ( pseudoColored )) 
       		XtVaSetValues (w, XvicNlutType, XvicPSEUDO, NULL );
	else 
		XtVaSetValues (w, XvicNlutType, XvicSTRETCH, NULL );
}
 
///////////////////////////////////////////////////////////////////
//	stretchImage (private) called by updateMode
//		Stretch table from *Browse user i/f*   (BW only at this time)
///////////////////////////////////////////////////////////////////
void DspDisplayPolicy::stretchImage(Widget w, 
					void (*fp_getStretch)(int *, int *, int *),
					Boolean isDialogStretch)
{
//	IF STRETCH TABLE FROM BROWSE i/f OR R/T CONTROL i/f (IE NOT STRETCH DIALOG)
	if ( !isDialogStretch) {
			
//		GET STRETCH FROM DSP CNTRL (IE BROWSE or r/t control) 
//		(NOTICE THIS HANDLES BW ONLY AT THIS TIME - NA FOR COLOR YET)			
		(*fp_getStretch)( _lutStretch[0]->getAsArray(),
					 _lutStretch[1]->getAsArray(),
					 _lutStretch[2]->getAsArray());
	}	
	 
//	SET STRETCH LUT 
	unsigned char	colorMode;
	XtVaGetValues(w, XvicNimageMode, &colorMode, NULL );
	if (colorMode != XvicCOLOR)
		XvicImageSetMonoLUT(w, 	_lutStretch[0]->getAsArray());
	else 
		XvicImageSetColorLUT(w, _lutStretch[__red]->getAsArray(),
						_lutStretch[__green]->getAsArray(),
						_lutStretch[__blue]->getAsArray());		
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspdisplaypolicy.imake
#define SUBROUTINE dspdisplaypolicy 
#define MODULE_LIST DspDisplayPolicy.cc

#define P2SUB         /* SUBROUTINE */
#define P2_SUBLIB

#define USES_C_PLUS_PLUS
#define LIB_GUISUB
#define LIB_GLLSUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_GUI

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
$ Return
$!#############################################################################
