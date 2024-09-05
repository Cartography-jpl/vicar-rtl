$!****************************************************************************
$!
$! Build proc for MIPL module dspapplication
$! VPACK Version 1.8, Thursday, October 17, 1996, 01:18:45
$!
$! Execute by entering:		$ @dspapplication
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
$ write sys$output "*** module dspapplication ***"
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
$ write sys$output "Invalid argument given to dspapplication.com file -- ", primary
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
$   if F$SEARCH("dspapplication.imake") .nes. ""
$   then
$      vimake dspapplication
$      purge dspapplication.bld
$   else
$      if F$SEARCH("dspapplication.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspapplication
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspapplication.bld "STD"
$   else
$      @dspapplication.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspapplication.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspapplication.com -mixed -
	-s DspApplication.cc -
	-i dspapplication.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspApplication.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// DspApplication.cc: 
////////////////////////////////////////////////////////////
#include "DspApplication.h"
#include <assert.h>
#include <stdlib.h>
#include <iostream.h>
#include <stdio.h>
#include <string.h>
#include <zvproto.h>
#if XmVERSION != 1 || XmREVISION > 1
#include <Xm/RepType.h>
#endif

extern	char *rts_log_prgm;


#if (XlibSpecificationRelease>=5)
void DspApplication::initialize_X ( int *argcp, char **argv )
#else
void DspApplication::initialize_X ( unsigned int *argcp, char **argv )
#endif
{
//	GET ARG
	int * argc = (int *) argcp;
	
//	CAST TO DSPBASICWINDOW
	DspWindowBasic ** dspWindows = (DspWindowBasic **) _windows;
	
//	CHECK TO MAKE SURE THE MAINWINDOW IS OF TYPE DSPBASICWINDOW
	assert (strcmp( dspWindows[0]->className(), "DspWindowBasic") == 0 ) ;

//	INIT TOP LEVEL ( _w = toplevel ) (See Note #1, at bottom)
	dspWindows[0]->initInterface(*argc, argv); // initIF before we can get name
	char * displayName = dspWindows[0]->getDisplayName(); // (See Note #2)
	XtToolkitInitialize();
	_appContext = XtCreateApplicationContext();
	
//	OPEN DISPLAY FOR displayName
	_display     = NULL;
	_display     = XtOpenDisplay(_appContext, displayName,
				_name,_applicationClass, NULL,0, argc, argv);
//	IF UNABLE TO OPEN displayName, DEFAULT TO LOCAL DISPLAY	(NULL)			
	if (_display == NULL ) {
		if ( strcmp(displayName,"") != 0) // Not an error for browse
			cout << "Unable to display to screen named: "<< displayName << "."<<endl;
		cout << "Will display to local screen. " << endl;
		_display = XtOpenDisplay(_appContext,NULL,
				_name,_applicationClass, NULL,0, argc, argv);
	}

	_w = XtAppCreateShell( _name,_applicationClass, 
				applicationShellWidgetClass,_display, NULL, 0);
}
//	NOTE #1:
//		This piece of code replaces:
//    		_w = XtAppInitialize ( &_appContext, 
//			_applicationClass, NULL, 0, 
//		        argc, argv, NULL, NULL, 0 );
//	NOTE #2:
//		The above code constrains This application to have only
//		one screen.  

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspapplication.imake
#define SUBROUTINE dspapplication
#define MODULE_LIST \
		DspApplication.cc  
 
/* #define DEBUG */
        /*!!!! for testing only !!!!*/
#define P2SUB         /* SUBROUTINE */ 
#define P2_SUBLIB
#define USES_C_PLUS_PLUS
#define LIB_FORTRAN
#define LIB_GLLSUB
#define LIB_GUISUB
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
