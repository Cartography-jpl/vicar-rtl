$!****************************************************************************
$!
$! Build proc for MIPL module dspwindow
$! VPACK Version 1.8, Thursday, October 17, 1996, 01:18:47
$!
$! Execute by entering:		$ @dspwindow
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
$ write sys$output "*** module dspwindow ***"
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
$ write sys$output "Invalid argument given to dspwindow.com file -- ", primary
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
$   if F$SEARCH("dspwindow.imake") .nes. ""
$   then
$      vimake dspwindow
$      purge dspwindow.bld
$   else
$      if F$SEARCH("dspwindow.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspwindow
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspwindow.bld "STD"
$   else
$      @dspwindow.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspwindow.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspwindow.com -mixed -
	-s DspWindowBasic.cc -
	-i dspwindow.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspWindowBasic.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// 	DspWindowBasic.cc: The base class for realtime and browse
//		      top-level window
//		NOTE:  The design should be to delete a display so
//		we can move to a different device.  But GUI code
//		use to crash - probably still does - when this
//		display code tries to delete. Now this code 
//		may not be able to delete it's own objects.
////////////////////////////////////////////////////////////////
#include "DspWindowBasic.h"

Boolean DspWindowBasic::_firstDisplay = TRUE;


XtResource DspWindowBasic::_resources [ ] = {
  {
    "readFrequency",
    "ReadFrequency",
    XmRInt,
    sizeof(int),
    XtOffset( DspWindowBasic *, _readFrequency),
    XmRString,
    (XtPointer) "1500",
 },
// {
//    "windowBorder",
//    "windowBorder",
//    XmRBoolean,
//    sizeof(Boolean),
//    XtOffset( DspWindowBasic *, _windowBorder),
//    XmRString,
//    (XtPointer) "on",
//  }
};

DspWindowBasic::~DspWindowBasic()
{
//	DELETE WORKAREA & FORM
	delete []_instrumentDisplayer;
	XtDestroyWidget( _form );

//	REMOVE TIMEOUT
	if ( _id ) XtRemoveTimeOut ( _id );
}
/////////////////////////////////////////////////////////
// 	createWorkArea
/////////////////////////////////////////////////////////
Widget DspWindowBasic::createWorkArea( Widget parent)
{
	_prevInstrument = dsp_invalid;	
	
// 	INIT VARIABLES
	_blankCursor = (Cursor) NULL;
        getResources(_resources, XtNumber( _resources) );
        int n = (int) dsp_invalid;
        _instrumentDisplayer = new DspInstrBasic* [n];
        for (int i = 0; i<n; i++ )
        	_instrumentDisplayer[i] = NULL;

//	CREATE FRAME TO HOLD ALL WIDGETS
	_form = XtVaCreateManagedWidget( _name, 
			xmFrameWidgetClass, parent, 
			XmNwidth, 640,
			XmNheight, 480,
			XmNshadowThickness, 0,
			NULL);

// 	DISABLE AUTO RESIZE
	Widget topLevel = (Widget) _form;
	while ((!XtIsShell(topLevel)) && (XtParent(topLevel) != NULL))
                topLevel = XtParent(topLevel);
	XtVaSetValues(topLevel, XtNallowShellResize, FALSE, NULL);
	
//      REMOVE BORDER ON SHELL, IF RESOURCE SET
        setShellBorder();
 
// 	SET UP TO CALL CREATEINSTRUMENTDISPLAYINWORKAREA (IN SUBCLASS)
	setup();

//	RETURN
   	return (_form);	
}

/****
/////////////////////////////////////////////////////////
// 	createInstrumentDisplayInWorkArea
// 		CALL BY SUBCLASS WHEN DATA ARRIVES
/////////////////////////////////////////////////////////
void DspWindowBasic::createInstrumentDisplayInWorkArea()
{

// 	CREATE IMAGE WINDOW (IF NOT ALREADY CREATED)
	int i         = (int) _instrument;
	
	if (_instrumentDisplayer[i] == NULL) {
	    if (((int)_instrument) == gll_dsp_ssi)	
		_instrumentDisplayer[i] = new DspInstrSsi( _form, "ssiDisplayer", 
								(DspWindowBasic *) this );
	    if (((int)_instrument) == gll_dsp_pws)
		_instrumentDisplayer[i] = new DspInstrPws( _form, "pwsDisplayer", 
								(DspWindowBasic *) this );
	    //if (((int)_instrument) == gll_dsp_nims_eng)	
	    //	_instrumentDisplayer[i] = new DspInstrNimEng( _form, "nimsDisplayerEng", 
	    //							(DspWindowBasic *) this ); //uses blank filename	
	    if (((int)_instrument) == gll_dsp_nims_sci)	
		_instrumentDisplayer[i] = new DspInstrNimSci( _form, "nimsDisplayerSci", 
								(DspWindowBasic *) this ); // uses blank filename
	    if (((int)_instrument) ==  mpf_dsp_rover_front) {
	    	_instrumentDisplayer[i] = new DspInstrMpfRvRt( _form, "mpfRvRtDisplayerSci", 
	    						(DspWindowBasic *) this );
	    }
	    if (((int)_instrument) ==  mpf_dsp_rover_rear) {
	    	_instrumentDisplayer[i] = new DspInstrMpfRvRr( _form, "mpfRvRrDisplayerSci",
	    							(DspWindowBasic *) this ); 
	    }
	    if (((int)_instrument) ==  mpf_dsp_apxs) {
	    	_instrumentDisplayer[i] = new DspInstrMpfApxs( _form, "mpfApxsDisplayerSci", 
	    							(DspWindowBasic *) this );
	    }
	}
		
// 	UNMANAGE PREVIOUS DISPLAY
	int max_instr = (int) dsp_invalid;
	for ( int n = 0; n < max_instr; n++ )
	       if ( (_instrumentDisplayer[n] != NULL) && (_instrumentDisplayer[n]->isManaged()) 
	             && (_prevInstrument != _instrument )) 
			_instrumentDisplayer[n]->unmanage();

//	MANAGE THE NEW DISPLAY
	if ((_instrumentDisplayer[i] != NULL) && (_instrument != dsp_invalid )) {
	     _instrumentDisplayer[i]->manage(); 
	     _prevInstrument = _instrument; 
	     _instrumentDisplayer[i]->update();
	}
}
****/
/////////////////////////////////////////////////////////
// 	setShellBorder (non-public) .. called from createWorkArea
//	    old way:
// 		removes motif border if resource set to "off".
//	    new way:
//		gets the info fromm gll_dsp_cntrl
// 	    Most likely to be used in r/t on small screens.
//	    
/////////////////////////////////////////////////////////
void DspWindowBasic::setShellBorder()
{
        Widget shell = _w;

//	FIND SHELL WIDGET        
          if (shell && !XtIsShell(shell)) {
                do {
                        shell = XtParent(shell);
                } while (shell && !XtIsShell(shell));
          } 
 
        
//     	GET SCREEN HEIGHT AND WIDTH 
	  int screenHt = XDisplayHeight(XtDisplay(shell),
                                       XDefaultScreen(XtDisplay(shell)));                               
          int screenWth = XDisplayWidth(XtDisplay(shell),
                                            XDefaultScreen(XtDisplay(shell)));
        
                
//	IF RESOURCE SET BORDER TO OFF      
//        if (_windowBorder == FALSE ) {
//??	  if (getBorderMode()) {
	  if ((screenHt < 550) || (screenWth < 650)) { 
        
          
//	   REMOVE BORDER          
           if (shell != NULL)
                XtVaSetValues(shell, XmNmwmDecorations, 0, NULL);
        }
}
///////////////////////////////////////////////////////////////
//	updateAllDisplays:
//		updates display and sets a timeout to come back 
//		
///////////////////////////////////////////////////////////////
void DspWindowBasic::updateAllDisplays()
{
	
// 	BLANK CURSOR IN REST OF DISPLAY WINDOW (in case it moves)
	if (_blankCursor != (Cursor) NULL )
	    if (XtIsRealized(_form)) 	 
	   	XDefineCursor( XtDisplay(_form), XtWindow(_form), _blankCursor);

//	IF THIS IS FIRST TIME THROUGH, 
//	WE MAY HAVE INFO FROM CALL TO INITDISPLAY FROM DSPAPPLICATION
	if (_firstDisplay) {
		_firstDisplay = FALSE;
		if (_newDataArrived == 0) 
			_newDataArrived = getNextFrame();
		if (_newDataArrived != 0) {
			createInstrumentDisplayInWorkArea();
		}
	}

// 	IF THIS IS NOT THE FIRST TIME CALLED && 
// 	DATA HAS ARRIVED.... CREATE DISPLAY 
	else {
	 	_newDataArrived = getNextFrame();
		if ( _newDataArrived != 0 ) {
			createInstrumentDisplayInWorkArea();
		}
	}	

// 	SET APP TIMEOUT TO READ LATER
	_id  = XtAppAddTimeOut ( XtWidgetToApplicationContext ( _form ),
                        _readFrequency,
                         &DspWindowBasic::timeoutCallback,
                         (XtPointer) this );
}	
///////////////////////////////////////////////////////////////
//	TIMEOUT CALLBACK
///////////////////////////////////////////////////////////////
void DspWindowBasic::timeoutCallback(  XtPointer clientData, XtIntervalId * )
{
    	DspWindowBasic *obj = (DspWindowBasic *) clientData;
    	obj->updateAllDisplays();
}

 
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspwindow.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE dspwindow
/*
/*   To Create the build file give the command:
/*
/*		$ vimake dspwindow			(VMS)
/*   or
/*		% vimake dspwindow			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE dspwindow		/* Only one of these */
/*#define PROCEDURE dspwindow		/* Only one of these */
/*#define PROGRAM dspwindow		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST	DspWindowBasic.cc

/***
#define MAIN_LANG_C
/**/

#define USES_C_PLUS_PLUS

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB
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
/**********  End of dspwindow imake file  **********/
$ Return
$!#############################################################################
