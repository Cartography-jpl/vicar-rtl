$!****************************************************************************
$!
$! Build proc for MIPL module dsputil
$! VPACK Version 1.8, Thursday, October 17, 1996, 01:18:47
$!
$! Execute by entering:		$ @dsputil
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
$ write sys$output "*** module dsputil ***"
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
$ write sys$output "Invalid argument given to dsputil.com file -- ", primary
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
$   if F$SEARCH("dsputil.imake") .nes. ""
$   then
$      vimake dsputil
$      purge dsputil.bld
$   else
$      if F$SEARCH("dsputil.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dsputil
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dsputil.bld "STD"
$   else
$      @dsputil.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dsputil.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dsputil.com -mixed -
	-s DspUtilMisc.cc -
	-i dsputil.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspUtilMisc.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//	DspUtilMisc.cc
//	
//		FUTURE:  setStretchForBw, setStretchForColor
///////////////////////////////////////////////////////////////////
#include "rts_inst_display.h"
#include <Xm/Xm.h>


///////////////////////////////////////////////////////////////////
//	CreateBlankCursor
///////////////////////////////////////////////////////////////////
Cursor createBlankCursor(Widget iw)
{
Cursor		blankCursor;
Pixmap 		blankPixmap;
XGCValues	gc_values;
GC		gc;
unsigned int	height, width;
XColor 		black;
Colormap 	colormap;


    
//	CREATE SMALL PIXMAP (1x1) FOR "BLANK" CURSOR
   	width = 1;
   	height = 1;
   	blankPixmap = XCreatePixmap(XtDisplay(iw),
			RootWindowOfScreen(XtScreen(iw)),
			width, height, 1);

//	CREATE TEMP GC
   	gc_values.foreground = 0;
   	gc_values.background = 0;	
   	gc = XCreateGC(XtDisplay(iw), blankPixmap,
		GCForeground | GCBackground, &gc_values);

//	FILL PIXMAP
   	XFillRectangle(XtDisplay(iw), blankPixmap, gc,
		0, 0, width, height);
		
//	SET BACKGROUND & FORGROUND COLORS
	colormap = DefaultColormapOfScreen(XtScreen((Widget)iw));			
	XParseColor(XtDisplay(iw), colormap, "black", &black);
		
//	CREATE CURSOR
   	blankCursor = XCreatePixmapCursor( XtDisplay(iw), 
   		blankPixmap, blankPixmap, &black,  &black, 0,0);

//	DESTROY PIXMAP
	XFreePixmap(XtDisplay(iw), blankPixmap); 
	
//	DISPLAY CURSOR
    	if (XtIsRealized((Widget) iw)) 
		XDefineCursor( XtDisplay(iw), XtWindow(iw), blankCursor);
		
    
//  RETURN
    return (blankCursor);
}
///////////////////////////////////////////////////////////////////
//      fillImageStretchLutWithRaw()
//              set stretch to ramp (IE off)
///////////////////////////////////////////////////////////////////
void   fillImageStretchLutWithRaw( int * lut1)
{
	for (int i = 0; i<256; i++) {
		lut1[i]=i;
	}
	return; // 
}

///////////////////////////////////////////////////////////////////
//      fillRGBImageStretchLutWithRaw()
//              set stretch to ramp (IE off)
///////////////////////////////////////////////////////////////////
void   fillRGBImageStretchLutWithRaw( int * lut1, int *lut2, int *lut3)
{
	fillImageStretchLutWithRaw(lut1);
	fillImageStretchLutWithRaw(lut2);
	fillImageStretchLutWithRaw(lut3);
	return; // 
}

///////////////////////////////////////////////////////////////////
//      setZoomParams2Off
///////////////////////////////////////////////////////////////////
//void    setZoomParams2Off( int * zoomX, int *zoomY, int *panY, int * panX ,int * oddEvenFlag){
void    setZoomParams2Off( int * , int *, int *panY, int * panX ,int * oddEvenFlag){
        *panY = 0;
        *panX = 0;
        *oddEvenFlag = 0;
}
///////////////////////////////////////////////////////////////////
//      setZoomParamsDouble (IE zoom by 2)
///////////////////////////////////////////////////////////////////
void    setZoomParamsDouble( int * zoomX, int *zoomY, int *panY, int * panX ,int * oddEvenFlag){
        *zoomX = 2;
        *zoomY = 2;
        *panY = 0;
        *panX = 0;
        *oddEvenFlag = 0;
}
///////////////////////////////////////////////////////////////////
//      setPseudoModeOff
//              for preventing pseudo mode from coming up in 
//              specified image
///////////////////////////////////////////////////////////////////
int     setPseudoModeOff()
{
        return(0);
}
///////////////////////////////////////////////////////////////////
//      setPseudoModeOn
//              
///////////////////////////////////////////////////////////////////
int     setPseudoModeOn()
{
        return(1);
}
///////////////////////////////////////////////////////////////////
//      
///////////////////////////////////////////////////////////////////
//void    nofillNimsIntBuffer(unsigned int * buf1, unsigned int * buf2, unsigned int * buf3) 
void    nofillNimsIntBuffer(unsigned int * , unsigned int * , unsigned int * ) 
{
	// nop
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dsputil.imake
#define SUBROUTINE dsputil
#define MODULE_LIST \
		DspUtilMisc.cc 
 
/* #define DEBUG */         /*!!!! for testing only !!!!*/
 
#define P2SUB         /* SUBROUTINE */ 
#define P2_SUBLIB

#define USES_C_PLUS_PLUS
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
