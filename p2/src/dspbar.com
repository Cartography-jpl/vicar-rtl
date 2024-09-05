$!****************************************************************************
$!
$! Build proc for MIPL module dspbar
$! VPACK Version 1.8, Monday, April 01, 1996, 21:52:47
$!
$! Execute by entering:		$ @dspbar
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
$ write sys$output "*** module dspbar ***"
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
$ write sys$output "Invalid argument given to dspbar.com file -- ", primary
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
$   if F$SEARCH("dspbar.imake") .nes. ""
$   then
$      vimake dspbar
$      purge dspbar.bld
$   else
$      if F$SEARCH("dspbar.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspbar
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspbar.bld "STD"
$   else
$      @dspbar.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspbar.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspbar.com -mixed -
	-s DspBarBasicView.cc DspBarVertView.cc DspBarHorizView.cc -
	-i dspbar.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspBarBasicView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// DspBarBasicView.cc:
////////////////////////////////////////////////////////
#include "DspBarBasicView.h"



DspBarBasicView::DspBarBasicView ( Widget parent, char *name, Widget widgetCap )
					 : DspUIComponent(name)
{
	_widgetCap = widgetCap;
	_w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
                                NULL );
	installDestroyHandler();
	
	 _ruler  = XtVaCreateManagedWidget ( "ruler",
                                        xmDrawingAreaWidgetClass, _w,
					XmNleftAttachment,	XmATTACH_FORM,
                     			XmNtopAttachment,     	XmATTACH_FORM,
                     			XmNrightAttachment,   	XmATTACH_FORM,
                     			XmNbottomAttachment,  	XmATTACH_FORM,
					XmNborderWidth, 0,
                                        NULL );


	XtAddCallback ( _ruler, XmNexposeCallback,
                        &DspBarBasicView::displayCallback,
                        ( XtPointer ) this );


	XGCValues values;
	values.line_width = 1; 
	values.foreground = XWhitePixelOfScreen( XtScreen( _w) ) ;
	unsigned long valueMask = GCLineWidth | GCForeground;
	_gc = XtGetGC ( _w, valueMask,&values);                 

}
DspBarBasicView::~DspBarBasicView ()
{
	RemoveFormAttachments( _ruler );
	if ( _w && _gc )
		XtReleaseGC ( _w, _gc );
	XtRemoveCallback ( _ruler, XmNexposeCallback,
                        &DspBarBasicView::displayCallback,
                        ( XtPointer ) this );
}
void  DspBarBasicView::displayCallback ( Widget,
                                 XtPointer clientData,
                                 XtPointer )
{
        DspBarBasicView *obj = ( DspBarBasicView * ) clientData;
        obj->display();
}

 


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DspBarVertView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// DspBarVertView.cc:
////////////////////////////////////////////////////////
#include "DspBarVertView.h"

DspBarVertView::DspBarVertView ( Widget parent, char *name, Widget widgetCap ) 
			: DspBarBasicView (parent, name, widgetCap) 
{

}
void DspBarVertView::display( )
{

   if (XtIsRealized(_w) ) {  
   
//	SET SIZE OF RULER = SIZE OF WidgetCap   
	Dimension length;
   	XtVaGetValues ( _widgetCap, XmNheight, &length, NULL );
//	length = length -2;
	XtVaSetValues ( _ruler, XmNheight,  length, NULL );    
                          		
//	DRAW LINE				
        XDrawLine ( XtDisplay(_ruler), XtWindow(_ruler), _gc, 
			0,  0, 
			0, (int) length );
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DspBarHorizView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// DspBarHorizView.cc:
////////////////////////////////////////////////////////
#include "DspBarHorizView.h"

DspBarHorizView::DspBarHorizView ( Widget parent, char *name, Widget widgetCap ) 
			: DspBarBasicView (parent, name, widgetCap) 
{

}
void DspBarHorizView::display( )
{
   if (XtIsRealized(_w) ) {  
   
//	SET SIZE OF RULER = SIZE OF WIDGETCAP 
	Dimension length;
   	XtVaGetValues ( _widgetCap, XmNwidth, &length, NULL );
//	length = length - 5; 
	XtVaSetValues ( _ruler, XmNwidth,  length, NULL );    		                	               	
	                		
//	DRAW LINE				
        XDrawLine ( XtDisplay(_ruler), XtWindow(_ruler), _gc, 
			0,  0, 
			(int) length, 0 );
   }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspbar.imake
#define SUBROUTINE dspbar
#define MODULE_LIST \
		DspBarBasicView.cc DspBarHorizView.cc DspBarVertView.cc 
 
/* #define DEBUG */          /*!!!! for testing only !!!!*/
 
#define P2SUB         /* SUBROUTINE */ 
#define P2_SUBLIB

#define USES_C_PLUS_PLUS
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_GUI



$ Return
$!#############################################################################
