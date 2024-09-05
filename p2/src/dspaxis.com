$!****************************************************************************
$!
$! Build proc for MIPL module dspaxis
$! VPACK Version 1.8, Monday, April 01, 1996, 21:52:46
$!
$! Execute by entering:		$ @dspaxis
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
$ write sys$output "*** module dspaxis ***"
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
$ write sys$output "Invalid argument given to dspaxis.com file -- ", primary
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
$   if F$SEARCH("dspaxis.imake") .nes. ""
$   then
$      vimake dspaxis
$      purge dspaxis.bld
$   else
$      if F$SEARCH("dspaxis.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspaxis
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspaxis.bld "STD"
$   else
$      @dspaxis.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspaxis.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspaxis.com -mixed -
	-s DspAxisBasicView.cc DspAxisVertView.cc DspAxisHorizView.cc -
	-i dspaxis.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspAxisBasicView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// DspAxisBasicView.cc:
////////////////////////////////////////////////////////
#include "DspAxisBasicView.h"


// Resources for this class

XtResource DspAxisBasicView::_resources [ ] = {
 
// {
//    "drawOffset",
//    "DrawOffset",
//    XmRDimension,
//    sizeof ( Dimension ),
//    XtOffset ( DspAxisBasicView *, _drawOffset ),
//    XmRString,
//    ( XtPointer ) "3",
// },
 {
    "longTickLength",
    "LongTickLength",
    XmRDimension,
    sizeof ( Dimension ),
    XtOffset ( DspAxisBasicView *, _longTickLength ),
    XmRString,
    ( XtPointer ) "7",
 },
  {
    "numbTicks",
    "NumbTicks",
    XmRInt,
    sizeof ( int ),
    XtOffset ( DspAxisBasicView *, _numTicks ),
    XmRString,
    ( XtPointer ) "21",
 },
};

DspAxisBasicView::DspAxisBasicView ( Widget parent, char *name, 
					   Widget insideWidget )
					 : DspUIComponent(name)
{
	_insideWidget = insideWidget;
	_w = XtVaCreateWidget ( _name,
                                xmFormWidgetClass, parent,
                                NULL );
	installDestroyHandler();
	
	getResources ( _resources, XtNumber ( _resources ) );

        _ruler  = XtVaCreateManagedWidget ( "ruler",
                                        xmDrawingAreaWidgetClass, _w,
					XmNleftAttachment,	XmATTACH_FORM,
                     			XmNtopAttachment,     	XmATTACH_FORM,
                     			XmNrightAttachment,   	XmATTACH_FORM,
                     			XmNbottomAttachment,  	XmATTACH_FORM,
					XmNborderWidth, 0,
                                        NULL );


	XtAddCallback ( _ruler, XmNexposeCallback,
                        &DspAxisBasicView::displayCallback,
                        ( XtPointer ) this );


	XGCValues values;
	values.line_width = 1; 
	values.foreground = XWhitePixelOfScreen( XtScreen( _w) ) ;
	unsigned long valueMask = GCLineWidth | GCForeground;
	_gc = XtGetGC ( _w, valueMask,&values); 
}
DspAxisBasicView::~DspAxisBasicView ()
{
	RemoveFormAttachments( _ruler );
	if ( _w && _gc )
		XtReleaseGC ( _w, _gc );
	XtRemoveCallback ( _ruler, XmNexposeCallback,
                        &DspAxisBasicView::displayCallback,
                        ( XtPointer ) this );
}
void  DspAxisBasicView::displayCallback ( Widget,
                                 XtPointer clientData,
                                 XtPointer )
{
        DspAxisBasicView *obj = ( DspAxisBasicView * ) clientData;
        obj->display();
}

 


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DspAxisVertView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// DspAxisVertView.cc:
////////////////////////////////////////////////////////
#include "DspAxisVertView.h"

DspAxisVertView::DspAxisVertView ( Widget parent, char *name, 
			  Widget insideWidget ) 
			: DspAxisBasicView (parent, name, insideWidget) 
{
	XtVaSetValues ( _ruler, XmNwidth, (_longTickLength+1), NULL );
}
void DspAxisVertView::display( )
{
   if (XtIsRealized(_w) ) {
   
        Dimension	insideLength;
        XtVaGetValues ( _insideWidget,
                        XmNheight,  &insideLength,
                        NULL );  
        XtVaSetValues ( _ruler, XmNheight, insideLength, NULL );    
                             
//	SET HORIZ OFFSET RELATIVE TO FORM
	XtVaSetValues( _w, 
        		XmNtopWidget,	   	XtParent(_w),
        		XmNtopAttachment, 	XmATTACH_WIDGET,
        		//XmNverticalSpacing,     _leftOrTop,
        		XmNtopOffset,		((int)_leftOrTop),
        		NULL );
                                                 		        		 
//	DRAW LINE CLOSEST TO INSIDE WIDGET  
	Widget 		leftW, rightW;
	XtVaGetValues ( _w,  XmNleftWidget,  &leftW, 
        				XmNrightWidget, &rightW,
        				NULL );        				       				      				
	        		
// 	DRAW TICKS
	int 	stepSize= int(insideLength-1) / (_numTicks - 1);
	int 	excess 	= int(insideLength-1) % (_numTicks - 1);
		
	double 	temp1 	= (double)excess / (double)(_numTicks - 1);
	double 	temp2 = temp1;
	
	int	pointY;
	int	pointX1 = 0;
	int	pointX2 = 0 + _longTickLength;
	
	for ( int i=0; i<_numTicks; ++i) {

	   pointY = int((i*stepSize)+temp1);
	   XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc, 
				pointX1, pointY,pointX2,pointY);
	   temp1 = temp1 + temp2;
        }             
   }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DspAxisHorizView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// DspAxisHorizView.cc:
////////////////////////////////////////////////////////
#include "DspAxisHorizView.h"

DspAxisHorizView::DspAxisHorizView ( Widget parent, char *name, 
			  Widget insideWidget ) 
			: DspAxisBasicView (parent, name, insideWidget) 
{
   XtVaSetValues ( _ruler, XmNheight, (_longTickLength+1), NULL );
}
void DspAxisHorizView::display( )
{
   if (XtIsRealized(_w) ) {      
   
        Dimension	insideLength;
        XtVaGetValues ( _insideWidget,
                        XmNwidth,  &insideLength,
                        NULL );  
        XtVaSetValues ( _ruler, XmNwidth, insideLength, NULL );   
        
        
//	SET HORIZ OFFSET  TO FORM
	XtVaSetValues( _w, 
        		XmNleftWidget,	   	XtParent(_w),
        		XmNleftAttachment, 	XmATTACH_WIDGET,
        		//XmNhorizontalSpacing,   _leftOrTop,
        		XmNleftOffset,		((int)_leftOrTop),
        		NULL );
        
                              
                          		
        		 
//	DETERMINE TOP OR BOTTOM ATTACHMENT TO INSIDE LENGTH   
	Widget 		topW, bottomW;
	XtVaGetValues ( _w,  XmNtopWidget,  &topW, 
        				XmNbottomWidget, &bottomW,
        				NULL );
	        		
// 	DRAW TICKS
	int 	stepSize= int(insideLength-1) / (_numTicks - 1);
	int 	excess 	= int(insideLength-1) % (_numTicks - 1);
		
	double 	temp1 	= (double)excess / (double)(_numTicks - 1);
	double 	temp2 = temp1;
	
	int	pointX;
	int	pointY1 = 0;
	int	pointY2 = 0 + _longTickLength;
	
	for ( int i=0; i<_numTicks; ++i) {
	   pointX = int((i*stepSize)+temp1 );
	   XDrawLine (XtDisplay(_ruler), XtWindow(_ruler), _gc, 
				pointX, pointY1,pointX,pointY2);
	   temp1 = temp1 + temp2;
        }
   }
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspaxis.imake
#define SUBROUTINE dspaxis
#define MODULE_LIST \
		DspAxisBasicView.cc DspAxisHorizView.cc DspAxisVertView.cc 
 
/* #define DEBUG */        /*!!!! for testing only !!!!*/
 
#define P2SUB         /* SUBROUTINE */ 
#define P2_SUBLIB
#define USES_C_PLUS_PLUS
#define LIB_FORTRAN
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_GUI



$ Return
$!#############################################################################
