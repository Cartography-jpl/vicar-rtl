$!****************************************************************************
$!
$! Build proc for MIPL module dsphist
$! VPACK Version 1.8, Thursday, October 17, 1996, 01:18:46
$!
$! Execute by entering:		$ @dsphist
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
$ write sys$output "*** module dsphist ***"
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
$ write sys$output "Invalid argument given to dsphist.com file -- ", primary
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
$   if F$SEARCH("dsphist.imake") .nes. ""
$   then
$      vimake dsphist
$      purge dsphist.bld
$   else
$      if F$SEARCH("dsphist.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dsphist
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dsphist.bld "STD"
$   else
$      @dsphist.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dsphist.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dsphist.com -mixed -
	-s DspHistBasic.cc DspHistPlain.cc -
	-i dsphist.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspHistBasic.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  DspHistBasic.cc
///////////////////////////////////////////////////////////////////
#include "DspHistBasic.h"

///////////////////////////////////////////////////////////////////
//	Resource Defaults  .. widget resources
/////////////////////////////////////////////////////////////////
String DspHistBasic::_defaults [ ] = {
	"*wedgeView.stretchPolicy:              XvicUSE_SW",
	"*wedgeView.colormapPolicy:             XvicALLOC",
	"*wedgeView.lutType:                    XvicRAW",
	"*wedgeView.grayLevels:			128",
	"*wedgeView.scrollBarDisplayPolicy:     XvicNEVER",
	"*wedgeView.shadowThickness:		0",
	"*wedgeView.highlightThickness:		0",	
	"*wedgeView*orientation:          	HORIZONTAL",
	"*wedgeView*nsteps:               	256",
	"*wedgeView*minPixelDN:           	0",
	"*wedgeView*maxPixelDN:           	255",
	"*wedgeFrame.marginWidth:		0"
	"*wedgeFrame.marginHeight:		0"	
	"*wedgeFrame.topShadowColor:		white",
	"*wedgeFrame.bottomShadowColor:		white",				
	"*wedgeFrame.shadowThickness:		1",
	"*titleString.fontList:			-*-*-*-r-normal-*-14-*-*-*-*-*-*-*",
	"*histView.height:			100",
	"*histView.width:			128",
	"*histView.redColor:                	white",
	"*titleString.marginWidth:		0",
	"*axisNorth*longTickLength:		4",
	"*axisSouth*longTickLength:		5",		
	"*axisEast*longTickLength:		4",
	"*axisWest*longTickLength:		4",
	"*histFrame.shadowThickness:		1",
	"*histFrame.topShadowColor:		white",
	"*histFrame.bottomShadowColor:		white",			
 	"*histFrame.marginWidth:	  	0",
	"*histFrame.marginHeight:	  	0",
	"*axisSouth*numbTicks:			9",
	"*axisNorth*numbTicks:			9",
	"*axisEast*numbTicks:			5",
	"*axisWest*numbTicks:			5",
       NULL,
};

///////////////////////////////////////////////////////////////////
//	Constructor:
///////////////////////////////////////////////////////////////////
DspHistBasic::DspHistBasic(	Widget 		parent, 
						char * 		name, 
						ImageData * dataModel, 
						char *		titleLabel,
					//// int (*fp_getHistData)(),
						DspDisplayPolicy * displayPolicy)	 
						: DspUIComponent(name)
{
//	SAVE ARGS
	  _titleLabel = titleLabel;
	  _dataModel = dataModel;
    ///////_fp_getHistData = fp_getHistData;
	  _displayPolicy = displayPolicy;
	  
//	SET DEFAULT RESOURCES
	  setDefaultResources ( parent, _defaults );
	
// 	CREATE A FORM TO HOLD HIST, FRAME, AXIS, WEDGE	
	  int n = 0;
	  Arg args[20];
	  XtSetArg(args[n], XmNmarginHeight,  0); n++;
	  XtSetArg(args[n], XmNmarginWidth, 0); n++;
	  _w = XtCreateWidget( _name, xmFormWidgetClass, parent, args, n);
       	  installDestroyHandler();
       	  
       	  
     /////createAllViews();
}
///////////////////////////////////////////////////////////////////
//	create: frame, axis, wedge, hist(in subclass)
///////////////////////////////////////////////////////////////////
void DspHistBasic::createAllViews()
{   
//	DISPLAY LABEL	  
          XmString xmstr = XmStringCreateSimple (_titleLabel);
          _titleString = XtCreateManagedWidget( "titleString", xmLabelWidgetClass, _w, NULL, 0);
          XtVaSetValues ( _titleString,		
          		XmNlabelString,		xmstr,
            		XmNtopAttachment,	XmATTACH_FORM,		
			XmNleftAttachment, 	XmATTACH_FORM,
          		XmNrightAttachment,	XmATTACH_FORM,
			XmNalignment,		XmALIGNMENT_CENTER,
                	NULL );       
          XmStringFree (xmstr); 

//	CREATE FORM FOR HOLDING AXIS, BAR, HISTOGRAM 
	  _histForm = XtCreateManagedWidget( "histForm", xmFormWidgetClass, _w, NULL, 0);	  
	  XtVaSetValues( _histForm, 
        		XmNleftAttachment, 	XmATTACH_FORM,
        		XmNrightAttachment,	XmATTACH_FORM,
        		XmNtopWidget,		_titleString,
               		XmNtopAttachment,    	XmATTACH_WIDGET,
        		NULL );	 
        				  	  	  	  
//	CREATE BASIC FRAMED HISTOGRAM
	  _histFrame = XtCreateManagedWidget( "histFrame", xmFrameWidgetClass, _histForm, NULL, 0);
	  createHistView(_histFrame);
	   		   	   	   
//	CREATE AXIS AROUND FRAMED HISTOGRAM 
	   _axisWest  = new DspAxisVertView(  _histForm, "axisWest",_histFrame);
	   _axisSouth = new DspAxisHorizView( _histForm, "axisSouth",_histFrame);
	   _axisEast  = new DspAxisVertView(  _histForm, "axisEast", _histFrame);
	   _axisNorth = new DspAxisHorizView( _histForm, "axisNorth",_histFrame);	
	   _axisWest->setOffset(  _axisNorth->getTickLength()+4 ); 
	   _axisEast->setOffset(  _axisNorth->getTickLength()+4 ); 
	   _axisNorth->setOffset( _axisWest->getTickLength()+1 ); 
	   _axisSouth->setOffset( _axisWest->getTickLength()+1); 	   
	   _axisWest->manage(); 
	   _axisSouth->manage();
	   _axisEast->manage();
	   _axisNorth->manage();	
	   
//	CREATE BAR OVER AXIS)
	  _barNorth = new DspBarHorizView( _histForm, "frameNorth", _axisNorth->baseWidget());	
	  XtVaSetValues (_barNorth->baseWidget(),
	   	XmNtopAttachment,	XmATTACH_FORM,
	  	XmNtopOffset,		2,	// TOP FRAME BAR GETS CUTOFF 
	  	XmNleftAttachment,	XmATTACH_FORM,
	  	XmNleftOffset,		((int) _axisWest->getTickLength()+1 ),
	 	NULL );	   
	  _barNorth->manage(); 	 	 	
	 	  		     	           		                                                	   	   	   
// 	ATTACH AXES 
          XtVaSetValues ( _axisNorth->baseWidget(),	// TOP AXIS
               XmNtopWidget,		_barNorth->baseWidget(),
               XmNtopAttachment,    	XmATTACH_WIDGET,
               NULL );
	   
         XtVaSetValues ( _axisEast->baseWidget(),	// RIGHT AXIS
               XmNrightAttachment,    	XmATTACH_FORM,
	       NULL );
               
         XtVaSetValues ( _axisWest->baseWidget(),	// LEFT AXIS
               XmNleftAttachment,     	XmATTACH_FORM,
               NULL );    
                               
	XtVaSetValues ( _histFrame,
		XmNtopWidget,		_axisNorth->baseWidget(),
		XmNtopAttachment,	XmATTACH_WIDGET,	
		XmNleftWidget,		 _axisWest->baseWidget(),
		XmNleftAttachment,	XmATTACH_WIDGET,	
		XmNrightWidget,          _axisEast->baseWidget() ,   
		XmNrightAttachment,	XmATTACH_WIDGET,
		NULL );	
		
         XtVaSetValues ( _axisSouth->baseWidget(),	// Bottom AXIS
	 	XmNtopWidget,		_histFrame,
                XmNtopAttachment,     	XmATTACH_WIDGET,
                NULL);        		 			
		
//	CREATE WEDGE 
	  int leftspacing = (int) (_axisWest->getTickLength() );
          int n = 0;
	  Arg args[20];
	  XtSetArg(args[n], XmNmarginHeight,  0); n++;
	  XtSetArg(args[n], XmNmarginWidth, 0); n++;
	  _wedgeFrame = XtCreateManagedWidget( "wedgeFrame", xmFrameWidgetClass, _w, args, n);
	  _wedgeView = new WedgeView( _wedgeFrame,  "wedgeView", 6, 128 );
	  _wedgeView->manage();
	  

//	ATTACH WEDGE UNDER HISTOGRAM & AXIS (UNDER _HISTFORM)
	  XtVaSetValues( _wedgeFrame, 
        		XmNleftAttachment, 	XmATTACH_FORM,
        		XmNleftOffset,     	leftspacing + 1,
        	 	XmNbottomAttachment,	XmATTACH_FORM,
        		XmNtopWidget,		_histForm,
               		XmNtopAttachment,    	XmATTACH_WIDGET,
        		NULL );	 		
			 	            
}
///////////////////////////////////////////////////////////////////
//	createHistView
///////////////////////////////////////////////////////////////////
void DspHistBasic::createHistView(Widget parent)
{
	   _histModel = new Histogram(0, 255, 2);
	   
	   int * histArray = getHistArray();
   //// int * histArray = (*fp_getHistData)();
	   for (int i = 0; i< _histModel->numBins(); i++ ) {
	   	_histModel->setBin( i, histArray[i] );
	   }	   
 	   _histView = new HistGraphView( parent,"histView", _histModel,
 	   					NULL,NULL,BLEND,VERTICAL,ASC);
	   _histView->manage();
}
///////////////////////////////////////////////////////////////////
//	Destructor
///////////////////////////////////////////////////////////////////
DspHistBasic::~DspHistBasic()
{
         RemoveFormAttachments( _histView->baseWidget());
	 RemoveFormAttachments( _wedgeView->baseWidget());
	 RemoveFormAttachments( _axisWest->baseWidget());
	 RemoveFormAttachments( _axisSouth->baseWidget());
	 RemoveFormAttachments( _axisEast->baseWidget());
	 RemoveFormAttachments( _axisNorth->baseWidget());
	 RemoveFormAttachments( _barNorth->baseWidget());
	 
        delete _histView;
	delete _wedgeView;
	delete _axisWest;
	delete _axisSouth;
	delete _axisEast;
	delete _axisNorth;
	delete _barNorth;
	XtDestroyWidget( _histForm) ;
	XtDestroyWidget( _titleString);
	delete _histModel;
}
///////////////////////////////////////////////////////////////////
//	updateHistView
///////////////////////////////////////////////////////////////////
void DspHistBasic::update()
{
//	GET NEW HISTOGRAM AND REDISPLAY
	int * histArray = getHistArray();
	 //// int * histArray = (*fp_getHistData)();
	for (int i = 0; i < _histModel->numBins(); i++ ) {  
		_histModel->setBin( i, histArray[i] );
	}	
	_histModel->updateViews();	   	   
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DspHistPlain.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  DspHistPlain.cc
///////////////////////////////////////////////////////////////////
#include "DspHistPlain.h"
#include "HistLogVerAxis.h"

///////////////////////////////////////////////////////////////////
//	Resource Defaults  .. widget resources
/////////////////////////////////////////////////////////////////
String DspHistPlain::_defaults [ ] = {
	"*titleString.fontList:			-misc-fixed-medium-r-normal-*-13-*-*-*-*-*-*-*",
	"*histView.height:			200",
	"*histView.width:			256",
	"*histView.redColor:                	white",
	"*titleString.marginWidth:		0",
	"*axisNorth*longTickLength:		4",
	"*axisSouth*longTickLength:		5",		
	"*axisEast*longTickLength:		4",
	"*axisWest*longTickLength:		4",
	"*histFrame*shadowThickness:		1",
	"*histFrame*topShadowColor:		white",
	"*histFrame*bottomShadowColor:		white",			
 	"*histFrame*marginWidth:	  	0",
	"*histFrame*marginHeight:	  	0",
	"*axisSouth*numbTicks:			9",
	"*axisNorth*numbTicks:			9",
	"*axisEast*numbTicks:			5",
	"*axisWest*numbTicks:			5",
       NULL,
};

///////////////////////////////////////////////////////////////////
//	Constructor:
///////////////////////////////////////////////////////////////////
DspHistPlain::DspHistPlain(	Widget 		parent, 
						char * 		name, 
						ImageData * dataModel, 
						char *		titleLabel,
					//// int (*fp_getHistData)(),
						DspDisplayPolicy * displayPolicy,
						int		displayLabel)	 
						: DspUIComponent(name)
{
//	SAVE ARGS
	  _titleLabel = titleLabel;
	  _dataModel = dataModel;
	  _displayPolicy = displayPolicy;
	_displayLabel = displayLabel;
	getHistArrayfunc = NULL;
	  
//	SET DEFAULT RESOURCES
	  setDefaultResources ( parent, _defaults );
	
// 	CREATE A FORM TO HOLD HIST, FRAME, AXIS, WEDGE	
	  int n = 0;
	  Arg args[20];
	  XtSetArg(args[n], XmNmarginHeight,  0); n++;
	  XtSetArg(args[n], XmNmarginWidth, 0); n++;
	  _w = XtCreateWidget( _name, xmFormWidgetClass, parent, args, n);
       	  installDestroyHandler();
       	  
}
///////////////////////////////////////////////////////////////////
//	create: frame, axis, wedge, hist(in subclass)
///////////////////////////////////////////////////////////////////
void DspHistPlain::createAllViews()
{   
//	CREATE FORM FOR HOLDING HISTOGRAM 
	  _histForm = XtCreateManagedWidget( "histForm", xmFormWidgetClass, _w, NULL, 0);	  
//	DISPLAY LABEL	  
	if(_displayLabel) {
          XmString xmstr = XmStringCreateSimple (_titleLabel);
          _titleString = XtCreateManagedWidget( "titleString", xmLabelWidgetClass, _w, NULL, 0);
	  if(_displayLabel & 1) {		// label to left
          		XtVaSetValues ( _titleString,		
          				XmNlabelString,		xmstr,
            				XmNtopAttachment,	XmATTACH_FORM,		
					XmNleftAttachment, 	XmATTACH_FORM,
          				XmNbottomAttachment,	XmATTACH_FORM,
					XmNalignment,		XmALIGNMENT_BEGINNING,
                			NULL );       
	  		XtVaSetValues( _histForm, 
        				XmNleftAttachment, 	XmATTACH_WIDGET,
					XmNleftWidget,		_titleString,
        				XmNrightAttachment,	XmATTACH_FORM,
               				XmNtopAttachment,    	XmATTACH_FORM,
               				XmNbottomAttachment,    	XmATTACH_FORM,
        				NULL );	 
	  } else if(_displayLabel & 2) {		// label to right
	  		XtVaSetValues( _histForm, 
        				XmNleftAttachment, 	XmATTACH_FORM,
               				XmNtopAttachment,    	XmATTACH_FORM,
               				XmNbottomAttachment,    XmATTACH_FORM,
        				NULL );	 
          		XtVaSetValues ( _titleString,		
          				XmNlabelString,		xmstr,
            				XmNtopAttachment,	XmATTACH_FORM,		
					XmNleftAttachment, 	XmATTACH_WIDGET,
					XmNleftWidget,		_histForm,
          				XmNrightAttachment,	XmATTACH_FORM,
          				XmNbottomAttachment,	XmATTACH_FORM,
//					XmNalignment,		XmALIGNMENT_BEGINNING,
					XmNalignment,		XmALIGNMENT_END,
                 			NULL );       
	  } else if(_displayLabel & 4) {		// label to top
          		XtVaSetValues ( _titleString,		
          				XmNlabelString,		xmstr,
            				XmNtopAttachment,	XmATTACH_FORM,		
					XmNleftAttachment, 	XmATTACH_FORM,
          				XmNrightAttachment,	XmATTACH_FORM,
					XmNheight,		24,
					XmNalignment,		XmALIGNMENT_BEGINNING,
                			NULL );       
	  		XtVaSetValues( _histForm, 
        				XmNleftAttachment, 	XmATTACH_FORM,
        				XmNrightAttachment,	XmATTACH_FORM,
               				XmNtopAttachment,    	XmATTACH_WIDGET,
					XmNtopWidget,		_titleString,
               				XmNbottomAttachment,    XmATTACH_FORM,
                			NULL );       
	  } else if(_displayLabel & 8) {		// label to bottom
	  		XtVaSetValues( _histForm, 
        				XmNleftAttachment, 	XmATTACH_FORM,
        				XmNrightAttachment,	XmATTACH_FORM,
               				XmNtopAttachment,    	XmATTACH_FORM,
        				NULL );	 
          		XtVaSetValues ( _titleString,		
          				XmNlabelString,		xmstr,
            				XmNtopAttachment,	XmATTACH_WIDGET,		
					XmNtopWidget,		_histForm,
					XmNleftAttachment, 	XmATTACH_FORM,
          				XmNrightAttachment,	XmATTACH_FORM,
          				XmNbottomAttachment,	XmATTACH_FORM,
					XmNalignment,		XmALIGNMENT_BEGINNING,
//					XmNalignment,		XmALIGNMENT_END,
                			NULL );       
	  } else if(_displayLabel & 16) {
			// bad idea for now
	}
        XmStringFree (xmstr); 
	} else {
	XtVaSetValues( _histForm, 
        		XmNleftAttachment, 	XmATTACH_FORM,
        		XmNrightAttachment,	XmATTACH_FORM,
               		XmNtopAttachment,    	XmATTACH_FORM,
               		XmNbottomAttachment,    XmATTACH_FORM,
        		NULL );	 
        				  	  	  	  
	}

//	CREATE BASIC FRAMED HISTOGRAM
	_histFrame = XtCreateManagedWidget( "histFrame", 
				xmFrameWidgetClass, _histForm, NULL, 0);
	createHistView(_histFrame);
	   		   	   	   
}
///////////////////////////////////////////////////////////////////
//	createHistView
///////////////////////////////////////////////////////////////////
void DspHistPlain::createHistView(Widget parent)
{
	Widget form = XtCreateManagedWidget("histAndAxisForm",
				xmFormWidgetClass, parent, NULL, 0);
	_histModel = new Histogram(0, 255, 1);
	   
	int * histArray = getHistArray();
	for (int i = 0; i< _histModel->numBins(); i++ ) {
	   	_histModel->setBin( i, histArray[i] );
	}
 	_histView = new HistBox( form,"histView", _histModel);
	_histView->setLogScale(True);
	_histView->showStat(False);
	_histView->setOrientType(VERTICAL);

     if(_displayLabel)
     {	HistAxisView *axisView = new HistLogVerAxis(form, "verAxis", 
				_histModel, NULL, NULL, HORIZONTAL);
	if(_displayLabel & 32)
	{ XtVaSetValues(axisView->baseWidget(), 
			XmNleftAttachment, 	XmATTACH_WIDGET,
			XmNleftWidget, 		_histView->baseWidget(),
        		XmNrightAttachment,	XmATTACH_FORM,
               		XmNtopAttachment,    	XmATTACH_FORM,
               		XmNbottomAttachment,    XmATTACH_FORM,
        		NULL );

	  XtVaSetValues(_histView->baseWidget(), 
			XmNleftAttachment, 	XmATTACH_FORM,
        		//XmNrightAttachment,	XmATTACH_FORM,
               		XmNtopAttachment,    	XmATTACH_FORM,
               		XmNbottomAttachment,    XmATTACH_FORM,
        		NULL );
	} else
	{ XtVaSetValues(_histView->baseWidget(), 
			XmNleftAttachment, 	XmATTACH_WIDGET,
			XmNleftWidget, 		axisView->baseWidget(),
        		XmNrightAttachment,	XmATTACH_FORM,
               		XmNtopAttachment,    	XmATTACH_FORM,
               		XmNbottomAttachment,    XmATTACH_FORM,
        		NULL );
	  XtVaSetValues(axisView->baseWidget(), 
			XmNleftAttachment, 	XmATTACH_FORM,
        		//XmNrightAttachment,	XmATTACH_FORM,
               		XmNtopAttachment,    	XmATTACH_FORM,
               		XmNbottomAttachment,    XmATTACH_FORM,
        		NULL );
	}

	_histView->manage();
        axisView->manage();
     } else
     {	XtVaSetValues(_histView->baseWidget(), 
			XmNleftAttachment, 	XmATTACH_WIDGET,
        		XmNrightAttachment,	XmATTACH_FORM,
               		XmNtopAttachment,    	XmATTACH_FORM,
               		XmNbottomAttachment,    XmATTACH_FORM,
        		NULL );
	_histView->manage();
    }

}
///////////////////////////////////////////////////////////////////
//	Destructor
///////////////////////////////////////////////////////////////////
DspHistPlain::~DspHistPlain()
{
        RemoveFormAttachments( _histView->baseWidget());
        delete _histView;

	XtDestroyWidget( _histForm) ;
	XtDestroyWidget( _titleString);
	delete _histModel;
}
///////////////////////////////////////////////////////////////////
//	updateHistView
///////////////////////////////////////////////////////////////////
void DspHistPlain::update()
{
//	GET NEW HISTOGRAM AND REDISPLAY
	int * histArray = getHistArray();
	 //// int * histArray = (*fp_getHistData)();
	for (int i = 0; i < _histModel->numBins(); i++ ) {  
		_histModel->setBin( i, histArray[i] );
	}	
	_histModel->updateViews();	   	   
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dsphist.imake
#define SUBROUTINE dsphist 
#define MODULE_LIST \
		DspHistBasic.cc DspHistPlain.cc
 
#define P2SUB         /* SUBROUTINE */
#define P2_SUBLIB

/* #define DEBUG */        /*!!!! for testing only !!!!*/
 
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
