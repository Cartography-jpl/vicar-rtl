$!****************************************************************************
$!
$! Build proc for MIPL module dspinstrument
$! VPACK Version 1.9, Friday, April 07, 2000, 15:45:20
$!
$! Execute by entering:		$ @dspinstrument
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
$ write sys$output "*** module dspinstrument ***"
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
$ write sys$output "Invalid argument given to dspinstrument.com file -- ", primary
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
$   if F$SEARCH("dspinstrument.imake") .nes. ""
$   then
$      vimake dspinstrument
$      purge dspinstrument.bld
$   else
$      if F$SEARCH("dspinstrument.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspinstrument
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspinstrument.bld "STD"
$   else
$      @dspinstrument.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspinstrument.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspinstrument.com -mixed -
	-s dspinstrbasic.cc -
	-i dspinstrument.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dspinstrbasic.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//       DspInstrBasic.cc: This class acts as a container for the  
//			major program display components
//
///////////////////////////////////////////////////////////////////
#include "DspWindowBasic.h"
#include "DspInstrBasic.h"
#include "DspDefs.h"
#include "DspFileNameCmd.h"
#include "LoadMenuCmd.h"
#include "PostSingleFileDialogCmd.h"
#include <iostream.h>	
#include <string.h>


///////////////////////////////////////////////////////////////////
//           Resource Defaults  .. widget resources
///////////////////////////////////////////////////////////////////
String DspInstrBasic::_defaults [ ] = {
	"*editMode:				XmMULTI_LINE_EDIT",
        "*cursorPositionVisible:		False",
	"*titleString.shadowThickness:		0",

        "*titleBar*columns: 			53",
        "*titleBar*rows:			1",
        "*titleBar*autoShowCursorPosition:	False",
	"*titleBar*shadowThickness:		0",

        "*imageText*columns: 			68",
        "*imageText*rows:			3",
        "*imageText*autoShowCursorPosition:	False",
	"*imageText*shadowThickness:		0",	

        "*picnoErt*columns: 			15",
        "*picnoErt*rows:			2",
        "*picnoErt*autoShowCursorPosition:	False",
	"*picnoErt*shadowThickness:		0",	

        "*stretchText*columns: 			19",
        "*stretchText*rows:			6",
        "*stretchText*autoShowCursorPosition:	False",
	"*stretchText*shadowThickness:		0",	

	"*histArea.leftOffset:			24",
	"*histArea.rightOffset:			25",
	"*histArea.topOffset:			4",	
	
	"*cursorPosCmd*labelString:             Display Pixel",
	"*cursMatrixCmd*labelString:            Display Pixels",
	"*Stretch*labelString:			Stretch...",
	"*Print Widget Tree*labelString:	Print Widget Tree",

	"*dumpMenuCmdList*cursDumpCmd*labelString:	Start Dump",
	"*dumpMenuCmdList*postFileDialogCmd*labelString:	Assign Dump File Name ...",
	"*dumpMenuCmdList*labelString:		Dump Pixels ...",

	
	"*histDisplayer2.topOffset:		9",
        NULL,
};
///////////////////////////////////////////////////////////////////
//	Constructor
///////////////////////////////////////////////////////////////////
DspInstrBasic::DspInstrBasic(Widget parent, char *name, DspWindowBasic * window,int numbModels ) 
				: DspUIComponent(name)
{
	_window		  = window;
	strcpy(_filename,_window->getFilename());
	_parent	  	  = parent;
	_imageDisplayer   = NULL;
	_histDisplayer1	  = NULL;
	_histDisplayer2   = NULL;
	_rawHistModel 	  = NULL;
	_stretchHistModel = NULL;
	_numbModels	  = numbModels;	
	_dataModel = new ImageData*[ _numbModels ]; // init list
	_pixelSize = 1;  // most of the time size = 1 un.char

}
///////////////////////////////////////////////////////////////////
//	displayAll
//		Called by constructor in subclass
///////////////////////////////////////////////////////////////////
void DspInstrBasic::createDisplay()
{
//	CREATE IMAGE DATA MODEL
	createImageModel(); //      routine in subclass
 
//	SET RESOURCES
	setDefaultResources ( _parent, _defaults );

// 	CREATE TOP WIDGET FOR THIS UICOMPONENT
	_w = XtCreateWidget( _name, xmFormWidgetClass, _parent, NULL, 0);
        installDestroyHandler();
               
//      CREATE IMAGE VIEW (W/WEDGE, IMAGE, AXIS) COMPONENTS
	_imageArea = XtCreateManagedWidget( "imageArea", xmFormWidgetClass, _w, NULL, 0);
	createImageDisplay(_imageArea);  //      routine in subclass
//	_imageArea = createImageDisplay(_w);  //      routine in subclass
	
//      CREATE HISTOGRAM COMPONENTS (W/ 1 or 2 WEDGES, HIST, AXIS)
	_histArea = XtCreateManagedWidget( "histArea", xmFormWidgetClass, _w, NULL, 0);
	createHistDisplay( _histArea );  //      routine in subclass
	
//      CREATE TEXT COMPONENTS
	createTextDisplay();
	
//      SET FORM WIDGET ATTACHMENTS
	this->setAttachments();
	
//	CREATE DEFAULT RAW AND STRETCH HISTS FOR STRETCHCMD IN POPUP
	_rawHistModel = new Histogram(0, 255, 2);
	//CollectHist( _dataModel[0], _rawHistModel, (Histogram *) NULL, (Histogram *) NULL);
	_stretchHistModel = new Histogram(0, 255, 2);
	Lut * lut = (Lut *) _imageDisplayer->getDisplayPolicy(0)->getStretchLut(__red);
	//CollectStretchedHist ( _rawHistModel, _stretchHistModel, lut);
					
// 	CREATE POPUP MENU
	createPopup();
	
}
///////////////////////////////////////////////////////////////////
//	Destructor
///////////////////////////////////////////////////////////////////
DspInstrBasic::~DspInstrBasic()
{
        int i,j;

//	REMOVE MENU AND MENU ITEMS (ATTACHED TO MOUSE BUTTON 3)
	int size = _dumpMenuCmdList->size();
	for (j=0; j<size; j++)
		delete _dumpMenuCmdList->operator[](j);
        size = _cmdList->size();
        for ( i =0; i< size; i++ )
           	delete _cmdList->operator[](i);
        delete _dumpMenuCmdList;
        delete _cmdList; 
//        delete _stretchDialog;
//        delete _stretchCmd;
        delete _fileNameCmd;
        delete _popup;

//      REMOVE ATTACHMENTS ON WIDGETS
	RemoveFormAttachments( _stretchText->baseWidget());	
	RemoveFormAttachments( _imageText->baseWidget() );
	RemoveFormAttachments( _picnoErt->baseWidget() );	
	RemoveFormAttachments( _titleBar->baseWidget());
	RemoveFormAttachments( _imageArea);
	RemoveFormAttachments( _histArea );	
	RemoveFormAttachments( _histDisplayer2->baseWidget());	
	RemoveFormAttachments( _histDisplayer1->baseWidget());	
	RemoveFormAttachments( _imageDisplayer->baseWidget() );
			
//	DELETE IMAGE-VIEWS FROM DISPLAYER	
	int numbImages = _imageDisplayer->getNumbImages();
	for ( i=0; i<numbImages; i++){
		DspDisplayPolicy * displayPolicy = _imageDisplayer->getDisplayPolicy(i);
		delete displayPolicy;
	}
	
//	DELETE HISTOGRAM OBJECTS
	if ( _rawHistModel != NULL ) {
//?		delete _rawHistModel;
//?		_rawHistModel = NULL;
	}
	if ( _stretchHistModel != NULL ) {
//?		delete _stretchHistModel;
//?		_stretchHistModel = NULL;
	}
	
//??	DELETE CMDS and cmdlist, etc.	
	
//?	delete []_dataModel;

	deleteViews();
	
}
///////////////////////////////////////////////////////////////////
//	setAttachments
//		Sets attachments of widgets in display.
//		Called by displayAll() during instantiation.
///////////////////////////////////////////////////////////////////
void DspInstrBasic::setAttachments()
{
//	ATTACH TITLE BAR
	XtVaSetValues( _titleBar->baseWidget(),
	XmNleftAttachment,	XmATTACH_FORM,
	XmNrightAttachment,    	XmATTACH_FORM,                	
	XmNtopAttachment,	XmATTACH_FORM,
	NULL);
	
//	ATTACH IMAGE & WEDGE
	XtVaSetValues( _imageArea,
	XmNtopWidget,		_titleBar->baseWidget(),
	XmNtopAttachment,	XmATTACH_WIDGET,
	XmNleftAttachment,	XmATTACH_FORM,
	XmNbottomAttachment,    XmATTACH_WIDGET,
	XmNbottomWidget,	_imageText->baseWidget(),
	NULL);

//	ATTACH PICNOERT TEXT
	XtVaSetValues( _picnoErt->baseWidget(),
	XmNleftWidget, 		_imageArea,
	XmNleftAttachment,	XmATTACH_WIDGET,
	XmNtopWidget, 		_titleBar->baseWidget(),	
	XmNtopAttachment,	XmATTACH_WIDGET,
	XmNrightAttachment,     XmATTACH_FORM,
	NULL );

//	ATTACH HIST STUFF
	XtVaSetValues( _histArea,
	XmNleftWidget, 		_imageArea,
	XmNleftAttachment,	XmATTACH_WIDGET,
	XmNrightAttachment,	XmATTACH_FORM,
	XmNtopWidget, 		_picnoErt->baseWidget(),	
	XmNtopAttachment,	XmATTACH_WIDGET,
	NULL );

//      ATTACH IMAGE TEXT 
        XtVaSetValues(  _imageText->baseWidget(),   
	XmNleftAttachment,      XmATTACH_FORM,
	XmNrightAttachment,     XmATTACH_FORM,
        XmNbottomAttachment, 	XmATTACH_FORM,
        NULL );

//	ATTACH STRETCH TEXT
	XtVaSetValues( _stretchText->baseWidget(),
	XmNtopAttachment,       XmATTACH_WIDGET,
	XmNtopWidget,		_histArea,
	XmNleftWidget, 		_imageArea,
	XmNleftAttachment,	XmATTACH_WIDGET,
	XmNleftOffset,		3,
	XmNrightAttachment,     XmATTACH_FORM,
	XmNbottomWidget,	_imageText->baseWidget(),
	XmNbottomAttachment,	XmATTACH_WIDGET,
	NULL );
}

///////////////////////////////////////////////////////////////////
//	createDisplayPolicy   (for all imagetype widgets)
//		note: This is the standard pseudoColor lut table 
//		      copied from Browse.  The values never change, 
//		      even if the image changes
///////////////////////////////////////////////////////////////////
DspDisplayPolicy * DspInstrBasic::createDisplayPolicy()
{	
//	CREATE DISPLAYPOLICY OBJECT: gets stretch 
	DspDisplayPolicy * displayPolicy = new DspDisplayPolicy( this, _name );
	
//	FILL __red PSEUDOCOLOR LUT	
	displayPolicy->setFlatInLutPseudoColor(  __red, 0,95,0);
	displayPolicy->setLinearInLutPseudoColor(__red, 96,160,0,160);
	displayPolicy->setLinearInLutPseudoColor(__red, 160,255,160,255);
	
//	FILL __green PSEUDOCOLOR LUT	
	displayPolicy->setLinearInLutPseudoColor(__green, 0,64,0,128);
	displayPolicy->setLinearInLutPseudoColor(__green, 64,128,128,255);
	displayPolicy->setLinearInLutPseudoColor(__green, 128,192,255,128);
	displayPolicy->setLinearInLutPseudoColor(__green, 192,255,128,0);
	
//	FILL __blue PSEUDOCOLOR LUT	
	displayPolicy->setLinearInLutPseudoColor(__blue, 0,96,255,160);
	displayPolicy->setLinearInLutPseudoColor(__blue, 96,160,160,0);
	displayPolicy->setFlatInLutPseudoColor(  __blue, 160,255,0);
	
//	RETURN
	return( displayPolicy );
		
} 
///////////////////////////////////////////////////////////////////
//	updateDisplay (called by DspWindowBasic)	
//		calls updateViews in superclass DspUIComponent to
//		update attached views (DspInfoView, DspImage*,DspHist*)
///////////////////////////////////////////////////////////////////
void DspInstrBasic::update()
{
//	UPDATE MODEL, TO READ NEW DATA
	strcpy(_filename,_window->getFilename()); 
  	updateImageModel();
  	
//	UPDATE ATTACHED VIEWS: images hist, infoView
  	updateViews(); 

}
///////////////////////////////////////////////////////////////////
//	update Image Model (called by updateDisplay)
//		Note that the filename is blank for NIMS data and
//		only NIMS data allows more than one model.
///////////////////////////////////////////////////////////////////
void DspInstrBasic::updateImageModel()
{
        int i;
	for ( i = 0; i< _numbModels; i++ ) {
  		if (_dataModel[i]->isDataSourceOpened())
      			_dataModel[i]->close();
   		_dataModel[i]->open( _filename );  
   	}
}

///////////////////////////////////////////////////////////////////
//	addPopup:  Create PopupMenu for Button 3
//		NOTE:  the histogram(s) in the display are
//		not stretched.
///////////////////////////////////////////////////////////////////
void DspInstrBasic::addPopup(
				ImageDisplayView * imageView, 
				ImageData * dataModel, 
				DspImageBasic * ,// imageDisplayer,
				int	, // imageNumber,
				Histogram * ,// rawHistModel,
				Histogram * ,//stretchHistModel,
				Boolean posSen,  // position button sensitive
				Boolean dnSen,   // dn button sensitive
				Boolean dumpSen,  // dump button sensitive
				Boolean ,// strSen,  // stretch button (dialog) sensitive
				Boolean treeSen) // widget tree button sensitive
{
	if (imageView) {
	
//		CREATE POPUP MENU 
            	_cmdList = new MenuCmdList("MainMenu" ); //prev Options

//	 	CREATE MENU BUTTON: SINGLE DN CURSOR 
		_cursorDnCmd = new DspCursCmd ("cursorPosCmd", "single", posSen,
                        dataModel, imageView, _w, _pixelSize);
            	_cmdList->addButton(_cursorDnCmd);
            
//		CREATE MENU BUTTON: MULTI DN CURSOR
            	_cursorMatrixCmd = new DspCursCmd ("cursMatrixCmd", "multi", dnSen,
						dataModel, imageView, _w, _pixelSize);
	    	_cmdList->addButton(_cursorMatrixCmd);
	    	
//		CREATE MENU BUTTON: CURSOR DUMP
//		unsigned char bitFlags = (unsigned char) 255;
//            	_cursDumpCmd = new DspCursDumpCmd ("cursDumpCmd",  "cursDump", dumpSen,
//			dataModel, imageView, _w, bitFlags);
//	   	_cmdList->addButton(cursDumpCmd);

// 		CREATE CASCADE MENU FOR CURSOR DUMP		
		createPulloutDumpMenu(_cmdList, dumpSen, dataModel,imageView);
	    		        		    	
//	**	CREATE MENU BUTTON: STRETCH DIALOG (TO STRETCH BW-IMAGE ONLY)
///		DspDisplayPolicy * displayPolicy = 
///					imageDisplayer->getDisplayPolicy(imageNumber);
///		DspLut * stretchLutR = displayPolicy->getStretchLut(__red);
///		_stretchCmd = new DspStretchCmd( "Stretch", False, 
///					imageDisplayer, imageNumber,
///					stretchLutR,
///					(Histogram *) NULL, (Histogram *) NULL 
///					//rawHistModel, stretchHistModel
///					); 
///					
///		_stretchDialog = new DspStretchCmdDialog("stretchDialog", _stretchCmd,
///								imageDisplayer);
///		_postStretchCmd = new DspPostDialogCmd("Stretch", strSen,
///								(_stretchDialog));
/// 		_cmdList->addButton(_postStretchCmd);
/// 		imageDisplayer->setDialogStretch( _stretchDialog->isStretchEnabled());
	    		   
	    		    		    	
//		CREATE MENU BUTTON: WIDGET TREE	    	
	    	_printWidgetTreeCmd = new PrintWidgetTreeCmd("Print Widget Tree",
                                                        treeSen, _w);
	    	_cmdList->addButton(_printWidgetTreeCmd);


//		CREATE POPUP
            	_popup = new PopupMenu(_parent, "Popup", _cmdList);
////            popup->attachPopup(_parent);
       }
}
///////////////////////////////////////////////////////////////////
//	Manage 	(Overrides BasicComponent)
///////////////////////////////////////////////////////////////////
void DspInstrBasic::manage()
{
	_popup->attachPopup(_parent);
	BasicComponent::manage();
	_cursorDnCmd->mapViews();
	_cursorMatrixCmd->mapViews();
//	_stretchCmd->mapViews();
//	_fileNameCmd->mapViews();

}
///////////////////////////////////////////////////////////////////
//	Unmanage (override BasicComponent)
///////////////////////////////////////////////////////////////////
void DspInstrBasic::unmanage()
{
	_popup->detachPopup(_parent);
	BasicComponent::unmanage();
	_cursorDnCmd->unmapViews();
	_cursorMatrixCmd->unmapViews();
//	_stretchCmd->unmapViews();
//	_fileNameCmd->unmapViews();

}

///////////////////////////////////////////////////////////////////
//	createDumpMenu (non-public)
//		called by createPopupMenu
//		(separated from code above for readability only)
///////////////////////////////////////////////////////////////////
void DspInstrBasic::createPulloutDumpMenu(MenuCmdList * menuCmdList, Boolean dumpSen,
						ImageData * dataModel, 
						ImageDisplayView * imageView)
{

//	NEW PULLOUT MENU FOR DUMP (PULLED OUT FROM MAIN MENU ABOVE)
 	_dumpMenuCmdList = new MenuCmdList("dumpMenuCmdList" ); // empty pulldown menu 
 
//	1st BUTTON:  START CURSOR DUMP (PUSH BUTTON TYPE)
   	unsigned char bitFlags = (unsigned char) 255;
   	_cursDumpCmd = new DspCursDumpCmd   (_w, "cursDumpCmd",  "cursDumpCmd", dumpSen,
		dataModel, imageView, bitFlags, _pixelSize);
   	_dumpMenuCmdList->addButton((Cmd *) _cursDumpCmd);

// 	2nd BUTTON:  LOAD FILE BUTTON (DIALOG, ETC)
  	_fileNameCmd = new DspFileNameCmd("fileNameCmd",dumpSen, 
 				(XtPointer)_cursDumpCmd, &DspCursDumpCmd::setNameLink); 
 	_postFileDialogCmd = new PostSingleFileDialogCmd("postFileDialogCmd", 
 								dumpSen, _fileNameCmd);
 	_dumpMenuCmdList->addButton(_postFileDialogCmd);
 	
// 	CASCADE BUTTON IN ORIGINAL (PARENT) POPUP MENU   ... 
  	menuCmdList->addSubmenu((Cmd*) _dumpMenuCmdList); // creates menuItemButton
	  
}
///////////////////////////////////////////////////////////////////
//	NOTES
///////////////////////////////////////////////////////////////////
//
/* */  
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspinstrument.imake
#define SUBROUTINE dspinstrument 
#define MODULE_LIST \
		dspinstrbasic.cc
#define P2SUB         /* SUBROUTINE */
#define P2_SUBLIB
/* #define DEBUG*/
#define USES_C_PLUS_PLUS
#define LIB_FORTRAN
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
