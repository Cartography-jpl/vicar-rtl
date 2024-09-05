$!****************************************************************************
$!
$! Build proc for MIPL module dspcursor
$! VPACK Version 1.9, Friday, April 07, 2000, 15:34:56
$!
$! Execute by entering:		$ @dspcursor
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
$ write sys$output "*** module dspcursor ***"
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
$ write sys$output "Invalid argument given to dspcursor.com file -- ", primary
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
$   if F$SEARCH("dspcursor.imake") .nes. ""
$   then
$      vimake dspcursor
$      purge dspcursor.bld
$   else
$      if F$SEARCH("dspcursor.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspcursor
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspcursor.bld "STD"
$   else
$      @dspcursor.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspcursor.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspcursor.com -mixed -
	-s dspcursorview.cc dspcurs2imageglue.cc dspcursbox.cc dspcurscmd.cc -
	   dspcurswindow.cc -
	-i dspcursor.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dspcursorview.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// DspCursorView.cc
//	Contains corrections to CursorDnView in GUI
//	   (1) use of gadget instead of widget
//	   (2) allows matrix of pixels to be displayed
//	   (3) getValueString allows pixel sizes larger 
//		than a byte.
///////////////////////////////////////////////////////////////
#include "DspCursorView.h"
#include "ImageData.h"
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Form.h>
#include <Xm/Xm.h>
#include <string.h>
#include <stdio.h>


XtResource DspCursorView::_resources [ ] = {
  {
    "numbRows",
    "NumbRows",
    XmRInt,
    sizeof(int),
    XtOffset( DspCursorView *, _numbRows),
    XmRString,
    (XtPointer) "1",
 },
   {
    "numbColumns",
    "NumbColumns",
    XmRInt,
    sizeof(int),
    XtOffset( DspCursorView *, _numbColumns),
    XmRString,
    (XtPointer) "1",
 },
  {
    "skipRows",
    "SkipRows",
    XmRInt,
    sizeof(int),
    XtOffset( DspCursorView *, _skipRows),
    XmRString,
    (XtPointer) "1",
 },
   {
    "skipColumns",
    "SkipColumns",
    XmRInt,
    sizeof(int),
    XtOffset( DspCursorView *, _skipColumns),
    XmRString,
    (XtPointer) "1",
 }
};

///////////////////////////////////////////////////////////////
//	CONSTRUCTOR
//
///////////////////////////////////////////////////////////////
DspCursorView::DspCursorView(
				Widget parent, 
				char * name,
				CursorModel * cursorModel,
				ImageData * imageData,
				unsigned char bitFlags,
				int pixelSize)
			: CursorBasicView(parent, name, cursorModel, imageData, bitFlags) 
{ 
int i;

// 	INIT VARIABLES
        getResources(_resources, XtNumber( _resources) );        
	_midRow = (_numbRows - 1) / 2;
	_midColumn = (_numbColumns - 1) / 2;
	_numbLines   = imageData->getNumbLines();
	_numbSamples = imageData->getNumbSamples();
	
        
//	SAVE ARGS
	_imageData = imageData;
	_pixelSize = pixelSize;
	
	int len = sizeof( _blankString ) / sizeof(_blankString[0]);
	for ( i=0; i < (len-1); i++ ) {
		_blankString[i]= ' ';
		_pixelBuffer[i]='\0';
	}
	_blankString[i]='\0';	
	_blankString[(_pixelSize+1)] = '\0';
	_pixelBuffer[i] = '\0';
	
//	GET numColumns from Resources for numb columns in row/column widget
	XtVaGetValues ( _w, XmNnumColumns, &_numColumnsResource, NULL );
	if ((_numbColumns != 0) && (_numbRows != 0 ) && (_numColumnsResource != 0)) {
	
	_labelRed = new Widget*[ _numbColumns ];
	_labelGrn = new Widget*[ _numbColumns ];
	_labelBlu = new Widget*[ _numbColumns ];
	_labelBw  = new Widget*[ _numbColumns ];
	for ( i=0; i<_numbColumns; i++) {
		_labelRed[i] = new Widget[ _numbRows ];
		_labelGrn[i] = new Widget[ _numbRows ];
		_labelBlu[i] = new Widget[ _numbRows ];
		_labelBw[i]  = new Widget[ _numbRows ];
	}
	
	_textRed = new Widget*[ _numbColumns ];
	_textGrn = new Widget*[ _numbColumns ];
	_textBlu = new Widget*[ _numbColumns ];
	_textBw  = new Widget*[ _numbColumns ];
	for ( i=0; i<_numbColumns; i++) {
		_textRed[i] = new Widget[ _numbRows ];
		_textGrn[i] = new Widget[ _numbRows ];
		_textBlu[i] = new Widget[ _numbRows ];
		_textBw[i]  = new Widget[ _numbRows ];
	}
	
	_formRed = new Widget*[ _numbColumns ];
	_formGrn = new Widget*[ _numbColumns ];
	_formBlu = new Widget*[ _numbColumns ];
	_formBw  = new Widget*[ _numbColumns ];
	for ( i=0; i<_numbColumns; i++) {
		_formRed[i] = new Widget[ _numbRows ];
		_formGrn[i] = new Widget[ _numbRows ];
		_formBlu[i] = new Widget[ _numbRows ];
		_formBw[i]  = new Widget[ _numbRows ];
	}
	
	createDisplay();
	_glue = new DspCurs2ImageGlue(imageData,this);    
	} 		

}	
///////////////////////////////////////////////////////////////
//	DESTRUCTOR
//
///////////////////////////////////////////////////////////////
DspCursorView::~DspCursorView()
{

	if (_glue)
		delete _glue;
	
	if ((_numbColumns != 0) || (_numbRows != 0 ) || (_numColumnsResource != 0)) 
		removeCursorDisplays();
			
}
///////////////////////////////////////////////////////////////
//      addNewSubView:
//              May be called by "createCursorDisplays"
//              function in subclass to create a single
//              display group consisting of: label and a
//              textfield. The textfield is used to show
//              values that are updated as the cursor
//              moves around in the image. A name string is
//              automatically created as :
//              "label" +  <displayName>,   etc.
///////////////////////////////////////////////////////////////
void DspCursorView::addNewSubView( Widget w, char * displayName,
        Widget *form, Widget * label, Widget * textfield, Boolean enabled )
{
        char name[132];
 
        if (enabled) {
                *form = XtVaCreateManagedWidget(
                			"form", xmFormWidgetClass, w, NULL);
 
//		CREATE VALUE DISPLAY(as a text field)
                strcpy( name, "textfield"); strcat( name, displayName);
                *textfield = XtVaCreateManagedWidget(name, 
                        xmTextFieldWidgetClass,*form,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_FORM,
                        NULL);
 
//      	CREATE LABEL
                strcpy( name, "label"); strcat( name, displayName);
                *label  = XtVaCreateManagedWidget ( name, 
                        xmLabelGadgetClass, *form,
                        XmNtopAttachment, XmATTACH_FORM,
                        XmNbottomAttachment, XmATTACH_FORM,
                        XmNrightAttachment, XmATTACH_WIDGET,
                        XmNrightWidget, *textfield,
                        NULL );
        }
}
///////////////////////////////////////////////////////////////
//	getValueString()
//		local function for getting a dn value and
//		its string equivalent.  Called once for 
//		each DN.
///////////////////////////////////////////////////////////////
void DspCursorView::getValueString( ColorType color,  
					int x, int y, 
					char * newValueString , 
					Boolean enabled  )
{
	unsigned long int dn;
	StatusType  status;
        int i;

	sprintf ( newValueString, " \0" );
	if ( enabled) {
		if ((x >= 0 && x <  _numbSamples) && ( y >= 0 && y < _numbLines )) 
			status = _imageData->readPixel( color,  x,  y,  _pixelBuffer );
		else 
			status = imFAILURE;
		if ( status == imSUCCESS ) {
			for (i=0, dn=0; i< _pixelSize; i++) 
				dn = ((unsigned long int)  (dn * (256)) + ((unsigned long int)_pixelBuffer[i]));
			sprintf ( newValueString, "%-*d", ((int)_columnsResource), dn );			
		}
		else {		   
		    	for (i=0; i < _pixelSize; i++ )
				newValueString[i]= '*';
		    	newValueString[i] = '\0';
		}
	}
}
///////////////////////////////////////////////////////////////
//	cursorMoved() .. implements pure virtual in abstract
//		updates each DN display with new DN
//		values under cursor.
///////////////////////////////////////////////////////////////
void DspCursorView::cursorMoved(XvicImageCallbackStruct * cb )
{
       	char buf[132];
       	int i,j;
       	int x,y;
       	
       	if ((_skipColumns > 1) || (_skipRows > 1)) {
		x = (cb->x) - ((cb->x)%_skipColumns);
		y = (cb->y) - ((cb->y )%_skipRows);
	}
	else {
		x = cb->x - _midRow;
		y = cb->y - _midColumn;
	}

	if ((_numbColumns != 0) && (_numbRows != 0 ) && (_numColumnsResource != 0)) {
	
//	UPDATE RED DN
	if (_redDnEnable) 
	   for (i=0; i<_numbColumns; i++)
           	for (j=0; j<_numbRows; j++) {
		   getValueString( RED,x+i, y+j, buf, _redDnEnable);
		   updateValue( _textRed[i][j], buf, _redDnEnable );
		}

//	UPDATE GREEN DN
	if (_greenDnEnable)
	   for (i=0; i<_numbColumns; i++)
        	for (j=0; j<_numbRows; j++) {
		   getValueString( GREEN,x+i, y+j, buf, _greenDnEnable);
		   updateValue( _textGrn[i][j], buf, _greenDnEnable );
		}

//	UPDATE BLUE DN
	if (_blueDnEnable)
	   for (i=0; i<_numbColumns; i++)
           	for (j=0; j<_numbRows; j++) {
		   getValueString( BLUE, x+i, y+j, buf, _blueDnEnable );
		   updateValue( _textBlu[i][j], buf, _blueDnEnable );
	   	}

//	UPDATE BW DN
	if (_bwDnEnable)
	   for (i=0; i<_numbColumns; i++)
	   	for (j=0; j<_numbRows; j++) {
		   getValueString( BWcolor, x+i, y+j, buf, _bwDnEnable);
		   updateValue( _textBw[i][j], buf, _bwDnEnable);
		}
		
      }
}
///////////////////////////////////////////////////////////////
//	createCursorDisplays()
//		
///////////////////////////////////////////////////////////////
void DspCursorView::createCursorDisplays()
{
     int i,j;

     if ((_numbColumns != 0) || (_numbRows != 0 ) || (_numColumnsResource != 0)) {
	

// 	GET COLOR MODE INFO.  
//		When mode=bw, we disable the rgb displays, and vice versa.
	ModeType mode =  _imageData->getMode(); 

// 	DECIPHER BIT FLAGS
	_redDnEnable = False;
	_greenDnEnable = False;
	_blueDnEnable = False;
	_bwDnEnable = False;
	if ( _bitFlags && (unsigned char) 1  && mode == COLORmode )
		_redDnEnable = True;
	if ( _bitFlags && (unsigned char) 2  && mode == COLORmode )
		_greenDnEnable = True;
	if ( _bitFlags && (unsigned char) 4  && mode == COLORmode )
		_blueDnEnable =   True;
	if ( _bitFlags && (unsigned char) 8  && mode == BWmode )
		_bwDnEnable = True  ;

//	CREATE RED DN DISPLAY
	for (i=0; i<_numbColumns; i++)
           for (j=0; j<_numbRows; j++)
		addNewSubView( _w, "RedDn",
			&_formRed[i][j], &_labelRed[i][j], &_textRed[i][j],
			_redDnEnable );

//	CREATE GREEN DN DISPLAY
	for (i=0; i<_numbColumns; i++)
           for (j=0; j<_numbRows; j++)
		addNewSubView( _w, "GrnDn",
			&_formGrn[i][j], &_labelGrn[i][j], &_textGrn[i][j],
			_greenDnEnable );

//	CREATE BLUE DN DISPLAY
	for (i=0; i<_numbColumns; i++)
           for (j=0; j<_numbRows; j++)
		addNewSubView( _w, "BluDn",
			&_formBlu[i][j], &_labelBlu[i][j], &_textBlu[i][j],
			_blueDnEnable);

//	CREATE BW DN DISPLAY
	for (i=0; i<_numbColumns; i++)
           for (j=0; j<_numbRows; j++)
		if ((_skipColumns == 1) && (_skipRows == 1)
			&& (i == _midColumn) && (j == _midRow) )	
		   addNewSubView( _w, "BwDnCross",
			&_formBw[i][j], &_labelBw[i][j], &_textBw[i][j],
			_bwDnEnable );
		else 
		   addNewSubView( _w, "BwDn",
                        &_formBw[i][j], &_labelBw[i][j], &_textBw[i][j],
                        _bwDnEnable );
                        
//	GET WIDTH OF TEXT WIDGET
	XtVaGetValues ( _textBw[0][0], XmNcolumns, &_columnsResource, NULL );
// _columnsResource = 3; // kluge
	                    
      }
}

///////////////////////////////////////////////////////////////
//	blankSubViews()
///////////////////////////////////////////////////////////////
void DspCursorView::blankSubViews()
{
     int i,j;
    
     if ((_numbColumns != 0) || (_numbRows != 0 ) || (_numColumnsResource != 0)) {
	
//	BLANK EACH VIEW BY DISPLAYING A BLANK STRING IN EACH
	for (i=0; i<_numbColumns; i++) 
	    for (j=0; j<_numbRows; j++) {
           
// 		BLANK OUT RED DN
		updateValue( _textRed[i][j], _blankString, _redDnEnable  );

// 		BLANK OUT  GREEN DN
		updateValue( _textGrn[i][j], _blankString,  _greenDnEnable);

// 		BLANK OUT  BLUE DN
		updateValue( _textBlu[i][j], _blankString, _blueDnEnable );

// 		BLANK OUT B&W DN
		updateValue( _textBw[i][j], _blankString, _bwDnEnable );
	   }
     }	   
}
///////////////////////////////////////////////////////////////
//	removeCursorDisplays()
//		Removes widgets created by createCursorDisplays.
///////////////////////////////////////////////////////////////
void DspCursorView::removeCursorDisplays()
{
        int i,j;
	
	for (j=0; j<_numbRows; j++)
           for (i=0; i<_numbColumns; i++) {
       		removeSubView_(_formRed[i][j], _labelRed[i][j], _textRed[i][j]);
		removeSubView_(_formGrn[i][j], _labelGrn[i][j], _textGrn[i][j]);
  		removeSubView_(_formBlu[i][j], _labelBlu[i][j], _textBlu[i][j]);
		removeSubView_(_formBw[i][j],  _labelBw[i][j], _textBw[i][j]);
	   }
}

///////////////////////////////////////////////////////////////
//      removeSubView_:
//              Removes widgets created by addNewSubView.
///////////////////////////////////////////////////////////////
void DspCursorView::removeSubView_(Widget form, Widget label,
                                Widget textfield)
{
        if (form) {
                XtUnmanageChild(form);
                XtDestroyWidget(form);
        }
        XtDestroyWidget(label);
        XtDestroyWidget(textfield);
        form = NULL;
        label = NULL;
        textfield = NULL;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create dspcurs2imageglue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// DspCurs2ImageGlue.cc: See DspImageToCursorDnViewGlue
////////////////////////////////////////////////////////////////////////
#include "DspCurs2ImageGlue.h"


////////////////////////////////////////////////////////////////////////
// CONSTRUCTOR
////////////////////////////////////////////////////////////////////////
DspCurs2ImageGlue::DspCurs2ImageGlue (ImageData *image, DspCursorView *cursorView)
		: BasicImageView("glue", image)
{  
	_cursorView = cursorView;
	_image = image;
	_oldMode = _image->getMode();
	_image->attachView(this);
}
////////////////////////////////////////////////////////////////////////
// UPDATE (CHANGE BW<->COLOR VIEW)
////////////////////////////////////////////////////////////////////////
void DspCurs2ImageGlue::update()	
{  
	if (_oldMode != _image->getMode()) {
	      _oldMode = _image->getMode();
	      _cursorView->removeCursorDisplays();
	      _cursorView->createCursorDisplays();
	}
}

 
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create dspcursbox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  DspCursBox.cc:
///////////////////////////////////////////////////////////////////
#include "DspCursBox.h"
#include "BasicImageView.h"
#include "ImageData.h"
#include "DspCursorView.h"
#include "CursorPositionView.h"
#include "CursorModel.h"
#include "BasicImageView.h"
#include <Xm/RowColumn.h>

String DspCursBox::_defaults [ ] = {
        "*isAligned:            True",
        "*orientation:          XmHORIZONTAL",
        "*entryAlignment:       XmALIGNMENT_END",
        "*packing:              XmPACK_COLUMN",
        ".numColumns:		1",
        "*adjustLast:           False",
        NULL,
};
XtResource DspCursBox::_resources [ ] = {
  {
    "enableLoc",
    "enableLoc",
    XmRInt,
    sizeof(int),
    XtOffset( DspCursBox *, _enableLoc),
    XmRString,
    (XtPointer) "1", // on
 }
};
DspCursBox::DspCursBox( Widget parent, char *name,
		ImageData *imageData, BasicImageView *imageView, int pixelSize )
		: UIComponent(name)
{
unsigned char bitFlags;

// 	INIT VARIABLES
 	setDefaultResources ( parent, _defaults );
	_w = XtVaCreateWidget ( _name,  
		xmRowColumnWidgetClass, parent, 
		NULL);
        getResources(_resources, XtNumber( _resources) );        
 	installDestroyHandler();
	
	
//	CREATE CURSOR MODEL
        _cursorModel = new CursorModel(  True,  imageView->getWidget() );


//	DISPLAY LINE/SAMPLE LOCATION  (POSITION) 
	bitFlags = (unsigned char) 255; 		// see Note
	if (_enableLoc == 1) {  // if enabled through resources
		_cursorPositionView = new CursorPositionView( _w, "cursorPositionView", 
					_cursorModel, imageData, bitFlags );
		_cursorPositionView->manage();				
	}


//	DISPLAY PIXEL VALUE UNDER CURSOR
	bitFlags = (unsigned char) 255; 		
	_cursorRangeDnView = new DspCursorView( _w, "dspCursorView", 
					_cursorModel,imageData,bitFlags,pixelSize );
	_cursorRangeDnView->manage();
	
}

DspCursBox::~DspCursBox()
{
        delete _cursorModel;
        delete _cursorRangeDnView;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create dspcurscmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// DspCursDspCmd.cc: Command class that displays a window 
//		    with (x,y) cursor current position 
//		    within the image area
//////////////////////////////////////////////////////////
#include "DspCursCmd.h"
#include "DspCursWindow.h"
#include "ImageData.h"
#include "ImageDisplayView.h"

///////////////////////////////////////////////////////////
//   CONSTRUCTOR
//  	Save the cursor model pointer and set value to create
//  	the display component once.
///////////////////////////////////////////////////////////
DspCursCmd::DspCursCmd ( char *name, char* titleName, int active,
		ImageData *imageData, ImageDisplayView *imageView,
		Widget parent, int pixelSize ) 
		: NoUndoCmd ( name, active )
{
    _created = FALSE;
    _imageData = imageData;
    _imageView = imageView;
    _title = titleName;
    _parent = parent;
        
//  CREATE WINDOW (See Note #1)
    if (active) {
    	_cursorRangeDnWindow = new DspCursWindow ( _title, _imageData, _imageView, _parent, pixelSize);
    	_cursorRangeDnWindow->initialize();
///    	XtVaSetValues(_cursorRangeDnWindow->baseWidget(),
///                		XmNdeleteResponse, XmUNMAP,
///               		NULL);
        _created = TRUE;
     }
         
}

///////////////////////////////////////////////////////////
//   doit()
// 	Execute the following upon button activation.
// 	Create display Window only once and then display it.
// 	Set the Close button to the UNMAP state so the Window
// 	is only unmanaged when it is closed and can therefore
// 	be managed again when the user hits the command button.
///////////////////////////////////////////////////////////
void DspCursCmd::doit()
{
  	
//	MANAGE WINDOW
	if (_created != FALSE ) 
  		_cursorRangeDnWindow->manage();

}

void DspCursCmd::mapViews()   { if (_created) _cursorRangeDnWindow->restoreState();}
void DspCursCmd::unmapViews() { if (_created) _cursorRangeDnWindow->saveState();}

// NOTE #1
//	Had to move the instantiation of DspCursWindow out of doit() 
//	to constructor. however, this means that model->readPixel
//	is being read even if it is desensitized.  In the case of 
//	PWSdata, pixels were not meant to be read.  SO..
//	I only instatiate if sensitized.  
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create dspcurswindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////
// DspCursWindow.cc:
//	Would have been better to use dialog instead of window here
//	but its too late to change it now.  MainWindow uses TopLevelShell
//	which doesn't manage/unmanage automatically when parent is 
//	managed and unmanaged. 
////////////////////////////////////////////////////////////////////
#include "DspCursWindow.h"
#include "ImageData.h"
#include "ImageDisplayView.h"
#include "DspCursBox.h"
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <zvproto.h>


DspCursWindow::DspCursWindow ( char *name, ImageData * imageData, 
		ImageDisplayView * imageView,
		Widget parent, int pixelSize) : MainWindow ( name )
{
    _imageData = imageData;
    _imageView = imageView;
    _parent = parent;
    _pixelSize = pixelSize;

}

Widget DspCursWindow::createWorkArea ( Widget parent )
{
   _form = XtVaCreateWidget("workArea",
                     xmFormWidgetClass, parent,
                     NULL);

   _cursorRangeDnBox = new DspCursBox(_form, "dspCursBox", 
   							_imageData, 
   							_imageView,
   							_pixelSize);

   XtVaSetValues   ( _cursorRangeDnBox->baseWidget(),
                     XmNtopAttachment,     XmATTACH_FORM,
                     XmNleftAttachment,    XmATTACH_FORM,
                     XmNrightAttachment,   XmATTACH_FORM,
                     XmNbottomAttachment,  XmATTACH_FORM,
                     NULL );
   _cursorRangeDnBox->manage();

   return (_form);
}
void DspCursWindow::initialize()
{

// 	All toplevel windows in the MotifApp framework are 
// 	implemented as a popup shell off the Application's
// 	base widget.
    	_w = XtVaCreatePopupShell ( _name, 
                             	topLevelShellWidgetClass,
                             	//theApplication->baseWidget(),
                             	_parent,
                             	XmNallowShellResize, True,
                             	XmNdeleteResponse, XmUNMAP,
                             	NULL );
                             	
        _pop = False;
                    	
                             	
    	installDestroyHandler();
    
// 	Use a Motif XmMainWindow widget to handle window layout
    	_main = XtCreateManagedWidget ( "mainWindow", 
                                   xmMainWindowWidgetClass,
                                   _w, 
                                   NULL, 0 );
    
// 	Called derived class to create the work area
    	_workArea = createWorkArea ( _main );  
    	assert ( _workArea );
    
// 	Designate the _workArea widget as the XmMainWindow
// 	widget's XmNworkWindow widget
    
    	XtVaSetValues ( _main, 
                   XmNworkWindow, _workArea, 
                   NULL );
    
// 	Manage the work area if the derived class hasn't already.   
    	if ( !XtIsManaged ( _workArea ) )
        	XtManageChild ( _workArea ); 
                    
}

void DspCursWindow::restoreState()
{
//char msg[132];
	if (_pop) MainWindow::manage();
}
void DspCursWindow::saveState()  // this doesn't work.    
{
//char msg[132];
	if ( XtIsManaged(_w)) _pop = True;
	else _pop = False;
	MainWindow::unmanage();
}
	
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspcursor.imake
#define SUBROUTINE dspcursor
#define MODULE_LIST \
	dspcursorview.cc dspcurs2imageglue.cc \
	dspcursbox.cc \
	dspcurscmd.cc dspcurswindow.cc
 
#define P2SUB
#define P2_SUBLIB
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
