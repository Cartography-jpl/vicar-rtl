$!****************************************************************************
$!
$! Build proc for MIPL module dspuicomponent
$! VPACK Version 1.8, Thursday, October 17, 1996, 01:18:46
$!
$! Execute by entering:		$ @dspuicomponent
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
$ write sys$output "*** module dspuicomponent ***"
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
$ write sys$output "Invalid argument given to dspuicomponent.com file -- ", primary
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
$   if F$SEARCH("dspuicomponent.imake") .nes. ""
$   then
$      vimake dspuicomponent
$      purge dspuicomponent.bld
$   else
$      if F$SEARCH("dspuicomponent.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspuicomponent
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspuicomponent.bld "STD"
$   else
$      @dspuicomponent.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspuicomponent.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspuicomponent.com -mixed -
	-s DspUIComponent.cc -
	-i dspuicomponent.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspUIComponent.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  DspUIComponent.cc  .. a superclass to view type subclass.
///////////////////////////////////////////////////////////////////
#include "DspUIComponent.h"


////////////////////////////////////////////////////////////////
//      Constructor
////////////////////////////////////////////////////////////////
DspUIComponent::DspUIComponent(char * name) : UIComponent(name)
{
	_numbViews = 0;
	_views = NULL;
	_numbAttachments = 0;
	_numbCallbacks = 0;
	_attachmentList = NULL;						     
}

////////////////////////////////////////////////////////////////
//     Destructor 
////////////////////////////////////////////////////////////////
DspUIComponent::~DspUIComponent()
{
	removeAllCallbacks();
	detachAllWidgetsFromForm();
      	if ( _views != NULL )
      		deleteViews();
}

////////////////////////////////////////////////////////////////
//      attachView
//		Unlike GUI, this is called by the parent object
//		on the child object 
////////////////////////////////////////////////////////////////
void DspUIComponent::attachView(DspUIComponent * view)
{
	AttachViewMacro( DspUIComponent, _views, _numbViews, view);
//      view->update ( );
}
////////////////////////////////////////////////////////////////
//      detachView
////////////////////////////////////////////////////////////////
void DspUIComponent::detachView (DspUIComponent *view)
{
 	DetachViewMacro( DspUIComponent, _views, _numbViews, view )
}
 
////////////////////////////////////////////////////////////////
//      updateViews
//		Calls update on all attached views
////////////////////////////////////////////////////////////////
void  DspUIComponent::updateViews()
{
      for (int i=0; i<_numbViews; i++)
              _views[i]->update( );
}

////////////////////////////////////////////////////////////////
//      deleteViews  
//		Deletes all views on the list
////////////////////////////////////////////////////////////////
void  DspUIComponent::deleteViews()
{
      	for (int i=(_numbViews-1); i>=0; i--) {
		detachView ((DspUIComponent *) _views[i]);
                delete _views[i];
                _views[i] = NULL;
	}
}

////////////////////////////////////////////////////////////////
//      attachWidgetToForm
////////////////////////////////////////////////////////////////
void DspUIComponent::attachWidgetToForm(
				Widget w,
				unsigned char  	attachTop , 
				Widget		wTop ,
				unsigned char  	attachLeft , 
				Widget		wLeft ,
				unsigned char  	attachRight , 
				Widget		wRight ,
				unsigned char  	attachBottom , 
				Widget		wBottom 
			)
{
//	FIRST ATTACH WIDGET ACCORDING TO ARGS
        XtVaSetValues   ( w,
        	     XmNtopWidget,	   wTop,
                     XmNtopAttachment,     attachTop,
                     XmNleftWidget,	   wLeft,
                     XmNleftAttachment,    attachLeft,
                     XmNrightWidget,	   wRight,
                     XmNrightAttachment,   attachRight,
                     XmNbottomWidget,	   wBottom,
                     XmNbottomAttachment,  attachBottom,                     
                     NULL );	

//	ADD ATTACHED WIDGET TO LIST (TO REMOVE LATER)
        Widget * newAttachmentList = new Widget[(_numbAttachments+1)];
        for (int i = 0; i< _numbAttachments; i++) 
        	newAttachmentList[i] = _attachmentList[i];
        if (_numbAttachments > 0) delete [] _attachmentList;
        _attachmentList = newAttachmentList;
        _attachmentList[_numbAttachments] = w;
        _numbAttachments++;
}

////////////////////////////////////////////////////////////////
//      detachAllWidgetsFromForm .. called by subclass destructor
////////////////////////////////////////////////////////////////
void DspUIComponent::detachAllWidgetsFromForm()
{
	if (_attachmentList != NULL) {
		for (int i=(_numbAttachments-1); i>=0; i--)
			RemoveFormAttachments( _attachmentList[i] );
		delete [] _attachmentList;
		_attachmentList = NULL;		
	}
	_numbAttachments = 0;
}

////////////////////////////////////////////////////////////////
// 	RemoveFormAttachments     
//			detaches any widget from parent form
///////////////////////////////////////////////////////////////
void DspUIComponent::RemoveFormAttachments( Widget w )  
{
    if ( w != NULL ) 
	XtVaSetValues ( w,	
		XmNtopWidget,		XmATTACH_NONE,	
          	XmNbottomAttachment, 	XmATTACH_NONE,	
		XmNleftAttachment, 	XmATTACH_NONE,	
          	XmNrightAttachment,	XmATTACH_NONE,	
		NULL );   
}

////////////////////////////////////////////////////////////////
// 	addCallback
///////////////////////////////////////////////////////////////
void DspUIComponent::addCallback( Widget w, String callback_name, 
					XtCallbackProc routine )
{
//	ADD CALLBACK TO LIST
	CallbackList **	newCallbackList;
	newCallbackList = new CallbackList*[(_numbCallbacks+1)];
	for ( int i = 0; i < _numbCallbacks; i++ ) 
		newCallbackList[i] = _callbackList[i];
	if (_numbCallbacks > 0) 
		delete []_callbackList;
	_callbackList = newCallbackList;

// 	CREATE NEW IMAGE (AT END OF LIST)
	_callbackList[_numbCallbacks] = new CallbackList;
	_callbackList[_numbCallbacks]->widget = w;
	strcpy(_callbackList[_numbCallbacks]->callback_name,callback_name);
	_callbackList[_numbCallbacks]->routine = routine;	
		
//	DO CALLBACK
	XtAddCallback( w, callback_name, routine, (XtPointer) this);

//	INC COUNTER
	_numbCallbacks++;
}

////////////////////////////////////////////////////////////////
// 	removeCallback .. doesn't need to be called except in this class
///////////////////////////////////////////////////////////////
void DspUIComponent::removeCallback( Widget w, String callback_name, 
					XtCallbackProc routine )

{	
	CallbackList **	newCallbackList;
	newCallbackList = new CallbackList*[(_numbCallbacks-1)];
	for (int i=0, j=0; i< _numbCallbacks; i++ ) {
		if ((_callbackList[j]->widget != w) &&
				(strcmp(_callbackList[j]->callback_name, callback_name)!= 0) &&
				(_callbackList[j]->routine != routine)) {
			newCallbackList[i] = _callbackList[j];
			j++;
		}
		else {
			XtRemoveCallback(w, callback_name, routine, (XtPointer)this);
		}
	}
	if (_numbCallbacks > 0)
		delete []_callbackList;
	_callbackList = newCallbackList;
	_numbCallbacks--;
}
////////////////////////////////////////////////////////////////
// 	removeAllCallback
///////////////////////////////////////////////////////////////
void DspUIComponent::removeAllCallbacks()
{

	for (int i=(_numbCallbacks-1); i>=0; i--) {
		XtRemoveCallback( 
			_callbackList[i]->widget,
			_callbackList[i]->callback_name,
			_callbackList[i]->routine,
			(XtPointer) this );
	}
	_numbCallbacks = 0;
	delete [] _callbackList;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspuicomponent.imake
#define SUBROUTINE dspuicomponent 
#define MODULE_LIST \
		DspUIComponent.cc 

#define P2SUB
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
