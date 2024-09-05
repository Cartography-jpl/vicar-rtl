$!****************************************************************************
$!
$! Build proc for MIPL module dspinfo
$! VPACK Version 1.8, Friday, July 21, 1995, 16:06:54
$!
$! Execute by entering:		$ @dspinfo
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
$ write sys$output "*** module dspinfo ***"
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
$ write sys$output "Invalid argument given to dspinfo.com file -- ", primary
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
$   if F$SEARCH("dspinfo.imake") .nes. ""
$   then
$      vimake dspinfo
$      purge dspinfo.bld
$   else
$      if F$SEARCH("dspinfo.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspinfo
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspinfo.bld "STD"
$   else
$      @dspinfo.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspinfo.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspinfo.com -mixed -
	-s DspInfoView.cc -
	-i dspinfo.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspInfoView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
//  DspInfoView.cc:
///////////////////////////////////////////////////////////////////
#include "DspInfoView.h"

String DspInfoView::_defaults [ ] = {
       NULL,
};

///////////////////////////////////////////////////////////////////
//	CONSTRUCTOR
///////////////////////////////////////////////////////////////////
DspInfoView::DspInfoView(Widget parent, char *name, void (*fp)(char*)) 
			: DspUIComponent(name)
{
	setDefaultResources( parent, _defaults );
	_fp = fp;
	(*_fp) (_text);
	int n = 0;
	Arg args[20];
	XtSetArg(args[n], XmNeditable, 	False );   n++;
	XtSetArg(args[n], XmNeditMode,	XmMULTI_LINE_EDIT); n++;	
	XtSetArg(args[n], XmNvalue, _text); n++;
	_w = XtCreateWidget( _name, xmTextWidgetClass, parent, args, n);
	installDestroyHandler();
}
///////////////////////////////////////////////////////////////////
//	DESTRUCTOR
///////////////////////////////////////////////////////////////////
DspInfoView::~DspInfoView()
{
	delete _blankString;
	delete _stringBuffer;
}
///////////////////////////////////////////////////////////////////
//	update:  
//		inserts new string
///////////////////////////////////////////////////////////////////
void DspInfoView::update()
{	
	(*_fp) (_text);
	
	// WRITE NEW STRING
        XtVaSetValues (  _w, XmNvalue, _text, NULL);
}
///////////////////////////////////////////////////////////////////
//	
///////////////////////////////////////////////////////////////////
char *	DspInfoView::getStringBuffer()
{
	strcpy( _stringBuffer, _blankString);
	return _stringBuffer;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspinfo.imake
#define SUBROUTINE dspinfo 
#define MODULE_LIST \
		DspInfoView.cc 

#define P2SUB
#define P2_SUBLIB
 
#define USES_C_PLUS_PLUS
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_GUI
$ Return
$!#############################################################################
