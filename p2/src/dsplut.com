$!****************************************************************************
$!
$! Build proc for MIPL module dsplut
$! VPACK Version 1.8, Monday, April 01, 1996, 21:52:56
$!
$! Execute by entering:		$ @dsplut
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
$ write sys$output "*** module dsplut ***"
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
$ write sys$output "Invalid argument given to dsplut.com file -- ", primary
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
$   if F$SEARCH("dsplut.imake") .nes. ""
$   then
$      vimake dsplut
$      purge dsplut.bld
$   else
$      if F$SEARCH("dsplut.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dsplut
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dsplut.bld "STD"
$   else
$      @dsplut.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dsplut.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dsplut.com -mixed -
	-s DspLut.cc -
	-i dsplut.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspLut.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// DspLut.cc: Is 1 PseudoColorLut or Stretch Lut
////////////////////////////////////////////////////////
#include "DspLut.h"


///////////////////////////////////////////////////////////////////////////
// Constructor
///////////////////////////////////////////////////////////////////////////
DspLut::DspLut(int pixelSize) : Lut()
{

	_pixelSize=pixelSize; 
	_lutSize=(int) pow((double)2,_pixelSize);

	_defaultLut = new int [_lutSize];
	
	delete [] _lut;
	_lut = new int [_lutSize];
		 
        setDefAsArray(_lut); 
}

///////////////////////////////////////////////////////////////////////////
// Set Default array
///////////////////////////////////////////////////////////////////////////
void DspLut::setDefAsArray ( int *array )
{
        for (int i=0; i<_lutSize; i++)
                _defaultLut[i] = array[i];
}

///////////////////////////////////////////////////////////////////////////
// Restore default values between two points (points inclusive)
///////////////////////////////////////////////////////////////////////////
void DspLut::restoreDefault ( int start, int end )
{
	for (int i = start; i <= end; i++) {
		_lut[i] = _defaultLut[i];
	}
}
///////////////////////////////////////////////////////////////////////////
// Do flat line between two points (points inclusive)
///////////////////////////////////////////////////////////////////////////
void DspLut::setFlat( int firstIndex, int lastIndex, int value)
{

	if (firstIndex > lastIndex) {
		int temp = firstIndex;
		firstIndex = lastIndex;
		lastIndex = temp;
	}
	for (int i=firstIndex; i<=lastIndex; i++)
		_lut[i] =value;

}
///////////////////////////////////////////////////////////////////////////
// Do linear interpolation between two points (points inclusive)
///////////////////////////////////////////////////////////////////////////
void DspLut::setLinear(int startIndex, int endIndex,
				int startValue, int endValue)
{
	assert(endIndex > startIndex);
	for (int i = startIndex; i <= endIndex; i++) 
                _lut[i] = ( (i-startIndex)*(endValue-startValue) +
                          startValue*(endIndex-startIndex) ) /
                          (endIndex-startIndex);
	
}
///////////////////////////////////////////////////////////////////////////
// 	setAsArray
//		Different than setAsArray in parent.  This copies in 
//		array to store locally.  SetAsArray only saves address
//		of array residing somewhere else.
///////////////////////////////////////////////////////////////////////////
void DspLut::setAsArray( int* array ) 
{
	for (int i=0; i<=_lutSize; i++)
		_lut[i] = array[i];
}
///////////////////////////////////////////////////////////////////////////
// 	resizeLut
///////////////////////////////////////////////////////////////////////////
void    DspLut::resizeLut(int pixelSize) // pixelSize = numb bits in pixel
{
//	SET PIXEL SIZE AND NEW SIZE OF COLORMAP
	_pixelSize=pixelSize;
	_lutSize=(int) pow((double)2,_pixelSize);
	
//	CREATE A NEW COLORMAP
	delete [] _lut;
	_lut = new int [_lutSize];	
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dsplut.imake
#define SUBROUTINE dsplut 
#define MODULE_LIST DspLut.cc

#define P2SUB         /* SUBROUTINE */
#define P2_SUBLIB

#define USES_C_PLUS_PLUS
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_GUI
$ Return
$!#############################################################################
