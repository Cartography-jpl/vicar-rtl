$!****************************************************************************
$!
$! Build proc for MIPL module dspfile
$! VPACK Version 1.8, Monday, April 01, 1996, 21:52:51
$!
$! Execute by entering:		$ @dspfile
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
$ write sys$output "*** module dspfile ***"
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
$ write sys$output "Invalid argument given to dspfile.com file -- ", primary
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
$   if F$SEARCH("dspfile.imake") .nes. ""
$   then
$      vimake dspfile
$      purge dspfile.bld
$   else
$      if F$SEARCH("dspfile.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dspfile
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dspfile.bld "STD"
$   else
$      @dspfile.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dspfile.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dspfile.com -mixed -
	-s DspFile.cc DspFileNameCmd.cc -
	-i dspfile.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DspFile.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// DspFile.cc
//	Warning - only Open and Write are tested
///////////////////////////////////////////////////////////////
#include "DspFile.h"
#include <errno.h>

///////////////////////////////////////////////////////////////
//	CONSTRUCTOR
///////////////////////////////////////////////////////////////
DspFile::DspFile()
{
//	
	_bytesWrittenInLastWrite = 0;
	_bytesReadInLastRead = 0;
	_eof = False;
	_open = False;
	_buffer = NULL;
	_bufferSize = 0; // for reading from file (not writing)
	strcpy(_fileName,"");
	_file = (FILE *) NULL;
	strcpy(_flags,"a+");
}

///////////////////////////////////////////////////////////////
//	open
///////////////////////////////////////////////////////////////
DSPSTATUSTYPE DspFile::open( char * fileName, int bufferSize,
					 Boolean read, Boolean write,
					 Boolean append, Boolean truncate) 			
{
	assert ( strlen(fileName) <= MAX_NAMESTRINGLENGTH );
	strcpy(_fileName, fileName);
	if ( bufferSize < 0) _bufferSize = 0;
	else _bufferSize = bufferSize;
	setFlags(read, write, append ,truncate); 
	if (createNewBuffer(_bufferSize)== __failure)
		return __failure;
	
	_file = fopen(_fileName, _flags );
	if (_file == NULL) {
	 	_open = False;
	 	return __failure;
	}
	else {
		_open = True;
		return __success;
	}
}

///////////////////////////////////////////////////////////////
//	close
///////////////////////////////////////////////////////////////
void DspFile::close()
{
	deleteBuffer();
	fclose(_file);
	_open = False;
}


///////////////////////////////////////////////////////////////
//	readAllBytes
///////////////////////////////////////////////////////////////
DSPSTATUSTYPE DspFile::readAllBytes(int nbytes, char * buffer)
{
	int totalBytesRead = 0;
	_eof = False;
	if (nbytes > 0) {
	  while (totalBytesRead < nbytes) {
		int n = fread ( buffer, (size_t) 1,(size_t) nbytes, _file);
		if ( n == 0) {
			_bytesReadInLastRead = totalBytesRead;
			return __success;
		}
		if ( n < 0 ) {
			_error = __success;
			_bytesReadInLastRead = totalBytesRead;
			return __failure;
		}
		totalBytesRead += n;
		nbytes -=  n;
		buffer += n;
	  }
	}
	_bytesReadInLastRead = totalBytesRead;
	return __success;
}
///////////////////////////////////////////////////////////////
//	readOnce
///////////////////////////////////////////////////////////////
DSPSTATUSTYPE DspFile::readOnce(int nbytes, char * buffer)
{
	_bytesReadInLastRead = 0;
	_eof = False;
	if  (nbytes > 0) {
	  int n = fread ( buffer, (size_t) 1, (size_t) nbytes,_file);
	  if ( n == 0) {
			_bytesReadInLastRead = 0;
			return __success;
	  }
	  if ( n < 0 ) {
			_error = __failure;
			_bytesReadInLastRead = 0;
			return __failure;
	  }
	  if ( n > 0) 
			_bytesReadInLastRead = n;
	}
	return __success;
}
///////////////////////////////////////////////////////////////
//	writeAllBytes
//		{tested and works}
///////////////////////////////////////////////////////////////
DSPSTATUSTYPE DspFile::writeAllBytes(int nbytes, char * buffer)
{
	_bytesWrittenInLastWrite = 0;
	int totalBytesRead = 0;
	_eof = False;
	int remainingBytes = nbytes;
	if (nbytes > 0 && isOpened()) {
	    while (remainingBytes > 0 ) {
		int n = fwrite ( buffer,(size_t) 1, (size_t) remainingBytes,_file);
		if ( n < 0 ) {
			_error = __failure;
			_bytesWrittenInLastWrite = totalBytesRead;
			return __failure;
		}
		totalBytesRead += n;	
		buffer = ((char *)(buffer + n));
		remainingBytes = nbytes - totalBytesRead;
	   }
	   _bytesWrittenInLastWrite = totalBytesRead;
	}
	return __success;
}
///////////////////////////////////////////////////////////////
//	writeOnce  
///////////////////////////////////////////////////////////////
DSPSTATUSTYPE DspFile::writeOnce(int nbytes, char * buffer)
{
	int n=0;
	_bytesWrittenInLastWrite = 0;
	_eof = False;
	if (nbytes > 0) {
	  int n = fwrite ( buffer, (size_t) 1, (size_t) nbytes,_file);
	  if ( n < 0 ) {
			_error = __failure;
			_bytesWrittenInLastWrite = 0;
			return __failure;
	  }
	}
	_bytesWrittenInLastWrite = n;
	return __success;
}
///////////////////////////////////////////////////////////////
//	printErrorToScreen
///////////////////////////////////////////////////////////////
void DspFile::printErrorToScreen(char * msg) 
{
	if (errno < 0 )
		perror( msg );
	else
		printf( msg );
}
///////////////////////////////////////////////////////////////
//	isEOF
///////////////////////////////////////////////////////////////
Boolean	DspFile::isEOF()
{ 
	assert (isOpened());
	if (feof( _file ) ) return True;
	else return False;
}
///////////////////////////////////////////////////////////////
//	createBuffer (non-public)
///////////////////////////////////////////////////////////////
DSPSTATUSTYPE DspFile::createNewBuffer(int bufferSize)
{
	if (_buffer != NULL ) deleteBuffer();
	_buffer = new char[bufferSize];
	if ( _buffer == NULL ) {
		_error = __failure;
		return __failure;
	}
	return __success;
}
///////////////////////////////////////////////////////////////
//	deleteBuffer (non-public)
///////////////////////////////////////////////////////////////
void DspFile::deleteBuffer()
{
	if (_buffer != NULL ) {
		delete _buffer ;
		_buffer = NULL;
	}
}
///////////////////////////////////////////////////////////////
//	setFlags (non-public)
//		if both append and truncate are set, append is used
//		
///////////////////////////////////////////////////////////////
void DspFile::setFlags(Boolean read, Boolean write,
				Boolean append,Boolean truncate)
{
	assert ( read | write );
	strcpy(_flags,"");
	if (read && !write) strcpy(_flags,"r");
	if (!read && write) strcpy(_flags,"w");
	if (read && write && !truncate) strcpy(_flags,"r+");
	if (read && write && truncate)strcpy(_flags, "w+");
	if (read && write && append ) strcpy(_flags, "a+");
	if (!read && write && truncate)strcpy(_flags, "w");
	if (!read && write && append) strcpy(_flags, "a");
		
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DspFileNameCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// DspFileNameCmd: A Command class that loads a file name.
//		very similar to MOTIFAPPLIB/DspFileNameCmd. 
////////////////////////////////////////////////////////////////////////
#include "DspFileNameCmd.h"
#include "CmdValue.h"
#include <assert.h>

DspFileNameCmd::DspFileNameCmd(char *name, int active, 
				XtPointer obj, void (*fp)(XtPointer, char *))
		: NoUndoCmd(name, active)
{
   _fp = fp;
   _obj = obj;
   _fp = fp; 
}

void DspFileNameCmd::doit()
{
   assert(_fp);
   
   (*_fp)(_obj,(char *)_value); // set value in another cmd class
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dspfile.imake
#define SUBROUTINE dspfile
#define MODULE_LIST \
		DspFile.cc DspFileNameCmd.cc 

#define P2SUB
#define P2_SUBLIB
 
#define USES_C_PLUS_PLUS
#define LIB_FORTRAN
#define LIB_GUISUB
#define LIB_MOTIFAPP
#define LIB_MOTIF
#define LIB_GUI
$ Return
$!#############################################################################
