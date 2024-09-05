$!****************************************************************************
$!
$! Build proc for MIPL module sfdu_object
$! VPACK Version 1.9, Wednesday, April 14, 2004, 15:35:49
$!
$! Execute by entering:		$ @sfdu_object
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
$!   TEST        Only the test files are created.
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
$ write sys$output "*** module sfdu_object ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sfdu_object.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
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
$   if F$SEARCH("sfdu_object.imake") .nes. ""
$   then
$      vimake sfdu_object
$      purge sfdu_object.bld
$   else
$      if F$SEARCH("sfdu_object.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sfdu_object
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sfdu_object.bld "STD"
$   else
$      @sfdu_object.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sfdu_object.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sfdu_object.com -mixed -
	-s SfduHeader.cc SfduBase.cc SfduPkt.cc SfduPvl.cc -
	-i sfdu_object.imake -
	-t tst_sfdu_base.cc tst_sfdu_base.imake tst_tds_if.cc tst_tds_if.imake -
	   tst_find_sfdu.cc tst_find_sfdu.imake README_tst tst_tds.upf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SfduHeader.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduHeader.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdio.h>

#include "SfduHeader.h"
#include "return_status.h"

//  SFDU Header specific constants ... could be globally defined, but
//  left here because it really isn't needed outside of this object

// Field Lengths
#define  SH_L_CNTRL		4
#define  SH_L_DESC		4
#define  SH_L_MARKER		8
#define  SH_L_LENGTH		8
#define  SH_L_EOF		8

// Field Offsets
#define  SH_O_CNTRL		0
#define  SH_O_VERSION		4
#define  SH_O_CLASS		5
#define  SH_O_DELIMITER		6
#define  SH_O_SPARE		7
#define  SH_O_DESC		8
#define  SH_O_EOF		12
#define  SH_O_MARKER		12
#define  SH_O_LENGTH		12
#define  SH_O_LENGTH_MSB	16
#define  SH_O_LENGTH_2SB	17
#define  SH_O_LENGTH_3SB	18
#define  SH_O_LENGTH_LSB	19

// Field Values
#define  SH_V_LENGTH_MSB	16777216
#define  SH_V_LENGTH_2SB	65536
#define  SH_V_LENGTH_3SB	256
#define  SH_V_LENGTH_LSB	1

//////////////////////////////////////////////////////////////////////////////
//
//				makeHeaderBuffer
//
//	Creates an SFDU Header buffer (20-bytes) based on the object's
//  component values.
//
//////////////////////////////////////////////////////////////////////////////

void	SfduHeader::makeHeaderBuffer(
  unsigned char	*buffer)
{ char	PrintString[256];

  if (_cntrlAuth == NULL)
     memmove(&buffer[SH_O_CNTRL],SFDU_CNTRL_AUTH_DEFAULT,SH_L_CNTRL);
  else memmove(&buffer[SH_O_CNTRL],_cntrlAuth,SH_L_CNTRL);
  buffer[SH_O_VERSION] = _version;
  buffer[SH_O_CLASS] = _classId;
  buffer[SH_O_DELIMITER] = _delimiter;
  if (_spare != 0) buffer[SH_O_SPARE] = _spare;
  else buffer[SH_O_SPARE] = '0';
  if (_dataDesc == NULL)
     memcpy(&buffer[SH_O_DESC],SFDU_DATA_DESC_DEFAULT,SH_L_DESC);
  else memcpy(&buffer[SH_O_DESC],_dataDesc,SH_L_DESC);

  switch (_version)
  { case '1': sprintf(PrintString,"%0*d",SH_L_LENGTH,_length);
              memmove(&buffer[SH_O_LENGTH],PrintString,SH_L_LENGTH);
              buffer[SH_O_DELIMITER] = '0';
    break;

    case '2': buffer[SH_O_LENGTH_MSB] = (_length / SH_V_LENGTH_MSB) % 256;
              buffer[SH_O_LENGTH_2SB] = (_length / SH_V_LENGTH_2SB) % 256;
              buffer[SH_O_LENGTH_3SB] = (_length / SH_V_LENGTH_3SB) % 256;
              buffer[SH_O_LENGTH_LSB] = (_length / SH_V_LENGTH_LSB) % 256;
              buffer[SH_O_DELIMITER] = '0';
    break;

    case '3': switch (_delimiter)
              { case 'A': sprintf(PrintString,"%0*d",SH_L_LENGTH,_length);
                          memmove(&buffer[SH_O_LENGTH],PrintString,SH_L_LENGTH);
                break;

                case 'B': buffer[SH_O_LENGTH_MSB] = (_length / SH_V_LENGTH_MSB) % 256;
                          buffer[SH_O_LENGTH_2SB] = (_length / SH_V_LENGTH_2SB) % 256;
                          buffer[SH_O_LENGTH_3SB] = (_length / SH_V_LENGTH_3SB) % 256;
                          buffer[SH_O_LENGTH_LSB] = (_length / SH_V_LENGTH_LSB) % 256;

                break;

                case 'C': // For disk files, C, E & F should have
                case 'E': // buffer[12] equal "00000001"
                case 'F': sprintf(PrintString,"%0*d",SH_L_EOF,_EOF_Count);
                          if (_classId == 'R')		// JPL TDS-ism
                             memmove(&buffer[SH_O_MARKER],_marker,SH_L_MARKER);
                          else memmove(&buffer[SH_O_EOF],PrintString,SH_L_EOF);
                break;

                case 'S': if (_marker == NULL)
                             memmove(&buffer[SH_O_MARKER],SFDU_MARKER_DEFAULT,
                                     SH_L_MARKER);
                          else memmove(&buffer[SH_O_MARKER],_marker,SH_L_MARKER);
                break;

                default:
                         cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
                              " ... Invalid delimiter: " << _delimiter << endl;
                break;
              }
    break;

    case '$': memcpy(buffer,"CCSD$$MARKER",12);
              if (_marker == NULL)
                 memmove(&buffer[SH_O_MARKER],SFDU_MARKER_DEFAULT,SH_L_MARKER);
              else memmove(&buffer[SH_O_MARKER],_marker,SH_L_MARKER);
    break;
  }

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				parseSfduHdr
//
//	Parses an input buffer containing the 20 bytes of an SFDU primary
//  header.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduHeader::parseSfduHdr(
  char	*buffer)
{ int	ReturnStatus = RTN_NORMAL;

  _length = 0;
  _EOF_Count = 0;
  memset(_marker,0,sizeof(_marker));

  memcpy(_cntrlAuth,&buffer[SH_O_CNTRL],SH_L_CNTRL);
  _version = buffer[SH_O_VERSION];
  _classId = buffer[SH_O_CLASS];
  _delimiter = buffer[SH_O_DELIMITER];
  _spare = buffer[SH_O_SPARE];
  memcpy(_dataDesc,&buffer[SH_O_DESC],SH_L_DESC);

  switch (_version)
  { case '1': _length = atoi((char *)&buffer[SH_O_LENGTH]);
              _delimiter = '0';
    break;

    //	Not platform independent code
    case '2': _length = ((unsigned char)buffer[SH_O_LENGTH_MSB] << 24) +
                        ((unsigned char)buffer[SH_O_LENGTH_2SB] << 16) +
                        ((unsigned char)buffer[SH_O_LENGTH_3SB] << 8)  +
                        (unsigned char)buffer[SH_O_LENGTH_LSB];
              _delimiter = '0';
    break;

    case '3': switch (_delimiter)
              { case 'A': _length = atoi((char *)&buffer[SH_O_LENGTH]);
                break;

                //	Not platform independent code
                case 'B': _length = ((unsigned char)buffer[SH_O_LENGTH_MSB] << 24) +
                                    ((unsigned char)buffer[SH_O_LENGTH_2SB] << 16) +
                                    ((unsigned char)buffer[SH_O_LENGTH_3SB] << 8)  +
                                    (unsigned char)buffer[SH_O_LENGTH_LSB];
                break;

                case 'C': // For disk files, C, E & F should have
                case 'E': // buffer[12] equal "00000001"
                case 'F': _EOF_Count = atoi((char *)&buffer[SH_O_EOF]);
                          memmove(_marker,(buffer+SH_O_MARKER),SH_L_MARKER);
                          ReturnStatus = RTN_SFDU_EOF;
                break;

                case 'S': memmove(_marker,(buffer+SH_O_MARKER),SH_L_MARKER);
                          if (_classId == 'Z' || _classId == 'U' ||
                              _classId == 'F') ReturnStatus =  RTN_SFDU_DATA;
                          else ReturnStatus = RTN_SFDU_UNKWN_LEN;
		break;

                default: 
                         cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
                              " ... Invalid delimiter: " << _delimiter << endl;
                         ReturnStatus = RTN_SFDU_INVALID;
		break;
              }
    break;

    case '$': if (strncmp(buffer,"CCSD$$MARKER",12) == 0)
                 ReturnStatus = RTN_SFDU_EOF;
              else
              { cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
                     " ... Invalid version identifier: " << _version << endl;
                ReturnStatus = RTN_SFDU_INVALID;
              }
    break;

    default: 
             cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
                  " ... Invalid version identifier: " << _version << endl;
             ReturnStatus = RTN_SFDU_INVALID;
  break;
  }

  return ReturnStatus;
}

//////////////////////////////////////////////////////////////////////////////
//
//				devPrint
//
//	Development to output header components.  The implementation of
//  this routine will probably change in the future.
//
//////////////////////////////////////////////////////////////////////////////

void	SfduHeader::devPrint( void )
{

  
  cout << _cntrlAuth << " Version: " << _version << " Class: " << _classId;
  cout << " Delimiter: " << _delimiter << endl;

  cout << "  Length: " << _length << " Marker: " << _marker << endl;


  return;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfduBase.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduBase.cc
//
//////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>

#include "SfduBase.h"
#include "Stopwatch.h"
#include "return_status.h"

#define DEV_DEBUG	0	// Turns on error debugging messages to cerr

//////////////////////////////////////////////////////////////////////////////
//
//				_resizeBuffer
//
//	Private function that will resize the SFDU buffer to accept large
//  SFDUs.  Firsts checks to make sure a larger buffer is required.  If
//  a buffer already exists, the data in the buffer is retained and copied
//  once the new, larger buffer has been created.  The retention of the
//  data is an optional, the default is to retain the data.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::_resizeBuffer(
  int	NewSize,
  int	RetainData )
{ int	OldSize = _bufferSize;
  unsigned char	*Temp = NULL;

  if (_bufferSize >= NewSize) return RTN_NORMAL;

  if (_buffer != NULL) Temp = _buffer;
  _bufferSize = (NewSize < DEFAULT_SFDU_SIZE) ? DEFAULT_SFDU_SIZE : NewSize;
  _buffer = new unsigned char [_bufferSize];

  if (_buffer == NULL)
  { _bufferSize = OldSize;
    _buffer = Temp;			// Retain old buffer size as recovery
    strcpy(_errorMessage,"Could not create 'new' buffer");
    return RTN_ALLOCATE_ERROR;
  }

  if (Temp != NULL)
  { if (RetainData) memcpy(_buffer,Temp,OldSize);
    delete [] Temp;
  }
  _data = &_buffer[SFDU_HEADER_LENGTH];


  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				wrapData
//
//	Accepts an unsigned char buffer and creates a variable-length SFDU
//  wrapping the data with an ending SFDU label.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::wrapData(
  int	Length,
  unsigned char	*Buffer,
  char	*Marker)
{ int	Status,
	TempSfduSize;
  SfduHeader	EOF_Sfdu;

  _sfduSize = 0;
  header.reset();

  Status = ingestData(Length,Buffer);
  if (RTN_FAILURE(Status)) return Status;

  if (Marker == NULL) header.setMarker(SFDU_MARKER_DEFAULT);
  else header.setMarker(Marker);
  header.setCntrlAuth("CCSD");
  header.setVersion('3');
  header.setClassId('Z');
  header.setDelimiter('S');
  header.setSpare('0');
  header.setDataDesc("0001");
  header.makeHeaderBuffer(_buffer);

  /***  Header SFDU complete, now add trailer SFDU  ***/

  TempSfduSize = _sfduSize + SFDU_HEADER_LENGTH;
  Status = _resizeBuffer(TempSfduSize,1);
  if (RTN_FAILURE(Status))
  { _sfduSize = 0;
    _sfduCount--;
    return Status;
  }

  EOF_Sfdu.parseSfduHdr(SFDU_HEADER_MARKER);
  EOF_Sfdu.setMarker(Marker);
  EOF_Sfdu.makeHeaderBuffer(&_buffer[_sfduSize]);
  _sfduSize = TempSfduSize;
  _sfduCount++;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				ingestData
//
//	Accepts an unsigned char buffer and creates a fixed-length SFDU
//  preceeding the data.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::ingestData(
  int	Length,
  unsigned char	*Buffer)
{ int	Status;

  _sfduSize = 0;
  header.reset();

  Status = _resizeBuffer((Length+SFDU_HEADER_LENGTH));
  if (RTN_FAILURE(Status)) return Status;
  _data = &_buffer[SFDU_HEADER_LENGTH];

  memmove(_data,Buffer,Length);

  header.setDefaultHeader();
  header.setDelimiter('A');
  header.setLength(Length);
  header.makeHeaderBuffer(_buffer);
  _sfduSize = Length + SFDU_HEADER_LENGTH;
  _sfduCount++;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				recordSfdu
//
//	Accepts a foreign SFDU object and records the byte stream in its
//  own SFDU log file.  This would be useful for added ancillary information
//  into the SFDU stream file that might help identify the file's use and
//  existance.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::recordSfdu(
  SfduBase	&Alien)
{ if (_sfduLog != NULL) return (Alien.sendSfdu(_sfduLog));
  return (RTN_NOT_AVAILABLE);
}

//////////////////////////////////////////////////////////////////////////////
//
//				getBufferSfdu
//
//	Extracts the SFDU structures from a memory buffer.  The buffer should
// start at the beginning of a known-length SFDU.  Transport SFDUs or SFDUs
// that a deliminated by a marker can not be processed by this routine.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::getBufferSfdu(
  unsigned char *Buffer)
{ int	Status,
	XferLength = SFDU_HEADER_LENGTH;

  _sfduSize = 0;
  header.reset();

  Status = header.parseSfduHdr( (char *)Buffer );
  if (RTN_FAILURE(Status))
  { sprintf(_errorMessage,"%s ... Could not parse buffer SFDU header",
            RTN_DFLT_MSG(Status));
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    if (_sfduLog != NULL) _sfduLog->output(Buffer,&XferLength);
    return Status;
  }

  if (Status == RTN_SFDU_EOF || Status == RTN_SFDU_DATA)
  { // Might want to verify marker if EOF or push marker on a stack
    if (Status == RTN_SFDU_EOF) _sfduLevel -= header.EOF_Count();
    else _sfduLevel++;
    _sfduCount++;
    _sfduSize = SFDU_HEADER_LENGTH;
    if (_sfduLog != NULL) _sfduLog->output(Buffer,&XferLength);
    return Status;
  }

  if (Status == RTN_SFDU_UNKWN_LEN)
  { _errorReason = Status;
    sprintf(_errorMessage,"%s ... Can not determine length for SFDU buffer",
            RTN_DFLT_MSG(Status));
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return RTN_SFDU_INVALID;
  }

  _sfduSize = header.length() + SFDU_HEADER_LENGTH;
  Status = _resizeBuffer ( _sfduSize );
  if (!RTN_SUCCESS(Status)) return Status;

  memcpy(_buffer,Buffer,_sfduSize);
  _data = &_buffer[SFDU_HEADER_LENGTH];
  _sfduCount++;
  XferLength = _sfduSize;
  if (_sfduLog != NULL) _sfduLog->output(_buffer,&XferLength);

  return (RTN_NORMAL);
}

//////////////////////////////////////////////////////////////////////////////
//
//				captureSfdu
//
//	Identifies an SFDU logging file that will contain a copy of the SFDU
//  byte stream.  If no file is specified, the logging of the SFDU records
//  will not occur.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::captureSfdu(
  char	*LogName)
{ int RtnStatus;

  if (LogName == NULL)
  { strcpy(_errorMessage,"Filename for SFDU capture log not supplied");
    return (RTN_INSUFF_DATA);
  }
  if (strlen(LogName) == 0)
  { strcpy(_errorMessage,"Filename for SFDU capture log not specified");
    return (RTN_MISSING_VALUE);
  }

  if (_sfduLog != NULL)		// Already created an SFDU logging file
  { strcpy(_errorMessage,"SFDU capture log already defined");
    return (RTN_EXISTS_ERROR);
  }
 
//  _sfduLog = new DataSinkDisk(LogName);
  _sfduLog = new DataSinkDisk();
  if (_sfduLog == NULL)
  { sprintf(_errorMessage,"%s ... Could not create SFDU sink",
            RTN_DFLT_MSG(RTN_ALLOCATE_ERROR));
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return RTN_ALLOCATE_ERROR;
  } else
  { RtnStatus = _sfduLog->open( LogName );
    if (!RTN_SUCCESS(RtnStatus))
    { sprintf(_errorMessage,"%s ... Could not open SFDU sink, %s",
            RTN_DFLT_MSG(RtnStatus),LogName);
#if DEV_DEBUG
      cerr << _errorMessage << endl;
#endif
      delete _sfduLog;
      _sfduLog = NULL;
    }
  }

  return RtnStatus;
}

//////////////////////////////////////////////////////////////////////////////
//
//				findNextSfdu
//
//	Searches the data source for a valid SFDU label.  By default, this
//  object keeps track of the SFDUs being received.  The only time this
//  function should be used is if the data source is corrupted.
//
//		NOT CURRENTLY IMPLEMENTED
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::findNextSfdu(
  DataSource	&Source,
  int		TimeOut)
{ int	Status,
	Idx,
	Remaining,
	XferLength;
  char	SearchBuffer[256],
	*pSB = SearchBuffer;
  Stopwatch	Timer;

/***
static int	CallLimiter = 0;
cerr << "Entering FindNextSfdu\n";
if (++CallLimiter > 2) exit(0);
***/

  /***
   ***  Set-up timer stuff (if needed)
   **/
  if (TimeOut > 0)
    if (RTN_FAILURE(Timer.start()))
  { strcpy(_errorMessage,"Could not get start timer");
    _errorReason = Timer.errorReason();
    TimeOut = 0;
#if DEV_DEBUG
      cerr << "System Resource Error: " << _errorMessage << " - " <<
              Timer.errorMessage() << endl;
#endif
  }

  /***
   ***  SFDU search loop
   ***
   ***      This search loop is a bit inefficent in that if it thinks it
   ***  found an SFDU (NJPL or CCSD), it rolls back the read buffer and
   ***  reenters the loop pointing to the beginning of what is hoped to be
   ***  the SFDU header.  This minimizes a bunch of logic needed to deal
   ***  with sucking in more data part way through the loop, but will
   ***  cause more iterations.  Barring an error, the only way out of this
   ***  loop is if an SFDU starts at the first byte read.
   **/
  do
  { /***
     ***  Read enough data for one SFDU header
     **/
    XferLength = SFDU_HEADER_LENGTH;
    Status = Source.input(SearchBuffer, &XferLength, TimeOut);

    if (!RTN_SUCCESS(Status) || (XferLength < SFDU_HEADER_LENGTH))
    { sprintf(_errorMessage,
              "%s ... Could not read data for SFDU search, %d vs %d",
              RTN_DFLT_MSG(Status),XferLength,SFDU_HEADER_LENGTH);
      _errorReason = Source.errorReason();
#if DEV_DEBUG
      cerr << _errorMessage << endl;
#endif
      if (RTN_NUMBER(Status) == RTN_V_TIMEOUT)
      { // Refill data buffer with what has already been read
        Source.reload(XferLength);
      }

      return Status;
    }

    /***
     ***  Determine time-out for next portion
     **/
    if (TimeOut > 0)
    { if ((Remaining = Timer.remaining(TimeOut)) < 0)
      { strcpy(_errorMessage,"Could not get time remaining");
        _errorReason = Timer.errorReason();
        TimeOut = 0;
#if DEV_DEBUG
        cerr << "System Resource Error: " << _errorMessage << " - " <<
                Timer.errorMessage() << endl;
#endif
      } else TimeOut = Remaining;			// set up for next cycle
    }


    /***
     ***  Search for NJPL or CCSD, an SFDU prefix
     **/
    for (Idx = 0; Idx<XferLength; Idx++)
    { for (;Idx<XferLength && (pSB[Idx] != 'N' && pSB[Idx] != 'C'); Idx++) ;
      if (XferLength-Idx < 4) break;
      if (strncmp(&pSB[Idx],"NJPL",4) == 0 ||
          strncmp(&pSB[Idx],"CCSD",4) == 0) break;	// Got one ... maybe
    }

    					// If less than 20 bytes in buffer,
					// reload remaining data and "reloop"
    if (Idx != 0) Source.reload((XferLength-Idx));
    else						// See if it is real
    { Status = header.parseSfduHdr(SearchBuffer);
      if (RTN_SUCCESS(Status))
      { // Refill data buffer with what has already been read
        Source.reload((SFDU_HEADER_LENGTH));
        _partialSfdu = 0;
        return (getNextSfdu(Source,TimeOut));
      } else Source.reload((SFDU_HEADER_LENGTH - 4));
    }

  } while ( 1 );		//  Only way out is finding one, or an error
}

//////////////////////////////////////////////////////////////////////////////
//
//				getNextSfdu
//
//	Obtains the next SFDU from the data source.  This is the main
//  operation of this object.  In the case of a "Data Unit" SFDU, which
//  contains many SFDU objects, this routine will only return the label
//  portion of that SFDU.  It will, however, track that it is "inside" a
//  multi-SFDU.  Both the begining and ending labels of a Data-Unit SFDU are
//  considered SFDUs and are book-keeped as such.
//
//	Upon successfully obtaining the next SFDU, this function identifies
//  the type of SFDU, as defined by the CCSDS standards, and returns this
//  as the return status.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::getNextSfdu(
  DataSource	&Source,
  int		TimeOut)
{ int	Status,
	LogStatus,
	Remaining,
	XferLength;
  char	HdrBuffer[24];
  Stopwatch	Timer;

  if (_partialSfdu) return(findNextSfdu(Source,TimeOut));

  _partialSfdu = _validSfdu = _sfduSize = 0;
  header.reset();

  /***
   ***  Set-up timer stuff (if needed)
   **/
  if (TimeOut > 0)
     if (RTN_FAILURE(Timer.start()))
  { strcpy(_errorMessage,"Could not get start timer");
    _errorReason = Timer.errorReason();
    TimeOut = 0;
#if DEV_DEBUG
      cerr << "System Resource Error: " << _errorMessage << " - " <<
              Timer.errorMessage() << endl;
#endif
  }

  /***
   ***  Get and parse the SFDU header
   **/
  XferLength = SFDU_HEADER_LENGTH;
  Status = Source.input(HdrBuffer, &XferLength, TimeOut);
  _sfduSize = XferLength;		// what we got so far

  if (!RTN_SUCCESS(Status) || (XferLength < SFDU_HEADER_LENGTH))
  { sprintf(_errorMessage,"%s ... Could not read data for header, %d vs %d",
            RTN_DFLT_MSG(Status),XferLength,SFDU_HEADER_LENGTH);
    _errorReason = Source.errorReason();
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif

    if (RTN_NUMBER(Status) == RTN_V_TIMEOUT)
    { // Refill data buffer with what has already been read
      Source.reload(XferLength);
      _sfduSize = XferLength = 0;
    } else
    { if (XferLength > 0) _partialSfdu = 1;
      if (_sfduLog != NULL)
      { LogStatus = _sfduLog->output(HdrBuffer,&XferLength);
#if DEV_DEBUG
        if (RTN_FAILURE(LogStatus))
           cerr << _sfduLog->errorMessage() << "; " <<
                   strerror(_sfduLog->errorReason()) << endl;
#endif
      }
    }

    return Status;
  }

  Status = header.parseSfduHdr(HdrBuffer);
  if (RTN_FAILURE(Status))
  { sprintf(_errorMessage,"%s ... Could not parse SFDU header",
            RTN_DFLT_MSG(Status));
    if (_sfduLog != NULL)
    { LogStatus = _sfduLog->output(HdrBuffer,&XferLength);
#if DEV_DEBUG
      if (RTN_FAILURE(LogStatus))
         cerr << _sfduLog->errorMessage() << "; " <<
                 strerror(_sfduLog->errorReason()) << endl;
#endif
    }

    _partialSfdu = 1;
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return Status;
  }

  /***
   ***  Handle compound & EOF SFDU records
   **/
  if (Status == RTN_SFDU_EOF || Status == RTN_SFDU_DATA)
  { // Might want to verify marker if EOF or push marker on stack
    if (Status == RTN_SFDU_EOF) _sfduLevel -= header.EOF_Count();
    else _sfduLevel++;
    _sfduCount++;
    _validSfdu = 1;
    _sfduSize = SFDU_HEADER_LENGTH;
    if (_sfduLog != NULL)
    { LogStatus = _sfduLog->output(HdrBuffer,&XferLength);
#if DEV_DEBUG
      if (RTN_FAILURE(LogStatus))
         cerr << _sfduLog->errorMessage() << "; " <<
                 strerror(_sfduLog->errorReason()) << endl;
#endif
    }
Source.discard();
    return Status;
  }

  /************************************************************
   ***
   ***  Any SFDU that gets here has data associated with the
   ***  SFDU label/header (LVO)
   ***
   ************************************************************/

  if (Status == RTN_SFDU_UNKWN_LEN)
  { Status = _determineLength(Source,TimeOut);
    if (!RTN_SUCCESS(Status))
    { _partialSfdu = 1;
      return Status;
    }
  }

  /***
   ***  Copy header to buffer and expand buffer if needed
   **/
  Status = _resizeBuffer( (header.length() + SFDU_HEADER_LENGTH) );
  if (!RTN_SUCCESS(Status))
  { // Refill data buffer with what has already been read
    Source.reload(SFDU_HEADER_LENGTH);
    header.reset();
    _sfduSize = XferLength = 0;
    return Status;
  }
  memcpy(_buffer,HdrBuffer,SFDU_HEADER_LENGTH);

  /***
   ***  Determine time-out for obtaining the data portion of sfdu
   **/
  if (TimeOut > 0)
  { if ((Remaining = Timer.remaining(TimeOut)) < 0)
    { strcpy(_errorMessage,"Could not get time remaining");
      _errorReason = Timer.errorReason();
      TimeOut = 0;
#if DEV_DEBUG
      cerr << "System Resource Error: " << _errorMessage << " - " <<
              Timer.errorMessage() << endl;
#endif
    } else TimeOut = Remaining;
  }

  /***
   ***  Get the rest of the pre-known-length SFDU data
   ***/
  XferLength = header.length();
  _data = &_buffer[SFDU_HEADER_LENGTH];
  Status = Source.input(_data,&XferLength,TimeOut);
  _sfduSize = XferLength + SFDU_HEADER_LENGTH;		// what we got so far

//
//  For TimeOut errors, it will be possible to have gotten a partial SFDU.
//  The next successful data trasnfer would/could be the tail-end of the
//  and the buffer would have to be re-synced. (Chances are this won't happen,
//  but need to prepare for it anyway.
//

  if (!RTN_SUCCESS(Status) || (XferLength != header.length()))
  { sprintf(_errorMessage,"%s ... Could not read data for SFDU, %d vs %d",
            RTN_DFLT_MSG(Status),XferLength,header.length());
    _errorReason = Source.errorReason();
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    if (RTN_NUMBER(Status) == RTN_V_TIMEOUT)
    { // Refill data buffer with what has already been read
      Source.reload(_sfduSize);
      header.reset();
      _sfduSize = XferLength = 0;
    } else
    { XferLength += SFDU_HEADER_LENGTH;			// for logging the SFDU
      _partialSfdu = 1;
    }
  } else
  { XferLength = _sfduSize;
    _validSfdu = 1;
    _sfduCount++;
Source.discard();
  }

  /***
   ***  Copy SFDU to SFDU logging file, if specificied
   ***/
  if (_sfduLog != NULL && XferLength)
  { LogStatus = _sfduLog->output(_buffer,&XferLength);
#if DEV_DEBUG
    if (RTN_FAILURE(LogStatus))
       cerr << _sfduLog->errorMessage() << "; " <<
               strerror(_sfduLog->errorReason()) << endl;
#endif
  }

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				_determineLength
//
//	Internal function that identifies the length of an SFDU that is
//  delimited by a character string marker.  This is only for "Data Objects"
//  not "Data Units".  If it can not identify the end of the SFDU, it returns
//  a non-zero value.  This routine assumes the the data buffer is at the
//  beginning of an SFDU.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::_determineLength( 
  DataSource	&Source,
  int	TimeOut )
/***
{ int	Idx,
	Status,
	XferLength;
  char	TempBuf[MAX_SFDU_LENGTH],
	*PtrWorking = TempBuf;
  SfduHeader	TempHeader;

  XferLength = MAX_SFDU_LENGTH;
//  Status = Source.look(TempBuf,&XferLength,TimeOut);
  Status = Source.look(TempBuf,&XferLength,0);
  if (RTN_FAILURE(Status)) return Status;  // well for now atleast

  for (Idx=12; Idx<XferLength; )
  { PtrWorking = strstr((TempBuf+Idx),header.marker());
    if (PtrWorking)
    { Idx = PtrWorking - TempBuf;
      Status = TempHeader.parseSfduHdr((PtrWorking-12));
      if (Status == RTN_SFDU_EOF)
      { header.setLength(Idx+8);	// Includes marker length
        return Status;
      } else Idx++; 
    } else
    { if (strlen(TempBuf+Idx) < XferLength) Idx += strlen(TempBuf+Idx) + 1; 
    }
  }

  if (Idx >= XferLength) return RTN_SFDU_INSUFF;

  return RTN_NORMAL;

***/
//
//  New
//
{ int	DataAvailable,
	Idx,
	MaxLook = MAX_SFDU_LENGTH,
	Remaining,
	Searched = 0,
	Status,
	XferLength;
  char	TempBuf[MAX_SFDU_LENGTH],
	*PtrWorking = TempBuf;
  SfduHeader	TempHeader;
  Stopwatch	Timer;



  /***
   ***  Set-up timer stuff (if needed)
   **/
  if (TimeOut > 0)
    if (RTN_FAILURE(Timer.start()))
  { strcpy(_errorMessage,"Could not get start timer");
    _errorReason = Timer.errorReason();
    TimeOut = 0;
#if DEV_DEBUG
      cerr << "System Resource Error: " << _errorMessage << " - " <<
              Timer.errorMessage() << endl;
#endif
  }


  /***
   ***  Grab all the expected data that is readily available
   **/

  XferLength = MaxLook;
  Status = Source.look(TempBuf,&XferLength,0);
  if (Status == RTN_INSUFF_MEMORY)	// Buffer too small, try buffer size
  { MaxLook = XferLength;
    Status = Source.look(TempBuf,&XferLength,0);
  }

  if (SFDU_HEADER_LENGTH > XferLength)	// Did not even get an SFDU header
  { if (RTN_FAILURE(Status)) return Status;
    XferLength = SFDU_HEADER_LENGTH;
    Status = Source.look(TempBuf,&XferLength,TimeOut);
    if (RTN_FAILURE(Status)) return Status;
  }

  // Have atleast an SFDU header by this point

  do
  { DataAvailable = XferLength;
    if (Searched > DataAvailable)
    { strcpy(_errorMessage,"Exceeded data available during length check");
      return RTN_SFDU_INSUFF;
    }

    for (Idx=Searched; Idx<=(DataAvailable-SFDU_MARKER_LENGTH); )
    { PtrWorking = strstr((TempBuf+Idx),header.marker());
      if (PtrWorking)
      { Idx = PtrWorking - TempBuf;
        Status = TempHeader.parseSfduHdr(PtrWorking-12);
        if (Status == RTN_SFDU_EOF)
        { header.setLength(Idx+SFDU_MARKER_LENGTH);
          return Status;
        } else Idx++;
      } else				// Could not find end marker string
					// possibly because 0x00 data in SFDU
      { if (strlen(TempBuf+Idx) < DataAvailable)
        { Idx += strlen(TempBuf+Idx) + 1;	// Skip string & NULL terminator
          while (*(TempBuf+Idx) == 0 && Idx < DataAvailable) Idx++;
        } else Idx = DataAvailable - SFDU_MARKER_LENGTH + 1;
      }
    }

    /***
     ***  Ran out of data in search, try to get more then resume search
     **/

    Searched = Idx;


    /***
     ***  Determine time-out for next portion
     **/
    if (TimeOut > 0)
    { if ((Remaining = Timer.remaining(TimeOut)) < 0)
      { strcpy(_errorMessage,"Could not get time remaining");
        _errorReason = Timer.errorReason();
        TimeOut = 0;
#if DEV_DEBUG
        cerr << "System Resource Error: " << _errorMessage << " - " <<
                Timer.errorMessage() << endl;
#endif
      } else TimeOut = Remaining;                       // set up for next cycle
    }

    // Get all data readily available
    XferLength = MaxLook;
    Status = Source.look(TempBuf,&XferLength,0);
    if (RTN_FAILURE(Status))
    { strcpy(_errorMessage,RTN_DFLT_MSG(Status));
      return RTN_SFDU_INSUFF;
    }

    // If no more is available, time-out the read for 1 additional byte
    if (XferLength == DataAvailable)
    { XferLength = DataAvailable + 1;
      Status = Source.look(TempBuf,&XferLength,TimeOut);
      if (!RTN_SUCCESS(Status))
      { strcpy(_errorMessage,RTN_DFLT_MSG(Status));
        return RTN_SFDU_INSUFF;
      }

      // Must-a got something, quickly look for more
      XferLength = MaxLook;
      Status = Source.look(TempBuf,&XferLength,0);
      if (RTN_FAILURE(Status))
      { strcpy(_errorMessage,RTN_DFLT_MSG(Status));
        return RTN_SFDU_INSUFF;
      }
    }
  } while (TimeOut != 0);
  strcpy(_errorMessage,"Timeout determining SFDU length");


  return RTN_SFDU_INSUFF;

}

//////////////////////////////////////////////////////////////////////////////
//
//				sendSfdu
//
//	This function sends an SFDU to the specified data sink.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduBase::sendSfdu(
  DataSink	*Sink,
  int		TimeOut)
{ int	Status,
	XferLength;

  XferLength = _sfduSize;
  if (_buffer == NULL) return(RTN_NOT_AVAILABLE);

  Status = Sink->output(_buffer,&XferLength, TimeOut);
  if (!RTN_SUCCESS(Status) || (XferLength != _sfduSize))
  { sprintf(_errorMessage,"%s ... Could not send SFDU, %d vs %d (%s)",
            RTN_DFLT_MSG(Status),XferLength,_sfduSize,
            strerror(Sink->errorReason()));
    _errorReason = Sink->errorReason();
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
  }

  return Status;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfduPkt.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduPkt.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>

#include "SfduPkt.h"
#include "Stopwatch.h"
#include "return_status.h"

#define DEV_DEBUG	0	// Turns on error debugging messages to cerr

//////////////////////////////////////////////////////////////////////////////
//
//				getNextPktSfdu
//
//	Gets the next Packet SFDU from the data source.  Routine decides that
//  an SFDU is a packet if the SFDU Class Id is not 'I'.  Additional checks
//  should be added if this is not appropriate.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPkt::getNextPktSfdu( 
  DataSource	&Source,
  int		TimeOut)
{ int	Status,
	Remaining;
  Stopwatch	Timer;

  /***
   ***  Set-up timer stuff (if needed)
   **/
  if (TimeOut > 0)
     if (RTN_FAILURE(Timer.start()))
  { strcpy(_errorMessage,"Could not get start timer");
    _errorReason = Timer.errorReason();
    TimeOut = 0;
#if DEV_DEBUG
      cerr << "System Resource Error: " << _errorMessage << " - " <<
              Timer.errorMessage() << endl;
#endif
  }

  Status = getNextSfdu(Source,TimeOut);
  if (!RTN_SUCCESS(Status))
  { return Status;
  }

  if (!pktSfdu())
  { if (TimeOut > 0)
       if ((Remaining = Timer.remaining(TimeOut)) < 0)
    { strcpy(_errorMessage,"Could not get time remaining");
      _errorReason = Timer.errorReason();
      TimeOut = 0;
#if DEV_DEBUG
      cerr << "System Resource Error: " << _errorMessage << " - " <<
              Timer.errorMessage() << endl;
#endif
    } else TimeOut = Remaining;
    return (getNextPktSfdu(Source,TimeOut));
  }

  _pktCount++;

  return RTN_NORMAL;
}

//////////////////////////////////////////////////////////////////////////////
//
//				getBufferPktSfdu
//
//	Gets a Packet SFDU from a supplied buffer.  Packet SFDUs are defined
//  as having an SFDU Class Id of 'I'.  Additionally, the buffer must start
//  at the beginning of a valid packet SFDU.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPkt::getBufferPktSfdu(
  unsigned char	*Buffer)
{ int	Status;

  Status = getBufferSfdu(Buffer);
  if (RTN_SUCCESS(Status) && header.classId() == 'I')
  { return RTN_NORMAL;
  }

//  strcpy(_errorMessage,"Buffer not a known-length data SFDU");
  sprintf(_errorMessage,"Buffer not a known-length data SFDU: %-8.8s",Buffer);
#if DEV_DEBUG
  cerr << _errorMessage << endl;
#endif

  return (RTN_SFDU_INVALID);
}

//////////////////////////////////////////////////////////////////////////////
//
//				extractChdo
//
//	Extracts a CHDO object identified by a specified CHDO Type or Id from
//  the SFDU object.  Obviously, if that CHDO does not exist in the SFDU, an
//  error will be returned.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPkt::extractChdo( 
  int	ChdoId,
  ChdoBase	&Chdo )
{ int	Idx = 0;

  do
  { Chdo.parse(&_data[Idx]);
    if (Chdo.type() == ChdoId)
    { if ((Chdo.length()+Idx+Chdo.headerLength()) > header.length())
         return RTN_CHDO_INCONSISTENT;
      return RTN_NORMAL;
    }
    /***
     ***  CHDO 001 is an aggregation header containing all but the
     ***  data CHDO.  It is returned if requested, but processing
     ***  continues directly after the 4-byte CHDO header for any
     ***  subsequent CHDOs
     **/
    if (Chdo.type() == 1) Idx += Chdo.headerLength();
    else Idx += Chdo.headerLength() + Chdo.length();
  } while (Idx < header.length());

  return (RTN_CHDO_NOT_FOUND);
}

//////////////////////////////////////////////////////////////////////////////
//
//				extractLabel
//
//	Extracts a CHDO object identified by position in the SFDU object.
//  If an invalid position is specified, an error is returned.  Generally
//  speaking, the positions are 1-based specified, however, there is a special
//  case CHDO (Type equal to '1') that includes all of the CHDOs.  In order
//  to get this CHDO, a position of '0' must be specified.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPkt::extractLabel(
  int	Position,
  ChdoBase	&Chdo )
{ int	Idx = 0;

  do
  { Chdo.parse(&_data[Idx]);
    if (Chdo.type() == 1)
    /***
     ***  CHDO 001 is an aggregation header containing all but the
     ***  data CHDO.  It is returned if requested (as position 0),
     ***  but is ignored with processing continuing directly after
     ***  the 4-byte CHDO header for any subsequent CHDOs
     **/
    { if (Position == 0)
      { if ((Chdo.length()+Idx+Chdo.headerLength()) > header.length())
           return RTN_CHDO_INCONSISTENT;
        return RTN_NORMAL;
      }
      Idx += Chdo.headerLength();
    } else
    { Idx += Chdo.headerLength() + Chdo.length();
      if (--Position == 0)
      { if (Idx > header.length()) return RTN_CHDO_INCONSISTENT;
        return RTN_NORMAL;
      }
    }
  } while (Idx < header.length());

  return (RTN_CHDO_NOT_FOUND);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfduPvl.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduPvl.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "SfduPvl.h"
#include "return_status.h"

#define DEV_DEBUG	0	// Turns on error debugging messages to cerr

//////////////////////////////////////////////////////////////////////////////
//
//				ingestPvlFile
//
//	Reads a PVL diskfile and generates a generic SFDU header around the
//  PVL object.  This will not generate a JPL AMMOS TDS acceptable query.
//  The routine is left here mainly for archival purposes.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPvl::ingestPvlFile(
  char	*fileName)
{ int	Status,
	PvlLength;
  char	PvlBuffer[PVL_BUFFER_SIZE];
  SfduBase	Temp;
  fstream	PvlFile;

  PvlFile.open(fileName, ios::in);
  if (!PvlFile)
  { sprintf(_errorMessage,"Could not open PVL file: %s",fileName);
    _errorReason = errno;
#if DEV_DEBUG
    cerr << "System Resource Error: " << _errorMessage << endl;
#endif
    return RTN_OPEN_ERROR;
  }

  PvlFile.read(PvlBuffer,PVL_BUFFER_SIZE);
  PvlLength = PvlFile.gcount();
  PvlFile.close();
  if (PvlLength == PVL_BUFFER_SIZE)
  { sprintf(_errorMessage,"Buffer too small for contents of PVL file: %s",
            fileName);
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return RTN_TOO_MUCH_DATA;
  }

  Status = wrapData( PvlLength, (unsigned char *)PvlBuffer,"PVL_FILE");
  if (RTN_FAILURE(Status))
  { sprintf(_errorMessage,"Could not ingest PVL file: %s",fileName);
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return Status;
  }

  header.setCntrlAuth("NJPL");
  header.setClassId('I');
  header.setDataDesc("L009");
  header.setDelimiter('S');
  header.makeHeaderBuffer(_buffer);

  /***  Out outer SFDU wrapper around the PVL SFDU  ***/
  Temp = *this;
  Status = wrapSfdu(Temp,"QUERYSPC");
  if (RTN_FAILURE(Status))
  { sprintf(_errorMessage,"Could not wrap PVL file: %s",fileName);
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return Status;
  }

  _data = _buffer + (2 * SFDU_HEADER_LENGTH);

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				ingestTdsPvlFile
//
//	Reads a PVL diskfile and generates a AMMOS/TDS SFDU header around
//  the PVL object.  This SFDU can then be used to query the TDS for data.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPvl::ingestTdsPvlFile(
  char	*fileName)
{ int	Status,
	PvlLength;
  char	PvlBuffer[PVL_BUFFER_SIZE];
  SfduBase	Temp;
  fstream	PvlFile;

  PvlFile.open(fileName, ios::in);
  if (!PvlFile)
  { sprintf(_errorMessage,"Could not open PVL file: %s",fileName);
    _errorReason = errno;
#if DEV_DEBUG
    cerr << "System Resource Error: " << _errorMessage << endl;
#endif
    return RTN_OPEN_ERROR;
  }

  PvlFile.read(PvlBuffer,PVL_BUFFER_SIZE);
  PvlLength = PvlFile.gcount();
  PvlFile.close();
  if (PvlLength == PVL_BUFFER_SIZE)
  { sprintf(_errorMessage,"Buffer too small for contents of PVL file: %s",
            fileName);
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return RTN_TOO_MUCH_DATA;
  }

  Status = _wrapTdsPvl( PvlLength, (unsigned char *)PvlBuffer);
  if (RTN_FAILURE(Status))
  { sprintf(_errorMessage,"Could not TDS wrap PVL file: %s",fileName);
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return Status;
  }

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				ingestTdsPvlBuffer
//
//	Generates a AMMOS/TDS SFDU header around the supplied buffer.  It is
//  expected that this buffer is a valid PVL object.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPvl::ingestTdsPvlBuffer(
  char	*buffer)
{ int	Status,
	PvlLength;

  PvlLength = strlen(buffer);
  Status = _wrapTdsPvl( PvlLength, (unsigned char *)buffer);
  if (RTN_FAILURE(Status))
  { sprintf(_errorMessage,"Could not TDS wrap PVL specification");
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
    return Status;
  }

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				extractPvlObject
//
//	Extracts the PVL object from the SFDU and places it in the supplied
//  buffer.
//
//////////////////////////////////////////////////////////////////////////////

int	SfduPvl::extractPvlObject(
  char	*buffer,
  int	length)
{ int	Status = RTN_NORMAL,
	PvlLength;

  PvlLength = _sfduSize - (4 * SFDU_HEADER_LENGTH);
  if (PvlLength > length)
  { strcpy(_errorMessage,"Buffer too small for PVL object");
    Status = RTN_INSUFF_MEMORY;
    PvlLength = length;
#if DEV_DEBUG
    cerr << _errorMessage << endl;
#endif
  }

  memmove(buffer,(_data+(2*SFDU_HEADER_LENGTH)),PvlLength);

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				_wrapTdsPvl
//
//	Wraps the required AMMOS/TDS SFDU labels around a PVL object,
//  supplied in the buffer.
//
//////////////////////////////////////////////////////////////////////////////

int     SfduPvl::_wrapTdsPvl(
  int	Length,
  unsigned char	*Buffer)
{ int	Status,
	TempSfduSize;
  unsigned char	*BufferPtr;

  _sfduSize = 0;
  TempSfduSize = Length + (4 * SFDU_HEADER_LENGTH);

  Status = _resizeBuffer(TempSfduSize);
  if (RTN_FAILURE(Status)) return Status;

  //  Primary Label
  BufferPtr = _buffer;
  header.parseSfduHdr(TDS_PRIMARY_RQST_LABEL);
  header.makeHeaderBuffer(BufferPtr);


  //  PVL SFDU Label
  BufferPtr += SFDU_HEADER_LENGTH;
  memmove(BufferPtr,TDS_PVL_RQST_LABEL,SFDU_HEADER_LENGTH);


  //  PVL Query Specification
  BufferPtr += SFDU_HEADER_LENGTH;
  memmove(BufferPtr,Buffer,Length);


  //  PVL End Marker Label
  BufferPtr += Length;
  memmove(BufferPtr,TDS_PVL_END_LABEL,SFDU_HEADER_LENGTH);


  //  End Marker Label
  BufferPtr += SFDU_HEADER_LENGTH;
  memmove(BufferPtr,TDS_PRIMARY_END_LABEL,SFDU_HEADER_LENGTH);


  _sfduSize = TempSfduSize;
  _sfduCount += 2;
  _data = _buffer + (2 * SFDU_HEADER_LENGTH);

  return RTN_NORMAL;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sfdu_object.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE sfdu_object
/*
/*   To Create the build file give the command:
/*
/*		$ vimake sfdu_object			(VMS)
/*   or
/*		% vimake sfdu_object			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE sfdu_object		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST SfduHeader.cc SfduBase.cc SfduPkt.cc SfduPvl.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
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
/**********  End of sfdu_object imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_sfdu_base.cc
//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()
#include <iostream.h>
#include <string.h>

#include "return_status.h"
#include "DataSourceDisk.h"
#include "SfduBase.h"

main(
  int	argc,
  char	*argv[])
{ int	Status;
  char	FileName[256];
  DataSourceDisk	Test;
  SfduBase		Sfdu;

  if (argc < 2)
  { cout << "Must supply an input SFDU filename: ";
    cin >> FileName;
  } else strcpy(FileName,argv[1]);

  if (argc > 2)
  { Status = Sfdu.captureSfdu(argv[2]);
    cout << "Catpure: " << RTN_DFLT_MSG(Status) << endl;
  }

  if (Test.open(FileName))
  { cout << "Could not open file: " << FileName << endl;
    exit(1);
  } else cout << "Opened " << FileName << endl << endl;

  while (RTN_SUCCESS(Status = Sfdu.getNextSfdu(Test,-1)))
  {
/**/
    cout << "Sfdu " << Sfdu.sfduCounter() << " bytes: " << Sfdu.sfduSize() <<
         endl; //" Status: " << RTN_DFLT_MSG(Status) << endl;
    Sfdu.header.devPrint();
/**/
/***
    cout << Sfdu.header.cntrlAuth() << " " << Sfdu.header.version() << " " <<
         Sfdu.header.classId() << " " << Sfdu.header.delimiter() << " - " <<
         Sfdu.header.length() << " (" << Sfdu.sfduCounter() << ")\n";
***/
  }

  cout << "Finished w/ " << Sfdu.sfduCounter() << " records" << endl;
  cout << "Status: " << RTN_DFLT_MSG(Status) << endl;

  return(Status);
}
$!-----------------------------------------------------------------------------
$ create tst_sfdu_base.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_sfdu_base
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_sfdu_base			(VMS)
/*   or
/*		% vimake tst_sfdu_base			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_sfdu_base		/* Only one of these */
/*#define PROCEDURE tst_sfdu_base		/* Only one of these */
/*#define SCRIPT tst_sfdu_base		/* Only one of these */
#define PROGRAM tst_sfdu_base		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_sfdu_base.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
/**/
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
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
/**********  End of tst_sfdu_base imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_tds_if.cc
//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()
#include <iostream.h>
#include <string.h>

#include "return_status.h"
#include "DataSourceDisk.h"
#include "DataSourceSocket.h"
#include "DataSinkSocket.h"
#include "SfduBase.h"
#include "SfduPvl.h"

typedef	struct {
	char	*Keyword;
	char	*Value;
	int	Length;
	} Table_typ;

char	_TdsHost[128];
char	_TdsPort[12];
char	_PvlFile[256];
char	_SfduLog[256];
int	_TdsPortInt;


void	paramProcessor( fstream &file );

main(
  int	argc,
  char	*argv[])
{ int	Status;
  char	ParamFileName[256];
  fstream	Param;
  SocketBase		Port;
  DataSourceDisk	Test;
  DataSourceSocket	TDS;
  DataSinkSocket	QUERY;
  SfduBase		Sfdu;
  SfduPvl		PVL;

  if (argc < 2)
  { cout << "Must supply an input parameter filename: ";
    cin >> ParamFileName;
  } else strcpy(ParamFileName,argv[1]);

  Param.open(ParamFileName,ios::in | ios::nocreate);
  if (Param.fail() || Param.bad())
  { cout << "Could not open file: " << ParamFileName << endl;
    return(1);
  } else cout << "Opened " << ParamFileName << endl;

  paramProcessor(Param);


  /***  Get PVL FIle  ***/
  Status = PVL.ingestTdsPvlFile(_PvlFile);
  if (RTN_FAILURE(Status))
  { cout << RTN_DFLT_MSG(Status) << " ... could not ingest PVL file: " <<
            _PvlFile << endl;
    return (1);
  } else cout << "Ingested PVL file: " << _PvlFile << endl;

  /***  Capture SFDU records  ***/
  if (strlen(_SfduLog))
  { Status = Sfdu.captureSfdu(_SfduLog);
    if (RTN_FAILURE(Status))
       cout << RTN_DFLT_MSG(Status) << " ... could not log SFDUs to disk\n";
    else
    { cout << "Capturing SFDUs to: " << _SfduLog << endl;
      Sfdu.recordSfdu(PVL);
    }
  } else cout << "Not capturing SFDUs\n";

  _TdsPortInt = atoi(_TdsPort);
  Status = Port.open(_TdsHost,_TdsPortInt);
  if (RTN_FAILURE(Status))
  { cout << RTN_DFLT_MSG(Status) << " ... openning TDS port\n";
    return (Status);
  } else cout << "TDS port opened\n";

  Status = QUERY.use(Port);
  if (RTN_FAILURE(Status))
  { cout << RTN_DFLT_MSG(Status) << " ... xfering port for TDS query\n";
    return (Status);
  }

  Status = TDS.use(Port);
  if (RTN_FAILURE(Status))
  { cout << RTN_DFLT_MSG(Status) << " ... xfering port for SFDU reading\n";
    return (Status);
  }

  Status = PVL.sendSfdu(&QUERY);
  if (RTN_FAILURE(Status))
  { cout << RTN_DFLT_MSG(Status) << " ... sending PVL SFDU\n";
    return (Status);
  } else cout << "Sent PVL SFDU\n";

  cout << endl;

  while (RTN_SUCCESS(Status = Sfdu.getNextSfdu(TDS,20)))
  { cout << "Sfdu " << Sfdu.sfduCounter() << " bytes: " << Sfdu.sfduSize() <<
         endl; //" Status: " << RTN_DFLT_MSG(Status) << endl;
//    Sfdu.header.devPrint();
    cout << flush;
    cout << endl << flush;
  }
  cout << "Finished w/ " << Sfdu.sfduCounter() << " records" << endl;
  cout << "Status: " << RTN_DFLT_MSG(Status) << endl;


  return(1);





cout << "Starting loop\n";
  while (RTN_SUCCESS(Status = Sfdu.getNextSfdu(Test,-1)))
  { cout << "Sfdu " << Sfdu.sfduCounter() << " bytes: " << Sfdu.sfduSize() <<
         endl; //" Status: " << RTN_DFLT_MSG(Status) << endl;
    Sfdu.header.devPrint();
  }

  cout << "Finished w/ " << Sfdu.sfduCounter() << " records" << endl;
  cout << "Status: " << RTN_DFLT_MSG(Status) << endl;

//  return(1);
}

void	paramProcessor( fstream &File )
{ int	Idx;
  char	Keyword[32],
	Equal[4],
	Value[256];

  Table_typ	Params[] = {
		{ "TDS_HOST", _TdsHost, sizeof(_TdsHost)},
		{ "TDS_PORT", _TdsPort, sizeof(_TdsPort)},
		{ "PVL_FILE", _PvlFile, sizeof(_PvlFile)},
		{ "SFDU_LOG", _SfduLog, sizeof(_SfduLog)},
		{ 0, 0 },
		};

  for (Idx=0; Params[Idx].Keyword; Idx++)
      memset(Params[Idx].Value,0,Params[Idx].Length);

  while (File >> Keyword >> Equal >> Value)
  { 
    for (Idx = 0; Params[Idx].Keyword; Idx++)
        if (strcmp(Keyword,Params[Idx].Keyword) == 0)
    { strcpy(Params[Idx].Value,Value);
      break;
    }
  }

/***
  for (Idx=0; Params[Idx].Keyword; Idx++)
      cout << Params[Idx].Keyword << " = " << Params[Idx].Value << endl;

  cout << flush;
**/

  return;

}
$!-----------------------------------------------------------------------------
$ create tst_tds_if.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_tds_if
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_tds_if			(VMS)
/*   or
/*		% vimake tst_tds_if			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_tds_if		/* Only one of these */
/*#define PROCEDURE tst_tds_if		/* Only one of these */
/*#define SCRIPT tst_tds_if		/* Only one of these */
#define PROGRAM tst_tds_if		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_tds_if.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
/**/
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
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
/**********  End of tst_tds_if imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_find_sfdu.cc
//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <stdlib.h>     // Cause of main()
#include <iomanip.h>    // Cause of main()
#include <iostream.h>
#include <string.h>

#include "return_status.h"
#include "DataSourceDisk.h"
#include "SfduBase.h"

main(
  int	argc,
  char	*argv[])
{ int	Status,
	Count = 0,
	Idx = 0,
	TestLength;
  char	FileName[256];
  DataSourceDisk	Test;
  SfduBase		Sfdu;

  if (argc < 2)
  { cout << "Must supply an input SFDU filename: ";
    cin >> FileName;
  } else strcpy(FileName,argv[1]);

  if (argc > 2) Count = atoi(argv[2]);

  if (Test.open(FileName))
  { cout << "Could not open file: " << FileName << endl;
    exit(1);
  } else cout << "Opened " << FileName << endl << endl;

  while ((Status = Sfdu.getNextSfdu(Test,-1)) != RTN_SFDU_EOF)
  { if (RTN_SUCCESS(Status))
    { cout << "Sfdu " << Sfdu.sfduCounter() << " bytes: " << Sfdu.sfduSize() <<
              endl; //" Status: " << RTN_DFLT_MSG(Status) << endl;
      Sfdu.header.devPrint();
    }

    if ((++Idx) == Count)
    { TestLength = SFDU_HEADER_LENGTH;
      Status = Test.input(FileName,&TestLength,-1);
      if (!RTN_SUCCESS(Status))
      { cerr << "Offest read stream failed: " << RTN_DFLT_MSG(Status) << endl;
      }
    }

   if (Idx > Count+4) break;
  }

  cout << "Finished w/ " << Sfdu.sfduCounter() << " records" << endl;
  cout << "Status: " << RTN_DFLT_MSG(Status) << endl;

  return(Status);
}
$!-----------------------------------------------------------------------------
$ create tst_find_sfdu.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_find_sfdu
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_find_sfdu			(VMS)
/*   or
/*		% vimake tst_find_sfdu			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_find_sfdu		/* Only one of these */
/*#define PROCEDURE tst_find_sfdu		/* Only one of these */
/*#define SCRIPT tst_find_sfdu		/* Only one of these */
#define PROGRAM tst_find_sfdu		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_find_sfdu.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
/**/
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
/**/
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
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
/**********  End of tst_find_sfdu imake file  **********/
$!-----------------------------------------------------------------------------
$ create README_tst
There are 3 test programs included for this module:
	tst_sfdu_base
	tst_find_sfdu
    &	tst_tds_if

'tst_sfdu_base' is basically an SFDU reader.  It will read any SFDU file and
copy it to a second SFDU log file if specified.  Upon completion, it displays
the number of SFDU records (not including the transport SFDU) read.
The required first parameter is the source SFDU file, the optional second
parameter is an output SFDU log file.  The SFDU log file will be an exact copy
ofthe original file.

'tst_find_sfdu' is very similar to 'tst_sfdu_base', except that it will
corrupt 1 record as it is reading the SFDU file and then try to find where the
next one starts.  The first parameter to the program is the SFDU filename, the
second one (and it is optional) is the SFDU record to corrupt.  When this is
run twice, once with and once withput the second parameter, the total number of
records read should differ by only one.  The program should be run without the
second parameter first, so that a valid number determined for the second
execution.

'tst_tds_if' is basically the same as tst_sfdu_base, although a valid TDS and
PVL file are needed to test this program.  The parameter file, tst_tds.upf, is
an example/template of the only input parameter this program uses.  Like the
tst_sfdu_base program, this one only reads SFDU records from a source,
in this case a TDS, and displays the number of records read.  It can also
log the SFDUs to a disk file if specified.  It is assumed, that a valid PVL
object and TDS are available to use with this program.
$!-----------------------------------------------------------------------------
$ create tst_tds.upf
TDS_HOST = <TDS hostname>
TDS_PORT = 6666
PVL_FILE = <PVL filename>
SFDU_LOG = <optional SFDU log filename>
$ Return
$!#############################################################################
