$!****************************************************************************
$!
$! Build proc for MIPL module sfdu_object_h
$! VPACK Version 1.9, Tuesday, October 31, 2000, 15:51:31
$!
$! Execute by entering:		$ @sfdu_object_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module sfdu_object_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to sfdu_object_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("sfdu_object_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @sfdu_object_h.bld "STD"
$   else
$      @sfdu_object_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sfdu_object_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sfdu_object_h.com -mixed -
	-s SfduHeader.h SfduBase.h SfduPkt.h SfduPvl.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SfduHeader.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef SFDU_HEADER_CLASS_H
#define SFDU_HEADER_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduHeader.h
//
//	SfduHeader is the class of an object that contains a generic SFDU
//  header.  It performs the primary SFDU header parsing/extraction, but does
//  not process, look at, or care about the actual data the SFDU contains.
//  This class does not validate the correctness of the header.  It just
//  parseHeaders the header into its defined fields.
//
//	The length of an SFDU can be defined by a number of mechanisms.  If
//  the length of the SFDU is determined by a character marker, this class
//  returns a length of 0.  All determinable lengths will return a value
//  that excludes the length of the SFDU header.  The minimum length of an
//  SFDU is 0 bytes.
//
//////////////////////////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>

#define  SFDU_HEADER_LENGTH	20
#define  SFDU_MARKER_LENGTH	 8
#define  SFDU_HEADER_DEFAULT		"CCSD3IA0000100000000"
#define  SFDU_HEADER_EXCHANGE_UNIT	"CCSD3ZS00001$DEFAULT"
#define  SFDU_HEADER_MARKER		"CCSD$$MARKER$DEFAULT"
#define  SFDU_MARKER_DEFAULT		"$DEFAULT"
#define  SFDU_DATA_DESC_DEFAULT		"0000"
#define  SFDU_CNTRL_AUTH_DEFAULT	"CCSD"


class	SfduHeader {

  public:
		void	devPrint( void );
			// Development tool to output the components of
			// a header ... implementation of this routine
			// will proabably change in the future.

		void	reset( void )
			// Clears all of the SFDU header components of
			// the object
			{ memset(_cntrlAuth,0,sizeof(_cntrlAuth));
			  memset(_dataDesc,0,sizeof(_dataDesc));
			  memset(_marker,0,sizeof(_marker));
			  _version = _classId = _delimiter = 0;
			  _length = _EOF_Count = 0;
			}
//
//  Routines for getting an SFDU header
//

		int	parseSfduHdr( char *buffer );
		// Extracts the individual SFDU Header components from the
		// supplied 20-byte buffer.

		//	SFDU header component fields
	const	char	*cntrlAuth( void ) { return (_cntrlAuth); }
		char	version( void) { return (_version); }
		char	classId( void ) { return (_classId); }
		char	delimiter( void ) { return (_delimiter); }
	const	char	*dataDesc( void ) { return (_dataDesc); }
	const	char	*marker( void ) { return (_marker); }
		int	EOF_Count(void) { return (_EOF_Count); }
		int	length( void ) { return (_length); }
		int	headerLength( void )	{ return (SFDU_HEADER_LENGTH); }

//
//  Routines for making/setting an SFDU header
//
		//	SFDU header component fields
		void	setCntrlAuth( char *a )	{ memmove(_cntrlAuth,a,4); }
		void	setVersion( char a)	{ _version = a; }
		void	setClassId( char a )	{ _classId = a; }
		void	setDelimiter( char a )	{ _delimiter = a; }
		void	setSpare( char a )	{ _spare = a; }
		void	setDataDesc( char *a )	{ memmove(_dataDesc,a,4); }
		void	setMarker( char *a )	{ memmove(_marker,a,8); }
		void	setEOF_Count( int a )	{ _EOF_Count = a; }
		void	setLength( int a )	{ _length = a; }

 		void	makeHeaderBuffer( unsigned char *buffer );
			// Creates the 20-byte SFDU header and stuffs it
			// into the supplied buffer.

		void	setDefaultHeader( void )
			// Sets the header object to a generic default
			{ _initializeSfduHdr();
			  parseSfduHdr(SFDU_HEADER_DEFAULT);
			}

	// Default Constructor
	SfduHeader ( char *buffer = NULL )
		{ _initializeSfduHdr();
		  if (buffer != NULL) parseSfduHdr(buffer);
		  return;
		}

	//  Assignment Constructor
	SfduHeader &operator= ( const SfduHeader &header )
		{ if (this == &header) return *this;

		  _version = header._version;
		  _classId = header._classId;
		  _delimiter = header._delimiter;
		  _EOF_Count = header._EOF_Count;
		  _length = header._length;
		  _spare = header._spare;
		  memmove(_cntrlAuth,header._cntrlAuth,sizeof(_cntrlAuth));
		  memmove(_dataDesc,header._dataDesc,sizeof(_dataDesc));
		  memmove(_marker,header._marker,sizeof(_marker));

		  return *this;
		}

	//  Destructor
	~SfduHeader ( void ) { }

  protected:
	char		_cntrlAuth[6];
	char		_version;
	char		_classId;
	char		_delimiter;
	char		_spare;
	char		_dataDesc[6];
	char		_marker[10];
	int		_EOF_Count;	// EOFs count in SFDU header
	int		_length;	// Could be unsigned, but it is
					// unlikely an SFDU would be that long


	void		_initializeSfduHdr( void ) { reset(); }

};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfduBase.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef SFDU_BASE_CLASS_H
#define SFDU_BASE_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduBase.h
//
//	SfduBase is the class of an object that contains a generic SFDU.
//  It performs the primary SFDU header parsing/extraction, but does
//  not process, look at, or care about the actual data the SFDU contains.
//
//	The length of an SFDU can be defined by a number of mechanisms.  If
//  this SFDU is a "Data Unit", which contains multiple SFDUs, this class
//  returns a length of 0, and only processes the first label as an SFDU
//  in and of itself.  All determinable legnths will return a value
//  that excludes the length of the SFDU header.  The minimum length of an
//  SFDU is 0 bytes.
//
//	This class of object has the capability to read, write, log and
//  count all SFDUs handled by the object.  It is possible for this class
//  to accept SFDUs from different data sources, this is not necessarily
//  a wise thing to do.  Asd an error recorvery feature, this class can
//  search an input source for the next possible SFDU.
//
//////////////////////////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>

#include "SfduHeader.h"
#include "DataSource.h"
#include "DataSink.h"
#include "DataSinkDisk.h"

#define  MAX_SFDU_LENGTH	4096
#define  DEFAULT_SFDU_SIZE	2048

class	SfduBase {

  public:
	SfduHeader	header;
//
//  Generic SFDU utility functions
//
	const	unsigned char	*buffer() { return (_buffer); }
		// Returns pointer to SFDU buffer

	int	captureSfdu( char *LogName );
		// Capture all 'read' SFDUs to an SFDU log file

	int	recordSfdu( SfduBase &Alien );
		// Ingest a foriegn SFDU to 'this's SFDU log file

	int	sendSfdu( DataSink *Sink, int TimeOut = -1 );
		// Send an SFDU to a data sink

	int	errorReason ( void ) { return _errorReason; }
		// Returns the lower-level result code that caused a failure

	const char	*errorMessage( void ) { return _errorMessage; }
		//  Returns a text message identifiing the error that occured

	int	sfduSize( void ) { return _sfduSize; }
		// Returns the current size of the SFDU

	int	pktSfdu( void )
		{ return (_validSfdu && header.classId() == 'I');
		}
		// Identifies the SFDU as containing a packet.
		// This function may need some refinement as more types
		// of packets are obtained.

	int	validSfdu( void ) { return _validSfdu; }
		// Identifies that the object contains a valid SFDU
//
//  Stuff for getting SFDUs
//
	int	findNextSfdu( DataSource &Source, int TimeOut = -1 );
		// Searches a buffer stream for the next SFDU. If this
		// routine is needed, things are not good.
		//	NOT CURRENTLY IMPLEMENTED

	int	getNextSfdu( DataSource &Source, int TimeOut = -1 );
		// Obtains the next complete 'known-length' SFDU.  Data
		// transport SFDUs (open-ended) are recognized, logged and
		// skipped.

	int	getBufferSfdu( unsigned char *Buffer );
		// Extracts an SFDU that is stored in a memory buffer.
		// This routine only works with known-length SFDUs

	int	sfduCounter( void ) { return _sfduCount; }
		// Returns the number of SFDUs 'read'.  Does not include
		// transport SFDUs
//
//  Stuff for making an SFDU
//
	int	ingestData( int Length, unsigned char *Buffer );
		// Fills the SFDU from a buffer instead of a data source

	int	wrapData( int Length, unsigned char *Buffer, char *Marker );
		// Wraps an SFDU buffer within a data transport SFDU

	int	wrapSfdu( SfduBase &Sfdu, char *Marker )
		// Wraps a foriegn SFDU object within a data transport SFDU
		{ int Status;
		  Status = wrapData(Sfdu.sfduSize(),
		                    (unsigned char*)Sfdu.buffer(),Marker);
		  if (_sfduSize != 0) _sfduCount++;
		  return Status;
		}


	// Default Constructor
	SfduBase ( char *LogName = NULL )
		{ _initialize();
		  if (LogName != NULL) _sfduLog = new DataSinkDisk(LogName);
		  return;
		}

	// Copy Constructor
	SfduBase ( const SfduBase &sfdu )
		{ *this = sfdu;
                  if (_sfduLog != NULL) _sfduLog = NULL; // no logging allowed
                  if (_buffer != NULL)
                  { _buffer = new unsigned char [_bufferSize];
                    if (_buffer != NULL)
                       memcpy(_buffer,sfdu._buffer,_sfduSize);
                  }
		  return;
		}

	//  Assignment Constructor
	SfduBase &operator= ( const SfduBase &sfdu )
		{ if (this == &sfdu) return *this;

		  _initialize();

		  this->header = sfdu.header;
		  if (sfdu._sfduLog != NULL) _sfduLog = NULL; // no logging allowed
		  if (sfdu._buffer != NULL)
                  { _buffer = new unsigned char [sfdu._bufferSize];
                    if (_buffer != NULL)
                    { memcpy(_buffer,sfdu._buffer,sfdu._sfduSize);
		      _sfduSize = sfdu._sfduSize;
		      _bufferSize = sfdu._bufferSize;
		      _sfduCount = sfdu._sfduCount;
		      _sfduLevel = sfdu._sfduLevel;
		      _partialCount = sfdu._partialCount;
		    }
                  }
		  return *this;
		}

	// Destructor
	virtual ~SfduBase ( void )
		{ if (_buffer != NULL) delete [] _buffer;
		  if (_sfduLog != NULL) delete _sfduLog;
		}

  protected:
	int		_errorReason;
	int		_sfduLevel;
	int		_sfduCount;
	int		_partialCount;
	int		_sfduSize;
	int		_bufferSize;
	int		_validSfdu;
	int		_partialSfdu;
	char		_errorMessage[256];
	unsigned char	*_buffer;
	unsigned char	*_data;
	DataSinkDisk	*_sfduLog;

	void	_initialize( void )
		{ _sfduLog = NULL;
		  _buffer = _data = NULL;
		  memset(_errorMessage,0,sizeof(_errorMessage));
		  _bufferSize = _sfduSize = _sfduCount = _sfduLevel =
		                _errorReason = _validSfdu = _partialSfdu =
		                _partialCount = 0;
		  return;
		}

	int	_resizeBuffer ( int NewSize, int RetainData = 1 );

	int	_determineLength( DataSource &Source, int TimeOut = -1 );
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfduPkt.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef SFDU_PKT_CLASS_H
#define SFDU_PKT_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduPkt.h
//
//	SfduPkt is the class of an object that contains a generic SFDU
//  header.  It performs the primary SFDU header parsing/extraction, but does
//  not process, look at, or care about the actual data the SFDU contains.
//  This class does not validate the correctness of the header.  It just
//  parseHeaders the header into its defined fields.
//
//	The length of an SFDU can be defined by a number of mechanisms.  If
//  the length of the SFDU is determined by a character marker, this class
//  returns a length of 0.  All determinable lengths will return a value
//  that excludes the length of the SFDU header.  The minimum length of an
//  SFDU is 0 bytes.
//
//////////////////////////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>

#include "SfduBase.h"
#include "ChdoBase.h"

class	SfduPkt : public SfduBase {

  public:
	int	extractChdo ( int ChdoId, ChdoBase &Chdo );
		// Extracts the CHDO identified by type

	int	extractLabel ( int Position, ChdoBase &Chdo );
		// Extracts a CHDO identified by position in the SFDU

	int	pktSfduCounter( void ) { return (_pktCount); }
		// Returns the count of Packet SFDUs that have been returned

	int	getNextPktSfdu( DataSource &Source, int TimeOut = -1 );
		// Gets the next Packet SFDU from the data source.

	int	getBufferPktSfdu( unsigned char *Buffer );
		// Gets a Packet SFDU from a supplied buffer.  The buffer
		// must start at the beginning of a valid Packet SFDU.

	// Default Constructor
	SfduPkt ( char *LogName = NULL )
		{ _pktCount = 0;
                  if (LogName != NULL) captureSfdu(LogName);
		  return;
		}

	// Destructor
	virtual ~SfduPkt ( void ) { }

  protected:
	int		_pktCount;
};

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SfduPvl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef SFDU_PVL_CLASS_H
#define SFDU_PVL_CLASS_H

//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduPvl.h
//
//	SfduPvl is the class of an object that contains a generic SFDU
//  header.  It performs the primary SFDU header parsing/extraction, but does
//  not process, look at, or care about the actual data the SFDU contains.
//  This class does not validate the correctness of the header.  It just
//  parseHeaders the header into its defined fields.
//
//	The length of an SFDU can be defined by a number of mechanisms.  If
//  the length of the SFDU is determined by a character marker, this class
//  returns a length of 0.  All determinable lengths will return a value
//  that excludes the length of the SFDU header.  The minimum length of an
//  SFDU is 0 bytes.
//
//////////////////////////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>

#include "SfduBase.h"

#define  TDS_PRIMARY_RQST_LABEL		"CCSD3ZS00001QUERYSPC"
#define  TDS_PRIMARY_END_LABEL		"CCSD3RE00000QUERYSPC"

#define  TDS_PVL_RQST_LABEL		"NJPL3IS0L009TDSQUERY"
#define  TDS_PVL_END_LABEL		"CCSD3RE00000TDSQUERY"

#define  PVL_BUFFER_SIZE	4096
/***
 ***  PVL SFDU structure looks like ...
 ***
 ***	SFDU_RQST_LABEL
 ***		PVL_RQST_LABEL
 ***			PVL statements
 ***		PVL_END_LABEL
 ***	SFDU_END_LABEL
 **/

class	SfduPvl : public SfduBase {

  public:
	int	ingestPvlFile( char *fileName );	// Generically wrapped
		// Reads a PVL diskfile and generates a generic SFDU header
		// around the data.

	int	ingestTdsPvlFile( char *fileName );	// JPL TDS specific
		// Reads a PVL diskfile and generates a AMMOS/TDS SFDU header
		// around the data.

	int	ingestTdsPvlBuffer( char *buffer );	//  JPL TDS specific
		// Generates a AMMOS/TDS SFDU header around the supplied buffer.

	int	extractPvlObject( char *buffer, int Length );
		// Extracts the PVL data from the SFDU and places it in the
		// supplied buffer.

	// Default Constructor
	SfduPvl ( void )
		{ 
		  return;
		}

	// Destructor
	virtual ~SfduPvl ( void ) { }

  protected:
	int	_wrapTdsPvl( int Length, unsigned char *Buffer );
};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
