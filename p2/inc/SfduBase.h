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
