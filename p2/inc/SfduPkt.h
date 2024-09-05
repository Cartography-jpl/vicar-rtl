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
