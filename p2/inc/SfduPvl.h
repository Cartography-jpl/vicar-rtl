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
