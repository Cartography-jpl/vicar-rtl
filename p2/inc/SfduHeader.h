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
