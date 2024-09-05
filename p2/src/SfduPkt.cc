//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduPkt.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <iostream>
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
