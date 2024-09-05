//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduBase.cc
//
//////////////////////////////////////////////////////////////////////////////
#include <iostream>
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
	{ if ((int) strlen(TempBuf+Idx) < DataAvailable)
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
