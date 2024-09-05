//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduPvl.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <iostream>
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

