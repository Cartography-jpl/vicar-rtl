//	Copyright (c) 1999, California Institute of Technology
//	U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				SfduHeader.cc
//
//////////////////////////////////////////////////////////////////////////////

#include <iostream>
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
		  std::cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
		    " ... Invalid delimiter: " << _delimiter << std::endl;
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
		  std::cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
		    " ... Invalid delimiter: " << _delimiter << std::endl;
                         ReturnStatus = RTN_SFDU_INVALID;
		break;
              }
    break;

    case '$': if (strncmp(buffer,"CCSD$$MARKER",12) == 0)
                 ReturnStatus = RTN_SFDU_EOF;
              else
		{ std::cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
		    " ... Invalid version identifier: " << _version << std::endl;
                ReturnStatus = RTN_SFDU_INVALID;
              }
    break;

    default: 
      std::cerr << RTN_DFLT_MSG(RTN_SFDU_INVALID) <<
	" ... Invalid version identifier: " << _version << std::endl;
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

  
  std::cout << _cntrlAuth << " Version: " << _version << " Class: " << _classId;
  std::cout << " Delimiter: " << _delimiter << std::endl;

  std::cout << "  Length: " << _length << " Marker: " << _marker << std::endl;


  return;
}

