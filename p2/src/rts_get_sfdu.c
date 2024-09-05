/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/***
 *	The purpose of these routines are to establish a link between
 *  either a Telemetry Delivery Subsystem (TDS) or SFDU file, and the
 *  MIPL Real Time telemetry processor.  For the TDS, this requires
 *  a TCP/IP connection  using client/server sockets.
 *
 *	This package will work on UNIX and VMS (running Multinet) platforms
 **/

#include "xvmaininc.h"

#if VMS_OS
#define  MULTINET_OLD_STYLE	0
#include <types.h>
#include <socket.h>
#include <ioctl.h>
/*
 * Internet address				.... MULTINET Sucks
 */
struct in_addr {
	u_long s_addr;
};
/*
 * Socket address, internet style.		.... MULTINET Sucks
 */
struct sockaddr_in {
	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	char	sin_zero[8];
};
#define	INADDR_ANY		(u_long)0x00000000
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <netdb.h>
#include <fcntl.h>
#include <unistd.h>
#endif
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "rts_sfdu.h"
#include "rts_cntrl.h"
#include "rts_errors.h"
#include "rts_logging.h"
#include "rts_typedefs.h"
#include "rts_const_defs.h"

#define      BUF_SIZE      32000

static char	LogMsgBuf[256];
static int	SfduCounter = 0, 
		NChan = -1,
		Chan = -1,
		TimeLimit = -1;
static FILE	*SfduFile = NULL;
static Ubyte	Buffer[BUF_SIZE],
		*Cptr = Buffer,
		*Eptr = Buffer;

/***  Function Prototypes  ***/
static	void	determine_sfdu_length( sfdu_record_typ *);
static	int	rts_accept_sfdu_socket();
static	int	rts_create_sfdu_socket( char *, int *);
	int	rts_get_file_sfdu( sfdu_record_typ *);
	int	rts_get_socket_sfdu( sfdu_record_typ *, int);
static	int	rts_read_sfdu_file( );
extern	int	rts_tds_connect( char *, int *, int);
extern	int	rts_tds_disconnect( int *);
extern	int	rts_tds_query( int, tds_query_typ *);
static	int	rts_verify_sfdu( sfdu_record_typ *);

#define MODULE_NAME "G_SFDU"
/*****************************************************************************
 *				DETERMINE_SFDU_LENGTH
 *
 *	Determines the length of a variable length SFDU (if possible).  It
 *  does not include the length of a termination SFDU header/record that may
 *  be following the current SFDU record.
 ****************************************************************************/
static	void	determine_sfdu_length(
  sfdu_record_typ	*Sfdu)
{ int	Idx;
  char	*PtrSfdu,
	*SfduBuffer = Sfdu->Buffer;

  for (Idx=20; Idx<Sfdu->BufferLth;)
  { PtrSfdu = strstr((char *)(SfduBuffer+Idx),"CCSD");
    if (PtrSfdu)
    { if (strncmp(Sfdu->Hdr.Marker,(PtrSfdu+12),8) == 0 && PtrSfdu[5] == 'R' &&
          (PtrSfdu[6] == 'E' || PtrSfdu[6] == 'F'))
      { Sfdu->Hdr.Length = (int)PtrSfdu - (int)SfduBuffer;
/***
      { Sfdu->Hdr.Length = (int)PtrSfdu - (int)SfduBuffer + 20;
 **/
        return;
      }
      Idx = (int)PtrSfdu - (int)SfduBuffer + 1;
    } else Idx++;
  }
  Sfdu->Hdr.Length = -1;		/* Can't determine length */

  return;
}

#undef  MODULE_NAME
#define MODULE_NAME "G_SFDU"
/*****************************************************************************
 *				RTS_GET_SFDU
 *
 ****************************************************************************/
int	rts_get_sfdu(
  telemproc_typ		*tlm_cntrl,
  sfdu_record_typ	*Sfdu)
{ int	status;
  static int	TdsQuerySocket;

  memset((void *)Sfdu,0,sizeof(sfdu_record_typ));

  switch (tlm_cntrl->SFDU_Source)
  { case SFDU_SRC_TDS:
    case SFDU_SRC_PORT:
               /*** Initialize socket connections ... if needed  ***/
               if (NChan < 0)
               { /*  Open output SFDU file for logging  */
                 if (!SfduFile && strlen(tlm_cntrl->SFDU_Filename))
                 { SfduFile = fopen(tlm_cntrl->SFDU_Filename,"wb");
                   if (!SfduFile)
                   { sprintf(LogMsgBuf,"ERROR opening output SFDU file (%s)",
                             tlm_cntrl->SFDU_Filename);
                      rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                                 LogMsgBuf);
                   }
                 }

                 tlm_cntrl->TDS_Query.TcpIpPort = tlm_cntrl->SFDU_Port;

                 if (tlm_cntrl->SFDU_Source == SFDU_SRC_TDS)
                 { if ( rts_tds_connect(tlm_cntrl->TDS_Query.HostName,
                                        &TdsQuerySocket,
                                        tlm_cntrl->TDS_Query.TcpIpPort))
                   { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                                "Error connecting to TDS server");
                     return RTS_ERROR;
                   }
                   if (rts_tds_query(TdsQuerySocket,&tlm_cntrl->TDS_Query))
                   { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                                "Error submitting TDS query");
                     return RTS_ERROR;
                   }

                   NChan = TdsQuerySocket;
#if VMS_OS
/***
                   status = socket_ioctl(NChan,FIONBIO,&1);
***/
#else
#if SUN4_ARCH
                   status = fcntl(NChan,F_SETFL,FNDELAY);
#else
                   status = fcntl(NChan,F_SETFL,O_NONBLOCK);
#endif
#endif
                   if (status == -1)
                   { sprintf(LogMsgBuf,"Error setting socket to NOBLOCK: %s",
                     strerror(errno));
                     rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                                LogMsgBuf);
                   }
                 } else
                 { if (rts_create_sfdu_socket(tlm_cntrl->SFDU_Filename,
                                            &(tlm_cntrl->TDS_Query.TcpIpPort)))
                   { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                              "Error initializing SFDU socket");
                     return RTS_ERROR;
                   }

                   if (rts_accept_sfdu_socket())
                   { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                                "Error connecting SFDU socket client");
                     return RTS_ERROR;
                   }                 
                 }
               }

               /***  Get an SFDU  ***/
               TimeLimit = tlm_cntrl->TDS_Query.TimeOutLimit;
               sprintf(LogMsgBuf,"Retrieving SFDU (%d) from socket",
                       ++SfduCounter);
               rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG1,MODULE_NAME,LogMsgBuf);
               status = rts_get_socket_sfdu(Sfdu,FALSE);
               if (status & 0x01) SfduCounter--;
               if (ERR_CODE(status) == ERR_ERROR ||
                   ERR_CODE(status) == ERR_FATAL)
               /* terminate socket connections on an error */
               { rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,
                            "Closing SFDU socket");

                 /* Close output SFDU log file */
                 if (SfduFile)
                 { sprintf(LogMsgBuf,"Closing output SFDU file: %s",
                           tlm_cntrl->SFDU_Filename);
                   rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
                   fclose(SfduFile);
                   SfduFile = NULL;
                 }

                 /* Terminate TDS Interface */
                 if (tlm_cntrl->SFDU_Source == SFDU_SRC_TDS)
                 { rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,
                              "Disconnecting TDS server");
                   rts_tds_disconnect(&TdsQuerySocket);
                 }

                 if (NChan >= 0) close(NChan);
                 if (Chan >= 0) close(Chan);
                 NChan = -1;
                 Chan = -1;
                 SfduCounter--;
               } else if (status == TIME_OUT) SfduCounter--;
         break;

    case SFDU_SRC_FILE:
               /***  Open file and initialize processing ... if needed  ***/
               if (!SfduFile)
               { SfduFile = fopen(tlm_cntrl->SFDU_Filename, "rb");
                 if (!SfduFile)
                 { sprintf(LogMsgBuf,"File open error: %s (%s)",
                           strerror(errno), tlm_cntrl->SFDU_Filename);
                   rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
                   return RTS_ERROR;
                 }
                 Cptr = Eptr = Buffer;
               }

               sprintf(LogMsgBuf,"Retrieving SFDU (%d) from file",
                       ++SfduCounter);
               rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG1,MODULE_NAME,LogMsgBuf);
               status = rts_get_file_sfdu(Sfdu);
               if (status & 0x01)
               { sprintf(LogMsgBuf,"Closing input SFDU file: %s",
                         tlm_cntrl->SFDU_Filename);
                 rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
                 fclose(SfduFile);
                 SfduFile = NULL;
                 SfduCounter--;
               }
         break;

    default:   sprintf(LogMsgBuf,"Invalid SFDU SOURCE specified: %d",
                       tlm_cntrl->SFDU_Source);
               rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
               status = RTS_ERROR;
         break;
  }

  return status;
}

#undef       MODULE_NAME
#define      MODULE_NAME "A_SFDU"
/*****************************************************************************
 *				RTS_ACCEPT_SFDU_SOCKET
 *
 ****************************************************************************/
static int	rts_accept_sfdu_socket()
{ int	addrlen,
	status;
  struct sockaddr	address;

  /*  The accept function establishes the TCP/IP connection between	*/
  /*  the Chan enviroment and the Unix enviroment.			*/
  rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,
             "Establishing TCP/IP connection");
  addrlen = 16;
  NChan = accept(Chan, &address, &addrlen);
  if (NChan < 0)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
               "Error establishing TCP connection");
    return RTS_ERROR;
  }

#if VMS_OS
/***
  status = socket_ioctl(NChan,FIONBIO,&1);
 **/
#else
  status = fcntl(NChan,F_SETFL,O_NONBLOCK);
#endif
  if (status == -1)
  { sprintf(LogMsgBuf,"Error setting socket to NOBLOCK mode: %s",
            strerror(errno));
    rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
  }

  return RTS_NORMAL;
}

#undef       MODULE_NAME
#define      MODULE_NAME "C_SFDU"
/*****************************************************************************
 *				RTS_CREATE_SFDU_SOCKET
 *
 ****************************************************************************/
static int	rts_create_sfdu_socket(
  char		*FileName,
  int		*PortNum)
{ int	status;
  struct servent	*serptr;
  struct sockaddr_in	sockstr;

  /*  The socket function establishes an end point for communication	*/
  /*  and returns a channel that describes the end point.		*/
  rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,"Creating SFDU socket");
  Chan = socket(AF_INET, SOCK_STREAM, 0);
  if (Chan < 0)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,"Error creating socket");
    return RTS_ERROR;
  }

  /*  Define port information as defined by function parameter	*/
  sockstr.sin_family = AF_INET;
  sockstr.sin_port = htons(*PortNum);
  sockstr.sin_addr.s_addr = INADDR_ANY; 

  /*  The bind function assigns an address to an unnamed socket.	*/
  rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,
             "Binding socket to address");
  status = bind(Chan, (void*)&sockstr, sizeof(sockstr));
  if (status < 0)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
               "Error binding socket to address");
    return RTS_ERROR;
  }

  /*  The listen function informs the kernel that the socket	*/
  /*  is listening for connections.				*/
  rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,
             "Setting up listen function");
  status = listen(Chan, SOMAXCONN);
  if (status < 0)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
               "Error establishing the listen function");
    return RTS_ERROR;
  }

  /*  Open output SFDU file for logging  */
  if (FileName && strlen(FileName))
  { SfduFile = fopen(FileName,"wb");
    if (!SfduFile)
    { sprintf(LogMsgBuf,"ERROR opening output SFDU file (%s)",
              FileName);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    }
  }

  return RTS_NORMAL;
}

#undef       MODULE_NAME
#define      MODULE_NAME "F_SFDU"
/*****************************************************************************
 *				RTS_GET_FILE_SFDU
 *
 ****************************************************************************/
int	rts_get_file_sfdu(
  sfdu_record_typ	*Sfdu)
{ int	DataLth,
	status;

  /***  Read more data to process the SFDU header, if needed  ***/
  DataLth = Eptr - Cptr;
  if (DataLth < SFDU_HDR_LTH)
  { status = rts_read_sfdu_file();
    if (status & 0x01)
    { if (status == EOD) strcpy(LogMsgBuf,"End of Data in SFDU file");
      else sprintf(LogMsgBuf,"SFDU file read error: %s",strerror(errno));
      rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return status;
    }
    DataLth = Eptr - Cptr;
  }

  /***  See if SFDU is supported by these routines  ***/
  Sfdu->BufferLth = DataLth;
  Sfdu->Buffer = Cptr;
  status = rts_verify_sfdu(Sfdu);
  if (status & 0x01)
  { if (Sfdu->Hdr.Length == -1)
    { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                 "Can not verify SFDU ... terminating");
      return RTS_FATAL;
    }

    rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
               "Can not verify SFDU ... skipping");
  }

  /***  SFDU is too large for the internal Buffer  ***/
  if (Sfdu->Hdr.Length > BUF_SIZE)
  { sprintf(LogMsgBuf,"SFDU too large for buffer (%d vs %d)",
            Sfdu->Hdr.Length, BUF_SIZE);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    /*** Should really skip this SFDU and try for the next one ***/
    return RTS_FATAL;
  }

  /***  SFDU is larger than what is in memory; read more and try again  ***/
  if (Sfdu->Hdr.Length > DataLth || Sfdu->Hdr.Length == -1)
  { status = rts_read_sfdu_file();
    if (status & 0x01)
    { if (status == EOD) strcpy(LogMsgBuf,"End of Data in SFDU file");
      else sprintf(LogMsgBuf,"File read error: %s",strerror(errno));
      rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return status;
    }
    return (rts_get_file_sfdu(Sfdu));
  }

  sprintf(LogMsgBuf,"SFDU number: %d  Length: %d",SfduCounter,Sfdu->Hdr.Length);
  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG3,MODULE_NAME,LogMsgBuf);

  /***  SFDU is too large for a normal SFDU  ***/
  if (Sfdu->Hdr.Length > MAX_SFDU_LTH)
  { sprintf(LogMsgBuf,"SFDU too larger than standard (%d vs %d)",
            Sfdu->Hdr.Length, MAX_SFDU_LTH);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);

    /**  Skip it and try the next one  **/
    Cptr += Sfdu->Hdr.Length;
    return (rts_get_file_sfdu(Sfdu));
  }

  Cptr += Sfdu->Hdr.Length;

  return status;
}

#undef       MODULE_NAME
#define      MODULE_NAME "S_SFDU"
/*****************************************************************************
 *				RTS_GET_SOCKET_SFDU
 *
 ****************************************************************************/
int	rts_get_socket_sfdu(
  sfdu_record_typ	*Sfdu,
  int		ForceRead)
{ int	DataLth,
	FreeLth,
	length, 
	status, 
	WriteLth;
  static int	NoDataCounter = 0;

  DataLth = Eptr - Cptr;
  FreeLth = BUF_SIZE - DataLth;
/***
  if (FreeLth > MAX_SFDU_LTH || DataLth < MAX_SFDU_LTH || ForceRead)
***/
  if (ForceRead)
  { memcpy(Buffer,Cptr,DataLth);
    Cptr = Buffer;
    Eptr = Cptr + DataLth;

    status = recv(NChan,(char*)Eptr,FreeLth,0);
    if (status > 0)		/*  Successful socket I/O */
    { if (SfduFile) 				/* Log SFDU Data */
      { WriteLth = fwrite(Eptr,sizeof(char),status,SfduFile);
        if (WriteLth < status)
        { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                     "ERROR writing to SFDU file");
          fclose(SfduFile);
          SfduFile = NULL;
        } else
        { sprintf(LogMsgBuf,"Bytes written to SFDU file: %d",WriteLth);
          rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
          fflush(SfduFile);
        }
      }
      Eptr += status;
      DataLth = Eptr - Cptr;
      NoDataCounter = 0;
    } else			/*  Unsuccesfull socket I/O */
    { if (errno != EWOULDBLOCK)			/* I/O error */
      { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                   "Error reading from SFDU socket");
/***
        sprintf(LogMsgBuf,"Error reading from socket: %s",strerror(errno));
        rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
***/
        return RTS_ERROR;
      }
						/* Just no data there */
      if (TimeLimit && NoDataCounter++ > TimeLimit)
      { NoDataCounter = 0;			/* Reset time-out counter */
        sprintf(LogMsgBuf,"Time-out waiting for SFDU (%d seconds)",
                TimeLimit);
        rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
        return(TIME_OUT);
      }

      sleep(1);					/* wait a bit before return */
      return (INSUFF_DATA);
    }
  }

  if (DataLth >= SFDU_HDR_LTH)
  { Sfdu->BufferLth = DataLth;
    Sfdu->Buffer = Cptr;
    status = rts_verify_sfdu(Sfdu);
    if (status & 0x01)
    { if (Sfdu->Hdr.Length == -1)
      { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                   "Can not verify SFDU ... terminating");
        return RTS_FATAL;
      }

      rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                 "Can not verify SFDU ... skipping");
    }
  } else return (rts_get_socket_sfdu(Sfdu,TRUE));

  if (Sfdu->Hdr.Length > BUF_SIZE)
  { sprintf(LogMsgBuf,"SFDU too large for buffer: (%d vs %d)",
            Sfdu->Hdr.Length, BUF_SIZE);
    rts_logger(RTS_LOG_SFDU, RTS_LOG_FATAL, MODULE_NAME,LogMsgBuf);
    return RTS_FATAL;
  }

  /***  SFDU is larger than what is in memory; read more and try again  ***/
  if (Sfdu->Hdr.Length > DataLth || Sfdu->Hdr.Length == -1)
     return (rts_get_socket_sfdu(Sfdu,TRUE));

  sprintf(LogMsgBuf,"SFDU number: %d  Length: %d",SfduCounter,Sfdu->Hdr.Length);
  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG3,MODULE_NAME,LogMsgBuf);

  /***  SFDU is too large for a normal SFDU  ***/
  if (Sfdu->Hdr.Length > MAX_SFDU_LTH)
  { sprintf(LogMsgBuf,"SFDU too larger than standard (%d vs %d)",
            Sfdu->Hdr.Length, MAX_SFDU_LTH);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);

    /**  Skip it and try the next one  **/
    Cptr += Sfdu->Hdr.Length;
    return (rts_get_socket_sfdu(Sfdu,FALSE));
  }

  Cptr += Sfdu->Hdr.Length;

  return status;
}

#undef       MODULE_NAME
#define      MODULE_NAME "L_SFDU"
/*****************************************************************************
 *				RTS_READ_SFDU_FILE
 *
 ****************************************************************************/
static int	rts_read_sfdu_file( void )
{ int	FreeLth,
	DataLth,
	status;
  static int	DataDelta = 0;

  DataLth = Eptr - Cptr;
  FreeLth = BUF_SIZE - DataLth;

  memmove(Buffer,Cptr,DataLth);
  Cptr = Buffer;
  Eptr = Buffer + DataLth;
  memset(Eptr,0,FreeLth);

  status = fread(Eptr, sizeof(Ubyte), FreeLth, SfduFile);
  if (ferror(SfduFile))
  { sprintf(LogMsgBuf,"SFDU file read error: %s",strerror(errno));
    rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    return RTS_ERROR;
  }

  Eptr += status;
  if (feof(SfduFile))
  { if (status == 0)
       rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,
                  "No data read from SFDU file");
    if (Eptr == Buffer) return (EOD);
    if (status < 0 || DataDelta == (Eptr - Buffer))
    { sprintf(LogMsgBuf,"%d bytes remain unprocessed in SFDU file",
              (Eptr - Buffer));
      rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
      return (EOD);
    }
    DataDelta = (Eptr - Buffer);
  }

  return RTS_NORMAL;
}

#undef       MODULE_NAME
#define      MODULE_NAME "V_SFDU"
/*****************************************************************************
 *				RTS_VERIFY_SFDU
 *
 *	Verifies that the SFDU is a proper SFDU for processing.  This routine
 *  returns TRUE is there is an invalid SFDU or and EOD SFDU.  The length of
 *  the SFDU is returned as one of the passed parameters.  The input buffer
 *  is expected to start at the beginning of the SFDU.
 ****************************************************************************/
static int	rts_verify_sfdu(
  sfdu_record_typ	*Sfdu)
{ char  TempBuf[256],
	*SfduBuffer = Sfdu->Buffer;

  /* Check Control Authority */
  if (strncmp(SfduBuffer,"NJPL",4) &&
      strncmp(SfduBuffer,"CCSD",4))
  { sprintf(LogMsgBuf,"Unknown SFDU Control Authority ID (%-4.4s)",SfduBuffer);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);

    sprintf(TempBuf,
            ": %02X%02X %02X%02X %02X%02X %02X%02X %-8.8s",
            SfduBuffer[0],SfduBuffer[1],SfduBuffer[2],SfduBuffer[3],
            SfduBuffer[4],SfduBuffer[5],SfduBuffer[6],SfduBuffer[7],SfduBuffer);
    strcat(LogMsgBuf,TempBuf);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG2,MODULE_NAME,LogMsgBuf);
    return (UNSUPPORTED_SFDU);
  } else memmove(Sfdu->Hdr.CntrlAuth,(void *)SfduBuffer,4);

  Sfdu->Hdr.Version = SfduBuffer[4];
  Sfdu->Hdr.ClassId = SfduBuffer[5];
  Sfdu->Hdr.Delimiter = SfduBuffer[6];
  Sfdu->Hdr.spare = SfduBuffer[7];
  memmove(Sfdu->Hdr.DataDesc,(SfduBuffer+8),4);

  switch (Sfdu->Hdr.Version)
  { case '1': Sfdu->Hdr.Length = atoi((char *)&SfduBuffer[12]) + 20;
              Sfdu->Hdr.Delimiter = '0';
    break;

    case '2': Sfdu->Hdr.Length = ((Ubyte)SfduBuffer[16] << 24) +
                                 ((Ubyte)SfduBuffer[17] << 16) +
                                 ((Ubyte)SfduBuffer[18] << 8)  +
                                 (Ubyte)SfduBuffer[19] + 20;
              Sfdu->Hdr.Delimiter = '0';
    break;

    case '3': switch (Sfdu->Hdr.Delimiter)
              { case 'A': Sfdu->Hdr.Length = atoi((char *)&SfduBuffer[12]) + 20;
                break;

                case 'B': Sfdu->Hdr.Length = ((Ubyte)SfduBuffer[16] << 24) +
                                             ((Ubyte)SfduBuffer[17] << 16) +
                                             ((Ubyte)SfduBuffer[18] << 8)  +
                                             (Ubyte)SfduBuffer[19] + 20;
                break;

                case 'C':
                case 'E':
                case 'F':
                case 'S': memmove(Sfdu->Hdr.Marker,(SfduBuffer+12),8);

                          if (Sfdu->Hdr.ClassId == 'Z' ||	/* Data Exchange Hdr */
                              Sfdu->Hdr.ClassId == 'U' ||
                              Sfdu->Hdr.ClassId == 'F' ||
                              Sfdu->Hdr.ClassId == 'R' ||
                              Sfdu->Hdr.ClassId == 'C')
                          { Sfdu->Hdr.Length = 20;
                            return (DELIMITER_SFDU);
                          } else determine_sfdu_length(Sfdu);
                break;

                default: sprintf(LogMsgBuf,"Unrecognized SFDU delimiter symbol: %c",
                                 Sfdu->Hdr.Delimiter);
                         rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
                         return (INVALID_SFDU);
                break;
              }
    break;

    case '$': if (strncmp(SfduBuffer,"CCSD$$MARKER",12))
              { sprintf(LogMsgBuf,"CCSD terminating marker: %-8.8s",
                        (SfduBuffer+12));
                rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
                Sfdu->Hdr.Length = 20;
                return (DELIMITER_SFDU);
              }

              /* no break, because this is an error */

    default: if (isprint(Sfdu->Hdr.Version))
                sprintf(LogMsgBuf,"Invalid SFDU Version ID: '%c'",
                        Sfdu->Hdr.Version);
             else sprintf(LogMsgBuf,"Invalid SFDU Version ID: (%d)",
                          Sfdu->Hdr.Version); 
             rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
             return (INVALID_SFDU);
    break;
  }

  return RTS_NORMAL;
}
