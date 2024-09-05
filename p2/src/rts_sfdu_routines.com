$!****************************************************************************
$!
$! Build proc for MIPL module rts_sfdu_routines
$! VPACK Version 1.9, Tuesday, May 04, 1999, 14:43:05
$!
$! Execute by entering:		$ @rts_sfdu_routines
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module rts_sfdu_routines ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to rts_sfdu_routines.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("rts_sfdu_routines.imake") .nes. ""
$   then
$      vimake rts_sfdu_routines
$      purge rts_sfdu_routines.bld
$   else
$      if F$SEARCH("rts_sfdu_routines.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rts_sfdu_routines
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rts_sfdu_routines.bld "STD"
$   else
$      @rts_sfdu_routines.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rts_sfdu_routines.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rts_sfdu_routines.com -
	-s rts_get_sfdu.c rts_chdo.c rts_sfdu_proc.c rts_tds_query.c -
	-i rts_sfdu_routines.imake -
	-t tst_rts_get_sfdu.c tst_rts_get_sfdu.imake -
	-o rts_get_sfdu.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rts_get_sfdu.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_chdo.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/


#include <stdio.h>
#include <string.h>
#include "rts_typedefs.h"
#include "rts_const_defs.h"
#include "rts_chdo_types.h"
#include "rts_sfdu.h"
#include "rts_gdrbyteconv.h"
#include "rts_logging.h"
#include "rts_errors.h"

#define  MODULE_NAME		"CHDO"

static char	log_msg_buf[128];
/*******************************************************************
 *				FREE_CHDO_BUFFER
 ******************************************************************/
void	free_chdo_buffer(
  chdo_hdr_typ	*chdo)
{
  if (chdo->Type == 10)
  { sprintf(log_msg_buf,"Deallocating CHDO 10 Data: %08X",
            chdo->Data.chdo_010->data);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);
    free(chdo->Data.chdo_010->data);
  } else  if (chdo->Type == 28)
  { if (!chdo->Data.chdo_028->lv_flag)
    { sprintf(log_msg_buf,"Deallocating CHDO 28 lc_value: %08X",
              chdo->Data.chdo_028->lc_value);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);
      free(chdo->Data.chdo_028->lc_value);
    }
  }

  sprintf(log_msg_buf,"Deallocating: %08X",chdo->Data.buffer);
  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);
  free(chdo->Data.buffer);
}

/*******************************************************************
 *				CHDO_002
 ******************************************************************/
int	load_chdo_002(
  chdo_002_typ	*chdo_002,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );
		chdo_002->major_type = *( ptr + 4 );
		chdo_002->minor_type = *( ptr + 5 );
		chdo_002->mission_id = *( ptr + 6 );
		chdo_002->format_id = *( ptr + 7 );

	return RTS_NORMAL;
}

/*******************************************************************
 *				CHDO_010
 ******************************************************************/
int	load_chdo_010(
  chdo_010_typ	*chdo_010,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

                chdo_010->data = (void *)malloc(length);
                if (!chdo_010->data)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                             "Could not allocate memory for CHDO 10 data");
                  return (ALLOCATE_ERROR);
                }

                sprintf(log_msg_buf,"Allocating CHDO 10 Data: %08X",
                        chdo_010->data);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);

                memcpy(chdo_010->data,(ptr+4),length);

        return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_027
 ******************************************************************/
int	load_chdo_027(
  chdo_027_typ	*chdo_027,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_027->decom_flags = *(ptr + 4);
		chdo_027->filler_length = *(ptr + 5);
		chdo_027->number_channels = C16_TO_U16( (ptr + 6) );
		chdo_027->map_id = C16_TO_U16( (ptr + 8) );

        return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_028
 ******************************************************************/
int	load_chdo_028(
  chdo_028_typ	*chdo_028,
  Ubyte		*ptr)
{ int	type,
	length,
	ValueLength;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_028->source = (*(ptr + 4) >> 3);
		chdo_028->lv_flag = (*(ptr + 4) >> 2) & 0x01;
		chdo_028->bad_data = (*(ptr + 4) >> 1) & 0x01;
		chdo_028->spare = (*(ptr + 4)) & 0x01;
		chdo_028->length_value = *(ptr + 5);
		chdo_028->filler_length = (*(ptr + 6) >> 4);
		chdo_028->channel_number = (C16_TO_U16( (ptr + 6) )) & 0x0FFF;

		if (!chdo_028->lv_flag)
		{ ValueLength = sizeof(Uword) * chdo_028->length_value;
		  chdo_028->lc_value = (void *)malloc(ValueLength);
                  if (!chdo_028->lc_value)
                  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                               "Could not allocate memory for CHDO 28 data");
                    return (ALLOCATE_ERROR);
                  }
                  sprintf(log_msg_buf,"Allocating CHDO 28 lc_value: %08X",
                         chdo_028->lc_value);
                  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,log_msg_buf);

                  memcpy(chdo_028->lc_value,(ptr+4),length);
		} else chdo_028->lc_value = (Uword *)0;

        return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_062
 ******************************************************************/
int	load_chdo_062(
  chdo_062_typ	*chdo_062,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_062->originator = *( ptr + 4 );
		chdo_062->last_modifier = *( ptr + 5 );
		chdo_062->scft_id = *( ptr + 6 );
		chdo_062->data_source = *( ptr + 7 );
		chdo_062->mode_flags = CBITS_TO_U8( ( ptr + 8 ), 0, 4 );
		chdo_062->status_flags = CBITS_TO_U16( ( ptr + 8 ), 4, 5 );
		chdo_062->spare_flags_1 = CBITS_TO_U8( ( ptr + 9 ), 1, 7 );
		memcpy( chdo_062->ert, ptr + 10, 6 );
		chdo_062->rs_codeword_status = *( ptr + 16 );
		chdo_062->frame_extract_count = *( ptr + 17 );
		chdo_062->dsn_record_seq = C32_TO_U32( ( ptr + 18 ) );
		chdo_062->bet = *( ptr + 22 );
		chdo_062->fly = *( ptr + 23 );
		chdo_062->decode_status = *( ptr + 24 );
		chdo_062->decode_method = *( ptr + 25 );
		chdo_062->sync_flags = *( ptr + 26 );
		chdo_062->pn_errors = *( ptr + 27 );
		chdo_062->virtual_channel_id = *( ptr + 28 );
		chdo_062->virtual_frame_count = *( ptr + 29 );
		chdo_062->frame_hdr_error_flag = *( ptr + 30 );
		chdo_062->spare_2 = *( ptr + 31 );
		chdo_062->spare_3 = *( ptr + 32 );
		chdo_062->frequency_band = *( ptr + 33 );
		chdo_062->bit_rate = C32_TO_U32( ( ptr + 34 ) );
		chdo_062->spare_4 = C16_TO_U16( ( ptr + 38 ) );
		chdo_062->snt = C32_TO_U32( ( ptr + 40 ) );
		chdo_062->ssnr = C32_TO_U32( ( ptr + 44 ) );
		chdo_062->signal_level = C32_TO_U32( ( ptr + 48 ) );
		chdo_062->antennas = *( ptr + 52 );
		chdo_062->receivers = *( ptr + 53 );
		chdo_062->master_antenna = *( ptr + 54 );
		chdo_062->master_receiver = *( ptr + 55 );
		chdo_062->dtm_group = *( ptr + 56 );
		chdo_062->tlm_channel = *( ptr + 57 );
		chdo_062->lock_status = C16_TO_U16( ( ptr + 58 ) );
		chdo_062->version = *( ptr + 60 );
		chdo_062->build = *( ptr + 61 );
		chdo_062->orig_source = *( ptr + 62 );
		chdo_062->curr_source = *( ptr + 63 );
		memcpy( chdo_062->rct, ptr + 64, 6 );
		chdo_062->anomaly_flags = C16_TO_U16( ( ptr + 70 ) );
		chdo_062->lock_count = C16_TO_U16( ( ptr + 72 ) );
		chdo_062->lrn = C16_TO_U16( ( ptr + 74 ) );
		memset( ( chdo_062->pub ), (int)( ptr + 76 ), 6 );
		chdo_062->frame_type = C16_TO_U16( ( ptr + 82 ) );

	return( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_063
 ******************************************************************/
int	load_chdo_063(
  chdo_063_typ	*chdo_063,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_063->packet_quality = CBITS_TO_U8( ( ptr + 4 ), 0, 2 );
		chdo_063->pds_error_flag = CBITS_TO_U8( ( ptr + 4 ), 2, 3 );
		chdo_063->spare_flag_1 = CBITS_TO_U8( ( ptr + 4 ), 5, 1 );
		chdo_063->pkt_fill_flag = CBITS_TO_U8( ( ptr + 4 ), 6, 1 );
		chdo_063->sclk_scet_cor_flag = CBITS_TO_U8( ( ptr + 4 ), 7, 1 );
		chdo_063->packet_type = *( ptr + 5 );
		chdo_063->source_pkt_seq_count = C16_TO_U16( ( ptr + 6 ) );
		chdo_063->non_fill_length = C16_TO_U16( ( ptr + 8 ) );
/***  MO CHDO 063 ***
		chdo_063->orbit_phase = *( ptr + 10 );
		chdo_063->spare_1 = *( ptr + 11 );
		chdo_063->orbit_number = C16_TO_U16( ( ptr + 12 ) );
		chdo_063->mo_sclk_sec = C32_TO_U32( ( ptr + 14 ) );
		chdo_063->mo_sclk_spare = *( ptr + 18 );
		chdo_063->mo_sclk_fine = *( ptr + 19 );
		memcpy( chdo_063->scet, ptr + 20, 6 );
		chdo_063->spare_2 = *( ptr + 26 );
		chdo_063->segment_subcount = *( ptr + 27 );
		chdo_063->spare_3 = C16_TO_U16( ( ptr + 28 ) );
****  MPF CHDO 063  ***/
		chdo_063->spare_1 = *( ptr + 10 );
		chdo_063->spare_2 = *( ptr + 11 );
		chdo_063->rover_retransmit_seq_count = C16_TO_U16( ( ptr + 12 ) );
		chdo_063->sclk_sec = C32_TO_U32( ( ptr + 14 ) );
		chdo_063->spare_3 = *( ptr + 18 );
		chdo_063->sclk_fine = *( ptr + 19 );
		memcpy( chdo_063->scet, ptr + 20, 6 );
		chdo_063->wrapped_sclk_sec = C32_TO_U32( ( ptr + 28 ) );

	return ( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_081
 ******************************************************************/
int	load_chdo_081(
  chdo_081_typ	*chdo_081,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_081->originator = *( ptr + 4 );
		chdo_081->last_modifier = *( ptr + 5 );
		chdo_081->scft_id = *( ptr + 6 );
		chdo_081->data_source = *( ptr + 7 );
		chdo_081->error_flags = C16_TO_U16( ( ptr + 8 ) );
		memcpy( chdo_081->ert, ptr + 10, 6 );
		chdo_081->spare_1 = C16_TO_U16( ( ptr + 16 ) );
		chdo_081->dsn_record_seq = C32_TO_U32( ( ptr + 18 ) );
		chdo_081->acq_bet = *( ptr + 22 );
		chdo_081->maint_bet = *( ptr + 23 );
		chdo_081->verify_cnt = *( ptr + 24 );
		chdo_081->flywheel_cnt = *( ptr + 25 );
		chdo_081->data_length = C16_TO_U16( ( ptr + 26 ) );
		chdo_081->sync_mode = *( ptr + 28 );
		chdo_081->sync_status = CBITS_TO_U8( ( ptr + 29 ), 0, 5 );
		chdo_081->bit_slip = CBITS_TO_U8( ( ptr + 29 ), 5, 3 );
		chdo_081->rs_decode_status = *( ptr + 30 );
		chdo_081->rs_codeword_status = *( ptr + 31 );
		chdo_081->bit_errors = *( ptr + 32 );
		chdo_081->frequency_band = *( ptr + 33 );
		chdo_081->bit_rate = C32_TO_U32( ( ptr + 34 ) );
		chdo_081->spare_2 = C16_TO_U16( ( ptr + 38 ) );
		chdo_081->snt = C32_TO_U32( ( ptr + 40 ) );
		chdo_081->ssnr = C32_TO_U32( ( ptr + 44 ) );
		chdo_081->signal_level = C32_TO_U32( ( ptr + 48 ) );
		chdo_081->antennas = *( ptr + 52 );
		chdo_081->receivers = *( ptr + 53 );
		chdo_081->master_antenna = *( ptr + 54 );
		chdo_081->master_receiver = *( ptr + 55 );
		chdo_081->dtm_group = *( ptr + 56 );
		chdo_081->tlm_channel = *( ptr + 57 );
		chdo_081->lock_status = C16_TO_U16( ( ptr + 58 ) );
		chdo_081->version = *( ptr + 60 );
		chdo_081->build = *( ptr + 61 );
		chdo_081->orig_source = *( ptr + 62 );
		chdo_081->curr_source = *( ptr + 63 );
		memcpy( chdo_081->rct, ptr + 64, 6 );
		chdo_081->anomaly_flags = C16_TO_U16( ( ptr + 70 ) );
		chdo_081->lock_count = C16_TO_U16( ( ptr + 72 ) );
		chdo_081->lrn = C16_TO_U16( ( ptr + 74 ) );

	return( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_082
 ******************************************************************/
int	load_chdo_082(
  chdo_082_typ	*chdo_082,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_082->originator = *( ptr + 4 );
		chdo_082->last_modifier = *( ptr + 5 );
		chdo_082->scft_id = *( ptr + 6 );
		chdo_082->data_source = *( ptr + 7 );
		chdo_082->mode_flags = CBITS_TO_U8( ( ptr + 8 ), 0, 4 );
		chdo_082->status_flags = CBITS_TO_U16( ( ptr + 8 ), 4, 5 );
		chdo_082->spare_flags_1 = CBITS_TO_U8( ( ptr + 9 ), 1, 7 );
		memcpy( chdo_082->ert, ptr + 10, 6 );
		chdo_082->rs_codeword_status = *( ptr + 16 );
		chdo_082->frame_extract_count = *( ptr + 17 );
		chdo_082->dsn_record_seq = C32_TO_U32( ( ptr + 18 ) );
		chdo_082->bet = *( ptr + 22 );
		chdo_082->fly = *( ptr + 23 );
		chdo_082->decode_status = *( ptr + 24 );
		chdo_082->decode_method = *( ptr + 25 );
		chdo_082->sync_flags = *( ptr + 26 );
		chdo_082->pn_errors = *( ptr + 27 );
		chdo_082->virtual_channel_id = *( ptr + 28 );
		chdo_082->virtual_frame_count = *( ptr + 29 );
		chdo_082->frame_hdr_error_flag = *( ptr + 30 );
		chdo_082->spare_01 = *( ptr + 31 );
		chdo_082->spare_3 = *( ptr + 32 );
		chdo_082->frequency_band = *( ptr + 33 );
		chdo_082->bit_rate = C32_TO_U32( ( ptr + 34 ) );
		chdo_082->spare_4 = C16_TO_U16( ( ptr + 38 ) );
		chdo_082->snt = C32_TO_U32( ( ptr + 40 ) );
		chdo_082->ssnr = C32_TO_U32( ( ptr + 44 ) );
		chdo_082->signal_level = C32_TO_U32( ( ptr + 48 ) );
		chdo_082->antennas = *( ptr + 52 );
		chdo_082->receivers = *( ptr + 53 );
		chdo_082->master_antenna = *( ptr + 54 );
		chdo_082->master_receiver = *( ptr + 55 );
		chdo_082->dtm_group = *( ptr + 56 );
		chdo_082->tlm_channel = *( ptr + 57 );
		chdo_082->lock_status = C16_TO_U16( ( ptr + 58 ) );
		chdo_082->version = *( ptr + 60 );
		chdo_082->build = *( ptr + 61 );
		chdo_082->orig_source = *( ptr + 62 );
		chdo_082->curr_source = *( ptr + 63 );
		memcpy( chdo_082->rct, ptr + 64, 6 );
		chdo_082->anomaly_flags = C16_TO_U16( ( ptr + 70 ) );
		chdo_082->lock_count = C16_TO_U16( ( ptr + 72 ) );
		chdo_082->lrn = C16_TO_U16( ( ptr + 74 ) );
		memset( ( chdo_082->pub ), (int)( ptr + 76 ), 6 );
		chdo_082->spare_41 = C16_TO_U16( ( ptr + 82 ) );

	return( RTS_NORMAL );
}

/*******************************************************************
 *				CHDO_128
 ******************************************************************/
int	load_chdo_128(
  chdo_128_typ	*chdo_128,
  Ubyte		*ptr)
{ int	type,
	length;

		type = C16_TO_U16( ( ptr + 0 ) );
		length = C16_TO_U16( ( ptr + 2 ) );

		chdo_128->pkt_fill_flag = (*(ptr + 3)) >> 7;
		chdo_128->apid = C16_TO_I16( ptr + 4 ) & 0x07FF;
		chdo_128->pkt_seq_count = C16_TO_I16( (ptr + 6) );
		chdo_128->non_fill_length = C16_TO_I16( (ptr + 8) );
        	chdo_128->extract_flags = *(ptr + 10);
		chdo_128->parent_sc_id = *(ptr + 11);
		chdo_128->spare_2 = C16_TO_I16( (ptr + 12) );
		memcpy(chdo_128->sclk, (ptr+14), 6);
		memcpy(chdo_128->scet, (ptr+20), 6);
		chdo_128->spare_3 = *(ptr + 26);
		chdo_128->scet_flags = *(ptr + 27);
		chdo_128->spare_4 = C16_TO_I16( (ptr + 28) );

	return ( RTS_NORMAL );
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_sfdu_proc.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "xvmaininc.h"

#if VMS_OS
#define  MULTINET_OLD_STYLE	0
#include <types.h>

#else
#include <stdlib.h>
#endif
#include <string.h>

#include "rts_typedefs.h"
#include "rts_const_defs.h"
#include "rts_sfdu.h"
#include "rts_logging.h"
#include "rts_errors.h"
#include "rts_gdrbyteconv.h"

#define  MODULE_NAME		"SFDU_HDR"

typedef enum	{ NOCHANGE, PUSHED, POPPED, ERROR } state_typ;
typedef	struct	{
		char	ClassId;
		char	*ClassName;
		} sfdu_class_typ;

static char	LogMsgBuf[128];

#undef  MODULE_NAME
#define  MODULE_NAME		"PAD"
/******************************************************************************
 *				process_application_data
 *
 *****************************************************************************/
static	int	process_application_data(
  sfdu_record_typ	*Sfdu)
{ int	idx = 0,
	offset = (SFDU_HDR_LTH + CHDO_HDR_LTH),
	status;

  do
  { status = process_chdo(((char *)Sfdu->Buffer+offset),&Sfdu->Chdo[idx]);
    if (RTS_RTN_BAD(status))
    { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                 "Could not process CHDO");
      return status;
    }
    offset += Sfdu->Chdo[idx].Length + CHDO_HDR_LTH;
    if (offset >= Sfdu->Hdr.Length)
       Sfdu->DataIdx = idx;
    else idx++;
  } while (idx < MAX_CHDO_HDRS && offset < Sfdu->Hdr.Length);

  if (idx >= MAX_CHDO_HDRS)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
               "Too many CHDOs in SFDU");
    return (TOO_MANY_CHDOS);
  }

  return DATA_SFDU;
}

#undef  MODULE_NAME
#define  MODULE_NAME		"PCD"
/******************************************************************************
 *				process_catalog_data
 *
 *****************************************************************************/
static	int	process_catalog_data(
  sfdu_record_typ	*Sfdu)
{ int	lth,
	MsgNum,
	RtnStatus = RTS_NORMAL;
  char	MsgType,
        MsgStr[128],
	*PtrObj,
	*CatalogObject;

  if (Sfdu->Hdr.Length <= 0) return (INFO_SFDU);	/* should not happen */

  if (strcmp(Sfdu->Hdr.DataDesc,"L009"))
  { sprintf(LogMsgBuf,"Unsupported Catalog-Data_Object SFDU: %s",
            Sfdu->Hdr.DataDesc);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
    return (UNSUPPORTED_SFDU);
  }

  /***  Copy into local buffer  ***/
  CatalogObject = malloc(Sfdu->Hdr.Length);
  if (!CatalogObject)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
               "Could not allocat memory to process Catalog-Object");
    return(ALLOCATE_ERROR);
  }

  memset(CatalogObject,0,Sfdu->Hdr.Length);
  memcpy(CatalogObject,((char *)Sfdu->Buffer+20),(Sfdu->Hdr.Length-20));

  /***  Can we process this 'catalog' SFDU?  ***/
  PtrObj = strstr(CatalogObject,"OBJECT = ");
  if (PtrObj)
  { if (strncmp((PtrObj+9),"Status Message",14) != 0 &&
        strncmp((PtrObj+9),"Error Message",13) != 0)
    {
      return (INFO_SFDU);
    }
  }

  PtrObj = strstr(CatalogObject,"MESSAGE_TYPE = ");
  if (PtrObj) MsgType = *(PtrObj+15);
  else MsgType = '?';

  PtrObj = strstr(CatalogObject,"MESSAGE_NUM = ");
  if (PtrObj) MsgNum = atoi((PtrObj+13));
  else MsgNum = 0;

  PtrObj = strstr(CatalogObject,"MESSAGE = ");
  if (PtrObj)
  { lth = strcspn((PtrObj+10),";\n\r");
    if (lth > sizeof(MsgStr)) lth = sizeof(MsgStr) - 1;
    memset(MsgStr,0,sizeof(MsgStr));
    memmove(MsgStr,(PtrObj+10),lth);
  } else strcat(MsgStr,"Can not extract message");

  sprintf(LogMsgBuf,"(%c, %d) - %s",MsgType,MsgNum,MsgStr);
  if (MsgType != 'S')
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    RtnStatus = ERROR_SFDU;
  } else
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
    RtnStatus = INFO_SFDU;
  }

  free(CatalogObject);
  return (RtnStatus);
}

#undef  MODULE_NAME
#define  MODULE_NAME		"PC"
/******************************************************************************
 *				PROCESS_CHDO
 *
 *****************************************************************************/
int	process_chdo(
  Ubyte		*ptr,
  chdo_hdr_typ	*chdo)
{
  chdo->Type = C16_TO_U16(ptr);
  chdo->Length = C16_TO_U16(ptr+2);

  sprintf(LogMsgBuf,"CHDO: %d  Length: %d",chdo->Type,chdo->Length);
  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG5,MODULE_NAME,LogMsgBuf);

  switch(chdo->Type)
  {
    case 2: chdo->Data.chdo_002 = (void *)malloc(sizeof(chdo_002_typ));
            if (!chdo->Data.chdo_002)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 2 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_002, 0, sizeof(chdo_002_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 2: %08X",chdo->Data.chdo_002);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_002(chdo->Data.chdo_002,ptr));
    break;

    case 10: chdo->Data.chdo_010 = (void *)malloc(sizeof(chdo_010_typ));
            if (!chdo->Data.chdo_010)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 10 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_010, 0, sizeof(chdo_010_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 10: %08X",chdo->Data.chdo_010);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_010(chdo->Data.chdo_010,ptr));
    break;

    case 62: chdo->Data.chdo_062 = (void *)malloc(sizeof(chdo_062_typ));
            if (!chdo->Data.chdo_062)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 62 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_062, 0, sizeof(chdo_062_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 62: %08X",chdo->Data.chdo_062);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_062(chdo->Data.chdo_062,ptr));
    break;

    case 63: chdo->Data.chdo_063 = (void *)malloc(sizeof(chdo_063_typ));
            if (!chdo->Data.chdo_063)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 63 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_063, 0, sizeof(chdo_063_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 63: %08X",chdo->Data.chdo_063);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_063(chdo->Data.chdo_063,ptr));
    break;

    case 81: chdo->Data.chdo_081 = (void *)malloc(sizeof(chdo_081_typ));
            if (!chdo->Data.chdo_081)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 81 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_081, 0, sizeof(chdo_081_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 81: %08X",chdo->Data.chdo_081);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_081(chdo->Data.chdo_081,ptr));
    break;

    case 82: chdo->Data.chdo_082 = (void *)malloc(sizeof(chdo_082_typ));
            if (!chdo->Data.chdo_082)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 82 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_082, 0, sizeof(chdo_082_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 82: %08X",chdo->Data.chdo_082);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_082(chdo->Data.chdo_082,ptr));
    break;

    case 128: chdo->Data.chdo_128 = (void *)malloc(sizeof(chdo_128_typ));
            if (!chdo->Data.chdo_128)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 128 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_128, 0, sizeof(chdo_128_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 128: %08X",chdo->Data.chdo_128);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_128(chdo->Data.chdo_128,ptr));
    break;

    default: chdo->Data.buffer = (void *)malloc((chdo->Length + 4));
            if (!chdo->Data.buffer)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 128 structure");
              return (ALLOCATE_ERROR);
            }
            sprintf(LogMsgBuf,"Allocating CHDO %03d: %08X",
                    chdo->Type,chdo->Data.buffer);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            memcpy(chdo->Data.buffer,ptr,(chdo->Length + 4));
            
            sprintf(LogMsgBuf,"Unsupported CHDO: %03d",chdo->Type);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
            return (UNSUPPORTED_CHDO);
    break;
  }
}

#undef  MODULE_NAME
#define  MODULE_NAME		"PS"
/******************************************************************************
 *				PROCESS_SFDU
 *
 *****************************************************************************/
int	process_sfdu(
  sfdu_record_typ	*Sfdu)
{ int	idx,
	ClassIdx;
  sfdu_class_typ	ExpandClass[12] = {
		{ 'Z', "Exchange-Unit"},
		{ 'U', "Application-Unit"},
		{ 'F', "Description-Unit"},
		{ 'R', "Replacement-Service"},
		{ 'C', "Admin-Service"},
		{ 'I', "Application-Object"},
		{ 'S', "Supplement-Object"},
		{ 'D', "DDR-Object"},
		{ 'E', "DED-Object"},
		{ 'K', "Catalog-Object"},
		{ 'V', "Volume-Object"},
		{   0, "UnKnown" }
	};
  state_typ	StackState;
  static int	StackIdx = -1;
  static char	MarkerStack[MAX_SFDU_STACK][10];

  StackState = NOCHANGE;
  for (ClassIdx=0; ExpandClass[ClassIdx].ClassId; ClassIdx++)
      if (Sfdu->Hdr.ClassId == ExpandClass[ClassIdx].ClassId) break;

  if (Sfdu->Hdr.Version == '3')
  { sprintf(LogMsgBuf,"Processing %s SFDU: %-20.20s",
            ExpandClass[ClassIdx].ClassName,Sfdu->Buffer);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG2,MODULE_NAME,LogMsgBuf);

    switch (Sfdu->Hdr.Delimiter)
    { case 'A': sprintf(LogMsgBuf,"ASCII length SFDU (%d)",Sfdu->Hdr.Length);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
      break;

      case 'B': sprintf(LogMsgBuf,"BINARY length SFDU (%d)",Sfdu->Hdr.Length);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
      break;

      case 'C': rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,
                           "'Contiguous End-Of-File' SFDU reached");
      break;

      case 'E': rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,
                           "'Sequential End-Of-File' SFDU reached");
                if (StackIdx < 0)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,
                             "Could not pop SFDU marker stack, already at top");
                  StackState = ERROR;
                } else if (strcmp(Sfdu->Hdr.Marker,MarkerStack[StackIdx]) == 0)
                { StackState = POPPED;
                  StackIdx--;
                } else
                { sprintf(LogMsgBuf,"SFDU Marker pop-stack mismatch %s vs %s",
                          Sfdu->Hdr.Marker,MarkerStack[StackIdx]);
                  rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
                  StackState = ERROR;
               }
      break;

      case 'F': rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,
                           "'Shared End-Of-File' SFDU reached");
                if (StackIdx < 0)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,
                             "Could not pop SFDU marker stack, already at top");
                  StackState = ERROR;
                } else if (strcmp(Sfdu->Hdr.Marker,MarkerStack[StackIdx]) == 0 ||
                           strcmp(Sfdu->Hdr.Marker,MarkerStack[0]) == 0)
                { StackState = POPPED;
                  StackIdx--;
                  if (strcmp(Sfdu->Hdr.Marker,MarkerStack[0]) == 0)
                     StackIdx = -1;
                } else
                { sprintf(LogMsgBuf,"SFDU Marker pop-stack mismatch %s vs %s",
                          Sfdu->Hdr.Marker,MarkerStack[StackIdx]);
                  rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
                  StackState = ERROR;
               }
      break;

      case 'S': sprintf(LogMsgBuf,"Marker Pattern SFDU (%s)",Sfdu->Hdr.Marker);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);

                if (++StackIdx >= MAX_SFDU_STACK)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,
                             "Could not push SFDU marker stack, already at bottom");
                  StackState = ERROR;
                } else
                { strcpy(MarkerStack[StackIdx],Sfdu->Hdr.Marker);
                  StackState = PUSHED;
                }
      break;

      default: sprintf(LogMsgBuf,"Unknown SFDU delimitation ID: %c",
                       Sfdu->Hdr.Delimiter);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
      break;
    }
  } else
  { sprintf(LogMsgBuf,"Processing %s SFDU: %-12.12s",
            ExpandClass[ClassIdx].ClassName,Sfdu->Buffer);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG2,MODULE_NAME,LogMsgBuf);
  }

  if (Sfdu->Hdr.Delimiter == 'S')
  { if (StackState == PUSHED)
    { sprintf(LogMsgBuf,"Pushed SFDU marker '%s' on stack",
             MarkerStack[StackIdx]);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
    }
  }

  if (Sfdu->Hdr.Delimiter == 'E' || Sfdu->Hdr.Delimiter == 'F')
  { if (StackState == POPPED)
    { sprintf(LogMsgBuf,"Popped SFDU marker '%s' off stack",
              MarkerStack[StackIdx+1]);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
    }
    if (StackIdx == -1) return (EOD);
  }

  if (Sfdu->Hdr.ClassId == 'Z' || Sfdu->Hdr.ClassId == 'U' ||
      Sfdu->Hdr.ClassId == 'F')
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG2,MODULE_NAME,
               "'Structure' class SFDU processed");
    if (StackState != PUSHED)
    { sprintf(LogMsgBuf,"Could not push %s SFDU Marker on stack",
              ExpandClass[ClassIdx].ClassName);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
    }
  }


  if (strncmp(Sfdu->Hdr.CntrlAuth,"NJPL",4) == 0)
  { sprintf(LogMsgBuf,"Processing NJPL '%c' (%02X) SFDU",
            Sfdu->Hdr.ClassId,Sfdu->Hdr.ClassId);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
    switch (Sfdu->Hdr.ClassId)
    { case 'I': Sfdu->ChdoCode = C16_TO_U16((char *)Sfdu->Buffer+20);
                Sfdu->ChdoHdrLth = C16_TO_U16((char *)Sfdu->Buffer+22);
                return (process_application_data(Sfdu));
      break;

      case 'K': return (process_catalog_data(Sfdu));
      break;

      default:
      break;
    }
  }

  return (RTS_NORMAL);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_tds_query.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "xvmaininc.h"

#if VMS_OS
#define  MULTINET_OLD_STYLE	0	/*	... MULTINET Sucks	*/
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

struct	tm {	/* see ctime(3) */
	int	tm_sec;
	int	tm_min;
	int	tm_hour;
	int	tm_mday;
	int	tm_mon;
	int	tm_year;
	int	tm_wday;
	int	tm_yday;
	int	tm_isdst;
};

extern struct tm *localtime(const time_t *);

#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>

#include "rts_logging.h"
#include "rts_cntrl.h"
#include "rts_errors.h"
#include "rts_const_defs.h"

#define  TDS_PORT_NUMBER	6666

#define  SFDU_RQST_LABEL 	"CCSD3ZS00001QUERYSPC"
#define  SFDU_END_LABEL 	"CCSD3RE00000QUERYSPC"

#define  PVL_RQST_LABEL 	"NJPL3IS0L009TDSQUERY"
#define  PVL_END_LABEL 		"CCSD3RE00000TDSQUERY"


static char	LogMsgBuf[256];


#define MODULE_NAME	"C_TDS"
/*****************************************************************************
 *				RTS_TDS_CONNECT
 *
 ****************************************************************************/
int	rts_tds_connect(
  char	*TdsServer,
  int	*TdsSocket,
  int	TdsPort)
{ struct hostent	*ServerData;
  struct sockaddr_in	ServerAddr;

  *TdsSocket = -1;

  /***  Verify TDS server host  ***/
  if (!(ServerData = gethostbyname(TdsServer)))
  { sprintf(LogMsgBuf,"Can't identify TDS Server: %s",TdsServer) ;
    rts_logger(RTS_LOG_TDS,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    return RTS_ERROR;
  }

  /***  Create a socket to TDS host  ***/
  memset((void *)&ServerAddr, 0, sizeof(ServerAddr));
  ServerAddr.sin_family = (short)AF_INET;
  memcpy((char *)&ServerAddr.sin_addr,
	   ServerData->h_addr,
	   ServerData->h_length);
  if (TdsPort == 0) TdsPort = TDS_PORT_NUMBER;
  ServerAddr.sin_port = (unsigned short)TdsPort;

  if ((*TdsSocket = socket(PF_INET, SOCK_STREAM, 0)) < 0)
  { sprintf(LogMsgBuf,"Could not create socket: %s",strerror(errno));
    rts_logger(RTS_LOG_TDS,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    *TdsSocket = -1;
    return RTS_ERROR;
  }

  /***  Connect to TDS Server  ***/
  if (connect(*TdsSocket,(void *)&ServerAddr,sizeof(struct sockaddr_in)) == -1)
  { sprintf(LogMsgBuf,"Can not connect to TDS Server: %s (%d)",
            TdsServer,TdsPort);
    rts_logger(RTS_LOG_TDS,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    *TdsSocket = -1;
    return RTS_ERROR;
  }

  sprintf(LogMsgBuf,"Opened socket (%d); filedesc = %d",TdsPort,*TdsSocket);
  rts_logger(RTS_LOG_TDS,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);

  return RTS_NORMAL;
}

#undef  MODULE_NAME
#define MODULE_NAME	"D_TDS"
/*****************************************************************************
 *				RTS_TDS_DISCONNECT
 *
 ****************************************************************************/
int	rts_tds_disconnect(
  int	*TdsSocket)
{
  close(*TdsSocket);

  TdsSocket = 0;
  return RTS_NORMAL;
}

#undef  MODULE_NAME
#define MODULE_NAME	"Q_TDS"
/*****************************************************************************
 *				RTS_TDS_QUERY
 *
 *	This routine generates an SFDU containing an "acceptable" TDS Query.
 *  The SFDU and Query structure are based on the syntax defined in the
 *  "TDS Query Request Specification Protocol" SIS; SFOC-1-TDS-ANY-QueryProt
 *  Revision Date: 07/14/97; V21 Baseline - Revision 04.
 *
 *	Only Frame or Packet based queries are generated; channel queries
 *  are not supported by this version.
 *
 *	By default if a string field is not empty, it is included in the
 *  query statement.  Most of the strings are project dependent and it is up
 *  to the calling program to format the string correctly.
 *
 ****************************************************************************/
int     rts_tds_query(
  int	TdsSocket,
  tds_query_typ	*TdsQuery)
{ char	QueryBuffer[1024],
	ItemBuffer[128];
  int	IdxCnt,
	PtrCnt,
	status;
  struct tm	StartTime,
		StopTime;

  memset(QueryBuffer,0,sizeof(QueryBuffer));

  /***  SFDU Primary and PVL labels  ***/
  strcat(QueryBuffer,SFDU_RQST_LABEL);
  strcat(QueryBuffer,PVL_RQST_LABEL);

  /*********************************/
  /***  PVL Query specification  ***/
  /*********************************/
  strcat(QueryBuffer,"OBJECT = frame_query;\n");

  /***  REQUESTER_NAME  ***/
  sprintf(ItemBuffer,"REQUESTER_NAME = \"%s\";\n",TdsQuery->Requester);
  strcat(QueryBuffer,ItemBuffer);

  /***  DESCRIPTION  ***/
  sprintf(ItemBuffer,"DESCRIPTION = \"%s\";\n",TdsQuery->Description);
  strcat(QueryBuffer,ItemBuffer);

  /***  MISSION_NAME  ***/
  sprintf(ItemBuffer,"MISSION_NAME = %s;\n",TdsQuery->Mission);
  strcat(QueryBuffer,ItemBuffer);

  /***  SPACECRAFT_NAME  ***/
  sprintf(ItemBuffer,"SPACECRAFT_NAME = %s;\n",TdsQuery->SpaceCraft);
  strcat(QueryBuffer,ItemBuffer);

  /***  TIME_RANGE  ***/
  if (TdsQuery->QueryType == TDS_QUERY_SCLK)
  { sprintf(ItemBuffer,"TIME_RANGE = { %s .. %s } %s;\n",
            TdsQuery->SCLK[0],TdsQuery->SCLK[1],
            TdsQueryTypes[TdsQuery->QueryType]);
    strcat(QueryBuffer,ItemBuffer);
  } else
  { switch (TdsQuery->QueryType)
    { case TDS_QUERY_SCET:
           memcpy(&StartTime,localtime(&TdsQuery->SCET[0]),sizeof(StartTime));
           memcpy(&StopTime,localtime(&TdsQuery->SCET[1]),sizeof(StopTime));
      break;
      case TDS_QUERY_ERT:
           memcpy(&StartTime,localtime(&TdsQuery->ERT[0]),sizeof(StartTime));
           memcpy(&StopTime,localtime(&TdsQuery->ERT[1]),sizeof(StopTime));
      break;
      case TDS_QUERY_RCT:
           memcpy(&StartTime,localtime(&TdsQuery->RCT[0]),sizeof(StartTime));
           memcpy(&StopTime,localtime(&TdsQuery->RCT[1]),sizeof(StopTime));
      break;
      default:
           sprintf(LogMsgBuf,"Invlaid TDS Query Type: %d",TdsQuery->QueryType);
           rts_logger(RTS_LOG_TDS,RTS_LOG_FATAL,MODULE_NAME,LogMsgBuf);
           return (RTS_ERROR);
      break;
    }

    strftime(ItemBuffer,sizeof(ItemBuffer)-1,
             "TIME_RANGE = { %Y/%j-%H:%M:%S .. ",&StartTime);
    strcat(QueryBuffer,ItemBuffer);
    strftime(ItemBuffer,sizeof(ItemBuffer)-1,"%Y/%j-%H:%M:%S } ",&StopTime);
    strcat(QueryBuffer,ItemBuffer);
    sprintf(ItemBuffer,"%s;\n",TdsQueryTypes[TdsQuery->QueryType]);
    strcat(QueryBuffer,ItemBuffer);
  }

  /***  DATA_FROM  (Optional, Default: NERT, then RT)  ***/
  if (TdsQuery->DataSource[0])
  { sprintf(ItemBuffer,"DATA_FROM = { %s };\n",TdsQuery->DataSource);
    strcat(QueryBuffer,ItemBuffer);
  }

  /***  TIME_ORDER  (Optional, Default: SCET)  ***/
  if (TdsQuery->SortOrder > 0)
  { sprintf(ItemBuffer,"TIME_ORDER = %s;\n",TdsSortOrder[TdsQuery->SortOrder]);
    strcat(QueryBuffer,ItemBuffer);
  }

  /***  DSS_ID  (Optional, Default: <all stations>)  ***/
  /*  not currently supported  */

  /***  TDS_LOADER_ID  (Optional, Default: <any loader>)  ***/
  /*  not currently supported  */

  /***  Package only supports  Frame or Packet queries ... not channels  ***/
  if (TdsQuery->DataType[0])
  { sprintf(ItemBuffer,"GROUP = FRAME;\n");
    strcat(QueryBuffer,ItemBuffer);

    /***  DATA_TYPE  ***/
    sprintf(ItemBuffer,"        DATA_TYPE = %s;\n",TdsQuery->DataType);
    strcat(QueryBuffer,ItemBuffer);

    /***  WSE_FILTER  (Optional, Default: <all frames/packets>)  ***/
    if (TdsQuery->WseFilter[0])
    { sprintf(ItemBuffer,"        WSE_FILTER = \"%s\";\n",TdsQuery->WseFilter);
      strcat(QueryBuffer,ItemBuffer);
    }

    /***  TELEMETRY_MODE  (Optional, Default: MERGED)  ***/
    /*  Explicity set just to make sure, otherwise not supported  */
    /*  Must be set if 'TdsQuery->DataSource' contains "RT"  */
    sprintf(ItemBuffer,"        TELEMETRY_MODE = MERGED;\n");
    strcat(QueryBuffer,ItemBuffer);

    sprintf(ItemBuffer,"END_GROUP = FRAME;\n");
    strcat(QueryBuffer,ItemBuffer);
  }

  sprintf(ItemBuffer,"END_OBJECT = frame_query;\n");
  strcat(QueryBuffer,ItemBuffer);

  strcat(QueryBuffer,PVL_END_LABEL);
  strcat(QueryBuffer,SFDU_END_LABEL);

  rts_logger(RTS_LOG_TDS,RTS_LOG_TEXT,MODULE_NAME,QueryBuffer);

/***	Remove 'new-line' characters .... not required by TDS
  for (IdxCnt=PtrCnt=0;QueryBuffer[IdxCnt] && IdxCnt<sizeof(QueryBuffer);
       IdxCnt++)
      if (QueryBuffer[IdxCnt] != '\n')
         QueryBuffer[PtrCnt++] = QueryBuffer[IdxCnt];
  QueryBuffer[PtrCnt++] = 0;
***/

  status = write(TdsSocket,QueryBuffer,strlen(QueryBuffer));
  if (status != strlen(QueryBuffer))
  { if (status < 0)
       sprintf(LogMsgBuf,"TDS query socket write error");
    else
       sprintf(LogMsgBuf,"TDS query transmit error; sent %d out of %d bytes",
               status,strlen(QueryBuffer));
    rts_logger(RTS_LOG_TDS,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
  }

  return RTS_NORMAL;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rts_sfdu_routines.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE rts_sfdu_routines
/*
/*   To Create the build file give the command:
/*
/*		$ vimake rts_sfdu_routines			(VMS)
/*   or
/*		% vimake rts_sfdu_routines			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE rts_sfdu_routines		/* Only one of these */
/*#define PROGRAM rts_sfdu_routines		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST rts_get_sfdu.c rts_chdo.c rts_sfdu_proc.c rts_tds_query.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

#if VMS_OS
#define LIB_NETWORK_NOSHR
#else
#define LIB_NETWORK
#endif

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of rts_sfdu_routines imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_rts_get_sfdu.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rts_typedefs.h"
#include "rts_sfdu.h"
#include "rts_logging.h"
#include "rts_errors.h"

#define  MAX_SFDU_LTH  5000
#define  MODULE_NAME	"TS"

static char	LogMsgBuf[256];

void	main(
  int	argc,
  char	*argv[])
{ int	idx,
	Status,
	SfduCount = 0;
  char	TempStrg[128];
  Byte	SfduBuf[MAX_SFDU_LTH];
  sfdu_record_typ	Sfdu;
  telemproc_typ	Cntrl;
  
  memset(&Sfdu,0,sizeof(sfdu_typ));
  memset(&Cntrl,0,sizeof(telemproc_typ));
  Sfdu.Buffer = SfduBuf;

  rts_set_prgmlogname("TSFDU");
  rts_set_logmasks("SFDU FATAL ERROR WARNING INFO T_ERROR T_WARN");

  if (argc == 2)
  { Cntrl.SFDU_Source = SFDU_SRC_FILE;
    strcpy(Cntrl.SFDU_Filename,argv[1]);
    sprintf(LogMsgBuf,"Testing SFDU read from file: %s",
            Cntrl.SFDU_Filename);
    rts_logger(RTS_LOG_ALWAYS,RTS_LOG_ALWAYS,MODULE_NAME,LogMsgBuf);
  } else
  { rts_logger(RTS_LOG_ALWAYS,RTS_LOG_ALWAYS,MODULE_NAME,
               "must supply only one parameter identifying SFDU filename");
    exit(0);
  }


  do 
  { Status = rts_get_sfdu(&Cntrl, &Sfdu);
    if (RTS_RTN_BAD(Status))
    { if (Status == INSUFF_DATA)
          rts_logger(RTS_LOG_ALWAYS,RTS_LOG_WARNING,MODULE_NAME,
                     "Insufficent data from READ");
      else if (Status == TIME_OUT)
              rts_logger(RTS_LOG_ALWAYS,RTS_LOG_WARNING,MODULE_NAME,
                         "Time-out waiting for SFDU");
      else if (Status == EOD)
              rts_logger(RTS_LOG_ALWAYS,RTS_LOG_WARNING,MODULE_NAME,
                         "'End Of Data' received while getting an SFDU");
      else
      { sprintf(LogMsgBuf,"Error reading SFDU (%d) file: %d",SfduCount,Status);
        rts_logger(RTS_LOG_ALWAYS,RTS_LOG_ALWAYS,MODULE_NAME,LogMsgBuf);
      }
    } else
    { SfduCount++;
      /***  Check SFDU  ***/
      Status = process_sfdu(&Sfdu);
      if (RTS_RTN_BAD(Status) || Status != DATA_SFDU)
      { if (Status == EOD)
        { if (Cntrl.SFDU_Source != SFDU_SRC_FILE) Status = EOD;
        } else if (RTS_RTN_BAD(Status))
        { sprintf(LogMsgBuf,"Error (%d) processing SFDU record",
                  ERR_NUMBER(Status));
          rts_logger(RTS_LOG_ALWAYS,RTS_LOG_TRACE_ERR,MODULE_NAME,LogMsgBuf);
        } else
         mpf_logger(MPF_LOG_TELEMPROC,RTS_LOG_DEBUG1,MODULE_NAME,
                          "SFDU does not contain telemetry data ... ignored");
        continue;
      }
      sprintf(LogMsgBuf,"SFDU %d contains CHDOs:",SfduCount);
      for (idx=0; idx<MAX_SFDU_HDRS; idx++)
      { if (Sfdu.Chdo[idx].Type && Sfdu.Chdo[idx].Length)
        { sprintf(TempStrg," %03d",Sfdu.Chdo[idx].Type);
          strcat(LogMsgBuf,TempStrg);
        }
        free_chdo_buffer(&Sfdu.Chdo[idx]);
      }
      rts_logger(RTS_LOG_ALWAYS,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
    }
  } while (RTS_RTN_GOOD(Status));

  sprintf(LogMsgBuf,"Read %d SFDU records",SfduCount);
  rts_logger(RTS_LOG_ALWAYS,RTS_LOG_ALWAYS,MODULE_NAME,LogMsgBuf);

  exit(0);
}
$!-----------------------------------------------------------------------------
$ create tst_rts_get_sfdu.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_rts_get_sfdu
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_rts_get_sfdu			(VMS)
/*   or
/*		% vimake tst_rts_get_sfdu			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_rts_get_sfdu		/* Only one of these */
#define PROGRAM tst_rts_get_sfdu		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_rts_get_sfdu.c

#define TEST

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_PVM
#define LIB_NETWORK
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_NETWORK

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_rts_get_sfdu imake file  **********/
$ Return
$!#############################################################################
$Other_File:
$ create rts_get_sfdu.hlp
1 RTS_GET_SFDU

  RTS_GET_SFDU is a routine that can extract SFDUs from an input file or they
  can be obtained from a socket.  The socket is a server socket and will wait
  until a connection is made to the socket.

  Calling Sequence:

  RTS_GET_SFDU( tdstot_cmnd, sfdu )
  tdstot_cmnd_struct  *tdstot_cmnd;
  unsigned char *sfdu;

  where	tdstot_cmnd is a command structure that, defined in gll_rt_cmnd_struct,
  that contains information for TDS/TOT processing and sfdu is an unsigned 
  char pointer that contains the header and data information.  RTS_GET_SFDU 
  returns a zero if data is still available, a one if data is no longer 
  available, and a minus one if no data is available.

3 History

  Original Programmer: Damon D. Knight, June 20, 1994 
  Cognizant Programmer: Damon D. Knight
  Source Language: C
  Revisions: 	

  Ported to UNIX by:  
  Revisions on Ported Version:
$ Return
$!#############################################################################
