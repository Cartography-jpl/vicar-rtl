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
