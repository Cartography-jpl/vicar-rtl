/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "return_status.h"
#include "rts_logging.h"
#include "rts_cat_defs.h"
#include "mdms_query.h"
#include "mdms_message.h"

#define  MODULE_NAME "CAT"
/*****************************************************************************
 *				RTS_CAT_UTIL
 *
 *	The RTS_CAT_UTIL module is a collection of higher-level interface
 *  routines that communicate with the MDMS database interface.  The routines
 *  of this module are table driven and require the calling application to
 *  define the table and the storage for the variables that will be transfered
 *  to and from the catalog.  The reason for requiring the application to
 *  define the table and storage is to allow these modules to be multimission
 *  and keep different projects from including or linking other project's
 *  modules, just to perform a multimission capability.
 *
 *	The routines of this module are also geared toward Realtime usage.
 *  This means that the modules are responsible for cleaning up any memory
 *  they allocate and more importantly, they do not write any messages to
 *  an output device (disk, stdout, stderr, etc).  Instead there is a RTS
 *  message facility that logs the text message(s) associated with an error
 *  and prints it out accordingly.
 *
 *  ASSUMPTIONS/CONSTRAINTS
 *
 *	The stored procedures that these routines support should not return
 *  anything fancy.  The returned value of the stored procedure must be a
 *  '0' for an successful execution.  If the returned value is positive and
 *  odd, an error occurred.  If the returned value is positive and even, a
 *  non-standard "event" occurred, but the procedure proceeded with its main
 *  purpose.  Even values are available for specific purposes and do not
 *  cause the multimission code to do anything to abort the call.  Negative
 *  return values are considered Sybase errors.
 *
 *	Except for the returned function call and the bindable table
 *  elements/columns/fields, the stored procedures should not return anything
 *  else.
 *
 *	The stored procedures are responsible for cleaning-up after any
 *  detectable errors (i.e., errors that would return an odd status value).
 *  This is too allow these routines to automatically issue a retry, if
 *  specified.  A typical retry could occur after a catalog dead-lock error
 *  terminated the query/transaction.  Sybase errors are not retried.
 *
 *****************************************************************************/

#define  MAX_OK_LOOP		5
#define  MAX_CMND_BUF_LTH	4096
#define  MAX_CMND_LTH		(MAX_CMND_BUF_LTH - 16)

/***  Global variables  ***/
static	char	LogMsgBuf[256];
static	char	CmndBuf[MAX_CMND_BUF_LTH];
static	int	CatInitialized = FALSE;
static	int	Elements = 0;
static	int	RetryProcess = FALSE;
static  MDMS_MSGTABLETYPE	MsgTable;
static  MDMS_MSGDESC		*MsgDesc;
static	MDMS_QI_DESC_OBJ	*qiDesc;
static	CatProcedureTable_typ	*Table = 0;
static  CatRetryDefs_typ	RetryParams =
				{	RTS_CAT_RETRY_DELAY,
					RTS_CAT_RETRY_NUM,
					RTS_CAT_RETRY_ANNOUN
				};
static	int	rts_cat_cmnd( int, CatParamElement_typ *, int );
static	void	rts_cat_bind( CatParamElement_typ * );
static	int	rts_mdms_err_msg( int, char * );

#undef   MODULE_NAME
#define  MODULE_NAME "RM"
/******************************************************************************
 *				RTS_MDMS_ERR_MSG
 *
 *	Process the error messages from an MDMS transaction request.
 *  Returns whether the operation is worth retrying based on the error(s)
 *  processed.
 *****************************************************************************/
static	int	rts_mdms_err_msg(
  int	CallStatus,
  char	*Caller)
{ int	Count,
	LogSvrtyMask,
	MdmsDbMsgNo,
	MdmsSeverityNum,
	TryAgain = FALSE;
  char	*MdmsMsgPtr,
	*TmpMsgPtr,
	MsgNoChar,
	MdmsSeverity[24],
	MdmsMsg[1024];
  MDMS_MSGINFOSTRUCT	*MsgInfo;

  MdmsSeverityNum = mdms_msgSeverity(MsgDesc);
  switch (MdmsSeverityNum)			/* MDMS transaction status */
  { case  -3: strcpy(MdmsSeverity,"Fatal");
              LogSvrtyMask = RTS_LOG_FATAL;
    break;

    case  -2: strcpy(MdmsSeverity,"Error");
              LogSvrtyMask = RTS_LOG_ERROR;
    break;

    case  -1: strcpy(MdmsSeverity,"Warning");	/* Only errors in this rtn */
              LogSvrtyMask = RTS_LOG_ERROR;
    break;

    case   1: strcpy(MdmsSeverity,"Normal");
              LogSvrtyMask = RTS_LOG_ERROR;	/* Only errors in this rtn */
    break;

    case   2: strcpy(MdmsSeverity,"Info");
              LogSvrtyMask = RTS_LOG_ERROR;	/* Only errors in this rtn */
    break;

    default:  sprintf(MdmsSeverity,"Unknown (%d)",MdmsSeverityNum);
              LogSvrtyMask = RTS_LOG_ERROR;
    break;
  }

  while (MsgInfo = mdms_getQueuedMessage(MsgDesc))
  { MdmsMsgPtr = MsgInfo->msgText;
    rts_logger(RTS_LOG_CATALOG,RTS_LOG_DEBUG4,Caller,MdmsMsgPtr);

    /***  Remove location and time stamp from MDMS message buffer  ***/
    memset(MdmsMsg,0,sizeof(MdmsMsg));

    TmpMsgPtr = MdmsMsgPtr;
    while (*TmpMsgPtr == '\n') TmpMsgPtr++;
    if (strstr(TmpMsgPtr,"MsgNo"))
       TmpMsgPtr = strstr(TmpMsgPtr,"MsgNo");
    else if (strstr(TmpMsgPtr,"\n"))
            TmpMsgPtr = strstr(TmpMsgPtr,"\n") + 1;

    /***  Concatenate message lines  ***/
    while (strlen(TmpMsgPtr))
    { Count = strcspn(TmpMsgPtr,"\n");

      if ((strlen(MdmsMsg)+Count+1) >= sizeof(MdmsMsg))
      { Count = sizeof(MdmsMsg) - (strlen(MdmsMsg) + 1); 
        strncat(MdmsMsg,TmpMsgPtr,Count);
        break;
      }

      strncat(MdmsMsg,TmpMsgPtr,Count);

      TmpMsgPtr += Count;
      if (*TmpMsgPtr == '\n')
      { if ((strlen(MdmsMsg)+6) > sizeof(MdmsMsg)) break;
        strcat(MdmsMsg," >> ");
        TmpMsgPtr++;
      }
    }

    MdmsDbMsgNo = MsgInfo->msgTable.sysMsgNo;
    MsgNoChar = 'S';
    if (!MdmsDbMsgNo)
    { MdmsDbMsgNo = MsgInfo->msgTable.dbLibMsgNo;
      MsgNoChar = 'L';
    }
    if (!MdmsDbMsgNo)
    { MdmsDbMsgNo = MsgInfo->msgTable.dbsMsgNo;
      MsgNoChar = 'D';
    }
/***  Revisit this after real-world testing  ***
    if (MdmsDbMsgNo == MDMS_DEADLOCK) TryAgain = TRUE;
 **/
    TryAgain = TRUE;

    if (CallStatus == MDMS_FATAL ||	/* Decide if Catalog is gone */
        CallStatus == MDMS_ERROR)
    {
/***  DBPROCESS is dead or not enabled  ***
      if (MdmsDbMsgNo = 20047 && MsgNoChar == 'L')
      { rts_cat_logout();
        CatInitialized = FALSE;
      }
 **/
    }

    sprintf(LogMsgBuf,"MDMS %s msg [%d%c]: %-.200s",
            MdmsSeverity,MdmsDbMsgNo,MsgNoChar,MdmsMsg);
    rts_logger(RTS_LOG_CATALOG,LogSvrtyMask,Caller,LogMsgBuf);
  }

  return TryAgain;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CA"
/******************************************************************************
 *				RTS_CAT_ADD
 *
 *	Adds one record to the data base.
 *****************************************************************************/
int	rts_cat_add(
  int	Procedure,		/* Index to TABLE identifying catalog info */
  void	*Record)		/* Information to add to catalog           */
{ int	Count,
	MdmsStatus = 0,
	OkCount = 0,
	RowsAffected = 0,
	Retries,
	Retry = FALSE,
	RtnStatus = RTN_ERROR,
	status;

  if (!CatInitialized)
  { rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
               "Can not complete catalog ADD operation ... not logged in");
    return (RTN_ERROR);
  }

  /*** Clear Error Message Queue  ***/

  if (!Record) return RTN_ERROR;
  if (Procedure >= Elements) return RTN_CAT_EXCEEDED_TABLE;

  memcpy(Table[Procedure].Record,Record,Table[Procedure].RecordLth);

  status = rts_cat_cmnd(Procedure,Table[Procedure].Query, -1);
  if (RTN_FAILURE(status))
  { rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
               "Can not complete catalog ADD operation ... command failure");
    return (status);
  }

  do switch (status = mdms_qiTblDesc(qiDesc))
  { case MDMS_OK:
             /***  Make sure this does not become an infinite loop  ***/
             if (OkCount++ >= MAX_OK_LOOP)
             { sprintf(LogMsgBuf,
                       "Stored procedure >%s< return error; loop count exit",
                       Table[Procedure].StoredProcedure);
               rts_logger(RTS_LOG_CATALOG,RTS_LOG_WARNING,MODULE_NAME,
                          LogMsgBuf);
               status = mdms_qiCancel(qiDesc);
               if (status == MDMS_OK)
               { sprintf(LogMsgBuf,"Stored procedure >%s< Cancelled",
                         Table[Procedure].StoredProcedure);
               } else
               { sprintf(LogMsgBuf,"Error cancelling stored procedure >%s<",
                         Table[Procedure].StoredProcedure);
               }
               rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
                          LogMsgBuf);

               RtnStatus = RTN_CAT_STORED_PROC;
               status = -1;
             }
         break;

    case MDMS_ENDOFQUERY:
             OkCount = 0;
             if (MDMS_HASRETSTAT(qiDesc) == MDMS_TRUE)
             { MdmsStatus = MDMS_PROCRETURN(qiDesc);
               /***  Check Status  ***/
               if (MdmsStatus & 0x01)
               { Retry = rts_mdms_err_msg(MdmsStatus,MODULE_NAME);
                 sprintf(LogMsgBuf,"Stored procedure >%s< Query error: %d",
                         Table[Procedure].StoredProcedure,MdmsStatus);
                 rts_logger(RTS_LOG_CATALOG,RTS_LOG_TRACE_ERR,MODULE_NAME,
                            LogMsgBuf);
               }
             }
             if ((Count = MDMS_RETVALUECOUNT(qiDesc)) > 0)
             { /***  Get Returned Values  ***/
             }
             RowsAffected = MDMS_AFFECTED(qiDesc);
         break;

    case MDMS_ENDOFTRANSACTION:
             OkCount = 0;
             if (!(MdmsStatus & 0x01)) RtnStatus =  RTN_NORMAL;
             status = -1;
         break;

    case MDMS_ERROR:
    case MDMS_FATAL:
             OkCount = 0;
             rts_mdms_err_msg(status,MODULE_NAME);
             sprintf(LogMsgBuf,"Stored procedure >%s< fatal error: %d",
                         Table[Procedure].StoredProcedure,status);
             rts_logger(RTS_LOG_CATALOG,RTS_LOG_TRACE_ERR,MODULE_NAME,
                        LogMsgBuf);
         break;

    default: /***  Sybase Error ... Bummer  ***/
             OkCount = 0;
             sprintf(LogMsgBuf,"Unsupported MDMS return status: %d",
                     status);
             rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
             rts_mdms_err_msg(status,MODULE_NAME);
             status = -1;
         break;
  } while (status >= 0);

  /***  Check if a Retry is needed  ***/
  if (Retry && !RetryProcess)
  { RetryProcess = TRUE;
    for (Retries=0; Retries<RetryParams.Retries; Retries++)
    { sleep(RetryParams.Delay);
      if (RetryParams.Announce)
      { sprintf(LogMsgBuf,"Retry %d of rts_cat_add: %s",
                Retries,Table[Procedure].StoredProcedure);
        rts_logger(RTS_LOG_CATALOG,RTS_LOG_ALWAYS,MODULE_NAME,LogMsgBuf);
      }
      RtnStatus = rts_cat_add(Procedure,Record);
      if (RTS_RTN_GOOD(RtnStatus)) break;
    }
    RetryProcess = FALSE;
  }

  return RtnStatus;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CB"
/******************************************************************************
 *				RTS_CAT_BIND
 *
 *	Binds all of the variables to the qiDescriptor as defined by the
 *  table.
 *****************************************************************************/
static void	rts_cat_bind(
  CatParamElement_typ	*Bind)
{ int	idx;

  for (idx=0; Bind[idx].Address; idx++)
      MDMS_BIND(qiDesc, (idx+1), Bind[idx].DataType, Bind[idx].MaxLength,
                Bind[idx].Address);

  return;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CC"
/******************************************************************************
 *				RTS_CAT_CMND
 *
 *	Formats the MDMS comand string based on the query parameters.
 *****************************************************************************/
static int	rts_cat_cmnd(
  int			Procedure,
  CatParamElement_typ	*Query,
  int			MaxRows)
{ int	idx;
  char	TmpStrg[256];

  rts_logger(RTS_LOG_CATALOG,RTS_LOG_DEBUG5,MODULE_NAME,
             "Entering rts_cat_cmnd");
/***
  "set rowcount <rows>\n"
  "<rts_cat_cmnd>\n"
  "set rowcount 0"
 ***/
  if (MaxRows < 0)
     sprintf(CmndBuf,"execute %s ",Table[Procedure].StoredProcedure);
  else
     sprintf(CmndBuf,"set rowcount %d\nexecute %s ",
             MaxRows,Table[Procedure].StoredProcedure);
  for (idx=0; Query[idx].Address; idx++)
      if (!Query[idx].Valid || *Query[idx].Valid)
  { switch (Query[idx].DataType)
    { case MDMS_TINYBIND:
                sprintf(TmpStrg,"%d, ",*((unsigned char *)Query[idx].Address));
           break;
      case MDMS_SMALLBIND:
                sprintf(TmpStrg,"%d, ",*((short *)Query[idx].Address));
           break;
      case MDMS_INTBIND:
                sprintf(TmpStrg,"%d, ",*((int *)Query[idx].Address));
           break;
      case MDMS_REALBIND:
                sprintf(TmpStrg,"%f, ",*((float *)Query[idx].Address));
           break;
      case MDMS_FLT8BIND:
                sprintf(TmpStrg,"%e, ",*((double *)Query[idx].Address));
           break;
      case MDMS_NTBSTRINGBIND:
                sprintf(TmpStrg,"\"%s\", ",(char *)Query[idx].Address);
           break;
      default:
                sprintf(LogMsgBuf,"Unsupported MDMS data type: %d",
                        Query[idx].DataType);
                rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
           break;
    }
    if ((strlen(CmndBuf)+strlen(TmpStrg)) > MAX_CMND_LTH)
    { sprintf(LogMsgBuf,"Command larger than buffer (%d vs %d)",
              (strlen(CmndBuf)+strlen(TmpStrg)),MAX_CMND_LTH);
      rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      rts_logger(RTS_LOG_CATALOG,RTS_LOG_DEBUG2,MODULE_NAME,CmndBuf);
      return RTN_TOO_MUCH_DATA;
    }
    strcat(CmndBuf,TmpStrg);
  } else
  { if ((strlen(CmndBuf)+6) > MAX_CMND_LTH)
    { sprintf(LogMsgBuf,"Command larger than buffer (%d vs %d)",
              (strlen(CmndBuf)+strlen(TmpStrg)),MAX_CMND_LTH);
      rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      rts_logger(RTS_LOG_CATALOG,RTS_LOG_DEBUG2,MODULE_NAME,CmndBuf);
      return RTN_TOO_MUCH_DATA;
    }
    strcat(CmndBuf,"NULL, ");
  }

  if (idx > 0) CmndBuf[strlen(CmndBuf)-2] = 0;
  if (MaxRows > 0) strcat(CmndBuf,"\nset rowcount 0");

  MDMS_SETCMD(qiDesc,CmndBuf);
  rts_logger(RTS_LOG_CATALOG,RTS_LOG_DEBUG2,MODULE_NAME,CmndBuf);

  return RTN_NORMAL;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CG"
/******************************************************************************
 *				RTS_CAT_GET
 *
 *	Obtains catalog records based on the query structure.
 *****************************************************************************/
int	rts_cat_get(
  int	Procedure,		/* Index to TABLE identifying catalog info */
  int	MaxRows,		/* Max number of rows to return            */
  int	RowOffset,		/* Number of inital rows to skip           */
  void	*Rows,			/* Where to put the catalog returned data  */
  int	*RowsReturned)
{ int	Count,
	idx,
	OkCount = 0,
	RowsAffected = 0,
	RowCnt = 0,
	RowLength = Table[Procedure].RecordLth,
	RtnStatus = RTN_ERROR,
	MdmsStatus = 0,
	status;
  CatParamElement_typ	*Param = Table[Procedure].Element;

  rts_logger(RTS_LOG_CATALOG,RTS_LOG_DEBUG5,MODULE_NAME,
             "Entering rts_cat_get");

  if (!CatInitialized)
  { rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
               "Can not complete catalog GET operation ... not logged in");
    return (RTN_ERROR);
  }

  /*** Clear Error Message Queue  ***/

  if (!Rows) return RTN_ERROR;
  if (RowsReturned) *RowsReturned = 0;
  if (Procedure >= Elements) return RTN_CAT_EXCEEDED_TABLE;

  status = rts_cat_cmnd(Procedure,Table[Procedure].Query,(MaxRows+RowOffset));
  if (RTN_FAILURE(status))
  { rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
               "Can not complete catalog GET operation ... command failure");
    return (status);
  }

  do switch (status = mdms_qiTblDesc(qiDesc))
  { case MDMS_OK:
             /***  Make sure this does not become an infinite loop  ***/
             if (OkCount++ >= MAX_OK_LOOP)
             { sprintf(LogMsgBuf,
                       "Stored procedure >%s< return error; loop count exit",
                       Table[Procedure].StoredProcedure);
               rts_logger(RTS_LOG_CATALOG,RTS_LOG_WARNING,MODULE_NAME,
                          LogMsgBuf);
               status = mdms_qiCancel(qiDesc);
               if (status == MDMS_OK)
               { sprintf(LogMsgBuf,"Stored procedure >%s< Cancelled",
                         Table[Procedure].StoredProcedure);
               } else
               { sprintf(LogMsgBuf,"Error cancelling stored procedure >%s<",
                         Table[Procedure].StoredProcedure);
               }
               rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
                          LogMsgBuf);

               RtnStatus = RTN_CAT_STORED_PROC;
               status = -1;
             }

             if (MDMS_ATTRCOUNT(qiDesc) <= 0)		/* nothin comin back */
             { rts_logger(RTS_LOG_CATALOG,RTS_LOG_INFO,MODULE_NAME,
                          "No Rows Found");
               break;
             }

             OkCount = 0;

             rts_cat_bind(Param);
             do switch (status = mdms_qiNextRow(qiDesc))
             { case MDMS_ROWRETURNED:
                        sprintf(LogMsgBuf,"Row %d returned",RowCnt+1);
                        rts_logger(RTS_LOG_CATALOG,RTS_LOG_DEBUG1,MODULE_NAME,
                                   LogMsgBuf);
                        if (RowOffset > 0)	/* Skip unwanted rows */
                        { RowOffset--;
                          break;
                        }

                       	if (RowCnt >= MaxRows)	/* Ignore extra rows  */
                            break;

                        for (idx=0; Param[idx].Address; idx++)
                            if (Param[idx].Valid)
                               *Param[idx].Valid =
                                     (MDMS_VALUELENGTH(qiDesc,idx) > 0);
                        memcpy((void *)((int)Rows+(RowCnt*RowLength)),
                               Table[Procedure].Record,RowLength);
                        RowCnt++;
                        if (RowsReturned) *RowsReturned = RowCnt;
                    break;

               case MDMS_ENDOFQUERY:
                        if (MDMS_HASRETSTAT(qiDesc) == MDMS_TRUE)
                        { MdmsStatus = MDMS_PROCRETURN(qiDesc);
                          /***  Check Status  ***/
                          if (MdmsStatus & 0x01)
                          { rts_mdms_err_msg(MdmsStatus,MODULE_NAME);
                            sprintf(LogMsgBuf,
                                    "Stored procedure >%s< EndQuery error: %d",
                                    Table[Procedure].StoredProcedure,MdmsStatus);
                            rts_logger(RTS_LOG_CATALOG,RTS_LOG_TRACE_ERR,
                                       MODULE_NAME,LogMsgBuf);
                          }
                        }
                        if ((Count = MDMS_RETVALUECOUNT(qiDesc)) > 0)
                        { /***  Get Returned Values  ***/
                        }
                        RowsAffected = MDMS_AFFECTED(qiDesc);
                    break;

               case MDMS_ERROR:
               case MDMS_FATAL:
                        rts_mdms_err_msg(status,MODULE_NAME);
                        sprintf(LogMsgBuf,"Stored procedure >%s< NextRow error: %d",
                                Table[Procedure].StoredProcedure,status);
                        rts_logger(RTS_LOG_CATALOG,RTS_LOG_TRACE_ERR,
                                   MODULE_NAME,LogMsgBuf);
                    break;

               default:
                        rts_mdms_err_msg(status,MODULE_NAME);
                        sprintf(LogMsgBuf,
                                "Unsupported NextRow return status: %d",
                                status);
                        rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
                                   LogMsgBuf);
                        /***  Bummer, shouldn't be here  ***/
                    break;
             } while (status == MDMS_ROWRETURNED);
         break;

    case MDMS_ENDOFQUERY:
             OkCount = 0;
             if (MDMS_HASRETSTAT(qiDesc) == MDMS_TRUE)
             { MdmsStatus = MDMS_PROCRETURN(qiDesc);
               /***  Check Status  ***/
               if (MdmsStatus & 0x01)
               { rts_mdms_err_msg(MdmsStatus,MODULE_NAME);
                 sprintf(LogMsgBuf,"Stored procedure >%s< Query error: %d",
                         Table[Procedure].StoredProcedure,MdmsStatus);
                 rts_logger(RTS_LOG_CATALOG,RTS_LOG_TRACE_ERR,MODULE_NAME,
                            LogMsgBuf);
               }
             }
             if ((Count = MDMS_RETVALUECOUNT(qiDesc)) > 0)
             { /***  Get Returned Values  ***/
             }
             RowsAffected = MDMS_AFFECTED(qiDesc);
         break;

    case MDMS_ENDOFTRANSACTION:
             if (MdmsStatus & 0x01) return RTN_ERROR;
             return RTN_NORMAL;
         break;

    case MDMS_ERROR:
    case MDMS_FATAL:
             OkCount = 0;
             rts_mdms_err_msg(status,MODULE_NAME);
             sprintf(LogMsgBuf,"Stored procedure >%s< fatal error: %d",
                         Table[Procedure].StoredProcedure,status);
             rts_logger(RTS_LOG_CATALOG,RTS_LOG_TRACE_ERR,MODULE_NAME,
                        LogMsgBuf);
         break;

    default: /***  Bummer, shouldn't be here  ***/
             OkCount = 0;
             rts_mdms_err_msg(status,MODULE_NAME);
             sprintf(LogMsgBuf,"Unsupported MDMS function return status: %d",
                     status);
             rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
         break;
  } while (status >= 0 && status != MDMS_ENDOFTRANSACTION);

  /***  Bummer ... An error Occured  ***/
  return RtnStatus;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CT"
/******************************************************************************
 *				RTS_CAT_LOAD_TABLE
 *
 *	This routine assigns the table used for all of the functions used by
 *  these routines.  The table is meant to be project specific and can be
 *  changed in the middle of a program if desired.  The parameter UserTable is
 *  expected to be a static variable in the program/routine that called this
 *  routine.
 *****************************************************************************/
void	rts_cat_load_table(
  void	*UserTable,
  int	UserElements)
{
  Table = (CatProcedureTable_typ *)UserTable;
  Elements = UserElements;

  return;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CI"
/******************************************************************************
 *				RTS_CAT_LOGIN
 *
 *****************************************************************************/
int	rts_cat_login(
  CatLogin_typ	*Login)
{ int	status;

  if (!Login) return RTN_ERROR;

/* - How do we set our own Message Handler
 **/
  if (strlen(Login->Program))
     MsgDesc = mdms_messageAlloc(Login->Program,"MIPL");
  else MsgDesc = mdms_messageAlloc("<ProgramName>","MIPL");
  if (!MsgDesc)
  { /***  Bummer ... could not initialize Message Descriptor  ***/
    rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
               "Could not initialize MDMS message descriptor");

    return RTN_ERROR;
  }
  mdms_sybErrHandler(MsgDesc, MDMS_ON);
  mdms_sybMsgHandler(MsgDesc, MDMS_ON);
  mdms_msgTerm(MsgDesc, MDMS_OFF);
  mdms_setMsgQueueState(MsgDesc, MDMS_ON);
/***
  mdms_initLastMessage(MsgDesc);
 ***
  mdms_bufferMessages(MsgDesc, MDMS_ON);
 ***
  mdms_sybSuppressMsgHeader(MsgDesc, 1000);
 **/

  if (!(qiDesc = mdms_qiDescAlloc(MsgDesc)))
  { /***  Bummer ... could not allocate Message Descriptor  ***/
    rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,
               "Could not allocate MDMS message descriptor");
    return RTN_ERROR;
  }

  if (strlen(Login->UserName)) MDMS_SETUSER(qiDesc,	Login->UserName);
  if (strlen(Login->Password)) MDMS_SETPSWD(qiDesc,	Login->Password);
  if (strlen(Login->Program))  MDMS_SETPROG(qiDesc,	Login->Program);
  if (strlen(Login->Server))   MDMS_SETSERVER(qiDesc,	Login->Server);
  if (strlen(Login->DataBase)) MDMS_SETDBNAME(qiDesc,	Login->DataBase);

  status = mdms_qiLogin(qiDesc);
  if (status != MDMS_OK)
  { /*** Bummer, not logged in ***/
    rts_mdms_err_msg(status,MODULE_NAME);
    sprintf(LogMsgBuf,"Could not log '%s' into '%s' database on '%s'",
            Login->UserName,Login->DataBase,Login->Server);
    rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    return RTN_ERROR;
  }

  sprintf(LogMsgBuf,"Logged '%s' into '%s' database on '%s'",
          Login->UserName,Login->DataBase,Login->Server);
  rts_logger(RTS_LOG_CATALOG,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);

  CatInitialized = TRUE;
  return RTN_NORMAL;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CO"
/******************************************************************************
 *				RTS_CAT_LOGOUT
 *
 *****************************************************************************/
int	rts_cat_logout()
{ int	status;

  status = mdms_qiExit();
  CatInitialized = FALSE;

  /***  Free everything we can  ***/

  if (status == MDMS_OK) return RTN_NORMAL;
  else return RTN_ERROR;
}

#undef   MODULE_NAME
#define  MODULE_NAME "CR"
/******************************************************************************
 *				RTS_CAT_GET_RETRY
 *
 *	Gets the parameters that handles re-execuition of commands that failed
 *****************************************************************************/
void	rts_cat_get_retry(
  CatRetryDefs_typ *UserOpts )
{
  memcpy(UserOpts,&RetryParams,sizeof(CatRetryDefs_typ));

  return;
}

/******************************************************************************
 *				RTS_CAT_SET_RETRY
 *
 *	Sets the parameters that handles re-execuition of commands that failed
 *****************************************************************************/
void	rts_cat_set_retry(
  CatRetryDefs_typ *UserOpts )
{
  if (UserOpts->Delay < RTS_CAT_RETRY_MIN_DLY ||
      UserOpts->Delay > RTS_CAT_RETRY_MAX_DLY)
  { sprintf(LogMsgBuf,
            "Requested delay time (%d) outside of valid range; %d to %d",
            UserOpts->Delay,RTS_CAT_RETRY_MIN_DLY,RTS_CAT_RETRY_MAX_DLY);
    rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    return;
  }

  if (UserOpts->Retries < RTS_CAT_RETRY_MIN_NUM ||
      UserOpts->Retries > RTS_CAT_RETRY_MAX_NUM)
  { sprintf(LogMsgBuf,
            "Requested number of retries (%d) outside of valid range; %d to %d",
            UserOpts->Retries,RTS_CAT_RETRY_MIN_NUM,RTS_CAT_RETRY_MAX_NUM);
    rts_logger(RTS_LOG_CATALOG,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    return;
  }

  memcpy(&RetryParams,UserOpts,sizeof(CatRetryDefs_typ));

  return;
}
