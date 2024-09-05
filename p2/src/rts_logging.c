/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <time.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "return_status.h"
#include "rts_typedefs.h"
#include "rts_ipc_defs.h"
#include "rts_log_masks.h"
#include "rts_const_defs.h"

Uint	rts_log_severity;	/* External for everyone else */
Uint	rts_log_mask;		/* External for everyone else */
Uint	gll_log_mask;		/* External for everyone else */
Uint	m94_log_mask;		/* External for everyone else */
Uint	mpf_log_mask;		/* External for everyone else */

static Flag	log_to_file = FALSE;
static Flag	log_to_logger = FALSE;
static Flag	log_to_stdio = TRUE;
static Flag	log_to_vicar = FALSE;
static Flag	JulianFormat = FALSE;
static Flag	UTC_Format = FALSE;
static FILE	*LogFile;
static char	*rts_severity[32] = {
			"FATAL",    "ERROR",  "T_ERROR", "WARNING",
                        "T_WARNING", "INFO",  "TEXT",    "SUMMARY",
			0, 0, 0, 0, 0, 0, 0, 0,
			"COMBO1",  "COMBO2",  "COMBO3",  "COMBO4",
			"DEBUG1",  "DEBUG2",  "DEBUG3",  "DEBUG4",
                        "DEBUG5",  "DEBUG6",  "DEBUG7",  "DEBUG8",
			0, 0,      "PAGE",    "ALWAYS"};
static char	*rts_masks[32] = {
			"GENERIC", "PARAM",  "SFDU",  "TDS",
                        "IPC",   0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, "DISPLAY", "PKTS", "CATALOG", "ALWAYS"};
static char	*gll_masks[32] = {
			"TLMPROC", "FLTR",  "PWS",     "SSI",
			"NIMS",    "ICT", 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, "RICE", "DISPLAY", "PKTS", "CATALOG",
			"ALWAYS"};
static char	*m94_masks[32] = {
			"TLMPROC", "HRSC",  "WAOSS",   "ARGUS",
			"REED-SOLOMON", "CHECK-SUM",  0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0,  "PKTS", "CATALOG",
			"ALWAYS"};
static char	*mpf_masks[32] = {
			"TLMPROC", "IMP",   "RVR",     "APX",
			"BTC",    "JPEG",   "MET", 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, "RICE", "DISPLAY", "PKTS", "CATALOG",
                        "ALWAYS"};



static	char	LogPrgmName[MAX_LOG_PGRM_LTH] = {""};
static	char	LogMsgBuf[255];
static	int	(*ptr_routine)(int,int,char *) = NULL;

	int	basic_logger( Uint, char *, char *, char *);
	void	rts_format_date_time( char *);
	int	rts_log_to_file ( char * );
	void	rts_log_to_logger ( Uint );
	void	rts_log_to_routine( int (*)(int,int,char*) );
	void	rts_log_to_stdio ( Uint );
	void	rts_log_to_vicar ( Uint );
	void	rts_set_julian_format( Uint );
	void	rts_set_UTC_format( Uint );
	void	rts_set_logmasks( char *);
	void	rts_set_prgmlogname( char *);

/***  From the redesign Realtime effort  ***/
#define  RT_LOG_TYPE_ALERT              1
#define  RT_LOG_TYPE_STATUS             2
#define  RT_LOG_TYPE_PERFORMANCE        3
#define  RT_LOG_TYPE_DEBUG              4

/******************************************************************************
 *				BASIC_LOGGER
 *
 *	Formats and sends a log packet to the RTCNTRL program.  Only log
 *  messages of the multimission type should use this routine.
 *****************************************************************************/

int	basic_logger(
  Uint	severity,
  char	*location,
  char	*subsystem,
  char	*message)
{ int	status;
  char	buffer[2200],
	date_time[24],
	*PrgmPtr = LogPrgmName;

  if (!message) return (RTN_MISSING_VALUE);

  if (severity & (RTS_LOG_TEXT | RTS_LOG_SUMMARY))
  { strncpy(buffer,message,2048);
    if (strlen(message) > 2048) buffer[2048] = 0;
  } else
  { rts_format_date_time(date_time);
    if (location && subsystem)
       sprintf(buffer,"%s [%s.%s.%s] %.2048s",
               date_time,location,PrgmPtr,subsystem,message);
    else sprintf(buffer,"%s %.2048s",date_time,message);
  }
  if (strlen(message) > 2048) strcat(buffer," ...");

  if (log_to_stdio)
     printf("%s\n",buffer);

  if (log_to_file)
     fprintf(LogFile,"%s\n",buffer);

  if (log_to_vicar)
     zvmessage(message,"");

  if (ptr_routine)	/* for CORBA based RT-redesign */
  { int verbosity;

    if (severity & 0x0F)
    { verbosity = (((severity & 0x0F)+3)/5 + 1);
       (*ptr_routine)(RT_LOG_TYPE_ALERT,verbosity,message);
    } else if (severity & RTS_LOG_DEBUG)
    { (*ptr_routine)(RT_LOG_TYPE_DEBUG,1,message);
    } else (*ptr_routine)(RT_LOG_TYPE_STATUS,1,message);
  }

  if (log_to_logger)
  { /* Send PVM packet to RT_LOGGER */
    status = rts_send_cmnd( RTS_PRJ_RTS, RTS_PRGM_LOGGER, buffer);
  }

  return ( RTN_NORMAL );		/* Only needed for define to work */
}

/******************************************************************************
 *				RTS_TO_ROUTINE
 *****************************************************************************/
void	rts_log_to_routine(
  int (*logging_routine)(int,int,char*)
)
{
  ptr_routine = logging_routine;

  return;
}

/******************************************************************************
 *				RTS_FLUSH_LOG
 *
 *	Flushes the system buffers for file and stdio logging
 *
 *****************************************************************************/
void	rts_flush_log( void )
{

  if (log_to_file) fflush(LogFile);
  if (log_to_stdio) fflush(stdout);

  return;
}

/******************************************************************************
 *				RTS_FORMAT_DATE_TIME
 *
 *	Formats the current date and time for a log message.  Only used by
 * routines in this module (but declared globally).
 * History:
 * 6-15-1998	T. Nguyen	Modified for Y2K
 * 8-03-1998	T. Nguyen	Replaced string length 18 with 20.
 *****************************************************************************/

void	rts_format_date_time(
  char	*buffer)
{ time_t	tp;
  struct tm	*d_t;

  time(&tp);

  if (tp == -1)
     if (JulianFormat) strcpy(buffer,"YYYY-DDDTHH:MM:SS");
     else  strcpy(buffer,"YYYY-MM-DDTHH:MM:SS");
  else
  { if (UTC_Format)
    { d_t = gmtime(&tp);
      if (!d_t)
      { UTC_Format = FALSE;
        d_t = localtime(&tp);
      }
    } else d_t = localtime(&tp);
    if (JulianFormat) strftime(buffer,20,"%Y-%jT%H:%M:%S",d_t);
    else strftime(buffer,20,"%Y-%m-%dT%H:%M:%S",d_t);
    if (UTC_Format) strcat(buffer,"Z");
  }

  return;
}

/******************************************************************************
 *				RTS_LOG_TO_FILE
 *
 *	This routine receives a filename that will cause all logging to be
 *  sent to the defined file.  The file will be opened in an append mode.
 *  If a NULL popinter is received, any open file is closed and obviously
 *  logging to the file is stopped.
 *****************************************************************************/
int	rts_log_to_file(
  char	*FileName)
{ static int	FileOpen = FALSE;

  if (FileName)
  { if (FileOpen) return (RTN_NORMAL);
    LogFile = fopen(FileName,"a");
    if (LogFile)
    { FileOpen = TRUE;
      log_to_file = TRUE;
    } else
    { sprintf(LogMsgBuf,"Error opening Logging file: %s",FileName);
      basic_logger(0, "LOG","SFL",LogMsgBuf);
      if (rts_log_severity & RTS_LOG_TRACE_ERR)
      { sprintf(LogMsgBuf,"%s (%d): %s",FileName,errno,strerror(errno));
        basic_logger(0, "LOG","SFL",LogMsgBuf);
      }
      return (RTN_OPEN_ERROR);
    }
  } else if (FileOpen)
  { fclose(LogFile);
    FileOpen = FALSE;
    log_to_file = FALSE;
  }

  return (RTN_NORMAL);
}

/******************************************************************************
 *				RTS_LOG_TO_LOGGER
 *
 *	This routine set/reset a flag which will cause all logging to be sent
 *  to the the RT_LOGGER task.
 *****************************************************************************/
void    rts_log_to_logger (
  Uint  set_reset)
{
  log_to_logger = set_reset;

  return;
}

/******************************************************************************
 *				RTS_LOG_TO_STDIO
 *
 *	This routine set/reset a flag which will cause all logging to be sent
 *  to the standard I/O (usually the terminal).
 *****************************************************************************/
void	rts_log_to_stdio (
  Uint	set_reset)
{
  log_to_stdio = set_reset;

  return;
}

/******************************************************************************
 *				RTS_LOG_TO_VICAR
 *
 *	This routine set/reset a flag which will cause all logging to be sent
 *  using the standard VICAR I/O (zvmessage), and without a date/time stamp.
 *****************************************************************************/
void	rts_log_to_vicar (
  Uint	set_reset)
{
  log_to_vicar = set_reset;

  return;
}

/******************************************************************************
 *				RTS_SET_UTC_FORMAT
 *
 *	This routine sets/resets a flag which will cause all time-stamps to
 * use the UTC time instead of the local time.  The default is local time.
 *****************************************************************************/
void    rts_set_UTC_format(
  Uint	set_reset)
{

  UTC_Format = set_reset;

  return;
}

/******************************************************************************
 *				RTS_SET_JULIAN_FORMAT
 *
 *	This routine set/reset a flag which will cause all time-stamps to
 *  use the Julian date format.  The default is Geogorian.
 *****************************************************************************/
void	rts_set_julian_format(
  Uint	set_reset)
{

  JulianFormat = set_reset;

  return;
}

/******************************************************************************
 *				RTS_SET_LOGMASKS
 *
 *	This routine takes an ASCII string containing the value from a
 * command packet and sets the log masks for the RTS, GLL, M94, & MPF based
 * on the values in the string.
 *****************************************************************************/
void	rts_set_logmasks(
  char	*value)
{ int	idx;

  /***  Initialize the log mask values  ***/
  if (strstr(value,"UPDATE"))
  { rts_log_mask |= RTS_LOG_ALWAYS;
    gll_log_mask |= RTS_LOG_ALWAYS;
    m94_log_mask |= RTS_LOG_ALWAYS;
    mpf_log_mask |= RTS_LOG_ALWAYS;
    rts_log_severity |= RTS_LOG_ALWAYS;
  } else
  { rts_log_mask = RTS_LOG_ALWAYS;
    gll_log_mask = RTS_LOG_ALWAYS;
    m94_log_mask = RTS_LOG_ALWAYS;
    mpf_log_mask = RTS_LOG_ALWAYS;
    rts_log_severity = RTS_LOG_ALWAYS;
  }

  /***  Parse the value string for log masks to enable  ***/
  for (idx=0; idx<32; idx++)
  { if (rts_severity[idx] && strstr(value,rts_severity[idx]))
       rts_log_severity |= (1 << idx);

    if (rts_masks[idx] && strstr(value,rts_masks[idx]))
       rts_log_mask |= (1 << idx);

    if (gll_masks[idx] && strstr(value,gll_masks[idx]))
       gll_log_mask |= (1 << idx);

    if (m94_masks[idx] && strstr(value,m94_masks[idx]))
       m94_log_mask |= (1 << idx);

    if (mpf_masks[idx] && strstr(value,mpf_masks[idx]))
       mpf_log_mask |= (1 << idx);
  }

  /***  Search for special value strings  ***/
  if (strstr(value,"DEBUG_ALL")) rts_log_severity |= RTS_LOG_DEBUG;
  if (strstr(value,"COMBO_ALL")) rts_log_severity |= RTS_LOG_COMBO;

  if (strstr(value,"DEFAULT"))
  { rts_log_severity |= RTS_DEFAULT_SEVERITY;
    rts_log_mask |= RTS_LOG_DEFAULT_MASK;
    gll_log_mask |= GLL_LOG_DEFAULT_MASK;
    m94_log_mask |= M94_LOG_DEFAULT_MASK;
    mpf_log_mask |= MPF_LOG_DEFAULT_MASK;
  }

  return;
}

/******************************************************************************
 *				RTS_SET_PRGMLOGNAME
 *
 *	This routine takes an ASCII string containing the value from a
 * command packet and sets the log masks for the RTS, GLL, M94, & MPF based
 * on the values in the string.
 *****************************************************************************/
void	rts_set_prgmlogname(
  char	*value)
{ int	Lth;

  Lth = (strlen(value) >= (MAX_LOG_PGRM_LTH-2)) ? (MAX_LOG_PGRM_LTH-2) : 
                                                  strlen(value);
  memset(LogPrgmName,0,MAX_LOG_PGRM_LTH);
  if (Lth > 0) strncpy(LogPrgmName,value,Lth);

  return;
}
