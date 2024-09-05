$!****************************************************************************
$!
$! Build proc for MIPL module rts_util_routines
$! VPACK Version 1.9, Monday, April 02, 2001, 09:37:02
$!
$! Execute by entering:		$ @rts_util_routines
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
$ write sys$output "*** module rts_util_routines ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to rts_util_routines.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$   if F$SEARCH("rts_util_routines.imake") .nes. ""
$   then
$      vimake rts_util_routines
$      purge rts_util_routines.bld
$   else
$      if F$SEARCH("rts_util_routines.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rts_util_routines
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rts_util_routines.bld "STD"
$   else
$      @rts_util_routines.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rts_util_routines.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rts_util_routines.com -
	-s rts_logging.c rts_link_buffers.c rts_gen_ipc.c rts_time_util.c -
	-i rts_util_routines.imake -
	-t tst_rts_logging.c tst_rts_link_buffers.c tst_rts_time.c -
	   tst_rts_logging.imake tst_rts_link_buffers.imake -
	   tst_rts_time.imake readme_rts_util.tst
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rts_logging.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_link_buffers.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/***
#include "rts_errors.h"
 **/
#include "rts_typedefs.h"
#include "rts_link_buffers.h"

/***  Function prototypes  ***/
static	link_buf_typ	*initialize_buffer_link( void );

/*******************************************************************************
 *				APPEND_BUFFER_LINK
 *
 *	Creates and appends a link after the passed buffer link.  It returns
 *  NULL if it could not allocate the memory for the link.  The forward and
 *  backward links are assigned, and the previous and next buffers' links are
 *  also modified.  The data pointer is set to NULL.  This routine will create
 *  the first link of a linked buffer if a NULL link is passed in.  For this
 *  first call, the pointer to the link MUST be a NULL.
 ******************************************************************************/
link_buf_typ	*append_buffer_link(
  link_buf_typ	*current)
{ link_buf_typ	*new;

  if (current == NULL)
     return(initialize_buffer_link());

  new = (link_buf_typ *)malloc(sizeof(link_buf_typ));
  if (new == NULL) return (NULL);

  memset((void *)new,0,sizeof(link_buf_typ));
  new->previous = current;
  new->next = current->next;
  ((link_buf_typ *)current->next)->previous = new;
  current->next = new;
  
  return (new);
}

/*******************************************************************************
 *				DELETE_BUFFER_LINK
 *
 *	Removes a link and frees the memory associated with the link, INCLUDING
 *  the data portion of the link.  It returns the link previous to the link
 *  deleted.  If the last link is deleted, then NULL is returned.
 ******************************************************************************/
link_buf_typ	*delete_buffer_link(
  link_buf_typ	*link)
{ void	*temp;

  if (link == NULL) return (NULL);

  ((link_buf_typ *)link->previous)->next = link->next;
  ((link_buf_typ *)link->next)->previous = link->previous;
  temp = link->previous;
  if ((void *)link == temp) temp = NULL;
  free(link->data);
  free((void *)link);

  return ((link_buf_typ *)temp);
}

/*******************************************************************************
 *				INITIALIZE_BUFFER_LINK
 *
 *	Defines the first buffer to point to itself for both the forward and
 *  backward links.  It returns NULL if it could not allocate the memory for
 *  the link, otheriwse it returns the address of the link.  This routine is
 *  only used by the INSERT_BUFFER_LINK, no application program should need
 *  to call this routine (hence, the static declaration).
 ******************************************************************************/
static link_buf_typ	*initialize_buffer_link( void )
{ link_buf_typ	*first;

  first = (link_buf_typ *)malloc(sizeof(link_buf_typ));
  if (first == NULL) return (NULL);

  memset((void *)first,0,sizeof(link_buf_typ));
  first->next = (void *)first;
  first->previous = (void *)first;

  return (first);
}

/*******************************************************************************
 *				INSERT_BUFFER_LINK
 *
 *	Creates and inserts a link before the passed buffer link.  It returns
 *  NULL if it could not allocate the memory for the link.  The forward and
 *  backward links are assigned, and the previous and next buffers' links are
 *  also modified.  The data pointer is set to NULL.  This routine will create
 *  the first link of a linked buffer if a NULL link is passed in.  For this
 *  first call, the pointer to the link MUST be a NULL.
 ******************************************************************************/
link_buf_typ	*insert_buffer_link(
  link_buf_typ	*current)
{ link_buf_typ	*new;

  if (current == NULL)
     return(initialize_buffer_link());

  new = (link_buf_typ *)malloc(sizeof(link_buf_typ));
  if (new == NULL) return (NULL);

  memset((void *)new,0,sizeof(link_buf_typ));
  new->previous = current->previous;
  new->next = current;
  ((link_buf_typ *)current->previous)->next = new;
  current->previous = new;

  return (new);
}

/*******************************************************************************
 *				PRINT_LINKED_LIST
 *
 *	Prints the data contents of a linked list structure.  Assumes the data
 *  is of type char *.
 ******************************************************************************/
void	print_linked_list (
  link_buf_typ	*List,
  long		Options )
{ int	LinkCount = 0;
  char	PrefixBuffer[32] = "",
	AddressBuffer[32] = "",
	Buffer[256] = "",
	*Ptr = "";
  link_buf_typ	*LocalLink = List;

  if (!List)
  { printf("No List Present\n");
    return;
  }

  do
  { if (Options & RLB_INDEX_PREFIX) sprintf(PrefixBuffer,"%4d: ",++LinkCount);
    if (Options & (RLB_ADDRESS_ALSO | RLB_ADDRESS_ONLY))
       sprintf(AddressBuffer,"(%08X) ",(int)LocalLink->data);
    if (!(Options & RLB_ADDRESS_ONLY))
    { Ptr = LocalLink->data;
      if (!Ptr) Ptr = "(null)";
    }
    printf("%s%s%s\n",PrefixBuffer,AddressBuffer,Ptr);
    LocalLink = LocalLink->next;
  } while (LocalLink != List);

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_gen_ipc.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>

#include "rts_const_defs.h"
#include "rts_logging.h"
#include "rts_typedefs.h"
#include "rts_ipc_defs.h"

#define  MAX_SIGNALS	64
typedef struct	{
		int	Type;
		char	*Message;
		Flag	ExitAction;
		} SignalInfo_typ;
static SignalInfo_typ	Signal[MAX_SIGNALS];
static int	Trapped = 0;
static char	ProgramId[128];

/******************************************************************************
 *				RTS_DISPLAY_IMAGE
 *
 ******************************************************************************/
int	rts_display_image(
  int	project,
  int	program,
  char	*image,
  char	*keyword)
{ char	cmnd[512];

  sprintf(cmnd,"%s=%s", keyword, image);
  return (rts_send_cmnd( project, program, cmnd));
}

/******************************************************************************
 *				RTS_UTC_STRING
 *
 ******************************************************************************/
char	*rts_utc_string(
  time_t	*Requested)
{ time_t	Current;
  static char	UTC_string[128];
  struct tm	*UTC;

  if (*Requested) memcpy(&Current,Requested,sizeof(time_t));
  else time(&Current);

  UTC = gmtime(&Current);
  if (UTC)
     strftime(UTC_string,sizeof(UTC_string),"%Y-%m-%dT%H:%M:%S.000",UTC);
  else strcpy(UTC_string,"");

  return UTC_string;
}

/******************************************************************************
 *				RTS_EXIT_HANDLER
 *
 ******************************************************************************/
static void	rts_exit_handler(
  int	SignalType)
{ int	idx;
  char  LogMsg[512];

  for (idx=0; idx<Trapped; idx++)
      if (Signal[idx].Type == SignalType)
  { sprintf(LogMsg,"%s trapped error (%d): %s",
            ProgramId,SignalType,Signal[idx].Message);
    rts_logger(RTS_LOG_ALWAYS,RTS_LOG_ALWAYS,"EH",LogMsg);
    if (Signal[idx].ExitAction) exit(SignalType);
    break;
  } 

  return;
}

/***************************************************************************** 
 *				RTS_SET_EXIT_HANDLER
 *
 ******************************************************************************/
int	rts_set_exit_handler(
  char	*ProgramName,
  int	SignalType,
  char	*Message,
  int	ForceExit)
{
  if (Trapped >= MAX_SIGNALS) return (TRUE);
  strncpy(ProgramId,ProgramName,sizeof(ProgramId)-1);

  memset(&Signal[Trapped],0,sizeof(SignalInfo_typ));
  Signal[Trapped].Type = SignalType;
  if (Message)
  { Signal[Trapped].Message = malloc(strlen(Message)+1);
    if (Signal[Trapped].Message) strcpy(Signal[Trapped].Message,Message);
  }
  Signal[Trapped].ExitAction = ForceExit;

  signal( SignalType, &rts_exit_handler );

  Trapped++;
  return FALSE;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_time_util.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>

#include "rts_typedefs.h"
#include "rts_time.h"
#include "return_status.h"

/* SFOC Epoch: 1-1-58 @ 00:00:00.000 */
#define  SFOC_EPOCH_YEAR	1958
#define  MS_PER_DAY		86400000
#define  INVLD_DATE_TIME	"1900-01-01T00:00:00.000"

/*****************************************************************************
 *                              RTS_UTC_TIME
 *
 *
 *	Determines the UTC time for the current host.  This relies on the 
 * time capability of the host.  A failure returns "1900-01-01T00:00:00
 ****************************************************************************/

char	*rts_utc_time(void)
{ time_t        Now;
  struct tm     *UtcNow;
  static char	UtcTime[256];

  strcpy(UtcTime,INVLD_DATE_TIME);

  if (time(&Now) == -1) return (UtcTime);

  UtcNow = gmtime(&Now);
  if (UtcNow == NULL) return (UtcTime);

  strftime(UtcTime,sizeof(UtcTime),"%Y-%m-%dT%H:%M:%S.000",UtcNow);

  return (UtcTime);
}

/*****************************************************************************
 *				CompareSfocTime	
 *
 *	Compares two SFOC times and returns -1, 0 or 1 for less than, equal,
 *  or greater than.
 ****************************************************************************/
int	CompareSfocTime(
  SfocTime_typ	*Upper,
  SfocTime_typ	*Test)
{
  if (Upper->Days > Test->Days) return 1;
  if (Upper->Days < Test->Days) return -1;
  if (Upper->MilliSeconds > Test->MilliSeconds) return 1;
  if (Upper->MilliSeconds < Test->MilliSeconds) return -1;

  return 0;
}

/*****************************************************************************
 *				ExtractSfocTimeBuffer	
 *
 *	Extracts the data from a 6-byte buffer and places the values into
 *  integer fields for processing.
 ****************************************************************************/
int	ExtractSfocTimeBuffer(
  SfocTime_typ	*Time)
{

  Time->Days = (Time->Buffer[0] * 256) + Time->Buffer[1];
  Time->MilliSeconds = (long)(Time->Buffer[2] * 16777216) +
                       (long)(Time->Buffer[3] * 65536) +
                       (long)(Time->Buffer[4] * 256) + (long)Time->Buffer[5];

  if (Time->Days < 0 || Time->MilliSeconds < 0 ||
      Time->MilliSeconds >= MS_PER_DAY)
     return RTN_INVLD_ARG_IGNORED;

  return RTN_NORMAL;
}

/*****************************************************************************
 *				PackSfocTimeBuffer	
 *
 *	Packs the integer values of the Sfoc Time into a 6-byte buffer for
 *  compatibility with SFOC usage.
 ****************************************************************************/
int	PackSfocTimeBuffer(
  SfocTime_typ	*Time)
{ Uword	TempShort;

  if (Time->Days > 0xFFFF)
  { memset(Time->Buffer,0,6);
    return RTN_INVLD_ARG;
  }

  TempShort = Time->Days;
  memcpy(Time->Buffer,&TempShort,2);
  memcpy(&Time->Buffer[2],&Time->MilliSeconds,4);

  return RTN_NORMAL;
}

/*****************************************************************************
 *				MaxSfocTime	
 *
 *	Determines which is the maximum SFOC time value and places the value
 *  in the first parameter/
 ****************************************************************************/
void	MaxSfocTime(
  SfocTime_typ	*Upper,
  SfocTime_typ	*Test)
{

  if (Upper->Days > Test->Days) return;
  if (Upper->Days == Test->Days &&
      Upper->MilliSeconds >= Test->MilliSeconds) return;

  memcpy(Upper,Test,sizeof(SfocTime_typ));

  return;
}

/*****************************************************************************
 *				MinSfocTime	
 *
 *	Determines which is the minimum SFOC time value and places the value
 *  in the first parameter.
 ****************************************************************************/
void	MinSfocTime(
  SfocTime_typ	*Lower,
  SfocTime_typ	*Test)
{

  if (Lower->Days < Test->Days) return;
  if (Lower->Days == Test->Days &&
      Lower->MilliSeconds <= Test->MilliSeconds) return;

  memcpy(Lower,Test,sizeof(SfocTime_typ));

  return;
}

/*****************************************************************************
 *				SfocTimeToAscii	
 *
 *	Converts a SFOC time to a PDS complient time string
 * History:
 * 6-15-1998	T. Nguyen	For Y2K task, modified to use zchk_leap().
 * 1-15-2000	A. Runkle	Removed zchk_leap ... not correct for Year 2100
 ****************************************************************************/
char	*SfocTimeToAscii(
  SfocTime_typ	*Time,
  int		Julian)
{ int	LeapYear,	/* Leap Year flag */
	LeapDays,	/* Number of Leap Days from Epoch to current year */
	Year,
	Month,
	Day,		/* Day of year/month (as required) */
	TDays = Time->Days,	/* Total Days (from SFOC construct) */
	Hour,
	Minute,
	Second,
	Milli;
  Byte	dom[12] = {31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30,  31};

  static char	Buffer[64];

  Year = TDays / 365 - 1;	/* '-1' needed in calculating 'LeapDays' */
  LeapDays = (Year+2) / 4 -	/* Added 2; 1958 is the middle of leap cycle */
             (((Year+SFOC_EPOCH_YEAR)/100) -        /* 15 non-leap centuries */
              ((Year+SFOC_EPOCH_YEAR)/400) - 15);   /* until Epoch of 1958   */
  Day = (TDays % 365) - LeapDays;
  Year += SFOC_EPOCH_YEAR + 1;
  while (Day < 0)
  { Year--;
    if (((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0)))
       Day+=366;
    else Day += 365;
  }

  Day++;        /* Added 1 to Day because it is a zero-offset number used */
                /* in a one-offset calculation */

  Hour = Time->MilliSeconds / (1000 * 60 * 60);
  Minute = (Time->MilliSeconds / (1000 * 60)) % 60;
  Second = (Time->MilliSeconds / 1000) % 60;
  Milli = Time->MilliSeconds % 1000;

  LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));

  if (Julian)
     sprintf(Buffer,"%04d-%03dT%02d:%02d:%02d.%03d",
             Year,Day,Hour,Minute,Second,Milli);
  else
  { if (LeapYear) dom[1]++;		/*  Add day in Feb. for leap year  */
    for (Month = 0; Month<12 && Day>dom[Month]; Month++) Day -= dom[Month];
    Month++;
    sprintf(Buffer,"%04d-%02d-%02dT%02d:%02d:%02d.%03d",
            Year,Month,Day,Hour,Minute,Second,Milli);
  }

  return (Buffer);
}


/*****************************************************************************
 *				AsciiToSfocTime
 *
 *	Converts a PDS complient time string to a SFOC time
 * History:
 * 6-15-1998	T. Nguyen	For Y2K task, modified to use zchk_leap().
 * 1-15-2000	A. Runkle	Removed zchk_leap ... not correct for Year 2100
 ****************************************************************************/
SfocTime_typ	*AsciiToSfocTime(
  char	*Time,
  int	Julian)
{ int	LeapYear,
	LeapDays,
	Year = SFOC_EPOCH_YEAR,
	Month = 1,
	Day = 0,
	Hour = 0,
	Minute = 0,
	Second = 0,
	Milli = 0;
  int	dts[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
  static SfocTime_typ	SfocTime = {0,0};

  if (Julian)
  { sscanf(Time,"%4d-%3dT%2d:%2d:%2d.%3d",
           &Year,&Day,&Hour,&Minute,&Second,&Milli);
  } else
  { sscanf(Time,"%4d-%2d-%2dT%2d:%2d:%2d.%3d",
           &Year,&Month,&Day,&Hour,&Minute,&Second,&Milli);
  }
  if (Year < 1958) return (&SfocTime);

  LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));

  Year--;				/*  Everything is zero-based here  */
  Month--;				/*  Everything is zero-based here  */
  Day--;				/*  Everything is zero-based here  */

  Day += dts[Month];
  if (LeapYear && Month > 1) Day++;

  LeapDays = (Year-SFOC_EPOCH_YEAR+2) / 4 - ((Year/100) - (Year/400) - 15);
  Year -= (SFOC_EPOCH_YEAR - 1);
  Day += (Year * 365) + LeapDays;
  Milli += (((Hour * 60 + Minute) * 60) + Second) * 1000;

  /* printf ("Time in days: %d\n", Day); */

  SfocTime.Days = Day;
  SfocTime.MilliSeconds = Milli;
  PackSfocTimeBuffer( &SfocTime );

  return (&SfocTime);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rts_util_routines.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE rts_util_routines
/*
/*   To Create the build file give the command:
/*
/*		$ vimake rts_util_routines			(VMS)
/*   or
/*		% vimake rts_util_routines			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE rts_util_routines		/* Only one of these */
/*#define PROGRAM rts_util_routines		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST	rts_logging.c rts_link_buffers.c rts_gen_ipc.c \
			rts_time_util.c

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

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
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
/**********  End of rts_util_routines imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_rts_logging.c
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "return_status.h"
#include "rts_logging.h"
#include "rts_log_masks.h"
#include "rts_const_defs.h"

int	LogRoutine(int, int, char *);
main()
{
  rts_log_mask = ~0;
  rts_log_severity = ~0;
  gll_log_mask = ~0;
  mpf_log_mask = ~0;
  rts_log_to_stdio( TRUE );
  rts_log_to_file("tst_rts_logging.log");
  rts_set_prgmlogname("Test_Log");

  rts_logger(1,RTS_LOG_TEXT,"","");
  rts_logger(1,RTS_LOG_TEXT,"","Double messages; testing 'log_to_routine'");
  rts_log_to_routine(&LogRoutine);
  rts_logger(1,RTS_LOG_ALWAYS,"TAG","Tag check");
  rts_logger(1,RTS_LOG_ALWAYS,"TEST","Testing Fully Qualified Log Messages");
  rts_logger(1,RTS_LOG_INFO,"main","Testing 'NULL' log_program name");
  gll_logger(5,RTS_LOG_FATAL,"Catalog","Could not read a thing!");
  rts_log_to_routine(0);

  rts_logger(1,RTS_LOG_TEXT,"","");
  rts_set_prgmlogname("New_Name");
  rts_logger(1,RTS_LOG_INFO,"TEST","Testing routine set 'log_program name'");
  mpf_logger(5,RTS_LOG_FATAL,"TLM","Just crusin'");
  mpf_logger(5,RTS_LOG_FATAL,"","Testing No Module Name");
  anon_logger(1,RTS_LOG_INFO,"Anon","Testing 'no subsystem' logging");
  anon_logger(1,RTS_LOG_INFO,"","Testing 'no subsystem or module logging");

  rts_set_julian_format ( TRUE );

  rts_logger(1,RTS_LOG_TEXT,"","");
  rts_set_prgmlogname("");			/* No Program Name */
  rts_logger(1,RTS_LOG_INFO,"TEST","Testing no program name");
  mpf_logger(5,RTS_LOG_FATAL,"main","Just crusin'");
  anon_logger(1,RTS_LOG_INFO,"Anon","Only a Module Name (not really useful)");
  anon_logger(1,RTS_LOG_INFO, 0,"Testing just a time tag log message");
  rts_log_to_file("tst_rts_logging.log");

  return(RTN_NORMAL);
}

int	LogRoutine(
  int	Number1,
  int	Number2,
  char	*Message)
{
  printf("Log_2_Routine: %d/%d: %s\n",Number1, Number2, Message);

  return 1;
}
$!-----------------------------------------------------------------------------
$ create tst_rts_link_buffers.c
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include "rts_link_buffers.h"
#include "rts_logging.h"
#include "rts_log_masks.h"
#include "rts_const_defs.h"

char	*rts_log_prgm = "Link_Test";

void	print_buffer_link(
  link_buf_typ	*link)
{
 if (!link)
 { printf("  Buffer is empty\n");
   return;
 }
 printf("  Buffer: %08X   Previous: %08X  Subsequent: %08X\n",
        link,link->previous,link->next);
}

main()
{ link_buf_typ	*buffer = NULL,
		*current,
		*older;

  rts_log_mask = 0xFFFFFFFF;
  gll_log_mask = 0xFFFFFFFF;
  rts_log_severity = 0xFFFFFFFF;
  rts_log_to_stdio( TRUE );

printf("\nStarting buffer linking\n\n");

printf("Initial buffer address: %08x\n",buffer);
  print_buffer_link(buffer);
  buffer = insert_buffer_link(NULL);
printf("\nCreating first buffer (reciprocally linked):\n");
  print_buffer_link(buffer);

printf("\nAdding two buffers to linked list:\n");
  current = insert_buffer_link(buffer);
printf("  Inserted buffer %08X before %08X\n",current,buffer);
  older = current;
  current = insert_buffer_link(older);
printf("  Inserted buffer %08X before %08X\n",current,older);
  print_buffer_link(buffer);
  print_buffer_link((link_buf_typ *)buffer->previous);
  print_buffer_link((link_buf_typ *)buffer->next);

print_linked_list(buffer,(RLB_INDEX_PREFIX | RLB_ADDRESS_ALSO));

printf("\nDeleting buffer at address: %08X\n",buffer);
  current = delete_buffer_link(buffer);
  print_buffer_link(current);
  print_buffer_link((link_buf_typ *)current->previous);

printf("\nDeleting buffer at address: %08X\n",current);
  current = delete_buffer_link(current);
  print_buffer_link(current);

printf("\nDeleting buffer at address: %08X\n",current);
  current = delete_buffer_link(current);
  print_buffer_link(current);

printf("\nDeleting buffer at address: %08X\n",current);
  current = delete_buffer_link(current);
  print_buffer_link(current);
}

$!-----------------------------------------------------------------------------
$ create tst_rts_time.c
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/**  History:                                                           **/
/**  6-16-1998  T. Nguyen  For Y2K task,  added test cases for testing  **/
/**                        time routines.                               **/
/*************************************************************************/
#include "rts_time.h"
#include "rts_log_masks.h"
#include "rts_const_defs.h"

void test_dates(char *);

main()
{ int	lc,
	Tdays,
	Ydays,
	Year,
	LeapYear; 
  char	utc[24];
  SfocTime_typ	PlayThing;

  printf("***\n***  Specific Date Checks\n***\n\n");

  strcpy(utc,rts_utc_time());
  if (strncmp(utc,"1900",4) == 0)
     printf ("Current UTC check failed; if this is VMS, this is normal\n");
  else
  { printf("\n Current UTC: %s \n",utc);
    test_dates(utc);
  }

  strcpy(utc,"2000-01-01T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2000-02-28T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2000-02-29T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2000-03-01T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2000-12-30T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2000-12-31T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2009-12-31T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2009-12-30T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2100-12-31T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2101-01-01T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);

  strcpy(utc,"2101-01-02T00:00:00.000");
  printf("\n Testing UTC: %s \n",utc);
  test_dates(utc);



  printf("\n***\n***  Iterative Inverse Checks\n***\n\n");

  strcpy(utc,"1958-01-01T00:00:00.000");
  printf("Beginning Date: %s\n",utc);

  memcpy(&PlayThing,AsciiToSfocTime(utc,0),sizeof(SfocTime_typ));
  if (PlayThing.Days != 0 || PlayThing.MilliSeconds != 0)
     printf("EPOCH check failed: %s  (%d, %d)\n",
            utc,PlayThing.Days,PlayThing.MilliSeconds);

  Year = 1958;
  Ydays = 0;
  Tdays = 0;
  do
  { LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
    if (LeapYear) Ydays = 366;
    else Ydays = 365;
    for (lc = 0; lc<Ydays; lc++)
    { strcpy(utc,SfocTimeToAscii(&PlayThing,0));
      memcpy(&PlayThing,AsciiToSfocTime(utc,0),sizeof(SfocTime_typ));
      if (strcmp(utc,SfocTimeToAscii(&PlayThing,0)) != 0)
         printf("Inverse Failure: %s -> %d -> %s\n",
                utc,PlayThing.Days,SfocTimeToAscii(&PlayThing,0));
      PlayThing.Days++;
      Tdays++;
    }
    Year++;
  } while (Year < 2041);
  strcpy(utc,SfocTimeToAscii(&PlayThing,0));


  printf("\nFinal Date: %d-01-01 (%d) vs %s (%d)\n\n",
         Year,Tdays,utc,PlayThing.Days);

}

void test_dates(
  char *utcTime)
{
  int	Status;
  char		TimeBufs[2][128];
  SfocTime_typ	PlayThings[2];

  strcpy(TimeBufs[0],utcTime);
/***  printf(" Current UTC: %s\n",TimeBufs[0]);  ***/

  memcpy(&PlayThings[0],AsciiToSfocTime(TimeBufs[0],0),sizeof(SfocTime_typ));
  printf("  Julian UTC: %s    (%u, %u)\n",
         SfocTimeToAscii(&PlayThings[0],1),
         PlayThings[0].Days,PlayThings[0].MilliSeconds);

  memcpy(&PlayThings[1],&PlayThings[0],sizeof(SfocTime_typ));
  PlayThings[1].Days++;
  PackSfocTimeBuffer(&PlayThings[1]);
  printf("Tomorrow UTC: %s  (%u, %u)\n",
         SfocTimeToAscii(&PlayThings[1],0),
         PlayThings[1].Days,PlayThings[1].MilliSeconds);

  if (CompareSfocTime(&PlayThings[0],&PlayThings[1]) > 0)
     printf("Today follows Tomorrow ... wrong\n");
  else if (CompareSfocTime(&PlayThings[0],&PlayThings[1]) == 0)
     printf("Today is the same as Tomorrow ... it only seems that way\n");
  else printf("Tomorrow follows Today\n");

  return;
}



$!-----------------------------------------------------------------------------
$ create tst_rts_logging.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_rts_logging
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_rts_logging			(VMS)
/*   or
/*		% vimake tst_rts_logging			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_rts_logging		/* Only one of these */
#define PROGRAM tst_rts_logging		/* Only one of these */

#define TEST
/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_rts_logging.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_NETWORK_NOSHR
#define LIB_PVM
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
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
/**********  End of tst_rts_logging imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_rts_link_buffers.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_rts_link_buffers
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_rts_link_buffers			(VMS)
/*   or
/*		% vimake tst_rts_link_buffers			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_rts_link_buffers		/* Only one of these */
#define PROGRAM tst_rts_link_buffers		/* Only one of these */
#define TEST

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_rts_link_buffers.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_NETWORK_NOSHR
#define LIB_PVM
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
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
/**********  End of tst_rts_link_buffers imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_rts_time.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_rts_time
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_rts_time			(VMS)
/*   or
/*		% vimake tst_rts_time			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_rts_time		/* Only one of these */
#define PROGRAM tst_rts_time		/* Only one of these */

#define TEST
/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_rts_time.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL_NOSHR
#define LIB_TAE_NOSHR
#define LIB_P2SUB
#define LIB_PVM
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
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
/**********  End of tst_rts_time imake file  **********/
$!-----------------------------------------------------------------------------
$ create readme_rts_util.tst
There are three test programs which must be created and run.  These programs
are stand-alone and should not be run under VICAR.  The test programs are:
    TST_RTS_LOGGING
    TST_LINK_BUFFERS
&   TST_RTS_TIME

The output from these programs is minor.  A better verification of these
routines is running the test program for RTS_PARAM_ROUTINES, which uses
the first two modules extensively for the diagnostic messages and the
definition of the parameter list.
$ Return
$!#############################################################################
