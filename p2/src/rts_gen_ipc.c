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
