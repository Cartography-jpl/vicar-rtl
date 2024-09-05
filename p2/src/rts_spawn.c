#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "rts_const_defs.h"
#include "rts_logging.h"
#include "rts_errors.h"
#include "rts_spawn.h"

static char	LogMsgBuf[256];

/*****************************************************************************
 *				RTS_MAKE_LIST
 *
 *	Allocates a list of pointers for the list.  The list pointers are
 *  all initialized to zero.
 ****************************************************************************/
int	rts_make_list(
  void	*List,
  int	Elements)
{ int	ByteSize;
  char	*Ptr;

  /***  Wanna try this
  if (*((char **)List)) rts_free_list((char **)List);
  ***/
  ByteSize = sizeof(void *) * (Elements+1);
  Ptr = malloc(ByteSize);
  if (!Ptr) return (ALLOCATE_ERROR);

  memset(Ptr,0,ByteSize);
  *((char **)List) = (void *)Ptr;

  return (RTS_NORMAL);
}

/*****************************************************************************
 *				RTS_FREE_LIST
 *
 *	Frees all of the memory associated with each list element, as well
 *  as the list itself.  The pointer to the list is then set to zero.
 ****************************************************************************/
void	rts_free_list(
  char	**List)
{ int	Idx = 0;

  if (!List) return;

  while (List[Idx]) free(List[Idx++]);

  free(List);
  *List = 0;

  return;
}

/*****************************************************************************
 *				RTS_DISPLAY_LIST
 *
 *	Displays all of the strings in the list.  This is mainly a debugging
 *  routine left as a convience.
 ****************************************************************************/
void	rts_display_list(
  char	**List)
{ int	Idx = 0;

  while (List[Idx]) printf("%s\n",List[Idx++]);

  printf("\n%d Elements Found\n",Idx);
  return;
}

/*****************************************************************************
 *				RTS_COUNT_LIST
 *
 *	Counts the number of strings in the list.  A very simple roution,
 *  but handy when just a quick number is needed.
 ****************************************************************************/
int	rts_count_list(
  char	**List)
{ int	Idx = 0;

  while (List[Idx]) Idx++;

  return Idx;
}

/*****************************************************************************
 *				RTS_COPY_LIST
 *
 *	Copies one list to another.  This routine will free any list
 *  element that is pointing somewhere other than NULL;
 ****************************************************************************/
int	rts_copy_list(
  char	**Dest,
  char	**Source)
{ int	Idx = 0;

  while (Source[Idx])
  { if (Dest[Idx]) free(Dest[Idx]);
    Dest[Idx] = malloc(strlen(Source[Idx])+1);
    if (!Dest[Idx])
    { while (Dest[--Idx])
      { free(Dest[Idx]);
        Dest[Idx] = 0;
      }
      return ALLOCATE_ERROR;
    }
    strcpy(Dest[Idx],Source[Idx]);
    Idx++;
  }

  return RTS_NORMAL;
}

/*****************************************************************************
 *				RTS_ADD_LIST_ITEM
 *
 *	Adds a list element to the end of a list.  This assumes that the
 *  can hold another element.
 ****************************************************************************/
int	rts_add_list_item(
  char	**List,
  char	*Item)
{ int	Idx = 0;

  while (List[Idx]) Idx++;
  List[Idx] = malloc(strlen(Item)+1);
  if (!List[Idx]) return (ALLOCATE_ERROR);
  strcpy(List[Idx],Item);

  return (RTS_NORMAL);
}

/*****************************************************************************
 *				RTS_INSERT_LIST_ITEM
 *
 *	Inserts a list element at a specific location in the list and
 *  shifts all the following items up by one.  This routine assumes there is
 *  room in the list to hold another element.
 ****************************************************************************/
int	rts_insert_list_item(
  char	**List,
  int	Offset,
  char	*Item)
{ int	Idx = 0;
  char	*Ptr;

  while (List[Idx]) Idx++;
  if (Offset > Idx) return (RTS_ERROR);

  Ptr = malloc(strlen(Item)+1);
  if (!Ptr) return (ALLOCATE_ERROR);
  strcpy(Ptr,Item);

  for (; Idx>Offset; Idx--) List[Idx] = List[Idx-1];
  List[Idx] = Ptr;

  return (RTS_NORMAL);
}

/*****************************************************************************
 *				RTS_PARSE_ARGV_LIST
 *
 *	Adds a list element to the end of a list.  This assumes that the
 *  can hold another element.
 ****************************************************************************/
int	rts_parse_argv_list(
  char	**List,
  char	*Item)
{ int	Idx = 0,
	InQuote = FALSE,
	status;
  char	CmndLine[512],
	*Ptr = CmndLine;

  if (strlen(Item) >= sizeof(CmndLine)) return (RTS_ERROR);
  strcpy(CmndLine,Item);

  while (Idx < (int)strlen(Ptr))
  { if (Ptr[Idx] == '"') InQuote = !InQuote;
    if (!InQuote && (Ptr[Idx] == ' ' || Ptr[Idx] == '\t'))
    { Ptr[Idx] = 0;
      status = rts_add_list_item(List,Ptr);
      if (RTS_RTN_BAD(status)) return status;
      Ptr += Idx + 1;
      Idx = 0;
      InQuote = FALSE;
    } else Idx++;
  }

  status = rts_add_list_item(List,Ptr);
  if (RTS_RTN_BAD(status)) return status;

  return (RTS_NORMAL);
}

/*****************************************************************************
 *				RTS_REPLACE_ENV_ITEM
 *
 *	Replaces an evironment variable in an environment list.  It assumes
 *  that the environment list element has the structure:
 *		<Environment Variable>=<Value>
 *  It looks for a match to the entire Variable name, including the '=',
 *  and replaces the entire element.
 ****************************************************************************/
int	rts_replace_env_item(
  char	**List,
  char	*Item)
{ int	Idx = 0,
	TargetLth;
  char	*Ptr;

  TargetLth = strcspn(Item,"=");
  if (TargetLth < (int)strlen(Item)) TargetLth++;

  while (List[Idx])
        if (strncmp(List[Idx],Item,TargetLth) == 0)
  { Ptr = malloc(strlen(Item)+1);
    if (!Ptr) return (ALLOCATE_ERROR);
    strcpy(Ptr,Item);

    free(List[Idx]);
    List[Idx] = Ptr;
    break;
  } else Idx++;

  Ptr = malloc(strlen(Item)+1);
  if (!Ptr) return (ALLOCATE_ERROR);
  strcpy(Ptr,Item);
  List[Idx] = Ptr;

  return (RTS_NORMAL);
}

/*****************************************************************************
 *				RTS_DELETE_LIST_ITEM
 *
 *	Deletes all list elements that start with the same string as passed.
 *  This routine was intended for evnrionment variables, but can be used in
 *  any list.
 ****************************************************************************/
void	rts_delete_list_item(
  char	**List,
  char	*Item)
{ int	Idx = 0,
	Temp;

  while (List[Idx])
        if (strncmp(List[Idx],Item,strlen(Item)) == 0)
  { Temp = Idx;
    while (List[Temp] = List[Temp+1]) Temp++;
  } else Idx++;

  return;
}

#define  MODULE_NAME		"RST"
/******************************************************************************
 *				RTS_SPAWN_TASK
 *
 *	Forks and Execs a new tasks.  This routine will set-up a two-way
 *  communication pipe between the parent and child as specified.
 *****************************************************************************/

int	rts_spawn_task(
  rts_spawn_typ	*Child)
{ int	idx,
	status,
	taskid;
  char	PipeId[12];
  int	P2C_Pipe[2],
	C2P_Pipe[2];

  /* Create Pipe */
  if ((Child->CreateReadPipe || Child->CreateWritePipe) &&
      (pipe(P2C_Pipe) < 0 || pipe(C2P_Pipe) < 0))
  { sprintf(LogMsgBuf,"Pipe create error for %s: %s",
            Child->Argv[0],strerror(errno));
    rts_logger(RTS_LOG_IPC,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    return (RTS_ERROR);
  }

  /* create subprocess */
  taskid = fork();
  if (taskid < 0)
  { sprintf(LogMsgBuf,"Fork error for %s: %s",
            Child->Argv[0],strerror(errno));
    rts_logger(RTS_LOG_IPC,RTS_LOG_FATAL,MODULE_NAME,LogMsgBuf);
    return (RTS_ERROR);
  }

  if (taskid)
  { Child->TaskId = taskid;

    if (Child->CreateReadPipe)		/* Parent Pipe is Write */
    { Child->WritePipe = P2C_Pipe[1];
    } else close(P2C_Pipe[1]);
    close(P2C_Pipe[0]);

    if (Child->CreateWritePipe)		/* Parent Pipe is Read */
    { Child->ReadPipe = C2P_Pipe[0];
    } else close(C2P_Pipe[0]);
    close(C2P_Pipe[1]);

    return (RTS_NORMAL);		/* Parent Done ... Back to work */
  } else
  { 
    if (Child->CreateReadPipe)
    { Child->ReadPipe = P2C_Pipe[0];
      if (Child->MapReadStdin)
      { if (dup2(Child->ReadPipe,STDIN_FILENO) != STDIN_FILENO)
        { sprintf(LogMsgBuf,"Could not map pipe to STDIN: %s",
                  strerror(errno));
          rts_logger(RTS_LOG_IPC,RTS_LOG_FATAL,MODULE_NAME,LogMsgBuf);
          exit (ALLOCATE_ERROR);
        }
        close(Child->ReadPipe);
      } else if (Child->MapReadArgv1)
      { sprintf(PipeId,"%10d",Child->ReadPipe);
        status = rts_insert_list_item(Child->Argv,1,PipeId);
        if (RTS_RTN_BAD(status))
        { rts_logger(RTS_LOG_IPC,RTS_LOG_FATAL,MODULE_NAME,
                     "Could not insert READ pipe into ARGV[1]");
          exit(status);
        }
      }
    } else close(P2C_Pipe[0]);
    close(P2C_Pipe[1]);

    if (Child->CreateWritePipe)
    { Child->WritePipe = C2P_Pipe[1];
      if (Child->MapWriteStdout)
      { if (dup2(Child->WritePipe,STDOUT_FILENO) != STDOUT_FILENO)
        { sprintf(LogMsgBuf,"Could not map pipe to STDOUT: %s",
                  strerror(errno));
          rts_logger(RTS_LOG_IPC,RTS_LOG_FATAL,MODULE_NAME,LogMsgBuf);
          exit (ALLOCATE_ERROR);
        }
        close(Child->WritePipe);
      } else if (Child->MapWriteArgv2)
      { sprintf(PipeId,"%10d",Child->WritePipe);
        status = rts_insert_list_item(Child->Argv,2,PipeId);
        if (RTS_RTN_BAD(status))
        { rts_logger(RTS_LOG_IPC,RTS_LOG_FATAL,MODULE_NAME,
                     "Could not insert WRITE pipe into ARGV[2]");
          exit(status);
        }
      }
    } else close(C2P_Pipe[1]);
    close(C2P_Pipe[0]);
  }

  if (Child->Environ)
  { status = execve(Child->Program,Child->Argv,Child->Environ);
    sprintf(LogMsgBuf,"Exec-ve error (%d) for %s: %s",
            errno,Child->Argv[0],strerror(errno));
  } else
  { status = execv(Child->Program,Child->Argv);
    sprintf(LogMsgBuf,"Exec-v error (%d) for %s: %s",
            errno,Child->Argv[0],strerror(errno));
  }
  mpf_logger(RTS_LOG_IPC,RTS_LOG_FATAL,MODULE_NAME,LogMsgBuf);

  exit(status);

  return(status);	/* Only for compiler warning */
}

