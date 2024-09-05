/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/*
 *	rts_ipc.c
 * These are a set of subroutine call used by the Real time System
 * for communication interface
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include  "rts_ipc_defs.h"
#include  "rts_const_defs.h"
#include  "rtsipc.h"
#include  "pvm3.h"

static	int	MyProject;		/* Mission ID */
static	int	MyProgram;		/* Program ID */
static	char	MyStream[128];		/* Stream Name */
static	int	MyTaskId;		/* My PVM task ID */
static	char	*host;			/* Host machine */
static	int	DebugFlag = FALSE;	/* TRUE: enable debug messages */
static	int	MyLogFlag = FALSE;	/* TRUE: logfile name included */
static	char	MyLogFile[256];		/* Logger Disk File Name */
static	int	procid[RTS_PRJ_MAX][RTS_PRGM_MAX];
/**/

/********************************************************************
 *		rts_ipc_cleanup()
 *******************************************************************/
void	rts_ipc_cleanup( void )
{
  char	filename[256];

#if VMS
  sprintf( filename, "sys$login:pvm_%s.tmp", MyStream );
#else
  if (getenv("HOME"))
     sprintf( filename, "%s/.pvm_%s.tmp", getenv("HOME"), MyStream );
  else sprintf( filename, "/tmp/pvm_%s.tmp", MyStream );
#endif
  remove(filename);
  return;
}

/********************************************************************
 *		rts_ipc_init_parent(
 *			int	project,	Project Identifier
 *			int	program,	Program Identifier
 *			char	*stream,	Stream identifier
 *			char	*logfile)	Logfile identifier
 *******************************************************************/

int	rts_ipc_init_parent(
	int	project,
	int	program,
	char	*stream,
	char	*logfile)
{ int	status = 0;
  int	nhost,
	narch;
#if VMS
  struct  hostinfo	*hostp;
#else
  struct  pvmhostinfo	*hostp;
#endif

  memset(procid,0,sizeof(procid));

  status = MyTaskId = pvm_mytid();
  if ( status < 0 ) return ( status );

  procid[project][program] = MyTaskId;
  MyProject = project;
  MyProgram = program;
  strcpy(MyStream,stream);
  if (logfile)
  { strcpy(MyLogFile,logfile);
    MyLogFlag = strlen(logfile);
  }

  status = pvm_config( &nhost,&narch,&hostp);

  return (status);
}
                                
/********************************************************************
 *		rts_ipc_init_child()
 *******************************************************************/
int	rts_ipc_init_child( )
{ int	ptid,
	ipc_cntrl = IpcWhoAmI;
#if VMS
  int	rts_vms_xfer_logicals( int );

  rts_vms_xfer_logicals(FALSE);	/* Gets required environment variables	*/
				/* that VMS does not pass to detatched	*/
				/* processes				*/
#endif

  memset(procid,0,sizeof(procid));

  MyTaskId = pvm_mytid();
  ptid = pvm_parent();

  /***  Ask Parent for Project, Program, Stream & Logfile  ***/
  pvm_initsend( PvmDataDefault );
  pvm_pkint(&ipc_cntrl,1,1);
  pvm_send(ptid,IpcInternal);

  /***  Get Info from Parent  ***/
  pvm_recv(ptid, IpcStartCmd);
  pvm_upkint( &MyProject,1,1);
  pvm_upkint( &MyProgram,1,1);
  pvm_upkstr( MyStream );
  pvm_upkint( &MyLogFlag,1,1);
  if ( MyLogFlag ) pvm_upkstr( MyLogFile );

  procid[MyProject][MyProgram] = MyTaskId;

  return ( MyTaskId );
}

/********************************************************************
 *		rts_ipc_start(
 *			int	project,	Project Identifier
 *			int	program,	Program Identifier
 *			char	*host,		Host of destination program
 *			char	*PrgmName)	Name of program being started
 *******************************************************************/
int	rts_ipc_start(
  int	project,
  int	program,
  char	*host,
  char	*ProgramName)
{ int	count = 0,
	ipc_cntrl,
	slavetid,
	status;

  if ( host == 0 )
     status = pvm_spawn(ProgramName,(char**)0,PvmTaskDefault,0,1,&slavetid);
  else
     status = pvm_spawn(ProgramName,(char**)0,PvmTaskHost, host,1,&slavetid); 

  if ( status <= 0 ) status = slavetid;
  else
  { procid[project][program] = slavetid;
    rts_ipc_upd_task_id();

    /***  Wait for task start  ***/
    do
    { status = pvm_nrecv(slavetid,IpcInternal);
      if (!status) sleep (1);
    } while (status == 0 && ++count<IpcStartTimeOut);

    if (status > 0)
    { pvm_upkint( &ipc_cntrl,1,1);
      if (ipc_cntrl != IpcWhoAmI)
         printf("IPC Error; Received unexpected INTERNAL during spawn: %d\n",
                ipc_cntrl);
    } else if (status == 0) printf("IPC Error; Timeout waiting for spawn\n");
    else printf("IPC Error; PVM receive during spawn: %d\n",status);

    /***  Send task info, even if didn't get start-up confirmation  ***/
    pvm_initsend( PvmDataDefault );
    pvm_pkint(&project,1,1);
    pvm_pkint(&program,1,1);
    pvm_pkstr(MyStream);
    pvm_pkint(&MyLogFlag,1,1);
    if ( MyLogFlag ) pvm_pkstr( MyLogFile );
    status = pvm_send (slavetid, IpcStartCmd);
  }

  return ( status );
} 

/*****************************************************************
 *	rts_ipc_kill(						  *
 *		int	project,				  *
 *		int	program)				  *
 *								  *
 * This routine kills the existing pvm process	                  *
 *****************************************************************/

int	rts_ipc_kill(
  int	project,
  int	program)
{ int	status;

  if ( project == MyProject && program == MyProgram ) status = pvm_exit();
  else status = pvm_kill( procid[project][program] );

  return ( status );
}

/*****************************************************************
 *	rts_ipc_read(						  *
 *		int	*project,				  *
 *		int	*program,				  *
 *		char	*buf,					  *
 *		int	buflen,					  *
 *		int	timeout					  *
 *								  *	
 *	timeout < 0; 	wait indefinitely			  *
 *	timeout = 0; 	return after read immeditately		  *
 *	timeout = X; 	return after read or at most X seconds	  *
 *								  *
 *	*project = RTS_PRJ_ANY					  *
 *   or *program = RTS_PRGM_ANY, accept all data		  *
 *								  *
 * This routine reads data			                  *
 *****************************************************************/

int	rts_ipc_read(
  int	*project,
  int	*program,
  char	*buf,
  int	buflen,
  int	timeout)
{ int	BufId,
	MsgTag,
	PvmTaskId,
	ipc_cntrl,
	ipc_reply_cntrl,
	count = 0,
	proj,
	prog,
	tid,
	status,
	bufsize;
  char	stream[128];

  /***  Get PVM buffer  ***/
  if ( timeout < 0 ) status = BufId = pvm_recv( IpcAnyTask, IpcBuffer );
  else do
  { status = BufId = pvm_nrecv( IpcAnyTask, IpcBuffer ); 
    if (!status && timeout) sleep (1);
  } while (status == 0 && ++count<timeout);
  if ( status == 0 ) status = IpcNoDataRcvd;
  if ( status < 0 ) return ( status );

  status = 0;
  pvm_upkint( &ipc_cntrl,1,1);
  status = pvm_bufinfo(BufId,&bufsize,&MsgTag,&PvmTaskId);
  if (status < 0) return (IpcRcvErr);

  /***  Check for Control Options  ***/
  if (ipc_cntrl == IpcHello)
  { pvm_initsend( PvmDataDefault );
    ipc_reply_cntrl = IpcIAm;
    pvm_pkint( &ipc_reply_cntrl,1,1 );
    pvm_pkint( &MyTaskId,1,1 );
    pvm_pkint( &MyProject,1,1 );
    pvm_pkint( &MyProgram,1,1 );
    pvm_pkstr( MyStream );
    pvm_send( PvmTaskId, IpcInternal);
    return(rts_ipc_read(project,program,buf,buflen,timeout));
  }

  if (ipc_cntrl != IpcNoOption && ipc_cntrl != IpcSendAck)
  { printf("IPC Error; Invalid control option received: %d\n",ipc_cntrl);
    return(rts_ipc_read(project,program,buf,buflen,timeout));
  }

  /***  Unpack Message: Header & Buffer  ***/
  pvm_upkint( &tid,1,1);
  pvm_upkint( &proj,1,1);
  pvm_upkint( &prog,1,1);
  pvm_upkstr( stream );
  pvm_upkint( &bufsize,1,1);

  /***  Check for Error Conditions  ***/
  if ( buflen < bufsize ) status = IpcBsizeErr;
  else  pvm_upkstr( buf );
  if ( *project != RTS_PRJ_ANY && proj != *project ) status = IpcProjErr;
  if ( *program != RTS_PRGM_ANY && prog != *program ) status = IpcProgErr;
  if ( strcmp( stream, MyStream ) != 0 )	/* Wrong stream name  */
     status = IpcStrmErr;

  /***  Need to Acknowledge message ?  ***/
  if ( ipc_cntrl == IpcSendAck ) 
  { pvm_initsend( PvmDataDefault );
    pvm_pkint( &status,1,1);
    status = pvm_send( tid, IpcAckMsg );
    if ( status >= 0 ) status = 0;
  } else status = 0;

  if (proj < 0 || prog < 0) return (status );
  if (procid[proj][prog] == 0) procid[proj][prog] = tid;
  *project = proj;			/* get project id */
  *program = prog;			/* get program id */

  return ( status );
}

/*****************************************************************
 *	rts_ipc_write(						  *
 *		int	project,				  *
 *		int	program,				  *
 *		char	*buf,					  *
 *		int	buflen,					  *
 *		int	ack_flag,				  *
 *		int	Timeout)				  *
 *								  *	
 *	timeout = -1; 	wait indefinitely			  *
 *	timeout = 0; 	return after read immeditately		  *
 *	ack_flag = 0;  no ACK required				  *
 *	ack_flag = 1;  ACK required				  *
 *								  *
 * This routine writes data			                  *
 *****************************************************************/

int	rts_ipc_write(
	int	proj,
	int	prog,
	char	*buf,
	int	buflen,
	int	ack_flag,
	int	timeout)
{ int	count = 0,
	ipc_cntrl,
	status = 0;

  if ( procid[proj][prog] == 0 ) rts_ipc_get_task_id( );
  if ( procid[proj][prog] == 0 ) return( IpcInactErr );	

  /***  Build PVM Buffer: Header and Message  ***/
  ipc_cntrl = (ack_flag) ? IpcSendAck : IpcNoOption;
  pvm_initsend( PvmDataDefault );
  pvm_pkint( &ipc_cntrl,1,1);
  pvm_pkint( &MyTaskId,1,1);
  pvm_pkint( &MyProject,1,1);
  pvm_pkint( &MyProgram,1,1);
  pvm_pkstr( MyStream );
  pvm_pkint( &buflen,1,1);
  pvm_pkstr( buf );

  status = pvm_send( procid[proj][prog], IpcBuffer );
  if ( status < 0 )
  { procid[proj][prog] = 0;
    return ( status );
  }

  /***  Wait for Acknowledgement  ***/
  if ( ack_flag )
  { if ( timeout == -1 ) status = pvm_recv(procid[proj][prog], IpcAckMsg);
    else do
    { status = pvm_nrecv(procid[proj][prog],IpcAckMsg);
      if ( !status && timeout  ) sleep(1);
    } while (status == 0 && ++count<timeout);

    if (status > 0) pvm_upkint( &status,1,1 );
    else status = IpcNoAckErr;
  }

  return ( status );
}	

/********************************************************************
 *		rts_ipc_get_task_id()
 *******************************************************************/

int	rts_ipc_get_task_id( )
{ int	status,
	program,
	project,
	taskid;
  char	filename[256];
  FILE	*fd;

#if VMS
  sprintf( filename, "sys$login:pvm_%s.tmp", MyStream );
#else
  if (getenv("HOME"))
     sprintf( filename, "%s/.pvm_%s.tmp", getenv("HOME"), MyStream );
  else sprintf( filename, "/tmp/pvm_%s.tmp", MyStream );
#endif

  if ((fd = fopen(filename, "r")) == NULL)
  { fclose( fd );
    status = IpcOpenErr;
    return ( status );
  }
  
  while ((status=fscanf(fd,"%d %d %d\n",&project,&program,&taskid)) == 3)
        procid[project][program] = taskid;

  procid[MyProject][MyProgram] = MyTaskId;

  fclose( fd );

  return ( 0 );
}

/********************************************************************
 *		rts_ipc_upd_task_id( )
 *******************************************************************/

int	rts_ipc_upd_task_id()
{
  int	status,
	project,
	program;
  char filename[256];
  FILE *fd=0;

#if VMS
  sprintf( filename, "sys$login:pvm_%s.tmp", MyStream );
#else
  if (getenv("HOME"))
     sprintf( filename, "%s/.pvm_%s.tmp", getenv("HOME"), MyStream );
  else sprintf( filename, "/tmp/pvm_%s.tmp", MyStream );
#endif

  remove(filename);				/* VMS file versions SUCK */

  if (!(fd = fopen(filename, "w")))
  { status = IpcOpenErr;
    return ( status );
  }
 
  for (project=0; project<RTS_PRJ_MAX; project++)
      for (program=0; program<RTS_PRGM_MAX; program++)
          if (procid[project][program])
  { status = fprintf(fd,"%d %d %d\n",
                     project,program,procid[project][program]);
    if (status < 0)
    { status = IpcWrtErr;
      return ( status );
    }
  }
  fclose( fd );

  return ( 0 );
}

/********************************************************************
 *		rts_ipc_get_task_stat(
 *			int	proj,	Project Identifier
 *			int	prog)	Program Identifier
 *******************************************************************/

int	rts_ipc_get_task_stat(
	int	proj,
	int	prog)
{
	int	status = PvmBadParam;
	int	tid;
	int	i, n, ntask,info;
	struct	taskinfo *taskp;

	tid = procid[proj][prog];
	if ( tid ) status = pvm_pstat( tid );
	return ( status );
}

/********************************************************************
 *		rts_ipc_stream_name(
 *			char	*buffer)	output stream name buffer
 *******************************************************************/

void	rts_ipc_stream_name(
	char	*buffer)
{
   strcpy(buffer,MyStream);

   return;
}

/********************************************************************
 *		rts_ipc_set_stream_name(
 *			char	*buffer)	input stream name buffer
 *******************************************************************/

void	rts_ipc_set_stream_name(
	char	*buffer)
{
   strcpy(MyStream, buffer);

   return;
}

/********************************************************************
 *		rts_ipc_log_name(
 *			char	*buffer)	output stream name buffer
 *******************************************************************/

int	rts_ipc_log_name(
	char	*buffer)
{
  if (MyLogFlag)
  { strcpy(buffer,MyLogFile);
    return( TRUE );
  }

  return( FALSE );
}

/********************************************************************
 *		rts_ipc_debug(
 *			int	debug_flag)	input debug option
 *******************************************************************/

void	rts_ipc_debug(
  int	flag)
{
  DebugFlag = flag;

  return;
}

/********************************************************************
 *		rts_ipc_taskid(
 *			int	project,	input project
 *			int	program)	input program
 *******************************************************************/

int	rts_ipc_taskid(
  int	project,
  int	program)
{
   return (procid[project][program]);
}
