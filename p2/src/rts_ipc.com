$!****************************************************************************
$!
$! Build proc for MIPL module rts_ipc
$! VPACK Version 1.9, Tuesday, August 03, 1999, 14:50:26
$!
$! Execute by entering:		$ @rts_ipc
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
$ write sys$output "*** module rts_ipc ***"
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
$ write sys$output "Invalid argument given to rts_ipc.com file -- ", primary
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
$   if F$SEARCH("rts_ipc.imake") .nes. ""
$   then
$      vimake rts_ipc
$      purge rts_ipc.bld
$   else
$      if F$SEARCH("rts_ipc.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rts_ipc
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rts_ipc.bld "STD"
$   else
$      @rts_ipc.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rts_ipc.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rts_ipc.com -
	-s rts_ipc.c rts_vms_tasks.c rts_spawn.c -
	-i rts_ipc.imake -
	-t tst_rts_spawn.c tst_rts_spawn.imake -
	-o rts_ipc.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rts_ipc.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_vms_tasks.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <lnmdef.h>
#include <prcdef.h>
#include <pqldef.h>
#include <psldef.h>
#include <shrdef.h>
#include <descrip.h>
#include <starlet.h>

typedef struct
	{
	char	*String;
	int	Required;
	} log_itm_typ;

/***  Function Prototypes  ***/
/*  All functions return the standard VMS status;  success is designated by */
/*  bit 0 being set to one (i.e., if (RTN_STATUS & 0x01) then success       */
int	rts_vms_copy_logical( char *, char *, char *);
int	rts_vms_Cprocess( char *, char *, int *);
int	rts_vms_Clogical( char *, char *, char **);
int	rts_vms_Ctable( char * );
int	rts_vms_free_list( log_itm_typ *, int, int);
int	rts_vms_Tlogical(char *, char *, char **, int);
int	rts_vms_xfer_logicals( int );

#define	MAX_LOGICALS		   8	/* Max values assigned to a logical */
#define MAX_LOGICAL_LTH		 256	/* Max length of logical value	*/
#define MAX_TABLE_ELEMENTS	  64
#define MAX_TABLE_SIZE		(MAX_TABLE_ELEMENTS * MAX_LOGICAL_LTH)

/*****************************************************************************
/*				RTS_VMS_XFER_LOGICALS
/*
/*	Transfers a set of logical names from the PROCESS table to a
/*  Global table and vice versa.  This is used to define VICAR system
/*  dependent logicals for a detatched process.
/****************************************************************************/
int	rts_vms_xfer_logicals(
  int	ToGlobal)		/* Boolean: transfer from process to global */
{ int	BufLth,
	elements = 0,
	idx,
	status;
  char	TableName[256],
	LogicalBuffer[128],
	*FromTable = "LNM$FILE_DEV",		/* Get it from where-ever */
	*ToTable = "LNM$PROCESS",		/* Set it for the process */
	*FileNames[2] = {	"PVM_ROOT:[LIB]VMS_LOGICALS.TXT",
				"SYS$LOGIN:PVM_LOGICALS.TXT" };
  FILE	*Names;
  log_itm_typ	Logicals[MAX_TABLE_ELEMENTS + 1],
		CannedLogicals[] = {
			{ "SYS$LOGIN",	TRUE },
			{ "R2LIB",	TRUE },
			{ "P2$INC",	TRUE },
			{ "GUI$LIB",	TRUE },
			{ "DECW$SYSTEM_DEFAULTS",	TRUE },
			{ "DECW$DISPLAY",	TRUE },

			{ "DECW$USER_DEFAULTS",	FALSE },
			{ "USER$LOCAL",	FALSE },
			{ 0,	FALSE }};
  char *cuserid_p2();

  /***  Load logicals to transfer  ***/
  memset(Logicals,0,sizeof(Logicals));
  while (CannedLogicals[elements].String)
  { BufLth = strlen(CannedLogicals[elements].String) + 1;
    Logicals[elements].String = malloc(BufLth);
    if (!Logicals[elements].String) return( 0 );
    strcpy(Logicals[elements].String,CannedLogicals[elements].String);
    Logicals[elements].Required = CannedLogicals[elements].Required;
    elements++;
  }

  for (idx=0; idx<2; idx++)
  { Names = fopen(FileNames[idx],"r");
    if (Names)
       while (fgets(LogicalBuffer,sizeof(LogicalBuffer)-1,Names))
    { BufLth = strcspn(LogicalBuffer," \n,:") + 1;
      Logicals[elements].String = malloc(BufLth);
      if (!Logicals[elements].String) return( 0 );
      memset(Logicals[elements].String,0,BufLth);
      strncpy(Logicals[elements].String,LogicalBuffer,BufLth-1); 
      if (idx == 0) Logicals[elements].Required = TRUE;
      else Logicals[elements].Required = FALSE;
      if (elements >= MAX_TABLE_ELEMENTS) break;
      elements++;
    }
    fclose(Names);
  }

  /***  Create table and transfer logicals  ***/
  sprintf(TableName,"LNM$USER_%s",cuserid_p2());
  if (ToGlobal)
  { status = rts_vms_Ctable(TableName);
    if (!(status & 0x01))
       return ( rts_vms_free_list(Logicals,elements,status) );
    ToTable = TableName;
  } else FromTable = TableName;

  for (idx=0; Logicals[idx].String; idx++)
  { if (Logicals[idx].Required)
       status = rts_vms_copy_logical(FromTable,ToTable,Logicals[idx].String);
    else rts_vms_copy_logical(FromTable,ToTable,Logicals[idx].String);
    if (!(status & 0x01))
       return ( rts_vms_free_list(Logicals,elements,status) );
  }

  return ( rts_vms_free_list(Logicals,elements,status) );
}

/*****************************************************************************
/*				RTS_VMS_COPY_LOGICAL
/*
/*	This routine copies the value(s) of one logical name from one
/*  table to an other.  Both tables and the logical name are passsed as
/*  parameters.
/****************************************************************************/
int	rts_vms_copy_logical(
  char	*TableFrom,		/*  Table where logical is defined	*/
  char	*TableTo,		/*  Table where logical will be defined	*/
  char	*Logical)		/*  Logical name to copy between tables	*/
{ int	idx,
	status;
  char	*LogicalName[MAX_LOGICALS];

  memset(LogicalName,0,sizeof(LogicalName));
  for (idx=0; idx<MAX_LOGICALS; idx++)
  { LogicalName[idx] = malloc(MAX_LOGICAL_LTH);
    if (LogicalName[idx]) memset(LogicalName[idx],0,MAX_LOGICAL_LTH);
    else break;
  }
  status = rts_vms_Tlogical(TableFrom,Logical,LogicalName,idx);
  if (status & 0x01)
     status = rts_vms_Clogical(TableTo,Logical,LogicalName);
  for (idx=0; idx<MAX_LOGICALS; idx++)
      if (LogicalName[idx]) free(LogicalName[idx]);

  return (status);
}

/*****************************************************************************
/*				RTS_VMS_CPROCESS
/*
/*	This process creates a detatched process.  It defines the standard
/*  output and error output to be files in the SYS$LOGIN directory and
/*  uses the name of the process as the file part of the filename.  The file
/*  extension is either "log" or "err" respectively.
/****************************************************************************/
int	rts_vms_Cprocess(
  char	*image_name,			/*  Program name to detatch	*/
  char	*process_name,			/*  Name to call this process	*/
  int	*pid)				/*  Assigned process ID of detatched */
					/*    process			*/
{
	int		stat;
struct	dsc$descriptor	image;			/* PROCESS IMAGE SPEC.	*/
struct	dsc$descriptor	input_dev;		/* PROCESS INPUT DEVICE */
struct	dsc$descriptor	output_dev;		/* PROCESS OUTPUT DEVICE*/
struct	dsc$descriptor	error_dev;		/* PROCESS ERROR DEVICE */
struct	dsc$descriptor	proc_name;		/* GIVEN PROCESS NAME	*/
	int		stsflags =		/* PROCESS CREATION FLAG*/
                            PRC$M_DETACH | PRC$M_SUBSYSTEM | PRC$M_IMGDMP;
	char	OutputFile[256],
		ErrorFile[256];
/*
 *------------------------------------------------------------------------------
 *	List of quotas to be given to the created process
 *------------------------------------------------------------------------------
 */
struct	{
	char	quota;
	int	amount;
	}	quota_dsc[] =
		{
/*		 {PQL$_PGFLQUOTA,	15000},	/* PAGE FILE SIZE	*/
/*		 {PQL$_WSQUOTA,		1800},	/* WORKING SET SIZE	*/
/*		 {PQL$_WSEXTENT,	4096},	/* WORKING SET EXTENTION*/
/*		 {PQL$_FILLM,		32},	/* File Open Limit	*/
/**/
		 {PQL$_LISTEND,		0}	/* LIST TERMINATOR	*/
		};			

  /*	BUILD THE STANDARD DEVICE DESCRIPTORS	*/
    image.dsc$b_dtype = DSC$K_DTYPE_T;
    image.dsc$b_class = DSC$K_CLASS_S;
    image.dsc$a_pointer = image_name;
    image.dsc$w_length = strlen( image_name);

    input_dev.dsc$b_dtype = DSC$K_DTYPE_T;
    input_dev.dsc$b_class = DSC$K_CLASS_S;
    input_dev.dsc$a_pointer = "_NLA0:";
    input_dev.dsc$w_length = strlen(input_dev.dsc$a_pointer);

    sprintf(OutputFile,"SYS$LOGIN:%s.log",process_name);
    output_dev.dsc$b_dtype = DSC$K_DTYPE_T;
    output_dev.dsc$b_class = DSC$K_CLASS_S;
    output_dev.dsc$a_pointer = "_NLA0:";
/**/
    output_dev.dsc$a_pointer = OutputFile;
/**/
    output_dev.dsc$w_length = strlen(output_dev.dsc$a_pointer);

    sprintf(ErrorFile,"SYS$LOGIN:%s.err",process_name);
    error_dev.dsc$b_dtype = DSC$K_DTYPE_T;
    error_dev.dsc$b_class = DSC$K_CLASS_S;
    error_dev.dsc$a_pointer = "_NLA0:";
/**/
    error_dev.dsc$a_pointer = ErrorFile;
/**/
    error_dev.dsc$w_length = strlen(error_dev.dsc$a_pointer);

    proc_name.dsc$b_dtype = DSC$K_DTYPE_T;
    proc_name.dsc$b_class = DSC$K_CLASS_S;
    proc_name.dsc$a_pointer = process_name;
    proc_name.dsc$w_length = strlen( process_name );

    stat = sys$creprc(  pid, &image, &input_dev, &output_dev, &error_dev,
			0, &quota_dsc, &proc_name, 5,0,0, stsflags);

    return (stat);
}

/*****************************************************************************
/*				RTS_VMS_CTABLE
/*
/*	Creates a shareable logical table for storing the VICAR dependent
/*  logicals in a global location that will not affect normal processing.
/****************************************************************************/
int	rts_vms_Ctable(
  char	*table_name)			/*  Name of shareable table */
{
  int	attr = LNM$M_CREATE_IF | LNM$M_NO_ALIAS,
	quota = MAX_TABLE_SIZE,
	status;
  short	promsk = 0xF000;
  char	acmode = PSL$C_SUPER;
  struct	dsc$descriptor	tabnam;
  struct	dsc$descriptor	partab;

  tabnam.dsc$b_dtype = DSC$K_DTYPE_T;
  tabnam.dsc$b_class = DSC$K_CLASS_S;
  tabnam.dsc$a_pointer = table_name;
  tabnam.dsc$w_length = strlen( table_name );

  partab.dsc$b_dtype = DSC$K_DTYPE_T;
  partab.dsc$b_class = DSC$K_CLASS_S;
  partab.dsc$a_pointer = "LNM$SYSTEM_DIRECTORY";
  partab.dsc$w_length = strlen( "LNM$SYSTEM_DIRECTORY" );

  status = sys$crelnt(&attr, 0, 0, &quota, &promsk, &tabnam, &partab, &acmode);

  return (status);
}

/*****************************************************************************
/*				RTS_VMS_CLOGICAL
/*
/*	Defines a logical name in a given table.  The logical name can be
/*  set to multiple values.
/****************************************************************************/
int	rts_vms_Clogical(
  char  *log_table,			/*  What table to define the logical */
  char	*log_name,			/*  Name of the logical to define */
  char	**value)			/*  Value(s) to assign to the logical */
					/*    The value list is terminated by */
					/*    a null pointer or null strings. */
{ int	idx,
	acmode = PSL$C_SUPER,
	status;
  struct {
	short	buf_lth;
	short	itm_code;
	void	*buf_addr;
	short	*rtn_lth_addr;
	} itm_lst[MAX_LOGICALS+1];
  $DESCRIPTOR(dscTable,"BS");
  $DESCRIPTOR(dscName,"BS");

  memset(itm_lst,0,sizeof(itm_lst));

  dscTable.dsc$w_length = strlen(log_table);
  dscTable.dsc$a_pointer = log_table;
  dscName.dsc$w_length = strlen(log_name);
  dscName.dsc$a_pointer = log_name;

  for (idx=0; idx<=MAX_LOGICALS && value[idx]; idx++)
      if (strlen(value[idx]))
  { itm_lst[idx].buf_lth = strlen(value[idx]);
    itm_lst[idx].itm_code = LNM$_STRING;
    itm_lst[idx].buf_addr = value[idx];
  } else break;

  status = sys$crelnm(0,&dscTable,&dscName,&acmode,&itm_lst);

  return (status);
}

/*****************************************************************************
/*				RTS_VMS_FREE_LIST
/*
/*	Frees memory allocated to logical name trasnfer list.
/****************************************************************************/
int	rts_vms_free_list(
  log_itm_typ *Logicals,
  int	elements,
  int	status)
{
  while (elements >= 0) free(Logicals[elements--].String);

  return status;
}

/*****************************************************************************
/*				RTS_VMS_TLOGICAL
/*
/*	Translates a logical name for a given table into its value(s).  The
/*  maximum number of values is specified by a parameter.
/****************************************************************************/
int	rts_vms_Tlogical(
  char	*log_tbl,		/* Which table to search */
  char	*logical,		/* What logical to translate */
  char	**translated,		/* The Brass Ring */
  int	max_values)		/* Maximum number of translations */
{ int	stat,
	idx,
	max_idx,
	length = 132,
	attr = LNM$M_CASE_BLIND;
  struct {
	short	len;
	short	item;
	void	*buffer;
	int	*length_add;
	}		itmlst[3];
  struct	dsc$descriptor  log_tbl_desc;
  struct	dsc$descriptor  node_desc;

  log_tbl_desc.dsc$b_dtype = DSC$K_DTYPE_T;	/* LOGICAL NAME TABLE 	*/
  log_tbl_desc.dsc$b_class = DSC$K_CLASS_S; 
  log_tbl_desc.dsc$a_pointer = log_tbl;
  log_tbl_desc.dsc$w_length = strlen( log_tbl);

  node_desc.dsc$b_dtype = DSC$K_DTYPE_T;
  node_desc.dsc$b_class = DSC$K_CLASS_S; 
  node_desc.dsc$a_pointer = logical;
  node_desc.dsc$w_length = strlen( logical);

  memset(itmlst,0,sizeof(itmlst));
  idx = 4;
  itmlst[0].len = 132;				/* MAX LENGTH		*/
  itmlst[0].item = LNM$_MAX_INDEX;		/* ITEM CODE		*/
  itmlst[0].buffer = &max_idx;			/* TRANSLATED STRING	*/
  itmlst[0].length_add = &idx;			/* LENGTH OF TRANSLATED */
  stat = sys$trnlnm( &attr, &log_tbl_desc, &node_desc, 0, &itmlst);

  for (idx=0; idx<=max_idx && idx<max_values && (stat & 0x01); idx++)
  { memset(itmlst,0,sizeof(itmlst));
    itmlst[0].len = 4;				/* MAX LENGTH		*/
    itmlst[0].item = LNM$_INDEX;		/* ITEM CODE		*/
    itmlst[0].buffer = &idx;			/* TRANSLATED STRING	*/
    itmlst[0].length_add = &length;		/* LENGTH OF TRANSLATED */

    itmlst[1].len = 132;			/* MAX LENGTH		*/
    itmlst[1].item = LNM$_STRING;		/* ITEM CODE		*/
    itmlst[1].buffer = translated[idx];		/* TRANSLATED STRING	*/
    itmlst[1].length_add = &length;		/* LENGTH OF TRANSLATED */

    stat = sys$trnlnm( &attr, &log_tbl_desc, &node_desc, 0, &itmlst);
    if (!(stat&1)) length = 0;
    translated[idx][length] = '\0';
  }

  return (stat);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rts_spawn.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rts_ipc.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE rts_ipc
/*
/*   To Create the build file give the command:
/*
/*		$ vimake rts_ipc			(VMS)
/*   or
/*		% vimake rts_ipc			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE rts_ipc		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#if VMS_OS
#define MODULE_LIST rts_ipc.c rts_vms_tasks.c
#else
#define MODULE_LIST rts_ipc.c rts_spawn.c
#endif

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

#define LIB_PVM
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
/**********  End of rts_ipc imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_rts_spawn.c
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

#include "rts_errors.h"
#include "rts_logging.h"
#include "rts_spawn.h"


#include "rts_const_defs.h"
char	*rts_log_prgm = "TST_IPC";
static char	LogMsgBuf[256];
main(
  int	argc,
  char	**argv,
  char	**environ)
{ int	lth,
	status;
  char	Buffer[256];
  rts_spawn_typ	Child;

  status = rts_make_list(&Child.Environ,(rts_count_list(environ)+1));
  status = rts_copy_list(Child.Environ,environ);


rts_set_logmasks("UPDATE INFO IPC");

  if (argc > 2)			/* Child version */
  {
printf("Child Started\n");
    gets(Buffer);
    Child.CreateWritePipe = TRUE;
    Child.MapWriteStdout = TRUE;
    strcpy(Child.Program,"/bin/ls");
    rts_make_list(&Child.Argv,8);
    rts_insert_list_item(Child.Argv,0,"ls");
    rts_insert_list_item(Child.Argv,1,Buffer);
    rts_spawn_task(&Child);

    memset(Buffer,0,256);

    printf("\nCHILD: Communication with Parent verification\n");
    while((lth = read(Child.ReadPipe,Buffer,255)) > 0)
    { Buffer[lth] = 0;
      puts(Buffer);
      memset(Buffer,0,256);
    }

    exit(0);
  }

printf("Parent Started\n");

    Child.CreateReadPipe = TRUE;
    Child.MapReadStdin = TRUE;
    strcpy(Child.Program,argv[0]);
    rts_make_list(&Child.Argv,8);
    rts_insert_list_item(Child.Argv,0,argv[0]);
    rts_insert_list_item(Child.Argv,1,"Boo");
    rts_insert_list_item(Child.Argv,2,"Blah");
    status = rts_spawn_task(&Child);
    printf("\nPARENT: Child birth verification: %d (0 is good)\n",status);

    printf("\nPARENT: Communication with Child verification\n\n");
    if (argc < 2)
    { printf("Directory parameter not supplied, using Current Working Directory\n");
      status = write(Child.WritePipe,".\n",2);
    } else
    { status = write(Child.WritePipe,argv[1],strlen(argv[1]));
      write(Child.WritePipe,"\n",1);
    }
sleep(10);


  printf("\nPARENT: Spawn utility tool verification - list manipulation\n");

  status = rts_make_list(&Child.Argv,rts_count_list(argv)+24);

  status = rts_copy_list(Child.Argv,argv);

  rts_add_list_item(Child.Argv,"DISPLAY=unix:0.0");
  rts_add_list_item(Child.Argv,"Testing is Fun!");
  rts_add_list_item(Child.Argv,"You have just won a mystery gift!");
  rts_add_list_item(Child.Argv, "Contact devloper for your prize");
  rts_add_list_item(Child.Argv,"The End");
  rts_display_list(Child.Argv);
  printf("\nPARENT: Spawn utility tool verification - adding and replacing\n");
  rts_insert_list_item(Child.Argv,5,"Number 5 is alive (zero based)");
  rts_replace_env_item(Child.Argv,"DISPLAY=fred:0.0");
  rts_display_list(Child.Argv);

  printf("\nPARENT: Spawn utility tool verification - list creation/extraction\n");
rts_make_list(&Child.Argv,20);
rts_parse_argv_list(Child.Argv,"This is the \"List that It\" included");
rts_display_list(Child.Argv); exit(0);
}
$!-----------------------------------------------------------------------------
$ create tst_rts_spawn.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_rts_spawn
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_rts_spawn			(VMS)
/*   or
/*		% vimake tst_rts_spawn			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_rts_spawn		/* Only one of these */
/*#define PROCEDURE tst_rts_spawn		/* Only one of these */
#define PROGRAM tst_rts_spawn		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_rts_spawn.c

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
/***	only one allowed
#define MARS_SUBLIB
/**/
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_PVM
#define LIB_NETWORK
#define LIB_P2SUB
#define LIB_MARSSUB

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
/**********  End of tst_rts_spawn imake file  **********/
$ Return
$!#############################################################################
$Other_File:
$ create rts_ipc.hlp
1 rts_ipc.c

  RTS_IPC contains common subroutines which provide basic functions
  for communication between processes. The following subroutines
  are provided:


        rts_ipc_init_parent     - Get parent process Id and host platform (e.g.
				  vms or unix).
	
	rts_ipc_init_child	- Get child process id, and get information
				  from parent information such as the 
				  logging file name.

	rts_ipc_start		- This routine spawns a named process based
				  on project and program id's.  It also sends
				  information such as the logging file name
				  to the spawned process.

	rts_ipc_kill		- This routine terminates a given running
				  process.

	rts_ipc_read		- This routine reads data coming from a
				  given process.

	rts_ipc_write		- This routine sends data to a given process.
				  Acknowledgement request is supported.

	rts_ipc_get_task_id	- This routine provides requestor all process
				  id's currently running on the virtual
				  machine.

	rts_ipc_get_task_stat	- This routine returns the status of a given
				  process.

	rts_ipc_stream_name	- This routine returns the stream name for
				  the current process.
	

2 rts_ipc_init_parent 

  Purpose:   This subroutine gets parent process id and host platform (e.g. 
	     vms, unix). 

  C Calling sequence:	#include "rts_ipc_defs.h"

			int	project;
			int	program;
			char	*stream;
			char	*logfile;

			status = rts_ipc_init_parent(project,program,
						     stream,logfile);

  Input:    project    	- project id as defined in the SDD-D-11221 
            program  	- program id as defined in the SDD-D-11221 
            stream	- stream name as defined in the SDD-D-11221
            logfile	- diskfile name to record run-time messages

  Output:   The parent process id.

  Status returns:   0 = OK
                   -1 = Error in the getting process id or host platform. 


2 rts_ipc_init_child 

  Purpose:   This subroutine gets child process id, and waits for information
	     such as the logging file name form parent process.

  C Calling sequence:	#include "rts_ipc_defs.h"

			status = rts_ipc_init_child();


  Input:    None.

  Output:   The child process id, host plaform, and logging file name if any.

  Status returns:   process id if OK
                    negative if error in the getting process id. 


2 rts_ipc_start 

  Purpose:   This subroutine is used to spawn a given process based on project
	     and program id's.  It also sends information such as the 
	     logging file name to the spawned process. 

  C Calling sequence:	#include "rts_ipc_defs.h"

			int	project;
			int	program;
			char	*host

			status = rts_ipc_start(project,program,host);

  Input:    project    	- project id as defined in the SDD-D-11221 
            program  	- program id as defined in the SDD-D-11221 
	    host	- host name

  Output:   A spawned process with its process id provided, and information
	    such as the logging file named passed to the spawned process.

  Status returns:   0 = OK
                   -1 = Error in the spawned process 


2 rts_ipc_kill 

  Purpose:   This subroutine terminates the given process, given by its 
	     project and program id's. 

  C Calling sequence:	#include "rts_ipc_defs.h"

			int	project;
			int	program;

			status = rts_ipc_kill(project,program);

  Input:    project    	- project id as defined in the SDD-D-11221 
            program  	- program id as defined in the SDD-D-11221 

  Output:   A given process is terminated. 

  Status returns:   0 = OK
                   -1 = Error in the termination 


2 rts_ipc_read 

  Purpose:   This subroutine reads data coming from a given process. The 
	     timeout ( in seconds ) is used to wait for the incoming data 
	     before the read is terminated.   

  C Calling sequence:	#include "rts_ipc_defs.h"

			int	project;
			int	program;
			char	*buf;
			int	buflen;
			int	timeout;

			status = rts_ipc_read(project,program,buf,
						bufen,timeout);

  Input:    project    	- project id as defined in the SDD-D-11221 
            program  	- program id as defined in the SDD-D-11221 
	    buf		- buffer where the received data is stored
	    buflen	- length of data to be read
	    timeout	- the amount of time in seconds to wait for data
			  to come in. -1 means wait indefinitely.

  Output:   Data is stored in the local buffer. 

  Status returns:   0 = OK
		   -1 = Error in reading or no data received in the specified
			time period.

2 rts_ipc_write 

  Purpose:   This subroutine sends data to a given process. The 
	     timeout ( in seconds ) is used to wait for acknowledgement 
	     from the sent process.   

  C Calling sequence:	#include "rts_ipc_defs.h"

			int	project;
			int	program;
			char	*buf;
			int	buflen;
			int	ack_flag;
			int	timeout;

			status = rts_ipc_write(project,program,buf,
						bufen,ack_flag,timeout);

  Input:    project    	- project id as defined in the SDD-D-11221 
            program  	- program id as defined in the SDD-D-11221 
	    buf		- buffer where the sent data is stored
	    buflen	- length of data to be sent 
	    ack_flag	- acknowledgement flag. 0 = no acknowledgment,
			  any non-zero value means acknowledgment requested.
	    timeout	- the amount of time in seconds to wait for 
			  acknowledgement when ack_flag is set. -1 means
			  to wait indefinitely.

  Output:   Data is sent to the given process. 

  Status returns:   0 = OK
		   -1 = Error in sending or no acknowledgement received from 
			the sent process after the specified time period.


2 rts_ipc_get_task_id 

  Purpose:   This subroutine retrieves all process id's that are currently
             active in the virtual machine. 

  C Calling sequence:	#include "rts_ipc_defs.h"

			status = rts_ipc_get_task_id( );

  Input:    none

  Output:   All process id's of the given project that are active are
	    retrieved. 

  Status returns:   n = the number of active processes with the specified
			project id 
                    1 = Only the requesting process is active. 


2 rts_ipc_get_task_stat 

  Purpose:   This subroutine returns the status of a given process. 

  C Calling sequence:	#include "rts_ipc_defs.h"

			int	project;
			int	program;

			status = rts_ipc_get_task_stat(project,program);

  Input:    project    	- project id as defined in the SDD-D-11221 
	    program	- program id as defined in the SDD-D-11221

  Output:   Returns the status of the given process. 

  Status returns:   0 = the given process is active
                    negative = The process is inactive. 

2 rts_ipc_stream_name

  Purpose:   This subroutine returns the stream name for the process.

  C Calling sequence:   #include "rts_ipc_defs.h"

                        char	stream[128];

                        rts_ipc_stream_name(stream);

  Output:   stream	- stream name used by the process.

  Status returns:   VOID


2 History

Original programmer: Raymind Lin, December 94
Cognizant programmer: Allan Runkle December 94
Source language:     C
$ Return
$!#############################################################################
