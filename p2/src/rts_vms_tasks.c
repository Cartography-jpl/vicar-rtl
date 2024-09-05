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
