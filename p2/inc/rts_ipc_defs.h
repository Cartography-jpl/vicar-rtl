#ifndef MIPS_RTS_IPC_DEFS_INCLUDED
#define MIPS_RTS_IPC_DEFS_INCLUDED	1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <signal.h>

#include "rtsipc.h"

#define  RTS_IPC_NO_ACK		0
#define  RTS_IPC_ACK_WAIT	1
#define  RTS_IPC_PKT_SIZE	2048

#define  RTS_PKT_INTERNAL	1
#define  RTS_PKT_MSG		2
#define  RTS_PKT_CNTRL		3

#define  RTS_PRJ_ANY		-1
#define  RTS_PRJ_RTS		0
#define  RTS_PRJ_VGR		1
#define	 RTS_PRJ_GLL		2
#define  RTS_PRJ_M94		3
#define	 RTS_PRJ_M96		4
#define  RTS_PRJ_MPF		5
#define  RTS_PRJ_CSS		6
#define  RTS_PRJ_MAX		7

#define  RTS_PRGM_ANY		0
#define  RTS_PRGM_CNTRL		1
#define  RTS_PRGM_DSP_LOG	2
#define  RTS_PRGM_LOGGER	3
#define  RTS_PRGM_TP		4
#define  RTS_PRGM_IB1		5
#define  RTS_PRGM_IB2		6
#define  RTS_PRGM_IB3		7
#define  RTS_PRGM_IB4		8
#define  RTS_PRGM_MAX		9

#define  rts_send_cmnd( proj, prog, cmnd) \
		rts_ipc_write(	proj, prog, cmnd, (strlen(cmnd)+1), \
				RTS_IPC_NO_ACK, 0 )

void	rts_ipc_cleanup( void );
int	rts_ipc_init_parent( int, int, char *, char * );
int	rts_ipc_init_child();
int	rts_ipc_start( int, int, char *, char * );
int	rts_ipc_kill( int, int );
int	rts_ipc_read( int *, int *, char *, int, int);
int	rts_ipc_write( int, int, char *, int, int, int );
int	rts_ipc_get_task_id( );
int	rts_ipc_get_task_stat( int, int );
int	rts_ipc_upd_task_id( );
void	rts_ipc_stream_name( char * );
void	rts_ipc_set_stream_name( char * );
int	rts_ipc_taskid( int, int );
int	rts_set_exit_handler( char *, int, char *, int);

/****  Extraneous stuff kept around for historical reasons ... for now 
#define  RTS_PKT_TYPE(x)	((x) & 0x0F)
#define  RTS_PKT_PRIORITY(x)	(((x) & 0x70) >> 4)
#define  RTS_PKT_ACK(x)		((x) & 0x80)

#define  RTS_PKT_PROJECT(x)	((x) & 0x0F)
#define  RTS_PKT_PROGRAM(x)	(((X) & 0xF0) >> 4)

typedef	struct	{
		Uword		Length;
		Ubyte		Id;
		Ubyte		Origin;
		Ubyte		Destination;
		Ubyte		Reserved[3];
		} rts_pkt_hdr_typ;
******/

/**************************************************************************/
/***	These are used by RTCNTRL, but are kept here so any changes	***/
/***  only need to be made in on place					***/

#define RTS_LOGGER_PRGM_NAME	"rtlogger"
#define RTS_DSP_LOG_PRGM_NAME	"rtdsplog"

static  char    *rts_projects[RTS_PRJ_MAX] = {
        "RTS",  "VGR",  "GLL",  "M94",  "M96",  "MPF",  "CSS"};

static	char	*rts_prgm_name_parts[RTS_PRGM_MAX] = {
	0, 0, 0, 0,	"TLMPRC",	"IMBLD1",	"IMBLD2",
	"IMBLD3",	"IMBLD4"};

static	char	*rts_upf_name_parts[RTS_PRGM_MAX] = {
	0, 0, 0, 0,	"TP",	"IB1",	"IB2",	"IB3",	"IB4"};
#endif
