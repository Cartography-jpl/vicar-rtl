#ifndef	MIPS_RTS_IPC_INCLUDED
#define MIPS_RTS_IPC_INCLUDED	1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/*
 *	rtsipc.h
 *
 *	Include file for rts_ipc.c routines
 */

/********************************************************************
 * 		IPC internal Message ID				    *
 *******************************************************************/
#define IpcAckMsg	1
#define IpcStartCmd	2
#define IpcInternal	3
#define IpcBuffer	4

/*   Control option sent as first variable of every packet */
#define IpcNoOption	0
#define IpcHello	100		/* General greeting */
#define IpcWhoAmI	101		/* Asked by child at start-up */
#define IpcIAm		102		/* returned from a 'hello' */
#define IpcSendAck	103

#define IpcAnyTask		-1
#define IpcStartTimeOut		15

/********************************************************************
 * 		IPC Error Messages				    *
 *******************************************************************/
#define IpcAckErr	-100
#define IpcBadIdErr	-101
#define IpcBsizeErr	-102
#define IpcInactErr	-103
#define IpcNoAckErr	-104
#define IpcOpenErr	-105
#define IpcProjErr	-106
#define IpcProgErr	-107
#define IpcRcvErr	-108
#define IpcStrmErr	-109
#define IpcWrtErr	-110
#define IpcTimeOutErr	-111
#define IpcNoDataRcvd	-112

#endif
