/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/* kernel header file */

/* general kernal types */

typedef LONG GSTATUS;
typedef COMPACT_COUNT SIZE;		/* CHANGED 12/14/84...KRW */
typedef LONG MILLISECONDS;
typedef LONG STATUS;
typedef char IDNAME;
typedef INT  ID;

/* general kernel definitions */
#define K_NULL 0
#define MAXMSGSZ 8192
#define MBXNAMSIZ 16		/* max. size of a mailbox name */

/*****************************************************************************
 *				Kernel status codes
 ****************************************************************************/
/*
 *	Success codes.
 */
					/* CHANGED 1/18/85....KRW */
#define	K_SUCCESS	(GSTATUS) 0	/* successful completion */

#define	WASSET		(GSTATUS) 1	/* state already set */
#define	WASCLEAR	(GSTATUS) 2	/* state already clear */
#define	NOWAIT		(GSTATUS) 3	/* wait not necessary */
#define	TRUNCATED	(GSTATUS) 4	/* truncation on data read */
/*
 *	Error codes.
 */
					/* CHANGED 1/18/85....KRW */
#define	K_FAILURE	(GSTATUS) -101	/* not successful completion */

#define	INVPARM		(GSTATUS) -1	/* invalid parameter */
#define	INSUFMEM	(GSTATUS) -2	/* insufficient memory */
#define	NOPRIV		(GSTATUS) -3	/* no privilege to perform function */
#define	NOTFOUND	(GSTATUS) -4	/* item not found */
#define	DELETED		(GSTATUS) -5	/* structure deleted */
#define	HOSTERROR	(GSTATUS) -6	/* host service error */
#define	DUPLICATE	(GSTATUS) -7	/* duplicate object */
#define	DATALOST	(GSTATUS) -8	/* data lost on data write */
#define	INVMODE		(GSTATUS) -9	/* service not permitted in current mode */
#define	DISCONNECTED	(GSTATUS) -10	/* mailbox disconnected */
#define	NORECEIVE	(GSTATUS) -11	/* no message to receive */
#define	NOT_IMPLEMENTED	(GSTATUS) -99	/* service not implemented */
/*
 *	Socket errors.
 */
#define	KC_SOCK_SOCK	(GSTATUS) -401	/* socket error */
#define	KC_SOCK_CON	(GSTATUS) -402	/* connect error */
#define	KC_SOCK_SEND 	(GSTATUS) -403	/* send error */
#define	KC_SOCK_RECV 	(GSTATUS) -404	/* RECV error */
#define	KC_SOCK_SEL 	(GSTATUS) -405	/* select error */
