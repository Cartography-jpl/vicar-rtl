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



/* TDM CHECKOUT FILE_TIME=26-MAR-1984 14:31 DUA0:[TAEV1.OLB]MMSG.C;7 */
/*
 *	m_msg. Send a message (from a 'c' application program )to the 
 *		terminal monitor for logging.
 *
 *	NOTE: THIS MODULE MUST BE MAINTAINED PARALLEL TO MPARM.C
 *
 *	CHANGE LOG:
 *
 *	11-mar-84	Change handshake implementation.
 *	21-aug-84	Fix error loop on handshake failure (PR #790)...dm
 *	14-sep-86	Fix bug in procexit calling seq...dm
 *	29-jan-87	Fix dimension in q_init calling seq...peb
 *	14-may-89	if null key, do not display...palm 
 *
 */

#include	"taeconf.inp"
#include	"parblk.inc"
#include	"tminc.inc"
#include 	"terminc.inc"
#include	"fileinc.inp"
#include	<stdio.h>
#include "taeintproto.h"



    GLOBAL 	TEXT savekey[KEYSIZ+1];		/* last message key 	*/
    GLOBAL  FILE  *stdo_file = NULL;		/* ptr to FILE for stdout */
    GLOBAL  BOOL	  term_std = TRUE;	/* terminal as stdout   */

static    TEXT	stdbuf[OUTMSGSIZ+KEYSIZ+4];     /* standard output buffer */


FUNCTION    VOID  m_msgout
(
 TEXT	msgbuf[OUTMSGSIZ+1],
 TEXT	msgkey[KEYSIZ+1]
 );


#define MSG_BYTES 3*sizeof(struct VARIABLE) + 3*OUTMSGSIZ
DEF_PARBLK(MSG_PARBLK, MSG_BYTES);

FUNCTION  CODE  m_msg
(
 TEXT	message[],			/* In: message to be logged */
 TEXT	key[]				/* In: message key */
 )
    {
    CODE 	code;

    struct MSG_PARBLK	block;		/* parameter block to send */
    struct	PARBLK  *parblk;		/* pointer to parameter block*/

    TEXT	*vector[1];			/* pointer to value */
    TEXT	msgbuf[OUTMSGSIZ+1];
    TEXT	msgkey[KEYSIZ+1];

    
    parblk = (struct PARBLK*) &block;
    q_init (parblk, MSG_BYTES, P_ABORT);	/* initialize block */
    s_bcopy((GENPTR)message, (GENPTR)msgbuf, OUTMSGSIZ); /* copy locally */
    if (s_length(message) > OUTMSGSIZ)			/* if too long-	*/
	msgbuf[OUTMSGSIZ] = EOS;			/* truncate	*/
    vector[0] = (TEXT *) msgbuf;
    code = q_string(parblk, "MESSAGE", 1, vector, P_ADD); /* add message */
    if (code == SUCCESS)
	{
	s_bcopy(key, msgkey, KEYSIZ);			/* copy key locally */
	if (s_length(key) > KEYSIZ)
	    msgkey[KEYSIZ] = EOS;			/* police length */
        vector[0] = (TEXT *) msgkey;			/* pointer to key   */
	code = q_string(parblk, "KEY", 1, vector, P_ADD); /* add key 	    */
	}
    if (code == SUCCESS)
	{
	code = q_sndp(parblk, M_HLOGMSG);		/* send message     */
	if (code == SUCCESS)
	    {
	    code = c_rcvp((GENPTR)parblk, sizeof(block)); /* rcv handsh blk */
	    block.hostcode = code;			/* save host code   */
	    if (code != SUCCESS)
		{
		t_write(
	"[TAE-MSGHANDS] Unable to confirm message reception by TM", T_STDCC);
		procexit(code);
		}
	    }
	}
    s_copy(msgkey, savekey);				/* save the key     */
    m_msgout(msgbuf, msgkey);				/* write to output  */
    return(code);
    }

/*
 *	m_msgout. write message to standard output device, if not terminal.
 *
 */

FUNCTION    VOID  m_msgout
(
 TEXT	msgbuf[OUTMSGSIZ+1],
 TEXT	msgkey[KEYSIZ+1]
 )
    {

    if (!term_std && stdo_file != NULL)	/* if stdout is a file */
	{
	stdbuf[0] = EOS;    
	if (msgkey && msgkey[0] != 0)		/* only if key exists   */
	    {
	    s_copy("[", stdbuf);		/* add the key		*/
	    s_append(msgkey, stdbuf);
	    s_append("] ", stdbuf);
	    }
	s_append(msgbuf, stdbuf);	/* add the message	*/
	s_append("\n", stdbuf);		/* add new line character */
	fputs(stdbuf, stdo_file);   	/* write to stdout	*/    
	}
    return;
    }
