#include "xvmaininc.h"
#include "zvproto.h"
#include "rtlintproto.h"
/*                                                                         */
/*                             UNIX VERSION  (may be Sun dependent)        */
/*                                                                         */
/***************************************************************************
 *	hostmsg.  Returns a string with the system error message.
 ***************************************************************************
 */


void hostmsg(code, msg, maxlen)
int code;				/* the system error code */
char *msg;				/* out: the string */
int maxlen;				/* in: max length of msg */

{
   /* A code of zero means that no error has occured. */

   if (code != 0) {
#if HOSTMSG_UNIX_OS
      if (code < sys_nerr && code != 0) {
	 len = strlen(sys_errlist[code]) + 1;	/* Include null terminator */
	 if (len > maxlen) 
	    len = maxlen;
         strncpy(msg, sys_errlist[code], len);
	 msg[maxlen] = '\0';		/* Make sure there's a null term */
      }
      else {
         strncpy(msg, "unknown system error", maxlen);
	 msg[maxlen] = '\0';		/* Make sure there's a null term */
      }
#else
      strncpy(msg, HOSTMSG_FUNC_OS(code), maxlen);
      msg[maxlen] = '\0';
#endif
   }
   else {
      if (maxlen > 0) 
	 msg[0] = '\0';
   }
}

/* Written by Mark Mann ASU 10/3/89 */
