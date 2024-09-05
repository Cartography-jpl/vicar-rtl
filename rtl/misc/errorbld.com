$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !	ERRORBLD.COM -- Build all the vicar files containing error
$ !		        information.  Files built are:
$ !	
$ !		VIC2FAC.MSG -- Help for error messages (must be converted
$ !			       to VIC2FAC.INX by MSGBLD)
$ !		SYS_MSG.C   -- Vicar2 internal routine to issue error messages
$ !		VIC2ERRORS.TEX -- Section in VICAR user's guide describing
$ !				  possible VICAR2 errors
$ !		ERRDEFS.H    -- C include file defining symbolic names for
$ !			       errors
$ !		ERRDEFS.FIN  -- FORTRAN version of ERRDEFS.H
$ !
$ !	******** This command procedure MUST be executed before the      *****
$ !	******** shareable image may be linked or the user guide built!! *****
$ !
$ !	Errors are defined in ERRORS.DAT, and must contain each of the
$ !	following fields:
$ !
$ !		KEY    -- Key associated with error, used to find error in
$ !		          VIC2FAC help files
$ !		NUMBER -- Integer number associated with error.
$ !		NAME   -- Symbolic name for error used inside vicar2
$ !		MESSAGE -- Error message text given upon occurrence of error
$ !		HELP   -- Help text describing the meaning and user action
$ !			  needed for the error.
$ !
$ !	In addition, the following directive is recognized:
$ !
$ !		SKIP  -- Skip to the next error number.  Used to create
$ !			 an empty slot in the message tables.  When deleting
$ !			 an error from ERRORS.DAT, it should be replaced with
$ !			 the SKIP directive, so that the error numbers of the
$ !			 errors following it do not change.  In addition,
$ !			 when adding a new error to the tables, a SKIP may
$ !			 be replaced with the error message data.
$ !
$ !	Syntax rules for ERRORS.DAT are as follows.  All records containing
$ !	keywords will start with the string "%%", followed immediately by
$ !	the keyword, one space, and then the data associated with the keyword.
$ !	The one exception to this rule is HELP, which shall start a block
$ !	of text.  The text shall be determined to be over by either reaching
$ !	the end of file or by encountering the double percent sign at the
$ !	beginning of a record.
$ !
$ !	A modest attempt is made to ensure that the ERRORS.DAT file is
$ !	in the proper format.
$ !
$ !	Example:  (All lines must have the "%%" in the first column)
$ !
$ !		%%KEY UNDEFOPT
$ !		%%NUMBER -2
$ !		%%NAME UNDEFINED_OPTIONAL
$ !		%%MESSAGE Undefined optional argument; program error
$ !		%%HELP
$ !		Explanation:
$ !				explanation
$ !
$ !		User action:
$ !				action to take . . .
$ !		%%KEY . . .
$ !
$ !	and so on for each message.  All errors MUST be defined in
$ !	ERRORS.DAT.  Any new error information that is added must be
$ !	added only to ERRORS.DAT.  Any new modules making use of specific
$ !	error information must be added to this command procedure to be
$ !	built properly.
$ !
$ !	D. Stanfill, April 1986
$ !
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ !	Create the beginning of the source for each module
$ !
$ Create ERRDEFS.H
#ifndef ERRDEFS_H
#define ERRDEFS_H

/* ERRDEFS -- Definition of all symbolic names for VICAR2 internal
 * errors.  This module is used by the VICAR2 routines themselves
 * as well as application programs.
 *
 *	NOTE :  This file is built by ERRORBLD.COM.  It should never
 *		be modified directly.  To make changes to the source,
 *		change ERRORBLD.COM.
 *
 */
$ !
$ Create ERRDEFS.FIN
C ERRDEFS -- Definition of all symbolic names for VICAR2 internal
C errors.  This module is used by application programs to define
C symbolic names for the VICAR2 errors.  The values are the same
C as those used internally by VICAR2.
C
C	NOTE :  This file is built by ERRORBLD.COM.  It should never
C		be modified directly.  To make changes to the source,
C		change ERRORBLD.COM.
C
$ !
$ Create SYS_MSG.C
/* sys_msg -- write out a message from the vicar2 system describing
 * an error.
 *
 *	NOTE :  This file is built by ERRORBLD.COM.  It should never
 *		be modified directly.  To make changes to the source,
 *		change ERRORBLD.COM.
 *
 */
#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#define NO_UNIT_FLAG 0x100
#define IO_FLAG 0x400
#define MASK	0x0FF

void sys_msg(unit, code)
int unit, code;
{
   static char *rname[] =
   {
      "XVADD", "XVCLOSE", "XVGET", "XVOPEN", "XVREAD", "XVWRITE",
      "XLADD", "XLDEL", "XLGET", "XLHINFO", "XLINFO",	"XLNINFO",
      "XVUNIT", "XVCOMMAND", "XVPARM", "XVIPARM", "XVP",
      "XVPCNT", "XVIPCNT", "XVPSTAT", "XVIPSTAT", "XVPONE", "XVIPONE",
      "XLGETLABEL", "XVPARMD", "XVIPARMD",
      "XVTRANS_SET", "XVTRANS_IN", "XVTRANS_OUT", "XVTRANS_INU",
      "XVPIXSIZEU", "XVPIXSIZE", "XVTPINFO", "XVTPMODE", "XVTPSET", "XVFILPOS",
      "XVCMDOUT", "XVIP", "XVPOPEN", "XVPOUT", "XVHOST", "XVPIXSIZEB",
      "XVTRANS_INB", "XLPINFO"
   };

   static struct
   {
      char *key;
      char *text;
   } err_messages[] =
   {
$ !
$ Create VIC2FAC.MSG
!
!	Message file for facility VIC2
!
!  This file contains the message id's and detailed texts for
!  the Vicar2 internal errors.
!
!
!	NOTE :  This file is built by ERRORBLD.COM.  It should never
!		be modified directly.  To make changes to the source,
!		change ERRORBLD.COM.
!
!
$ !
$ Create vic2errors.tex
\newpage
\section{Error Messages}
\label{errors}
\subsection{Error message format}
This section describes the meaning of the VICAR error messages.
VICAR error messages are given in the following form:
\begin{quote}
[VIC2--{\em key}] {\em message}
\end{quote}
where VIC2 indicates that the message was issued from the VICAR2
package, and {\em key} is the specific key or identifier for the message
given.  The message key may be used to ask for help with the
HELP-MESSAGE command in the VICAR supervisor or to look up a message
in this section.  In addition, the key is stored internally by the 
supervisor, so that by simply typing a question mark (?) to the 
prompt, help on the last error message given is received.
\subsection{Messages by key}
This section lists VICAR2 error messages in alphabetical order by
key.  The accompanying message, the numerical value, and the symbolic
name by which the error may be referenced in a program are given,
followed by a detailed description of what the message means, and the
action required to correct the error.

\begin{enumerate}
$ !
$ Create BOTTOM.TEX	! This will go on the bottom of VIC2ERRORS
\end{enumerate}

\subsection{Messages by numerical value}
For easy reference, the VICAR2 error messages are listed here in
their numerical order, giving the key associated with the value, and
the symbolic name which may be used to reference it in a program.
The detailed help for each message may be found in the previous
section under the key name.
\begin{itemize}
$ !
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ !	Now start reading data file ERRORS.DAT and writing
$ !	the data to their respective files.  Each file will
$ !	have an output record which shall be built and finally
$ !	written to the file, except VIC2ERRORS.TEX, in which the errors
$ !	must be sorted alphabetically.  To do this, we shall
$ !	build a separate little file for each error message,
$ !	and then append them in alphabetical order to VIC2ERRORS.TEX.
$ !	The f$search() function will do this automatically for us.
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ !	Open the output files for appending
$ Open/append C_ERRDEFS		ERRDEFS.H
$ Open/append FOR_ERRDEFS	ERRDEFS.FIN
$ Open/append SYS_MSG		SYS_MSG.C
$ Open/append VIC2FAC		VIC2FAC.MSG
$ Open/append BOTTOM_TEX	BOTTOM.TEX
$ !
$ !	Now open the data file and start processing
$ !
$ cur_num = -2	! cur_num is to check the NUMBER field for consistency
$ Open/read ERROR_DATA		ERRORS.DAT
$FIND_TOP:
$ Read/end=EARLY_END ERROR_DATA in_record
$ If f$extract(0,6,in_record) .eqs. "%%SKIP" then cur_num = cur_num - 1
$ If f$extract(0,6,in_record) .nes. "%%KEY " then goto FIND_TOP
$ !
$TOP:		! At this point, in_record contains "%%KEY xxxx"
$ !
$ key = in_record - "%%KEY "
$ !
$ Read/end=EARLY_END ERROR_DATA in_record
$ expected = "%%NUMBER "
$ If f$extract(0,9,in_record) .nes. expected then Goto BAD_DIRECTIVE
$ number = in_record - "%%NUMBER "
$ If number .ne. cur_num then goto BAD_NUMBER
$ cur_num = cur_num - 1
$ !
$ Read/end=EARLY_END ERROR_DATA in_record
$ expected = "%%NAME "
$ If f$extract(0,7,in_record) .nes. expected then Goto BAD_DIRECTIVE
$ name = in_record - "%%NAME "
$ !
$ Read/end=EARLY_END ERROR_DATA in_record
$ expected = "%%MESSAGE "
$ If f$extract(0,10,in_record) .nes. expected then Goto BAD_DIRECTIVE
$ message = in_record - "%%MESSAGE "
$ !
$ !	Now write the records to the files without the help text
$ !
$ Write SYS_MSG "/*''number'*/ ""''key'"",""''message'"","
$ !
$ Write C_ERRDEFS "#define ''name' ''number'"
$ !
$ Write FOR_ERRDEFS "      INTEGER ''name'"
$ Write FOR_ERRDEFS "      PARAMETER (''name' = ''number')"
$ !
$ Write BOTTOM_TEX "\item ''number' Key: ''key' \\"
$ Write BOTTOM_TEX "Symbolic Name: ''name'"
$ !
$ !	Now start writing detailed help to appropriate files
$ !
$ Read/end=EARLY_END ERROR_DATA in_record
$ expected = "%%HELP"
$ If f$extract(0,6,in_record) .nes. expected then Goto BAD_DIRECTIVE
$ Write VIC2FAC ".KEY ''key'	! ''name'"
$ create 'key'.TMP_TEX

$ Open/append TMP_TEX 'key'.TMP_TEX
$ Write TMP_TEX "\item ''key' Symbolic Name: ''name'"
$ Write TMP_TEX ""
$ Write TMP_TEX "[VIC2-''key'] ''message'"
$ Write TMP_TEX ""
$ !
$ !	Loop through writing help text
$ !
$WRT_HLP:
$ in_record = "  "
$ Read/end=HLP_DONE ERROR_DATA in_record
$ If f$extract(0,2,in_record) .eqs. "%%" then Goto HLP_DONE
$ Write VIC2FAC in_record
$ Write TMP_TEX in_record
$ Goto WRT_HLP
$ !
$ !	All the text is written, now polish off temp file
$HLP_DONE:
$ Close TMP_TEX
$ If f$extract(0,2,in_record) .nes. "%%" then Goto DONE
$ !
$ !	Get the next directive -- must be either SKIP or KEY.
$ !	If SKIPs are encountered, keep skipping until KEY is found.
$ !
$CHECK_SKIP:
$ If f$extract(0,6,in_record) .nes. "%%SKIP" then Goto CHECK_KEY
$ cur_num = cur_num - 1
$ Write SYS_MSG "0,0,"
$ Read/end=DONE ERROR_DATA in_record
$ Goto CHECK_SKIP
$CHECK_KEY:
$ expected = "%%KEY "
$ If f$extract(0,6,in_record) .nes. expected then Goto BAD_DIRECTIVE
$ Goto TOP
$ !
$DONE:		! At this point, the end of ERRORS.DAT has been reached
$ !
$ !	Now we need to tack the end of each file if needed onto what
$ !	has been created so far, and then build the VIC2ERRORS.TEX file
$ !	from all the .TMP_TEX files.
$ !
$ Close ERROR_DATA		! Close the input
$ !
$ Close SYS_MSG			! Need to close it before appending tail
$ append sys$input SYS_MSG.C
0,0		/* Terminator entry */
   };

   static char msgbuf[200];
   int i;

   if ((current_call & NO_UNIT_FLAG) || (unit == NO_UNIT))
      sprintf(msgbuf, "Exception in %s", rname[MASK&current_call]);
   else
      sprintf(msgbuf, "Exception in %s, processing file: %s",
              rname[MASK&current_call], CURRENT_S_VALUE(NAME));
   zvmessage(msgbuf, "VIC2-GENERR");

   if (code<0) {
      if ((code >= LAST_ERROR) && (err_messages[-code-2].key != 0)) {
	 char err_key[18];

	 strcpy(err_key,"VIC2-");
	 strcat(err_key,err_messages[-code-2].key);

	 zvmessage(err_messages[-code-2].text, err_key);
      }
      else {
	 sprintf(msgbuf,
	    "Unrecognized error status %d;  Notify system programmer", 
	    code);
	 zvmessage(msgbuf, "VIC2-BADSTAT");
      }
   }
   else	{	/* if status is positive, it is a system error, so ask	*/
    		/* the system for the message.				*/
      hostmsg(code, msgbuf+1, 199);		/* get the error msg */

      msgbuf[0] = (msgbuf[1] == '%') ? '%' : ' ';  /* protect C string from % */
					     /* 'cuz TAE runs through sprintf */
      zvmessage(msgbuf, "VIC2-HOSTMSG");
   }
   if (current_call & IO_FLAG) {		/* if read/write */
      sprintf(msgbuf, " Current line in image = %d", CURRENT_I_VALUE(IMG_REC));
      zvmessage(msgbuf, " ");
   }

   return;
}
$ !
$ cur_num = cur_num + 1
$ Write C_ERRDEFS "#define LAST_ERROR ''cur_num'" ! Needed by V2 internals
$ Write C_ERRDEFS ""
$ Write C_ERRDEFS "#endif /* ERRDEFS_H */"
$ Close C_ERRDEFS
$ Write FOR_ERRDEFS "      INTEGER LAST_ERROR"
$ Write FOR_ERRDEFS "      PARAMETER (LAST_ERROR = ''cur_num')"
$ Close FOR_ERRDEFS
$ Close VIC2FAC
$ Append sys$input VIC2FAC.MSG
.KEY GENERR	! general error
Explanation:

This message is informational, indicating that some error has
occurred.  It should always be accompanied by a detailed error message.

User action:

Look at the accompanying message to find out what the error is
and act accordingly.

.KEY HOSTMSG	! A host dependent error occurred, message to follow
Explanation:

This is an informational message only.  A host dependent error
occurred, and the message is given.  The host message (and
probably a stack trace if under VMS) should appear immediately
following this message.

User action:

Interpret the host error message and act accordingly.

.KEY BADSTAT	! Unrecognized error status
Explanation:

VICAR's internal error handler has been asked to issue a message
for a status it does not recognize.  This is probably a failure
on the part of the system programmer to keep the executive error
modules up to date.

User action:

Record the status value given and notify the system programmer
immediately.

$ !
$ !	Now everything is done except the LaTeX file, VIC2ERRORS.TEX.
$ !	Fetch the .TMP_TEX files one at a time and append them to
$ !	the VIC2ERRORS file.
$ !
$TEX_BUILD:
$ spec = f$search("*.TMP_TEX")
$ If spec .eqs. "" then Goto TEX_DONE
$ Append 'spec' vic2errors.tex
$ Goto TEX_BUILD
$TEX_DONE:
$ Close BOTTOM_TEX
$ Append BOTTOM.TEX VIC2ERRORS.TEX
$ Append sys$input VIC2ERRORS.TEX
\end{itemize}
\end{document}
$ !
$ edit/edt/comm=sys$input vic2errors.tex
s:_:\_:who
exit
$ purge vic2errors.tex
$ delete bottom.tex.*
$ delete *.tmp_tex.*
$ Type sys$input
****************************************
*                                      *
* ERRORBLD.COM:                        *
* All files successfully created       *
*                                      *
****************************************
$ Exit
$ !
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ !	Here are the error condition exits
$ !
$EARLY_END:
$ Type sys$input
**** Premature end of file encountered processing ERRORS.DAT
**** Check file contents
$ Goto PLS_FIX
$BAD_DIRECTIVE:
$ Write sys$output "**** Bad directive in ERRORS.DAT"
$ Write sys$output "**** Expected ''expected',"
$ Write sys$output "**** Found ''in_record'"
$ Goto PLS_FIX
$BAD_NUMBER:
$ Write sys$output "**** Numbering sequence in ERRORS.DAT is bad"
$ Write sys$output "**** Found ''number' when expecting ''cur_num'"
$PLS_FIX:
$ Write sys$output ""
$ Write sys$output "Please see documentation in ERRORBLD.COM and fix"
$ Write sys$output "ERROR file build aborted"
$CLEANUP:
$ If f$logical("C_ERRDEFS")   .nes. "" then Close C_ERRDEFS
$ If f$logical("FOR_ERRDEFS") .nes. "" then Close FOR_ERRDEFS
$ If f$logical("SYS_MSG")     .nes. "" then Close SYS_MSG
$ If f$logical("VIC2FAC")     .nes. "" then Close VIC2FAC
$ If f$logical("ERROR_DATA")  .nes. "" then Close ERROR_DATA
$ Exit $status
