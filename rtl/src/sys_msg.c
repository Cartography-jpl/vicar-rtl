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
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include <string.h>

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
/*-2*/ {"UNDEFOPT","Undefined optional argument; program error"},
/*-3*/ {"NOFREUN","No free units available"},
/*-4*/ {"INSUFMEM","Insufficient memory; consult system programmer"},
/*-5*/ {"STOROPT","Unable to store optional; consult system programmer"},
/*-6*/ {"NOSCHUN","No such unit; probable error in unit number"},
/*-7*/ {"MODOPNUN","An open unit cannot be modified; call XVCLOSE first"},
/*-8*/ {"NONASC","String has non-ASCII characters"},
/*-9*/ {"BADSIZ","Improper image size parameter; program error"},
/*-10*/ {"BADMETH","Improper METHOD string; program error"},
/*-11*/ {"BADOPSTR","Improper OP string; program error"},
/*-12*/ {"BADFOR","Improper FORMAT string; program error"},
/*-13*/ {"ODDOPT","Unpaired optionals; program error"},
/*-14*/ {"ALROPN","Attempt to open an open file; program error"},
/*-15*/ {"BADNAM","Bad file parameter name; program error"},
/*-16*/ {"NOTERM","Terminal not allowed for file name, use another name"},
/*-17*/ {"SIZREQ","Image size required; re-enter command"},
/*-18*/ {"BADDIM","Improper dimension; program error"},
/*-19*/ {"NOARRAY","Array i/o not allowed to non-disk device"},
/*-20*/ {"SECDEL","Unable to free array file, consult system programmer"},
/*-21*/ {"WAITFL","I/O wait fail; consult system programmer"},
/*-22*/ {"BUG","Internal VICAR bug check failure - Notify system programmer"},
/*-23*/ {"NOEFLG","Unable to get an event flag; re-try or consult system programmer"},
/*-24*/ {"BADCVT","Improper CONVERT or BIN_CVT string; program error"},
/*-25*/ {"NOTOPN","File not open; program error"},
/*-26*/ {"BADPARINST","Parameter instance not found in XVPONE"},
/*-27*/ {"NOTAVAIL","Function is not yet implemented;  Program error."},
/*-28*/ {"BADORG","ORG keyword (file organization) is not valid."},
/*-29*/ {"BADLBL","Bad input label; check file contents"},
/*-30*/ {"EOF","End of file"},
/*-31*/ {"OPNINP","Unable to open primary input; check file specification"},
/*-32*/ {"BADINTFMT","Invalid INTFMT or BINTFMT string"},
/*-33*/ {"BADOPR","Operation conflicts with open attributes; program error"},
/*-34*/ {"HSTNTASC","History name has non-ASCII characters; program error"},
/*-35*/ {"BADHOST","Invalid HOST string"},
/*-36*/ {"BADREALFMT","Invalid REALFMT or BREALFMT string"},
/*-37*/ {"NOTASK","No tasks in label; check file contents or create new label"},
/*-38*/ {"FNDKEY","Cannot find key; program error"},
/*-39*/ {"BADLBLTP","Bad label type; check file contents"},
/*-40*/ {"ORGMSMTCH","File organization is not that required by this program"},
/*-41*/ {"LNGMESS","Error message too long; program error"},
/*-42*/ {"NOMEM","No memory for label process; consult system programmer"},
/*-43*/ {"STRTREC","Bad starting record for read or write operation; program error."},
/*-44*/ {"NOSCHTSK","No such task in label"},
/*-45*/ {"NOKEY","No such key in the indicated task"},
/*-46*/ {"NOLBL","No system label; check file contents"},
/*-47*/ {"BADINST","Illegal instance; program error"},
/*-48*/ {"ILLFOREQ","Illegal format request; program error"},
/*-49*/ {"CONVERR","Conversion error; program error"},
/*-50*/ {"HOSTLONG","HOST or BHOST name too long; program error"},
/*-51*/ {"BADLINST","Improper label instance number; program error"},
/*-52*/ {"MODINPLBL","Attempt to modify input label; program error"},
/*-53*/ {"BADLEN","Improper length; program error"},
/*-54*/ {"BADSAMP","Improper sample size parameter; program error"},
/*-55*/ {"BADLINE","Improper line size parameter; program error"},
/*-56*/ {"NULLREQ","Null request; program error"},
/*-57*/ {"EOLAB","End of label"},
/*-58*/ {"TAPOPR","Tape cannot be opened for update."},
/*-59*/ {"TAPMETH","Tape cannot be opened for random access."},
/*-60*/ {"BADBAND","Improper band size parameter; program error"},
/*-61*/ {"TAPPOS","Tape positioning error; check drive status"},
/*-62*/ {"PARMVERS","Unrecognized version number in PARMS file"},
/*-63*/ {"TOOLATE","Attempt to modify tape label after write; program error"},
/*-64*/ {"NOLAB","File has no label; check file contents"},
/*-65*/ {"DUPKEY","Duplicate key; program error"},
/*-66*/ {"BADBINSIZ","Improper binary size parameter; program error"},
/*-67*/ {"LNGCOND","COND string too long; program error"},
/*-68*/ {"LNGACT","ACT string too long; program error"},
/*-69*/ {"ERRACT","Bad error action; program error"},
/*-70*/ {"BADELEM","Improper element number; program error"},
/*-71*/ {"FORREQ","The FORMAT optional is required with XLADD"},
/*-72*/ {"VARREC","COND=VARREC must have NOLABELS, NOBLOCK and tape"},
/*-73*/ {"BADFILE","Bad file number for tape"},
/*-74*/ {"NOTMULT","Tape blocksize is not an integral number of records"},
/*-75*/ {"ENDOFVOL","End of volume (double tape mark) reached"},
/*-76*/ {"NOTAPE","Tapes are not supported in this build of the RTL"},
/*-77*/ {"NOTPARMFILE","File specified in PARMS is not a parameter file"},
/*-78*/ {"MULTPARMFILE","Multiple parameter files cannot be open at once"},
/*-79*/ {"FILETYPE","Invalid file type"},
/*-80*/ {"BADTRANS","Invalid format translation"},
/*-81*/ {"NOTAE","TAE is not supported in this build of the RTL"},
/*-82*/ {"NONSEQWRIT","A non-sequential write was attempted on a sequential-only device"},
/*-83*/ {"NOTMOUNTED","A file open was attempted on a tape device that is not mounted"},
/*-84*/ {"XVCMDERR","Internal error in XVCOMMAND"},
/*-85*/ {"XVCMDFAIL","The command submitted via XVCOMMAND had an error"},
/*-86*/ {"PARNOTFND","A program parameter was not found in the PDF"},
/*-87*/ {"BLTYPELONG","BLTYPE name too long; program error"},
/*-88*/ {"PARBLKERR","Internal error in GET_PARM"},
/*-89*/ {"BADMODESTR","Improper MODE string; program error"},
/*-90*/ {"NOSCHPROP","No such property in label"},
/*-91*/ {"PROPREQ","The PROPERTY optional is required for property label routines"},
/*-92*/ {"UNDEFENV","Undefined environment variable in filename"},
/*-93*/ {"UNDEFUSR","Undefined user name in filename"},
/*-94*/ {"UNCLSBRC","A closing brace was not found in a filename"},
/*-95*/ {"TRUNCNAME","A filename was truncated due to insufficient buffer space"},
/*-96*/ {"BADUPDHIST","Improper UPD_HIST string; program error"},
/*-97*/ {"TOOBIG","File too big for this version of VICAR"},
/*-98*/ {"INVCMPR","Undefined compression type"},
/*-99*/ {"UNSFMTCMP","unsupported format type for compression"},
/*-100*/ {"UNSORGCMP","unsupported org type for compression"},
/*-101*/ {"UNSOPCMP","unsupported option for compression"},
/*-102*/ {"UNSDEVCMP","unsupported device for compression"},
/*-103*/ {"UNSBINCMP","unsupported binary header or prefix for compression"},
{0,0}		/* Terminator entry */
   };

   static char msgbuf[200];

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
