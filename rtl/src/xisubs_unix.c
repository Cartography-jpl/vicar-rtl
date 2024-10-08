#include "xvmaininc.h"

#if RTL_USE_TAPE
#include "taeconf.inp"
#endif

#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

/* Low-level tape positioning subroutines */

/************************************************************************/
/* Position tape to absolute file position.  Note that we don't trust	*/
/* i_file to be correct before the file is opened, because the lack of	*/
/* an exit handler means a program crash will mess up i_file.		*/
/************************************************************************/

int i_position_tape (channel, index, filenr)
int channel;		/* in: channel number		*/
int index;		/* in: index in tape tables	*/
int filenr;		/* in: file number		*/

{
   int skip;
   int code;
   struct mtget info;

   if (i_file[index] == 0) {		/* tape position unknown */
      if (filenr == 0)			/* if asking for next file... */
         return EINVAL;			/* a bad idea */
      else {
         code = i_rewind (channel, index);	/* get to known position */
         if (code != SUCCESS)
            return code;
      }
   }

   if (filenr == 0)			/* use "next" file */
      filenr = i_nxt_file[index];

   if (filenr == 1) {			/* use first file, so rewind */
      code = i_rewind (channel, index);
      if (code != SUCCESS)
         return code;
   }
   else {				/* use Nth file, so file skip */
      code = ioctl(channel, MTIOCGET, &info); /* get file number from driver */
      if (code == -1) {
         code = errno;
         i_rewind(channel,index);
         return code;
      }
      skip = filenr - (info.mt_fileno+1);	/* files to skip */
      code = i_space_file (channel, index, skip);
      if (code != SUCCESS)
         return code;
   }
   i_nxt_file[index] = filenr;		/* new "next" file */

   return SUCCESS;
}

/************************************************************************/
/* Rewind tape.								*/
/************************************************************************/

int i_rewind (channel, index)
int channel;			/* in: channel (file descriptor) of tape */
int index;			/* in: tape index */

{
   int code;
   short iosb[4];
   struct mtop op;

   op.mt_op = MTREW;
   op.mt_count = 1;
   code = ioctl(channel, MTIOCTOP, &op);

   if (code == -1) {
      i_rec[index] = 0;		/* tape position not known */
      i_file[index] = 0;
      return errno;
   }

   i_rec[index] = 1;		/* beginning of tape */
   i_file[index] = 1;
   i_nxt_file[index] = 1;	/* new "next" file */

   return SUCCESS;
}

/************************************************************************/
/* Space forward or back file marks.					*/
/************************************************************************/

int i_space_file (channel, index, files)
int channel;		/* in: channel # of tape		*/
int index;		/* in: index in tape tables		*/
int files;		/* in: number of files; negative means backwards */

{
   int code;
   struct mtop op;
   struct mtget info;
   int i;

   errno = 0;

   if (files > 0) {
      op.mt_op = MTFSF;
      op.mt_count = files;
      code = ioctl(channel, MTIOCTOP, &op);
   }
   else {
      op.mt_op = MTNBSF;
      op.mt_count = -files;			/* Make it positive */
      code = ioctl(channel, MTIOCTOP, &op);
   }

   if (code == -1) {				/* tape position unknown */
      op.mt_op = MTREW;
      op.mt_count = 1;
      i = errno;
      ioctl(channel, MTIOCTOP, &op);	/* rewind to restore to a sane state */

      i_rec[index] = 1;
      i_file[index] = 1;
      return i;
   }

   /* Get the true tape position from the driver to make sure it's	*/
   /* correct, since i_file can be wrong if a program crashed.		*/

   code = ioctl(channel, MTIOCGET, &info);	/* get true tape position */
   if (code == -1) {
      op.mt_op = MTREW;
      op.mt_count = 1;
      i = errno;
      ioctl(channel, MTIOCTOP, &op);	/* rewind to restore to a sane state */

      i_rec[index] = 1;
      i_file[index] = 1;
      return i;
   }

   i_rec[index] = 1;
   i_file[index] = info.mt_fileno+1;	/* new file number from driver */

   i_nxt_file[index] = i_file[index];	/* new "next" file */

   return SUCCESS;
}

/************************************************************************/
/* Backspace tape one record.  Note that eof is considered SUCCESS.	*/
/************************************************************************/

int i_back_space (channel, index)
int channel;			/* in: tape channel number */
int index;			/* in: index in tape tables */

{
   int code;
   struct mtop op;

   op.mt_op = MTBSR;
   op.mt_count = 1;

   code = ioctl(channel, MTIOCTOP, &op);

   if (code == -1) {
      i_rec[index] = 0;			/* tape position not known */
      i_file[index] = 0;
      return errno;
   }

   i_rec[index]--;

   return SUCCESS;
 }

/************************************************************************/
/* Space forward n records.						*/
/************************************************************************/

int i_space_record (channel, index, records)
int channel;			/* in: channel number of tape	*/
int index;			/* in: index in tape tables	*/
int records;			/* in: number of records	*/

{
   int code;
   struct mtop op;

   op.mt_op = MTFSR;
   op.mt_count = records;
   code = ioctl(channel, MTIOCTOP, &op);

   if (code == -1) {
      i_rec[index] = 0;
      i_file[index] = 0;
      return errno;
   }

   i_rec[index] += records;

   return SUCCESS;
}

#else

int i_position_tape(void)
{
   return NO_TAPE_SUPPORT;
}

int i_rewind (void)
{
   return NO_TAPE_SUPPORT;
}

int i_space_file (void)
{
   return NO_TAPE_SUPPORT;
}

int i_back_space (void)
{
   return NO_TAPE_SUPPORT;
}

int i_space_record (void)
{
   return NO_TAPE_SUPPORT;
}

#endif

