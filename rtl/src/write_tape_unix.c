#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <sys/file.h>
#include <errno.h>

/* Write a number of blocks out to a foreign tape */

int v2_write_tape(state, buf, block, nblocks, async_flag, transfer_count)
struct tapestate *state;
char *buf;
V2_OFFSET block, nblocks;
int async_flag;
int *transfer_count;
{    
   int bytes, code, i;
   int blocksize;
   int index;
   int delta;

   index = state->tindex;
   delta = block+1 - i_rec[index];	/* i_rec is 1 based, block is 0 based */

   if (delta != 0) {
      if (space_record(state, delta) != SUCCESS)      /* updates i_rec[index] */
         return TAPE_POSITIONING_ERROR;
   }

   for (i=0; i<nblocks; i++) {
      blocksize = state->blocksize;
      if ((blocksize & 1) == 1)	/* Odd size tape block, so make it even */
         blocksize++;		/* since the xt driver can't do odd blocks */
      code = write(state->channel, buf, blocksize);
      if (code == -1)
         return errno;
      if ((state->blocksize & 1) == 1 && code != 0)
         code--;			/* compensate for odd blocks */
      if (code != state->blocksize)
         return END_OF_VOLUME;		/* what else could cause this? */
      bytes += code;
      buf += code;
   }

   state->pos += bytes;
   *transfer_count = bytes;
   i_rec[index] += nblocks;

   return SUCCESS;
}

#else

int v2_write_tape(void)
{
   return NO_TAPE_SUPPORT;
}

#endif

