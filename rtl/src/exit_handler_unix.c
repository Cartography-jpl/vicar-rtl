#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"

/* Declare an exit handler */

int exit_handler(void (*func)(int,void*))
{

#if ON_EXIT_AVAIL_OS

   if (on_exit(func, 0) == 0)
      return SUCCESS;

   return FAILURE;

#else

   return FAILURE;

#endif

}

