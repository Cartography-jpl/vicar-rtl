/************************************************************************
 * get_seconds_big.c 
 ************************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>
#include <stdio.h>
#include <time.h>
/*#include <unistd.h>  */         /* for sleep function */
#define Y2K_CORRECTION 0X7FFFFFFF

/* Y2K_CORRECTION will turn off the first bit when time returned by
   time() function will be greater then 2**31 - 1, thus forcing the
   value to be positive in 2's complement for the calling fortran
   program. */

#define TEST_PURPOSE   0X00000000

/* Only used for purpose of testing. Set the flag to some hex value such
   as 0XC0000000 to forward time in future. */

/* prototypes */
void F77_FUNC_(get_seconds_big, GET_SECONDS) (long int *sec_out);

/************************************************************************/
/* Fortran-Callable Version (no C-version needed)                       */
/************************************************************************/


void F77_FUNC_(get_seconds_big, GET_SECONDS)
(sec_out)
long *sec_out;
{
  time_t thetime;
  char msg[100];
  thetime = time(NULL);

  if(TEST_PURPOSE) {
     zvmessage("******************************************************","");
     zvmessage("*** This is test of get_secondsi_big for Y2K only. ***","");
     zvmessage("*** This is simulation of how program will         ***","");
     zvmessage("*** behave once the time function starts           ***","");
     zvmessage("*** returning values greater then 2**31-1          ***","");
     zvmessage("******************************************************","");
     sprintf(msg,"Time returned by time function is %u",(int)thetime);
     zvmessage(msg,NULL);
     thetime+=TEST_PURPOSE;
  }

//  *sec_out = (long) (thetime & Y2K_CORRECTION);
/* 
   sleep was called here for a test which demonstrated that successive
   calls to vicar programs using get_seconds_big  yields the same time.
   hence, seed value. This is due today's fast GHz processors 
   See warnings in programs such as gausnois
*/
/*    sleep (2);        */
    *sec_out = (long) (thetime);
  if(TEST_PURPOSE) {
    sprintf(msg,"Time after offseted by get_seconds_big is %u",(int)thetime);
     zvmessage(msg,NULL);
     sprintf(msg,"Time after corrected by get seconds_big is %d",(int)*sec_out);
     zvmessage(msg,NULL);
  }

  return;
}
