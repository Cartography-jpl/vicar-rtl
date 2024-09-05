/*  This is the gtprcs subroutine for all Operating Systems.
    This subroutine returns a pointer to the current user's real 
    name.  If the user's real name cannot be found then a blank
    character string is returned.                                 */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "vmachdep.h"

/*    Fortran Callable Subroutine    */

char F77_FUNC(gtprcs, GTPRCS)
(char s[8], ZFORSTR_PARAM)
{
     ZFORSTR_BLOCK
     char c_string[8];
     zgtprcs(c_string);
     zsc2for(c_string,8,s,&s,1,1,1, s);
}

/*    C-Callable Subroutine     */

zgtprcs(s)
char s[8];
{
     char *cuserid_p2();

     {
          strcpy(s,cuserid_p2());
     }

}
