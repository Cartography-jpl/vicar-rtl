/* C-bridge for FORTRAN routines init_spice */
#include "xvmaininc.h"
#include "ftnbridge.h"

initspice()
{
  F77_FUNC_(init_spice, INIT_SPICE) ();
}
