// adc_types.h
//
//  This file brings the corba-defined types into the adc_ namespace.

#include "jpl_mipl_spice_corba.C.h"

/** An alias for double[3] */
typedef jpl::mipl::spice::corba::Vector3 adc_Vector3;

/** An alias for double[3][3] */
typedef jpl::mipl::spice::corba::Matrix33 adc_Matrix33;
