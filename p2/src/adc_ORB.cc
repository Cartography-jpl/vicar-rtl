// adc_ORB.h
//
//  April 9, 2002
//  Michael Brady

#include "adc_ORB.h"



static CORBA::ORB_var g_orb = CORBA::ORB::_nil();

void adc_ORB::init(int argc, char** argv)
{
    if (CORBA::is_nil(g_orb.in()))
	{
	    g_orb = CORBA::ORB_init (argc, argv, 0);
	}
}

   
CORBA::ORB_ptr adc_ORB::instance()
throw ()
{
    return g_orb.in();
}

