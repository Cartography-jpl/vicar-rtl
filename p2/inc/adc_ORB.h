// adc_ORB.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADC_ORB_H_INCLUDED
#define ADC_ORB_H_INCLUDED

#include "tao/ORB.h"


class adc_ORB
{
    public:
    /**
     *  The first time this is called, creates a new ORB object initialized
     *  to the specified parameters.
     *  Subsequent calls have no effect.
     */
    static void init(int argc, char** argv);
    
    /**
     *  Return an instance of the one and only ORB object.
     *
     *  @return the ORB if init() has been called, otherwise nil.
       */
    static CORBA::ORB_ptr instance()
    throw();
};


#endif // !defined ADC_ORB_H_INCLUDED
