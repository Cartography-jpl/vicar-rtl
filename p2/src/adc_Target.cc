// adc_Target.cc
//
// Apr 25, 2002
// Michael Brady

#include "adc_Target.h"

adc_Target::adc_Target(jpl::mipl::spice::corba::Target* target)
   : m_target(target)
{
}

adc_Target::~adc_Target()
{
}

const adc_Vector3& adc_Target::radii() const
{
   return m_target->radii;
}


double adc_Target::rotationRate() const
{
   return m_target->rotationRate;
}
