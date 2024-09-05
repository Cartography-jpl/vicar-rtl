//  Michael Brady
//  September 25, 2000
//  adc_Body.cc

#include "adc_Body.h"


adc_Body::adc_Body (jpl::mipl::spice::corba::SpiceLib_ptr spiceLib,
                    Code body)
{
   m_spiceLib = jpl::mipl::spice::corba::SpiceLib::_duplicate(spiceLib);
   set(body);
}

adc_Body::adc_Body(jpl::mipl::spice::corba::SpiceLib_ptr spiceLib)
{
   m_spiceLib = jpl::mipl::spice::corba::SpiceLib::_duplicate(spiceLib);
   set(EARTH);
}

int adc_Body::getNaifId() const
{
   return m_naifId;
}

const char* adc_Body::getName()
{
   CORBA::Boolean wasFound = 0;

   m_spiceLib->bodc2n(getNaifId(), m_name.out(), wasFound);

   return m_name.in();
   
}

void adc_Body::set (const char* name, int& wasFound)
{
   int id = 0;
   CORBA::Boolean found = 0;

   m_spiceLib->bodn2c(name, id, found);
    
   if (found)
   {
      m_naifId = id;
      wasFound = 1;
   }
   else
   {
      wasFound = 0;
   }
}

void adc_Body::set(int bodyId)
{
   m_naifId = bodyId;
}
