// adc_InstrumentData.cc
//
// Apr 25, 2002
// Michael Brady

#include "adc_InstrumentData.h"

adc_InstrumentData::
adc_InstrumentData(jpl::mipl::spice::corba::InstrumentData* instrument)
   : m_instrument(instrument)
{
}

adc_InstrumentData::~adc_InstrumentData()
{
}

double adc_InstrumentData::cameraBoresightOffset() const
{
   return m_instrument->cameraBoresightOffset;
}

double adc_InstrumentData::focalLength() const
{
   return m_instrument->focalLength;
}

double adc_InstrumentData::pictureScale() const
{
   return m_instrument->pictureScale;
}

double adc_InstrumentData::opticalAxisOffset() const
{
   return m_instrument->opticalAxisOffset;
}
