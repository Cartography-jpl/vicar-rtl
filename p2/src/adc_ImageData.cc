// adc_ImageData.cc
//
// Apr 12, 2002
// Michael Brady

#include "adc_ImageData.h"

adc_ImageData::adc_ImageData ( jpl::mipl::spice::corba::MiplSpiceLib_ptr spiceLib,
                               jpl::mipl::spice::corba::Instrument instrument,
                               jpl::mipl::spice::corba::SpInt target,
                               const char* utcScet )
   :  m_geometryHasBeenFound(0),
      m_criteria(),
      m_instDataHasBeenFound(0),
      m_targetHasBeenFound(0)
{
   m_spiceLib = jpl::mipl::spice::corba::MiplSpiceLib::_duplicate(spiceLib);

   m_image.inst = instrument;
   m_image.target = target;
   m_image.utcTime = CORBA::string_dup(utcScet);
   m_image.referenceFrame = CORBA::string_dup("J2000");
   m_image.tolerance = -1;
}

adc_ImageData::
adc_ImageData ( jpl::mipl::spice::corba::MiplSpiceLib_ptr spiceLib,
                jpl::mipl::spice::corba::Instrument instrument,
                jpl::mipl::spice::corba::SpInt target,
                const char* utcScet,
                const jpl::mipl::spice::corba::GeometryMetaData& criteria )
   :  m_geometryHasBeenFound(0),
      m_criteria(criteria),
      m_instDataHasBeenFound(0),
      m_targetHasBeenFound(0)
{
   m_spiceLib = jpl::mipl::spice::corba::MiplSpiceLib::_duplicate(spiceLib);

   m_image.inst = instrument;
   m_image.target = target;
   m_image.utcTime = CORBA::string_dup(utcScet);
   m_image.referenceFrame = CORBA::string_dup("J2000");
   m_image.tolerance = -1;
}


adc_ImageData::~adc_ImageData()
{
}

adc_Geometry adc_ImageData::getGeometry()
{
   if (!m_geometryHasBeenFound)
   {
      m_spiceLib->getGeometry(m_image, 
                              m_criteria, 
                              m_geometry, 
                              m_geometryMetaData.out());         
      
      m_geometryHasBeenFound = 1;
   }

   return adc_Geometry(m_geometry, m_geometryMetaData.in());
}

void adc_ImageData::updateGeometry(const adc_Geometry& update)
{
   m_spiceLib->storeGeometry(m_image,
                             update.m_geometry,
                             update.m_metaData.m_metaData);
}

adc_InstrumentData adc_ImageData::getInstrumentData()
{
   if (!m_instDataHasBeenFound)
   {
      m_spiceLib->getInstrument(m_image, m_instData );
      m_instDataHasBeenFound = 1;
   }

   return adc_InstrumentData(&m_instData);
}

adc_Target adc_ImageData::getTarget()
{
   if (!m_targetHasBeenFound)
   {
      m_spiceLib->getTarget(m_image, m_target);
      m_targetHasBeenFound = 1;
   }
   
   return adc_Target(&m_target);
}
