// adc_Geometry.cc
//
// Apr 12, 2002
// Michael Brady

#include "adc_Geometry.h"

adc_Geometry::
adc_Geometry(const jpl::mipl::spice::corba::Geometry& geometry,
             const jpl::mipl::spice::corba::GeometryMetaData& metaData)
   : m_geometry(geometry),
     m_metaData(metaData)
{
}

adc_Geometry::~adc_Geometry()
{
}


const adc_GeometryMetaData& adc_Geometry::metaData() const
{
   return m_metaData;
}

void adc_Geometry::setMetaData(const adc_GeometryMetaData& metaData)
{
   m_metaData = metaData;
}

const adc_Vector3& adc_Geometry::spacecraftVector() const
{
   return m_geometry.spacecraftVector;
}

const adc_Vector3& adc_Geometry::solarVector() const
{
   return m_geometry.solarVector;
}

const adc_Vector3& adc_Geometry::planetVector() const
{
   return m_geometry.planetVector;
}

const adc_Vector3& adc_Geometry::spacecraftVelocityVector() const
{
   return m_geometry.spacecraftVelocityVector;
}

const adc_Matrix33& adc_Geometry::instrumentPointing() const
{
   return m_geometry.instrumentPointing;
}

const adc_Matrix33& adc_Geometry::targetOrientation() const
{
   return m_geometry.targetOrientation;
}


void adc_Geometry::setSpacecraftVector(const adc_Vector3& v)
{
   memcpy(m_geometry.spacecraftVector, v, sizeof(adc_Vector3));
}

void adc_Geometry::setSolarVector(const adc_Vector3& v)
{
   memcpy(m_geometry.solarVector, v, sizeof(adc_Vector3));
}

void adc_Geometry::setPlanetVector(const adc_Vector3& v)
{
   memcpy(m_geometry.planetVector, v, sizeof(adc_Vector3));
}

void adc_Geometry::setSpacecraftVelocityVector(const adc_Vector3& v)
{
   memcpy(m_geometry.spacecraftVelocityVector, v, sizeof(adc_Vector3));
}
   
void adc_Geometry::setInstrumentPointing(const adc_Matrix33& m)
{
   memcpy(m_geometry.instrumentPointing, m, sizeof(adc_Matrix33));
}

void adc_Geometry::setTargetOrientation(const adc_Matrix33& m)
{
   memcpy(m_geometry.targetOrientation, m, sizeof(adc_Matrix33));
}
   
