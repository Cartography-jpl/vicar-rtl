// adc_GeometryMetaData.cc
//
// Apr 25, 2002
// Michael Brady

#include "adc_GeometryMetaData.h"

adc_GeometryMetaData::adc_GeometryMetaData()
{
}

adc_GeometryMetaData::
adc_GeometryMetaData(const jpl::mipl::spice::corba::GeometryMetaData& src)
   : m_metaData(src)
{
}


adc_GeometryMetaData::~adc_GeometryMetaData()
{
}

adc_GeometryMetaData::adc_GeometryMetaData(const adc_GeometryMetaData& src)
   : m_metaData(src.m_metaData)
{
}

adc_GeometryMetaData& adc_GeometryMetaData::operator=(const adc_GeometryMetaData& rhs)
{
   if (this != &rhs)
   {
      m_metaData = rhs.m_metaData;
   }

   return *this;
}

const char* adc_GeometryMetaData::user() const
{
   return m_metaData.user.in();
}

const char* adc_GeometryMetaData::facility() const
{
   return m_metaData.facility.in();
}

const char* adc_GeometryMetaData::application() const
{
   return m_metaData.application.in();
}

const char* adc_GeometryMetaData::notes() const
{
   return m_metaData.notes.in();
}

void adc_GeometryMetaData::setUser(const char* user)
{
   m_metaData.user = CORBA::string_dup(user);
}

void adc_GeometryMetaData::setFacility(const char* facility)
{
   m_metaData.facility = CORBA::string_dup(facility);   
}

void adc_GeometryMetaData::setApplication(const char* application)
{
   m_metaData.application = CORBA::string_dup(application);
}

void adc_GeometryMetaData::setNotes(const char* notes)
{
   m_metaData.notes = CORBA::string_dup(notes);
}
