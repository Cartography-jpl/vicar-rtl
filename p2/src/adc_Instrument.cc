// adc_Instrument.cc
//
// April 11, 2002
// Michael Brady

#include "adc_Instrument.h"

static const int CASSINI_ISS_NAC_NAIF_ID = -82360;
static const int CASSINI_ISS_WAC_NAIF_ID = -82361;
static const int GALILEO_SSI_NAIF_ID     = -77001;
static const int VGR_1_ISSNA_NAIF_ID = -31001;
static const int VGR_1_ISSWA_NAIF_ID = -31002;
static const int VGR_2_ISSNA_NAIF_ID = -32001;
static const int VGR_2_ISSWA_NAIF_ID = -32002;

static int naifIdOf(jpl::mipl::spice::corba::Instrument src)
{
   switch (src)
   {
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_FULL:
      return jpl::mipl::spice::corba::NaifId::CASSINI_ISS_NAC_FULL;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM22:
      return jpl::mipl::spice::corba::NaifId::CASSINI_ISS_NAC_SUM22;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM44:
      return jpl::mipl::spice::corba::NaifId::CASSINI_ISS_NAC_SUM44;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_FULL:
      return jpl::mipl::spice::corba::NaifId::CASSINI_ISS_WAC_FULL;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM22:
      return jpl::mipl::spice::corba::NaifId::CASSINI_ISS_WAC_SUM22;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM44:
      return jpl::mipl::spice::corba::NaifId::CASSINI_ISS_WAC_SUM44;
   case jpl::mipl::spice::corba::GALILEO_SSI_FULL:
      return jpl::mipl::spice::corba::NaifId::GALILEO_SSI_FULL;
   case jpl::mipl::spice::corba::GALILEO_SSI_SUM:
      return jpl::mipl::spice::corba::NaifId::GALILEO_SSI_SUM;
   case jpl::mipl::spice::corba::VGR_1_ISSNA:
      return jpl::mipl::spice::corba::NaifId::VGR_1_ISSNA;
   case jpl::mipl::spice::corba::VGR_1_ISSWA:
      return jpl::mipl::spice::corba::NaifId::VGR_1_ISSWA;
   case jpl::mipl::spice::corba::VGR_2_ISSNA:
      return jpl::mipl::spice::corba::NaifId::VGR_2_ISSNA;
   case jpl::mipl::spice::corba::VGR_2_ISSWA:
      return jpl::mipl::spice::corba::NaifId::VGR_2_ISSWA;
   }

   ACE_ERROR (( LM_ERROR, "adc_Instrument.cc:naifIdOf:  "
                "Programming error.  Reached unreachable spot.  "
                "Perhaps a new instrument was added to the system, "
                "but this function was not modified.\n" ));
   ACE_OS::exit(1);

   // Can't get here, but to quiet the compiler...
   return 0;
}

static int miplIdOf(jpl::mipl::spice::corba::Instrument src)
{
   switch (src)
   {
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_FULL:
   return jpl::mipl::spice::corba::MiplId::CASSINI_ISS_NAC_FULL;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM22:
   return jpl::mipl::spice::corba::MiplId::CASSINI_ISS_NAC_SUM22;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM44:
   return jpl::mipl::spice::corba::MiplId::CASSINI_ISS_NAC_SUM44;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_FULL:
   return jpl::mipl::spice::corba::MiplId::CASSINI_ISS_WAC_FULL;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM22:
   return jpl::mipl::spice::corba::MiplId::CASSINI_ISS_WAC_SUM22;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM44:
   return jpl::mipl::spice::corba::MiplId::CASSINI_ISS_WAC_SUM44;
   case jpl::mipl::spice::corba::GALILEO_SSI_FULL:
   return jpl::mipl::spice::corba::MiplId::GALILEO_SSI_FULL;
   case jpl::mipl::spice::corba::GALILEO_SSI_SUM:
   return jpl::mipl::spice::corba::MiplId::GALILEO_SSI_SUM;
   case jpl::mipl::spice::corba::VGR_1_ISSNA:
   return jpl::mipl::spice::corba::MiplId::VGR_1_ISSNA;
   case jpl::mipl::spice::corba::VGR_1_ISSWA:
   return jpl::mipl::spice::corba::MiplId::VGR_1_ISSWA;
   case jpl::mipl::spice::corba::VGR_2_ISSNA:
   return jpl::mipl::spice::corba::MiplId::VGR_2_ISSNA;
   case jpl::mipl::spice::corba::VGR_2_ISSWA:
   return jpl::mipl::spice::corba::MiplId::VGR_2_ISSWA;
   }

   ACE_ERROR (( LM_ERROR, "adc_Instrument.cc:miplIdOf:  "
                "Programming error.  Reached unreachable spot.  "
                "Perhaps a new instrument was added to the system, "
                "but this function was not modified.\n" ));
   ACE_OS::exit(1);

   // Can't get here, but to quiet the compiler...
   return 0;
}

static const char* miplMissionNameOf(jpl::mipl::spice::corba::Instrument src)
{
   switch (src)
   {
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_FULL:
   return jpl::mipl::spice::corba::MiplMissionName::CASSINI_ISS_NAC_FULL;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM22:
   return jpl::mipl::spice::corba::MiplMissionName::CASSINI_ISS_NAC_SUM22;
   case jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM44:
   return jpl::mipl::spice::corba::MiplMissionName::CASSINI_ISS_NAC_SUM44;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_FULL:
   return jpl::mipl::spice::corba::MiplMissionName::CASSINI_ISS_WAC_FULL;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM22:
   return jpl::mipl::spice::corba::MiplMissionName::CASSINI_ISS_WAC_SUM22;
   case jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM44:
   return jpl::mipl::spice::corba::MiplMissionName::CASSINI_ISS_WAC_SUM44;
   case jpl::mipl::spice::corba::GALILEO_SSI_FULL:
   return jpl::mipl::spice::corba::MiplMissionName::GALILEO_SSI_FULL;
   case jpl::mipl::spice::corba::GALILEO_SSI_SUM:
   return jpl::mipl::spice::corba::MiplMissionName::GALILEO_SSI_SUM;
   case jpl::mipl::spice::corba::VGR_1_ISSNA:
   return jpl::mipl::spice::corba::MiplMissionName::VGR_1_ISSNA;
   case jpl::mipl::spice::corba::VGR_1_ISSWA:
   return jpl::mipl::spice::corba::MiplMissionName::VGR_1_ISSWA;
   case jpl::mipl::spice::corba::VGR_2_ISSNA:
   return jpl::mipl::spice::corba::MiplMissionName::VGR_2_ISSNA;
   case jpl::mipl::spice::corba::VGR_2_ISSWA:
   return jpl::mipl::spice::corba::MiplMissionName::VGR_2_ISSWA;
   }

   ACE_ERROR (( LM_ERROR, "adc_Instrument.cc:miplMissionNameOf:  "
                "Programming error.  Reached unreachable spot.  "
                "Perhaps a new instrument was added to the system, "
                "but this function was not modified.\n" ));
   ACE_OS::exit(1);

   // Can't get here, but to quiet the compiler...
   return 0;
}


adc_Instrument::adc_Instrument(jpl::mipl::spice::corba::Instrument src)
   : m_instrument(src)
{
}

int adc_Instrument::naifId() const
{
   return naifIdOf(m_instrument);
}

int adc_Instrument::miplId() const
{
   return miplIdOf(m_instrument);
}

const char* adc_Instrument::miplMissionName() const
{
   return miplMissionNameOf(m_instrument);
}


jpl::mipl::spice::corba::Instrument adc_Instrument::value() const
{
   return m_instrument;
}
