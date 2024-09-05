// adc_Instrument.h
//
// April 11, 2002
// Michael Brady

#if !defined ADC_INSTRUMENT_H_INCLUDED
#define ADC_INSTRUMENT_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

/** A class to identify a particular instrument to the system,
 *  and to convert between NAIF and MIPL instrument naming schemes.
 */
class adc_Instrument
{
public:
   /** Creates an Instrument from a Name. */
   adc_Instrument(jpl::mipl::spice::corba::Instrument instrument);

   /** Returns the NAIF ID for this instrument. */
   int naifId() const;

   /** Returns the MIPL ID for this instrument. */
   int miplId() const;

   /** Returns the MIPL mission name for this insturment. */
   const char* miplMissionName() const;

   /** Returns the enum value of this object. */
   jpl::mipl::spice::corba::Instrument value() const;

private:
   
   /** TheID for this instrument. */
   jpl::mipl::spice::corba::Instrument m_instrument;
};

#endif // !defined ADC_INSTRUMENT_H_INCLUDED
