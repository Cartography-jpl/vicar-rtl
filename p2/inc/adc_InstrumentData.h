// adc_InstrumentData.h
//
// Apr 25, 2002
// Michael Brady

#if !defined ADC_INSTRUMENTDATA_H_INCLUDED
#define ADC_INSTRUMENTDATA_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

/**
 * Information about the instrument which created an image.
 * Objects of this class can be obtained from an adc_ImageData object.
 */
class adc_InstrumentData
{
   friend class adc_ImageData;

private:
   /** Creates a new adc_InstrumentData object. */
   adc_InstrumentData(jpl::mipl::spice::corba::InstrumentData* instrument);


public:
   /** Destructor. */
   ~adc_InstrumentData();

   /** The boresight offset of the camera. */
   double cameraBoresightOffset() const;

   /** The focal length of the camera. */
   double focalLength() const;

   /** The picture scale of the camera. */
   double pictureScale() const;

   /** The optical axis offset of the camera. */
   double opticalAxisOffset() const;

private:

   jpl::mipl::spice::corba::InstrumentData* m_instrument;
};

#endif // !defined ADC_INSTRUMENTDATA_H_INCLUDED
