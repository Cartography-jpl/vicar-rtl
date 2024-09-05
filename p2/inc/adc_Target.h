// adc_Target.h
//
// Apr 25, 2002
// Michael Brady

#if !defined ADC_TARGET_H_INCLUDED
#define ADC_TARGET_H_INCLUDED

#include "adc_types.h"
#include "jpl_mipl_spice_corba.C.h"

/**
 *  Information about the target of an image.
 */
class adc_Target
{
   friend class adc_ImageData;

private:
   /** Creates a new adc_Target object. */
   adc_Target(jpl::mipl::spice::corba::Target* target);

public:
   /** Destructor. */
   virtual ~adc_Target();

   /** The 3 radii of the target. */
   const adc_Vector3& radii() const;

   /** The rotation rate of the target. */
   double rotationRate() const;

   
private:

   jpl::mipl::spice::corba::Target* m_target;

};

#endif // !defined ADC_TARGET_H_INCLUDED
