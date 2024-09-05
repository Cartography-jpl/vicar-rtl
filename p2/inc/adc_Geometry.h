// adc_Geometry.h
//
// Apr 12, 2002
// Michael Brady

#if !defined ADC_GEOMETRY_H_INCLUDED
#define ADC_GEOMETRY_H_INCLUDED

#include "adc_types.h"
#include "adc_GeometryMetaData.h"

/** The geometry of the bodies involved in creating an image.
 *  An object of this class can be obtained from an adc_ImageData object.
 */
class adc_Geometry
{
   friend class adc_ImageData;

private:
   /** Creates a new adc_Geometry object. */
   adc_Geometry(const jpl::mipl::spice::corba::Geometry& geometry,
                const jpl::mipl::spice::corba::GeometryMetaData& metaData);

public:
   /** Destructor. */
   ~adc_Geometry();

   /** The meta data for this information. */
   const adc_GeometryMetaData& metaData() const;

   /** Sets the meta data for this information. */
   void setMetaData(const adc_GeometryMetaData& metaData);

   /** The vector from the target to the spacecraft. */
   const adc_Vector3& spacecraftVector() const;
   
   /** The vector from the target to the sun. */
   const adc_Vector3& solarVector() const;

   /** If the target is a moon of a planet,
    *  this is the vector from the target to the planet.
    *  Otherwise, this is undefined.
    */
   const adc_Vector3& planetVector() const;

   /** The velocity of the spacecraft when the image was created. */
   const adc_Vector3& spacecraftVelocityVector() const;
   
   /** The pointing of the instrument when the image was created. */
   const adc_Matrix33& instrumentPointing() const;

   /** The orientation of the image target. */
   const adc_Matrix33& targetOrientation() const;


   /** Sets the vector from the target to the spacecraft. */
   void setSpacecraftVector(const adc_Vector3& v);
   
   /** Sets the vector from the target to the sun. */
   void setSolarVector(const adc_Vector3& v);

   /** If the target is a moon of a planet,
    *  this is the vector from the target to the planet.
    *  Otherwise, this is undefined.
    */
   void setPlanetVector(const adc_Vector3& v);

   /** Sets the velocity of the spacecraft when the image was created. */
   void setSpacecraftVelocityVector(const adc_Vector3& v);
   
   /** Sets the pointing of the instrument when the image was created. */
   void setInstrumentPointing(const adc_Matrix33& m);

   /** Sets the orientation of the image target. */
   void setTargetOrientation(const adc_Matrix33& m);
   
private:

   jpl::mipl::spice::corba::Geometry m_geometry;
   adc_GeometryMetaData m_metaData;

};

#endif // !defined ADC_GEOMETRY_H_INCLUDED
