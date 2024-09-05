// adc_ImageData.h
//
// Apr 12, 2002
// Michael Brady

#if !defined ADC_IMAGEDATA_H_INCLUDED
#define ADC_IMAGEDATA_H_INCLUDED

#include "adc_Geometry.h"
#include "adc_InstrumentData.h"
#include "adc_Target.h"

#include "jpl_mipl_spice_corba.C.h"

/**
 *  Allows access to anciliary data for a single image.
 */
class adc_ImageData
{
   friend class adc_Connection;

private:
   /** Creates a new adc_ImageData object which will get geometry information
      from the default source. */
   adc_ImageData ( jpl::mipl::spice::corba::MiplSpiceLib_ptr spiceLib,
                   jpl::mipl::spice::corba::Instrument instrument,
                   jpl::mipl::spice::corba::SpInt target,
                   const char* utcScet );

   /** Creates a new adc_ImageData object which will get geometry information
      which matches the specified criteria. */
   adc_ImageData ( jpl::mipl::spice::corba::MiplSpiceLib_ptr spiceLib,
                   jpl::mipl::spice::corba::Instrument instrument,
                   jpl::mipl::spice::corba::SpInt target,
                   const char* utcScet,
                   const jpl::mipl::spice::corba::GeometryMetaData& criteria );


public:

   /** Destructor. */
   virtual ~adc_ImageData();

   /** Returns the geometry of this image.
    *  Note that the returned geometry object is only valid as long
    *  as this object exists.
   */
   adc_Geometry getGeometry();

   /** Updates the geometry of this image.
   */
   void updateGeometry(const adc_Geometry& update);

   /** Returns a description of the instrument which created this image.
   *  Note that the returned object is only valid as long
   *  as this object exists.
   */
   adc_InstrumentData getInstrumentData();

   /** Returns a description of the target of this image.
   *  Note that the returned object is only valid as long
   *  as this object exists.
   */
   adc_Target getTarget();

  

private:

   jpl::mipl::spice::corba::MiplSpiceLib_var m_spiceLib;

   jpl::mipl::spice::corba::ImageData m_image;

   jpl::mipl::spice::corba::Geometry m_geometry;
   int m_geometryHasBeenFound;
   jpl::mipl::spice::corba::GeometryMetaData_var m_geometryMetaData;

   jpl::mipl::spice::corba::GeometryMetaData m_criteria;
   
   jpl::mipl::spice::corba::InstrumentData m_instData;
   int m_instDataHasBeenFound;

   jpl::mipl::spice::corba::Target m_target;
   int m_targetHasBeenFound;




};

#endif // !defined ADC_IMAGEDATA_H_INCLUDED
