// adc_GeometryMetaData.h
//
// Apr 25, 2002
// Michael Brady

#if !defined ADC_GEOMETRYMETADATA_H_INCLUDED
#define ADC_GEOMETRYMETADATA_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

/**
 *  Describes the source of geometry data for an image.
 */
class adc_GeometryMetaData
{
   friend class adc_ImageData;
   friend class adc_Connection;

public:
   /** Creates a new adc_GeometryMetaData object with all fields empty. */
   adc_GeometryMetaData();

   /** Creates a new adc_GeometryMetaData object with all fields set
    *  to the value of the specified object. */
   adc_GeometryMetaData(const jpl::mipl::spice::corba::GeometryMetaData& src);

   /** Destructor. */
   virtual ~adc_GeometryMetaData();

   /** Creates a new adc_GeometryMetaData object 
    *  which is a copy of the specified object. 
    */
   adc_GeometryMetaData(const adc_GeometryMetaData& src);

   /** Sets this to the value of the specified object. */
   adc_GeometryMetaData& operator=(const adc_GeometryMetaData& rhs);


   /** The user who stored the data. */
   const char* user() const;
   
   /** The facility at which the data was created. */
   const char* facility() const;
   
   /** The software application which created the data. */
   const char* application() const;
   
   /** Any other information stored by the data producer. */
   const char* notes() const;

   /** Sets the user who stored the data. */
   void setUser(const char* user);
   
   /** Sets the facility at which the data was created. */
   void setFacility(const char* facility);
   
   /** Sets the software application which created the data. */
   void setApplication(const char* application);
   
   /** Sets any other information. */
   void setNotes(const char* notes);
   
private:

   jpl::mipl::spice::corba::GeometryMetaData m_metaData;
};

#endif // !defined ADC_GEOMETRYMETADATA_H_INCLUDED
