// adc_Connection.h
//
// Apr 12, 2002
// Michael Brady

#if !defined ADC_CONNECTION_H_INCLUDED
#define ADC_CONNECTION_H_INCLUDED

#include "adc_Instrument.h"
#include "adc_Body.h"
#include "adc_ImageData.h"

#include "jpl_mipl_spice_corba.C.h"

/**
 *  Manages a connection to the anciliary data server.
 */
class adc_Connection
{
public:
   /** Creates a new adc_Connection object which can be
    *  used to retreive information about the specified instrument.
    *
    *  @param context a string which can be used to find a server.
    */
   adc_Connection(const char* context,
                  const adc_Instrument& inst);

   /** Creates a new adc_Connection object which can be
    *  used to retreive information about the specified instrument.
    *
    *  This will use the ADS_HOST and ADS_PORT environment variables
    *  to find the server.
    */
   adc_Connection(const adc_Instrument& inst);

   /** Destructor. */
   ~adc_Connection();

private:
   /** No copy construction allowed. */
   adc_Connection(const adc_Connection& src);

   /** No assignment allowed. */
   adc_Connection& operator=(const adc_Connection& rhs);
   
public:
   /** Closes this connection.
    *  Any further operations on this object will result in errors.
    */
   void close();

   /** Creates a body for the specified code. */
   adc_Body createBody(adc_Body::Code code);

   /** Creates a body for the specified name. 
    *  @param wasFound returns false if the string was not a valid name.
    */
   adc_Body createBody(const char* name, int& wasFound);

   /** Creates an ImageData object which can be used to retrieve
    *  anciliary data about that image.
    */
   adc_ImageData createImageData(adc_Body target, const char* scet);

   /** Creates an ImageData object which can be used to retrieve
    *  anciliary data about that image.
    *  @param criteria The geometry data for the image will meet this criteria.
    */
   adc_ImageData createImageData(adc_Body target, 
                                 const char* scet,
                                 const adc_GeometryMetaData& criteria);

private:

   jpl::mipl::spice::corba::MiplSpiceLib_var m_spiceLib;
   adc_Instrument m_instrument;

   int m_isClosed;
};

#endif // !defined ADC_CONNECTION_H_INCLUDED
