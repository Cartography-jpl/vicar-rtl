//  Michael Brady
//  September 25, 2000
//  Body.h

#if !defined ADC_BODY_H_INCLUDED
#define ADC_BODY_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

/**
 *  The adc_Body class encapsulates varous ways of identifiying
 *  extraterrestrial bodies.
 *
 *  The safest way is to use the enumerated value adc_Body::Code.
 *  You may also use the NAIF integer codes, or string values.
 */
class adc_Body
{
   
   friend class adc_Connection;

public:
   
   /**
    *  Body codes.
    */
   enum Code 
      {
         
         //  Barycenters
      
         /** */	 SOLAR_SYSTEM_BARYCENTER = 0,
         /** */	 MERCURY_BARYCENTER = 1,
         /** */	 VENUS_BARYCENTER = 2,
         /** */	 EARTH_BARYCENTER = 3,
         /** */	 MARS_BARYCENTER = 4,
         /** */	 JUPITER_BARYCENTER = 5,
         /** */	 SATURN_BARYCENTER = 6,
         /** */	 URANUS_BARYCENTER = 7,
         /** */	 NEPTURE_BARYCENTER = 8,
         /** */	 PLUTO_BARYCENTER = 9,
         /** */	 SUN_BARYCENTER = 10,
	 
	 
	 
         // Planets and Satellites
	 
         /** */	 MERCURY = 199,
         /** */	 VENUS = 299,
         /** */	 EARTH = 399,
         /** */	 MARS = 499,
         /** */	 JUPITER = 599,
         /** */	 SATURN = 699,
         /** */	 URANUS = 799,
         /** */	 NEPTURE = 899,
         /** */	 PLUTO = 999,
	 
         /** */	 MOON = 301,
	 
         /** */	 PHOBOS = 401,
         /** */	 DEIMOS = 402,
	 
         /** */	 IO = 501,
         /** */	 EUROPA = 502,
         /** */	 GANYMEDE = 503,
         /** */	 CALLISTO = 504,
         /** */	 AMALTHEA = 505,
         /** */	 HIMALIA = 506,
         /** */	 ELARA = 507,
         /** */	 PASIPHAE = 508,
         /** */	 SINOPE = 509,
         /** */	 LYSITHEA = 510,
         /** */	 CARME = 511,
         /** */	 ANANKE = 512,
         /** */	 LEDA = 513,
         /** */	 THEBE = 514,
         /** */	 ADRASTEA = 515,
         /** */	 METIS = 516,
	 
         /** */	 MIMAS = 601,
         /** */	 ENCELADUS = 602,
         /** */	 TETHYS = 603,
         /** */	 DIONE = 604,
         /** */	 RHEA = 605,
         /** */	 TITAN = 606,
         /** */	 HYPERION = 607,
         /** */	 IAPETUS = 608,
         /** */	 PHOEBE = 609,
         /** */	 JANUS = 610,
         /** */	 EPIMETHEUS = 611,
         /** */	 HELENE = 612,
         /** */	 TELESTO = 613,
         /** */	 CALYPSO = 614,
         /** */	 ATLAS = 615,
         /** */	 PROMETHEUS = 616,
         /** */	 PANDORA = 617,
         /** */	 PAN = 618,
	 
         /** */	 ARIEL = 701,
         /** */	 UMBRIEL = 702,
         /** */	 TITANIA = 703,
         /** */	 OBERON = 704,
         /** */	 MIRANDA = 705,
         /** */	 CORDELIA = 706,
         /** */	 OPHELIA = 707,
         /** */	 BIANCA = 708,
         /** */	 CRESSIDA = 709,
         /** */	 DESDEMONA = 710,
         /** */	 JULIET = 711,
         /** */	 PORTIA = 712,
         /** */	 ROSALIND = 713,
         /** */	 BELINDA = 714,
         /** */	 PUCK = 715,
         /** */	 CALIBAN = 716,
         /** */	 SYCORAX = 717,
         /** */	 U1985U10 = 718,
	 
         /** */	 TRITON = 801,
         /** */	 NEREID = 802,
         /** */	 NAIAD = 803,
         /** */	 THALASSA = 804,
         /** */	 DESPINA = 805,
         /** */	 GALATEA = 806,
         /** */	 LARISSA = 807,
         /** */	 PROTEUS = 808,
	 
         /** */	 CHARON = 901,
	 
         //  Spacecraft
	 
         /** */	 DS1 = -30,
         /** */	 MARS_PATHFINDER = -53,
         /** */	 GALILEO = -77,
         /** */	 CASSINI = -82	  
      };



private:

   /**
    *  Creates a body with the specified value.
    */
   adc_Body ( jpl::mipl::spice::corba::SpiceLib_ptr spiceLib,
              adc_Body::Code body);

   /**
    *  Creates a body with value set to Earth.
    */
   adc_Body( jpl::mipl::spice::corba::SpiceLib_ptr spiceLib);

public:

   /**
    *  Returns the NAIF ID for this body.
    */
   int getNaifId() const;
   
   /**
    *  Returns the name of this body.
    */
   const char* getName();
      
   /**
    *  Sets this body to the specified value.
    *  @param name the name of the body.
    *  @param wasFound An out parameter.  It's incoming value is ignored.
    *                  After this method has returned, if it is true, 
    *                  the set was succesful.
    *                  If false, then the name was not known to the sytem.
    */
   void set (const char* name, int& wasFound);
   
   /**
    *  Sets this body to the specified value.
    *  Note that a Code enum value may be passed as the 'bodyId' parameter.
    */
   void set (int bodyId);


private:

   int m_naifId;
   CORBA::String_var m_name;
   jpl::mipl::spice::corba::SpiceLib_var m_spiceLib;

};

#endif // !defined ADC_BODY_H_INCLUDED
