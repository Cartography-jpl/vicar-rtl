$!****************************************************************************
$!
$! Build proc for MIPL module adc_sub_h
$! VPACK Version 1.9, Monday, April 29, 2002, 10:53:30
$!
$! Execute by entering:		$ @adc_sub_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module adc_sub_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to adc_sub_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("adc_sub_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @adc_sub_h.bld "STD"
$   else
$      @adc_sub_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create adc_sub_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack adc_sub_h.com -mixed -
	-s adc_Body.h adc_Connection.h adc_Geometry.h adc_GeometryMetaData.h -
	   adc_ImageData.h adc_Instrument.h adc_InstrumentData.h adc_ORB.h -
	   adc_Target.h adc_getspice95.h adc_types.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create adc_Body.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Connection.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Geometry.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_GeometryMetaData.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_ImageData.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Instrument.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_InstrumentData.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_ORB.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_ORB.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADC_ORB_H_INCLUDED
#define ADC_ORB_H_INCLUDED

#include "tao/ORB.h"


class adc_ORB
{
    public:
    /**
     *  The first time this is called, creates a new ORB object initialized
     *  to the specified parameters.
     *  Subsequent calls have no effect.
     */
    static void init(int argc, char** argv);
    
    /**
     *  Return an instance of the one and only ORB object.
     *
     *  @return the ORB if init() has been called, otherwise nil.
       */
    static CORBA::ORB_ptr instance()
    throw();
};


#endif // !defined ADC_ORB_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Target.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_getspice95.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_getspice95.h
//
// Apr 18, 2002
// Michael Brady

#if !defined ADC_GETSPICE95_H_INCLUDED
#define ADC_GETSPICE95_H_INCLUDED


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "ms_defines.h"

/**
 *  Obtains the getspice information from 
 *  the Anciliary Data Server (ads) system.
 */
int adc_getspice(msUserRequestStruct*    req,
		msCkStruct*             ckdata,
		msSpkStruct*            spkdata);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif // !defined ADC_GETSPICE95_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_types.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_types.h
//
//  This file brings the corba-defined types into the adc_ namespace.

#include "jpl_mipl_spice_corba.C.h"

/** An alias for double[3] */
typedef jpl::mipl::spice::corba::Vector3 adc_Vector3;

/** An alias for double[3][3] */
typedef jpl::mipl::spice::corba::Matrix33 adc_Matrix33;
$ VOKAGLEVE
$ Return
$!#############################################################################
