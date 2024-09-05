$!****************************************************************************
$!
$! Build proc for MIPL module adc_sub
$! VPACK Version 1.9, Wednesday, January 24, 2007, 11:31:25
$!
$! Execute by entering:		$ @adc_sub
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   DOC         Only the documentation files are created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module adc_sub ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Doc = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "DOC" then Create_Doc = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Doc .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to adc_sub.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Doc then gosub Doc_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Doc = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_Doc = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_Doc = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("adc_sub.imake") .nes. ""
$   then
$      vimake adc_sub
$      purge adc_sub.bld
$   else
$      if F$SEARCH("adc_sub.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake adc_sub
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @adc_sub.bld "STD"
$   else
$      @adc_sub.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create adc_sub.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack adc_sub.com -mixed -
	-s adc_Body.cc adc_Connection.cc adc_Geometry.cc -
	   adc_GeometryMetaData.cc adc_ImageData.cc adc_Instrument.cc -
	   adc_InstrumentData.cc adc_ORB.cc adc_Target.cc adc_getspice95.cc -
	-i adc_sub.imake -
	-d README-adc_sub.txt -
	-t adc_test.cc adc_test.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create adc_Body.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//  Michael Brady
//  September 25, 2000
//  adc_Body.cc

#include "adc_Body.h"


adc_Body::adc_Body (jpl::mipl::spice::corba::SpiceLib_ptr spiceLib,
                    Code body)
{
   m_spiceLib = jpl::mipl::spice::corba::SpiceLib::_duplicate(spiceLib);
   set(body);
}

adc_Body::adc_Body(jpl::mipl::spice::corba::SpiceLib_ptr spiceLib)
{
   m_spiceLib = jpl::mipl::spice::corba::SpiceLib::_duplicate(spiceLib);
   set(EARTH);
}

int adc_Body::getNaifId() const
{
   return m_naifId;
}

const char* adc_Body::getName()
{
   CORBA::Boolean wasFound = 0;

   m_spiceLib->bodc2n(getNaifId(), m_name.out(), wasFound);

   return m_name.in();
   
}

void adc_Body::set (const char* name, int& wasFound)
{
   int id = 0;
   CORBA::Boolean found = 0;

   m_spiceLib->bodn2c(name, id, found);
    
   if (found)
   {
      m_naifId = id;
      wasFound = 1;
   }
   else
   {
      wasFound = 0;
   }
}

void adc_Body::set(int bodyId)
{
   m_naifId = bodyId;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Connection.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_Connection.cc
//
// Apr 12, 2002
// Michael Brady

#include "adc_Connection.h"

#include "adc_ORB.h"

typedef jpl::mipl::spice::corba::MiplSpiceLib MiplSpiceLib;
typedef jpl::mipl::spice::corba::MiplSpiceLib_var MiplSpiceLib_var;
typedef jpl::mipl::spice::corba::MiplSpiceLib_ptr MiplSpiceLib_ptr;
typedef jpl::mipl::spice::corba::SpiceLibFactory SpiceLibFactory;
typedef jpl::mipl::spice::corba::SpiceLibFactory_var SpiceLibFactory_var;
typedef jpl::mipl::spice::corba::SpiceLibFactory_ptr SpiceLibFactory_ptr;

static MiplSpiceLib_ptr findSpiceLibFromFactory (const char* iorStr,
                                                 const adc_Instrument& inst)
{
   CORBA::Object_var obj = adc_ORB::instance()->string_to_object(iorStr);
   if (CORBA::is_nil(obj.in()))
   {
      ACE_DEBUG (( LM_ERROR, "Factory object is null.\n" ));
      ACE_OS::exit(1);
   }

   SpiceLibFactory_var factory;
   try
   {
      factory = SpiceLibFactory::_narrow(obj.in());
   }
   catch(CORBA::SystemException& ex)
   {
      ACE_ERROR((LM_ERROR,"Error:  Couldn't contact SpiceLibFactory.\n"));
      ACE_OS::exit(1);      
   }

   if (CORBA::is_nil(factory.in()))
   {
      ACE_ERROR (( LM_ERROR, "Error:  Object couldn't be "
		   "narrowed to a SpiceLibFactory.\n" ));
      ACE_OS::exit(1);
   }


   MiplSpiceLib_var spiceLib = factory->makeSpiceLib(inst.miplMissionName());
   if (CORBA::is_nil(spiceLib.in()))
   {
      ACE_ERROR (( LM_ERROR, "Error:  received null MiplSpiceLib.\n" ));
      ACE_OS::exit(1);
   }


   return spiceLib._retn();
}

static MiplSpiceLib_ptr findSpiceLibFromIor (const char* iorStr,
                                             const adc_Instrument& inst)
{
   CORBA::Object_var obj = adc_ORB::instance()->string_to_object(iorStr);
   if (CORBA::is_nil(obj.in()))
   {
      ACE_DEBUG (( LM_ERROR, "MiplSpiceLib object is null.\n" ));
      ACE_OS::exit(1);
   }

   MiplSpiceLib_var spiceLib;
   try
   {
      spiceLib = MiplSpiceLib::_narrow(obj.in());
   }
   catch(CORBA::SystemException& ex)
   {
      ACE_ERROR((LM_ERROR,"Error:  Couldn't contact MiplSpiceLib.\n"));
      ACE_OS::exit(1);      
   }

   if (CORBA::is_nil(spiceLib.in()))
   {
      ACE_ERROR (( LM_ERROR, "Error:  Object couldn't be "
		   "narrowed to a MiplSpiceLib.\n" ));
      ACE_OS::exit(1);
   }

   spiceLib->init(SpiceLibFactory::_nil(), 0, inst.miplMissionName());

   return spiceLib._retn();
}

adc_Connection::adc_Connection(const adc_Instrument& inst)
   : m_instrument(inst),
     m_isClosed(0)
{
   // Initialize the ORB if neccessary.
   if (CORBA::is_nil(adc_ORB::instance()))
   {
      int argc = 0;
      char* argv[1] = { "" };
      adc_ORB::init(argc, argv);
   }

   // Assemble the factory location string.

   const char HOST_VAR[] = "ADS_HOST";
   const char PORT_VAR[] = "ADS_PORT";

   const char* host = getenv(HOST_VAR);
   const char* port = getenv(PORT_VAR);

   if ((host == 0) || (port == 0))
   {
      ACE_ERROR (( LM_ERROR, 
                   "Error: Environment variables %s and %s are not set.\n",
                   HOST_VAR,
                   PORT_VAR ));
      ACE_OS::exit(1);
   }


   char context[128];
   memset(context, '\0', sizeof(context));
   sprintf(context, "corbaloc::%s:%s/%s", host, port, SpiceLibFactory::NAME);

   m_spiceLib = findSpiceLibFromFactory(context, inst);

}

adc_Connection::adc_Connection(const char* context,
                               const adc_Instrument& inst)
   : m_instrument(inst),
     m_isClosed(0)
{
   // Initialize the ORB if neccessary.
   if (CORBA::is_nil(adc_ORB::instance()))
   {
      int argc = 0;
      char* argv[1] = { "" };
      adc_ORB::init(argc, argv);
   }

   // If the context string starts with 's', it's a service.
   // Otherwise it's a factory.

   if (context[0] == 's' && context[1] == ':')
   {
      const char* ior = context + 2;
      m_spiceLib = findSpiceLibFromIor(ior, inst);
   }
   else
   {
      m_spiceLib = findSpiceLibFromFactory(context, inst);
   }
}

adc_Connection::~adc_Connection()
{
   if (!m_isClosed)
   {
      // C++ doesn't allow exceptions to be thrown in a destructor,
      // so catch them.
      try
      {
         close();
      }
      catch (CORBA::SystemException& ex)
      {
         ACE_ERROR((LM_ERROR, 
                    "adc_Connection::~adc_Connection():  Network error.\n" ));
      }
   }
}

void adc_Connection::close()
{   
   m_spiceLib->close();
   m_isClosed = 1;
}



adc_Body adc_Connection::createBody(adc_Body::Code code)
{
   return adc_Body(m_spiceLib.in(), code);
}

adc_Body adc_Connection::createBody(const char* name, int& wasFound)
{
   adc_Body ret(m_spiceLib.in());
   ret.set(name, wasFound);
   return ret;
}

adc_ImageData 
adc_Connection::createImageData(adc_Body target, const char* scet)
{
   return adc_ImageData(m_spiceLib.in(), 
                        m_instrument.value(), 
                        target.getNaifId(), 
                        scet);
}

adc_ImageData 
adc_Connection::createImageData(adc_Body target, 
                                const char* scet,
                                const adc_GeometryMetaData& criteria)
{
   return adc_ImageData(m_spiceLib.in(), 
                        m_instrument.value(), 
                        target.getNaifId(), 
                        scet,
                        criteria.m_metaData);
   
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Geometry.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_Geometry.cc
//
// Apr 12, 2002
// Michael Brady

#include "adc_Geometry.h"

adc_Geometry::
adc_Geometry(const jpl::mipl::spice::corba::Geometry& geometry,
             const jpl::mipl::spice::corba::GeometryMetaData& metaData)
   : m_geometry(geometry),
     m_metaData(metaData)
{
}

adc_Geometry::~adc_Geometry()
{
}


const adc_GeometryMetaData& adc_Geometry::metaData() const
{
   return m_metaData;
}

void adc_Geometry::setMetaData(const adc_GeometryMetaData& metaData)
{
   m_metaData = metaData;
}

const adc_Vector3& adc_Geometry::spacecraftVector() const
{
   return m_geometry.spacecraftVector;
}

const adc_Vector3& adc_Geometry::solarVector() const
{
   return m_geometry.solarVector;
}

const adc_Vector3& adc_Geometry::planetVector() const
{
   return m_geometry.planetVector;
}

const adc_Vector3& adc_Geometry::spacecraftVelocityVector() const
{
   return m_geometry.spacecraftVelocityVector;
}

const adc_Matrix33& adc_Geometry::instrumentPointing() const
{
   return m_geometry.instrumentPointing;
}

const adc_Matrix33& adc_Geometry::targetOrientation() const
{
   return m_geometry.targetOrientation;
}


void adc_Geometry::setSpacecraftVector(const adc_Vector3& v)
{
   memcpy(m_geometry.spacecraftVector, v, sizeof(adc_Vector3));
}

void adc_Geometry::setSolarVector(const adc_Vector3& v)
{
   memcpy(m_geometry.solarVector, v, sizeof(adc_Vector3));
}

void adc_Geometry::setPlanetVector(const adc_Vector3& v)
{
   memcpy(m_geometry.planetVector, v, sizeof(adc_Vector3));
}

void adc_Geometry::setSpacecraftVelocityVector(const adc_Vector3& v)
{
   memcpy(m_geometry.spacecraftVelocityVector, v, sizeof(adc_Vector3));
}
   
void adc_Geometry::setInstrumentPointing(const adc_Matrix33& m)
{
   memcpy(m_geometry.instrumentPointing, m, sizeof(adc_Matrix33));
}

void adc_Geometry::setTargetOrientation(const adc_Matrix33& m)
{
   memcpy(m_geometry.targetOrientation, m, sizeof(adc_Matrix33));
}
   
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_GeometryMetaData.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_GeometryMetaData.cc
//
// Apr 25, 2002
// Michael Brady

#include "adc_GeometryMetaData.h"

adc_GeometryMetaData::adc_GeometryMetaData()
{
}

adc_GeometryMetaData::
adc_GeometryMetaData(const jpl::mipl::spice::corba::GeometryMetaData& src)
   : m_metaData(src)
{
}


adc_GeometryMetaData::~adc_GeometryMetaData()
{
}

adc_GeometryMetaData::adc_GeometryMetaData(const adc_GeometryMetaData& src)
   : m_metaData(src.m_metaData)
{
}

adc_GeometryMetaData& adc_GeometryMetaData::operator=(const adc_GeometryMetaData& rhs)
{
   if (this != &rhs)
   {
      m_metaData = rhs.m_metaData;
   }

   return *this;
}

const char* adc_GeometryMetaData::user() const
{
   return m_metaData.user.in();
}

const char* adc_GeometryMetaData::facility() const
{
   return m_metaData.facility.in();
}

const char* adc_GeometryMetaData::application() const
{
   return m_metaData.application.in();
}

const char* adc_GeometryMetaData::notes() const
{
   return m_metaData.notes.in();
}

void adc_GeometryMetaData::setUser(const char* user)
{
   m_metaData.user = CORBA::string_dup(user);
}

void adc_GeometryMetaData::setFacility(const char* facility)
{
   m_metaData.facility = CORBA::string_dup(facility);   
}

void adc_GeometryMetaData::setApplication(const char* application)
{
   m_metaData.application = CORBA::string_dup(application);
}

void adc_GeometryMetaData::setNotes(const char* notes)
{
   m_metaData.notes = CORBA::string_dup(notes);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_ImageData.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_ImageData.cc
//
// Apr 12, 2002
// Michael Brady

#include "adc_ImageData.h"

adc_ImageData::adc_ImageData ( jpl::mipl::spice::corba::MiplSpiceLib_ptr spiceLib,
                               jpl::mipl::spice::corba::Instrument instrument,
                               jpl::mipl::spice::corba::SpInt target,
                               const char* utcScet )
   :  m_geometryHasBeenFound(0),
      m_criteria(),
      m_instDataHasBeenFound(0),
      m_targetHasBeenFound(0)
{
   m_spiceLib = jpl::mipl::spice::corba::MiplSpiceLib::_duplicate(spiceLib);

   m_image.inst = instrument;
   m_image.target = target;
   m_image.utcTime = CORBA::string_dup(utcScet);
   m_image.referenceFrame = CORBA::string_dup("J2000");
   m_image.tolerance = -1;
}

adc_ImageData::
adc_ImageData ( jpl::mipl::spice::corba::MiplSpiceLib_ptr spiceLib,
                jpl::mipl::spice::corba::Instrument instrument,
                jpl::mipl::spice::corba::SpInt target,
                const char* utcScet,
                const jpl::mipl::spice::corba::GeometryMetaData& criteria )
   :  m_geometryHasBeenFound(0),
      m_criteria(criteria),
      m_instDataHasBeenFound(0),
      m_targetHasBeenFound(0)
{
   m_spiceLib = jpl::mipl::spice::corba::MiplSpiceLib::_duplicate(spiceLib);

   m_image.inst = instrument;
   m_image.target = target;
   m_image.utcTime = CORBA::string_dup(utcScet);
   m_image.referenceFrame = CORBA::string_dup("J2000");
   m_image.tolerance = -1;
}


adc_ImageData::~adc_ImageData()
{
}

adc_Geometry adc_ImageData::getGeometry()
{
   if (!m_geometryHasBeenFound)
   {
      m_spiceLib->getGeometry(m_image, 
                              m_criteria, 
                              m_geometry, 
                              m_geometryMetaData.out());         
      
      m_geometryHasBeenFound = 1;
   }

   return adc_Geometry(m_geometry, m_geometryMetaData.in());
}

void adc_ImageData::updateGeometry(const adc_Geometry& update)
{
   m_spiceLib->storeGeometry(m_image,
                             update.m_geometry,
                             update.m_metaData.m_metaData);
}

adc_InstrumentData adc_ImageData::getInstrumentData()
{
   if (!m_instDataHasBeenFound)
   {
      m_spiceLib->getInstrument(m_image, m_instData );
      m_instDataHasBeenFound = 1;
   }

   return adc_InstrumentData(&m_instData);
}

adc_Target adc_ImageData::getTarget()
{
   if (!m_targetHasBeenFound)
   {
      m_spiceLib->getTarget(m_image, m_target);
      m_targetHasBeenFound = 1;
   }
   
   return adc_Target(&m_target);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Instrument.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_InstrumentData.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_InstrumentData.cc
//
// Apr 25, 2002
// Michael Brady

#include "adc_InstrumentData.h"

adc_InstrumentData::
adc_InstrumentData(jpl::mipl::spice::corba::InstrumentData* instrument)
   : m_instrument(instrument)
{
}

adc_InstrumentData::~adc_InstrumentData()
{
}

double adc_InstrumentData::cameraBoresightOffset() const
{
   return m_instrument->cameraBoresightOffset;
}

double adc_InstrumentData::focalLength() const
{
   return m_instrument->focalLength;
}

double adc_InstrumentData::pictureScale() const
{
   return m_instrument->pictureScale;
}

double adc_InstrumentData::opticalAxisOffset() const
{
   return m_instrument->opticalAxisOffset;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_ORB.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_ORB.h
//
//  April 9, 2002
//  Michael Brady

#include "adc_ORB.h"



static CORBA::ORB_var g_orb = CORBA::ORB::_nil();

void adc_ORB::init(int argc, char** argv)
{
    if (CORBA::is_nil(g_orb.in()))
	{
	    g_orb = CORBA::ORB_init (argc, argv, 0);
	}
}

   
CORBA::ORB_ptr adc_ORB::instance()
throw ()
{
    return g_orb.in();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_Target.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_Target.cc
//
// Apr 25, 2002
// Michael Brady

#include "adc_Target.h"

adc_Target::adc_Target(jpl::mipl::spice::corba::Target* target)
   : m_target(target)
{
}

adc_Target::~adc_Target()
{
}

const adc_Vector3& adc_Target::radii() const
{
   return m_target->radii;
}


double adc_Target::rotationRate() const
{
   return m_target->rotationRate;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create adc_getspice95.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
// adc_getspice95.cc
//
// Apr 18, 2002
// Michael Brady

#include "adc_getspice95.h"


extern "C"
{

#include "ms_defines.h"

}

#include "adc_Connection.h"
#include "adc_ORB.h"

#include "jpl_mipl_spice_corba.C.h"
#include <iostream>


typedef jpl::mipl::spice::corba::Instrument Instrument;
typedef 
jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound PointingNotFound;
typedef jpl::mipl::spice::corba::SpiceLib::ToolkitException ToolkitException;



static 
jpl::mipl::spice::corba::Instrument 
valueOf(int spacecraftNaifId, const char* name)
{
   switch (spacecraftNaifId)
   {
   case -82:
      if (0 == strcmp(name, CASISSWA_STR))
         return jpl::mipl::spice::corba::CASSINI_ISS_WAC_FULL;
      else if (0 == strcmp(name, CASISSWA_SUM22_STR))
         return jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM22;
      else if (0 == strcmp(name, CASISSWA_SUM44_STR))
         return jpl::mipl::spice::corba::CASSINI_ISS_WAC_SUM44;
      else if (0 == strcmp(name, CASISSNA_STR))
         return jpl::mipl::spice::corba::CASSINI_ISS_NAC_FULL;
      else if (0 == strcmp(name, CASISSNA_SUM22_STR))
         return jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM22;
      else if (0 == strcmp(name, CASISSNA_SUM44_STR))
         return jpl::mipl::spice::corba::CASSINI_ISS_NAC_SUM44;
      else
      {
         ACE_ERROR (( LM_ERROR, "Bad instrument string '%s'.\n", name ));
         exit(1);
      }
   default:
         ACE_ERROR (( LM_ERROR, "Bad spacecraft ID code '%d'.\n", 
                      spacecraftNaifId ));
         exit(1);
   }
}










int adc_getspice(msUserRequestStruct*    req,
                msCkStruct*             ckdata,
                msSpkStruct*            spkdata)
{
   
   try
   {
      int argc = 1;
      char* argv[] = {"program"};

      adc_Connection con(valueOf(req->sc_id, req->instrument_name));

      int wasFound = 0;
      adc_Body target = con.createBody(req->target_name, wasFound);
      if (!wasFound)
      {
         ACE_ERROR (( LM_ERROR, "Bad target name '%s'.\n", 
                      req->target_name ));
         exit(1);   
      }

      char utc_str[128];
      memset(utc_str, '\0', sizeof(utc_str));
      sprintf(utc_str, "%04d-%03d // %02d:%02d:%02d.%03d",
	      req->scet[0], req->scet[1], req->scet[2],
	      req->scet[3], req->scet[4], req->scet[5]);
      
      adc_ImageData image = con.createImageData(target, utc_str);
      

      memcpy(ckdata->c_matrix, 
             &image.getGeometry().instrumentPointing(), 
             sizeof(ckdata->c_matrix));


      // TODO Finish this.  The other data items need to be copied.
      
      return 0;
   }
   catch (const PointingNotFound& ex)
   {
      std::cout << "Caught PointingNotFound: " << std::endl;
      return -1;
   }
   catch (const ToolkitException& ex)
   {
      std::cout << "Caught TookitException.\n"
           << "  Name: " << ex.name.in() << "\n"
           << "  Description: " << ex.description.in() << "\n"
           << "  longMessage: " << ex.longMessage.in() << "\n"
           << "  trace:\n";

      for (unsigned int i=0; i < ex.trace.length(); ++i)
      {
         std::cout  << ex.trace[i].in() << "\n";
      }
      std::cout.flush();
      
      return -1;
   }
   catch (...)
   {
      std::cout << "Caught unknown exception" << std::endl;
      return -1;
   }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create adc_sub.imake
#define SUBROUTINE adc_sub
#define MODULE_LIST \
   adc_Body.cc \
   adc_Connection.cc \
   adc_Geometry.cc \
   adc_GeometryMetaData.cc \
   adc_ImageData.cc \
   adc_Instrument.cc \
   adc_InstrumentData.cc \
   adc_ORB.cc \
   adc_Target.cc \
   adc_getspice95.cc \

#define P2_SUBLIB
#define LIB_P2SUB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define LIB_ACE_WRAPPERS
#define LIB_TAO
#define LIB_TAO_COS_NAMING
#define LIB_TAO_PORTABLE_SERVER
#define LIB_PTHREAD

/* #define LIB_LOCAL */ /* Remove before delivery. */
/* #define DEBUG */ /* Remove before delivery. */

$ Return
$!#############################################################################
$Doc_File:
$ create README-adc_sub.txt
$ DECK/DOLLARS="$ VOKAGLEVE"
// README-adc_sub.txt

The Anciliary Data Client (ADC) subroutines.
$ VOKAGLEVE
$ Return
$!#############################################################################
$Test_File:
$ create adc_test.cc
// adc_test.cc

#include "adc_types.h"
#include "adc_Instrument.h"
#include "adc_Connection.h"
#include "adc_ORB.h"

#include <iostream.h>

static void writeMatrix(ostream& os, const adc_Matrix33& mat)
{
   os << "[ ";
   for (int i=0; i < 3; ++i)
   {
      if (i > 0)
      {
         os << "  ";
      }
      for (int j=0; j < 3; ++j)
      {
         os << mat[i][j] << " ";
      }
      if (i >= 2)
      {
         os << " ]";
      }
      os << "\n";
   }
}

static ostream& operator<<(ostream& os, const adc_Matrix33& mat)
{
   writeMatrix(os, mat);
   return os;
}

static void writeVector(ostream& os, const adc_Vector3& v)
{
   os << "[ ";
   for (int i=0; i < 3; ++i)
   {
      os << v[i] << " ";
   }
   os << "]";   
}

static ostream& operator<<(ostream& os, const adc_Vector3& v)
{
   writeVector(os, v);

   return os;
}


static void writeImage(adc_ImageData& image)
{
     adc_Geometry geom = image.getGeometry();

      cout << "-- Geometry ---------------------------------" << endl;

      cout << "spacecraftVector\n";
      writeVector(cout, geom.spacecraftVector());
      cout << endl;

      cout <<		    "solarVector\n";
      writeVector(cout, geom.solarVector());
      cout << endl;

      cout << "planetVector\n";
      writeVector(cout,  geom.planetVector() );
      cout << endl;

      cout << "spacecraftVelocityVector\n";
      writeVector(cout, geom.spacecraftVelocityVector());
      cout << endl;

      cout << "instrumentPointing\n";
      writeMatrix(cout, geom.instrumentPointing());
      cout << endl;

      cout << "targetOrientation\n";
      writeMatrix(cout, geom.targetOrientation());
      cout << endl;

      adc_GeometryMetaData md = geom.metaData();

      cout << "-- Geometry metadata -----------------------" << endl;

      cout << "user = '" << md.user() << "'\n"
           << "facility = '" << md.facility() << "'\n"
           << "application = '" << md.application() << "'\n"
           << "notes = '" << md.notes() << "'"
           << endl;

      adc_InstrumentData inst = image.getInstrumentData();

      cout << "-- Instrument ---------------------------------" << endl;      

      cout << "cameraBoresightOffset = " << inst.cameraBoresightOffset()
           << "\nfocalLength = " << inst.focalLength()
           << "\npictureScale = " << inst.pictureScale()
           << "\nopticalAxisOffset = " << inst.opticalAxisOffset()
           << endl;

      adc_Target target = image.getTarget();

      cout << "-- Target ---------------------------------" << endl;

      cout << "target radii\n";
      writeVector(cout, target.radii());
		cout << "\nrotationRate = " << target.rotationRate()
           << endl;

      cout << "-------------------------------------------" << endl;
}

int main(int argc, char* argv[])
{
   try
   {  
      // First get the original NAIF data.
 
      adc_Instrument iss(jpl::mipl::spice::corba::CASSINI_ISS_NAC_FULL);
      
      adc_Connection con (iss);
      
      adc_Body saturn = con.createBody(adc_Body::SATURN);
      
      
      const char scet[] = "2001-194::8:02:07.55";
      adc_GeometryMetaData mdata;
      mdata.setFacility("NAIF");
      adc_ImageData image = con.createImageData(saturn, scet, mdata);
      
      writeImage(image);


      // Now store an update.

      adc_Matrix33 m = { {1.0, 1.0, 1.0}, {2.0, 2.0, 2.0}, {3.0, 3.0, 3.0} };
      adc_Vector3 v = { 1.0, 2.0, 3.0 };

      adc_Geometry geom = image.getGeometry();
      geom.setSpacecraftVector(v);
      geom.setSolarVector(v);
      geom.setPlanetVector(v);
      geom.setSpacecraftVelocityVector(v);
      geom.setInstrumentPointing(m);
      geom.setTargetOrientation(m);
      
      adc_GeometryMetaData md;
      md.setUser("Test user");
      md.setFacility("Test Facility");
      md.setApplication("test");
      md.setNotes("No note");
      geom.setMetaData(md);

      image.updateGeometry(geom);

      
      // Now search for the update with a second connection.

      adc_Connection con2(iss);

      image = con.createImageData(saturn, scet, md);
      writeImage(image);

   }
   catch (jpl::mipl::spice::corba::SpiceLibFactory::FailedToCreate& ex)
   {
      cout << "Failed to create exception:\n"
           << ex.msg.in() << endl;

      return 1;
   }
   catch (jpl::mipl::spice::corba::SpiceLib::ToolkitException& ex)
   {
      cout << "Toolkit Exception:\n"
           << "Name: " << ex.name.in() << "\n"
           << "Desc: " << ex.description.in() << "\n"
           << "Long: " << ex.longMessage.in() << "\n";
      
      cout << "Trace:\n";
      for (size_t i=0; i < ex.trace.length(); ++i)
      {
         cout << ex.trace[i] << "\n";
      }
      cout.flush();

      return 2;
   } 
   catch (jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound& ex)
   {
      cout << "PointingNotFound exception." << endl;

      return 3;
   } 
   catch (CORBA::SystemException& ex)
   {
      cout << "CORBA::SystemException" << endl;

      ACE_PRINT_EXCEPTION ( ex, "adc_test" );

      return 4;
   }

   return 0;
}
$!-----------------------------------------------------------------------------
$ create adc_test.imake
#define PROGRAM adc_test
#define MODULE_LIST \
        adc_test.cc

#define LIB_P2SUB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define LIB_ACE_WRAPPERS
#define LIB_TAO
#define LIB_PTHREAD

/* #define LIB_LOCAL */ /* Remove before delivery. */
/* #define DEBUG */ /* Remove before delivery. */
$ Return
$!#############################################################################
