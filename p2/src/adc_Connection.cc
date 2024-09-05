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
