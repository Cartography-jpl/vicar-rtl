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
