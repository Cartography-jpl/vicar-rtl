$!****************************************************************************
$!
$! Build proc for MIPL module ads_server_h
$! VPACK Version 1.9, Monday, April 29, 2002, 10:57:37
$!
$! Execute by entering:		$ @ads_server_h
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
$ write sys$output "*** module ads_server_h ***"
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
$ write sys$output "Invalid argument given to ads_server_h.com file -- ", primary
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
$   if F$SEARCH("ads_server_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @ads_server_h.bld "STD"
$   else
$      @ads_server_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create ads_server_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack ads_server_h.com -mixed -
	-s ads_Body.h ads_CassiniDataFinder.h ads_DataBase.h -
	   ads_DefaultDataFinder.h ads_DefaultGeometryFinder.h ads_error.h -
	   ads_FixedInstGeometryFinder.h ads_Frame.h ads_GeometryFinder.h -
	   ads_getcamcon.h ads_MissionDataFinder.h -
	   ads_Rotate180GeometryFinder.h ads_ServerHelper.h ads_SpiceLib_i.h -
	   ads_ORB.h ads_Time.h ads_TraceAdder.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ads_Body.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_Body.h
//
// April 9, 2002
// Michael Brady

#if !defined ADS_BODY_H_INCLUDED
#define ADS_BODY_H_INCLUDED

#include "SpiceUsr.h"

#include "ads_Frame.h"

#include <string>
using std::string;

/**
 *  A class which holds a single NAIF body ID.
 */
class ads_Body
{
public:
   static const ads_Body& sun();
  
public:
   /** Constructs a new Body object with the specified NAIF body ID. */
   explicit ads_Body(SpiceInt naifId);
  
   /** Returns the value of this object as a NAIF ID. */
   SpiceInt naifId() const;
  
   // Returns true if this body is a valid moon code, false otherwise.
   bool isMoon() const;
  
   // Returns the planet of which this body is a moon.
   // If this body is not a moon, the result is undefined.
   //
   // The NAIF ID for a planet is the moon ID rounded up to 99.
   // For example, moon 301 orbits planet 399.
   ads_Body centerPlanet() const;
  
   // Returns the value of the spacecraft to which this instrument is attached.
   // If this is not an instrument, behavior is undefined.
   ads_Body spacecraft() const;

   // Returns the value of the reference frame for this spacecraft.
   // If this body is not a spacecraft, results are undefined.
   ads_Frame scFrame() const;
  
   // Returns the name of the reference frame of which this object
   // is the center.
   string frameName() const;

   // Returns the name of this body, as specified by the SPICE toolkit.
   string name() const;

private:
   SpiceInt m_naifId;
};

#endif // !defined ADS_BODY_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_CassiniDataFinder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_CassiniDataFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_CASSINI_DATA_FINDER_H_INCLUDED
#define ADS_CASSINI_DATA_FINDER_H_INCLUDED


#include "ads_DefaultDataFinder.h"
#include "ads_GeometryFinder.h"

using namespace jpl::mipl::spice::corba;

/**
 *  Finds Cassini ISS image information.
 */
class ads_CassiniDataFinder : public ads_DefaultDataFinder
{
public:

   /** Creates a new CassiniDataFinder */
   ads_CassiniDataFinder();
  
  /** Destructor. */
  virtual ~ads_CassiniDataFinder();
     
  /** Inherited from ads_MissionDataFinder. */
   virtual void getGeometryNoUpdates (int instNaifId,
                                      int targetNaifId,
                                      const char* utcTime,
                                      const char* referenceFrame,
                                      double tolerance,
                                      Geometry_out theGeometry,
                                      GeometryMetaData_out metaData)
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound );

private:
   std::auto_ptr<ads_GeometryFinder> m_geometryFinder;
};


#endif // !defined ADS_CASSINI_DATA_FINDER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_DataBase.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_DataBase.h
//
// Apr 25, 2002
// Michael Brady

#if !defined ADS_DATABASE_H_INCLUDED
#define ADS_DATABASE_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

/**
 *  Encapsulates access to the geometry update data base.
 */
class ads_DataBase
{
private:
   /** Creates a new ads_DataBase object. */
   ads_DataBase();

   /** Destructor. */
   virtual ~ads_DataBase();

   /** Creates a new ads_DataBase object 
    *  which is a copy of the specified object. 
    */
   ads_DataBase(const ads_DataBase& src);

   /** Sets this to the value of the specified object. */
   ads_DataBase& operator=(const ads_DataBase& rhs);
   
public:

   /** Finds a geometry which matches the specified criteria.
    * @return true if one was found, else false.
    */
   static 
   bool findGeometry(const jpl::mipl::spice::corba::ImageData & image,
                     const jpl::mipl::spice::corba::GeometryMetaData & criteria,
                     jpl::mipl::spice::corba::Geometry_out theGeometry,
                     jpl::mipl::spice::corba::GeometryMetaData_out metaData );


   /** Stores the specified geometry with the specified meta data.
    */
   static
   void storeGeometry(
            const jpl::mipl::spice::corba::ImageData & image,
            const jpl::mipl::spice::corba::Geometry & theGeometry,
            const jpl::mipl::spice::corba::GeometryMetaData & theMetaData );


};

#endif // !defined ADS_DATABASE_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_DefaultDataFinder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_DefaultDataFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_DEFAULT_DATA_FINDER_H_INCLUDED
#define ADS_DEFAULT_DATA_FINDER_H_INCLUDED


#include "ads_MissionDataFinder.h"
#include "ads_DefaultGeometryFinder.h"

using namespace jpl::mipl::spice::corba;

/**
 *  Finds image information.  
 *  A default implementation of ads_MissionDataFinder.
 */
class ads_DefaultDataFinder : public ads_MissionDataFinder
{
public:

   /** Creates a new DefaultDataFinder */
   ads_DefaultDataFinder();
  
  /** Destructor. */
  virtual ~ads_DefaultDataFinder();
     
  /** Inherited from ads_MissionDataFinder. */
  virtual void getGeometryNoUpdates (int instNaifId,
                                     int targetNaifId,
                                     const char* utcTime,
                                     const char* referenceFrame,
                                     double tolerance,
                                     Geometry_out theGeometry,
                                     GeometryMetaData_out metaData)
     throw ( SpiceLib::ToolkitException,
             MiplSpiceLib::PointingNotFound );

   /** Inherited from ads_MissionDataFinder. */
   virtual void getGeometry ( const ImageData & image,
                              const GeometryMetaData & criteria,
                              Geometry_out theGeometry,
                              GeometryMetaData_out metaData )
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound );
          
   /** Inherited from ads_MissionDataFinder. */
   virtual void storeGeometry (
      const jpl::mipl::spice::corba::ImageData & image,
      const jpl::mipl::spice::corba::Geometry & theGeometry,
      const jpl::mipl::spice::corba::GeometryMetaData & theMetaData
      )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException);

   /** Inherited from ads_MissionDataFinder. */
   virtual void getInstrument ( const ImageData & image,
                                InstrumentData_out theInstrument)
      throw ( SpiceLib::ToolkitException );
          
   /** Inherited from ads_MissionDataFinder. */
   virtual void getTarget ( const ImageData & image,
                            Target_out theTarget )
      throw ( SpiceLib::ToolkitException );

};


#endif // !defined ADS_DEFAULT_DATA_FINDER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_DefaultGeometryFinder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_DefaultGeometryFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_DEFAULT_GEOMETRY_FINDER_H_INCLUDED
#define ADS_DEFAULT_GEOMETRY_FINDER_H_INCLUDED


#include "ads_GeometryFinder.h"

#include "jpl_mipl_spice_corba.C.h"

using namespace jpl::mipl::spice::corba;

/**
 *  A default implementation of GeometryFinder which will work for
 *  instruments which have their own C kernel.
 */
class ads_DefaultGeometryFinder : public ads_GeometryFinder
{
public:
  
  /** Destructor. */
  virtual ~ads_DefaultGeometryFinder();
     
  /** Inherited from ads_GeometryFinder. */
  virtual void getGeometry (int instNaifId,
                            int targetNaifId,
                            const char* utcTime,
                            const char* referenceFrame,
                            double tolerance,	
                            Geometry_out theGeometry,
                            GeometryMetaData_out metaData)
    throw ( SpiceLib::ToolkitException,
	    MiplSpiceLib::PointingNotFound );

};


#endif // !defined ADS_DEFAULT_GEOMETRY_FINDER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_error.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_error.h
//
// Apr 18, 2002
// Michael Brady

#if !defined ADS_ERROR_H_INCLUDED
#define ADS_ERROR_H_INCLUDED

/** If there is a SPICE error currently in effect, 
 *  resets the SPICE error status to 'no error' and throws a ToolkitException
 *  based on the current error.
 */
void ads_checkForError();

#endif // !defined ADS_ERROR_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_FixedInstGeometryFinder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_FixedInstGeometryFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_FIXED_INST_GEOMETRY_FINDER_H_INCLUDED
#define ADS_FIXED_INST_GEOMETRY_FINDER_H_INCLUDED


#include "ads_GeometryFinder.h"

#include <memory>

using namespace jpl::mipl::spice::corba;

/**
 *  Finds the pointing for instruments which are fixed to the
 *  spacecraft and whose offset is defined in a SPICE frame kernel.
 */
class ads_FixedInstGeometryFinder : public ads_GeometryFinder
{
public:
   /** Creates a FixedInstGeometryFinder which uses the specified
    * finder to find the geometry of the spacecraft to which the
    * instrument is attached.
    *
    * @param finder should be allocated using 'new'.  This object
    *               takes ownership and will 'delete' it.
    */
   ads_FixedInstGeometryFinder(ads_GeometryFinder* finder);
  
  /** Destructor. */
  virtual ~ads_FixedInstGeometryFinder();
     
  /** Inherited from ads_GeometryFinder. */
  virtual void getGeometry (int instNaifId,
                            int targetNaifId,
                            const char* utcTime,
                            const char* referenceFrame,
                            double tolerance,
                            Geometry_out theGeometry,
                            GeometryMetaData_out metaData )
     throw ( SpiceLib::ToolkitException,
             MiplSpiceLib::PointingNotFound );

private:
   std::auto_ptr<ads_GeometryFinder> m_finder;

};


#endif // !defined ADS_FIXED_INST_GEOMETRY_FINDER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_Frame.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_Frame.h
//
// Apr 19, 2002
// Michael Brady

#if !defined ADS_FRAME_H_INCLUDED
#define ADS_FRAME_H_INCLUDED

#include <string>

/**
 * A SPICE Toolkit frame.
 */
class ads_Frame
{
public:
   /** Creates a new ads_Frame object. */
   ads_Frame(int naifId);

   /** Destructor. */
   virtual ~ads_Frame();

   /** Creates a new ads_Frame object as a copy of the specified object. */
   ads_Frame(const ads_Frame& src);

   /** Sets this to the value of the specified object. */
   ads_Frame& operator=(const ads_Frame& rhs);

   /** Returns the NAIF ID for this frame. */
   int naifId() const;
   
   /** Returns the name of this frame. */
   std::string name() const;

private:

   int m_naifId;
};

#endif // !defined ADS_FRAME_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_GeometryFinder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_GeometryFinder.h
//
// Apr 18, 2002
// Michael Brady

#if !defined ADS_GEOMETRYFINDER_H_INCLUDED
#define ADS_GEOMETRYFINDER_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

using namespace jpl::mipl::spice::corba;

/**
 * Abstract class which defines the GeometryFinder interface.
 * This is the base class of a decorator hierarchy.
 * (For more information on the Decorator pattern, see "Design Patterns"
 *  by Gamma, et al).
 */
class ads_GeometryFinder
{
public:


   virtual ~ads_GeometryFinder() = 0;

   virtual void getGeometry (int instNaifId,
                             int targetNaifId,
                             const char* utcTime,
                             const char* referenceFrame,
                             double tolerance,
                             Geometry_out theGeometry,
                             GeometryMetaData_out metaData )
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound ) = 0;   

};

#endif // !defined ADS_GEOMETRYFINDER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_getcamcon.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_getcamcon.h
//
// Apr 22, 2002
// Michael Brady

#if !defined ADS_GETCAMCON_H_INCLUDED
#define ADS_GETCAMCON_H_INCLUDED

/**
 *  Wrapper for the VICAR getcamcon function.
 */
void ads_getcamcon( const char* miplMissionName,
                    int miplCameraId,
                    double focalLength,
                    double lineObjectSpace,
                    double sampleObjectSpace,
                    double scale );


#endif // !defined ADS_GETCAMCON_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_MissionDataFinder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_MissionDataFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_DATA_FINDER_H_INCLUDED
#define ADS_DATA_FINDER_H_INCLUDED

#include <memory>

#include "jpl_mipl_spice_corba.C.h"

/**
 *  This class encapsulates the mission-specific data finding process.
 */
class ads_MissionDataFinder
{
public:
   
   virtual ~ads_MissionDataFinder() = 0;
   
   /** Gets geometry without looking at updates. */
   virtual void getGeometryNoUpdates (
          int instNaifId,
          int targetNaifId,
          const char* utcTime,
          const char* referenceFrame,
          double tolerance,
	       jpl::mipl::spice::corba::Geometry_out theGeometry,
	       jpl::mipl::spice::corba::GeometryMetaData_out metaData
          )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException,
              jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound ) = 0;
   
   /** Gets a geometry which matches the specified criteria. */
   virtual void getGeometry (
         const jpl::mipl::spice::corba::ImageData & image,
         const jpl::mipl::spice::corba::GeometryMetaData & criteria,
         jpl::mipl::spice::corba::Geometry_out theGeometry,
         jpl::mipl::spice::corba::GeometryMetaData_out metaData
         )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException,
              jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound ) = 0;

   virtual void storeGeometry (
      const jpl::mipl::spice::corba::ImageData & image,
      const jpl::mipl::spice::corba::Geometry & theGeometry,
      const jpl::mipl::spice::corba::GeometryMetaData & theMetaData
      )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) = 0;
          
    virtual void getInstrument (
              const jpl::mipl::spice::corba::ImageData & image,
              jpl::mipl::spice::corba::InstrumentData_out theInstrument
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) = 0;
          
    virtual void getTarget (
              const jpl::mipl::spice::corba::ImageData & image,
              jpl::mipl::spice::corba::Target_out theTarget
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) = 0;


   static
   ads_MissionDataFinder* 
   createDataFinder(jpl::mipl::spice::corba::Instrument inst);


};


#endif // !defined ADS_DATA_FINDER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_Rotate180GeometryFinder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_Rotate180GeometryFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_ROTATE_180_GEOMETRY_FINDER_H_INCLUDED
#define ADS_ROTATE_180_GEOMETRY_FINDER_H_INCLUDED


#include "ads_GeometryFinder.h"

using namespace jpl::mipl::spice::corba;

#include <memory>

/**
 *  Gets the geometry from another GeometryFinder, then
 *  rotates the instrument pointing 180 degrees.
 */
class ads_Rotate180GeometryFinder : public ads_GeometryFinder
{
public:

   /** Creates a Rotate180GeometryFinder which gets geometry info
    *  from  the specified
    * finder, then rotates the instrument pointing by 180 degrees.
    *
    * @param finder should be allocated using 'new'.  This object
    *               takes ownership and will 'delete' it.
    */  
   ads_Rotate180GeometryFinder(ads_GeometryFinder* finder);
   
   /** Destructor. */
   virtual ~ads_Rotate180GeometryFinder();
   
   /** Inherited from ads_DataFinder. */
   virtual void getGeometry (int instNaifId,
                             int targetNaifId,
                             const char* utcTime,
                             const char* referenceFrame,
                             double tolerance,
                             Geometry_out theGeometry,
                             GeometryMetaData_out metaData)
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound );

private:
   std::auto_ptr<ads_GeometryFinder> m_finder;
};


#endif // !defined ADS_ROTATE_180_GEOMETRY_FINDER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_ServerHelper.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_ServerHelper.h
//
// August 27, 2001
// Michael Brady

#if !defined ADS_SERVER_HELPER_H_INCLUDED
#define ADS_SERVER_HELPER_H_INCLUDED

#include "tao/PortableServer/PortableServer.h"
#include "tao/PortableServer/Servant_Base.h"

/**
 *  <P>A utility class for CORBA servers.</P>
 *
 *  <P>After creating all objects of this class which will be used in his
 *  program, the user should run the orb event loop by calling:</P>
 *
 *  <P><CODE>TLM_ORB::instance()->run();</CODE></P>
 */
class ads_ServerHelper
{
public:
  
  /** <P>Creates a new ServerHelper for the specified servant.</P>
   *
   *  <P>It is the caller's responsibility to initialize 
   *  the ORB before calling
   *  this method.  If you do not understand why you would want to 
   *  initialize the ORB yourself, you should probably use the other
   *  constructor.</P>
   *
   *  <P>The servant will not be deleted by this object, so if it was
   *  created via 'new', it is the caller's responsibility 
   *  to delete it.</P>
   *
   *  @param servant If null, behavior is undefined.
   */
  ads_ServerHelper(PortableServer::Servant servant);
  
  /** <P>Creates a new ServerHelper for the specified servant 
   *  and initializes 
   *  the ORB with the specified parameters.</P>
   *
   *  <P>The servant will not be deleted by this object, so if it was
   *  created via 'new', 
   *  it is the caller's responsibility to delete it.</P>
   *
   *  @param servant If null, behavior is undefined.
   */
  ads_ServerHelper(int& argc, 
		   char* argv[], 
		   PortableServer::Servant servant);
  
  /** Destructor */
  ~ads_ServerHelper();
  
  
  /**
   *  Writes this object's servant's reference (IOR) to the specified file.
   */
  void writeToFile(const ACE_CString& fileName);
  
  /**
   *  Writes this object's servant's reference (IOR) to the specified file
   *  if there is a "-o fileName" on the command line.
   */
  void writeToFile(int& argc, char* argv[]);
  
  /** Returns the POA in which the servant is registered. */
  PortableServer::POA_var thePoa();
  
  /** Starts the ORB event loop. */
  void run();
  
  /** Halts the ORB event loop. */
  void halt();
  
private:

  void init(PortableServer::Servant servant);

  void deactivate();
  
  void fini();
  
  
private:
  
  PortableServer::POA_var poa_;
  PortableServer::Servant servant_;
  CORBA::Object_var servantObject_;
      
  int i_own_the_orb_;
  int has_been_deactivated_;
  
};

#endif // !defined ADS_SERVER_HELPER_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_SpiceLib_i.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_SpiceLib_i.h

#ifndef ADS_SPICELIB_I_H_INCLUDED
#define ADS_SPICELIB_I_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"
#include "jpl_mipl_spice_corba.S.h"

#include "ads_MissionDataFinder.h"

#include <memory>

/** Implementation of the jpl.mipl.spice.corba.SpiceLib CORBA IDL interface.
 */
class ads_SpiceLib_i : public virtual POA_jpl::mipl::spice::corba::MiplSpiceLib
{
    public:
    ads_SpiceLib_i ();
    ads_SpiceLib_i (PortableServer::POA_ptr poa);
    ads_SpiceLib_i (const ads_SpiceLib_i& rhs);
    virtual ~ads_SpiceLib_i (void);
    
    virtual PortableServer::POA_ptr _default_POA();
    
    
    private:
    PortableServer::POA_ptr m_poa;

   jpl::mipl::spice::corba::SpiceLibFactory_var m_factory;

   CORBA::String_var m_loadFilesHint;

   CORBA::Long m_id;
    
    ///////////////////////////////////////////////////////////
    // Non-SPICE toolkit methods
    ///////////////////////////////////////////////////////////
    
    public:
    
           virtual void getGeometry (
               const jpl::mipl::spice::corba::ImageData & image,
              const jpl::mipl::spice::corba::GeometryMetaData & criteria,
	       jpl::mipl::spice::corba::Geometry_out theGeometry,
	       jpl::mipl::spice::corba::GeometryMetaData_out metaData,
	       CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
	     )
	     throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException,
		     jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound );

          virtual void storeGeometry (
              const jpl::mipl::spice::corba::ImageData & image,
              const jpl::mipl::spice::corba::Geometry & theGeometry,
              const jpl::mipl::spice::corba::GeometryMetaData & theMetaData,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
             throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException );

          virtual void getInstrument (
              const jpl::mipl::spice::corba::ImageData & image,
              jpl::mipl::spice::corba::InstrumentData_out theInstrument,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException );
          
          virtual void getTarget (
              const jpl::mipl::spice::corba::ImageData & image,
              jpl::mipl::spice::corba::Target_out theTarget,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException );



          virtual void init (
              jpl::mipl::spice::corba::SpiceLibFactory_ptr factory,
              CORBA::Long id,
              const char * loadFilesHint,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment ()
            )
             throw (jpl::mipl::spice::corba::SpiceLib::ToolkitException);

   virtual 
   CORBA::Long id (CORBA::Environment &ACE_TRY_ENV=TAO_default_environment())
      throw ();
          



        virtual void loadFiles (
            const char * hint,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

   virtual char * 
   loadFilesHint(CORBA::Environment &ACE_TRY_ENV = TAO_default_environment())
      throw ();

   virtual void 
   close (CORBA::Environment &ACE_TRY_ENV = TAO_default_environment())
      throw ();

        virtual void shutdown (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ();



private:

   std::auto_ptr<ads_MissionDataFinder> m_dataFinder;

	///////////////////////////////////////////////////////////
        // SPICE toolkit methods
	///////////////////////////////////////////////////////////

public:

        virtual void axisar (
            const jpl::mipl::spice::corba::Vector3 axis,
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::Matrix33_out r,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;


        virtual jpl::mipl::spice::corba::SpDouble b1900 (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble b1950 (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void bodc2n (
            jpl::mipl::spice::corba::SpInt code,
            CORBA::String_out name,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void boddef (
            const char * name,
            jpl::mipl::spice::corba::SpInt code,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;


        virtual CORBA::Boolean bodfnd (
            jpl::mipl::spice::corba::SpInt body,
            const char * item,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw() ;

        virtual void bodn2c (
            const char * name,
            jpl::mipl::spice::corba::SpInt_out code,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;


        virtual void bodvar (
            jpl::mipl::spice::corba::SpInt body,
            const char * item,
            jpl::mipl::spice::corba::SpInt_out dim,
            jpl::mipl::spice::corba::SpDouble8_out values,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void cgv2el (
            const jpl::mipl::spice::corba::Vector3 center,
            const jpl::mipl::spice::corba::Vector3 vec1,
            const jpl::mipl::spice::corba::Vector3 vec2,
            jpl::mipl::spice::corba::SpEllipse_out ellipse,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void chkin (
            const char * moduleName,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void chkout (
            const char * moduleName,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void cidfrm (
            jpl::mipl::spice::corba::SpInt cent,
            jpl::mipl::spice::corba::SpInt_out frcode,
            CORBA::String_out frname,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void ckgp (
            jpl::mipl::spice::corba::SpInt inst,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            jpl::mipl::spice::corba::SpDouble tol,
            const char * ref,
            jpl::mipl::spice::corba::Matrix33_out cmat,
            jpl::mipl::spice::corba::SpDouble_out clkout,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void ckgpav (
            jpl::mipl::spice::corba::SpInt inst,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            jpl::mipl::spice::corba::SpDouble tol,
            const char * ref,
            jpl::mipl::spice::corba::Matrix33_out cmat,
            jpl::mipl::spice::corba::Vector3_out av,
            jpl::mipl::spice::corba::SpDouble_out clkout,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpDouble clight (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void cnmfrm (
            const char * cname,
            jpl::mipl::spice::corba::SpInt_out frcode,
            CORBA::String_out frname,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;


        virtual void conics (
            const jpl::mipl::spice::corba::SpDouble8 elts,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::State_out theState,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void convrt (
            jpl::mipl::spice::corba::SpDouble x,
            const char * inStr,
            const char * outStr,
            jpl::mipl::spice::corba::SpDouble_out y,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void cyllat (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble lonc,
            jpl::mipl::spice::corba::SpDouble z,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out lat,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void cylrec (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble z,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void cylsph (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble lonc,
            jpl::mipl::spice::corba::SpDouble z,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out colat,
            jpl::mipl::spice::corba::SpDouble_out lon,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble det (
            const jpl::mipl::spice::corba::Matrix33 m1,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble dpmax (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble dpmin (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble dpr (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void dtpool (
            const char * name,
            CORBA::Boolean_out found,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::SpChar1_out type,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpDouble dvdot (
            const jpl::mipl::spice::corba::State s1,
            const jpl::mipl::spice::corba::State s2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void dvhat (
            const jpl::mipl::spice::corba::State s1,
            jpl::mipl::spice::corba::State_out sout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void edlimb (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 viewpt,
            jpl::mipl::spice::corba::SpEllipse_out limb,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;


          virtual void ekccnt (
              const char * table,
              jpl::mipl::spice::corba::SpInt_out ccount,
              CORBA::Environment &ACE_TRY_ENV =  TAO_default_environment()
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException );
          

          virtual void ekcii (
              const char * table,
              jpl::mipl::spice::corba::SpInt cindex,
              CORBA::String_out column,
              jpl::mipl::spice::corba::EKAttDsc_out attdsc,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException );


        virtual void ekfind (
            const char * query,
            jpl::mipl::spice::corba::SpInt_out nmrows,
            CORBA::Boolean_out error,
            CORBA::String_out errmsg,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void ekgc (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            jpl::mipl::spice::corba::SpInt elment,
            CORBA::String_out cdata,
            CORBA::Boolean_out null,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;


        virtual void ekgd (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            jpl::mipl::spice::corba::SpInt elment,
            jpl::mipl::spice::corba::SpDouble_out ddata,
            CORBA::Boolean_out null,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void ekgi (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            jpl::mipl::spice::corba::SpInt elment,
            jpl::mipl::spice::corba::SpInt_out idata,
            CORBA::Boolean_out null,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;


        virtual jpl::mipl::spice::corba::SpInt eknelt (
            jpl::mipl::spice::corba::SpInt selidx,
            jpl::mipl::spice::corba::SpInt row,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

	  
          virtual void ekntab (
              jpl::mipl::spice::corba::SpInt_out n,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
	    throw ();
	  

        virtual void ekpsel (
            const char * query,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::SpIntSeq_out xbegs,
            jpl::mipl::spice::corba::SpIntSeq_out xends,
            jpl::mipl::spice::corba::EKDataTypeSeq_out xtypes,
            jpl::mipl::spice::corba::EKExprClassSeq_out xclass,
            jpl::mipl::spice::corba::stringSeq_out tabs,
            jpl::mipl::spice::corba::stringSeq_out cols,
            CORBA::Boolean_out error,
            CORBA::String_out errmsg,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void el2cgv (
            const jpl::mipl::spice::corba::SpEllipse & ellipse,
            jpl::mipl::spice::corba::Vector3_out center,
            jpl::mipl::spice::corba::Vector3_out smajor,
            jpl::mipl::spice::corba::Vector3_out sminor,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void et2utc (
            jpl::mipl::spice::corba::SpDouble et,
            const char * format,
            jpl::mipl::spice::corba::SpInt prec,
            CORBA::String_out utcstr,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void etcal (
            jpl::mipl::spice::corba::SpDouble et,
            CORBA::String_out str,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void eul2m (
            jpl::mipl::spice::corba::SpDouble angle3,
            jpl::mipl::spice::corba::SpDouble angle2,
            jpl::mipl::spice::corba::SpDouble angle1,
            jpl::mipl::spice::corba::SpInt axis3,
            jpl::mipl::spice::corba::SpInt axis2,
            jpl::mipl::spice::corba::SpInt axis1,
            jpl::mipl::spice::corba::Matrix33_out r,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void eul2xf (
            const jpl::mipl::spice::corba::SpDouble6 eulang,
            jpl::mipl::spice::corba::SpInt axisa,
            jpl::mipl::spice::corba::SpInt axisb,
            jpl::mipl::spice::corba::SpInt axisc,
            jpl::mipl::spice::corba::Matrix66_out xform,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void expool (
            const char * name,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        static void expool_skel (
            CORBA::ServerRequest &_tao_req, 
            void *_tao_obj, 
            void *_taoontext, 
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          );

        virtual void frame (
            jpl::mipl::spice::corba::Vector3 x,
            jpl::mipl::spice::corba::Vector3_out y,
            jpl::mipl::spice::corba::Vector3_out z,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void frinfo (
            jpl::mipl::spice::corba::SpInt frcode,
            jpl::mipl::spice::corba::SpInt_out cent,
            jpl::mipl::spice::corba::SpInt_out frclss,
            jpl::mipl::spice::corba::SpInt_out clssid,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

          virtual void frmnam (
              jpl::mipl::spice::corba::SpInt frcode,
              CORBA::String_out frname,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
	    throw ();

        virtual void gcpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt lenout,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::stringSeq_out cvals,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void gdpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::SpDoubleSeq_out values,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void georec (
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble lat,
            jpl::mipl::spice::corba::SpDouble alt,
            jpl::mipl::spice::corba::SpDouble re,
            jpl::mipl::spice::corba::SpDouble f,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void getfov (
            jpl::mipl::spice::corba::SpInt instid,
            jpl::mipl::spice::corba::SpInt room,
            CORBA::String_out shape,
            CORBA::String_out frame,
            jpl::mipl::spice::corba::Vector3_out bsight,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::Vector3Seq_out bounds,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void gipool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::SpIntSeq_out ivals,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void gnpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt start,
            jpl::mipl::spice::corba::SpInt room,
            jpl::mipl::spice::corba::SpInt lenout,
            jpl::mipl::spice::corba::SpInt_out n,
            jpl::mipl::spice::corba::stringSeq_out kvars,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpDouble halfpi (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void ident (
            jpl::mipl::spice::corba::Matrix33_out matrix,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void illum (
            const char * target,
            jpl::mipl::spice::corba::SpDouble et,
            const char * abcorr,
            const char * obsrvr,
            const jpl::mipl::spice::corba::Vector3 spoint,
            jpl::mipl::spice::corba::SpDouble_out phase,
            jpl::mipl::spice::corba::SpDouble_out solar,
            jpl::mipl::spice::corba::SpDouble_out emissn,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void inedpl (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::SpEllipse_out ellipse,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

          virtual void inelpl (
              const jpl::mipl::spice::corba::SpEllipse & ellips,
              const jpl::mipl::spice::corba::SpPlane & plane,
              jpl::mipl::spice::corba::SpInt_out nxpts,
              jpl::mipl::spice::corba::Vector3_out xpt1,
              jpl::mipl::spice::corba::Vector3_out xpt2,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void inrypl (
            const jpl::mipl::spice::corba::Vector3 vertex,
            const jpl::mipl::spice::corba::Vector3 dir,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::SpInt_out nxpts,
            jpl::mipl::spice::corba::Vector3_out xpt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpInt intmax (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpInt intmin (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void invert (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual CORBA::Boolean isrot (
            const jpl::mipl::spice::corba::Matrix33 m,
            jpl::mipl::spice::corba::SpDouble ntol,
            jpl::mipl::spice::corba::SpDouble dtol,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpDouble j1900 (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble j1950 (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble j2000 (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble j2100 (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble jyear (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void kdata (
            jpl::mipl::spice::corba::SpInt which,
            const char * kind,
            CORBA::String_out file,
            CORBA::String_out filtyp,
            CORBA::String_out source,
            jpl::mipl::spice::corba::SpInt_out handle,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void ktotal (
            const char * kind,
            jpl::mipl::spice::corba::SpInt_out count,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void latcyl (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble lat,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out lonc,
            jpl::mipl::spice::corba::SpDouble_out z,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void latrec (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble longitude,
            jpl::mipl::spice::corba::SpDouble latitude,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void latsph (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::SpDouble lat,
            jpl::mipl::spice::corba::SpDouble_out rho,
            jpl::mipl::spice::corba::SpDouble_out colat,
            jpl::mipl::spice::corba::SpDouble_out lons,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void ltime (
            jpl::mipl::spice::corba::SpDouble etobs,
            jpl::mipl::spice::corba::SpInt obs,
            const char * dir,
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble_out ettarg,
            jpl::mipl::spice::corba::SpDouble_out elapsd,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void m2eul (
            const jpl::mipl::spice::corba::Matrix33 r,
            jpl::mipl::spice::corba::SpInt axis3,
            jpl::mipl::spice::corba::SpInt axis2,
            jpl::mipl::spice::corba::SpInt axis1,
            jpl::mipl::spice::corba::SpDouble_out angle3,
            jpl::mipl::spice::corba::SpDouble_out angle2,
            jpl::mipl::spice::corba::SpDouble_out angle1,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void m2q (
            const jpl::mipl::spice::corba::Matrix33 r,
            jpl::mipl::spice::corba::Quaternion_out q,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void mequ (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void mtxm (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Matrix33 m2,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void mtxv (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Vector3 vin,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void mxm (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Matrix33 m2,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void mxmt (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Matrix33 m2,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void mxv (
            const jpl::mipl::spice::corba::Matrix33 m1,
            const jpl::mipl::spice::corba::Vector3 vin,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

          virtual void namfrm (
              const char * frname,
              jpl::mipl::spice::corba::SpInt_out frcode,
              CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
            )
	    throw ();

        virtual void nearpt (
            const jpl::mipl::spice::corba::Vector3 positn,
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            jpl::mipl::spice::corba::Vector3_out npoint,
            jpl::mipl::spice::corba::SpDouble_out alt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void npedln (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 linept,
            const jpl::mipl::spice::corba::Vector3 linedr,
            jpl::mipl::spice::corba::Vector3_out pnear,
            jpl::mipl::spice::corba::SpDouble_out dist,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void npelpt (
            const jpl::mipl::spice::corba::Vector3 point,
            const jpl::mipl::spice::corba::SpEllipse & ellips,
            jpl::mipl::spice::corba::Vector3_out pnear,
            jpl::mipl::spice::corba::SpDouble_out dist,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void nplnpt (
            const jpl::mipl::spice::corba::Vector3 linpt,
            const jpl::mipl::spice::corba::Vector3 lindir,
            const jpl::mipl::spice::corba::Vector3 point,
            jpl::mipl::spice::corba::Vector3_out pnear,
            jpl::mipl::spice::corba::Vector3_out dist,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void nvc2pl (
            const jpl::mipl::spice::corba::Vector3 normal,
            jpl::mipl::spice::corba::SpDouble constant,
            jpl::mipl::spice::corba::SpPlane_out plane,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void nvp2pl (
            const jpl::mipl::spice::corba::Vector3 normal,
            const jpl::mipl::spice::corba::Vector3 point,
            jpl::mipl::spice::corba::SpPlane_out plane,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void oscelt (
            const jpl::mipl::spice::corba::State theState,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::SpDouble mu,
            jpl::mipl::spice::corba::SpDouble8_out elts,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void pcpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt n,
            jpl::mipl::spice::corba::SpInt lenvals,
            const jpl::mipl::spice::corba::stringSeq & cvals,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void pdpool (
            const char * name,
            jpl::mipl::spice::corba::SpInt n,
            const jpl::mipl::spice::corba::SpDoubleSeq & dvals,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpDouble pi (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void pipool (
            const char * name,
            jpl::mipl::spice::corba::SpInt n,
            const jpl::mipl::spice::corba::SpIntSeq & ivals,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void pjelpl (
            const jpl::mipl::spice::corba::SpEllipse & elin,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::SpEllipse_out elout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void pl2nvc (
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out normal,
            jpl::mipl::spice::corba::SpDouble_out constant,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void pl2nvp (
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out normal,
            jpl::mipl::spice::corba::Vector3_out point,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void pl2psv (
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out point,
            jpl::mipl::spice::corba::Vector3_out span1,
            jpl::mipl::spice::corba::Vector3_out span2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void prop2b (
            jpl::mipl::spice::corba::SpDouble gm,
            const jpl::mipl::spice::corba::State pvinit,
            jpl::mipl::spice::corba::SpDouble dt,
            jpl::mipl::spice::corba::State_out pvprop,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void psv2pl (
            const jpl::mipl::spice::corba::Vector3 point,
            const jpl::mipl::spice::corba::Vector3 span1,
            const jpl::mipl::spice::corba::Vector3 span2,
            jpl::mipl::spice::corba::SpPlane_out plane,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void pxform (
            const char * from,
            const char * to,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix33_out rotate,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void q2m (
            const jpl::mipl::spice::corba::Quaternion q,
            jpl::mipl::spice::corba::Matrix33_out r,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void radrec (
            jpl::mipl::spice::corba::SpDouble range,
            jpl::mipl::spice::corba::SpDouble ra,
            jpl::mipl::spice::corba::SpDouble dec,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void rav2xf (
            const jpl::mipl::spice::corba::Matrix33 rot,
            const jpl::mipl::spice::corba::Vector3 av,
            jpl::mipl::spice::corba::Matrix66_out xform,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void raxisa (
            const jpl::mipl::spice::corba::Matrix33 matrix,
            jpl::mipl::spice::corba::Vector3_out axis,
            jpl::mipl::spice::corba::SpDouble_out angle,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void reccyl (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out z,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void recgeo (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble re,
            jpl::mipl::spice::corba::SpDouble f,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out lat,
            jpl::mipl::spice::corba::SpDouble_out alt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void reclat (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out longitude,
            jpl::mipl::spice::corba::SpDouble_out latitude,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void recrad (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out range,
            jpl::mipl::spice::corba::SpDouble_out ra,
            jpl::mipl::spice::corba::SpDouble_out dec,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void recsph (
            const jpl::mipl::spice::corba::Vector3 rectan,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out colat,
            jpl::mipl::spice::corba::SpDouble_out lon,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void rotate (
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::SpInt iaxis,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

       virtual void rotmat (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::SpInt iaxis,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;


        virtual void rotvec (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::SpDouble angle,
            jpl::mipl::spice::corba::SpInt iaxis,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble rpd (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void rquad (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            jpl::mipl::spice::corba::SpDouble2_out root1,
            jpl::mipl::spice::corba::SpDouble2_out root2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void saelgv (
            const jpl::mipl::spice::corba::Vector3 vec1,
            const jpl::mipl::spice::corba::Vector3 vec2,
            jpl::mipl::spice::corba::Vector3_out smajor,
            jpl::mipl::spice::corba::Vector3_out sminor,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void scdecd (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            CORBA::String_out sclkch,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void sce2c (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::SpDouble_out sclkdp,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void sce2s (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble et,
            CORBA::String_out sclkch,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void scencd (
            jpl::mipl::spice::corba::SpInt sc,
            const char * sclkch,
            jpl::mipl::spice::corba::SpDouble_out sclkdp,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void scfmt (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble ticks,
	    CORBA::String_out clkstr,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void scpart (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpInt_out nparts,
            jpl::mipl::spice::corba::SpDouble_out pstart,
            jpl::mipl::spice::corba::SpDouble_out pstop,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void scs2e (
            jpl::mipl::spice::corba::SpInt sc,
            const char * sclkch,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void sct2e (
            jpl::mipl::spice::corba::SpInt sc,
            jpl::mipl::spice::corba::SpDouble sclkdp,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void sctiks (
            jpl::mipl::spice::corba::SpInt sc,
            const char * clkstr,
            jpl::mipl::spice::corba::SpDouble_out ticks,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpDouble spd (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void sphcyl (
            jpl::mipl::spice::corba::SpDouble radius,
            jpl::mipl::spice::corba::SpDouble colat,
            jpl::mipl::spice::corba::SpDouble slon,
            jpl::mipl::spice::corba::SpDouble_out r,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out z,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void sphlat (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble colat,
            jpl::mipl::spice::corba::SpDouble lons,
            jpl::mipl::spice::corba::SpDouble_out radius,
            jpl::mipl::spice::corba::SpDouble_out lon,
            jpl::mipl::spice::corba::SpDouble_out lat,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void sphrec (
            jpl::mipl::spice::corba::SpDouble r,
            jpl::mipl::spice::corba::SpDouble colat,
            jpl::mipl::spice::corba::SpDouble lon,
            jpl::mipl::spice::corba::Vector3_out rectan,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void spkapo (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const jpl::mipl::spice::corba::State sobs,
            const char * abcorr,
            jpl::mipl::spice::corba::Vector3_out ptarg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkapp (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const jpl::mipl::spice::corba::State sobs,
            const char * abcorr,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkez (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkezp (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::Vector3_out ptarg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkezr (
            const char * targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            const char * obs,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkgeo (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::State_out starg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkgps (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            jpl::mipl::spice::corba::SpInt obs,
            jpl::mipl::spice::corba::Vector3_out pos,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkpos (
            const char * targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            const char * abcorr,
            const char * obs,
            jpl::mipl::spice::corba::Vector3_out ptarg,
            jpl::mipl::spice::corba::SpDouble_out lt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void spkssb (
            jpl::mipl::spice::corba::SpInt targ,
            jpl::mipl::spice::corba::SpDouble et,
            const char * ref,
            jpl::mipl::spice::corba::State_out starg,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void stelab (
            const jpl::mipl::spice::corba::Vector3 pobj,
            const jpl::mipl::spice::corba::Vector3 vobs,
            jpl::mipl::spice::corba::Vector3_out appobj,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void stpool (
            const char * item,
            jpl::mipl::spice::corba::SpInt nth,
            const char * contin,
            jpl::mipl::spice::corba::SpInt lenout,
            CORBA::String_out str,
            jpl::mipl::spice::corba::SpInt_out size,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void str2et (
            const char * str,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void subpt (
            const char * method,
            const char * target,
            jpl::mipl::spice::corba::SpDouble et,
            const char * abcorr,
            const char * obsrvr,
            jpl::mipl::spice::corba::Vector3_out spoint,
            jpl::mipl::spice::corba::SpDouble_out alt,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void subsol (
            const char * method,
            const char * target,
            jpl::mipl::spice::corba::SpDouble et,
            const char * abcorr,
            const char * obsrvr,
            jpl::mipl::spice::corba::Vector3_out spoint,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void surfnm (
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 point,
            jpl::mipl::spice::corba::Vector3_out normal,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void surfpt (
            const jpl::mipl::spice::corba::Vector3 positn,
            const jpl::mipl::spice::corba::Vector3 u,
            jpl::mipl::spice::corba::SpDouble a,
            jpl::mipl::spice::corba::SpDouble b,
            jpl::mipl::spice::corba::SpDouble c,
            jpl::mipl::spice::corba::Vector3_out point,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void sxform (
            const char * from,
            const char * to,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix66_out xform,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void timdef (
            const char * action,
            const char * item,
            jpl::mipl::spice::corba::SpInt lenout,
            CORBA::String_out value,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void timout (
            jpl::mipl::spice::corba::SpDouble et,
            const char * pictur,
            CORBA::String_out output,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void tipbod (
            const char * ref,
            jpl::mipl::spice::corba::SpInt body,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix33_out tipm,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void tisbod (
            const char * ref,
            jpl::mipl::spice::corba::SpInt body,
            jpl::mipl::spice::corba::SpDouble et,
            jpl::mipl::spice::corba::Matrix66_out tsipm,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual char * tkvrsn (
            const char * item,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void tparse (
            const char * str,
            jpl::mipl::spice::corba::SpDouble_out sp2000,
            CORBA::String_out errmsg,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void tpictr (
            const char * sample,
            CORBA::String_out pictur,
            CORBA::Boolean_out ok,
            CORBA::String_out errmsg,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void tsetyr (
            jpl::mipl::spice::corba::SpInt year,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble twopi (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void twovec (
            const jpl::mipl::spice::corba::Vector3 axdef,
            jpl::mipl::spice::corba::SpInt indexa,
            const jpl::mipl::spice::corba::Vector3 plndef,
            jpl::mipl::spice::corba::SpInt indexp,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual jpl::mipl::spice::corba::SpDouble tyear (
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void ucrss (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;


        virtual jpl::mipl::spice::corba::SpDouble unitim (
            jpl::mipl::spice::corba::SpDouble epoch,
            const char * insys,
            const char * outsys,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void unorm (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            jpl::mipl::spice::corba::SpDouble_out vmag,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

       virtual void utc2et (
            const char * utcstr,
            jpl::mipl::spice::corba::SpDouble_out et,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) ;

        virtual void vadd (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vcrss (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble vdist (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble vdot (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vequ (
            const jpl::mipl::spice::corba::Vector3 vin,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vhat (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vlcom3 (
            jpl::mipl::spice::corba::SpDouble a,
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::SpDouble b,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::SpDouble c,
            const jpl::mipl::spice::corba::Vector3 v3,
            jpl::mipl::spice::corba::Vector3_out sum,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vlcom (
            jpl::mipl::spice::corba::SpDouble a,
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::SpDouble b,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out sum,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vminus (
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble vnorm (
            const jpl::mipl::spice::corba::Vector3 v1,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vperp (
            const jpl::mipl::spice::corba::Vector3 a,
            const jpl::mipl::spice::corba::Vector3 b,
            jpl::mipl::spice::corba::Vector3_out p,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vprjp (
            const jpl::mipl::spice::corba::Vector3 vin,
            const jpl::mipl::spice::corba::SpPlane & plane,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vprjpi (
            const jpl::mipl::spice::corba::Vector3 vin,
            const jpl::mipl::spice::corba::SpPlane & projpl,
            const jpl::mipl::spice::corba::SpPlane & invpl,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Boolean_out found,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vproj (
            const jpl::mipl::spice::corba::Vector3 a,
            const jpl::mipl::spice::corba::Vector3 b,
            jpl::mipl::spice::corba::Vector3_out p,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble vrel (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vrotv (
            const jpl::mipl::spice::corba::Vector3 v,
            const jpl::mipl::spice::corba::Vector3 axis,
            jpl::mipl::spice::corba::SpDouble theta,
            jpl::mipl::spice::corba::Vector3_out r,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

         virtual void vscl (
            jpl::mipl::spice::corba::SpDouble s,
            const jpl::mipl::spice::corba::Vector3 v1,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble vsep (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void vsub (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Vector3 v2,
            jpl::mipl::spice::corba::Vector3_out vout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual jpl::mipl::spice::corba::SpDouble vtmv (
            const jpl::mipl::spice::corba::Vector3 v1,
            const jpl::mipl::spice::corba::Matrix33 matrix,
            const jpl::mipl::spice::corba::Vector3 v2,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual CORBA::Boolean vzero (
            const jpl::mipl::spice::corba::Vector3 v,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void xf2eul (
            const jpl::mipl::spice::corba::Matrix66 xform,
            jpl::mipl::spice::corba::SpInt axisa,
            jpl::mipl::spice::corba::SpInt axisb,
            jpl::mipl::spice::corba::SpInt axisc,
            jpl::mipl::spice::corba::SpDouble6_out eulang,
            CORBA::Boolean_out unique,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void xf2rav (
            const jpl::mipl::spice::corba::Matrix66 xform,
            jpl::mipl::spice::corba::Matrix33_out rot,
            jpl::mipl::spice::corba::Vector3_out av,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void xpose6 (
            const jpl::mipl::spice::corba::Matrix66 m,
            jpl::mipl::spice::corba::Matrix66_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;

        virtual void xpose (
            const jpl::mipl::spice::corba::Matrix33 m1,
            jpl::mipl::spice::corba::Matrix33_out mout,
            CORBA::Environment &ACE_TRY_ENV = TAO_default_environment()
          )
          throw () ;



      };  // ads_SpiceLib_i


#endif // ADS_SPICELIB_I_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_ORB.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_ORB.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_ORB_H_INCLUDED
#define ADS_ORB_H_INCLUDED

#include "tao/ORB.h"


class ads_ORB
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


#endif // !defined ADS_ORB_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_Time.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_Time.h
//
// April 9, 2002
// Michael Brady

#if !defined ADS_TIME_H_INCLUDED
#define ADS_TIME_H_INCLUDED

#include "SpiceUsr.h"


/**
 *  A class which holds a single epoch which can be converted between
 *  UTC, ephemeris, and spacecraft clock time.
 */
class ads_Time
{
    public:
    /** Constructs a new Time object with the specified UTC time. */
    ads_Time(const char* utcTime);

    /** Returns the value of this object as a spacecraft clock time. */
    SpiceDouble spacecraftClock(SpiceInt spacecraftId);

    /** Returns the value of this object in ephemeris time. */
    SpiceDouble ephemeris();

    private:
    SpiceDouble m_ephemeris;
};

#endif // !defined ADS_TIME_H_INCLUDED
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ads_TraceAdder.h
$ DECK/DOLLARS="$ VOKAGLEVE"
// ads_TraceAdder.h
//
// Apr 16, 2002
// Michael Brady

#if !defined ADS_TRACEADDER_H_INCLUDED
#define ADS_TRACEADDER_H_INCLUDED

#include <string>

/** This macro adds the current function to the SPICE stack trace.
 *  Put this at the beginning of the function.  It will add the current
 *  function to the stack trace, and automatically
 *  remove the current function from the stack trace when it goes out of scope.
 */
#define ADS_TRACE 
if 0           // was this, but disabled due to inability to find ads_traceObject()
#define ADS_TRACE \
ads_TraceAdder ads_traceObject ( __FILE__, __func__ );
#endif

/**
 *  At creation, adds the specified function name to the Spice trace,
 *  and removes it when the object goes out of scope.
 */
class ads_TraceAdder
{
public:
   /** Creates a new ads_TraceAdder object. */
   ads_TraceAdder(const char* fileName, const char* functionName);

   /** Destructor. */
   virtual ~ads_TraceAdder();

private:
   /** Creates a new ads_TraceAdder object as a copy of the specified object. */
   ads_TraceAdder(const ads_TraceAdder& src);

   /** Sets this to the value of the specified object. */
   ads_TraceAdder& operator=(const ads_TraceAdder& rhs);
   
private:

   std::string  m_functionName;
};

#endif // !defined ADS_TRACEADDER_H_INCLUDED
$ VOKAGLEVE
$ Return
$!#############################################################################
