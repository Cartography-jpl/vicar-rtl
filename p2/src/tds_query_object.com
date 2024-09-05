$!****************************************************************************
$!
$! Build proc for MIPL module tds_query_object
$! VPACK Version 1.9, Tuesday, October 31, 2000, 13:41:18
$!
$! Execute by entering:		$ @tds_query_object
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
$ write sys$output "*** module tds_query_object ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
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
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tds_query_object.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
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
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tds_query_object.imake") .nes. ""
$   then
$      vimake tds_query_object
$      purge tds_query_object.bld
$   else
$      if F$SEARCH("tds_query_object.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tds_query_object
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tds_query_object.bld "STD"
$   else
$      @tds_query_object.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tds_query_object.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tds_query_object.com -mixed -
	-s TdsQuery.cc -
	-i tds_query_object.imake -
	-t tst_tds_query.cc tst_tds_query.imake README_tst tst_tds.upf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create TdsQuery.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

//////////////////////////////////////////////////////////////////////////////
//
//				TdsQuery.h
//
//      TdsQuery is a class that prepares and sends a PVL query to a TDS.
//  The query can be an existing PVL file or can be generated by components.
//  Once the query has be created and sent, this is not needed for further
//  TDS interface effort.
//
//////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <string.h>

#include "DataSinkSocket.h"
#include "TdsQuery.h"
#include "return_status.h"

//////////////////////////////////////////////////////////////////////////////
//
//				issueQuery
//
//	Attaches to the specified Host/Port via a socket and transmits the
//  AMMOS SFDU wrapped PVL object.  The connection to the TDS is then
//  transfered/copied and used by the SFDU source object.
//
//////////////////////////////////////////////////////////////////////////////
int	TdsQuery::issueQuery(
  char	*Host,
  int	PortNumber,
  DataSourceSocket	&TdsSource)
{ int	Status;
  SocketBase		Port;
  DataSinkSocket	QUERY;

  if (!_pvlGenerated) return RTN_MISSING_DATA;

  Status = Port.open(Host,PortNumber);
  if (RTN_FAILURE(Status))
  { cerr << RTN_DFLT_MSG(Status) << " ... openning TDS query port\n";
    return (Status);
  } //  else cout << "Port opened\n";

  Status = QUERY.use(Port);
  if (RTN_FAILURE(Status))
  { cerr << RTN_DFLT_MSG(Status) << " ... X-fering port to Query\n";
    return (Status);
  }

  Status = TdsSource.use(Port);
  if (RTN_FAILURE(Status))
  { cerr << RTN_DFLT_MSG(Status) << " ... X-fering port to SFDU Source\n";
    return (Status);
  }

  Status = _pvlSfdu.sendSfdu(&QUERY);
  if (RTN_FAILURE(Status))
  { cerr << RTN_DFLT_MSG(Status) << " ... sending PVL SFDU\n";
    return (Status);
  } // else cout << RTN_DFLT_MSG(Status) << " ... sending PVL SFDU\n";

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				loadPvlFromFile
//
//	Creates a PVL-SFDU object by read reading the specified file.  The
//  PVL file must only contain the PVL object and no SFDU headers/labels.
//
//////////////////////////////////////////////////////////////////////////////
int	TdsQuery::loadPvlFromFile(
  char	*PvlFile)
{ int	Status;

  Status = _pvlSfdu.ingestTdsPvlFile( PvlFile );
  if (RTN_SUCCESS(Status))
  { _pvlGenerated = 1;
    memset(_pvlBuffer,0,PVL_BUFFER_LTH);
    _pvlSfdu.extractPvlObject(_pvlBuffer,PVL_BUFFER_LTH);
  }

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				loadPvlFromSfdu
//
//	Creates a PVL-SFDU object by extracting the PVL object from an
//  exisiting SFDU object.
//
//////////////////////////////////////////////////////////////////////////////
int	TdsQuery::loadPvlFromSfdu(
  SfduPvl	&Sfdu)
{ int	Status;

  memset(_pvlBuffer,0,PVL_BUFFER_LTH);
  Status = Sfdu.extractPvlObject(_pvlBuffer,PVL_BUFFER_LTH);
  if (RTN_SUCCESS(Status))
  { Status = _pvlSfdu.ingestTdsPvlBuffer(_pvlBuffer);
    if (RTN_SUCCESS(Status)) _pvlGenerated = 1;
  }

  return Status;
}

//////////////////////////////////////////////////////////////////////////////
//
//				savePvlToFile
//
//	Writes the PVL object to the specified disk file.  No SFDU labels
//  are written to the file.
//
//////////////////////////////////////////////////////////////////////////////
int	TdsQuery::savePvlToFile(
  char	*PvlFile)
{

  return RTN_NOT_IMPLEMENTED;
}

//////////////////////////////////////////////////////////////////////////////
//
//				generatePvlBuffer
//
//	Generates an ASCII PVL object from the component keyword/value pairs.
//
//////////////////////////////////////////////////////////////////////////////
int	TdsQuery::generatePvlBuffer( void )
{

  return RTN_NOT_IMPLEMENTED;
}

//////////////////////////////////////////////////////////////////////////////
//
//				setTimeRange
//
//	Sets the component values for the start/end times of a PVL object
//  when the values are given as the strings: "NOW" or "FOREVER", or as
//  a formatted time range, SCET, ERT, RCT or SCLK.
//
//////////////////////////////////////////////////////////////////////////////
void	TdsQuery::setTimeRange(
  int	type,
  char	*start,
  char	*stop)
{

  return;
}


//////////////////////////////////////////////////////////////////////////////
//
//				setTimeRange
//
//	Sets the component values for the start/end times of a PVL object
//  when the values can be stored as a UNIX time.
//
//////////////////////////////////////////////////////////////////////////////
void	TdsQuery::setTimeRange(
  int	type,
  time_t	*start,
  time_t	*stop)
{

  return;
}


//////////////////////////////////////////////////////////////////////////////
//
//				timeRange
//
//	Returns the specified time range as strings.
//
//////////////////////////////////////////////////////////////////////////////
void	TdsQuery::timeRange(
  char	*start,
  char	*stop)
{

  return;
}


//////////////////////////////////////////////////////////////////////////////
//
//				timeRange
//
//	Returns the specified time range as UNIX times.  This will not be
//  appropriate for some time ranges (e.g., SCLKs or the NOW/FOREVER values).
//
//////////////////////////////////////////////////////////////////////////////
void	TdsQuery::timeRange(
  time_t	*start,
  time_t	*stop)
{

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				setDataType
//
//	Specifies the data type(s) to be queried from the TDS.  This value
//  must be defined in the TDS configuration, or the query will fail.
//
//////////////////////////////////////////////////////////////////////////////
void	TdsQuery::setDataType(
  char	*buffer)
{

  return;
}


//////////////////////////////////////////////////////////////////////////////
//
//				addDataType
//
//	Appends a data type to be queried from the TDS.
//
//////////////////////////////////////////////////////////////////////////////
void	TdsQuery::addDataType(
  char	*buffer)
{

  return;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tds_query_object.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tds_query_object
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tds_query_object			(VMS)
/*   or
/*		% vimake tds_query_object			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE tds_query_object		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST TdsQuery.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tds_query_object imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_tds_query.cc
//      Copyright (c) 1999, California Institute of Technology
//      U. S. Government sponsorship under NASA contract is acknowledged

#include <stdio.h>
#include <iostream.h>
#include <iomanip.h>    // Cause of main()
#include <stdlib.h>
#include <string.h>

#include "return_status.h"
#include "TdsQuery.h"
#include "SfduBase.h"
#include "DataSourceSocket.h"

#define	TDS_TIMEOUT	300		// 5 minutes

int	doCommand( const char * );

//////////////////////////////////////////////////////////////////////////////
//
//				main
//
//////////////////////////////////////////////////////////////////////////////

main(
  int	argc,
  char	*argv[])
{ char	commandFile[512];

  if (argc < 2)
  { cout << "Must supply an input filename: ";
    cin >> commandFile;
  } else strcpy(commandFile,argv[1]);

  doCommand(commandFile);

  return 0;
}

//////////////////////////////////////////////////////////////////////////////
//
//				doCommand
//
//////////////////////////////////////////////////////////////////////////////

int	doCommand(
  const char	*commandFile)
{ int	Idx,
	Status,
	TdsPort,
	PktCounter = 0;
  char  Keyword[32],
        Equal[4],
        Value[256],
	TdsHost[256],
	*Commands[] =
	{"TDS_HOST", "TDS_PORT", "PVL_FILE", "SFDU_LOG", 0};
  fstream	Param;
  TdsQuery	*Query;
  SfduBase	*Sfdu;
  DataSourceSocket	Tds;

  Param.open(commandFile,ios::in | ios::nocreate);
  if (Param.fail() || Param.bad())
  { cout << "\n*** Could not parameter file: " << commandFile << endl;
    return -1;
  } else cout << "Opened parameter file: " << commandFile << endl;


  Query = new TdsQuery;
  Sfdu = new SfduBase;

  while (Param >> Keyword >> Equal >> Value)
  {
    for (Idx = 0; Commands[Idx]; Idx++)
        if (strcmp(Keyword,Commands[Idx]) == 0)
    { switch(Idx)
      { case 0: // TDS_HOST
                strcpy(TdsHost,Value);
                break;
        case 1: // TDS_PORT
                TdsPort = atoi(Value);
                break;
        case 2: // PVL_FILE
                Status = Query->loadPvlFromFile(Value);
                if (!RTN_SUCCESS(Status))
                { cout << "\n*** Could not load PVL file: " << Value  << "; " <<
                       RTN_DFLT_MSG(Status) << endl;
                  return -1;
                }

                break;
        case 3: // SFDU_LOG
                Status = Sfdu->captureSfdu(Value);
                if (RTN_FAILURE(Status))
                   cout << "\n*** Could not log SFDUs to file: " << Value << 
                        "; " << RTN_DFLT_MSG(Status) << endl;
                else
                { cout << "Capturing SFDUs to: " << Value << endl;
                  //Sfdu->recordSfdu(PVL); // TDS automatically returns query
                }
                break;
        default:
                break;
      }
      break;
    }
  }

  Param.close();

  Status = Query->issueQuery(TdsHost,TdsPort,Tds);
  if (!RTN_SUCCESS(Status))
  { cout << "\n*** Could not Query TDS: " << TdsHost <<
         "(" << TdsPort << "); " << RTN_DFLT_MSG(Status);
    return -1;
  }

  /*** Snarf SFDUs ***/
  while (RTN_SUCCESS(Status = Sfdu->getNextSfdu(Tds,TDS_TIMEOUT)))
  { if (Sfdu->pktSfdu())
    { if (!((++PktCounter) % 100)) printf("Packet SFDUs: %d\n",PktCounter);
//    cout << "Packet SFDU\n";
    } else if (Sfdu->header.classId() == 'K')
    { printf("\nTDS Informational Message:\n%.*s\n",
             Sfdu->sfduSize()-(2*Sfdu->header.headerLength()),
             (char *)(Sfdu->buffer()+Sfdu->header.headerLength()));
    }

//  cout << "Sfdu " << Sfdu->sfduCounter() << " bytes: " << Sfdu->sfduSize() <<
//       endl; //" Status: " << RTN_DFLT_MSG(Status) << endl;
//  cout << endl << flush;
  }

  cout << "\n\nFinished w/ " << Sfdu->sfduCounter() << " SFDUs; ";
  cout << PktCounter << " Packet SFDUs\n";
  cout << "Status: " << RTN_DFLT_MSG(Status) << endl;

  delete Sfdu;

  return 0;
}
$!-----------------------------------------------------------------------------
$ create tst_tds_query.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_tds_query
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_tds_query			(VMS)
/*   or
/*		% vimake tst_tds_query			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define PROGRAM tst_tds_query		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_tds_query.cc


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_C_PLUS_PLUS
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C_PLUS_PLUS
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***	Others as needed
#define LIB_MDMS
#define LIB_SYBASE
#define LIB_NETWORK
#define LIB_MARSSUB
#define LIB_KERBEROS
#define DEBUG
/**/

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of tst_tds_query imake file  **********/
$!-----------------------------------------------------------------------------
$ create README_tst
There is 1 test program included for this module:
	tst_tds_query

'tst_tds_query' is basically the same as tst_tds_if, as such, valid UPF and
PVL files are needed to test this program.  The parameter file, tst_tds.upf, is
an example/template of the only input parameter this program uses.  Like the
tst_tds_if program, this one only reads SFDU records from a source, and
displays the TDS info messages/SFDUs in addition to the number of all and
packet SFDU records read.  It can also log the SFDUs to a disk file if
specified.  It is assumed, that a valid PVL object and TDS are available to
use with this program.
$!-----------------------------------------------------------------------------
$ create tst_tds.upf
TDS_HOST = <TDS hostname>
TDS_PORT = 6666
PVL_FILE = <PVL filename>
SFDU_LOG = <optional SFDU log filename>
$ Return
$!#############################################################################
