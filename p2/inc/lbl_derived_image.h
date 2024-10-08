#ifndef MIPS_LBL_DERIVED_IMAGE_INCLUDED
#define MIPS_LBL_DERIVED_IMAGE_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

/******************************************************************************
 *				LBL_DERTIVED_IMAGE
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Image property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The primary routine used by a typical application program is
 *  LblDerivedImage.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *============================================================================
 *
 * History of modifications:
 *
 * Date		who		Description
 * -----------  --------------- ----------------------------------------------
 * 12-Nov-2003  Hyun Lee        Added RadianceOffsetUnit
 * 10-Nov-2003  Hyun Lee        Added RadianceScaleFactorUnit
 * 17-Jun-2003  Hyun Lee        Added ConfigurationBandId & InstrumentBandId
 * 23-May-2003  Hyun Lee        Changed ReferenceCoordSystemSolutionId to
 *                              ReferenceCoordSystemSolnId
 * 11-Feb-2003	Payam Zamani	Moved GeometryProjectionType to identification
 *				 group.  See lbl_identification.h
 * ?		Allan Runkle	Original development and release
 *****************************************************************************/

#define  LBL_BAND_ID_ELEMENTS           16

typedef struct
	{
        LblApiIdItem_typ                ConfigurationBandId[LBL_BAND_ID_ELEMENTS];
	LblApiTypeItem_typ		DerivedImageType;
        LblApiIdItem_typ                InstrumentBandId[LBL_BAND_ID_ELEMENTS];
	LblApiRealItem_typ		RadianceOffset;
        LblApiTypeItem_typ              RadianceOffsetUnit;
	LblApiRealItem_typ		RadianceScaleFactor;
        LblApiTypeItem_typ              RadianceScaleFactorUnit;
	LblApiTypeItem_typ		RadiometricCorrectionType;
	LblApiRealVectorItem_typ	RangeOriginVector;
	LblApiIntItem_typ		ReferenceCoordSystemIndex[LBL_COORD_SYS_INDEX]; 
	LblApiNameItem_typ		ReferenceCoordSystemName;
	LblApiIdItem_typ		ReferenceCoordSystemSolnId;
	} LblDerivedImage_typ;

/***  Function prototypes  ***/
int	LblDerivedImage( int, int, LblDerivedImage_typ *, int );
int	LblDerivedImageParms( int, int, LblDerivedImage_typ *, int );
	/***  For development & internal use  ***/
int	LblDerivedImageApi( int, int, LblDerivedImage_typ *, int );
void	LblSetDerivedImage( const char * );
void	LblTestDerivedImage( LblDerivedImage_typ *);
void	LblPrintDerivedImage( LblDerivedImage_typ *);

#ifdef __cplusplus
}
#endif

#endif
