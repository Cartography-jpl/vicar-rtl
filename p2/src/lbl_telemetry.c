/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_telemetry.h"

/******************************************************************************
 *				LBL_TELEMETRY
 *
 *	This module contains routines to help create, read/write and print
 *  a Telemetry property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_telemetry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblTelemetry.  This routine requires exactly 4 parameters.
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
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 2003-04-24   H. Lee          Changed SPICE_FILE_ID as LBL_OPTIONAL
 * 2003-01-13   P. Zamani       Added APPLICATION_PROCESS_SUBTYPE_ID
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblTelemetry_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"APPLICATION_PACKET_ID",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketId.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationPacketId.Value)},

	{"APPLICATION_PACKET_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketName.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationPacketName.Value)},

	{"APPLICATION_PROCESS_ID",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessId.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationProcessId.Value)},

	{"APPLICATION_PROCESS_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessName.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationProcessName.Value)},

	{"APPLICATION_PROCESS_SUBTYPE_ID",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessSubtypeId.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessSubtypeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationProcessSubtypeId.Value)},

	{"EARTH_RECEIVED_START_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStartTime.Value),
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStartTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarthReceivedStartTime.Value)},

	{"EARTH_RECEIVED_STOP_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStopTime.Value),
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStopTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarthReceivedStopTime.Value)},

	{"EXPECTED_PACKETS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ExpectedPackets.Value),
		LBL_OFFSET(LblTelemetry_typ, ExpectedPackets.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExpectedPackets.Value)},

	{"PACKET_CREATION_SCLK",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, PacketCreationSclk.Value),
		LBL_OFFSET(LblTelemetry_typ, PacketCreationSclk.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PacketCreationSclk.Value)},

	{"PACKET_MAP_MASK",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, PacketMapMask.Value),
		LBL_OFFSET(LblTelemetry_typ, PacketMapMask.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PacketMapMask.Value)},

	{"PACKET_SEQUENCE_NUMBER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, PacketSequenceNumber.Value),
		LBL_OFFSET(LblTelemetry_typ, PacketSequenceNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PacketSequenceNumber.Value)},

	{"RECEIVED_PACKETS",			"INT",		LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ReceivedPackets.Value),
		LBL_OFFSET(LblTelemetry_typ, ReceivedPackets.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReceivedPackets.Value)},

	{"SOFTWARE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SoftwareName.Value),
		LBL_OFFSET(LblTelemetry_typ, SoftwareName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SoftwareName.Value)},

	{"SOFTWARE_VERSION_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SoftwareVersionId.Value),
		LBL_OFFSET(LblTelemetry_typ, SoftwareVersionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SoftwareVersionId.Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[0].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[0].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[1].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[1].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[2].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[2].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[3].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[3].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[4].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[4].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[0].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[0].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[1].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[1].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[2].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[2].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[3].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[3].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[4].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[4].Value)},

	{"TELEMETRY_PROVIDER_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderId.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetryProviderId.Value)},

	{"TELEMETRY_PROVIDER_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderType.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetryProviderType.Value)},

	{"TELEMETRY_SOURCE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceName.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetrySourceName.Value)},

	{"TELEMETRY_SOURCE_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceType.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetrySourceType.Value)},

	{"TLM_CMD_DISCREPANCY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TlmCmdDiscrepancyFlag.Value),
		LBL_OFFSET(LblTelemetry_typ, TlmCmdDiscrepancyFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TlmCmdDiscrepancyFlag.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "TELEMETRY",
	LBL_NULL };

/******************************************************************************
 *				LBL_TELEMETRY
 *
 *****************************************************************************/
int     LblTelemetry(
  int   Unit,
  int   Obtain,
  LblTelemetry_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblTelemetry_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_TELEMETRY
 *
 *****************************************************************************/
void	LblPrintTelemetry(
  LblTelemetry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_TELEMETRY
 *
 *****************************************************************************/
void	LblTestTelemetry(
  LblTelemetry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
