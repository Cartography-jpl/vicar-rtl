#ifndef RTS_CNTRL_INCLUDED
#define RTS_CNTRL_INCLUDED	1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "xvmaininc.h"

#if VMS_OS
/*
 * Times are screwed		.... MULTINET Sucks
 */
#include <types.h>
#else
#include <time.h>
#endif

#include "rts_typedefs.h"

/***  TelemProc execution defines  ***/
#define  TP_NULL		0
#define  TP_RUN			1		/* Default */
#define  TP_EOD_HOLD		2
#define  TP_PAUSE		3
#define  TP_ABORT		4
#define  TP_STOP		5
#define  TP_AUTONAMOUS		6

#define  TP_INST_MAX		4

#define  SFDU_SRC_INV		0
#define  SFDU_SRC_FILE		1		/* Default */
#define  SFDU_SRC_TDS		2
#define  SFDU_SRC_PORT		3

#define  TDS_QUERY_INV		0
#define  TDS_QUERY_SCLK		1		/* Default */
#define  TDS_QUERY_SCET		2
#define  TDS_QUERY_ERT		3
#define  TDS_QUERY_RCT		4
static char	*TdsQueryTypes[] = {
			"invld", "SCLK", "SCET", "ERT", "RCT", 0};

#define  TDS_SORT_INV		0
#define  TDS_SORT_ASIS		1		/* Default */
#define  TDS_SORT_SCLK		2
#define  TDS_SORT_SCET		3
#define  TDS_SORT_ERT		4
#define  TDS_SORT_RCT		5
static char	*TdsSortOrder[] = {
			"invld", "ASIS", "SCLK", "SCET", "ERT", "RCT", 0};

#define  TDS_SOURCE_INV		0
#define  TDS_SOURCE_NERT	1
#define  TDS_SOURCE_CDB		2
#define  TDS_SOURCE_RT		3
static char	*TdsDataSource[] = {
			"invld", "NERT", "CDB", "RT", 0};

typedef struct	{
		char	ParamName[32];
		void	*Target;
		int	(*Extract)(void *);
		} param_dispatch_typ;

#ifndef RTS_UNIQUE
#define  RTS_UNIQUE	void *
#endif

typedef	struct	{
		Field	PurgeImage	: 1;	/* ... data w/o creating a file	*/
		Field	UpdateCatalog	: 1;	/* ... about new files		*/
		Field	SpawnNext	: 1;	/* ... step for auto processing	*/
		Field	CatalogBias	: 1;	/* Use catalog info over telem  */
		Field	SubmitToFei	: 1;	/* Submit image to FEI server	*/
		Field	Reserved	:27;
		int	PktFilter[2];
		char	PktType[128];
		char	UdrPathName[64];
		RTS_UNIQUE	Unique;	/* Project/Inst specific data */
		} instr_cntrl_typ;
#undef  RTS_UNIQUE

typedef	struct	{
		char	HostName[64];
		int	TcpIpPort;
		char	Mission[32];
		char	SpaceCraft[32];
		time_t	SCET[2];
		time_t	ERT[2];
		time_t	RCT[2];
		char	SCLK[2][32];
		int	QueryType;
		int	SortOrder;
		int	TimeOutLimit;
		char	DataSource[128];
		char	DataType[128];
		char	WseFilter[128];
		char	Requester[32];
		char	Description[128];
		} tds_query_typ;

typedef	struct {
		int	Execution;
		int	SFDU_Source;
		int	SFDU_Port;
		char	SFDU_Filename[256];
		char	CatalogServer[128];
		char	CatalogUserName[64];
		char	CatalogPassword[64];
		char	CatalogDataBase[64];
		tds_query_typ	TDS_Query;
		instr_cntrl_typ	Instrmnt[TP_INST_MAX];
		} telemproc_typ;

/***  Instrument Identifiers  ***/
#define  GLL_INSTR_SSI		0
#define  GLL_INSTR_PWS		1
#define  GLL_INSTR_PWL		2
#define  GLL_INSTR_NIMS		3

#define  MPF_INSTR_IMP		0
#define  MPF_INSTR_RVR		1
#define  MPF_INSTR_APX		2
#define  MPF_INSTR_MET		3

#endif
