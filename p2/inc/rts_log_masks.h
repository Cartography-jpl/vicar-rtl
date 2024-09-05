#ifndef  MIPS_RTS_LOG_MASKS_INCLUDED
#define  MIPS_RTS_LOG_MASKS_INCLUDED 1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/*****************************************************************************
 *
 *	Any changes to these defines must also be made in the keyword tables
 *  in the module RTS_LOGGING.C.  The keyword values are tied to the user
 *  parameter values; any decision to modify these values should NOT be made
 *  lightly!!!
 *
 ****************************************************************************/

#define  MAX_LOG_PGRM_LTH	16

/***  Log Severity Definitions  ***/
#define  RTS_DEFAULT_SEVERITY   0x00000003
#define  RTS_LOG_NORMAL		0x00000000	/* Just a place-holder ...  */
						/* will not cause any logging */
#define  RTS_LOG_FATAL          0x00000001
#define  RTS_LOG_ERROR          0x00000002
#define  RTS_LOG_TRACE_ERR      0x00000004      /* Calling routine logs error */
#define  RTS_LOG_WARNING        0x00000008
#define  RTS_LOG_TRACE_WARN     0x00000010      /* Calling routine logs warn */
#define  RTS_LOG_INFO           0x00000020
#define  RTS_LOG_TEXT           0x00000040      /* Don't time tag message */
#define  RTS_LOG_SUMMARY        0x00000080      /* Don't time tag message */
#define  RTS_LOG_COMBO		0x000F0000
#define  RTS_LOG_COMBO0		0x000F0000
#define  RTS_LOG_COMBO1		0x00010000
#define  RTS_LOG_COMBO2		0x00020000
#define  RTS_LOG_COMBO3		0x00040000
#define  RTS_LOG_COMBO4		0x00080000
#define  RTS_LOG_DEBUG          0x0FF00000
#define  RTS_LOG_DEBUG0         0x0FF00000
#define  RTS_LOG_DEBUG1         0x00100000
#define  RTS_LOG_DEBUG2         0x00200000
#define  RTS_LOG_DEBUG3         0x00400000
#define  RTS_LOG_DEBUG4         0x00800000
#define  RTS_LOG_DEBUG5         0x01000000
#define  RTS_LOG_DEBUG6         0x02000000
#define  RTS_LOG_DEBUG7         0x04000000
#define  RTS_LOG_DEBUG8         0x08000000
#define  RTS_LOG_PAGE_OP        0x40000000      /* Page someone */
#define  RTS_LOG_ALWAYS		0x80000000

/***  Multimission Log Masks  ***/
#define  RTS_LOG_DEFAULT_MASK   0x80000001
#define  RTS_LOG_GENERIC	0x00000001
#define  RTS_LOG_PARAM          0x00000002
#define  RTS_LOG_SFDU           0x00000004
#define  RTS_LOG_TDS            0x00000008
#define  RTS_LOG_IPC		0x00000010
#define  RTS_LOG_DISPLAY	0x10000000
#define  RTS_LOG_PKTS           0x20000000
#define  RTS_LOG_CATALOG	0x40000000

/***  GLL Project Specific Log Masks  ***/
#define  GLL_LOG_DEFAULT_MASK   0x80000001
#define  GLL_LOG_TLMPROC        0x00000001
#define  GLL_LOG_FLTR           0x00000002
#define  GLL_LOG_PWS            0x00000004
#define  GLL_LOG_SSI            0x00000008
#define  GLL_LOG_NIMS		0x00000010
#define  GLL_LOG_ICT		0x00000020
/* These log masks are shared by more than one project.  Each	*/
/* mask has been named for its project, however, only one	*/
/* keyword has been defined for this bit-mask position in	*/
/* RTS_LOGGING.C.  Any changes in position must be made for all	*/
/* projects and in the keyword tables in RTS_LOGGING.C.		*/
#define  GLL_LOG_RICE		0x08000000	/* NOTE: MPF mask for RICE */
#define  GLL_LOG_DISPLAY	0x10000000
#define  GLL_LOG_PKTS           0x20000000
#define  GLL_LOG_CATALOG	0x40000000
#define  GLL_LOG_ALWAYS		0x80000000

/***  M94 Project Specific Log Masks  ***/
#define  M94_LOG_DEFAULT_MASK   0x8000000F
#define  M94_LOG_TLMPROC	0x00000001
#define  M94_LOG_HRSC		0x00000002
#define  M94_LOG_WAOSS		0x00000004
#define  M94_LOG_ARGUS		0x00000008
#define  M94_LOG_RS		0x00000010
#define  M94_LOG_CHK_SUM	0x00000020
/* These log masks are shared by more than one project.  Each	*/
/* mask has been named for its project, however, only one	*/
/* keyword has been defined for this bit-mask position in	*/
/* RTS_LOGGING.C.  Any changes in position must be made for all	*/
/* projects and in the keyword tables in RTS_LOGGING.C.		*/
#define  M94_LOG_PKTS           0x20000000
#define  M94_LOG_CATALOG	0x40000000
#define  M94_LOG_ALWAYS		0x80000000

/***  MPF Project Specific Log Masks  ***/
#define  MPF_LOG_DEFAULT_MASK   0x8000007F
#define  MPF_LOG_TELEMPROC	0x00000001
#define  MPF_LOG_IMP		0x00000002
#define  MPF_LOG_RVR		0x00000004
#define  MPF_LOG_APX		0x00000008
#define  MPF_LOG_BTC		0x00000010
#define  MPF_LOG_JPEG		0x00000020
#define  MPF_LOG_MET		0x00000040
/* These log masks are shared by more than one project.  Each	*/
/* mask has been named for its project, however, only one	*/
/* keyword has been defined for this bit-mask position in	*/
/* RTS_LOGGING.C.  Any changes in position must be made for all	*/
/* projects and in the keyword tables in RTS_LOGGING.C.		*/
#define  MPF_LOG_RICE		0x08000000	/* NOTE: GLL mask for RICE */
#define  MPF_LOG_DISPLAY	0x10000000
#define  MPF_LOG_PKTS           0x20000000
#define  MPF_LOG_CATALOG	0x40000000
#define  MPF_LOG_ALWAYS		0x80000000

#endif
