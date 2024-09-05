#ifndef RTS_ERRORS_INCLUDED
#define  RTS_ERRORS_INCLUDED	1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/***  General error types  ***
 ***  0x00 - Normal
 ***  0x01 - Warning
 ***  0x02 - Informational
 ***  0x03 - Error
 ***  0x04 - Debugging informational
 ***  0x05 - Undefined
 ***  0x06 - Recovered
 ***  0x07 - Fatal error
 ***  0x08 - Notify error flag ... used with any error code
 **/

#define  ERR_CODE_MASK	0x0F
#define  ERR_NORMAL	0x00
#define  ERR_WARNING	0x01
#define  ERR_INFO	0x02
#define  ERR_ERROR	0x03
#define  ERR_DEBUG	0x04
#define  ERR_RECOVER	0x06
#define  ERR_FATAL	0x07
#define  ERR_NOTIFY	0x08

#define  RTS_RTN_BAD(x)		((x) & 0x01)
#define  RTS_RTN_GOOD(x)	(!((x) & 0x01))
#define  ERR_NUMBER(x)		(((unsigned int)(x)) >> 4)
#define  ERR_CODE(x)		((x) & ERR_CODE_MASK)

/***  Generic Codes  ***/
#define  RTS_NORMAL		(0x0000 | ERR_NORMAL)	/* Generic Success */ 
#define  RTS_INFO		(0x0000 | ERR_INFO)	/* Generic Info    */
#define  RTS_WARNING		(0x0000 | ERR_WARNING)	/* Generic Warning */
#define  RTS_ERROR		(0x0000 | ERR_ERROR)	/* Generic Error   */
#define  RTS_DEBUG		(0x0000 | ERR_DEBUG)	/* Generic Debug   */
#define  RTS_RECOVER		(0x0000 | ERR_RECOVER)	/* Generic Recover */
#define  RTS_FATAL		(0x0000 | ERR_FATAL)	/* Generic Fatal   */
/***  General Codes  ***/
#define  TIME_OUT		(0x0010 | ERR_WARNING)	/* Could not complete operation in specified time limit */
#define  EOD			(0x0020 | ERR_WARNING)	/* No more data available */
#define  ALLOCATE_ERROR		(0x0030 | ERR_ERROR)	/* Could not allocat memory - malloc */
#define  OPEN_ERROR		(0x0040 | ERR_ERROR)	/* Could not open a disk file */
#define  VICAR_IO_ERR		(0x0050 | ERR_ERROR)	/* VICAR Run-time library I/O error */
/***  Parameter Processing Codes  ***/
#define  MISSING_VALUE		(0x0060 | ERR_WARNING)	/* Missing a parameter value */
#define  PARAM_NO_PERIOD	(0x0070 | ERR_ERROR)	/* Parameter resource name invalid ... */
							/* ... must be more than one word, seperated by a period */
#define  PARAM_BAD_PATH		(0x0080 | ERR_ERROR)	/* Syntax error in parameter resource name */
#define  PARAM_EOF		(0x0090 | ERR_NORMAL)	/* Premature end of parameter definition data */
#define  PARAM_UNDEF		(0x00A0 | ERR_ERROR)	/* Could not identify parameter */
/***  SFDU Codes  ***/
#define  INSUFF_DATA		(0x0100 | ERR_WARNING)	/* Insufficient data available to complete 'SFDU' */
#define  DATA_SFDU		(0x0110 | ERR_NORMAL)	/* SFDU contains data to process */
#define  INFO_SFDU		(0x0120 | ERR_NORMAL)	/* SFDU contains status information only ... no data */
#define  DELIMITER_SFDU		(0x0130 | ERR_NORMAL)	/* SFDU is 'Mask' delimited ... not a defined length */
#define  INVALID_SFDU		(0x0140 | ERR_ERROR)	/* SFDU not defined according to spec: CCSD 620.0-B-2 (May-92) */
#define  ERROR_SFDU		(0x0150 | ERR_ERROR)	/* SFDU conatins error status information */
#define  UNSUPPORTED_SFDU	(0x0160 | ERR_ERROR)	/* SFDU not supported by MIPS RTS */
#define  TOO_MANY_CHDOS		(0x0170 | ERR_ERROR)	/* Number of CHDOs exceeds arbitrary RTS limit */
#define  UNSUPPORTED_CHDO	(0x0180 | ERR_RECOVER)	/* CHDO not supported by MIPS RTS */
/***  Packet Codes  ***/
#define  INVALID_PKT		(0x0200 | ERR_ERROR)	/* Packet invalid */
#define  UNDEF_PKT_HDR		(0x0210 | ERR_ERROR)	/* undefined packet header values */
/***  Catalog Codes  ***/
#define  EXCEEDED_TABLE		(0x0300 | ERR_ERROR)	/* Exceeded table entries */
#endif
