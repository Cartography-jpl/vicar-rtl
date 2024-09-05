#ifndef  RTS_CATDEFS_INCLUDED
#define  RTS_CATDEFS_INCLUDED		1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/************************************************************************/
/******  Multimission Include File  *************************************/
/******        rts_cat_defs.h       *************************************/
/*					 				*/
/* History:				 				*/
/*					 				*/
/* svl (3/17/1997):			 				*/
/*		add #ifdef __cplusplus		 			*/
/*          	extern "C" {		 				*/
/*	    	#endif.....			 			*/
/* 					 				*/
/*      	before & after function proto.   			*/
/*		to make it C++ compatible. This	 			*/
/*		should have not effect on non-C++			*/
/*		codes.				 			*/
/* svl (6/12/1997):							*/
/*		remove all nested comments to avoid compiler warnings.	*/
/*		This should have no affect on the current codes that	*/
/*		uses this include file.					*/
/*					 				*/
/************************************************************************/

#define  RTS_CAT_RETRY_NUM	1	/* Number of command retries 	*/
#define  RTS_CAT_RETRY_DELAY	1	/* Delay (sec) between retries	*/
#define  RTS_CAT_RETRY_ANNOUN	0	/* Do not log retry attempts	*/

#define  RTS_CAT_RETRY_MIN_DLY	0	/* Maximum Delay time for retry	*/
#define  RTS_CAT_RETRY_MAX_DLY	60	/* Maximum Delay time for retry	*/
#define  RTS_CAT_RETRY_MIN_NUM	0	/* Maximum number of retries	*/
#define  RTS_CAT_RETRY_MAX_NUM	60	/* Maximum number of retries	*/

/***  Supported MDMS Data Types:
 ***	MDMS_TINYBIND
 ***	MDMS_SMALLBIND
 ***	MDMS_INTBIND
 ***	MDMS_REALBIND
 ***	MDMS_FLT8BIND
 ***	MDMS_NTBSTRINGBIND
 **/

/***	This structure defines the variable(s) used to query and store
 ***	information from the catalog.
 **/
typedef	struct	{
	void	*Address;		/* Address of variable to process   */
	int	*Valid;			/* Address of flag indicating an    */
					/* catalog field contains a value,  */
					/* can be set to ZERO if not needed */
	int	DataType;		/* MDMS data type of field/variable */
	int	MaxLength;		/* Maximum length of string field   */
	} CatParamElement_typ;
		
/***	This structure defines all the neccessary information for the
 ***	RTS_CAT_UTIL module routines to process a catalog query for
 ***	one stored procedure.
 **/
typedef	struct	{
	void	*Record;		/* Structure to place values */
	int	RecordLth;		/* Structure length (bytes)  */
	char	*StoredProcedure;	/* Name of stored procedure  */
	CatParamElement_typ	*Query;	/* Storage area for Sysbase  */
					/* query parameters.  To     */
					/* identify the last param,  */
					/* the last element of the   */
                                        /* array must be all zeros.  */
	CatParamElement_typ	*Element;	/* Storage area for  */
						/* processing catalog fields */
						/* First element of array is */
						/* not used by RTS_CAT_UTIL  */
						/* routines.  To identify    */
						/* last catalog field, the   */
						/* last element of the array */
						/* must be all zeros.        */ 
	} CatProcedureTable_typ;

/***	This structure defines the necessary catalog server login information
 **/
typedef	struct	{
	char	Program[64];
	char	Server[32];
	char	DataBase[32];
	char	UserName[32];
	char	Password[32];
	} CatLogin_typ;

/***	This structure defines the parameters associated with re-executing an
 ***	MDMS command that had failed
 **/
typedef struct	{
	int	Delay;			/* Delay in seconds until next retry  */
	int	Retries;		/* Number of retries before giving-up */
	int	Announce;		/* Announce each retry                */
	} CatRetryDefs_typ;


#ifdef __cplusplus
extern "C" {
#endif

/***  Function Prototypes  ***/
int     rts_cat_add( int, void * );
int     rts_cat_get( int, int, int, void *, int * );
void	rts_cat_get_retry ( CatRetryDefs_typ * );
int     rts_cat_login( CatLogin_typ * );
int     rts_cat_logout();
void	rts_cat_load_table( void *, int );
void	rts_cat_set_retry ( CatRetryDefs_typ * );
#ifdef __cplusplus
}
#endif

#endif
