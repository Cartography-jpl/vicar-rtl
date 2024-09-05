#ifndef  MIPS_RTS_LOGGING_INCLUDED
#define  MIPS_RTS_LOGGING_INCLUDED 1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "rts_log_masks.h"

#ifdef        __cplusplus
extern "C" {
#endif

extern	unsigned int	rts_log_severity;
extern	unsigned int	rts_log_mask;
extern	unsigned int	gll_log_mask;
extern	unsigned int	m94_log_mask;
extern	unsigned int	mpf_log_mask;

int	basic_logger( unsigned int, char *, char *, char *);
	/* Lower level logger used by all 'project' loggers */
	/* Param 1 - Severity indicator  */
	/* Param 2 - Location indicator  */
	/* Param 3 - Subsystem indicator */
	/* Param 4 - Log Message         */

void	rts_flush_log( void );
	/* Flushes the system buffers for logging to file or stdio.	*/

void	rts_format_date_time( char * );
	/* Returns the current data and Time in PDS standard form	*/
	/*	yyyy-dddThh:mm:ss  or yyyy-mm-ddThh:mm:ss		*/

int	rts_log_to_file ( char * );
	/* Send log messages to the file specified by the parameter.	*/
	/* Messages will be appended to exisiting files.		*/

void    rts_log_to_logger ( unsigned int );
	/* Send messages to PVM-based realtime logger (capability is	*/
	/* now obsolete and is now routed to stdio).			*/

void	rts_log_to_routine( int (*)(int,int,char*) );
	/* Send message to supplied routine.  This has been added to	*/
	/* support the CORBA-based realtime effort.			*/

void    rts_log_to_stdio ( unsigned int );
	/* Send message to the C stdio.					*/

void    rts_log_to_vicar ( unsigned int );
	/* Send message to VICAR subsystem to allow for session logging.*/

void	rts_set_julian_format ( unsigned int );
	/* Use Julian time format when logging messages, default is	*/
	/* Gregorian							*/

void	rts_set_UTC_format ( unsigned int );
	/* Time stamps log messages with UTC time instead of local time.*/

void	rts_set_logmasks( char *);
	/* Sets the log masks (that control what messages will be	*/
	/* logged) for the generic and project masks.  The parameter is	*/
	/* a string of keywords specific for each mask.  Typically, the	*/
	/* keywords are seperated by white space or commas.  Actually,	*/
	/* anything except a NULL will work (even no seperation).	*/

void	rts_set_prgmlogname( char *);
	/* Defines the program name for logging purposes (so it doesn't	*/
	/* have to be passed for each logging call.			*/

#define rts_logger(msk,sev,mod,msg) \
        (((((msk) & rts_log_mask) == (msk)) && (rts_log_severity & (sev))) &&\
         basic_logger(sev,"RTS",mod,msg) )

#define gll_logger(msk,sev,mod,msg) \
        (((((msk) & gll_log_mask) == (msk)) && (rts_log_severity & (sev))) &&\
         basic_logger(sev,"GLL",mod,msg) )

#define mpf_logger(msk,sev,mod,msg) \
        (((((msk) & mpf_log_mask) == (msk)) && (rts_log_severity & (sev))) &&\
         basic_logger(sev,"MPF",mod,msg) )

#define m94_logger(msk,sev,mod,msg) \
        (((((msk) & m94_log_mask) == (msk)) && (rts_log_severity & (sev))) &&\
         basic_logger(sev,"M94",mod,msg) )

#define anon_logger(msk,sev,mod,msg) \
        (((((msk) & rts_log_mask) == (msk)) && (rts_log_severity & (sev))) &&\
         basic_logger(sev,"",mod,msg) )

#ifdef        __cplusplus
}                                     /* end of extern "C"    */
#endif

#endif
