#ifndef MIPS_RTS_SPAWN_INCLUDED
#define MIPS_RTS_SPAWN_INCLUDED	1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "rts_typedefs.h"

typedef	struct	{
		Field		CreateReadPipe	: 1;
		Field		CreateWritePipe	: 1;
		Field		MapReadStdin	: 1;	/* Child Only */
		Field		MapWriteStdout	: 1;	/* Child Only */
		Field		MapReadArgv1	: 1;	/* Child Only */
		Field		MapWriteArgv2	: 1;	/* Child Only */
		Field		MapWriteStderr	: 1;	/* Child Only */
		Field		Reserved	:25;
		int		TaskId;
		int		ReadPipe;
		int		WritePipe;
		char		**Environ;
		char		**Argv;
		char		Program[256];
		} rts_spawn_typ;

/***  Function Prototypes  ***/
int	rts_add_list_item( char **, char *);
int	rts_copy_list( char **, char ** );
int	rts_count_list( char ** );
void	rts_delete_list_item( char **, char *);
void	rts_display_list( char ** );
void	rts_free_list( char ** );
int	rts_insert_list_item( char **, int, char* );
int	rts_make_list( void *, int );
int	rts_parse_argv_list( char **, char * );
int	rts_replace_env_item( char **, char * );
int	rts_spawn_task( rts_spawn_typ * );

#endif
