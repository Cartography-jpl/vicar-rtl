#ifndef MIPS_RTS_PKT_DEFS_INCLUDED
#define MIPS_RTS_PKT_DEFS_INCLUDED	1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "rts_link_buffers.h"
#include "rts_param_defs.h"

typedef struct	{
		char	ParamName[32];
		void	*Target;
		int	MaxElements;
		int	(*Extract)(void *, int, char *);
		} param_dispatch_type;

int	create_cmnd_pkt(char *, int, link_buf_typ *);
int     create_value_pair( char *, param_defs_typ *, int);
int	extract_date_value(date_time_typ *, int, char *);
int	extract_double_value(double *, int, char *);
int	extract_float_value(float *, int, char *);
int	extract_integer_value(int *, int, char *);
int	extract_string_value(char **, int, char *);
char    *gll_clock_strg( gen_sclk_typ *);
char	*next_param_pair ( char *);
int	num_param_pairs( char *);
int	param_keyword( char *);
char	*param_pair(char *, int, int *);
char	*param_value(char *, int *);

#endif
