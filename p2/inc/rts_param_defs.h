#ifndef  MIPS_RTS_PARAM_DEF_INCLUDED
#define  MIPS_RTS_PARAM_DEF_INCLUDED  1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "rts_link_buffers.h"

/***  Parameter field limits  ***/
#define  MAX_TCL_FORMS		 128
#define  MAX_VALUE_ELEMENTS	  48
#define  MAX_LIST_ENTRIES	 512
#define  MAX_RANGE_ENTRIES	 128
#define  MAX_PATH_LTH		 256
#define  MAX_RECORD_LTH		1024
#define  MAX_NAME_LTH		  64
#define  MAX_KEYWORD_LTH	  32
#define  MAX_STRING_LTH		 256
#define  MAX_VALUE_LTH		2048

/***	Structures	***/
typedef	struct	{
		char		name[MAX_NAME_LTH];
		int		type;
		int		clock_type;
		int		max_elements;
		int		max_list;
		int		max_range;
		void		*default_value;
		void		*valid_list;
		void		*valid_range[2];
		int		num_elements;
		void		*value;
		int		num_undo_elements;
		void		*undo_value;
		int		updated;
		char		tcl_name[MAX_STRING_LTH];
		int		tcl_form;
		char		*strg_buffer;
		int		num_defaults;
		} param_defs_typ;

typedef struct	{
		unsigned	Year		:12;
		unsigned	Month		: 4;
		unsigned	Day		: 5;
		unsigned	Hour		: 5;
		unsigned	Minute		: 6;
		unsigned	Second		: 6;
		unsigned	Millisecond	:10;
		unsigned	Unused		:16;
		char		*DateStrg;	/* TCL needs a MALLOCed string */
		} date_time_typ;

typedef struct	{
		char		*SclkStrg;	/* Must be first element in */
						/* ... every SCLK structure */
						/* TCL needs a MALLOCed string */
		unsigned char	Sclk[8];
		unsigned char	Reserved[12];
		} rts_sclk_typ;

typedef struct	{
		char		*SclkStrg;
		unsigned int   	rim;	/* Real time image count */
		unsigned char	mod91;	/* mod 91 counter */
		unsigned char	mod10;	/* mod 10 counter */
		unsigned char	mod8;	/* mod 8 counter */
		unsigned char	Reserved[13];
		} gll_sclk_typ;

typedef struct	{
		char		*SclkStrg;
		unsigned int   	Coarse;
		unsigned char	Fine;
		unsigned char	Reserved[15];
		} mpf_sclk_typ;

typedef struct	{
		char		*SclkStrg;
		unsigned int	Days;
		unsigned short	MilliSeconds;
		unsigned char	Reserved[14];
		} sfoc_sclk_typ;

typedef union	{
                rts_sclk_typ	rts;		/* Template SCLK structure */
		gll_sclk_typ	gll;
		mpf_sclk_typ	mpf;
		sfoc_sclk_typ	sfoc;
		unsigned char	data[24];
		} gen_sclk_typ;

typedef int *		int_array_typ;
typedef float *		float_array_typ;
typedef double *	double_array_typ;
typedef char **		string_array_typ;

typedef int *		int_value_typ;
typedef float *		float_value_typ;
typedef double *	double_value_typ;
typedef char **		string_value_typ;
typedef char *		date_value_typ;
typedef char *		clock_value_typ;

/***	Valid Parameter Value Types	***/
#define  TYPE_NULL		0
#define  TYPE_INTEGER		1
#define  TYPE_REAL		2
#define  TYPE_DOUBLE		3
#define  TYPE_STRING		4
#define  TYPE_DATE		5
#define  TYPE_CLOCK		6
#define  TYPE_KEYWORD		7
#define  TYPE_MAX		7		/* one greater than last */
static	char	*valid_param_types[] =
		{	"NULL",		"INTEGER",	"REAL",
			"DOUBLE",	"STRING",	"DATE",
			"CLOCK",	0
		};
static int	sizeof_types[] =
		{ 0,	sizeof(int),	sizeof(float),	sizeof(double),
			sizeof(char*),	sizeof(date_time_typ),
			sizeof(gen_sclk_typ),		0
		};

#define  CLOCK_NULL		0
#define  CLOCK_GLL		1
#define  CLOCK_MPF		2
#define  CLOCK_COBT		3
#define  CLOCK_MAX		4
static	char	*valid_project_sclks[] =
		{	"NULL",	"GLL",	"MPF",	"COBT",	0
		};

/***	Character Sets	***/
#define  VALID_NAME_SET \
	 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.*-"
#define  VALID_KEYWORD_SET \
	 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"
#define  DELIMETER_REQUIRED	"!@#$%&*|\"\n\\ "
#define  VALID_TOKEN_SET	"!@#$%&*|\""
#define  WHITE_SPACE		" \t"
#define  R_V_DELIMETER		": \t"
#define  PKT_DELIMETER		",\v\r"
#define  CONFIG_DELIMETER	",:"
#define  PARAM_DELIMETER	"= \t"
#define  PKT_PAIR_SEPERATOR	"\r"
#define  PKT_STRG_SEPERATOR	"\v"
#define  PKT_NUMBER_SEPERATOR	","

/***	Configuration Resource Types	***/
#define  KEYWORD_NAME		"ParamName"
#define  VALUE_TYPE		"ParamType"
#define  VALID_VALUES		"ParamValid"
#define  DEFAULT_VALUE		"ParamDefault"
#define  CLOCK_TYPE		"ParamClock"
#define  TCL_NAME		"TclName"
#define  TCL_FORM		"TclForm"

#ifndef SEEK_SET
#define SEEK_SET	0
#endif

/***  Function prototypes for routines in module RTS_PARAM_ROUTINES  ***/
int		compare_values(param_defs_typ *);
char		*date_to_string(date_time_typ *);
link_buf_typ	*find_keyword_buffer(link_buf_typ *, char *);
void		free_parameter_buffers( link_buf_typ * );
void		free_parameter_definitions( param_defs_typ **);
int		get_next_date(char **, date_time_typ *);
int		get_next_gll_clock(char **, gll_sclk_typ *);
int		get_next_mpf_clock(char **, mpf_sclk_typ *);
int		load_config_file(char *, link_buf_typ **, int *);
int		load_param_file(char *, link_buf_typ *);
void		print_clock_values(param_defs_typ *);
void		print_date_values(param_defs_typ *);
void		print_double_values(param_defs_typ *);
void		print_int_values(param_defs_typ *);
void		print_real_values(param_defs_typ *);
void		print_rsrc(param_defs_typ *);
void		print_string_values(param_defs_typ *);
int		set_clock_value(param_defs_typ *, char *, int);
int		set_date_value(param_defs_typ *, char *, int);
int		set_double_value(param_defs_typ *, char *, int);
int		set_integer_value(param_defs_typ *, char *, int);
int		set_real_value(param_defs_typ *, char *, int);
int		set_string_value(param_defs_typ *, char *, int);
int		transfer_values(param_defs_typ *, int);
int		verify_all_values(int, link_buf_typ *, char *,
                                  param_defs_typ **);
int		verify_clock_value(param_defs_typ *, gen_sclk_typ *, int);
int		verify_date_value(param_defs_typ *, date_time_typ *);
int		verify_double_value(param_defs_typ *, double);
int		verify_integer_value(param_defs_typ *, int);
int		verify_real_value(param_defs_typ *, float);
int		verify_string_value(param_defs_typ *, char *);
int		write_param_file(char *, link_buf_typ *);

#endif
