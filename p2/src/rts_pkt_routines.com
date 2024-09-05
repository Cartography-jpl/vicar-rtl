$!****************************************************************************
$!
$! Build proc for MIPL module rts_pkt_routines
$! VPACK Version 1.8, Monday, March 25, 1996, 12:51:37
$!
$! Execute by entering:		$ @rts_pkt_routines
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module rts_pkt_routines ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to rts_pkt_routines.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("rts_pkt_routines.imake") .nes. ""
$   then
$      vimake rts_pkt_routines
$      purge rts_pkt_routines.bld
$   else
$      if F$SEARCH("rts_pkt_routines.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rts_pkt_routines
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rts_pkt_routines.bld "STD"
$   else
$      @rts_pkt_routines.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rts_pkt_routines.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rts_pkt_routines.com -
	-s rts_pkt_routines.c -
	-i rts_pkt_routines.imake -
	-t tst_rts_pkt_routines.c tst_rts_pkt_routines.imake test_param.cnf -
	   test_param.upf readme_rts_pkt.tst
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rts_pkt_routines.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>

#include "rts_const_defs.h"
#include "rts_typedefs.h"
#include "rts_errors.h"
#include "rts_link_buffers.h"
#include "rts_logging.h"
#include "rts_param_defs.h"

#define  MODULE_NAME		"PKTS"
#define  FLOAT_SIG_DIGITS	6

int	create_cmnd_pkt( char *, int, link_buf_typ *);
int	create_value_pair( char *, param_defs_typ *, int);
char	*gll_clock_strg( gen_sclk_typ *);
char	*mpf_clock_strg( gen_sclk_typ *);
int	num_param_pairs( char *);
int	param_keyword( char *);
char	*param_pair( char *, int, int *);
char	*param_value( char *, int *);

static char	log_msg_buf[256];

/******************************************************************************
/*				CREATE_CMND_PKT
/*
/*	Generates a command packet buffer using a linked parameter list.  This
/*  routine only generates parameters that have been flagged as "updated".
/*  once the parameter has been included in the packet buffer, this flag
/*  is reset.  If the buffer is too small for all of the parameters, only
/*  those that fit will be included.  The "updated" flag for those items that
/*  did not fit will not be changed.
/*  RETURNS the length of the command buffer.
/*****************************************************************************/

int	create_cmnd_pkt(
  char	*pkt_buf,
  int	buf_size,
  link_buf_typ	*params)
{ int	pair_lth;
  char	value_pair[MAX_VALUE_LTH];
  link_buf_typ	*l_params = params;
  param_defs_typ	*keyword;

  memset(pkt_buf, 0, buf_size);
  if (l_params == 0)
  { rts_logger(RTS_LOG_PKTS,RTS_LOG_INFO,MODULE_NAME,
               "Null parameter buffer");
    return FALSE;
  }

  do
  { keyword = (param_defs_typ *)l_params->data;
    if (keyword->updated)
    { pair_lth = create_value_pair(value_pair, keyword, TRUE);
      if ((pair_lth+(int)strlen(pkt_buf)+1) > buf_size)
      { rts_logger(RTS_LOG_PKTS,RTS_LOG_WARNING,MODULE_NAME,
                   "Packet buffer too small for command packet");
        pkt_buf[strlen(pkt_buf)-1] = 0;
        return strlen(pkt_buf);
      }
      if (pair_lth)
      { strcat(pkt_buf,value_pair);
        strcat(pkt_buf,PKT_PAIR_SEPERATOR);
      }
      keyword->updated = FALSE;
      transfer_values(keyword, TRUE);
    }

    l_params = (link_buf_typ *)l_params->next;
  } while (l_params != params);

  pkt_buf[strlen(pkt_buf)-1] = 0;
  return strlen(pkt_buf);
}

/******************************************************************************
/*				CREATE_VALUE_PAIR
/*
/*	This routine generates keyword/value pair strings for inclusion in
/*  the command packet buffer.  It determines the type of value the parameter
/*  is and formats the string accordingly.  This routine does not included
/*  the keyword/value pair delimiting character used to identify the end of
/*  the string.
/*  RETURNS the length of the keyword/value pair string.
/*****************************************************************************/

int	create_value_pair(
  char	*pair,
  param_defs_typ	*keyword,
  int	pkt_pair)
{ int	value_idx,
	idx,
	width,
	precision;
  char	number_value[32],
	used_chars[32],
	delimeter[2] = { 0, 0},
	*ptr;
  date_time_typ	*pdt;

  if (keyword->type <= TYPE_NULL || keyword->type >= TYPE_MAX)
  { sprintf(log_msg_buf,"Invalid parameter type: %d",keyword->type);
    rts_logger(RTS_LOG_PKTS,RTS_LOG_ERROR,MODULE_NAME,log_msg_buf);
    return 0;
  }

  sprintf(pair,"%s=",keyword->name);

  switch (keyword->type)
  { case TYPE_INTEGER:
             for (value_idx=0; value_idx<keyword->num_elements; value_idx++)
             { sprintf(number_value,"%d",((int*)keyword->value)[value_idx]);
               strcat(pair,number_value);
               strcat(pair,PKT_NUMBER_SEPERATOR);
             }
         break;

    case TYPE_REAL:
             for (value_idx=0; value_idx<keyword->num_elements; value_idx++)
             { width = (int)log10(fabs((double)
                                       ((float*)keyword->value)[value_idx]));
               precision = (width > FLOAT_SIG_DIGITS) ? 0 :
                           (FLOAT_SIG_DIGITS - width);
               /*  Include room for precision part, sign and "0."  */
               width += precision +
                        ((((float*)keyword->value)[value_idx] < 0.0) ? 3 : 2);
               sprintf(number_value,"%*.*f", width, precision,
                       ((float*)keyword->value)[value_idx]);
               strcat(pair,number_value);
               strcat(pair,PKT_NUMBER_SEPERATOR);
             }
         break;

    case TYPE_DOUBLE:
             for (value_idx=0; value_idx<keyword->num_elements; value_idx++)
             { sprintf(number_value,"%E",((double*)keyword->value)[value_idx]);
               strcat(pair,number_value);
               strcat(pair,PKT_NUMBER_SEPERATOR);
             }
         break;

    case TYPE_STRING:
             for (value_idx=0; value_idx<keyword->num_elements; value_idx++)
                 if (pkt_pair)
             { strcat(pair,((char **)keyword->value)[value_idx]);
               strcat(pair,PKT_STRG_SEPERATOR);
             } else
             { memset(used_chars,0,sizeof(used_chars));
               do
               { ptr = strpbrk(((char**)keyword->value)[value_idx],
                               DELIMETER_REQUIRED);
                 if (ptr)
                 { if (*ptr == '\n') *ptr = '$';
                   if (!strchr(used_chars,*ptr))
                      strncat(used_chars,ptr,1);
                 }
               } while (ptr);

               if (strlen(used_chars))		/* Need to use delimeters */
               { ptr = VALID_TOKEN_SET;
                 idx = strcspn(VALID_TOKEN_SET,used_chars);
                 if (idx >= (int)strlen(ptr))
                 { /* bummer .... use default ? */
                 } else delimeter[0] = ptr[idx];
               }

               sprintf(number_value,"%s%s%s",delimeter,
                       ((char **)keyword->value)[value_idx],delimeter);
               strcat(pair,number_value);
               strcat(pair,PKT_NUMBER_SEPERATOR);               
             }
         break;

    case TYPE_DATE:
             pdt = (date_time_typ *)keyword->value;
             for (value_idx=0; value_idx<keyword->num_elements; value_idx++)
             { sprintf(number_value,"%04d-%02d-%02dT%02d:%02d:%02d.%03d",
                       pdt[value_idx].Year, pdt[value_idx].Month,
                       pdt[value_idx].Day,
                       pdt[value_idx].Hour, pdt[value_idx].Minute,
                       pdt[value_idx].Second, pdt[value_idx].Millisecond);
               strcat(pair,number_value);
               strcat(pair,PKT_NUMBER_SEPERATOR);
             }
         break;

    case TYPE_CLOCK:
             for (value_idx=0; value_idx<keyword->num_elements; value_idx++)
             { switch (keyword->clock_type)
               { case CLOCK_GLL:
                      strcat(pair,
                             gll_clock_strg(&((gen_sclk_typ *)keyword->value)
                                              [value_idx]));
                      strcat(pair,PKT_NUMBER_SEPERATOR);
                      break;
                 case CLOCK_MPF:
                      strcat(pair,
                             mpf_clock_strg(&((gen_sclk_typ *)keyword->value)
                                              [value_idx]));
                      strcat(pair,PKT_NUMBER_SEPERATOR);
                      break;
                 default: sprintf(log_msg_buf,
                                  "Unsupported Spacecraft Clock type: %d",
                                  keyword->clock_type);
                      rts_logger(RTS_LOG_PKTS,RTS_LOG_ERROR,MODULE_NAME,
                                 log_msg_buf);
                      break;
               }
             }
         break;

    default:
             pair[0] = 0;
         break;
  }

  pair[strlen(pair) - 1] = 0;
  return (strlen(pair));
}

/*****************************************************************************
/*				EXTRACT_DATE_VALUE
/*
/*	Extracts the date values from a character buffer.  The values are
/*  expected to be generated from the command packet generation routine and
/*  contain those value delimiters.
/*****************************************************************************/
int	extract_date_value(
  date_time_typ	**Target,
  int	MaxElements,
  char	*Values)
{ int	idx,
	status;
  char	*V_ptr = Values;
  date_time_typ	Temp;

  for (idx=0; idx<MaxElements && *V_ptr; idx++)
  { status = get_next_date(&V_ptr,&Temp);
    if (status) break;
    memcpy(Target[idx],&Temp,sizeof(date_time_typ));
    if (*V_ptr) V_ptr++;
  }

  return ( FALSE );
}

/*****************************************************************************
/*				EXTRACT_DOUBLE_VALUE
/*
/*	Extracts the double values from a character buffer.  The values are
/*  expected to be generated from the command packet generation routine and
/*  contain those value delimiters.
/*****************************************************************************/
int	extract_real_value(
  double	*Target,
  int	MaxElements,
  char	*Values)
{ int	idx;
  char	*V_ptr;

  V_ptr = strpbrk(Values,"0123456789+-.Ee");		/* Find number      */
  for (idx=0; idx<MaxElements && V_ptr; idx++)
  { Target[idx] = atof(V_ptr);				/* Assign number    */
    V_ptr = strstr(V_ptr,"0123456789+-.Ee");		/* Skip number      */
    V_ptr = strpbrk(V_ptr,"0123456789+-.Ee");		/* Find next number */
  }

  return ( FALSE );
}

/*****************************************************************************
/*				EXTRACT_FLOAT_VALUE
/*
/*	Extracts the float values from a character buffer.  The values are
/*  expected to be generated from the command packet generation routine and
/*  contain those value delimiters.
/*****************************************************************************/
int	extract_float_value(
  float	*Target,
  int	MaxElements,
  char	*Values)
{ int	idx;
  char	*V_ptr;

  V_ptr = strpbrk(Values,"0123456789+-.");		/* Find number      */
  for (idx=0; idx<MaxElements && V_ptr; idx++)
  { Target[idx] = (float)atof(V_ptr);			/* Assign number    */
    V_ptr = strstr(V_ptr,"0123456789+-.");		/* Skip number      */
    V_ptr = strpbrk(V_ptr,"0123456789+-.");		/* Find next number */
  }

  return ( FALSE );
}

/*****************************************************************************
/*				EXTRACT_INTEGER_VALUE
/*
/*	Extracts the integer values from a character buffer.  The values are
/*  expected to be generated from the command packet generation routine and
/*  contain those value delimiters.
/*****************************************************************************/
int	extract_integer_value(
  int	*Target,
  int	MaxElements,
  char	*Values)
{ int	idx;
  char	*V_ptr;

  V_ptr = strpbrk(Values,"0123456789+-");		/* Find number      */
  for (idx=0; idx<MaxElements && V_ptr; idx++)
  { Target[idx] = atoi(V_ptr);				/* Assign number    */
    V_ptr = strstr(V_ptr,"0123456789+-");		/* Skip number      */
    V_ptr = strpbrk(V_ptr,"0123456789+-");		/* Find next number */
  }

  return ( FALSE );
}

/*****************************************************************************
/*				EXTRACT_STRING_VALUE
/*
/*	Extracts the string values from a character buffer.  The values are
/*  expected to be generated from the command packet generation routine and
/*  contain those value delimiters.
/*****************************************************************************/
int	extract_string_value(
  char	**Target,
  int	MaxElements,
  char	*Values)
{ int	idx,
	lth;
  char  *V_ptr = Values;

  for (idx=0; idx<MaxElements && *V_ptr; idx++)  
  { lth = strcspn(V_ptr,PKT_STRG_SEPERATOR);
    memcpy(Target[idx],V_ptr,lth);
    Target[idx][lth] = 0;
    V_ptr += lth + 1;
    if (*V_ptr) V_ptr++;				/* Skip seperator */
  }

  return ( FALSE );
}

/******************************************************************************
/*				GLL_CLOCK_STRG
/*
/*	Returns a pointer to a character string containing the full SCLK
/*  value as passed to the routine.
/*  RETURNS a pointer to a character string.
/*****************************************************************************/
char	*gll_clock_strg(
  gen_sclk_typ	*sclk)
{ static char	clk_strg[256];

  sprintf(clk_strg,"%08d:%02d:%01d.%01d",sclk->gll.rim,sclk->gll.mod91,
          sclk->gll.mod10,sclk->gll.mod8);

  return (clk_strg);
}

/******************************************************************************
/*				MPF_CLOCK_STRG
/*
/*	Returns a pointer to a character string containing the full SCLK
/*  value as passed to the routine.
/*  RETURNS a pointer to a character string.
/*****************************************************************************/
char	*mpf_clock_strg(
  gen_sclk_typ	*sclk)
{ static char	clk_strg[256];

  sprintf(clk_strg,"%010d.%03d",sclk->mpf.Coarse,sclk->mpf.Fine);

  return (clk_strg);
}

/******************************************************************************
/*				NEXT_PARAM_PAIR
/*
/*	Locates the start of the next keyword/value pair in a command packet
/*  buffer.
/*  RETURNS a valid pointer to a character.  If no more pairs are present, a
/*  pointer to a NULL string is returned.
/*****************************************************************************/
char	*next_param_pair(
  char	*pkt)
{ 

  pkt += strcspn(pkt, PKT_PAIR_SEPERATOR);

  return (pkt);
}

/******************************************************************************
/*				NUM_PARAM_PAIRS
/*
/*	Identifies the number of keyword/value pairs that are contained in
/*  the command packet buffer.
/*  RETURNS the number of pairs found.
/*****************************************************************************/
int	num_param_pairs(
  char	*cmnd_pkt)
{ int	pkt_idx = 0,
	lp_cntr = 0;

  while (cmnd_pkt[pkt_idx] != 0)
  { pkt_idx++;
    lp_cntr++;
    pkt_idx += strcspn(&cmnd_pkt[pkt_idx],PKT_PAIR_SEPERATOR);
  }

  return (lp_cntr);
}

/******************************************************************************
/*				PARAM_KEYWORD
/*
/*	Identifies the length of the keyword associated with the value pair
/*  passed in.  The routine assumes there are no leading spaces.
/*  RETURNS the length of the keyword.
/*****************************************************************************/
int	param_keyword(
  char	*kv_pair)
{ int	key_lth,
	pkt_lth;

  pkt_lth = strcspn(kv_pair,PKT_PAIR_SEPERATOR);
  key_lth = strcspn(kv_pair,PARAM_DELIMETER);
  if (pkt_lth < key_lth) return (pkt_lth);

  return (key_lth);
}

/******************************************************************************
/*				PARAM_PAIR
/*
/*	Identifies the start of a keyword/value pair in a command buffer
/*  packet defined by the "pair number" passed as an arguement.  The routine
/*  will return a pointer to the first character of the keyword and the
/*  length of the keyword/value pair.  If the "pair number" specificed is
/*  greater than the number of keyword/value pairs in the buffer, a null
/*  pointer is returned and the length will be 0.  The "pair number" is zero
/*  based, if the first pair is requested, a value of '0' should be specified.
/*  RETURNS a pointer to the first character of a keyword.
/*****************************************************************************/
char	*param_pair(
  char	*cmnd_pkt,
  int	pair_idx,
  int	*kv_lth)
{ int	pkt_idx = 0,
	lp_cntr;

  for (lp_cntr=0; lp_cntr<pair_idx; lp_cntr++)
  { pkt_idx += strcspn(&cmnd_pkt[pkt_idx],PKT_PAIR_SEPERATOR);
    if (cmnd_pkt[pkt_idx] == 0)
    { *kv_lth = 0;
      return (NULL);
    } else pkt_idx++;
  }

  pkt_idx += strspn(&cmnd_pkt[pkt_idx],WHITE_SPACE);
  *kv_lth = strcspn(&cmnd_pkt[pkt_idx],PKT_PAIR_SEPERATOR);
  return (cmnd_pkt+pkt_idx);
}

/******************************************************************************
/*				PARAM_VALUE
/*
/*	Identifies the start of the value portion of a keyword/value pair
/*  as obtained from the command packet buffer.  The length of the value
/*  part is also returned.
/*  RETURNS a pointer to the first character of the value portion or a NULL
/*  pointer if a value portion can not be found.
/*****************************************************************************/
char	*param_value(
  char	*kv_pair,
  int	*value_lth)
{ int   pkt_lth,
	pkt_idx = 0;

  pkt_lth = strcspn(&kv_pair[pkt_idx],PKT_PAIR_SEPERATOR);
  pkt_idx += strcspn(&kv_pair[pkt_idx],PARAM_DELIMETER);
  pkt_idx += strspn(&kv_pair[pkt_idx],PARAM_DELIMETER);
  if (kv_pair[pkt_idx] == 0 || pkt_idx > pkt_lth)
  { *value_lth = 0;
    return (NULL);
  }

  *value_lth = strcspn(&kv_pair[pkt_idx],PKT_PAIR_SEPERATOR);
  return (kv_pair+pkt_idx);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rts_pkt_routines.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE rts_pkt_routines
/*
/*   To Create the build file give the command:
/*
/*		$ vimake rts_pkt_routines			(VMS)
/*   or
/*		% vimake rts_pkt_routines			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE rts_pkt_routines		/* Only one of these */
/*#define PROGRAM rts_pkt_routines		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST rts_pkt_routines.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of rts_pkt_routines imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_rts_pkt_routines.c
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include "rts_typedefs.h"
#include "rts_param_defs.h"
#include "rts_pkt_defs.h"
#include "rts_logging.h"
#include "rts_link_buffers.h"
#include "rts_const_defs.h"

char	*rts_log_prgm = "Pkt_Test";
extern	Uint	rts_log_mask;
extern	Uint	gll_log_mask;
extern	Uint	rts_log_severity;
void	print_rsrcs(link_buf_typ *, int);

main (
  int	argc,
  char	**argv)
{ int   elements,
        lc,
        lth;
  char  *ptr,
        pkt_buffer[2048];
  link_buf_typ  *root = NULL;

  if (argc < 2)
  { printf("No files to parse\n\n");
    printf("Command line arguments are:\n");
    printf("\t1st - CNF file (input)\n");
    printf("\t2nd - UPF file (input) - optional\n");
    exit(0);
  }

  rts_log_mask = 0xFFFFFFFF;
  gll_log_mask = 0xFFFFFFFF;
  rts_log_severity = 0xFFFFFFFF;
  rts_log_to_stdio( TRUE );

  printf("\n");

  load_config_file(argv[1],&root,&elements);
  if (argc > 2) load_param_file(argv[2],root);
  print_rsrcs(root,elements);

  create_cmnd_pkt(pkt_buffer, 2048, root);

  print_rsrcs(root,elements);

  elements = num_param_pairs(pkt_buffer);
  elements = num_param_pairs(pkt_buffer);
  printf("Packet Processing - %d keyword/value pairs\n",elements);
  for (lc=0; lc<elements; lc++)
  { ptr = param_pair(pkt_buffer,lc,&lth);
    if (ptr)
    { printf("Pair %d (%d): %*.*s\n",lc,lth,lth,lth,ptr);
      lth = param_keyword(ptr);
      printf("  Keyword (%d): >%*.*s<\n",lth,lth,lth,ptr);
      ptr = param_value(ptr,&lth);
      if (ptr)
         printf("  Value (%d): %*.*s\n",lth,lth,lth,ptr);
      else printf("  Could not find value\n");
    } else printf("Could not find pair: %d\n",lc);
  }

  exit (0);
}

void	print_rsrcs(
  link_buf_typ	*buffer,
  int		entries)
{ int loop;
  link_buf_typ	*next = buffer;

  printf("\n%d: entries\n",entries);
  for (loop=0; loop<entries; loop++)
  { next = (link_buf_typ *)(next->next);
    print_rsrc((param_defs_typ *)(next->data));
  }

  return;
}
$!-----------------------------------------------------------------------------
$ create tst_rts_pkt_routines.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_rts_pkt_routines
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_rts_pkt_routines			(VMS)
/*   or
/*		% vimake tst_rts_pkt_routines			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_rts_pkt_routines		/* Only one of these */
#define PROGRAM tst_rts_pkt_routines		/* Only one of these */
#define TEST

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_rts_pkt_routines.c

#define MAIN_LANG_C

#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#endif

#ifdef SUBROUTINE
#define P2_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_PVM

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/**********  End of tst_rts_pkt_routines imake file  **********/
$!-----------------------------------------------------------------------------
$ create test_param.cnf
TestProgram.Path.Rsrc1.ParamName:	Integer
TestProgram.Path.Rsrc1.ParamType:	integer(5)
TestProgram.Path.Rsrc1.ParamValid:	:1,2,3,6:12,18
TestProgram.Path.Rsrc1.ParamDefault:	0
TestProgram.Path.Rsrc1.TclName:		TstInteger
TestProgram.Path.Rsrc1.TclForm:		1
!
TestProgram.Path.Rsrc2.ParamName: 	Real_Value
TestProgram.Path.Rsrc2.ParamType :\
					real
TestProgram.Path.Rsrc2.ParamValid:	: 999.99
TestProgram.Path.Rsrc2.ParamDefault:	666.0
TestProgram.Path.Rsrc2.TclName:		TstReal
TestProgram.Path.Rsrc2.TclForm:		2
!
TestProgram.Path.Rsrc3.ParamName:	SCLK
TestProgram.Path.Rsrc3.ParamType:	CLOCK
TestProgram.Path.Rsrc3.ParamClock:	GLL
TestProgram.Path.Rsrc3.ParamValid:	0:0:0.0 : 19999999:90:9.7
TestProgram.Path.Rsrc3.ParamDefault:	123:45:4.3
TestProgram.Path.Rsrc3.TclName:		TstSCLK
TestProgram.Path.Rsrc3.TclForm:		3
!
TestProgram.Path.Rsrc4.ParamName:	Packets
TestProgram.Path.Rsrc4.ParamType:	string(5)
TestProgram.Path.Rsrc4.ParamValid:	ALL, SSI, OPN, PWL, NIMS
TestProgram.Path.Rsrc4.ParamDefault:	ALL
TestProgram.Path.Rsrc4.TclName:		TstPackets
TestProgram.Path.Rsrc4.TclForm:		4
!
TestProgram.Path.Rsrc5.ParamName:	SCET
TestProgram.Path.Rsrc5.ParamType:	DATE
!TestProgram.Path.Rsrc5.ParamValid:	: 1988-03-22T14:15:12.123 
TestProgram.Path.Rsrc5.ParamValid:	: 1989-09-13T17:24:58.987
!TestProgram.Path.Rsrc5.ParamDefault:	1989-09-13T17:24:58.987
TestProgram.Path.Rsrc5.ParamDefault:	1988-03-22T14:15:12.123
TestProgram.Path.Rsrc5.TclName:		TstScet
TestProgram.Path.Rsrc5.TclForm:		5
!

$!-----------------------------------------------------------------------------
$ create test_param.upf
Integer=1,2,3
Real_Value=12.75
SCLK=123:45:6.7
Packets=SSI,OPN,PWL
!Packets=PWH
SCET =	1989-09-13T17:24:58.987
$!-----------------------------------------------------------------------------
$ create readme_rts_pkt.tst
This test program is executed from the VMS or UNIX command line.

For VMS:

  1) A symbolic command must be defined before the test program can be run.
     A command is defined by the following VMS DCL command:
	CMND :== $DEV:[USA001.DIRECTORY]PROGRAM.EXE

		CMND is the symbolic command being defined
		DEV is the disk where the test program exists
		[USA001.DIRECTORY] is the directory/pathname where the
			test program exists
		PROGRAM.EXE is the test program (TST_RTS_PKT_ROUTINES.EXE)

     This symbolic command must be redefined every time you log in.

  2) Execute the following command:
	CMND TEST_PARAM.CNF TEST_PARAM.UPF

For UNIX:

  From the directory where the test program is built, execute the command:
	./tst_rts_pkt_routines test_param.cnf test_param.upf

This is just a short test which defines a few parameters from the configuration
file, modifies them with a parameter file, displays information about each
parameter, converts the parameters into a command packet, and then parses and
displays the packet contents by keyword/value pairs.
$ Return
$!#############################################################################
