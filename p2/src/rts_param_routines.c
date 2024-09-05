/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <time.h>
#include <ctype.h>
#include <stdio.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "rts_errors.h"
#include "rts_logging.h"
#include "rts_typedefs.h"
#include "rts_const_defs.h"
#include "rts_param_defs.h"
#include "rts_link_buffers.h"

/***  Valid resource list  ***/
static	char	*valid_rsrc[] =
		{	KEYWORD_NAME,	VALUE_TYPE,	CLOCK_TYPE,
			VALID_VALUES,	DEFAULT_VALUE,	TCL_NAME,
			TCL_FORM,	0
		};
/* Index relationship of resource list */
#define  PN	0		/* KEYWORD_NAME  */
#define  PT	1		/* VALUE_TYPE    */
#define  PC	2		/* CLOCK_TYPE    */
#define  PV	3		/* VALID_VALUES  */
#define  PD	4		/* DEFAULT_VALUE */
#define  TN	5		/* TCL_NAME      */
#define  TF	6		/* TCL_FORM      */
#define  MAX_RSRC	7	/* Always one greater than last resource */

#define  MODULE_NAME	"PP"
static char	LogMsgBuf[128];

/***	Function declarations	***/
int	check_invalid_date(date_time_typ *);
double	date_diff(date_time_typ *, date_time_typ *);
int	get_next_double(char **, double *);
int	get_next_float(char **, float *);
int	get_next_int(char **, int *);
int	get_next_string(char **, char *);
int	get_param_value(FILE *, char *, char *);
int	get_rsrc_value(FILE *, char *, char *, char *);
int	gll_sclk_valid(gen_sclk_typ *, gen_sclk_typ *, gen_sclk_typ *);
int	obtain_parameter_rsrcs(param_defs_typ **, FILE *);
int	mpf_sclk_valid(gen_sclk_typ *, gen_sclk_typ *, gen_sclk_typ *);
int	parse_param_record(char *, char *, FILE *);
void	parse_value_type(char *, param_defs_typ *);
int	set_clock_valids(param_defs_typ *, char *, int); 
int	set_date_valids(param_defs_typ *, char *);
int	set_double_valids(param_defs_typ *, char *);
int	set_integer_valids(param_defs_typ *, char *);
int	set_real_valids(param_defs_typ *, char *);
int	set_string_valids(param_defs_typ *, char *);
int	valid_range(int, int, int);

/*****************************************************************************
/*				CHECK_INVALID_DATE
/*
/*	Verifies that the supplied date contains a valid day value for the
/*  given month.  It also checks for leap years.
/*  RETURNS TRUE if there is an error, FALSE if it is okay
/*
/*  History:
/*  6-15-1998	T. Nguyen	For Y2K task, modified to use zchk_leap().
/****************************************************************************/
int	check_invalid_date(
  date_time_typ	*value)
{ int	feb_days = 28,
	dom[13] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  if (value->Month != 2)
     if ((int)value->Day > dom[value->Month]) return TRUE;
     else return FALSE;

  if (zchk_leap(value->Year)) feb_days++;
  if ((int)value->Day > feb_days) return TRUE;
  else return FALSE;
}

/*****************************************************************************
/*				COMPARE_VALUES
/*
/*	Checks current values against undo buffer to see if there are
/*  differences.  Returns TRUE if there are difference, FALSE if there are
/*  none
/****************************************************************************/
int	compare_values(
  param_defs_typ	*param)
{ int	idx;
  date_time_typ	*Date,
		*UnDate;
  gen_sclk_typ	*Sclk,
		*UnSclk;

  if (param->num_elements != param->num_undo_elements) return (TRUE);

  switch (param->type)
  { case TYPE_STRING:
         for (idx=0; idx<param->num_elements; idx++)
             if (strcmp(((char **)param->value)[idx],
                        ((char **)param->undo_value)[idx])) return (TRUE);
    break;

    case TYPE_INTEGER:
         for (idx=0; idx<param->num_elements; idx++)
             if (((int *)param->value)[idx] !=
                 ((int *)param->undo_value)[idx]) return (TRUE);
    break;

    case TYPE_REAL:
         for (idx=0; idx<param->num_elements; idx++)
             if (((float *)param->value)[idx] !=
                 ((float *)param->undo_value)[idx]) return (TRUE);
    break;

    case TYPE_DOUBLE:
         for (idx=0; idx<param->num_elements; idx++)
             if (((double *)param->value)[idx] !=
                 ((double *)param->undo_value)[idx]) return (TRUE);
    break;

    case TYPE_DATE:
         for (idx=0; idx<param->num_elements; idx++)
         { Date = &((date_time_typ *)param->value)[idx];
           UnDate = &((date_time_typ *)param->undo_value)[idx];
           if (Date->Year != UnDate->Year) return (TRUE);
           if (Date->Month != UnDate->Month) return (TRUE);
           if (Date->Day != UnDate->Day) return (TRUE);
           if (Date->Hour != UnDate->Hour) return (TRUE);
           if (Date->Minute != UnDate->Minute) return (TRUE);
           if (Date->Second != UnDate->Second) return (TRUE);
           if (Date->Millisecond != UnDate->Millisecond) return (TRUE);
         }
    break;

    case TYPE_CLOCK:
         for (idx=0; idx<param->num_elements; idx++)
         {
         }
    break;

    default:
    break;
  }

  return FALSE;
}

/*****************************************************************************
/*				DATE_DIFF
/*
/*	Calculates the differnece in seconds between the two date/time
/*  parameters.  The first date parameter is subtracted by the second.
/*  RETURNS the difference in seconds
/****************************************************************************/
double	date_diff(
  date_time_typ	*upper,
  date_time_typ	*lower)
{ time_t	upper_time,
		lower_time;
  struct tm	upper_tm,
		lower_tm;

  memset(&upper_tm,0,sizeof(struct tm));
  memset(&lower_tm,0,sizeof(struct tm));
  upper_tm.tm_isdst = lower_tm.tm_isdst = -1;

  upper_tm.tm_year = upper->Year - 1900;
  upper_tm.tm_mon = upper->Month - 1;
  upper_tm.tm_mday = upper->Day;
  upper_tm.tm_hour = upper->Hour;
  upper_tm.tm_min = upper->Minute;
  upper_tm.tm_sec = upper->Second;

  lower_tm.tm_year = lower->Year - 1900;
  lower_tm.tm_mon = lower->Month - 1;
  lower_tm.tm_mday = lower->Day;
  lower_tm.tm_hour = lower->Hour;
  lower_tm.tm_min = lower->Minute;
  lower_tm.tm_sec = lower->Second;

  upper_time = mktime(&upper_tm);
  lower_time = mktime(&lower_tm);
/***
printf("Upper_TM: %s  Upper_t: %s\n",asctime(&upper_tm),ctime(&upper_time));
printf("Lower_TM: %s  Lower_t: %s\n",asctime(&lower_tm),ctime(&lower_time));
/**/

  return(difftime(upper_time, lower_time));
}

/*****************************************************************************
/*				DATE_TO_STRING
/*
/*	Converts the date/time parameter to a string value using sprintf
/*  RETURNS a pointer to the character string containing the date
/****************************************************************************/
char	*date_to_string(
  date_time_typ	*value)
{ static char	DateStrg[32];

  sprintf(DateStrg,"%d-%02d-%02dT%02d:%02d:%02d.%03d",value->Year,
          value->Month,value->Day,value->Hour,value->Minute,value->Second,
          value->Millisecond);
  return (DateStrg);
}

/*****************************************************************************
/*				FIND_KEYWORD_BUFFER
/*
/*	Given a linked buffer list of parameter structures, and a keyword
/*  string, this routine searches the link buffer list for the supplied
/*  keyword and returns a pointer to the buffer link containing the parameter
/*  strucutre.
/*  RETURNS a pointer to the link buffer if there is a match, otherwise it
/*  returns a zero.
/****************************************************************************/
link_buf_typ	*find_keyword_buffer(
  link_buf_typ	*home,
  char	*keyword)
{ int	checked_all = FALSE;
  link_buf_typ	*buffer = home;
  param_defs_typ	*param;

  do
  { buffer = buffer->next;
    param = (param_defs_typ *)buffer->data;
  } while (strcmp(param->name,keyword) && !(checked_all = (home == buffer)));

  if (checked_all) return (link_buf_typ *)0;
  else return buffer;
}

/*****************************************************************************
/*				FREE_PARAMETER_BUFFERS
/*
/*	Frees all the allocated memory of a parameter linked-list buffer.
/*  It verifies that the memory has been allocated and resets the buffer
/*  pointer when finished.  This routine wipes out an entire parameter list,
/*  not just the contents for one parameter entry.
/*  NO RETURN VALUE
/****************************************************************************/
void	free_parameter_buffers(
  link_buf_typ	*buffer)
{ param_defs_typ	*item;

  while (buffer)
  { item = (param_defs_typ *)(buffer->data);
    free_parameter_definitions(&item);
    buffer = delete_buffer_link(buffer);
  }

  return;
}

/*****************************************************************************
/*				FREE_PARAMETER_DEFINITIONS
/*
/*	Frees all the allocated memory of a parameter structure.  It verifies
/*  that the memory has been allocated, and resets the parameter buffer
/*  pointer when it is finished.
/*  NO RETURN VALUE
/****************************************************************************/
void	free_parameter_definitions(
  param_defs_typ	**param_buf)
{ int	idx;
  param_defs_typ	*trash = *param_buf;

  if (!trash) return;

  if (trash->default_value)
     free(trash->default_value);
  trash->default_value = NULL;

  if (trash->value)
  { if (trash->type == TYPE_STRING)
       for (idx=0; idx<trash->max_elements; idx++)
    { if (((string_array_typ)trash->value)[idx])
      { free(((string_array_typ)trash->value)[idx]);
        ((string_array_typ)trash->value)[idx] = NULL;
      }
    } else if (trash->type == TYPE_CLOCK)
       for (idx=0; idx<trash->max_elements; idx++)
    { if (((gen_sclk_typ *)trash->value)[idx].rts.SclkStrg)
      { free(((gen_sclk_typ *)trash->value)[idx].rts.SclkStrg);
        ((gen_sclk_typ *)trash->value)[idx].rts.SclkStrg = NULL;
      }
    } else if (trash->type == TYPE_DATE)
       for (idx=0; idx<trash->max_elements; idx++)
    { if (((date_time_typ *)trash->value)[idx].DateStrg)
      { free(((date_time_typ *)trash->value)[idx].DateStrg);
        ((date_time_typ *)trash->value)[idx].DateStrg = NULL;
      }
    }

    free(trash->value);
    trash->value = NULL;
  }

  if (trash->undo_value)
  { if (trash->type == TYPE_STRING)
       for (idx=0; idx<trash->max_elements; idx++)
    { if (((string_array_typ)trash->undo_value)[idx])
      { free(((string_array_typ)trash->undo_value)[idx]);
        ((string_array_typ)trash->undo_value)[idx] = NULL;
      }
    }

    free(trash->undo_value);
    trash->undo_value = NULL;
  }
  
  if (trash->valid_list)
     free(trash->valid_list);
  trash->valid_list = NULL;

  if (trash->valid_range)
  { free(trash->valid_range[0]);
    free(trash->valid_range[1]);
  }
  trash->valid_range[0] = NULL;
  trash->valid_range[1] = NULL;

  if (trash->strg_buffer)
     free(trash->strg_buffer);
  trash->strg_buffer = NULL;

  free(trash);
  *param_buf = NULL;
}

/*****************************************************************************
/*				GET_NEXT_DATE
/*
/*	Searches a "value" string for the next date value and converts the
/*  string into its binary value.  This routine completely verifies the 
/*  value of the date before returning.  The pointer to the character "value"
/*  string is updated to point to the first character after the parsed value.
/*  RETURNS FALSE if a valid value is returned, TRUE or an error code for any
/*  errors.  If there no value was found, TRUE is returned
/****************************************************************************/

int	get_next_date(
  char	**ptr,
  date_time_typ	*value)
{ int	lth;
  date_time_typ	smallest = {1970,1,1,0,0,0,0,0};

  memcpy(value,&smallest,sizeof(date_time_typ));
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip leading white space */
  if (**ptr == ':' || **ptr == 0)
     return MISSING_VALUE;
  if (**ptr != 'T')			/* Check for Time only field */
  { if (**ptr != '-') value->Year = valid_range(atoi(*ptr),0,2048);
    *ptr = strpbrk(*ptr,"-T:");
    if (*ptr == NULL) return FALSE;
    if (**ptr != '-')
    { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                 "Invalid date format");
      return TRUE;
    } else (*ptr)++;
    value->Month = valid_range(atoi(*ptr),1,12);
    *ptr = strpbrk(*ptr,"-T:");
    if (*ptr == NULL) return FALSE;
    if (**ptr != '-')
    { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                 "Invalid date format");
      return TRUE;
    } else (*ptr)++;
    value->Day = valid_range(atoi(*ptr),1,31);
    *ptr = strpbrk(*ptr,"-T:");
    if (*ptr == NULL) return FALSE;
    if (**ptr != 'T')
    { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                 "Invalid date format");
      return TRUE;
    } else (*ptr)++;
  } else (*ptr)++;

  value->Hour = valid_range(atoi(*ptr),0,23);
  *ptr = strpbrk(*ptr,":");
  if (*ptr == NULL) return FALSE;
  else (*ptr)++;

  value->Minute = valid_range(atoi(*ptr),0,59);
  *ptr = strpbrk(*ptr,":");
  if (*ptr == NULL) return FALSE;
  else (*ptr)++;

  value->Second = valid_range(atoi(*ptr),0,59);
  *ptr = strpbrk(*ptr,".");
  if (*ptr == NULL) return FALSE;
  else (*ptr)++;

  value->Millisecond = valid_range(atoi(*ptr),0,999);
  lth = strspn(*ptr,"0123456789");
  *ptr += lth;
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip trailing white space */

  if (check_invalid_date(value))
  { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Invalid Date/Time specified");
    return TRUE;
  }

  return FALSE;
}

/*****************************************************************************
/*				GET_NEXT_DOUBLE
/*
/*	Searches a "value" string for the next double value and converts the
/*  string into its binary value.  This routine completely verifies the value
/*  of the double before returning.  The pointer to the character "value"
/*  string is updated to point to the first character after the parsed value.
/*  RETURNS FALSE if a valid value is returned, TRUE or an error code for any
/*  errors.  If there no value was found, TRUE is returned
/****************************************************************************/

int	get_next_double(
  char	**ptr,
  double	*value)
{ int	lth;

  *value = -(DBL_MAX);
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip leading white space */
  lth = strspn(*ptr,"0123456789+-.eE");	/* Get length of value */
  if (lth == 0) return MISSING_VALUE;
  if (!strlen(*ptr)) return TRUE;

  *value = atof(*ptr);

  *ptr += lth;				/* Skip valid characters */
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip trailing white space */
  return FALSE;
}

/*****************************************************************************
/*				GET_NEXT_FLOAT
/*
/*	Searches a "value" string for the next float value and converts the
/*  string into its binary value.  This routine completely verifies the value
/*  of the float before returning.  The pointer to the character "value"
/*  string is updated to point to the first character after the parsed value.
/*  RETURNS FALSE if a valid value is returned, TRUE or an error code for any
/*  errors.  If there no value was found, TRUE is returned
/****************************************************************************/

int	get_next_float(
  char	**ptr,
  float	*value)
{ int	lth;

  *value = -(FLT_MAX);
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip leading white space */
  lth = strspn(*ptr,"0123456789+-.");	/* Get length of value */
  if (lth == 0) return MISSING_VALUE;
  if (!strlen(*ptr)) return TRUE;

  *value = (float)atof(*ptr);

  *ptr += lth;				/* Skip valid characters */
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip trailing white space */
  return FALSE;
}

/*****************************************************************************
/*				GET_NEXT_GLL_CLOCK
/*
/*	Searches a "value" string for the next GLL SCLK value and converts the
/*  string into its binary value.  This routine completely verifies the value
/*  of the GLL SCLK before returning.  The pointer to the character "value"
/*  string is updated to point to the first character after the parsed value.
/*  RETURNS FALSE if a valid value is returned, TRUE or an error code for any
/*  errors.  If there no value was found, TRUE is returned
/****************************************************************************/

int	get_next_gll_clock(
  char	**ptr,
  gll_sclk_typ	*value)
{ int	lth;

  memset(value,0,sizeof(gll_sclk_typ));
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip leading white space */
  if (**ptr == 0) return TRUE;

  value->rim = valid_range(atoi(*ptr),0,1677215);
  *ptr = strpbrk(*ptr,":,");
  if (*ptr == 0 || **ptr == ',') return FALSE;
  else (*ptr)++;

  value->mod91 = valid_range(atoi(*ptr),0,90);
  *ptr = strpbrk(*ptr,":,");
  if (*ptr == 0 || **ptr == ',') return FALSE;
  else (*ptr)++;

  value->mod10 = valid_range(atoi(*ptr),0,9);
  *ptr = strpbrk(*ptr,".,");
  if (*ptr == 0 || **ptr == ',') return FALSE;
  else (*ptr)++;

  value->mod8 = valid_range(atoi(*ptr),0,7);
  lth = strspn(*ptr,"0123456789");
  *ptr += lth;
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip trailing white space */

  return FALSE;
}

/*****************************************************************************
/*				GET_NEXT_MPF_CLOCK
/*
/*	Searches a "value" string for the next MPF SCLK value and converts the
/*  string into its binary value.  This routine completely verifies the value
/*  of the MPF SCLK before returning.  The pointer to the character "value"
/*  string is updated to point to the first character after the parsed value.
/*  RETURNS FALSE if a valid value is returned, TRUE or an error code for any
/*  errors.  If there no value was found, TRUE is returned
/****************************************************************************/

int	get_next_mpf_clock(
  char	**ptr,
  mpf_sclk_typ	*value)
{ int	lth;

  memset(value,0,sizeof(gll_sclk_typ));
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip leading white space */
  if (**ptr == 0) return TRUE;

  value->Coarse = valid_range(atoi(*ptr),0,0x7FFFFFFF);
  *ptr = strpbrk(*ptr,".,");
  if (*ptr == 0 || **ptr == ',') return FALSE;
  else (*ptr)++;

  value->Fine = valid_range(atoi(*ptr),0,255);
  lth = strspn(*ptr,"0123456789");
  *ptr += lth;
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip trailing white space */

  return FALSE;
}

/*****************************************************************************
/*				GET_NEXT_INT
/*
/*	Searches a "value" string for the next integer value and converts the
/*  string into its binary value.  This routine completely verifies the value
/*  of the integer before returning.  The pointer to the character "value"
/*  string is updated to point to the first character after the parsed value.
/*  RETURNS FALSE if a valid value is returned, TRUE or an error code for any
/*  errors.  If there no value was found, TRUE is returned
/****************************************************************************/

int	get_next_int(
  char	**ptr,
  int	*value)
{ int	lth;

  *value = LONG_MIN;
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip leading white space */
  lth = strspn(*ptr,"0123456789+-");	/* Get length of value */
  if (lth == 0) return MISSING_VALUE;
  if (!strlen(*ptr)) return TRUE;

  *value = atoi(*ptr);

  *ptr += lth;				/* Skip valid characters */
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip trailing white space */
  return FALSE;
}

/*****************************************************************************
/*				GET_NEXT_STRING
/*
/*	Searches a "value" string for the next string value.  This routine
/*  checks for string delimiters and removes before loading the string into
/*  its value memory.  The pointer to the character "value" string is updated
/*  to point to the first character after the parsed value.
/*  RETURNS FALSE if a valid value is returned, TRUE or an error code for any
/*  errors.  If there no value was found, TRUE is returned
/****************************************************************************/

int	get_next_string(
  char	**ptr,
  char	*value)
{ int	lth;
  char	*temp;

  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip leading white space	*/
  if (!(**ptr)) return TRUE;
  if (strchr(VALID_TOKEN_SET,**ptr))	/* String enclosed in tokens 	*/
  { temp = strchr((*ptr+1),**ptr);
    if (temp)				/* Found terminating token 	*/
    { lth = temp - (++(*ptr));
      *temp = ' ';			/* Erase terminating token, it	*/
					/* will screw-up code later on	*/
    } else
    { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                 "Missing terminating delimiter for string value");
      *ptr += strlen(*ptr);
      return TRUE;
    }
  } else
  { lth = strcspn(*ptr,",");
					/* Remove trailing spaces */
    while (lth > 0 && *(*ptr + (lth-1)) == ' ') lth--;
  }

  memcpy(value,*ptr,lth);		/* Copy over the value		*/
  value[lth] = 0;
  *ptr += lth;
  *ptr += strspn(*ptr,WHITE_SPACE);	/* Skip trailing white space	*/

  return FALSE;
}

/*****************************************************************************
/*				GET_PARAM_VALUE
/*
/*	This routine reads parameter file records and parses it into 2 parts,
/*  the parameter name, and the string containing the value(s).  The
/*  record must be in the following format:
/*     <parameter name> = <value>
/*  All leading spaces and tabs are removed from the components.  
/*
/*	Records which start with an exclaimation point are treated as comments
/*  and ignored.  If a value can not be defined on one line, a back slash
/*  character indicates a continuation to the next line.  The back slash is
/*  removed from the buffer, and the next line is concatrnated to the
/*  previous.  If the next line is terminated by a back slash, this process
/*  is repeated.  The exclamation point as the first character is ignored is
/*  it follows a line terminated by a back slash.
/****************************************************************************/
int	get_param_value(
  FILE	*p_file,
  char	*param,
  char	*value)
{ char	buffer[MAX_RECORD_LTH],
	*p_status,
	*ptr;
  int	lth;

  do
  { p_status = fgets(buffer, (MAX_RECORD_LTH-1), p_file);
    if (p_status != buffer)
    { if (feof(p_file)) return PARAM_EOF;
      /*** I/O Error ... Log it  ***/
      sprintf(LogMsgBuf,"Error reading UPF file - %s",strerror(errno));
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return PARAM_UNDEF;
    }
  } while (buffer[0] == '!');

  ptr = buffer + (strspn(buffer,WHITE_SPACE));	/* Skip leading white space */
  if (strcspn(ptr,VALID_NAME_SET)) return PARAM_BAD_PATH;
  lth = strcspn(ptr,PARAM_DELIMETER);
  memcpy(param,ptr,lth);
  param[lth] = 0;

  ptr += (strspn(ptr+(lth+1),PARAM_DELIMETER));
  strcpy(value,ptr);
  if (buffer[strlen(buffer)] == '\\')		/* Get continuation records */
  { do
    { p_status = fgets(buffer, (MAX_RECORD_LTH-1), p_file);
      if (p_status != buffer)
      { if (feof(p_file)) return PARAM_EOF;
        /*** I/O Error ... Log it  ***/
        sprintf(LogMsgBuf,"Error reading UPF continuation record - %s",
                strerror(errno));
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return PARAM_UNDEF;
      }
      value[strlen(value)-1] = 0;
      strcat(value,buffer);
    } while (buffer[strlen(buffer)] == '\\');
  }

  return FALSE;
}

/*****************************************************************************
/*				GET_RSRC_VALUE
/*
/*	This routine reads configuration records and parses it into 3 parts,
/*  the parameter path, the resource and the value of the resource.  The
/*  record must be in the following format:
/*     <parameter path>.<resource> :   <value>
/*  The <parameter path> may contain a number of levels, each one seperated
/*  by a period.  The pathname and resource variable names may only contain
/*  alphanumeric characters and the dash and underscore character.  All
/*  leading spaces and tabs are removed from the components.  
/*
/*	Records which start with an exclaimation point are treated as comments
/*  and ignored.  If a value can not be defined on one line, a back slash
/*  character indicates a continuation to the next line.  The back slash is
/*  removed from the buffer, and the next line is concatrnated to the
/*  previous.  If the next line is terminated by a back slash, this process
/*  is repeated.  The exclamation point as the first character is ignored is
/*  it follows a line terminated by a back slash.
/****************************************************************************/
int	get_rsrc_value(
  FILE	*c_file,
  char	*path,
  char	*rsrc,
  char	*value)
{ char	buffer[MAX_RECORD_LTH],
	*c_status,
	*ptr,
	*ptr_2;
  int	lth;

  memset(buffer,0,MAX_RECORD_LTH);

  do
  { c_status = fgets(buffer, (MAX_RECORD_LTH-1), c_file);
    if (c_status != buffer)
    { if (feof(c_file)) return PARAM_EOF;
      /*** I/O Error ... Log it  ***/
      sprintf(LogMsgBuf,"Error reading CNF file - %s",
              strerror(errno));
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return PARAM_UNDEF;
    }
  } while (buffer[0] == '!' || (int)strlen(buffer) <= 1);

  lth = strlen(buffer) - 1;
  if (buffer[lth-1] == '\\')		/* Get continuation records */
  { do
    { lth--;
      c_status = fgets(&buffer[lth], (MAX_RECORD_LTH-(lth+1)), c_file);
      if (c_status != &buffer[lth])
      { if (feof(c_file)) return PARAM_EOF;
        /*** I/O Error ... Log it  ***/
        sprintf(LogMsgBuf,"Error reading CNF continuation record - %s",
                strerror(errno));
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return PARAM_UNDEF;
      }
      lth = strlen(buffer) - 1;
    } while (buffer[lth-1] == '\\');
  }
  if (buffer[lth] == '\n') buffer[lth] = 0;

  ptr = buffer + (strspn(buffer,WHITE_SPACE));	/* Skip leading white space */
  lth = strcspn(ptr,R_V_DELIMETER);
  if (lth > MAX_PATH_LTH+MAX_NAME_LTH) return PARAM_UNDEF;
  memcpy(path,ptr,lth);				/* path & rsrc strings      */
  path[lth] = 0;

  ptr_2 = strrchr(path,'.');
  if (!ptr_2) return PARAM_NO_PERIOD;
  *ptr_2 = 0;					/* Terminates path correctly */
  if ((int)strlen(path) > MAX_PATH_LTH) return PARAM_BAD_PATH;
  if ((int)strlen(ptr_2+1) > MAX_NAME_LTH) return PARAM_UNDEF;
  strcpy(rsrc,ptr_2+1);

  ptr_2 = strpbrk(ptr+lth,":");
  if (ptr_2 == NULL) return PARAM_UNDEF;
  ptr_2 += strspn(ptr_2+1,WHITE_SPACE) + 1;
  strcpy(value,ptr_2);

  return FALSE;
}

/*****************************************************************************
/*				GLL_SCLK_VALID
/*
/*	Verifies that the three GLL SCLK parameters are ordered in time
/*  sequential order with the first SCLK being the earliest, and the last
/*  being the most recent.
/*  RETURNS TRUE if the SCLKs are ordered correctly, FALSE if there is an
/*  error in the order of the SCLKs.
/****************************************************************************/
int	gll_sclk_valid(
  gen_sclk_typ	*lower,
  gen_sclk_typ	*target,
  gen_sclk_typ	*upper)
{

  if (lower->gll.rim > target->gll.rim) return FALSE;
  if (lower->gll.mod91 > target->gll.mod91) return FALSE; 
  if (lower->gll.mod10 > target->gll.mod10) return FALSE;
  if (lower->gll.mod8 > target->gll.mod8) return FALSE;

  if (target->gll.rim > upper->gll.rim) return FALSE;
  if (target->gll.mod91 > upper->gll.mod91) return FALSE;
  if (target->gll.mod10 > upper->gll.mod10) return FALSE;
  if (target->gll.mod8 > upper->gll.mod8) return FALSE;

  return TRUE;
}

/*****************************************************************************
/*				LOAD_CONFIG_FILE
/*
/*	Reads a configuration file and creates the linked buffer list of
/*  parameter structures.  This routine also returns the number of valid
/*  parameters parsed.
/*  RETURNS FALSE if there were no processing errors, otherwise either an
/*  error code is returned.
/****************************************************************************/
int	load_config_file(
  char		*filename,
  link_buf_typ	**parameters,
  int		*elements)
{ int	status;
  FILE	*config_file;
  link_buf_typ	*current = NULL;
  param_defs_typ	*param;

  /* open file */
  config_file = fopen(filename, "r");
  if (!config_file)
  { sprintf(LogMsgBuf,"File open error: %s (%s)",
            strerror(errno),filename);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               LogMsgBuf);
    return OPEN_ERROR;
  }

  *elements = 0;
  do
  {
    /* Allocate param_defs_typ buffer */
    current = append_buffer_link(current);
    if (current) (*elements)++;
    else
    { /*** Memory Allocation Error ***/
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                 "Error allocating memory for parameter buffer link");
      return ALLOCATE_ERROR;
    }

    sprintf(LogMsgBuf,"New Resource: %d",*elements);
    param = NULL;			/* Start with an empty pointer */
    rts_logger(RTS_LOG_PARAM,RTS_LOG_DEBUG,MODULE_NAME,LogMsgBuf);
    status = obtain_parameter_rsrcs(&param,config_file);
    current->data = (void *)param;
    if (status)
    { /* Error ? */
      if (!current->data)		/* Might have created it though */
      { current = delete_buffer_link(current);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,
                   "Could not completely define parameter");
        if (*elements) (*elements)--;
      }
    }
  } while (status != PARAM_EOF);
  *parameters = current;

  /* Close file */
  fclose(config_file);
  return FALSE;
}

/*****************************************************************************
/*				LOAD_PARAM_FILE
/*
/*	Loads a parameter file into an existing linked buffer list of
/*  parameter strucutres.
/*  RETURNS FALSE if there were no errors, otherwise it returns a file open
/*  error code.
/****************************************************************************/
int	load_param_file(
  char		*filename,
  link_buf_typ	*parameters)
{ int	status;
  char	keyword[MAX_NAME_LTH],
	value[MAX_VALUE_LTH];
  FILE	*param_file;
  link_buf_typ	*buffer = parameters;
  param_defs_typ	*param;

  /* open file */
  param_file = fopen(filename, "r");
  if (!param_file)
  { sprintf(LogMsgBuf,"File open error: %s (%s)",
            strerror(errno),filename);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               LogMsgBuf);
    return OPEN_ERROR;
  }

  rts_logger(RTS_LOG_PARAM,RTS_LOG_DEBUG,MODULE_NAME,"Loading Parameters");
  while (!parse_param_record(keyword,value,param_file))
  { buffer = find_keyword_buffer(buffer,keyword);
    if (!buffer)
    { sprintf(LogMsgBuf,"Undefined keyword: %s",keyword);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
      buffer = parameters;
      continue;
    }

    param = (param_defs_typ *)buffer->data;
    switch (param->type)
    { case TYPE_INTEGER:
                   status = set_integer_value(param, value, FALSE);
      break;
      case TYPE_REAL:
                   status = set_real_value(param, value, FALSE);
      break;
      case TYPE_DOUBLE:
                   status = set_double_value(param, value, FALSE);
      break;
      case TYPE_STRING:
                   status = set_string_value(param, value, FALSE);
      break;
      case TYPE_DATE:
                   status = set_date_value(param, value, FALSE);
      break;
      case TYPE_CLOCK:
                   status = set_clock_value(param, value, FALSE);
      break;
      default: sprintf(LogMsgBuf,"Undefined keyword type: %d",param->type);
               rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      break;
    }

    if (status)
    { sprintf(LogMsgBuf,"Error processing value(s) for parameter: %s",
              param->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_TRACE_ERR,MODULE_NAME,LogMsgBuf);
    }
  }

  fclose(param_file);
  return FALSE;
}

/*****************************************************************************
/*				MPF_SCLK_VALID
/*
/*	Verifies that the three MPF SCLK parameters are ordered in time
/*  sequential order with the first SCLK being the earliest, and the last
/*  being the most recent.
/*  RETURNS TRUE if the SCLKs are ordered correctly, FALSE if there is an
/*  error in the order of the SCLKs.
/****************************************************************************/
int	mpf_sclk_valid(
  gen_sclk_typ	*lower,
  gen_sclk_typ	*target,
  gen_sclk_typ	*upper)
{

  if (lower->mpf.Coarse > target->mpf.Coarse) return FALSE;
  if (lower->mpf.Fine > target->mpf.Fine) return FALSE; 

  if (target->mpf.Coarse > upper->mpf.Coarse) return FALSE;
  if (target->mpf.Fine > upper->mpf.Fine) return FALSE;

  return TRUE;
}

/*****************************************************************************
/*				OBTAIN_PARAMETER_RSRCS
/*
/*	Searches for the first record that contains a configuration record
/* with a resource of KEYWORD_NAME.  Once that record is found, it allocates
/* memory for a parameter definition structure and continues reading all the
/* records with the same parameter path.  It determines what resource is being
/* defined and fills in the value into the parameter definition strucutre.
/*
/*	If there are not enough resources defined to support parameter
/* parameter processing, the parameter definition structure's memory is
/* released and the returned pointer is set to set to zero.
/*
/*	If an error occurred while processing the records, the function's
/* return value is set to the error code.  It is possible to generate a
/* parameter definition structure and still return an error (e.g., EOF).
/* Both the return status and value of the structure pointer must be checked.
/****************************************************************************/

int	obtain_parameter_rsrcs(
  param_defs_typ	**param,
  FILE			*c_file)
{ int	status,
	idx,
	def_value,
	need_name = TRUE,
	need_type = TRUE,
	need_clock = TRUE;
  char	rsrc[MAX_NAME_LTH],
	value[MAX_VALUE_LTH],
	rsrc_path[MAX_PATH_LTH+MAX_NAME_LTH],
	current_path[MAX_PATH_LTH];
  long	last_position;
  param_defs_typ	*local;

  status = get_rsrc_value(c_file,current_path,rsrc,value);
  if (status)
  { /*** Resource Definition Error  ***/
    rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Could not process resource defintion record(s)");
    return status;
  }
  last_position = ftell(c_file);

  *param = (param_defs_typ *)malloc(sizeof(param_defs_typ));
  if (*param) local = *param;
  else
  { /***  Could not allocate Memory  ***/
    rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Error allocating parameter buffer");
    return ALLOCATE_ERROR;
  }
  memset(local,0,sizeof(param_defs_typ));

  while (TRUE)
  { for (idx=0; valid_rsrc[idx] && strcmp(rsrc,valid_rsrc[idx]); idx++);
    switch (idx)
    { case  PN: /***  ParamName  ***/
                strcpy(local->name,value);
		need_name = FALSE;
      break;
      case  PT: /***  ValueType  ***/
                parse_value_type(value,local);
                need_type = !local->type;
                if (local->type != TYPE_CLOCK) need_clock = FALSE;
      break;
      case  PC: /***  ParamClock  ***/
                for (idx=0; strcmp(value,valid_project_sclks[idx]) && 
                            valid_project_sclks[idx]; idx++);
                if (idx < CLOCK_MAX && idx > CLOCK_NULL)
                { local->clock_type = idx;
                  need_clock = FALSE;
                }
      break;
      case  PV: /***  ParamValid  ***/
      case  PD: /***  ParamDefault  ***/
                def_value = (idx == PD);

                switch (local->type)
                { case TYPE_INTEGER:
                         if (def_value)
                            status = set_integer_value(local,value,def_value);
                         else status = set_integer_valids(local,value);
                       break;
                  case TYPE_REAL:
                         if (def_value)
                            status = set_real_value(local,value,def_value);
                         else status = set_real_valids(local,value);
                       break;
                  case TYPE_DOUBLE:
                         if (def_value)
                            status = set_double_value(local,value,def_value);
                         else status = set_double_valids(local,value);
                       break;
                  case TYPE_STRING:
                         if (def_value)
                            status = set_string_value(local,value,def_value);
                         else status = set_string_valids(local,value);
                       break;
                  case TYPE_DATE:
                         if (def_value)
                            status = set_date_value(local,value,def_value);
                         else status = set_date_valids(local,value);
                       break;
                  case TYPE_CLOCK:
                         if (def_value)
                            status = set_clock_value(local,value,def_value);
                         else status = set_clock_valids(local,value,
                                                        local->clock_type);
                       break;
                  default:
                         sprintf(LogMsgBuf,"Undefined value type: %d",
                                 local->type);
                         rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,
                                    LogMsgBuf);
                       break;
                }
      break;
      case  TN: /***  TclName  ***/
                strcpy(local->tcl_name,value);
      break;
      case  TF: /***  TclForm  ***/
                local->tcl_form = atoi(value);
      break;
      default:
                strcpy(LogMsgBuf,"Unrecognized resource type: ");
                strcat(LogMsgBuf,rsrc);
                rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                           LogMsgBuf);
      break;
    }

    status = get_rsrc_value(c_file,rsrc_path,rsrc,value);
    if (status && status != PARAM_EOF)
    { /***  Get RSRC Value Error  ***/
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                 "Could not process resource definition record(s)");
    }

    if (strcmp(rsrc_path,current_path) || feof(c_file))
    { if (need_name || need_type || need_clock)
      { /***  Free entire structure ... imcomplete  ***/
        rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,
                   "Resource definition incomplete");
        free_parameter_definitions(param);
      }

      if (feof(c_file)) return PARAM_EOF;

      status = fseek(c_file, last_position, SEEK_SET);
      if (status)
      { /*** File Seek Error  ***/
        rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,
                   "Could not rewind configuration file");
      }

      if (need_name || need_type || need_clock) return TRUE;
      return FALSE;
    }
    last_position = ftell(c_file);
  }
}

/*****************************************************************************
/*				PARSE_PARAM_RECORD
/*
/*	Parses a parameter file record(s) into a keyword and value part.  This
/*  routine will read and concatonate continuation records to the value
/*  part, and skip over any comment records (those starting with a '!')
/*  RETURNS FALSE if the record(s) was read correctly, and an error code or
/*  TRUE if there was an error extracting the keyword and value.
/****************************************************************************/
int	parse_param_record(
  char	*keyword,
  char	*value,
  FILE	*p_file)
{ int	lth,
	status;
  char	*ptr,
	*p_status,
	buffer[MAX_RECORD_LTH];

  memset(buffer,0,MAX_RECORD_LTH);

  do
  { p_status = fgets(buffer, (MAX_RECORD_LTH-1), p_file);
    if (p_status != buffer)
    { if (feof(p_file)) return PARAM_EOF;
      /***  I/O Error ... Log it  ***/
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                 "Could not read parameter file record");
      return PARAM_UNDEF;
    }
  } while (buffer[0] == '!');

  lth = strlen(buffer) - 1;
  if (buffer[lth-1] == '\\')		/* Get continuation records */
  { do
    { lth--;
      p_status = fgets(&buffer[lth], (MAX_RECORD_LTH-(lth+1)), p_file);
      if (p_status != &buffer[lth])
      { if (feof(p_file)) return PARAM_EOF;
        /*** I/O Error ... Log it  ***/
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                   "Could not read parameter continuation record");
        return PARAM_UNDEF;
      }
      lth = strlen(buffer) - 1;
    } while (buffer[lth-1] == '\\');
  }
  if (buffer[lth] == '\n') buffer[lth] = 0;
  
  ptr = buffer + (strspn(buffer,WHITE_SPACE));
  if (strcspn(ptr,VALID_KEYWORD_SET))
  { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Invalid character where keyword should be");
    return PARAM_UNDEF;
  }
  lth = strcspn(ptr,PARAM_DELIMETER);
  if (lth >= MAX_NAME_LTH)
  { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Keyword name too long");
    return PARAM_UNDEF;
  }
  memcpy(keyword,ptr,lth);
  keyword[lth] = 0;

  ptr += lth + 1;
  ptr += strspn(ptr,PARAM_DELIMETER);
  if ((int)strlen(ptr) > MAX_VALUE_LTH)
  { sprintf(LogMsgBuf,"Value for keyword (%s) too long (%d vs %d)",
            keyword, strlen(ptr), MAX_VALUE_LTH);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    keyword[0] = 0;			/* clear the keyword string */
    return TRUE;
  }
  strcpy(value,ptr);

  return FALSE;
}

/*****************************************************************************
/*				PARSE_VALUE_TYPE
/*
/*	Identifies the value of resource: ParamType, and if it is an array
/* or just a single element.
/*
/*	Also allocate the memory for the default and value elements of the
/* parameter definition structure.
/*
/*	If it can not identify the resource or allocate the memory, it sets
/* the ParamType to undefined.  If the array size is too large, it truncates
/* it to the maximum allowed array size.
/****************************************************************************/

void	parse_value_type(
  char	*buffer,
  param_defs_typ	*param)
{ int	idx;
  char	*e_buf,
	l_buf[32];

  param->type = TYPE_NULL;
  param->max_elements = 1;
  memset(l_buf,0,sizeof(l_buf));

  /***  Convert alpha portion of string to Uppercase  ***/
  for (idx=0; idx<(int)sizeof(l_buf) && idx<(int)strlen(buffer); idx++)
  { if (!isalpha(buffer[idx]))
    { l_buf[idx] = 0;
      break;
    }
    l_buf[idx] = (char)toupper(buffer[idx]); 
  }
  /***  Check against valid types list  ***/
  for (idx=0; idx<TYPE_MAX; idx++)
      if (!strcmp(valid_param_types[idx],l_buf)) break;
  if (idx < TYPE_MAX && idx != TYPE_NULL) param->type = idx;
  else
  { strcpy(LogMsgBuf,"Undefined parameter type: ");
    strcat(LogMsgBuf,buffer);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
    return;
  }
  /***  Get number of elements  ***/
  e_buf = strchr(buffer,'(');
  if (e_buf) param->max_elements = atoi(e_buf+1);
  if (param->max_elements < 1 || param->max_elements > MAX_VALUE_ELEMENTS)
  { sprintf(LogMsgBuf,"Array definition out of bounds (%d vs %d)",
            param->max_elements,MAX_VALUE_ELEMENTS);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
    param->max_elements = MAX_VALUE_ELEMENTS;
  }

  if ((param->type == TYPE_DATE || param->type == TYPE_CLOCK) &&
      param->max_elements > 1)
  { param->max_elements = 1;
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,
               "DATEs and CLOCKs can not be array parameters");
  }

  /***  Allocate memory to contain the value(s) of the parameter  ***/
  param->value = malloc(param->max_elements*sizeof_types[param->type]);
  if (!param->value)
  { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Error allocating memory for valid values");
    param->type = 0;
    return;
  }
  memset(param->value,0,(param->max_elements*sizeof_types[param->type]));

  /***  Allocate memory to contain the undo value(s) of the parameter  ***/
  param->undo_value = malloc(param->max_elements*sizeof_types[param->type]);
  if (!param->undo_value)
  { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Error allocating memory for valid undo values");
    param->type = 0;
    return;
  }
  memset(param->undo_value,0,(param->max_elements*sizeof_types[param->type]));

  /***  Allocate memory to contain the default value of the parameter  ***/
  param->default_value = malloc(sizeof_types[param->type]);
  if (!param->default_value)
  { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
               "Error allocating memory for default value");
    param->type = 0;
  }
  memset(param->default_value,0,(sizeof_types[param->type]));

  return;
}

/*****************************************************************************
/*				PRINT_XXX_VALUES
/*
/*	The following "print" routines are mainly for debugging purposes.
/* They are included here as an easy way of verifying what was read and
/* processed by the parameter processing routines.
/****************************************************************************/
void	print_clock_values(
  param_defs_typ	*rsrc)
{ int	loop;
  gen_sclk_typ	*sclk_0,
		*sclk_1;

  printf("  Valid Range:\n");
  for (loop=0; loop<rsrc->max_range; loop++)
  { sclk_0 = &((gen_sclk_typ *)rsrc->valid_range[0])[loop];
    sclk_1 = &((gen_sclk_typ *)rsrc->valid_range[1])[loop];

    switch(rsrc->clock_type)
    { case CLOCK_GLL: printf("   (%08d:%02d:%01d.%01d : ",
                sclk_0->gll.rim,sclk_0->gll.mod91,sclk_0->gll.mod10,
                sclk_0->gll.mod8);
                printf("%08d:%02d:%01d.%01d)\n",
                       sclk_1->gll.rim,sclk_1->gll.mod91,sclk_1->gll.mod10,
                       sclk_1->gll.mod8);
      break;

      case CLOCK_MPF: printf("   (%10d.%03d : ",
                sclk_0->mpf.Coarse,sclk_0->mpf.Fine);
                printf("%-10d.%03d)\n",
                       sclk_1->mpf.Coarse,sclk_1->mpf.Fine);
      break;

      case CLOCK_COBT: printf("COBT clock type\n");
      break;

      default: printf("wrong clock type\n");
      break;
    }
  }
  printf("  Default Value: ");
  sclk_0 = (gen_sclk_typ *)rsrc->default_value;
  switch(rsrc->clock_type)
  { case CLOCK_GLL: printf("%08d:%02d:%01d.%01d\n",
              sclk_0->gll.rim,sclk_0->gll.mod91,sclk_0->gll.mod10,
              sclk_0->gll.mod8);
    break;
    case CLOCK_MPF: printf("%10d.%03d\n",
                sclk_0->mpf.Coarse,sclk_0->mpf.Fine);
    break;
    case CLOCK_COBT: printf("COBT clock type\n");
    break;
    default: printf("wrong clock type\n");
    break;
  }

  printf("  Values:\n");
  for (loop=0; loop<rsrc->num_elements; loop++)
  { sclk_0 = &((gen_sclk_typ *)rsrc->value)[loop];
    switch(rsrc->clock_type)
    { case CLOCK_GLL: printf("   %08d:%02d:%01d.%01d\n",
                sclk_0->gll.rim,sclk_0->gll.mod91,sclk_0->gll.mod10,
                sclk_0->gll.mod8);
      break;

      case CLOCK_MPF: printf("%10d.%03d\n",
                sclk_0->mpf.Coarse,sclk_0->mpf.Fine);
      break;

      case CLOCK_COBT: printf("COBT clock type\n");
      break;

      default: printf("wrong clock type\n");
      break;
    }
  }

  printf("  Undo Values:\n");
  for (loop=0; loop<rsrc->num_undo_elements; loop++)
  { sclk_0 = &((gen_sclk_typ *)rsrc->undo_value)[loop];
    switch(rsrc->clock_type)
    { case CLOCK_GLL: printf("   %08d:%02d:%01d.%01d\n",
                sclk_0->gll.rim,sclk_0->gll.mod91,sclk_0->gll.mod10,
                sclk_0->gll.mod8);
      break;

      case CLOCK_MPF: printf("%10d.%03d\n",
                sclk_0->mpf.Coarse,sclk_0->mpf.Fine);
      break;

      case CLOCK_COBT: printf("COBT clock type\n");
      break;

      default: printf("wrong clock type\n");
      break;
    }
  }

  return;
}

void	print_date_values(
  param_defs_typ	*rsrc)
{ int	loop;

  printf("  Valid Range:\n");
  for (loop=0; loop<rsrc->max_range; loop++)
  { printf("   (%s : ",
           date_to_string(&((date_time_typ *)rsrc->valid_range[0])[loop]));
    printf("%s)\n",
           date_to_string(&((date_time_typ *)rsrc->valid_range[1])[loop]));
  }
  printf("  Default Value: %s\n",
         date_to_string((date_time_typ *)rsrc->default_value));
  printf("  Values:\n");
  for (loop=0; loop<rsrc->num_elements; loop++)
      printf("\t%s\n",date_to_string(&((date_time_typ *)rsrc->value)[loop]));
  printf("  Undo Values:\n");
  for (loop=0; loop<rsrc->num_undo_elements; loop++)
      printf("\t%s\n",date_to_string(&((date_time_typ *)rsrc->undo_value)[loop]));

  return;
}

void	print_double_values(
  param_defs_typ	*rsrc)
{ int	loop;

  printf("  Valid List: ");
  for (loop=0; loop<rsrc->max_list; loop++)
      printf("%e  ",((double *)rsrc->valid_list)[loop]);
  printf("\n  Valid Range: ");
  for (loop=0; loop<rsrc->max_range; loop++)
      printf("(%e : %e)   ",((double *)rsrc->valid_range[0])[loop],
             ((double *)rsrc->valid_range[1])[loop]);
  printf("\n");
  printf("  Default Value: %e\n",*(double *)rsrc->default_value);
  printf("  Values: ");
  for (loop=0; loop<rsrc->num_elements; loop++)
      printf("%e  ",((double *)rsrc->value)[loop]);
  printf("\n");
  printf("  Undo Values: ");
  for (loop=0; loop<rsrc->num_undo_elements; loop++)
      printf("%e  ",((double *)rsrc->undo_value)[loop]);
  printf("\n");

  return;
}

void	print_int_values(
  param_defs_typ	*rsrc)
{ int	loop;

  printf("  Valid List: ");
  for (loop=0; loop<rsrc->max_list; loop++)
      printf("%d  ",((int *)rsrc->valid_list)[loop]);
  printf("\n  Valid Range: ");
  for (loop=0; loop<rsrc->max_range; loop++)
      printf("(%d : %d)   ",((int *)rsrc->valid_range[0])[loop],
             ((int *)rsrc->valid_range[1])[loop]);
  printf("\n");
  printf("  Default Value: %d\n",*(int*)rsrc->default_value);
  printf("  Values: ");
  for (loop=0; loop<rsrc->num_elements; loop++)
      printf("%d  ",((int *)rsrc->value)[loop]);
  printf("\n");
  printf("  Undo Values: ");
  for (loop=0; loop<rsrc->num_undo_elements; loop++)
      printf("%d  ",((int *)rsrc->undo_value)[loop]);
  printf("\n");

  return;
}

void	print_real_values(
  param_defs_typ	*rsrc)
{ int	loop;

  printf("  Valid List: ");
  for (loop=0; loop<rsrc->max_list; loop++)
      printf("%f  ",((float *)rsrc->valid_list)[loop]);
  printf("\n  Valid Range: ");
  for (loop=0; loop<rsrc->max_range; loop++)
      printf("(%f : %f)   ",((float *)rsrc->valid_range[0])[loop],
             ((float *)rsrc->valid_range[1])[loop]);
  printf("\n");
  printf("  Default Value: %f\n",*(float *)rsrc->default_value);
  printf("  Values: ");
  for (loop=0; loop<rsrc->num_elements; loop++)
      printf("%f  ",((float *)rsrc->value)[loop]);
  printf("\n");
  printf("  Undo Values: ");
  for (loop=0; loop<rsrc->num_undo_elements; loop++)
      printf("%f  ",((float *)rsrc->undo_value)[loop]);
  printf("\n");

  return;
}

void	print_rsrc(
  param_defs_typ	*rsrc)
{
  printf("Rsrc Name: %s\n",rsrc->name);
  printf("Rsrc Type: %s (%d)\n",valid_param_types[rsrc->type],
         rsrc->max_elements);
  printf("Clock Type: %s\n",valid_project_sclks[rsrc->clock_type]);
  switch (rsrc->type)
  { case TYPE_INTEGER: print_int_values(rsrc);
    break;
    case TYPE_REAL: print_real_values(rsrc);
    break;
    case TYPE_DOUBLE: print_double_values(rsrc);
    break;
    case TYPE_STRING: print_string_values(rsrc);
    break;
    case TYPE_DATE: print_date_values(rsrc);
    break;
    case TYPE_CLOCK: print_clock_values(rsrc);
    break;
    default: printf("Value displays under construction\n");
    break;
  }
  printf("TCL Name: %s\n",rsrc->tcl_name);
  printf("TCL Form: %d\n",rsrc->tcl_form);

  printf("\n");
  return;
}

void	print_string_values(
  param_defs_typ	*rsrc)
{ int	loop;

  printf("  Valid List:\n");
  for (loop=0; loop<rsrc->max_list; loop++)
      printf("\t>%s<\n",((string_array_typ)rsrc->valid_list)[loop]);
  printf("  Default Value: >%s<\n",(char *)rsrc->default_value);
  printf("  Values:\n");
  for (loop=0; loop<rsrc->num_elements; loop++)
      printf("\t>%s<\n",((string_array_typ)rsrc->value)[loop]);
  printf("  Undo Values:\n");
  for (loop=0; loop<rsrc->num_undo_elements; loop++)
      printf("\t>%s<\n",((string_array_typ)rsrc->undo_value)[loop]);

  return;
}

/*****************************************************************************
/*				SET_CLOCK_VALIDS
/*
/*	Loads the valid range elements of a parameter strucutre for a SCLK
/*  parameter.
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/
int	set_clock_valids(
  param_defs_typ	*local,
  char			*value,
  int			clk_type)
{ int	idx,
	clock_size,
	r_idx = 0,
	status;
  char	*ptr = value;
  gen_sclk_typ	temp,
		sclk_range[MAX_RANGE_ENTRIES][2];

  switch(clk_type)
  { case CLOCK_GLL: clock_size = sizeof(gll_sclk_typ);
    break;

    case CLOCK_MPF: clock_size = sizeof(mpf_sclk_typ);
    break;

    case CLOCK_COBT: clock_size = sizeof(gen_sclk_typ);
    break;

    default: sprintf(LogMsgBuf,"%s; Unsupported clock type: %d",
                     local->name,clk_type);
             rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
             return TRUE;
    break;
  }

  do
  { switch(clk_type)
    { case CLOCK_GLL: status = get_next_gll_clock(&ptr,&temp.gll);
      break;

      case CLOCK_MPF: status = get_next_mpf_clock(&ptr,&temp.mpf);
      break;

      case CLOCK_COBT: status = TRUE;	/* unlimited values */
      break;

      default: sprintf(LogMsgBuf,"%s; Fatal logic error in clock routine",
                       local->name);
               rts_logger(RTS_LOG_PARAM,RTS_LOG_FATAL,MODULE_NAME,LogMsgBuf);
               return TRUE;
      break;
    }
    if (status) continue;
    if (*ptr == ':')
    { memcpy(&sclk_range[r_idx][0],&temp,clock_size);
      ptr++;
      switch(clk_type)
      { case CLOCK_GLL: status = get_next_gll_clock(&ptr,&temp.gll);
        break;

        case CLOCK_MPF: status = get_next_mpf_clock(&ptr,&temp.mpf);
        break;

        case CLOCK_COBT:
        break;

        default: sprintf(LogMsgBuf,"%s; Fatal logic error in clock routine",
                         local->name);
                 rts_logger(RTS_LOG_PARAM,RTS_LOG_FATAL,MODULE_NAME,LogMsgBuf);
                 return TRUE;
        break;
      }
      if (status)
         memset(&temp,0xFF,clock_size);
      /* check range for correct order */
      memcpy(&sclk_range[r_idx++][1],&temp,clock_size);
    } else
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,"%s; Illegal value seperator between values: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    }

    if (*ptr != 0 && *ptr != ',')
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,
              "%s; Illegal value seperator after range definition: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    } else if (*ptr != 0) ptr++;
  } while (strlen(ptr));
  local->max_range = r_idx;

  /***  Allocate valid range for param_def structure, and fill it  ***/
  if (r_idx)
  { local->valid_range[0] = malloc(r_idx*clock_size);
    if (!local->valid_range[0])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - lower",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }

    local->valid_range[1] = malloc(r_idx*clock_size);
    if (!local->valid_range[1])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - upper",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<r_idx; idx++)
    { memcpy(&((gen_sclk_typ*)local->valid_range[0])[idx],&sclk_range[idx][0],
             clock_size);
      memcpy(&((gen_sclk_typ*)local->valid_range[1])[idx],&sclk_range[idx][1],
             clock_size);
    }
  }

  return FALSE;
}

/*****************************************************************************
/*                              SET_CLOCK_VALUE
/*
/*	Loads the value or default value element of a parameter strucutre for
/*  a SCLK parameter.  If the value element is set, the updated element is
/*  also set to TRUE;
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/
int	set_clock_value(
  param_defs_typ	*param,
  char			*buffer,
  int			def_flag)
{ int	max_idx,
	idx,
	status;
  char	*b_ptr = buffer,
	SclkStrg[24];
  gen_sclk_typ	*destination,
		value;

  if (def_flag)
  { destination = (gen_sclk_typ *)param->default_value;
    max_idx = 1;
  } else
  { destination = (gen_sclk_typ *)param->value;
    max_idx = param->max_elements;
  }

  for (idx=0; idx<max_idx; idx++)
  { switch (param->clock_type)
    { case CLOCK_GLL:
           status = get_next_gll_clock(&b_ptr,(gll_sclk_typ *)(&value));
           if (!(status))
              sprintf(SclkStrg,"%08d:%02d:%01d.%01d",
                      value.gll.rim,value.gll.mod91,value.gll.mod10,
                      value.gll.mod8);
      break;

      case CLOCK_MPF:
           status = get_next_mpf_clock(&b_ptr,(mpf_sclk_typ *)(&value));
           if (!(status))
              sprintf(SclkStrg,"%010d.%03d",value.mpf.Coarse,value.mpf.Fine);
      break;


      default: status = TRUE;
      break;
    }

    if (status) break;
    if (verify_clock_value(param,&value,param->clock_type))
    { memcpy(&destination[idx],&value,sizeof(gen_sclk_typ));
    }
    else
    { /*** invalid clock  ***/
      sprintf(LogMsgBuf,"%s; Invalid parameter value",param->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      idx--;
    }

    if (b_ptr == 0) b_ptr = buffer + strlen(buffer);
    if (*b_ptr == ',') b_ptr++;
  }

  /*** Check for more values in buffer ***/
  switch (param->clock_type)
  { case CLOCK_GLL:
         status = get_next_gll_clock(&b_ptr,(gll_sclk_typ*)(&value));
    break;

    case CLOCK_MPF:
         status = get_next_mpf_clock(&b_ptr,(mpf_sclk_typ*)(&value));
    break;

    default: status = TRUE;
    break;
  }
  if (!status)
  { /*** more clock values defined ***/
    sprintf(LogMsgBuf,"%s; Extra parameter values defined",param->name);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
  }

  /***  Load default value initially, and on error  ***/
  if (def_flag || idx == 0)
  { param->num_elements = idx = 1;
    memcpy((gen_sclk_typ *)param->value,(gen_sclk_typ *)param->default_value,
            sizeof(gen_sclk_typ));
    param->updated = TRUE;

    if (def_flag)
    { param->num_undo_elements = 1;
      memcpy((void *)param->undo_value,(void *)param->default_value,
             sizeof(gen_sclk_typ));
    }
  }

  param->num_elements = idx;
  /***  Allocate and load TCL string buffer  ***/
  if (idx > 0)
  { destination = (gen_sclk_typ *)param->value;
    for (idx=0; idx<param->num_elements; idx++)
    { if (destination[idx].rts.SclkStrg)		/* Clear prev MALLOC */
      { free(destination[idx].rts.SclkStrg);
        destination[idx].rts.SclkStrg = NULL;
      }
      destination[idx].rts.SclkStrg = malloc(strlen(SclkStrg)+1);
      if (!destination[idx].rts.SclkStrg)
      { sprintf(LogMsgBuf,"%s; Error allocating memory for TCL buffer",
                param->name);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return ALLOCATE_ERROR;
      } else strcpy(destination[idx].rts.SclkStrg,SclkStrg);
    }
    return FALSE;
  } else return TRUE;
}

/*****************************************************************************
/*				SET_DATE_VALIDS
/*
/*	Loads the valid range elements of a parameter strucutre for a SCLK
/*  parameter.
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/
int	set_date_valids(
  param_defs_typ	*local,
  char			*value)
{ int	idx,
	r_idx = 0,
	status;
  char	*ptr = value;
  date_time_typ	temp,
		date_range[MAX_RANGE_ENTRIES][2];
  date_time_typ	biggest = {2037,12,31,23,59,59,999,0};

  do
  { status = get_next_date(&ptr,&temp);
    if (status && status != MISSING_VALUE) continue;
    if (*ptr == ':')
    { memcpy(&date_range[r_idx][0],&temp,sizeof(date_time_typ));
      ptr++;
      status = get_next_date(&ptr,&temp);
      if (status)
         memcpy(&temp,&biggest,sizeof(date_time_typ));
      /* check range for correct order */
      memcpy(&date_range[r_idx++][1],&temp,sizeof(date_time_typ));
    } else
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,"%s; Illegal value seperator between values: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    }

    if (*ptr != 0 && *ptr != ',')
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,
              "%s; Illegal value seperator after range definition: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    } else if (*ptr != 0) ptr++;
  } while (strlen(ptr));
  local->max_range = r_idx;

  /***  Allocate valid range for param_def structure, and fill it  ***/
  if (r_idx)
  { local->valid_range[0] = malloc(r_idx*sizeof(date_time_typ));
    if (!local->valid_range[0])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - lower",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }

    local->valid_range[1] = malloc(r_idx*sizeof(date_time_typ));
    if (!local->valid_range[1])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - upper",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<r_idx; idx++)
    { memcpy(&((date_time_typ*)local->valid_range[0])[idx],&date_range[idx][0],
             sizeof(date_time_typ));
      memcpy(&((date_time_typ*)local->valid_range[1])[idx],&date_range[idx][1],
             sizeof(date_time_typ));
    }
  }

  return FALSE;
}

/*****************************************************************************
/*                              SET_DATE_VALUE
/*
/*	Loads the value or default value element of a parameter strucutre for
/*  a date parameter.  If the value element is set, the updated element is
/*  also set to TRUE;
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/
int	set_date_value(
  param_defs_typ	*param,
  char			*buffer,
  int			def_flag)
{
  int	max_idx,
	idx,
	status;
  char	*b_ptr = buffer,
	*DateStrg;
  date_time_typ	*destination,
		value;

  if (def_flag)
  { destination = (date_time_typ *)param->default_value;
    max_idx = 1;
  } else
  { destination = (date_time_typ *)param->value;
    max_idx = param->max_elements;
  }

  for (idx=0; idx<max_idx; idx++)
  { status = get_next_date(&b_ptr,&value);
    if (status && status != MISSING_VALUE) break;
    if (verify_date_value(param,&value))
       memcpy(&destination[idx],&value,sizeof(date_time_typ));
    else
    { /*** invalid date  ***/
      sprintf(LogMsgBuf,"%s; Invalid parameter value: %s",
              param->name,date_to_string(&value));
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      idx--;
    }
    if (status == MISSING_VALUE) break;
    if (*b_ptr == ',') b_ptr++;
  }

  /*** Check for more values in buffer ***/
  status = get_next_date(&b_ptr,&value);
  if (!status)
  { /*** more values defined ***/
    sprintf(LogMsgBuf,"%s; Extra parameter values defined",param->name);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
  }

  /***  Load default value initially, and on error  ***/
  if (def_flag || idx == 0)
  { param->num_elements = idx = 1;
    memcpy((date_time_typ*)param->value,(date_time_typ*)param->default_value,
           sizeof(date_time_typ));
    param->updated = TRUE;
    if (def_flag)
    { param->num_undo_elements = 1;
      memcpy((void *)param->undo_value,(void *)param->default_value,
             sizeof(date_time_typ));
    }
  }

  param->num_elements = idx;
  /***  Allocate and load TCL string buffer  ***/
  if (idx > 0)
  { destination = (date_time_typ *)param->value;
    for (idx=0; idx<param->num_elements; idx++)
    { DateStrg = date_to_string(&((date_time_typ*)param->value)[idx]);
      if (destination[idx].DateStrg)		/* Clear previous MALLOC  */
      { free(destination[idx].DateStrg);
        destination[idx].DateStrg = NULL;
      }
      destination[idx].DateStrg = malloc(strlen(DateStrg)+1);
      if (!destination[idx].DateStrg)
      { sprintf(LogMsgBuf,"%s; Error allocating memory for TCL buffer",
                param->name);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return ALLOCATE_ERROR;
      } else strcpy(destination[idx].DateStrg,DateStrg);
    }

    return FALSE;
  } else return TRUE;
}

/*****************************************************************************
/*				SET_DOUBLE_VALIDS
/*
/*	Loads the valid list and range elements of a parameter strucutre for
/*  a double parameter.
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/

int	set_double_valids(
  param_defs_typ	*local,
  char	*value)
{ int	idx,
	l_idx = 0,
	r_idx = 0,
	status;
  char	*ptr = value;
  double	temp,
		double_list[MAX_LIST_ENTRIES],
		double_range[MAX_RANGE_ENTRIES][2];

  do
  { status = get_next_double(&ptr,&temp);
    if (status && status != MISSING_VALUE) continue;
    if (status == MISSING_VALUE && *ptr == 0) continue;
    if (*ptr == ',' || *ptr == 0)
    { double_list[l_idx++] = temp;
      if (*ptr != 0) ptr++;
    } else if (*ptr == ':')
    { double_range[r_idx][0] = temp;
      ptr++;
      status = get_next_double(&ptr,&temp);
      if (status == MISSING_VALUE)
         double_range[r_idx++][1] = DBL_MAX;
      else if (temp < double_range[r_idx][0])
      { sprintf(LogMsgBuf,"%s; Range parameters reversed (%e : %e) - swapped",
                local->name,double_range[r_idx][0],temp);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
        double_range[r_idx][1] = double_range[r_idx][0];
	double_range[r_idx++][0] = temp;
      } else double_range[r_idx++][1] = temp;
      
      if (*ptr != 0 && *ptr != ',')
      { /*** Illegal value seperator at ptr ***/
        sprintf(LogMsgBuf,
                "%s; Illegal value seperator after range definition: >%c<",
                local->name,*ptr);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return TRUE;
      } else if (*ptr != 0) ptr++;
    } else
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,"%s; Illegal value seperator between values: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    }
  } while (strlen(ptr));
  local->max_list = l_idx;
  local->max_range = r_idx;

  /***  Allocate valid list for param_def structure, and fill it  ***/
  if (l_idx)
  { local->valid_list = malloc(l_idx*sizeof(double));
    if (!local->valid_list)
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid list",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<l_idx; idx++)
        ((double_array_typ)local->valid_list)[idx] = double_list[idx];
  }

  /***  Allocate valid range for param_def structure, and fill it  ***/
  if (r_idx)
  { local->valid_range[0] = malloc(r_idx*sizeof(double));
    if (!local->valid_range[0])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - lower",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }

    local->valid_range[1] = malloc(r_idx*sizeof(double));
    if (!local->valid_range[1])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - upper",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<r_idx; idx++)
    { ((double*)local->valid_range[0])[idx] = double_range[idx][0];
      ((double*)local->valid_range[1])[idx] = double_range[idx][1];
    }
  }

  return FALSE;
}

/*****************************************************************************
/*				SET_DOUBLE_VALUE
/*
/*	This routine parses an input buffer and generates double values.
/*  These values are checked for validity and then assigned in the parameter
/*  structure.  The value can either be assigned to the default value or the
/*  parameter value(s) by setting or resetting the DEFault_FLAG.
/****************************************************************************/
int	set_double_value(
  param_defs_typ	*param,
  char			*buffer,
  int			def_flag)
{
  double	*destination,
		value;
  int	max_idx,
	idx,
	status;
  char	*b_ptr = buffer;

  if (def_flag)
  { destination = (double *)param->default_value;
    max_idx = 1;
  } else
  { destination = (double *)param->value;
    max_idx = param->max_elements;
    param->updated = TRUE;
  }

  for (idx=0; idx<max_idx; idx++)
  { status = get_next_double(&b_ptr,&value);
    if (status) break;
    if (verify_double_value(param,value))
       destination[idx] = value;
    else
    { /*** invalid number  ***/
      sprintf(LogMsgBuf,"%s; Invalid parameter value: %e",param->name,value);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      idx--;
    }
    if (*b_ptr == ',') b_ptr++;
  }

  /*** Check for more values in buffer ***/
  status = get_next_double(&b_ptr,&value);
  if (!status)
  { /*** more values defined ***/
    sprintf(LogMsgBuf,"%s; Extra parameter values defined: %e",
            param->name,value);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
  }

  /***  Load default value initially, and on error  ***/
  if (def_flag || idx == 0)
  { param->num_elements = idx = 1;
    *(double *)param->value = *(double *)param->default_value;
    param->updated = TRUE;
    if (def_flag)
    { param->num_undo_elements = 1;
      *(double *)param->undo_value = *(double *)param->default_value;
    }
  }

  param->num_elements = idx;
  if (idx > 0) return FALSE;
  else return TRUE;
}

/*****************************************************************************
/*				SET_INTEGER_VALIDS
/*
/*	Loads the valid list and range elements of a parameter strucutre for
/*  an integer parameter.
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/

int	set_integer_valids(
  param_defs_typ	*local,
  char	*value)
{ int	idx,
	l_idx = 0,
	r_idx = 0,
	status,
	temp;
  char	*ptr = value;
  int	int_list[MAX_LIST_ENTRIES],
	int_range[MAX_RANGE_ENTRIES][2];

  do
  { status = get_next_int(&ptr,&temp);
    if (status && status != MISSING_VALUE) continue;
    if (status == MISSING_VALUE && *ptr == 0) continue;
    if (*ptr == ',' || *ptr == 0)
    { int_list[l_idx++] = temp;
      if (*ptr != 0) ptr++;
    } else if (*ptr == ':')
    { int_range[r_idx][0] = temp;
      ptr++;
      status = get_next_int(&ptr,&temp);
      if (status == MISSING_VALUE)
         int_range[r_idx++][1] = INT_MAX;
      else if (temp < int_range[r_idx][0])
      { sprintf(LogMsgBuf,"%s; Range parameters reversed (%d : %d) - swapped",
                local->name,int_range[r_idx][0],temp);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
        int_range[r_idx][1] = int_range[r_idx][0];
	int_range[r_idx++][0] = temp;
      } else int_range[r_idx++][1] = temp;
      
      if (*ptr != 0 && *ptr != ',')
      { /*** Illegal value seperator at ptr ***/
        sprintf(LogMsgBuf,
                "%s; Illegal value seperator after range definition: >%c<",
                local->name,*ptr);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return TRUE;
      } else if (*ptr != 0) ptr++;
    } else
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,"%s; Illegal value seperator between values: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    }
  } while (strlen(ptr));
  local->max_list = l_idx;
  local->max_range = r_idx;

  /***  Allocate valid list for param_def structure, and fill it  ***/
  if (l_idx)
  { local->valid_list = malloc(l_idx*sizeof(int));
    if (!local->valid_list)
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid list",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<l_idx; idx++)
        ((int_array_typ)local->valid_list)[idx] = int_list[idx];
  }

  /***  Allocate valid range for param_def structure, and fill it  ***/
  if (r_idx)
  { local->valid_range[0] = malloc(r_idx*sizeof(int));
    if (!local->valid_range[0])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - lower",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }

    local->valid_range[1] = malloc(r_idx*sizeof(int));
    if (!local->valid_range[1])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - upper",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<r_idx; idx++)
    { ((int*)(local->valid_range[0]))[idx] = int_range[idx][0];
      ((int*)(local->valid_range[1]))[idx] = int_range[idx][1];
    }
  }

  return FALSE;
}

/*****************************************************************************
/*				SET_INTEGER_VALUE
/*
/*	This routine parses an input buffer and generates integer values.
/*  These values are checked for validity and then assigned in the parameter
/*  structure.  The value can either be assigned to the default value or the
/*  parameter value(s) by setting or resetting the DEFault_FLAG.
/****************************************************************************/
int	set_integer_value(
  param_defs_typ	*param,
  char			*buffer,
  int			def_flag)
{
  int	*destination,
	value;
  int	max_idx,
	idx,
	status;
  char	*b_ptr = buffer;

  if (def_flag)
  { destination = (int *)param->default_value;
    max_idx = 1;
  } else
  { destination = (int *)param->value;
    max_idx = param->max_elements;
  }

  for (idx=0; idx<max_idx; idx++)
  { status = get_next_int(&b_ptr,&value);
    if (status) break;
    if (verify_integer_value(param,value))
       destination[idx] = value;
    else
    { /*** invalid number  ***/
      sprintf(LogMsgBuf,"%s; Invalid parameter value: %d",param->name,value);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      idx--;
    }
    if (*b_ptr == ',') b_ptr++;
  }

  /*** Check for more values in buffer ***/
  status = get_next_int(&b_ptr,&value);
  if (!status)
  { /*** more values defined ***/
    sprintf(LogMsgBuf,"%s; Extra parameter values defined: %d",
            param->name,value);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
  }

  /***  Load default value initially, and on error  ***/
  if (def_flag || idx == 0)
  { param->num_elements = idx = 1;
    *(int *)param->value = *(int *)param->default_value;
    param->updated = TRUE;
    if (def_flag)
    { param->num_undo_elements = 1;
      *(int *)param->undo_value = *(int *)param->default_value;
    }
  }

  param->num_elements = idx;
  if (idx > 0) return FALSE;
  else return TRUE;
}

/*****************************************************************************
/*				SET_REAL_VALIDS
/*
/*	Loads the valid list and range elements of a parameter strucutre for
/*  a real parameter.
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/

int	set_real_valids(
  param_defs_typ	*local,
  char	*value)
{ int	idx,
	l_idx = 0,
	r_idx = 0,
	status;
  float	temp;
  char	*ptr = value;
  float	real_list[MAX_LIST_ENTRIES],
	real_range[MAX_RANGE_ENTRIES][2];

  do
  { status = get_next_float(&ptr,&temp);
    if (status && status != MISSING_VALUE) continue;
    if (status == MISSING_VALUE && *ptr == 0) continue;
    if (*ptr == ',' || *ptr == 0)
    { real_list[l_idx++] = temp;
      if (*ptr != 0) ptr++;
    } else if (*ptr == ':')
    { real_range[r_idx][0] = temp;
      ptr++;
      status = get_next_float(&ptr,&temp);
      if (status == MISSING_VALUE)
         real_range[r_idx++][1] = FLT_MAX;
      else if (temp < real_range[r_idx][0])
      { sprintf(LogMsgBuf,"%s; Range parameters reversed (%f : %f) - swapped",
                local->name,real_range[r_idx][0],temp);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
        real_range[r_idx][1] = real_range[r_idx][0];
	real_range[r_idx++][0] = temp;
      } else real_range[r_idx++][1] = temp;
      
      if (*ptr != 0 && *ptr != ',')
      { /*** Illegal value seperator at ptr ***/
        sprintf(LogMsgBuf,
                "%s; Illegal value seperator after range definition: >%c<",
                local->name,*ptr);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return TRUE;
      } else if (*ptr != 0) ptr++;
    } else
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,"%s; Illegal value seperator between values: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    }
  } while (strlen(ptr));
  local->max_list = l_idx;
  local->max_range = r_idx;

  /***  Allocate valid list for param_def structure, and fill it  ***/
  if (l_idx)
  { local->valid_list = malloc(l_idx*sizeof(float));
    if (!local->valid_list)
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid list",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<l_idx; idx++)
        ((float_array_typ)local->valid_list)[idx] = real_list[idx];
  }

  /***  Allocate valid range for param_def structure, and fill it  ***/
  if (r_idx)
  { local->valid_range[0] = malloc(r_idx*sizeof(float));
    if (!local->valid_range[0])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - lower",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    local->valid_range[1] = malloc(r_idx*sizeof(float));
    if (!local->valid_range[1])
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid range - upper",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    for (idx=0; idx<r_idx; idx++)
    { ((float*)local->valid_range[0])[idx] = real_range[idx][0];
      ((float*)local->valid_range[1])[idx] = real_range[idx][1];
    }
  }

  return FALSE;
}

/*****************************************************************************
/*				SET_REAL_VALUE
/*
/*	This routine parses an input buffer and generates real values.
/*  These values are checked for validity and then assigned in the parameter
/*  structure.  The value can either be assigned to the default value or the
/*  parameter value(s) by setting or resetting the DEFault_FLAG.
/****************************************************************************/
int	set_real_value(
  param_defs_typ	*param,
  char			*buffer,
  int			def_flag)
{
  float	*destination,
	value;
  int	max_idx,
	idx,
	status;
  char	*b_ptr = buffer;

  if (def_flag)
  { destination = (float *)param->default_value;
    max_idx = 1;
  } else
  { destination = (float *)param->value;
    max_idx = param->max_elements;
  }

  for (idx=0; idx<max_idx; idx++)
  { status = get_next_float(&b_ptr,&value);
    if (status) break;
    if (verify_real_value(param,value))
       destination[idx] = value;
    else
    { /*** invalid number  ***/
      sprintf(LogMsgBuf,"%s; Invalid parameter value: %f",param->name,value);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      idx--;
    }
    if (*b_ptr == ',') b_ptr++;
  }

  /*** Check for more values in buffer ***/
  status = get_next_float(&b_ptr,&value);
  if (!status)
  { /*** more values defined ***/
    sprintf(LogMsgBuf,"%s; Extra parameter values defined: %f",
            param->name,value);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
  }

  /***  Load default value initially, and on error  ***/
  if (def_flag || idx == 0)
  { param->num_elements = idx = 1;
    *(float *)param->value = *(float *)param->default_value;
    param->updated = TRUE;
    if (def_flag)
    { param->num_undo_elements = 1;
      *(float *)param->undo_value = *(float *)param->default_value;
    }
  }

  param->num_elements = idx;
  if (idx > 0) return FALSE;
  else return TRUE;
}

/*****************************************************************************
/*				SET_STRING_VALIDS
/*
/*	Loads the valid list element(s) of a parameter strucutre for a string
/*  parameter.
/*  RETURNS FALSE if there were no errors, TRUE otherwise.
/****************************************************************************/
int	set_string_valids(
  param_defs_typ	*local,
  char			*value)
{ int	idx,
	l_idx = 0,
	status;
  char	*ptr = value,
	temp[MAX_VALUE_LTH],
	string_list[MAX_LIST_ENTRIES][MAX_KEYWORD_LTH];

  do
  { status = get_next_string(&ptr,temp);
    if (status) continue;
    if (*ptr == ',' || *ptr == 0)
    { if ((int)strlen(temp) > (MAX_KEYWORD_LTH-1))
      { sprintf(LogMsgBuf,"%s; String values limited to a %d character length",
                local->name,(MAX_KEYWORD_LTH-1));
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      } else strcpy(string_list[l_idx++],temp);
      if (*ptr != 0) ptr++;
    } else
    { /*** Illegal value seperator at ptr ***/
      sprintf(LogMsgBuf,"%s; Illegal value seperator between values: >%c<",
              local->name,*ptr);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return TRUE;
    }
  } while (strlen(ptr));
  local->max_list = l_idx;

  /***  Allocate valid list for param_def structure, and fill it  ***/
  if (l_idx)
  { local->valid_list = malloc(l_idx*sizeof(char *));
    if (!local->valid_list)
    { sprintf(LogMsgBuf,"%s; Error allocating memory for valid list",
              local->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }

    for (idx=0; idx<l_idx; idx++)
    { ((string_array_typ)local->valid_list)[idx] =
                                malloc(strlen(string_list[idx])+1);
      if (!((string_array_typ)local->valid_list)[idx])
      { sprintf(LogMsgBuf,"%s; Error allocating memory for valid list elements",
              local->name);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return ALLOCATE_ERROR;
      }
      strcpy(((string_array_typ)local->valid_list)[idx],string_list[idx]);
    }
  }

  return FALSE;
}

/*****************************************************************************
/*				SET_STRING_VALUE
/*
/*	This routine parses an input buffer and generates string values.
/*  These values are checked for validity and then assigned in the parameter
/*  structure.  The value can either be assigned to the default value or the
/*  parameter value(s) by setting or resetting the DEFault_FLAG.
/****************************************************************************/
int	set_string_value(
  param_defs_typ	*param,
  char			*buffer,
  int			def_flag)
{
  int	max_idx,
	idx,
	status;
  char	*b_ptr = buffer,
	value[MAX_VALUE_LTH];
  string_array_typ	destination;

  if (def_flag)
  { max_idx = 1;
    destination = (string_array_typ)(&param->default_value);
  } else
  { max_idx = param->max_elements;
    destination = (string_array_typ)param->value;
    param->updated = TRUE;
  }

  for (idx=0; idx<max_idx; idx++)
  { status = get_next_string(&b_ptr,value);
    if (status) break;

    /***  Limit length of string to something reasonable  ***/
    if ((int)strlen(value) > (MAX_STRING_LTH-1))
    { sprintf(LogMsgBuf,"%s; Value truncated from %d to %d characters",
              param->name,strlen(value),(MAX_STRING_LTH-1));
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    }

    if (verify_string_value(param,value))
    { if (destination[idx])
      { free(destination[idx]);
        destination[idx] = 0;
      }
      destination[idx] = malloc(strlen(value)+1);
      if (!destination[idx])
      { sprintf(LogMsgBuf,"%s; Error allocating memory for value element",
                param->name);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return ALLOCATE_ERROR;
      }
      strcpy(destination[idx],value);
    } else
    { /*** invalid string  ***/
      sprintf(LogMsgBuf,"%s; Invalid parameter value: %-.16s",
              param->name,value);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      idx--;
    }
    if (*b_ptr == ',') b_ptr++;
  }

  /*** Check for more values in buffer ***/
  status = get_next_string(&b_ptr,value);
  if (!status)
  { /*** more values defined ***/
    sprintf(LogMsgBuf,"%s; Extra parameter values defined: %-.16s",
            param->name,value);
    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
  }

  /***  Load default value initially, and on error  ***/
  if (def_flag || idx == 0)
  { param->num_elements = idx = 1;
    destination = (string_array_typ)param->value;
    if (destination[0])
    { free(destination[0]);
      destination[0] = NULL;
    }
    destination[0] = malloc(strlen((char *)param->default_value)+1);
    if (!destination[0])
    { destination[0] = NULL;
      sprintf(LogMsgBuf,"%s; Error allocating memory for value",param->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }
    strcpy(destination[0],(char*)param->default_value);
    param->updated = TRUE;
    if (def_flag)
    { param->num_undo_elements = 1;
      destination = (string_array_typ)param->undo_value;
      if (destination[0])
      { free(destination[0]);
        destination[0] = NULL;
      }
      destination[0] = malloc(strlen((char *)param->default_value)+1);
      if (!destination[0])
      { destination[0] = NULL;
        sprintf(LogMsgBuf,"%s; Error allocating memory for undo values",
                param->name);
        rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        return ALLOCATE_ERROR;
      }
      strcpy(destination[0],(char*)param->default_value);
    }
  }

  param->num_elements = idx;
  /***  Allocate and load TCL string buffer  ***/
  if (idx > 0)
  { int	max_lth;

    destination = (string_array_typ)param->value;
    /* Determine size of new strg_buffer */
    max_lth = 0;
    for (idx=0; idx<param->num_elements; idx++)
        max_lth += strlen(destination[idx]);

    /* Clear previous MALLOC  */
    if (param->strg_buffer)
    { free(param->strg_buffer);
      param->strg_buffer = NULL;
    }

    if (max_lth == 0) return FALSE;
    else max_lth += (idx + 4);			/* Included a bit of pad */

    param->strg_buffer = malloc(max_lth);
    if (!param->strg_buffer)
    { sprintf(LogMsgBuf,"%s; Error allocating memory for buffer",param->name);
      rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
      return ALLOCATE_ERROR;
    }

    /* Copy the string values into the new strg_buf */
    memset(param->strg_buffer,0,max_lth);
    for (idx=0; idx<param->num_elements; idx++)
    { strcat(param->strg_buffer,destination[idx]);
      strcat(param->strg_buffer," ");
    }
    /***  Remove last ' '  ***/
    param->strg_buffer[strlen(param->strg_buffer)-1] = 0;
    return FALSE;
  } else return TRUE;
}

/*****************************************************************************
/*				TRANSFER_VALUE
/*
/*	Transfers values between the current value and the undo value.  The
/*  parameters for the routine identify the parameter for value trasnfer and
/*  the direction of the transfer.  The subroutine returns FALSE for a 
/*  successful transfer adn a TRUE for a failure.  The only cause for a
/*  failure is a malloc error.
/****************************************************************************/
int	transfer_values(
  param_defs_typ	*param,
  int			to_undo)
{ int	idx,
	*from_elements,
	*to_elements;
  void	*from_addr,
	*to_addr;

  if (to_undo)
  { from_elements = &param->num_elements;
    to_elements = &param->num_undo_elements;
    from_addr = param->value;
    to_addr = param->undo_value;
  } else
  { to_elements = &param->num_elements;
    from_elements = &param->num_undo_elements;
    to_addr = param->value;
    from_addr = param->undo_value;
  }

  *to_elements = *from_elements;
  switch (param->type)
  { case TYPE_DATE:  memcpy(to_addr,from_addr,sizeof(date_time_typ));
         break;
    case TYPE_CLOCK: memcpy(to_addr,from_addr,sizeof(gen_sclk_typ));
         break;
    case TYPE_INTEGER:
                for (idx=0; idx<*from_elements; idx++)
                    ((int *)to_addr)[idx] = ((int *)from_addr)[idx];
         break;
    case TYPE_REAL: for (idx=0; idx<*from_elements; idx++)
                    ((float *)to_addr)[idx] = ((float *)from_addr)[idx];
         break;
    case TYPE_DOUBLE:
                for (idx=0; idx<*from_elements; idx++)
                    ((double *)to_addr)[idx] = ((double *)from_addr)[idx];
         break;
    case TYPE_STRING:
                for (idx=0; idx<*from_elements; idx++)
                { if (((string_array_typ)to_addr)[idx])
                     free(((string_array_typ)to_addr)[idx]);
                  ((string_array_typ)to_addr)[idx] = NULL;
                  ((string_array_typ)to_addr)[idx] =
                          malloc(strlen(((string_array_typ)from_addr)[idx])+1);
                  if (!((string_array_typ)to_addr)[idx])
                  { rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                               "Error allocating memory for string Xfer");
                    *to_elements=idx;
                    return ( TRUE );
                  }
                  strcpy(((string_array_typ)to_addr)[idx],
                         ((string_array_typ)from_addr)[idx]);
                }
         break;
    default:    rts_logger(RTS_LOG_PARAM,RTS_LOG_WARNING,MODULE_NAME,
                           "Illegal parameter type for transfer");
         break;
  }

  return ( FALSE );
}

/*****************************************************************************
/*				VALID_RANGE
/*
/*	Insures that the value of the first parameter is between the value
/*  of the next two.
/*  RETURNS an integer value between the second and third parameter
/****************************************************************************/
int	valid_range(
  int	value,
  int	lower,
  int	upper)
{
  if (value < lower) return lower;
  if (value >= upper) return upper;
  return value;
}

/*****************************************************************************
/*				VERIFY_ALL_VALUES
/*
/*	This routine compares all parameter values against the valid list and
/*  range of values.  It will return a TRUE if the values are defined by
/*  a list or range.
/****************************************************************************/
int	verify_all_values(
  int	form,
  link_buf_typ	*root,
  char		*variable_name,
  param_defs_typ	**error_param)
{ int	idx,
	status = TRUE;
  param_defs_typ	*param;
  link_buf_typ		*current = root;

  if (!root) return TRUE;

  do
  { param = current->data;

    for (idx=0; idx<param->num_elements; idx++)
    { if (form == -1 || form == param->tcl_form)
      { switch(param->type)
        { case TYPE_INTEGER:
               status = verify_integer_value(param,((int *)param->value)[idx]);
               if (!status)
               { sprintf(LogMsgBuf,"Invalid %s value: %d",
                         param->name,((int *)param->value)[idx]);
                 rts_logger(RTS_LOG_PARAM,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
               }
          break;
          case TYPE_REAL:
               status = verify_real_value(param,((float *)param->value)[idx]);
               if (!status)
               { sprintf(LogMsgBuf,"Invalid %s value: %f",
                         param->name,((float *)param->value)[idx]);
                 rts_logger(RTS_LOG_PARAM,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
               }
          break;
          case TYPE_DOUBLE:
               status = verify_double_value(param,((double *)param->value)[idx]);
               if (!status)
               { sprintf(LogMsgBuf,"Invalid %s value: %e",
                         param->name,((double *)param->value)[idx]);
                 rts_logger(RTS_LOG_PARAM,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
               }
          break;
          case TYPE_STRING:
               status = verify_string_value(param,((char **)param->value)[idx]);
               if (!status)
               { sprintf(LogMsgBuf,"Invalid %s value: %s",
                         param->name,((char **)param->value)[idx]);
                 rts_logger(RTS_LOG_PARAM,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
               }
          break;
          case TYPE_DATE:
               status = verify_date_value(param,
                                        &((date_time_typ *)param->value)[idx]);
               if (!status)
               { sprintf(LogMsgBuf,"Invalid %s value",param->name);
                 rts_logger(RTS_LOG_PARAM,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
               }
          break;
          case TYPE_CLOCK:
               status = verify_clock_value(param,
                                           &((gen_sclk_typ *)param->value)[idx],
                                           param->clock_type);
               if (!status)
               { sprintf(LogMsgBuf,"Invalid %s value",param->name);
                 rts_logger(RTS_LOG_PARAM,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
               }
          break;
          default:
               status = TRUE;
          break;
        }
        if (!status)
        { if (variable_name)
             strcpy(variable_name,param->name);
          if (error_param) *error_param = param;
          return FALSE;
        }
      }
    }

    current = current->next;
  } while (current != root);

  return TRUE;
}

/*****************************************************************************
/*				VERIFY_CLOCK_VALUE
/*
/*	Checks the SCLK value against the valid range of SCLKs in the
/*  parameter strucuture.
/*  RETURNS TRUE if the SCLK is valid, FALSE if there is an error or the SCLK
/*  is not valid.
/****************************************************************************/
int	verify_clock_value(
  param_defs_typ	*param,
  gen_sclk_typ		*value,
  int			clock_type)
{ int	idx;

  switch(clock_type)
  { case CLOCK_GLL:
           if (param->max_range == 0) return TRUE;
           for (idx=0; idx<param->max_range; idx++)
           { if (gll_sclk_valid(&((gen_sclk_typ *)param->valid_range[0])[idx],
                                value,
                                &((gen_sclk_typ *)param->valid_range[1])[idx]))
                return TRUE;
           }
    break;

    case CLOCK_MPF: if (param->max_range == 0) return TRUE;
           for (idx=0; idx<param->max_range; idx++)
           { if (mpf_sclk_valid(&((gen_sclk_typ *)param->valid_range[0])[idx],
                                value,
                                &((gen_sclk_typ *)param->valid_range[1])[idx]))
                return TRUE;
           }
    break;

    case CLOCK_COBT: return TRUE;
    break;

    default:
           rts_logger(RTS_LOG_PARAM,RTS_LOG_ERROR,MODULE_NAME,
                      "Invalid clock type for valid check");
    break;
  }

  return FALSE;
}

/*****************************************************************************
/*				VERIFY_DATE_VALUE
/*
/*	Checks the date value against the valid range of dates in the
/*  parameter strucuture.
/*  RETURNS TRUE if the date is valid, FALSE if there is an error or the date
/*  is not valid.
/****************************************************************************/
int	verify_date_value(
  param_defs_typ	*param,
  date_time_typ		*value)
{ int	idx;
  double	range_seconds,
		value_seconds;

  if (!param->max_range) return TRUE;

  for (idx=0; idx<param->max_range; idx++)
  { range_seconds = date_diff(&((date_time_typ *)param->valid_range[1])[idx],
                              &((date_time_typ *)param->valid_range[0])[idx]);
    value_seconds = date_diff(&((date_time_typ *)param->valid_range[1])[idx],
                              value);
    if (range_seconds >= value_seconds && value_seconds >= 0)
       return TRUE;
    if (range_seconds <= value_seconds && value_seconds <= 0)
       return TRUE;
  }

  return FALSE;
}

/*****************************************************************************
/*				VERIFY_DOUBLE_VALUE
/*
/*	This routine compares a given value against the valid list and range
/*  of values.  It will return a TRUE if the given value is defined by the
/*  list or range.
/****************************************************************************/
int	verify_double_value(
  param_defs_typ	*param,
  double		value)
{ int	idx;

  if (!(param->max_list || param->max_range)) return TRUE;

  for (idx=0; idx<param->max_range; idx++)
  { if (value >= ((double *)param->valid_range[0])[idx] &&
        value <= ((double *)param->valid_range[1])[idx]) return TRUE;
  }

  for (idx=0; idx<param->max_list; idx++)
      if (value == ((double *)param->valid_list)[idx]) return TRUE;


  return FALSE;
}

/*****************************************************************************
/*				VERIFY_INTEGER_VALUE
/*
/*	This routine compares a given value against the valid list and range
/*  of values.  It will return a TRUE if the given value is defined by the
/*  list or range.
/****************************************************************************/
int	verify_integer_value(
  param_defs_typ	*param,
  int			value)
{ int	idx;

  if (!(param->max_list || param->max_range)) return TRUE;

  for (idx=0; idx<param->max_range; idx++)
  { if (value >= ((int *)param->valid_range[0])[idx] &&
        value <= ((int *)param->valid_range[1])[idx]) return TRUE;
  }

  for (idx=0; idx<param->max_list; idx++)
      if (value == ((int *)param->valid_list)[idx]) return TRUE;

  return FALSE;
}

/*****************************************************************************
/*				VERIFY_REAL_VALUE
/*
/*	This routine compares a given value against the valid list and range
/*  of values.  It will return a TRUE if the given value is defined by the
/*  list or range.
/****************************************************************************/
int	verify_real_value(
  param_defs_typ	*param,
  float			value)
{ int	idx;

  if (!(param->max_list || param->max_range)) return TRUE;

  for (idx=0; idx<param->max_range; idx++)
  { if (value >= ((float *)param->valid_range[0])[idx] &&
        value <= ((float *)param->valid_range[1])[idx]) return TRUE;
  }

  for (idx=0; idx<param->max_list; idx++)
      if (value == ((float *)param->valid_list)[idx]) return TRUE;

  return FALSE;
}

/*****************************************************************************
/*				VERIFY_STRING_VALUE
/*
/*	This routine compares a given value against the valid list
/*  of values.  It will return a TRUE if the given value is defined by the
/*  list or range.
/****************************************************************************/
int	verify_string_value(
  param_defs_typ	*param,
  char			*value)
{ int	idx;

  if (!param->max_list) return TRUE;

  for (idx=0; idx<param->max_list; idx++)
      if (!strcmp(((string_array_typ)param->valid_list)[idx],value))
         return TRUE;

  return FALSE;
}

/******************************************************************************
/*				WRITE_PARAM_FILE
/*
/*	Writes a User Parameter File based on the current values stored in
/*  the linke parameter buffer.  This routine formats the file to as close
/*  to an 80 column record length as possible.
/*  RETURNS FALSE if it generates a file (even if nothing is in it), and
/*  a file open error status on a failure.
/*****************************************************************************/

int	write_param_file(
  char	*filename,
  link_buf_typ	*params)
{
  int	status,
	idx,
	pair_lth;
  char	upf_record[MAX_RECORD_LTH+1],
	value_pair[MAX_VALUE_LTH];
  FILE	*UPF;
  link_buf_typ	*l_params = params;
  param_defs_typ	*keyword;

  if (!params)
  { rts_logger(RTS_LOG_PARAM, RTS_LOG_WARNING, MODULE_NAME,
               "Parameter buffer empty");
    return FALSE;
  }

  UPF = fopen(filename, "w");
  if (!UPF)
  { sprintf(LogMsgBuf,"File open error: %s (%s)",
            strerror(errno),filename);
    rts_logger(RTS_LOG_PKTS,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    return ( OPEN_ERROR );
  }

  do
  { keyword = (param_defs_typ *)l_params->data;
    pair_lth = create_value_pair(value_pair, keyword, FALSE);
    for (idx=0; idx<(int)strlen(value_pair); idx+=(MAX_RECORD_LTH-1))
    { if ((int)strlen(&value_pair[idx]) < MAX_RECORD_LTH)
         status = fputs(&value_pair[idx], UPF);
      else
      { memcpy(upf_record,&value_pair[idx],(MAX_RECORD_LTH-1));
        upf_record[(MAX_RECORD_LTH-1)] = '\\';
        upf_record[MAX_RECORD_LTH] = 0;
        status = fputs(upf_record,UPF);
      }
      if (status != EOF) status = fputc('\n',UPF);

      if (status == EOF)
      { sprintf(LogMsgBuf,"File write error: %s (%s)",
                strerror(errno),filename);
        rts_logger(RTS_LOG_PKTS,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
        fputs("! Error writing to file", UPF);
      }
    }

    l_params = (link_buf_typ *)l_params->next;
  } while (l_params != params);

  status = fclose(UPF);
  if (status == EOF)
  { sprintf(LogMsgBuf,"File close error: %s (%s)",
            strerror(errno),filename);
    rts_logger(RTS_LOG_PKTS,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
  }

  return ( FALSE );
}
