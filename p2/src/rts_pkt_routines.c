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
