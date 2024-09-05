/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "xvmaininc.h"

#if VMS_OS
#define  MULTINET_OLD_STYLE	0
#include <types.h>

#else
#include <stdlib.h>
#endif
#include <string.h>

#include "rts_typedefs.h"
#include "rts_const_defs.h"
#include "rts_sfdu.h"
#include "rts_logging.h"
#include "rts_errors.h"
#include "rts_gdrbyteconv.h"

#define  MODULE_NAME		"SFDU_HDR"

typedef enum	{ NOCHANGE, PUSHED, POPPED, ERROR } state_typ;
typedef	struct	{
		char	ClassId;
		char	*ClassName;
		} sfdu_class_typ;

static char	LogMsgBuf[128];

#undef  MODULE_NAME
#define  MODULE_NAME		"PAD"
/******************************************************************************
 *				process_application_data
 *
 *****************************************************************************/
static	int	process_application_data(
  sfdu_record_typ	*Sfdu)
{ int	idx = 0,
	offset = (SFDU_HDR_LTH + CHDO_HDR_LTH),
	status;

  do
  { status = process_chdo(((char *)Sfdu->Buffer+offset),&Sfdu->Chdo[idx]);
    if (RTS_RTN_BAD(status))
    { rts_logger(RTS_LOG_SFDU,RTS_LOG_TRACE_ERR,MODULE_NAME,
                 "Could not process CHDO");
      return status;
    }
    offset += Sfdu->Chdo[idx].Length + CHDO_HDR_LTH;
    if (offset >= Sfdu->Hdr.Length)
       Sfdu->DataIdx = idx;
    else idx++;
  } while (idx < MAX_CHDO_HDRS && offset < Sfdu->Hdr.Length);

  if (idx >= MAX_CHDO_HDRS)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
               "Too many CHDOs in SFDU");
    return (TOO_MANY_CHDOS);
  }

  return DATA_SFDU;
}

#undef  MODULE_NAME
#define  MODULE_NAME		"PCD"
/******************************************************************************
 *				process_catalog_data
 *
 *****************************************************************************/
static	int	process_catalog_data(
  sfdu_record_typ	*Sfdu)
{ int	lth,
	MsgNum,
	RtnStatus = RTS_NORMAL;
  char	MsgType,
        MsgStr[128],
	*PtrObj,
	*CatalogObject;

  if (Sfdu->Hdr.Length <= 0) return (INFO_SFDU);	/* should not happen */

  if (strcmp(Sfdu->Hdr.DataDesc,"L009"))
  { sprintf(LogMsgBuf,"Unsupported Catalog-Data_Object SFDU: %s",
            Sfdu->Hdr.DataDesc);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
    return (UNSUPPORTED_SFDU);
  }

  /***  Copy into local buffer  ***/
  CatalogObject = malloc(Sfdu->Hdr.Length);
  if (!CatalogObject)
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
               "Could not allocat memory to process Catalog-Object");
    return(ALLOCATE_ERROR);
  }

  memset(CatalogObject,0,Sfdu->Hdr.Length);
  memcpy(CatalogObject,((char *)Sfdu->Buffer+20),(Sfdu->Hdr.Length-20));

  /***  Can we process this 'catalog' SFDU?  ***/
  PtrObj = strstr(CatalogObject,"OBJECT = ");
  if (PtrObj)
  { if (strncmp((PtrObj+9),"Status Message",14) != 0 &&
        strncmp((PtrObj+9),"Error Message",13) != 0)
    {
      return (INFO_SFDU);
    }
  }

  PtrObj = strstr(CatalogObject,"MESSAGE_TYPE = ");
  if (PtrObj) MsgType = *(PtrObj+15);
  else MsgType = '?';

  PtrObj = strstr(CatalogObject,"MESSAGE_NUM = ");
  if (PtrObj) MsgNum = atoi((PtrObj+13));
  else MsgNum = 0;

  PtrObj = strstr(CatalogObject,"MESSAGE = ");
  if (PtrObj)
  { lth = strcspn((PtrObj+10),";\n\r");
    if (lth > sizeof(MsgStr)) lth = sizeof(MsgStr) - 1;
    memset(MsgStr,0,sizeof(MsgStr));
    memmove(MsgStr,(PtrObj+10),lth);
  } else strcat(MsgStr,"Can not extract message");

  sprintf(LogMsgBuf,"(%c, %d) - %s",MsgType,MsgNum,MsgStr);
  if (MsgType != 'S')
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,LogMsgBuf);
    RtnStatus = ERROR_SFDU;
  } else
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_INFO,MODULE_NAME,LogMsgBuf);
    RtnStatus = INFO_SFDU;
  }

  free(CatalogObject);
  return (RtnStatus);
}

#undef  MODULE_NAME
#define  MODULE_NAME		"PC"
/******************************************************************************
 *				PROCESS_CHDO
 *
 *****************************************************************************/
int	process_chdo(
  Ubyte		*ptr,
  chdo_hdr_typ	*chdo)
{
  chdo->Type = C16_TO_U16(ptr);
  chdo->Length = C16_TO_U16(ptr+2);

  sprintf(LogMsgBuf,"CHDO: %d  Length: %d",chdo->Type,chdo->Length);
  rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG5,MODULE_NAME,LogMsgBuf);

  switch(chdo->Type)
  {
    case 2: chdo->Data.chdo_002 = (void *)malloc(sizeof(chdo_002_typ));
            if (!chdo->Data.chdo_002)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 2 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_002, 0, sizeof(chdo_002_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 2: %08X",chdo->Data.chdo_002);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_002(chdo->Data.chdo_002,ptr));
    break;

    case 10: chdo->Data.chdo_010 = (void *)malloc(sizeof(chdo_010_typ));
            if (!chdo->Data.chdo_010)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 10 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_010, 0, sizeof(chdo_010_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 10: %08X",chdo->Data.chdo_010);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_010(chdo->Data.chdo_010,ptr));
    break;

    case 62: chdo->Data.chdo_062 = (void *)malloc(sizeof(chdo_062_typ));
            if (!chdo->Data.chdo_062)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 62 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_062, 0, sizeof(chdo_062_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 62: %08X",chdo->Data.chdo_062);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_062(chdo->Data.chdo_062,ptr));
    break;

    case 63: chdo->Data.chdo_063 = (void *)malloc(sizeof(chdo_063_typ));
            if (!chdo->Data.chdo_063)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 63 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_063, 0, sizeof(chdo_063_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 63: %08X",chdo->Data.chdo_063);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_063(chdo->Data.chdo_063,ptr));
    break;

    case 81: chdo->Data.chdo_081 = (void *)malloc(sizeof(chdo_081_typ));
            if (!chdo->Data.chdo_081)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 81 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_081, 0, sizeof(chdo_081_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 81: %08X",chdo->Data.chdo_081);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_081(chdo->Data.chdo_081,ptr));
    break;

    case 82: chdo->Data.chdo_082 = (void *)malloc(sizeof(chdo_082_typ));
            if (!chdo->Data.chdo_082)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 82 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_082, 0, sizeof(chdo_082_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 82: %08X",chdo->Data.chdo_082);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_082(chdo->Data.chdo_082,ptr));
    break;

    case 128: chdo->Data.chdo_128 = (void *)malloc(sizeof(chdo_128_typ));
            if (!chdo->Data.chdo_128)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 128 structure");
              return (ALLOCATE_ERROR);
            }
            memset(chdo->Data.chdo_128, 0, sizeof(chdo_128_typ));
            sprintf(LogMsgBuf,"Allocating CHDO 128: %08X",chdo->Data.chdo_128);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            return (load_chdo_128(chdo->Data.chdo_128,ptr));
    break;

    default: chdo->Data.buffer = (void *)malloc((chdo->Length + 4));
            if (!chdo->Data.buffer)
            { rts_logger(RTS_LOG_SFDU,RTS_LOG_ERROR,MODULE_NAME,
                         "Could not allocate memory for CHDO 128 structure");
              return (ALLOCATE_ERROR);
            }
            sprintf(LogMsgBuf,"Allocating CHDO %03d: %08X",
                    chdo->Type,chdo->Data.buffer);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG7,MODULE_NAME,LogMsgBuf);
            memcpy(chdo->Data.buffer,ptr,(chdo->Length + 4));
            
            sprintf(LogMsgBuf,"Unsupported CHDO: %03d",chdo->Type);
            rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
            return (UNSUPPORTED_CHDO);
    break;
  }
}

#undef  MODULE_NAME
#define  MODULE_NAME		"PS"
/******************************************************************************
 *				PROCESS_SFDU
 *
 *****************************************************************************/
int	process_sfdu(
  sfdu_record_typ	*Sfdu)
{ int	idx,
	ClassIdx;
  sfdu_class_typ	ExpandClass[12] = {
		{ 'Z', "Exchange-Unit"},
		{ 'U', "Application-Unit"},
		{ 'F', "Description-Unit"},
		{ 'R', "Replacement-Service"},
		{ 'C', "Admin-Service"},
		{ 'I', "Application-Object"},
		{ 'S', "Supplement-Object"},
		{ 'D', "DDR-Object"},
		{ 'E', "DED-Object"},
		{ 'K', "Catalog-Object"},
		{ 'V', "Volume-Object"},
		{   0, "UnKnown" }
	};
  state_typ	StackState;
  static int	StackIdx = -1;
  static char	MarkerStack[MAX_SFDU_STACK][10];

  StackState = NOCHANGE;
  for (ClassIdx=0; ExpandClass[ClassIdx].ClassId; ClassIdx++)
      if (Sfdu->Hdr.ClassId == ExpandClass[ClassIdx].ClassId) break;

  if (Sfdu->Hdr.Version == '3')
  { sprintf(LogMsgBuf,"Processing %s SFDU: %-20.20s",
            ExpandClass[ClassIdx].ClassName,Sfdu->Buffer);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG2,MODULE_NAME,LogMsgBuf);

    switch (Sfdu->Hdr.Delimiter)
    { case 'A': sprintf(LogMsgBuf,"ASCII length SFDU (%d)",Sfdu->Hdr.Length);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
      break;

      case 'B': sprintf(LogMsgBuf,"BINARY length SFDU (%d)",Sfdu->Hdr.Length);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
      break;

      case 'C': rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,
                           "'Contiguous End-Of-File' SFDU reached");
      break;

      case 'E': rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,
                           "'Sequential End-Of-File' SFDU reached");
                if (StackIdx < 0)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,
                             "Could not pop SFDU marker stack, already at top");
                  StackState = ERROR;
                } else if (strcmp(Sfdu->Hdr.Marker,MarkerStack[StackIdx]) == 0)
                { StackState = POPPED;
                  StackIdx--;
                } else
                { sprintf(LogMsgBuf,"SFDU Marker pop-stack mismatch %s vs %s",
                          Sfdu->Hdr.Marker,MarkerStack[StackIdx]);
                  rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
                  StackState = ERROR;
               }
      break;

      case 'F': rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,
                           "'Shared End-Of-File' SFDU reached");
                if (StackIdx < 0)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,
                             "Could not pop SFDU marker stack, already at top");
                  StackState = ERROR;
                } else if (strcmp(Sfdu->Hdr.Marker,MarkerStack[StackIdx]) == 0 ||
                           strcmp(Sfdu->Hdr.Marker,MarkerStack[0]) == 0)
                { StackState = POPPED;
                  StackIdx--;
                  if (strcmp(Sfdu->Hdr.Marker,MarkerStack[0]) == 0)
                     StackIdx = -1;
                } else
                { sprintf(LogMsgBuf,"SFDU Marker pop-stack mismatch %s vs %s",
                          Sfdu->Hdr.Marker,MarkerStack[StackIdx]);
                  rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
                  StackState = ERROR;
               }
      break;

      case 'S': sprintf(LogMsgBuf,"Marker Pattern SFDU (%s)",Sfdu->Hdr.Marker);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);

                if (++StackIdx >= MAX_SFDU_STACK)
                { rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,
                             "Could not push SFDU marker stack, already at bottom");
                  StackState = ERROR;
                } else
                { strcpy(MarkerStack[StackIdx],Sfdu->Hdr.Marker);
                  StackState = PUSHED;
                }
      break;

      default: sprintf(LogMsgBuf,"Unknown SFDU delimitation ID: %c",
                       Sfdu->Hdr.Delimiter);
                rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
      break;
    }
  } else
  { sprintf(LogMsgBuf,"Processing %s SFDU: %-12.12s",
            ExpandClass[ClassIdx].ClassName,Sfdu->Buffer);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG2,MODULE_NAME,LogMsgBuf);
  }

  if (Sfdu->Hdr.Delimiter == 'S')
  { if (StackState == PUSHED)
    { sprintf(LogMsgBuf,"Pushed SFDU marker '%s' on stack",
             MarkerStack[StackIdx]);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
    }
  }

  if (Sfdu->Hdr.Delimiter == 'E' || Sfdu->Hdr.Delimiter == 'F')
  { if (StackState == POPPED)
    { sprintf(LogMsgBuf,"Popped SFDU marker '%s' off stack",
              MarkerStack[StackIdx+1]);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
    }
    if (StackIdx == -1) return (EOD);
  }

  if (Sfdu->Hdr.ClassId == 'Z' || Sfdu->Hdr.ClassId == 'U' ||
      Sfdu->Hdr.ClassId == 'F')
  { rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG2,MODULE_NAME,
               "'Structure' class SFDU processed");
    if (StackState != PUSHED)
    { sprintf(LogMsgBuf,"Could not push %s SFDU Marker on stack",
              ExpandClass[ClassIdx].ClassName);
      rts_logger(RTS_LOG_SFDU,RTS_LOG_WARNING,MODULE_NAME,LogMsgBuf);
    }
  }


  if (strncmp(Sfdu->Hdr.CntrlAuth,"NJPL",4) == 0)
  { sprintf(LogMsgBuf,"Processing NJPL '%c' (%02X) SFDU",
            Sfdu->Hdr.ClassId,Sfdu->Hdr.ClassId);
    rts_logger(RTS_LOG_SFDU,RTS_LOG_DEBUG4,MODULE_NAME,LogMsgBuf);
    switch (Sfdu->Hdr.ClassId)
    { case 'I': Sfdu->ChdoCode = C16_TO_U16((char *)Sfdu->Buffer+20);
                Sfdu->ChdoHdrLth = C16_TO_U16((char *)Sfdu->Buffer+22);
                return (process_application_data(Sfdu));
      break;

      case 'K': return (process_catalog_data(Sfdu));
      break;

      default:
      break;
    }
  }

  return (RTS_NORMAL);
}
