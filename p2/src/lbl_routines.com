$!****************************************************************************
$!
$! Build proc for MIPL module lbl_routines
$! VPACK Version 1.9, Tuesday, November 13, 2007, 06:41:52
$!
$! Execute by entering:		$ @lbl_routines
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
$ write sys$output "*** module lbl_routines ***"
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
$ write sys$output "Invalid argument given to lbl_routines.com file -- ", primary
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
$   if F$SEARCH("lbl_routines.imake") .nes. ""
$   then
$      vimake lbl_routines
$      purge lbl_routines.bld
$   else
$      if F$SEARCH("lbl_routines.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lbl_routines
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lbl_routines.bld "STD"
$   else
$      @lbl_routines.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lbl_routines.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lbl_routines.com -mixed -
	-s lbl_gen_api.c lbl_camera_model.c lbl_command.c lbl_compression.c -
	   lbl_derived_geometry.c lbl_identification.c lbl_image_geometry.c -
	   lbl_image_data.c lbl_instrument_state.c lbl_telemetry.c -
	   lbl_surface_model.c lbl_surface_projection.c lbl_articulation.c -
	   lbl_coordinate.c lbl_ground_support.c lbl_derived_image.c -
	   lbl_pdshistory.c -
	-i lbl_routines.imake -
	-t tst_lbl_gen_api.c tst_lbl_gen_api.imake tst_lbl_routines.c -
	   tst_lbl_routines.imake test_lbl_routines.txt
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lbl_gen_api.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "return_status.h"
#include "lbl_gen_api.h"

#define USE_NEW_VERSIONS	1

static char	ErrorBuffer[256];
static int	SelectedVersion = LBL_KEYWORD_VERSION;

/******************************************************************************
 *				LBL_PROCESSOR
 *
 *	This routine uses the structures in the lbl_gen_api.h that define a
 *  label and how to process it.
 *
 *	The control structure (LblApiCntrl_typ) defines:
 *		VICAR file unit number
 *		Instance of the property or history label
 *		The return status
 *		The error count and error message, if any
 *		Processing flags: Proceed-On-Error and Obtain/Write
 *
 *	The label structure (LblApiProcess_typ) defines :
 *		The list of label elements in the label
 *		The type of label (property, history, etc.)
 *		The keyword of the label type (PROPERTY, HISTORY, SYSTEM)
 *		The value of the label type
 *		The address of the buffer to receive/send the label values
 *
 *	The label structure LblApiProcess_typ points to a table that
 *  defines all of the label elements.  This structure (LblApiElement_typ)
 *  defines:
 *		The keyword list (versioning)
 *		The format of the label item
 *		A Required flag
 *		A Continuation flag (part of previous item)
 *		Maximum values for this label item
 *		The value element number
 *		The keyword version used to define this value (debugging only)
 *		The byte offset to the address of the value
 *		The byte offset to the address of the valid flag
 *		The byte offset to the address of the num of elements returned
 *		The storage size available to the value (development/debugging)
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/
int	LblProcessor(
  LblApiCntrl_typ	*Cntrl,
  LblApiProcess_typ	*Label)
{ int	lc,
	status = 0,
        delItems,
	BadContinueLink = LBL_TRUE,	/* Continued label flagged as invalid */
					/* First label can not be continued   */
	Element,
	Idx,
	Items,
	Instance,
	MaxLength,
	MaxElements,
	RtnStatus = RTN_NORMAL,
	StringWriteFlag,
	VersionIdx,
	*ValidId;			/* Validity indicator of label value */
  char	*Keyword,			/* Keyword of label item             */
	ContinuationKeyword[LBL_KEYWORD_LTH],
	PrimaryKeyword[LBL_KEYWORD_LTH],
	LabelFormat[24],
	*PdsCheck,
	StringValue[LBL_MAX_STRING],
	*Buffer = Label->Buffer,
  	KeywordList[LBL_KEYWORDLIST_LTH];
  char	*KeywordVersions[LBL_KEYWORD_VERSION],
	KeywordNames[LBL_KEYWORD_VERSION][LBL_KEYWORD_LTH];
  LblApiElement_typ	*Fields = Label->Table;

  Cntrl->ErrorCount = 0;
  ErrorBuffer[0] = 0;

  Instance = (Cntrl->Instance < 1) ? 1 : Cntrl->Instance;

  if (Cntrl->Obtain==LBL_READ)
     memset(Label->Buffer,0,Label->BufferSize);
  else			/* Check for writing required labels */
  { for (Items=0; Fields[Items].KeywordList; Items++)
        if (Fields[Items].Required && !(int*)(Buffer+Fields[Items].ValidOffset))
    { RtnStatus = RTN_LBL_MISSING_REQ;

      /* Display error message only for LBL_WRITE */
      if (Cntrl->Obtain==LBL_WRITE) {
         sprintf(Cntrl->ErrMsgBuf,"Error missing label required: %-.200s",
                 Fields[Items].KeywordList);
         strcpy(ErrorBuffer,Cntrl->ErrMsgBuf);
      }
      if (!Cntrl->ProceedOnError) return RtnStatus;
    }
  }

  for (Items=0; Fields[Items].KeywordList; Items++)
  { /***  Check for label element continuation.  If a label item has
     ***  multiple values, it can be referenced one element at a time.
     ***  Continuation elements must use the same keyword, and must not
     ***  have gaps in the valid list (i.e., 1, 2, 4, 5).
     ***/
    ValidId = (int*)(Buffer+Fields[Items].ValidOffset);
    if (Fields[Items].Continuation)
    { if (Cntrl->Obtain==LBL_READ)
      { if (BadContinueLink)
        { /* Previous label 'obtain' must be successful for a continuation */
          *ValidId = LBL_INVALID;
          continue;
        }
      } else
      { /* Previous label 'put' and/or current item must be valid */
        if (!(*ValidId))
           BadContinueLink = LBL_TRUE;
        if (BadContinueLink) continue;
      }
    } else /* load up keyword(s) for a new item */
    { strncpy(KeywordList,Fields[Items].KeywordList,(LBL_KEYWORDLIST_LTH-1));
      KeywordList[LBL_KEYWORDLIST_LTH-1] = 0;
      Keyword = strtok(KeywordList,LBL_KEYWORD_DELIMITERS);

      /* Build a Keyword Generations table that points */
      /* to the valid keyword for each version.        */
      if (!Keyword)		/* There has to be atleast one */
      { *ValidId = LBL_INVALID;
        continue;
      } else Idx = -1;
      for (lc=0; lc<LBL_KEYWORD_VERSION; lc++)
      { if (Keyword && strcmp(Keyword,"-"))	/* Found a 'new' keyword */
           strcpy(KeywordNames[++Idx],Keyword);
        KeywordVersions[lc] = KeywordNames[Idx];
        Keyword = strtok(NULL,LBL_KEYWORD_DELIMITERS);
      }
      Keyword = KeywordVersions[VersionIdx=(SelectedVersion-1)];
      strcpy(PrimaryKeyword,Keyword);	 /* for error messages */
      strcpy(ContinuationKeyword,Keyword);
      if (Fields[Items].KeywordUsed)
         Fields[Items].KeywordUsed[0] = 0;  /* in case one is not found */
      StringWriteFlag = 0;		/* reset flag */
      BadContinueLink = LBL_FALSE;
    }

    status = 0;				/* clear status value */
    do	/* for all keywords in list */
      if (Cntrl->Obtain==LBL_READ)
    /********************************************************************
     ********************************************************************
     ***
     ***  Read the label items from the file
     ***
     ********************************************************************
     *******************************************************************/
    { Keyword = KeywordVersions[VersionIdx];

      if (!Fields[Items].Continuation)		/* 1st value for keyword */
      { if (VersionIdx+1 == SelectedVersion)	/* 1st time thru loop */
        { /*** Verify compatibility of label first  ***/
          status = zlinfo(Cntrl->FileUnit, Label->Type, Keyword,
			LabelFormat, &MaxLength, &MaxElements,
			"INSTANCE",	Instance,
			Label->NameKeyword,	Label->NameValue, 0);
          *ValidId = LBL_INVALID;

          if (status != 1)		/* Can there be any error recorvery ? */
          { BadContinueLink = LBL_TRUE;
            continue;
          }

          if (MaxLength > LBL_MAX_STRING)  /* String larger than buffer */
          { BadContinueLink = LBL_TRUE;
            continue;
          }
        } else
        { /* Check to see if the last keyword is same as current .... */
          /* ... if last one failed, this one should too, so skip it. */
          if (strcmp(Keyword,KeywordVersions[VersionIdx+1]) == 0)
              continue;
        }
        strcpy(ContinuationKeyword,Keyword);
      } else	/* Insure multi-value items use the same keyword version */
      { VersionIdx = 0;
        Keyword = ContinuationKeyword;
      }

      /*** Verify existance of elements being read ***/
      if ((Fields[Items].Element+Fields[Items].MaxElements-1) > MaxElements)
      { BadContinueLink = LBL_TRUE;
        continue;
      }

      /***  Check all string labels, element by element, to see  ***/
      /***  if they could be a PDS: NULL, UNKN, or N/A           ***/
      if (strcmp(LabelFormat,"STRING") == 0)
         for (Element=0; Element<Fields[Items].MaxElements; Element++)
      { status = zlget(Cntrl->FileUnit, Label->Type,
                      Keyword,        StringValue,
                      "FORMAT",       LabelFormat,
                      "INSTANCE",     Instance,
                      "ELEMENT",      Fields[Items].Element+Element,
                      "NELEMENT",     1,
                      "ULEN",         (MaxLength+1),
                      Label->NameKeyword,     Label->NameValue, 0);
        if (status != 1)		/* Can there be any error recorvery ? */
        { BadContinueLink = LBL_TRUE;
          continue;
        }

        /***  An expected numeric label element has a simpler check  ***/
        /***  than a string, so branch for efficency (?)             ***/
        if (strcmp(Fields[Items].Format,LabelFormat) != 0)	/** Numeric **/
           switch (StringValue[1])
        { case '/': *ValidId = LBL_PDS_NA;
               break;
          case 'N': *ValidId = LBL_PDS_UNK;
               break;
          case 'U': *ValidId = LBL_PDS_NULL;
               break;
          default: *ValidId = LBL_VALID;
                   switch(Fields[Items].Format[0])
                   { case 'I': *((int*)(Buffer+Fields[Items].ValueOffset)) =
                               atoi(StringValue);
                          break;

                     case 'R': *((float*)(Buffer+Fields[Items].ValueOffset)) =
                               atof(StringValue);
                          break;

                     case 'D': scanf(StringValue,"%e",
                                     (double*)(Buffer+Fields[Items].ValueOffset)); 
                          break;

                     default: *ValidId = LBL_INVALID;
                              BadContinueLink = LBL_TRUE;
                          break;
                    }
               break;
        } else							/** String **/
        { strcpy((Buffer+Fields[Items].ValueOffset),StringValue);
          if (strlen(StringValue) != 3 && strlen(StringValue) != 4)
             *ValidId = LBL_VALID;
          else if (strcmp(StringValue,LBL_PDS_STRING_NULL) == 0)
             *ValidId = LBL_PDS_NULL;
          else if (strcmp(StringValue,LBL_PDS_STRING_NA) == 0)
             *ValidId = LBL_PDS_NA;
          else if (strcmp(StringValue,LBL_PDS_STRING_UNK) == 0)
             *ValidId = LBL_PDS_UNK;
          else *ValidId = LBL_VALID;
        }
      } else				/*  Label is only numeric  */
      { /***  Return the number of values associated with a label item if  ***/
        /***  the label element has defined a location to place the value  ***/
        if (Fields[Items].RtnElementOffset == LBL_NO_RETURN)
           status = zlget(Cntrl->FileUnit, Label->Type,
			Keyword,	(Buffer+Fields[Items].ValueOffset),
			"FORMAT",	Fields[Items].Format,
			"INSTANCE",	Instance,
			"ELEMENT",	Fields[Items].Element,
			"NELEMENT",	Fields[Items].MaxElements,
			Label->NameKeyword,	Label->NameValue, 0);
        else status = zlget(Cntrl->FileUnit, Label->Type,
			Keyword,	(Buffer+Fields[Items].ValueOffset),
			"FORMAT",	Fields[Items].Format,
			"INSTANCE",	Instance,
			"ELEMENT",	Fields[Items].Element,
			"NELEMENT",	Fields[Items].MaxElements,
			"NRET",		(Buffer+Fields[Items].RtnElementOffset),
			Label->NameKeyword,	Label->NameValue, 0);

        if (status == 1)
        { *(int*)(Buffer+Fields[Items].ValidOffset) = LBL_VALID;
          BadContinueLink = LBL_FALSE;
        } else BadContinueLink = LBL_TRUE;
      }
    } else if (LBL_CHK_PRESENT(*ValidId))
    /********************************************************************
     ********************************************************************
     ***
     ***  Write the label items to the file
     ***
     ********************************************************************
     *******************************************************************/
    { if (LBL_CHK_VALID(*ValidId))
      { if (StringWriteFlag)		/* Previous element is NULL/NA/UNK */
        { switch(Fields[Items].Format[0])
          { case 'S': strcpy(StringValue,(char*)(Buffer+Fields[Items].ValueOffset));
                 break;
            case 'I': sprintf(StringValue,"%d",
                              *((int*)(Buffer+Fields[Items].ValueOffset)));
                 break;
            case 'R': sprintf(StringValue,"%f",
                              *((float*)(Buffer+Fields[Items].ValueOffset)));
                 break;
            case 'D': sprintf(StringValue,"%e",
                              *((double*)(Buffer+Fields[Items].ValueOffset)));
                 break;
            default: strcpy(StringValue,LBL_PDS_STRING_UNK);
                 break;
          }
          /** may need to handle deleting some elements here as well as below **/
          status = zladd(Cntrl->FileUnit, Label->Type,
                         Keyword,        StringValue,
                         "FORMAT",       "STRING",
                         "INSTANCE",     Instance,
                         "NELEMENT",     1,
                         "ELEMENT",      Fields[Items].Element,
                         "MODE",         "REPLACE",
                         Label->NameKeyword,     Label->NameValue, 0);
        } else 
        {
          status = zlinfo(Cntrl->FileUnit, Label->Type, Keyword,
                          LabelFormat, &MaxLength, &MaxElements,
                          "INSTANCE",    Instance,
                          Label->NameKeyword,    Label->NameValue, 0);

          /** delete the existing keyword first before adding **/
          if ((Fields[Items].MaxElements<MaxElements) && (Fields[Items].Element==1)) 
          { status = zldel(Cntrl->FileUnit, Label->Type, Keyword,
                           "INSTANCE",  Instance,
                           Label->NameKeyword,  Label->NameValue, 0);
          }

          status = zladd(Cntrl->FileUnit, Label->Type,
                        Keyword,        (Buffer+Fields[Items].ValueOffset),
                        "FORMAT",       Fields[Items].Format,
			"INSTANCE",	Instance,
			"NELEMENT",	Fields[Items].MaxElements,
			"ELEMENT",	Fields[Items].Element,
			"MODE",		"REPLACE",
			Label->NameKeyword,	Label->NameValue, 0);
        }
      } else if (*ValidId == LBL_DELETE) 
      {
        status = zlinfo(Cntrl->FileUnit, Label->Type, Keyword,
                        LabelFormat, &MaxLength, &MaxElements,
                        "INSTANCE",    Instance,
                        Label->NameKeyword,    Label->NameValue, 0);

        status = zldel(Cntrl->FileUnit, Label->Type, Keyword,
                       "INSTANCE",  Instance,
                       "ELEMENT", Fields[Items].Element,
                       Label->NameKeyword,  Label->NameValue, 0);

        /*** If a label item has multiple values and the deleted element above is in the 
         *** middle of the list, delete rest of the elements since the label item must   
         *** not have gaps.  
         ***/
        if (MaxElements > Fields[Items].Element) 
        { for (delItems=Fields[Items].Element+1; delItems<=MaxElements; delItems++) 
            status = zldel(Cntrl->FileUnit, Label->Type, Keyword,
                           "INSTANCE",       Instance,
                           "ELEMENT", delItems,
                           Label->NameKeyword,       Label->NameValue, 0);
        }          
      } else
      { StringWriteFlag = 1;	/* Indicates remainging elements are strings */ 
        for (Element=0; Element<Fields[Items].MaxElements; Element++)
            switch (*ValidId)
        { case LBL_PDS_NULL:
               status = zladd(Cntrl->FileUnit, Label->Type,
                              Keyword,        LBL_PDS_STRING_NULL,
                              "FORMAT",       "STRING",
                              "INSTANCE",     Instance,
                              "NELEMENT",     1,
                              "ELEMENT",      Fields[Items].Element+Element,
                              "MODE",         "REPLACE",
                              Label->NameKeyword,     Label->NameValue, 0);
               break;

          case LBL_PDS_UNK:
               status = zladd(Cntrl->FileUnit, Label->Type,
                              Keyword,        LBL_PDS_STRING_UNK,
                              "FORMAT",       "STRING",
                              "INSTANCE",     Instance,
                              "NELEMENT",     1,
                              "ELEMENT",      Fields[Items].Element+Element,
                              "MODE",         "REPLACE",
                              Label->NameKeyword,     Label->NameValue, 0);
               break;

          case LBL_PDS_NA:
               status = zladd(Cntrl->FileUnit, Label->Type,
                              Keyword,        LBL_PDS_STRING_NA,
                              "FORMAT",       "STRING",
                              "INSTANCE",     Instance,
                              "NELEMENT",     1,
                              "ELEMENT",      Fields[Items].Element+Element,
                              "MODE",         "REPLACE",
                              Label->NameKeyword,     Label->NameValue, 0);
               break;

          default:
               sprintf(Cntrl->ErrMsgBuf,"Errorenous validity option (%d): %s",
                       (*ValidId),Keyword);
               status = RTN_LBL_UNSUPPORTED;
               break;
        }
      }

      if (status != 1) BadContinueLink = LBL_TRUE;
      break;				/* Only 1st keyword used for writes */
    } else	/***  Label value not supplied for writing this keyword  ***/
    { if (Fields[Items].Continuation)
         BadContinueLink = LBL_TRUE;	/* Subsequent Continues won't work */
      break;				/* Only 1st keyword used for writes */
    } while (status != 1 && --VersionIdx >= 0);	/* continue with keyword list */

    /****  probably should rethink error processing response  ***/

    if (status != 1)			 /* Label item error processing */
    { if ((Cntrl->Obtain == LBL_READ) || /* ANY read operation  */
          (Cntrl->Obtain == LBL_WRITE && /* Only if Required or error on write */
           (status != 0 || Fields[Items].Required)))
      { Cntrl->ErrorCount++;
        if (RtnStatus != RTN_LBL_MISSING_REQ)	/* Check for previous error */
        { if (Fields[Items].Required)
          { RtnStatus = RTN_LBL_MISSING_REQ;
            sprintf(Cntrl->ErrMsgBuf,"Error (%d) %s required label: %s",
                    status,(Cntrl->Obtain ? "reading" : "writing"),
                    PrimaryKeyword);
          } else if (RtnStatus == RTN_NORMAL)
          { RtnStatus = RTN_LBL_MISSING_OPT;
            sprintf(Cntrl->ErrMsgBuf,"Error (%d) processing label (%d): %s",
                    status,Items,PrimaryKeyword);
          }
          strcpy(ErrorBuffer,Cntrl->ErrMsgBuf);
          if (!Cntrl->ProceedOnError) return RtnStatus;
        }
      }
    } else
    { if (Fields[Items].KeywordUsed)
         strcpy(Fields[Items].KeywordUsed,Keyword);
    }
  }

  return RtnStatus;
}

/*******************************************************************************
 *				LBL_SET_VERSION
 *
 *	Sets the version of the keyword to be used
 ******************************************************************************/
int	LblSetVersion(
  const char	*Version )
{
  int	lc;
  char	*Name,
	NameList[1024];
  const char	*VersionNames[LBL_KEYWORD_VERSION] =
		{ LBL_VERSION_ONE, LBL_VERSION_TWO };

  for (lc=(LBL_KEYWORD_VERSION-1); lc>=0; lc--)
  { strcpy(NameList,VersionNames[lc]);
    Name = strtok(NameList,LBL_KEYWORD_DELIMITERS);
    do
    { if (strcmp(Name,Version) == 0)
      { SelectedVersion = lc + 1;
        return RTN_NORMAL;
      }
      Name = strtok(NULL,LBL_KEYWORD_DELIMITERS);
    } while (Name);
  }

  return RTN_INVLD_ARG;
}

/*******************************************************************************
 *				LBL_ERROR_MESSAGE
 *
 *	Returns an error message from the generic label API
 ******************************************************************************/
const char	*LblErrorMessage( void )
{
  return (ErrorBuffer);
}

/*******************************************************************************
 *				PRINT_LABEL_ELEMENTS
 *
 *	Routine is basically used for testing and debugging the implementation
 * of a label API and not for use by indivifual applications.
 ******************************************************************************/
void	PrintLabelElements(
  LblApiProcess_typ	*Label)
{ int	lc,
	lc_lf,
	ValueIdx,
	*ValidId,
	status,
	MaxLength,
	MaxElements;
  char	*Keyword,
	*Buffer = Label->Buffer,
  	KeywordList[LBL_KEYWORDLIST_LTH];
  LblApiElement_typ	*Fields = Label->Table;

  char	*LabelFormat[] = { "INT", "REAL", "DOUB", "STRING", "" };

  printf("\n\nLabel Type: %s\n",Label->Type);
  printf("%s Name: %s\n",Label->NameKeyword,Label->NameValue);


  for (lc=0; Fields[lc].KeywordList; lc++)
  {
    /***  Print out General info about the label item  ***/
    /***  Keyword, Format, max elements, etc  ***/

    if (Fields[lc].Continuation)
    { printf("  + (%02d at %02d): ",
             Fields[lc].MaxElements,Fields[lc].Element);
    } else
    { if (Fields[lc].Required)
         printf("\nKeywords (Req): ");
      else printf("\nKeywords (Opt): ");
      printf("%s\n",Fields[lc].KeywordList);
      if (Fields[lc].KeywordUsed)
         printf("         Found: %s\n",Fields[lc].KeywordUsed);
/***  Extra print statement that just adds clutter
      else printf("         Found: *** Not Requested ***\n");
 ***/
      printf("        Format: %s [%d] (%d)\n",
             Fields[lc].Format,Fields[lc].MaxElements,
             Fields[lc].MemoryAllocated);
      printf("        Values: ");
    }

    /***  Print out the value(s) of the label item  ***/
    ValidId = (int*)(Buffer+Fields[lc].ValidOffset);

    if (!LBL_CHK_PRESENT(*ValidId))
    { printf("<NULL>\n");
      continue;
    }


    /* Determine Label value format: int, real, doub, string */
    for (lc_lf=0; strlen(LabelFormat[lc_lf]); lc_lf++)
        if (strcmp(Fields[lc].Format,LabelFormat[lc_lf]) == 0) break;

    /* Determine number of values associated with keyword */
    if (Fields[lc].RtnElementOffset == LBL_NO_RETURN)
       MaxElements = Fields[lc].MaxElements;
    else
    { MaxElements = *(int*)(Buffer+Fields[lc].RtnElementOffset);
      if (MaxElements == 0) MaxElements = Fields[lc].MaxElements;
    }

    for (ValueIdx=0; ValueIdx<MaxElements; ValueIdx++)
    { if (ValueIdx > 0) printf("                ");
      if (LBL_CHK_VALID(*ValidId))
         switch (lc_lf)
      { case 0: /* INT */
                printf("%d\n",*((int*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        case 1: /* REAL */
                printf("%f\n",*((float*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        case 2: /* DOUB */
                printf("%e\n",*((double*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        case 3: /* STRING */
                printf("%s\n",((char*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        default: printf(" ***  Undeterminable  ***\n");
          break;
      } else
         switch (*ValidId)
      { case LBL_PDS_NULL:
                printf("'%s'\n",LBL_PDS_STRING_NULL);
          break;

        case LBL_PDS_UNK:
                printf("'%s'\n",LBL_PDS_STRING_UNK);
          break;

        case LBL_PDS_NA:
                printf("'%s'\n",LBL_PDS_STRING_NA);
          break;

        default: printf("Undefined valid flag: %d\n",*ValidId);
          break;
      }
    }
  }
  printf("\n");

  return;
}

/*******************************************************************************
 *				TEST_LOAD_LABEL_ELEMENTS
 *
 *	Routine is basically used for testing and debugging the implementation
 * of a label API and not for use by individual applications.
 ******************************************************************************/
void	TestLoadLabelElements(
  LblApiProcess_typ	*Label)
{ int	BaseI = 0,
	lc,
	lc_lf,
	ValueIdx,
	status,
	MaxLength,
	MaxElements;
  char	BaseC[8] = "AAAAAA",
	*Buffer = Label->Buffer;
  float	BaseR = 100.0;
  double	BaseD = 10000.0;
  LblApiElement_typ	*Fields = Label->Table;

  char	*LabelFormat[] = { "INT", "REAL", "DOUB", "STRING", "" };

  for (lc=0; Fields[lc].KeywordList; lc++)
  {
    /* Determine Label value format: int, real, doub, string */
    for (lc_lf=0; strlen(LabelFormat[lc_lf]); lc_lf++)
        if (strcmp(Fields[lc].Format,LabelFormat[lc_lf]) == 0) break;

    /* Determine number of values associated with keyword */
    if (Fields[lc].RtnElementOffset == LBL_NO_RETURN)
       MaxElements = Fields[lc].MaxElements;
    else
    { MaxElements = *(int*)(Buffer+Fields[lc].RtnElementOffset);
      if (MaxElements == 0) MaxElements = Fields[lc].MaxElements;
    }

    for (ValueIdx=0; ValueIdx<MaxElements; ValueIdx++)
    { *(int*)(Buffer+Fields[lc].ValidOffset) = LBL_VALID;
      switch (lc_lf)
      { case 0: /* INT */
                *((int*)(Buffer+Fields[lc].ValueOffset)+ValueIdx) = BaseI;
                BaseI++;
          break;

        case 1: /* REAL */
                *((float*)(Buffer+Fields[lc].ValueOffset)+ValueIdx) = BaseR;
		BaseR += 10.0;
          break;

        case 2: /* DOUB */
                *((double*)(Buffer+Fields[lc].ValueOffset)+ValueIdx) = BaseD;
		BaseD += 1000.0;
          break;

        case 3: /* STRING */
                strcpy(((char*)(Buffer+Fields[lc].ValueOffset)+ValueIdx),
                       BaseC);
                if (BaseC[5] == 'Z') BaseC[5] = 'a' - 1;
                if (BaseC[5] == 'z') BaseC[5] = '0' - 1;
                if (BaseC[5] == '9') BaseC[5] = 'A' - 1;
                BaseC[0] = BaseC[1] = BaseC[2] = BaseC[3] =
                           BaseC[4] = BaseC[5] += 1; 
          break;

        default: *(int*)(Buffer+Fields[lc].ValidOffset) = LBL_INVALID;
          break;
      }
    }
  }

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_camera_model.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_camera_model.h"

/******************************************************************************
 *				LBL_MODEL
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Camera Model label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_camera_model.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblCameraModel.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *============================================================================
 *	History of Modifications
 *
 * Date		who		Description
 * ----------   --------------- ---------------------------------------------
 * 2003-05-14   Hyun Lee        Added REFERENCE_COORD_SYSTEM_SOLN_ID
 * 2003-02-11	Payam Zamani	Added FILTER_NAME
 * 2003-01-10	p. Zamani	Changed GEOMETRY_SOURCE_ID,
 *				DERIVED_GEOMETRY_NOTE, and
 *				DERIVED_GEOMETRY_TYPE to LBL_OPTIONAL
 * 2003-01-07	P. Zamani	Changed MODEL_NAME to be LBL_OPTIONAL
 * ?		A. Runkle	Initial development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblCameraModel_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, SolutionId.Value),
		LBL_OFFSET(LblCameraModel_typ, SolutionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolutionId.Value)},

	{"CALIBRATION_SOURCE_ID",		"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, CalibrationSourceId.Value),
		LBL_OFFSET(LblCameraModel_typ, CalibrationSourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CalibrationSourceId.Value)},

	{"MODEL_DESC__PTR",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelDesc.Value),
		LBL_OFFSET(LblCameraModel_typ, ModelDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelDesc.Value)},

	{"MODEL_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelName.Value),
		LBL_OFFSET(LblCameraModel_typ, ModelName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelName.Value)},

	{"MODEL_TYPE",				"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelType.Value),
		LBL_OFFSET(LblCameraModel_typ, ModelType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelType.Value)},

	{"GEOMETRY_SOURCE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, GeometrySourceId.Value),
		LBL_OFFSET(LblCameraModel_typ, GeometrySourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GeometrySourceId.Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[0].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[1].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[2].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[3].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[4].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[5].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[6].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[6].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[7].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[7].Value)},

	{"MODEL_COMPONENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[8].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentId[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentId[8].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[0].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[1].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[2].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[3].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[4].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[5].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[6].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[6].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[7].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[7].Value)},

	{"MODEL_COMPONENT_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[8].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentName[8].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[0].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[1].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[2].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[3].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[4].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[5].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[6].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[6].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[7].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[7].Value)},

	{"MODEL_COMPONENT_UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[8].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponentUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponentUnit[8].Value)},

	{"MODEL_COMPONENT_1",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent1[0].Value)},

	{"MODEL_COMPONENT_1",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent1[2].Value)},

	{"MODEL_COMPONENT_1",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent1[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent1[2].Value)},

	{"MODEL_COMPONENT_2",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent2[0].Value)},

	{"MODEL_COMPONENT_2",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent2[2].Value)},

	{"MODEL_COMPONENT_2",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent2[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent2[2].Value)},

	{"MODEL_COMPONENT_3",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent3[0].Value)},

	{"MODEL_COMPONENT_3",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent3[2].Value)},

	{"MODEL_COMPONENT_3",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent3[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent3[2].Value)},

	{"MODEL_COMPONENT_4",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent4[0].Value)},

	{"MODEL_COMPONENT_4",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent4[2].Value)},

	{"MODEL_COMPONENT_4",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent4[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent4[2].Value)},

	{"MODEL_COMPONENT_5",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent5[0].Value)},

	{"MODEL_COMPONENT_5",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent5[2].Value)},

	{"MODEL_COMPONENT_5",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent5[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent5[2].Value)},

	{"MODEL_COMPONENT_6",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent6[0].Value)},

	{"MODEL_COMPONENT_6",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent6[2].Value)},

	{"MODEL_COMPONENT_6",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent6[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent6[2].Value)},

	{"MODEL_COMPONENT_7",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent7[0].Value)},

	{"MODEL_COMPONENT_7",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent7[2].Value)},

	{"MODEL_COMPONENT_7",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent7[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent7[2].Value)},

	{"MODEL_COMPONENT_8",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent8[0].Value)},

	{"MODEL_COMPONENT_8",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent8[2].Value)},

	{"MODEL_COMPONENT_8",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent8[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent8[2].Value)},

	{"MODEL_COMPONENT_9",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent9[0].Value)},

	{"MODEL_COMPONENT_9",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent9[2].Value)},

	{"MODEL_COMPONENT_9",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ModelComponent9[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ModelComponent9[2].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, FilterName.Value),
		LBL_OFFSET(LblCameraModel_typ, FilterName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName.Value)},


	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

        {"REFERENCE_COORD_SYSTEM_SOLN_ID",      "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemSolnId.Value),
                LBL_OFFSET(LblCameraModel_typ, ReferenceCoordSystemSolnId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static	LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_CAMERA_MODEL
 *
 *****************************************************************************/
void	LblSetCameraModel(
  const	char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_CAMERA_MODEL
 *
 *****************************************************************************/
int     LblCameraModel(
  int   Unit,
  int   Obtain,
  LblCameraModel_typ      *LabelItems,
  int	Instance )
{
  LblSetCameraModel("CAMERA_MODEL");
  return (LblCameraModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_GEOMETRIC_CAMERA_MODEL
 *
 *****************************************************************************/
int     LblGeometricCameraModel(
  int   Unit,
  int   Obtain,
  LblCameraModel_typ      *LabelItems,
  int	Instance )
{
  LblSetCameraModel("GEOMETRIC_CAMERA_MODEL");
  return (LblCameraModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_CAMERA_MODEL_API
 *
 *****************************************************************************/
int     LblCameraModelApi(
  int   Unit,
  int   Obtain,
  LblCameraModel_typ      *LabelItems,
  int	Instance )
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCameraModel_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_CAMERA_MODEL
 *
 *****************************************************************************/
void	LblPrintCameraModel(
  LblCameraModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_CAMERA_MODEL
 *
 *****************************************************************************/
void	LblTestCameraModel(
  LblCameraModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_command.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_command.h"

/******************************************************************************
 *				LBL_COMMAND
 *
 *	This module contains routines to help create, read/write and print
 *  a Command property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_command.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblCommand.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblCommand_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"AUTO_EXPOSURE_DATA_CUT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, AutoExposureDataCut.Value),
		LBL_OFFSET(LblCommand_typ, AutoExposureDataCut.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AutoExposureDataCut.Value)},

	{"AUTO_EXPOSURE_PIXEL_FRACTION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, AutoExposurePixelFraction.Value),
		LBL_OFFSET(LblCommand_typ, AutoExposurePixelFraction.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AutoExposurePixelFraction.Value)},

	{"BAD_PIXEL_REPLACEMENT_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, BadPixelReplacementFlag.Value),
		LBL_OFFSET(LblCommand_typ, BadPixelReplacementFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BadPixelReplacementFlag.Value)},

	{"COMMAND_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, CommandDesc.Value),
		LBL_OFFSET(LblCommand_typ, CommandDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CommandDesc.Value)},

	{"COMMAND_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, CommandName.Value),
		LBL_OFFSET(LblCommand_typ, CommandName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CommandName.Value)},

	{"DARK_CURRENT_CORRECTION_FLAG",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, DarkCurrentCorrectionFlag.Value),
		LBL_OFFSET(LblCommand_typ, DarkCurrentCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DarkCurrentCorrectionFlag.Value)},

	{"DOWNLOAD_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, DownloadType.Value),
		LBL_OFFSET(LblCommand_typ, DownloadType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DownloadType.Value)},

	{"FLAT_FIELD_CORRECTION_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, FlatFieldCorrectionFlag.Value),
		LBL_OFFSET(LblCommand_typ, FlatFieldCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionFlag.Value)},

	{"INSTRUMENT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, InstrumentModeId.Value),
		LBL_OFFSET(LblCommand_typ, InstrumentModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentModeId.Value)},

	{"MAX_AUTO_EXPOS_ITERATION_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, MaxAutoExposIterationCount.Value),
		LBL_OFFSET(LblCommand_typ, MaxAutoExposIterationCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MaxAutoExposIterationCount.Value)},

	{"SHUTTER_EFFECT_CORRECTION_FLAG",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, ShutterEffectCorrectionFlag.Value),
		LBL_OFFSET(LblCommand_typ, ShutterEffectCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterEffectCorrectionFlag.Value)},

	{"SQRT_COMPRESSION_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCommand_typ, SqrtCompressionFlag.Value),
		LBL_OFFSET(LblCommand_typ, SqrtCompressionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SqrtCompressionFlag.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "COMMAND",
	LBL_NULL };

/******************************************************************************
 *				LBL_COMMAND
 *
 *****************************************************************************/
int     LblCommand(
  int   Unit,
  int   Obtain,
  LblCommand_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCommand_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_COMMAND
 *
 *****************************************************************************/
void	LblPrintCommand(
  LblCommand_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_COMMAND
 *
 *****************************************************************************/
void	LblTestCommand(
  LblCommand_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_compression.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_compression.h"

/******************************************************************************
 *				LBL_COMPRESSION
 *
 *	This module contains routines to help create, read/write and print
 *  a Compression property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_compression.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblCompression.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *============================================================================
 *
 * History of modifications:
 *
 * Date         who         Description
 * -----------  --------------- ----------------------------------------------
 * 18-Apr-2003  Hyun Lee        Changed INST_CMPRS_SEG_QUALITY to 
 *                              INST_CMPRS_SEGMENT_QUALITY
 * 25-Mar-2003  Hyun Lee        Added INST_CMPRS_SEG_MISSING_PIXELS, and 
 *                              INST_CMPRS_SEG_QUALITY 
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblCompression_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"ERROR_PIXELS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, ErrorPixels.Value),
		LBL_OFFSET(LblCompression_typ, ErrorPixels.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ErrorPixels.Value)},

	{"INST_CMPRS_BLK_SIZE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsBlkSize.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsBlkSize.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsBlkSize.Value)},

	{"INST_CMPRS_BLOCKS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsBlocks.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsBlocks.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsBlocks.Value)},

	{"INST_CMPRS_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsDesc.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsDesc.Value)},

	{"INST_CMPRS_FILTER",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsFilter.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsFilter.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsFilter.Value)},

	{"INST_CMPRS_ENTROPY",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsEntropy.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsEntropy.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsEntropy.Value)},

	{"INST_CMPRS_MODE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsMode.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsMode.Value)},

	{"INST_CMPRS_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsName.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsName.Value)},

	{"INST_CMPRS_PARAM",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsParam.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsParam.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsParam.Value)},

	{"INST_CMPRS_QUALITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsQuality.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsQuality.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQuality.Value)},

	{"INST_CMPRS_QUANTZ_TBL_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzTblId.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzTblId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQuantzTblId.Value)},

	{"INST_CMPRS_QUANTZ_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzType.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsQuantzType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQuantzType.Value)},

	{"INST_CMPRS_RATE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsRate.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsRate.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsRate.Value)},

	{"INST_CMPRS_RATIO",			"REAL",		LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsRatio.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsRatio.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsRatio.Value)},

	{"INST_CMPRS_SEGMENTS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegments.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegments.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegments.Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[0].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[1].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[2].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[3].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[4].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[5].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[6].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[7].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[8].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[9].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[10].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[11].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[12].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[13].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[14].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[15].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[16].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[17].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[18].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[19].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[20].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[21].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[22].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[23].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[24].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[25].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[26].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[27].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[28].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[29].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[30].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegmentStatus[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[31].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[0].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[1].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[2].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[3].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[4].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[5].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[6].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[7].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[8].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[9].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[10].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[11].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[12].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[13].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[14].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[15].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[16].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[17].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[18].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[19].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[20].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[21].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[22].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[23].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[24].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[25].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[26].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[27].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[28].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[29].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[30].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLine[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[31].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[0].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[1].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[2].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[3].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[4].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[5].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[6].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[7].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[8].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[9].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[10].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[11].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[12].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[13].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[14].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[15].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[16].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[17].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[18].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[19].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[20].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[21].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[22].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[23].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[24].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[25].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[26].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[27].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[28].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[29].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[30].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegFirstLineSamp[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[31].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[0].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[1].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[2].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[3].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[4].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[5].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[6].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[7].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[8].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[9].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[10].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[11].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[12].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[13].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[14].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[15].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[16].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[17].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[18].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[19].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[20].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[21].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[22].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[23].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[24].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[25].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[26].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[27].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[28].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[29].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[30].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegLines[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[31].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[0].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[1].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[2].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[3].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[4].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[5].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[6].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[7].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[8].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[9].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[10].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[11].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[12].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[13].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[14].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[15].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[16].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[17].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[18].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[19].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[20].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[21].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[22].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[23].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[24].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[25].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[26].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[27].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[28].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[29].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[30].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegSamples[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[31].Value)},

        {"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[0].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[1].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[2].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[3].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[4].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[5].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[6].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[7].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[8].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[9].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[10].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[11].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[12].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[13].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[14].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[15].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[16].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[17].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[18].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[19].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[20].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[21].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[22].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[23].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[24].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[25].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[26].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[27].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[28].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[29].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[30].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegMissingPixels[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[31].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[0].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[0].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[1].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[1].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[2].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[2].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[3].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[3].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[4].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[4].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[5].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[5].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[6].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[6].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[7].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[7].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[8].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[8].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[9].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[9].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[10].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[10].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[11].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[11].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[12].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[12].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[13].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[13].Value)},
 
	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[14].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[14].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[15].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[15].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[16].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[16].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[17].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[17].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[18].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[18].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[19].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[19].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[20].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[20].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[21].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[21].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[22].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[22].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[23].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[23].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[24].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[24].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[25].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[25].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[26].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[26].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[27].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[27].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[28].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[28].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[29].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[29].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[30].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[30].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[31].Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSegQuality[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[31].Value)},

	{"INST_CMPRS_SYNC_BLKS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstCmprsSyncBlks.Value),
		LBL_OFFSET(LblCompression_typ, InstCmprsSyncBlks.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSyncBlks.Value)},

	{"INST_DECOMP_STAGES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, InstDecompStages.Value),
		LBL_OFFSET(LblCompression_typ, InstDecompStages.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstDecompStages.Value)},

	{"PIXEL_AVERAGING_HEIGHT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, PixelAveragingHeight.Value),
		LBL_OFFSET(LblCompression_typ, PixelAveragingHeight.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingHeight.Value)},

	{"PIXEL_AVERAGING_WIDTH",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, PixelAveragingWidth.Value),
		LBL_OFFSET(LblCompression_typ, PixelAveragingWidth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingWidth.Value)},

	{"RICE_OPTION_VALUE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, RiceOptionValue.Value),
		LBL_OFFSET(LblCompression_typ, RiceOptionValue.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RiceOptionValue.Value)},

	{"RICE_START_OPTION",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, RiceStartOption.Value),
		LBL_OFFSET(LblCompression_typ, RiceStartOption.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RiceStartOption.Value)},

	{"SQRT_MAXIMUM_PIXEL",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, SqrtMaximumPixel.Value),
		LBL_OFFSET(LblCompression_typ, SqrtMaximumPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SqrtMaximumPixel.Value)},

	{"SQRT_MINIMUM_PIXEL",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCompression_typ, SqrtMinimumPixel.Value),
		LBL_OFFSET(LblCompression_typ, SqrtMinimumPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SqrtMinimumPixel.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_COMPRESSION
 *
 *****************************************************************************/
void     LblSetCompression(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_COMPRESSION
 *
 *****************************************************************************/
int     LblCompression(
  int   Unit,
  int   Obtain,
  LblCompression_typ      *LabelItems,
  int	Instance)
{
  LblSetCompression("COMPRESSION");
  return (LblCompressionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_COMPRESSION_PARMS
 *
 *****************************************************************************/
int     LblCompressionParms(
  int   Unit,
  int   Obtain,
  LblCompression_typ      *LabelItems,
  int	Instance)
{
  LblSetCompression("COMPRESSION_PARMS");
  return (LblCompressionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_COMPRESSION_API
 *
 *****************************************************************************/
int     LblCompressionApi(
  int   Unit,
  int   Obtain,
  LblCompression_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCompression_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_COMPRESSION
 *
 *****************************************************************************/
void	LblPrintCompression(
  LblCompression_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_COMPRESSION
 *
 *****************************************************************************/
void	LblTestCompression(
  LblCompression_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_derived_geometry.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_derived_geometry.h"

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Geometry label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_derived_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblDerivedGeometry.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *============================================================================
 *      History of Modifications
 *
 * Date         who             Description
 * ----------   --------------- ---------------------------------------------
 * 2003-01-07   P. Zamani       Changed DERIVED_GEOMETRY_NAME to be
 *				  LBL_OPTIONAL
 * ?            A. Runkle       Initial development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblDerivedGeometry_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolutionId.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolutionId.Valid),
		LBL_NO_RETURN,  LBL_SIZE(SolutionId.Value)},

	{"COORDINATE_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, CoordinateSystemName.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, CoordinateSystemName.Valid),
		LBL_NO_RETURN,  LBL_SIZE(CoordinateSystemName.Value)},

	{"DERIVED_GEOMETRY_DESC",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryDesc.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryDesc.Value)},

	{"DERIVED_GEOMETRY_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryName.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryName.Value)},

	{"DERIVED_GEOMETRY_NOTE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryNote.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryNote.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryNote.Value)},

	{"DERIVED_GEOMETRY_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryType.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, DerivedGeometryType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedGeometryType.Value)},

	{"LANDER_INSTRUMENT_AZIMUTH, INSTRUMENT_AZIMUTH",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LanderInstrumentAzimuth.Value)},

	{"INSTRUMENT_AZIMUTH__UNIT",
		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentAzimuthUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentAzimuthUnit.Value)},

	{"LANDER_INSTRUMENT_ELEVATION, INSTRUMENT_ELEVATION",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LanderInstrumentElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LanderInstrumentElevation.Value)},

	{"INSTRUMENT_ELEVATION__UNIT",
		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentElevationUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, InstrumentElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentElevationUnit.Value)},

	{"LANDER_LOCAL_LEVEL_QUATERNION, INST_HOST_TO_FIXED_QUATERNION",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	4,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LanderLocalLevelQuaternion.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LanderLocalLevelQuaternion.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LanderLocalLevelQuaternion.Value)},

	{"LCL_LVL_SRFC_FXD_VECTOR, LOCAL_TO_FIXED_OFFSET_VECTOR",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LclLvlSrfcFxdVector.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LclLvlSrfcFxdVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LclLvlSrfcFxdVector.Value)},

	{"LOCAL_LEVEL_INST_AZIMUTH, LOCAL_INSTRUMENT_AZIMUTH",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalLevelInstAzimuth.Value)},

	{"LOCAL_LEVEL_INST_ELEVATION, LOCAL_INSTRUMENT_ELEVATION",
		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, LocalLevelInstElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalLevelInstElevation.Value)},

	{"NORTH_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, NorthAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, NorthAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(NorthAzimuth.Value)},

	{"POSITIVE_AZIMUTH_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveAzimuthDirection.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveAzimuthDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveAzimuthDirection.Value)},

	{"POSITIVE_ELEVATION_DIRECTION",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveElevationDirection.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, PositiveElevationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveElevationDirection.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"SLANT_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SlantDistance.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SlantDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SlantDistance.Value)},

	{"SMEAR_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SmearAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SmearAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SmearAzimuth.Value)},

	{"SMEAR_MAGNITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SmearMagnitude.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SmearMagnitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SmearMagnitude.Value)},

	{"SOLAR_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarAzimuth.Value)},

	{"SOLAR_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuthUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarAzimuthUnit.Value)},

	{"SOLAR_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarElevation.Value)},

	{"SOLAR_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevationUnit.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SolarElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarElevationUnit.Value)},

	{"SRFC_FXD_LCL_LVL_VECTOR,",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SrfcFxdLclLvlVector.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SrfcFxdLclLvlVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SrfcFxdLclLvlVector.Value)},

	{"SURFACE_FIXED_INST_AZIMUTH, FIXED_INSTRUMENT_AZIMUTH",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstAzimuth.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedInstAzimuth.Value)},

	{"SURFACE_FIXED_INST_ELEVATION, FIXED_INSTRUMENT_ELEVATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstElevation.Value),
		LBL_OFFSET(LblDerivedGeometry_typ, SurfaceFixedInstElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedInstElevation.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "DERIVED_GEOMETRY",
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void     LblSetDerivedGeometry(
  const char    *Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY
 *
 *****************************************************************************/
int     LblDerivedGeometry(
  int   Unit,
  int   Obtain,
  LblDerivedGeometry_typ      *LabelItems,
  int	Instance )
{
  LblSetDerivedGeometry("DERIVED_GEOMETRY");
  return (LblDerivedGeometryApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY_PARMS
 *
 *****************************************************************************/
int     LblDerivedGeometryParms(
  int   Unit,
  int   Obtain,
  LblDerivedGeometry_typ      *LabelItems,
  int	Instance )
{
  LblSetDerivedGeometry("DERIVED_GEOMETRY_PARMS");
  return (LblDerivedGeometryApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_DERIVED_GEOMETRY_API
 *
 *****************************************************************************/
int     LblDerivedGeometryApi(
  int   Unit,
  int   Obtain,
  LblDerivedGeometry_typ      *LabelItems,
  int	Instance )
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblDerivedGeometry_typ);


  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void	LblPrintDerivedGeometry(
  LblDerivedGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void	LblTestDerivedGeometry(
  LblDerivedGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_identification.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_identification.h"

/******************************************************************************
 *				LBL_IDENTIFICATION
 *
 *	This module contains routines to help create, read/write and print
 *  an Identification property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_identification.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblIdentification.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *============================================================================
 *
 * History of modifications:
 *
 * Date         who             Description
 * -----------  --------------- ----------------------------------------------
 * 23-May-2003  Hyun Lee        Changed INSTRUMENT_HOST_ID & INSTRUMENT_ID as
 *                              array values
 * 14-Feb-2003	P. Zamani	Changed PLANET_DAY_NUMBER to INT
 * 12-Feb-2003  Payam Zamani	Added RELEASE_ID, change SOLAR_LONGITUE to REAL
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblIdentification_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"DATA_SET_ID",				"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, DataSetId.Value),
		LBL_OFFSET(LblIdentification_typ, DataSetId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DataSetId.Value)},

	{"DATA_SET_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, DataSetName.Value),
		LBL_OFFSET(LblIdentification_typ, DataSetName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DataSetName.Value)},

	{"COMMAND_SEQUENCE_NUMBER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, CommandSequenceNumber.Value),
		LBL_OFFSET(LblIdentification_typ, CommandSequenceNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CommandSequenceNumber.Value)},

	{"FEATURE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FeatureName.Value),
		LBL_OFFSET(LblIdentification_typ, FeatureName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FeatureName.Value)},

	{"FEATURE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FeatureType.Value),
		LBL_OFFSET(LblIdentification_typ, FeatureType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FeatureType.Value)},

	{"FRAME_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FrameId[0].Value),
		LBL_OFFSET(LblIdentification_typ, FrameId[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FrameId[0].Value)},

	{"FRAME_ID",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FrameId[1].Value),
		LBL_OFFSET(LblIdentification_typ, FrameId[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FrameId[1].Value)},

	{"FRAME_ID",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FrameId[2].Value),
		LBL_OFFSET(LblIdentification_typ, FrameId[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FrameId[2].Value)},

	{"FRAME_ID",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FrameId[3].Value),
		LBL_OFFSET(LblIdentification_typ, FrameId[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FrameId[3].Value)},

	{"FRAME_ID",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FrameId[4].Value),
		LBL_OFFSET(LblIdentification_typ, FrameId[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FrameId[4].Value)},

	{"FRAME_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, FrameType.Value),
		LBL_OFFSET(LblIdentification_typ, FrameType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FrameType.Value)},

        {"GEOMETRY_PROJECTION_TYPE",            "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblIdentification_typ, GeometryProjectionType.Value),
                LBL_OFFSET(LblIdentification_typ, GeometryProjectionType.Valid),
                LBL_NO_RETURN,  LBL_SIZE(GeometryProjectionType.Value)},

	{"IMAGE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ImageId.Value),
		LBL_OFFSET(LblIdentification_typ, ImageId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ImageId.Value)},

	{"IMAGE_TIME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ImageTime.Value),
		LBL_OFFSET(LblIdentification_typ, ImageTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ImageTime.Value)},

	{"IMAGE_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ImageType.Value),
		LBL_OFFSET(LblIdentification_typ, ImageType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ImageType.Value)},

	{"INSTRUMENT_HOST_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[0].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostId[0].Value)},

	{"INSTRUMENT_HOST_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[1].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostId[1].Value)},

	{"INSTRUMENT_HOST_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[2].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostId[2].Value)},

	{"INSTRUMENT_HOST_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[3].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostId[3].Value)},

	{"INSTRUMENT_HOST_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[4].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostId[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostId[4].Value)},

	{"INSTRUMENT_HOST_NAME",		"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[0].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostName[0].Value)},

	{"INSTRUMENT_HOST_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[1].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostName[1].Value)},

	{"INSTRUMENT_HOST_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[2].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostName[2].Value)},

	{"INSTRUMENT_HOST_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[3].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostName[3].Value)},

	{"INSTRUMENT_HOST_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[4].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentHostName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentHostName[4].Value)},

	{"INSTRUMENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentId[0].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentId[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentId[0].Value)},

	{"INSTRUMENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentId[1].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentId[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentId[1].Value)},

	{"INSTRUMENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentId[2].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentId[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentId[2].Value)},

	{"INSTRUMENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentId[3].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentId[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentId[3].Value)},

	{"INSTRUMENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentId[4].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentId[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentId[4].Value)},

	{"INSTRUMENT_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentName[0].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentName[0].Value)},

	{"INSTRUMENT_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentName[1].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentName[1].Value)},

	{"INSTRUMENT_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentName[2].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentName[2].Value)},

	{"INSTRUMENT_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentName[3].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentName[3].Value)},

	{"INSTRUMENT_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentName[4].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentName[4].Value)},

	{"INSTRUMENT_SERIAL_NUMBER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentSerialNumber.Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentSerialNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentSerialNumber.Value)},

	{"INSTRUMENT_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentType[0].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentType[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentType[0].Value)},

	{"INSTRUMENT_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentType[1].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentType[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentType[1].Value)},

	{"INSTRUMENT_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentType[2].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentType[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentType[2].Value)},

	{"INSTRUMENT_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentType[3].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentType[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentType[3].Value)},

	{"INSTRUMENT_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentType[4].Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentType[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentType[4].Value)},

	{"INSTRUMENT_VERSION_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, InstrumentVersionId.Value),
		LBL_OFFSET(LblIdentification_typ, InstrumentVersionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentVersionId.Value)},

	{"LOCAL_TRUE_SOLAR_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, LocalTrueSolarTime.Value),
		LBL_OFFSET(LblIdentification_typ, LocalTrueSolarTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalTrueSolarTime.Value)},

	{"MAGNET_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MagnetId.Value),
		LBL_OFFSET(LblIdentification_typ, MagnetId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MagnetId.Value)},

	{"MEASUREMENT_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MeasurementId.Value),
		LBL_OFFSET(LblIdentification_typ, MeasurementId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MeasurementId.Value)},

	{"MEASUREMENT_TIME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MeasurementTime.Value),
		LBL_OFFSET(LblIdentification_typ, MeasurementTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MeasurementTime.Value)},

	{"MEASUREMENT_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MeasurementType.Value),
		LBL_OFFSET(LblIdentification_typ, MeasurementType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MeasurementType.Value)},

	{"MISSION_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MissionName[0].Value),
		LBL_OFFSET(LblIdentification_typ, MissionName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissionName[0].Value)},

	{"MISSION_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MissionName[1].Value),
		LBL_OFFSET(LblIdentification_typ, MissionName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissionName[1].Value)},

	{"MISSION_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MissionName[2].Value),
		LBL_OFFSET(LblIdentification_typ, MissionName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissionName[2].Value)},

	{"MISSION_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MissionName[3].Value),
		LBL_OFFSET(LblIdentification_typ, MissionName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissionName[3].Value)},

	{"MISSION_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MissionName[4].Value),
		LBL_OFFSET(LblIdentification_typ, MissionName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissionName[4].Value)},

	{"MISSION_PHASE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, MissionPhaseName.Value),
		LBL_OFFSET(LblIdentification_typ, MissionPhaseName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissionPhaseName.Value)},

	{"OBSERVATION_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ObservationId.Value),
		LBL_OFFSET(LblIdentification_typ, ObservationId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ObservationId.Value)},

	{"OBSERVATION_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ObservationName.Value),
		LBL_OFFSET(LblIdentification_typ, ObservationName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ObservationName.Value)},

	{"OBSERVATION_TIME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ObservationTime.Value),
		LBL_OFFSET(LblIdentification_typ, ObservationTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ObservationTime.Value)},

	{"OBSERVATION_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ObservationType.Value),
		LBL_OFFSET(LblIdentification_typ, ObservationType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ObservationType.Value)},

	{"ORBIT_NUMBER",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, OrbitNumber.Value),
		LBL_OFFSET(LblIdentification_typ, OrbitNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OrbitNumber.Value)},

    {"OPS_TOKEN",            "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIdentification_typ, OpsToken.Value),
        LBL_OFFSET(LblIdentification_typ, OpsToken.Valid),
        LBL_NO_RETURN,  LBL_SIZE(OpsToken.Value)},

    {"OPS_TOKEN_ACTIVITY",            "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIdentification_typ, OpsTokenActivity.Value),
        LBL_OFFSET(LblIdentification_typ, OpsTokenActivity.Valid),
        LBL_NO_RETURN,  LBL_SIZE(OpsTokenActivity.Value)},

    {"OPS_TOKEN_COMMAND",            "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIdentification_typ, OpsTokenCommand.Value),
        LBL_OFFSET(LblIdentification_typ, OpsTokenCommand.Valid),
        LBL_NO_RETURN,  LBL_SIZE(OpsTokenCommand.Value)},

    {"OPS_TOKEN_PAYLOAD",            "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIdentification_typ, OpsTokenPayload.Value),
        LBL_OFFSET(LblIdentification_typ, OpsTokenPayload.Valid),
        LBL_NO_RETURN,  LBL_SIZE(OpsTokenPayload.Value)},

	{"PLANET_DAY_NUMBER",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, PlanetDayNumber.Value),
		LBL_OFFSET(LblIdentification_typ, PlanetDayNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PlanetDayNumber.Value)},

	{"PROCESSING_HISTORY_TEXT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ProcessingHistoryText.Value),
		LBL_OFFSET(LblIdentification_typ, ProcessingHistoryText.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProcessingHistoryText.Value)},

	{"PRODUCER_FULL_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ProducerFullName.Value),
		LBL_OFFSET(LblIdentification_typ, ProducerFullName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProducerFullName.Value)},

	{"PRODUCER_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ProducerId.Value),
		LBL_OFFSET(LblIdentification_typ, ProducerId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProducerId.Value)},

	{"PRODUCER_INSTITUTION_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ProducerInstitutionName.Value),
		LBL_OFFSET(LblIdentification_typ, ProducerInstitutionName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProducerInstitutionName.Value)},

	{"PRODUCT_CREATION_TIME",		"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ProductCreationTime.Value),
		LBL_OFFSET(LblIdentification_typ, ProductCreationTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProductCreationTime.Value)},

	{"PRODUCT_ID",				"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ProductId.Value),
		LBL_OFFSET(LblIdentification_typ, ProductId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProductId.Value)},

	{"PRODUCT_VERSION_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ProductVersionId.Value),
		LBL_OFFSET(LblIdentification_typ, ProductVersionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProductVersionId.Value)},

	{"RELEASE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, ReleaseId.Value),
		LBL_OFFSET(LblIdentification_typ, ReleaseId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReleaseId.Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[0].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[0].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[1].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[1].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[2].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[2].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[3].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[3].Value)},

	{"ROVER_MOTION_COUNTER",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[4].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounter[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounter[4].Value)},

	{"ROVER_MOTION_COUNTER_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[0].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[0].Value)},

	{"ROVER_MOTION_COUNTER_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[1].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[1].Value)},

	{"ROVER_MOTION_COUNTER_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[2].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[2].Value)},

	{"ROVER_MOTION_COUNTER_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[3].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[3].Value)},

	{"ROVER_MOTION_COUNTER_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[4].Value),
		LBL_OFFSET(LblIdentification_typ, RoverMotionCounterName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(RoverMotionCounterName[4].Value)},

	{"SEQ_ID,SEQUENCE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, SequenceId.Value),
		LBL_OFFSET(LblIdentification_typ, SequenceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SequenceId.Value)},

	{"SEQUENCE_NAME, SEQUENCE_TITLE",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, SequenceName.Value),
		LBL_OFFSET(LblIdentification_typ, SequenceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SequenceName.Value)},

	{"SEQUENCE_VERSION_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, SequenceVersionId.Value),
		LBL_OFFSET(LblIdentification_typ, SequenceVersionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SequenceVersionId.Value)},

	{"SOLAR_LONGITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, SolarLongitude.Value),
		LBL_OFFSET(LblIdentification_typ, SolarLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLongitude.Value)},

	{"SPACECRAFT_CLOCK_CNT_PARTITION",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, SpacecraftClockCntPartition.Value),
		LBL_OFFSET(LblIdentification_typ, SpacecraftClockCntPartition.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpacecraftClockCntPartition.Value)},

	{"SPACECRAFT_CLOCK_START_COUNT",	"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, SpacecraftClockStartCount.Value),
		LBL_OFFSET(LblIdentification_typ, SpacecraftClockStartCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpacecraftClockStartCount.Value)},

	{"SPACECRAFT_CLOCK_STOP_COUNT",		"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, SpacecraftClockStopCount.Value),
		LBL_OFFSET(LblIdentification_typ, SpacecraftClockStopCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpacecraftClockStopCount.Value)},

	{"START_TIME",				"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, StartTime.Value),
		LBL_OFFSET(LblIdentification_typ, StartTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StartTime.Value)},

	{"STOP_TIME",				"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, StopTime.Value),
		LBL_OFFSET(LblIdentification_typ, StopTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StopTime.Value)},

	{"TARGET_NAME",				"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, TargetName.Value),
		LBL_OFFSET(LblIdentification_typ, TargetName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetName.Value)},

	{"TARGET_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblIdentification_typ, TargetType.Value),
		LBL_OFFSET(LblIdentification_typ, TargetType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetType.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
	
static LblApiProcess_typ	Label = {
	LabelTbl,	"PROPERTY",	"PROPERTY",	"IDENTIFICATION",
	LBL_NULL };

/******************************************************************************
 *				LBL_IDENTIFIER
 *
 *****************************************************************************/
int     LblIdentification(
  int   Unit,
  int   Obtain,
  LblIdentification_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblIdentification_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IDENTIFIER
 *
 *****************************************************************************/
void     LblPrintIdentification(
  LblIdentification_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IDENTIFIER
 *
 *****************************************************************************/
void     LblTestIdentification(
  LblIdentification_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_image_geometry.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_image_geometry.h"

/******************************************************************************
 *				LBL_IMAGE_GEOMETRY
 *
 *	This module contains routines to help create, read/write and print
 *  an Image Geometry property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblImageGeometry.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblImageGeometry_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"CENTRAL_BODY_DISTANCE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, CentralBodyDistance.Value),
		LBL_OFFSET(LblImageGeometry_typ, CentralBodyDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CentralBodyDistance.Value)},

	{"EMISSION_ANGLE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, EmissionAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, EmissionAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EmissionAngle.Value)},

	{"INCIDENCE_ANGLE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, IncidenceAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, IncidenceAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(IncidenceAngle.Value)},

	{"INTERCEPT_POINT_LATITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLatitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLatitude.Value)},

	{"INTERCEPT_POINT_LINE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLine.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLine.Value)},

	{"INTERCEPT_POINT_LINE_SAMPLE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLineSample.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLineSample.Value)},

	{"INTERCEPT_POINT_LONGITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLongitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, InterceptPointLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterceptPointLongitude.Value)},

	{"LOCAL_HOUR_ANGLE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalHourAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalHourAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalHourAngle.Value)},

	{"LOCAL_MEAN_SOLAR_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalMeanSolarTime.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalMeanSolarTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalMeanSolarTime.Value)},

	{"LOCAL_TIME",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalTime.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalTime.Value)},

	{"LOCAL_TRUE_SOLAR_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, LocalTrueSolarTime.Value),
		LBL_OFFSET(LblImageGeometry_typ, LocalTrueSolarTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LocalTrueSolarTime.Value)},

	{"NTV_SAT_TIME_FROM_CLOSEST_APRH",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, NtvSatTimeFromClosestAprh.Value),
		LBL_OFFSET(LblImageGeometry_typ, NtvSatTimeFromClosestAprh.Valid),
		LBL_NO_RETURN,	LBL_SIZE(NtvSatTimeFromClosestAprh.Value)},

	{"NTV_TIME_FROM_CLOSEST_APPROACH",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, NtvTimeFromClosestApproach.Value),
		LBL_OFFSET(LblImageGeometry_typ, NtvTimeFromClosestApproach.Valid),
		LBL_NO_RETURN,	LBL_SIZE(NtvTimeFromClosestApproach.Value)},

	{"PHASE_ANGLE",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, PhaseAngle.Value),
		LBL_OFFSET(LblImageGeometry_typ, PhaseAngle.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PhaseAngle.Value)},

	{"SOLAR_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarAzimuth.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarAzimuth.Value)},

	{"SOLAR_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarDistance.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarDistance.Value)},

	{"SOLAR_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarElevation.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarElevation.Value)},

	{"SOLAR_LATITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLatitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLatitude.Value)},

	{"SOLAR_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLine.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLine.Value)},

	{"SOLAR_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLineSample.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLine.Value)},

	{"SOLAR_LONGITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SolarLongitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SolarLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolarLongitude.Value)},

	{"SPACECRAFT_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SpacecraftDistance.Value),
		LBL_OFFSET(LblImageGeometry_typ, SpacecraftDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpacecraftDistance.Value)},

	{"SUB_SPACECRAFT_AZIMUTH",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftAzimuth.Value),
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubSpacecraftAzimuth.Value)},

	{"SUB_SPACECRAFT_LATITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLatitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubSpacecraftLatitude.Value)},

	{"SUB_SPACECRAFT_LONGITUDE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLongitude.Value),
		LBL_OFFSET(LblImageGeometry_typ, SubSpacecraftLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubSpacecraftLongitude.Value)},

	{"SURCAFE_FIXED_SOLAR_AZIMUTH",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarAzimuth.Value),
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedSolarAzimuth.Value)},

	{"SURFACE_FIXED_SOLAR_ELEVATION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarElevation.Value),
		LBL_OFFSET(LblImageGeometry_typ, SurfaceFixedSolarElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceFixedSolarElevation.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "IMAGE_GEOMETRY",
	LBL_NULL };

/******************************************************************************
 *				LBL_IMAGE_GEOMETRY
 *
 *****************************************************************************/
int     LblImageGeometry(
  int   Unit,
  int   Obtain,
  LblImageGeometry_typ      *LabelItems,
  int	Instance )
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblImageGeometry_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IMAGE_GEOMETRY
 *
 *****************************************************************************/
void	LblPrintImageGeometry(
  LblImageGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IMAGE_GEOMETRY
 *
 *****************************************************************************/
void	LblTestImageGeometry(
  LblImageGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_image_data.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_image_data.h"

/******************************************************************************
 *				LBL_IMAGE_DATA
 *
 *	This module contains routines to help create, read/write and print
 *  an Image Data property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_data.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblImageData.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 2003-11-12   H. Lee          Added RADIANCE_OFFSET__UNIT & 
 *                              RADIANCE_SCALING_FACTOR__UNIT
 * 2003-07-08   H. Lee          Changed MAXIMUM & MINIMUM to REAL from STRING
 *                              Changed CHECKSUM to REAL from INT
 * 2003-07-01   H. Lee          Changed MISSING_CONSTANT as an array
 * 2003-05-20   H. Lee          Changed RADIANCE_SCALE_FACTOR to
 *                              RADIANCE_SCALING_FACTOR
 * 2003-03-03   H. Lee          Changed INVALID_CONSTANT & MISSING_CONSTANT
 *                              to REAL from INT
 * 2003-02-26   H. Lee          Changed INVALID_CONSTANT & MISSING_CONSTANT
 *                              to INT from STRING
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblImageData_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"BANDS",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Bands.Value),
		LBL_OFFSET(LblImageData_typ, Bands.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Bands.Value)},

	{"CHECKSUM",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Checksum.Value),
		LBL_OFFSET(LblImageData_typ, Checksum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Checksum.Value)},

	{"FIRST_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, FirstLine.Value),
		LBL_OFFSET(LblImageData_typ, FirstLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FirstLine.Value)},

	{"FIRST_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, FirstLineSample.Value),
		LBL_OFFSET(LblImageData_typ, FirstLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FirstLineSample.Value)},

	{"INTERCHANGE_FORMAT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InterchangeFormat.Value),
		LBL_OFFSET(LblImageData_typ, InterchangeFormat.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InterchangeFormat.Value)},

	{"INVALID_CONSTANT",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InvalidConstant[0].Value),
		LBL_OFFSET(LblImageData_typ, InvalidConstant[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InvalidConstant[0].Value)},

	{"INVALID_CONSTANT",			"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InvalidConstant[1].Value),
		LBL_OFFSET(LblImageData_typ, InvalidConstant[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InvalidConstant[1].Value)},

	{"INVALID_CONSTANT",			"REAL",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, InvalidConstant[2].Value),
		LBL_OFFSET(LblImageData_typ, InvalidConstant[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InvalidConstant[2].Value)},

	{"LINE_PREFIX_BYTES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LinePrefixBytes.Value),
		LBL_OFFSET(LblImageData_typ, LinePrefixBytes.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LinePrefixBytes.Value)},

	{"LINE_PREFIX_MEAN",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LinePrefixMean.Value),
		LBL_OFFSET(LblImageData_typ, LinePrefixMean.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LinePrefixMean.Value)},

	{"LINE_SUFFIX_BYTES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LineSuffixBytes.Value),
		LBL_OFFSET(LblImageData_typ, LineSuffixBytes.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineSuffixBytes.Value)},

	{"LINE_SUFFIX_MEAN",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LineSuffixMean.Value),
		LBL_OFFSET(LblImageData_typ, LineSuffixMean.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineSuffixMean.Value)},

	{"LINE_SAMPLES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, LineSamples.Value),
		LBL_OFFSET(LblImageData_typ, LineSamples.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineSamples.Value)},

	{"LINES",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Lines.Value),
		LBL_OFFSET(LblImageData_typ, Lines.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Lines.Value)},

	{"MAXIMUM",				"REAL",  	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Maximum.Value),
		LBL_OFFSET(LblImageData_typ, Maximum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Maximum.Value)},

	{"MEAN",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Mean.Value),
		LBL_OFFSET(LblImageData_typ, Mean.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Mean.Value)},

	{"MEDIAN",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Median.Value),
		LBL_OFFSET(LblImageData_typ, Median.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Median.Value)},

	{"MINIMUM",				"REAL",  	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, Minimum.Value),
		LBL_OFFSET(LblImageData_typ, Minimum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Minimum.Value)},

	{"MISSING_CONSTANT",			"REAL",  	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, MissingConstant[0].Value),
		LBL_OFFSET(LblImageData_typ, MissingConstant[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MissingConstant[0].Value)},

        {"MISSING_CONSTANT",                    "REAL",         LBL_OPTIONAL,
                LBL_CONTINUE,   1,      2,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, MissingConstant[1].Value),
                LBL_OFFSET(LblImageData_typ, MissingConstant[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(MissingConstant[1].Value)},

        {"MISSING_CONSTANT",                    "REAL",         LBL_OPTIONAL,
                LBL_CONTINUE,   1,      3,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, MissingConstant[2].Value),
                LBL_OFFSET(LblImageData_typ, MissingConstant[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(MissingConstant[2].Value)},

	{"RADIANCE_OFFSET",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, RadianceOffset.Value),
		LBL_OFFSET(LblImageData_typ, RadianceOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceOffset.Value)},

        {"RADIANCE_OFFSET__UNIT",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, RadianceOffsetUnit.Value),
                LBL_OFFSET(LblImageData_typ, RadianceOffsetUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceOffsetUnit.Value)},

	{"RADIANCE_SCALING_FACTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, RadianceScaleFactor.Value),
		LBL_OFFSET(LblImageData_typ, RadianceScaleFactor.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceScaleFactor.Value)},

        {"RADIANCE_SCALING_FACTOR__UNIT",       "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblImageData_typ, RadianceScaleFactorUnit.Value),
                LBL_OFFSET(LblImageData_typ, RadianceScaleFactorUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceScaleFactorUnit.Value)},

	{"SAMPLE_BIT_MASK",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SampleBitMask.Value),
		LBL_OFFSET(LblImageData_typ, SampleBitMask.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitMask.Value)},

	{"SAMPLE_BITS",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SampleBits.Value),
		LBL_OFFSET(LblImageData_typ, SampleBits.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBits.Value)},

	{"SAMPLE_TYPE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SampleType.Value),
		LBL_OFFSET(LblImageData_typ, SampleType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleType.Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[0].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[0].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[1].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[1].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[2].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[2].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[3].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[3].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[4].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[4].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[5].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[5].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[6].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[6].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, SpiceFileName[7].Value),
		LBL_OFFSET(LblImageData_typ, SpiceFileName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[7].Value)},

	{"STANDARD_DEVIATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageData_typ, StandardDeviation.Value),
		LBL_OFFSET(LblImageData_typ, StandardDeviation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StandardDeviation.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "IMAGE_DATA",
	LBL_NULL };

/******************************************************************************
 *				LBL_IMAGE_DATA
 *
 *****************************************************************************/
int     LblImageData(
  int   Unit,
  int   Obtain,
  LblImageData_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblImageData_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IMAGE_DATA
 *
 *****************************************************************************/
void	LblPrintImageData(
  LblImageData_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IMAGE_DATA
 *
 *****************************************************************************/
void	LblTestImageData(
  LblImageData_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_instrument_state.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_instrument_state.h"

/******************************************************************************
 *				LBL_INSTRUMENT_STATE
 *
 *	This module contains routines to help create, read/write and print an
 *  Instrument State property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_instrument_state.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblInstrumentState.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 2003-05-12   H. Lee          Changed SUN_FIND_PARM to REAL
 * 2003-04-18   H. Lee          Changed DETECTOR_TO_IMAGE_ROTATION to REAL
 * 2003-01-13   P. Zamani       Changed BadPixelReplaceFlag to
 *                                BadPixelReplacementId
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblInstrumentState_typ*)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"AZIMUTH_FOV",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFov.Value),
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFov.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AzimuthFov.Value)},

	{"AZIMUTH_FOV__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFovUnit.Value),
		LBL_OFFSET(LblInstrumentState_typ, AzimuthFovUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AzimuthFovUnit.Value)},

	{"ELEVATION_FOV",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ElevationFov.Value),
		LBL_OFFSET(LblInstrumentState_typ, ElevationFov.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ElevationFov.Value)},

	{"ELEVATION_FOV__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ElevationFovUnit.Value),
		LBL_OFFSET(LblInstrumentState_typ, ElevationFovUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ElevationFovUnit.Value)},

	{"BAD_PIXEL_REPLACEMENT_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, BadPixelReplacementId.Value),
		LBL_OFFSET(LblInstrumentState_typ, BadPixelReplacementId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BadPixelReplacementId.Value)},

	{"DETECTOR_FIRST_LINE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DetectorFirstLine.Value),
		LBL_OFFSET(LblInstrumentState_typ, DetectorFirstLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DetectorFirstLine.Value)},

	{"DETECTOR_LINES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DetectorLines.Value),
		LBL_OFFSET(LblInstrumentState_typ, DetectorLines.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DetectorLines.Value)},

	{"DETECTOR_TO_IMAGE_ROTATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DetectorToImageRotation.Value),
		LBL_OFFSET(LblInstrumentState_typ, DetectorToImageRotation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DetectorToImageRotation.Value)},

	{"DOWNSAMPLE_METHOD",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, DownsampleMethod.Value),
		LBL_OFFSET(LblInstrumentState_typ, DownsampleMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DownsampleMethod.Value)},

	{"EXPOSURE_COUNT",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureCount.Value)},

	{"EXPOSURE_DURATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureDuration.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureDuration.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureDuration.Value)},

	{"EXPOSURE_DURATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationUnit.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureDurationUnit.Value)},

	{"EXPOSURE_DURATION_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureDurationCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureDurationCount.Value)},

	{"EXPOSURE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ExposureType.Value),
		LBL_OFFSET(LblInstrumentState_typ, ExposureType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureType.Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[0].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[1].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[2].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[3].Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterName[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName[4].Value)},

	{"FILTER_NUMBER",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FilterNumber.Value),
		LBL_OFFSET(LblInstrumentState_typ, FilterNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterNumber.Value)},

	{"FLAT_FIELD_CORRECTION_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionFlag.Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionFlag.Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[0].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[1].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[2].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[3].Value)},

	{"FLAT_FIELD_CORRECTION_PARM",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, FlatFieldCorrectionParm[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionParm[4].Value)},

	{"GAIN_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, GainModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, GainModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GainModeId.Value)},

    {"IMAGE_BIAS",        "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, ImageBias.Value),
        LBL_OFFSET(LblInstrumentState_typ, ImageBias.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageBias.Value)},

	{"INST_AZ_ROTATION_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstAzRotationDirection.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstAzRotationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstAzRotationDirection.Value)},

	{"INST_EL_ROTATION_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstElRotationDirection.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstElRotationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstElRotationDirection.Value)},

	{"INST_HOST_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstHostPosition[0].Value)},

	{"INST_HOST_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstHostPosition[1].Value)},

	{"INST_HOST_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstHostPosition[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstHostPosition[2].Value)},

	{"INSTRUMENT_AZIMUTH_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentAzimuthCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentAzimuthCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentAzimuthCount.Value)},

	{"INSTRUMENT_COVER_STATE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentCoverStateId.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentCoverStateId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentCoverStateId.Value)},

	{"INSTRUMENT_DATA_RATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDataRate.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDataRate.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentDataRate.Value)},

	{"INSTRUMENT_DEPLOYMENT_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDeploymentState.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentDeploymentState.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentDeploymentState.Value)},

	{"INSTRUMENT_ELEVATION_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentElevationCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentElevationCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentElevationCount.Value)},

	{"INSTRUMENT_FOCAL_LENGTH_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentFocalLengthCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentFocalLengthCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentFocalLengthCount.Value)},

	{"INSTRUMENT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentModeId.Value)},

	{"INSTRUMENT_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentPosition[0].Value)},

	{"INSTRUMENT_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentPosition[1].Value)},

	{"INSTRUMENT_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentPosition[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentPosition[2].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[0].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[1].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[2].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[3].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[4].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[5].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[6].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[7].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[8].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[9].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[10].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[11].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[12].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[13].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[14].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[15].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[16].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[17].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[18].Value)},

	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperature[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperature[19].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[0].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[1].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[2].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[3].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[4].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[5].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[6].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[7].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[8].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[9].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[10].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[11].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[12].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[13].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[14].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[15].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[16].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[17].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[18].Value)},

	{"INSTRUMENT_TEMPERATURE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureUnit[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureUnit[19].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[0].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[1].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[2].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[3].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[4].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[5].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[6].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[7].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[8].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[9].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[10].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[11].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[12].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[13].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[14].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[15].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[16].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[17].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[18].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCountUnit[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCountUnit[19].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[0].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[1].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[2].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[3].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[4].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[5].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[6].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[7].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[8].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[9].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[10].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[11].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[12].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[13].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[14].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[15].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[16].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[17].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[18].Value)},

	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureCount[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureCount[19].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[0].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[1].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[2].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[3].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[4].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[5].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[5].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[6].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[6].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[7].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[7].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[8].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[8].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[9].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[9].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[10].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[10].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[11].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[11].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[12].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[12].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[13].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[13].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[14].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[14].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[15].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[15].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[16].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[16].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[17].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[17].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[18].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[18].Value)},

	{"INSTRUMENT_TEMPERATURE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[19].Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentTemperatureName[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentTemperatureName[19].Value)},

	{"INSTRUMENT_VOLTAGE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, InstrumentVoltageCount.Value),
		LBL_OFFSET(LblInstrumentState_typ, InstrumentVoltageCount.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentVoltageCount.Value)},

	{"LED_BITMASK",		"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, LedBitmask.Value),
		LBL_OFFSET(LblInstrumentState_typ, LedBitmask.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LedBitmask.Value)},

	{"OFFSET_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, OffsetModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, OffsetModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OffsetModeId.Value)},

	{"OFFSET_NUMBER",			"INT",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, OffsetNumber.Value),
		LBL_OFFSET(LblInstrumentState_typ, OffsetNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OffsetNumber.Value)},

	{"ONBOARD_IMAGE_BIAS",			"INT",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, OnboardImageBias.Value),
		LBL_OFFSET(LblInstrumentState_typ, OnboardImageBias.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OnboardImageBias.Value)},

	{"PIXEL_AVERAGING_HEIGHT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingHeight.Value),
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingHeight.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingHeight.Value)},

	{"PIXEL_AVERAGING_WIDTH",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingWidth.Value),
		LBL_OFFSET(LblInstrumentState_typ, PixelAveragingWidth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelAveragingWidth.Value)},

	{"SAMPLE_BIT_METHOD",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SampleBitMethod.Value),
		LBL_OFFSET(LblInstrumentState_typ, SampleBitMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitMethod.Value)},

	{"SAMPLE_BIT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SampleBitModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, SampleBitModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitModeId.Value)},

	{"SHUTTER_EFFECT_CORRECTION_FLAG",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ShutterEffectCorrectionFlag.Value),
		LBL_OFFSET(LblInstrumentState_typ, ShutterEffectCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterEffectCorrectionFlag.Value)},

	{"SHUTTER_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, ShutterModeId.Value),
		LBL_OFFSET(LblInstrumentState_typ, ShutterModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterModeId.Value)},

	{"SUN_FIND_FLAG",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindFlag.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindFlag.Value)},

    {"SUN_FIND",           "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, SunFind.Value),
        LBL_OFFSET(LblInstrumentState_typ, SunFind.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SunFind.Value)},

    {"SUN_FIND_ACTIVE_FLAG",           "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblInstrumentState_typ, SunFindActiveFlag.Value),
        LBL_OFFSET(LblInstrumentState_typ, SunFindActiveFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SunFindActiveFlag.Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[0].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[1].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[2].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[3].Value)},

	{"SUN_FIND_PARM",			"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParm[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParm[4].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[0].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[0].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[1].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[1].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[2].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[2].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[3].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[3].Value)},

	{"SUN_FIND_PARM_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[4].Value),
		LBL_OFFSET(LblInstrumentState_typ, SunFindParmName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunFindParmName[4].Value)},

	{"SUN_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunLine.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunLine.Value)},

	{"SUN_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunLineSample.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunLineSample.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunLineSample.Value)},

	{"SUN_VIEW_POSITION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunViewPosition.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunViewPosition.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunViewPosition.Value)},

	{"SUN_VIEW_DIRECTION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblInstrumentState_typ, SunViewDirection.Value),
		LBL_OFFSET(LblInstrumentState_typ, SunViewDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SunViewDirection.Value)},


	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_INSTRUMENT_STATE
 *
 *****************************************************************************/
void     LblSetInstrumentState(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_INSTRUMENT_STATE
 *
 *****************************************************************************/
int     LblInstrumentState(
  int   Unit,
  int   Obtain,
  LblInstrumentState_typ      *LabelItems,
  int	Instance)
{
  LblSetInstrumentState("INSTRUMENT_STATE");
  return (LblInstrumentStateApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_INSTRUMENT_STATE_PARMS
 *
 *****************************************************************************/
int     LblInstrumentStateParms(
  int   Unit,
  int   Obtain,
  LblInstrumentState_typ      *LabelItems,
  int	Instance)
{
  LblSetInstrumentState("INSTRUMENT_STATE_PARMS");
  return (LblInstrumentStateApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_INSTRUMENT_STATE_API
 *
 *****************************************************************************/
int     LblInstrumentStateApi(
  int   Unit,
  int   Obtain,
  LblInstrumentState_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblInstrumentState_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_INSTRUMENT_STATE
 *
 *****************************************************************************/
void	LblPrintInstrumentState(
  LblInstrumentState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_INSTRUMENT_STATE
 *
 *****************************************************************************/
void	LblTestInstrumentState(
  LblInstrumentState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_telemetry.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_telemetry.h"

/******************************************************************************
 *				LBL_TELEMETRY
 *
 *	This module contains routines to help create, read/write and print
 *  a Telemetry property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_telemetry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblTelemetry.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 2003-04-24   H. Lee          Changed SPICE_FILE_ID as LBL_OPTIONAL
 * 2003-01-13   P. Zamani       Added APPLICATION_PROCESS_SUBTYPE_ID
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblTelemetry_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"APPLICATION_PACKET_ID",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketId.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationPacketId.Value)},

	{"APPLICATION_PACKET_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketName.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationPacketName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationPacketName.Value)},

	{"APPLICATION_PROCESS_ID",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessId.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationProcessId.Value)},

	{"APPLICATION_PROCESS_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessName.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationProcessName.Value)},

	{"APPLICATION_PROCESS_SUBTYPE_ID",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessSubtypeId.Value),
		LBL_OFFSET(LblTelemetry_typ, ApplicationProcessSubtypeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ApplicationProcessSubtypeId.Value)},

	{"EARTH_RECEIVED_START_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStartTime.Value),
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStartTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarthReceivedStartTime.Value)},

	{"EARTH_RECEIVED_STOP_TIME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStopTime.Value),
		LBL_OFFSET(LblTelemetry_typ, EarthReceivedStopTime.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarthReceivedStopTime.Value)},

	{"EXPECTED_PACKETS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ExpectedPackets.Value),
		LBL_OFFSET(LblTelemetry_typ, ExpectedPackets.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExpectedPackets.Value)},

	{"PACKET_CREATION_SCLK",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, PacketCreationSclk.Value),
		LBL_OFFSET(LblTelemetry_typ, PacketCreationSclk.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PacketCreationSclk.Value)},

	{"PACKET_MAP_MASK",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, PacketMapMask.Value),
		LBL_OFFSET(LblTelemetry_typ, PacketMapMask.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PacketMapMask.Value)},

	{"PACKET_SEQUENCE_NUMBER",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, PacketSequenceNumber.Value),
		LBL_OFFSET(LblTelemetry_typ, PacketSequenceNumber.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PacketSequenceNumber.Value)},

	{"RECEIVED_PACKETS",			"INT",		LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, ReceivedPackets.Value),
		LBL_OFFSET(LblTelemetry_typ, ReceivedPackets.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReceivedPackets.Value)},

	{"SOFTWARE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SoftwareName.Value),
		LBL_OFFSET(LblTelemetry_typ, SoftwareName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SoftwareName.Value)},

	{"SOFTWARE_VERSION_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SoftwareVersionId.Value),
		LBL_OFFSET(LblTelemetry_typ, SoftwareVersionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SoftwareVersionId.Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[0].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[0].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[1].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[1].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[2].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[2].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[3].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[3].Value)},

	{"SPICE_FILE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[4].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileId[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileId[4].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[0].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[0].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[1].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[1].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[2].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[2].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[3].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[3].Value)},

	{"SPICE_FILE_NAME",			"STRING",	LBL_REQUIRED,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[4].Value),
		LBL_OFFSET(LblTelemetry_typ, SpiceFileName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(SpiceFileName[4].Value)},

	{"TELEMETRY_PROVIDER_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderId.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetryProviderId.Value)},

	{"TELEMETRY_PROVIDER_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderType.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetryProviderType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetryProviderType.Value)},

	{"TELEMETRY_SOURCE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceName.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetrySourceName.Value)},

	{"TELEMETRY_SOURCE_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceType.Value),
		LBL_OFFSET(LblTelemetry_typ, TelemetrySourceType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TelemetrySourceType.Value)},

	{"TLM_CMD_DISCREPANCY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblTelemetry_typ, TlmCmdDiscrepancyFlag.Value),
		LBL_OFFSET(LblTelemetry_typ, TlmCmdDiscrepancyFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TlmCmdDiscrepancyFlag.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "TELEMETRY",
	LBL_NULL };

/******************************************************************************
 *				LBL_TELEMETRY
 *
 *****************************************************************************/
int     LblTelemetry(
  int   Unit,
  int   Obtain,
  LblTelemetry_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblTelemetry_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_TELEMETRY
 *
 *****************************************************************************/
void	LblPrintTelemetry(
  LblTelemetry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_TELEMETRY
 *
 *****************************************************************************/
void	LblTestTelemetry(
  LblTelemetry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_surface_model.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_surface_model.h"

/******************************************************************************
 *				LBL_SURFACE_MODEL
 *
 *	This module contains routines to help create, read/write and print
 *  a Surface Model property label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblSurfaceModel.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblSurfaceModel_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblSurfaceModel_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"SURFACE_MODEL_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelDesc.Value),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceModelDesc.Value)},

	{"SURFACE_MODEL_TYPE",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelType.Value),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceModelType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceModelType.Value)},

	{"SURFACE_NORMAL_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Value[0]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceNormalVector.Value[0])},

	{"SURFACE_NORMAL_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Value[1]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceNormalVector.Value[1])},

	{"SURFACE_NORMAL_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Value[2]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceNormalVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceNormalVector.Value[2])},


	{"SURFACE_GROUND_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Value[0]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGroundLocation.Value[0])},

	{"SURFACE_GROUND_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Value[1]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGroundLocation.Value[1])},

	{"SURFACE_GROUND_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Value[2]),
		LBL_OFFSET(LblSurfaceModel_typ, SurfaceGroundLocation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGroundLocation.Value[2])},



	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SURFACE_MODEL
 *
 *****************************************************************************/
int     LblSurfaceModel(
  int   Unit,
  int   Obtain,
  LblSurfaceModel_typ      *LabelItems,
  int	Instance)
{
  LblSetSurfaceModel("SURFACE_MODEL");
  return (LblSurfaceModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SURFACE_MODEL_PARMS
 *
 *****************************************************************************/
int     LblSurfaceModelParms(
  int   Unit,
  int   Obtain,
  LblSurfaceModel_typ      *LabelItems,
  int	Instance)
{
  LblSetSurfaceModel("SURFACE_MODEL_PARMS");
  return (LblSurfaceModelApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SET_SURFACE_MODEL
 *
 *****************************************************************************/
void     LblSetSurfaceModel(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_SURFACE_MODEL_API
 *
 *****************************************************************************/
int     LblSurfaceModelApi(
  int   Unit,
  int   Obtain,
  LblSurfaceModel_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblSurfaceModel_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_SURFACE_MODEL
 *
 *****************************************************************************/
void	LblPrintSurfaceModel(
  LblSurfaceModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SURFACE_MODEL
 *
 *****************************************************************************/
void	LblTestSurfaceModel(
  LblSurfaceModel_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_surface_projection.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_surface_projection.h"

/******************************************************************************
 *				LBL_SURFACE_PROJECTION
 *
 *	This module contains routines to help create, read/write and print
 *  a Surface Projection property label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblSurfaceProjection.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblSurfaceProjection_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"LINE_CAMERA_MODEL_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineCameraModelOffset.Value)},

	{"LINE_CAMERA_MODEL_OFFSET__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineCameraModelOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineCameraModelOffsetUnit.Value)},

	{"LINE_PROJECTION_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineProjectionOffset.Value)},

	{"LINE_PROJECTION_OFFSET__UNIT",	"STRING",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, LineProjectionOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineProjectionOffsetUnit.Value)},

	{"MAP_PROJECTION_DESC",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionDesc.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionDesc.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionDesc.Value)},

	{"MAP_PROJECTION_NOTE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionNote.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionNote.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionNote.Value)},

	{"MAP_PROJECTION_TYPE",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionType.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapProjectionType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionType.Value)},

	{"MAP_RESOLUTION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	2,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolution.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolution.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolution.Value)},

	{"MAP_RESOLUTION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[0].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolutionUnit[0].Value)},

	{"MAP_RESOLUTION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[1].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapResolutionUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolutionUnit[1].Value)},

	{"MAP_SCALE",				"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale.Value[0]),
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScale.Value[0])},

	{"MAP_SCALE",				"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale.Value[1]),
		LBL_OFFSET(LblSurfaceProjection_typ, MapScale.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScale.Value[1])},

	{"MAP_SCALE__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MapScaleUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MapScaleUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScaleUnit.Value)},

	{"MAXIMUM_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevation.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MaximumElevation.Value)},

	{"MAXIMUM_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevationUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MaximumElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MaximumElevationUnit.Value)},

	{"MINIMUM_ELEVATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevation.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MinimumElevation.Value)},

	{"MINIMUM_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevationUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, MinimumElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MinimumElevationUnit.Value)},

	{"PROJECTION_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionAzimuth.Value)},

	{"PROJECTION_AZIMUTH__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionAzimuthUnit.Value)},

	{"PROJECTION_ELEVATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevation.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevation.Value)},

	{"PROJECTION_ELEVATION__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevationUnit.Value)},

	{"PROJECTION_ELEVATION_LINE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLine.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevationLine.Value)},

	{"PROJECTION_ELEVATION_LINE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLineUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionElevationLineUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionElevationLineUnit.Value)},

	{"PROJECTION_ORIGIN_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Value[0]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVector.Value[0])},

	{"PROJECTION_ORIGIN_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Value[1]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVector.Value[1])},

	{"PROJECTION_ORIGIN_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Value[2]),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVector.Value[2])},

	{"PROJECTION_ORIGIN_VECTOR__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVectorUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ProjectionOriginVectorUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProjectionOriginVectorUnit.Value)},

	{"REFERENCE_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceAzimuth.Value)},

	{"REFERENCE_AZIMUTH__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceAzimuthUnit.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{"SAMPLE_CAMERA_MODEL_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleCameraModelOffset.Value)},

	{"SAMPLE_CAMERA_MODEL_OFFSET__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleCameraModelOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleCameraModelOffsetUnit.Value)},

	{"SAMPLE_PROJECTION_OFFSET",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffset.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleProjectionOffset.Value)},

	{"SAMPLE_PROJECTION_OFFSET__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffsetUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SampleProjectionOffsetUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleProjectionOffsetUnit.Value)},

	{"START_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StartAzimuth.Value)},

	{"START_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StartAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StartAzimuthUnit.Value)},

	{"STOP_AZIMUTH",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuth.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StopAzimuth.Value)},

	{"STOP_AZIMUTH__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuthUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, StopAzimuthUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(StopAzimuthUnit.Value)},

	{"SURFACE_GEOMETRY_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, SurfaceGeometryId.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, SurfaceGeometryId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SurfaceGeometryId.Value)},

	{"X_AXIS_MAXIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMaximum.Value)},

	{"X_AXIS_MAXIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMaximumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMaximumUnit.Value)},

	{"X_AXIS_MINIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMinimum.Value)},

	{"X_AXIS_MINIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, XAxisMinimumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(XAxisMinimumUnit.Value)},

	{"Y_AXIS_MAXIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMaximum.Value)},

	{"Y_AXIS_MAXIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMaximumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMaximumUnit.Value)},

	{"Y_AXIS_MINIMUM",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimum.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimum.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMinimum.Value)},

	{"Y_AXIS_MINIMUM__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimumUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, YAxisMinimumUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(YAxisMinimumUnit.Value)},

	{"ZERO_ELEVATION_LINE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLine.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLine.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ZeroElevationLine.Value)},

	{"ZERO_ELEVATION_LINE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLineUnit.Value),
		LBL_OFFSET(LblSurfaceProjection_typ, ZeroElevationLineUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ZeroElevationLineUnit.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_SURFACE_PROJECTION
 *
 *****************************************************************************/
void     LblSetSurfaceProjection(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_SURFACE_PROJECTION
 *
 *****************************************************************************/
int     LblSurfaceProjection(
  int   Unit,
  int   Obtain,
  LblSurfaceProjection_typ      *LabelItems,
  int Instance)
{
  LblSetSurfaceProjection("SURFACE_PROJECTION");
  return (LblSurfaceProjectionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SURFACE_PROJECTION_PARMS
 *
 *****************************************************************************/
int     LblSurfaceProjectionParms(
  int   Unit,
  int   Obtain,
  LblSurfaceProjection_typ      *LabelItems,
  int Instance)
{
  LblSetSurfaceProjection("SURFACE_PROJECTION_PARMS");
  return (LblSurfaceProjectionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_SURFACE_PROJECTION_API
 *
 *****************************************************************************/
int     LblSurfaceProjectionApi(
  int   Unit,
  int   Obtain,
  LblSurfaceProjection_typ      *LabelItems,
  int Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblSurfaceProjection_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_SURFACE_PROJECTION
 *
 *****************************************************************************/
void	LblPrintSurfaceProjection(
  LblSurfaceProjection_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SURFACE_PROJECTION
 *
 *****************************************************************************/
void	LblTestSurfaceProjection(
  LblSurfaceProjection_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_articulation.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_articulation.h"

/******************************************************************************
 *				LBL_ARTICULATION
 *
 *	This module contains routines to help create, read/write and print
 *  a Articulation property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_articulation.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblArticulation.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 2003-01-13   P. Zamani       Added ArticulationDevInstrumentId
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblArticulation_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, SolutionId.Value),
		LBL_OFFSET(LblArticulation_typ, SolutionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolutionId.Value)},

	{"ARTICULATION_DEVICE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceId.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceId.Value)},

	{"ARTICULATION_DEVICE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceName.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceName.Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[0].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[1].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[2].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[3].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[4].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[5].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[6].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[7].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[8].Value)},

	{"ARTICULATION_DEVICE_ANGLE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngle[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngle[9].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[0].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[1].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[2].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[3].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[4].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[5].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[6].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[7].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[8].Value)},

	{"ARTICULATION_DEVICE_ANGLE__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleUnit[9].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[0].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[1].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[2].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[3].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[4].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[5].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[6].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[7].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[8].Value)},

	{"ARTICULATION_DEVICE_ANGLE_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceAngleName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceAngleName[9].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[0].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[1].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[2].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[3].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[4].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[5].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[6].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[7].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[8].Value)},

	{"ARTICULATION_DEVICE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCount[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCount[9].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[0].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[1].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[2].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[3].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[4].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[5].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[6].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[7].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[8].Value)},

	{"ARTICULATION_DEVICE_COUNT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountUnit[9].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[0].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[1].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[2].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[3].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[4].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[5].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[6].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[7].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[8].Value)},

	{"ARTICULATION_DEVICE_COUNT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceCountName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceCountName[9].Value)},

/**/

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[0].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[1].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[2].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[3].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[4].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[5].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[6].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[7].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[8].Value)},

	{"ARTICULATION_DEV_LOCATION",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocation[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocation[9].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[0].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[1].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[2].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[3].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[4].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[5].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[6].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[7].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[8].Value)},

	{"ARTICULATION_DEV_LOCATION__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationUnit[9].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[0].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[1].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[2].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[3].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[4].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[5].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[6].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[7].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[8].Value)},

	{"ARTICULATION_DEV_LOCATION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevLocationName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevLocationName[9].Value)},

/**/

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[0].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[1].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[2].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[3].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[4].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[5].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[6].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[7].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[8].Value)},

	{"ARTICULATION_DEV_ORIENT",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrient[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrient[9].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[0].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[1].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[2].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[3].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[4].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[5].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[6].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[7].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[8].Value)},

	{"ARTICULATION_DEV_ORIENT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientUnit[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientUnit[9].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[0].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[1].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[2].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[3].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[4].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[5].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[6].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[7].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[8].Value)},

	{"ARTICULATION_DEV_ORIENT_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevOrientName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevOrientName[9].Value)},

	{"ARTICULATION_DEV_POSITION",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition[0].Value)},

	{"ARTICULATION_DEV_POSITION",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition[1].Value)},

	{"ARTICULATION_DEV_POSITION",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition[2].Value)},

	{"ARTICULATION_DEV_POSITION_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition_Id[0].Value)},

	{"ARTICULATION_DEV_POSITION_ID",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition_Id[1].Value)},

	{"ARTICULATION_DEV_POSITION_ID",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPosition_Id[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPosition_Id[2].Value)},

	{"ARTICULATION_DEV_POSITION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPositionName[0].Value)},

	{"ARTICULATION_DEV_POSITION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPositionName[1].Value)},

	{"ARTICULATION_DEV_POSITION_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevPositionName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevPositionName[2].Value)},

	{"ARTICULATION_DEVICE_MODE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceMode.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceMode.Value)},

	{"ARTICULATION_DEVICE_TEMP",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTemp[0].Value)},

	{"ARTICULATION_DEVICE_TEMP",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTemp[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTemp[1].Value)},

	{"ARTICULATION_DEVICE_TEMP__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempUnit[0].Value)},

	{"ARTICULATION_DEVICE_TEMP__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempUnit[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempUnit[1].Value)},

	{"ARTICULATION_DEVICE_TEMP_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempName[0].Value)},

	{"ARTICULATION_DEVICE_TEMP_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceTempName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceTempName[1].Value)},

	{"ARTICULATION_DEV_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVector.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevVector.Value)},

	{"ARTICULATION_DEV_INSTRUMENT_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevInstrumentId.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevInstrumentId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevInstrumentId.Value)},

	{"ARTICULATION_DEV_VECTOR_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVectorName.Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDevVectorName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDevVectorName.Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[0].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[0].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[1].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[1].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[2].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[2].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[3].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[3].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[4].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[4].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[5].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[5].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[6].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[6].Value)},

	{"CONTACT_SENSOR_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[7].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorState[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorState[7].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[0].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[1].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[2].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[3].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[4].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[5].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[6].Value)},

	{"CONTACT_SENSOR_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ContactSensorStateName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ContactSensorStateName[7].Value)},

	{"ARTICULATION_DEV_INSTRUMENT_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, InstrumentId.Value),
		LBL_OFFSET(LblArticulation_typ, InstrumentId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentId.Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[0].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[1].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[2].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[3].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[4].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[5].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[6].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[7].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[8].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[9].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[10].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[10].Value)},

	{"ARTICULATION_DEVICE_STATE",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[11].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceState[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceState[11].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[0].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[0].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[1].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[1].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[2].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[2].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[3].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[3].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[4].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[4].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[5].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[5].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[6].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[6].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[7].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[7].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[8].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[8].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[9].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[9].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[10].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[10].Value)},

	{"ARTICULATION_DEVICE_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[11].Value),
		LBL_OFFSET(LblArticulation_typ, ArticulationDeviceStateName[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceStateName[11].Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",	LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_ARTICULATION
 *
 *****************************************************************************/
void     LblSetArticulation(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_ARTICULATION
 *
 *****************************************************************************/
int     LblArticulation(
  int   Unit,
  int   Obtain,
  LblArticulation_typ      *LabelItems,
  int	Instance)
{
  LblSetArticulation("ARTICULATION_STATE");
  return (LblArticulationApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_ARTICULATION_API
 *
 *****************************************************************************/
int     LblArticulationApi(
  int   Unit,
  int   Obtain,
  LblArticulation_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblArticulation_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_ARTICULATION
 *
 *****************************************************************************/
void	LblPrintArticulation(
  LblArticulation_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_ARTICULATION
 *
 *****************************************************************************/
void	LblTestArticulation(
  LblArticulation_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_coordinate.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_coordinate.h"

/******************************************************************************
 *				LBL_COORDINATE
 *
 *	This module contains routines to help create, read/write and print
 *  a Coordinate property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_articulation.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblCoordinate.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *============================================================================
 *      History of Modifications
 *
 * Date         who             Description
 * ----------   --------------- ---------------------------------------------
 * 2003-01-07   P. Zamani       Changed SOLUTION_ID to be LBL_OPTIONAL
 * ?            A. Runkle       Initial development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblCoordinate_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOLUTION_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, SolutionId.Value),
		LBL_OFFSET(LblCoordinate_typ, SolutionId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SolutionId.Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[0].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[0].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[1].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[1].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[2].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[2].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[3].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[3].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[4].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[4].Value)},

	{"COORDINATE_SYSTEM_INDEX",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[5].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndex[5].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[0].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[0].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[1].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[1].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[2].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[2].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[3].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[3].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[4].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[4].Value)},

	{"COORDINATE_SYSTEM_INDEX_NAME",	"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[5].Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemIndexName[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemIndexName[5].Value)},

	{"COORDINATE_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemName.Value),
		LBL_OFFSET(LblCoordinate_typ, CoordinateSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemName.Value)},

	{"ORIGIN_OFFSET_VECTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, OriginOffsetVector.Value),
		LBL_OFFSET(LblCoordinate_typ, OriginOffsetVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OriginOffsetVector.Value)},

	{"ORIGIN_ROTATION_QUATERNION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	4,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, OriginRotationQuaternion.Value),
		LBL_OFFSET(LblCoordinate_typ, OriginRotationQuaternion.Valid),
		LBL_NO_RETURN,	LBL_SIZE(OriginRotationQuaternion.Value)},

	{"POSITIVE_AZIMUTH_DIRECTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, PositiveAzimuthDirection.Value),
		LBL_OFFSET(LblCoordinate_typ, PositiveAzimuthDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveAzimuthDirection.Value)},

	{"POSITIVE_ELEVATION_DIRECTION",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, PositiveElevationDirection.Value),
		LBL_OFFSET(LblCoordinate_typ, PositiveElevationDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveElevationDirection.Value)},

	{"QUATERNION_MEASUREMENT_METHOD",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, QuaternionMeasurementMethod.Value),
		LBL_OFFSET(LblCoordinate_typ, QuaternionMeasurementMethod.Valid),
		LBL_NO_RETURN,	LBL_SIZE(QuaternionMeasurementMethod.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblCoordinate_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",	LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_COORDINATE
 *
 *****************************************************************************/
void     LblSetCoordinate(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_COORDINATE
 *
 *****************************************************************************/
int     LblCoordinate(
  int   Unit,
  int   Obtain,
  LblCoordinate_typ      *LabelItems,
  int	Instance)
{
  LblSetCoordinate( "COORDINATE_SYSTEM" );
  return (LblCoordinateApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_COORDINATE_API
 *
 *****************************************************************************/
int     LblCoordinateApi(
  int   Unit,
  int   Obtain,
  LblCoordinate_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblCoordinate_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_COORDINATE
 *
 *****************************************************************************/
void	LblPrintCoordinate(
  LblCoordinate_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_COORDINATE
 *
 *****************************************************************************/
void	LblTestCoordinate(
  LblCoordinate_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_ground_support.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_ground_support.h"

/******************************************************************************
 *				LBL_GROUND_SUPPORT
 *
 *	This module contains routines to help create, read/write and print
 *  a Ground Support property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_ground_support.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblGroundSupport.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblGroundSupport_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"CAMERA_LOCATION_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, CameraLocationId.Value),
		LBL_OFFSET(LblGroundSupport_typ, CameraLocationId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CameraLocationId.Value)},

	{"FACILITY_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, FacilityName.Value),
		LBL_OFFSET(LblGroundSupport_typ, FacilityName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FacilityName.Value)},

	{"LIGHT_SOURCE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceName.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceName.Value)},

	{"LIGHT_SOURCE_DISTANCE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistance.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceDistance.Value)},

	{"LIGHT_SOURCE_DISTANCE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistanceUnit.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceDistanceUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceDistanceUnit.Value)},

	{"LIGHT_SOURCE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, LightSourceType.Value),
		LBL_OFFSET(LblGroundSupport_typ, LightSourceType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LightSourceType.Value)},

	{"PRESSURE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, Pressure.Value),
		LBL_OFFSET(LblGroundSupport_typ, Pressure.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Pressure.Value)},

	{"PRODUCER_FULL_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, ProducerFullName.Value),
		LBL_OFFSET(LblGroundSupport_typ, ProducerFullName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProducerFullName.Value)},

	{"TARGET_DISTANCE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TargetDistance.Value),
		LBL_OFFSET(LblGroundSupport_typ, TargetDistance.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetDistance.Value)},

	{"TARGET_DISTANCE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TargetDistanceUnit.Value),
		LBL_OFFSET(LblGroundSupport_typ, TargetDistanceUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetDistanceUnit.Value)},

	{"TARGET_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TargetName.Value),
		LBL_OFFSET(LblGroundSupport_typ, TargetName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TargetName.Value)},

	{"TEST_PHASE_NAME",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, TestPhaseName.Value),
		LBL_OFFSET(LblGroundSupport_typ, TestPhaseName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(TestPhaseName.Value)},

	{"NOTE",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblGroundSupport_typ, Note.Value),
		LBL_OFFSET(LblGroundSupport_typ, Note.Valid),
		LBL_NO_RETURN,	LBL_SIZE(Note.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"GROUND_SUPPORT_EQUIPMENT",	LBL_NULL };

/******************************************************************************
 *				LBL_GROUND_SUPPORT
 *
 *****************************************************************************/
int     LblGroundSupport(
  int   Unit,
  int   Obtain,
  LblGroundSupport_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblGroundSupport_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_GROUND_SUPPORT
 *
 *****************************************************************************/
void	LblPrintGroundSupport(
  LblGroundSupport_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_GROUND_SUPPORT
 *
 *****************************************************************************/
void	LblTestGroundSupport(
  LblGroundSupport_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_derived_image.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_derived_image.h"

/******************************************************************************
 *				LBL_DERIVED_IMAGE
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Image property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_data.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblDerivedImage.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 2003-11-12   H. Lee          Added RADIANCE_OFFSET__UNIT
 * 2003-11-10   H. Lee          Added RADIANCE_SCALING_FACTOR__UNIT
 * 2003-06-17   H. Lee          Added CONFIGURATION_BAND_ID & INSTRUMENT_BAND_ID
 * 2003-05-20   H. Lee          Changed RADIANCE_SCALE_FACTOR to
 *                              RADIANCE_SCALING_FACTOR
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblDerivedImage_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[0].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[1].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[2].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[3].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      5,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[4].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[4].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[4].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      6,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[5].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[5].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[5].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      7,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[6].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[6].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[6].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      8,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[7].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[7].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[7].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      9,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[8].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[8].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[8].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     10,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[9].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[9].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[9].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     11,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[10].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[10].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[10].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     12,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[11].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[11].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[11].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     13,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[12].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[12].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[12].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     14,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[13].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[13].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[13].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     15,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[14].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[14].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[14].Value)},

	{"CONFIGURATION_BAND_ID",               "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     16,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[15].Value),
                LBL_OFFSET(LblDerivedImage_typ, ConfigurationBandId[15].Valid),
                LBL_NO_RETURN,  LBL_SIZE(ConfigurationBandId[15].Value)},

	{"DERIVED_IMAGE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, DerivedImageType.Value),
		LBL_OFFSET(LblDerivedImage_typ, DerivedImageType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(DerivedImageType.Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[0].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[0].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      2,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[1].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[1].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      3,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[2].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[2].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      4,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[3].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[3].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[3].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      5,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[4].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[4].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[4].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      6,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[5].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[5].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[5].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      7,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[6].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[6].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[6].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      8,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[7].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[7].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[7].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,      9,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[8].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[8].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[8].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     10,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[9].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[9].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[9].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     11,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[10].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[10].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[10].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     12,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[11].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[11].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[11].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     13,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[12].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[12].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[12].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     14,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[13].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[13].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[13].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     15,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[14].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[14].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[14].Value)},

	{"INSTRUMENT_BAND_ID",                  "STRING",       LBL_OPTIONAL,
                LBL_CONTINUE,   1,     16,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[15].Value),
                LBL_OFFSET(LblDerivedImage_typ, InstrumentBandId[15].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentBandId[15].Value)},   

	{"RADIANCE_OFFSET",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RadianceOffset.Value),
		LBL_OFFSET(LblDerivedImage_typ, RadianceOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceOffset.Value)},

        {"RADIANCE_OFFSET__UNIT",               "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, RadianceOffsetUnit.Value),
                LBL_OFFSET(LblDerivedImage_typ, RadianceOffsetUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceOffsetUnit.Value)},

	{"RADIANCE_SCALING_FACTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactor.Value),
		LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactor.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadianceScaleFactor.Value)},

        {"RADIANCE_SCALING_FACTOR__UNIT",       "STRING",       LBL_OPTIONAL,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactorUnit.Value),
                LBL_OFFSET(LblDerivedImage_typ, RadianceScaleFactorUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(RadianceScaleFactorUnit.Value)},

	{"RADIOMETRIC_CORRECTION_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RadiometricCorrectionType.Value),
		LBL_OFFSET(LblDerivedImage_typ, RadiometricCorrectionType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RadiometricCorrectionType.Value)},

	{"RANGE_ORIGIN_VECTOR",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, RangeOriginVector.Value),
		LBL_OFFSET(LblDerivedImage_typ, RangeOriginVector.Valid),
		LBL_NO_RETURN,	LBL_SIZE(RangeOriginVector.Value)},

	{"REFERENCE_COORD_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemName.Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemName.Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[0].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[0].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[1].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[1].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[2].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[2].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[3].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[3].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[4].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[4].Value)},

	{"REFERENCE_COORD_SYSTEM_INDEX",	"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[5].Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemIndex[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemIndex[5].Value)},

	{"REFERENCE_COORD_SYSTEM_SOLN_ID",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemSolnId.Value),
		LBL_OFFSET(LblDerivedImage_typ, ReferenceCoordSystemSolnId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ReferenceCoordSystemSolnId.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_DERIVED_IMAGE
 *
 *****************************************************************************/
void	LblSetDerivedImage(
  const char 	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_DERIVED_IMAGE
 *
 *****************************************************************************/
int     LblDerivedImage(
  int   Unit,
  int   Obtain,
  LblDerivedImage_typ      *LabelItems,
  int	Instance)
{
  LblSetDerivedImage("DERIVED_IMAGE");
  return (LblDerivedImageApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_DERIVED_IMAGE_PARMS
 *
 *****************************************************************************/
int     LblDerivedImageParms(
  int   Unit,
  int   Obtain,
  LblDerivedImage_typ      *LabelItems,
  int	Instance)
{
  LblSetDerivedImage("DERIVED_IMAGE_PARMS");
  return (LblDerivedImageApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_DERIVED_IMAGE_API
 *
 *****************************************************************************/
int     LblDerivedImageApi(
  int   Unit,
  int   Obtain,
  LblDerivedImage_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblDerivedImage_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_DERIVED_IMAGE
 *
 *****************************************************************************/
void	LblPrintDerivedImage(
  LblDerivedImage_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_DERIVED_IMAGE
 *
 *****************************************************************************/
void	LblTestDerivedImage(
  LblDerivedImage_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lbl_pdshistory.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_pdshistory.h"

/******************************************************************************
 *				LBL_PDSHISTORY
 *
 *	This module contains routines to help create, read/write and print
 *  an PdsHistory property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_pdshistory.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblPdsHistory.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *============================================================================
 *
 * History of modifications:
 *
 * Date         who             Description
 * -----------  --------------- ----------------------------------------------
 * 12-Feb-2003  Payam Zamani	Added RELEASE_ID
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblPdsHistory_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"PROCESSING_HISTORY_TEXT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblPdsHistory_typ, ProcessingHistoryText.Value),
		LBL_OFFSET(LblPdsHistory_typ, ProcessingHistoryText.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ProcessingHistoryText.Value)},

        {"SOFTWARE_NAME",                       "STRING",       LBL_REQUIRED,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblPdsHistory_typ, SoftwareName.Value),
                LBL_OFFSET(LblPdsHistory_typ, SoftwareName.Valid),
                LBL_NO_RETURN,  LBL_SIZE(SoftwareName.Value)},

        {"SOFTWARE_VERSION_ID",                 "STRING",       LBL_REQUIRED,
                LBL_NO_CONT,    1,      1,      LBL_NULL,
                LBL_OFFSET(LblPdsHistory_typ, SoftwareVersionId.Value),
                LBL_OFFSET(LblPdsHistory_typ, SoftwareVersionId.Valid),
                LBL_NO_RETURN,  LBL_SIZE(SoftwareVersionId.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
	
static LblApiProcess_typ	Label = {
	LabelTbl,	"PROPERTY",	"PROPERTY",	"PDS_HISTORY",
	LBL_NULL };

/******************************************************************************
 *				LBL_IDENTIFIER
 *
 *****************************************************************************/
int     LblPdsHistory(
  int   Unit,
  int   Obtain,
  LblPdsHistory_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblPdsHistory_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IDENTIFIER
 *
 *****************************************************************************/
void     LblPrintPdsHistory(
  LblPdsHistory_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IDENTIFIER
 *
 *****************************************************************************/
void     LblTestPdsHistory(
  LblPdsHistory_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lbl_routines.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE lbl_routines
/*
/*   To Create the build file give the command:
/*
/*		$ vimake lbl_routines			(VMS)
/*   or
/*		% vimake lbl_routines			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE lbl_routines		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/

#define MODULE_LIST lbl_gen_api.c lbl_camera_model.c lbl_command.c \
		    lbl_pdshistory.c lbl_compression.c lbl_derived_geometry.c \
		    lbl_identification.c lbl_image_data.c lbl_image_geometry.c
#define MODULE_LIST2 lbl_instrument_state.c lbl_surface_model.c \
		    lbl_surface_projection.c lbl_telemetry.c lbl_articulation.c \
		    lbl_coordinate.c lbl_ground_support.c lbl_derived_image.c

#define MAIN_LANG_C

#define USES_ANSI_C

/*#define LOCAL_INCLUDE -I.
#define LIB_LOCAL
#define LOCAL_LIBRARY `ar r sublib.a`
*/

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
/*#define DEBUG
#ifdef PROGRAM
#endif*/

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/*#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
*/
/***  End of local library definitions  ***/
/**********  End of lbl_routines imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tst_lbl_gen_api.c
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "return_status.h"
#include "lbl_gen_api.h"

typedef struct
	{
	LblApiStringItem_typ	label1;
	LblApiIntItem_typ	label2[4];
	LblApiRealItem_typ	label3;
	LblApiDoubleItem_typ	label4;
	char			KeywordItem[255];
	int			DummyValue;
	} LblExample_1_typ;

typedef struct
	{
	char	label1[256];
	int	label2[4];
	float	label3;
	double	label4;
        int	label1Valid;
        int	label2Valid;
        int	label3Valid;
        int	label4Valid;
        int	label1Element;
        int	label2Element;
        int	label3Element;
        int	label4Element;
	char			KeywordItem[255];
	} LblExample_2_typ;


/***  Function Prototypes  ***/
	void	read_labels( int );
	void	read_labels1( int );
	void	read_labels2( int );
	void	write_labels(int );
	void	write_labels1(int );
	void	write_labels2(int );

/*******************************************************************************
/*				MAIN
/*
/******************************************************************************/
int	main(
  int	argc,
  char	*argv[])
{ int	cnt,
	Line,
	OutUnit,
	status;
  char	Pixels[100];

  if (argc > 1) strcpy(Pixels,argv[1]);
  else strcpy(Pixels,"label_test.img");

  status = zvunit(&OutUnit,"NA",1,"U_NAME",Pixels,0);
  if (status != 1)
  { zvmessage("Could not open output file"," ");
    zabend();
  }

  status = zvopen(OutUnit, "OP","WRITE", "O_FORMAT","BYTE", "U_FORMAT","BYTE",
                  "OPEN_ACT","AS", "U_NL",10, "U_NS",10, "U_NB",1,  0);
  zvsignal(OutUnit, status, 1);

  printf("***\n***\t Writing Labels\n***\n");

  write_labels(OutUnit);

  for (Line=1; Line<=10; Line++)
      status =  zvwrit(OutUnit, Pixels, 0);
  zvclose(OutUnit, 0);

  status = zvopen(OutUnit, "OP","READ", "OPEN_ACT","AS",  0);
  zvsignal(OutUnit, status, 1);

  printf("***\n***\t Reading Labels\n***\n");
  read_labels(OutUnit);
  zvclose(OutUnit, 0);

  return 0;
}

/******************************************************************************
/*				LBL_EXAMPLE_1
/*
/*****************************************************************************/
int	LblExample_1(
  int	Unit,
  int	Obtain,
  LblExample_1_typ	*LabelItems)
{ int	RtnStatus;
  LblApiCntrl_typ	Cntrl;
  static LblApiElement_typ	LabelTbl[] = {

	{"KEYWORD1",	"STRING",	1,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label1.Value),
	 LBL_OFFSET(LblExample_1_typ,label1.Valid),
/***
	 LBL_NO_RETURN},
**/
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[0].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[0].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	1,	1,	2,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[1].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[1].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	1,	1,	3,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[2].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[2].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	1,	1,	4,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[3].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[3].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD3,KEYWORD3A",	"REAL",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label3.Value),
	 LBL_OFFSET(LblExample_1_typ,label3.Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD4",	"DOUB",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label4.Value),
	 LBL_OFFSET(LblExample_1_typ,label4.Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} };

  LblApiProcess_typ	Label = {
	LabelTbl,	"PROPERTY",	"PROPERTY",	"EXAMPLE_1_PROPERTY",
	NULL };

  LabelTbl[2].KeywordUsed = LabelItems->KeywordItem,
  Label.Buffer = (void *)LabelItems;

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;
  
  RtnStatus = LblProcessor(&Cntrl, &Label);
  if (RTN_FAILURE(RtnStatus))
  { printf("\n\nLabel Type: %s\n",Label.Type);
    printf("%s Name: %s\n",Label.NameKeyword,Label.NameValue);
    printf("Label Processing failed: %s\n\t'%s'\n",
           RTN_DFLT_MSG(RtnStatus),Cntrl.ErrMsgBuf);
  }
  PrintLabelElements(&Label);

  return (RtnStatus);
}

/******************************************************************************
/*				LBL_EXAMPLE_2
/*
/*****************************************************************************/
int	LblExample_2(
  int	Unit,
  int	Obtain,
  LblExample_2_typ	*LabelItems)
{ int	RtnStatus;
  LblApiCntrl_typ	Cntrl;
  static LblApiElement_typ	LabelTbl[] = {

	{"KEYWORD1",	"STRING",	0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label1),
	 LBL_OFFSET(LblExample_2_typ,label1Valid),
	 LBL_OFFSET(LblExample_2_typ,label1Element)},
	 
	{"KEYWORD2",	"INT",		0,	0,	4,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label2),
	 LBL_OFFSET(LblExample_2_typ,label2Valid),
	 LBL_OFFSET(LblExample_2_typ,label2Element)},
	 
	{"KEYWORD3",	"REAL",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label3),
	 LBL_OFFSET(LblExample_2_typ,label3Valid),
	 LBL_OFFSET(LblExample_2_typ,label3Element)},
	 
	{"KEYWORD4,KEYWORD4A",	"DOUB",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label4),
	 LBL_OFFSET(LblExample_2_typ,label4Valid),
	 LBL_OFFSET(LblExample_2_typ,label4Element)},
	 
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} };

  LblApiProcess_typ	Label = {
	LabelTbl,	"HISTORY",	"HIST",		"TASK",
	NULL };

  LabelTbl[3].KeywordUsed = LabelItems->KeywordItem;	/* 4th Item */
  Label.Buffer = (void *)LabelItems;

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;
  
  RtnStatus = LblProcessor(&Cntrl, &Label);
  if (RTN_FAILURE(RtnStatus))
  { printf("\n\nLabel Type: %s\n",Label.Type);
    printf("%s Name: %s\n",Label.NameKeyword,Label.NameValue);
    printf("Label Processing failed: %d '%s'\n",
           RTN_NUMBER(RtnStatus),Cntrl.ErrMsgBuf);
  } else PrintLabelElements(&Label);

  return (RtnStatus);
}

/*******************************************************************************
/*				write_labels
/*
/******************************************************************************/
void	write_labels(
  int	Unit)
{
  LblSetVersion("V2");
  write_labels1(Unit);
  LblSetVersion("V1");
  write_labels2(Unit);
  return;
}

/*******************************************************************************
/*				read_labels
/*
/******************************************************************************/
void	read_labels(
  int	Unit)
{
  read_labels1(Unit);
  read_labels2(Unit);
  return;
}

/*******************************************************************************
/*				write_labels1
/*
/******************************************************************************/
void	write_labels1(
  int	Unit)
{ int	lc;
  LblExample_1_typ	Test;

  memset(&Test,0,sizeof(LblExample_1_typ));

  strcpy(Test.label1.Value,"This is a test String");
  for (lc=1234; lc<1238; lc++) Test.label2[lc-1234].Value = lc*2;
  Test.label3.Value = 1234.56;
  Test.label4.Value = 1.2e34;

  Test.label1.Valid = LBL_VALID;
  Test.label2[0].Valid = LBL_VALID;
  Test.label2[1].Valid = LBL_VALID;
  Test.label2[2].Valid = LBL_VALID;
  Test.label2[3].Valid = LBL_VALID;
  Test.label3.Valid = LBL_VALID;
  Test.label4.Valid = LBL_VALID;

  Test.label2[1].Valid = LBL_PDS_NULL;
  Test.label2[3].Valid = LBL_PDS_UNK;
/***
  Test.label2[3].Valid = LBL_PDS_NA;
***/

  LblExample_1(Unit,LBL_FALSE,&Test);
  return;
}

/*******************************************************************************
/*				write_labels2
/*
/******************************************************************************/
void	write_labels2(
  int	Unit)
{ int	lc;
  LblExample_2_typ	Test;

  memset(&Test,0,sizeof(LblExample_2_typ));

  strcpy(Test.label1,"This is a test String");
  for (lc=0; lc<4; lc++) Test.label2[lc] = lc*2;
  Test.label3 = 1234.56;
  Test.label4 = 1.2e34;

  Test.label1Valid = LBL_VALID;
  Test.label2Valid = LBL_VALID;
  Test.label3Valid = LBL_VALID;
  Test.label4Valid = LBL_VALID;

/***
***/
  Test.label2Valid = LBL_PDS_NA;

  LblExample_2(Unit,LBL_FALSE,&Test);
  return;
}

/*******************************************************************************
/*				read_labels1
/*
/******************************************************************************/
void	read_labels1(
  int	Unit)
{ int	lc,
	status;
  LblExample_1_typ	Test;

  memset(&Test,0,sizeof(LblExample_2_typ));

  status = LblExample_1(Unit,LBL_TRUE,&Test);

  return;
}

/*******************************************************************************
/*				read_labels2
/*
/******************************************************************************/
void	read_labels2(
  int	Unit)
{ int	lc;
  LblExample_2_typ	Test;

  memset(&Test,0,sizeof(LblExample_2_typ));

  LblExample_2(Unit,LBL_TRUE,&Test);
}
$!-----------------------------------------------------------------------------
$ create tst_lbl_gen_api.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_lbl_gen_api
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_lbl_gen_api			(VMS)
/*   or
/*		% vimake tst_lbl_gen_api			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_lbl_gen_api		/* Only one of these */
/*#define PROCEDURE tst_lbl_gen_api		/* Only one of these */
/*#define SCRIPT tst_lbl_gen_api		/* Only one of these */
#define PROGRAM tst_lbl_gen_api		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_lbl_gen_api.c


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
/***  Deal with the fact that the SUN OS C Compiler is not TRUE ANSI  ***/
#define USES_ANSI_C
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

#ifdef SUBROUTINE
#define MARS_SUBLIB
#endif

/***  Defines required for both Programs and Subroutines  ***/
#define LIB_P2SUB

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
/*#define DEBUG
#ifdef PROGRAM
#define LIB_LOCAL
#endif*/

/*#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif*/
/***  End of local library definitions  ***/
/**********  End of tst_lbl_gen_api imake file  **********/
$!-----------------------------------------------------------------------------
$ create tst_lbl_routines.c
/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "return_status.h"
#include "lbl_gen_api.h"

#include "lbl_articulation.h"
#include "lbl_camera_model.h"
#include "lbl_command.h"
#include "lbl_compression.h"
#include "lbl_coordinate.h"
#include "lbl_derived_geometry.h"
#include "lbl_derived_image.h"
#include "lbl_ground_support.c"
#include "lbl_identification.h"
#include "lbl_image_data.h"
#include "lbl_image_geometry.h"
#include "lbl_instrument_state.h"
#include "lbl_surface_model.h"
#include "lbl_surface_projection.h"
#include "lbl_telemetry.h"

typedef	struct	{
		char	*Name;
		int	(*Process)();
		void	(*Load)();
		void	(*Print)();
		void	*Items;
		} TestTable_typ;

LblArticulation_typ	Articulation;
LblCameraModel_typ 	CameraModel;
LblCommand_typ 		Command;
LblCompression_typ	Compression;
LblCoordinate_typ	Coordinate;
LblDerivedGeometry_typ 	DerivedGeometry;
LblDerivedImage_typ 	DerivedImage;
LblGroundSupport_typ	GroundSupport;
LblIdentification_typ	Identification;
LblImageData_typ 	ImageData;
LblImageGeometry_typ 	ImageGeometry;
LblInstrumentState_typ 	InstrumentState;
LblSurfaceModel_typ	SurfaceModel;
LblTelemetry_typ 	Telemetry;
LblSurfaceProjection_typ	SurfaceProjection;

/***  TestTable_typ Labels[15];  ****/
/****  Quicker way, but Alpha VMS doesn't like it  ***/
TestTable_typ Labels[] = {
		{ "COMMAND",
		  *LblCommand,
                  *LblTestCommand,
                  *LblPrintCommand,
		  &Command },
		{ "COMPRESSION",
		  *LblCompression,
                  *LblTestCompression,
                  *LblPrintCompression,
		  &Compression },
		{ "COMPRESSION_PARMS",
		  *LblCompressionParms,
                  *LblTestCompression,
                  *LblPrintCompression,
		  &Compression },
		{ "CAMERA_MODEL",
		  *LblCameraModel,
                  *LblTestCameraModel,
                  *LblPrintCameraModel,
		  &CameraModel },
		{ "GEOMETRIC_CAMERA",
		  *LblGeometricCameraModel,
                  *LblTestCameraModel,
                  *LblPrintCameraModel,
		  &CameraModel },
		{ "DERIVED_GEOMETRY",
		  *LblDerivedGeometry,
                  *LblTestDerivedGeometry,
                  *LblPrintDerivedGeometry,
		  &DerivedGeometry },
		{ "IDENTIFICATION",
		  *LblIdentification,
                  *LblTestIdentification,
                  *LblPrintIdentification,
		  &Identification },
		{ "IMAGE_DATA",
		  *LblImageData,
                  *LblTestImageData,
                  *LblPrintImageData,
		  &ImageData },
		{ "IMAGE_GEOMETRY",
		  *LblImageGeometry,
                  *LblTestImageGeometry,
                  *LblPrintImageGeometry,
		  &ImageGeometry },
		{ "INSTRUMENT_STATE",
		  *LblInstrumentState,
                  *LblTestInstrumentState,
                  *LblPrintInstrumentState,
		  &InstrumentState },
		{ "INSTRUMENT_STATE_PARMS",
		  *LblInstrumentStateParms,
                  *LblTestInstrumentState,
                  *LblPrintInstrumentState,
		  &InstrumentState },
		{ "SURFACE_MODEL",
		  *LblSurfaceModel,
                  *LblTestSurfaceModel,
                  *LblPrintSurfaceModel,
		  &SurfaceModel },
		{ "SURFACE_MODEL_PARMS",
		  *LblSurfaceModelParms,
                  *LblTestSurfaceModel,
                  *LblPrintSurfaceModel,
		  &SurfaceModel },
		{ "SURFACE_PROJECTION",
		  *LblSurfaceProjection,
                  *LblTestSurfaceProjection,
                  *LblPrintSurfaceProjection,
		  &SurfaceProjection },
		{ "SURFACE_PROJECTION_PARMS",
		  *LblSurfaceProjectionParms,
                  *LblTestSurfaceProjection,
                  *LblPrintSurfaceProjection,
		  &SurfaceProjection },
		{ "TELEMETRY",
		  *LblTelemetry,
                  *LblTestTelemetry,
                  *LblPrintTelemetry,
		  &Telemetry },
		{ "ARTICULATION",
		  *LblArticulation,
                  *LblTestArticulation,
                  *LblPrintArticulation,
		  &Articulation },
		{ "COORDINATE",
		  *LblCoordinate,
                  *LblTestCoordinate,
                  *LblPrintCoordinate,
		  &Coordinate },
		{ "GROUND_SUPPORT",
		  *LblGroundSupport,
                  *LblTestGroundSupport,
                  *LblPrintGroundSupport,
		  &GroundSupport },
		{ "DERIVED_IMAGE",
		  *LblDerivedImage,
                  *LblTestDerivedImage,
                  *LblPrintDerivedImage,
		  &DerivedImage },
		{ "DERIVED_IMAGE_PARMS",
		  *LblDerivedImageParms,
                  *LblTestDerivedImage,
                  *LblPrintDerivedImage,
		  &DerivedImage },
		{ 0, 0, 0, 0, 0 }
              };
/***/

/*******************************************************************************
 *				MAIN
 *
 ******************************************************************************/
main(
  int	argc,
  char	*argv[])
{ int	cnt,
	Idx,
	Line,
	OutUnit,
	status;
  char	Pixels[100],
	Choice[128];;

  if (argc > 1)
  { if (strcmp(argv[1],"help") == 0 || strcmp(argv[1],"HELP") == 0)
    { printf("tst_lbl_routines [<file_name> [READ | WRITE [<property label>]]]\n");
      return 1;
    }
    strcpy(Pixels,argv[1]);
  } else strcpy(Pixels,"label_test.img");

  if (argc > 2) strcpy(Choice,argv[2]);
  else strcpy(Choice,"READ_WRITE");

  status = zvunit(&OutUnit,"NA",1,"U_NAME",Pixels,0);
  if (status != 1)
  { zvmessage("Could not open output file"," ");
    zabend();
  }

  if (strstr(Choice,"WRITE"))
  { status = zvopen(OutUnit, "OP","WRITE", "O_FORMAT","BYTE", "U_FORMAT","BYTE",
                    "OPEN_ACT","AS", "U_NL",10, "U_NS",10, "U_NB",1,  0);
    zvsignal(OutUnit, status, 1);

    for (Idx=0; Labels[Idx].Name; Idx++)
    { if (argc > 3 && !strstr(Labels[Idx].Name,argv[3])) continue;
      if (Labels[Idx].Load) Labels[Idx].Load(Labels[Idx].Items);
      status = Labels[Idx].Process(OutUnit,LBL_WRITE,Labels[Idx].Items,1);
      if (RTN_FAILURE(status))
         printf("Label Processing failed: %d\n%s\n",
                RTN_NUMBER(status), LblErrorMessage());
      if (Labels[Idx].Print) Labels[Idx].Print(Labels[Idx].Items);
    }

    for (Line=1; Line<=10; Line++)
        status =  zvwrit(OutUnit, Pixels, 0);
    zvclose(OutUnit, 0);
  }

  if (strstr(Choice,"READ"))
  { status = zvopen(OutUnit, "OP","READ", "OPEN_ACT","AS",  0);
    zvsignal(OutUnit, status, 1);

    for (Idx=0; Labels[Idx].Name; Idx++)
    { if (argc > 3 && !strstr(Labels[Idx].Name,argv[3])) continue;
      status = Labels[Idx].Process(OutUnit,LBL_READ,Labels[Idx].Items,1);
      if (RTN_FAILURE(status))
         printf("Label Processing failed: %d\n%s\n",
                RTN_NUMBER(status), LblErrorMessage());
      if (Labels[Idx].Print) Labels[Idx].Print(Labels[Idx].Items);
    }
    zvclose(OutUnit, 0);
  }


  return 1;
}

$!-----------------------------------------------------------------------------
$ create tst_lbl_routines.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE tst_lbl_routines
/*
/*   To Create the build file give the command:
/*
/*		$ vimake tst_lbl_routines			(VMS)
/*   or
/*		% vimake tst_lbl_routines			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
/*#define SUBROUTINE tst_lbl_routines		/* Only one of these */
/*#define PROCEDURE tst_lbl_routines		/* Only one of these */
/*#define SCRIPT tst_lbl_routines		/* Only one of these */
#define PROGRAM tst_lbl_routines		/* Only one of these */

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST tst_lbl_routines.c


#if defined (SUBROUTINE) || defined (PROGRAM)
/**  this contains compilable code  ***/
#define USES_ANSI_C
#else
/***  this contains 'scripts'  ***/
#define USES_CSH
#endif

#define LIB_LOCAL
#define LOCAL_INCLUDE -I.

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define MAIN_LANG_C
#define R2LIB
#define LIB_RTL
#define LIB_TAE
#endif

/***  Local library definitions ...
/***  ... must be commented out when delivered
/***
#define DEBUG
#ifdef PROGRAM
#endif


/***  Defines required for both Programs and Subroutines  ***/
/*#define LIB_P2SUB*/

/*#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif*/
/***  End of local library definitions  ***/
/**********  End of tst_lbl_routines imake file  **********/
$!-----------------------------------------------------------------------------
$ create test_lbl_routines.txt


Label Type: PROPERTY
PROPERTY Name: COMMAND

Keywords (Opt): AUTO_EXPOSURE_DATA_CUT
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): AUTO_EXPOSURE_PIXEL_FRACTION
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): BAD_PIXEL_REPLACEMENT_FLAG
        Format: STRING [1] (8)
        Values: AAAAAA

Keywords (Opt): COMMAND_DESC
        Format: STRING [1] (256)
        Values: BBBBBB

Keywords (Req): COMMAND_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): DARK_CURRENT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: DDDDDD

Keywords (Opt): DOWNLOAD_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): FLAT_FIELD_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: FFFFFF

Keywords (Opt): INSTRUMENT_MODE_ID
        Format: STRING [1] (48)
        Values: GGGGGG

Keywords (Opt): MAX_AUTO_EXPOS_ITERATION_COUNT
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): SHUTTER_EFFECT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: HHHHHH

Keywords (Opt): SQRT_COMPRESSION_FLAG
        Format: STRING [1] (8)
        Values: IIIIII



Label Type: PROPERTY
PROPERTY Name: COMPRESSION

Keywords (Opt): ERROR_PIXELS
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): INST_CMPRS_BLK_SIZE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): INST_CMPRS_BLOCKS
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): INST_CMPRS_DESC
        Format: STRING [1] (256)
        Values: AAAAAA

Keywords (Opt): INST_CMPRS_FILTER
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): INST_CMPRS_ENTROPY
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): INST_CMPRS_MODE
        Format: INT [1] (4)
        Values: 3

Keywords (Req): INST_CMPRS_NAME
        Format: STRING [1] (128)
        Values: CCCCCC

Keywords (Opt): INST_CMPRS_PARAM
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): INST_CMPRS_QUALITY
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_CMPRS_QUANTZ_TBL_ID
        Format: STRING [1] (48)
        Values: DDDDDD

Keywords (Opt): INST_CMPRS_QUANTZ_TYPE
        Format: STRING [1] (256)
        Values: EEEEEE

Keywords (Opt): INST_CMPRS_RATE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Req): INST_CMPRS_RATIO
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): INST_CMPRS_SEGMENTS
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INST_CMPRS_SEGMENT_STATUS
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ
  + (01 at 06): KKKKKK
  + (01 at 07): LLLLLL
  + (01 at 08): MMMMMM
  + (01 at 09): NNNNNN
  + (01 at 10): OOOOOO
  + (01 at 11): PPPPPP
  + (01 at 12): QQQQQQ
  + (01 at 13): RRRRRR
  + (01 at 14): SSSSSS
  + (01 at 15): TTTTTT
  + (01 at 16): UUUUUU
  + (01 at 17): VVVVVV
  + (01 at 18): WWWWWW
  + (01 at 19): XXXXXX
  + (01 at 20): YYYYYY
  + (01 at 21): ZZZZZZ
  + (01 at 22): aaaaaa
  + (01 at 23): bbbbbb
  + (01 at 24): cccccc
  + (01 at 25): dddddd
  + (01 at 26): eeeeee
  + (01 at 27): ffffff
  + (01 at 28): gggggg
  + (01 at 29): hhhhhh
  + (01 at 30): iiiiii
  + (01 at 31): jjjjjj
  + (01 at 32): kkkkkk

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE
        Format: INT [1] (4)
        Values: 7
  + (01 at 02): 8
  + (01 at 03): 9
  + (01 at 04): 10
  + (01 at 05): 11
  + (01 at 06): 12
  + (01 at 07): 13
  + (01 at 08): 14
  + (01 at 09): 15
  + (01 at 10): 16
  + (01 at 11): 17
  + (01 at 12): 18
  + (01 at 13): 19
  + (01 at 14): 20
  + (01 at 15): 21
  + (01 at 16): 22
  + (01 at 17): 23
  + (01 at 18): 24
  + (01 at 19): 25
  + (01 at 20): 26
  + (01 at 21): 27
  + (01 at 22): 28
  + (01 at 23): 29
  + (01 at 24): 30
  + (01 at 25): 31
  + (01 at 26): 32
  + (01 at 27): 33
  + (01 at 28): 34
  + (01 at 29): 35
  + (01 at 30): 36
  + (01 at 31): 37
  + (01 at 32): 38

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE_SAMP
        Format: INT [1] (4)
        Values: 39
  + (01 at 02): 40
  + (01 at 03): 41
  + (01 at 04): 42
  + (01 at 05): 43
  + (01 at 06): 44
  + (01 at 07): 45
  + (01 at 08): 46
  + (01 at 09): 47
  + (01 at 10): 48
  + (01 at 11): 49
  + (01 at 12): 50
  + (01 at 13): 51
  + (01 at 14): 52
  + (01 at 15): 53
  + (01 at 16): 54
  + (01 at 17): 55
  + (01 at 18): 56
  + (01 at 19): 57
  + (01 at 20): 58
  + (01 at 21): 59
  + (01 at 22): 60
  + (01 at 23): 61
  + (01 at 24): 62
  + (01 at 25): 63
  + (01 at 26): 64
  + (01 at 27): 65
  + (01 at 28): 66
  + (01 at 29): 67
  + (01 at 30): 68
  + (01 at 31): 69
  + (01 at 32): 70

Keywords (Opt): INST_CMPRS_SEG_LINES
        Format: INT [1] (4)
        Values: 71
  + (01 at 02): 72
  + (01 at 03): 73
  + (01 at 04): 74
  + (01 at 05): 75
  + (01 at 06): 76
  + (01 at 07): 77
  + (01 at 08): 78
  + (01 at 09): 79
  + (01 at 10): 80
  + (01 at 11): 81
  + (01 at 12): 82
  + (01 at 13): 83
  + (01 at 14): 84
  + (01 at 15): 85
  + (01 at 16): 86
  + (01 at 17): 87
  + (01 at 18): 88
  + (01 at 19): 89
  + (01 at 20): 90
  + (01 at 21): 91
  + (01 at 22): 92
  + (01 at 23): 93
  + (01 at 24): 94
  + (01 at 25): 95
  + (01 at 26): 96
  + (01 at 27): 97
  + (01 at 28): 98
  + (01 at 29): 99
  + (01 at 30): 100
  + (01 at 31): 101
  + (01 at 32): 102

Keywords (Opt): INST_CMPRS_SEG_SAMPLES
        Format: INT [1] (4)
        Values: 103
  + (01 at 02): 104
  + (01 at 03): 105
  + (01 at 04): 106
  + (01 at 05): 107
  + (01 at 06): 108
  + (01 at 07): 109
  + (01 at 08): 110
  + (01 at 09): 111
  + (01 at 10): 112
  + (01 at 11): 113
  + (01 at 12): 114
  + (01 at 13): 115
  + (01 at 14): 116
  + (01 at 15): 117
  + (01 at 16): 118
  + (01 at 17): 119
  + (01 at 18): 120
  + (01 at 19): 121
  + (01 at 20): 122
  + (01 at 21): 123
  + (01 at 22): 124
  + (01 at 23): 125
  + (01 at 24): 126
  + (01 at 25): 127
  + (01 at 26): 128
  + (01 at 27): 129
  + (01 at 28): 130
  + (01 at 29): 131
  + (01 at 30): 132
  + (01 at 31): 133
  + (01 at 32): 134

Keywords (Opt): INST_CMPRS_SEG_MISSING_PIXELS
        Format: INT [1] (4)
        Values: 135
  + (01 at 02): 136
  + (01 at 03): 137
  + (01 at 04): 138
  + (01 at 05): 139
  + (01 at 06): 140
  + (01 at 07): 141
  + (01 at 08): 142
  + (01 at 09): 143
  + (01 at 10): 144
  + (01 at 11): 145
  + (01 at 12): 146
  + (01 at 13): 147
  + (01 at 14): 148
  + (01 at 15): 149
  + (01 at 16): 150
  + (01 at 17): 151
  + (01 at 18): 152
  + (01 at 19): 153
  + (01 at 20): 154
  + (01 at 21): 155
  + (01 at 22): 156
  + (01 at 23): 157
  + (01 at 24): 158
  + (01 at 25): 159
  + (01 at 26): 160
  + (01 at 27): 161
  + (01 at 28): 162
  + (01 at 29): 163
  + (01 at 30): 164
  + (01 at 31): 165
  + (01 at 32): 166

Keywords (Opt): INST_CMPRS_SEGMENT_QUALITY
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000
  + (01 at 04): 160.000000
  + (01 at 05): 170.000000
  + (01 at 06): 180.000000
  + (01 at 07): 190.000000
  + (01 at 08): 200.000000
  + (01 at 09): 210.000000
  + (01 at 10): 220.000000
  + (01 at 11): 230.000000
  + (01 at 12): 240.000000
  + (01 at 13): 250.000000
  + (01 at 14): 260.000000
  + (01 at 15): 270.000000
  + (01 at 16): 280.000000
  + (01 at 17): 290.000000
  + (01 at 18): 300.000000
  + (01 at 19): 310.000000
  + (01 at 20): 320.000000
  + (01 at 21): 330.000000
  + (01 at 22): 340.000000
  + (01 at 23): 350.000000
  + (01 at 24): 360.000000
  + (01 at 25): 370.000000
  + (01 at 26): 380.000000
  + (01 at 27): 390.000000
  + (01 at 28): 400.000000
  + (01 at 29): 410.000000
  + (01 at 30): 420.000000
  + (01 at 31): 430.000000
  + (01 at 32): 440.000000

Keywords (Opt): INST_CMPRS_SYNC_BLKS
        Format: INT [1] (4)
        Values: 167

Keywords (Opt): INST_DECOMP_STAGES
        Format: INT [1] (4)
        Values: 168

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 169

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 170

Keywords (Opt): RICE_OPTION_VALUE
        Format: INT [1] (4)
        Values: 171

Keywords (Opt): RICE_START_OPTION
        Format: INT [1] (4)
        Values: 172

Keywords (Opt): SQRT_MAXIMUM_PIXEL
        Format: INT [1] (4)
        Values: 173

Keywords (Opt): SQRT_MINIMUM_PIXEL
        Format: INT [1] (4)
        Values: 174



Label Type: PROPERTY
PROPERTY Name: COMPRESSION_PARMS

Keywords (Opt): ERROR_PIXELS
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): INST_CMPRS_BLK_SIZE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): INST_CMPRS_BLOCKS
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): INST_CMPRS_DESC
        Format: STRING [1] (256)
        Values: AAAAAA

Keywords (Opt): INST_CMPRS_FILTER
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): INST_CMPRS_ENTROPY
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): INST_CMPRS_MODE
        Format: INT [1] (4)
        Values: 3

Keywords (Req): INST_CMPRS_NAME
        Format: STRING [1] (128)
        Values: CCCCCC

Keywords (Opt): INST_CMPRS_PARAM
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): INST_CMPRS_QUALITY
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_CMPRS_QUANTZ_TBL_ID
        Format: STRING [1] (48)
        Values: DDDDDD

Keywords (Opt): INST_CMPRS_QUANTZ_TYPE
        Format: STRING [1] (256)
        Values: EEEEEE

Keywords (Opt): INST_CMPRS_RATE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Req): INST_CMPRS_RATIO
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): INST_CMPRS_SEGMENTS
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INST_CMPRS_SEGMENT_STATUS
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ
  + (01 at 06): KKKKKK
  + (01 at 07): LLLLLL
  + (01 at 08): MMMMMM
  + (01 at 09): NNNNNN
  + (01 at 10): OOOOOO
  + (01 at 11): PPPPPP
  + (01 at 12): QQQQQQ
  + (01 at 13): RRRRRR
  + (01 at 14): SSSSSS
  + (01 at 15): TTTTTT
  + (01 at 16): UUUUUU
  + (01 at 17): VVVVVV
  + (01 at 18): WWWWWW
  + (01 at 19): XXXXXX
  + (01 at 20): YYYYYY
  + (01 at 21): ZZZZZZ
  + (01 at 22): aaaaaa
  + (01 at 23): bbbbbb
  + (01 at 24): cccccc
  + (01 at 25): dddddd
  + (01 at 26): eeeeee
  + (01 at 27): ffffff
  + (01 at 28): gggggg
  + (01 at 29): hhhhhh
  + (01 at 30): iiiiii
  + (01 at 31): jjjjjj
  + (01 at 32): kkkkkk

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE
        Format: INT [1] (4)
        Values: 7
  + (01 at 02): 8
  + (01 at 03): 9
  + (01 at 04): 10
  + (01 at 05): 11
  + (01 at 06): 12
  + (01 at 07): 13
  + (01 at 08): 14
  + (01 at 09): 15
  + (01 at 10): 16
  + (01 at 11): 17
  + (01 at 12): 18
  + (01 at 13): 19
  + (01 at 14): 20
  + (01 at 15): 21
  + (01 at 16): 22
  + (01 at 17): 23
  + (01 at 18): 24
  + (01 at 19): 25
  + (01 at 20): 26
  + (01 at 21): 27
  + (01 at 22): 28
  + (01 at 23): 29
  + (01 at 24): 30
  + (01 at 25): 31
  + (01 at 26): 32
  + (01 at 27): 33
  + (01 at 28): 34
  + (01 at 29): 35
  + (01 at 30): 36
  + (01 at 31): 37
  + (01 at 32): 38

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE_SAMP
        Format: INT [1] (4)
        Values: 39
  + (01 at 02): 40
  + (01 at 03): 41
  + (01 at 04): 42
  + (01 at 05): 43
  + (01 at 06): 44
  + (01 at 07): 45
  + (01 at 08): 46
  + (01 at 09): 47
  + (01 at 10): 48
  + (01 at 11): 49
  + (01 at 12): 50
  + (01 at 13): 51
  + (01 at 14): 52
  + (01 at 15): 53
  + (01 at 16): 54
  + (01 at 17): 55
  + (01 at 18): 56
  + (01 at 19): 57
  + (01 at 20): 58
  + (01 at 21): 59
  + (01 at 22): 60
  + (01 at 23): 61
  + (01 at 24): 62
  + (01 at 25): 63
  + (01 at 26): 64
  + (01 at 27): 65
  + (01 at 28): 66
  + (01 at 29): 67
  + (01 at 30): 68
  + (01 at 31): 69
  + (01 at 32): 70

Keywords (Opt): INST_CMPRS_SEG_LINES
        Format: INT [1] (4)
        Values: 71
  + (01 at 02): 72
  + (01 at 03): 73
  + (01 at 04): 74
  + (01 at 05): 75
  + (01 at 06): 76
  + (01 at 07): 77
  + (01 at 08): 78
  + (01 at 09): 79
  + (01 at 10): 80
  + (01 at 11): 81
  + (01 at 12): 82
  + (01 at 13): 83
  + (01 at 14): 84
  + (01 at 15): 85
  + (01 at 16): 86
  + (01 at 17): 87
  + (01 at 18): 88
  + (01 at 19): 89
  + (01 at 20): 90
  + (01 at 21): 91
  + (01 at 22): 92
  + (01 at 23): 93
  + (01 at 24): 94
  + (01 at 25): 95
  + (01 at 26): 96
  + (01 at 27): 97
  + (01 at 28): 98
  + (01 at 29): 99
  + (01 at 30): 100
  + (01 at 31): 101
  + (01 at 32): 102

Keywords (Opt): INST_CMPRS_SEG_SAMPLES
        Format: INT [1] (4)
        Values: 103
  + (01 at 02): 104
  + (01 at 03): 105
  + (01 at 04): 106
  + (01 at 05): 107
  + (01 at 06): 108
  + (01 at 07): 109
  + (01 at 08): 110
  + (01 at 09): 111
  + (01 at 10): 112
  + (01 at 11): 113
  + (01 at 12): 114
  + (01 at 13): 115
  + (01 at 14): 116
  + (01 at 15): 117
  + (01 at 16): 118
  + (01 at 17): 119
  + (01 at 18): 120
  + (01 at 19): 121
  + (01 at 20): 122
  + (01 at 21): 123
  + (01 at 22): 124
  + (01 at 23): 125
  + (01 at 24): 126
  + (01 at 25): 127
  + (01 at 26): 128
  + (01 at 27): 129
  + (01 at 28): 130
  + (01 at 29): 131
  + (01 at 30): 132
  + (01 at 31): 133
  + (01 at 32): 134

Keywords (Opt): INST_CMPRS_SEG_MISSING_PIXELS
        Format: INT [1] (4)
        Values: 135
  + (01 at 02): 136
  + (01 at 03): 137
  + (01 at 04): 138
  + (01 at 05): 139
  + (01 at 06): 140
  + (01 at 07): 141
  + (01 at 08): 142
  + (01 at 09): 143
  + (01 at 10): 144
  + (01 at 11): 145
  + (01 at 12): 146
  + (01 at 13): 147
  + (01 at 14): 148
  + (01 at 15): 149
  + (01 at 16): 150
  + (01 at 17): 151
  + (01 at 18): 152
  + (01 at 19): 153
  + (01 at 20): 154
  + (01 at 21): 155
  + (01 at 22): 156
  + (01 at 23): 157
  + (01 at 24): 158
  + (01 at 25): 159
  + (01 at 26): 160
  + (01 at 27): 161
  + (01 at 28): 162
  + (01 at 29): 163
  + (01 at 30): 164
  + (01 at 31): 165
  + (01 at 32): 166

Keywords (Opt): INST_CMPRS_SEGMENT_QUALITY
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000
  + (01 at 04): 160.000000
  + (01 at 05): 170.000000
  + (01 at 06): 180.000000
  + (01 at 07): 190.000000
  + (01 at 08): 200.000000
  + (01 at 09): 210.000000
  + (01 at 10): 220.000000
  + (01 at 11): 230.000000
  + (01 at 12): 240.000000
  + (01 at 13): 250.000000
  + (01 at 14): 260.000000
  + (01 at 15): 270.000000
  + (01 at 16): 280.000000
  + (01 at 17): 290.000000
  + (01 at 18): 300.000000
  + (01 at 19): 310.000000
  + (01 at 20): 320.000000
  + (01 at 21): 330.000000
  + (01 at 22): 340.000000
  + (01 at 23): 350.000000
  + (01 at 24): 360.000000
  + (01 at 25): 370.000000
  + (01 at 26): 380.000000
  + (01 at 27): 390.000000
  + (01 at 28): 400.000000
  + (01 at 29): 410.000000
  + (01 at 30): 420.000000
  + (01 at 31): 430.000000
  + (01 at 32): 440.000000

Keywords (Opt): INST_CMPRS_SYNC_BLKS
        Format: INT [1] (4)
        Values: 167

Keywords (Opt): INST_DECOMP_STAGES
        Format: INT [1] (4)
        Values: 168

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 169

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 170

Keywords (Opt): RICE_OPTION_VALUE
        Format: INT [1] (4)
        Values: 171

Keywords (Opt): RICE_START_OPTION
        Format: INT [1] (4)
        Values: 172

Keywords (Opt): SQRT_MAXIMUM_PIXEL
        Format: INT [1] (4)
        Values: 173

Keywords (Opt): SQRT_MINIMUM_PIXEL
        Format: INT [1] (4)
        Values: 174



Label Type: PROPERTY
PROPERTY Name: CAMERA_MODEL

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Req): CALIBRATION_SOURCE_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): MODEL_DESC__PTR
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MODEL_NAME
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Req): MODEL_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): GEOMETRY_SOURCE_ID
        Format: STRING [1] (48)
        Values: FFFFFF

Keywords (Opt): MODEL_COMPONENT_ID
        Format: STRING [1] (8)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK
  + (01 at 06): LLLLLL
  + (01 at 07): MMMMMM
  + (01 at 08): NNNNNN
  + (01 at 09): OOOOOO

Keywords (Opt): MODEL_COMPONENT_NAME
        Format: STRING [1] (64)
        Values: PPPPPP
  + (01 at 02): QQQQQQ
  + (01 at 03): RRRRRR
  + (01 at 04): SSSSSS
  + (01 at 05): TTTTTT
  + (01 at 06): UUUUUU
  + (01 at 07): VVVVVV
  + (01 at 08): WWWWWW
  + (01 at 09): XXXXXX

Keywords (Opt): MODEL_COMPONENT_UNIT
        Format: STRING [1] (32)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa
  + (01 at 04): bbbbbb
  + (01 at 05): cccccc
  + (01 at 06): dddddd
  + (01 at 07): eeeeee
  + (01 at 08): ffffff
  + (01 at 09): gggggg

Keywords (Opt): MODEL_COMPONENT_1
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): MODEL_COMPONENT_2
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000

Keywords (Opt): MODEL_COMPONENT_3
        Format: REAL [1] (4)
        Values: 160.000000
  + (01 at 02): 170.000000
  + (01 at 03): 180.000000

Keywords (Opt): MODEL_COMPONENT_4
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): MODEL_COMPONENT_5
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): MODEL_COMPONENT_6
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000

Keywords (Opt): MODEL_COMPONENT_7
        Format: REAL [1] (4)
        Values: 280.000000
  + (01 at 02): 290.000000
  + (01 at 03): 300.000000

Keywords (Opt): MODEL_COMPONENT_8
        Format: REAL [1] (4)
        Values: 310.000000
  + (01 at 02): 320.000000
  + (01 at 03): 330.000000

Keywords (Opt): MODEL_COMPONENT_9
        Format: REAL [1] (4)
        Values: 340.000000
  + (01 at 02): 350.000000
  + (01 at 03): 360.000000

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (48)
        Values: hhhhhh

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: iiiiii

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: jjjjjj



Label Type: PROPERTY
PROPERTY Name: GEOMETRIC_CAMERA_MODEL

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Req): CALIBRATION_SOURCE_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): MODEL_DESC__PTR
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MODEL_NAME
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Req): MODEL_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): GEOMETRY_SOURCE_ID
        Format: STRING [1] (48)
        Values: FFFFFF

Keywords (Opt): MODEL_COMPONENT_ID
        Format: STRING [1] (8)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK
  + (01 at 06): LLLLLL
  + (01 at 07): MMMMMM
  + (01 at 08): NNNNNN
  + (01 at 09): OOOOOO

Keywords (Opt): MODEL_COMPONENT_NAME
        Format: STRING [1] (64)
        Values: PPPPPP
  + (01 at 02): QQQQQQ
  + (01 at 03): RRRRRR
  + (01 at 04): SSSSSS
  + (01 at 05): TTTTTT
  + (01 at 06): UUUUUU
  + (01 at 07): VVVVVV
  + (01 at 08): WWWWWW
  + (01 at 09): XXXXXX

Keywords (Opt): MODEL_COMPONENT_UNIT
        Format: STRING [1] (32)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa
  + (01 at 04): bbbbbb
  + (01 at 05): cccccc
  + (01 at 06): dddddd
  + (01 at 07): eeeeee
  + (01 at 08): ffffff
  + (01 at 09): gggggg

Keywords (Opt): MODEL_COMPONENT_1
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): MODEL_COMPONENT_2
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000

Keywords (Opt): MODEL_COMPONENT_3
        Format: REAL [1] (4)
        Values: 160.000000
  + (01 at 02): 170.000000
  + (01 at 03): 180.000000

Keywords (Opt): MODEL_COMPONENT_4
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): MODEL_COMPONENT_5
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): MODEL_COMPONENT_6
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000

Keywords (Opt): MODEL_COMPONENT_7
        Format: REAL [1] (4)
        Values: 280.000000
  + (01 at 02): 290.000000
  + (01 at 03): 300.000000

Keywords (Opt): MODEL_COMPONENT_8
        Format: REAL [1] (4)
        Values: 310.000000
  + (01 at 02): 320.000000
  + (01 at 03): 330.000000

Keywords (Opt): MODEL_COMPONENT_9
        Format: REAL [1] (4)
        Values: 340.000000
  + (01 at 02): 350.000000
  + (01 at 03): 360.000000

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (48)
        Values: hhhhhh

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: iiiiii

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: jjjjjj



Label Type: PROPERTY
PROPERTY Name: DERIVED_GEOMETRY

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): COORDINATE_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: BBBBBB

Keywords (Opt): DERIVED_GEOMETRY_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): DERIVED_GEOMETRY_NAME
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Opt): DERIVED_GEOMETRY_NOTE
        Format: STRING [1] (256)
        Values: EEEEEE

Keywords (Opt): DERIVED_GEOMETRY_TYPE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Opt): LANDER_INSTRUMENT_AZIMUTH, INSTRUMENT_AZIMUTH
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): INSTRUMENT_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: GGGGGG

Keywords (Opt): LANDER_INSTRUMENT_ELEVATION, INSTRUMENT_ELEVATION
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): INSTRUMENT_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): LANDER_LOCAL_LEVEL_QUATERNION, INST_HOST_TO_FIXED_QUATERNION
        Format: REAL [4] (16)
        Values: 120.000000
                130.000000
                140.000000
                150.000000

Keywords (Opt): LCL_LVL_SRFC_FXD_VECTOR, LOCAL_TO_FIXED_OFFSET_VECTOR
        Format: REAL [3] (12)
        Values: 160.000000
                170.000000
                180.000000

Keywords (Opt): LOCAL_LEVEL_INST_AZIMUTH, LOCAL_INSTRUMENT_AZIMUTH
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): LOCAL_LEVEL_INST_ELEVATION, LOCAL_INSTRUMENT_ELEVATION
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): NORTH_AZIMUTH
        Format: REAL [1] (4)
        Values: 210.000000

Keywords (Opt): POSITIVE_AZIMUTH_DIRECTION
        Format: STRING [1] (48)
        Values: IIIIII

Keywords (Opt): POSITIVE_ELEVATION_DIRECTION
        Format: STRING [1] (48)
        Values: JJJJJJ

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: KKKKKK

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: LLLLLL

Keywords (Opt): SLANT_DISTANCE
        Format: REAL [1] (4)
        Values: 220.000000

Keywords (Opt): SMEAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 230.000000

Keywords (Opt): SMEAR_MAGNITUDE
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): SOLAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SOLAR_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): SOLAR_ELEVATION
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SOLAR_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): SRFC_FXD_LCL_LVL_VECTOR,
        Format: REAL [3] (12)
        Values: 270.000000
                280.000000
                290.000000

Keywords (Opt): SURFACE_FIXED_INST_AZIMUTH, FIXED_INSTRUMENT_AZIMUTH
        Format: REAL [1] (4)
        Values: 300.000000

Keywords (Opt): SURFACE_FIXED_INST_ELEVATION, FIXED_INSTRUMENT_ELEVATION
        Format: REAL [1] (4)
        Values: 310.000000



Label Type: PROPERTY
PROPERTY Name: IDENTIFICATION

Keywords (Req): DATA_SET_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): DATA_SET_NAME
        Format: STRING [1] (64)
        Values: BBBBBB

Keywords (Opt): COMMAND_SEQUENCE_NUMBER
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): FEATURE_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): FEATURE_TYPE
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): FRAME_ID
        Format: STRING [1] (48)
        Values: EEEEEE
  + (01 at 02): FFFFFF
  + (01 at 03): GGGGGG
  + (01 at 04): HHHHHH
  + (01 at 05): IIIIII

Keywords (Opt): FRAME_TYPE
        Format: STRING [1] (32)
        Values: JJJJJJ

Keywords (Opt): GEOMETRY_PROJECTION_TYPE
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): IMAGE_ID
        Format: STRING [1] (48)
        Values: LLLLLL

Keywords (Opt): IMAGE_TIME
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): IMAGE_TYPE
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): INSTRUMENT_HOST_ID
        Format: STRING [1] (48)
        Values: OOOOOO
  + (01 at 02): PPPPPP
  + (01 at 03): QQQQQQ
  + (01 at 04): RRRRRR
  + (01 at 05): SSSSSS

Keywords (Req): INSTRUMENT_HOST_NAME
        Format: STRING [1] (64)
        Values: TTTTTT
  + (01 at 02): UUUUUU
  + (01 at 03): VVVVVV
  + (01 at 04): WWWWWW
  + (01 at 05): XXXXXX

Keywords (Opt): INSTRUMENT_ID
        Format: STRING [1] (48)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa
  + (01 at 04): bbbbbb
  + (01 at 05): cccccc

Keywords (Req): INSTRUMENT_NAME
        Format: STRING [1] (64)
        Values: dddddd
  + (01 at 02): eeeeee
  + (01 at 03): ffffff
  + (01 at 04): gggggg
  + (01 at 05): hhhhhh

Keywords (Opt): INSTRUMENT_SERIAL_NUMBER
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): INSTRUMENT_TYPE
        Format: STRING [1] (32)
        Values: iiiiii
  + (01 at 02): jjjjjj
  + (01 at 03): kkkkkk
  + (01 at 04): llllll
  + (01 at 05): mmmmmm

Keywords (Opt): INSTRUMENT_VERSION_ID
        Format: STRING [1] (48)
        Values: nnnnnn

Keywords (Opt): LOCAL_TRUE_SOLAR_TIME
        Format: STRING [1] (32)
        Values: oooooo

Keywords (Opt): MAGNET_ID
        Format: STRING [1] (48)
        Values: pppppp

Keywords (Opt): MEASUREMENT_ID
        Format: STRING [1] (48)
        Values: qqqqqq

Keywords (Opt): MEASUREMENT_TIME
        Format: STRING [1] (32)
        Values: rrrrrr

Keywords (Opt): MEASUREMENT_TYPE
        Format: STRING [1] (32)
        Values: ssssss

Keywords (Req): MISSION_NAME
        Format: STRING [1] (64)
        Values: tttttt
  + (01 at 02): uuuuuu
  + (01 at 03): vvvvvv
  + (01 at 04): wwwwww
  + (01 at 05): xxxxxx

Keywords (Opt): MISSION_PHASE_NAME
        Format: STRING [1] (64)
        Values: yyyyyy

Keywords (Opt): OBSERVATION_ID
        Format: STRING [1] (48)
        Values: zzzzzz

Keywords (Opt): OBSERVATION_NAME
        Format: STRING [1] (64)
        Values: 000000

Keywords (Opt): OBSERVATION_TIME
        Format: STRING [1] (32)
        Values: 111111

Keywords (Opt): OBSERVATION_TYPE
        Format: STRING [1] (32)
        Values: 222222

Keywords (Opt): ORBIT_NUMBER
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): OPS_TOKEN
        Format: STRING [1] (64)
        Values: 333333

Keywords (Opt): OPS_TOKEN_ACTIVITY
        Format: STRING [1] (64)
        Values: 444444

Keywords (Opt): OPS_TOKEN_COMMAND
        Format: STRING [1] (64)
        Values: 555555

Keywords (Opt): OPS_TOKEN_PAYLOAD
        Format: STRING [1] (64)
        Values: 666666

Keywords (Opt): PLANET_DAY_NUMBER
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): PROCESSING_HISTORY_TEXT
        Format: STRING [1] (256)
        Values: 777777

Keywords (Opt): PRODUCER_FULL_NAME
        Format: STRING [1] (64)
        Values: 888888

Keywords (Opt): PRODUCER_ID
        Format: STRING [1] (48)
        Values: 999999

Keywords (Opt): PRODUCER_INSTITUTION_NAME
        Format: STRING [1] (64)
        Values: AAAAAA

Keywords (Req): PRODUCT_CREATION_TIME
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Req): PRODUCT_ID
        Format: STRING [1] (48)
        Values: CCCCCC

Keywords (Opt): PRODUCT_VERSION_ID
        Format: STRING [1] (48)
        Values: DDDDDD

Keywords (Opt): RELEASE_ID
        Format: STRING [1] (48)
        Values: EEEEEE

Keywords (Opt): ROVER_MOTION_COUNTER
        Format: INT [1] (4)
        Values: 3
  + (01 at 02): 4
  + (01 at 03): 5
  + (01 at 04): 6
  + (01 at 05): 7

Keywords (Opt): ROVER_MOTION_COUNTER_NAME
        Format: STRING [1] (64)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ

Keywords (Opt): SEQ_ID,SEQUENCE_ID
        Format: STRING [1] (48)
        Values: KKKKKK

Keywords (Opt): SEQUENCE_NAME, SEQUENCE_TITLE
        Format: STRING [1] (64)
        Values: LLLLLL

Keywords (Opt): SEQUENCE_VERSION_ID
        Format: STRING [1] (48)
        Values: MMMMMM

Keywords (Opt): SOLAR_LONGITUDE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): SPACECRAFT_CLOCK_CNT_PARTITION
        Format: INT [1] (4)
        Values: 8

Keywords (Req): SPACECRAFT_CLOCK_START_COUNT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Req): SPACECRAFT_CLOCK_STOP_COUNT
        Format: STRING [1] (32)
        Values: OOOOOO

Keywords (Req): START_TIME
        Format: STRING [1] (32)
        Values: PPPPPP

Keywords (Req): STOP_TIME
        Format: STRING [1] (32)
        Values: QQQQQQ

Keywords (Req): TARGET_NAME
        Format: STRING [1] (64)
        Values: RRRRRR

Keywords (Opt): TARGET_TYPE
        Format: STRING [1] (32)
        Values: SSSSSS



Label Type: PROPERTY
PROPERTY Name: IMAGE_DATA

Keywords (Opt): BANDS
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): CHECKSUM
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): FIRST_LINE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): FIRST_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): INTERCHANGE_FORMAT
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): INVALID_CONSTANT
        Format: REAL [1] (4)
        Values: 110.000000
  + (01 at 02): 120.000000
  + (01 at 03): 130.000000

Keywords (Opt): LINE_PREFIX_BYTES
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): LINE_PREFIX_MEAN
        Format: REAL [1] (4)
        Values: 140.000000

Keywords (Opt): LINE_SUFFIX_BYTES
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): LINE_SUFFIX_MEAN
        Format: REAL [1] (4)
        Values: 150.000000

Keywords (Opt): LINE_SAMPLES
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): LINES
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): MAXIMUM
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): MEAN
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): MEDIAN
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): MINIMUM
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): MISSING_CONSTANT
        Format: REAL [1] (4)
        Values: 200.000000
  + (01 at 02): 210.000000
  + (01 at 03): 220.000000

Keywords (Opt): RADIANCE_OFFSET
        Format: REAL [1] (4)
        Values: 230.000000

Keywords (Opt): RADIANCE_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): RADIANCE_SCALING_FACTOR
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): RADIANCE_SCALING_FACTOR__UNIT
        Format: STRING [1] (32)
        Values: CCCCCC

Keywords (Opt): SAMPLE_BIT_MASK
        Format: STRING [1] (128)
        Values: DDDDDD

Keywords (Opt): SAMPLE_BITS
        Format: INT [1] (4)
        Values: 7

Keywords (Opt): SAMPLE_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): SPICE_FILE_NAME
        Format: STRING [1] (256)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ
  + (01 at 06): KKKKKK
  + (01 at 07): LLLLLL
  + (01 at 08): MMMMMM

Keywords (Opt): STANDARD_DEVIATION
        Format: REAL [1] (4)
        Values: 250.000000



Label Type: PROPERTY
PROPERTY Name: IMAGE_GEOMETRY

Keywords (Opt): CENTRAL_BODY_DISTANCE
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): EMISSION_ANGLE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): INCIDENCE_ANGLE
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): INTERCEPT_POINT_LATITUDE
        Format: REAL [1] (4)
        Values: 130.000000

Keywords (Opt): INTERCEPT_POINT_LINE
        Format: REAL [1] (4)
        Values: 140.000000

Keywords (Opt): INTERCEPT_POINT_LINE_SAMPLE
        Format: REAL [1] (4)
        Values: 150.000000

Keywords (Opt): INTERCEPT_POINT_LONGITUDE
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): LOCAL_HOUR_ANGLE
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): LOCAL_MEAN_SOLAR_TIME
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): LOCAL_TIME
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): LOCAL_TRUE_SOLAR_TIME
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): NTV_SAT_TIME_FROM_CLOSEST_APRH
        Format: STRING [1] (32)
        Values: CCCCCC

Keywords (Opt): NTV_TIME_FROM_CLOSEST_APPROACH
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): PHASE_ANGLE
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): SOLAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): SOLAR_DISTANCE
        Format: REAL [1] (4)
        Values: 210.000000

Keywords (Opt): SOLAR_ELEVATION
        Format: REAL [1] (4)
        Values: 220.000000

Keywords (Opt): SOLAR_LATITUDE
        Format: REAL [1] (4)
        Values: 230.000000

Keywords (Opt): SOLAR_LINE
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): SOLAR_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): SOLAR_LONGITUDE
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): SPACECRAFT_DISTANCE
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SUB_SPACECRAFT_AZIMUTH
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SUB_SPACECRAFT_LATITUDE
        Format: REAL [1] (4)
        Values: 270.000000

Keywords (Opt): SUB_SPACECRAFT_LONGITUDE
        Format: REAL [1] (4)
        Values: 280.000000

Keywords (Opt): SURCAFE_FIXED_SOLAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 290.000000

Keywords (Opt): SURFACE_FIXED_SOLAR_ELEVATION
        Format: REAL [1] (4)
        Values: 300.000000



Label Type: PROPERTY
PROPERTY Name: INSTRUMENT_STATE

Keywords (Opt): AZIMUTH_FOV
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): AZIMUTH_FOV__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): ELEVATION_FOV
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): ELEVATION_FOV__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): BAD_PIXEL_REPLACEMENT_ID
        Format: STRING [1] (48)
        Values: CCCCCC

Keywords (Opt): DETECTOR_FIRST_LINE
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): DETECTOR_LINES
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): DETECTOR_TO_IMAGE_ROTATION
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): DOWNSAMPLE_METHOD
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Opt): EXPOSURE_COUNT
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): EXPOSURE_DURATION
        Format: REAL [1] (4)
        Values: 130.000000

Keywords (Opt): EXPOSURE_DURATION__UNIT
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): EXPOSURE_DURATION_COUNT
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): EXPOSURE_TYPE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (64)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK

Keywords (Opt): FILTER_NUMBER
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): FLAT_FIELD_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: LLLLLL

Keywords (Opt): FLAT_FIELD_CORRECTION_PARM
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000
  + (01 at 03): 160.000000
  + (01 at 04): 170.000000
  + (01 at 05): 180.000000

Keywords (Opt): GAIN_MODE_ID
        Format: STRING [1] (48)
        Values: MMMMMM

Keywords (Opt): IMAGE_BIAS
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_AZ_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: NNNNNN

Keywords (Opt): INST_EL_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: OOOOOO

Keywords (Opt): INST_HOST_POSITION
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): INSTRUMENT_AZIMUTH_COUNT
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INSTRUMENT_COVER_STATE_ID
        Format: STRING [1] (48)
        Values: PPPPPP

Keywords (Opt): INSTRUMENT_DATA_RATE
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_DEPLOYMENT_STATE
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): INSTRUMENT_ELEVATION_COUNT
        Format: INT [1] (4)
        Values: 7

Keywords (Opt): INSTRUMENT_FOCAL_LENGTH_COUNT
        Format: INT [1] (4)
        Values: 8

Keywords (Opt): INSTRUMENT_MODE_ID
        Format: STRING [1] (48)
        Values: SSSSSS

Keywords (Opt): INSTRUMENT_POSITION
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000
  + (01 at 04): 280.000000
  + (01 at 05): 290.000000
  + (01 at 06): 300.000000
  + (01 at 07): 310.000000
  + (01 at 08): 320.000000
  + (01 at 09): 330.000000
  + (01 at 10): 340.000000
  + (01 at 11): 350.000000
  + (01 at 12): 360.000000
  + (01 at 13): 370.000000
  + (01 at 14): 380.000000
  + (01 at 15): 390.000000
  + (01 at 16): 400.000000
  + (01 at 17): 410.000000
  + (01 at 18): 420.000000
  + (01 at 19): 430.000000
  + (01 at 20): 440.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT
  + (01 at 02): UUUUUU
  + (01 at 03): VVVVVV
  + (01 at 04): WWWWWW
  + (01 at 05): XXXXXX
  + (01 at 06): YYYYYY
  + (01 at 07): ZZZZZZ
  + (01 at 08): aaaaaa
  + (01 at 09): bbbbbb
  + (01 at 10): cccccc
  + (01 at 11): dddddd
  + (01 at 12): eeeeee
  + (01 at 13): ffffff
  + (01 at 14): gggggg
  + (01 at 15): hhhhhh
  + (01 at 16): iiiiii
  + (01 at 17): jjjjjj
  + (01 at 18): kkkkkk
  + (01 at 19): llllll
  + (01 at 20): mmmmmm

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT__UNIT
        Format: STRING [1] (32)
        Values: nnnnnn
  + (01 at 02): oooooo
  + (01 at 03): pppppp
  + (01 at 04): qqqqqq
  + (01 at 05): rrrrrr
  + (01 at 06): ssssss
  + (01 at 07): tttttt
  + (01 at 08): uuuuuu
  + (01 at 09): vvvvvv
  + (01 at 10): wwwwww
  + (01 at 11): xxxxxx
  + (01 at 12): yyyyyy
  + (01 at 13): zzzzzz
  + (01 at 14): 000000
  + (01 at 15): 111111
  + (01 at 16): 222222
  + (01 at 17): 333333
  + (01 at 18): 444444
  + (01 at 19): 555555
  + (01 at 20): 666666

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT
        Format: INT [1] (4)
        Values: 9
  + (01 at 02): 10
  + (01 at 03): 11
  + (01 at 04): 12
  + (01 at 05): 13
  + (01 at 06): 14
  + (01 at 07): 15
  + (01 at 08): 16
  + (01 at 09): 17
  + (01 at 10): 18
  + (01 at 11): 19
  + (01 at 12): 20
  + (01 at 13): 21
  + (01 at 14): 22
  + (01 at 15): 23
  + (01 at 16): 24
  + (01 at 17): 25
  + (01 at 18): 26
  + (01 at 19): 27
  + (01 at 20): 28

Keywords (Opt): INSTRUMENT_TEMPERATURE_NAME
        Format: STRING [1] (64)
        Values: 777777
  + (01 at 02): 888888
  + (01 at 03): 999999
  + (01 at 04): AAAAAA
  + (01 at 05): BBBBBB
  + (01 at 06): CCCCCC
  + (01 at 07): DDDDDD
  + (01 at 08): EEEEEE
  + (01 at 09): FFFFFF
  + (01 at 10): GGGGGG
  + (01 at 11): HHHHHH
  + (01 at 12): IIIIII
  + (01 at 13): JJJJJJ
  + (01 at 14): KKKKKK
  + (01 at 15): LLLLLL
  + (01 at 16): MMMMMM
  + (01 at 17): NNNNNN
  + (01 at 18): OOOOOO
  + (01 at 19): PPPPPP
  + (01 at 20): QQQQQQ

Keywords (Opt): INSTRUMENT_VOLTAGE_COUNT
        Format: INT [1] (4)
        Values: 29

Keywords (Opt): LED_BITMASK
        Format: INT [1] (4)
        Values: 30

Keywords (Opt): OFFSET_MODE_ID
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): OFFSET_NUMBER
        Format: INT [1] (4)
        Values: 31

Keywords (Opt): ONBOARD_IMAGE_BIAS
        Format: INT [1] (4)
        Values: 32

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 33

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 34

Keywords (Opt): SAMPLE_BIT_METHOD
        Format: STRING [1] (64)
        Values: SSSSSS

Keywords (Opt): SAMPLE_BIT_MODE_ID
        Format: STRING [1] (48)
        Values: TTTTTT

Keywords (Opt): SHUTTER_EFFECT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: UUUUUU

Keywords (Opt): SHUTTER_MODE_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): SUN_FIND_FLAG
        Format: STRING [1] (8)
        Values: WWWWWW

Keywords (Opt): SUN_FIND
        Format: STRING [1] (8)
        Values: XXXXXX

Keywords (Opt): SUN_FIND_ACTIVE_FLAG
        Format: STRING [1] (8)
        Values: YYYYYY

Keywords (Opt): SUN_FIND_PARM
        Format: REAL [1] (4)
        Values: 450.000000
  + (01 at 02): 460.000000
  + (01 at 03): 470.000000
  + (01 at 04): 480.000000
  + (01 at 05): 490.000000

Keywords (Opt): SUN_FIND_PARM_NAME
        Format: STRING [1] (64)
        Values: ZZZZZZ
  + (01 at 02): aaaaaa
  + (01 at 03): bbbbbb
  + (01 at 04): cccccc
  + (01 at 05): dddddd

Keywords (Opt): SUN_LINE
        Format: INT [1] (4)
        Values: 35

Keywords (Opt): SUN_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 36

Keywords (Opt): SUN_VIEW_POSITION
        Format: REAL [3] (12)
        Values: 500.000000
                510.000000
                520.000000

Keywords (Opt): SUN_VIEW_DIRECTION
        Format: REAL [3] (12)
        Values: 530.000000
                540.000000
                550.000000



Label Type: PROPERTY
PROPERTY Name: INSTRUMENT_STATE_PARMS

Keywords (Opt): AZIMUTH_FOV
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): AZIMUTH_FOV__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): ELEVATION_FOV
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): ELEVATION_FOV__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): BAD_PIXEL_REPLACEMENT_ID
        Format: STRING [1] (48)
        Values: CCCCCC

Keywords (Opt): DETECTOR_FIRST_LINE
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): DETECTOR_LINES
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): DETECTOR_TO_IMAGE_ROTATION
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): DOWNSAMPLE_METHOD
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Opt): EXPOSURE_COUNT
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): EXPOSURE_DURATION
        Format: REAL [1] (4)
        Values: 130.000000

Keywords (Opt): EXPOSURE_DURATION__UNIT
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): EXPOSURE_DURATION_COUNT
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): EXPOSURE_TYPE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (64)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK

Keywords (Opt): FILTER_NUMBER
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): FLAT_FIELD_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: LLLLLL

Keywords (Opt): FLAT_FIELD_CORRECTION_PARM
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000
  + (01 at 03): 160.000000
  + (01 at 04): 170.000000
  + (01 at 05): 180.000000

Keywords (Opt): GAIN_MODE_ID
        Format: STRING [1] (48)
        Values: MMMMMM

Keywords (Opt): IMAGE_BIAS
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_AZ_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: NNNNNN

Keywords (Opt): INST_EL_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: OOOOOO

Keywords (Opt): INST_HOST_POSITION
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): INSTRUMENT_AZIMUTH_COUNT
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INSTRUMENT_COVER_STATE_ID
        Format: STRING [1] (48)
        Values: PPPPPP

Keywords (Opt): INSTRUMENT_DATA_RATE
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_DEPLOYMENT_STATE
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): INSTRUMENT_ELEVATION_COUNT
        Format: INT [1] (4)
        Values: 7

Keywords (Opt): INSTRUMENT_FOCAL_LENGTH_COUNT
        Format: INT [1] (4)
        Values: 8

Keywords (Opt): INSTRUMENT_MODE_ID
        Format: STRING [1] (48)
        Values: SSSSSS

Keywords (Opt): INSTRUMENT_POSITION
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000
  + (01 at 04): 280.000000
  + (01 at 05): 290.000000
  + (01 at 06): 300.000000
  + (01 at 07): 310.000000
  + (01 at 08): 320.000000
  + (01 at 09): 330.000000
  + (01 at 10): 340.000000
  + (01 at 11): 350.000000
  + (01 at 12): 360.000000
  + (01 at 13): 370.000000
  + (01 at 14): 380.000000
  + (01 at 15): 390.000000
  + (01 at 16): 400.000000
  + (01 at 17): 410.000000
  + (01 at 18): 420.000000
  + (01 at 19): 430.000000
  + (01 at 20): 440.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT
  + (01 at 02): UUUUUU
  + (01 at 03): VVVVVV
  + (01 at 04): WWWWWW
  + (01 at 05): XXXXXX
  + (01 at 06): YYYYYY
  + (01 at 07): ZZZZZZ
  + (01 at 08): aaaaaa
  + (01 at 09): bbbbbb
  + (01 at 10): cccccc
  + (01 at 11): dddddd
  + (01 at 12): eeeeee
  + (01 at 13): ffffff
  + (01 at 14): gggggg
  + (01 at 15): hhhhhh
  + (01 at 16): iiiiii
  + (01 at 17): jjjjjj
  + (01 at 18): kkkkkk
  + (01 at 19): llllll
  + (01 at 20): mmmmmm

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT__UNIT
        Format: STRING [1] (32)
        Values: nnnnnn
  + (01 at 02): oooooo
  + (01 at 03): pppppp
  + (01 at 04): qqqqqq
  + (01 at 05): rrrrrr
  + (01 at 06): ssssss
  + (01 at 07): tttttt
  + (01 at 08): uuuuuu
  + (01 at 09): vvvvvv
  + (01 at 10): wwwwww
  + (01 at 11): xxxxxx
  + (01 at 12): yyyyyy
  + (01 at 13): zzzzzz
  + (01 at 14): 000000
  + (01 at 15): 111111
  + (01 at 16): 222222
  + (01 at 17): 333333
  + (01 at 18): 444444
  + (01 at 19): 555555
  + (01 at 20): 666666

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT
        Format: INT [1] (4)
        Values: 9
  + (01 at 02): 10
  + (01 at 03): 11
  + (01 at 04): 12
  + (01 at 05): 13
  + (01 at 06): 14
  + (01 at 07): 15
  + (01 at 08): 16
  + (01 at 09): 17
  + (01 at 10): 18
  + (01 at 11): 19
  + (01 at 12): 20
  + (01 at 13): 21
  + (01 at 14): 22
  + (01 at 15): 23
  + (01 at 16): 24
  + (01 at 17): 25
  + (01 at 18): 26
  + (01 at 19): 27
  + (01 at 20): 28

Keywords (Opt): INSTRUMENT_TEMPERATURE_NAME
        Format: STRING [1] (64)
        Values: 777777
  + (01 at 02): 888888
  + (01 at 03): 999999
  + (01 at 04): AAAAAA
  + (01 at 05): BBBBBB
  + (01 at 06): CCCCCC
  + (01 at 07): DDDDDD
  + (01 at 08): EEEEEE
  + (01 at 09): FFFFFF
  + (01 at 10): GGGGGG
  + (01 at 11): HHHHHH
  + (01 at 12): IIIIII
  + (01 at 13): JJJJJJ
  + (01 at 14): KKKKKK
  + (01 at 15): LLLLLL
  + (01 at 16): MMMMMM
  + (01 at 17): NNNNNN
  + (01 at 18): OOOOOO
  + (01 at 19): PPPPPP
  + (01 at 20): QQQQQQ

Keywords (Opt): INSTRUMENT_VOLTAGE_COUNT
        Format: INT [1] (4)
        Values: 29

Keywords (Opt): LED_BITMASK
        Format: INT [1] (4)
        Values: 30

Keywords (Opt): OFFSET_MODE_ID
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): OFFSET_NUMBER
        Format: INT [1] (4)
        Values: 31

Keywords (Opt): ONBOARD_IMAGE_BIAS
        Format: INT [1] (4)
        Values: 32

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 33

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 34

Keywords (Opt): SAMPLE_BIT_METHOD
        Format: STRING [1] (64)
        Values: SSSSSS

Keywords (Opt): SAMPLE_BIT_MODE_ID
        Format: STRING [1] (48)
        Values: TTTTTT

Keywords (Opt): SHUTTER_EFFECT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: UUUUUU

Keywords (Opt): SHUTTER_MODE_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): SUN_FIND_FLAG
        Format: STRING [1] (8)
        Values: WWWWWW

Keywords (Opt): SUN_FIND
        Format: STRING [1] (8)
        Values: XXXXXX

Keywords (Opt): SUN_FIND_ACTIVE_FLAG
        Format: STRING [1] (8)
        Values: YYYYYY

Keywords (Opt): SUN_FIND_PARM
        Format: REAL [1] (4)
        Values: 450.000000
  + (01 at 02): 460.000000
  + (01 at 03): 470.000000
  + (01 at 04): 480.000000
  + (01 at 05): 490.000000

Keywords (Opt): SUN_FIND_PARM_NAME
        Format: STRING [1] (64)
        Values: ZZZZZZ
  + (01 at 02): aaaaaa
  + (01 at 03): bbbbbb
  + (01 at 04): cccccc
  + (01 at 05): dddddd

Keywords (Opt): SUN_LINE
        Format: INT [1] (4)
        Values: 35

Keywords (Opt): SUN_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 36

Keywords (Opt): SUN_VIEW_POSITION
        Format: REAL [3] (12)
        Values: 500.000000
                510.000000
                520.000000

Keywords (Opt): SUN_VIEW_DIRECTION
        Format: REAL [3] (12)
        Values: 530.000000
                540.000000
                550.000000



Label Type: PROPERTY
PROPERTY Name: SURFACE_MODEL

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: AAAAAA

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): SURFACE_MODEL_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Req): SURFACE_MODEL_TYPE
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): SURFACE_NORMAL_VECTOR
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): SURFACE_GROUND_LOCATION
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000



Label Type: PROPERTY
PROPERTY Name: SURFACE_MODEL_PARMS

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: AAAAAA

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): SURFACE_MODEL_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Req): SURFACE_MODEL_TYPE
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): SURFACE_NORMAL_VECTOR
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): SURFACE_GROUND_LOCATION
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000



Label Type: PROPERTY
PROPERTY Name: SURFACE_PROJECTION

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): LINE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): LINE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): MAP_PROJECTION_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MAP_PROJECTION_NOTE
        Format: STRING [1] (256)
        Values: DDDDDD

Keywords (Req): MAP_PROJECTION_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): MAP_RESOLUTION
        Format: REAL [2] (12)
        Values: 120.000000
                130.000000

Keywords (Opt): MAP_RESOLUTION__UNIT
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG

Keywords (Opt): MAP_SCALE
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000

Keywords (Opt): MAP_SCALE__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): MAXIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): MAXIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: IIIIII

Keywords (Opt): MINIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): MINIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: JJJJJJ

Keywords (Opt): PROJECTION_AZIMUTH
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): PROJECTION_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): PROJECTION_ELEVATION
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): PROJECTION_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: LLLLLL

Keywords (Opt): PROJECTION_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): PROJECTION_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): PROJECTION_ORIGIN_VECTOR
        Format: REAL [1] (4)
        Values: 210.000000
  + (01 at 02): 220.000000
  + (01 at 03): 230.000000

Keywords (Opt): PROJECTION_ORIGIN_VECTOR__UNIT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): REFERENCE_AZIMUTH
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): REFERENCE_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: OOOOOO

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: PPPPPP

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: RRRRRR

Keywords (Opt): SAMPLE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SAMPLE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: SSSSSS

Keywords (Opt): START_AZIMUTH
        Format: REAL [1] (4)
        Values: 270.000000

Keywords (Opt): START_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT

Keywords (Opt): STOP_AZIMUTH
        Format: REAL [1] (4)
        Values: 280.000000

Keywords (Opt): STOP_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: UUUUUU

Keywords (Opt): SURFACE_GEOMETRY_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): X_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 290.000000

Keywords (Opt): X_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: WWWWWW

Keywords (Opt): X_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 300.000000

Keywords (Opt): X_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: XXXXXX

Keywords (Opt): Y_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 310.000000

Keywords (Opt): Y_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: YYYYYY

Keywords (Opt): Y_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 320.000000

Keywords (Opt): Y_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: ZZZZZZ

Keywords (Opt): ZERO_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 330.000000

Keywords (Opt): ZERO_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: aaaaaa



Label Type: PROPERTY
PROPERTY Name: SURFACE_PROJECTION_PARMS

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): LINE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): LINE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): MAP_PROJECTION_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MAP_PROJECTION_NOTE
        Format: STRING [1] (256)
        Values: DDDDDD

Keywords (Req): MAP_PROJECTION_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): MAP_RESOLUTION
        Format: REAL [2] (12)
        Values: 120.000000
                130.000000

Keywords (Opt): MAP_RESOLUTION__UNIT
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG

Keywords (Opt): MAP_SCALE
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000

Keywords (Opt): MAP_SCALE__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): MAXIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): MAXIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: IIIIII

Keywords (Opt): MINIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): MINIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: JJJJJJ

Keywords (Opt): PROJECTION_AZIMUTH
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): PROJECTION_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): PROJECTION_ELEVATION
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): PROJECTION_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: LLLLLL

Keywords (Opt): PROJECTION_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): PROJECTION_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): PROJECTION_ORIGIN_VECTOR
        Format: REAL [1] (4)
        Values: 210.000000
  + (01 at 02): 220.000000
  + (01 at 03): 230.000000

Keywords (Opt): PROJECTION_ORIGIN_VECTOR__UNIT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): REFERENCE_AZIMUTH
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): REFERENCE_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: OOOOOO

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: PPPPPP

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: RRRRRR

Keywords (Opt): SAMPLE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SAMPLE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: SSSSSS

Keywords (Opt): START_AZIMUTH
        Format: REAL [1] (4)
        Values: 270.000000

Keywords (Opt): START_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT

Keywords (Opt): STOP_AZIMUTH
        Format: REAL [1] (4)
        Values: 280.000000

Keywords (Opt): STOP_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: UUUUUU

Keywords (Opt): SURFACE_GEOMETRY_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): X_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 290.000000

Keywords (Opt): X_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: WWWWWW

Keywords (Opt): X_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 300.000000

Keywords (Opt): X_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: XXXXXX

Keywords (Opt): Y_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 310.000000

Keywords (Opt): Y_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: YYYYYY

Keywords (Opt): Y_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 320.000000

Keywords (Opt): Y_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: ZZZZZZ

Keywords (Opt): ZERO_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 330.000000

Keywords (Opt): ZERO_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: aaaaaa



Label Type: PROPERTY
PROPERTY Name: TELEMETRY

Keywords (Opt): APPLICATION_PACKET_ID
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): APPLICATION_PACKET_NAME
        Format: STRING [1] (128)
        Values: AAAAAA

Keywords (Opt): APPLICATION_PROCESS_ID
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): APPLICATION_PROCESS_NAME
        Format: STRING [1] (128)
        Values: BBBBBB

Keywords (Opt): APPLICATION_PROCESS_SUBTYPE_ID
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): EARTH_RECEIVED_START_TIME
        Format: STRING [1] (32)
        Values: CCCCCC

Keywords (Opt): EARTH_RECEIVED_STOP_TIME
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): EXPECTED_PACKETS
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): PACKET_CREATION_SCLK
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): PACKET_MAP_MASK
        Format: STRING [1] (256)
        Values: FFFFFF

Keywords (Opt): PACKET_SEQUENCE_NUMBER
        Format: INT [1] (4)
        Values: 4

Keywords (Req): RECEIVED_PACKETS
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): SOFTWARE_NAME
        Format: STRING [1] (64)
        Values: GGGGGG

Keywords (Opt): SOFTWARE_VERSION_ID
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): SPICE_FILE_ID
        Format: STRING [1] (48)
        Values: IIIIII
  + (01 at 02): JJJJJJ
  + (01 at 03): KKKKKK
  + (01 at 04): LLLLLL
  + (01 at 05): MMMMMM

Keywords (Req): SPICE_FILE_NAME
        Format: STRING [1] (256)
        Values: NNNNNN
  + (01 at 02): OOOOOO
  + (01 at 03): PPPPPP
  + (01 at 04): QQQQQQ
  + (01 at 05): RRRRRR

Keywords (Opt): TELEMETRY_PROVIDER_ID
        Format: STRING [1] (48)
        Values: SSSSSS

Keywords (Opt): TELEMETRY_PROVIDER_TYPE
        Format: STRING [1] (32)
        Values: TTTTTT

Keywords (Opt): TELEMETRY_SOURCE_NAME
        Format: STRING [1] (64)
        Values: UUUUUU

Keywords (Opt): TELEMETRY_SOURCE_TYPE
        Format: STRING [1] (32)
        Values: VVVVVV

Keywords (Opt): TLM_CMD_DISCREPANCY_FLAG
        Format: STRING [1] (8)
        Values: WWWWWW



Label Type: PROPERTY
PROPERTY Name: ARTICULATION_STATE

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): ARTICULATION_DEVICE_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): ARTICULATION_DEVICE_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): ARTICULATION_DEVICE_ANGLE
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000
  + (01 at 04): 130.000000
  + (01 at 05): 140.000000
  + (01 at 06): 150.000000
  + (01 at 07): 160.000000
  + (01 at 08): 170.000000
  + (01 at 09): 180.000000
  + (01 at 10): 190.000000

Keywords (Opt): ARTICULATION_DEVICE_ANGLE__UNIT
        Format: STRING [1] (32)
        Values: DDDDDD
  + (01 at 02): EEEEEE
  + (01 at 03): FFFFFF
  + (01 at 04): GGGGGG
  + (01 at 05): HHHHHH
  + (01 at 06): IIIIII
  + (01 at 07): JJJJJJ
  + (01 at 08): KKKKKK
  + (01 at 09): LLLLLL
  + (01 at 10): MMMMMM

Keywords (Opt): ARTICULATION_DEVICE_ANGLE_NAME
        Format: STRING [1] (64)
        Values: NNNNNN
  + (01 at 02): OOOOOO
  + (01 at 03): PPPPPP
  + (01 at 04): QQQQQQ
  + (01 at 05): RRRRRR
  + (01 at 06): SSSSSS
  + (01 at 07): TTTTTT
  + (01 at 08): UUUUUU
  + (01 at 09): VVVVVV
  + (01 at 10): WWWWWW

Keywords (Opt): ARTICULATION_DEVICE_COUNT
        Format: REAL [1] (4)
        Values: 200.000000
  + (01 at 02): 210.000000
  + (01 at 03): 220.000000
  + (01 at 04): 230.000000
  + (01 at 05): 240.000000
  + (01 at 06): 250.000000
  + (01 at 07): 260.000000
  + (01 at 08): 270.000000
  + (01 at 09): 280.000000
  + (01 at 10): 290.000000

Keywords (Opt): ARTICULATION_DEVICE_COUNT__UNIT
        Format: STRING [1] (32)
        Values: XXXXXX
  + (01 at 02): YYYYYY
  + (01 at 03): ZZZZZZ
  + (01 at 04): aaaaaa
  + (01 at 05): bbbbbb
  + (01 at 06): cccccc
  + (01 at 07): dddddd
  + (01 at 08): eeeeee
  + (01 at 09): ffffff
  + (01 at 10): gggggg

Keywords (Opt): ARTICULATION_DEVICE_COUNT_NAME
        Format: STRING [1] (64)
        Values: hhhhhh
  + (01 at 02): iiiiii
  + (01 at 03): jjjjjj
  + (01 at 04): kkkkkk
  + (01 at 05): llllll
  + (01 at 06): mmmmmm
  + (01 at 07): nnnnnn
  + (01 at 08): oooooo
  + (01 at 09): pppppp
  + (01 at 10): qqqqqq

Keywords (Opt): ARTICULATION_DEV_LOCATION
        Format: REAL [1] (4)
        Values: 300.000000
  + (01 at 02): 310.000000
  + (01 at 03): 320.000000
  + (01 at 04): 330.000000
  + (01 at 05): 340.000000
  + (01 at 06): 350.000000
  + (01 at 07): 360.000000
  + (01 at 08): 370.000000
  + (01 at 09): 380.000000
  + (01 at 10): 390.000000

Keywords (Opt): ARTICULATION_DEV_LOCATION__UNIT
        Format: STRING [1] (32)
        Values: rrrrrr
  + (01 at 02): ssssss
  + (01 at 03): tttttt
  + (01 at 04): uuuuuu
  + (01 at 05): vvvvvv
  + (01 at 06): wwwwww
  + (01 at 07): xxxxxx
  + (01 at 08): yyyyyy
  + (01 at 09): zzzzzz
  + (01 at 10): 000000

Keywords (Opt): ARTICULATION_DEV_LOCATION_NAME
        Format: STRING [1] (64)
        Values: 111111
  + (01 at 02): 222222
  + (01 at 03): 333333
  + (01 at 04): 444444
  + (01 at 05): 555555
  + (01 at 06): 666666
  + (01 at 07): 777777
  + (01 at 08): 888888
  + (01 at 09): 999999
  + (01 at 10): AAAAAA

Keywords (Opt): ARTICULATION_DEV_ORIENT
        Format: REAL [1] (4)
        Values: 400.000000
  + (01 at 02): 410.000000
  + (01 at 03): 420.000000
  + (01 at 04): 430.000000
  + (01 at 05): 440.000000
  + (01 at 06): 450.000000
  + (01 at 07): 460.000000
  + (01 at 08): 470.000000
  + (01 at 09): 480.000000
  + (01 at 10): 490.000000

Keywords (Opt): ARTICULATION_DEV_ORIENT__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB
  + (01 at 02): CCCCCC
  + (01 at 03): DDDDDD
  + (01 at 04): EEEEEE
  + (01 at 05): FFFFFF
  + (01 at 06): GGGGGG
  + (01 at 07): HHHHHH
  + (01 at 08): IIIIII
  + (01 at 09): JJJJJJ
  + (01 at 10): KKKKKK

Keywords (Opt): ARTICULATION_DEV_ORIENT_NAME
        Format: STRING [1] (64)
        Values: LLLLLL
  + (01 at 02): MMMMMM
  + (01 at 03): NNNNNN
  + (01 at 04): OOOOOO
  + (01 at 05): PPPPPP
  + (01 at 06): QQQQQQ
  + (01 at 07): RRRRRR
  + (01 at 08): SSSSSS
  + (01 at 09): TTTTTT
  + (01 at 10): UUUUUU

Keywords (Opt): ARTICULATION_DEV_POSITION
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2

Keywords (Opt): ARTICULATION_DEV_POSITION_ID
        Format: STRING [1] (48)
        Values: VVVVVV
  + (01 at 02): WWWWWW
  + (01 at 03): XXXXXX

Keywords (Opt): ARTICULATION_DEV_POSITION_NAME
        Format: STRING [1] (64)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa

Keywords (Opt): ARTICULATION_DEVICE_MODE
        Format: STRING [1] (48)
        Values: bbbbbb

Keywords (Opt): ARTICULATION_DEVICE_TEMP
        Format: REAL [1] (4)
        Values: 500.000000
  + (01 at 02): 510.000000

Keywords (Opt): ARTICULATION_DEVICE_TEMP__UNIT
        Format: STRING [1] (32)
        Values: cccccc
  + (01 at 02): dddddd

Keywords (Opt): ARTICULATION_DEVICE_TEMP_NAME
        Format: STRING [1] (64)
        Values: eeeeee
  + (01 at 02): ffffff

Keywords (Opt): ARTICULATION_DEV_VECTOR
        Format: REAL [3] (12)
        Values: 520.000000
                530.000000
                540.000000

Keywords (Opt): ARTICULATION_DEV_INSTRUMENT_ID
        Format: STRING [1] (48)
        Values: gggggg

Keywords (Opt): ARTICULATION_DEV_VECTOR_NAME
        Format: STRING [1] (64)
        Values: hhhhhh

Keywords (Opt): CONTACT_SENSOR_STATE
        Format: STRING [1] (48)
        Values: iiiiii
  + (01 at 02): jjjjjj
  + (01 at 03): kkkkkk
  + (01 at 04): llllll
  + (01 at 05): mmmmmm
  + (01 at 06): nnnnnn
  + (01 at 07): oooooo
  + (01 at 08): pppppp

Keywords (Opt): CONTACT_SENSOR_STATE_NAME
        Format: STRING [1] (64)
        Values: qqqqqq
  + (01 at 02): rrrrrr
  + (01 at 03): ssssss
  + (01 at 04): tttttt
  + (01 at 05): uuuuuu
  + (01 at 06): vvvvvv
  + (01 at 07): wwwwww
  + (01 at 08): xxxxxx

Keywords (Opt): ARTICULATION_DEV_INSTRUMENT_ID
        Format: STRING [1] (48)
        Values: yyyyyy

Keywords (Opt): ARTICULATION_DEVICE_STATE
        Format: STRING [1] (48)
        Values: zzzzzz
  + (01 at 02): 000000
  + (01 at 03): 111111
  + (01 at 04): 222222
  + (01 at 05): 333333
  + (01 at 06): 444444
  + (01 at 07): 555555
  + (01 at 08): 666666
  + (01 at 09): 777777
  + (01 at 10): 888888
  + (01 at 11): 999999
  + (01 at 12): AAAAAA

Keywords (Opt): ARTICULATION_DEVICE_STATE_NAME
        Format: STRING [1] (64)
        Values: BBBBBB
  + (01 at 02): CCCCCC
  + (01 at 03): DDDDDD
  + (01 at 04): EEEEEE
  + (01 at 05): FFFFFF
  + (01 at 06): GGGGGG
  + (01 at 07): HHHHHH
  + (01 at 08): IIIIII
  + (01 at 09): JJJJJJ
  + (01 at 10): KKKKKK
  + (01 at 11): LLLLLL
  + (01 at 12): MMMMMM



Label Type: PROPERTY
PROPERTY Name: COORDINATE_SYSTEM

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): COORDINATE_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): COORDINATE_SYSTEM_INDEX_NAME
        Format: STRING [1] (64)
        Values: BBBBBB
  + (01 at 02): CCCCCC
  + (01 at 03): DDDDDD
  + (01 at 04): EEEEEE
  + (01 at 05): FFFFFF
  + (01 at 06): GGGGGG

Keywords (Opt): COORDINATE_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: HHHHHH

Keywords (Opt): ORIGIN_OFFSET_VECTOR
        Format: REAL [3] (12)
        Values: 100.000000
                110.000000
                120.000000

Keywords (Opt): ORIGIN_ROTATION_QUATERNION
        Format: REAL [4] (16)
        Values: 130.000000
                140.000000
                150.000000
                160.000000

Keywords (Opt): POSITIVE_AZIMUTH_DIRECTION
        Format: STRING [1] (64)
        Values: IIIIII

Keywords (Opt): POSITIVE_ELEVATION_DIRECTION
        Format: STRING [1] (64)
        Values: JJJJJJ

Keywords (Opt): QUATERNION_MEASUREMENT_METHOD
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 6
  + (01 at 02): 7
  + (01 at 03): 8
  + (01 at 04): 9
  + (01 at 05): 10
  + (01 at 06): 11

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: LLLLLL

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: MMMMMM



Label Type: PROPERTY
PROPERTY Name: GROUND_SUPPORT_EQUIPMENT

Keywords (Opt): CAMERA_LOCATION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): FACILITY_NAME
        Format: STRING [1] (64)
        Values: BBBBBB

Keywords (Opt): LIGHT_SOURCE_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): LIGHT_SOURCE_DISTANCE
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): LIGHT_SOURCE_DISTANCE__UNIT
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): LIGHT_SOURCE_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): PRESSURE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Req): PRODUCER_FULL_NAME
        Format: STRING [1] (128)
        Values: GGGGGG

Keywords (Opt): TARGET_DISTANCE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): TARGET_DISTANCE__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): TARGET_NAME
        Format: STRING [1] (64)
        Values: IIIIII

Keywords (Opt): TEST_PHASE_NAME
        Format: STRING [1] (64)
        Values: JJJJJJ

Keywords (Opt): NOTE
        Format: STRING [1] (256)
        Values: KKKKKK



Label Type: PROPERTY
PROPERTY Name: DERIVED_IMAGE

Keywords (Opt): CONFIGURATION_BAND_ID
        Format: STRING [1] (48)
        Values: AAAAAA
  + (01 at 02): BBBBBB
  + (01 at 03): CCCCCC
  + (01 at 04): DDDDDD
  + (01 at 05): EEEEEE
  + (01 at 06): FFFFFF
  + (01 at 07): GGGGGG
  + (01 at 08): HHHHHH
  + (01 at 09): IIIIII
  + (01 at 10): JJJJJJ
  + (01 at 11): KKKKKK
  + (01 at 12): LLLLLL
  + (01 at 13): MMMMMM
  + (01 at 14): NNNNNN
  + (01 at 15): OOOOOO
  + (01 at 16): PPPPPP

Keywords (Opt): DERIVED_IMAGE_TYPE
        Format: STRING [1] (32)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_BAND_ID
        Format: STRING [1] (48)
        Values: RRRRRR
  + (01 at 02): SSSSSS
  + (01 at 03): TTTTTT
  + (01 at 04): UUUUUU
  + (01 at 05): VVVVVV
  + (01 at 06): WWWWWW
  + (01 at 07): XXXXXX
  + (01 at 08): YYYYYY
  + (01 at 09): ZZZZZZ
  + (01 at 10): aaaaaa
  + (01 at 11): bbbbbb
  + (01 at 12): cccccc
  + (01 at 13): dddddd
  + (01 at 14): eeeeee
  + (01 at 15): ffffff
  + (01 at 16): gggggg

Keywords (Opt): RADIANCE_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): RADIANCE_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: hhhhhh

Keywords (Opt): RADIANCE_SCALING_FACTOR
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): RADIANCE_SCALING_FACTOR__UNIT
        Format: STRING [1] (32)
        Values: iiiiii

Keywords (Opt): RADIOMETRIC_CORRECTION_TYPE
        Format: STRING [1] (32)
        Values: jjjjjj

Keywords (Opt): RANGE_ORIGIN_VECTOR
        Format: REAL [3] (12)
        Values: 120.000000
                130.000000
                140.000000

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: kkkkkk

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: llllll



Label Type: PROPERTY
PROPERTY Name: DERIVED_IMAGE_PARMS

Keywords (Opt): CONFIGURATION_BAND_ID
        Format: STRING [1] (48)
        Values: AAAAAA
  + (01 at 02): BBBBBB
  + (01 at 03): CCCCCC
  + (01 at 04): DDDDDD
  + (01 at 05): EEEEEE
  + (01 at 06): FFFFFF
  + (01 at 07): GGGGGG
  + (01 at 08): HHHHHH
  + (01 at 09): IIIIII
  + (01 at 10): JJJJJJ
  + (01 at 11): KKKKKK
  + (01 at 12): LLLLLL
  + (01 at 13): MMMMMM
  + (01 at 14): NNNNNN
  + (01 at 15): OOOOOO
  + (01 at 16): PPPPPP

Keywords (Opt): DERIVED_IMAGE_TYPE
        Format: STRING [1] (32)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_BAND_ID
        Format: STRING [1] (48)
        Values: RRRRRR
  + (01 at 02): SSSSSS
  + (01 at 03): TTTTTT
  + (01 at 04): UUUUUU
  + (01 at 05): VVVVVV
  + (01 at 06): WWWWWW
  + (01 at 07): XXXXXX
  + (01 at 08): YYYYYY
  + (01 at 09): ZZZZZZ
  + (01 at 10): aaaaaa
  + (01 at 11): bbbbbb
  + (01 at 12): cccccc
  + (01 at 13): dddddd
  + (01 at 14): eeeeee
  + (01 at 15): ffffff
  + (01 at 16): gggggg

Keywords (Opt): RADIANCE_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): RADIANCE_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: hhhhhh

Keywords (Opt): RADIANCE_SCALING_FACTOR
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): RADIANCE_SCALING_FACTOR__UNIT
        Format: STRING [1] (32)
        Values: iiiiii

Keywords (Opt): RADIOMETRIC_CORRECTION_TYPE
        Format: STRING [1] (32)
        Values: jjjjjj

Keywords (Opt): RANGE_ORIGIN_VECTOR
        Format: REAL [3] (12)
        Values: 120.000000
                130.000000
                140.000000

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: kkkkkk

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: llllll



Label Type: PROPERTY
PROPERTY Name: COMMAND

Keywords (Opt): AUTO_EXPOSURE_DATA_CUT
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): AUTO_EXPOSURE_PIXEL_FRACTION
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): BAD_PIXEL_REPLACEMENT_FLAG
        Format: STRING [1] (8)
        Values: AAAAAA

Keywords (Opt): COMMAND_DESC
        Format: STRING [1] (256)
        Values: BBBBBB

Keywords (Req): COMMAND_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): DARK_CURRENT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: DDDDDD

Keywords (Opt): DOWNLOAD_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): FLAT_FIELD_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: FFFFFF

Keywords (Opt): INSTRUMENT_MODE_ID
        Format: STRING [1] (48)
        Values: GGGGGG

Keywords (Opt): MAX_AUTO_EXPOS_ITERATION_COUNT
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): SHUTTER_EFFECT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: HHHHHH

Keywords (Opt): SQRT_COMPRESSION_FLAG
        Format: STRING [1] (8)
        Values: IIIIII



Label Type: PROPERTY
PROPERTY Name: COMPRESSION

Keywords (Opt): ERROR_PIXELS
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): INST_CMPRS_BLK_SIZE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): INST_CMPRS_BLOCKS
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): INST_CMPRS_DESC
        Format: STRING [1] (256)
        Values: AAAAAA

Keywords (Opt): INST_CMPRS_FILTER
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): INST_CMPRS_ENTROPY
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): INST_CMPRS_MODE
        Format: INT [1] (4)
        Values: 3

Keywords (Req): INST_CMPRS_NAME
        Format: STRING [1] (128)
        Values: CCCCCC

Keywords (Opt): INST_CMPRS_PARAM
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): INST_CMPRS_QUALITY
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_CMPRS_QUANTZ_TBL_ID
        Format: STRING [1] (48)
        Values: DDDDDD

Keywords (Opt): INST_CMPRS_QUANTZ_TYPE
        Format: STRING [1] (256)
        Values: EEEEEE

Keywords (Opt): INST_CMPRS_RATE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Req): INST_CMPRS_RATIO
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): INST_CMPRS_SEGMENTS
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INST_CMPRS_SEGMENT_STATUS
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ
  + (01 at 06): KKKKKK
  + (01 at 07): LLLLLL
  + (01 at 08): MMMMMM
  + (01 at 09): NNNNNN
  + (01 at 10): OOOOOO
  + (01 at 11): PPPPPP
  + (01 at 12): QQQQQQ
  + (01 at 13): RRRRRR
  + (01 at 14): SSSSSS
  + (01 at 15): TTTTTT
  + (01 at 16): UUUUUU
  + (01 at 17): VVVVVV
  + (01 at 18): WWWWWW
  + (01 at 19): XXXXXX
  + (01 at 20): YYYYYY
  + (01 at 21): ZZZZZZ
  + (01 at 22): aaaaaa
  + (01 at 23): bbbbbb
  + (01 at 24): cccccc
  + (01 at 25): dddddd
  + (01 at 26): eeeeee
  + (01 at 27): ffffff
  + (01 at 28): gggggg
  + (01 at 29): hhhhhh
  + (01 at 30): iiiiii
  + (01 at 31): jjjjjj
  + (01 at 32): kkkkkk

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE
        Format: INT [1] (4)
        Values: 7
  + (01 at 02): 8
  + (01 at 03): 9
  + (01 at 04): 10
  + (01 at 05): 11
  + (01 at 06): 12
  + (01 at 07): 13
  + (01 at 08): 14
  + (01 at 09): 15
  + (01 at 10): 16
  + (01 at 11): 17
  + (01 at 12): 18
  + (01 at 13): 19
  + (01 at 14): 20
  + (01 at 15): 21
  + (01 at 16): 22
  + (01 at 17): 23
  + (01 at 18): 24
  + (01 at 19): 25
  + (01 at 20): 26
  + (01 at 21): 27
  + (01 at 22): 28
  + (01 at 23): 29
  + (01 at 24): 30
  + (01 at 25): 31
  + (01 at 26): 32
  + (01 at 27): 33
  + (01 at 28): 34
  + (01 at 29): 35
  + (01 at 30): 36
  + (01 at 31): 37
  + (01 at 32): 38

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE_SAMP
        Format: INT [1] (4)
        Values: 39
  + (01 at 02): 40
  + (01 at 03): 41
  + (01 at 04): 42
  + (01 at 05): 43
  + (01 at 06): 44
  + (01 at 07): 45
  + (01 at 08): 46
  + (01 at 09): 47
  + (01 at 10): 48
  + (01 at 11): 49
  + (01 at 12): 50
  + (01 at 13): 51
  + (01 at 14): 52
  + (01 at 15): 53
  + (01 at 16): 54
  + (01 at 17): 55
  + (01 at 18): 56
  + (01 at 19): 57
  + (01 at 20): 58
  + (01 at 21): 59
  + (01 at 22): 60
  + (01 at 23): 61
  + (01 at 24): 62
  + (01 at 25): 63
  + (01 at 26): 64
  + (01 at 27): 65
  + (01 at 28): 66
  + (01 at 29): 67
  + (01 at 30): 68
  + (01 at 31): 69
  + (01 at 32): 70

Keywords (Opt): INST_CMPRS_SEG_LINES
        Format: INT [1] (4)
        Values: 71
  + (01 at 02): 72
  + (01 at 03): 73
  + (01 at 04): 74
  + (01 at 05): 75
  + (01 at 06): 76
  + (01 at 07): 77
  + (01 at 08): 78
  + (01 at 09): 79
  + (01 at 10): 80
  + (01 at 11): 81
  + (01 at 12): 82
  + (01 at 13): 83
  + (01 at 14): 84
  + (01 at 15): 85
  + (01 at 16): 86
  + (01 at 17): 87
  + (01 at 18): 88
  + (01 at 19): 89
  + (01 at 20): 90
  + (01 at 21): 91
  + (01 at 22): 92
  + (01 at 23): 93
  + (01 at 24): 94
  + (01 at 25): 95
  + (01 at 26): 96
  + (01 at 27): 97
  + (01 at 28): 98
  + (01 at 29): 99
  + (01 at 30): 100
  + (01 at 31): 101
  + (01 at 32): 102

Keywords (Opt): INST_CMPRS_SEG_SAMPLES
        Format: INT [1] (4)
        Values: 103
  + (01 at 02): 104
  + (01 at 03): 105
  + (01 at 04): 106
  + (01 at 05): 107
  + (01 at 06): 108
  + (01 at 07): 109
  + (01 at 08): 110
  + (01 at 09): 111
  + (01 at 10): 112
  + (01 at 11): 113
  + (01 at 12): 114
  + (01 at 13): 115
  + (01 at 14): 116
  + (01 at 15): 117
  + (01 at 16): 118
  + (01 at 17): 119
  + (01 at 18): 120
  + (01 at 19): 121
  + (01 at 20): 122
  + (01 at 21): 123
  + (01 at 22): 124
  + (01 at 23): 125
  + (01 at 24): 126
  + (01 at 25): 127
  + (01 at 26): 128
  + (01 at 27): 129
  + (01 at 28): 130
  + (01 at 29): 131
  + (01 at 30): 132
  + (01 at 31): 133
  + (01 at 32): 134

Keywords (Opt): INST_CMPRS_SEG_MISSING_PIXELS
        Format: INT [1] (4)
        Values: 135
  + (01 at 02): 136
  + (01 at 03): 137
  + (01 at 04): 138
  + (01 at 05): 139
  + (01 at 06): 140
  + (01 at 07): 141
  + (01 at 08): 142
  + (01 at 09): 143
  + (01 at 10): 144
  + (01 at 11): 145
  + (01 at 12): 146
  + (01 at 13): 147
  + (01 at 14): 148
  + (01 at 15): 149
  + (01 at 16): 150
  + (01 at 17): 151
  + (01 at 18): 152
  + (01 at 19): 153
  + (01 at 20): 154
  + (01 at 21): 155
  + (01 at 22): 156
  + (01 at 23): 157
  + (01 at 24): 158
  + (01 at 25): 159
  + (01 at 26): 160
  + (01 at 27): 161
  + (01 at 28): 162
  + (01 at 29): 163
  + (01 at 30): 164
  + (01 at 31): 165
  + (01 at 32): 166

Keywords (Opt): INST_CMPRS_SEGMENT_QUALITY
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000
  + (01 at 04): 160.000000
  + (01 at 05): 170.000000
  + (01 at 06): 180.000000
  + (01 at 07): 190.000000
  + (01 at 08): 200.000000
  + (01 at 09): 210.000000
  + (01 at 10): 220.000000
  + (01 at 11): 230.000000
  + (01 at 12): 240.000000
  + (01 at 13): 250.000000
  + (01 at 14): 260.000000
  + (01 at 15): 270.000000
  + (01 at 16): 280.000000
  + (01 at 17): 290.000000
  + (01 at 18): 300.000000
  + (01 at 19): 310.000000
  + (01 at 20): 320.000000
  + (01 at 21): 330.000000
  + (01 at 22): 340.000000
  + (01 at 23): 350.000000
  + (01 at 24): 360.000000
  + (01 at 25): 370.000000
  + (01 at 26): 380.000000
  + (01 at 27): 390.000000
  + (01 at 28): 400.000000
  + (01 at 29): 410.000000
  + (01 at 30): 420.000000
  + (01 at 31): 430.000000
  + (01 at 32): 440.000000

Keywords (Opt): INST_CMPRS_SYNC_BLKS
        Format: INT [1] (4)
        Values: 167

Keywords (Opt): INST_DECOMP_STAGES
        Format: INT [1] (4)
        Values: 168

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 169

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 170

Keywords (Opt): RICE_OPTION_VALUE
        Format: INT [1] (4)
        Values: 171

Keywords (Opt): RICE_START_OPTION
        Format: INT [1] (4)
        Values: 172

Keywords (Opt): SQRT_MAXIMUM_PIXEL
        Format: INT [1] (4)
        Values: 173

Keywords (Opt): SQRT_MINIMUM_PIXEL
        Format: INT [1] (4)
        Values: 174



Label Type: PROPERTY
PROPERTY Name: COMPRESSION_PARMS

Keywords (Opt): ERROR_PIXELS
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): INST_CMPRS_BLK_SIZE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): INST_CMPRS_BLOCKS
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): INST_CMPRS_DESC
        Format: STRING [1] (256)
        Values: AAAAAA

Keywords (Opt): INST_CMPRS_FILTER
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): INST_CMPRS_ENTROPY
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): INST_CMPRS_MODE
        Format: INT [1] (4)
        Values: 3

Keywords (Req): INST_CMPRS_NAME
        Format: STRING [1] (128)
        Values: CCCCCC

Keywords (Opt): INST_CMPRS_PARAM
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): INST_CMPRS_QUALITY
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_CMPRS_QUANTZ_TBL_ID
        Format: STRING [1] (48)
        Values: DDDDDD

Keywords (Opt): INST_CMPRS_QUANTZ_TYPE
        Format: STRING [1] (256)
        Values: EEEEEE

Keywords (Opt): INST_CMPRS_RATE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Req): INST_CMPRS_RATIO
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): INST_CMPRS_SEGMENTS
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INST_CMPRS_SEGMENT_STATUS
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ
  + (01 at 06): KKKKKK
  + (01 at 07): LLLLLL
  + (01 at 08): MMMMMM
  + (01 at 09): NNNNNN
  + (01 at 10): OOOOOO
  + (01 at 11): PPPPPP
  + (01 at 12): QQQQQQ
  + (01 at 13): RRRRRR
  + (01 at 14): SSSSSS
  + (01 at 15): TTTTTT
  + (01 at 16): UUUUUU
  + (01 at 17): VVVVVV
  + (01 at 18): WWWWWW
  + (01 at 19): XXXXXX
  + (01 at 20): YYYYYY
  + (01 at 21): ZZZZZZ
  + (01 at 22): aaaaaa
  + (01 at 23): bbbbbb
  + (01 at 24): cccccc
  + (01 at 25): dddddd
  + (01 at 26): eeeeee
  + (01 at 27): ffffff
  + (01 at 28): gggggg
  + (01 at 29): hhhhhh
  + (01 at 30): iiiiii
  + (01 at 31): jjjjjj
  + (01 at 32): kkkkkk

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE
        Format: INT [1] (4)
        Values: 7
  + (01 at 02): 8
  + (01 at 03): 9
  + (01 at 04): 10
  + (01 at 05): 11
  + (01 at 06): 12
  + (01 at 07): 13
  + (01 at 08): 14
  + (01 at 09): 15
  + (01 at 10): 16
  + (01 at 11): 17
  + (01 at 12): 18
  + (01 at 13): 19
  + (01 at 14): 20
  + (01 at 15): 21
  + (01 at 16): 22
  + (01 at 17): 23
  + (01 at 18): 24
  + (01 at 19): 25
  + (01 at 20): 26
  + (01 at 21): 27
  + (01 at 22): 28
  + (01 at 23): 29
  + (01 at 24): 30
  + (01 at 25): 31
  + (01 at 26): 32
  + (01 at 27): 33
  + (01 at 28): 34
  + (01 at 29): 35
  + (01 at 30): 36
  + (01 at 31): 37
  + (01 at 32): 38

Keywords (Opt): INST_CMPRS_SEG_FIRST_LINE_SAMP
        Format: INT [1] (4)
        Values: 39
  + (01 at 02): 40
  + (01 at 03): 41
  + (01 at 04): 42
  + (01 at 05): 43
  + (01 at 06): 44
  + (01 at 07): 45
  + (01 at 08): 46
  + (01 at 09): 47
  + (01 at 10): 48
  + (01 at 11): 49
  + (01 at 12): 50
  + (01 at 13): 51
  + (01 at 14): 52
  + (01 at 15): 53
  + (01 at 16): 54
  + (01 at 17): 55
  + (01 at 18): 56
  + (01 at 19): 57
  + (01 at 20): 58
  + (01 at 21): 59
  + (01 at 22): 60
  + (01 at 23): 61
  + (01 at 24): 62
  + (01 at 25): 63
  + (01 at 26): 64
  + (01 at 27): 65
  + (01 at 28): 66
  + (01 at 29): 67
  + (01 at 30): 68
  + (01 at 31): 69
  + (01 at 32): 70

Keywords (Opt): INST_CMPRS_SEG_LINES
        Format: INT [1] (4)
        Values: 71
  + (01 at 02): 72
  + (01 at 03): 73
  + (01 at 04): 74
  + (01 at 05): 75
  + (01 at 06): 76
  + (01 at 07): 77
  + (01 at 08): 78
  + (01 at 09): 79
  + (01 at 10): 80
  + (01 at 11): 81
  + (01 at 12): 82
  + (01 at 13): 83
  + (01 at 14): 84
  + (01 at 15): 85
  + (01 at 16): 86
  + (01 at 17): 87
  + (01 at 18): 88
  + (01 at 19): 89
  + (01 at 20): 90
  + (01 at 21): 91
  + (01 at 22): 92
  + (01 at 23): 93
  + (01 at 24): 94
  + (01 at 25): 95
  + (01 at 26): 96
  + (01 at 27): 97
  + (01 at 28): 98
  + (01 at 29): 99
  + (01 at 30): 100
  + (01 at 31): 101
  + (01 at 32): 102

Keywords (Opt): INST_CMPRS_SEG_SAMPLES
        Format: INT [1] (4)
        Values: 103
  + (01 at 02): 104
  + (01 at 03): 105
  + (01 at 04): 106
  + (01 at 05): 107
  + (01 at 06): 108
  + (01 at 07): 109
  + (01 at 08): 110
  + (01 at 09): 111
  + (01 at 10): 112
  + (01 at 11): 113
  + (01 at 12): 114
  + (01 at 13): 115
  + (01 at 14): 116
  + (01 at 15): 117
  + (01 at 16): 118
  + (01 at 17): 119
  + (01 at 18): 120
  + (01 at 19): 121
  + (01 at 20): 122
  + (01 at 21): 123
  + (01 at 22): 124
  + (01 at 23): 125
  + (01 at 24): 126
  + (01 at 25): 127
  + (01 at 26): 128
  + (01 at 27): 129
  + (01 at 28): 130
  + (01 at 29): 131
  + (01 at 30): 132
  + (01 at 31): 133
  + (01 at 32): 134

Keywords (Opt): INST_CMPRS_SEG_MISSING_PIXELS
        Format: INT [1] (4)
        Values: 135
  + (01 at 02): 136
  + (01 at 03): 137
  + (01 at 04): 138
  + (01 at 05): 139
  + (01 at 06): 140
  + (01 at 07): 141
  + (01 at 08): 142
  + (01 at 09): 143
  + (01 at 10): 144
  + (01 at 11): 145
  + (01 at 12): 146
  + (01 at 13): 147
  + (01 at 14): 148
  + (01 at 15): 149
  + (01 at 16): 150
  + (01 at 17): 151
  + (01 at 18): 152
  + (01 at 19): 153
  + (01 at 20): 154
  + (01 at 21): 155
  + (01 at 22): 156
  + (01 at 23): 157
  + (01 at 24): 158
  + (01 at 25): 159
  + (01 at 26): 160
  + (01 at 27): 161
  + (01 at 28): 162
  + (01 at 29): 163
  + (01 at 30): 164
  + (01 at 31): 165
  + (01 at 32): 166

Keywords (Opt): INST_CMPRS_SEGMENT_QUALITY
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000
  + (01 at 04): 160.000000
  + (01 at 05): 170.000000
  + (01 at 06): 180.000000
  + (01 at 07): 190.000000
  + (01 at 08): 200.000000
  + (01 at 09): 210.000000
  + (01 at 10): 220.000000
  + (01 at 11): 230.000000
  + (01 at 12): 240.000000
  + (01 at 13): 250.000000
  + (01 at 14): 260.000000
  + (01 at 15): 270.000000
  + (01 at 16): 280.000000
  + (01 at 17): 290.000000
  + (01 at 18): 300.000000
  + (01 at 19): 310.000000
  + (01 at 20): 320.000000
  + (01 at 21): 330.000000
  + (01 at 22): 340.000000
  + (01 at 23): 350.000000
  + (01 at 24): 360.000000
  + (01 at 25): 370.000000
  + (01 at 26): 380.000000
  + (01 at 27): 390.000000
  + (01 at 28): 400.000000
  + (01 at 29): 410.000000
  + (01 at 30): 420.000000
  + (01 at 31): 430.000000
  + (01 at 32): 440.000000

Keywords (Opt): INST_CMPRS_SYNC_BLKS
        Format: INT [1] (4)
        Values: 167

Keywords (Opt): INST_DECOMP_STAGES
        Format: INT [1] (4)
        Values: 168

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 169

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 170

Keywords (Opt): RICE_OPTION_VALUE
        Format: INT [1] (4)
        Values: 171

Keywords (Opt): RICE_START_OPTION
        Format: INT [1] (4)
        Values: 172

Keywords (Opt): SQRT_MAXIMUM_PIXEL
        Format: INT [1] (4)
        Values: 173

Keywords (Opt): SQRT_MINIMUM_PIXEL
        Format: INT [1] (4)
        Values: 174



Label Type: PROPERTY
PROPERTY Name: CAMERA_MODEL

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Req): CALIBRATION_SOURCE_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): MODEL_DESC__PTR
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MODEL_NAME
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Req): MODEL_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): GEOMETRY_SOURCE_ID
        Format: STRING [1] (48)
        Values: FFFFFF

Keywords (Opt): MODEL_COMPONENT_ID
        Format: STRING [1] (8)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK
  + (01 at 06): LLLLLL
  + (01 at 07): MMMMMM
  + (01 at 08): NNNNNN
  + (01 at 09): OOOOOO

Keywords (Opt): MODEL_COMPONENT_NAME
        Format: STRING [1] (64)
        Values: PPPPPP
  + (01 at 02): QQQQQQ
  + (01 at 03): RRRRRR
  + (01 at 04): SSSSSS
  + (01 at 05): TTTTTT
  + (01 at 06): UUUUUU
  + (01 at 07): VVVVVV
  + (01 at 08): WWWWWW
  + (01 at 09): XXXXXX

Keywords (Opt): MODEL_COMPONENT_UNIT
        Format: STRING [1] (32)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa
  + (01 at 04): bbbbbb
  + (01 at 05): cccccc
  + (01 at 06): dddddd
  + (01 at 07): eeeeee
  + (01 at 08): ffffff
  + (01 at 09): gggggg

Keywords (Opt): MODEL_COMPONENT_1
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): MODEL_COMPONENT_2
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000

Keywords (Opt): MODEL_COMPONENT_3
        Format: REAL [1] (4)
        Values: 160.000000
  + (01 at 02): 170.000000
  + (01 at 03): 180.000000

Keywords (Opt): MODEL_COMPONENT_4
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): MODEL_COMPONENT_5
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): MODEL_COMPONENT_6
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000

Keywords (Opt): MODEL_COMPONENT_7
        Format: REAL [1] (4)
        Values: 280.000000
  + (01 at 02): 290.000000
  + (01 at 03): 300.000000

Keywords (Opt): MODEL_COMPONENT_8
        Format: REAL [1] (4)
        Values: 310.000000
  + (01 at 02): 320.000000
  + (01 at 03): 330.000000

Keywords (Opt): MODEL_COMPONENT_9
        Format: REAL [1] (4)
        Values: 340.000000
  + (01 at 02): 350.000000
  + (01 at 03): 360.000000

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (48)
        Values: hhhhhh

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: iiiiii

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: jjjjjj



Label Type: PROPERTY
PROPERTY Name: GEOMETRIC_CAMERA_MODEL

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Req): CALIBRATION_SOURCE_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): MODEL_DESC__PTR
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MODEL_NAME
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Req): MODEL_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): GEOMETRY_SOURCE_ID
        Format: STRING [1] (48)
        Values: FFFFFF

Keywords (Opt): MODEL_COMPONENT_ID
        Format: STRING [1] (8)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK
  + (01 at 06): LLLLLL
  + (01 at 07): MMMMMM
  + (01 at 08): NNNNNN
  + (01 at 09): OOOOOO

Keywords (Opt): MODEL_COMPONENT_NAME
        Format: STRING [1] (64)
        Values: PPPPPP
  + (01 at 02): QQQQQQ
  + (01 at 03): RRRRRR
  + (01 at 04): SSSSSS
  + (01 at 05): TTTTTT
  + (01 at 06): UUUUUU
  + (01 at 07): VVVVVV
  + (01 at 08): WWWWWW
  + (01 at 09): XXXXXX

Keywords (Opt): MODEL_COMPONENT_UNIT
        Format: STRING [1] (32)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa
  + (01 at 04): bbbbbb
  + (01 at 05): cccccc
  + (01 at 06): dddddd
  + (01 at 07): eeeeee
  + (01 at 08): ffffff
  + (01 at 09): gggggg

Keywords (Opt): MODEL_COMPONENT_1
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): MODEL_COMPONENT_2
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000

Keywords (Opt): MODEL_COMPONENT_3
        Format: REAL [1] (4)
        Values: 160.000000
  + (01 at 02): 170.000000
  + (01 at 03): 180.000000

Keywords (Opt): MODEL_COMPONENT_4
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): MODEL_COMPONENT_5
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): MODEL_COMPONENT_6
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000

Keywords (Opt): MODEL_COMPONENT_7
        Format: REAL [1] (4)
        Values: 280.000000
  + (01 at 02): 290.000000
  + (01 at 03): 300.000000

Keywords (Opt): MODEL_COMPONENT_8
        Format: REAL [1] (4)
        Values: 310.000000
  + (01 at 02): 320.000000
  + (01 at 03): 330.000000

Keywords (Opt): MODEL_COMPONENT_9
        Format: REAL [1] (4)
        Values: 340.000000
  + (01 at 02): 350.000000
  + (01 at 03): 360.000000

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (48)
        Values: hhhhhh

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: iiiiii

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: jjjjjj



Label Type: PROPERTY
PROPERTY Name: DERIVED_GEOMETRY

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): COORDINATE_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: BBBBBB

Keywords (Opt): DERIVED_GEOMETRY_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): DERIVED_GEOMETRY_NAME
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Opt): DERIVED_GEOMETRY_NOTE
        Format: STRING [1] (256)
        Values: EEEEEE

Keywords (Opt): DERIVED_GEOMETRY_TYPE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Opt): LANDER_INSTRUMENT_AZIMUTH, INSTRUMENT_AZIMUTH
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): INSTRUMENT_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: GGGGGG

Keywords (Opt): LANDER_INSTRUMENT_ELEVATION, INSTRUMENT_ELEVATION
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): INSTRUMENT_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): LANDER_LOCAL_LEVEL_QUATERNION, INST_HOST_TO_FIXED_QUATERNION
        Format: REAL [4] (16)
        Values: 120.000000
                130.000000
                140.000000
                150.000000

Keywords (Opt): LCL_LVL_SRFC_FXD_VECTOR, LOCAL_TO_FIXED_OFFSET_VECTOR
        Format: REAL [3] (12)
        Values: 160.000000
                170.000000
                180.000000

Keywords (Opt): LOCAL_LEVEL_INST_AZIMUTH, LOCAL_INSTRUMENT_AZIMUTH
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): LOCAL_LEVEL_INST_ELEVATION, LOCAL_INSTRUMENT_ELEVATION
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): NORTH_AZIMUTH
        Format: REAL [1] (4)
        Values: 210.000000

Keywords (Opt): POSITIVE_AZIMUTH_DIRECTION
        Format: STRING [1] (48)
        Values: IIIIII

Keywords (Opt): POSITIVE_ELEVATION_DIRECTION
        Format: STRING [1] (48)
        Values: JJJJJJ

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: KKKKKK

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: LLLLLL

Keywords (Opt): SLANT_DISTANCE
        Format: REAL [1] (4)
        Values: 220.000000

Keywords (Opt): SMEAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 230.000000

Keywords (Opt): SMEAR_MAGNITUDE
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): SOLAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SOLAR_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): SOLAR_ELEVATION
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SOLAR_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): SRFC_FXD_LCL_LVL_VECTOR,
        Format: REAL [3] (12)
        Values: 270.000000
                280.000000
                290.000000

Keywords (Opt): SURFACE_FIXED_INST_AZIMUTH, FIXED_INSTRUMENT_AZIMUTH
        Format: REAL [1] (4)
        Values: 300.000000

Keywords (Opt): SURFACE_FIXED_INST_ELEVATION, FIXED_INSTRUMENT_ELEVATION
        Format: REAL [1] (4)
        Values: 310.000000



Label Type: PROPERTY
PROPERTY Name: IDENTIFICATION

Keywords (Req): DATA_SET_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): DATA_SET_NAME
        Format: STRING [1] (64)
        Values: BBBBBB

Keywords (Opt): COMMAND_SEQUENCE_NUMBER
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): FEATURE_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): FEATURE_TYPE
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): FRAME_ID
        Format: STRING [1] (48)
        Values: EEEEEE
  + (01 at 02): FFFFFF
  + (01 at 03): GGGGGG
  + (01 at 04): HHHHHH
  + (01 at 05): IIIIII

Keywords (Opt): FRAME_TYPE
        Format: STRING [1] (32)
        Values: JJJJJJ

Keywords (Opt): GEOMETRY_PROJECTION_TYPE
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): IMAGE_ID
        Format: STRING [1] (48)
        Values: LLLLLL

Keywords (Opt): IMAGE_TIME
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): IMAGE_TYPE
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): INSTRUMENT_HOST_ID
        Format: STRING [1] (48)
        Values: OOOOOO
  + (01 at 02): PPPPPP
  + (01 at 03): QQQQQQ
  + (01 at 04): RRRRRR
  + (01 at 05): SSSSSS

Keywords (Req): INSTRUMENT_HOST_NAME
        Format: STRING [1] (64)
        Values: TTTTTT
  + (01 at 02): UUUUUU
  + (01 at 03): VVVVVV
  + (01 at 04): WWWWWW
  + (01 at 05): XXXXXX

Keywords (Opt): INSTRUMENT_ID
        Format: STRING [1] (48)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa
  + (01 at 04): bbbbbb
  + (01 at 05): cccccc

Keywords (Req): INSTRUMENT_NAME
        Format: STRING [1] (64)
        Values: dddddd
  + (01 at 02): eeeeee
  + (01 at 03): ffffff
  + (01 at 04): gggggg
  + (01 at 05): hhhhhh

Keywords (Opt): INSTRUMENT_SERIAL_NUMBER
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): INSTRUMENT_TYPE
        Format: STRING [1] (32)
        Values: iiiiii
  + (01 at 02): jjjjjj
  + (01 at 03): kkkkkk
  + (01 at 04): llllll
  + (01 at 05): mmmmmm

Keywords (Opt): INSTRUMENT_VERSION_ID
        Format: STRING [1] (48)
        Values: nnnnnn

Keywords (Opt): LOCAL_TRUE_SOLAR_TIME
        Format: STRING [1] (32)
        Values: oooooo

Keywords (Opt): MAGNET_ID
        Format: STRING [1] (48)
        Values: pppppp

Keywords (Opt): MEASUREMENT_ID
        Format: STRING [1] (48)
        Values: qqqqqq

Keywords (Opt): MEASUREMENT_TIME
        Format: STRING [1] (32)
        Values: rrrrrr

Keywords (Opt): MEASUREMENT_TYPE
        Format: STRING [1] (32)
        Values: ssssss

Keywords (Req): MISSION_NAME
        Format: STRING [1] (64)
        Values: tttttt
  + (01 at 02): uuuuuu
  + (01 at 03): vvvvvv
  + (01 at 04): wwwwww
  + (01 at 05): xxxxxx

Keywords (Opt): MISSION_PHASE_NAME
        Format: STRING [1] (64)
        Values: yyyyyy

Keywords (Opt): OBSERVATION_ID
        Format: STRING [1] (48)
        Values: zzzzzz

Keywords (Opt): OBSERVATION_NAME
        Format: STRING [1] (64)
        Values: 000000

Keywords (Opt): OBSERVATION_TIME
        Format: STRING [1] (32)
        Values: 111111

Keywords (Opt): OBSERVATION_TYPE
        Format: STRING [1] (32)
        Values: 222222

Keywords (Opt): ORBIT_NUMBER
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): OPS_TOKEN
        Format: STRING [1] (64)
        Values: 333333

Keywords (Opt): OPS_TOKEN_ACTIVITY
        Format: STRING [1] (64)
        Values: 444444

Keywords (Opt): OPS_TOKEN_COMMAND
        Format: STRING [1] (64)
        Values: 555555

Keywords (Opt): OPS_TOKEN_PAYLOAD
        Format: STRING [1] (64)
        Values: 666666

Keywords (Opt): PLANET_DAY_NUMBER
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): PROCESSING_HISTORY_TEXT
        Format: STRING [1] (256)
        Values: 777777

Keywords (Opt): PRODUCER_FULL_NAME
        Format: STRING [1] (64)
        Values: 888888

Keywords (Opt): PRODUCER_ID
        Format: STRING [1] (48)
        Values: 999999

Keywords (Opt): PRODUCER_INSTITUTION_NAME
        Format: STRING [1] (64)
        Values: AAAAAA

Keywords (Req): PRODUCT_CREATION_TIME
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Req): PRODUCT_ID
        Format: STRING [1] (48)
        Values: CCCCCC

Keywords (Opt): PRODUCT_VERSION_ID
        Format: STRING [1] (48)
        Values: DDDDDD

Keywords (Opt): RELEASE_ID
        Format: STRING [1] (48)
        Values: EEEEEE

Keywords (Opt): ROVER_MOTION_COUNTER
        Format: INT [1] (4)
        Values: 3
  + (01 at 02): 4
  + (01 at 03): 5
  + (01 at 04): 6
  + (01 at 05): 7

Keywords (Opt): ROVER_MOTION_COUNTER_NAME
        Format: STRING [1] (64)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ

Keywords (Opt): SEQ_ID,SEQUENCE_ID
        Format: STRING [1] (48)
        Values: KKKKKK

Keywords (Opt): SEQUENCE_NAME, SEQUENCE_TITLE
        Format: STRING [1] (64)
        Values: LLLLLL

Keywords (Opt): SEQUENCE_VERSION_ID
        Format: STRING [1] (48)
        Values: MMMMMM

Keywords (Opt): SOLAR_LONGITUDE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): SPACECRAFT_CLOCK_CNT_PARTITION
        Format: INT [1] (4)
        Values: 8

Keywords (Req): SPACECRAFT_CLOCK_START_COUNT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Req): SPACECRAFT_CLOCK_STOP_COUNT
        Format: STRING [1] (32)
        Values: OOOOOO

Keywords (Req): START_TIME
        Format: STRING [1] (32)
        Values: PPPPPP

Keywords (Req): STOP_TIME
        Format: STRING [1] (32)
        Values: QQQQQQ

Keywords (Req): TARGET_NAME
        Format: STRING [1] (64)
        Values: RRRRRR

Keywords (Opt): TARGET_TYPE
        Format: STRING [1] (32)
        Values: SSSSSS



Label Type: PROPERTY
PROPERTY Name: IMAGE_DATA

Keywords (Opt): BANDS
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): CHECKSUM
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): FIRST_LINE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): FIRST_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): INTERCHANGE_FORMAT
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): INVALID_CONSTANT
        Format: REAL [1] (4)
        Values: 110.000000
  + (01 at 02): 120.000000
  + (01 at 03): 130.000000

Keywords (Opt): LINE_PREFIX_BYTES
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): LINE_PREFIX_MEAN
        Format: REAL [1] (4)
        Values: 140.000000

Keywords (Opt): LINE_SUFFIX_BYTES
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): LINE_SUFFIX_MEAN
        Format: REAL [1] (4)
        Values: 150.000000

Keywords (Opt): LINE_SAMPLES
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): LINES
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): MAXIMUM
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): MEAN
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): MEDIAN
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): MINIMUM
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): MISSING_CONSTANT
        Format: REAL [1] (4)
        Values: 200.000000
  + (01 at 02): 210.000000
  + (01 at 03): 220.000000

Keywords (Opt): RADIANCE_OFFSET
        Format: REAL [1] (4)
        Values: 230.000000

Keywords (Opt): RADIANCE_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): RADIANCE_SCALING_FACTOR
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): RADIANCE_SCALING_FACTOR__UNIT
        Format: STRING [1] (32)
        Values: CCCCCC

Keywords (Opt): SAMPLE_BIT_MASK
        Format: STRING [1] (128)
        Values: DDDDDD

Keywords (Opt): SAMPLE_BITS
        Format: INT [1] (4)
        Values: 7

Keywords (Opt): SAMPLE_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): SPICE_FILE_NAME
        Format: STRING [1] (256)
        Values: FFFFFF
  + (01 at 02): GGGGGG
  + (01 at 03): HHHHHH
  + (01 at 04): IIIIII
  + (01 at 05): JJJJJJ
  + (01 at 06): KKKKKK
  + (01 at 07): LLLLLL
  + (01 at 08): MMMMMM

Keywords (Opt): STANDARD_DEVIATION
        Format: REAL [1] (4)
        Values: 250.000000



Label Type: PROPERTY
PROPERTY Name: IMAGE_GEOMETRY

Keywords (Opt): CENTRAL_BODY_DISTANCE
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): EMISSION_ANGLE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): INCIDENCE_ANGLE
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): INTERCEPT_POINT_LATITUDE
        Format: REAL [1] (4)
        Values: 130.000000

Keywords (Opt): INTERCEPT_POINT_LINE
        Format: REAL [1] (4)
        Values: 140.000000

Keywords (Opt): INTERCEPT_POINT_LINE_SAMPLE
        Format: REAL [1] (4)
        Values: 150.000000

Keywords (Opt): INTERCEPT_POINT_LONGITUDE
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): LOCAL_HOUR_ANGLE
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): LOCAL_MEAN_SOLAR_TIME
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): LOCAL_TIME
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): LOCAL_TRUE_SOLAR_TIME
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): NTV_SAT_TIME_FROM_CLOSEST_APRH
        Format: STRING [1] (32)
        Values: CCCCCC

Keywords (Opt): NTV_TIME_FROM_CLOSEST_APPROACH
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): PHASE_ANGLE
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): SOLAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): SOLAR_DISTANCE
        Format: REAL [1] (4)
        Values: 210.000000

Keywords (Opt): SOLAR_ELEVATION
        Format: REAL [1] (4)
        Values: 220.000000

Keywords (Opt): SOLAR_LATITUDE
        Format: REAL [1] (4)
        Values: 230.000000

Keywords (Opt): SOLAR_LINE
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): SOLAR_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): SOLAR_LONGITUDE
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): SPACECRAFT_DISTANCE
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SUB_SPACECRAFT_AZIMUTH
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SUB_SPACECRAFT_LATITUDE
        Format: REAL [1] (4)
        Values: 270.000000

Keywords (Opt): SUB_SPACECRAFT_LONGITUDE
        Format: REAL [1] (4)
        Values: 280.000000

Keywords (Opt): SURCAFE_FIXED_SOLAR_AZIMUTH
        Format: REAL [1] (4)
        Values: 290.000000

Keywords (Opt): SURFACE_FIXED_SOLAR_ELEVATION
        Format: REAL [1] (4)
        Values: 300.000000



Label Type: PROPERTY
PROPERTY Name: INSTRUMENT_STATE

Keywords (Opt): AZIMUTH_FOV
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): AZIMUTH_FOV__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): ELEVATION_FOV
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): ELEVATION_FOV__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): BAD_PIXEL_REPLACEMENT_ID
        Format: STRING [1] (48)
        Values: CCCCCC

Keywords (Opt): DETECTOR_FIRST_LINE
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): DETECTOR_LINES
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): DETECTOR_TO_IMAGE_ROTATION
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): DOWNSAMPLE_METHOD
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Opt): EXPOSURE_COUNT
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): EXPOSURE_DURATION
        Format: REAL [1] (4)
        Values: 130.000000

Keywords (Opt): EXPOSURE_DURATION__UNIT
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): EXPOSURE_DURATION_COUNT
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): EXPOSURE_TYPE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (64)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK

Keywords (Opt): FILTER_NUMBER
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): FLAT_FIELD_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: LLLLLL

Keywords (Opt): FLAT_FIELD_CORRECTION_PARM
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000
  + (01 at 03): 160.000000
  + (01 at 04): 170.000000
  + (01 at 05): 180.000000

Keywords (Opt): GAIN_MODE_ID
        Format: STRING [1] (48)
        Values: MMMMMM

Keywords (Opt): IMAGE_BIAS
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_AZ_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: NNNNNN

Keywords (Opt): INST_EL_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: OOOOOO

Keywords (Opt): INST_HOST_POSITION
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): INSTRUMENT_AZIMUTH_COUNT
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INSTRUMENT_COVER_STATE_ID
        Format: STRING [1] (48)
        Values: PPPPPP

Keywords (Opt): INSTRUMENT_DATA_RATE
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_DEPLOYMENT_STATE
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): INSTRUMENT_ELEVATION_COUNT
        Format: INT [1] (4)
        Values: 7

Keywords (Opt): INSTRUMENT_FOCAL_LENGTH_COUNT
        Format: INT [1] (4)
        Values: 8

Keywords (Opt): INSTRUMENT_MODE_ID
        Format: STRING [1] (48)
        Values: SSSSSS

Keywords (Opt): INSTRUMENT_POSITION
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000
  + (01 at 04): 280.000000
  + (01 at 05): 290.000000
  + (01 at 06): 300.000000
  + (01 at 07): 310.000000
  + (01 at 08): 320.000000
  + (01 at 09): 330.000000
  + (01 at 10): 340.000000
  + (01 at 11): 350.000000
  + (01 at 12): 360.000000
  + (01 at 13): 370.000000
  + (01 at 14): 380.000000
  + (01 at 15): 390.000000
  + (01 at 16): 400.000000
  + (01 at 17): 410.000000
  + (01 at 18): 420.000000
  + (01 at 19): 430.000000
  + (01 at 20): 440.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT
  + (01 at 02): UUUUUU
  + (01 at 03): VVVVVV
  + (01 at 04): WWWWWW
  + (01 at 05): XXXXXX
  + (01 at 06): YYYYYY
  + (01 at 07): ZZZZZZ
  + (01 at 08): aaaaaa
  + (01 at 09): bbbbbb
  + (01 at 10): cccccc
  + (01 at 11): dddddd
  + (01 at 12): eeeeee
  + (01 at 13): ffffff
  + (01 at 14): gggggg
  + (01 at 15): hhhhhh
  + (01 at 16): iiiiii
  + (01 at 17): jjjjjj
  + (01 at 18): kkkkkk
  + (01 at 19): llllll
  + (01 at 20): mmmmmm

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT__UNIT
        Format: STRING [1] (32)
        Values: nnnnnn
  + (01 at 02): oooooo
  + (01 at 03): pppppp
  + (01 at 04): qqqqqq
  + (01 at 05): rrrrrr
  + (01 at 06): ssssss
  + (01 at 07): tttttt
  + (01 at 08): uuuuuu
  + (01 at 09): vvvvvv
  + (01 at 10): wwwwww
  + (01 at 11): xxxxxx
  + (01 at 12): yyyyyy
  + (01 at 13): zzzzzz
  + (01 at 14): 000000
  + (01 at 15): 111111
  + (01 at 16): 222222
  + (01 at 17): 333333
  + (01 at 18): 444444
  + (01 at 19): 555555
  + (01 at 20): 666666

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT
        Format: INT [1] (4)
        Values: 9
  + (01 at 02): 10
  + (01 at 03): 11
  + (01 at 04): 12
  + (01 at 05): 13
  + (01 at 06): 14
  + (01 at 07): 15
  + (01 at 08): 16
  + (01 at 09): 17
  + (01 at 10): 18
  + (01 at 11): 19
  + (01 at 12): 20
  + (01 at 13): 21
  + (01 at 14): 22
  + (01 at 15): 23
  + (01 at 16): 24
  + (01 at 17): 25
  + (01 at 18): 26
  + (01 at 19): 27
  + (01 at 20): 28

Keywords (Opt): INSTRUMENT_TEMPERATURE_NAME
        Format: STRING [1] (64)
        Values: 777777
  + (01 at 02): 888888
  + (01 at 03): 999999
  + (01 at 04): AAAAAA
  + (01 at 05): BBBBBB
  + (01 at 06): CCCCCC
  + (01 at 07): DDDDDD
  + (01 at 08): EEEEEE
  + (01 at 09): FFFFFF
  + (01 at 10): GGGGGG
  + (01 at 11): HHHHHH
  + (01 at 12): IIIIII
  + (01 at 13): JJJJJJ
  + (01 at 14): KKKKKK
  + (01 at 15): LLLLLL
  + (01 at 16): MMMMMM
  + (01 at 17): NNNNNN
  + (01 at 18): OOOOOO
  + (01 at 19): PPPPPP
  + (01 at 20): QQQQQQ

Keywords (Opt): INSTRUMENT_VOLTAGE_COUNT
        Format: INT [1] (4)
        Values: 29

Keywords (Opt): LED_BITMASK
        Format: INT [1] (4)
        Values: 30

Keywords (Opt): OFFSET_MODE_ID
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): OFFSET_NUMBER
        Format: INT [1] (4)
        Values: 31

Keywords (Opt): ONBOARD_IMAGE_BIAS
        Format: INT [1] (4)
        Values: 32

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 33

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 34

Keywords (Opt): SAMPLE_BIT_METHOD
        Format: STRING [1] (64)
        Values: SSSSSS

Keywords (Opt): SAMPLE_BIT_MODE_ID
        Format: STRING [1] (48)
        Values: TTTTTT

Keywords (Opt): SHUTTER_EFFECT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: UUUUUU

Keywords (Opt): SHUTTER_MODE_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): SUN_FIND_FLAG
        Format: STRING [1] (8)
        Values: WWWWWW

Keywords (Opt): SUN_FIND
        Format: STRING [1] (8)
        Values: XXXXXX

Keywords (Opt): SUN_FIND_ACTIVE_FLAG
        Format: STRING [1] (8)
        Values: YYYYYY

Keywords (Opt): SUN_FIND_PARM
        Format: REAL [1] (4)
        Values: 450.000000
  + (01 at 02): 460.000000
  + (01 at 03): 470.000000
  + (01 at 04): 480.000000
  + (01 at 05): 490.000000

Keywords (Opt): SUN_FIND_PARM_NAME
        Format: STRING [1] (64)
        Values: ZZZZZZ
  + (01 at 02): aaaaaa
  + (01 at 03): bbbbbb
  + (01 at 04): cccccc
  + (01 at 05): dddddd

Keywords (Opt): SUN_LINE
        Format: INT [1] (4)
        Values: 35

Keywords (Opt): SUN_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 36

Keywords (Opt): SUN_VIEW_POSITION
        Format: REAL [3] (12)
        Values: 500.000000
                510.000000
                520.000000

Keywords (Opt): SUN_VIEW_DIRECTION
        Format: REAL [3] (12)
        Values: 530.000000
                540.000000
                550.000000



Label Type: PROPERTY
PROPERTY Name: INSTRUMENT_STATE_PARMS

Keywords (Opt): AZIMUTH_FOV
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): AZIMUTH_FOV__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): ELEVATION_FOV
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): ELEVATION_FOV__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): BAD_PIXEL_REPLACEMENT_ID
        Format: STRING [1] (48)
        Values: CCCCCC

Keywords (Opt): DETECTOR_FIRST_LINE
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): DETECTOR_LINES
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): DETECTOR_TO_IMAGE_ROTATION
        Format: REAL [1] (4)
        Values: 120.000000

Keywords (Opt): DOWNSAMPLE_METHOD
        Format: STRING [1] (64)
        Values: DDDDDD

Keywords (Opt): EXPOSURE_COUNT
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): EXPOSURE_DURATION
        Format: REAL [1] (4)
        Values: 130.000000

Keywords (Opt): EXPOSURE_DURATION__UNIT
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): EXPOSURE_DURATION_COUNT
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): EXPOSURE_TYPE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Opt): FILTER_NAME
        Format: STRING [1] (64)
        Values: GGGGGG
  + (01 at 02): HHHHHH
  + (01 at 03): IIIIII
  + (01 at 04): JJJJJJ
  + (01 at 05): KKKKKK

Keywords (Opt): FILTER_NUMBER
        Format: INT [1] (4)
        Values: 4

Keywords (Opt): FLAT_FIELD_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: LLLLLL

Keywords (Opt): FLAT_FIELD_CORRECTION_PARM
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000
  + (01 at 03): 160.000000
  + (01 at 04): 170.000000
  + (01 at 05): 180.000000

Keywords (Opt): GAIN_MODE_ID
        Format: STRING [1] (48)
        Values: MMMMMM

Keywords (Opt): IMAGE_BIAS
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): INST_AZ_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: NNNNNN

Keywords (Opt): INST_EL_ROTATION_DIRECTION
        Format: STRING [1] (48)
        Values: OOOOOO

Keywords (Opt): INST_HOST_POSITION
        Format: REAL [1] (4)
        Values: 190.000000
  + (01 at 02): 200.000000
  + (01 at 03): 210.000000

Keywords (Opt): INSTRUMENT_AZIMUTH_COUNT
        Format: INT [1] (4)
        Values: 6

Keywords (Opt): INSTRUMENT_COVER_STATE_ID
        Format: STRING [1] (48)
        Values: PPPPPP

Keywords (Opt): INSTRUMENT_DATA_RATE
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_DEPLOYMENT_STATE
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): INSTRUMENT_ELEVATION_COUNT
        Format: INT [1] (4)
        Values: 7

Keywords (Opt): INSTRUMENT_FOCAL_LENGTH_COUNT
        Format: INT [1] (4)
        Values: 8

Keywords (Opt): INSTRUMENT_MODE_ID
        Format: STRING [1] (48)
        Values: SSSSSS

Keywords (Opt): INSTRUMENT_POSITION
        Format: REAL [1] (4)
        Values: 220.000000
  + (01 at 02): 230.000000
  + (01 at 03): 240.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE
        Format: REAL [1] (4)
        Values: 250.000000
  + (01 at 02): 260.000000
  + (01 at 03): 270.000000
  + (01 at 04): 280.000000
  + (01 at 05): 290.000000
  + (01 at 06): 300.000000
  + (01 at 07): 310.000000
  + (01 at 08): 320.000000
  + (01 at 09): 330.000000
  + (01 at 10): 340.000000
  + (01 at 11): 350.000000
  + (01 at 12): 360.000000
  + (01 at 13): 370.000000
  + (01 at 14): 380.000000
  + (01 at 15): 390.000000
  + (01 at 16): 400.000000
  + (01 at 17): 410.000000
  + (01 at 18): 420.000000
  + (01 at 19): 430.000000
  + (01 at 20): 440.000000

Keywords (Opt): INSTRUMENT_TEMPERATURE__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT
  + (01 at 02): UUUUUU
  + (01 at 03): VVVVVV
  + (01 at 04): WWWWWW
  + (01 at 05): XXXXXX
  + (01 at 06): YYYYYY
  + (01 at 07): ZZZZZZ
  + (01 at 08): aaaaaa
  + (01 at 09): bbbbbb
  + (01 at 10): cccccc
  + (01 at 11): dddddd
  + (01 at 12): eeeeee
  + (01 at 13): ffffff
  + (01 at 14): gggggg
  + (01 at 15): hhhhhh
  + (01 at 16): iiiiii
  + (01 at 17): jjjjjj
  + (01 at 18): kkkkkk
  + (01 at 19): llllll
  + (01 at 20): mmmmmm

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT__UNIT
        Format: STRING [1] (32)
        Values: nnnnnn
  + (01 at 02): oooooo
  + (01 at 03): pppppp
  + (01 at 04): qqqqqq
  + (01 at 05): rrrrrr
  + (01 at 06): ssssss
  + (01 at 07): tttttt
  + (01 at 08): uuuuuu
  + (01 at 09): vvvvvv
  + (01 at 10): wwwwww
  + (01 at 11): xxxxxx
  + (01 at 12): yyyyyy
  + (01 at 13): zzzzzz
  + (01 at 14): 000000
  + (01 at 15): 111111
  + (01 at 16): 222222
  + (01 at 17): 333333
  + (01 at 18): 444444
  + (01 at 19): 555555
  + (01 at 20): 666666

Keywords (Opt): INSTRUMENT_TEMPERATURE_COUNT
        Format: INT [1] (4)
        Values: 9
  + (01 at 02): 10
  + (01 at 03): 11
  + (01 at 04): 12
  + (01 at 05): 13
  + (01 at 06): 14
  + (01 at 07): 15
  + (01 at 08): 16
  + (01 at 09): 17
  + (01 at 10): 18
  + (01 at 11): 19
  + (01 at 12): 20
  + (01 at 13): 21
  + (01 at 14): 22
  + (01 at 15): 23
  + (01 at 16): 24
  + (01 at 17): 25
  + (01 at 18): 26
  + (01 at 19): 27
  + (01 at 20): 28

Keywords (Opt): INSTRUMENT_TEMPERATURE_NAME
        Format: STRING [1] (64)
        Values: 777777
  + (01 at 02): 888888
  + (01 at 03): 999999
  + (01 at 04): AAAAAA
  + (01 at 05): BBBBBB
  + (01 at 06): CCCCCC
  + (01 at 07): DDDDDD
  + (01 at 08): EEEEEE
  + (01 at 09): FFFFFF
  + (01 at 10): GGGGGG
  + (01 at 11): HHHHHH
  + (01 at 12): IIIIII
  + (01 at 13): JJJJJJ
  + (01 at 14): KKKKKK
  + (01 at 15): LLLLLL
  + (01 at 16): MMMMMM
  + (01 at 17): NNNNNN
  + (01 at 18): OOOOOO
  + (01 at 19): PPPPPP
  + (01 at 20): QQQQQQ

Keywords (Opt): INSTRUMENT_VOLTAGE_COUNT
        Format: INT [1] (4)
        Values: 29

Keywords (Opt): LED_BITMASK
        Format: INT [1] (4)
        Values: 30

Keywords (Opt): OFFSET_MODE_ID
        Format: STRING [1] (48)
        Values: RRRRRR

Keywords (Opt): OFFSET_NUMBER
        Format: INT [1] (4)
        Values: 31

Keywords (Opt): ONBOARD_IMAGE_BIAS
        Format: INT [1] (4)
        Values: 32

Keywords (Opt): PIXEL_AVERAGING_HEIGHT
        Format: INT [1] (4)
        Values: 33

Keywords (Opt): PIXEL_AVERAGING_WIDTH
        Format: INT [1] (4)
        Values: 34

Keywords (Opt): SAMPLE_BIT_METHOD
        Format: STRING [1] (64)
        Values: SSSSSS

Keywords (Opt): SAMPLE_BIT_MODE_ID
        Format: STRING [1] (48)
        Values: TTTTTT

Keywords (Opt): SHUTTER_EFFECT_CORRECTION_FLAG
        Format: STRING [1] (8)
        Values: UUUUUU

Keywords (Opt): SHUTTER_MODE_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): SUN_FIND_FLAG
        Format: STRING [1] (8)
        Values: WWWWWW

Keywords (Opt): SUN_FIND
        Format: STRING [1] (8)
        Values: XXXXXX

Keywords (Opt): SUN_FIND_ACTIVE_FLAG
        Format: STRING [1] (8)
        Values: YYYYYY

Keywords (Opt): SUN_FIND_PARM
        Format: REAL [1] (4)
        Values: 450.000000
  + (01 at 02): 460.000000
  + (01 at 03): 470.000000
  + (01 at 04): 480.000000
  + (01 at 05): 490.000000

Keywords (Opt): SUN_FIND_PARM_NAME
        Format: STRING [1] (64)
        Values: ZZZZZZ
  + (01 at 02): aaaaaa
  + (01 at 03): bbbbbb
  + (01 at 04): cccccc
  + (01 at 05): dddddd

Keywords (Opt): SUN_LINE
        Format: INT [1] (4)
        Values: 35

Keywords (Opt): SUN_LINE_SAMPLE
        Format: INT [1] (4)
        Values: 36

Keywords (Opt): SUN_VIEW_POSITION
        Format: REAL [3] (12)
        Values: 500.000000
                510.000000
                520.000000

Keywords (Opt): SUN_VIEW_DIRECTION
        Format: REAL [3] (12)
        Values: 530.000000
                540.000000
                550.000000



Label Type: PROPERTY
PROPERTY Name: SURFACE_MODEL

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: AAAAAA

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): SURFACE_MODEL_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Req): SURFACE_MODEL_TYPE
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): SURFACE_NORMAL_VECTOR
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): SURFACE_GROUND_LOCATION
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000



Label Type: PROPERTY
PROPERTY Name: SURFACE_MODEL_PARMS

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: AAAAAA

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): SURFACE_MODEL_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Req): SURFACE_MODEL_TYPE
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): SURFACE_NORMAL_VECTOR
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000

Keywords (Opt): SURFACE_GROUND_LOCATION
        Format: REAL [1] (4)
        Values: 130.000000
  + (01 at 02): 140.000000
  + (01 at 03): 150.000000



Label Type: PROPERTY
PROPERTY Name: SURFACE_PROJECTION

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): LINE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): LINE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): MAP_PROJECTION_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MAP_PROJECTION_NOTE
        Format: STRING [1] (256)
        Values: DDDDDD

Keywords (Req): MAP_PROJECTION_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): MAP_RESOLUTION
        Format: REAL [2] (12)
        Values: 120.000000
                130.000000

Keywords (Opt): MAP_RESOLUTION__UNIT
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG

Keywords (Opt): MAP_SCALE
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000

Keywords (Opt): MAP_SCALE__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): MAXIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): MAXIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: IIIIII

Keywords (Opt): MINIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): MINIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: JJJJJJ

Keywords (Opt): PROJECTION_AZIMUTH
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): PROJECTION_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): PROJECTION_ELEVATION
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): PROJECTION_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: LLLLLL

Keywords (Opt): PROJECTION_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): PROJECTION_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): PROJECTION_ORIGIN_VECTOR
        Format: REAL [1] (4)
        Values: 210.000000
  + (01 at 02): 220.000000
  + (01 at 03): 230.000000

Keywords (Opt): PROJECTION_ORIGIN_VECTOR__UNIT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): REFERENCE_AZIMUTH
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): REFERENCE_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: OOOOOO

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: PPPPPP

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: RRRRRR

Keywords (Opt): SAMPLE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SAMPLE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: SSSSSS

Keywords (Opt): START_AZIMUTH
        Format: REAL [1] (4)
        Values: 270.000000

Keywords (Opt): START_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT

Keywords (Opt): STOP_AZIMUTH
        Format: REAL [1] (4)
        Values: 280.000000

Keywords (Opt): STOP_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: UUUUUU

Keywords (Opt): SURFACE_GEOMETRY_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): X_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 290.000000

Keywords (Opt): X_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: WWWWWW

Keywords (Opt): X_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 300.000000

Keywords (Opt): X_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: XXXXXX

Keywords (Opt): Y_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 310.000000

Keywords (Opt): Y_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: YYYYYY

Keywords (Opt): Y_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 320.000000

Keywords (Opt): Y_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: ZZZZZZ

Keywords (Opt): ZERO_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 330.000000

Keywords (Opt): ZERO_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: aaaaaa



Label Type: PROPERTY
PROPERTY Name: SURFACE_PROJECTION_PARMS

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): LINE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: AAAAAA

Keywords (Opt): LINE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): LINE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB

Keywords (Opt): MAP_PROJECTION_DESC
        Format: STRING [1] (256)
        Values: CCCCCC

Keywords (Opt): MAP_PROJECTION_NOTE
        Format: STRING [1] (256)
        Values: DDDDDD

Keywords (Req): MAP_PROJECTION_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): MAP_RESOLUTION
        Format: REAL [2] (12)
        Values: 120.000000
                130.000000

Keywords (Opt): MAP_RESOLUTION__UNIT
        Format: STRING [1] (32)
        Values: FFFFFF
  + (01 at 02): GGGGGG

Keywords (Opt): MAP_SCALE
        Format: REAL [1] (4)
        Values: 140.000000
  + (01 at 02): 150.000000

Keywords (Opt): MAP_SCALE__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): MAXIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 160.000000

Keywords (Opt): MAXIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: IIIIII

Keywords (Opt): MINIMUM_ELEVATION
        Format: REAL [1] (4)
        Values: 170.000000

Keywords (Opt): MINIMUM_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: JJJJJJ

Keywords (Opt): PROJECTION_AZIMUTH
        Format: REAL [1] (4)
        Values: 180.000000

Keywords (Opt): PROJECTION_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): PROJECTION_ELEVATION
        Format: REAL [1] (4)
        Values: 190.000000

Keywords (Opt): PROJECTION_ELEVATION__UNIT
        Format: STRING [1] (32)
        Values: LLLLLL

Keywords (Opt): PROJECTION_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 200.000000

Keywords (Opt): PROJECTION_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: MMMMMM

Keywords (Opt): PROJECTION_ORIGIN_VECTOR
        Format: REAL [1] (4)
        Values: 210.000000
  + (01 at 02): 220.000000
  + (01 at 03): 230.000000

Keywords (Opt): PROJECTION_ORIGIN_VECTOR__UNIT
        Format: STRING [1] (32)
        Values: NNNNNN

Keywords (Opt): REFERENCE_AZIMUTH
        Format: REAL [1] (4)
        Values: 240.000000

Keywords (Opt): REFERENCE_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: OOOOOO

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: PPPPPP

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: QQQQQQ

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET
        Format: REAL [1] (4)
        Values: 250.000000

Keywords (Opt): SAMPLE_CAMERA_MODEL_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: RRRRRR

Keywords (Opt): SAMPLE_PROJECTION_OFFSET
        Format: REAL [1] (4)
        Values: 260.000000

Keywords (Opt): SAMPLE_PROJECTION_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: SSSSSS

Keywords (Opt): START_AZIMUTH
        Format: REAL [1] (4)
        Values: 270.000000

Keywords (Opt): START_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: TTTTTT

Keywords (Opt): STOP_AZIMUTH
        Format: REAL [1] (4)
        Values: 280.000000

Keywords (Opt): STOP_AZIMUTH__UNIT
        Format: STRING [1] (32)
        Values: UUUUUU

Keywords (Opt): SURFACE_GEOMETRY_ID
        Format: STRING [1] (48)
        Values: VVVVVV

Keywords (Opt): X_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 290.000000

Keywords (Opt): X_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: WWWWWW

Keywords (Opt): X_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 300.000000

Keywords (Opt): X_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: XXXXXX

Keywords (Opt): Y_AXIS_MAXIMUM
        Format: REAL [1] (4)
        Values: 310.000000

Keywords (Opt): Y_AXIS_MAXIMUM__UNIT
        Format: STRING [1] (32)
        Values: YYYYYY

Keywords (Opt): Y_AXIS_MINIMUM
        Format: REAL [1] (4)
        Values: 320.000000

Keywords (Opt): Y_AXIS_MINIMUM__UNIT
        Format: STRING [1] (32)
        Values: ZZZZZZ

Keywords (Opt): ZERO_ELEVATION_LINE
        Format: REAL [1] (4)
        Values: 330.000000

Keywords (Opt): ZERO_ELEVATION_LINE__UNIT
        Format: STRING [1] (32)
        Values: aaaaaa



Label Type: PROPERTY
PROPERTY Name: TELEMETRY

Keywords (Opt): APPLICATION_PACKET_ID
        Format: INT [1] (4)
        Values: 0

Keywords (Opt): APPLICATION_PACKET_NAME
        Format: STRING [1] (128)
        Values: AAAAAA

Keywords (Opt): APPLICATION_PROCESS_ID
        Format: INT [1] (4)
        Values: 1

Keywords (Opt): APPLICATION_PROCESS_NAME
        Format: STRING [1] (128)
        Values: BBBBBB

Keywords (Opt): APPLICATION_PROCESS_SUBTYPE_ID
        Format: INT [1] (4)
        Values: 2

Keywords (Opt): EARTH_RECEIVED_START_TIME
        Format: STRING [1] (32)
        Values: CCCCCC

Keywords (Opt): EARTH_RECEIVED_STOP_TIME
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): EXPECTED_PACKETS
        Format: INT [1] (4)
        Values: 3

Keywords (Opt): PACKET_CREATION_SCLK
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): PACKET_MAP_MASK
        Format: STRING [1] (256)
        Values: FFFFFF

Keywords (Opt): PACKET_SEQUENCE_NUMBER
        Format: INT [1] (4)
        Values: 4

Keywords (Req): RECEIVED_PACKETS
        Format: INT [1] (4)
        Values: 5

Keywords (Opt): SOFTWARE_NAME
        Format: STRING [1] (64)
        Values: GGGGGG

Keywords (Opt): SOFTWARE_VERSION_ID
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): SPICE_FILE_ID
        Format: STRING [1] (48)
        Values: IIIIII
  + (01 at 02): JJJJJJ
  + (01 at 03): KKKKKK
  + (01 at 04): LLLLLL
  + (01 at 05): MMMMMM

Keywords (Req): SPICE_FILE_NAME
        Format: STRING [1] (256)
        Values: NNNNNN
  + (01 at 02): OOOOOO
  + (01 at 03): PPPPPP
  + (01 at 04): QQQQQQ
  + (01 at 05): RRRRRR

Keywords (Opt): TELEMETRY_PROVIDER_ID
        Format: STRING [1] (48)
        Values: SSSSSS

Keywords (Opt): TELEMETRY_PROVIDER_TYPE
        Format: STRING [1] (32)
        Values: TTTTTT

Keywords (Opt): TELEMETRY_SOURCE_NAME
        Format: STRING [1] (64)
        Values: UUUUUU

Keywords (Opt): TELEMETRY_SOURCE_TYPE
        Format: STRING [1] (32)
        Values: VVVVVV

Keywords (Opt): TLM_CMD_DISCREPANCY_FLAG
        Format: STRING [1] (8)
        Values: WWWWWW



Label Type: PROPERTY
PROPERTY Name: ARTICULATION_STATE

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): ARTICULATION_DEVICE_ID
        Format: STRING [1] (48)
        Values: BBBBBB

Keywords (Opt): ARTICULATION_DEVICE_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): ARTICULATION_DEVICE_ANGLE
        Format: REAL [1] (4)
        Values: 100.000000
  + (01 at 02): 110.000000
  + (01 at 03): 120.000000
  + (01 at 04): 130.000000
  + (01 at 05): 140.000000
  + (01 at 06): 150.000000
  + (01 at 07): 160.000000
  + (01 at 08): 170.000000
  + (01 at 09): 180.000000
  + (01 at 10): 190.000000

Keywords (Opt): ARTICULATION_DEVICE_ANGLE__UNIT
        Format: STRING [1] (32)
        Values: DDDDDD
  + (01 at 02): EEEEEE
  + (01 at 03): FFFFFF
  + (01 at 04): GGGGGG
  + (01 at 05): HHHHHH
  + (01 at 06): IIIIII
  + (01 at 07): JJJJJJ
  + (01 at 08): KKKKKK
  + (01 at 09): LLLLLL
  + (01 at 10): MMMMMM

Keywords (Opt): ARTICULATION_DEVICE_ANGLE_NAME
        Format: STRING [1] (64)
        Values: NNNNNN
  + (01 at 02): OOOOOO
  + (01 at 03): PPPPPP
  + (01 at 04): QQQQQQ
  + (01 at 05): RRRRRR
  + (01 at 06): SSSSSS
  + (01 at 07): TTTTTT
  + (01 at 08): UUUUUU
  + (01 at 09): VVVVVV
  + (01 at 10): WWWWWW

Keywords (Opt): ARTICULATION_DEVICE_COUNT
        Format: REAL [1] (4)
        Values: 200.000000
  + (01 at 02): 210.000000
  + (01 at 03): 220.000000
  + (01 at 04): 230.000000
  + (01 at 05): 240.000000
  + (01 at 06): 250.000000
  + (01 at 07): 260.000000
  + (01 at 08): 270.000000
  + (01 at 09): 280.000000
  + (01 at 10): 290.000000

Keywords (Opt): ARTICULATION_DEVICE_COUNT__UNIT
        Format: STRING [1] (32)
        Values: XXXXXX
  + (01 at 02): YYYYYY
  + (01 at 03): ZZZZZZ
  + (01 at 04): aaaaaa
  + (01 at 05): bbbbbb
  + (01 at 06): cccccc
  + (01 at 07): dddddd
  + (01 at 08): eeeeee
  + (01 at 09): ffffff
  + (01 at 10): gggggg

Keywords (Opt): ARTICULATION_DEVICE_COUNT_NAME
        Format: STRING [1] (64)
        Values: hhhhhh
  + (01 at 02): iiiiii
  + (01 at 03): jjjjjj
  + (01 at 04): kkkkkk
  + (01 at 05): llllll
  + (01 at 06): mmmmmm
  + (01 at 07): nnnnnn
  + (01 at 08): oooooo
  + (01 at 09): pppppp
  + (01 at 10): qqqqqq

Keywords (Opt): ARTICULATION_DEV_LOCATION
        Format: REAL [1] (4)
        Values: 300.000000
  + (01 at 02): 310.000000
  + (01 at 03): 320.000000
  + (01 at 04): 330.000000
  + (01 at 05): 340.000000
  + (01 at 06): 350.000000
  + (01 at 07): 360.000000
  + (01 at 08): 370.000000
  + (01 at 09): 380.000000
  + (01 at 10): 390.000000

Keywords (Opt): ARTICULATION_DEV_LOCATION__UNIT
        Format: STRING [1] (32)
        Values: rrrrrr
  + (01 at 02): ssssss
  + (01 at 03): tttttt
  + (01 at 04): uuuuuu
  + (01 at 05): vvvvvv
  + (01 at 06): wwwwww
  + (01 at 07): xxxxxx
  + (01 at 08): yyyyyy
  + (01 at 09): zzzzzz
  + (01 at 10): 000000

Keywords (Opt): ARTICULATION_DEV_LOCATION_NAME
        Format: STRING [1] (64)
        Values: 111111
  + (01 at 02): 222222
  + (01 at 03): 333333
  + (01 at 04): 444444
  + (01 at 05): 555555
  + (01 at 06): 666666
  + (01 at 07): 777777
  + (01 at 08): 888888
  + (01 at 09): 999999
  + (01 at 10): AAAAAA

Keywords (Opt): ARTICULATION_DEV_ORIENT
        Format: REAL [1] (4)
        Values: 400.000000
  + (01 at 02): 410.000000
  + (01 at 03): 420.000000
  + (01 at 04): 430.000000
  + (01 at 05): 440.000000
  + (01 at 06): 450.000000
  + (01 at 07): 460.000000
  + (01 at 08): 470.000000
  + (01 at 09): 480.000000
  + (01 at 10): 490.000000

Keywords (Opt): ARTICULATION_DEV_ORIENT__UNIT
        Format: STRING [1] (32)
        Values: BBBBBB
  + (01 at 02): CCCCCC
  + (01 at 03): DDDDDD
  + (01 at 04): EEEEEE
  + (01 at 05): FFFFFF
  + (01 at 06): GGGGGG
  + (01 at 07): HHHHHH
  + (01 at 08): IIIIII
  + (01 at 09): JJJJJJ
  + (01 at 10): KKKKKK

Keywords (Opt): ARTICULATION_DEV_ORIENT_NAME
        Format: STRING [1] (64)
        Values: LLLLLL
  + (01 at 02): MMMMMM
  + (01 at 03): NNNNNN
  + (01 at 04): OOOOOO
  + (01 at 05): PPPPPP
  + (01 at 06): QQQQQQ
  + (01 at 07): RRRRRR
  + (01 at 08): SSSSSS
  + (01 at 09): TTTTTT
  + (01 at 10): UUUUUU

Keywords (Opt): ARTICULATION_DEV_POSITION
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2

Keywords (Opt): ARTICULATION_DEV_POSITION_ID
        Format: STRING [1] (48)
        Values: VVVVVV
  + (01 at 02): WWWWWW
  + (01 at 03): XXXXXX

Keywords (Opt): ARTICULATION_DEV_POSITION_NAME
        Format: STRING [1] (64)
        Values: YYYYYY
  + (01 at 02): ZZZZZZ
  + (01 at 03): aaaaaa

Keywords (Opt): ARTICULATION_DEVICE_MODE
        Format: STRING [1] (48)
        Values: bbbbbb

Keywords (Opt): ARTICULATION_DEVICE_TEMP
        Format: REAL [1] (4)
        Values: 500.000000
  + (01 at 02): 510.000000

Keywords (Opt): ARTICULATION_DEVICE_TEMP__UNIT
        Format: STRING [1] (32)
        Values: cccccc
  + (01 at 02): dddddd

Keywords (Opt): ARTICULATION_DEVICE_TEMP_NAME
        Format: STRING [1] (64)
        Values: eeeeee
  + (01 at 02): ffffff

Keywords (Opt): ARTICULATION_DEV_VECTOR
        Format: REAL [3] (12)
        Values: 520.000000
                530.000000
                540.000000

Keywords (Opt): ARTICULATION_DEV_INSTRUMENT_ID
        Format: STRING [1] (48)
        Values: yyyyyy

Keywords (Opt): ARTICULATION_DEV_VECTOR_NAME
        Format: STRING [1] (64)
        Values: hhhhhh

Keywords (Opt): CONTACT_SENSOR_STATE
        Format: STRING [1] (48)
        Values: iiiiii
  + (01 at 02): jjjjjj
  + (01 at 03): kkkkkk
  + (01 at 04): llllll
  + (01 at 05): mmmmmm
  + (01 at 06): nnnnnn
  + (01 at 07): oooooo
  + (01 at 08): pppppp

Keywords (Opt): CONTACT_SENSOR_STATE_NAME
        Format: STRING [1] (64)
        Values: qqqqqq
  + (01 at 02): rrrrrr
  + (01 at 03): ssssss
  + (01 at 04): tttttt
  + (01 at 05): uuuuuu
  + (01 at 06): vvvvvv
  + (01 at 07): wwwwww
  + (01 at 08): xxxxxx

Keywords (Opt): ARTICULATION_DEV_INSTRUMENT_ID
        Format: STRING [1] (48)
        Values: yyyyyy

Keywords (Opt): ARTICULATION_DEVICE_STATE
        Format: STRING [1] (48)
        Values: zzzzzz
  + (01 at 02): 000000
  + (01 at 03): 111111
  + (01 at 04): 222222
  + (01 at 05): 333333
  + (01 at 06): 444444
  + (01 at 07): 555555
  + (01 at 08): 666666
  + (01 at 09): 777777
  + (01 at 10): 888888
  + (01 at 11): 999999
  + (01 at 12): AAAAAA

Keywords (Opt): ARTICULATION_DEVICE_STATE_NAME
        Format: STRING [1] (64)
        Values: BBBBBB
  + (01 at 02): CCCCCC
  + (01 at 03): DDDDDD
  + (01 at 04): EEEEEE
  + (01 at 05): FFFFFF
  + (01 at 06): GGGGGG
  + (01 at 07): HHHHHH
  + (01 at 08): IIIIII
  + (01 at 09): JJJJJJ
  + (01 at 10): KKKKKK
  + (01 at 11): LLLLLL
  + (01 at 12): MMMMMM



Label Type: PROPERTY
PROPERTY Name: COORDINATE_SYSTEM

Keywords (Opt): SOLUTION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): COORDINATE_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): COORDINATE_SYSTEM_INDEX_NAME
        Format: STRING [1] (64)
        Values: BBBBBB
  + (01 at 02): CCCCCC
  + (01 at 03): DDDDDD
  + (01 at 04): EEEEEE
  + (01 at 05): FFFFFF
  + (01 at 06): GGGGGG

Keywords (Opt): COORDINATE_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: HHHHHH

Keywords (Opt): ORIGIN_OFFSET_VECTOR
        Format: REAL [3] (12)
        Values: 100.000000
                110.000000
                120.000000

Keywords (Opt): ORIGIN_ROTATION_QUATERNION
        Format: REAL [4] (16)
        Values: 130.000000
                140.000000
                150.000000
                160.000000

Keywords (Opt): POSITIVE_AZIMUTH_DIRECTION
        Format: STRING [1] (64)
        Values: IIIIII

Keywords (Opt): POSITIVE_ELEVATION_DIRECTION
        Format: STRING [1] (64)
        Values: JJJJJJ

Keywords (Opt): QUATERNION_MEASUREMENT_METHOD
        Format: STRING [1] (32)
        Values: KKKKKK

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 6
  + (01 at 02): 7
  + (01 at 03): 8
  + (01 at 04): 9
  + (01 at 05): 10
  + (01 at 06): 11

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: LLLLLL

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: MMMMMM



Label Type: PROPERTY
PROPERTY Name: GROUND_SUPPORT_EQUIPMENT

Keywords (Opt): CAMERA_LOCATION_ID
        Format: STRING [1] (48)
        Values: AAAAAA

Keywords (Opt): FACILITY_NAME
        Format: STRING [1] (64)
        Values: BBBBBB

Keywords (Opt): LIGHT_SOURCE_NAME
        Format: STRING [1] (64)
        Values: CCCCCC

Keywords (Opt): LIGHT_SOURCE_DISTANCE
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): LIGHT_SOURCE_DISTANCE__UNIT
        Format: STRING [1] (32)
        Values: DDDDDD

Keywords (Opt): LIGHT_SOURCE_TYPE
        Format: STRING [1] (32)
        Values: EEEEEE

Keywords (Opt): PRESSURE
        Format: STRING [1] (32)
        Values: FFFFFF

Keywords (Req): PRODUCER_FULL_NAME
        Format: STRING [1] (128)
        Values: GGGGGG

Keywords (Opt): TARGET_DISTANCE
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): TARGET_DISTANCE__UNIT
        Format: STRING [1] (32)
        Values: HHHHHH

Keywords (Opt): TARGET_NAME
        Format: STRING [1] (64)
        Values: IIIIII

Keywords (Opt): TEST_PHASE_NAME
        Format: STRING [1] (64)
        Values: JJJJJJ

Keywords (Opt): NOTE
        Format: STRING [1] (256)
        Values: KKKKKK



Label Type: PROPERTY
PROPERTY Name: DERIVED_IMAGE

Keywords (Opt): CONFIGURATION_BAND_ID
        Format: STRING [1] (48)
        Values: AAAAAA
  + (01 at 02): BBBBBB
  + (01 at 03): CCCCCC
  + (01 at 04): DDDDDD
  + (01 at 05): EEEEEE
  + (01 at 06): FFFFFF
  + (01 at 07): GGGGGG
  + (01 at 08): HHHHHH
  + (01 at 09): IIIIII
  + (01 at 10): JJJJJJ
  + (01 at 11): KKKKKK
  + (01 at 12): LLLLLL
  + (01 at 13): MMMMMM
  + (01 at 14): NNNNNN
  + (01 at 15): OOOOOO
  + (01 at 16): PPPPPP

Keywords (Opt): DERIVED_IMAGE_TYPE
        Format: STRING [1] (32)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_BAND_ID
        Format: STRING [1] (48)
        Values: RRRRRR
  + (01 at 02): SSSSSS
  + (01 at 03): TTTTTT
  + (01 at 04): UUUUUU
  + (01 at 05): VVVVVV
  + (01 at 06): WWWWWW
  + (01 at 07): XXXXXX
  + (01 at 08): YYYYYY
  + (01 at 09): ZZZZZZ
  + (01 at 10): aaaaaa
  + (01 at 11): bbbbbb
  + (01 at 12): cccccc
  + (01 at 13): dddddd
  + (01 at 14): eeeeee
  + (01 at 15): ffffff
  + (01 at 16): gggggg

Keywords (Opt): RADIANCE_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): RADIANCE_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: hhhhhh

Keywords (Opt): RADIANCE_SCALING_FACTOR
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): RADIANCE_SCALING_FACTOR__UNIT
        Format: STRING [1] (32)
        Values: iiiiii

Keywords (Opt): RADIOMETRIC_CORRECTION_TYPE
        Format: STRING [1] (32)
        Values: jjjjjj

Keywords (Opt): RANGE_ORIGIN_VECTOR
        Format: REAL [3] (12)
        Values: 120.000000
                130.000000
                140.000000

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: kkkkkk

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: llllll



Label Type: PROPERTY
PROPERTY Name: DERIVED_IMAGE_PARMS

Keywords (Opt): CONFIGURATION_BAND_ID
        Format: STRING [1] (48)
        Values: AAAAAA
  + (01 at 02): BBBBBB
  + (01 at 03): CCCCCC
  + (01 at 04): DDDDDD
  + (01 at 05): EEEEEE
  + (01 at 06): FFFFFF
  + (01 at 07): GGGGGG
  + (01 at 08): HHHHHH
  + (01 at 09): IIIIII
  + (01 at 10): JJJJJJ
  + (01 at 11): KKKKKK
  + (01 at 12): LLLLLL
  + (01 at 13): MMMMMM
  + (01 at 14): NNNNNN
  + (01 at 15): OOOOOO
  + (01 at 16): PPPPPP

Keywords (Opt): DERIVED_IMAGE_TYPE
        Format: STRING [1] (32)
        Values: QQQQQQ

Keywords (Opt): INSTRUMENT_BAND_ID
        Format: STRING [1] (48)
        Values: RRRRRR
  + (01 at 02): SSSSSS
  + (01 at 03): TTTTTT
  + (01 at 04): UUUUUU
  + (01 at 05): VVVVVV
  + (01 at 06): WWWWWW
  + (01 at 07): XXXXXX
  + (01 at 08): YYYYYY
  + (01 at 09): ZZZZZZ
  + (01 at 10): aaaaaa
  + (01 at 11): bbbbbb
  + (01 at 12): cccccc
  + (01 at 13): dddddd
  + (01 at 14): eeeeee
  + (01 at 15): ffffff
  + (01 at 16): gggggg

Keywords (Opt): RADIANCE_OFFSET
        Format: REAL [1] (4)
        Values: 100.000000

Keywords (Opt): RADIANCE_OFFSET__UNIT
        Format: STRING [1] (32)
        Values: hhhhhh

Keywords (Opt): RADIANCE_SCALING_FACTOR
        Format: REAL [1] (4)
        Values: 110.000000

Keywords (Opt): RADIANCE_SCALING_FACTOR__UNIT
        Format: STRING [1] (32)
        Values: iiiiii

Keywords (Opt): RADIOMETRIC_CORRECTION_TYPE
        Format: STRING [1] (32)
        Values: jjjjjj

Keywords (Opt): RANGE_ORIGIN_VECTOR
        Format: REAL [3] (12)
        Values: 120.000000
                130.000000
                140.000000

Keywords (Opt): REFERENCE_COORD_SYSTEM_NAME
        Format: STRING [1] (64)
        Values: kkkkkk

Keywords (Opt): REFERENCE_COORD_SYSTEM_INDEX
        Format: INT [1] (4)
        Values: 0
  + (01 at 02): 1
  + (01 at 03): 2
  + (01 at 04): 3
  + (01 at 05): 4
  + (01 at 06): 5

Keywords (Opt): REFERENCE_COORD_SYSTEM_SOLN_ID
        Format: STRING [1] (48)
        Values: llllll

$ Return
$!#############################################################################
