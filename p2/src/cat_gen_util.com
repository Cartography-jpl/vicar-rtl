$!****************************************************************************
$!
$! Build proc for MIPL module cat_gen_util
$! VPACK Version 1.8, Friday, June 13, 1997, 17:47:18
$!
$! Execute by entering:		$ @cat_gen_util
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
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module cat_gen_util ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to cat_gen_util.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("cat_gen_util.imake") .nes. ""
$   then
$      vimake cat_gen_util
$      purge cat_gen_util.bld
$   else
$      if F$SEARCH("cat_gen_util.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake cat_gen_util
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @cat_gen_util.bld "STD"
$   else
$      @cat_gen_util.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create cat_gen_util.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack cat_gen_util.com -
	-s cat_gen_util.c -
	-i cat_gen_util.imake -
	-t tcat_gen_util.imake tzcat_gen_util.c tcat_gen_util.f -
	   tcat_gen_util.pdf tstcat_gen_util.pdf tstcat_gen_util.load -
	   tstcat_gen_util.unload -
	-o cat_gen_util.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cat_gen_util.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************
******************************************************************************

  module cat_gen_util.c

  Megan O'Shaughnessy, 6/13/1994

  This module contains a set of subroutines which are of generic use in 
  interfacing between VICAR programs and SYBASE:

  C-Callable           FORTRAN-Callable      description
  ------------------   ----------------      -----------
** generic routines *********************************************************

  zsplitFilespec       xsplitfilespec        split filespec into parts
  zcombineFilespec     xcombinefilespec      assemble parts into filespec
  cat_strcpy_toupper                         copies the alphabetic part of a 
                                             string, converting to uppercase.

** auxilliary catalog routines ***********************************************

  zcatGetMsg           xcatgetmsg            translate catalog error# into msg
  zcatGetSybaseMsg     xcatgetsybasemsg      translate sybase error# into msg
  zcatGetUserData      xcatgetuserdata       grabs username, password, server
                                             and database    

** Catalog interface routines ***********************************************
  catLogin				     initializes the message handler,
					     query descriptor and 
					     performs the log on.
  catLogout				     frees descriptors and
					     performs the log off.
  catFillQiDescCmd			     sets the query descriptor command
					     pointer.
  catExecuteCmd                              execute the stored proc.
  
** internal subroutines ******************************************************

  catcomma
  catquote
  cat_test_null_int                          converts nullint to Sybase NULL
  cat_test_null_short                        converts nullshort to Sybase NULL
  cat_test_null_float                        converts nullfloat to Sybase NULL
  cat_test_null_char                         converts nullchar to Sybase NULL
  catSetNulls                                sets translations for defualt val.
  catGetRets                                 get output parms, optional print
  checkSlashes                               adds slashes to path if needed
  checkDots                                  removes '.'s from file or ext

******************************************************************************
******************************************************************************/
#include "xvmaininc.h"
#include <stdio.h>
#include "ftnbridge.h"
#include "vmachdep.h"

#define FILL_CMD_FN_PTR          /* Turn on defines so that the right  */ 
#define FN_PTR                   /* things will get declared in the    */
#define PRINTOUT_FN_PTR          /* include file below.                */
#define RETVALS_FN_PTR
#define CAT_DUMMY_STRUCT_DEF
#define CAT_DUMMY_FN
 
#include "cat_gen_util.h"

static MDMS_MSGDESC *msgDesc;
static MDMS_QI_DESC_OBJ *qiDesc;

static int debug = 0;  /* Set to 1 to turn on debug messages, 0 turns it off */

/*****************************************************************************/
/* utility routines for constructing commands                                */
/*****************************************************************************/
ROUTINE int catcomma(char *cmd, char *s)
{
  strcat(cmd,s);
  strcat(cmd,", ");
  memset(s,'\0',strlen(s));  
  return 1;  
}

ROUTINE int catquote(char *cmd, char *s)
{
  if(strncmp(s,"NULL",4)==0) {
    strcat(cmd,s);
    strcat(cmd,", ");
  }
  else {
    strcat(cmd,"\"");
    strcat(cmd,s);
    strcat(cmd,"\"");
    strcat(cmd,", ");
  }
  return 1;
}

/*****************************************************************************/
/* Null testers                                                              */
/*****************************************************************************/
ROUTINE char *cat_test_null_int(int i) 
{
  static char buf[40];
  if (i != CAT_NULL_INT) {
    memset(buf,'\0',strlen(buf));       
    sprintf (buf,"%d\0",i);
    return (buf);
  }
  else return (CAT_NULL_SYBASE);
}

ROUTINE char *cat_test_null_short(short i) 
{
  static char buf[40];
  if (i != CAT_NULL_SHORT) {
    memset(buf,'\0',strlen(buf));           
    sprintf (buf,"%d\0",i);
    return (buf);
  }     
  else return (CAT_NULL_SYBASE);
}

ROUTINE char *cat_test_null_float(double n) 
{ 
  static char buf[40];
  float temp;
  temp = (float) n;
  if (temp != CAT_NULL_FLOAT) { 
    memset(buf,'\0',strlen(buf));           
    sprintf (buf,"%f\0",temp);
    return (&buf[0]);
  }
  else return (CAT_NULL_SYBASE);
}

ROUTINE char *cat_test_null_char(char *s) 
{
  if (strcmp(s,CAT_NULL_CHAR) != 0) return(s);
  else return (CAT_NULL_SYBASE);
}

/*****************************************************************************/
/* catGetMsg                                                                 */
/*                                                                           */
/* This is a routine for calling programs to use to translate a catalog      */
/* error message.                                                            */
/*****************************************************************************/
/* C-Callable routine */
ROUTINE char *zcatGetMsg(int err)
{
  static char *catalog_status[MAX_ERR + 1] = {
    "NULL",
    "CATALOG: Successful catalog routine operation",
    "CATALOG:cat_get_msg> Unrecognized status number",
    "CATALOG: SYBASE server error received",
    "CATALOG: An undefined error has occurred",
    "CATALOG: A undefined problem was encountered; continuing...",
    "CATALOG: Pathname exceeds maximum length",
    "CATALOG: Filename exceeds maximum length",
    "CATALOG: Combined file specification is too long",
    "CATALOG: Did not find '/' in pathname!",
    "CATALOG: Insufficient memory for allocation of space",
    "CATALOG: No rows found",
    "CATALOG: No rows affected"
  };
  static char buf[132];

  if ((err > 0) && (err <= MAX_ERR)) {
    strcpy(buf,catalog_status[err]);
    return(buf);
  }
  else {
    if (err == CAT_BADTRANSLATION) {
      strcpy(buf,"CATALOG: Invalid translation index or code encountered");
      return(buf);
    }
    else {
/*      strcpy(buf,catalog_status[CAT_ERROR_ERR]);*/
        sprintf(buf,"CATALOG:cat_get_msg> Unrecognized status number: %d",err);
      return(buf);
    }
  }
}

/* FORTRAN-callable */
ROUTINE void FTN_NAME(xcatgetmsg) (int err, char *buf, ZFORSTR_PARAM)
#if 0
  int err;   /* input */
  char *buf; /* output */
#endif
{
  ZFORSTR_BLOCK
  static char tbuf[132];
  strcpy(tbuf,zcatGetMsg(err));
  zsc2for(tbuf,256,buf,&buf,2,2,1, buf);
  return;
}

/*****************************************************************************/
/* catGetSybaseMsg                                                           */
/*                                                                           */
/* This is a routine for calling programs to use to translate the Sybase     */
/* status returned from the MDMS "getNextRow" routine.                       */
/*****************************************************************************/
/* C-Callable routine */
ROUTINE char *zcatGetSybaseMsg(int err)
{
  static char *catalog_sybase_status [MAX_SYB_ERR+1] = {
    "NULL",
    "CATALOG:SYBASE: Row returned",
    "CATALOG:SYBASE: End of query",
    "CATALOG:SYBASE: End of transaction",
  };
  static char buf[132];

  if ((err > 0) && (err <= MAX_SYB_ERR)) {
    return(catalog_sybase_status[err]);
  }
  else if (err == MDMS_DEADLOCK) {
    return("CATALOG:SYBASE: Database is deadlocked");
  }
  else if (err == MDMS_FATAL) {
    return("CATALOG:MDMS: Fatal error received from MDMS routine");
  }
  else if (err == MDMS_ERROR) {
    return("CATALOG:MDMS: Error received from MDMS routine");
  }
  else if (err == MDMS_WARNING) {
    return("CATALOG:MDMS: Warning received from MDMS routine");
  }
  else {  /* We've got some weird Sybase error; return the status number. */
   sprintf(buf,
          "CATALOG: Unrecognized Sybase error, status = %d",
           err);
   return(buf);
 }
}

/* FORTRAN-callable */
ROUTINE void FTN_NAME(xcatgetsybasemsg) (int err, char *buf, ZFORSTR_PARAM)
#if 0
  int err;   /* input */
  char *buf; /* output */
#endif
{
  ZFORSTR_BLOCK
  static char tbuf[132];
  strcpy(tbuf,zcatGetSybaseMsg(err)); 
  zsc2for(tbuf,256,buf,&buf,2,2,1, buf);
  return;
}

/**************************************************************************/
/* This subroutine checks for leading and trailing "/" in a pathname, and */
/* adds them if they do not exist.                                        */
/**************************************************************************/
ROUTINE int checkSlashes(char *p)
{
  int status = CAT_SUCCESS;
  int len;
  char temp[MIPS_FILEPATH_MAX_LEN];

  memset(temp,'\0',strlen(temp));
  len = strlen(p);

  if (strncmp(&p[0],"/",1) != 0) {
    if ( len + 1 > MIPS_FILEPATH_MAX_LEN) { 
      return(CAT_PATH_TOOBIG);
    }
    message1("CATALOG:UTIL:checkSlashes> added leading forwardslash to %s",p);
    strcpy(temp,"/");
    strcat(temp,p);
    strcpy(p,temp);
  }
  len = strlen(p);
  if (strncmp(&p[len-1],"/",1) != 0) {  
    if ( len + 1 > MIPS_FILEPATH_MAX_LEN) {
      return(CAT_PATH_TOOBIG);
    }
    message1("CATALOG:UTIL:checkSlashes> added trailing forwardslash to %s",p);
    strcat(p,"/");
  }
    
  return(status);
}

/**************************************************************************/
/* This subroutine checks to see if there are any "."'s in a string       */
/* (typically a filename or a file extension) and if it finds any, it     */
/* removes each one.                                                      */
/**************************************************************************/
ROUTINE int checkDots(char *s)
{
  char ts[MIPS_FILEPATH_MAX_LEN];
  char temp[MIPS_FILEPATH_MAX_LEN];
  int i, len;
  int status = CAT_SUCCESS;

  memset(temp,'\0',strlen(temp));
  memset(ts,'\0',strlen(ts));
  strcpy(ts,s);
  len = strlen(ts);
  for (i=0; i< len; i++) {
    if (strncmp(&ts[i],".",1) == 0) {
      message1("CATALOG:checkDots> removing dot from string: %s",ts);    
      if (i != 0) {                         /* copy everything up to the "." */
        strncpy(temp,ts,i);             
        if (i < len-1) {
          strncat(temp,&ts[i+1],strlen(&ts[i+1]));
        }
      }
      else {
        strncpy(temp,&ts[i+1],strlen(&ts[i+1])); /* skip . and copy the rest */
      }
      strcpy(ts,temp);
      memset(temp,'\0',strlen(temp));      
    } 
  }
  strcpy(s,ts);
  return(status);
}

/******************************************************************************
  catGetUserData --

  Get data necessary to access the SYBASE server from the PDF file of the
  program this subroutine is being called from.
******************************************************************************/
/* C-callable version                                                        */
ROUTINE int zcatGetUserData(char *mipsServer,char *mipsDb,
                            char *mipsUsr,char *mipsPw)
{
   int status = CAT_SUCCESS;
   int cnt;

   status = zvp("CATSRV", mipsServer, &cnt);
   message1("getSybaseUserData> using the %s server",mipsServer);
   status = zvp("CATDB",  mipsDb, &cnt);
   message1("                   using the %s database",mipsDb);
   status = zvp("CATUSR", mipsUsr, &cnt);
   message1("                   SYBASE user is %s", mipsUsr);
   status = zvp("CATPW",  mipsPw, &cnt);

   return(status);
 }

/* FORTRAN-callable version                                                 */
ROUTINE void FTN_NAME(xcatgetuserdata) (char *server, char *db, char *usr,
	char *pw, int *status, ZFORSTR_PARAM)
#if 0
char *server, /* returned value */
     *db,     /* returned value */
     *usr,    /* returned value */
     *pw;     /* returned value */
int  *status;
#endif
{
  ZFORSTR_BLOCK
  char mipsServer[MDMS_NAME_LEN],
       mipsDb[MDMS_NAME_LEN],
       mipsUsr[MDMS_NAME_LEN],
       mipsPw[MDMS_NAME_LEN];

  *status = zcatGetUserData(mipsServer,mipsDb,mipsUsr,mipsPw);
  if (*status != CAT_SUCCESS) {
    message1("CATALOG:UTIL:xcatgetuserdata> severe error from zvp: %d",*status);
  }
  zsc2for(mipsServer,MDMS_NAME_LEN,server,&server,4,1,1, status);
  zsc2for(mipsDb,MDMS_NAME_LEN,db,&server,4,2,2, status);
  zsc2for(mipsUsr,MDMS_NAME_LEN,usr,&server,4,3,3, status);
  zsc2for(mipsPw,MDMS_NAME_LEN,pw,&server,4,4,4, status);
  return;
}

/*****************************************************************************
  catGetRets -- internal routine, called by catExecuteCmd
  
Converts all stored procedure OUTPUT returned parms to character strings, and
outputs them to the screen.

These are for the output vars that get returned by the stored proc.
*****************************************************************************/
ROUTINE void catGetRets(void (*catRetvalsFnPtr)(),   /* input */
		       int printflag,               /* input */
		       void *catRetvalsDataStruct   /* output */ )
{
   int numRets = MDMS_RETVALUECOUNT (qiDesc);
   int i;
   char buf[512];

   if (numRets < 1) { /* no returned values; exit */
     if (printflag) {
       message("CATALOG:catGetRets> This stored proc has no output values. Continuing...");
     }
     return;       
   }

   if (printflag) {
     message("CATALOG:catGetRets> The following values were returned:");
     for (i=0; i<numRets; i++) {
         MDMS_RETVALUENTS (qiDesc, i, buf, (DBINT)-1);
         message2("\t%s:   %s", MDMS_RETVALUENAME(qiDesc,i), buf);
     }
   }

   /* Now buffer the returned values */
   (*catRetvalsFnPtr)(qiDesc,catRetvalsDataStruct);

       /* Note:  Here is how you get values, their types and length.
       * int valueType;
       * int valueLength;
       * char *value;
       *
       * valueType = MDMS_RETVALUETYPE (qiDesc, i);
       * valueLength = MDMS_RETVALUELENGTH (qiDesc, i);
       * value = MDMS_RETVALUE (qiDesc, i);
       *
       * But, we just want to convert them to Null terminated strings
       * for showing test results.
       */
   return;
}

/*****************************************************************************
  splitFilespec
  Given a filespec in the format /path/file.ext, this routine will return
  the three components:
    /path/
    file
    ext  
*****************************************************************************/
/* C-callable version */
ROUTINE int zsplitFilespec(char *filespec, /* in */
			   char *path,     /* out */
			   char *afile,    /* out */
			   char *ext      /* out */)
{ 
  char tmp[MIPS_FILEPATH_MAX_LEN],*temp;
  int i,j,index1,index2, len;
  int status = CAT_SUCCESS;

  temp = &tmp[0];

  len = strlen(filespec); 
  if (len > (MIPS_FILEPATH_MAX_LEN + MIPS_FILENAME_MAX_LEN)) {
    message("CATALOG:splitFilespec> filespec length exceeds maximum legal length");
    return(CAT_FILENAME_TOOBIG);
  }

  index1 = 0;
  for (i=0; i<len; i++) {     /* get index of last / in spec */
    if (strncmp(&filespec[i],"/",1) == 0) index1 = i;  
  }
  if (index1 == 0) { /* no slash in pathname! */
    return(CAT_NOSLASHINPATH);
  }

  index1++;                         /* include the last '/' in the pathname */
  strncpy(path,filespec,index1);
  path[index1]=0;

  /* start indexing at index1+1 so we don't get the last '/' in the filename */
  index2 = index1 + 1;
  for (i=index1 + 1; i<len; i++) {
    if (strncmp(&filespec[i],".",1) == 0) index2 = i;      /* position of . */
  }

  if (index2 == index1 + 1 ) {
    message1("CATALOG:UTIL:splitFilespec> no file extension found in %s, continuing...",
             filespec);  
    i = strlen(&filespec[index1]);     /*  i = len - index1  */
    strncpy(afile,&filespec[index1],i);
    afile[i]=0;
    /* extension left as it was memset()ed above. */
  }
  else {
    i = strlen(&filespec[index2+1]);   /*  i = len - (index2+1)  */
    strncpy(ext,&filespec[index2+1],i);
    ext[i]=0;
    j=strlen(&filespec[index1])-strlen(ext)-1;
    strncpy(afile,&filespec[index1],j);
    afile[j]=0;
  }
  return(status);
}

/* FORTRAN-callable version                                               */
ROUTINE void FTN_NAME(xsplitfilespec) (char *filespec, char *path, char *afile,
		char *ext, int *status, ZFORSTR_PARAM)
#if 0
char *filespec, /* input */
     *path,     /* returned value */
     *afile,    /* returned value */
     *ext;      /* returned value */
int  *status;
#endif
{
  ZFORSTR_BLOCK
  char tspec[MIPS_FILESPEC_MAX_LEN],
       tpath[MIPS_FILEPATH_MAX_LEN],
       tfile[MIPS_FILENAME_MAX_LEN],
       text[MIPS_FILETYPE_MAX_LEN];

  zsfor2c(tspec,MIPS_FILESPEC_MAX_LEN-1,filespec,&filespec,4,1,1, status);
  *status = zsplitFilespec(tspec,tpath,tfile,text);
  zsc2for(tpath,MIPS_FILEPATH_MAX_LEN-1,path,&filespec,4,2,2, status);
  zsc2for(tfile,MIPS_FILENAME_MAX_LEN-1,afile,&filespec,4,3,3, status);
  zsc2for(text,MIPS_FILETYPE_MAX_LEN-1,ext,filespec,4,4,4, status);
  return;
}

/*******************************************************************
  This copies the alphabetic part of a string, converting to uppercase.
  It assumes the string starts with alphabetic characters, which are followed
  by non alphabetic characters.  */

void   cat_strcpy_toupper( char *tostr, char *fromstr)
{
  int k;
/*  ==================================================================  */
     for (k=0; isalpha( fromstr[k] ); ++k){
         tostr[k] = toupper(fromstr[k]);
     }
     tostr[k] = '\0';
     return;
}

/****************************************************************************
 combineFilespec            
  Given the three components:
    /path/
    file
    ext  
  This routine will return a filespec in the format /path/file.ext
*****************************************************************************/
/* C-callable version */
ROUTINE int zcombineFilespec(char *path,     /* input */
			     char *afile,    /* input */
			     char *ext,      /* input */
			     char *filespec /* returned value */)
{
  int status = CAT_SUCCESS;
  int len_path,len_file,len_text;
  char tpath[MIPS_FILEPATH_MAX_LEN],
       tfile[MIPS_FILENAME_MAX_LEN],
       text[MIPS_FILETYPE_MAX_LEN],
       tspec[MIPS_FILESPEC_MAX_LEN];

  memset(filespec,'\0',strlen(filespec));

/* The input vars have to be copied over because their lengths may change  */
/* if any /'s are added or .'s deleted, and that will clobber any values   */
/* to the right of the changed value in the parameter list of this         */
/* subroutine.                                                             */

  strcpy(tpath,&path[0]);
  strcpy(tfile,&afile[0]);
  strcpy(text,&ext[0]);

/* check path, filename, and file extension lengths */
  len_path = strlen(tpath);
  if (len_path > MIPS_FILEPATH_MAX_LEN) return(CAT_PATH_TOOBIG);

  len_file = strlen(tfile);
  len_text = strlen(text);
  if (len_file + 1 + len_text > MIPS_FILENAME_MAX_LEN) {
    return(CAT_FILESPEC_TOOBIG);
  }

/* make sure there are leading and trailing slashes in the pathname, and  */
/* remove any "."'s from the filename and extension.                      */

  status = checkSlashes(&tpath[0]);
  if (status != CAT_SUCCESS) {
    message("CATALOG:UTIL:zcombineFilespec> returning control to caller");
    return(status);
  }  
  status = checkDots(&tfile[0]);
  if (status != CAT_SUCCESS) {
    message("CATALOG:UTIL:zcombineFilespec> returning control to caller");
    return(status);
  }
  status = checkDots(&text[0]);
  if (status != CAT_SUCCESS) {
    message("CATALOG:UTIL:zcombineFilespec> returning control to caller");
    return(status);
  }
 
/* concatenate the filespec */
  strcpy(filespec,tpath);
  strcat(filespec,tfile);
  strcat(filespec,".");
  strcat(filespec,text);

  return(status);
} /* end zcombineFilespec */

/* FORTRAN-callable version */
ROUTINE void FTN_NAME(xcombinefilespec) (char * path, char *afile, char *ext,
		char *filespec, int *status, ZFORSTR_PARAM)
#if 0
char *path,     /* input */
     *afile,    /* input */
     *ext,      /* input */
     *filespec; /* returned value */
int  *status;   /* returned value */
#endif
{ 
  ZFORSTR_BLOCK
  char tspec[MIPS_FILESPEC_MAX_LEN],
       tpath[MIPS_FILEPATH_MAX_LEN],
       tfile[MIPS_FILENAME_MAX_LEN],
       text[MIPS_FILETYPE_MAX_LEN];

  zsfor2c(tpath,MIPS_FILEPATH_MAX_LEN-1,path,&path,4,1,1, status);
  zsfor2c(tfile,MIPS_FILENAME_MAX_LEN-1,afile,&path,4,2,2, status);
  zsfor2c(text,MIPS_FILETYPE_MAX_LEN-1,ext,&path,4,3,3, status);  
  *status = zcombineFilespec(tpath,tfile,text,tspec);
  zsc2for(tspec,MIPS_FILESPEC_MAX_LEN-1,filespec,&path,4,4,4, status);
  return;
}

/*****************************************************************************

    catLogin

    This is the first routine you should call when you are going to interface
    with the Sybase catalog.  catLogin initializes Sybase message handlers 
    and the query descriptor and it performs the log in. 

    catLogout

    This is the last routine you should call when you are done interfacing 
    with the Sybase catalog.  catLogout terminates the catalog connection 
    and frees all descriptors.  

    msgDesc and qiDesc in these routines are accessed as a globals.

    Both catLogin and catLogout should only be called once by a program.
******************************************************************************/
ROUTINE int catLogin(cat_user_struct_typ *userInfo)
{
  int status = CAT_SUCCESS;
   /* 
     Initialize the message descriptor and the message handlers. 
   */
   if ((msgDesc=mdms_messageInit(userInfo->progname,"MIPL")) == 
	(MDMS_MSGDESC *) NULL) {
      message1("CATALOG:UTIL:catLogin> Sybase error, status = %d",
	        MDMS_FATAL);
      return(CAT_SYBASE_ERR);
   }
   (void) mdms_sybErrHandler (msgDesc, MDMS_ON);
   (void) mdms_sybMsgHandler (msgDesc, MDMS_ON);
   
   /* 
     Get a query descriptor, and assign the message descriptor to it. 
   */
   if ((qiDesc = mdms_qiDescAlloc (msgDesc)) == (MDMS_QI_DESC_OBJ *) NULL) {
      message1("CATALOG:UTIL:catLogin> Sybase error: %s",
               zcatGetSybaseMsg(MDMS_FATAL));
      return(CAT_SYBASE_ERR);     
   }

   /* 
     Fill the query descriptor with program name, the user's server login
     name, user password, server name, and database name. 
   */ 
    
    /*login info supplied by user*/

   if (userInfo) { 
     strcpy( qiDesc->program,  userInfo->progname );
     strcpy( qiDesc->username, userInfo->user );
     strcpy( qiDesc->password, userInfo->passwd );
     strcpy( qiDesc->server,   userInfo->server );
     strcpy( qiDesc->dbname,   userInfo->db );
   }
  /* will be supplied indirectly */
   else {              
    message(" Login prompt is not yet supported. ");
    message(" Login info must supplied ");
    return(CAT_SYBASE_ERR);
   }
   /*
     Perform the log on
   */

   if((status = mdms_qiLogin(qiDesc)) < MDMS_OK) {
      message1("CATALOG:UTIL:catLogin> Sybase error: %s",
               zcatGetSybaseMsg(status));
      return(CAT_SYBASE_ERR);     
   }

   return(status);
} /* end catLogin */

ROUTINE int catLogout(void) 
{
  int status = CAT_SUCCESS;

  /*
    Perform the logout from Sybase and free qiDesc 
  */

   (void) mdms_qiExit ();         

  /*
    Free the message descriptor.
  */

  if ((status = mdms_messageFreeDesc(msgDesc)) != MDMS_OK) {
    message1("CATALOG:UTIL:catLogout> Sybase error, status=%d",
	     status);
    return(CAT_SYBASE_ERR);
  }

   return(status);

} /* end catLogout */


/*****************************************************************************

    catFillQiDescCmd

    This is the second routine you should call when you are going to interface
    with the Sybase catalog.

    It may be called repeatedly.

    This routine sets the query descriptor's command pointer.

******************************************************************************/
ROUTINE void catFillQiDescCmd(
  void (*catFillCmdFnPtr)(),  /* input; function pointer to a sp-specific
                                        routine that fills the data struct. */
  void *catDataQueryStruct   /* input; QUERY data structure specific to 
				        a sp. */
)
{
  static char tempCmd[102400];
   /* 
     Set the query descriptor's command pointer: Call the routine
     passed in by function pointer, which knows how to correctly 
     sprintf the elements of the query data structure (also passed in) 
     into the buffer tempCmd.
     Every stored proc has its own fillCmdFn and dataQueryStruct.
    */

   (*catFillCmdFnPtr)(catDataQueryStruct,tempCmd);
   qiDesc->cmd = tempCmd;  /* Was SETCMD macro. */
 
   return;

 } /* end catFillQiDescCmd */

/*****************************************************************************
  catSetNulls

  This little routine setts up the null values that will get translated 
  from the Sybase NULL.

*****************************************************************************/
ROUTINE void catSetNulls(void)
{
  int status;  /* Sybase succeed = , fail =  */
  BYTE *cat_null_byte_p;
  BYTE *cat_null_short_p;
  BYTE *cat_null_int_p;
  BYTE *cat_null_float_p;
  BYTE *cat_null_double_p;
  BYTE *cat_null_char_p;
  BYTE null_byte;
  short null_short;
  int null_int;
  float temp_float,null_float;
  double temp_double,null_double;
  char null_char[1 +1];
  static int float_conv[12];
  static int double_conv[12];

  /*  Initialize local variables */

  null_byte = CAT_NULL_BYTE;
  null_short = CAT_NULL_SHORT;
  null_int = CAT_NULL_INT;

  if (CONVERT_SYBASE_IEEE2D) {
    temp_float = CAT_NULL_FLOAT;
    zvtrans_out(float_conv,"REAL","REAL","NATIVE","RIEEE");
    zvtrans(float_conv,&temp_float,&null_float,1);

    temp_double = (double) CAT_NULL_FLOAT;
    zvtrans_out(double_conv,"DOUB","DOUB","NATIVE","RIEEE");
    zvtrans(double_conv,&temp_double,&null_double,1);
  }
  else {
    null_float = CAT_NULL_FLOAT;
    null_double = (double) CAT_NULL_FLOAT;
  }
 
  strcpy(null_char,CAT_NULL_CHAR);
 
  cat_null_byte_p = (BYTE *) &null_byte;
  cat_null_short_p = (BYTE *) &null_short;
  cat_null_int_p = (BYTE *) &null_int;
  cat_null_float_p = (BYTE *) &null_float;
  cat_null_double_p = (BYTE *) &null_double;
  cat_null_char_p = (BYTE *) null_char;

  status = dbsetnull( qiDesc->dbproc, MDMS_TINYBIND, 0, cat_null_byte_p);
  status = dbsetnull( qiDesc->dbproc, MDMS_SMALLBIND,0, cat_null_short_p);
  status = dbsetnull( qiDesc->dbproc, MDMS_INTBIND,  0, cat_null_int_p);
  status = dbsetnull( qiDesc->dbproc, MDMS_REALBIND, 0, cat_null_float_p);
  status = dbsetnull( qiDesc->dbproc, MDMS_FLT8BIND, 0, cat_null_double_p); 
  status = dbsetnull( qiDesc->dbproc, MDMS_NTBSTRINGBIND,
                    strlen(CAT_NULL_CHAR), cat_null_char_p);

  /* for variables of type sybase native DBDATETIME, the default null is
     8 bytes of zeros which is 1900-JAN-01 */
 
  return;
}


/*****************************************************************************
  catExecuteCmd

  (analagous to qiPrintResults in the cmdgen output)

  This is the routine that actually executes the stored proc (i.e. processes
  the results sets.)

  qiDesc is used in this routine as a global.
*****************************************************************************/
ROUTINE int catExecuteCmd(
  int (*catGetRowsFnPtr)(),  /* input; sp-specific call to buffer data       */
  char *catDataStruct,       /* output; sp-specific data structure           */
                             /*         An array pointers to void (which is  */
                             /*         actually an array of ptrs to struct. */
  int rec_length,            /* size of data structure */
  int *nrows,                /* output; # rows found and buffered            */
  int maxrows,               /* input */
  int printflag,             /* input; 1 = print values in structs to screen or
			               log */
  void (*catPrintoutFnPtr)(), /* input; sp-specific routine to print values  */
  void *catRetvalsDataStruct, /* output; sp-specific returned vals struct    */
  void (*catRetvalsFnPtr)()   /* input */
)
{
   int rows_affected = 0; 
   int rows_returned = 0;
   int status;
   int temp_stat,mstat;
   char *temp;
   char msg[256];
   int n=0;
   int proc = 0;

   if (printflag) null_message();

   /*
	initialize temporary buffer
   */

   temp = (char *) malloc(rec_length);
   do {

     status = (*catGetRowsFnPtr) (qiDesc,temp);

     if (status == MDMS_ROWRETURNED)
     {
	memcpy(catDataStruct + (n*rec_length),temp,rec_length);

        /* print out contents of data structure & result sets if needed */
        if (printflag)
           (*catPrintoutFnPtr)(qiDesc,catDataStruct + (n*rec_length) );

	rows_returned++;
        n ++;
     }
     else if (status == MDMS_ENDOFQUERY)
     {
   /***********************************************************************
    * Buffer the returned values into the retvals data structure. Only one 
    * set of returned values is returned per stored proc call, so you don't
    * have to do anything like returning an array of structs.
    * If "printflag" is 1, then also print out the values. 
    **********************************************************************/
	catGetRets(catRetvalsFnPtr,printflag,catRetvalsDataStruct);
        if (MDMS_HASRETSTAT(qiDesc) || proc == 1) {
          if (!proc) proc = 1;
          rows_affected += MDMS_PROCRETURN(qiDesc);
	}
	else if (MDMS_AFFECTED(qiDesc) >= 0 && proc == 0)
	    rows_affected += MDMS_AFFECTED(qiDesc);
     } 
   } while ((status == MDMS_ENDOFQUERY || status == MDMS_ROWRETURNED) 
	     && (rows_returned < maxrows));

   if (status < MDMS_OK )
   {
     message1("CATALOG:catExecuteCmd-getrowsError> Sybase error: %s",
               zcatGetSybaseMsg(status));
     return(CAT_SYBASE_ERR);    
   }

   /*************
	Flushing out Sybase transaction
   **************/
   temp_stat = status;
   while(temp_stat != MDMS_ENDOFTRANSACTION)
   {
     temp_stat = (*catGetRowsFnPtr) (qiDesc,
                                  temp);

     if (temp_stat < MDMS_OK ) break;
   }

   /*
	free the temporary buffer
   */
   free(temp);

   if (rows_returned == 0)  {
      *nrows = rows_affected;
      sprintf(msg,"CATALOG:catExecuteCmd> %d row(s) affected.",*nrows);
   }
   else  {
      *nrows = rows_returned;
      sprintf(msg,"CATALOG:catExecuteCmd> %d row(s) returned.",*nrows);
   }
   if (printflag) {
      zvmessage(msg,"");
   }

   status = CAT_SUCCESS;
   return (status);
} /*end catExecuteCmd */

/* end module */
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create cat_gen_util.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE cat_gen_util
/*
/*   To Create the build file give the command:
/*
/*		$ vimake cat_gen_util			(VMS)
/*   or
/*		% vimake cat_gen_util			(Unix)
/*
/*****************************************************************************/

#define SUBROUTINE cat_gen_util

#define MODULE_LIST cat_gen_util.c

#define MAIN_LANG_C

#define USES_ANSI_C

#define P2_SUBLIB
#define LIB_MDMS
#define LIB_SYBASE

/**********  End of cat_gen_util imake file  **********/
$ Return
$!#############################################################################
$Test_File:
$ create tcat_gen_util.imake
#define PROGRAM tcat_gen_util

#define MODULE_LIST tzcat_gen_util.c tcat_gen_util.f
#define MAIN_LANG_C
#define USES_ANSI_C
#define USES_FORTRAN
#define TEST /* This is a unit test for the subroutine module cat_gen_util */

#define LIB_FORTRAN
#define LIB_SYBASE
#define LIB_MDMS
#define LIB_P2SUB 
#define LIB_TAE
#define LIB_RTL
#define LIB_KERBEROS
/* remove the line below for delivery */
/*****
#define LIB_LOCAL 
******/
$!-----------------------------------------------------------------------------
$ create tzcat_gen_util.c
/* Unit test program for cat_gen_util */
#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"

#ifndef GLL_SSI_RTS_CAT_H
#include "gll_ssi_rts_cat.h"
#endif

void main44() {
  int cnt;
  char tests[10];

  zvp("TESTS",tests,&cnt);
  if ((strcmp(tests,"both") == 0) || (strcmp(tests,"gen") == 0)) {
    message("Testing null routines:");
    test_null_routines();
    null_message();
    null_message();
    message("Testing routines with bridges:");
    test_bridged_routines();
    null_message();
    null_message();
  }
  if ((strcmp(tests,"both") == 0) || (strcmp(tests,"sybase") == 0)) {  
    message("Testing Sybase routines:");
    test_sybase_routines();
  }
  return;
}

ROUTINE test_null_routines() 
{
  char buf[132];
  long null_int;

  null_int = CAT_NULL_INT;

  message2("  testing int %d: returns %s",
              10,cat_test_null_int(10));
  message2("  testing int %d: returns %s",
              -50,cat_test_null_int(-50));
  message2("  testing null int %d: returns %s",	   
              null_int,cat_test_null_int(null_int));
  null_message();

  message2("  testing short %d: returns %s",
              1,cat_test_null_short(1));
  message2("  testing short %d: returns %s",
              -10,cat_test_null_short(-10));
  message2("  testing null short %d: returns %s",
              CAT_NULL_SHORT,cat_test_null_short(CAT_NULL_SHORT));
  null_message();

/* cat_test_null_float seems to have an allergy to message2(). */
  sprintf(buf,"  testing float %f: returns %s",
              10.0,cat_test_null_float(10.0));
  zvmessage(buf,0);
  sprintf(buf,"  testing float %f: returns %s",
              1.020, cat_test_null_float(1.020));
  zvmessage(buf,0);
  sprintf(buf,"  testing float %f: returns %s",
              -55.2, cat_test_null_float(-55.2) );
  zvmessage(buf,0);
  sprintf(buf,"  testing null float %f: returns %s",
              CAT_NULL_FLOAT, cat_test_null_float(CAT_NULL_FLOAT) );
  zvmessage(buf,0);
  null_message();

  message2("  testing char '%s': returns %s",
              "x",cat_test_null_char("x"));
  message2("  testing char '%s': returns %s",
              "test",cat_test_null_char("test"));
  message2("  testing char '%s': returns %s",
              "testing 123",cat_test_null_char("testing 123"));
  message2("  testing null char %s:  returns %s",
              CAT_NULL_CHAR,cat_test_null_char(CAT_NULL_CHAR));
  null_message();
  return 1;
}

ROUTINE test_sybase_routines() 
{
  static cat_user_struct_typ           userInfo;
  int cnt,i,status;
  char tables[10];

  null_message();

  zcatGetUserData(userInfo.server,userInfo.db,userInfo.user,userInfo.passwd);
  strcpy(userInfo.progname,"tstcat_gen_util");
  userInfo.printflag = 1;  /* print out all the results structures */

  /* Initialize descriptors for the program and login to Sybase. */
  status = catLogin(&userInfo);
  if (status != CAT_SUCCESS) return(status);

  zvp("TABLES",tables,&cnt);

  if ((strcmp(tables,"all") == 0) || (strcmp(tables,"raw") == 0)) 
    test_raw(userInfo.printflag);

  if ((strcmp(tables,"all") == 0) || (strcmp(tables,"over") == 0)) 
    test_over(userInfo.printflag);

  if ((strcmp(tables,"all") == 0) || (strcmp(tables,"ict") == 0)) 
    test_ict(userInfo.printflag);

  if ((strcmp(tables,"all") == 0) || (strcmp(tables,"opnav") == 0)) 
    test_opnav(userInfo.printflag);

  /*
     Free all descriptors and logout from Sybase.
  */
  catLogout();

  return 1;
}

ROUTINE test_raw(int printflag)
{
  int status = CAT_SUCCESS;
  int mergenum;
  cat_ssiRtsRaw_struct_query_typ   rawQueryStruct;
  
  null_message();
  message("******************************************************************");
  message("Testing catSsiRtsRaw");
  message("******************************************************************");
/* First, stuff the query structure. */
  rawQueryStruct.sclkstrtcnt    = 13131313;
  rawQueryStruct.sclkpartition  = 1;
  rawQueryStruct.piccount       = 1;
  strcpy (rawQueryStruct.filepath,     "testpath");
  strcpy (rawQueryStruct.filename,     "testname");
  strcpy (rawQueryStruct.filetype,     "MOS");
  rawQueryStruct.filestatus     = 1;
  rawQueryStruct.telemfmtid     = 23;               /* translates to "AI8" */
  rawQueryStruct.framedur       = 3;
  rawQueryStruct.exposdur       = 10.0;
  rawQueryStruct.filtname       = 1;               /* translates to "GREEN" */
  rawQueryStruct.gainmodeid     = 3;               /* translates to "400K" */
  rawQueryStruct.expostype      = 1;               /* translates to "EXT" */
  rawQueryStruct.blemprotflag   = 1;               /* translates to "ON" */
  rawQueryStruct.invclckstflag  = 1;               /* translates to "INV" */
  rawQueryStruct.litefldstflag  = 1;               /* translates to "ON" */
  rawQueryStruct.meantruncbits  = 5.0;
  rawQueryStruct.meantruncsamp  = 5.0;
  strcpy (rawQueryStruct.encodingtype, "ICT");
  strcpy (rawQueryStruct.obstructid,   "P");
  rawQueryStruct.rearopttemp    = -10.0;
  rawQueryStruct.ccdtemp        = -100.0;
  rawQueryStruct.rotorposition  = 1.0;
  rawQueryStruct.coneang        = 1.0;
  rawQueryStruct.tml            = 10;
  rawQueryStruct.maxcml         = 10;
  rawQueryStruct.startgapline1  = 1;
  rawQueryStruct.startgapline2  = 1;
  rawQueryStruct.startgapline3  = 3;  
  rawQueryStruct.stopgapline1   = 10;
  rawQueryStruct.stopgapline2   = 20;
  rawQueryStruct.stopgapline3   = 30;
  rawQueryStruct.gaps           = 5;	   
  rawQueryStruct.averagedn      = 50;
  strcpy (rawQueryStruct.ert,          "1994-MAR-15");
  rawQueryStruct.ertmilli       = 0;
  rawQueryStruct.meanictratio   = 50.0;
  rawQueryStruct.meanhufratio   = 50.0;
  rawQueryStruct.minictratio    = 10.0;      
  rawQueryStruct.minhufratio    = 10.0;
  rawQueryStruct.maxictratio    = 100.0;
  rawQueryStruct.maxhufratio    = 100.0;
  rawQueryStruct.maxssnr        = 20.0;
  rawQueryStruct.minssnr        = 2.0;
  strcpy (rawQueryStruct.dsnstation,   "MD");
  strcpy (rawQueryStruct.telemmode,    "RT");
  strcpy (rawQueryStruct.note, "This is bogus data for a unit test.");

  status = catSsiRtsRaw(printflag,&mergenum,&rawQueryStruct);
  if (status == CAT_SUCCESS) {
    message2("status = %d, mergenum = %d",status,mergenum);
  }
  else {
    message2("error returned: status %d:\n%s",status,zcatGetMsg(status));
  }
  return 1;
} /* end test_raw */

ROUTINE test_over(int printflag)
{
  int status = CAT_SUCCESS;
  cat_ssiRtsOver_struct_query_typ  overQueryStruct;
  cat_ssiRtsOver_struct_typ        overStruct[MAX_SSI_OVER_ROWS];
  char tbuf[132];

  null_message();
  message("******************************************************************");
  message("Testing catSsiRtsOver for a non-existing row");
  message(" Should get 'no rows found'");
  message("******************************************************************");
  /* First, stuff the query structure. */
  overQueryStruct.searchSclk    = 11011;
  overQueryStruct.sclkpartition  = 222;

  /* Pass in the array of pointers to struct via "overStruct". */

  status = catSsiRtsOver(printflag,&overQueryStruct,overStruct);
  if (status == CAT_SUCCESS )
    message("ERROR: should not find any rows.");
  else {
    message2("status %d:\n%s",status,zcatGetMsg(status));
  }

  null_message();
  message("******************************************************************");
  message("Testing catSsiRtsOver for an existing row");
  message("******************************************************************");
  /* First, stuff the query structure. */
  overQueryStruct.searchSclk    = 11111;
  overQueryStruct.sclkpartition  = 222;

  /* Pass in the array of pointers to struct via "overStruct". */

  status = catSsiRtsOver(printflag,&overQueryStruct,overStruct);
  if (status == CAT_SUCCESS )
    message1("status = %d",status);
  else {
    message2("status %d:\n%s",status,zcatGetMsg(status));
    return 1;
  }

  /* Now, be paranoid and check all the values that are in the struct. */
  message("Values returned from Overview:");
  message1("obsid: %s",overStruct[0].obsid);
  message1("targname: %s",overStruct[0].targname);
  message1("orbnumchar: %s",overStruct[0].orbnumchar);
  message1("imageid: %s",overStruct[0].imageid);
  message1("imagetime: %s",overStruct[0].imagetime);
  message1("imagetimemilli: %d",overStruct[0].imagetimemilli);
  message1("predtelemfmtid_str: %s",overStruct[0].predtelemfmtid_str);
  message1("predtelemfmtid: %d",overStruct[0].predtelemfmtid);
  sprintf(tbuf,"predframedur: %f",overStruct[0].predframedur);
  message(tbuf);
  sprintf(tbuf,"predexposdur: %f",overStruct[0].predexposdur);
  message(tbuf);
  message1("predfiltname_str: %s",overStruct[0].predfiltname_str);
  message1("predfiltname: %d",overStruct[0].predfiltname);
  message1("predgainmodeid_str: %s",overStruct[0].predgainmodeid_str);
  message1("predgainmodeid: %d",overStruct[0].predgainmodeid);
  message1("onchipmosaic: %d",overStruct[0].onchipmosaic);
  message1("opnavflag: %d",overStruct[0].opnavflag);
  message1("timeclsapr: %d",overStruct[0].timeclsapr);
  message1("sattmclsapr: %d",overStruct[0].sattmclsapr);
  sprintf(tbuf,"targctrdist: %f",overStruct[0].targctrdist);
  message(tbuf);
  sprintf(tbuf,"centbdydist: %f",overStruct[0].centbdydist);
  message(tbuf);
  sprintf(tbuf,"subscrange: %f",overStruct[0].subscrange);
  message(tbuf);
  sprintf(tbuf,"subsclat: %f",overStruct[0].subsclat);
  message(tbuf);
  sprintf(tbuf,"subsclon: %f",overStruct[0].subsclon);
  message(tbuf);
  sprintf(tbuf,"subsollat: %f",overStruct[0].subsollat);
  message(tbuf);
  sprintf(tbuf,"subsollon: %f",overStruct[0].subsollon);
  message(tbuf);
  sprintf(tbuf,"minlat: %f",overStruct[0].minlat);
  message(tbuf);
  sprintf(tbuf,"minlon: %f",overStruct[0].minlon);
  message(tbuf);
  sprintf(tbuf,"maxlat: %f",overStruct[0].maxlat);
  message(tbuf);
  sprintf(tbuf,"maxlon: %f",overStruct[0].maxlon);
  message(tbuf);
  sprintf(tbuf,"angsemidiam: %f",overStruct[0].angsemidiam);
  message(tbuf);
  sprintf(tbuf,"centerlat: %f",overStruct[0].centerlat);
  message(tbuf);
  sprintf(tbuf,"centerlon: %f",overStruct[0].centerlon);
  message(tbuf);
  sprintf(tbuf,"slantdist: %f",overStruct[0].slantdist);
  message(tbuf);
  sprintf(tbuf,"resolution: %f",overStruct[0].resolution);
  message(tbuf);
  sprintf(tbuf,"smearmag: %f",overStruct[0].smearmag);
  message(tbuf);
  sprintf(tbuf,"localhourang: %f",overStruct[0].localhourang);
  message(tbuf);
  sprintf(tbuf,"incidang: %f",overStruct[0].incidang);
  message(tbuf);
  sprintf(tbuf,"emissang: %f",overStruct[0].emissang);
  message(tbuf);
  sprintf(tbuf,"phsang: %f",overStruct[0].phsang);
  message(tbuf);
  sprintf(tbuf,"ra: %f",overStruct[0].ra);
  message(tbuf);
  sprintf(tbuf,"declination: %f",overStruct[0].declination);
  message(tbuf);
  sprintf(tbuf,"twist: %f",overStruct[0].twist);
  message(tbuf);

  return 1;
}

ROUTINE test_ict(int printflag)
{
  int status = CAT_SUCCESS;
  cat_ssiRtsIct_struct_query_typ   ictQueryStruct;
  cat_ssiRtsIct_struct_typ         ictStruct[MAX_SSI_ICT_ROWS];
  int nrows;

  null_message();
  message("******************************************************************");
  message("Testing catSsiRtsIct");
  message("******************************************************************");
  /* First, stuff the query structure. */
  ictQueryStruct.sclkstrtcnt    = 12345;
  ictQueryStruct.sclkpartition  = 123;
  strcpy(ictQueryStruct.searchErt,"1995-JAN-03");

  status = catSsiRtsIct(printflag,&nrows,&ictQueryStruct,ictStruct);
  if (status == CAT_SUCCESS) {
    message2("status = %d, nrows = %d",status,nrows);
  }
  else {
    message2("error returned: status %d:\n%s",status,zcatGetMsg(status));
  }

  /* Now, be paranoid and check all the values that are in the struct. */
  message("Values returned from ICT[0]:");
  message1("ictseqnum: %d",ictStruct[0].ictseqnum);
  message1("quantstepsize: %d",ictStruct[0].quantstepsize);

  message1("quantmatrixname_str: %s",ictStruct[0].quantmatrixname_str);
  message1("quantmatrixname: %d",ictStruct[0].quantmatrixname);

  message1("zigzagtablename_str: %s",ictStruct[0].zigzagtablename_str);
  message1("zigzagtablename: %d",ictStruct[0].zigzagtablename);

  message1("huftablename_str: %s",ictStruct[0].huftablename_str);
  message1("huftablename: %d",ictStruct[0].huftablename);

  message1("startcountoutwinline: %d",ictStruct[0].startcutoutwinln);
  message1("startcutoutwinsample: %d",ictStruct[0].startcutoutwinsamp);
  message1("cutoutwinlines: %d",ictStruct[0].cutoutwinlines);
  message1("cutoutwinsamples: %d",ictStruct[0].cutoutwinsamples);
  message1("starttruthwinline: %d",ictStruct[0].starttruthwinln);
  message1("starttruthwinsample: %d",ictStruct[0].starttruthwinsamp);
  message1("predert: %s",ictStruct[0].predert);
  message1("despikethresh: %d",ictStruct[0].despikethresh);
  message1("encodingtype: %s",ictStruct[0].encodingtype);
  return 1;
}

ROUTINE test_opnav(int printflag)
{
  int status = CAT_SUCCESS;
  cat_ssiRtsOpnav_struct_query_typ opnavQueryStruct;
  cat_ssiRtsOpnav_struct_typ       opnavStruct[MAX_SSI_OPNAV_ROWS];
  int nstructs = 0;

  null_message();
  message("******************************************************************");
  message("Testing catSsiRtsOpnav");
  message("******************************************************************");
/* First, stuff the query structure. */
  opnavQueryStruct.sclkstrtcnt    = 12345;
  opnavQueryStruct.sclkpartition  = 123;

  status = catSsiRtsOpnav(printflag,&opnavQueryStruct,opnavStruct);
  if (status == CAT_SUCCESS) {
    message1("status = %d",status);
  }
  else {
    message2("error returned: status %d:\n%s",status,zcatGetMsg(status));
  }

  /* Now, be paranoid and check all the values that are in the struct. */
  message("Values returned from Opnav:");
  message1("starareas: %d",opnavStruct[0].starareas);
  message1("stararealines1: %d",opnavStruct[0].stararealines1);
  message1("starareasamp1: %d",opnavStruct[0].starareasamp1);
  message1("stararealines2: %d",opnavStruct[0].stararealines2);
  message1("starareasamp2: %d",opnavStruct[0].starareasamp2);
  message1("stararealines3: %d",opnavStruct[0].stararealines3);
  message1("starareasamp3: %d",opnavStruct[0].starareasamp3);
  message1("stararealines4: %d",opnavStruct[0].stararealines4);
  message1("starareasamp4: %d",opnavStruct[0].starareasamp4);
  message1("stararealines1: %d",opnavStruct[0].stararealines5);
  message1("starareasamp1: %d",opnavStruct[0].starareasamp5);

  return 1;
}

/****************************************************************************/
/*****************************************************************************/
ROUTINE test_bridged_routines() {
  char server[MDMS_NAME_LEN], 
     db[MDMS_NAME_LEN], 
     usr[MDMS_NAME_LEN], 
     pw[MDMS_NAME_LEN],
     filespec[255],
     path[255],
     file[120],
     product_type[10],
     ext[10];
  int status, i;
  char buf[512];

  message("...C interface...");
  message("******************************************************************");
  message("Testing zcatGetMsg");
  message("******************************************************************");
  for (i=1; i<= MAX_ERR; i++) {
    message2("  msg %d: %s",i,zcatGetMsg(i));    
  }
  message2("  msg %d: %s",CAT_BADTRANSLATION,zcatGetMsg(CAT_BADTRANSLATION));
  message("  Test error handling:");
  message2("  msg %d: %s",MAX_ERR + 100,zcatGetMsg(MAX_ERR + 100));
  null_message();

  message("******************************************************************");
  message("Testing zcatGetSybaseMsg");
  message("******************************************************************");
  for (i=1; i<= MAX_SYB_ERR; i++) {
    message2("  msg %d: %s",i,zcatGetSybaseMsg(i));    
  }
  message2("  msg %d: %s",MDMS_DEADLOCK,zcatGetSybaseMsg(MDMS_DEADLOCK));
  message2("  msg %d: %s",MDMS_FATAL,zcatGetSybaseMsg(MDMS_FATAL));
  message2("  msg %d: %s",MDMS_ERROR,zcatGetSybaseMsg(MDMS_ERROR));
  message2("  msg %d: %s",MDMS_WARNING,zcatGetSybaseMsg(MDMS_WARNING));
  message("  Test error handling:");
  message2("  msg %d: %s",MAX_SYB_ERR + 100,
           zcatGetSybaseMsg(MAX_SYB_ERR + 100));    
  null_message();

  message("******************************************************************");
  message("Testing catGetUserData");
  message("******************************************************************");
  status = zcatGetUserData(server,db,usr,pw);
  message1("  Server is %s",server);
  message1("  Database is %s",db);
  message1("  Username is %s",usr);
  message1("  Password is %s",pw);
  null_message();

  message("******************************************************************");
  message("Testing splitFilespec");
  message("******************************************************************");
  message("  splitting filespec /dev/mos/testdir/imagefile1.ext1");

  memset(path,0,sizeof(path));
  memset(file,0,sizeof(file));
  memset(ext,0,sizeof(ext));

  zsplitFilespec("/dev/mos/testdir/imagefile1.ext1",path,file,ext);
  message1("   Path is %s",path);
  message1("   File is %s",file);
  message1("   Extension is %s",ext);  
  null_message();

  message("Testing cat_strcpy_toupper");
  cat_strcpy_toupper(product_type,ext);  /*  extract alphabetic part  */
  message1("   Producttype is %s",product_type);  
  null_message();

  message("  splitting filespec /dev/mos/testdir/dir2/calibrationfile");

  memset(path,0,sizeof(path));
  memset(file,0,sizeof(file));
  memset(ext,0,sizeof(ext));

  zsplitFilespec("/dev/mos/testdir/dir2/calibrationfile",path,file,ext);
  message1("   Path is %s",path);
  message1("   File is %s",file);
  message1("   Extension is %s",ext);  
  null_message();

  message("******************************************************************");
  message("Testing combineFilespec");
  message("******************************************************************");
  message("  combining path = /dev/mos/testdir/, file = imagefile, ext = ext1");
  zcombineFilespec("/dev/mos/testdir/","imagefile","ext1",filespec);
  message1("   Filespec is %s",filespec);
  null_message();
  message("  combining path = dev/mos/testdir/, file = imagefile., ext = .ext1");
  zcombineFilespec("dev/mos/testdir/","imagefile.",".ext1",filespec);
  message1("   Filespec is %s",filespec);
  null_message();
  message("  combining path = /dev/mos/testdir, file = ima.ge.file, ext = ext1");
  zcombineFilespec("/dev/mos/testdir","ima.ge.file","ext1",filespec);
  message1("   Filespec is %s",filespec);
  null_message();
  null_message();

/*
  message("*******************************************************************");
  message("*******************************************************************");
  message("...FORTRAN interface for some routines");
  FTN_NAME(tcat_gen_util)();
*/
  return 1;
}



$!-----------------------------------------------------------------------------
$ create tcat_gen_util.f
c
c test the FORTRAN bridges of the subroutines in the module cat_gen_util
c
      subroutine tcat_gen_util()
    
      character*16  server,db
      character*21  user,passwd
      character*100 msg
      character*255 filespec,path
      character*120 file
      character*10  ext
      character*512 buf
      integer*4     i

c*************************************************************************
c catgetmsg
c*************************************************************************
      call xvmessage('***** Testing xcatgetmsg',' ')
      do i = 1,10
        call xcatgetmsg(i,buf)
        write(msg,3)i,buf
 3      format(' msg ',i2,'> ',a80)
        call xvmessage(msg,' ')
      enddo

      call xvmessage(' ',' ')

c*************************************************************************
c catgetsybasemsg
c*************************************************************************
      call xvmessage('***** Testing xcatgetsybasemsg',' ')
      do i = 1,3
        call xcatgetsybasemsg(i,buf)
        write(msg,3)i,buf
        call xvmessage(msg,' ')
      enddo
      call xcatgetsybasemsg(1205,buf)
      write(msg,4)buf
 4    format(' msg 1205> ',a80)
      call xvmessage(msg,' ')

      call xvmessage(' ',' ')

c*************************************************************************
c catgetuserdata
c*************************************************************************
      call xvmessage('***** Testing xcatgetuserdata',' ')
      call xcatgetuserdata(server,db,user,passwd)
      write(msg,10)server
      call xvmessage(msg,' ')
10    format(' server = ',a16)
      write(msg,20)db 
      call xvmessage(msg,' ')
20    format(' database = ',a16)
      write(msg,30)user
      call xvmessage(msg,' ')
30    format(' username = ',a31)
      write(msg,40)passwd
      call xvmessage(msg,' ')
40    format(' password = ',a31)
 
      call xvmessage(' ',' ')

c*************************************************************************
c splitfilespec
c*************************************************************************
c Test standard case for splitfilespec
      call xvmessage('***** Testing xsplitfilespec',' ')
      call xvmessage('splitting /dev/mos/testdir/imagefile.ext1',' ')
      call xsplitfilespec('/dev/mos/testdir/imagefile.ext1',
     +                     path,file,ext)
      write(msg,50)path
      call xvmessage(msg,' ')
50    format(' path = ',a50)
      write(msg,60)file
      call xvmessage(msg,' ')
60    format(' file = ',a50)
      write(msg,70)ext
      call xvmessage(msg,' ')
70    format(' ext = ',a50)

      call xvmessage(' ',' ')

c*************************************************************************
c combinefilespec
c*************************************************************************
c Test standard case for combinefilespec
      call xvmessage('***** Testing xcombinefilespec',' ')
      call xvmessage('combining /dev/mos/testdir/,imagefile,ext1',' ')
      call xcombinefilespec('/dev/mos/testdir/',
     +                      'imagefile','ext1',filespec)
      write(msg,80)filespec
      call xvmessage(msg,' ')

      call xvmessage(' ',' ')

c Test typical error handling for combinefilespec
      call xvmessage('combining dev/mos/testdir/,imagefile.,.ext1',' ')
      call xcombinefilespec('dev/mos/testdir/',
     +                      'imagefile.','.ext1',filespec)
      write(msg,80)filespec
      call xvmessage(msg,' ')

      call xvmessage(' ',' ')

      call xvmessage('combining /dev/mos/testdir,ima.ge.file,ext1',' ')
      call xcombinefilespec('/dev/mos/testdir',
     +                      'ima.ge.file','ext1',filespec)
      write(msg,80)filespec
      call xvmessage(msg,' ')
80    format(' filespec = ',a50)

      call xvmessage(' ',' ')

      end
$!-----------------------------------------------------------------------------
$ create tcat_gen_util.pdf
!pdf for tcat_gen_util.pdf
process
parm catsrv string 
parm catdb  string 
parm catusr string
parm catpw  string
parm tests  string
parm tables string
end-proc
$!-----------------------------------------------------------------------------
$ create tstcat_gen_util.pdf
!unit test for cat_gen_util

procedure help = *
parm user   (string,30)
parm pass   (string,30)
parm server (string,30) default="MIPSDB1"
parm db     (string,30) default="devCat"
parm tests  keyword valid = (both,sybase,gen) default = both
parm tables keyword valid = (all,raw,over,ict,opnav) default = all
body
write "  **************************************************************"
write "  **************************************************************"
write "  ***  LOAD TEST DATA INTO CATALOG ********"
write "  **************************************************************"
write "  **************************************************************"

run_isql user=&user  pass=&pass server=&server +
	sy_filename=tstcat_gen_util.load

write "  ********run test ********************** "
tcat_gen_util catsrv=&server catdb=&db catusr=&user catpw=&pass +
              tests=&tests tables=&tables

!	
!
write "  **************************************************************"
write "  **************************************************************"
write "  ***  REMOVE TEST DATA FROM CATALOG *** "
write "  **************************************************************"
write "  **************************************************************"
run_isql user=&user  pass=&pass server=&server +
	sy_filename=tstcat_gen_util.unload

end-proc
.title tstcat_gen_util
.help
This unit test does not require any source files or output disk space
(except for its output log file.)

The subroutines in this module are portable and should be tested on all
available platforms.

Author: Megan O'Shaughnessy
Date:   6/13/94

.vari user
Sybase username
.vari pass
Sybase password
.vari server
Sybase server
.vari db
Sybase database
.vari tests
Developer debugging switch.
Default = both
.vari tables
Developer debugging switch.
Default = all
.end

$!-----------------------------------------------------------------------------
$ create tstcat_gen_util.load
insert into ssioverview (sclkstrtcnt,sclkpartition,ictseqnum,obsid,targname,
imagetime,predtelemfmtid,predframedur,predexposdur,predfiltname,predgainmodeid,
onchipmosaic,prtsclkstrtcnt,opnavflag,timeclsapr,sattimeclsapr,targctrdist,
ctrbdydist,subscrange,subsclat,subsclon,minlat,minlon,angsemidiam,centerlat,
resolution,smearmag,localhourang,incidang,emissang,phsang,sspiceid,espiceid,
mergedone,sysdone,galsosdone,redrfilldone,note,sclkstopcnt) 
values (11111,222,1,"obsid","targname","1995-JAN-02","HIM",3,5.6,"CLEAR",
"400K",2,11110,1,345,678,1.2,2.3,3.4,4.5,6.7,3.3,4.4,8.8,9.9,1.1,6.6,5.5,2.3,
3.3,6.6,"SOV","AB",1,1,1,1,"testing getssiover",11112)
go
insert into ssiict (sclkstrtcnt,sclkpartition,ictseqnum,quantstepsize,
zigzagtablename,startcutoutwinln,starttruthwinln,predert,despikethresh,
encodingtype)
values (12345,123,1,2,"ZIGZAG",3,4,"1995-JAN-03",5,"ICT")
go
insert into ssiopnav (sclkstrtcnt,sclkpartition,starareas,stararealines3,
starareasamp5)
values (12345,123,1,3,5)
go
$!-----------------------------------------------------------------------------
$ create tstcat_gen_util.unload
delete from ssiraw where sclkstrtcnt=13131313 and sclkpartition=1
go
delete from ssioverview where sclkstrtcnt=11111 and sclkpartition=222
go
delete from ssiict where sclkstrtcnt=12345 and sclkpartition=123
go
delete from ssiopnav where sclkstrtcnt=12345 and sclkpartition=123
go

$ Return
$!#############################################################################
$Other_File:
$ create cat_gen_util.hlp
1 cat_gen_util

  This module contains a set of subroutines which are of generic use in
  interfacing between VICAR programs and the SYBASE data catalog.  These
  routines are described in brief detail below. Calling sequences are listed
  in a help sub-menu.

** auxilliary catalog routines ***********************************************

These are the routines which a user/programmer will most likely wish to use
in their code.

 zcatGetUserData() (or something like it) is absolutely necessary
 in a calling program because the server username, password, etc must be 
 passed in to the main catalog routines.
 
 The zcatGetMsg() routine is handy for translating a status into a descriptive
 text message. (There is also a zcatGetSybaseMsg() routine, but as the 
 interface routines never pass back any Sybase statuses, it is considered
 an internal routine.

 zsplitFilespec() and zcombineFilespec() are almost exactly analogous to the 
 DATATRIEVE auxilliary routines split_filename() and combine_filename(). 
 The difference is that these routines can translate a Unix-style pathname
 (e.g. /path/filename.ext), whereas the DTR ones translate VAX filespecs
 (e.g. [disk]:[dir]filename.ext;version).

 cat_strcpy_toupper generates the uppercase, alphabetic "producttype" field
 from the file extension.  This copies the alphabetic part of a string, 
 converting to uppercase, stopping when a non-alphabetic character is
 encountered, and adding a null terminator.

  C routine 	        FORTRAN bridge       Description
  -------------------------------------------------------------------
  zcatGetUserData      xcatgetuserdata       Gets username, password, server
                                             and database.

  zcatGetMsg           xcatgetmsg            Translate catalog error status
                                             into a text message.

  zsplitFilespec	xsplitfilespec	     Splits filespec into its 
 					     component parts.

  zcombineFilespec	xcombinefilespec     Assembles filespec from its
				             component parts.

  cat_strcpy_toupper                         Generates the "producttype" from
                                             the file extension by converting
                                             the leading alphabetic characters
                                             to uppercase.

** Catalog interface routines ***********************************************

All five of these routines must be called, in order, to successfully interface
with the catalog.

  C routine 	                             Description
  -------------------------------------------------------------------
  catLogin				     initializes the message handler,
					     query descriptor and 
					     performs the log on.
  catLogout				     frees descriptors and
					     performs the log off.
  catFillQiDescCmd			     sets the query descriptor command
					     pointer.
  catExecuteCmd                              Execute the stored proc.

** internal subroutines ******************************************************

(No detailed help will be given for these; for further information, see the 
 code.)

  C routine 	       FORTRAN bridge        Description
  -------------------------------------------------------------------
  zcatGetSybaseMsg     xcatgetsybasemsg      Translate sybase error status
                                             into a text message.
  catcomma                                   Inserts number into command 
                                             buffer.
  catquote                                   Inserts string into command 
                                             buffer.    
  cat_test_null_int                          converts nullint to Sybase NULL
  cat_test_null_short                        converts nullshort to Sybase NULL
  cat_test_null_float                        converts nullfloat to Sybase NULL
  cat_test_null_char                         converts nullchar to Sybase NULL
  catSetNulls                                Sets translations for Sybase NULL.
  catGetRets                                 Get stored procedure returned
                                             values; print option.
  checkSlashes                               Adds slashes to path if needed.
  checkDots                                  Removes '.'s from file or ext.

2 calling sequences
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
C Calling sequences
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

  zcatGetUserData()
  --------------------------------------------------------------------------
  Parameters:
  -----------
  int zcatGetUserData(mipsServer,mipsDb,mipsUsr,mipsPw)
  char *mipsServer,
       *mipsDb,
       *mipsUsr,
       *mipsPw;

  Example:
  --------
  #include "cat_gen_util.h"
  int status;
  char server[MDMS_NAME_LEN], 
       db[MDMS_NAME_LEN], 
       usr[MDMS_NAME_LEN], 
       passwd[MDMS_NAME_LEN];

  status = zcatGetUserData(server,db,usr,passwd);
	where:
                status = integer status returned (output)
		server = Sybase server (output)
		db     = Sybase database (output)
		usr    = Sybase username (output)
                passwd = Sybase password (output)
	Notes:  
 	*********************************************************************
        ** This subroutine should be called before any other Sybase DB     **
        ** interface routines are called!!                                 **
        *********************************************************************
        * Use of this subroutine requires that the calling program have the 
          following parameters in its PDF file, or be in tier with a PDF which 
          does have them:
		parm catsrv (string,16) default = "MIPSDB1"
        	parm catdb  (string,16) default = "opCat" 
        	parm catusr (string,31)
        	parm catpw  (string,31)  
        * The only time status would not be "1" (success) is if there is a 
          problem in calling zvp. (This might happen if you forget to include 
         the parameters listed above!)

  zcatGetMsg()
  --------------------------------------------------------------------------
  Parameters:
  -----------
  char *zcatGetMsg(err)
  int err;   

  Example:
  --------
  #include "cat_gen_util.h"
  int status;
  char buf[80];

  strcpy(buf,zcatGetMsg(status));
  	Where: 
		status = an error status to be translated (input)
                buf    = text buffer for the error message produced (output)
        Notes:
        * There are a number of ways to call this routine; perhaps the 
          easiest is:
                         zvmessage(zcatGetMsg(status),0);

  zsplitFilespec()
  --------------------------------------------------------------------------
  Parameters:
  -----------
  int zsplitFilespec(filespec,path,afile,ext)
  char *filespec, 
       *path,     
       *afile,    
       *ext;      

  Example:
  --------
  #include "cat_gen_util.h"
  char filespec [MIPS_FILESPEC_MAX_LEN],
       path [MIPS_FILEPATH_MAX_LEN],
       file [MIPS_FILENAME_MAX_LEN],
       ext  [MIPS_FILETYPE_MAX_LEN];
  int status;

  memset(path,0,strlen(path));
  memset(file,0,strlen(file));
  memset(ext,0,strlen(ext));

  status = zsplitFilespec(filespec,path,file,ext);
  	where:
		filespec = file specification        (input)
                path     = pathname                  (output)                
                file     = filename prefix           (output)
                ext      = filename suffix/extension (output)
                status   = returned status           (output)
        Notes:
        * filespec should be in the Unix format: "/path/filename.extension"
          If no "/"'s are found in the filespec, the routine will abend.
	* all output parameters should be passed in as null strings.
        * path is in the format "/path/"
        * file is in the format "filename"  It will not contain a "."
        * ext is in the format "extension"  It will not contain a "."

  zcombineFilespec()
  --------------------------------------------------------------------------
  Parameters:
  -----------
  int zcombineFilespec(path,afile,ext,filespec)
  char *path,     
       *afile,    
       *ext,      
       *filespec; 

  Example:
  --------
  #include "cat_gen_util.h"
  int status;
  char filespec [MIPS_FILESPEC_MAX_LEN],
       path [MIPS_FILEPATH_MAX_LEN],
       file [MIPS_FILENAME_MAX_LEN],
       ext  [MIPS_FILETYPE_MAX_LEN];

  status = zcombineFilespec(path,file,ext,filespec);
  	where:
                path     = pathname                  (input)                
                file     = filename prefix           (input)
                ext      = filename suffix/extension (input)
		filespec = file specification        (output)
                status   = returned status           (output)
        Notes:
        * path is expected to be in the format "/path/".  HOWEVER, if it is
          missing a leading and/or trailing "/", the routine is smart enough
          to realize it, add the appropriate "/"(es), issue a message to
          that effect, and continue.
        * file is expected to be in the format "filename".  It should not 
          contain a trailing "." (or any internal ones, either.)  However, if
          the routine encounters a "." it will remove it, issue a message to 
          that effect, and continue.
        * ext is in the format "extension".  It should not contain a leading 
          "." (or any internal ones, either.)  However, if the routine 
          encounters a "." it will remove it, issue a message to that effect, 
          and continue.
        * filespec will be in the Unix format: "/path/filename.extension"

  cat_strcpy_toupper()
  --------------------------------------------------------------------------
  Parameters:
  -----------
  void cat_strcpy_toupper(char *tostr, char *fromstr)

  Example:
  --------
  #include "cat_gen_util.h"
  char product_type [MIPS_FILETYPE_MAX_LEN],
       ext  [MIPS_FILETYPE_MAX_LEN];

  cat_strcpy_toupper( product_type, ext);
  	where:
                ext      = filename suffix/extension (input)
		product_type = alphabetic portion of ext in uppercase. (output)
        Notes:
        * ext is in the format "extension".  It should not contain a leading 
          "." (or any internal ones, either.)  
          It is assumed that the extension contains "alphabetic characters
          followed by numbers". (See for example the Galileo SSI Catalog SIS
          description for the "filename" field in the ssi_corrected table.)
        * product_type is null terminated and formatted to meet the
          producttype field specification for a number of Galileo Image Catalog
          tables.

  catLogin()
  --------------------------------------------------------------------------

    This is the first routine you should call when you are going to interface
    with the Sybase catalog.  catLogin initializes Sybase message handlers 
    and the query descriptor and it performs the log in. 

  Parameters:
  -----------
  int catLogin(userInfo)
  cat_user_struct_typ *userInfo;

  Example:
  --------
  See example below.

	Notes:
        *

  catFillQiDescCmd()
  --------------------------------------------------------------------------
  Parameters:
  -----------
  int catFillQiDescCmd(catFillCmdFnPtr,catDataQueryStruct)
  void (*catFillCmdFnPtr)();  
  void *catDataQueryStruct;   

  Example:
  --------
  See example below.

	Notes:
        *
        
  catExecuteCmd()
  --------------------------------------------------------------------------
  Parameters:
  -----------
  int catExecuteCmd(      catGetRowsFnPtr,catDataStruct,
                          rec_length,nrows,maxrows,
                          printflag,catPrintoutFnPtr,
                          catRetvalsDataStruct,catRetvalsFnPtr)
  int (*catGetRowsFnPtr)();  
  char *catDataStruct;     
  int rec_length;
  int *nrows;                
  int maxrows;               
  int printflag;             			     
  void (*catPrintoutFnPtr)(); 
  void *catRetvalsDataStruct; 
  void (*catRetvalsFnPtr)();  
 
  Example:
  --------
  See example below.

  catLogout()
  --------------------------------------------------------------------------
    This is the last routine you should call when you are done interfacing 
    with the Sybase catalog.  catLogout terminates the catalog connection 
    and frees all descriptors.  

  Parameters:
  -----------
  void catLogout(void)

  Example:
  --------
  #include "cat_gen_util.h"

  catLogout();

  			Notes:
                        * This routine performs an mdms_qiExit().

  ----------------------------------------------------------------------------
  Example of catLogin, catFillQiDescCmd, catExecuteCmd, and 
  catLogout
  ----------------------------------------------------------------------------
  #include "vicmain_c"
  #include "cat_gen_util.h"
  #include "gll_ssi_rts_cat.h" /* this file actually itself refers to 
                                  cat_gen_util.h, but for clarity they
                                  are listed seperately here. */
  main44() {
  cat_user_struct_typ *userInfo;
  int nrows,status,rec_length;
  int status = CAT_SUCCESS;
  cat_rtsIct_struct_typ ictStruct[MAX_ICT_ROWS]; 
  cat_rtsIct_struct_query_typ *ictQueryStruct;
  /* 
    *cat_fill_rtsIct, *cat_getrows_rtsIct, and *cat_printout_rtsIct
    are all pointers to functions which are specific to the stored procedure 
    being called. These functions are within the module gll_ssi_rts_cat.h.

    The structure cat_dummy_struct and the function pointer *cat_dummyFn
    are "dummy" routines. Normally they would be a returned values 
    structure and a function pointer for a returned values routine, but
    in this specific case, the stored procedure doesn't have any output
    values, so the dummy struct and routine just hold the place.
  */

  zcatGetUserData(userInfo.server,userInfo.db,userInfo.user,userInfo.passwd);
  strcpy(userInfo.progname,"tstcat_gen_util");
  userInfo.printflag = 1;  /* print out all the results structures */

  /* Initialize descriptors for the program and login to Sybase. */
  status = catLogin(&userInfo);
  if (status != CAT_SUCCESS) return(status);

  status = catFillQiDescCmd(*cat_fill_rtsIct,
                            ictQueryStruct);                   
  if (status != CAT_SUCCESS) return(status);

  rec_length = sizeof(cat_rtsIct_struct_typ);
  status = catExecuteCmd(*cat_getrows_rtsIct,
                       (void *) ictStruct,
		       rec_length,
                       &nrows,
                       MAX_ICT_ROWS,
                       userInfo.printflag,
                       *cat_printout_rtsIct,
                       &cat_dummy_struct,
                       *cat_dummyFn); 
  if (status != CAT_SUCCESS) return(status);

  catLogout();
  return(CAT_SUCCESS);
  }
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
FORTRAN Calling sequences
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

      The routines listed below are the FORTRAN bridges to the C routines
      above.

      character*16  server,db
      character*21  user,passwd
      character*255 filespec,path
      character*120 filename
      character*10  ext
      character*80  buf
      integer*4 status

      xsplitfilespec()
      ----------------
      call xsplitfilespec(filespec,path,filename,ext)

      * See C Calling sequence for description.


      xcombinefilespec()
      ------------------
      call xcombinefilespec(path,file,ext,filespec)

      * See C Calling sequence for description.

      xcatgetusername()
      --------------------
      call xcatgetuserdata(server,db,user,passwd)

      * See C Calling sequence for description.
      * Unlike the C version, this routine does not return a status variable.
        However, if the C version (which it calls) receives a failing status,
        the routine will abend.

     xcatgetmsg()
     ------------
     call xcatgetmsg(status,buf)

      * See C Calling sequence for description.

2 links and includes
  
  The following include file is required:
  
     #ifndef CAT_GEN_UTIL_H
     #include "cat_gen_util.h"
     #endif

  (The pre-processor if-statement will prevent link errors arising from
  multiply defined global symbols.)

  You must have the following lines in your imake file in order for your code
  to compile with these routines:

     #define LIB_SYBASE
     #define LIB_MDMS
     #define LIB_KERBEROS

2 operation

   This example of a total calling sequence for routines uses the
   ssiOverview table.  Unimportant parameters, declarations, and error 
   handling are left out.  This example is subject to change without notice
   and is meant to be only a guide to how all of these routines work together.

   Key: 
     *x            -- Function pointer as passed from routine to routine
     (*x)(a,b,c)   -- Specific instance of generic function pointer
     (( action )); -- required action not supported by catalog code
     /* stuff */   -- comment or abbreviation
     indentation   -- next lower subroutine level

   main44()
     int status = CAT_SUCCESS;
     cat_user_struct_typ           userInfo;
     cat_rtsOver_struct_query_typ  overQueryStruct;
     cat_rtsOver_struct_typ        overStruct[MAX_OVER_ROWS];
     
     (( Fill the userInfo struct; could use zcatGetUserData for
        some elements ));

     catLogin(&userInfo); /* Login and initialize global descriptors */

     (( Fill the overQueryStruct ));

     catSsiRtsOver(userInfo.printflag,&overQueryStruct,&overStruct);
        int nrows,printflag;
        catFillQiDescCmd(*cat_fill_rtsOver,
                         overQueryStruct);
          static char tempCmd[1024];

          /* With query struct and fill function, assemble the appropriate  */
          /* SQL command.                                                   */
          (*cat_fill_rtsOver)(overQueryStruct,&tempCmd[0]);

          /* Fill query descriptor with tempCmd.                            */

        catExecuteCmd(*cat_getrows_rtsOver,(void *) overStruct,&nrows,
			MAX_OVER_ROWS,printflag,*cat_printout_rtsOver,
			&cat_dummy_struct,*cat_dummyFn);
          int rows_returned = 0;
          int status;
	  /*
		initialize temporary buffer
	  */

	  temp = (char *) malloc(rec_length);

	  do {

	  status = (*catGetRowsFnPtr) (qiDesc,
	                                  temp);
		/* load the query descriptor with information about */
		/* the result set, with mdms_qiTblDesc(qiDesc)*/
            	if login okay,
               		catSetNulls(qiDesc); 
              		/* Bind all the variables in overStruct to the qiDesc.*/
            	else if end of query, return;
            	else if end of transaction, return;

	    	/* Get the row and copy it into the bound overStruct, with  */
            	/* routine mdms_qiNextRow(qiDesc).                          */
            	/* If status okay, convert any encoded strings, and any IEEE*/
            	/*   floats.                                                */
            	return;

	  if(status == MDMS_ROWRETURNED)
	  {
		memcpy(catDataStruct + n*rec_length,temp,rec_length);
		rows_returned++;
		n++;
	  }
          if (printflag) (*cat_printout_rtsOver)(overStruct[rows_returned-1]);

          } while ((status == MDMS_ROWRETURNED) && (rows_returned < maxrows));

          /* Overview doesn't have any returned values, but this next call
          included here for completeness */
          if(status == MDMS_ENDOFQUERY)
	  {
	    catGetRets(*retvalsFnPtr,printflag,retvalsStruct);
        	  	/* determine the number of returned values            */
          	  	/* if number > 1, buffer them into the data structure */
		  	return;
          }

          /* flush out transaction */
        
          return;
             
      catLogout();

     /* Main program uses the data in overStruct.*/
      
     return;
2 history

	Original Programmer: Megan O'Shaughnessy, 15 June 1994
        Source Language: C
        
        Current Programmer: Thuy Truong, 7 October 1994
        Revisions:

	15 June 1994	MOS	Original
         6 July 1994    MOS     Modified catSetNulls to handle null floating
                                point value in reversed IEEE format for the 
                                Alpha. 
                                Rearranged CatExecuteCmd() and changed its
                                error & status handling.
                                Added "operation" section to help file.
                                Added "#define LIB_MDMS" to imake files.
        24 August 1994  MOS     Converted to ANSI C, updated imake file.
 				Added new subroutine catInitMessageDesc().
				Made msgDesc a static global variable.
				Corrected small problems in catFree, return
                                  statuses, and changed one error message
                                  for use by GLLTELEMPROC.
	07 Oct. 1994    TLT     Removed subroutines: catInitMessageDesc,
				  catTerminate, & catFree.
				Added subroutines: catLogin & catLogout.
				Replaced catInitQueryDesc with catFillQiDescCmd
				Made query descriptor global, thus, calling 
				  sequences of catExecuteCmd,catGetRets, & 
				  catSetNulls were revised.
				Removed cat_null_datetime_p from catSetNulls.
				Revised function prototype of
				  catGetRets and catFillQiDescCmd
				Revised catExecuteCmd to use local buffer.
	10 Nov. 1994	TLT	Revised catQuote to handle NULL condition.
				FR 85810.
	21 Nov. 1994	TLT	Removed memsets from zsplitFilespec.
				Revised NULL to double precision data types 
				in catSetNulls.
				Replace MDMS_CHARBIND with MDMS_NTBSTRINGBIND
				(FR 85851).
				Revised function definition of catExecuteCmd
					from void *catdataStruct to char *.
					(FR 85853).
	07 Feb. 1995	TLT	Added "#define LIB_KERBEROS" to unit test
				    imake file.
				Moved the assignment of nrows outside of
				    printflag if-block.
	14 Mar. 1995    TLT     Added status message for CAT_NOT_FOUND and
				    CAT_NOT_AFFECTED in catgetmsg.
				Added script files in test procedure.
	11 Apr. 1995	TLT	Fixed bug in zsplitFilespec by adding null
				terminators to output character arrays.
				(FR 87210).
	18 Dec. 1995	TLT	Fixed bug in cat_test_null_int,
					     cat_test_null_short, and
					     cat_test_null_float by adding
				null terminators to the output character
				buffer.
				(FR 88225).
				Renamed sclkstrtcnt to searchSclk and
				orbnum to orbnumchar in the unit test to
				reflect changes in the sybase stored procs.
	14 Apr. 1997	TLT	Added capability to execute SQL commands.
				Updated test program.

	18 Apr. 1997	TLT	Ported to HP.
 	08 May  1997	TLT	Added function pointer defines to remove 
				duplicate symbol warnings on SGI and HP (DFR).
        06 June 1997    SP      Corrected overindexing problem is zsplitFilename
                                and added new routine cat_strcpy_toupper for
                                generating producttype values.
$ Return
$!#############################################################################
