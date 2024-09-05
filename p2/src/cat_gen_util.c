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
ROUTINE void F77_FUNC(xcatgetmsg, XCATGETMSG) 
(int err, char *buf, ZFORSTR_PARAM)
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
ROUTINE void F77_FUNC(xcatgetsybasemsg, XCATGETSYBASEMSG) 
(int err, char *buf, ZFORSTR_PARAM)
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
ROUTINE void F77_FUNC(xcatgetuserdata, XCATGETUSERDATA) 
(char *server, char *db, char *usr,
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
ROUTINE void F77_FUNC(xsplitfilespec, XSPLITFILESPEC) 
(char *filespec, char *path, char *afile,
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
ROUTINE void F77_FUNC(xcombinefilespec, XCOMBINEFILESPEC) 
(char * path, char *afile, char *ext,
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
