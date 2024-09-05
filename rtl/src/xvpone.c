#include "xvmaininc.h"
#if RTL_USE_TAE
#include "taeconf.inp"
#include "symtab.inc"
#endif
#include "ftnbridge.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine gets a single parameter item from a multi-	*/
/* valued parameter.  Returns SUCCESS if found, status code if	*/
/* not (in addition to calling error_handler).			*/

#if RTL_USE_TAE

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int F77_FUNC(xvpone, XVPONE)
(char *name, char *value, int *instance, int *maxlen,
						ZFORSTR_PARAM)
/* char *name;		In: Name of parameter to get */
/* char *value;		Out: returned value (may be int,real,or string) */
/* int *instance;	In: which item to get (starts at 1) */
/* int *maxlen;		In: max length of string to get */
{
   ZFORSTR_BLOCK
   char c_name[2*NAMESIZE+2];	/* *1 for name, *2 for qual, +1 for '.' */
   struct VARIABLE *v;

   current_call = VPONE;

   zsfor2c(c_name, 2*NAMESIZE+1, name, &name, 4, 1, 1, maxlen);

   v = p_fvar((struct PARBLK*) &parb, c_name); /* p_fvar() ignores case */
   if (v == NULL) {
      error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   if (*instance < 1 || *instance > v->v_count) {
      error_handler(NO_UNIT, BAD_PARAM_INSTANCE);
      return BAD_PARAM_INSTANCE;
   }

   switch (v->v_type) {
      case V_REAL:
         *(float *)value = RVAL(*v, *instance-1);
         break;
      case V_INTEGER:
         *(int *)value = IVAL(*v, *instance-1);
         break;
      case V_STRING:
         zsc2for(SVAL(*v, *instance-1), *maxlen, value, &name, 4, 2, 2, maxlen);
         break;
      default:
         error_handler(NO_UNIT, PARBLK_ERROR);
         return PARBLK_ERROR;
   }

   return SUCCESS;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvpone(name, value, instance, maxlen)
char *name;		/* In: Name of parameter to get */
void *value;		/* Out: returned value (may be int,real,or string) */
int instance;		/* In: which item to get (starts at 1) */
int maxlen;		/* In: max length of string to get */
{
   struct VARIABLE *v;

   current_call = VPONE;

   v = p_fvar((struct PARBLK*) &parb, name); /* p_fvar() ignores case */
   if (v == NULL) {
      error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   if (instance < 1 || instance > v->v_count) {
      error_handler(NO_UNIT, BAD_PARAM_INSTANCE);
      return BAD_PARAM_INSTANCE;
   }

   switch (v->v_type) {
      case V_REAL:
         *(float *)value = RVAL(*v, instance-1);
         break;
      case V_INTEGER:
         *(int *)value = IVAL(*v, instance-1);
         break;
      case V_STRING:
         if (maxlen > 0) {
            strncpy(value, SVAL(*v, instance-1), maxlen);
            *(((char*)value)+maxlen-1) = '\0';
         }
         else
            strcpy(value, SVAL(*v, instance-1));
         break;
      default:
         error_handler(NO_UNIT, PARBLK_ERROR);
         return PARBLK_ERROR;
   }

   return SUCCESS;
}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int F77_FUNC(xvipone, XVIPONE)
(char *name, char *value, int *instance, int *maxlen,
								ZFORSTR_PARAM)
/* char *name;		In: Name of parameter to get */
/* char *value;		Out: returned value (may be int,real,or string) */
/* int *instance;	In: which item to get (starts at 1) */
/* int *maxlen;		In: max length of string to get */
{
   ZFORSTR_BLOCK
   char c_name[2*NAMESIZE+2];	/* *1 for name, *2 for qual, +1 for '.' */
   struct VARIABLE *v;

   current_call = VPONE;

   zsfor2c(c_name, 2*NAMESIZE+1, name, &name, 4, 1, 1, maxlen);

   v = p_fvar((struct PARBLK*) &iparb, c_name); /* p_fvar() ignores case */
   if (v == NULL) {
      error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   if (*instance < 1 || *instance > v->v_count) {
      error_handler(NO_UNIT, BAD_PARAM_INSTANCE);
      return BAD_PARAM_INSTANCE;
   }

   switch (v->v_type) {
      case V_REAL:
         *(float *)value = RVAL(*v, *instance-1);
         break;
      case V_INTEGER:
         *(int *)value = IVAL(*v, *instance-1);
         break;
      case V_STRING:
         zsc2for(SVAL(*v, *instance-1), *maxlen, value, &name, 4, 2, 2, maxlen);
         break;
      default:
         error_handler(NO_UNIT, PARBLK_ERROR);
         return PARBLK_ERROR;
   }

   return SUCCESS;
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvipone(name, value, instance, maxlen)
char *name;		/* In: Name of parameter to get */
void *value;		/* Out: returned value (may be int,real,or string) */
int instance;		/* In: which item to get (starts at 1) */
int maxlen;		/* In: max length of string to get */
{
   struct VARIABLE *v;

   current_call = VPONE;

   v = p_fvar((struct PARBLK*) &iparb, name); /* p_fvar() ignores case */
   if (v == NULL) {
      error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   if (instance < 1 || instance > v->v_count) {
      error_handler(NO_UNIT, BAD_PARAM_INSTANCE);
      return BAD_PARAM_INSTANCE;
   }

   switch (v->v_type) {
      case V_REAL:
         *(float *)value = RVAL(*v, instance-1);
         break;
      case V_INTEGER:
         *(int *)value = IVAL(*v, instance-1);
         break;
      case V_STRING:
         if (maxlen > 0) {
            strncpy(value, SVAL(*v, instance-1), maxlen);
            *(((char*)value)+maxlen-1) = '\0';
         }
         else
            strcpy(value, SVAL(*v, instance-1));
         break;
      default:
         error_handler(NO_UNIT, PARBLK_ERROR);
         return PARBLK_ERROR;
   }

   return SUCCESS;
}

#else

int XVPONE()
{
   current_call = VPONE;
   error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

int zvpone(char *UNUSED(name),
	   void *UNUSED(value),
	   int UNUSED(instance),
	   int UNUSED(maxlen))

{
   current_call = VPONE;
   error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

int XVIPONE()
{
   current_call = VIPONE;
   error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

int zvipone(char *UNUSED(name),
	    void *UNUSED(value),
	    int UNUSED(instance),
	    int UNUSED(maxlen))
{
   current_call = VIPONE;
   error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

#endif

