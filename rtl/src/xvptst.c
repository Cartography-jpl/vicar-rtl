#include "xvmaininc.h"
#include "ftnbridge.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Returns TRUE if a keyword was specified and FALSE if not.	*/
/* The keyword must be a value specified for a parameter of	*/
/* type KEYWORD.  *vptst tests the initial parameter parblk;	*/
/* *viptst tests the interactive parblk.			*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int F77_FUNC(xvptst, XVPTST)
(char *key, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_key[MAX_STRING_SIZE+1];
   size_t i;

   zsfor2c(c_key, MAX_STRING_SIZE, key, &key, 1, 1, 1, key);

   for (i=0; i<strlen(c_key); i++) {	/* Can't be any blanks in string */
      if (c_key[i] == ' ') {
         c_key[i] = '\0';
         break;
      }
   }

   return test_keyword((struct PARBLK*) &parb, c_key);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvptst(key)
char *key;
{
   return test_keyword((struct PARBLK*) &parb, key);
}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

int F77_FUNC(xviptst, XVIPTST)
(char *key, ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char c_key[250];
   size_t i;

   zsfor2c(c_key, 249, key, &key, 1, 1, 1, key);

   for (i=0; i<strlen(c_key); i++) {	/* Can't be any blanks in string */
      if (c_key[i] == ' ') {
         c_key[i] = '\0';
         break;
      }
   }

   return test_keyword((struct PARBLK*) &iparb, c_key);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zviptst(key)
char *key;
{
   return test_keyword((struct PARBLK*) &iparb, key);
}
