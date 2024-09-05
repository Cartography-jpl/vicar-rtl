/************************************************************************/
/*   N  O  T  E								*/
/* Maintain this file in parallel with util/v2param_standalone.com.	*/
/* See that file for further comments and explanation.			*/
/************************************************************************/

/************************************************************************/
/* v2param - A program to facilitate access to parameter files written	*/
/* by shell-VICAR output routines (q_out, xqout).  This program is	*/
/* typically run in backquotes in some kind of variable setting routine,*/
/* for example:								*/
/* $R2LIB/minmax ...							*/
/* setenv MIN "`v2param minival`"					*/
/*									*/
/* See below for usage notes.						*/
/************************************************************************/

#include "xvmaininc.h"
#include "zvproto.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "strcasecmp.h"

char *v2param_get_file();
char *v2param_find_entry(char*, char*);
int v2param_count_elements(char*, char*);
char *v2param_get_one_value(char**, char*);
void v2param_remove_file(char*);

int main(int argc, char *argv[])
{
   int help_flag = 0;
   int name_flag = 0;
   int remove_flag = 0;
   int index_flag = 0;
   int index;
   int count_flag = 0;
   int test_flag = 0;
   int verbatim_flag = 0;

   char *param_name = NULL;
   char *param_value;
   int element_count;
   char *p, *element;

   int i;
   char *filename;

   /* Process command-line options.  Any unrecognized options are	*/
   /* assumed to be the parameter name.					*/

   for (i=1; i<argc; i++) {
      if (strcasecmp(argv[i], "-h") == 0 || strcasecmp(argv[i], "-help") == 0)
         help_flag++;
      else if (strcasecmp(argv[i], "-file") == 0)
         name_flag++;
      else if (strcasecmp(argv[i], "-rm") == 0)
         remove_flag++;
      else if (strcasecmp(argv[i], "-i") == 0) {
         index_flag++;
         if (i >= argc-1)
            help_flag++;		/* usage error */
         else {
            i++;			/* get count argument */
            index = atoi(argv[i]);
         }
      }
      else if (strcasecmp(argv[i], "-n") == 0)
         count_flag++;
      else if (strcasecmp(argv[i], "-test") == 0)
         test_flag++;
      else if (strcasecmp(argv[i], "-v") == 0)
         verbatim_flag++;
      else
         param_name = argv[i];
   }

   /* Check for help, bail out after printing message (to stderr) */

   if (help_flag) {
      fprintf(stderr, "Usage:\n\
v2param -h              Print help message (or -help)\n\
v2param -file           Report name of param file\n\
v2param -rm             Remove param file (may be combined with other options)\n\
v2param param           Report value of given param, (separate lines if needed)\n\
v2param -i 3 param      Report 4th element of given param (0 based)\n\
v2param -n param        Report number of elements in given param\n\
v2param -test param     Report 1 if param exists or 0 if not\n\
v2param -v param        Report value of given param verbatim from the file\n\
                        TBD: quotes on strings?\n" );

      exit(1);
   }

   /* Get name of parameter file to use. */

   filename = v2param_get_file();

   if (filename == NULL) {
      fprintf(stderr, "Unable to find param file name.\n");
      exit(1);
   }

   if (name_flag) {
      printf("%s\n", filename);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* If param is needed but not given, print an error */

   if (param_name == NULL) {
      if (remove_flag) {		/* remove by itself is okay */
         v2param_remove_file(filename);
         exit(0);
      }
      fprintf(stderr, "Parameter name required.\n");
      exit(1);
   }

   /* Find parameter entry */

   param_value = v2param_find_entry(filename, param_name);

   if (param_value == NULL) {
      if (test_flag) {
         printf("0\n");
         if (remove_flag)
            v2param_remove_file(filename);
         exit(0);
      }
      else
         fprintf(stderr, "Parameter '%s' not found.\n", param_name);
      exit(1);
   }
   if (test_flag) {
      printf("1\n");
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Count number of elements */

   if (count_flag) {
      element_count = v2param_count_elements(param_value, param_name);
      printf("%d\n", element_count);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Print verbatim mode */

   if (verbatim_flag) {
      printf("%s\n", param_value);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Check index */

   if (index_flag && index < 0) {
      fprintf(stderr, "Invalid index: %d\n", index);
      exit(1);
   }

   /* Print value(s) for scalar entry */

   if (*param_value != '(') {
      if (index_flag && index != 0) {
         fprintf(stderr, "Index %d is past end of parameter %s\n",
					index, param_name);
         exit(1);
      }
      p = param_value;
      element = v2param_get_one_value(&p, param_name);
      while (isspace(*p)) p++;
      if (*p != '\0')
         fprintf(stderr, "Syntax error on param %s: extra data at end\n",
						param_name);
      printf("%s\n", element);
      if (remove_flag)
         v2param_remove_file(filename);
      exit(0);
   }

   /* Print value(s) for multivalued entry.  Different elements are	*/
   /* printed on different lines, although this will look the same as	*/
   /* embedded spaces to the backquote (`) command.			*/

   p = param_value+1;		/* skip the paren */
   element_count = 0;

   do {
      while (isspace(*p)) p++;
      element = v2param_get_one_value(&p, param_name);

      /* Print the value if we're not indexing, or if the index matches */

      if (element && !index_flag || index == element_count)
         printf("%s\n", element);
      if (element)
         element_count++;

      while (isspace(*p)) p++;
      if (*p != ',' && *p != ')') {
         fprintf(stderr, "Syntax error on param %s: missing , or )\n",
						param_name);
         break;
      }
      if (*p == ',') p++;		/* skip comma */
   } while (*p != ')' && *p != '\0');
   if (*p != ')') {
      fprintf(stderr, "Syntax error on param %s: missing )\n", param_name);
   }
   p++;
   while (isspace(*p)) *p++;
   if (*p != '\0') {
      fprintf(stderr, "Syntax error on param %s: extra data at end\n",
						param_name);
   }
   if (index_flag && index >= element_count)
      fprintf(stderr, "Index %d is past end of parameter %s\n",
					index, param_name);

   if (remove_flag)
      v2param_remove_file(filename);
   exit(0);

}

