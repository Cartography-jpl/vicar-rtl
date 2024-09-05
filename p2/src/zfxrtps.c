#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/XrtGraph.h>
/*#include <Xm/xrt_com.h>* -jaw - file doesn't exist in v2.1 */
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/ToggleB.h>
#include <Xm/PushBG.h>

/* zfxrtps.f & zcxrtps.c contains the FORTRAN and C interfaces necessary to
produce an Encapsulated PostScript File (EPSF) representation of a graph.


============================================================================ 
     10 July 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING) 
============================================================================ 


FORTRAN Bridge Interface:

============================================================================ */

void ZrtPushStack (char *);

static char  msg [132];
static int   thefont, xpos, ypos;
static float xoff, yoff, xscale, yscale, pennum, plotx, ploty, fact;

/* FORTRAN Interface to PostScript print functions: */

#define SizeOfPostScriptFile 512
#define SizeOfAxisString     512
#define EXIT 999
/* ======================================================================= */

/* header () is called to set the header titles information for the
output PostScript file. 

If the header strings are not provided, then headers will not be printed */

void F77_FUNC (header, HEADER) 
(char *for_string, int *nelements, int *adjustment,
							ZFORSTR_PARAM) 
#if 0
  char *for_string;     /* Pointer to FORTRAN strings */
  int  *nelements;      /* Number of lines in header */
  int  *adjustment;     /* Header placement */
#endif
{
ZFORSTR_BLOCK
XrtAdjust Xadjustment;
int    i;
char **ptr = NULL;
char  *c_string = NULL;
char   test [24];
int    maxlen = 0;
void  *arg1ptr = NULL;
int    nargs  = 3;
int    argno  = 1;
int    strno  = 1;
size_t howbig = 0;

   switch (*adjustment) {
      case 0:
            Xadjustment = XRT_ADJUST_LEFT;
            break;
      case 1:
            Xadjustment = XRT_ADJUST_CENTER;
            break;
      case 2:
            Xadjustment = XRT_ADJUST_RIGHT;
            break;
      default:
            Xadjustment = XRT_ADJUST_CENTER;
            break;
   }
   arg1ptr = (void *) &for_string;
   maxlen = 0;

   /* Convert a fortran string array into corresponding C string array */
   zsfor2c_array (&c_string, &maxlen, *nelements, for_string, arg1ptr, 
                 nargs, argno, strno, adjustment);

   if (c_string == NULL) {
      zvmessage ("Address of header string is NULL ... exiting","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack (c_string);
 
   /* Allocate space for list of character string pointers */
   howbig = (*nelements +1) * sizeof (char *);

   /* Allocate memory for array of pointers to C string arrays */
   ptr = (char **) malloc (howbig);
   if (ptr == NULL) {
      zvmessage("Header error ... unable to allocate memory for string conversion","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack ((char *)ptr);
 
   /* Build list of pointers to header strings */
   for (i = 0; i < *nelements; i++) {
      *(ptr + i) = (char *) (c_string + i * maxlen);
   }

   /* Insert NULL for NULL-terminated list of pointers */ 
   *(ptr + *nelements) = NULL;
 
   /* Call ZrtHeader to set output PostScript File's header strings */
   ZrtHeader (ptr, Xadjustment);

   return;
}

/* ======================================================================= */

/* footer () is called to set the footer information for the
output PostScript file. 

If the footer strigs are not provided, then footers will not be printed */

void F77_FUNC(footer, FOOTER) 
(char *for_string, int *nelements, int *adjustment,
							ZFORSTR_PARAM) 
#if 0
char *for_string;     /* Pointer to FORTRAN strings */
int  *nelements;      /* Number of lines in Footer */
int  *adjustment;     /* Footer placement */
#endif
{
ZFORSTR_BLOCK
XrtAdjust Xadjustment;
int    i;
char **ptr = NULL;
char  *c_string = NULL;
char   test [24];
int    maxlen = 0;
void  *arg1ptr = NULL;
int    nargs  = 3;
int    argno  = 1;
int    strno  = 1;
size_t howbig = 0;

   switch (*adjustment) {
      case 0:
            Xadjustment = XRT_ADJUST_LEFT;
            break;
      case 1:
            Xadjustment = XRT_ADJUST_CENTER;
            break;
      case 2:
            Xadjustment = XRT_ADJUST_RIGHT;
            break;
      default:
            Xadjustment = XRT_ADJUST_CENTER;
            break;
   }
   arg1ptr = (void *) &for_string;
   maxlen = 0;

   /* Convert a fortran string array into corresponding C string array */
   zsfor2c_array (&c_string, &maxlen, *nelements, for_string, arg1ptr, 
                 nargs, argno, strno, adjustment);

   if (c_string == NULL) {
      zvmessage ("Address of footer string is NULL ... exiting","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack (c_string);

   /* Allocate space for list of character string pointers */
   howbig = (*nelements +1) * sizeof (char *);

   /* Allocate memory for array of pointers to C string arrays */
   ptr = (char **) malloc (howbig);
   if (ptr == NULL) {
      zvmessage("Footer error ... unable to allocate memory for string conversion","");
      return;
   }
 
   /* Push allocated memory pointer onto stack */
   ZrtPushStack ((char *)ptr);

   /* Build list of pointers to header strings */
   for (i = 0; i < *nelements; i++) {
      *(ptr + i) = (char *) (c_string + i * maxlen);
   }

   /* Insert NULL for NULL-terminated list of pointers */ 
   *(ptr + *nelements) = NULL;
 
   /* Call ZrtHeader to set output PostScript File's header strings */
   ZrtFooter (ptr,Xadjustment);

   return;
}

/* ======================================================================= */

/* setlabel () is called to set the label information for the specified set 
of the output PostScript file. 

If the string is not provided, set label will default to blanks */

void F77_FUNC(setlabel, SETLABEL)
(char *for_string, int *nelements, int *orientation,
						int *anchor, ZFORSTR_PARAM) 
{
ZFORSTR_BLOCK
XrtAlign  Xorientation;  /* Align horizontal or vertical */
XrtAnchor Xanchor;       /* Aligh in one of eight locations*/
int    i, value;
char **ptr = NULL;
char  *c_string = NULL;
char   test [24];
int    maxlen = 0;
void  *arg1ptr = NULL;
int    nargs  = 4;
int    argno  = 1;
int    strno  = 1;
size_t howbig = 0;
/*
   Valid orientation enum values are:
         XRT_ALIGN_HORIZONTAL            XRT_ALIGN_VERTICAL

   Valid anchor enum values are:
         XRT_ANCHOR_NORTH                XRT_ANCHOR_SOUTH
         XRT_ANCHOR_EAST                 XRT_ANCHOR_WEST
         XRT_ANCHOR_NORTHWEST            XRT_ANCHOR_SOUTHWEST
         XRT_ANCHOR_NORTHEAST            XRT_ANCHOR_SOUTHEAST
*/
   value = *orientation;
   switch (value) {
      case 0:
             Xorientation = XRT_ALIGN_HORIZONTAL;
             break;
      case 1:
             Xorientation = XRT_ALIGN_VERTICAL;
             break;
      default:
             Xorientation = XRT_ALIGN_VERTICAL;
             break;
   }

   value = *anchor;
   switch (value) {
      case 0:
             Xanchor = XRT_ANCHOR_NORTH;
             break;
      case 1:
             Xanchor = XRT_ANCHOR_SOUTH;
             break;
      case 2:
             Xanchor = XRT_ANCHOR_EAST; 
             break;
      case 3:
             Xanchor = XRT_ANCHOR_WEST; 
             break;
      case 4:
             Xanchor = XRT_ANCHOR_NORTHWEST; 
             break;
      case 5:
             Xanchor = XRT_ANCHOR_NORTHEAST; 
             break;
      case 6:
             Xanchor = XRT_ANCHOR_SOUTHWEST; 
             break;
      case 7:
             Xanchor = XRT_ANCHOR_SOUTHEAST; 
             break;
      default:
             Xanchor = XRT_ANCHOR_EAST; 
             break;
   }


   arg1ptr = (void *) &for_string;
   maxlen = 0;

   /* Convert a fortran string array into corresponding C string array */
   zsfor2c_array (&c_string, &maxlen, *nelements, for_string, arg1ptr, 
                 nargs, argno, strno, anchor);

   if (c_string == NULL) {
      zvmessage ("Address of label string is NULL ... exiting","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack (c_string);

   /* Allocate space for list of character string pointers */
   howbig = (*nelements +1) * sizeof (char *);

   /* Allocate memory for array of pointers to C string arrays */
   ptr = (char **) malloc (howbig);
   if (ptr == NULL) {
      zvmessage("SetLabel error ... unable to allocate memory for string conversion","");
      return;
   }
 
   /* Push allocated memory pointer onto stack */
   ZrtPushStack ((char *)ptr);

   /* Build list of pointers to header strings */
   for (i = 0; i < *nelements; i++) {
      *(ptr + i) = (char *) (c_string + i * maxlen);
   }

   /* Insert NULL for NULL-terminated list of pointers */ 
   *(ptr + *nelements) = NULL;
 
   /* Call ZrtSetLabel to set output PostScript File's set label strings */
   ZrtSetLabel (ptr, Xorientation, Xanchor);

   return;
}
          
/* ======================================================================= */

/* newpen () Changes pen */

void F77_FUNC(newpen, NEWPEN) 
(pen)
int *pen;
{
   ZrtNewPen (*pen);
   return;
}

/* ======================================================================= */

/* NEWPPLT () Clears the plot */

void F77_FUNC(newplt, NEWPLT) 
(void)
{
   ZrtNewPlt ();
   return;
}

/* ======================================================================= */

/* LINE () Graphs the points in the arrays */

void F77_FUNC(line, LINE) 
(xarray, yarray, npts, inc, lintyp, inteq)
float *xarray[];
float *yarray[];
int   *npts, *inc, *lintyp, *inteq;
{
   ZrtLine (xarray, yarray, *npts, *inc, *lintyp, *inteq);
   return;
}

/* ======================================================================= */

/* number () Plots the number 'fpn' at position (x,y) with given height
and angle. ndec is a code telling how to format the number. */

void F77_FUNC(number, NUMBER) 
(x, y, height, fpn, angle, ndec)
  float *x;		/* Input */
  float *y;		/* Input */
  float *height;	/* Input */
  float *fpn;		/* Input */
  float *angle;		/* Input */
  int   *ndec;		/* Input */
{
  ZrtNumber (*x, *y, *height, *fpn, *angle, *ndec);
  return;
}

/* ======================================================================= */

/* symbolanchor () Anchors the 'text' at specified location with respect
to the location of symbol. The default is XRT_ANCHOR_BEST. */

void F77_FUNC(symbolanchor, SYMBOLANCHOR)
(anchor)
int *anchor;
/*   Valid anchor positions are:
         XRT_ANCHOR_NORTH                XRT_ANCHOR_SOUTH
         XRT_ANCHOR_EASTH                XRT_ANCHOR_WEST
         XRT_ANCHOR_NORTHWEST            XRT_ANCHOR_SOUTHWEST
         XRT_ANCHOR_NORTHEAST            XRT_ANCHOR_SOUTHEAST
         and
         XRT_ANCHOR_HOME                 XRT_ANCHOR_BEST                   
*/
{
int         value;
XrtAnchor   Xanchor;

   value = *anchor;
   switch (value) {
      case 0:
             Xanchor = XRT_ANCHOR_NORTH;
             break;
      case 1:
             Xanchor = XRT_ANCHOR_SOUTH;
             break;
      case 2:
             Xanchor = XRT_ANCHOR_EAST; 
             break;
      case 3:
             Xanchor = XRT_ANCHOR_WEST; 
             break;
      case 4:
             Xanchor = XRT_ANCHOR_NORTHWEST; 
             break;
      case 5:
             Xanchor = XRT_ANCHOR_NORTHEAST; 
             break;
      case 6:
             Xanchor = XRT_ANCHOR_SOUTHWEST; 
             break;
      case 7:
             Xanchor = XRT_ANCHOR_SOUTHEAST; 
             break;
      case 8:
             Xanchor = XRT_ANCHOR_BEST; 
             break;
      case 9:
             Xanchor = XRT_ANCHOR_HOME; 
             break;
      default:
             Xanchor = XRT_ANCHOR_BEST; 
             break;
   }
  ZrtSymbolAnchor (Xanchor);
  return;
}

/* ======================================================================= */

/* symbol () Plots the 'text' at position (x,y) with given height
and angle. */

void F77_FUNC(symbol, SYMBOL) 
(float *x, float *y, float *height, char *text,
		int *inteq, float *angle, int *nchar, ZFORSTR_PARAM)
#if 0
float *x;		/* Input */
float *y;		/* Input */
float *height;		/* Input */
char  *text;		/* Input */
int   *inteq;		/* Input */
float *angle;		/* Input */
int   *nchar;		/* Input */
#endif
{
ZFORSTR_BLOCK
int   length;
char *ptr;

   /* Obtain length of the input text string */
   zsfor2len (length, text,&x,7,4,1, nchar);

   /* Allocate memory for string conversion */
   ptr = (char *) malloc ((size_t)length+1);
   if (ptr == NULL) {
      zvmessage("Symbol error ... unable to allocate memory for string conversion","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack (ptr);
 
   /* Convert FORTRAN string to C string and move */
   zsfor2c (ptr, length, text, &x,7,4,1, nchar);

   ZrtSymbol (*x, *y, *height, ptr, *inteq, *angle, *nchar);
   return;
}

/* ======================================================================= */

/* ZrtDrawLine () draws line from (x1,y1) to (x2,y2) */

void F77_FUNC(drawline, DRAWLINE)
(x1, y1, x2, y2)
int *x1;       /* Input */
int *y1;       /* Input */
int *x2;       /* Input */
int *y2;       /* Input */
{
  ZrtDrawLine (*x1, *y1, *x2, *y2);
  return;
}

/* ======================================================================= */

/* scale () calculates the scale factors for the data in the array and puts
them at the end of the array */

void F77_FUNC_(scale, SCALE)
(array, axlen, npts, inc)
float *array;
float *axlen;
int   *npts, *inc;
{
  ZrtScale (array, *axlen, *npts, *inc);
  return;
}

/* ======================================================================= */


/* factor ()  */

void F77_FUNC(factor, FACTOR)
(newfact)
float *newfact;
{
  ZrtFactor (*newfact);
  return;
}

/* ======================================================================= */

/* axis () draws an annotated axis of length axlen inches at an angle;
   labels each tic mark every inch with a number, starting with firstv
   and with an increment of deltav;  labels the axis with the title.
   calculates the scale factors for the data in the array and puts
   them at the end of the array */

void F77_FUNC(axis, AXIS) 
(float *x, float *y, char *title, int *nchar,
	float *axlen, float *angle, float *firstv, float *deltav, ZFORSTR_PARAM)
{
ZFORSTR_BLOCK
char  string[SizeOfAxisString];
int   length;

   /* If the length of the input string is greater than the recptacle */
   zsfor2len (length, title, &x, 8, 3, 1, deltav);
   if ( length > sizeof (string)-1) {
      sprintf (string,"Axis title is too long: %d", length);
      zvmessage (string,"");
      return;
   }

   /* Convert FORTRAN string to C string and move string*/
   zsfor2c (string, sizeof(string)-1, title, &x, 8, 3, 1, deltav);
   
   ZrtAxis (*x, *y, string, *nchar, *axlen, *angle, *firstv, *deltav);
   return;
}

/* ======================================================================= */

/* ZrtAxesTitles () is called to set the X, Y & Y2 axis titles, and rotation
of titles about the axis.  If the X-axis, Y-Axis or the Y2-axis are
not set, then Axes titles will not be displayed.  Note: Only the
vertical axes (Y & Y2) can be rotated. */

void F77_FUNC(axestitles, AXESTITLES)
(char *xtitle, char *ytitle, int *yrotation,
		char *y2title, int *y2rotation, ZFORSTR_PARAM) 
{
ZFORSTR_BLOCK
int       xlength, ylength, y2length;
char     *xptr, *yptr, *y2ptr;
XrtRotate yrotate, y2rotate;

   /* Set specified rotation for axes titles */
   switch (*yrotation) {
     case 0:
            yrotate = XRT_ROTATE_NONE;
            break;
          
     case 90:
            yrotate = XRT_ROTATE_90;
            break;
          
     case 270:
            yrotate = XRT_ROTATE_270;
            break;
          
     default:
            yrotate = XRT_ROTATE_NONE;
            break;
   }
          
   switch (*y2rotation) {
     case 0:
            y2rotate = XRT_ROTATE_NONE;
            break;
          
     case 90:
            y2rotate = XRT_ROTATE_90;
            break;
          
     case 270:
            y2rotate = XRT_ROTATE_270;
            break;
          
     default:
            y2rotate = XRT_ROTATE_NONE;
            break;
   }
          

   /* Obtain length of the xtitle input string */
   zsfor2len (xlength, xtitle,&xtitle,5,1,1, y2rotation);

   /* Allocate memory for string conversion */
   xptr = (char *) malloc ((size_t)xlength+1);
   if (xptr == NULL) {
      zvmessage("AxesTitles error #1 ... unable to allocate memory for string conversion","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack (xptr);
 
   /* Convert FORTRAN string to C string and move */
   zsfor2c (xptr, xlength, xtitle, &xtitle,5,1,1, y2rotation);

   /* Obtain length of the ytitle input string */
   zsfor2len (ylength, ytitle,&xtitle,5,2,2, y2rotation);

   /* Allocate memory for string conversion */
   yptr = (char *) malloc ((size_t)ylength+1);
   if (yptr == NULL) {
      zvmessage("AxesTitles error #2 ... Unable to allocate memory for string conversion","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack (yptr);
 
   /* Convert FORTRAN string to C string and move */
   zsfor2c (yptr, ylength, ytitle, &xtitle,5,2,2, y2rotation);

   /* Obtain length of the y2title input string */
   zsfor2len (y2length, y2title,&xtitle,5,4,3, y2rotation);

   /* Allocate memory for string conversion */
   y2ptr = (char *) malloc ((size_t)y2length+1);
   if (y2ptr == NULL) {
      zvmessage("AxesTitles error #3 ... Unable to allocate memory for string conversion","");
      return;
   }

   /* Push allocated memory pointer onto stack */
   ZrtPushStack (y2ptr);
 
   /* Convert FORTRAN string to C string and move */
   zsfor2c (y2ptr, y2length, y2title, &xtitle,5,4,3, y2rotation);

   /* Call ZrtAxesTitles to set output PostScript File's axis titles */
   ZrtAxesTitles (xptr, yptr, yrotate, y2ptr, y2rotate);

   return;
}

/* ======================================================================= */

/* ZrtSetWidgetAspect () is called to set the aspect ratio for the whole graph
Widget. The default settings are; X=1200, and Y=900. To adjust the size of the 
annotated graph only, refer to ZrtSetGraphAspect. */

void F77_FUNC(setwidgetaspect, SETWIDGETASPECT) 
(xaxis, yaxis)
int *xaxis, *yaxis;
{
   ZrtSetWidgetAspect (*xaxis, *yaxis);
   return;
}

/* ======================================================================= */

/* ZrtSetGraphAspect () is called to set the aspect ratio for the annotated 
graph only. The default settings are; X=1200, and Y=900. To adjust the size 
of the whole graph widget, refer to ZrtSetWidgetAspect. */

void F77_FUNC(setgraphaspect, SETGRAPHASPECT) 
(xaxis, yaxis)
int *xaxis, *yaxis;
{
   ZrtSetGraphAspect (*xaxis, *yaxis);
   return;
}

/* ======================================================================= */

/* setlandscape () is called to set the printer output to either 
LandScape (TRUE) or to Portrait (FALSE).  Default is LandScape */

void F77_FUNC(setlandscape, SETLANDSCAPE) 
(mode)
int *mode;
{
   ZrtSetLandScape (*mode);
   return;
}

/* ======================================================================= */

/* axesreverse () reverses the X and/or Y axis orientation.  If 'X' is TRUE:
the X axis annotation increases from top to bottom. If 'Y' is TRUE: the Y axis 
annotation will increase from right to left  */ 

void F77_FUNC(axesreverse, AXESREVERSE) 
(xaxis, yaxis)
int *xaxis, *yaxis;
{
   ZrtAxesReverse (*xaxis, *yaxis);
   return;
}

/* ======================================================================= */

/* plotfn () is called to set the output PostScript filename. If the 
filename is not set, then the filename will default to 'postscript.plt'.*/

void F77_FUNC(plotfn, PLOTFN) 
(char *filename, ZFORSTR_PARAM) 
#if 0
  char *filename;		/* PostScript output filename */
#endif
{
ZFORSTR_BLOCK
int   length;
char  *string;

   /* Obtain length of the Fortran input filename string */
   zsfor2len (length, filename,&filename,1,1,1, filename);

   /* Allocate memory for string conversion */
   string = (char *) malloc ((size_t)length);
   if (string == NULL) {
      zvmessage("PlotFn error ... Unable to allocate memory for PlotFn conversion","");
      return;
   }
   /* Push allocated memory pointer onto stack */
   ZrtPushStack (string);
 
   /* Convert FORTRAN string to C string and move */
   zsfor2c (string, length, filename, &filename,1,1,1, filename);

   /* Call ZrtPlotFn to set output PostScript File name */
   ZrtPlotFn (string); 

   return; 
}

/* ======================================================================= */

/* xrtbegin () Initializes plotter (display) and plotter variables.  Returned
status may be PASS, FAIL, EXIT.*/

void F77_FUNC(xrtbegin, XRTBEGIN) 
(status)
int *status;
{
   *status = ZrtBegin ();
   return;
}

/* ======================================================================= */

/* plots () Initializes plotter (display) and plotter variables. */

void F77_FUNC(plots, PLOTS) 
(isf, indexd, idev)
int *isf, *indexd, *idev;
{
   ZrtBegin (*isf, *indexd, *idev);
   return;
}

/* ======================================================================= */

/* plot () Plots a line or moves the pen depending on 'action' requested */

void F77_FUNC(plot, PLOT) 
(x, y, action)
float *x;
float *y;
int   *action;		/* Input */
{

   ZrtPlot (*x, *y, *action);
   return;
}

/* ======================================================================= */

/* WHERE () Returns the coordinate position of the pen */

void F77_FUNC(where, WHERE) 
(rxpage, rypage, rfact)
float *rxpage, *rypage, *rfact;
{
   ZrtWhere (rxpage, rypage, rfact);
   return;
}

/* ======================================================================= */

/* displayaxes () is called to display/suppress the automatic generation
of the X-axis, Y-axis, & the Y2-axis by XRT/graph.  If the calling
function sets the xaxis, yaxis or y2axis to FALSE (0), the display of
the specified axis will be suppressed.  The default settings for 
XRT/graph is to display the xaxis and yaxis, and suppress the display
of the y2axis.  Suppressing an axis will also suppress the corresponding
axis title. */


void F77_FUNC(displayaxes, DISPLAYAXES) 
(xaxis, yaxis, y2axis)
int *xaxis, *yaxis, *y2axis; 
{
   ZrtDisplayXrtAxis (*xaxis, *yaxis, *y2axis);
   return;
}


/* ======================================================================= */

/* ZrtAxesOrigin () is called to specify where the axes should be rendered.
For example, setting the X origin to 5.0  will cause the Y axis to cross the
X-axis at X=5.0 */

void F77_FUNC(axesorigin, AXESORIGIN) 
(xorigin, yorigin)
int *xorigin, *yorigin;
{
   ZrtAxesOrigin (*xorigin, *yorigin);
   return;
}

/* ======================================================================= */

/* ZrtAxesOriginPlacement () is called to specify the placement for the
axis origins on a plot. The legal values for placement are:
...Fortran.,...,,C.............Placement
     0      XRT_ORIGIN_AUTO    ORIGIN x is placed at the minimum axis value
                               or at zero if the dataset contains positive
                               and negative values (default). 
     1      XRT_ORIGIN_ZERO:   Origin is placed at zero.
     2      XRT_ORIGIN_MIN:    Origin is placed at the minimum axis value.
     3      XRT_ORIGIN_MAX     Origin is placed at the minimum axis value.
*/


void F77_FUNC(axesoriginplacement, AXESORIGINPLACEMENT) 
(xorigin, yorigin)
int *xorigin, *yorigin;
{
int  xplacement, yplacement;

   switch (*xorigin) {

      case 0:
             xplacement = XRT_ORIGIN_AUTO;
             break;

      case 1:
             xplacement = XRT_ORIGIN_ZERO;
             break;

      case 2:
             xplacement = XRT_ORIGIN_MIN; 
             break;

      case 3:
             xplacement = XRT_ORIGIN_MAX;  
             break;

      default:
             zvmessage ("Error ... AxisOriginPlacement values","");
             return;
             break;
      }

   switch (*yorigin) {

      case 0:
             yplacement = XRT_ORIGIN_AUTO;
             break;

      case 1:
             yplacement = XRT_ORIGIN_ZERO;
             break;

      case 2:
             yplacement = XRT_ORIGIN_MIN; 
             break;

      case 3:
             yplacement = XRT_ORIGIN_MAX;  
             break;

      default:
             zvmessage ("Error ... AxisOriginPlacement values","");
             return;
             break;
      }

   ZrtAxesOriginPlacement (xplacement, yplacement);

   return;
}

/* ========================================================================== */

/* ZrtSetAxesMaximums () is called to specify the maximum values for the axes.
By setting these values, graph data may be clipped and not displayed.
*/

void F77_FUNC(setaxesmaximums, SETAXESMAXIMUMS) 
(xaxis, yaxis, y2axis)
float *xaxis, *yaxis, *y2axis;
{
   ZrtSetAxesMaximums (*xaxis, *yaxis, *y2axis);
   return;
}

/* ========================================================================== */

/* ZrtSetAxesMinimums () is called to specify the minimum values for the axes.
By setting these values, graph data may be clipped and not displayed.
*/

void F77_FUNC(setaxesminimums, SETAXESMINIMUMS) 
(xaxis, yaxis, y2axis)
float *xaxis, *yaxis, *y2axis;
{
   ZrtSetAxesMinimums (*xaxis, *yaxis, *y2axis);
   return;
}

/* ======================================================================= */

/* ZrtSetDataMaximums () is called to specify the maximum values for the data.
By setting these values, graph data may be clipped and not displayed.
*/

void FTN_NAME(setdatamaximums) (xaxis, yaxis, y2axis)
float *xaxis, *yaxis, *y2axis;
{
   ZrtSetDataMaximums (*xaxis, *yaxis, *y2axis);
   return;
}

/* ========================================================================== */

/* ZrtSetDataMinimums () is called to specify the minimum values for the data.
By setting these values, graph data may be clipped and not displayed.
*/

void F77_FUNC(setdataminimums, SETDATAMINIMUMS) 
(xaxis, yaxis, y2axis)
float *xaxis, *yaxis, *y2axis;
{
   ZrtSetDataMinimums (*xaxis, *yaxis, *y2axis);
   return;
}

/* ========================================================================== */

/* xrtpage () is called to cause the graph being created to be displayed.
Once the graph has been displayed the graph may be saved for printing,
terminate the program, or page to the next graph.  If the interactive 
user selects EXIT on the menu bar, a status od EXIT will be returned, 
otherwise a status of PASS will be returned..
*/

void F77_FUNC(xrtpage, XRTPAGE) 
(status)
int *status;
{
   *status = ZrtPage ();
   return;
}

/* ========================================================================== */

/* setactiveset () allows the application program to write into one of  of the
two default data sets, 0 or 1, or up 11 if specified through ZrtSetActiveCount.
Data may be added to any data set.  Data set '0' displays a small triangle
for the point symbol adjacent to each point entered. Data set '1' does not
display the point sysmol. The specified data set will  remain active until
changed. */

void F77_FUNC(setactiveset, SETACTIVESET) 
(set)
int *set;
{
   ZrtSetActiveSet (*set);
   return;
}

/* ========================================================================== */

