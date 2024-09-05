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
/*#include <Xm/xrt_com.h> -jaw - file doesn't exist in v2.1 */
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/ToggleB.h>
#include <Xm/PushBG.h>
/*#include "xrt_dfloat.h" -jaw - only required for unsupported VMS*/


       void ZrtPushStack (char *);
       int  ZrtPage ();
       void ZrtClose (void);
static int  ZrtPageDevice (void);
static int  ZrtPageContinue (void);
static void ZrtCloseDevice (void);
static void MakePostScriptFile (char *);
static int  ZrtInitDevice (char *);
static void PerformHouseCleaning (void); 

#define MAXSETS 11
#define MAX(a,b) (a > b ? a : b)
#define MIN(a,b) (a < b ? a : b)
#define SIGN (a,b) (b >= 0.0 ? abs(a) : -abs(a))
#define NINT(a)  (int) (a > 0.0 ? a + 0.5 : a - 0.5)
#define EXIT 999
#define PASS 1
#define FAIL 0
#define PAPER_WIDTH   8.5
#define PAPER_HEIGHT 11.0
#define PAPER_MARGIN  0.25
static Arg    wargs[10];
static XrtTextHandle handle;
static XrtTextDesc   text;
static XrtTextDesc   xtext;
static XrtDataStyle  DataStyle0;
static XrtDataStyle  DataStyle1;
static XrtDataStyle  *Ds0Ptr;        /* Pointer to data set zero */
static XrtDataStyle  *Ds1Ptr;        /* Pointer to data set one  */

XrtAnchor            XRT_SYMBOL_ANCHOR = XRT_ANCHOR_BEST;
static int           DATASTYLESET = FALSE;
static int           ZRT_LANDSCAPE = FALSE;
static int           ZRT_WIDGET_ASPECT = FALSE;
static int           ZRT_GRAPH_ASPECT = FALSE;
static int           ZRT_HEADER = FALSE;
static int           ZRT_FOOTER = FALSE;
static int           ZRT_LABELS = FALSE;
static int           ZRT_AXES_TITLES = FALSE;
static int           ZRT_SHOW_AXES = FALSE;
static int           ZRT_AXES_ORIGIN_PLACEMENT = FALSE;
static int           ZRT_AXES_REVERSED = FALSE;
static int           ZRT_AXES_ORIGIN = FALSE;
static int           ZRT_AXES_MINIMUMS = FALSE;
static int           ZRT_AXES_MAXIMUMS = FALSE;
static int           ZRT_DATA_MINIMUMS = FALSE;
static int           ZRT_DATA_MAXIMUMS = FALSE;
static float         XrtXAxisOrigin;
static float         XrtYAxisOrigin;
static float         XrtXAxisMin;   
static float         XrtYAxisMin;   
static float         XrtY2AxisMin;   
static float         XrtXAxisMax;   
static float         XrtYAxisMax;   
static float         XrtY2AxisMax;   
static float         XrtXDataMin;
static float         XrtYDataMin;
static float         XrtY2DataMin;   
static float         XrtXDataMax;   
static float         XrtYDataMax;   
static float         XrtY2DataMax;   
static XrtOriginPlacement XrtXOriginPlacement; /* Legal values are:
                                               XRT_ORIGIN_AUTO, XRT_ORIGIN_ZERO, 
                                               XRT_ORIGIN_MIN, XRT_ORIGIN_MAX */
static XrtOriginPlacement XrtYOriginPlacement; /* Legal values are:
                                               XRT_ORIGIN_AUTO, XRT_ORIGIN_ZERO,
                                               XRT_ORIGIN_MIN, XRT_ORIGIN_MAX */
static int           FileSaved = 0;
static int           ExitFlag = 0;
static int           SaveFlag = 0;
static int           PageFlag  = 0;
static int           BigLoop   = False;
static int           WaitNoMore = False;
char  *FreeStackPtrs [400];
int    FreeStackIndex = 0;
static XtInputMask   mask;
static char  *fonts[] = {"Times-Roman",
                         "Times-Bold",
                         "Times-Italic",
                         "Times-BoldItalic",
                         "Palatino-Roman",
                         "Palatino-Bold",
                         "Palatino-Italic",
                         "Palatino-BoldItalic",
                         "Courier"};			/* 8 */


static XrtDataStyle DataStyles [] = {
     { XRT_LPAT_SOLID,      /* 00 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "red",                     /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_NONE,           /* XrtPoint         - point */
      "magenta",                 /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 01 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "red",                     /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_TRI,            /* XrtPoint         - point */
      "magenta",                 /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 02 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "tan",                     /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_BOX,            /* XrtPoint         - point */
      "green",                   /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 03 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "limeGreen",               /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_DOT,            /* XrtPoint         - point */
      "medium blue",             /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 04 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "MediumTurquoise",         /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_DIAMOND,        /* XrtPoint         - point */
      "red",                     /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 05 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "blue",                    /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_STAR,           /* XrtPoint         - point */
      "red",                     /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 06 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "magenta",                 /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_SQUARE,         /* XrtPoint         - point */
      "red",                     /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 07 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "medium blue",             /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_HORIZ_LINE,     /* XrtPoint         - point */
      "light sky blue",          /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 08 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "medium blue",             /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_CROSS,          /* XrtPoint         - point */
      "LimeGreen",               /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 09 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "red",                     /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_CIRCLE,         /* XrtPoint         - point */
      "MediumTurquoise",         /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     },
     { XRT_LPAT_SOLID,      /* 10 - XrtLinePattern   - lpat */
       XRT_FPAT_SOLID,           /* XrtFillPattern   - fpat */
      "red",                     /* char *           - color */
       1,                        /* int              - width */
       XRT_POINT_VERT_LINE,      /* XrtPoint         - point */
      "blue",                    /* char *           - pcolor */
       XRT_DEFAULT_POINT_SIZE    /* int              - psize */
     }
};

static XrtDataStyle *DataStylesPtr[] = {
      &DataStyles[0],
      &DataStyles[1],
      &DataStyles[2],
      &DataStyles[3],
      &DataStyles[4],
      &DataStyles[5],
      &DataStyles[6],
      &DataStyles[7],
      &DataStyles[8],
      &DataStyles[9],
      &DataStyles[10],
      NULL 
};


static char   newpointsfile [] = "pointsfile.tmp";
static char  *header_strings[] = {NULL,NULL,NULL};
static char  *footer_strings[] = {NULL,NULL,NULL};
static char  *set_labels[]     = {NULL,NULL,NULL};
static char  *point_labels[]   = {NULL,NULL,NULL};
static char **XrtHeader     = NULL;
static char **XrtFooter     = NULL;
static char **XrtSetLabel   = NULL;
static char  *XrtXTitle     = NULL;
static char  *XrtYTitle     = NULL;
static char  *XrtY2Title    = NULL;
static XrtRotate XrtYRotate = XRT_ROTATE_270;
static XrtRotate XrtY2Rotate = XRT_ROTATE_270;
static Boolean XRT_XReversed = FALSE;
static Boolean XRT_YReversed = FALSE;
static int    PostScriptFileNameIsSet = FALSE;
static int    XrtLandScape = TRUE; 
static char   PostScriptFileName [80];
static char   msg [132];
static float  xoff, yoff, xscale, yscale, plotx, ploty, fact;
static int    pennum;
static FILE  *PS_fptr;
static int    PageNo;
static int    XrtShowXAxis = TRUE;
static int    XrtShowYAxis = TRUE;
static int    XrtShowY2Axis = TRUE;
static int    XrtWidgetWidth = 1200; /* Use: Set width of whole graph widget */
static int    XrtWidgetHeight = 900; /* Use: Set height of whole graph widget*/ 
static int    XrtGraphWidth = 1200;  /* Use: Set width of graph area */
static int    XrtGraphHeight = 900;  /* Use: Set height of graph area */  

static Boolean     XrtLegendShow   = FALSE; 
static XrtAnchor   XrtLegendAnchor;   
static XrtAlign    XrtLegendOrientation;   
static XrtAdjust   XrtHeaderAdjust = XRT_ADJUST_LEFT;   
static XrtAdjust   XrtFooterAdjust = XRT_ADJUST_RIGHT;  

static XrtTextDesc   XrtTextDescStruct;
static XrtTextDesc  *XrtTextDescPtr;

static char   PSFont [] = "Courier";
static char   PostScriptDefaultFileName[] = "postscript.psf";
static int    PageContinue = FALSE;

/* Variables required for XrtDrawPs () */
static Widget PS_graph, field;
static int    inches;
static float  paper_width;
static float  paper_height;
static float  margin;
static int    landscape;
static float  x_offset;
static float  y_offset;
static float  width;
static float  height;
static char  *header_font;
static int    header_size;
static char  *footer_font;
static int    footer_size;
static char  *anno_font;
static int    anno_size;
static char  *legend_font;
static int    legend_size;
static int    fill_background;
static int    smart_mono;
static int    showpage;


/* #define XT TRUE */

/* Set up global Xintrinsics variables */ 
static XtPointer client_data;

/* jaw - replaced all pointers to XrtData to pointers to XrtDataHandle as per XRTGraph 3.0*/

typedef struct {
   Widget           graph;
   XrtDataHandle    *XrtDataHandlePtr;
} WX;
static WX           *WXPtrArray;
static WX           *WXPtr;
static Widget        graph, toplevel, form, rbox, plot, bar, xexit;
static XrtDataHandle      *XrtDataHandlePtr = NULL;
static XtAppContext  app_context;
static int           activesetcount = 0;
static int           activeset = 0;
static int           maxpoints = 65535;
static int           maxsets   = MAXSETS;
static int           npoints[MAXSETS] = {0,0,0,0,0,0,0,0,0,0,0};
static int           dataset = 1;

/* zfxrtps.f & zcxrtps.c contains the FORTRAN and C interfaces necessary to
produce an Encapsulated PostScript File (EPSF) representation of a graph.

zfxrtps.f provides the FORTRAN bridge between the 'FORTRAN' programs and 
zcxrtps.c; the 'C' program that interfaces with Xrt/graph which is the 
PostScript driver interface. 

The software described in this document interfaces with MOTIF and with the
interfaces described in XRT/graph for Motif; Widget Programmer's Guide &
Reference Manual, Software Objects for GUI Development. 

The PostrScript file is suitable for sending to a PostScript printer, or
may be used for importing into another PostScript file. The PostScript file
conforms to EPSE-2.0 format. 

=========================================================================== */

/* C External Interface  */

/* ZrtPlotFn () is called to set the output PostScript filename. If the 
filename is not set, the filename will default to 'postscript.psf'.  The 
call to ZrtPlotFn must be made before the call to ZrtBegin ().  
*/

void ZrtPlotFn (filename) 
char *filename;		/* Pointer to PostScript filename */
{
int length;

   /* If the length of the input string is greater than the recptacle */
   if ( strlen (filename) > sizeof(PostScriptFileName)-1) {
      length = strlen (filename);
      sprintf (msg,"Post Script file name is too long: %d", length);
      zvmessage (msg,"");
      return;
   }

   /* Copy in new filename and indicate that it is set */
   strcpy (PostScriptFileName,filename);

   PostScriptFileNameIsSet = TRUE;

   return;
}
	
/* ========================================================================== */

static void SetHeadersAndFooters ()
{
   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } 

   if (ZRT_LANDSCAPE) {
      landscape = XrtLandScape;  
      ZRT_LANDSCAPE = FALSE;
   }

   if (ZRT_WIDGET_ASPECT) {
      XtVaSetValues (graph, 
                     XtNwidth,               XrtWidgetWidth,
                     XtNheight,              XrtWidgetHeight,
                     NULL);
      ZRT_WIDGET_ASPECT = FALSE;
   }

   if (ZRT_GRAPH_ASPECT) {
      XtVaSetValues (graph, 
                     XtNxrtGraphWidth,       XrtGraphWidth,
                     XtNxrtGraphHeight,      XrtGraphHeight,
                     NULL);
      ZRT_GRAPH_ASPECT = FALSE;
   }

   if (ZRT_AXES_REVERSED) {
      XtVaSetValues (graph, 
                     XtNxrtXAxisReversed,    XRT_XReversed,    
                     XtNxrtYAxisReversed,    XRT_YReversed,    
                     NULL);
      ZRT_AXES_REVERSED = FALSE;
   }

   if (ZRT_HEADER) {
      XtVaSetValues (graph, 
                     XtNxrtHeaderStrings,    XrtHeader,
                     XtNxrtHeaderAdjust,     XrtHeaderAdjust,
                     NULL);
      ZRT_HEADER = FALSE;
   }

   if (ZRT_FOOTER) {
      XtVaSetValues (graph, 
                     XtNxrtFooterStrings,    XrtFooter,
                     XtNxrtFooterAdjust,     XrtFooterAdjust,
                     NULL);
      ZRT_FOOTER = FALSE;
   }

   if (ZRT_LABELS) {
      XtVaSetValues (graph, 
                     XtNxrtSetLabels,          XrtSetLabel, 
                     XtNxrtLegendAnchor,       XrtLegendAnchor,   
                     XtNxrtLegendOrientation,  XrtLegendOrientation,   
                     XtNxrtLegendShow,         XrtLegendShow,   
                     NULL);
      ZRT_LABELS = FALSE;
   }
   if (ZRT_AXES_TITLES) {
      XtVaSetValues (graph, 
                     XtNxrtXTitle,             XrtXTitle, 
                     XtNxrtXTitleRotation,     XRT_ROTATE_NONE,
                     XtNxrtYTitle,             XrtYTitle, 
                     XtNxrtYTitleRotation,     XrtYRotate,
                     XtNxrtY2Title,            XrtY2Title, 
                     XtNxrtY2TitleRotation,    XrtY2Rotate,
                     NULL);
      ZRT_AXES_TITLES = FALSE;
   }

   if (ZRT_SHOW_AXES) {
      XtVaSetValues (graph, 
                     XtNxrtXAxisShow,        XrtShowXAxis,
                     XtNxrtYAxisShow,        XrtShowYAxis,
                     XtNxrtY2AxisShow,       XrtShowY2Axis,
                     XtNxrtYAxisMult,        XrtFloatToArgVal(1.0),
                     NULL);
      ZRT_SHOW_AXES = FALSE;
   }
   if (ZRT_AXES_ORIGIN_PLACEMENT) {
      XtVaSetValues (graph, 
                     XtNxrtXOriginPlacement, XrtXOriginPlacement,
                     XtNxrtYOriginPlacement, XrtYOriginPlacement,
                     NULL);
      ZRT_AXES_ORIGIN_PLACEMENT = FALSE;
   }
   if (ZRT_AXES_ORIGIN) {
      XtVaSetValues (graph, 
                     XtNxrtXOrigin,          XrtFloatToArgVal(XrtXAxisOrigin),
                     XtNxrtYOrigin,          XrtFloatToArgVal(XrtYAxisOrigin),
                     NULL);
      ZRT_AXES_ORIGIN = FALSE;
   }
   if (ZRT_AXES_MINIMUMS) {
      XtVaSetValues (graph, 
                     XtNxrtXAxisMin,         XrtFloatToArgVal(XrtXAxisMin),
                     XtNxrtYAxisMin,         XrtFloatToArgVal(XrtYAxisMin),
                     XtNxrtY2AxisMin,        XrtFloatToArgVal(XrtY2AxisMin),
                     NULL);
      ZRT_AXES_MINIMUMS = FALSE;
   }
   if (ZRT_AXES_MAXIMUMS) {
      XtVaSetValues (graph, 
                     XtNxrtXAxisMax,         XrtFloatToArgVal(XrtXAxisMax),
                     XtNxrtYAxisMax,         XrtFloatToArgVal(XrtYAxisMax),
                     XtNxrtY2AxisMax,        XrtFloatToArgVal(XrtY2AxisMax),
                     NULL);
      ZRT_AXES_MAXIMUMS = FALSE;
   }
   if (ZRT_DATA_MINIMUMS) {
      XtVaSetValues (graph, 
                     XtNxrtXMin,             XrtFloatToArgVal(XrtXAxisMin),
                     XtNxrtYMin,             XrtFloatToArgVal(XrtYAxisMin),
                     XtNxrtY2Min,            XrtFloatToArgVal(XrtY2AxisMin),
                     NULL);
      ZRT_DATA_MINIMUMS = FALSE;
   }
   if (ZRT_DATA_MAXIMUMS) {
      XtVaSetValues (graph, 
                     XtNxrtXMax,             XrtFloatToArgVal(XrtXAxisMax),
                     XtNxrtYMax,             XrtFloatToArgVal(XrtYAxisMax),
                     XtNxrtY2Max,            XrtFloatToArgVal(XrtY2AxisMax),
                     NULL);
      ZRT_DATA_MAXIMUMS = FALSE;
   }
   return;
}

/* ========================================================================== */

/* ZrtSetLandScape () is called to set the output page format to either
landscape or portrait.  The default format is landscape (TRUE).  A
setting of FALSE will set the format to portrait. */

void ZrtSetLandScape(format)
int format;
{
   ZRT_LANDSCAPE = TRUE;
   XrtLandScape = format;

   /* If the graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... Set value now */
      landscape = XrtLandScape;
   }
}

/* ========================================================================== */

/* ZrtSetWidgetAspect () is called to set the aspect ratio for the whole graph
Widget. The default settings are; X=1200, and Y=900. To adjust the size of the 
annotated graph only, refer to ZrtSetGraphAspect. */


void ZrtSetWidgetAspect (xaxis, yaxis)
int xaxis, yaxis;
{
   XrtWidgetWidth  = xaxis;
   XrtWidgetHeight = yaxis;

   ZRT_WIDGET_ASPECT = TRUE;

   /* If the graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNwidth,               XrtWidgetWidth,
                     XtNheight,              XrtWidgetHeight,
                     NULL);
      ZRT_WIDGET_ASPECT = FALSE;
   }
   return;
}

/* ========================================================================== */

/* ZrtSetGraphAspect () is called to set the aspect ratio for the annotated 
graph only. The default settings are; X=1200, and Y=900. To adjust the size 
of the whole graph widget, refer to ZrtSetWidgetAspect. */

void ZrtSetGraphAspect (xaxis, yaxis)
int xaxis, yaxis;
{
   XrtGraphWidth  = xaxis;
   XrtGraphHeight = yaxis;

   ZRT_GRAPH_ASPECT = TRUE;

   /* If the graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtGraphWidth,       XrtGraphWidth,
                     XtNxrtGraphHeight,      XrtGraphHeight,
                     NULL);
      ZRT_GRAPH_ASPECT = FALSE;
   }
   return;
}

/* ========================================================================== */

/* ZrtAxesReverse () is called to set the Axes orientation.  
If the XAxis is TRUE (reversed), annotation will increase from right to left, 
and if the YAxis is TRUE (reversed), annotation will increase from top 
to bottom */

void ZrtAxesReverse (xaxis, yaxis)
int xaxis, yaxis;
{

   ZRT_AXES_REVERSED = TRUE;

   /* Initialize boolean values */
   if (xaxis) { 
     XRT_XReversed = 1;
   } else {
     XRT_XReversed = 0;
   }

   /* Initialize boolean values */
   if (yaxis) { 
     XRT_YReversed = 1;
   } else {
     XRT_YReversed = 0;
   }

   /* If the graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin () */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXAxisReversed,    XRT_XReversed,    
                     XtNxrtYAxisReversed,    XRT_YReversed,    
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtHeader () is called to set the header string(s). If the header is not set,
then a header will not be displayed. */

void ZrtHeader (header,adjustment) 
char **header;	       /* Pointer to header string */
XrtAdjust adjustment;  /* Set for left, center or right */
{
   ZRT_HEADER = TRUE;

   /* Initialize pointer to the list of pointers to strings */
   XrtHeader = header;
   XrtHeaderAdjust = adjustment;

   /* If the graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtHeaderStrings,    XrtHeader,
                     XtNxrtHeaderAdjust,     XrtHeaderAdjust,
                     NULL);
   }
   /* Set point symbol in data set '1' to NONE */

   return;
}

/* ========================================================================== */

/* ZrtFooter () is called to set the footer string(s). If the footer is not set,
then a footer will not be displayea footer will not be displaye. */

void ZrtFooter (footer, adjustment) 
char **footer;		/* Pointer to footer string */
XrtAdjust adjustment;  /* Set for left, center or right */
{
   ZRT_FOOTER = TRUE;

   /* Initialize pointer to list of pointers to strings */
   XrtFooter = footer;
   XrtFooterAdjust = adjustment;

   /* If the graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtFooterStrings,    XrtFooter,
                     XtNxrtFooterAdjust,     XrtFooterAdjust,
                     NULL);
   }
   return;
}
	
/* ========================================================================== */

/* ZrtDisplayXrtAxis () is called to suppress the automatic generation
of the X-axis & the Y-axis.  If the calling function sets either xaxis or
the yaxis (or both) to FALSE (0) the axis will be suppressed.  The default 
will be for the X and Y axis to be displayed.  The purpose of this 
function is to allow the calling function to use ZrtAxis to draw the 
X and Y axis. */

void ZrtDisplayXrtAxis (xaxis,yaxis,y2axis) 
int xaxis;
int yaxis;
int y2axis;
{
   ZRT_SHOW_AXES = TRUE;

   XrtShowXAxis  = xaxis;
   XrtShowYAxis  = yaxis;
   XrtShowY2Axis = y2axis;

   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXAxisShow,        XrtShowXAxis,
                     XtNxrtYAxisShow,        XrtShowYAxis,
                     XtNxrtY2AxisShow,       XrtShowY2Axis,
                     XtNxrtYAxisMult,        XrtFloatToArgVal(1.0),
                     NULL);
   }
   return;
}
	
/* ========================================================================== */

/* ZrtAxesOrigin () is called to specify where the axis should be rendered.
For example, setting the X origin to 5.0 will cause the Y axis to cross the 
X-Axis at X=5.0.
*/

void ZrtAxesOrigin (xorigin, yorigin)
float xorigin, yorigin;
{
   ZRT_AXES_ORIGIN = TRUE;

   XrtXAxisOrigin = xorigin;
   XrtYAxisOrigin = yorigin;

   /* If the graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXOrigin,          XrtFloatToArgVal(XrtXAxisOrigin),
                     XtNxrtYOrigin,          XrtFloatToArgVal(XrtYAxisOrigin),
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtAxesOriginPlacement () is called to specify the placement for the 
axis origins on a plot. The legal values for placement are:
     XRT_ORIGIN_AUTO         ORIGIN x is placed at the minimum axis value
                             or at zero if the dataset contains positive 
                             and negative values.  
     XRT_ORIGIN_ZERO:        Origin is placed at zero.
     XRT_ORIGIN_MIN:         Origin is placed at the minimum axis value.
     XRT_ORIGIN_MAX          Origin is placed at the minimum axis value.
*/

void ZrtAxesOriginPlacement (xorigin, yorigin)
XrtOriginPlacement xorigin;
XrtOriginPlacement yorigin;
{
   ZRT_AXES_ORIGIN_PLACEMENT = TRUE;

   XrtXOriginPlacement = xorigin;
   XrtYOriginPlacement = yorigin;

   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... Set then perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXOriginPlacement, XrtXOriginPlacement,
                     XtNxrtYOriginPlacement, XrtYOriginPlacement,
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtSetAxesMaximums () is called to specify the maximum values for the axes.
By setting these values, graph data may be clipped and not displayed.
*/

void ZrtSetAxesMaximums (xaxis, yaxis, y2axis)
float xaxis;
float yaxis;
float y2axis;
{
   ZRT_AXES_MAXIMUMS = TRUE;

   XrtXAxisMax = xaxis;
   XrtYAxisMax = yaxis;
   XrtY2AxisMax = y2axis;

   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... Set then perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXAxisMax,           XrtFloatToArgVal(XrtXAxisMax),
                     XtNxrtYAxisMax,           XrtFloatToArgVal(XrtYAxisMax),
                     XtNxrtY2AxisMax,          XrtFloatToArgVal(XrtY2AxisMax),
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtSetAxesMinimums () is called to specify the minimum values for the axes.
By setting these values, graph data may be clipped and not displayed.
*/

void ZrtSetAxesMinimums (xaxis, yaxis, y2axis)
float xaxis;
float yaxis;
float y2axis;
{
   ZRT_AXES_MINIMUMS = TRUE;

   XrtXAxisMin  = xaxis;
   XrtYAxisMin  = yaxis;
   XrtY2AxisMin = y2axis;

   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXAxisMin,         XrtFloatToArgVal(XrtXAxisMin),
                     XtNxrtYAxisMin,         XrtFloatToArgVal(XrtYAxisMin),
                     XtNxrtY2AxisMin,        XrtFloatToArgVal(XrtY2AxisMin),
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtSetDataMaximums () is called to specify the maximum values of the data.
Graph data may be clipped and not displayed.
*/

void ZrtSetDataMaximums (xaxis, yaxis, y2axis)
float xaxis;
float yaxis;
float y2axis;
{
   ZRT_DATA_MAXIMUMS = TRUE;

   XrtXAxisMax = xaxis;
   XrtYAxisMax = yaxis;
   XrtY2AxisMax = y2axis;

   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... Set then perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXMax,             XrtFloatToArgVal(XrtXAxisMax),
                     XtNxrtYMax,             XrtFloatToArgVal(XrtYAxisMax),
                     XtNxrtY2Max,            XrtFloatToArgVal(XrtY2AxisMax),
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtSetDataMinimums () is called to specify the minimum values of the data.
Graph data may be clipped and not displayed.
*/

void ZrtSetDataMinimums (xaxis, yaxis, y2axis)
float xaxis;
float yaxis;
float y2axis;
{
   ZRT_DATA_MINIMUMS = TRUE;

   XrtXAxisMin  = xaxis;
   XrtYAxisMin  = yaxis;
   XrtY2AxisMin = y2axis;

   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXMin,             XrtFloatToArgVal(XrtXAxisMin),
                     XtNxrtYMin,             XrtFloatToArgVal(XrtYAxisMin),
                     XtNxrtY2Min,            XrtFloatToArgVal(XrtY2AxisMin),
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtSetLabel () is called to set the setlabel string. If the label is not set,
then the setlabel will default to blanks. zcxrtps() uses the  
XRT_DATA_GENERAL data mode.  In this mode, Point-labels are ignored, Set-Labels
are used to annotate the various lines of data.
*/

void ZrtSetLabel (setlabel, orientation ,anchor) 
char    **setlabel;		/* Pointer to list of pointers for setlabel strings */
XrtAlign  orientation;  /* Align horizontal or vertical */
XrtAnchor anchor;       /* Aligh in one of eight locations*/
{
/* 
   Valid orientation enum values are:
         XRT_ALIGN_HORIZONTAL            XRT_ALIGN_VERTICAL 

   Valid anchor positions are:
         XRT_ANCHOR_NORTH                XRT_ANCHOR_SOUTH
         XRT_ANCHOR_EASTH                XRT_ANCHOR_WEST 
         XRT_ANCHOR_NORTHWEST            XRT_ANCHOR_SOUTHWEST
         XRT_ANCHOR_NORTHEAST            XRT_ANCHOR_SOUTHEAST
*/

   ZRT_LABELS           = TRUE;
   XrtLegendAnchor      = anchor;
   XrtLegendOrientation = orientation;
   XrtLegendShow        = True; 


   XrtSetLabel = setlabel; 
   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtSetLabels,          XrtSetLabel, 
                     XtNxrtLegendAnchor,       XrtLegendAnchor,   
                     XtNxrtLegendOrientation,  XrtLegendOrientation,   
                     XtNxrtLegendShow,         XrtLegendShow,          
                     NULL);
   }
   return;
}

/* ========================================================================== */

/* ZrtAxesTitles () is called to set the X, Y & Y2 axis titles, and rotation 
of titles about the axis. 
If the X-axis or the Y-axis or both will default to blanks. 

Legal values for the Y and Y2 axis title rotation are:
XRT_ROTATE_NONE
XRT_ROTATE_90
XRT_ROTATE_270

Only the verticle axis titles can be rotated. The X axis title can be 
be rotated only if the axis orientation is switched through the 
setting of XtNxrtInvertOrientation to TRUE; which is not done in release of 
XRTPS. Default settings are XRT_ROTATE_270.
*/

void ZrtAxesTitles (XTitle, YTitle, YRotate, Y2Title, Y2Rotate) 
char     *XTitle, *YTitle, *Y2Title;    	/* Pointer to X, Y and Y2 titles */
XrtRotate YRotate, Y2Rotate;
{
   ZRT_AXES_TITLES = TRUE;

   XrtXTitle   = XTitle;
   XrtYTitle   = YTitle;
   XrtY2Title  = Y2Title;
   XrtYRotate  = YRotate;
   XrtY2Rotate = Y2Rotate;

   /* If a graph widget has not been declared then return */
   if (graph == NULL) {
      /* Return. Values will be set when plot is started with ZrtBegin() */
      return;
   } else {
      /* Otherwise ... Set then perform set values function */
      XtVaSetValues (graph, 
                     XtNxrtXTitle,             XrtXTitle, 
                     XtNxrtXTitleRotation,     XRT_ROTATE_NONE,
                     XtNxrtYTitle,             XrtYTitle, 
                     XtNxrtYTitleRotation,     XrtYRotate,
                     XtNxrtY2Title,            XrtY2Title, 
                     XtNxrtY2TitleRotation,    XrtY2Rotate,
                     NULL);
   }
   /* Set point symbol in data set '1' to NONE */

   return;
}
	
/* ========================================================================== */

/* Callback routines */

static void ExitProc (Widget w, caddr_t client_data,
               XmToggleButtonCallbackStruct *call_data)
{
   if (call_data->set) {
                 XtVaSetValues (toplevel, XmNtitle,
                "An XRT/graph & PostScript Application Page Graph",NULL);
   }
   PageFlag = FALSE;
   SaveFlag = FALSE;
   ExitFlag = TRUE;
}

/* ========================================================================== */

/* Callback routines */

static void PageProc (Widget w, caddr_t client_data,
               XmToggleButtonCallbackStruct *call_data)
{
   if (call_data->set) {
                 XtVaSetValues (toplevel, XmNtitle,
                "An XRT/graph & PostScript Application Page Graph",NULL);
   }
   PageFlag = TRUE;
   SaveFlag = FALSE;
   ExitFlag = FALSE;
}

/* ========================================================================== */

/* Callback routines */

static void SaveProc (Widget w, caddr_t client_data,
               XmToggleButtonCallbackStruct *call_data)
{
   if (call_data->set)  {
                 XtVaSetValues (toplevel, XmNtitle, 
                "An XRT/graph & PostScript Application Print Graph",NULL);
   }
   PageFlag = FALSE;
   SaveFlag = TRUE; 
   ExitFlag = FALSE;
}

/* ========================================================================== */

static int SetupMenuBar (void)
{
int   n;

   /* Create an XmForm manager Widget */
   form = XtCreateManagedWidget ("form", xmFormWidgetClass, toplevel, NULL,0);
   if (form == FAIL) {
      zvmessage ("Call to XrCreateManagedWidget for 'form' failed","");
      return FAIL;
   }

   /* Create Radio Box */
   n = 0;
   XtSetArg (wargs[n], XmNentryClass, xmToggleButtonWidgetClass); n++;
   XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
   XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
   XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
   rbox = XmCreateRadioBox (form, "rbox", wargs, n);
   if (rbox == FAIL) {
      zvmessage ("Create Radio Button 'rbox' failed","");
      return FAIL;
   }
   XtManageChild (rbox); 
   
   /* Create xexit, page and save Toggle Buttons */
   xexit = XtVaCreateManagedWidget ("exit  ", 
                                   xmToggleButtonWidgetClass,   rbox,
                                   XmNset,                       TRUE,
                                   0);
   if (xexit == FAIL) {
      zvmessage ("Create Radio Button for 'exit ' failed","");
      return FAIL;
   }
   XtAddCallback(xexit,XmNvalueChangedCallback,(XtCallbackProc)ExitProc,NULL);

   bar = XtVaCreateManagedWidget ("page ", 
                                   xmToggleButtonWidgetClass,   rbox,
                                   XmNset,                       TRUE,
                                   0);
   if (bar == FAIL) {
      zvmessage ("Create Radio Button for 'page ' failed","");
      return FAIL;
   }
   XtAddCallback (bar, XmNvalueChangedCallback, (XtCallbackProc) PageProc,NULL);

   plot = XtVaCreateManagedWidget ("save ", 
                                   xmToggleButtonWidgetClass,   rbox,
                                   0);
   if (plot == FAIL) {
      zvmessage ("Create Radio Button 'save' failed","");
      return FAIL;
   }
   XtAddCallback (plot,XmNvalueChangedCallback,(XtCallbackProc)SaveProc,NULL);

   return PASS;
}

/* ========================================================================== */

static int SetupRadioButtons (void)
{
int   n;

   /* Create an XmForm manager Widget */
   form = XtCreateManagedWidget ("form", xmFormWidgetClass, toplevel, NULL,0);
   if (form == FAIL) {
      zvmessage ("Call to XrCreateManagedWidget for 'form' failed","");
      return FAIL;
   }

   /* Create Radio Box */
   n = 0;
   XtSetArg (wargs[n], XmNentryClass, xmToggleButtonWidgetClass); n++;
   XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
   XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_FORM); n++;
   XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
   rbox = XmCreateRadioBox (form, "rbox", wargs, n);
   if (rbox == FAIL) {
      zvmessage ("Create Radio Button 'rbox' failed","");
      return FAIL;
   }
   XtManageChild (rbox); 
   
   /* Create xexit, page and save Toggle Buttons */
   xexit = XtVaCreateManagedWidget ("exit", 
                                   xmToggleButtonWidgetClass,   rbox,
                                   XmNset,                       TRUE,
                                   0);
   if (xexit == FAIL) {
      zvmessage ("Create Radio Button for 'exit ' failed","");
      return FAIL;
   }
   XtAddCallback(xexit,XmNvalueChangedCallback,(XtCallbackProc)ExitProc,NULL);

   bar = XtVaCreateManagedWidget ("page ", 
                                   xmToggleButtonWidgetClass,   rbox,
                                   XmNset,                       TRUE,
                                   0);
   if (bar == FAIL) {
      zvmessage ("Create Radio Button for 'page ' failed","");
      return FAIL;
   }
   XtAddCallback (bar, XmNvalueChangedCallback, (XtCallbackProc) PageProc,NULL);

   plot = XtVaCreateManagedWidget ("save ", 
                                   xmToggleButtonWidgetClass,   rbox,
                                   0);
   if (plot == FAIL) {
      zvmessage ("Create Radio Button 'save' failed","");
      return FAIL;
   }
   XtAddCallback (plot,XmNvalueChangedCallback,(XtCallbackProc)SaveProc,NULL);

   return PASS;
}
/* ========================================================================== */

static int SetupXIntrinsics (void)
{
int   argc;
char *argv[1];
int   n;

   argc = 1;
   argv[0] = "xrtps";

   /* Initialize the X toolkit internals, create an application context,
   open and initialize a display, and create the initial application shell 
   instance. */

   toplevel = XtVaAppInitialize (&app_context,
                                 "PostScriptFile",
                                 NULL,
                                 0,
                                 &argc,
                                 argv, NULL,
                                 NULL);
   /* removed "(Cardinal *)" cast from the &argc parameter -- lwk (Jul2006) */
 
   if ( toplevel == NULL) {
      zvmessage ("Failed to create 'toplevel' widget","");
      return FAIL;
   }

   XtVaSetValues (toplevel, 
                  XmNtitle,           "An XRT/graph & PostScript Application",
                  NULL);

   /* In main proghram is in BATCH or command line indicates NODISP */
   if (WaitNoMore) {
   XtVaSetValues (toplevel, 
				  XmNmappedWhenManaged, False,
                  NULL);
   } else {
   XtVaSetValues (toplevel, 
				  XmNmappedWhenManaged, True,
                  NULL);
   }
   return PASS;
}

/* ========================================================================== */

static int SetupXrtGraph (WXPtr)
WX *WXPtr;
{
XrtDataHandle *XrtDataHandlePtr;
Widget   graph;
int      ii;
   /* Allocate an empty XrtData area.  The accumulated points will be 
   appended to the end of the 'general' XrtData set as each point is 
   specified. */
  
   maxpoints = 65535;
   maxsets = 11;

   XrtDataHandlePtr = NULL;
   XrtDataHandlePtr = XrtDataCreate (XRT_DATA_GENERAL, maxsets, maxpoints); 
   if (!XrtDataHandlePtr) {
      zvmessage (msg,"Unable to create XrtData area ... exiting","");
      return FAIL;
   }
   for (ii = 0; ii < maxsets; ii++) {
      npoints[ii] = 0;         /* Initialize points number */
   }
		 
   /* Create the graph manager widget */
   graph = NULL;
   graph = XtVaCreateManagedWidget
            ("Graph", 
              xtXrtGraphWidgetClass,        form,  
              XtNxrtData,                   XrtDataHandlePtr,
              XtNxrtType,                   XRT_TYPE_PLOT,
              XtNwidth,                     XrtWidgetWidth,
              XtNheight,                    XrtWidgetHeight,
              XtNxrtDataStyles,             DataStylesPtr, 

              /* Constraint Resources */
              XmNmappedWhenManaged,         True, 
              XmNtopWidget,                 rbox,              
              XmNtopAttachment,             XmATTACH_WIDGET,
              XmNleftAttachment,            XmATTACH_FORM,
              XmNrightAttachment,           XmATTACH_FORM,
              XmNbottomAttachment,          XmATTACH_FORM,
              NULL);

   if (graph == NULL) {
      zvmessage ("Error return from XtVaCreateManagedWidget",""); 
      return FAIL;
   }

   XtVaSetValues (graph, 
              XtNxrtRepaint,                False, 
              NULL);

   /* Return allocated values */
   WXPtr->XrtDataHandlePtr = XrtDataHandlePtr;

   WXPtr->graph      = graph;

   return PASS;
}
	
/* ========================================================================== */

/* ZrtPlots () Initializes graph program and graph variables. */

int ZrtPlots (isf, indexd, idev)
int isf, indexd, idev;
{
int status;
   status = ZrtBegin ();
   return status;
}

/* ========================================================================== */

/* ZrtBegin () Initializes graph program and graph variables. */

int ZrtBegin (void)
{

int i, count, status;
char string [] = "       "; 
Boolean wait;

   /* xrtps will run without the generated graphs going to the display 
   units by entering into 'batch' mode or by putting the 'NODISP keyword
   into the program pdf files and entering the 'NODISP on the VICAR 
   command line.  In the 'WAITNOMORE' mode, all graph imagess will be 
   save in the PostScript image file. */

   if (zbatch() == 1 || zvptst("NODISP")) {  
      WaitNoMore = TRUE;
   } else {
      WaitNoMore = FALSE;
   }


   XrtHeader   = NULL;
   XrtFooter   = NULL;
   XrtSetLabel = NULL;
   XrtXTitle   = NULL;
   XrtYTitle   = NULL;
   XrtY2Title  = NULL;

   PageNo      = 0;

   xoff        = 0.0;
   yoff        = 0.0;
   xscale      = 1.0;
   yscale      = 1.0;

   /* Select user supplied output PostScript file name or select default */
   if (ZrtInitDevice (PostScriptFileName) == FAIL) {
      zvmessage ("Call to ZrtInitDevice () failed","");
      return FAIL;
   }

   /* Open the output postscript file for write */     
   PS_fptr = fopen (PostScriptFileName, "w+");
   if (PS_fptr == NULL) {
      sprintf (msg,"Error: PostScript file: '%s', file open error", 
               PostScriptFileName);
      zvmessage (msg,"");
      return FAIL;
   }   

   /* Indicate that no PostScript images have been saved in output 
   PostScript file */
   FileSaved = 0;

   /* Initialize Xintrinsics  */
   if (SetupXIntrinsics () == FAIL) {
      zvmessage ("Call to SetupXIntrinsics () failed ","");
      return FAIL;
   }

   /* Initialize Form widget and 'page' and 'print' Radio Buttons  */
   if (SetupRadioButtons () == FAIL) {
      zvmessage ("Call to SetupRadioButtons () failed","");
      return FAIL;
   }

   /* Allocate space for 40 XrtDataHandlePtr pointers & graph widget IDs */
   WXPtrArray = (WX *) malloc ((size_t)(40 * sizeof(WX)));

   /* Put allocated memory pointer onto stack */
   ZrtPushStack ((char *)(WXPtrArray));

   WXPtr      = WXPtrArray;

   for (i = 0; i < PageNo; i++) {
       (WXPtr + i)->XrtDataHandlePtr = NULL;
       (WXPtr + i)->graph      = NULL;
   }

   /* Initiate call to SetupXrtGraph to allocate XrtData space and to 
   create a managed Widget 'graph' */
   if (SetupXrtGraph (WXPtr+PageNo) == FAIL) {
      zvmessage ("Call to SetupXrtGraph () failed","");
      return FAIL;
   }

   /* Initialize the global variables */
   graph      = (WXPtr + PageNo)->graph;
   XrtDataHandlePtr = (WXPtr + PageNo)->XrtDataHandlePtr;

   /* Widget must be realized (but not necessarily mapped to the display)
   before calling the XrtDrawPS function */

   XtRealizeWidget (toplevel);
   SetHeadersAndFooters ();

   pennum = 1;

   plotx = 0.0;
   ploty = 0.0;
   fact  = 1.0;

   return PASS;
}

/* ========================================================================== */

static int AttachText (DataSetNo, SetNo, PointNo, textptr)
int     DataSetNo, SetNo, PointNo;
char   *textptr;
{
int   status;
char *strings[2] = {NULL,NULL};

   strings[0] = textptr;

   text.position.data.type    = XRT_TEXT_ATTACH_DATA;
   text.position.data.dataset = DataSetNo;
   text.position.data.set     = SetNo;
   text.position.data.point   = PointNo;
   text.strings = strings;
   text.anchor  = XRT_SYMBOL_ANCHOR;
   text.offset  = 0.10;     /* offset anchor position */
   text.connected = False;  /* Do not connect line and do not display point */
   text.adjust = XRT_ADJUST_LEFT;
   text.fore_color = NULL;
   text.back_color = NULL;
   text.border = XRT_BORDER_NONE;
   text.border_width = 0;
   text.font = NULL;
   text.psfont = "courier";
   text.psfont_size = NULL;
   handle = 0;
   handle = XrtTextCreate (graph, &text); 

   return PASS;
}
	
/* ========================================================================== */

/* ZrtPlot () Draws a line to a specified point (x,y) or moves to point (x,y)
without drawing a line, depending upon 'action' requested */

int ZrtPlot (x, y, action)
float x;	/* Input */
float y;	/* Input */
int   action;	
            /* Input action =   3; Move to (x,y); Do not draw line
               Input action =   2; Move to (x,y); Draw line
               Input action =  -3; Move to (x,y); Set new origin
               Input action =  -2; Move to (x,y); Draw line & set new origin
               Input action =  10; Eject to new page
               Input action = 999; End of PostScript print - close device */
{
int   status;
float fx2, fy2;

   fx2 =  (xoff + xscale * x);
   fy2 =  (yoff + yscale * y);

   /* Switch on the action requested */
   switch (action) {

      case   3:           /* Move to (x,y) and do not draw line */
         /* Insert a 'hole' in the points list */
         status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, 
                                      0.0, XRT_HUGE_VAL);
         if (status == FAIL) {
           zvmessage ("Append XRT_HUGE_VAL point failed, case 3","");
         }

         /* Maintain number of points accumulated */
         npoints[activeset]++;

         /* Followed by new (x,y) point */
         status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, fx2, fy2);
         if (status == FAIL) {
           zvmessage ("Append point failed, case 3+","");
         }

         /* Maintain number of points accumulated */
         npoints[activeset]++;

         break;

      case   2:           /* Draw line to point (x,y) */
         /* Append new point */
         status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, fx2, fy2);
         if (status == 0) {
           zvmessage ("Append point failed, case 2","");
         }

         /* Maintain number of points accumulated */
         npoints[activeset]++;

         break;

      case  -3:           /* Move to (x,y) and set origin, do not draw line */
         /* Insert a 'hole' in the points list */
         status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, 

                                      0.0, XRT_HUGE_VAL);
         if (status == FAIL) {
           zvmessage ("Append point failed, case -3","");
         }

         /* Maintain number of points accumulated */
         npoints[activeset]++;

         /* Followed by the new point */
         status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, fx2, fy2);
         if (status == FAIL) {
           zvmessage ("Append point failed, -3+","");
         }

         /* Maintain number of points accumulated */
         npoints[activeset]++;

         /* Update new origin values */
         xoff = fx2;
         yoff = fy2;

         break;

      case  -2:           /* Draw line and move to (x,y) */
         /* Append new point */
         status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, fx2, fy2);
         if (status == FAIL) {
           zvmessage ("Append point failed, -2","");
         }

         /* Update new origin values */
         xoff = fx2;
         yoff = fy2;

         /* Maintain number of points accumulated */
         npoints[activeset]++;

         break;

      case  10:           /* Eject Page */
         status = ZrtPage();
         if (status == EXIT || status == FAIL)
             return status;
         break;

      case 999:           /* End of PostScript print. Close file and exit  */
         ZrtClose ();
         break;

      default:            /* None of the above */
         sprintf (msg,"Invalid plot request; %d, request #: %d, x: %f, y: %f",
                  action,npoints[activeset], x, y);
         zvmessage (msg,"");
         break;

   }
   /* Set point symbol in data set '1' to NONE */

   plotx = x;
   ploty = y;

   return PASS;
}
	
/* ========================================================================== */

/* ZrtClose () is called to terminate the Xrt/graph program, release all 
allocated memory, close the PostScript file and terminate the program 
in an orderly fashion. Formerly case 999 of plot.
*/ 

void ZrtClose (void)
{
   SetHeadersAndFooters ();
   ZrtCloseDevice();
   return;
}
/* ========================================================================== */

/* ZrtPage () is called to cause the graph being created to be displayed.
Once the graph has been displayed the graph may be saved for printing,
the program terminated, or page to the next graph.  If the program 
is terminated, a status of EXIT (999) will be returned. Otherwise a value of 
PASS will be returned.
*/

int ZrtPage ()
{
int status;
   xoff   = 0.0;
   yoff   = 0.0;
   xscale = 1.0;
   yscale = 1.0;
   plotx  = 0.0;
   ploty  = 0.0;
   fact   = 1.0;

   XtVaSetValues (graph, 
                  XtNxrtRepaint,                True, 
                  NULL);

   /* Eject page */
   status = ZrtPageDevice();

   if (status != PASS) {
      PerformHouseCleaning ();
      return status;
   }

   ZrtPageContinue ();

   return PASS;
}

/* ========================================================================== */

/* ZrtSymbolAnchor () Anchors the 'text' at specified location with respect
to location of symbol. The default is XRT_ANCHOR_BEST. */

void ZrtSymbolAnchor (anchor) 
XrtAnchor anchor;

/*   Valid anchor positions are:
         XRT_ANCHOR_NORTH                XRT_ANCHOR_SOUTH
         XRT_ANCHOR_EASTH                XRT_ANCHOR_WEST 
         XRT_ANCHOR_NORTHWEST            XRT_ANCHOR_SOUTHWEST
         XRT_ANCHOR_NORTHEAST            XRT_ANCHOR_SOUTHEAST
         and
         XRT_ANCHOR_HOME                 XRT_ANCHOR_BEST                   
*/
{
   XRT_SYMBOL_ANCHOR = anchor;
   return;
}
/* ========================================================================== */

/* ZrtSymbol () Plots the 'text' at position (x,y) with given height,
angle, and nchar number of characters. */

void ZrtSymbol (x, y, height, text, inteq, angle, nchar) 
float  x;		    /* Input */ 
float  y;		    /* Input */
float  height;      /* Input */
char  *text;		/* Input */
int    inteq;		/* Input */
float  angle;		/* Input */
int    nchar;		/* Input */
{

  /* Go to the text position by moving the pen */
  if (x != 999.0 && y != 999.0) {
     if (nchar <= -1) {
        ZrtPlot (x, y, 2);      /* Move to (x,y) and draw line */
     } else {
        ZrtPlot (x, y, 3);      /* Move to (x,y) and do not draw line */
     }
  }
 
  /* Create a new text area, and then attach text to the point using 
  XRT_TEXT_ATTACH_DATA */
  AttachText (dataset, activeset, npoints[activeset]-1, text);

  return;
}
	
/* ========================================================================== */

/* ZrtNumber () Plots the number 'fpn' at position (x,y) with given height
and angle, and ndec is a code telling how to format the number. 

If ndec is >= 0.0 then fpn is real.
If < 0.0, then fpn is an integer.

-1 then display 'fpn' as an integer value, otherwise display 'fpn' as a 
floating point number with ndec precision */

void ZrtNumber (x, y, height, fpn, angle, ndec)
  float x;		    /* Input */
  float y;		    /* Input */
  float height;		/* Input */
  float fpn;		/* Input */
  float angle;		/* Input */
  int   ndec;		/* Input */
{
int mag, nchar, intfpn; 
char string [132];

   /* Put the number (in the right format) in a string */
   if (fpn == 0.0)
      mag = 1;
   else if (fpn < 0.0)
      mag = MAX( (int)(log10((double)(fabs(fpn))))+1, 0);
   else
      mag = MAX( (int)(log10((double)(fabs(fpn)))), 0);

   if (ndec >= 0) {
      sprintf (string,"%*.*f",mag+ndec+2,ndec,fpn);
      nchar = mag+2+ndec;
   } else if (ndec == -1) {
      sprintf (string,"%*d",mag, NINT(fpn));
      nchar = mag+1;
   } else if (ndec < -1) {
      intfpn = NINT(fpn/((float)(pow(10,(-ndec-1)))));
      if (intfpn != 0) {
         mag = (int)log10((double)(abs(intfpn))) + 1;
         sprintf (string,"%*d",mag+1,intfpn);
         nchar = mag+1;
      } else {
         strcpy (string,"   ");
         nchar = 1;
      }
   }

   /* Call symbol to plot the string */
   ZrtSymbol (x,y, height, string, 0, angle, nchar);

   return;

}

/* ========================================================================== */

/* ZrtSetActiveSet () allows the application program to write into one of
two data sets, 0 or 1.  The specified set will stay active until changed.  */

void ZrtSetActiveSet (newactiveset)
int newactiveset;
{
                          
  if (newactiveset > 10 || newactiveset < 0) {
     zvmessage ("Error ... set number invalid","");
     return;
  }
  activeset = newactiveset; 

  return;
}
	
/* ========================================================================== */

/* ZrtWhere () */

void ZrtWhere  (rxpage, rypage, rfact)
float *rxpage;	/* Output */
float *rypage;	/* Output */
float *rfact;		/* output */
{
                          
  *rxpage = plotx;
  *rypage = ploty;
  *rfact  = fact;
  return;
}
	
/* ========================================================================== */

/* ZrtNewPen () Changes Pen???? */

void ZrtNewPen  (pen)
int pen;
{
  pennum = MIN (MAX(pen,0), 1);
  return;
}
	
/* ========================================================================== */

/* ZrtNewPlt () Clears the plot */

void ZrtNewPlt  (void)
{
/*
                        What is the significance of this function 

*/                          
  xoff   = 0.0;
  yoff   = 0.0;
  xscale = 1.0;
  yscale = 1.0;
  plotx  = 0.0;
  ploty  = 0.0;
  fact   = 1;

  return;
}
	
/* ========================================================================== */

/* Detach attached text areas */
static void DetachTextAreas (wxptr)
WX *wxptr;
{
int count, i;
XrtTextHandle  *list;
/* changed **list to *list in above -- lwk (Jul2006) */
XrtTextHandle   handle;

   /* Get count and list of pointers to attached text areas */
   count = XrtGetTextHandles (wxptr->graph, &list);
   /* restored "&" in previous line [removed by jaw] to prevent crash -- lwk */
   for (i = 0; i < count; i++) {
      handle = *(list + i);
      /* Detach text areas from this graph */
      XrtTextDestroy (WXPtr->graph, handle);
   }
   return;
}
 
/* ========================================================================== */

static void WaitXtAppProcessNextEvent ()
{

   /* Enter into an infinate loop, allow graph to be displayed, and wait
   for operator to press 'page' or 'print' */
   mask = XtIMAll;
   while (!ExitFlag && !SaveFlag && !PageFlag) {
      XtAppProcessEvent (app_context, mask);
   }

   return;
}

/* ========================================================================== */

/* ZrtPageDevice() outputs PostScript file to printer. */

static int ZrtPageDevice (void)
{
char ASCIIPageNo[5];
char string [128];
int  status;

/* 
  Upon receiving the PageDevice request the following actions will be performed:

  The XtAppProcessNextEvent will be called to wait for the operator to
  initiate the action of 'page' or 'print'.

  Common PAGE and PRINT processing

  A new empty data set area will be created with the XrtMakeData request.
  A new graph widget will be created with the XtVaCreateManagedWidget
    request.
  Any additional data will be set for the graph widget via the XtVaSetValues
    request.
*/  

   /* Boost Page number */
   PageNo++;

   /* Set Page and print PS to not requested */
   ExitFlag = FALSE; 
   SaveFlag = FALSE; 
   PageFlag = FALSE;
   BigLoop  = TRUE; 
   status   = PASS;

   while (BigLoop) {

      if (WaitNoMore) {
         BigLoop = FALSE;
         SaveFlag = TRUE;
      } else {
         /* Invoke XtAppProcessNextEvent to wait for operator action to 
         exit, goto next page, or save currently displayed page */
         WaitXtAppProcessNextEvent ();
      }

      /* If this is a 'exit' out of xrtps () then */
      if (ExitFlag == TRUE) { 
         BigLoop = FALSE;
         status = EXIT;
         break;
      } else 
   
      /* If this is a 'page' to next page request then */
      if (PageFlag == TRUE) { 
         BigLoop = FALSE;
         break;
      } else 
   
      /* If this is a PostScript file print request then */
      if (SaveFlag == TRUE) { 
         BigLoop = FALSE;
         /* Create graph & PostScript output; an instance of a widget */
         MakePostScriptFile (PostScriptFileName);
      }
   }
   return status;
}

/* ========================================================================== */

/* ZrtPageContinue ()  is called to set up for the 'next' graph page following
   the ZrtPageDevice request */

static int ZrtPageContinue (void)
{
   /* Processing for 'page' and 'print' requests 
   A new empty data set area will be created with the XrtMakeData request.
   A new graph widget will be created with the XtVaCreateManagedWidget
   request.
   Any additional data will be set for the graph widget via the XtVaSetValues
   request. */

   PageContinue = TRUE;

   /* Initiate call to SetupXrtGraph to allocate XrtData space and to 
   create a managed Widget 'graph' */
   if (SetupXrtGraph ((WXPtr+PageNo)) == FAIL) {
      zvmessage ("Call to SetupXrtGraph () failed","");
      return FAIL;
   }

   /* Initialize global variables */
   graph      = (WXPtr + PageNo)->graph;
   XrtDataHandlePtr = (WXPtr + PageNo)->XrtDataHandlePtr;

   return PASS;
}

/* ========================================================================== */

/* Save all allocated memory pointers; allocated from all subroutines in this 
package and from the fortran bridge*/
void ZrtPushStack (StackPtr)
char *StackPtr;
{
int i;

   if (FreeStackIndex >= sizeof(FreeStackPtrs)/sizeof (char *) ) {
      sprintf (msg, "Array of MALLOC pointers exceeds limits: %d",
               FreeStackIndex); 
      zvmessage (msg,"");
      return;
   } else {
      FreeStackPtrs [FreeStackIndex++] = StackPtr;
   }

   return;
}

/* ========================================================================== */

/* Free all allocated memory; allocated from all subroutines in this 
package and from the fortran bridge*/
void ZrtFreeStack (void)
{
int i;

   if (FreeStackIndex == 0) {
      return;
   } else {
      for (i = 0; i < FreeStackIndex; i++) {
         free (FreeStackPtrs[i]);
      }
      return;
   }
}

/* ========================================================================== */

/* PerformHouseCleaning () performs the end-of-process for xrtps ().
Closes all files, frees allocated memory etc. */

static void PerformHouseCleaning () 
{
int i;

   /* Close the output PostScript file */     
   fclose (PS_fptr);
   PS_fptr = NULL;

   /* If PostScript images have not been written to output file then remove */
   if (FileSaved == 0) {
     remove (PostScriptFileName);
   }

   /* Delete all acquired data ... if done too quickly this will destroy 
   XrtData before the PostScript file can be created for output */

   /* Detach all text areas */
   DetachTextAreas (WXPtr);
   if (PageNo != 0) {
      for (i = 0; i < PageNo-1; i++) {
         /* Destroy all point data */
         XrtDataDestroy ((WXPtr + i)->XrtDataHandlePtr);
      }
   }

   /* Free memory allocated from all subroutines in zcxrtps package */
   ZrtFreeStack ();
   return;
}

/* ========================================================================== */

/* ZrtCloseDevice () Outputs the encapsulated PostScript (EPSF) representation
of the graph to the PostScript file. */

static void ZrtCloseDevice (void)
{
XEvent *event_return;
char    ASCIIPageNo[5];
char    string [128];
int     i, status;

   /* Bump page number */
   PageNo++;

   /* Set SaveFlag to print not requested */
   SaveFlag = FALSE;
   PageFlag  = FALSE;
   ExitFlag  = FALSE;
   status = PASS;

   XtVaSetValues (graph, 
                  XtNxrtRepaint,                True, 
                  NULL);

   /* Invoke XtAppProcessEvent to wait for operator action */
   BigLoop = TRUE;
   while (BigLoop) {

      if (WaitNoMore) {
         BigLoop = FALSE;
         SaveFlag = TRUE;
      } else {
         /* Invoke XtAppProcessNextEvent to wait for operator action to 
         print currently displayed page or page to next graph */
         WaitXtAppProcessNextEvent ();
      }
      /* If the EXIT flag is set then exit */
      if (ExitFlag == TRUE) {
         BigLoop = FALSE;
         status = EXIT;
         break;
      } else 
      /* If this is a PostScript file print request then */
      if (PageFlag == TRUE) { 
         BigLoop = FALSE;
         break;
      } else 
   
      /* If this is a PostScript file print request then */
      if (SaveFlag == TRUE) { 
         BigLoop = FALSE;

         /* Create graph & PostScript output; an instance of a widget */
         MakePostScriptFile (PostScriptFileName);

         /* Exit program at conclusion of XrtDrawPS request */
         break;
      }
   } /* End while (BigLoop) */

   PerformHouseCleaning ();

   return;
}
	
/* ========================================================================== */

static void MakePostScriptFile (filename)
char *filename;
{   
int status;

  /* Output the encapsulated PostScript (EPSF) representation, described by
  Widget PS_graph to stream file pointed to by PS_fptr, set color to 'FALSE' */

  /* Initialize data required for drawing the encapsulated PostScript file */
  inches       = True;
  paper_width  = PAPER_WIDTH;
  paper_height = PAPER_HEIGHT;
  margin       = PAPER_MARGIN;
  landscape    = XrtLandScape; /* Print with landscape orientation */
  x_offset     =  0.0;  /* Start from lower left corner of image at x_scale */
  y_offset     =  0.0;  /* Start from lower left corner of image at y_scale */
  width        =  0.0;	/* 0.0 allows image to print as large as possible */
  height       =  0.0;	/* 0.0 allows image to print as large as possible */
  header_font  = PSFont;
  header_size  = 12;			/* Font size is ignored */
  footer_font  = PSFont;
  footer_size  = 12;
  anno_font    = PSFont;
  anno_size    = 12;
  legend_font  = PSFont;
  legend_size  = 12;
  fill_background = False;
  smart_mono   = True;
  showpage     = True;

   /* Initiate call to rcceatPostScript file. If an error is detected with 
   EPSF output, then output error message */
   status = XrtDrawPS (graph, 
                 PS_fptr, 
                 msg,
                 inches,
                 paper_width,
                 paper_height,
                 margin,
                 landscape,
                 x_offset,
                 y_offset,
                 width,
                 height,
                 header_font,
                 header_size,
                 footer_font,
                 footer_size,
                 anno_font,
                 anno_size,
                 legend_font,
                 legend_size,
                 fill_background,
                 smart_mono,
                 showpage );
    if (status == 0) {
       /* Display error message returned by ZrtDrawPS */
       zvmessage (msg,"");
    }
    /* Indicate PostScript image has been entered into output PostScript File */
    FileSaved++;

    return;
}

/*  ====================================================================== */

/*  ZrtInitDevice () Opens output PostScript file. */

static int ZrtInitDevice (filename)
char *filename;
{
char *ptr;
int   length;

   /* If a name has not been provided for the output PostScript File,
   then use the default filename */
   if (PostScriptFileNameIsSet != TRUE) {
      strcpy (PostScriptFileName, PostScriptDefaultFileName);
   }

   return PASS;
}
	
/* ========================================================================== */

/* ZrtDrawLine () draws line from (x1,y1) to (x2,y2) */

void ZrtDrawLine (x1, y1, x2, y2)
  int x1;		/* Input */
  int y1;		/* Input */
  int x2;		/* Input */
  int y2;		/* Input */
{
int status;

  /* Append new point */
  status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, (float)x1, (float)y1);


  /* Maintain number of points accumulated */
  npoints[activeset]++;

  /* Append new point */
  status = XrtGenDataAppendPt (XrtDataHandlePtr, activeset, (float)x2, (float)y2);

  /* Maintain number of points accumulated */
  npoints[activeset]++;

  return;
}
	
/* ========================================================================== */

/* ZrtFactor () ........ */

void ZrtFactor(newfact)
float newfact;
{
   xscale = newfact * xscale / fact;
   yscale = newfact * yscale / fact;
   fact = newfact;

   return;
}
	
/* ========================================================================== */

/* ZrtScale () Calculates the scale factors for the data in the array and puts
   scale factors at the end of the array */

void ZrtScale (array, axlen, npts, inc)
float array[];
float axlen;
int   npts, inc;
{
int   i;
float araymin, araymax, firstv, deltav, expon, mantis;
double Dtemp, DDtemp;

   araymin =  1.0e38;
   araymax = -1.0e38;
   for (i = 0; i < npts * abs(inc); i += abs(inc)) {
      araymin = MIN (araymin, array[i]);
      araymax = MAX (araymax, array[i]);
   }

   if (inc > 0) {
      firstv = araymin;
      deltav = (araymax-araymin)/axlen;
   } else {
      firstv = araymax;
      deltav = (araymin-araymax)/axlen;
   } 

   if (deltav == 0.0) deltav = 1.0;
   
   expon  = (float) log10 (fabs((double)deltav));
   Dtemp  = modf ((double)expon, &DDtemp);
   mantis = (float) pow (10.0, Dtemp);

   if (mantis < 1.0) {
       mantis = 10.0 * mantis;
       expon = expon - 1.0;
   }

   if (mantis < 2.0) 
      mantis = 2.0;
   else if (mantis < 4.0) 
      mantis = 4.0;
   else if (mantis < 5.0)
      mantis = 5.0;
   else if (mantis < 8.0)
      mantis = 8.0;
   else if (mantis < 10.0)
      mantis = 10.0;

   Dtemp  = modf ((double)expon, &DDtemp);
   mantis = mantis * (float)(pow(10.0, DDtemp));

   /* deltav = sign (mantis, deltav) */
   if (deltav >= 0.0) {
      if (mantis >= 0.0) 
         deltav =  mantis;
      else
         deltav = -mantis;
   } else {
      if (mantis >= 0.0) 
         deltav = -mantis;
      else
         deltav =  mantis;
   }

   if (inc > 0) {
      if (firstv >= 0.0) 
         firstv = mantis * (float)((int)(firstv/mantis));
      else
         firstv = mantis * (float)((int)(firstv/mantis+0.001) - 1);
   } else {
      if (firstv >= 0.0)
         firstv = mantis * (float)((int)(firstv/mantis) + 1);
      else                                                   
         firstv = mantis * (float)((int)(firstv/mantis));
   }

   array [(npts+0) * abs(inc)] = firstv;
   array [(npts+1) * abs(inc)] = deltav;

   return;
}

/* ========================================================================== */

/* ZrtAxis () draws an annotated axis of length 'axlen' inches at an angle;
labels each tic mark every inch with a number, starting with 'firstv'
and with an increment of 'deltav'; labels the axis with the title. */

void ZrtAxis (x,y, title, nchar, axlen, angle, firstv,deltav)
float x, y, axlen, angle, firstv, deltav;
int   nchar;
char *title;
{
double radians, radians90, ddeltav, dfirstv;
char  temp [127], temp1 [12];
float xend,yend, xtic1,ytic1, xtic2,ytic2;
float ticlen, ticside, ticpos, textheight, textwidth;
float valxpos,valypos, valpos, valoffs;
float value, dspval;
float titlepos, titleoffs, titlexpos, titleypos;
int   valscale, valfscale, numchar;

   textheight = 0.12;
   textwidth  = textheight;
   ticlen     = 2.0/32.;
   ticside    = -1.0;

   /* +1 for tics on positive side */
   if (nchar >= 0)  ticside = 1.0; 
   numchar = abs (nchar);

   radians   = (double)((angle * 2.0 * 3.14159265)/360.0); 
   radians90 = (double)(((angle+90.0) * 2.0 * 3.14159265)/360.0); 

   /* draw the axis line */
   xend = x + axlen * (float)cos(radians);
   yend = y + axlen * (float)sin(radians);

   ZrtPlot (x, y, 3);
   ZrtPlot (xend, yend, 2);

   ddeltav = (double) deltav;
   dfirstv = (double) firstv;

   /* Find the scaling for the annotation values */
   valscale = (int)(log10((double)fabs(deltav))); 

   if (firstv != 0.0) {
      valfscale = (int)(log10((double)fabs(firstv)));
      if (valfscale >= 3) valscale = valfscale;
   }
   if (valscale >= -1 && valscale <= 1) {
      valscale = 0;
   }

   /* At each inch draw a tic and label the tic with a value */
   value = firstv;
   ticpos = 0.0;

   while (ticpos <= axlen) {
      xtic1 = x + ticpos * (float)cos(radians);
      ytic1 = y + ticpos * (float)sin(radians);
      xtic2 = xtic1 + ticlen * ticside * (float)cos((radians90));
      ytic2 = ytic1 + ticlen * ticside * (float)sin((radians90));

      ZrtPlot (xtic1, ytic1, 3);
      ZrtPlot (xtic2, ytic2, 2);

      valpos = -1.0 * textwidth;
      valoffs = ticside * (2.0 * ticlen + textheight * (1.0-ticside)/2.0 );

      /* If angle is zero ... working on X-Axis, otherwise Y-Axis */
      if (angle == 0.0) {
         valxpos = xtic1 + valoffs * (float)cos((radians90)) + 
                   valpos * (float)cos(radians);
         valypos = ytic1 + valoffs * (float)sin(radians90) + 
                   valpos * (float)sin(radians);
      } else {
         valxpos = xtic1 - 0.625;
         valypos = ytic1;
      }

      dspval =  value/((float)(pow(10.0, (double)valscale)));  
      ZrtNumber (valxpos, valypos, textheight, dspval, angle, 2);

      /* tic every inch */
      ticpos +=  1.0;

      value += deltav;
      if (ticpos > axlen) break;
   }

   /* Plot the axis title */
   if (valscale == 0) {
      titlepos = (axlen - numchar * textwidth )/2.0;
   } else {
      titlepos = (axlen - (numchar+8) * textwidth )/2.0;
   }

   titleoffs = ticside * (3.0 * ticlen + textheight * (3.0-ticside)/2.0);

   titlexpos = x + titlepos * (float)cos(radians) + 
               titleoffs * (float)cos(radians90);

   titleypos = y + titlepos * (float)sin(radians) + 
               titleoffs * (float)sin(radians90);

   ZrtSymbol (titlexpos, titleypos, textheight, title, 0, angle, numchar);

   /* If the axis labels are scaled then print the exponent value */
   if (valscale != 0.0) {
      strcpy (temp, "  *10**");
      sprintf (temp1,"%d",valscale);
      strcat  (temp,temp1);
      if (angle == 0.0) {
         titleypos = titleypos - textheight - 0.01;
      } else {
         titlexpos = titlexpos - 8 * textheight - 0.01;
         titleypos = titleypos - 1 * textheight - 0.01;
      } 
      ZrtSymbol (titlexpos, titleypos, textheight, temp , 0, angle, 7);
   }
   
   /* put pen back at beginning of axis */
   ZrtPlot (x, y, 3);

   return;
}

/* =========================================================================== */

/* ZrtLine () Graphs the points in an array */

void ZrtLine (xarray, yarray, npts, inc, lintyp, inteq)
float  xarray[];
float  yarray[];
int     npts;	
int     inc;		/* Input */
int     lintyp;		/* Input */
int     inteq;		/* Input */
{
int   II, JJ, i, symcount;
float x, y, xmin, ymin, xscale, yscale;

   /* Obtain scaling factors */
   xmin = xarray [npts * abs(inc)];
   ymin = yarray [npts * abs(inc)];

   xscale = 1.0 / xarray [npts * abs(inc) +1];
   yscale = 1.0 / yarray [npts * abs(inc) +1];

   x = (xarray[0] - xmin ) * xscale;
   y = (yarray[0] - ymin ) * yscale;

   ZrtPlot (x, y, 3);

   symcount = abs(lintyp);
   II = npts * abs(inc);
   JJ = abs(inc);
   for (i = 0; i < II; i += JJ) {
      x = (xarray[i] - xmin ) * xscale;
      y = (yarray[i] - ymin ) * yscale;

      if (lintyp >= 0 ) {
         ZrtPlot (x, y, 2);
      } else {
         ZrtPlot (x, y, 3);
      }
      if (lintyp != 0) {
         if (symcount == abs(lintyp)) {
            symcount = 0;
            ZrtSymbol (x, y, 0.12, " ", inteq, 0.0, -1);
         }
         symcount += 1;
      }
   }
   return;
}
/* =========================================================================== */

