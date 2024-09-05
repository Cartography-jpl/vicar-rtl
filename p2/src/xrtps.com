$!****************************************************************************
$!
$! Build proc for MIPL module xrtps
$! VPACK Version 1.9, Tuesday, July 25, 2006, 09:39:47
$!
$! Execute by entering:		$ @xrtps
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
$ write sys$output "*** module xrtps ***"
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
$ write sys$output "Invalid argument given to xrtps.com file -- ", primary
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
$   if F$SEARCH("xrtps.imake") .nes. ""
$   then
$      vimake xrtps
$      purge xrtps.bld
$   else
$      if F$SEARCH("xrtps.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xrtps
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xrtps.bld "STD"
$   else
$      @xrtps.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xrtps.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xrtps.com -mixed -
	-s zcxrtps.c zfxrtps.c -
	-i xrtps.imake -
	-t tcxrtps.c tfxrtps.f tcxrtps.imake tfxrtps.imake tcxrtps.pdf -
	   tfxrtps.pdf tstxrtps.pdf -
	-o xrtps.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create zcxrtps.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zfxrtps.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

void FTN_NAME (header) (char *for_string, int *nelements, int *adjustment,
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

void FTN_NAME (footer) (char *for_string, int *nelements, int *adjustment,
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

void FTN_NAME (setlabel)(char *for_string, int *nelements, int *orientation,
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

void FTN_NAME (newpen) (pen)
int *pen;
{
   ZrtNewPen (*pen);
   return;
}

/* ======================================================================= */

/* NEWPPLT () Clears the plot */

void FTN_NAME (newplt) (void)
{
   ZrtNewPlt ();
   return;
}

/* ======================================================================= */

/* LINE () Graphs the points in the arrays */

void FTN_NAME (line) (xarray, yarray, npts, inc, lintyp, inteq)
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

void FTN_NAME (number) (x, y, height, fpn, angle, ndec)
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

void FTN_NAME (symbolanchor)(anchor)
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

void FTN_NAME (symbol) (float *x, float *y, float *height, char *text,
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

void FTN_NAME (drawline)(x1, y1, x2, y2)
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

void FTN_NAME (scale)(array, axlen, npts, inc)
float *array;
float *axlen;
int   *npts, *inc;
{
  ZrtScale (array, *axlen, *npts, *inc);
  return;
}

/* ======================================================================= */


/* factor ()  */

void FTN_NAME (factor)(newfact)
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

void FTN_NAME (axis) (float *x, float *y, char *title, int *nchar,
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

void FTN_NAME (axestitles)(char *xtitle, char *ytitle, int *yrotation,
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

void FTN_NAME (setwidgetaspect) (xaxis, yaxis)
int *xaxis, *yaxis;
{
   ZrtSetWidgetAspect (*xaxis, *yaxis);
   return;
}

/* ======================================================================= */

/* ZrtSetGraphAspect () is called to set the aspect ratio for the annotated 
graph only. The default settings are; X=1200, and Y=900. To adjust the size 
of the whole graph widget, refer to ZrtSetWidgetAspect. */

void FTN_NAME (setgraphaspect) (xaxis, yaxis)
int *xaxis, *yaxis;
{
   ZrtSetGraphAspect (*xaxis, *yaxis);
   return;
}

/* ======================================================================= */

/* setlandscape () is called to set the printer output to either 
LandScape (TRUE) or to Portrait (FALSE).  Default is LandScape */

void FTN_NAME (setlandscape) (mode)
int *mode;
{
   ZrtSetLandScape (*mode);
   return;
}

/* ======================================================================= */

/* axesreverse () reverses the X and/or Y axis orientation.  If 'X' is TRUE:
the X axis annotation increases from top to bottom. If 'Y' is TRUE: the Y axis 
annotation will increase from right to left  */ 

void FTN_NAME (axesreverse) (xaxis, yaxis)
int *xaxis, *yaxis;
{
   ZrtAxesReverse (*xaxis, *yaxis);
   return;
}

/* ======================================================================= */

/* plotfn () is called to set the output PostScript filename. If the 
filename is not set, then the filename will default to 'postscript.plt'.*/

void FTN_NAME (plotfn) (char *filename, ZFORSTR_PARAM) 
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

void FTN_NAME (xrtbegin) (status)
int *status;
{
   *status = ZrtBegin ();
   return;
}

/* ======================================================================= */

/* plots () Initializes plotter (display) and plotter variables. */

void FTN_NAME (plots) (isf, indexd, idev)
int *isf, *indexd, *idev;
{
   ZrtBegin (*isf, *indexd, *idev);
   return;
}

/* ======================================================================= */

/* plot () Plots a line or moves the pen depending on 'action' requested */

void FTN_NAME (plot) (x, y, action)
float *x;
float *y;
int   *action;		/* Input */
{

   ZrtPlot (*x, *y, *action);
   return;
}

/* ======================================================================= */

/* WHERE () Returns the coordinate position of the pen */

void FTN_NAME (where) (rxpage, rypage, rfact)
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


void FTN_NAME (displayaxes) (xaxis, yaxis, y2axis)
int *xaxis, *yaxis, *y2axis; 
{
   ZrtDisplayXrtAxis (*xaxis, *yaxis, *y2axis);
   return;
}


/* ======================================================================= */

/* ZrtAxesOrigin () is called to specify where the axes should be rendered.
For example, setting the X origin to 5.0  will cause the Y axis to cross the
X-axis at X=5.0 */

void FTN_NAME (axesorigin) (xorigin, yorigin)
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


void FTN_NAME (axesoriginplacement) (xorigin, yorigin)
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

void FTN_NAME (setaxesmaximums) (xaxis, yaxis, y2axis)
float *xaxis, *yaxis, *y2axis;
{
   ZrtSetAxesMaximums (*xaxis, *yaxis, *y2axis);
   return;
}

/* ========================================================================== */

/* ZrtSetAxesMinimums () is called to specify the minimum values for the axes.
By setting these values, graph data may be clipped and not displayed.
*/

void FTN_NAME (setaxesminimums) (xaxis, yaxis, y2axis)
float *xaxis, *yaxis, *y2axis;
{
   ZrtSetAxesMinimums (*xaxis, *yaxis, *y2axis);
   return;
}

/* ======================================================================= */

/* ZrtSetDataMaximums () is called to specify the maximum values for the data.
By setting these values, graph data may be clipped and not displayed.
*/

void FTN_NAME (setdatamaximums) (xaxis, yaxis, y2axis)
float *xaxis, *yaxis, *y2axis;
{
   ZrtSetDataMaximums (*xaxis, *yaxis, *y2axis);
   return;
}

/* ========================================================================== */

/* ZrtSetDataMinimums () is called to specify the minimum values for the data.
By setting these values, graph data may be clipped and not displayed.
*/

void FTN_NAME (setdataminimums) (xaxis, yaxis, y2axis)
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

void FTN_NAME (xrtpage) (status)
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

void FTN_NAME (setactiveset) (set)
int *set;
{
   ZrtSetActiveSet (*set);
   return;
}

/* ========================================================================== */

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xrtps.imake
/* Imake file for VICAR subroutine xrtps */
#define SUBROUTINE xrtps

#define MODULE_LIST zcxrtps.c zfxrtps.c 
#define MAIN_LANG_C
#define P2_SUBLIB
#define FTN_STRING
/* define USES_C -jaw - needs ANSI_C */ 
#define USES_ANSI_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define LIB_MOTIF
#define LIB_XRT_GRAPH
#define LIB_XRT_3D
#define LIB_P2SUB
$ Return
$!#############################################################################
$Test_File:
$ create tcxrtps.c

#include <stdio.h>
#include "vicmain_c"
#include "ftnbridge.h"
#include <string.h>
#include <math.h>
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
/*#include "xrt_dfloat.h" -jaw - only needed for unsupported VMS */



/************************************************************************/
/*   10 July 1995 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)         */
/************************************************************************/



static char  text [132];
  
#define PASS 1
#define FAIL 0
#define TRUE 1
#define FALSE 0


void main44()
{

int   i, nchar, inteq, ndec;
float x, y, height, angle, fpn;
double radians;
double radius;
double Pi = 3.1415926535;

char *header1 [] = {"Test XRTPS & XRT/graph Using 'C' Interface",
                   "C Language Header Text Line 2 Page 1",
                   "C Language Header Text Line 3 Page 1",
                    NULL};
char *header2[] = {"Header Text Line 1 Page 2",
                   "Header Text Line 2 Page 2",
                   "Header Text Line 3 Page 2",
                    NULL};
char *header3[] = {"Header Text Line 1 Page 3",
                   "Header Text Line 2 Page 3",
                   "Header Text Line 3 Page 3",
                    NULL};

char *footer1 [] = {"Footer Text Line 1 Page 1",
                    "Footer Text Line 2 Page 1",
                    "Footer Text Line 3 Page 1",
                     NULL};
char *footer2[] = {"Footer Text Line 1 Page 2",
                   "Footer Text Line 2 Page 2",
                   "Footer Text Line 3 Page 2",
                    NULL};

char *footer3[] = {"Footer Text Line 1 Page 3",
                   "Footer Text Line 2 Page 3",
                   "Footer Text Line 3 Page 3",
                    NULL};

char *setlabel1 [] = {"Data Set  0 Group 1",
                      "Data Set  1 Group 1", 
                      "Data Set  2 Group 1", 
                      "Data Set  3 Group 1", 
                      "Data Set  4 Group 1", 
                      "Data Set  5 Group 1", 
                      "Data Set  6 Group 1", 
                      "Data Set  7 Group 1", 
                      "Data Set  8 Group 1", 
                      "Data Set  9 Group 1", 
                      "Data Set 10 Group 1", 
                       NULL};
char *setlabel2 [] = {"Data Set  0 Group 2",
                      "Data Set  1 Group 2", 
                      "Data Set  2 Group 2", 
                      "Data Set  3 Group 2", 
                      "Data Set  4 Group 2", 
                      "Data Set  5 Group 2", 
                      "Data Set  6 Group 2", 
                      "Data Set  7 Group 2", 
                      "Data Set  8 Group 2", 
                      "Data Set  9 Group 2", 
                      "Data Set 10 Group 2", 
                       NULL};
char *setlabel3 [] = {"Data Set  0 Group 3",
                      "Data Set  1 Group 3", 
                      "Data Set  2 Group 3", 
                      "Data Set  3 Group 3", 
                      "Data Set  4 Group 3", 
                      "Data Set  5 Group 3", 
                      "Data Set  6 Group 3", 
                      "Data Set  7 Group 3", 
                      "Data Set  8 Group 3", 
                      "Data Set  9 Group 3", 
                      "Data Set 10 Group 3", 
                       NULL};
char *xaxistitle1 = "X-Axis Title Page 1";
char *xaxistitle2 = "X-Axis Title Page 2";
char *xaxistitle3 = "X-Axis Title Page 3";

char *yaxistitle1 = "Y-Axis Title Page 1 ... 90 degrees";
char *yaxistitle2 = "Y-Axis Title Page 2 ... 270 degrees";
char *yaxistitle3 = "Y-Axis Title Page 3 ... 90 degrees";

char *y2axistitle1 = "Y2-Axis Title Page 1 ... 90 degrees";
char *y2axistitle2 = "Y2-Axis Title Page 2 ... 270 degrees";
char *y2axistitle3 = "Y2-Axis Title Page 3 ... 90 degrees";
float NO;

/* 
      Example: ZrtPlot (x, y, Input Action);
      Input Action =   3; Move to (x,y); Do not draw line
      Input Action =   2; Move to (x,y); Draw line
      Input Action =  -3; Move to (x,y); Do not draw line & Set new origin
      Input Action =  -2; Move to (x,y); Draw line & set new origin
      Input Action =  10; Eject to new page
      Input Action = 999; End of PostScript print - close device 
*/

   zifmessage ("TCXRTPS version 10 July 95");

   Pi     = 3.1415926535;
   height = 1.5;
   angle  = 0.0;
   ndec   = 3;
   fpn    = 12.345;
   inteq  = 0;

   /* Specify output PostScript file */
   if (ZrtPlotFn ("tstcxrtps.psf") == FAIL) 
      return;

   /* Initialize XRTPS and XRT/graph PostScript Function */
   if (ZrtBegin () == !PASS) {
      return;
   }

   /* Switch to data set 1 and plot with point symbol display */
   ZrtSetActiveSet (1);
   ZrtHeader (header1,XRT_ADJUST_LEFT);
   ZrtFooter (footer1,XRT_ADJUST_LEFT);
   ZrtDisplayXrtAxis (TRUE, TRUE, TRUE);
   ZrtSetLabel (setlabel1, XRT_VERTICAL, XRT_ANCHOR_NORTHWEST); 

   ZrtAxesTitles (xaxistitle1, 
                  yaxistitle1, XRT_ROTATE_90,
                  y2axistitle1,XRT_ROTATE_90);

   ZrtSetLandScape (TRUE);
   ZrtSetWidgetAspect (1024,1024);  /* Set Display to 1024 * 1024 pixels */ 
                                    /* Xmax=1280,Ymax=1024 */ 

   for (i = 1; i <= 11; i++) {
      /* Switch to data set 0 and plot without point symbol display */
      ZrtSetActiveSet (i-1);
      /* Setup new origin */
      NO = (float) i;
      ZrtPlot   ( 0.5,NO * 1.0, -3); /* Move to (x,y) and set origin */
      ZrtPlot   (10.0,NO * 3.0,  2); /* Move to (x,y) and draw line */
   }

   /* Switch to data set 0 and plot without point symbol display */
   ZrtSetActiveSet (0);

   /* Setup new origin */
   ZrtPlot (6.0, 8.0, -3); 		    /* Move to (x,y) and set origin */

   /* Switch to data set 0 and plot without point symbol display */
   ZrtSetActiveSet (0);

   /* Display graph image */
   if (ZrtPage() != PASS)
      return;

   /* Start new graph image #2 */

   /* Switch to data set 1 and plot with point symbol display */
   ZrtSetActiveSet (1);

   ZrtHeader (header2,XRT_ADJUST_RIGHT);
   ZrtFooter (footer2,XRT_ADJUST_RIGHT);
   ZrtDisplayXrtAxis (TRUE, TRUE, TRUE);
   ZrtSetLabel (setlabel2, XRT_VERTICAL, XRT_ANCHOR_NORTHWEST); 

   ZrtAxesTitles (xaxistitle2, 
                  yaxistitle2, XRT_ROTATE_270,
                  y2axistitle2,XRT_ROTATE_270);

   ZrtAxesReverse (FALSE, TRUE);        /* Set X to normal. Set Y to reverse */

   /* Setup new origin */ 
   ZrtPlot   (0.0, 0.0, -3); 		    /* Move to (x,y) and set origin */

   ZrtSetActiveSet (1);
   ZrtSymbol ( 3.0,  4.0, height, "P1", inteq, angle, 2);
   ZrtSymbol ( 7.0, 14.0, height, "P2", inteq, angle, 2);
   ZrtSymbol ( 9.0, 24.0, height, "P3", inteq, angle, 2);
   ZrtSymbol (13.0,  4.0, height, "P4", inteq, angle, 2);
   ZrtSymbol (33.0, 10.0, height, "P5", inteq, angle, 2);

   ZrtSetActiveSet (5);
   ZrtPlot  ( 15.0, 6.0, -3); 		/* Move to (x,y), set new origin */
   ZrtPlot  (  4.0, 4.0,  2); 		/* Move to (x,y) & draw line */
   ZrtPlot  ( 15.0,30.0,  2); 		/* Move to (x,y) & draw line */
   ZrtPlot  ( 30.0,20.0,  2); 		/* Move to (x,y) & draw line */
   ZrtPlot  (-10.0,10.0,  2); 		/* Move to (x,y) & draw line */
   ZrtPlot  (  0.0, 0.0,  2);       /* Move to (x,y) & do not draw line */

   ZrtSetActiveSet (6);
   ZrtPlot   (0.0, 7.0,  -2); 		/* Move to (x,y), draw line, set origin */

   ZrtSymbol ( 7.0, 14.0, height, "P6", inteq, angle, 2);
   ZrtSymbol ( 9.0, 24.0, height, "P7", inteq, angle, 2);
   ZrtSymbol (13.0,  4.0, height, "P8", inteq, angle, 2);

   ZrtNumber (10.0, 10.0, height,10.12345, angle, 3);
   ZrtNumber (20.0, 20.0, height,20.12345, angle,-1);
   ZrtNumber (30.0, 30.0, height,30.12345, angle, 3);
   ZrtNumber (40.0, 40.0, height,40.12345, angle,-1);

   if (ZrtPage() != PASS)
      return;

   /* Start new graph image #3 */

   /* Switch to data set 1 and plot with point symbol display */
   ZrtSetActiveSet (1);
   ZrtAxesReverse (FALSE, FALSE);        /* Set X to normal. Set Y to reverse */
   ZrtHeader (header3,XRT_ADJUST_CENTER);
   ZrtFooter (footer3,XRT_ADJUST_CENTER);
   ZrtAxesTitles (xaxistitle3, 
                  yaxistitle3, XRT_ROTATE_90,
                  y2axistitle3,XRT_ROTATE_90);
   ZrtSetLabel (setlabel3, XRT_VERTICAL, XRT_ANCHOR_SOUTHEAST); 


   ZrtDisplayXrtAxis ( FALSE, TRUE, TRUE); /* suppress X, display Y & Y2 */

   ZrtPlot   ( 0.0,  0.0, -3); 		    /* Move pen to (x,y) and draw line */

   ZrtSymbol ( 0.0,  0.0, height, "P100", inteq, angle, -1);
   ZrtSymbol ( 2.0,  2.0, height, "P101", inteq, angle, -1);
   ZrtSymbol ( 4.0,  4.0, height, "P102", inteq, angle, -1);
   ZrtSymbol ( 6.0,  8.0, height, "P103", inteq, angle, -1);
   ZrtSymbol ( 8.0, 16.0, height, "P104", inteq, angle, -1);
   ZrtSymbol (10.0, 32.0, height, "P105", inteq, angle, -1);
   ZrtSymbol (12.0, 64.0, height, "P106", inteq, angle, -1);
   ZrtSymbol (14.0,128.0, height, "P107", inteq, angle, -1);

   if (ZrtPage() != PASS)
      return;

   /* Start new graph image #4 */

   /* Switch to data set 1 and plot with point symbol display */
   ZrtSetActiveSet (1);
   ZrtAxesReverse (FALSE, FALSE);        /* Set X to normal. Set Y to reverse */
   ZrtHeader (header3,XRT_ADJUST_CENTER);
   ZrtFooter (footer3,XRT_ADJUST_CENTER);
   ZrtAxesTitles (xaxistitle3, 
                  yaxistitle3, XRT_ROTATE_90,
                  y2axistitle3,XRT_ROTATE_90);
   ZrtSetLabel (setlabel3, XRT_VERTICAL, XRT_ANCHOR_SOUTHEAST); 


   ZrtDisplayXrtAxis ( FALSE, TRUE, TRUE); /* suppress X, display Y & Y2 */

   ZrtSetGraphAspect (600, 450);

   ZrtPlot   ( 0.0,  0.0, -3); 		    /* Move pen to (x,y) and draw line */

   ZrtSymbol ( 0.0,  0.0, height, "P100", inteq, angle, -1);
   ZrtSymbol ( 2.0,  2.0, height, "P101", inteq, angle, -1);
   ZrtSymbol ( 4.0,  4.0, height, "P102", inteq, angle, -1);
   ZrtSymbol ( 6.0,  8.0, height, "P103", inteq, angle, -1);
   ZrtSymbol ( 8.0, 16.0, height, "P104", inteq, angle, -1);
   ZrtSymbol (10.0, 32.0, height, "P105", inteq, angle, -1);
   ZrtSymbol (12.0, 64.0, height, "P106", inteq, angle, -1);
   ZrtSymbol (14.0,128.0, height, "P107", inteq, angle, -1);

   /* Close XRT/graph */
   ZrtClose ();		/* Close device */ 

   return;
}
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
$!-----------------------------------------------------------------------------
$ create tfxrtps.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

! Fortran test driver for the XRTPS bridge and XRT/graph 

      integer  status, inteq, ndec
      real     height, angle, fpn, NO

      character*80 header1(3)
      character*80 footer1(3)
      character*80 slabels1(11)
      character*80 xaxistitle1
      character*80 yaxistitle1
      character*80 y2axistitle1
      
      character*80 header2(3)
      character*80 footer2(3)
      character*80 slabels2(11)
      character*80 xaxistitle2
      character*80 yaxistitle2
      character*80 y2axistitle2
      
      character*80 header3(3)
      character*80 footer3(3)
      character*80 slabels3(11)
      character*80 xaxistitle3
      character*80 yaxistitle3
      character*80 y2axistitle3

      call xvmessage ('TFXRTPS version 10 July 95',' ') 

      header1(1)  =  'Test XRTPS & XRT/graph Using ''Fortran'' Bridge'
      header1(2)  =  'Fortran Language Header Text Line 2 Page 1'
      header1(3)  =  'Fortran Language Header Text Line 3 Page 1'

      header2(1)  =  'Header Text Line 1 Page 2'
      header2(2)  =  'Header Text Line 2 Page 2'
      header2(3)  =  'Header Text Line 3 Page 2'

      header3(1)  =  'Header Text Line 1 Page 3'
      header3(2)  =  'Header Text Line 2 Page 3'
      header3(3)  =  'Header Text Line 3 Page 3'

      footer1(1)  =  'Footer Text Line 1 Page 1'
      footer1(2)  =  'Footer Text Line 2 Page 1'
      footer1(3)  =  'Footer Text Line 3 Page 1'

      footer2(1)  =  'Footer Text Line 1 Page 2'
      footer2(2)  =  'Footer Text Line 2 Page 2'
      footer2(3)  =  'Footer Text Line 3 Page 2'

      footer3(1)  =  'Footer Text Line 1 Page 3'
      footer3(2)  =  'Footer Text Line 2 Page 3'
      footer3(3)  =  'Footer Text Line 3 Page 3'

      slabels1(1)  =  'Data Set  0 Group 1'
      slabels1(2)  =  'Data Set  1 Group 1'
      slabels1(3)  =  'Data Set  2 Group 1'
      slabels1(4)  =  'Data Set  3 Group 1'
      slabels1(5)  =  'Data Set  4 Group 1'
      slabels1(6)  =  'Data Set  5 Group 1'
      slabels1(7)  =  'Data Set  6 Group 1'
      slabels1(8)  =  'Data Set  7 Group 1'
      slabels1(9)  =  'Data Set  8 Group 1'
      slabels1(10) =  'Data Set  9 Group 1'
      slabels1(11) =  'Data Set 10 Group 1'

      slabels2(1)  =  'Data Set  0 Group 2'
      slabels2(2)  =  'Data Set  1 Group 2'
      slabels2(3)  =  'Data Set  2 Group 2'
      slabels2(4)  =  'Data Set  3 Group 2'
      slabels2(5)  =  'Data Set  4 Group 2'
      slabels2(6)  =  'Data Set  5 Group 2'
      slabels2(7)  =  'Data Set  6 Group 2'
      slabels2(8)  =  'Data Set  7 Group 2'
      slabels2(9)  =  'Data Set  8 Group 2'
      slabels2(10) =  'Data Set  9 Group 2'
      slabels2(11) =  'Data Set 10 Group 2'

      slabels3(1)  =  'Data Set  0 Group 3'
      slabels3(2)  =  'Data Set  1 Group 3'
      slabels3(3)  =  'Data Set  2 Group 3'
      slabels3(4)  =  'Data Set  3 Group 3'
      slabels3(5)  =  'Data Set  4 Group 3'
      slabels3(6)  =  'Data Set  5 Group 3'
      slabels3(7)  =  'Data Set  6 Group 3'
      slabels3(8)  =  'Data Set  6 Group 3'
      slabels3(9)  =  'Data Set  7 Group 3'
      slabels3(10) =  'Data Set  9 Group 3'
      slabels3(11) =  'Data Set 10 Group 3'

      xaxistitle1 = 'X-Axis Title Page 1'
      xaxistitle2 = 'X-Axis Title Page 2'
      xaxistitle3 = 'X-Axis Title Page 3'

      yaxistitle1 = 'Y-Axis Title Page 1 ... 90 degrees'
      yaxistitle2 = 'Y-Axis Title Page 2 ... 270 degrees'
      yaxistitle3 = 'Y-Axis Title Page 3 ... 90 degrees'

      y2axistitle1 = 'Y2-Axis Title Page 1 ... 90 degrees'
      y2axistitle2 = 'Y2-Axis Title Page 2 ... 270 degrees'
      y2axistitle3 = 'Y2-Axis Title Page 3 ... 90 degrees'

      Pi     = 3.1415926535
      height = 1.5
      angle  = 0.0
      ndec   = 3
      fpn    = 12.345
      inteq  = 0

      status = 1
!!      
!     Example: CALL PLOT (x, y, Input Action)
!     Where:
!     Input Action =   3; Move to (x,y); Do not draw line
!     Input Action =   2; Move to (x,y); Draw line
!     Input Action =  -3; Move to (x,y); Do not draw line & Set new origin
!     Input Action =  -2; Move to (x,y); Draw line & set new origin
!     Input Action =  10; Eject to new page
!     Input Action = 999; End of PostScript print - close device
!!     

!     Specify output PostScript file */
      call plotfn ('tstfxrtps.psf')

!     Initialize XRTPS and XRT/graph PostScript Function 
      call xrtbegin (status)
      if (status .ne. 1) goto 9999

!     Switch to data set 1 and plot with point symbol display 
      call setactiveset (1)
      call header (header1, 3, 0)   ! Header #1, 3 lines, Adjust left
      call footer (footer1, 3, 0)   ! Footer #1, 3 lines, adjust left 
      call displayaxes (1,1,1)      ! Display three axes
      call setlabel (slabels1,11,1,4)! Label #1, 11 lines, Vertical, North West
      call axestitles(xaxistitle1,
     +                yaxistitle1,90,
     +                y2axistitle1,90)
      call setlandscape (1)         ! Set print mode to Landscape 
      call setwidgetaspect (1024, 1024)   ! Set display to 1024 * 1024 pixels 
                                    ! Xmax=1280,Ymax=1024 

      do 10 I = 1, 11, 1
         !! Switch to data set 0 and plot without point symbol display 
         call setactiveset (I-1)
         NO = real (I)
         call plot ( 0.5,NO * 1.0, -3)  ! Move to (x,y) and set origin 
         call plot (10.0,NO * 3.0,  2)  ! Move to (x,y) and draw line 
   10 continue

      !! Switch to data set 0 and plot without point symbol display 
      call setactiveset (0)

      !! Set up new origin
      call plot (6.0, 8.0, -3)      ! Move to (x,y) and set origin

      !! Switch to data set 0 and plot without point symbol display 
      call setactiveset (0)

      !! Display graph image
      call xrtpage (status)
      if (status .ne. 1) return

      !! Start new graph image #2

      !! Switch to data set 1 and plot with point symbol display 
      call setactiveset (1)

      call header (header2, 3, 2)   ! Header, 3 lines, XRT_ADJUST_RIGHT
      call footer (footer2, 3, 2)   ! Footer, 3 lines, XRT_ADJUST_RIFHT
      call displayaxes (1, 1, 1)    ! Display X, Y & Y2 axis
      call setlabel (slabels2,11,1,4)! display label2, 11 lines, 
     +                              ! XRT_ALIGN_VERTICAL, ANCHOR_NORTHWEST
      call axestitles (xaxistitle2, 
     +                 yaxistitle2,270,
     +                 y2axistitle2,270)

      call axesreverse (0, 1)       ! Set X to normal. Set Y to reverse

      !! Set up new origin
      call plot (0.0, 0.0, -3)      ! Move to (x,y) and set origin 

      call setactiveset (1)
      call symbol ( 3.0,  4.0, height, 'P1', inteq, angle, 2)
      call symbol ( 7.0, 14.0, height, 'P2', inteq, angle, 2)
      call symbol ( 9.0, 24.0, height, 'P3', inteq, angle, 2)
      call symbol (13.0,  4.0, height, 'P4', inteq, angle, 2)
      call symbol (33.0, 10.0, height, 'P5', inteq, angle, 2)

      call setactiveset (5)
      call plot ( 15.0, 6.0, -3)   ! Move to (x,y), set new origin
      call plot (  4.0, 4.0,  2)   ! Move to (x,y) & draw line 
      call plot ( 15.0,30.0,  2)   ! Move to (x,y) & draw line 
      call plot ( 30.0,20.0,  2)   ! Move to (x,y) & draw line
      call plot (-10.0,10.0,  2)   ! Move to (x,y) & draw line
      call plot (  0.0, 0.0,  2)   ! Move to (x,y) & do not draw line

      call setactiveset (6)
      call plot   (0.0, 7.0,  -2)  ! Move to (x,y), draw line, set origin

      call symbol (7.0, 14.0, height, 'P6', inteq, angle, 2)
      call symbol (9.0, 24.0, height, 'P7', inteq, angle, 2)
      call symbol (13.0, 4.0, height, 'P8', inteq, angle, 2)

      call number (10.0,10.0, height,10.12345, angle, 3)
      call number (20.0,20.0, height,20.12345, angle,-1)
      call number (30.0,30.0, height,30.12345, angle, 3)
      call number (40.0,40.0, height,40.12345, angle,-1)

      call xrtpage (status)
      if (status .ne. 1) goto 9999

      !! Start new graph image #3

      !! Switch to data set 1 and plot with point symbol display 
      call setactiveset (1)
      call axesreverse (0, 0)       ! Set X & Y to normal.
      call header (header3, 3, 2)
      call footer (footer3, 3, 2)
      call axestitles (xaxistitle3, 
     +                 yaxistitle3,90,
     +                 y2axistitle3,90)
      call setlabel (slabels3, 11, 1, 7) !Label 3, 11 lines, southeast
      call displayaxes (0, 1, 1)    ! Suppress X, Display Y & Y2 axis

      call plot   ( 0.0,  0.0, -3)  ! Move to (x,y) and set origin 

      call symbol ( 0.0,  0.0, height, 'P100', inteq, angle, -1)
      call symbol ( 2.0,  2.0, height, 'P101', inteq, angle, -1)
      call symbol ( 4.0,  4.0, height, 'P102', inteq, angle, -1)
      call symbol ( 6.0,  8.0, height, 'P103', inteq, angle, -1)
      call symbol ( 8.0, 16.0, height, 'P104', inteq, angle, -1)
      call symbol (10.0, 32.0, height, 'P105', inteq, angle, -1)
      call symbol (12.0, 64.0, height, 'P106', inteq, angle, -1)
      call symbol (14.0,128.0, height, 'P107', inteq, angle, -1)

      call xrtpage (status)
      if (status .ne. 1) goto 9999

      !! Start new graph image #4

      !! Switch to data set 1 and plot with point symbol display 
      call setactiveset (1)
      call axesreverse (0, 0)       ! Set X & Y to normal.
      call header (header3, 3, 2)
      call footer (footer3, 3, 2)
      call axestitles (xaxistitle3, 
     +                 yaxistitle3,90,
     +                 y2axistitle3,90)
      call setlabel (slabels3, 11, 1, 7) !Label 3, 11 lines, southeast
      call displayaxes (0, 1, 1)    ! Suppress X, Display Y & Y2 axis

      call setgraphaspect (600,450)

      call plot   ( 0.0,  0.0, -3)  ! Move to (x,y) and set origin 

      call symbol ( 0.0,  0.0, height, 'P100', inteq, angle, -1)
      call symbol ( 2.0,  2.0, height, 'P101', inteq, angle, -1)
      call symbol ( 4.0,  4.0, height, 'P102', inteq, angle, -1)
      call symbol ( 6.0,  8.0, height, 'P103', inteq, angle, -1)
      call symbol ( 8.0, 16.0, height, 'P104', inteq, angle, -1)
      call symbol (10.0, 32.0, height, 'P105', inteq, angle, -1)
      call symbol (12.0, 64.0, height, 'P106', inteq, angle, -1)
      call symbol (14.0,128.0, height, 'P107', inteq, angle, -1)

      !! Close XRT/graph 
      call plot (0, 0, 999)
 9999 continue
      end
    
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
$!-----------------------------------------------------------------------------
$ create tcxrtps.imake
#define PROGRAM tcxrtps

#define MODULE_LIST tcxrtps.c
/* for debugging, replace with:
#define MODULE_LIST tcxrtps.c zcxrtps.c zfxrtps.c
 and enable last DEBUG line below */

#define MAIN_LANG_C
#define R2LIB

/*#define USES_C -jaw - changed to ANSI_C to match xrtps*/
#define USES_ANSI_C
#define LIB_MOTIF
#define LIB_XRT_GRAPH
#define LIB_RTL
#define LIB_TAE
/* #define LIB_LOCAL */
#define LIB_P2SUB
/*#define DEBUG /* remove on delivery */
$!-----------------------------------------------------------------------------
$ create tfxrtps.imake
/* Imake file for FORTRAN interface Test of VICAR subroutine xrtps */

#define PROGRAM tfxrtps

#define MODULE_LIST tfxrtps.f

#define MAIN_LANG_FORTRAN
#define TEST
#define FTN_STRING
#define USES_FORTRAN
/*#define USES_C -jaw - changed to ANSI_C to match xrtps*/
#define USES_ANSI_C

#define LIB_RTL


#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
#define LIB_MOTIF
#define LIB_XRT_GRAPH
/* #define LIB_LOCAL */
$!-----------------------------------------------------------------------------
$ create tcxrtps.pdf
process help=*
PARM NODISP STATUS=KEYWORD COUNT=(0,1) VALID=NODISP DEFAULT=--
 END-PROC
.TITLE
VICAR program TCXRTPS
.HELP
PURPOSE
       TCXRTPS tests the 'C' interface to XRTPS and XRT/graph
.END
$!-----------------------------------------------------------------------------
$ create tfxrtps.pdf
process help=*
PARM NODISP STATUS=KEYWORD COUNT=(0,1) VALID=NODISP DEFAULT=--
 END-PROC
.TITLE
VICAR program TFXRTPS
.HELP
PURPOSE
       TFXRTPS tests the fortran interface to XRTPS and XRT/graph
.END
$!-----------------------------------------------------------------------------
$ create tstxrtps.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
write " "
WRITE "This is a test of subroutine XRTPS and the FORTRAN bridge for XRTPS"
write " "
write "Interactive Test of C interface to xrtps()"
tcxrtps 
write " "
write "Non-Interactive Test of C interface to xrtps()"
tcxrtps  'NODISP
write " "
write "Interactive Test of Fortran Bridge interface to xrtps()"
tfxrtps  
write " "
write "Non-Interactive Test of Fortran Bridge interface to xrtps()"
tfxrtps  'NODISP
write " "
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create xrtps.hlp
1 XRTPS 

   XRTPS is a FORTRAN and C library package that provides an interface to
   the XRT/graph library of graphic plotting functions. XRT/graph provides
   the capability of producing a graph and then optionally producing an 
   Encapsulated PostScriptFile from the displayed data.

   The output PostScript file is suitable for sending to a PostScript
   printer, or may be used for importing into another PostScript file. The
   PostScript file conforms to EPSE-2.0 format.
    
   The software described in this document interfaces with MOTIF and with
   the interfaces described in: XRT/graph for Motif; Widget  Programmer's
   Guide & Reference Manual, 'Software Objects for GUI  Development'.
   Manuals for VMS and UNIX are available.

   XRTPS was adapted from the Printronix plotting package, CPLTPR. The 
   interfaces to CPLTPR were preserved as much as possible in the creation
   of XRTPS. Several new interfaces have been added to take advantage of the
   many capabilities provided by XRT/graph and to overcome obstacles found
   in the plotter/printer and CPLTPR packages. Two of the major obstacles 
   encountered when using CPLTPR were: 1) all  data had to be scaled to the
   size of the paper used by the plotter/printer, and 2) CPLTPR did not
   create an X & Y axis from the data received.  As a result, applications
   using CPLTPR were required to scale their data before going to CPLTPR,
   and had to draw their own X & Y axes using a combination of CPLTPR's
   subroutines; axis, line, number, symbol and plot. 

   The major benefits of XRT/graph is its ability to receive data from the
   application program with values large and small, scale the data to fit on
   a printed page in landscape format, and then automatically create an X &
   Y axes to match the application data. The paper size for a printed page
   is 8.5 x 11.0 inches, with a 0.25 inch margin, for all printed output
   PostScript files. With XRT/graph's ability to scale received data to fit
   a page and its ability to create an appropriately scaled X & Y axis, the
   use of the two XRTPS subroutines AXIS and SCALE should be discouraged.

XRTPS & XRT/graph RUNNING IN THE NONDISPLAY MODE

   The display of the graph produced by XRT/graph may be suppressed. In this
   mode, XRTPS automatically produces the output PostScript file and
   suppresses the XRT/graph output display. When run, XRTPS initiates two
   tests to determine if the display should be suppressed. If the 'NODISP
   keyword is found on the VICAR run-time command line, or XRTPS  determines
   that it is running in batch mode, by calling subroutine BATCH, XRTPS
   suppresses the XRT/graph output display. In order for XRTPS to process
   the 'NODISP keyword, the following line must be added to the application
   program's pdf file.

   PARM NODISP STATUS=KEYWORD COUNT=(0,1) VALID=NODISP DEFAULT=--


USER INTERACTIVE MODE

   In the interactive mode, as each graph is displayed, XRTPS displays a set
   of Motif Radio Buttons, 'exit', 'page' and 'save', and then waits for an
   operator switch action to occur before proceeding to the next output
   display or before exiting the program. The 'save' switch action  results
   in the currently displayed graph being saved in the output PostScript
   file.  The 'page' switch action results in the currently displayed graph
   being removed from the display, not saved, and then XRTPS proceeds to the
   next page of the display or exits if finished. The 'exit' switch action
   results in a status being returned to the application program  indicating
   that the application is to be terminated. 


POSTSCRIPT FILE OUTPUT

   XRTPS outputs all 'saved' XRT/graph output images in PostScript format to
   a single file. As each image is displayed, the operator has the option of
   saving or not-saving the image to the output PostScript file. Saved
   images may be printed on the QMS Laser Printer by entering 'qpr
   filename'.  The output PostScript filename may be specified by the
   application through the subroutine call 'PLOTFN (filename)'.  If the
   filename is not specified by the application program, then the output
   filename defaults to 'postscript.psf'.
   

PORTING AN EXISTING APPLICATION:

   Application programs using CPLTPR to create and plot images to the
   Printronix printer may be ported to run under XRTPS and XRT/graph by
   performing the following minimal steps:

   Note: all references to subroutines are with respect to the Fortran
   subroutine interface. Reference the Fortran subroutine description for
   the C language interface.

   From the calls to AXIS, LINE, PLOT, and SYMBOL, determine which
   annotation belongs on the X, Y & optionally, the Y2 axis. 

   Using the SETAXESTITLES subroutine call, annotate the X, Y, and
   optionally the Y2 axis, and specify the rotation of the Y and Y2 axis
   titles about the axes; 0, 90, or 270. Note; the X-axis title cannot be
   rotated.

   Remove and replace the subroutine call 'CALL PLOT (0,0,9)' with 'CALL
   XRTBEGIN'.  This new interface allows a status to be returned indicating
   whether or not XRTPS and XRT/graph start-up was successful.

   Delete or Comment-out all calls to subroutines AXIS and SCALE. Remove,
   delete or otherwise nullify all scaling calculations and prepare to use
   nonscaled data for output to XRTPS. Note: It is not absolutely required
   that calls to these subroutines be deleted, but to take advantage of
   XRT/graph's many capabilities it is recommended.

   Using the DISPLAYAXES subroutine, specify which of the three axes; X, Y,
   & Y2 are to be displayed.

   From the printout obtained from the application program's baseline plot,
   determine axes origins.  The default axes origins are X = 0.0 and Y = 0.0
   at the lower-left corner of the displayed/printed page.  Using
   XRT/graph's default values, the axes origin will begin at the lower
   left-hand corner with increasing X values to the right, and increasing Y
   values up, towards the top-of-page. Through the use of subroutine
   AXESREVERSE, the X and Y axes origin and consequently, increasing values
   for X & Y can be made to originate from any-one of the four corners of
   the graph.  Refer to section AXES CONTROL for further information.

   Calls to SYMBOL and calls to NUMBER may be left unchanged. However, the X
   & Y parameter values in calls to SYMBOL and NUMBER should be the
   nonscaled values.  A symbol's anchor position, with respect to the
   (x,.y) point to which it is attached, may be adjusted through the
   subroutine interface SYMBOLANCHOR

   XRTPS can display up to eleven data sets. All data sets can be used to
   display plotting information. The first data set (set 0) annotates each
   plotted point, number, or symbol with a 'blank'  point-symbol. Data sets
   2 thru 10 each uses a unique when plotting a point, symbol or number.

   Calls to PLOT for 'page' control, where the ipen number is 10 ('CALL PLOT
   (0,0,10)'), should be changed to 'call xrtpage (status)'.  The two
   subroutine calls are used by XRTPS to initiate the following actions; 1)
   display the graph currently being created, 2) display three radio
   buttons, and 3) wait for the operator to initiate a switch action on one
   of the three buttons. When the switch action occurs, XRTPS returns a
   switch action  status to the application. A returned status other than
   PASS (1) should result in the termination of the application. A returned
   status of PASS (1) indicates that the operator did not select 'exit', but
   instead chose 'page' or 'save'. The subroutine call 'call plot (0,0,10)'
   is unable to receive and process a returned status, and therefore should
   not be used.


DEVELOPING A NEW APPLICATION:

   New application programs may be developed to run under XRTPS and
   XRT/graph by performing the following steps:

   Note: all references to subroutines are with respect to the Fortran
   interface. Reference the subroutine description for the C language
   interface.

   Using the PLOTFN subroutine call, specify the output PostScript filename. 
   If a filename is not specified, the filename will default to
   'postscript.psf'.

   Using the DISPLAYAXES subroutine, specify which of the three axes; X, Y,
   & Y2  are to be displayed.

   Determine the annotation that is to be placed on each axis. Using the
   SETAXESTITLES subroutine call, annotate the X, Y, and optionally the Y2
   axis, and specify the rotation of the Y and Y2 axis titles about the
   axes; 0, 90, or 270. Note; the X-axis title cannot be rotated.  It is not
   necessary to specify the X & Y values for annotation of the axes, as
   these values will be automatically provided by XRT/graph.

   Determine axes origins. The default axes origins are X = 0.0 and Y = 0.0
   at the lower-left corner of the graph.  Using the XRT/graph default
   values, the origin will originate at the lower left-hand corner with
   increasing X values to the right, and increasing Y values up, towards the
   top-of-page. Through the use of subroutine AXESREVERSE, the X and Y axes
   origin and consequently, increasing values for X & Y can be made to
   originate from any-one of the four corners of the graph.

   If further axes control is required by the application, reference the
   section titled AXES CONTROL.

   Subroutines HEADER and FOOTER are used to annotate the top and the bottom
   of graphs. XRT/graph does not place a limit on the number of lines of
   text that can be displayed in the header or footer. The calls to HEADER
   and FOOTER may be made at any time before a page is displayed, or may be
   made once at the start of the application and left unchanged through out
   the remainder of the application.

   XRTPS displays up to eleven (11) data sets, where each of the 11 data
   sets can be displayed simultaneously; the first data set overalys the
   second, which overlays the third, etc.  All data sets can be used to
   display plotting information. However, the first data set (set 0)
   XRT/graph annotates each plotted point, number or symbol with a with a
   'blank' point symbol, and annotates each of the remaining ten (10) points
   with a different point-symbol. Use subroutine SETACTIVESET to specify the
   active data set, 0 thru 10. Once set, the specified data set remains as
   the active data set until changed. A legend may be displayed with a line
   of text to describe each of the 11 data sets.  The graphs in data set 0
   and 1 may be annotated with a label by the subroutine SETLABEL. The first
   data set (0) is ideally suited for joining line segments together where
   the point symbol would clutter the graph.

   Calls to SYMBOL and calls to NUMBER may be used to attach a character
   string or a numeric value to a point.

   XRTPS provides subroutine PLOT to draw lines and points on a graph.
   Subroutine PLOT can be used to; 1) move to a new point (x,y) and draw a
   line to the new point, 2) move to a new point (x,y) and do not draw a
   line, 3) draw a line from the current position (x,y) to point (x,y) and
   set a new origin at the new point, 4) move to new point (x,y) without
   drawing a line and set a new origin at the new point. When an origin has
   been established, all X and Y points are plotted relative to the
   established origin. 

   Subroutine DRAWLINE can be used to draw a line between two points. 

   Subroutine LINE draws lines from the X & Y point values contained within
   an array of X & Y point values.

   Calls to XRTPS for 'page' control (e.g., to go from page 2 to page 3),
   will be to subroutine XRTPAGE(status)'.  The  call to subroutine XRTPAGE
   is used by XRTPS to; 1) display the graph currently being created, 2)
   display three menu items, and 3) wait for the operator to initiate a
   switch action on one of the three items. XRTPS returns a status to the
   application indicating the operator-initiated switch action. A returned
   status other than PASS (1) should result in the termination of the
   application program. A returned status of PASS (1) indicates that the
   operator did not select 'exit', but instead selected 'page' or 'save'.

   To terminate XRTPS and to display the last application graph, use the
   subroutine call 'CALL PLOT(0,0,999)'. The  subroutine call is used by
   XRTPS to; 1) display the graph currently being created, 2) display the
   three radio buttons, and 3) wait for the operator to initiate action on
   one of the three buttons. The status of the graph image in the output
   PostScript file is predicated on which switch action was taken. XRTPS
   frees all allocated memory and closes the output PostScript file upon
   receiving this request.

   ============================================================================


Members

   The C interfaces and their respective Fortran-Bridge interfaces are 
   segregated into several sections and are described described as follows: 
 
STARTUP

  Startup is the signal to XRTPS that an application is about to use the
  XRT/graph collection of plotting functions. Upon successful completion of
  STARTUP, XRPS and XRT/graph will be capable of receiving, processing and
  displaying an application's plotting requests.


  =====

  ZrtPlotFn ()
  plotfn ()

    ZrtPlotFn () sets the output post script filename to a specified 
    filename. If a filename is not specified via ZrtPlotFn, then the 
    output filename defaults to "postscript.psf".  This request must be 
    made before the call to ZrtBegin().
        
        
  C Interface
    void ZrtPlotFn (filename);
    char *filename;        // Input  

  Fortran Interface
    call plotfn (filename)
    character*n filename   // Input

  =====

  ZrtBegin ()
  xrtbegin ()

    ZrtBegin () initializes XRTPS () and XRT/graph.  If the output
    postscript file cannot be opened, or XRTPS, or XRT/graph cannot be
    initialized, a status of FAIL (zero) is returned. 

  C Interface
    int ZrtBegin ();            
 
  Fortran Interface
    call xrtbegin (status)
    integer status              // Output status


ANNOTATION

  Annotation is provided by XRT/graph to annotate page headers and footer,
  to provide annotation for the X, Y, & Y2 axes, and provide a legend for
  the eleven data sets.

  ZrtHeader ():
  header ()

    ZrtHeader () is called to set the header title string(s). If the header 
    is not set, then a header not be displayed.

  C Interface
    void ZrtHeader (header,adjustment)
    char    **header;           // Pointer to a list of pointers to header 
                                   strings .
    XrtAdjust adjustment;       // Set for left, center, or right .
                                   Legal values for adjustment are:
                                   adjustment = XRT_ADJUST_LEFT;
                                   adjustment = XRT_ADJUST_CENTER;
                                   adjustment = XRT_ADJUST_RIGHT;

  Fortran Interface
    call header (header, lines, adjustment)
    character*m header(n)       // Array of strings, n lines long 
    integer     n               // Number of lines in header
    integer     adjustment      // Set for left, center, or right.
                                   Legal values for adjustment are:
                                   adjustment = 0 ! XRT_ADJUST_LEFT;
                                   adjustment = 1 ! XRT_ADJUST_CENTER;
                                   adjustment = 2 ! XRT_ADJUST_RIGHT;

  =====

  ZrtFooter ()
  footer ()

    ZrtFooter () is called to set the footer title string(s). If the footer 
    is not set, then a footer not be displayed.

  C Interface
    void ZrtFooter (footer, adjustment)
    char    **footer;           // Pointer to a list of pointers to footer
                                   strings 
    XrtAdjust adjustment;       // Set for left, center or right.
                                   Legal values for adjustment are:
                                   adjustment = XRT_ADJUST_LEFT;
                                   adjustment = XRT_ADJUST_CENTER;
                                   adjustment = XRT_ADJUST_RIGHT;

  Fortran Interface
    call footer (footer, lines, adjustment)
    character*m header(n)       // Array of strings, n lines long 
    integer     n               // Number of lines in footer
    integer     adjustment      // Set for left, center, or right
                                   Legal values for adjustment are:
                                   adjustment = 0 ! XRT_ADJUST_LEFT;
                                   adjustment = 1 ! XRT_ADJUST_CENTER;
                                   adjustment = 2 ! XRT_ADJUST_RIGHT;

  =====

  ZrtSymbolAnchor ()
  symbolanchor ()

     ZrtSymbolAnchor () Anchors the 'text' at specified location with respect
     to the location of symbol. The default is XRT_ANCHOR_BEST.

  C Interface
     void ZrtSymbolAnchor (anchor);
     XrtAnchor  anchor;
                Valid anchor positions are:
                XRT_ANCHOR_NORTH                XRT_ANCHOR_SOUTH
                XRT_ANCHOR_EASTH                XRT_ANCHOR_WEST
                XRT_ANCHOR_NORTHWEST            XRT_ANCHOR_SOUTHWEST
                XRT_ANCHOR_NORTHEAST            XRT_ANCHOR_SOUTHEAST
                XRT_ANCHOR_HOME                 XRT_ANCHOR_BEST
        
  Fortran Interface
     call symbolanchor (anchor)
     integer    anchor
                Valid anchor positions are:
                anchor = 0 ! XRT_ANCHOR_NORTH
                anchor = 1 ! XRT_ANCHOR_SOUTH
                anchor = 2 ! XRT_ANCHOR_EAST 
                anchor = 3 ! XRT_ANCHOR_WEST
                anchor = 4 ! XRT_ANCHOR_NORTHWEST
                anchor = 5 ! XRT_ANCHOR_NORTHEAST
                anchor = 6 ! XRT_ANCHOR_SOUTHWEST
                anchor = 7 ! XRT_ANCHOR_SOUTHEAST
                anchor = 8 ! XRT_ANCHOR_BEST     
                anchor = 9 ! XRT_ANCHOR_HOME     

  =====

  ZrtSymbol ()
  symbol ()

     ZrtSymbol () plots the 'text' at position (x,y) for 'nchar' number of
     characters.

  C Interface
     void ZrtSymbol (x, y, height, text, inteq, angle, ndec):
     float x;                // Input  
     float y;                // Input  
     float height;           // Input - not used
     char *text;             // Input  
     int   inteq;            // Input - not used  
     float angle;            // Input - not used  
     int   nchar;            // Input. If NCHAR == -1, then symbol
                                   will be connected with a line.
     
  Fortran Interface
     call symbol (x, y, height, text, inteq, angle, ndec):
     real            x       // Input  
     real            y       // Input  
     real            height  // Input - not used
     character*n     text    // Input  
     integer         inteq   // Input - not used
     real            angle   // Input - not used
     integer         nchar   // Input. If NCHAR == -1, then symbol
                                   will be connected with a line.
      
  =====

  ZrtSetLabel ():
  setlabel():

     ZrtSetLabel () is called to annotate the two data-sets. If the label is
     not labeled, then the data set will not be annotated.
     

  C Interface
     void ZrtSetLabel (label, orientation ,anchor)
     char    **label;           // Pointer to list of pointers for label 
                                   strings. 
     XrtAlign  orientation;     // Align horizontal or vertical 
                                   Valid orientation enum values are:
                                   orientation = XRT_ALIGN_HORIZONTAL
                                   orientation = XRT_ALIGN_VERTICAL
     XrtAnchor anchor;          // Anchor in one of eight locations
                                   Valid anchor positions are:
                                   anchor = XRT_ANCHOR_NORTH
                                   anchor = XRT_ANCHOR_SOUTH
                                   anchor = XRT_ANCHOR_EAST 
                                   anchor = XRT_ANCHOR_WEST
                                   anchor = XRT_ANCHOR_NORTHWEST
                                   anchor = XRT_ANCHOR_SOUTHWEST
                                   anchor = XRT_ANCHOR_NORTHEAST
                                   anchor = XRT_ANCHOR_SOUTHEAST

  Fortran Interface:
     setlabel (label, orientation ,anchor)
     chararacter*m label(n)     // Label strings
     integer  n                 // Number of lines in label array (two max)
     integer  orientation       // Align horizontal or vertical 
                                   Valid orientation enum values are:
                                   orientation = 0 ! XRT_ALIGN_HORIZONTAL
                                   orientation = 1 ! XRT_ALIGN_VERTICAL
     integer  anchor            // Anchor in one of eight locations
                                   Valid anchor positions are:
                                   anchor = 0 ! XRT_ANCHOR_NORTH
                                   anchor = 1 ! XRT_ANCHOR_SOUTH
                                   anchor = 2 ! XRT_ANCHOR_EAST 
                                   anchor = 3 ! XRT_ANCHOR_WEST
                                   anchor = 4 ! XRT_ANCHOR_NORTHWEST
                                   anchor = 5 ! XRT_ANCHOR_NORTHEAST
                                   anchor = 6 ! XRT_ANCHOR_SOUTHWEST
                                   anchor = 7 ! XRT_ANCHOR_SOUTHEAST

  =====

  ZrtAxesTitles ()
  axestitles ()

     ZrtAxesTitles () is called to set the X, Y & Y2 axis titles, and
     rotation of titles about the axis.  If the X-axis, Y-Axis or the
     Y2-axis are not displayed, then Axes titles will not be displayed. 
     Note: Only the vertical axes (Y & Y2) can be rotated.

  C Interface
     void ZrtAxesTitles (XTitle, YTitle, YRotate, Y2Title, Y2Rotate)
     char *XTitle               // Pointer to X title
     char *YTitle               // Pointer to Y title
     char *Y2Title              // Pointer to Y2 title
     XrtRotate YRotate;         // Y-axis Rotation
     XrtRotate Y2Rotate;        // Y2-axis Rotation
                                   Legal values for YRotate and Y2Rotate axis 
                                   title rotation are:
                                   XRT_ROTATE_NONE
                                   XRT_ROTATE_90
                                   XRT_ROTATE_270
  Fortran Interface:
     call axestitles (XTitle, YTitle, YRotate, Y2Title, Y2Rotate)
     character*n  XTitle        // X title
     character*n  YTitle        // Y title
     character*n  Y2Title       // Y2 title
     integer YRotate            // Y-axis rotation 
     integer Y2Rotate           // Y2-axis rotation
                                   Legal values for YRotate and Y2Rotate axis 
                                   title rotation are:
                                   YRotate, Y2Rotate = 0 ! XRT_ROTATE_NONE
                                   YRotate, Y2Rotate = 1 ! XRT_ROTATE_90
                                   YRotate, Y2Rotate = 2 ! XRT_ROTATE_270


PAGE FORMATTING

  Page formatting allows the application to set the format for the output
  PostScript file to either landscape or portrait, and allows the
  application to specify the pixel dismensions for the display.


  ZrtSetLandScape ()
  setlandscape ()

     ZrtLandscape () is called to set the output page format to either
     landscape or portrait.  The default format is landscape.  A setting of
     FALSE (0) sets the format to portrait. Reference setwidgetaspect ().

  C Interface
    void ZrtSetLandScape(format)
    int format;                         // Input

  Fortran Interface
    call setlandscape (format)
    integer format                      // Input

  =====

  ZrtSetWidgetAspect ():
  setwidgetaspect ()

     ZrtSetWidgetAspect () is called to set the aspect ratio for the whole
     graph Widget. The default settings are; X=1200, and Y=900. To adjust the
     size of the  annotated graph only, refer to ZrtSetGraphAspect.

  C Interface
    void ZrtSetWidgetAspect (xaxis, yaxis)
    int xaxis;                          // Input
    int xaxis;                          // Input

  Fortran Interface
    setwidgetaspect (xaxis, yaxis)
    integer xaxis                       // Input
    integer xaxis                       // Input


  =====

  ZrtSetGraphAspect ():
  setgraphaspect ()

     ZrtSetGraphAspect () is called to set the aspect ratio for the annotated 
     graph only. The default settings are; X=1200, and Y=900. To adjust the
     size of the whole graph widget, refer to ZrtSetWidgetAspect.

  C Interface
    void ZrtSetGraphAspect (xaxis, yaxis)
    int xaxis;                          // Input
    int xaxis;                          // Input

  Fortran Interface
    setgraphaspect (xaxis, yaxis)
    integer xaxis                       // Input
    integer xaxis                       // Input


AXES CONTROL

  Axes control allows the application to control the X & Y axes orientation.
  The axes display defaults to displaying X = 0.0 & Y = 0.0, or the lowest X
  & Y, values in the lower-left corner of the page. Through the use of axes
  control. the axes may be configured to originate from any corner of the
  display and control the direction of increasing and decreasing values for
  the X & Y axes.


  ZrtAxesReverse ():
  axesreverse ()

    ZrtAxesReverse () is called to set the Axes orientation. The default
    origin  for the X & Y axes is at the lower-left corner of the printed
    page. If the xaxis is set TRUE (reversed), annotation will increase from
    right to  left.  If the yaxis is set TRUE (reversed), annotation will
    increase  from top to bottom.

  C Interface
    void ZrtAxesReverse (xaxis, yaxis)
    int xaxis;                          // Input
    int yaxis;                          // Input

  Fortran Interface
    call axesreverse (xaxis, yaxis)
    integer xaxis                       // Input
    integer yaxis                       // Input

  =====

  ZrtDisplayXrtAxis ()
  displayaxes ()

    ZrtDisplayAxis () is called to display/suppress the automatic generation
    of the X-axis, Y-axis, & the Y2-axis by XRT/graph.  If the application
    sets the X-axis, or the Y-axis or the Y2-axis to FALSE (0), the display
    of the specified axis will be suppressed.  The default settings for
    XRT/graph is to display the X-axis and Y-axis, and suppress the display
    of the Y2-axis.  Suppressing an axis will also suppress the
    corresponding axis title.

  C Interface

    void ZrtDisplayXrtAxis (xaxis,yaxis,y2axis)
    int xaxis;                 // Input 
    int yaxis;                 // Input
    int y2axis;                // Input

  Fortran Interface:

    call displayaxes (xaxis, yaxis, y2axis)
    integer xaxis              // Input 
    integer yaxis              // Input
    integer y2axis             // Input


  =====

  ZrtAxesOrigin ():
  axesorigin ()

    ZrtASxisOrigin () is called to specify where the axes should be
    rendered. For example, setting the X origin to 5.0 will cause the Y-axis
    to cross the X-Axis at X=5.0.

  C Interface

    void ZrtAxesOrigin (xorigin, yorigin)
    float xorigin;             // Input
    float yorigin;             // Input

  Fortran Interface:

    axesorigin (xorigin, yorigin)
    real  xorigin              // Input
    real  yorigin              // Input

  =====

  ZrtAxesOriginPlacement ()
  axesoriginplacement ()

    ZrtAxesOriginPlacement () is called to specify the placement for the
    axis origins on a plot. 

  C Interface

    void ZrtAxesOriginPlacement (xorigin, yorigin)
    XrtOriginPlacement xorigin; // Input
    XrtOriginPlacement yorigin; // Input

                             Note: The legal values for origin placement are:
                             XRT_ORIGIN_AUTO: Origin is placed at the minimum
                                              axis value or at zero if the 
                                              dataset contains positive
                                              and negative values (default).
                             XRT_ORIGIN_ZERO: Origin is placed at zero.
                             XRT_ORIGIN_MIN:  Origin is placed at the minimum
                                              axis value.
                             XRT_ORIGIN_MAX   Origin is placed at the maximum 
                                              axis value.

  Fortran Interface:

    axesoriginplacement (xorigin, yorigin)
    integer xorigin         // Input
    integer yorigin         // Input

                             Note: The legal values for origin placement are:
                             0 =  XRT_ORIGIN_AUTO: Origin is placed at the 
                                                   minimum axis value or at 
                                                   zero if the dataset 
                                                   contains positive and 
                                                   negative values.
                             1 =  XRT_ORIGIN_ZERO: Origin is placed at zero.
                             2 =  XRT_ORIGIN_MIN:  Origin is placed at the 
                                                   minimum axis value.
                             3 =  XRT_ORIG0IN_MAX  Origin is placed at the 
                                                   maximum axis value.
      

  =====

  ZrtSetAxesMaximums ():
  setaxesmaximums ()

    ZrtSetAxesMaximums () is called to specify the maximum values for the
    axes. By setting these values, graph data may be clipped and not
    displayed.

  C Interface

    void ZrtSetAxesMaximums (xaxis, yaxis, y2axis)
    float xaxis;               // Input
    float yaxis;               // Input
    float y2axis;              // Input

  Fortran Interface:

    setaxesmaximums (xaxis, yaxis, y2axis)
    real  xaxis                // Input
    real  yaxis                // Input
    real  y2axis               // Input

  =====               


  ZrtSetAxesMinimums ():
  setaxesminimums ():

    XrtSetAxesMinimums () is called to specify the minimum values for the
    axes. By setting these values, graph data may be clipped and not
    displayed.

  C Interface:

    void ZrtSetAxesMinimums (xaxis, yaxis, y2axis)
    float xaxis;               // Input
    float yaxis;               // Input
    float y2axis;              // Input

  Fortran Interface:

    setaxesminimums (xaxis, yaxis, y2axis)
    real  xaxis                // Input
    real  yaxis                // Input
    real  y2axis               // Input

  =====

  ZrtSetDataMaximums (): 
  setdatamaximums (): 

    ZrtSetDataMaximums () is called to specify the maximum values for the
    data.  By setting these maximum values, graph data may be clipped and
    not  displayed.

  C Interface

    void ZrtSetDataMaximums (xaxis, yaxis, y2axis)
    float xaxis;               // Input
    float yaxis;               // Input
    float y2axis;              // Input

  Fortran Interface:

    setdatamaximums (xaxis, yaxis, y2axis)
    real  xaxis                // Input
    real  yaxis                // Input
    real  y2axis               // Input

  =====

  ZrtSetDataMinimums (): 
  setdataminimums (): 

    ZrtSetDataMinimums () is called to specify the minimum values for the
    data. By setting these minimum values, graph data may be clipped and not
    displayed.

  C Interface:

    void ZrtSetDataMinimums (xaxis, yaxis, y2axis)
    float xaxis;               // Input
    float yaxis;               // Input
    float y2axis;              // Input

  Fortran Interface:

    setdataminimums (xaxis, yaxis, y2axis)
    real  xaxis                // Input
    real  yaxis                // Input
    real  y2axis               // Input


DATA CONTROL

  Data Control provides the interface for placing plotting information on
  the graphs.  Data Control provides an interface to draw poins, lines
  symbols, numeric values, and designate the data set into which the data
  will be placed.


  =====

  ZrtPlot ()
  plot ()

    ZrtPlot () moves to point (x,y) with and without out drawing a line, and
    optionally set a new origin for the data, depending upon the 'action'
    requested.

  C Interface

    void ZrtPlot (x, y, action):
    float x;               // Input  
    float y;               // Input  
    int   action;          // Input  
                              Input action:
                              =   3; Move to (x,y); Do not draw line
                              =   2; Move to (x,y); Draw line
                              =  -3; Move to (x,y); Do not draw line Set origin
                              =  -2; Move to (x,y); Draw line & set new origin
                              =  10; Eject to new page
                              = 999; End of PostScript
        
  Fortran Interface

    call plot (x, y, action)
    real  x                // Input  
    real  y                // Input  
    integer  action        // Input  
                              Input action:
                              =   3; Move to (x,y); Do not draw line
                              =   2; Move to (x,y); Draw line
                              =  -3; Move to (x,y); Do not draw line Set origin
                              =  -2; Move to (x,y); Draw line & set new origin
                              =  10; Eject to new page. See ZrtPage.
                              = 999; End of PostScript. See ZrtClose.


  =====

  ZrtLine ()
  line ()

    ZrtLine () Graphs the points in an array. ZrtLine moves to the first
    (x,y) point in the array pair and sets a new origin, without drawing a
    line. If lintyp > 0 then each point (x,y) is connected by a line. 
    Otherwise just point (x,y) is plotted.

  C Interface:

    void ZrtLine (xarray, yarray, npts, inc, lintyp, inteq)
    float  xarray[];           // Input
    float  yarray[];           // Input
    int    npts;               // Input
    int    inc;                // Input 
    int    lintyp;             // Input 
    int    inteq;              // Input 

  Fortran Interface:

    call line (xarray, yarray, npts, inc, lintyp, inteq)
    real   xarray()            // Input
    real   yarray()            // Input
    int    npts                // Input
    int    inc                 // Input 
    int    lintyp              // Input 
    int    inteq               // Input 
    
  =====

  ZrtNumber ()
  number ()

    ZrtNumber () plots the number 'fpn' at point (x,y). Parameter  'ndec' is
    a code telling how to format the number. 

  C Interface

    void ZrtNumber (x, y, height, fpn, angle, ndec):
    float x;                   // Input  
    float y;                   // Input  
    float height;              // Not used
    float fpn;                 // Input  
    float angle;               // Input  
    int   ndec;                // Input. If NDEC == -1, then fpn
                                  is interpreted as integer, otherwise
                                  NDEC defines the precision forthe
                                  float fpn value.
        
  Fortran Interface

    call number (x, y, height, fpn, angle, ndec):
    real  x                    // Input  
    real  y                    // Input  
    real  height               // Not used
    real  fpn                  // Input  
    real  angle                // Input  
    integer ndec;              // Input. If NDEC == -1, then fpn 
                                  is interpreted as integer, otherwise
                                  NDEC defines the precision for the
                                  real fpn value.
        
  =====

  ZrtSetActiveSet ()
  setactiveset ()

    ZrtSetActiveSet () allows the application program to write into one of
    eleven (11) data sets; 0 thru 10.  Data may be written into any data
    set.  Data set 0 provides a 'balnk' point symbol to indicate a point on
    the rendered graph.  Data set 1 thru 10 provides a unique point symbol.
    Data set 0 is ideally suited for connecting line segments where a point
    symbol would clutter the graph.  A specified data set will remain active 
    until changed. 

  C Interface:

    void ZrtSetActiveSet (newactiveset)
    int newactiveset;          // Input
                               
  Fortran Interface:

    call setactiveset (newactiveset)
    integer newactiveset       // Input

  =====

  ZrtDrawLine ()
  drawline ()

    ZrtDrawLine () draws a line from (x1,y1) to (x2,y2) 

  C Interface:

    void ZrtDrawLine (x1, y1, x2, y2)
    int x1;                    // Input 
    int y1;                    // Input 
    int x2;                    // Input 
    int y2;                    // Input 

  Fortran Interface:

    call drawline (x1, y1, x2, y2)
    integer x1                 // Input 
    integer y1                 // Input 
    integer x2                 // Input 
    integer y2                 // Input 

  =====

  ZrtScale ()
  scale ()

    ZrtScale () Calculates the scale factors for the data in an array and 
    then places the scale factors at the end of the array 

    This function is a carry-over from the Printronix plotting functions
    where all data going to the plotter had to be scaled to the height and
    width of the plotter paper.  XRT/graph does not require that data be 
    scaled to fit the paper width and height, as XRT/graph will 
    automatically scale the data to fit the size of the laser printer  paper
    (8.5 x 11.0 inches), and will automatically create, annotate, and
    display an X & Y axis. The use of this function should be discouraged.
     
  C Interface:

    void ZrtScale (array, axlen, npts, inc)
    float array[];             // Input/Output
    float axlen;               // Input
    int   npts, inc;           // Input

  Fortran Interface:

    call scale (array, axlen, npts, inc)
    real    array()            // Input/Output
    real    axlen              // Input
    integer npts               // Input
    integer inc                // Input

  =====

  ZrtAxis ()
  axis ()

    ZrtAxis () draws an annotated axis of length 'axlen' inches at an angle,
    labels each tic mark every inch with a number, starting with 'firstv'
    and with an increment of 'deltav', labels the axis with the title. 
     
    This function is a carry-over from the Printronix plotting functions
    where all data going to the plotter had to scaled to the height and
    width of the plotter paper.  XRT/graph does not require that data be 
    scaled to fit the paper width and height, as XRT/graph will 
    automatically scale the data to fit the size of the laser printer  paper
    (8.5 x 11.0 inches), and will automatically create, annotate, and
    display an X & Y axis. The use of this function should be discouraged.
     
  C Interface:

    void ZrtAxis (x,y, title, nchar, axlen, angle, firstv,deltav)
    float x, y, axlen, angle, firstv, deltav;
    int   nchar                // Input - Number of characters in title
    char  title                // Title to be placed on axis line

  Fortran Interface:

    call axis (x,y, title, nchar, axlen, angle, firstv,deltav)
    real        x, y, axlen, angle, firstv, deltav
    integer     nchar          // Input - Number of character in title
    character*n title          // Title to be placed on axis line



PROGRAM CONTROL

  Program Control provides the interfaces to allow the application program
  to display the graph currently being created and then exit the program, and
  the interface for the application to page to the next graph to be displayed.


  ZrtPage ()
  xrtpage ()

    ZrtPage () is called to display the next page of a graph. When more than
    one graph image is created in a program, the 'page' request is used to
    seperate the images and force the current image to the display. Once the
    graph has been displayed, under operator interactive cpntrol, the graph
    may be saved for printing 'save', the program may be terminated 'exit',
    or may be 'paged' to the next graph image.  If the operator pages or
    saves the graph, a status of PASS (1) will be returned to the
    application. Any status returned that is not PASS (1) should result in
    the application program terminating the program. The use of plot
    (0,0,10) should not be used to 'page' the display as a status will not
    be returned with this request. Note: The last image is always displayed
    using ZrtClose or CALL PLOT (0,0,999). 

  C Interface

    int ZrtPage ()

  Fortran Interface

    call xrtpage (status)
    integer status

  =====

  ZrtClose () 
  plot (0,0,999) 

    ZrtClose () is called to display the last graph in a series of graph
    images. When XRTPS receives this request, XRTPS displays the graph
    currently being created, displays the three menu items, and waits for
    the operator to initiate action on one of the three switches. XRTPS will
    process the 'save' switch action and save the image in the output
    PostScript file. The switch action to 'page' or 'exit' will not save the
    current image to the output PostScript file, and will return control to
    the application. 

  C Interface

    void ZrtClose (void)

  Fortran Interface

    call plot (0, 0, 999)


 ============================================================================

2 Documentation

   The software described in this document interfaces with MOTIF and with
   the interfaces described in XRT/graph for Motif; Widget  Programmer's
   Guide & Reference Manual, 'Software Objects for GUI Development'. Manuals
   are available for VMS & UNIX platforms.

2 History

  XRTPS is modeled after the program 'CPLTPR.COM'; a collection of
  subroutines which processes requests for plotting (x,y) coordinate
  positions, alphanumeric labels and titles, and upon completion,  creates
  an output file capable of being printed on a Printronix printer.

  Original CPLTPR Programming effort:
       Calcomp plotting written in Pascal by Frank Evans   March 1986
       Converted from Pascal to Fortran by Susanna Gross   April 1987
       Converted to Printronix plotting by Frank Evans     May 1987

  Original XRTPS Programmer, including XRTPS UNIX portablity:
                                      J. C. Turner (CRI) May 1995

  Fixes to prevent crash in PLOT(0,0,999) with NODISP and to remove some
  compiler warnings                           - lwk -  July 2006

  XRTPS Source Language: C
  XRTPS FORTRAN Bridge Source Language: C


$ Return
$!#############################################################################
