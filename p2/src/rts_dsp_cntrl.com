$!****************************************************************************
$!
$! Build proc for MIPL module rts_dsp_cntrl
$! VPACK Version 1.8, Thursday, April 10, 1997, 01:47:06
$!
$! Execute by entering:		$ @rts_dsp_cntrl
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
$ write sys$output "*** module rts_dsp_cntrl ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to rts_dsp_cntrl.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("rts_dsp_cntrl.imake") .nes. ""
$   then
$      vimake rts_dsp_cntrl
$      purge rts_dsp_cntrl.bld
$   else
$      if F$SEARCH("rts_dsp_cntrl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rts_dsp_cntrl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rts_dsp_cntrl.bld "STD"
$   else
$      @rts_dsp_cntrl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rts_dsp_cntrl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rts_dsp_cntrl.com -
	-s rts_dsp_cntrl.c -
	-i rts_dsp_cntrl.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rts_dsp_cntrl.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"

#include <math.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#if VMS_OS
#include <file.h>
#include <processes.h>
#include <unixio.h>
#define F_SETFL		4
#endif

#include "file_name.h"
#include "rts_typedefs.h"
#include "rts_const_defs.h"
#include "rts_logging.h"
#include "rts_ipc_defs.h"
#include "rts_pkt_defs.h"
#include "rts_param_defs.h"
#include "rts_inst_display.h"
#include "DisplayInstrumentDefs.h"

#define  MODULE_NAME		"RDC"
#define  MAX_STRTCH_TBL		 16

/***  Function Prototypes  ***/
static	void	rts_dsp_auto_strtch( int *, int *, int *);
static	void	rts_dsp_lut( int );

/***  Global Declarations  ***/
static	char	LogMsgBuf[256];
static	int	dsp_s_lut[3][256];
static	InstrumentType	dsp_crrnt_inst = dsp_invalid;
static	RtsStretchCntrl_typ	Stretch[3];
static	RtsFilterCntrl_typ	Filter[3];

char		dsp_activity[24],
		dsp_alternate_strg[48],		/* Used for NIMS LINK strg */
		dsp_device[64],
		dsp_img_info[512],
		dsp_pic_no[24],
                dsp_program[4],
		dsp_project[4],
		dsp_sclk[16],
		dsp_target[16];
int		dsp_border = FALSE,
		dsp_ert_year = 1776,
		dsp_ert_day = 186,
		dsp_ert_hour = 14,
		dsp_ert_min = 5,
		dsp_input_bins[256],
		dsp_no_out_hist = FALSE,
		dsp_o_e_flag = 0,
		dsp_output_bins[256],
		dsp_pseudocolor = FALSE,
		*dsp_s_histogram = dsp_input_bins,
		dsp_start_line = 0,
		dsp_start_samp = 0,
		dsp_zoom_factor = -2;

/*******************************************************************************
/*  				 fillDeviceName
/*
/*	Copies the parameter defined X-window device name into the supplied
/*  character buffer.
/******************************************************************************/
void	fillDeviceName(
  char	*buffer)
{
  strcpy(buffer,dsp_device);
  return;
}

/*******************************************************************************
/*				fillImageInfoBuffer
/*
/*	Copies three lines of upto 68 characters into a supplied character
/*  buffer.  This buffer is to be displayed at the bottom of the RT display
/*  and contains image specific information.
/*
/*  History:
/*  6-15-1998	T. Nguyen	Modified for Y2K.
/******************************************************************************/
void	fillImageInfoBuffer(
  char	*buffer)
{ char	line_buffer[128];
  time_t	time_now;

  time(&time_now);

  strcpy(buffer,dsp_img_info);
  strftime(line_buffer,128,"      %d-%b-%Y",localtime(&time_now));
  strcat(buffer,line_buffer);

  return;
}

/*******************************************************************************
/*				fillRGBImageStretchLut
/*
/*	
/******************************************************************************/
void	fillRGBImageStretchLut(
  int	*RedBwLut,
  int	*GrnLut,
  int	*BluLut)
{ int	idx;

  /***  Create Lut(s)  ***/
  if (RedBwLut) rts_dsp_lut(0);
  if (GrnLut) rts_dsp_lut(1);
  if (BluLut) rts_dsp_lut(2);

  /***  Copy Lut(s)  ***/
  for (idx=0; idx<256; idx++)
  { if (RedBwLut) RedBwLut[idx] = dsp_s_lut[0][idx];
    if (GrnLut) GrnLut[idx] = dsp_s_lut[1][idx];
    if (BluLut) BluLut[idx] = dsp_s_lut[2][idx];
  }

  return;
}

/*******************************************************************************
/*				fillImageStretchLut
/*
/*	
/******************************************************************************/
void	fillImageStretchLut(
  int	*RedBwLut)
{ 

  fillRGBImageStretchLut(RedBwLut, NULL, NULL);
  return;
}

/*******************************************************************************
/*				fillPicnoErtInfoBuffer
/*
/*	Copies a data into a supplied character buffer for the PicnoErt area
/*  in the RT display.
/******************************************************************************/
void	fillPicnoErtInfoBuffer(
  char	*buffer)
{
  sprintf(buffer,"  %-14.14s\n  %04d-%03d:%02d:%02d",
          dsp_pic_no,dsp_ert_year,dsp_ert_day,dsp_ert_hour,dsp_ert_min);

  return;
}

/*******************************************************************************
/*				fillStretchInfoBuffer
/*
/*	Copies a data into a supplied character buffer for the stretch info
/*  area in the RT display.
/******************************************************************************/
void	fillStretchInfoBuffer(
  char	*buffer)
{ char	line_buffer[128];

  /***	Fill Filter lines (first 2 lines)  ***/
  if (Filter[0].On)
  { sprintf(buffer,"ADD: %03d %03d %03d\n",
            Filter[0].Length,Filter[0].Thresh,Filter[0].Addback);

    sprintf(line_buffer,"     %03d %03d %03d\n",
            Filter[0].Pweight,Filter[0].Nweight,Filter[0].Median);
    strcat(buffer,line_buffer);
  } else sprintf(buffer,"%-18.18s\n%-18.18s\n",
                 dsp_alternate_strg,&dsp_alternate_strg[18]);

  /***  Fill Stretch Lines (next 3 lines)  ***/
  if (Stretch[0].Type != RAW_STRTCH)
  { sprintf(line_buffer,"STRETCH: %-12.12s\n",
            dsp_strtch_names[Stretch[0].Type]);
    strcat(buffer,line_buffer);

    sprintf(line_buffer,"L%03d=%03d   H%03d=%03d\n",
            Stretch[0].InputDn[0],Stretch[0].OutputDn[0],
            Stretch[0].InputDn[1],Stretch[0].OutputDn[1]);
    strcat(buffer,line_buffer);

    if (Stretch[0].Type == AUTO_STRTCH)
    { sprintf(line_buffer," %03d+%5.2f%% %03d+%5.2f%%\n",
              Stretch[0].Start[0],Stretch[0].Percent[0],
              Stretch[0].Start[1],Stretch[0].Percent[1]);
      strcat(buffer,line_buffer);
    } else strcat(buffer,"\n");
  } else strcat(buffer,"\n\n\n");

  sprintf(line_buffer,"%-20.20s",dsp_device);
  strcat(buffer,line_buffer);

  return;
}

/*******************************************************************************
/*				fillTitleBarInfoBuffer
/*
/*	Generates the the title bar for the RT display in the suppliec
/*  character buffer.
/******************************************************************************/
void	fillTitleBarInfoBuffer(
  char	*buffer)
{ char	ProgNamePlus[8];

  strcpy(ProgNamePlus,dsp_program);
  if (strcmp(dsp_program,"RTS") == 0 && (int)strcspn(dsp_device,":") > 0 &&
      (int)strlen(dsp_device) > (int)strcspn(dsp_device,":"))
  { if (isdigit(*(dsp_device+(strcspn(dsp_device,":")-1))))
       sprintf(ProgNamePlus,"%s-%c",
               dsp_program,*(dsp_device+(strcspn(dsp_device,":")-1)));
  }

  sprintf(buffer,"MIPS  %-6.6s  %-4.4s  %-12.12s  %-20.20s  %-15.15s",
          ProgNamePlus,dsp_project,dsp_target,dsp_activity,dsp_sclk);
  return;
}

/*******************************************************************************
/*	        			fillZoomParams
/*
/******************************************************************************/
void    fillZoomParams(
  int *ZoomX,
  int *ZoomY,
  int *Sline,
  int *Ssample,
  int *odd_even_flag)
{
  *ZoomX = dsp_zoom_factor;
  *ZoomY = dsp_zoom_factor;
  *Sline = dsp_start_line;
  *Ssample = dsp_start_samp;
  *odd_even_flag = dsp_o_e_flag;

  return;
}

/******************************************************************************
/*				getBorderMode
/*
/*	Returns TRUE if an X-window border should be placed around the
/*      display window.
/*****************************************************************************/
int	getBorderMode()
{
  return (dsp_border);
}

/*******************************************************************************
/*				getHistInputArray
/*
/*	Returns the input image histogram for the current
/******************************************************************************/
int     *getHistInputArray()
{ static int	dsp_local_bins[256];

  memset((void *)dsp_local_bins,0,sizeof(dsp_local_bins));

  /***  Move this out to to the Imaging Stuff  ***/
  shrinkHist(dsp_s_histogram,dsp_local_bins);

  return ( dsp_local_bins );
}

/*******************************************************************************
/*				getHistOutputArray
/*
/*	Returns the output image histogram for the current
/******************************************************************************/
int     *getHistOutputArray()
{ int	idx,
	max_val = 1;

  memset((void *)dsp_output_bins,0,sizeof(dsp_output_bins));

  if (dsp_no_out_hist) return ( dsp_output_bins );

  /***  Stretch It  ***/
  for (idx=0; idx<256; idx++)
      dsp_output_bins[dsp_s_lut[0][idx]] = dsp_s_histogram[idx];

  /***  Move this out to to the Imaging Stuff  ***/
  shrinkHist(dsp_output_bins,dsp_output_bins);

  return ( dsp_output_bins );
}

/*******************************************************************************
/*				getPseudoMode
/*
/* 	function to return PseudoColor turned on or off 
/******************************************************************************/
int  getPseudoMode()  
{
  return (dsp_pseudocolor); 
}

/*******************************************************************************
/*				INIT_RTS_DSP_GLOBALS
/*
/*	Initializes all the global variables for the first time, so that there
/*  are no NULL pointers, and other horrible memory stompers
/******************************************************************************/
void	init_rts_dsp_globals()
{ int	band,
	idx;

  /***  General variables  ***/
  strcpy(dsp_activity,"INACTIVE");
  strcpy(dsp_device,"unix:0.0");
  strcpy(dsp_pic_no,"   N/A");
  strcpy(dsp_project,"N/A");
  strcpy(dsp_program,"RTS");
  strcpy(dsp_sclk,"N/A");
  strcpy(dsp_target,"UNDEFINED");

  for (band=0; band<3; band++)
  { memset((void *)&Filter[band],0,sizeof(RtsFilterCntrl_typ));
    memset((void *)&Stretch[band],0,sizeof(RtsStretchCntrl_typ));
    Stretch[band].Type = RAW_STRTCH;
    Stretch[band].InputDn[0] = Stretch[band].InputDn[1] = 0;
    Stretch[band].OutputDn[0] = Stretch[band].OutputDn[1] = 255;
    Stretch[band].Start[0] = Stretch[band].Start[1] = 0;
    Stretch[band].Percent[0] = Stretch[band].Percent[1] = 0.0;
    Stretch[band].TblElements = 0;
    for (idx=0; idx<256; idx++) dsp_s_lut[band][idx] = idx;
  }
  dsp_s_histogram = dsp_input_bins;
  memset(dsp_img_info,0,sizeof(dsp_img_info));
  memset(dsp_alternate_strg,0,sizeof(dsp_alternate_strg));
  memset((void *)dsp_input_bins,0,sizeof(dsp_input_bins));
  memset((void *)dsp_output_bins,0,sizeof(dsp_output_bins));

  return;
}

/*******************************************************************************
/*				loadFilterParams
/*
/******************************************************************************/
int	loadFilterParams(
  RtsFilterCntrl_typ	*ClientFilter,
  int			Band)
{
  if (Band > 2 || Band < 0) return TRUE;

  memmove(&Filter[Band], ClientFilter, sizeof(RtsFilterCntrl_typ));

  return FALSE;
}

/*******************************************************************************
/*				loadStretchParams
/*
/******************************************************************************/
int	loadStretchParams(
  RtsStretchCntrl_typ	*ClientStretch,
  int			Band)
{
  if (Band > 2 || Band < 0) return TRUE;

  memmove(&Stretch[Band], ClientStretch, sizeof(RtsStretchCntrl_typ));

  return FALSE;
}

/*******************************************************************************
/*				RTS_DSP_AUTO_STRTCH
/*
/*	This function returns low and high input stretch points based on
/*      auto stretch parameters and the histogram.
/******************************************************************************/
static	void	rts_dsp_auto_strtch(
  int	*lowdn,
  int	*highdn,
  int	*histo)
{ int	area,
	dn;
  float	high_area,
	low_area;

  area = 0;
  for (dn=Stretch[0].Start[0]; dn<=(int)(Stretch[0].Start[1]); dn++)
      area += histo[dn];
  low_area = Stretch[0].Percent[0] * area / 100;
  high_area = Stretch[0].Percent[1] * area / 100;

  area = 0;
  for (dn=Stretch[0].Start[0]; area<low_area && dn<=(int)(Stretch[0].Start[1]);
       dn++) area += histo[dn];
  *lowdn = (dn) ? dn-1 : dn;

  area = 0;
  for (dn=Stretch[0].Start[1]; area<high_area && dn>=(int)(Stretch[0].Start[0]);
       dn--) area += histo[dn];
  *highdn = (dn < 255) ? dn+1 : dn;

  return;
}

/*******************************************************************************
/*				RTS_DSP_LUT
/*
/*	Generates the display lut based on the stretch parameters.
/******************************************************************************/
static	void	rts_dsp_lut(
  int	Band)
{ int	value_num,
	dn,
	level;
  int	str_points[MAX_STRTCH_TBL*2];		/*arguments for stretch*/
  int	str_num;				/*number of stretch points*/
  double	slope,
		offset;
		
  switch (Stretch[Band].Type) 
  { case MAN_STRTCH:					/* MANUAL STRETCH */
         str_points[0] = Stretch[Band].InputDn[0];
         str_points[1] = 0;
         str_points[2] = Stretch[Band].InputDn[1];
         str_points[3] = 255;
         str_num = 2;
    break;
           
    case TBL_STRTCH:					/* TABLE STRETCH  */
         for (value_num=0; value_num<Stretch[Band].TblElements; value_num++)
         { str_points[2*value_num] = Stretch[Band].Table[value_num][0];
           str_points[2*value_num+1] = Stretch[Band].Table[value_num][1];
         }
         str_num = Stretch[Band].TblElements;
    break;
           
    case AUTO_STRTCH:					/* AUTO STRETCH   */
         rts_dsp_auto_strtch(&str_points[0],&str_points[2],dsp_s_histogram);
         str_points[1] = Stretch[Band].OutputDn[0];
         str_points[3] = Stretch[Band].OutputDn[1];
         str_num = 2;
    break;

    case LOG_STRTCH:					/* LOG STRETCH    */
         slope = 255.0 / (log((double)(1.0 + Stretch[Band].InputDn[1]) /
                              (1.0 + Stretch[Band].InputDn[0])) + .001);
         offset = -slope * log((double)Stretch[Band].InputDn[0] + 1.) + 0.5;

         for (dn=0; dn<256; dn++)
         { level = log(dn + 1.) * slope + offset;
           if (level < 0) dsp_s_lut[Band][dn] = 0;
           else if (level > 255) dsp_s_lut[Band][dn] = 255;
                else dsp_s_lut[Band][dn] = level;
         }

         Stretch[Band].OutputDn[0] = 0;
         Stretch[Band].OutputDn[1] = 255;
    return;

    case RAW_STRTCH:					/* RAW STRETCH   */
         for (dn=0; dn<256; dn++) dsp_s_lut[Band][dn] = dn;

         Stretch[Band].InputDn[0] = Stretch[Band].OutputDn[0] = 0;
         Stretch[Band].InputDn[1] = Stretch[Band].OutputDn[1] = 255;
    return;

    default:
    break;
  }

  value_num = 2;
  for (dn = 0; dn < 256; dn++)
  { for (; value_num<(2*str_num-2) && dn>str_points[value_num];
           value_num += 2) ;

    level = (str_points[value_num + 1] - str_points[value_num - 1]) *
            (dn - str_points[value_num - 2]) /
            (str_points[value_num] - str_points[value_num - 2] + .001) +
            str_points [value_num - 1] + .5;

    if (level < 0) dsp_s_lut[Band][dn] = 0;
    else if (level > 255) dsp_s_lut[Band][dn] = 255;
         else dsp_s_lut[Band][dn] = level;
  }

  Stretch[Band].InputDn[0] = str_points[0];
  Stretch[Band].InputDn[1] = str_points[2 * str_num - 2];
  Stretch[Band].OutputDn[0] = str_points[1];
  Stretch[Band].OutputDn[1] = str_points[2 * str_num - 1];

  return;
}

/*******************************************************************************
/*				shrinkHist
/*
/******************************************************************************/
void	shrinkHist(
  int	*inArray,
  int	*outArray)
{ int	dn;

  /***  Include "shrink factor" ?  ***/

  for (dn=0; dn<128; dn++) outArray[dn] = inArray[(2*dn)] + inArray[((2*dn)+1)];

  return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rts_dsp_cntrl.imake
/******************************************************************************
/*
/*                     IMAKE FILE FOR MODULE rts_dsp_cntrl
/*
/*   To Create the build file give the command:
/*
/*		$ vimake rts_dsp_cntrl			(VMS)
/*   or
/*		% vimake rts_dsp_cntrl			(Unix)
/*
/*****************************************************************************/

/***  Define for whom this file exisits  ***/
#define SUBROUTINE rts_dsp_cntrl

/***  List all modules which are used by locally by this module  ***/
#define MODULE_LIST rts_dsp_cntrl.c

#define MAIN_LANG_C
#define USES_ANSI_C

/***  Specify  Program or Subroutine specific DEFINES  ***/
#ifdef PROGRAM
#define R2LIB
#define LIB_FORTRAN
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
#ifdef PROGRAM
#define LIB_LOCAL
#endif

#if VMS_OS
#define LOCAL_LIBRARY test_lib.olb
#else
#define LOCAL_INCLUDE -I$(IncludePath)
#define LOCAL_LIBRARY $(ObjectPath)/librts.a
#endif
/***  End of local library definitions  ***/
/**********  End of rts_dsp_cntrl imake file  **********/
$ Return
$!#############################################################################
