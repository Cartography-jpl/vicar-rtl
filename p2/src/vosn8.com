$!****************************************************************************
$!
$! Build proc for MIPL module vosn8
$! VPACK Version 1.5, Wednesday, November 18, 1992, 10:35:52
$!
$! Execute by entering:		$ @vosn8
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
$ write sys$output "*** module vosn8 ***"
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
$   if F$SEARCH("vosn8.imake") .nes. ""
$   then
$      vimake vosn8
$      purge vosn8.bld
$   else
$      if F$SEARCH("vosn8.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vosn8
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vosn8.bld "STD"
$   else
$      @vosn8.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vosn8.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vosn8.com -
	-s vosn8.f -
	-i vosn8.imake -
	-t tvosn8.f tvosn8.imake tvosn8.pdf tstvosn8.pdf -
	-o vosn8.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vosn8.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      Subroutine  Vosn8(RESTAB)
c
C     20 MAY 80   ...JAM...   INITIAL RELEASE
c     18 Nov 92   ...WPL...   Ported for UNIX Conversion
c
      REAL*4 RESTAB(800)
C
C     VIKING 1976 CALIBRATION ON SPC 2 A   S/N 8
C     GENERATED MARCH 1976 BY GARY YAGI
c
      REAL      VI2A(800)
      BYTE      BH(8)
      INTEGER   IH
      BYTE      BV(8)
      INTEGER   IV
      BYTE      BTIE(8)
c
c      REAL VI2A(800),SN81(74)/'NAH ','    ',Z00000015,'NAV ','    ',
c     &Z00000008,'TIEP','OINT',
c
       Real*4  SN81(66)/
     *  43.852,  30.093,   3.500,  25.000,  43.852,  30.093,
     *   3.500,  25.000,  44.106, 148.877,   7.274, 141.753,
     *  44.106, 148.877,   7.274, 141.753,  43.887, 268.209,
     *   9.987, 258.271,  43.887, 268.209,   9.987, 258.271,
     *  43.787, 387.288,  12.470, 374.257,  43.787, 387.288,
     *  12.470, 374.257,  43.904, 506.074,  14.513, 490.132,
     *  43.904, 506.074,  14.513, 490.132,  43.746, 625.065,
     *  16.197, 605.649,  43.746, 625.065,  16.197, 605.649,
     *  43.556, 744.246,  17.759, 721.165,  43.556, 744.246,
     *  17.759, 721.165,  43.829, 862.991,  19.149, 836.597,
     *  43.829, 862.991,  19.149, 836.597,  43.945, 981.924/
      REAL*4 SN82(66)/
     *  20.355, 952.034,  43.945, 981.924,  20.355, 952.034,
     *  44.180,1100.764,  21.391,1067.420,  44.180,1100.764,
     *  21.391,1067.420,  44.337,1220.333,  22.100,1183.000,
     *  44.337,1220.333,  22.100,1183.000, 177.166,  30.062,
     * 132.000,  25.200, 177.279,  89.487, 133.813,  83.673,
     * 177.279,  89.487, 133.813,  83.673, 176.920, 208.586,
     * 136.293, 200.136, 176.920, 208.586, 136.293, 200.136,
     * 176.521, 327.883, 138.281, 316.127, 176.521, 327.883,
     * 138.281, 316.127, 176.098, 446.685, 140.346, 431.511,
     * 176.098, 446.685, 140.346, 431.511, 176.094, 565.399,
     * 142.183, 546.864, 176.094, 565.399, 142.183, 546.864/
      REAL*4 SN83(66)/
     * 176.191, 684.589, 143.706, 662.296, 176.191, 684.589,
     * 143.706, 662.296, 176.131, 803.572, 145.294, 777.464,
     * 176.131, 803.572, 145.294, 777.464, 176.414, 922.592,
     * 146.627, 892.948, 176.414, 922.592, 146.627, 892.948,
     * 176.595,1041.531, 147.711,1008.175, 176.595,1041.531,
     * 147.711,1008.175, 177.044,1160.402, 149.151,1122.914,
     * 177.044,1160.402, 149.151,1122.914, 177.447,1219.905,
     * 150.500,1180.300, 309.972,  30.026, 260.500,  25.300,
     * 309.972,  30.026, 260.500,  25.300, 309.812, 148.937,
     * 263.354, 142.198, 309.812, 148.937, 263.354, 142.198,
     * 309.194, 267.848, 265.436, 257.520, 309.194, 267.848/
      REAL*4 SN84(66)/
     * 265.436, 257.520, 309.110, 387.044, 267.609, 373.361,
     * 309.110, 387.044, 267.609, 373.361, 308.952, 505.857,
     * 269.495, 488.589, 308.952, 505.857, 269.495, 488.589,
     * 309.021, 624.906, 271.172, 603.761, 309.021, 624.906,
     * 271.172, 603.761, 309.192, 744.176, 272.713, 719.192,
     * 309.192, 744.176, 272.713, 719.192, 309.135, 862.903,
     * 273.866, 834.050, 309.135, 862.903, 273.866, 834.050,
     * 309.416, 982.140, 275.634, 949.397, 309.416, 982.140,
     * 275.634, 949.397, 309.864,1100.952, 277.312,1064.248,
     * 309.864,1100.952, 277.312,1064.248, 310.287,1220.129,
     * 278.500,1178.500, 310.287,1220.129, 278.500,1178.500/
      REAL*4 SN85(66)/
     * 442.800,  29.932, 389.800,  25.500, 442.523,  89.563,
     * 390.913,  84.255, 442.523,  89.563, 390.913,  84.255,
     * 442.404, 208.465, 393.447, 200.249, 442.404, 208.465,
     * 393.447, 200.249, 441.885, 327.534, 395.091, 315.573,
     * 441.885, 327.534, 395.091, 315.573, 441.922, 446.419,
     * 397.218, 430.856, 441.922, 446.419, 397.218, 430.856,
     * 442.233, 565.654, 399.307, 546.469, 442.233, 565.654,
     * 399.307, 546.469, 442.094, 684.674, 400.512, 661.004,
     * 442.094, 684.674, 400.512, 661.004, 441.931, 803.595,
     * 401.765, 776.165, 441.931, 803.595, 401.765, 776.165,
     * 442.009, 922.387, 403.167, 890.931, 442.009, 922.387/
      REAL*4 SN86(66)/
     * 403.167, 890.931, 442.046,1041.450, 404.552,1005.998,
     * 442.046,1041.450, 404.552,1005.998, 442.369,1160.410,
     * 406.150,1120.713, 442.369,1160.410, 406.150,1120.713,
     * 442.820,1220.175, 407.400,1177.800, 575.597,  30.298,
     * 518.500,  26.000, 575.597,  30.298, 518.500,  26.000,
     * 575.561, 149.143, 521.494, 142.378, 575.561, 149.143,
     * 521.494, 142.378, 575.382, 268.157, 523.346, 258.110,
     * 575.382, 268.157, 523.346, 258.110, 574.837, 387.025,
     * 524.571, 373.180, 574.837, 387.025, 524.571, 373.180,
     * 574.854, 505.977, 526.585, 488.402, 574.854, 505.977,
     * 526.585, 488.402, 575.000, 625.000, 528.191, 603.413/
      REAL*4 SN87(66)/
     * 575.000, 625.000, 528.191, 603.413, 574.840, 744.058,
     * 529.556, 718.295, 574.840, 744.058, 529.556, 718.295,
     * 575.130, 863.009, 530.897, 833.425, 575.130, 863.009,
     * 530.897, 833.425, 575.090, 982.037, 532.360, 948.162,
     * 575.090, 982.037, 532.360, 948.162, 575.312,1100.951,
     * 533.849,1063.106, 575.312,1100.951, 533.849,1063.106,
     * 575.702,1220.061, 536.000,1177.800, 575.702,1220.061,
     * 536.000,1177.800, 708.374,  30.650, 647.600,  25.700,
     * 708.255,  89.936, 648.542,  83.960, 708.255,  89.936,
     * 648.542,  83.960, 708.357, 208.795, 650.883, 200.075,
     * 708.357, 208.795, 650.883, 200.075, 708.178, 327.842/
      REAL*4 SN88(66)/
     * 652.645, 315.458, 708.178, 327.842, 652.645, 315.458,
     * 707.970, 446.483, 654.383, 430.383, 707.970, 446.483,
     * 654.383, 430.383, 708.034, 565.541, 655.842, 545.851,
     * 708.034, 565.541, 655.842, 545.851, 707.966, 684.661,
     * 657.471, 660.536, 707.966, 684.661, 657.471, 660.536,
     * 707.943, 803.584, 658.589, 775.515, 707.943, 803.584,
     * 658.589, 775.515, 708.055, 922.331, 659.967, 890.445,
     * 708.055, 922.331, 659.967, 890.445, 708.223,1041.439,
     * 661.762,1005.431, 708.223,1041.439, 661.762,1005.431,
     * 708.469,1160.228, 663.645,1119.919, 708.469,1160.228,
     * 663.645,1119.919, 708.524,1219.790, 664.500,1176.700/
      REAL*4 SN89(66)/
     * 841.106,  30.772, 776.500,  25.300, 841.106,  30.772,
     * 776.500,  25.300, 840.889, 149.202, 778.311, 141.252,
     * 840.889, 149.202, 778.311, 141.252, 840.955, 268.243,
     * 780.450, 257.294, 840.955, 268.243, 780.450, 257.294,
     * 840.944, 387.264, 782.251, 372.948, 840.944, 387.264,
     * 782.251, 372.948, 840.825, 506.072, 783.691, 487.927,
     * 840.825, 506.072, 783.691, 487.927, 840.784, 624.985,
     * 784.975, 602.890, 840.784, 624.985, 784.975, 602.890,
     * 840.974, 744.166, 786.503, 717.685, 840.974, 744.166,
     * 786.503, 717.685, 840.725, 862.894, 787.654, 832.670,
     * 840.725, 862.894, 787.654, 832.670, 840.854, 981.837/
      REAL*4 SN810(66)/
     * 789.006, 947.646, 840.854, 981.837, 789.006, 947.646,
     * 840.965,1100.649, 790.727,1062.493, 840.965,1100.649,
     * 790.727,1062.493, 841.066,1219.487, 792.500,1176.600,
     * 841.066,1219.487, 792.500,1176.600, 974.009,  30.782,
     * 905.500,  24.500, 973.815,  90.133, 906.372,  83.148,
     * 973.815,  90.133, 906.372,  83.148, 974.180, 208.987,
     * 909.056, 199.313, 974.180, 208.987, 909.056, 199.313,
     * 974.104, 328.174, 910.318, 315.081, 974.104, 328.174,
     * 910.318, 315.081, 973.765, 446.862, 911.365, 430.202,
     * 973.765, 446.862, 911.365, 430.202, 973.831, 565.706,
     * 912.818, 545.160, 973.831, 565.706, 912.818, 545.160/
      REAL*4 SN811(66)/
     * 973.812, 684.658, 914.193, 659.933, 973.812, 684.658,
     * 914.193, 659.933, 973.762, 803.410, 915.488, 774.829,
     * 973.762, 803.410, 915.488, 774.829, 973.856, 922.351,
     * 916.676, 889.832, 973.856, 922.351, 916.676, 889.832,
     * 974.052,1041.128, 918.181,1004.783, 974.052,1041.128,
     * 918.181,1004.783, 973.984,1159.959, 919.727,1119.455,
     * 973.984,1159.959, 919.727,1119.455, 974.016,1219.425,
     * 920.500,1176.500,1106.427,  31.247,1034.200,  23.700,
     *1106.427,  31.247,1034.200,  23.700,1106.561, 149.616,
     *1035.646, 139.901,1106.561, 149.616,1035.646, 139.901,
     *1106.545, 268.457,1037.280, 256.183,1106.545, 268.457/
      REAL*4 SN812(66)/
     *1037.280, 256.183,1106.587, 387.428,1038.725, 371.588,
     *1106.587, 387.428,1038.725, 371.588,1106.679, 506.205,
     *1040.227, 486.867,1106.679, 506.205,1040.227, 486.867,
     *1106.587, 625.070,1041.489, 601.783,1106.587, 625.070,
     *1041.489, 601.783,1106.529, 743.957,1042.643, 716.726,
     *1106.529, 743.957,1042.643, 716.726,1106.562, 862.567,
     *1043.747, 831.627,1106.562, 862.567,1043.747, 831.627,
     *1106.638, 981.516,1044.912, 946.608,1106.638, 981.516,
     *1044.912, 946.608,1106.624,1100.319,1046.430,1061.571,
     *1106.624,1100.319,1046.430,1061.571,1106.891,1219.303,
     *1048.500,1175.500,1106.891,1219.303,1048.500,1175.500/
c
c
      EQUIVALENCE  (VI2A(1), BH(1))
      Equivalence  (VI2A(3), IH)
      Equivalence  (VI2A(4), BV(1))
      EQuivalence  (VI2A(6), IV)
      Equivalence  (VI2A(7), BTIE(1))
c
      EQUIVALENCE (VI2A(9),SN81(1)),(VI2A(75),SN82(1)),
     & (VI2A(141),SN83(1)),(VI2A(207),SN84(1)),(VI2A(273),SN85(1)),
     & (VI2A(339),SN86(1)),
     & (VI2A(405),SN87(1)),(VI2A(471),SN88(1)),(VI2A(537),SN89(1)),
     & (VI2A(603),SN810(1)),(VI2A(669),SN811(1)),(VI2A(735),SN812(1))
c
c
      Call MVCL('NAH     ',BH, 8)
      IH = 21
      Call MVCL('NAV     ',BV, 8)
      IV =  8
      Call MVCL('TIEPOINT',BTIE, 8)
c
      Do  20  IJ = 1, 800
        RESTAB(IJ) = VI2A(IJ)
20    Continue
c
c      CALL MVL(VI2A,RESTAB,3200)
c
      Return
      End
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vosn8.imake
/* Imake file for VICAR subroutine VOSN8  */

#define SUBROUTINE  vosn8

#define MODULE_LIST  vosn8.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tvosn8.f
      INCLUDE 'VICMAIN_FOR'
c
      Subroutine  Main44
c
C  PROGRAM TVOSN8
C
C  THIS IS A TESTPROGRAM FOR SUBROUTINE VOSN8.
C  VOSN8 PROVIDES THE CALLING RPOGRAM A BUFFER CONTAINING
C  NOMINAL VO DISTORTION CORRECTION DATA IN GEOMA
C  FORMAT.  VOSN8 RETURNS DATA FOR THE CAMERA.
c
      REAL*4  BUF(800)
c
      Call  Vosn8(BUF)
c
      CALL QPRINT(' FIRST EIGHT ELEMENTS IN BUF, STARTING WITH NAH',47)
c
c     CALL PRNT(7,8,BUF)
c
      Call Prnt(99, 8, BUF(1), ' FIRST 2 BUF = .')
      Call Prnt( 4, 1, BUF(3), ' Value of NAH = .')
      Call Prnt(99, 8, BUF(4), ' NEXT  2 BUF = .')
      Call Prnt( 4, 1, BUF(6), ' Value of NAV = .')
      Call Prnt(99, 8, BUF(7), ' NEXT  2 BUF = .')
c
      CALL QPRINT(' GEOMA PARAMETERS:',18)
      CALL PRNT(7,80,BUF(81),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(161),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(241),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(321),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(401),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(481),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(561),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(641),'.')
      CALL QPRINT(' ',1)
      CALL PRNT(7,80,BUF(721),'.')
      CALL QPRINT(' ',1)
c
      Return
      End
$!-----------------------------------------------------------------------------
$ create tvosn8.imake
/* IMAKE file for Test of VICAR subroutine  VOSN8  */

#define PROGRAM  tvosn8

#define MODULE_LIST tvosn8.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define   LIB_RTL         
#define   LIB_TAE           
/*  #define   LIB_LOCAL    */      /*  Disable during delivery   */
#define   LIB_P2SUB         
$!-----------------------------------------------------------------------------
$ create tvosn8.pdf
Process 
End-Proc
$!-----------------------------------------------------------------------------
$ create tstvosn8.pdf
Procedure
Refgbl $Echo
Body
Let  _Onfail="Continue"
Let  $Echo="Yes"
! THIS IS A TEST OF SUBROUTINE VOSN8.
! VOSN8 PROVIDES THE CALLING PROGRAM A BUFFER CONTAINING
! NOMINAL VO DISTORTION CORRECTION DATA IN GEOMA FORMAT.
! VOSN8 RETURNS DATA FOR THE CAMERA.  THE DATA IS RETURNED
! IN AN 800 ELEMENT ARRAY.  THE VALUES ARE INITIALIZED IN THE
! SUBROUTINE.
TVOSN8
Let  $Echo="No"
End-Proc
$ Return
$!#############################################################################
$Other_File:
$ create vosn8.hlp
1 VOSN8

2  PURPOSE

     To provide the calling program a buffer containing nominal Viking
     Orbiter distortion correction data in the GEOMA format.

2  CALLING SEQUENCE

     CALL VOSN8(BUF)

     BUF    is an 800 word array of GEOMA parameters returned.

     VOSN4 should be called to get data for the camera serial number 4.
     VOSN6 should be called to get data for the camera serial number 6.
     VOSN7 should be called to get data for the camera serial number 7.
     VOSN8 should be called to get data for the camera serial number 8.

2  OPERATION

     The data in the array is similar to the format as the parameter
     file which can be input to GEOMA.  The difference between the
     two formats is in the first word.  This subroutine begins with
     NAH and the first word in the GEOMA dataset is the number of words
     (800) following the first word.

2  HISTORY

     Original Programmer:  Gary Yagi
     Current Cognizant Programmer:  Joel Mosher
     Source Language:  Fortran
     Latest Revision: 1, 28 July 1980

     Ported for UNIX Conversion:   W.P. Lee, Nov-18-1992
$ Return
$!#############################################################################
