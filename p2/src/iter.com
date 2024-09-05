$!****************************************************************************
$!
$! Build proc for MIPL module iter
$! VPACK Version 1.5, Thursday, January 21, 1993, 13:33:43
$!
$! Execute by entering:		$ @iter
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
$ write sys$output "*** module iter ***"
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
$   if F$SEARCH("iter.imake") .nes. ""
$   then
$      vimake iter
$      purge iter.bld
$   else
$      if F$SEARCH("iter.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake iter
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @iter.bld "STD"
$   else
$      @iter.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create iter.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack iter.com -
	-s iter.f -
	-i iter.imake -
	-t titer.f titer.imake titer.pdf tstiter.pdf -
	-o iter.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create iter.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE ITER(CHI,PHI,EPS,PI)
C     21 JAN 93    SP  PORTED TO UNIX
C	5/21/83 -JAM- CONVERT VAX
C     16 JAN 78    ...JJL...   INITIAL RELEASE
C  GET PHI FROM CHI
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      VAL(EPS,PHIL)=((1.D0+EPS*DSIN(PHIL))/(1.D0-EPS*DSIN(PHIL)))**
     * (EPS/2.D0)
C==================================================================
      A=DTAN(PI/4.D0+CHI/2.D0)
      Q=A*VAL(EPS,CHI)
      PHIL=2.D0*(DATAN(Q)-PI/4.D0)
10    Q=A*VAL(EPS,PHIL)
      PHI =2.D0*(DATAN(Q)-PI/4.D0)
      IF(DABS(PHI-PHIL).LT.1.D-7) RETURN
      PHIL=PHI
      GO TO 10
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create iter.imake
/* Imake file for VICAR subroutine iter */

#define SUBROUTINE iter

#define MODULE_LIST iter.f

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create titer.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
C  THIS IS A TEST PROGRAM FOR SUBROUTINE ITER.  
C  ITER PROVIDES ITERATIVE CONVERGENCE FOR OBLATE
C  PLANETS FOR SUBROUTINE TRANV.   IN THE TEST PROGRAM
C  PROGRAM THE RADIAL ECCENTRICITY (EPS) VARIES FROM
C  .1D0 TO .9DO.

	DOUBLE PRECISION PI, CHI, PHI, EPS
	PI = 3.141592653589D0
	CHI = PI/4.D0
        EPS = .1D0
	DO 10 I = 1,9
	   CALL PRNT(8,1,EPS,'ECCENTRICITY = .')
           CALL ITER(CHI,PHI,EPS,PI)
	   CALL PRNT(8,1,PHI,'         PHI = .')
	   CALL XVMESSAGE(' ',' ')
           EPS = EPS + .1D0
 10	CONTINUE
	RETURN
	END
$!-----------------------------------------------------------------------------
$ create titer.imake
/* Imake file for Test of VICAR subroutine iter */

#define PROGRAM titer

#define MODULE_LIST titer.f 

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB


$!-----------------------------------------------------------------------------
$ create titer.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstiter.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!THIS IS A TEST OF SUBROUTINE ITER.
!ITER PROVIDES ITERATIVE CONVERGENCE FOR
!OBLATE PLANETS FOR SUBROUTINE TRANV.  IN THE
!TEST PROGRAM RADIAL ECCENTRICITY (EPS) VARIES
!FROM .1DO TO .9D0.
titer
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create iter.hlp
1  ITER

      To provide iterative convergence for oblate planets for
      subroutine TRANV.  JPL publication 77-7, page 8.

2  CALLING SEQUENCE from FORTRAN 

      CALL ITER(CHI,PHI,EPS,PI)

2  ARGUMENTS
      All of the arguments are DOUBLE PRECISION.

      CHI    conformal latitude 
      PHI    geodetic latitude (returned)
      EPS    radial eccentricity
      PI     pi

2  HISTORY

      Original Programmer: J. J. Lorre, 16 June 1977
      Current Cognizant Programmer: J. J. Lorre
      Source Language: Fortran
      Ported to UNIX:  Steve Pohorsky


$ Return
$!#############################################################################
