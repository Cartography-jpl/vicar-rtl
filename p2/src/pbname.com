$!****************************************************************************
$!
$! Build proc for MIPL module pbname
$! VPACK Version 1.7, Monday, March 21, 1994, 09:45:45
$!
$! Execute by entering:		$ @pbname
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
$ write sys$output "*** module pbname ***"
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
$ write sys$output "Invalid argument given to pbname.com file -- ", primary
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
$   if F$SEARCH("pbname.imake") .nes. ""
$   then
$      vimake pbname
$      purge pbname.bld
$   else
$      if F$SEARCH("pbname.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pbname
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pbname.bld "STD"
$   else
$      @pbname.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pbname.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pbname.com -
	-s pbname.f zpbname.c -
	-i pbname.imake -
	-t tpbname.f tzpbname.c tpbname.imake tpbname.pdf tstpbname.pdf -
	-o pbname.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pbname.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c
      Subroutine PBNAME(ID,PAR,*)
c
C  ROUTINE TO RETURN PLANET NAME GIVEN ID
c
C ID NUMBERING SCHEME IS CONSISTENT WITH GLL SPICE
C  LOOKUP TABLE FOR PLANET I.D.  FROM SEDR WORD # 9
c
      INTEGER  N/65/
       Character*12  Par
       Character*12  p8
       Character*12  pname(65) 
      INTEGER  LOOK(65)
      Data  LOOK /
c mercury
     + 199,
c venus
     + 299,
c earth
     + 399,301,
c mars
     + 499,401,402,
c jupiter
     + 599,501,502,503,504,505,506,507,508,509,510,511,512,513,
     + 514,515,516,
c saturn
     + 699,601,602,603,604,605,606,607,608,609,610,611,612,613,
     + 614,615,616,617,
c uranus
     + 799,701,702,703,704,705,706,707,708,709,710,711,712,713,
     + 714,715,
c neptune
     + 899,801,802,
c pluto
     + 999,901,
c gaspra
     + 9511010,
c ida
     + 2431010 /
c
       Data Pname /   'MERCURY     ','VENUS       ','EARTH       ',
     + 'MOON        ','MARS        ','PHOBOS      ','DEIMOS      ',
     + 'JUPITER     ','IO          ','EUROPA      ','GANYMEDE    ',
     + 'CALLISTO    ','AMALTHEA    ','HIMALIA     ','ELARA       ',
     + 'PASIPHAE    ','SINOPE      ','LYSITHEA    ','CARME       ',
     + 'ANANKE      ','LEDA        ','THEBE       ','ADRASTEA    ',
     + 'METIS       ','SATURN      ','MIMAS       ','ENCELADUS   ',
     + 'TETHYS      ','DIONE       ','RHEA        ','TITAN       ',
     + 'HYPERION    ','IAPETUS     ','PHOEBE      ','JANUS       ',
     + 'EPIMETHEUS  ','HELENE      ','TELESTO     ','CALYPSO     ',
     + 'ATLAS       ','PROMETHEUS  ','PANDORA     ','URANUS      ',
     + 'ARIEL       ','UMBRIEL     ','TITANIA     ','OBERON      ',
     + 'MIRANDA     ','CORDELIA    ','OPHELIA     ','BIANCA      ',
     + 'CRESSIDA    ','DESDEMONA   ','JULIET      ','PORTIA      ',
     + 'ROSALIND    ','BELINDA     ','PUCK        ','NEPTUNE     ',
     + 'TRITON      ','NEREID      ','PLUTO       ','CHARON      ',
     + 'GASPRA      ','IDA         ' /
c
C
      Do 5  I=1,N
       If (ID .EQ. LOOK(I)) GoTo  6
5     Continue
      RETURN1
6     Par = pname(i)      
      Return
C
C ROUTINE TO RETURN PLANET ID GIVEN PLANET NAME (PAR)
c
      ENTRY PBID(PAR,ID,*)
C
      p8=par
C
      call ccase(p8,1,-1)      ! ensure case is UPPER
      DO 10 I=1,N
      If (PNAME(I) .EQ. P8) GoTo 11
10     CONTINUE
      RETURN1
C
11    ID = LOOK(I)
c
      Return
      End
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zpbname.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#define  N  65

zpbname(id, par)
int        id   ;
char       par[12]  ;
{
 /*  Routine to return PLANET name given ID#   */
 /*   char   p8[12] ;      */
 char   pname[65][12] ;
 int    look[65] ;
 int    j, stat ;

 /*   ID numbering scheme is consistent with GLL SPICE
      LOOKUP table for PLANET I.D.  FROM SEDR WORD # 9   */

   /*   MERCURY  */
  look[0] = 199 ;  

   /*  VENUS     */   
  look[1] = 299 ;

   /*  EARTH     */
  look[2] = 399 ;  look[3] = 301 ;

   /*  MARS      */
  look[4] = 499 ;  look[5] = 401 ;  look[6] = 402 ;

   /*  JUPITER   */
  look[7] = 599 ;  
  for (j=0; j < 16; j++)  look[8+j] = 501 + j;

   /*  SATURN    */
  look[24] = 699 ;
  for (j=0; j < 17; j++)  look[25+j] = 601 + j ;

   /*  URANUS    */
  look[42] = 799 ;
  for (j=0; j < 15; j++)  look[43+j] = 701 + j ;

   /*  NEPTUNE    */
  look[58] = 899 ;   look[59] = 801 ;  look[60] = 802 ;

   /*  PLUTO      */
  look[61] = 999 ;    look[62] = 901 ;

   /*  GASPRA      */
  look[63] = 9511010  ;

   /*  IDA   */
  look[64] = 2431010;

  /*   Now the PLANETARY Body name  */

  strcpy(pname[0],  "MERCURY") ; 
  strcpy(pname[1],  "VENUS")   ;
  strcpy(pname[2],  "EARTH")   ;
  strcpy(pname[3],  "MOON")   ;
  strcpy(pname[4],  "MARS")   ;
  strcpy(pname[5],  "PHOBOS")   ;
  strcpy(pname[6],  "DEIMOS")   ;
  strcpy(pname[7],  "JUPITER")   ;
  strcpy(pname[8],  "IO")   ;
  strcpy(pname[9],  "EUROPA")   ;
  strcpy(pname[10], "GANYMEDE")   ;
  strcpy(pname[11], "CALLISTO")   ;
  strcpy(pname[12], "AMALTHEA")   ;
  strcpy(pname[13], "HIMALIA")   ;
  strcpy(pname[14], "ELARA")   ;
  strcpy(pname[15], "PASIPHAE")   ;
  strcpy(pname[16], "SINOPE")   ;
  strcpy(pname[17], "LYSITHEA")   ;
  strcpy(pname[18], "CARME")   ;
  strcpy(pname[19], "ANANKE")   ;
  strcpy(pname[20], "LEDA")   ;
  strcpy(pname[21], "THEBE")   ;
  strcpy(pname[22], "ADRASTEA")   ;
  strcpy(pname[23], "METIS")   ;
  strcpy(pname[24], "SATURN")   ;
  strcpy(pname[25], "MIMAS")   ;
  strcpy(pname[26], "ENCELADUS")   ;
  strcpy(pname[27], "TETHYS")   ;
  strcpy(pname[28], "DIONE")   ;
  strcpy(pname[29], "RHEA")   ;
  strcpy(pname[30], "TITAN")   ;
  strcpy(pname[31], "HYPERION")   ;
  strcpy(pname[32], "IAPETUS")   ;
  strcpy(pname[33], "PHOEBE")   ;
  strcpy(pname[34], "JANUS")   ;
  strcpy(pname[35], "EPIMETHEUS")   ;
  strcpy(pname[36], "HELENE")   ;
  strcpy(pname[37], "TELESTO")   ;
  strcpy(pname[38], "CALYPSO")   ;
  strcpy(pname[39], "ATLAS")   ;
  strcpy(pname[40], "PROMETHEUS")   ;
  strcpy(pname[41], "PANDORA")   ;
  strcpy(pname[42], "URANUS")   ;
  strcpy(pname[43], "ARIEL")   ;
  strcpy(pname[44], "UMBRIEL")   ;
  strcpy(pname[45], "TITANIA")   ;
  strcpy(pname[46], "OBERON")   ;
  strcpy(pname[47], "MIRANDA")   ;
  strcpy(pname[48], "CORDELIA")   ;
  strcpy(pname[49], "OPHELIA")   ;
  strcpy(pname[50], "BIANCA")   ;
  strcpy(pname[51], "CRESSIDA")   ;
  strcpy(pname[52], "DESDEMONA")   ;
  strcpy(pname[53], "JULIET")   ;
  strcpy(pname[54], "PORTIA")   ;
  strcpy(pname[55], "ROSALIND")   ;
  strcpy(pname[56], "BELINDA")   ;
  strcpy(pname[57], "PUCK")   ;
  strcpy(pname[58], "NEPTUNE")   ;
  strcpy(pname[59], "TRITON")   ;
  strcpy(pname[60], "NEREID")   ;
  strcpy(pname[61], "PLUTO")   ;
  strcpy(pname[62], "CHARON")   ;
  strcpy(pname[63], "GASPRA")   ;
  strcpy(pname[64], "IDA")   ;

 /*  Constants are now loaded ....  */ 

  stat = 0;  
  for (j=0; j < N; j++)
  {
   if (id == look[j])
   {
    stat = 1; 
    strcpy(par, pname[j]) ;
    return stat ;
   }
  }

}



/*  include "xvmaininc.h"
    #define  N  64          */

int  zpbid(par, id)
char       par[12]  ;
int        *id   ;
{
 /*  Routine to return ID# given PLANET Name  */
 char   p8[12] ;  
 char   pname[65][12] ;
 int    look[65] ;
 int    j, stat ;

 /*   ID numbering scheme is consistent with GLL SPICE
      LOOKUP table for PLANET I.D.  FROM SEDR WORD # 9   */
   /*   MERCURY  */
  look[0] = 199 ;  

   /*  VENUS     */   
  look[1] = 299 ;

   /*  EARTH     */
  look[2] = 399 ;  look[3] = 301 ;

   /*  MARS      */
  look[4] = 499 ;  look[5] = 401 ;  look[6] = 402 ;

   /*  JUPITER   */
  look[7] = 599 ;  
  for (j=0; j < 16; j++)  look[8+j] = 501 + j;

   /*  SATURN    */
  look[24] = 699 ;
  for (j=0; j < 17; j++)  look[25+j] = 601 + j ;

   /*  URANUS    */
  look[42] = 799 ;
  for (j=0; j < 15; j++)  look[43+j] = 701 + j ;

   /*  NEPTUNE    */
  look[58] = 899 ;   look[59] = 801 ;  look[60] = 802 ;

   /*  PLUTO      */
  look[61] = 999 ;    look[62] = 901 ;

   /*  GASPRA      */
  look[63] = 9511010  ;

   /*  IDA        */
  look[64] = 2431010  ;

  /*   Now the PLANETARY Body name  */

  strcpy(pname[0],  "MERCURY") ; 
  strcpy(pname[1],  "VENUS")   ;
  strcpy(pname[2],  "EARTH")   ;
  strcpy(pname[3],  "MOON")   ;
  strcpy(pname[4],  "MARS")   ;
  strcpy(pname[5],  "PHOBOS")   ;
  strcpy(pname[6],  "DEIMOS")   ;
  strcpy(pname[7],  "JUPITER")   ;
  strcpy(pname[8],  "IO")   ;
  strcpy(pname[9],  "EUROPA")   ;
  strcpy(pname[10], "GANYMEDE")   ;
  strcpy(pname[11], "CALLISTO")   ;
  strcpy(pname[12], "AMALTHEA")   ;
  strcpy(pname[13], "HIMALIA")   ;
  strcpy(pname[14], "ELARA")   ;
  strcpy(pname[15], "PASIPHAE")   ;
  strcpy(pname[16], "SINOPE")   ;
  strcpy(pname[17], "LYSITHEA")   ;
  strcpy(pname[18], "CARME")   ;
  strcpy(pname[19], "ANANKE")   ;
  strcpy(pname[20], "LEDA")   ;
  strcpy(pname[21], "THEBE")   ;
  strcpy(pname[22], "ADRASTEA")   ;
  strcpy(pname[23], "METIS")   ;
  strcpy(pname[24], "SATURN")   ;
  strcpy(pname[25], "MIMAS")   ;
  strcpy(pname[26], "ENCELADUS")   ;
  strcpy(pname[27], "TETHYS")   ;
  strcpy(pname[28], "DIONE")   ;
  strcpy(pname[29], "RHEA")   ;
  strcpy(pname[30], "TITAN")   ;
  strcpy(pname[31], "HYPERION")   ;
  strcpy(pname[32], "IAPETUS")   ;
  strcpy(pname[33], "PHOEBE")   ;
  strcpy(pname[34], "JANUS")   ;
  strcpy(pname[35], "EPIMETHEUS")   ;
  strcpy(pname[36], "HELENE")   ;
  strcpy(pname[37], "TELESTO")   ;
  strcpy(pname[38], "CALYPSO")   ;
  strcpy(pname[39], "ATLAS")   ;
  strcpy(pname[40], "PROMETHEUS")   ;
  strcpy(pname[41], "PANDORA")   ;
  strcpy(pname[42], "URANUS")   ;
  strcpy(pname[43], "ARIEL")   ;
  strcpy(pname[44], "UMBRIEL")   ;
  strcpy(pname[45], "TITANIA")   ;
  strcpy(pname[46], "OBERON")   ;
  strcpy(pname[47], "MIRANDA")   ;
  strcpy(pname[48], "CORDELIA")   ;
  strcpy(pname[49], "OPHELIA")   ;
  strcpy(pname[50], "BIANCA")   ;
  strcpy(pname[51], "CRESSIDA")   ;
  strcpy(pname[52], "DESDEMONA")   ;
  strcpy(pname[53], "JULIET")   ;
  strcpy(pname[54], "PORTIA")   ;
  strcpy(pname[55], "ROSALIND")   ;
  strcpy(pname[56], "BELINDA")   ;
  strcpy(pname[57], "PUCK")   ;
  strcpy(pname[58], "NEPTUNE")   ;
  strcpy(pname[59], "TRITON")   ;
  strcpy(pname[60], "NEREID")   ;
  strcpy(pname[61], "PLUTO")   ;
  strcpy(pname[62], "CHARON")   ;
  strcpy(pname[63], "GASPRA")   ;
  strcpy(pname[64], "IDA")   ;

  strcpy(p8, par) ; 
  
  stat = 0 ;
  for (j=0; j < N; j++)
  {
   if ( strcmp(p8, pname[j]) == 0)
   {
    stat = 1 ;
    *id = look[j] ;
    return  stat ;
   }
  }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pbname.imake
/* Imake file for VICAR subroutine PBNAME   */

#define SUBROUTINE  pbname

#define MODULE_LIST  pbname.f  zpbname.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tpbname.f
c
c-----Program tpbname
c-----Test program for subroutine PBNAME and PBID
c
        Include 'VICMAIN_FOR'
        Subroutine  Main44
	Character*12  Name
c	Byte  Pbuf(30)
        Character*30  Pbuf
        Integer*2     Ibuf(15)
c
	Call MVCL(' id = xxx  name = xxxxxxxx',Ibuf,26)
        Call MVLC(Ibuf, PBUF, 26)
c
	Do 10 i=1,999
         Call Pbname(i, name, &10)
         Call Pbid(name, j, &10)
c        Call outcon(j,pbuf(9),3)
         Write(Pbuf(7:9),'(I3)') j
c         Call mvl(%ref(name),pbuf(19),12)
         Do 20  k=1,12
          k1 = k + 18
          PBUF(k1:k1) = Name(k:k)
20       Continue          
         Call Xvmessage(Pbuf, ' ')
10	Continue
c
c   Testing the C-Bridge  
c
        Call Tzpbname
c
        Return
	End
$!-----------------------------------------------------------------------------
$ create tzpbname.c
/*   A C-bridge routine, called by TPBNAME.F, that tests the C-bridge version
   of PBNAME, ZPbname.c    
*/

#include "xvmaininc.h"
#include "ftnbridge.h"
void FTN_NAME(tzpbname) ()
{
  int  i, jj, k1, k2 ;  
  char  pbuf[32];
  char  name[12];

  zvmessage(" ", " ");
  zvmessage(" ******  Testing C-Bridge Version  ****** ", " ");
  zvmessage(" ", " ");

  for (i=0; i < 999; i++)
  {
    k1 = zpbname(i, name) ;
    k2 = zpbid(name, &jj) ;
    if (k1 == 1  &&  k2 == 1)
    {
     sprintf(pbuf," id = %03d  name = %s", jj, name) ;
     zvmessage(pbuf, " ") ;
    }
  }

}
$!-----------------------------------------------------------------------------
$ create tpbname.imake
/* Imake file for Fortran-Test of VICAR subroutine  PBNAME  */

#define PROGRAM tpbname

#define MODULE_LIST tpbname.f  tzpbname.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE       
/*  #define LIB_LOCAL  */    /*  disable when delivery   */
#define LIB_P2SUB    
$!-----------------------------------------------------------------------------
$ create tpbname.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstpbname.pdf
procedure
refgbl $echo
body
let _onfail="continue"
!let $echo="yes"
WRITE "THIS IS A TEST OF MODULE PBNAME"
WRITE "This test file includes PCA printout"
WRITE "  "
WRITE "To test this module do the following:"
WRITE "1.  dcl @PBNAME debug"
WRITE "2.  dcl @PBNAME test"
WRITE "3.  tstPBNAME|ru=(ba,mipl3_slow)|"
WRITE "  "
!
! pca stuff...
!dcl assign tstPBNAME.pcac pcac$init
!dcl assign tstPBNAME.pcaa pcaa$init

TPBNAME

! pca stuff again...
! dcl pca tPBNAME.pca
! dcl type tstPBNAME.pcatxt
! dcl del tstPBNAME.pca*.*
! dcl del tPBNAME.pca.*
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create pbname.hlp
1 PBNAME

2  PURPOSE

  PBNAME and PBID are Voyager specific subroutines.  Given the target-body
  name, PBID will return the corresponding target-body ID as specified in the
  NAIF SPICE P-constants kernel and VGR SEDR. Given the target-body ID, PBNAME
  will return corresponding target-body name.

  CALLING SEQUENCE:

       CALL PBNAME(ID,name,&nnn)     or
       CALL PBID(NAME,id,&nnn)
  where 
       INTEGER*4 ID	   is the target-body ID
       CHARACTER*12 NAME   is the target-body name
       &nnn		   is the statement label of an alternate return 

  ID is input to PBNAME and output by PBID.
  NAME is input to PBID and output by PBNAME.
  NAME must be left justified and padded on the right with blanks.
  The alternate return is taken if an unidentified ID or NAME is detected.

2 OPERATION

  Tables of target names and target numbers are maintained internal to the
  subroutine (i.e. no SPICE files are accessed).

2 HISTORY

  Original Programmer: Gary Yagi, 23 July 1980
  Current Cognizant Programmer: Gary Yagi
  Revisions:
    24 Jan 86  GMY  Fix order of Uranian satellites
     9 Sep 88  GMY  Added new satellites and rename some Jupiter satellites
    25 oct 89  JJL  change to GLL sedr numbering & change to character*12
    12 Nov 91  GMY  Restore previous numbering to be NAIF compatible
                    Change MERCURY, VENUS, MARS to 199, 299, 499.
                    Add GASPRA to target list.  Add PCA garbage.

    28 Jan 93  WPL  Ported for UNIX Conversion
    21 Mar 94  FFM  Added ccase in PBID(FR 85159),
                    changed TETTHYS to TETHYS in ZPBNAME.C. 
    22 Aug 94  GMY  Added IDA (FR 85139)
$ Return
$!#############################################################################
