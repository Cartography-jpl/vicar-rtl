$!****************************************************************************
$!
$! Build proc for MIPL module pbdata
$! VPACK Version 1.7, Monday, August 22, 1994, 18:25:40
$!
$! Execute by entering:		$ @pbdata
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
$ write sys$output "*** module pbdata ***"
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
$ write sys$output "Invalid argument given to pbdata.com file -- ", primary
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
$   if F$SEARCH("pbdata.imake") .nes. ""
$   then
$      vimake pbdata
$      purge pbdata.bld
$   else
$      if F$SEARCH("pbdata.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pbdata
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pbdata.bld "STD"
$   else
$      @pbdata.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pbdata.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pbdata.com -
	-s pbdata.f zpbdata.c xpbdata.f -
	-i pbdata.imake -
	-t tpbdata.f tzpbdata.c tpbdata.imake tpbdata.pdf tstpbdata.pdf -
	-o pbdata.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create pbdata.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Returns picture body data given pb name (12 characters)
C
      SUBROUTINE PBDATA(name,BUF,*)

      REAL*4 BUF(*)
      INTEGER N,J,K
      INTEGER*4 id(65)
      REAL*4 SRANGE(11)
      REAL*4 RADPE(4,65),
     +       radpe_1(4,16),radpe_2(4,16),radpe_3(4,16),radpe_4(4,17)
      equivalence(radpe_1,radpe(1,1))
      equivalence(radpe_2,radpe(1,17))
      equivalence(radpe_3,radpe(1,33))
      equivalence(radpe_4,radpe(1,49))
      
      REAL*4 ROT(65)
      character*12 p8,pname(65)
      character*12 name
      data n/65/
      data id/
c mercury
     + 1,
c venus
     + 2,
c earth
     + 3,3,
c mars
     + 4,4,4,
c jupiter
     + 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
c saturn
     + 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
c uranus
     + 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
c neptune
     + 8,8,8,
c pluto
     + 9,9,
c gaspra
     + 10,
c ida
     + 11/

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
C
C Planet radii ra,rb,rc,lora in km & degrees
C where ra = (major) equatorial radius at longitude lora
C       rb = equatorial radius at longitude lora + 90 degrees
C       rc = polar radius (km)
      data RADPE_1/
C        MERCURY                         VENUS
     *2439.7,2439.7,2439.7,0.,        6137.,6137.,6137.,0.,
C        EARTH                           MOON
     *6378.14,6378.14,6356.75,0.,     1737.4,1737.4,1737.4,0.,
C        MARS                            PHOBOS
     *3397.,3397.,3375.,0.,             13.4,11.2,9.2,0.,
C        DEIMOS
     *7.5,6.1,5.2,0.,
C        JUPITER                         IO
     *71492.,71492.,66854.,0.,         1830.,1818.7,1815.3,0.,
C        EUROPA                         GANYMEDE
     *1565.,1565.,1565.,0.,            2634.,2634.,2634.,0.,
C        CALLISTO                       AMALTHEA
     *2403.,2403.,2403.,0.,            131.,73.,67.,0.,
C        HIMALIA                        ELARA
     *85.,85.,85.,0.,                  40.,40.,40.,0.,
C        PASIPHAE
     *18.,18.,18.,0./

      data radpe_2/
c        SINOPE
     *14.,14.,14.,0.,
C        LYSITHEA                       CARME
     *12.,12.,12.,0.,                  15.,15.,15.,0.,
C        ANANKE                         LEDA
     *10.,10.,10.,0.,                   5.,5.,5.,0.,
C        THEBE                          ADRASTEA
     *55.,55.,45.,0.,                    13.,10.,8.,0.,
C        METIS
     *20.,20.,20.,0.,
C        SATURN
     *60268.,60268.,54364.,0.,
C        MIMAS                          ENCELADUS
     *210.3,197.4,192.6,0.,            256.2,247.3,244.,0.,
C        TETHYS                         DIONE
     *523.,523.,523.,0.,               560.,560.,560.,0.,
C        RHEA                           TITAN
     *764.,764.,764.,0.,               2575.,2575.,2575.,0.,
C        HYPERION             
     *180.,140.,112.5,0./

      data radpe_3/
c          IAPETUS
     *718.,718.,718.,0.,
C        PHEOBE                         JANUS
     *115.,110.,105.,0.,                97.,95.,77.,0.,
C       EPIMETHEUS                      HELENE
     *69.,55.,55.,0.,                    17.5,17.5,17.5,0.,
C       TELESTO                         CALYPSO
     *15.2,12.5,7.5,0.,                  15.,8.,8.,0.,
C       ATLAS                           PROMETHEUS
     *18.5,17.2,13.5,0.,                74.,50.,34.,0.,
C       PANDORA
     *55.,44.,31.,0.,
C       URANUS 			        ARIEL
     *25559.0,25559.0,24973.0,0.,      581.1,577.9,577.7,0.,
C       UMBRIEL			         TITANIA
     *584.7,584.7,584.7,0.,            788.9,788.9,788.9,0.,
C       OBERON                          MIRANDA
     *761.4,761.4,761.4,0.,            240.4,234.2,232.9,0./

      data radpe_4/
C       CORDELIA                        OPHELIA
     *13.0,13.0,13.0,0.,               15.0,15.0,15.0,0.,
C       BIANCA                          CRESSIDA
     *21.,21.,21.,0.,                  31.,31.,31.,0.,
C       DESDEMONA                       JULIET
     *27.,27.,27.,0.,                  42.,42.,42.,0.,
C       PORTIA                          ROSALIND
     *54.,54.,54.,0.,                  27.,27.,27.,0.,
C       BELINDA                         PUCK
     *33.,33.,33.,0.,                  77.,77.,77.,0.,
C       NEPTUNE                         TRITON
     *25269.,25269.,24800.,0.,         1352.6,1352.6,1352.6,0.,
C       NEREID
     *170.,170.,170.,0.,
C       PLUTO                           CHARON
     *1162.,1162.,1162.,0.,            606.,606.,606.,0.,
C       GASPRA                          IDA
     *9.,5.5,5.,0.,                    28.,12.,10.5,0./


C AXIAL ROTATION RATE (DEGREES PER DAY)
      data ROT/
C MERCURY,VENUS,EARTH,MOON
     &6.1385025,-1.4813291,360.9856235,13.1763581,
C MARS,PHOBOS,DEIMOS
     &350.9819830,1128.8444790,285.161903,
C JUPITER,IO,EUROPA,GANYMEDE,CALLISTO,
     &870.536,203.4889538,101.3747235,50.3176081,21.5710715,
C AMALTHEA,HIMALIA,ELARA,PASIPHAE,SINOPE,LYSITHEA,
     &722.6303746,1.4365522,1.384083,0.4897959,0.474934,1.3846153,
C CARME,ANANKE,LEDA,THEBE,ADRASTEA,METIS
     &0.5202312,0.5834683,1.5,0.,0.,0.,
C SATURN,MIMAS,ENDELADUS,TETHYS,DIONE,
     &810.7939024,381.994555,262.7318996,190.6979085,131.5349316,
C RHEA,TITAN,HYPERION,IAPETUS,PHOEBE,JANUS,EPIMETHEUS,HELENE,TELESTO,
     &79.6900478,22.5769768,16.91729,4.5379572,0.6540697,0.,0.,0.,0.,
C CALYPSO,ATLAS,PROMETHEUS,PANDORA,
     &0.,0.,0.,0.,
C URANUS,ARIEL,UMBRIEL,TITANIA,OBERON,
     &-501.1600928,-142.8356681,-86.8688923,-41.3514316,-26.7394932,
C MIRANDA,CORDELIA,OPHELIA,BIANCA,CRESSIDA,DESDEMONA,JULIET,PORTIA,
     &-254.6906892,0.,0.,0.,0.,0.,0.,0.,
C ROSALIND,BELINDA,PUCK,NEPTUNE,TRITON,NEREID,PLUTO,CHARON
     &0.,0.,0.,483.7625981,-61.2572684,0.999549,-56.364,-56.3624607,
C GASPRA,IDA
     &0.,0./

C Mean solar ranges of the planets (AU)...
      data SRANGE/0.387098,0.723331,1.0,1.523679,5.2027,9.546,
     &   19.2,30.09,39.5,2.2016,2.9485/
C

      p8=name
C
      DO 50 J=1,N
      IF(PNAME(J).EQ.P8) GOTO 51
   50 CONTINUE
      RETURN1
C
   51 CALL MVE(7,4,RADPE(1,J),BUF,1,1)
      IF (ROT(J) .EQ. 0) THEN 
        BUF(5) = 0
      ELSE 
        BUF(5) = 360.D0 / ROT(J)	!Rotation period in days
      ENDIF
      K = ID(J)
      BUF(6) = 149597871.D0*SRANGE(K)	!Solar range in km
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zpbdata.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/*
Bridge for PBDATA in C, called from C
*/
int zpbdata(name,buf)

char *name;
float *buf;
{
   int i, status;
   i=strlen(name);

   status = FTN_NAME(xpbdata) (name, &i, buf );
   return status;
 }
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create xpbdata.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C  Bridge to PBDATA, in Fortran

      INTEGER FUNCTION xpbdata(name, i, buf)
     
      real*4 buf(20)
      byte name(1)
      integer i
      character*12 text

      text=' '

      if (i.gt.80) call xvmessage('xpbdata, string is too long',' ')

C     Transformation to Fortran-string
      call mvlc(name, text, i)
      call pbdata(text,buf,*100)
      xpbdata = 1
      goto 999

 100  xpbdata = 0

 999  return
      end
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pbdata.imake
/* Imake file for VICAR subroutine PBDATA   */

#define SUBROUTINE  pbdata

#define MODULE_LIST  pbdata.f  zpbdata.c xpbdata.f

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN

$ Return
$!#############################################################################
$Test_File:
$ create tpbdata.f
C Test for routine PBDATA
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      CHARACTER*132 BUF
      REAL*4 D(20)
      INTEGER NTARGETS
      character*12 name(65)
      data ntargets/65/
      data name/   'MERCURY     ','VENUS       ','EARTH       ',
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
     + 'GASPRA      ','IDA'/

      call xvmessage('********FORTRAN CALLABLE VERSION****',' ')
      call xvmessage('1       TARGET  TARGET                  
     +         ROTATION     SOLAR',' ')
      call xvmessage('        NUMBER   NAME       A       B   
     + C       PERIOD      RANGE',' ')
      BUF(1:80) = ' '

      DO 10 I=1,65
      CALL PBDATA(NAME(I),D,*8)
    8 CALL PBID(NAME(I),ID,*10)		!SEDR ID
      WRITE (BUF(4:11),'(I8)') ID !Target number
      buf(16:23) = name(i)             !Target name
      WRITE (BUF(26:32),'(F7.1)') D(1) !RA
      WRITE (BUF(34:40),'(F7.1)') D(2) !RB
      WRITE (BUF(42:48),'(F7.1)') D(3) !RC
      WRITE (BUF(50:61),'(F12.7)') D(5) !Rotation period
      WRITE (BUF(63:78),'(E16.10)') D(6) !Solar range
      CALL XVMESSAGE(BUF,' ')
   10 CONTINUE
C
c********C CALLABLE VERSION****
      call tzpbdata()

   20 RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzpbdata.c
#include "xvmaininc.h"
#include "ftnbridge.h"

#define SUCCESS 1
#define FAILURE 0

void FTN_NAME(tzpbdata)()
{
  char buf[132];
  float d[20];
  int datastat,idstat,id,i;
  int ntargets = 65;
  char name[65][12];
  char binpool[132];

  /* initialize planetary body names array */

  strcpy(name[0],  "MERCURY") ; 
  strcpy(name[1],  "VENUS")   ;
  strcpy(name[2],  "EARTH")   ;
  strcpy(name[3],  "MOON")   ;
  strcpy(name[4],  "MARS")   ;
  strcpy(name[5],  "PHOBOS")   ;
  strcpy(name[6],  "DEIMOS")   ;
  strcpy(name[7],  "JUPITER")   ;
  strcpy(name[8],  "IO")   ;
  strcpy(name[9],  "EUROPA")   ;
  strcpy(name[10], "GANYMEDE")   ;
  strcpy(name[11], "CALLISTO")   ;
  strcpy(name[12], "AMALTHEA")   ;
  strcpy(name[13], "HIMALIA")   ;
  strcpy(name[14], "ELARA")   ;
  strcpy(name[15], "PASIPHAE")   ;
  strcpy(name[16], "SINOPE")   ;
  strcpy(name[17], "LYSITHEA")   ;
  strcpy(name[18], "CARME")   ;
  strcpy(name[19], "ANANKE")   ;
  strcpy(name[20], "LEDA")   ;
  strcpy(name[21], "THEBE")   ;
  strcpy(name[22], "ADRASTEA")   ;
  strcpy(name[23], "METIS")   ;
  strcpy(name[24], "SATURN")   ;
  strcpy(name[25], "MIMAS")   ;
  strcpy(name[26], "ENCELADUS")   ;
  strcpy(name[27], "TETHYS")   ;
  strcpy(name[28], "DIONE")   ;
  strcpy(name[29], "RHEA")   ;
  strcpy(name[30], "TITAN")   ;
  strcpy(name[31], "HYPERION")   ;
  strcpy(name[32], "IAPETUS")   ;
  strcpy(name[33], "PHOEBE")   ;
  strcpy(name[34], "JANUS")   ;
  strcpy(name[35], "EPIMETHEUS")   ;
  strcpy(name[36], "HELENE")   ;
  strcpy(name[37], "TELESTO")   ;
  strcpy(name[38], "CALYPSO")   ;
  strcpy(name[39], "ATLAS")   ;
  strcpy(name[40], "PROMETHEUS")   ;
  strcpy(name[41], "PANDORA")   ;
  strcpy(name[42], "URANUS")   ;
  strcpy(name[43], "ARIEL")   ;
  strcpy(name[44], "UMBRIEL")   ;
  strcpy(name[45], "TITANIA")   ;
  strcpy(name[46], "OBERON")   ;
  strcpy(name[47], "MIRANDA")   ;
  strcpy(name[48], "CORDELIA")   ;
  strcpy(name[49], "OPHELIA")   ;
  strcpy(name[50], "BIANCA")   ;
  strcpy(name[51], "CRESSIDA")   ;
  strcpy(name[52], "DESDEMONA")   ;
  strcpy(name[53], "JULIET")   ;
  strcpy(name[54], "PORTIA")   ;
  strcpy(name[55], "ROSALIND")   ;
  strcpy(name[56], "BELINDA")   ;
  strcpy(name[57], "PUCK")   ;
  strcpy(name[58], "NEPTUNE")   ;
  strcpy(name[59], "TRITON")   ;
  strcpy(name[60], "NEREID")   ;
  strcpy(name[61], "PLUTO")   ;
  strcpy(name[62], "CHARON")   ;
  strcpy(name[63], "GASPRA")   ;
  strcpy(name[64], "IDA")   ;


  zvmessage("********C CALLABLE VERSION****","");
  zvmessage("1       TARGET  TARGET                              ROTATION     SOLAR","");
  zvmessage("        NUMBER   NAME       A       B       C       PERIOD      RANGE","");

  for (i=0;i<65;i++)
    {
    datastat = zpbdata(name[i],d);
    if(datastat == FAILURE) zmabend("error in pbdata","");
    idstat = zpbid(name[i],&id);		/*SEDR ID*/
    if(idstat == FAILURE) zmabend("error in pbid","");

    sprintf(buf,"%11d    %-9s %7.1f %7.1f %7.1f %12.7f %16.10E",id,
	    name[i],d[0],d[1],d[2],d[4],d[5]);
    zvmessage(buf,"");
    }

    return;
}
$!-----------------------------------------------------------------------------
$ create tpbdata.imake
#define PROGRAM tpbdata

#define MODULE_LIST tpbdata.f  tzpbdata.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define LIB_LOCAL*/
$!-----------------------------------------------------------------------------
$ create tpbdata.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstpbdata.pdf
procedure
refgbl $echo
body
let _onfail="continue"
!let $echo="yes"
WRITE "*************NOTE TO TESTERS:********************"
WRITE "Differences in testlogs may exist between platforms"
WRITE "due to round off differences and differences in"
WRITE "the display of character strings"
WRITE " "
WRITE "Test of subroutine PBDATA"
tpbdata
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create pbdata.hlp
1  PBDATA

  PBDATA will return the planet radii, axial rotation rate, and 
  solar range of a specified planetary body.

2  CALLING SEQUENCE

   Fortran calling sequence:

       CALL PBDATA(NAME,buf,*nnn)

       CHARACTER*12 NAME	!Input target-body name
       REAL*4 BUF(20)		!Output physical constants.
       *nnn			!Statement label of an alternate return address

    NAME must be left justified and padded on the right with blanks.
    The alternate return is taken if an the input NAME is invalid.

   C calling sequence:

       status = zpbdata(name,buf);

       char name[12];		!Input target-body name
       float buf([20];		!Output physical constants.
       int status;		!0 = failure, 1 = success

2  OPERATION

   The physical constants are retrieved from look-up tables within the
   source file.
   
   PBDATA will fill BUF with the following data:

        Word       Physical constant (MAP2 symbol and unit)
        1          equatorial radius at long. LORA (RA, km)
        2          equatorial radius at long. LORA+90 (RB, km)
        3          polar radius  (RC, km)
        4          long. of long axis  RA  (LORA, deg)
        5          axial rotation period  (days)
        6          solar range (km)
        7-20       unused

2  HISTORY

       Original Programmer: Gary Yagi, 23 July 1980
       Current Cognizant Programmer: Gary Yagi
       Source Language:  Fortran and C
       Revisions:

   22 AUG 94     GMY    Remove all SPICE calls (FR 85627)
   22 FEB 94     TLT    Ported to Unix
   10 Nov 91     GMY    Get constants from SPICE P-constants kernel
   31 Oct 90     GMY    Update radii for Venus, Triton, & Nereid
   25 oct 89     jjl    change name to character*12
                        & numbering to GLL system.
   09 Sept 88 ...SMT... Update all radii and rotation rates
   24 June 87 ...GMY... Change Mars polar radius
   11 June 87 ...GMY... Add Solar range
   11 June 87 ...GMY... Update Saturn and Uranus satellite data
   25 Jan 86  ...GMY... Update Uranus radii and rotation rate
   05 AUG  83   ...CCA...    CONVERT TO VAX
   02 SEPT 82   ...CCA...    UPDATE RADII AND ROTATION RATES
   23 AUG 81   ...GMY...    UPDATE ID AND MIMAS RADII
   05 JUNE 81 ...GMY...  UPDATE SATURN VALUES BASED ON VGR1 DATA
   21 AUG 80   ...GMY...    INITIAL RELEASE
$ Return
$!#############################################################################
