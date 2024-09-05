      subroutine DIVA(TSPECS, Y, F, KORD, NEQ, DERIVS, OUTPUT, IDIMT,
     1   IDIMY, IDIMF, IDIMK, IOPT)
c     .  Copyright (C) 1989, California Institute of Technology.
c     .  U. S. Government sponsorship acknowledged.
c>> 1995-12-18 DIVA  Krogh  Fixed so no solution dump on 0 length integ.
c>> 1995-11-09 DIVA  Krogh  Fixed so char. data at col. 72 is not ' '.
c>> 1995-06-19 DIVA  Krogh  Fixed prob. with discon. just after restart.
c>> 1995-05-09 DIVA  Krogh  Fixed G-Stop/discontinuity code interaction
C>> 1995-04-26 DIVA  Krogh  Use KQMAXS instead of KQMAXI when LDIS>1000.
C>> 1995-04-26 DIVA  Krogh  Keep current KQL on discontinutiy.
C>> 1994-12-16 DIVA  Krogh  Fixed option 12 with K12 < 0.
C>> 1994-11-11 DIVA  Krogh  Declared all vars.
c>> 1994-11-02 DIVA  Krogh  Changes to use M77CON
c>> 1994-09-08 DIVA  Krogh  Added CHGTYP code.
c>> 1994-07-11 DIVA  Krogh  Fix to get same state with/without var. eqs.
c>> 1994-03-07 DIVA  Krogh  Allow larger order in single precision.
c>> 1994-01-14 DIVA  Krogh  Minor change to allow changing TFINAL.
c>> 1993-04-27 DIVA  Krogh  Additions for Conversion to C.
c>> 1993-04-12 DIVA  Krogh  Converted to use slightly altered MESS.
c>> 1993-04-12 DIVA  Krogh  Fixed LSC so sol. saved when HMAX is small.
c>> 1992-10-13 DIVA  Krogh  Fixed G-Stop/discontinuity code interaction.
c>> 1992-09-21 DIVA  Krogh  Fixed bug in discontinuity code.
c>> 1992-09-09 DIVA  Krogh  Fixed bug - Var. Eqs. with discontinuities.
c>> 1992-08-07 DIVA  Krogh  Storage map printed only if option 10 .ne. 0
c>> 1992-07-16 DIVA  Krogh  Restored correct discontinuity code.
c>> 1992-06-16 DIVA  Krogh  Eliminate reuse of storage for option 12.
c>> 1992-04-08 DIVA  Krogh  Removed unused labels, 1020, 2120.
c>> 1992-03-30 DIVA  Krogh  Fixed bug in DIVAOP error message.
c>> 1992-03-12 DIVA  Krogh  Simplified DIVABU, more digits in B's.
c>> 1992-01-16 DIVA  Krogh  Fixed minor bug in error messages.
c>> 1991-12-03 DIVA  Krogh  Major change for improved error checks.
c>> 1991-06-17 DIVA  Krogh  Fixed bug in checking storage allocation.
c>> 1991-04-11 DIVA  Krogh  Fixed minor bug re. option 12 in DIVAOP.
c>> 1991-03-28 DIVA  Krogh  Removed check at label 650 for KORD2I<0.
c>> 1991-02-08 DIVA  Krogh  Changed some floats to generics
c>> 1990-11-08 DIVA  Krogh  Fixed bug on TSPECS on discon.
c>> 1990-09-14 DIVA  Krogh  Fixed bug when discon. and sol. save.
c>> 1990-09-13 DIVA  Krogh  Increased dimension of BETA by 1.
c>> 1990-09-13 DIVA  Krogh  Added one more poss. on rel. error test.
c>> 1990-09-11 DIVA  Krogh  Recent change messed up getting dump output.
c>> 1990-06-05 DIVA  Krogh  Fixed bug in noise test, comments in IVACOM.
c>> 1990-05-08 DIVA  Krogh  Fixed new bug when TMARK hit in DIVAG.
c>> 1990-04-17 DIVA  Krogh  Fixed minor problem in DIVAIN error msg.
c>> 1990-04-10 DIVA  Krogh  Fixed interaction between discon. & dump.
c>> 1990-03-23 DIVA  Krogh  Fixed bug on option "-2", see 1989-12-07.
c>> 1990-03-20 DIVA  Krogh  Fixed rarely occuring loop.
c>> 1990-01-29 DIVA  Krogh  Removed unneeded labels.
c>> 1989-12-14 DIVA  Krogh  Saved common block DIVAEV.
c>> 1989-12-07 DIVA  Krogh  Added option "2" to DIVAOP.
c>> 1989-11-09 DIVA  Krogh  Made GG a save var. in DIVAHC
c>> 1989-08-21 DIVA  Krogh  Fix out of bounds ref. to V in DIVABU
c>> 1989-07-26 DIVA  Krogh  Fix bug in initial dim. check
c>> 1989-07-21 DIVA  Krogh  Code for integrating discontinuities
c>> 1987-12-07 DIVA  Krogh  Initial code.
c
c--D replaces "?": ?IVA,?IVAA,?IVABU,?IVACO,?IVACR,?IVAEV,?IVAHC,?IVAG
c-- &  ?IVAIN,?IVAMC,?IVAOP,?IVAPR,?IVASC,?IVACE,?IVAIE,?IVAPE,?MESS
c
c++S Default KDIM = 16
c++  Default KDIM = 20
c++  Default MAXORD = 2, MAXSTF = 1
c++  Default INTEGO, VAREQ, OUTPUT, DUMP, GSTOP, EXTRAP
c++  Default STIFF=.F., ARGM=.F., ERRSTO=.F.
c
      integer NEQ, IDIMT, IDIMY, IDIMF, IDIMK
      integer KORD(*), IOPT(*)
c--D Next line special: P=>D, X=>Q
      double precision TSPECS(*), Y(*)
      double precision F(*)
      external DERIVS, OUTPUT
c
c *********************** Internal Variables ***************************
c
c Comments for variables used in this package can be found in the file
c   IVACOM.
c
c *********************** Type Declarations ****************************
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2
      double precision EEPT75, EOVEP2, OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      integer KGO, INTCHK(0:30), NXTCHK
      integer IHI, JL, J, ILOW, K, KQQ, JLIM, KFERR
c
      equivalence (INTCHK(1), NXTCHK)
      double precision CM1, C1
      parameter (CM1 = (-1.D0))
      parameter (C1 = 1.D0)
      integer IOPIVA(2)
      save IOPIVA
c
c                      Declarations for error message processing.
c
      integer  MENTXT,MEIDAT,MEMDA1,MECONT,MERET,MEEMES,METEXT,MEIVEC
      parameter (MENTXT =23)
      parameter (MEIDAT =24)
      parameter (MEMDA1 =27)
      parameter (MECONT =50)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      parameter (MEIVEC =57)
c
      integer MACT(16), MLOC(11), MACT1(4)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVA$B
cAB The interval [1, 10**6], bounds the allowed values for NTE=$I.$E
cAC For option $I, the interval [$I, $I], bounds the allowed $C
c   values for the integration order which is set to $I.$E
cAD Option 16 must be used for error control.$E
cAE F($I) = $F, but it must be -1.0 when skipping the error check.$E
cAF For option $I, the interval [$I, $I] bounds the allowed $C
c   values for KORD($I)=$I, which is used to specify an $B
cAG output type for printing.$E
cAH output group for printing.$E
cAI equation group for variational equations.$E
cAJ order for a differential equation.$E
cAK equation group for diagnostic print.$E
cAL equation group for integration order control.$E
cAM equation group for error control.$E
c   $
cAN KORD values for this option starting at KORD($M) are:$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN
      parameter (LTXTAA=  1,LTXTAB=  7,LTXTAC= 71,LTXTAD=183,LTXTAE=226,
     * LTXTAF=290,LTXTAG=400,LTXTAH=427,LTXTAI=455,LTXTAJ=498,
     * LTXTAK=535,LTXTAL=573,LTXTAM=620,LTXTAN=  1)
      character MTXTAA(3) * (218)
      character MTXTAB(1) * (55)
      data MTXTAA/'DIVA$BThe interval [1, 10**6], bounds the allowed val
     *ues for NTE=$I.$EFor option $I, the interval [$I, $I], bounds the$
     * allowed values for the integration order which is set to $I.$EOpt
     *ion 16 must be used for error con','trol.$EF($I) = $F, but it must
     * be -1.0 when skipping the error check.$EFor option $I, the interv
     *al [$I, $I] bounds the allowed values for KORD($I)=$I, which is us
     *ed to specify an $Boutput type for printing.$Eoutput gro','up for$
     * printing.$Eequation group for variational equations.$Eorder for a
     * differential equation.$Eequation group for diagnostic print.$Eequ
     *ation group for integration order control.$Eequation group for err
     *or control.$E'/
      data MTXTAB/'KORD values for this option starting at KORD($M) are:
     *$E'/
c
c        for KGO =     1      2      3      4      5      6      7
      data MLOC / LTXTAI,LTXTAK,LTXTAL,LTXTAM,LTXTAJ,LTXTAG,LTXTAH,
     2            LTXTAB,LTXTAC,LTXTAD,LTXTAE /
c           KGO        8      9     10     11
c
c                      1  2  3 4       5  6       7       8       9 10
      data MACT / MEEMES,38,24,0, MENTXT, 0, METEXT, MECONT, MEMDA1,0,
     1  METEXT, MEIDAT,0, MEIVEC,0, MECONT /
c           11      12 13     14 15     16
      data MACT1 / METEXT, MEIVEC, 0, MERET /
      data IOPIVA(1) / 1111 /
c
c ************** START OF EXECUTABLE CODE ******************
c
c     **** TEST IF CONTINUING AN INTEGRATION
      if (KORD(1) .ne. 0) go to 330
c     **** INITIALIZE VARIOUS SCALARS
      KSTEP = 0
      KORD2I = -5
      KORD(2) = -1
      NTE = NEQ
      NE = NTE
c
c     **** SET UP OPTIONS
      if (IOPT(1) .ne. 0) call DIVAOP(IOPT, F)
      call DIVAOP(IOPIVA, F)
      if (IOPT(1) .eq. 0) IOPIVA(2) = 1
c
      if ((NE .le. 0) .or. (NE .gt. 1000000)) then
         IDAT(1) = NE
         KGO = 8
         go to 650
      end if
c                         Set up diagnostic print on storage allocation.
      INTCHK(0) = 245
      if (IOP10 .ne. 0) INTCHK(0) = 247
c
c     **** CHECK TSPECS STORAGE ALLOCATION
      INTCHK(2) = IDIMT
      INTCHK(3) = 4
      NXTCHK = 4
      if (IOP5 .ne. 0) then
         INTCHK(4) = 5
         INTCHK(5) = 5
         if (IOP5 .gt. 0) then
            INTCHK(6) = IOP5 - 4
         else
            IHI = -IOP5
            JL = 4
            do 15 IHI = IHI, IDIMK-3, 3
               J = abs(KORD(IHI))
               if (J .eq. 0) go to 20
               if (abs(KORD(IHI + 2)) .gt. 1) then
                  IDAT(2) = -1
                  IDAT(3) = 1
                  KGO = 6
                  go to 600
               end if
               if ((J .le. JL) .or. (J .gt. KORD(IHI+1))) then
                  KGO = 7
                  IDAT(2) = JL + 1
                  IDAT(3) = KORD(IHI+1)
                  go to 610
               end if
               JL = KORD(IHI+1)
   15       continue
            if (KORD(IHI) .ne. 0) IHI = IHI + 3
   20       INTCHK(6) = JL - 4
         end if
         NXTCHK = 7
      end if
   25 call OPTCHK(INTCHK, IOPT, 'DIVA / TSPECS$E')
      if (NXTCHK .lt. 0) KORD2I = -4
c
c     **** CHECK KORD STORAGE ALLOCATION
      INTCHK(2) = IDIMK
      INTCHK(3) = NE + 3
      NXTCHK = 4
      if (IOP5 .lt. 0) then
         INTCHK(4) = 5
         INTCHK(5) = -IOP5
         INTCHK(6) = IHI + IOP5
         NXTCHK = 7
      end if
c
c++  Code for VAREQ is active
      if (IOP18 .ne. 0) then
         NE = abs(KORD(IOP18))
         INTCHK(NXTCHK) = 18
         ILOW = IOP18
         KGO = 1
c.       **** CHECK OPTION FOR VALID INPUT
         go to 430
      end if
c++  End
   30 continue
      if (NKDKO .ne. 0) then
c                        **** STORAGE ALLOCATED FOR ODE ORDERS
         INTCHK(NXTCHK) = 17
         INTCHK(NXTCHK+1) = NKDKO
         INTCHK(NXTCHK+2) = NTE
         NXTCHK = NXTCHK + 3
      end if
c++  Code for STIFF is inactive
c      IF (IOPST .ne. 0) then
c         INTCHK(NXTCHK) = 17
c         INTCHK(NXTCHK+1) = IOPST
c         INTCHK(NXTCHK+2) = NTE
c         NXTCHK = NXTCHK + 3
c      end if
c++  End
c
c **** SET INITIAL INTEGRATION ORDERS, TEST ODE ORDERS ****
c
      MAXINT = 0
      MAXDIF = 0
      NY = 0
      do 80 K = 1, NTE
         if (NKDKO .ne. 0) KORDI = KORD(NKDKO + K - 1)
         NY = NY + abs(KORDI)
c++  Code for STIFF is inactive
c      IF (IOPST .EQ. 0) GO TO 60
cc.    **** CHECK FOR POTENTIAL STIFF EQUATION
c      JS = abs(KORD(IOPST+K-1)) - 1
c      IF ( JS ) 52,60,54
cc.    **** EQUATION IS NOT ACTIVE
c   52 KQQ = 0
c      GO TO 56
cc.    **** EQUATION USES IMPLICIT METHOD
c   54 KQQ = -1
c      IF (JS .GT. abs(KORDI)) then
c        Set up an error message.
c      end if
c      MAXINT = max(MAXINT, abs(KORDI) - JS)
c   56 IF (KORDI .GE. 0) GO TO 70
c      KORDI = -1 - KORDI
c      JS = JS - 1
c      MAXDIF = max(MAXDIF, JS, 1)
c      GO TO 70
c++  End
c     **** EQUATION IS TO USE AN EXPLICIT METHOD
   60    KQQ = 1
         MAXINT = max(MAXINT, KORDI)
   70    if ((KORDI .gt. MAXORD) .or. (KORDI .le. 0)) then
c                    Set up error message.  KORDI is out of range.
            IDAT(1) = 17
            IDAT(2) = 1
            IDAT(3) = MAXORD
            if (NKDKO .ne. 0) then
               KGO = 5
               ILOW = NKDKO
               IHI = NKDKO + K - 1
               go to 640
            else
               KGO = 9
               IDAT(4) = KORDI
               go to 650
            end if
         end if
         KORD(K + 3) = KQQ
   80 continue
c     **** SET FLAGS WHICH DEPEND ON METHOD USED
c++  Code for STIFF is inactive
c      METHOD = 1
c      IF (MAXINT .GT. 0) IF (MAXDIF) 85,90,85
c      METHOD = -1
c   85 CONTINUE
c      KPRED = 5
c      GO TO 100
c++  End
   90 METHOD = 0
      KPRED = 1
  100 continue
c
c ******* CHECK KORD FOR DIAGNOSTIC OUTPUT CONTROL *********
c
c++  Code for OUTPUT is active
      if (IOP10 .gt. 0) then
         if (NOUTKO .ne. 0) then
            INTCHK(NXTCHK) = 10
            ILOW = NOUTKO
c.    **** Check option for valid input
            KGO = 2
            go to 430
         end if
      end if
c++  End
  110 continue
c
c ********** CHECK KORD FOR INTEGRATION ORDER CONTROL ******
c
c++  Code for INTEGO is active
      if (IOP19 .ne. 0) then
c.           **** Check option for valid input
         INTCHK(NXTCHK) = 19
         ILOW = IOP19
         JLIM = -30
         KGO = 3
         go to 430
      end if
c++  End
  120 continue
c
c ********** CHECK SET UP FOR ERROR TOLERANCES *************
c
      INTCHK(NXTCHK) = 16
      ILOW = IOP16
      JLIM = -5
      KGO = 4
      if (IOP16 .ne. 0) go to 430
c.                      **** IN CURRENT CODE, IOP16=0 IS AN ERROR
      KGO = 10
      go to 650
  150 continue
c     **** CHECK KORD STORAGE ALLOCATION
      call OPTCHK(INTCHK, IOPT, 'DIVA / KORD$E')
      if (NXTCHK .lt. 0) KORD2I = -4
c
c     ******** DONE CHECKING KORD STORAGE ALLOCATION *******
c
c     **** CHECK  Y  STORAGE ALLOCATION
      INTCHK(2) = IDIMY
      INTCHK(3) = NY + NY
      NXTCHK = 4
      NYNY = NY + 1
      call OPTCHK(INTCHK, IOPT, 'DIVA / Y$E')
      if (NXTCHK .lt. 0) KORD2I = -4
c
c     **** CHECK  F  STORAGE ALLOCATION
      INTCHK(2) = IDIMF
      INTCHK(3) = NTE
      NXTCHK = 4
      if (IOP16 .ne. 0) then
c                                Error tolerance info.
         INTCHK(4) = 16
         INTCHK(5) = NTOLF
         INTCHK(6) = IHI - IOP16 + 1
         NXTCHK = 7
      end if
      if (IOP12 .gt. 0) then
         INTCHK(NXTCHK) = 12
         INTCHK(NXTCHK+1) = IOP12
         INTCHK(NXTCHK+2) = 4
         NXTCHK = NXTCHK + 3
      end if
c
c++  Code for ERRSTO is inactive
c      IF (IOP20 .ne. 0) then
cc.                                Space for saving error estimates
c         INTCHK(NXTCHK) = 20
c         INTCHK(NXTCHK) = IOP20
c         INTCHK(NXTCHK) = NTE
c         NXTCHK = NXTCHK + 3
c      end if
c++  Code for STIFF is inactive
c      if (IOP21 .gt. 0) then
cc.                               Info. for stiff equations
c         INTCHK(NXTCHK) = 21
c         INTCHK(NXTCHK+1) = IOP21
c         INTCHK(NXTCHK+2) = IOP21S
c         NXTCHK = NXTCHK + 3
c      end if
c      MAXKQD = min(MAXKQI, 6)
c++  End
c                          Set aside space for the difference tables.
      INTCHK(NXTCHK) = 0
      INTCHK(NXTCHK+1) = -KDIM * NTE
      INTCHK(NXTCHK+2) = 0
      NXTCHK = NXTCHK + 3
      INTCHK(NXTCHK) = -5 * NTE
      call OPTCHK(INTCHK, IOPT, 'DIVA / F$E')
      if (NXTCHK .lt. 0) then
         KORD2I = -4
      else if (KORD2I .ne. -4) then
         do 290 K = NXTCHK+1, INTCHK(NXTCHK)
            if (INTCHK(INTCHK(K)) .eq. 0) then
               NDTF = INTCHK(INTCHK(K)+1)
               NUMDT = min(KDIM, (INTCHK(INTCHK(K)+2)-NDTF+1) / NTE)
               MAXKQI = NUMDT - 1
            else
c         Take a quick return if needed space was not specified by user.
               KORD2I = -4
            end if
  290    continue
      end if
  320 continue
      if ((KORD2I .eq. -4) .or. (IOP10 .ne. 0)) then
         MACT1(3) = IOPIVA(2)
         call MESS(MACT1, 'IOPT()= $B', IOPT)
         KORD1I = 24
         KORD(1) = 24
      end if
      TMARK = TSPECS(1)
c
c     **** DONE WITH INITIALIZATION AND CHECKING INPUTS
      if (IOP13 + IOP14 + IOP15 .ne. 0) return
  330 call DIVAA(TSPECS, Y, F, KORD, DERIVS, OUTPUT)
      return
c
c ************ LOOP TO CHECK OPTION SPECIFICATIONS *********
c
  430 JL = 0
      do 560 IHI = ILOW, IDIMK
         J = KORD(IHI)
         go to (460, 480, 490, 490), KGO
c     **** CHECK ON VARIATIONAL EQUATIONS
  460    continue
c++  Code for VAREQ is active
         if (J - NTE) 470, 565, 620
  470    if (J .eq. 0) go to 560
         if (J .le. JL) go to 620
c++  End
c     **** Check on diagnostic output option
  480    continue
c++  Code for OUTPUT is active
c.    **** CHECK IF DONE
         if (J .ge. NTE) go to 565
         if (J .le. JL) go to 620
         go to 550
c++  End
  490    continue
c     **** Check integration order control (KGO=3) and
c     **** error tolerance equation grouping (KGO=4).
         if (J - NTE) 500, 565, 620
  500    if (J) 510, 530, 540
  510    if ((JL .le. 0) .and. (IHI .ne. ILOW)) go to 620
         if (J .lt. JLIM) then
c                         Output an error message.
            IDAT(2) = JLIM
            IDAT(3) = 0
            go to 630
         end if
  520    JL = -JL
         go to 560
  530    if (KGO .eq. 3) go to 520
         KFERR = NTOLF + IHI - ILOW
         if (F(KFERR) .eq. CM1) go to 510
c                         Set up error message, TOL must be -1.
            IDAT(1) = KFERR
            KGO = 11
            go to 650
  540    if (abs(JL) .ge. abs(J)) go to 620
  550    JL = J
  560    continue
  565 NXTCHK = NXTCHK + 3
      INTCHK(NXTCHK-2) = ILOW
      INTCHK(NXTCHK-1) = IHI - ILOW + 1
      go to (30, 110, 120, 150), KGO
c
c     **** AN ERROR HAS BEEN MADE
c                  Error in setting up TSPECS for extra output
  600 IHI = IHI + 2
  610 ILOW = -IOP5
      go to 630
c                  Error in KORD indices
  620 IDAT(2) = abs(JL) + 1
      IDAT(3) = NTE
c                  Set up for print of message about KORD
  630 IDAT(1) = INTCHK(NXTCHK)
  640 IDAT(4) = IHI
      IDAT(5) = KORD(IHI)
c
c ***************** Process Errors *************************************
c
  650 KORD2I = -4
      MACT(4) = LTXTAF
      if (KGO .ge. 8) MACT(4) = -1
      MACT(6) = MLOC(KGO)
c--D Next line special: P=>S, X=>D
      CALL DMESS(MACT, MTXTAA, IDAT, FDAT)
      if (KGO .lt. 8) then
         MACT(10) = ILOW
         MACT(13) = ILOW
         MACT(15) = -min(IHI+2, IDIMK)
         CALL MESS(MACT(9), MTXTAB, KORD)
         if (KGO .le. 4) go to 565
      end if
c              5   6   7    8    9   10   11
      go to (100, 25, 25, 320, 100, 150, 660), KGO - 4
  660 KGO = 4
      go to 565
      end
c   End of DIVA

      subroutine DIVAA(TSPECS, Y, F, KORD, DERIVS, OUTPUT)
c>> 19890224 DIVAA  Krogh   Big error with BETA(2)=1+epsilon -- looped
c>> 19880727 DIVAA  Krogh   Fixed to allow restart on a restart.
c>> 19880307 DIVAA  Krogh   Initial code.
c
c  MAIN SUBROUTINE FOR VARIABLE ORDER INTEGRATION OF ORDINARY
c  DIFFERENTIAL EQUATIONS
c
      integer KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision TSPECS(*), Y(*)
      double precision F(*)
      external DERIVS, OUTPUT
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2
      double precision EEPT75, EOVEP2, OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      double precision CMP75, C0, C1M5, C1M3, C2P5M3, C8M3, CP0625, CP1
      double precision CP25, CP3, CP4, CP5, CP875, C1, C1P125, C1P3, C2
      double precision C6, C10, C16, C4096
      parameter (CMP75 = -.75D0)
      parameter (C0 = 0.D0)
      parameter (C1M5 = 1.D-5)
      parameter (C1M3 = 1.D-3)
      parameter (C2P5M3 = 2.5D-3)
      parameter (C8M3 = 8.D-3)
      parameter (CP0625 = .0625D0)
      parameter (CP1 = .1D0)
      parameter (CP25 = .25D0)
      parameter (CP3 = .3D0)
      parameter (CP4 = .4D0)
      parameter (CP5 = .5D0)
      parameter (CP875 = .875D0)
      parameter (C1 = 1.D0)
      parameter (C1P125 = 1.125D0)
      parameter (C1P3 = 1.3D0)
      parameter (C2 = 2.D0)
      parameter (C6 = 6.D0)
      parameter (C10 = 10.D0)
      parameter (C16 = 16.D0)
      parameter (C4096 = 4096.D0)
c
      integer LDIS, KEXIT, I, J, K, J1, J2, L, LX
      double precision TP, TP1, TP2, TP3, HH, DISADJ
      double precision SIGMAS, TPS1, TPS2, EIMINO, EXR
c--D Next line special: P=>D, X=>Q
      double precision  TMARKA(2), XP, XP1
      equivalence (G(1, 1), HH), (TMARKA(1), TMARK)
      equivalence (KEXIT, IOP17)
      save EIMINO, EXR, SIGMAS, TP, TP1, TP2, TP3, TPS1, TPS2, DISADJ,
     1   LDIS
c
c                      Declarations for error message processing.
c
      integer MACT(17), MLOC(8), MENTXT, MERET, MEEMES, METEXT
      parameter (MENTXT =23)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVAA$B
cAB At: TN=$F, KSTEP=$I, with H=$F$E
cAC A previously reported error was fatal.$E
cAD Print points not properly ordered: TSPEC($I)=$F$E
cAE An error tolerance of 0 requires setting special flags.  $B
cAF Step size reduced too fast, doing a restart.  $B
cAG H is so small that TN + H = TN.  $B
cAH Error tolerance too small.  $B
cAI Step size at end of start < HMIN=$F, $B
cAJ Error estimates require a stepsize < HMIN=$F, $B
cAK (Estimated Error) / (Requested Error) for equation $I is $F.  $B
cAL Tolerance $I is F($I) = $F.$B
cAM Tolerance $I is F($I) * F($I) = $F * $F = $F.$B
cAN   Replacing F($I) with $F.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 40,LTXTAD= 80,LTXTAE=129,
     * LTXTAF=189,LTXTAG=237,LTXTAH=272,LTXTAI=302,LTXTAJ=342,
     * LTXTAK=390,LTXTAL=455,LTXTAM=484,LTXTAN=531)
      character MTXTAA(3) * (186)
c
      integer LOCM, MLOCAC, MLOCAD, MLOCAE, MLOCAF, MLOCAG, MLOCAH,
     1   MLOCAI, MLOCAJ

      parameter (LOCM = 32 * 256)
c                     KORD1I   Severity   Loc. message
      parameter (MLOCAC = 23 + 32 * (99 + 256 * LTXTAC))
      parameter (MLOCAD = 13 + 32 * (38 + 256 * LTXTAD))
      parameter (MLOCAE = 22 + 32 * (38 + 256 * LTXTAE))
      parameter (MLOCAF =  3 + 32 * (14 + 256 * LTXTAF))
      parameter (MLOCAG = 21 + 32 * (38 + 256 * LTXTAG))
      parameter (MLOCAH =  2 + 32 * (25 + 256 * LTXTAH))
      parameter (MLOCAI = 11 + 32 * (38 + 256 * LTXTAI))
      parameter (MLOCAJ = 12 + 32 * (38 + 256 * LTXTAJ))
c
      data MTXTAA/'DIVAA$BAt: TN=$F, KSTEP=$I, with H=$F$EA previously r
     *eported error was fatal.$EPrint points not properly ordered: TSPEC
     *($I)=$F$EAn error tolerance of 0 requires setting special flags. $
     * ','$BStep size reduced too fast, doing a restart.  $BH is so smal
     *l that TN + H = TN.  $BError tolerance too small.  $BStep size at$
     * end of start < HMIN=$F, $BError estimates require a steps','ize <
     * HMIN=$F, $B(Estimated Error) / (Requested Error) for equation $I$
     * is $F.  $BTolerance $I is F($I) = $F.$BTolerance $I is F($I) * F(
     *$I) = $F * $F = $F.$B  Replacing F($I) with $F.$E'/
      data MLOC / MLOCAC, MLOCAD, MLOCAE, MLOCAF, MLOCAG, MLOCAH,
     1   MLOCAI, MLOCAJ /
c
c                      1 2 3 4       5 6       7       8 9      10
      data MACT / MEEMES,0,0,0, MENTXT,0, METEXT, MENTXT,0, METEXT,
     1  MENTXT, 0, METEXT, MENTXT, LTXTAN, METEXT, MERET /
c           11 12      13      14      15      16     17
c
      data EXR, EIMINO / CP1, C8M3 /
      data LDIS / 0 /
c
c ************** START OF EXECUTABLE CODE ******************************
c
  660 if (KORD2I) 670, 1380, 1840
  670 if (KORD2I .eq. -1) go to 2140
      if (KORD2I .eq. -5) go to 720
      if (KORD2I+8) 1840, 710, 1840
c     **** SPECIAL OUTPUT CASE (EXTRAPOLATION OR GSTOP)
  680 go to (1220, 1190, 830, 690, 690, 2190), KEXIT
c     **** RESET TMARK BEFORE GOING WHERE DIRECTED BY KEXIT
  690 KEXIT = KEXIT - 2
      go to 1890
c     **** USER HAS REQUESTED A RESTART
  700 KORD2I = -5
      IGFLG = 0
      if ((KORD(2) .ge. 2) .and. (LSC .lt. 3)) KORD2I = -8
      if ((KORD1I .le. 3) .or. (KORD1I .eq. 5)) go to 1890
      if (KORD2I .eq. -5) go to 720
c                Set up for a discontinuity
  710 XP = TSPECS(1)
      if (KORD(2) .eq. 3) then
c                Adjust for discontinuity in Y
         J = 0
         do 712 I = 1, NTE
            if (NKDKO .ne. 0) KORDI = KORD(NKDKO + I -1)
            K = 1
            J = J + KORDI
            XP1 = Y(J)
  711       Y(NYNY + J - K) = Y(NYNY + J - K) + XP1
            if (K .lt. KORDI) then
               XP1 = Y(J-K) + (TN - XP) * XP1 / dble(K)
               K = K + 1
               go to 711
            end if
  712    continue
      else
         IGFLG = -3
      end if
      DISADJ = HH
      XP1 = (XP - TN) / XI(1)
      if (XP1 .lt. CP25) then
         K = 1
         if (XP1 .lt. CMP75) K = 2
         TSPECS(1) = TN - XI(K)
         TSPECS(2) = 2.D0*(TN - TSPECS(1))
         call DIVAIN(TSPECS(1), Y, F, KORD)
         do 713 J = 1, NY
            Y(NYNY + J - 1) = Y(J)
  713    continue
c          Move difference tables back one step
         TN = TSPECS(1)
  714    call DIVABU(F, KORD)
         if (K .eq. 2) then
            KSC = max(KSC - 1, 1)
            do 715 K = max(1, KSC), IOP11-1
               BETA(K+1) = BETA(K) * (XI(K) / (XI(K+1) - XI(1)))
  715       continue
            K = 0
            go to 714
         end if
      end if
c      Take step to discontinuity.
      HH = (XP - TN)
      EREP = -abs(EREP)
      KIS = 1000
      LDIS = 1
      LSC = 0
      LINC = 0
      HINCC = C1P125
      KSSTRT = KSTEP + 2
      go to 1115
c ********
c INITIALIZE FOR STARTING AN INTEGRATION
c ********
  720 HH = TSPECS(2)
      LSC = 8
      LINC = -5
      LINCD = 64
      LDT = -4
      KQMAXI = 0
      EIMIN = CP3
      EAVE = C10
      XI(1) = C0
      IGFLG = 0
      KIS = 0
c GO COMPUTE INITIAL DERIVATIVES
  730 KORD2I = -6
c++  Code for VAREQ is active
      ICF = NE
c++  End
  740 KORD1I = KPRED
      go to 1360
c   RETURN AFTER COMPUTING INITIAL (OR NOISE TEST) DERIVATIVES
  750 continue
c++  Code for VAREQ is active
      if (IOP18 .eq. 0) go to 790
c.    **** SPECIAL LOGIC TO COMPUTE VARIATIONAL DERIVATIVES
  760 if (KORD1I .eq. 3) go to 770
      KORD1I = 3
      KORD(3) = 0
  770 if (ICF .eq. NTE) go to 790
      ICS = ICF + 1
  780 KORD(3) = KORD(3) + 1
      ICF = IOP18 + KORD(3)
      ICF = abs(KORD(ICF))
      if (ICF) 1360, 780, 1360
c++  End
  790 ICS = 1
      ICF = NE
c++  Code for VAREQ is active
      if (KORD2I .eq. 0) if (EREP) 2220, 2220, 1430
c++  End
      if (LINC + 5) 1490, 810, 1490
c END OF SPECIAL CODE FOR INITIALIZATION AT THE START
c ********
c UPDATE VARIABLES TO PREPARE FOR NEXT STEP
c ********
  800 LDT = 0
      EIMIN = C2P5M3+EIMIN*(C6*EAVE+EIMAX)/((EIMIN+C1)*(C6*EIMAX+EAVE))
  810 do 820 J = 1, NY
  820    Y(NYNY + J - 1) = Y(J)
      TN = TSPECS(1)
c ********
c TEST FOR VARIOUS TYPES OF OUTPUT
c ********
c     TEST FOR END OF STEP OUTPUT (OR IF DUMP OUTPUT TO BE TESTED FOR)
      if (IOP6 .eq. 0) go to 840
c   SET UP FOR END OF STEP OUTPUT
      KORD1I = 6
      go to 1810
c     SET UP AFTER OTHER COMPUTATIONS HAVE BEEN MADE (TSPECS(1).NE.TN)
  830 KORD1I = 6
      go to 860
c     TEST FOR AN OUTPUT POINT
  840 if (HH * (TMARK - TN)) 1760, 1760, 850
c     TEST FOR TOO MANY STEPS OUTPUT
  850 continue
      if (KSOUT .gt. KSTEP) go to 890
      KORD1I = 4
  860 if (TSPECS(1) .eq. TN) go to 1810
c     GO INTERPOLATE VALUES AT END OF LAST STEP
  870 TSPECS(1) = TN
      go to 1780
c     CONTINUE AFTER TOO MANY STEPS OUTPUT
  880 continue
      KSOUT = KSTEP + IOP4
  890 continue
c++  Code for DUMP is active
      if (IOP9 .eq. 0) go to 920
c++  Code for DUMP & STIFF is inactive
c      KQMXDS=KQMAXD
c++  Code for DUMP is active
      KQMXIS = KQMAXI
      KIS = KIS + 1
      if (KIS .ne. 0) go to 920
c.   TIME TO DUMP THE SOLUTION
  900 KORD1I = 9
c.    SET TO UPDATE THE DIFFERENCE TABLE
      if (LDT .eq. 1) go to 1810
c Note that doing this update can lead to very small differences in the
c results because of round off differences.
      LDT = -2
      go to 1780
c++  End
c RETURN AFTER DUMPING THE SOLUTION
  910 continue
c++  Code for DUMP is active
      KIS = 2
c.    TEST IF SOLUTION DUMP DUE TO RESTART, END, OR
c.    DROP IN INTEG. ORDER
      if (LINC .lt. 0) if (LINC + 6) 1860, 1750, 1180
c++  End
c END OF TESTING FOR VARIOUS TYPES OF OUTPUT
c   TEST IF STEPSIZE MAY BE INCREASED OR IF TESTS SHOULD BE MADE
c   FOR DECREASING THE STEPSIZE (OR IF STARTING OR USER SELECTING H)
  920 KSTEP = KSTEP + 1
      if (LINC) 930, 980, 940
  930 continue
c     **** ONLY POSSIBLE VALUES AT THIS POINT ARE
c          LINC = -2 OR -5
      if (LINC + 5) 1120, 1110, 1120
c ********
c ERROR ESTIMATES INDICATE STEPSIZE CAN BE INCREASED
c ********
  940 HC = HINCC
      if (LINC .gt. 1) HC = HC ** LINC
      HH = HC * HH
c     TEST IF NEW STEPSIZE IS TOO BIG
      if (abs(HH) .gt. HMAX) if (HMAX) 970, 970, 960
  950 EAVE = EIMAX
      ROBND = CP3 + HINCC
      go to 1110
c     NEW STEPSIZE IS TOO BIG
  960 if (abs(XI(1)) .ge. HMAXP9) go to 970
      HH = sign(HMAX, HH)
      go to 950
c     RESTORE THE OLD STEPSIZE
  970 HH = XI(1)
      LINC = 0
      go to 1150
c END OF CODE FOR CASE WHEN ERROR ESTIMATES INDICATE STEPSIZE INCREASE
c ********
c TEST IF ESTIMATED ERRORS INDICATE STEPSIZE SHOULD BE DECREASED
c ********
  980 ROBND = C1P3
      if (EIMAX .le. EAVE) go to 990
      EAVE = EAVE + CP4 * (EIMAX - EAVE)
      if ((EIMAX * EMAX) - C1M3) 1000, 1010, 1010
  990 EAVE = EIMAX
      if ((EIMAX * EMAX) .ge. EIMIN) go to 1010
 1000 ROBND = CP3 + (SIGMA(KQMAXS)/SIGMA(KQMAXS-1))
      go to 1180
c     TEST IF STEPSIZE SHOULD BE REDUCED
 1010 if (EMAX * EIMAX .lt. EXR * EAVE) go to 1180
c ********
c ERROR ESTIMATES INDICATE STEPSIZE SHOULD BE REDUCED
c ********
      HC = HDEC
      if (EIMIN .le. EIMINO) go to 1030
      EIMIN = EIMINO
      HC = CP875
 1030 HH = HC * XI(1)
      if (LSC - 1) 1040, 1080, 1090
 1040 if (abs(HH) .ge. HMIN) go to 1090
      if (abs(CP875*XI(1)) .le. HMIN) if (LINC) 1050, 970, 970
      HH = sign(HMIN, HH)
      go to 1090
c     STEPSIZE IS TOO SMALL TO BE REDUCED
c     SET UP ERROR INDICATORS AND PREPARE FOR RETURN TO USER
 1050 KORD1I = 8
      go to 2240
c     PROCEED WITH CURRENT STEPSIZE DESPITE ERROR BEING TOO BIG
 1070 HH = XI(1)
      TSPECS(2) = HH
      EMAX = C0
      LINC = 0
      if (KORD1I - 2) 1420, 1430, 1430
c     SET LSC TO END STARTING PHASE
 1080 LSC = 2
c     CHECK IF REPEATING A STEP
 1090 if (LINC .ne. -1) go to 1110
c   WHEN REPEATING A STEP, BACK UP THE DIFFERENCES AND STEPSIZE INFO.
 1100 call DIVABU(F, KORD)
c   TEST IF NOISE TEST (LINC = -7) OR IF H IS NOT
c     BEING CHANGED (LINC = -4)
      if (LINC + 4) 1780, 1180, 1110
c ********
c STEPSIZE IS TO BE CHANGED
c ********
 1110 continue
c MODIFY STEPSIZE TO REDUCE ROUNDOFF ERROR IN ACCUMULATING INDEP. VAR.
      TP = C2 * abs(TN) + C4096 * abs(HH)
      TP = (TP + abs(HH)) - TP
      if (TP .ne. C0) HH = sign(TP, HH)
c     TEST IF NEW STEPSIZE SELECTED ACTUALLY GIVES A CHANGE
      if (HH .eq. TSPECS(2)) go to 1140
 1115 TSPECS(2) = HH
      if (IOP8 .eq. 0) go to 1140
c     SETUP TO TELL USER ABOUT STEPSIZE CHANGE (OR TO CHANGE STEPSIZE)
 1120 KORD1I = 8
      go to 860
c     RETURN AFTER TELLING USER ABOUT STEPSIZE CHANGE
 1130 HH = TSPECS(2)
 1140 if (HH .ne. XI(1)) KQICON = -1
 1150 HC = min(EOVEP2, abs(HH)) / EEPS2
c ********
c PREPARE FOR BEGINNING A NEW STEP
c ********
      if (LINC .gt. 0) then
         LINC = min(LINC, LINCQ) + LINCQ
         go to 1190
      end if
 1180 LINC = LINCD
 1190 if (HC .gt. abs(TN)) go to 1200
c     **** GIVE SINGULARITY DIAGNOSTIC
      KORD1I = 5
      go to 2240
 1200 TSPECS(1) = TN + HH
      if (LEX .eq. 0) go to 1250
      if (HH * (TSPECS(1) - TMARKX) .lt. C0) go to 1250
      TSPECS(1) = TMARKX
      HH = TMARKX - TN
      LINC = 64
      if (LEX .gt. 0) go to 1240
      if ((LSC .lt. 4) .and. (HH / XI(1) .lt. CP3)) go to 1230
 1220 HH = CP875 * HH
      go to 1110
c     **** GIVE OUTPUT AT CURRENT TMARK (WITH EXTRAPOLATION)
 1230 KORD1I = -KMARK
      go to 1770
c     **** INTEGRATE TO TMARKX
 1240 KQICON = -1
c   TEST IF SUBROUTINE FOR COMPUTING INTEGRATION COEFF. SHOULD BE CALLED
 1250 continue
c++  Code for STIFF is inactive
c      IF ((KQMAXI .LT.KQICON) .OR. (KQMAXD.LT.KQDCON)) GO TO 1320
c++  Code for ~STIFF is active
      if (KQMAXI .lt. KQICON) go to 1320
c++  End
c   GO COMPUTE COEFFICIENTS REQUIRED FOR THE INTEGRATION
c     TEST IF STARTING
      if (LSC .lt. 7) go to 1310
 1260 KQMAXI = 2
c++  Code for STIFF is inactive
c      IF (METHOD) 1262,1270,1264
c 1262 KQMAXI=0
c 1264 KQMAXD=max(MAXDIF,2)
c      CALL DIVAHC
cc.  SET UP TO GO DO INITIALIZATION FOR CASE OF STIFF EQUATIONS
c      KORD1I=5
c      GO TO 1350
c++  End
c   INITIALIZE FOR EQUATIONS WHICH ARE NOT STIFF
 1270 KQMAXD = 0
      call DIVAHC
      J = NDTF
      do 1300 I = 1, NTE
         if (KORD(I + 3) .le. 0) go to 1290
         KORD(I + 3) = 1
c     INITIALIZE THE DIFFERENCE TABLE
         if (LDT .eq. -4) F(J) = F(I)
         F(J + 1) = C0
         F(J + 2) = C0
 1290    continue
         J = J + NUMDT
 1300    continue
      if (LSC .eq. 5) go to 1340
      LSC = 7
      LDT = 1
      go to 1330
c   INTEGRATION IS NOT BEING STARTED
 1310 K = KORD(KEMAX + 3)
      SIGMAS = SIGMA(K)
      call DIVAHC
c     **** ADJUST EAVE
      TPS1 = BETA(K)
      if (TPS1 .gt. C1) TPS1 = CP5 * TPS1 + CP5
      EAVE = EAVE * TPS1 * (SIGMA(K) / SIGMAS)
c     TEST BELOW USED TO GET SAME RESULTS WITH/WITHOUT EXTRA EQUATIONS
      if (K .gt. KQICON) LSC = max(LSC, -3)
c END OF SPECIAL LOGIC FOR CASE WHEN INTEG. COEFF. ROUTINE IS CALLED
 1320 continue
c ********
c PREDICT Y
c ********
 1330 continue
c++  Code for ~ARGM is active
      call DIVAPR(Y, Y(NYNY), F, KORD)
c++  Code for ARGM is inactive
c      CALL DIVAPE
c++  End
c     GO GET PREDICTED DERIVATIVES
 1340 KORD1I = KPRED
c ********
c CALL DERIVS  (OR RETURN)
c ********
 1350 KORD2I = 0
 1360 KORD(1) = KORD1I
      KORD(2) = 0
      if (IOP13 .ne. 0) return
      call DERIVS(TSPECS(1), Y, F, KORD(1))
c     TEST FOR SPECIAL USER RETURN
 1380 if (KORD(1) .lt. 0) go to 2130
c     TEST FOR SPECIAL CASE
      if (KORD2I .ne. 0) go to 660
c ********
c TRANSFER CONTROL TO PROPER PLACE AFTER COMPUTING DERIVATIVES
c ********
      if (KORD1I - 2) 1400, 800, 1390
 1390 continue
c++  Code for VAREQ is active
      if (ICS - ICF) 1410, 1410, 760
c++  End
c ********
c PREPARE FOR CORRECTING, AND CORRECT Y
c ********
 1400 ITOLEP = 0
      ILGREP = 0
      IY = 1
      EIMAX = C1M5
      EMAX = C0
      KQMAXI = 2
      KQMAXS = 2
c++  Code for STIFF is inactive
c      IF (METHOD) 1404,1410,1406
c 1404 KQMAXI=0
c 1406 KQMAXD=2
c++  End
 1410 continue
c++  Code for ~ARGM is active
      call DIVACR(Y, F, KORD, F(NTOLF), KORD(IOP16))
c++  Code for ARGM is inactive
c      CALL DIVACE
c++  End
c     TEST IF ESTIMATED ERROR IS TOO BIG (OR IF DIAGNOSTIC CALLED FOR)
      if (EMAX .gt. EREP) if (EREP) 2210, 2210, 1670
 1420 continue
c++  Code for VAREQ is active
      if (IOP18 .ne. 0) go to 760
c++  End
 1430 KORD1I = 2
c     TEST IF NOISE APPEARS TO LIMIT PRECISION
      if (EMAX .lt. C0) go to 1470
c++  Code for ~STIFF is active
      if (LSC) 1450, 1360, 1610
c++  Code for STIFF is inactive
c      IF (LSC) 1450,1460,1610
c++  End
c     SET LSC=0 IF NOISE NO LONGER APPEARS TO LIMIT PRECISION
c     OR IF THE END OF THE STARTING PHASE HAS BEEN REACHED
 1450 LSC = 0
 1460 if (METHOD) 800, 1350, 1350
c ********
c NOISE APPEARS TO BE LIMITING PRECISION
c ********
 1470 continue
      if (LSC .le. 0) LSC = max(LSC - 1, -KQMAXS)
      if (LSC .eq. -1) go to 1460
      if (abs(EMAX) .lt. EXR) go to 1590
      LINC = -7
      TPS2 = (C1 + BETA(NOISEQ - 1)) ** NOISEQ
      if (SNOISE .lt. EEPS10 * TPS2) go to 1550
      TP = sign(EEPT75 * abs(TN) + OVTM75, HH)
      if (abs(TP) .gt. abs(HH)) go to 1550
      TSPECS(1) = TN + TP
      KORD1I = 0
c     **** GO TO BACK UP THE DIFFERENCES AND GET F(TSPECS(1))
      go to 1100
c     **** SOLUTION HAS BEEN INTERPOLATED AND F COMPUTED
 1490 continue
      KORD1I = 0
      LINC = LINC - 1
      if (LINC + 9) 1510, 1520, 1500
 1500 TSPECS(1) = TN + (TP + TP)
      TP1 = F(KEMAX)
      TP2 = F(NDTF + NUMDT * KEMAX - NUMDT)
      go to 1780
c     **** COMPUTE 2-ND DIFFERENCE AT CLOSE SPACED T VALUES
 1510 TP2 = TP3
 1520 TP3 = F(KEMAX)
      TPS1 = abs((TP3 - TP1) - (TP1 - TP2))
      if ((C16 * TPS1 * TPS2) .ge. DNOISE) if (LINC + 9) 1550, 870, 1550
 1530 TPS1 = EEPS10
      TPS2 = CP25 * SNOISE / RBQ(NOISEQ)
      do 1540 K = 2, NUMDT
         TPS1 = TPS1 + TPS1
         RBQ(K) = max(TPS1, TPS2 * RBQ(K))
 1540    continue
      LINC = 0
      HH = CP875 * HH
      go to 1040
c     **** SET UP TO GIVE NOISE DIAGNOSTIC
 1550 KORD1I = 6
      go to 2240
c     **** AFTER GIVING NOISE DIAGNOSTIC
 1560 KORD1I = 2
      if (KORD(2) .ge. 0) go to 1530
c     **** SET NEW VALUE FOR OFFENDING TOL
      F(NTOLF + ITOLEP - 1) = FDAT(7)
      if (LINC + 7) 1180, 1570, 1180
 1570 LINC = 0
 1580 if (LSC) 1460, 1460, 1610
c     **** CHANGE HINCC AND ADJUST SIGMA( )
 1590 if (LSC .ne. -4) go to 1580
      if (HINCC .eq. C1P125) go to 1580
      TPS1 = C1P125 / HINCC
      TPS2 = 1.0
      do 1600 K = 2, IOP11
         TPS2 = TPS2 * TPS1
         SIGMA(K) = SIGMA(K) * TPS2
 1600    continue
      EAVE = EAVE * TPS1 ** (1-KORD(KEMAX+3))
      LINCD = 6
      LINCQ = 12
      HINCC = C1P125
      go to 1460
c   END OF CODE FOR CASE WHEN NOISE APPEARS TO LIMIT PRECISION
c ********
c SPECIAL LOGIC FOR STARTING THE INTEGRATION
c ********
 1610 if (LSC .eq. 1) go to 800
      LSC = LSC - 1
      if (LSC - 2) 1620, 1640, 1650
 1620 if (EIMAX .le. (CP0625*EAVE*(SIGMA(KQMAXS)/SIGMAS)*(BETA(KQMAXS+
     1   1))**2)) go to 800
 1630 KSSTRT = KSTEP + 2
c   TEST IF STEPSIZE IS TOO SMALL BEFORE ENDING STARTING PHASE
      if (abs(HH) .ge. HMIN) go to 1450
c     GIVE DIAGNOSTIC FOR STEPSIZE TOO SMALL AT END OF START
      KORD1I = 7
      go to 2240
c   SET LSC TO DO ONE DERIVATIVE EVAL. PER STEP
 1640 LSC = 1
      go to 800
c     TEST IF FIRST TIME THROUGH THE FIRST STEP
 1650 if (LSC .eq. 6) go to 1340
c     END STARTING PHASE IF CONVERGENCE OF CORRECTOR ITERATES TOO SLOW
      if (LDT .eq. -5) go to 1660
      LSC = min(LSC, 4)
      go to 800
 1660 LDT = 0
      if (LSC - 4) 1260, 1630, 1260
c END OF SPECIAL LOGIC FOR STARTING THE INTEGRATION
c ********
c ESTIMATED ERROR IS TOO BIG
c ********
 1670 if (BETA(2) - C1) 1690, 1730, 1680
 1680 HC = C1 / BETA(2)
      if (BETA(2) .ge. C1P125) go to 1740
 1690 if (BETA(2) .gt. CP3) go to 1730
c   REQUIRED STEPSIZE REDUCTION IS TOO RAPID -- GIVE A DIAGNOSTIC
      KORD1I = 4
      go to 2240
c
c     TEST KORD(2) AFTER ABOVE DIAGNOSTIC OR A DISCONTINUITY DIAGNOSTIC
 1700 continue
      if (KORD(2) .eq. 0) go to 1730
c  TEST IF SOLUTION MUST BE DUMPED BEFORE A RESTART
 1710 LINC = -1
c++  Code for DUMP is active
      if (IOP9 .eq. 0) go to 1750
      if (KIS .eq. 2) go to 1750
      LINC = -6
c.    GO DUMP SOLUTION BEFORE REPEATING THE STEP
 1720 KQMAXI = KQMXIS
c++  Code for DUMP & STIFF is inactive
c      KQMAXD=KQMXDS
c++  Code for DUMP is active
      call DIVABU(F, KORD)
      go to 900
c++  End
c   SET UP TO REPEAT THE STEP
 1730 HC = CP5
 1740 LINC = -1
      if (LSC .le. 3) go to 1030
c   RESTART THE INTEGRATION IF ERROR IS TOO BIG ON FIRST OR SECOND STEP
c LOOP TO SELECT A NEW INITIAL STEPSIZE
 1750 LSC = 7
 1755 HH = HH * CP5
      EMAX = EMAX * CP25
      if (EMAX .ge. CP3) go to 1755
      go to 1090
c   END OF SELECTING A NEW INITIAL STEPSIZE
c END OF LOGIC FOR CASE WHEN ESTIMATED ERROR IS TOO BIG
c ********
c INTEGRATION HAS REACHED AN OUTPUT POINT
c ********
 1760 if (KMARK .eq. 0) go to 1920
      KORD1I = min(KMARK, 5)
      KORD(3) = KMARK
      if (TSPECS(1) .eq. TMARK) go to 1790
 1770 TSPECS(1) = TMARK
 1780 call DIVAIN(TSPECS(1), Y, F, KORD)
 1790 continue
      if (KORD1I) 1800, 730, 1810
c   OUTPUT POINT IS OBTAINED BY EXTRAPOLATION
 1800 continue
c++  Code for EXTRAP is active
      KORD1I = -KORD1I
      KORD2I = -7
      KEXIT = 4
c.  TEST IF GSTOP-S ARE PRESENT
c++  Code for EXTRAP &  GSTOP is active
      if (NGTOT .eq. 0) go to 1820
      IGFLG = 4
      KEXIT = 2
      KORD1I = 7
      if (IOP7) 740, 1820, 740
c++  End
c ********
c CALL OUTPUT  (OR RETURN)
c ********
 1810 KORD2I = 1
 1820 KORD(1) = KORD1I
      KORD(2) = 1
      if (IOP14 .ne. 0) return
      call OUTPUT(TSPECS(1), Y, F, KORD(1))
c     TEST FOR SPECIAL USER RETURN OR FOR A RESTART
c++  Code for ~DUMP is inactive
c 1840 IF (KORD(1)) 2130,700,1880
c++  Code for DUMP is active
 1840 if (KORD(1) .gt. 0) go to 1880
 1850 if (IOP9 .eq. 0) go to 1870
c.    **** GO DUMP THE SOLUTION
      LINC = -7
      ITOLEP = KORD(1)
      IDAT(1) = KORD(2)
      NEPTOL = KORD1I
      if (LSC .ne. 8) go to 900
 1860 LINC = min(0, LINCD)
      KORD1I = NEPTOL
      KORD(1) = ITOLEP
      KORD(2) = IDAT(1)
 1870 if (KORD(1)) 2130, 700, 2100
c++  End
 1880 if (KORD2I .lt. 0) go to (2140, 1810, 1350, 2110, 720, 750, 680,
     1   710), -KORD2I
      if (KORD2I .eq. 0) go to 1380
c ********
c TRANSFER CONTROL TO PROPER PLACE AFTER OUTPUT
c ********
 1890 if (KORD1I - 5) 1910, 1930, 1900
 1900 if (KORD1I - 8) 840, 1130, 910
 1910 if (KORD1I - 3) 1920, 1930, 880
c   GET NEW TOUT
 1920 TOUT = TSPECS(1) + TSPECS(3)
c GET NEW TMARK (NEXT INDEP. VAR. OUTPUT POINT)
 1930 XP = TMARK
      K = KMARK
      TMARK = TOUT
      KMARK = 2
      LEX = 0
      if (IOP5) 1940, 1980, 1970
 1940 I = -IOP5
 1950 I = I + 3
      J1 = KORD(I - 3)
      if (J1) 1950, 1980, 1960
 1960 J2 = KORD(I - 2)
      L = KORD(I - 1)
      go to 1990
 1970 J1 = 5
      J2 = IOP5
      L = 0
      if (J2 .ge. J1) go to 1990
 1980 J1 = 4
      J2 = 4
      L = IOP3
c
c     **** LOOP TO SET NEW TMARK (AND TMARKX)
 1990 do 2060 J = J1, J2
c        **** TEST IF EXTRAPOLATION NOT POSSIBLE
         if (L .eq. 0) go to 2010
         LX = 2
         if (LEX) 2020, 2030, 2020
 2000    LEX = L
 2010    LX = 1
 2020    if (HH * (TSPECS(J) - TMARKA(LX))) 2030, 2060, 2060
 2030    if (J .eq. 4) go to 2050
         if (HH * (TSPECS(J) - XP)) 2060, 2040, 2050
 2040    if ((K .ge. J) .or. (K .eq. 3)) go to 2060
 2050    TMARKA(LX) = TSPECS(J)
         if (LX .eq. 2) go to 2000
         KMARK = J
 2060    continue
      if (IOP5 .lt. 0) go to 1950
      if (J1 .ne. 4) go to 1980
      if (KMARK .eq. 4) KMARK = 3
c     **** TEST IF NEW TMARK IS ACCEPTABLE
      if (HH * (XP - TMARK)) 2070, 2080, 2090
 2070 if (KORD2I - 1) 670, 840, 670
 2080 if (K .ne. KMARK) go to 2070
c++  Code for DUMP is active
      if (KORD1I .eq. 3) go to 1850
c++  Code for ~DUMP is inactive
c      IF (KORD1I .EQ. 3) GO TO 2100
c++  End
 2090 if (KORD1I .eq. 13) go to 2190
c SETUP TO INDICATE ERROR IN SPECIFICATION OF OUTPUT POINTS
      KORD1I = 2
      IDAT(2) = KMARK
      if (KMARK .le. 3) IDAT(2) = KMARK + 1
      FDAT(3) = TSPECS(IDAT(2))
      go to 2240
c     SET KORD1I=1 TO INDICATE THAT END OF INTEGRATION HAS BEEN REACHED
 2100 KORD1I = 1
c ********
c RETURN TO USER
c ********
 2110 KORD2I = -1
      KORD(1) = KORD1I
 2130 KORD(2) = -1
      return
c ********
c TRANSFER CONTROL TO PROPER PLACE AFTER RETURN TO USER
c ********
 2140 if (KORD1I - 2) 2150, 1560, 2160
 2150 KORD2I = 1
      go to 1930
 2160 if (KORD1I - 4) 1700, 2200, 2170
 2170 if (KORD1I - 13) 2180, 1930, 2190
 2180 if (abs(HH) .ge. HMIN) if (KORD1I - 11) 1030, 1450, 1030
      if (KORD(2) .eq. 0) if (KORD1I - 11) 1070, 800, 1070
c   ERROR MESSAGES HAVE BEEN IGNORED -- COMPUTATION CAN NOT CONTINUE
 2190 KORD1I = 1
      go to 2240
c
c        AFTER A DISCONTINUITY RETURN
 2200 LINC = -4
      if (KORD(2)) 1710, 1730, 1100
c ********
c PROBLEM ENCOUNTERED WHEN CORRECTING
c ********
 2210 if (LDIS .eq. 0) go to 2230
c           Extra checks when had a user specified discontinuity.
c++  Code for VAREQ is active
      if (IOP18 .ne. 0) go to 760
c++  End
 2220 KORD1I = 2
      LDIS = LDIS + 1
      TP = DISADJ / HH
      if (KIS .ge. 1000) then
         if (LDIS .eq. 2) then
            HH = HH*min(min(TP, TP**2), (CP25 * EXR/EMAX)**.333333333D0)
            if (KQMAXS .le. 3) then
               LDIS = 0
               EREP = abs(EREP)
               TSPECS(2) = HH
               go to 720
            end if
            LINC = -5
            if (IOP9 .eq. 0) KIS = 1001
            go to 800
         end if
         if (IOP9 .eq. 0) KIS = KIS + 1
         if (KQMAXS .le. LDIS + 2) KIS = LDIS + 1
         LINC = min(LINC, LDIS-2)
      end if
      if (LDIS .gt. 2*KQMAXS) then
         EREP = abs(EREP)
         LDIS = 0
         if (EMAX .gt. EREP) go to 1670
         go to 1430
      end if
      if (TP .ge. HINCC**(LINC+2)) then
         if ((LDIS .ne. 3) .and. (TP .gt. dble(KQMAXS))) LSC = 1
         EIMIN = CP5
         EAVE = EAVE * TP**8
      end if
      if (LSC .eq. 2) go to 1630
      if (EMAX .gt. EXR) go to 1730
      go to 1430
c
 2230 EREP = abs(EREP)
c++  Code for DUMP is active
      if (LINC .lt. -3) go to 1720
c++  End
c     BAD TOL
      KORD1I = 3
c ********
c ERROR PROCESSING
c ********
 2240 FDAT(1) = TSPECS(1)
      FDAT(2) = HH
      IDAT(1) = KSTEP
      ITOLEP = max(NEPTOL, -NEPTOL - 1)
      J = 3
      if (KORD1I .ge. 7) then
         J = 4
         FDAT(3) = HMIN
      end if
      if (KORD1I .le. 3) then
         if (KORD1I .lt. 3) then
            K = 8
         else
            MACT(9) = LTXTAL
            FDAT(3) = C0
            IDAT(2) = ITOLEP
            IDAT(3) = ITOLEP + NTOLF - 1
            K = 11
         end if
      else
         MACT(9) = LTXTAK
         FDAT(J) = EMAX
         IDAT(2) = KEMAX
         IDAT(3) = ITOLEP
         IDAT(4) = ITOLEP + NTOLF - 1
         FDAT(J+1) = F(IDAT(4))
         K = 14
         if (KORD1I .eq. 6) then
            K = 17
            IDAT(5) = IDAT(4)
            FDAT(7) = 32.D0 * abs(EMAX) * FDAT(J+1)
            FDAT(J+2) = FDAT(7)
         end if
         MACT(12) = LTXTAL
         if (NEPTOL .lt. 0) then
            MACT(12) = LTXTAM
            IDAT(6) = IDAT(4)
            IDAT(5) = IDAT(4) + 1
            FDAT(J+2) = F(IDAT(5))
            FDAT(J+3) = FDAT(J+1) * FDAT(J+2)
         end if
      end if
c Set the location for the first part of the message that varies, set
c the error severity, and the index number, print the error and
c return or stop.
      L = MLOC(KORD1I)
      MACT(6) = L / LOCM
      MACT(2) = (L - MACT(6) * LOCM) / 32
      KORD1I = mod(L, 32)
      MACT(3) = KORD1I
      MACT(K) = MERET
c--D Next line special: P=>S, X=>D
      call DMESS(MACT, MTXTAA, IDAT, FDAT)
      MACT(K) = MENTXT
      go to 2110
c
      end
c   End of DIVAA

      subroutine DIVABU(F, KORD)
c>> 1987-12-07 DIVABU Krogh   Initial code.
c
c THIS SUBROUTINE RESTORES THE DIFFERENCE TABLE TO ITS STATE
c AT THE BEGINNING OF THE CURRENT STEP.  IF THE INTEGRATION ORDER
c WAS INCREASED, IT IS REDUCED. THE COMMON ARRAY XI IS ALSO
c RESTORED TO ITS STATE AT THE BEGINNING OF THE STEP. IF THE
c STEPSIZE IS NOT BEING CHANGED, THE ARRAY V USED TO COMPUTE
c INTEGRATION COEFFICIENTS IS RESTORED.
c
      integer KORD(*)
      double precision F(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      integer I, L, KQQ, J, K
      double precision TPD, C0, C2
      parameter (C0 = 0.D0)
      parameter (C2 = 2.D0)
c ********* START OF EXECUTABLE CODE **********
c
c ********
c BEGIN LOOP TO BACK UP DIFFERENCE TABLES
c ********
      L = NDTF - 1
      do 2410 I = 1, NTE
         KQQ = KORD(I + 3)
c++  Code for STIFF is inactive
c         IF (KQQ) 2302,2400,2310
cc.           EQUATION IS STIFF
c 2302    IF (LINC.GE.0) GO TO 2310
c         IF (F(L+1+I)) 2306,2308,2304
cc.     ORDER WAS INCREASED, AND THUS MUST BE DECREASED (KQQ.LT.0)
c 2304    KQQ=KQQ+1
c         KORD(I+3) = KQQ
c         GO TO 2308
cc.     ORDER WAS DECREASED
c 2306    KQQ=KQQ-1
c 2308    KQQ=max(2,-KQQ)
c         GO TO 2350
c++  End
c     EQUATION IS NOT STIFF
 2310    if (KQQ .gt. 2) then
            if (F(L + KQQ) .eq. C0) then
c                 ORDER WAS INCREASED, AND THUS MUST BE DECREASED
               KQQ = KQQ - 1
               KORD(I + 3) = KQQ
            end if
         end if
         J = min(KQQ, KSC)
         KQMAXI = max(KQMAXI, KQQ)
         if (KQQ .ne. 1) F(L + KQQ + 1) = 0.
c           BACK UP FOR BACKWARD DIFFERENCES
         do 2360 K = 1, J
            F(L + K) = F(L + K) - F(L + K + 1)
 2360    continue
         if (KQQ .gt. KSC) then
c           BACK UP FOR MODIFIED DIVIDED DIFFERENCES
            do 2390 K = J+1, KQQ
               F(L + K) = (F(L+K) - F(L+K+1)) / BETA(K)
 2390       continue
         end if
 2400    F(L + KQQ + 1) = F(L + KQQ + 1) / BETA(KQQ + 1)
         L = L + NUMDT
 2410 continue
c END OF LOOP TO BACK UP DIFFERENCE TABLES
c ********
c BACK UP XI TO BEGINNING OF THE STEP
c ********
      I = KSC + 1
      if (I - IOP11 - 1) 2420, 2440, 2450
 2420 TPD = XI(1)
c                Check below needed when starting?
      if (TPD .eq. XI(2)) go to 2450
      do 2430 K = I, IOP11
 2430    XI(K - 1) = XI(K) - TPD
 2440 XI(IOP11) = C2 * XI(IOP11 - 1)
      if (IOP11 .ne. 2) XI(IOP11) = XI(IOP11) - XI(IOP11 - 2)
 2450 KQICON = -1
      ICF = NE
      ICS = 1
      LDT = 1
      return
      end
c   End of DIVABU

      subroutine DIVACO(ID, RD)
c>> 1987-12-07 DIVACO Krogh   Initial code.
c
c THIS SUBROUTINE RETURNS THE FOLLOWING DATA FROM COMMON
c ID(1) = KEMAX  =  INDEX OF EQUATION WITH LARGEST ERROR ESTIMATE
c ID(2) = KSTEP  =  CURRENT STEP NUMBER
c ID(3) = NUMDT  =  NUMBER OF DIFFERENCES USED FOR EACH EQUATION
c ID(4) =           RESERVED FOR FUTURE USE
c ID(5) =           RESERVED FOR FUTURE USE
c RD(1) = EMAX   =  MAX. RATIO OF ESTIMATED ERROR TO REQUESTED ERROR
c RD(2) =           RESERVED FOR FUTURE USE
c RD(3) =           RESERVED FOR FUTURE USE
c
      integer ID(5)
      double precision RD(3)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c
      ID(1) = KEMAX
      ID(2) = KSTEP
      ID(3) = NUMDT
      RD(1) = EMAX
      return
      end
c   End of DIVACO

      subroutine DIVACR(Y, F, KORD, TOL, LGROUP)
c>> 1988-08-25 DIVACR Krogh   Fix bug in relative error test.
c>> 1988-01-15 DIVACR Krogh   Initial code.
c
c THIS SUBROUTINE
c   1. CORRECTS Y FOR EQUATIONS WHICH ARE NOT STIFF
c   2. ESTIMATES ERRORS
c   3. SELECTS INTEGRATION ORDERS
c   4. TESTS IF NOISE LIMITS THE PRECISION
c
c     Y = VECTOR OF PREDICTED VALUES ON ENTRY, AND OF CORRECTED
c         VALUES WHEN THE RETURN IS MADE.
c LGROUP= VECTOR INDICATING HOW ERROR TOLERANCES ARE TO BE GROUPED
c         (AND POSSIBLY HOW INTEGRATION ORDERS ARE TO BE GROUPED).
c   TOL = VECTOR CONTAINING ERROR TOLERANCES (AND POSSIBLY RELATIVE
c         ERROR FACTORS).
c     F = VECTOR GIVING PREDICTED DERIVATIVE VALUES AND DIFF. TABLES.
c    KD = VECTOR GIVING ORDERS OF THE DIFFERENTIAL EQUATIONS
c         (IF EQUATIONS HAVE DIFFERENT ORDERS).
c    KQ = VECTOR OF INTEGRATION ORDERS.
c
      integer LGROUP(*), KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision Y(*)
      double precision TOL(*), F(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2, EEPT75, EOVEP2
      double precision OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
c
      integer L, I, KQL, KQN, KQD, JLGREP, J, K, ILGROR, ITOLOR, JLGROR,
     1   IORD, KQLORD, LL, LKQMAX
      double precision CM8, CM2, CMP5, C0, CQ3125, CP1, CP125, CP25, CP5
      double precision CP75, CP8, CP9375, C1, C1P4, C2, C4, C10, C20
      double precision C1000, C40
      parameter (CM8 = -8.D0)
      parameter (CM2 = -2.D0)
      parameter (CMP5 = -.5D0)
      parameter (C0 = 0.D0)
      parameter (CQ3125 = .03125D0)
      parameter (CP1 = .1D0)
      parameter (CP125 = .125D0)
      parameter (CP25 = .25D0)
      parameter (CP5 = .5D0)
      parameter (CP75 = .75D0)
      parameter (CP8 = .8D0)
      parameter (CP9375 = .9375D0)
      parameter (C1 = 1.D0)
      parameter (C1P4 = 1.4D0)
      parameter (C2 = 2.D0)
      parameter (C4 = 4.D0)
      parameter (C10 = 10.D0)
      parameter (C20 = 20.D0)
      parameter (C40 = 40.D0)
      parameter (C1000 = 1000.D0)
      double precision TPP, HH, E, EI, EPS, ERCOEF, RND, RNOISE, S
      double precision TP2, TPS1, TPS2, TPS3, TPS4, TPS5, TPS6, TPS7
      double precision REF(4)
      double precision EIBND(KDIM-1)
c++  Code for INTEGO is active
      double precision TEMPA(4), TEMPAO(4)
c++  End
      save LKQMAX
      equivalence (TPS1,TEMPA(1)), (TPS2,TEMPA(2)), (TPS3,TEMPA(3)),
     1   (TPS4, TEMPA(4))
      equivalence (G(1, 1), HH)
      integer MACT1(2), MACT2(12)
c             Parameters for Interface to MESS and DMESS
      integer MERET, METEXT, METABL
      parameter (MERET  =51)
      parameter (METEXT =53)
      parameter (METABL =55)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA KSTEP=$(I6) T=$(E15.8) H=$(E12.5) LSC=$(I3) $C
c   EIMIN=$(E8.2) EAVE=$G KSC=$(I2) SIGMA($J)=$G $C
c   RQ=$(E11.5)$G$E
c   $
cAB I$HKQ$HLI$HE$HEI$HEPS$HF$H$H$H$H
c   HIGH ORDER PREDICTED DIFFERENCES$HRNOISE$HSTIFF$HBETA$E
      integer LTXTAA,LTXTAB
      parameter (LTXTAA=  1,LTXTAB=  1)
      character MTXTAA(1) * (104)
      character MTXTAB(1) * (88)
      data MTXTAA/'KSTEP=$(I6) T=$(E15.8) H=$(E12.5) LSC=$(I3) EIMIN=$(E
     *8.2) EAVE=$G KSC=$(I2) SIGMA($J)=$G RQ=$(E11.5)$G$E'/
      data MTXTAB/'I$HKQ$HLI$HE$HEI$HEPS$HF$H$H$H$HHIGH ORDER PREDICTED$
     * DIFFERENCES$HRNOISE$HSTIFF$HBETA$E'/
c
      data MACT1 / METEXT, MERET /
c (rr=repeat, t=3/5 for I/E format)  wwddtrr  wwddtrr  wwddtrr
      data MACT2 / METABL, 1, 0, 14, 0400201, 0300202, 0801503,
     1    1507501, 1103504, 0901501, 1002501, 1205501 /
c         wwddtrr  wwddtrr  wwddtrr  wwddtrr  wwddtrr
c          End of stuff for interface to message processor
c
      data REF(1), REF(2), REF(3), REF(4) / C1, CP9375, CP75, CP5 /
      data EIBND(1) / .1D0 / EIBND(2) / .1D0 /, EIBND(3) / .14D0 /
c++ Of next 17 lines, only the first KDIM-4 are active
      data EIBND(4) / .19D0 /
      data EIBND(5) / .26D0 /
      data EIBND(6) / .36D0 /
      data EIBND(7) / .50D0 /
      data EIBND(8) / .69D0 /
      data EIBND(9) / .94D0 /
      data EIBND(10) / C1 /
      data EIBND(11) / C1 /
      data EIBND(12) / C1 /
      data EIBND(13) / C1 /
      data EIBND(14) / C1 /
      data EIBND(15) / C1 /
      data EIBND(16) / C1 /
      data EIBND(17) / C1 /
      data EIBND(18) / C1 /
      data EIBND(19) / C1 /
C     data EIBND(20) / C1 /
c
c++  Code for ARGM is inactive
c      RETURN
c      ENTRY DIVACE
c++  End
c ********
c START OF CODE
c ********
      L = NDTF - 1
      if (ICS .ne. 1) L = L + (ICS - 1) * NUMDT
      do 3340 I = ICS, ICF
         if (NKDKO .ne. 0) KORDI = KORD(NKDKO + I - 1)
         IY = IY + abs(KORDI)
         KQL = KORD(I + 3)
         KQN = abs(KQL)
         KQD = max(2, KQN)
c ********
c OBTAIN ERROR TOLERANCE SPECIFIED BY THE USER
c ********
         if (I .le. ILGREP) if (KQL) 2600, 3310, 2610
         ITOLEP = abs(ITOLEP) + 1
         EPS = TOL(ITOLEP)
         ILGREP = LGROUP(ITOLEP)
c   TEST IF SIMPLE ABSOLUTE ERROR TEST IS BEING USED
         if (ILGREP .gt. 0) go to 2580
         JLGREP = ILGREP
c     GET OLD RELATIVE ERROR FACTOR
         TPS6 = TOL(ITOLEP + 1)
         ILGREP = LGROUP(ITOLEP + 1)
         ITOLEP = -ITOLEP - 1
c
         if (JLGREP + 1) 2540, 2570, 2510
c   NO CHECK ON THE ERROR ESTIMATE IS TO BE MADE
 2510    if (EPS + C1) 2520, 2590, 2520
c   ERROR TOLERANCE IS SPECIFIED IMPROPERLY
 2520    KEMAX = I
         NEPTOL = ITOLEP
         LINC = -3
         EREP = -abs(EREP)
         return
c   COMPUTE NEW RELATIVE ERROR FACTOR
 2540    continue
         TPS1 = C0
         do 2550 J = I, ILGREP
            TPS1 = TPS1 + abs(F(J))
 2550       continue
         TPS1 = abs(HH) * TPS1 / dble(ILGREP - I + 1)
         if (LSC .le. 2) go to 2560
c     ON FIRST 3 STEPS INCREASE TPS6 WHEN COMPUTING REL. ERROR FACTOR
         TPS6 = max(C4 * TPS1, TPS6)
c     ON 1-ST TIME THROUGH THE FIRST STEP, REL. ERR. FAC. IS NOT STORED
         if (LSC .eq. 7) go to 2570
 2560    continue
         TPS6 = max(TPS1, TPS6)
c   STORE NEW RELATIVE ERROR FACTOR
         TOL(-ITOLEP) = TPS6 * REF(-JLGREP - 1)
c   COMPUTE ABSOLUTE ERROR TOLERANCE
 2570    EPS = EPS * TPS6
 2580    if (EPS .le. C0) go to 2520
 2590    if (KQL) 2600, 3330, 2610
c END OF OBTAINING ERROR TOLERANCE
c ********
c OBTAIN INFORMATION USED FOR ERROR ESTIMATION, ORDER SELECTION, ETC.
c ********
c EQUATION IS STIFF
 2600    continue
c++  Code for STIFF is inactive
c      JS=abs(KORD(NJSKO+I-1))-1
c      JSI=JS
c      TPP=C0
c      TPS4=F(L+KQD+2)
c      TPS3=F(L+KQD+1)
c      TPS2=F(L+KQD)
c      TPS1=F(L+KQD-1)
c      IF (KQD.EQ.2) TPS1=Y(IY-1)
c      E=ABS(TPS3)+ABS(TPS4)
c      EI=E+ABS(TPS2)
c      RND=EI
c      IF (KORDI.GE.0) GO TO 2604
cc.    EQUATION IS IMPLICIT
c      JSI=JSI-1
c      IF (JSI.NE.0) GO TO 2604
c      IF (KORDI.EQ.-1) GO TO 2602
c      ERCOEF=GS(KQN+1)
c      GO TO 2606
c 2602 ERCOEF=.5D0*DS(KQD,1)
c      JSI=1
c      GO TO 2606
cc.    END OF SPECIAL CODE FOR IMPLICIT EQUATIONS
c 2604 ERCOEF = DS(KQD,JSI)
c 2606 ERCOEF = ABS(ERCOEF) / EPS
c      IF (LSC.LE.2)  GO TO 2710
c      IF (LSC-5) 2650,2710,2710
cc.  END OF CODE FOR STIFF EQUATIONS
c++  End
c
c EQUATION IS NOT STIFF
 2610    TPP = F(I) - F(L + 1)
         TPS3 = TPP
         TPS4 = TPP - F(L + KQD + 1)
         TPS2 = TPP + F(L + KQD)
         TPS1 = TPP + F(L + KQD - 1)
         E = abs(TPS3) + abs(TPS4)
         RND = E
         EI = E + abs(TPS2)
         ERCOEF = abs(GS(KQN + 1)) / EPS
         if (KQL .ge. 4) go to 2710
c   TEST IF STARTING OR IF INTEGRATION ORDER IS ONE
         if (LSC .le. 2) if (KQL - 2) 2660, 2710, 2710
c ********
c LOGIC ASSOCIATED WITH STARTING THE INTEGRATION
c ********
         TPS4 = C0
         if (LSC - 4) 2650, 2640, 2620
c FIRST STEP
 2620    E = E * CQ3125
         TPS3 = C0
c   TEST IF FIRST TIME THROUGH THE FIRST STEP
         if (LSC .eq. 7) go to 2690
c   COMPUTE S=ESTIMATE OF H * EIGENVALUE OF JACOBIAN = 2*(F(A)-F(B))/
c   (F(B)-F(C)) WHERE F(A)=CURRENT F(I), AND F(B) AND F(C) PRECEDING
c   VALUES OR ESTIMATES OF F(I)
         TPP = F(I) - F(L + 5)
         TPS4 = TPP
         E = C2 * abs(TPS4)
         S = F(L + 4)
         F(L + 4) = C0
         if (S .ne. C0) S = (TPS4 + TPS4) / S
         if (S + CP125) 2630, 2700, 2700
c     SET LDT=-5  TO INDICATE POSSIBLE PROBLEMS DUE TO INSTABILITY
 2630    LDT = -5
         go to 2690
c   ADJUST CORRECTION MADE ON SECOND STEP
 2640    TPP = CP8 * TPP
c   ADJUST ESTIMATED ERRORS ON SECOND AND THIRD STEPS
 2650    E = abs(TPS3)
         RND = C4 * E
         go to 2710
c END OF SPECIAL LOGIC FOR STARTING
c ********
c INTEGRATION ORDER =1 IS TREATED AS A SPECIAL CASE
c ********
 2660    TPP = TPP + F(L + 2)
         if (BETA(2) .ge. C1P4) EI = EI * C1000
c   ESTIMATE NEW VALUE FOR S
         S = F(L + 4)
         if (S .eq. C0) go to 2680
         S = max(CM8, C2 * BETA(2) * (TPS1 - TPS2 - F(L + 5)) / S)
         if (S .ge. CMP5) go to 2670
c   MODIFY TPP (TO GET BETTER STABILITY CHARACTERISTICS)
         TPP = TPP * max(CP25, (CM2 - C2 * S) / (S * S))
 2670    TPS4 = TPS4 * abs(S)
 2680    E = CP25 * (E + abs(TPS4))
         EI = EI + abs(TPS4 * S)
c     STORE INFORMATION REQUIRED TO ESTIMATE S ON NEXT STEP
 2690    F(L + 4) = TPP
 2700    F(L + 5) = F(I)
c END OF SPECIAL CODE FOR INTEGRATION ORDER =1
c ********
c CODE FOR NOISE TEST AND GETTING ERROR ESTIMATE
c ********
 2710    E = E * ERCOEF
         TPS5 = abs(F(L + 2)) + abs(F(I))
         if (TPS5 .ne. C0) go to 2720
         RNOISE = C0
         go to 2760
 2720    RNOISE = RND / TPS5
         if (RNOISE .gt. RBQ(KQD)) if (RNOISE - C1) 2760, 2750, 2750
c   NOISE IS APPARENTLY SLOWING CONVERGENCE OF THE DIFFERENCES
c     REDUCE EI
         EI = RND
         TPS5 = abs(EEPS2 * Y(IY - 1)) / EPS
         if (TPS5 .lt. abs(E)) if (LSC) 2730, 2730, 2760
         E = TPS5
         RNOISE = C0
 2730    E = -abs(E)
         if (EIMIN .gt. CP1) EI = (C10 * EIMIN) * EI
c     COMPUTE REDUCTION TO BE MADE IN EI
         if (RNOISE .gt. (C20 * RBQ(KQD))) go to 2760
         K = -6 - LSC
 2740    if (K .le. 0) go to 2760
c     REDUCE EI WHEN NOISE APPARENTLY LIMITS PRECISION
         K = K - 1
         EI = CP5 * EI
         if (EI .gt. EIMIN) go to 2740
         go to 2760
 2750    TPS4 = 1.1 * RND
         TPS3 = RND
 2760    continue
c
c   TEST FOR STIFFNESS GOES HERE WHEN IMPLEMENTED
c *       INGREDIENTS OF TEST MAY INCLUDE --
c *       RNOISE, WHETHER (ABS(TPS4).GT.ABS(TPS3)),
c *       WHETHER EMAX IS INCREASING, RESULT OF TEST ON
c *       PREVIOUS STEPS, ETC.
c
c ********
c COMPUTE ERROR ESTIMATES AND INFORMATION FOR SELECTING THE STEPSIZE
c ********
         if (E .ge. abs(EMAX)) go to 2770
         if (E .ge. C0) go to 2780
         if (EPS .lt. C0) go to 2810
         if (abs(E) .le. abs(EMAX)) go to 2780
         SNOISE = RNOISE
         DNOISE = RND
         NOISEQ = KQD
c   STORE PARAMETERS ASSOCIATED WITH LARGEST VALUE OF E
 2770    EMAX = E
         KEMAX = I
         NEPTOL = ITOLEP
c   DETERMINE HOW MUCH STEPSIZE CAN BE INCREASED
 2780    EI = EI * ERCOEF * SIGMA(KQD)
         EIMAX = max(EIMAX, EI)
         if (LINC .le. 0) go to 2810
         K = 0
 2790    if (EI .ge. min(EIMIN, EIBND(KQN))) go to 2800
         K = K + 1
         if (K .eq. LINC) go to 2810
         EI = EI * SIGMA(KQD)
         go to 2790
 2800    LINC = K
c END OF COMPUTING ERROR ESTIMATES
 2810    continue
c++  Code for ERRSTO is inactive
c      IF (IOP20 .EQ. 0) GO TO 780
cc.********
cc.STORE ERROR ESTIMATE (OPTIONAL)
cc.********
c      F(IOP20+I-1)=TPS3*GS(KQN+1)
cc.END OF STORING ERROR ESTIMATE
c++  Code for INTEGO | ERRSTO is active
         if (IOP19 .eq. 0) go to 3090
c.********
c.EQUATIONS ARE GROUPED TO USE SAME INTEGRATION METHOD (OPTIONAL)
c.********
c++  Code for INTEGO is active
         if (I .gt. 1) if (I - ILGROR) 2900, 2900, 2830
         ITOLOR = IOP19
 2830    JLGROR = KORD(ITOLOR)
         ITOLOR = ITOLOR + 1
         if (JLGROR .gt. 0) go to 2870
         ILGROR = KORD(ITOLOR)
         ITOLOR = ITOLOR + 1
         if (JLGROR + 1) 2840, 2850, 2890
 2840    if (JLGROR .lt. -2) if (KQD + JLGROR) 2850, 2880, 2880
c.INITIALIZE FOR ACCUMULATING VARIABLES USED IN ORDER SELECTION
 2850    IORD = I
         KQLORD = KQL
         do 2860 K = 1, 4
 2860       TEMPAO(K) = abs(TEMPA(K))
         go to 2930
c.ORDERS IN CURRENT GROUP CAN BE DIFFERENT
 2870    ILGROR = JLGROR
         go to 3090
c.ORDER IS NOT GOING TO BE CHANGED
 2880    JLGROR = 0
 2890    if (KQL) 3240, 3270, 3270
c.TAKE ACTION FOR EQUATION WHICH IS NOT THE FIRST IN THE GROUP
 2900    if (JLGROR) 2910, 2890, 3090
c.ACCUMULATE VARIABLES USED IN ORDER SELECTION
 2910    do 2920 K = 1, 4
 2920       TEMPAO(K) = TEMPAO(K) + abs(TEMPA(K))
c.    TEST IF THIS IS LAST EQUATION IN THE GROUP
 2930    if (I .ne. ILGROR) if (KQL) 3310, 3290, 3290
c.SET UP TO GO SELECT INTEGRATION ORDER
         KQL = 0
         do 2940 K = 1, 4
 2940       TEMPA(K) = TEMPAO(K)
         go to 3090
c.INTEGRATION ORDER HAS BEEN SELECTED
c++  Code for INTEGO | STIFF is active
 2950    continue
c++  Code for INTEGO is active
         KQL = KQLORD
         if (KQN - abs(KQL)) 2960, 2980, 3020
c.  TEST IF ORDER CAN BE DECREASED
 2960    if (JLGROR .ge. -2) if (KQL) 3010, 3040, 3040
c.    INTEGRATION ORDER WAS SELECTED OUTSIDE PERMITTED RANGE
 2970    KQN = abs(KQL)
c.    INTEGRATION ORDER IS NOT GOING TO BE CHANGED
 2980    if ((KQL .ne. 1) .or. (LSC .gt. 0)) if (KQL) 3030, 3040, 3040
c.    SET  4-TH ENTRY IN DIFFERENCE TABLES SO THAT STANDARD ADAMS
c.    METHOD IS USED WHEN KQL=1
 2990    do 3000 K = IORD, I
 3000       F(NDTF + K*NUMDT - NUMDT + 3) = C0
         go to 3270
c.  ORDER FOR STIFF EQUATION WAS REDUCED
 3010    continue
c++  Code for INTEGO & STIFF is inactive
c      IF (KQN.LT.JSI) GO TO 990
c      TPP=-C1
c      GO TO 1090
cc.  TEST IF ORDER CAN BE INCREASED
c++  Code for INTEGO is active
 3020    if (JLGROR .eq. -2) go to 2970
c++  Code for INTEGO & STIFF is inactive
c      IF (KQL.GE.0) GO TO 1140
c      IF ((JSI.NE.0).AND.(KQN.GT.(MAXKQD+JSI))) GO TO 990
c      TPP=C1
cc.  STORE RESULTS FOR STIFF EQUATIONS
c++  Code for INTEGO is active
 3030    continue
c++  Code for INTEGO & STIFF is inactive
c      DO 3035 K=IORD,I
c      KORD(K+3) = -KQN
c 3035 F(NDTF+K*NUMDT-NUMDT)=TPP
c      GO TO 3245
cc.  STORE RESULTS FOR EQUATIONS WHICH ARE NOT STIFF
c++  Code for INTEGO is active
 3040    LL = NDTF + NUMDT * IORD - NUMDT
         do 3080 J = IORD, I
            KORD(J + 3) = KQN
            if (KQN - KQL) 3050, 3070, 3060
 3050       F(LL + KQD - 1) = F(LL + KQD - 1) + (F(J) - F(LL))
            go to 3080
 3060       F(LL + KQN) = F(LL + KQD)
 3070       F(LL + KQD) = C0
 3080       LL = LL + NUMDT
         if (KQN - 1) 3270, 2990, 3270
c++  End
c.********
c.SELECT INTEGRATION ORDER
c.********
 3090    if (LSC .le. 0) go to 3120
c. SPECIAL ORDER SELECTION WHEN STARTING
         if (LSC - 3) 3110, 3210, 3100
 3100    if (LSC .eq. 5) if (S + .125D0) 3160, 3130, 3130
         if (LSC - 6) 3130, 3130, 3210
 3110    if (C40 * min(abs(TPS4), abs(TPS3)) .gt. abs(TPS2)) then
            if (EPS .ne. -C1) LSC = 2
         end if
         if (abs(TPS4) .lt. abs(TPS3)) if (C4 * abs(TPS4) - abs(TPS2))
     1      3130, 3130, 3210
c.  CHECK IF ORDER CAN BE INCREASED OR SHOULD BE DECREASED
 3120    TPS5 = ROBND * abs(TPS4)
         TPS6 = ROBND * (TPS5 + abs(TPS3))
         TPS7 = abs(TPS1) + abs(TPS2)
         if (TPS5 .ge. abs(TPS3)) go to 3140
         if (TPS6 .ge. TPS7) go to 3210
 3130    if (KQN .ge. MAXKQI) go to 3210
c.    INCREASE THE INTEGRATION ORDER
         KQN = KQN + 1
c++  Code for INTEGO | STIFF is active
         if (KQL) 3230, 2950, 3250
c++  Code for ~(INTEGO | STIFF) is inactive
c      GO TO 3250
c++  End
c.  CHECK IF ORDER SHOULD BE DECREASED
 3140    if (TPS6 .lt. TPS7) go to 3210
         if (TPS5 .lt. abs(TPS3 - TPS4)) go to 3210
         if ((TPS3.eq.TPS4) .and. (LSC.le.0)) go to 3210
         if (KQN - 2) 3210, 3160, 3180
 3160    KQN = 1
c++  Code for INTEGO | STIFF is active
         if (KQL) 3220, 2950, 3170
c++  End
c.    WHEN ORDER IS REDUCED TO 1 WITH ADAMS METHOD SET F(L+4)=0
 3170    F(L + 4) = C0
         go to 3260
c.    DECREASE THE INTEGRATION ORDER
 3180    KQN = KQN - 1
c++  Code for INTEGO | STIFF is active
         if (KQL) 3220, 2950, 3200
c++  End
 3200    F(L+KQD) = F(L+KQD) + TPP
         go to 3260
c   NO CHANGE IN INTEGRATION ORDER IS BEING MADE
 3210    continue
c++  Code for INTEGO is active
         if (KQL) 3240, 2950, 3270
c++  Code for ~INTEGO is inactive
c      GO TO 1530
c++  End
c END OF SELECTING INTEGRATION ORDER
c ********
c COMPUTE MAXIMUM INTEGRATION ORDERS AND SET NEW ONES (IF ANY)
c ********
c EQUATION IS STIFF
c     ORDER WAS DECREASED
c++  Code for INTEGO | STIFF is active
 3220    continue
c++  Code for STIFF is inactive
c      IF (KQN.LT.JSI) GO TO 3236
c      F(L+1)=-C1
c      GO TO 3233
cc.    ORDER WAS INCREASED
c++  Code for INTEGO |  STIFF  is active
 3230    continue
c++  Code for STIFF is inactive
c      IF ((JSI.NE.0).AND.(KQN.GT.(MAXKQD+JSI))) GO TO 3236
c      F(L+1)=C1
c 3233 KORD(I+3) = -KQN
c      GO TO 3245
c      ORDER WAS SET TO AN UNACCEPTABLE VALUE
c 3236 KQN=abs(KQL)
c      ORDER IS NOT BEING CHANGED
c++  Code for STIFF |  INTEGO is active
 3240    continue
c++  Code for STIFF is inactive
c      F(L+1)=C0
c 3245 IF (JSI.NE.0) KQMAXD=max(KQN,KQMAXD)
c      IF (JS.LT.abs(KORDI)) KQMAXI=max(KQN,KQMAXI)
c      GO TO 3290
c++  End
c EQUATION IS NOT STIFF
c     ORDER INCREASED
 3250    F(L + KQN + 1) = -F(L + KQD + 1)
         if (LSC .gt. 0) F(L + KQN + 1) = F(L + 1) - F(I)
c     ORDER CHANGED
 3260    KORD(I + 3) = KQN
 3270    KQMAXI = max(KQN, KQMAXI)
         if (EPS .gt. C0) KQMAXS = max(KQN, KQMAXS)
         F(L + KQD + 1) = C0
 3290    continue
         if (KQN .gt. KIS) go to 3310
c.********
c.DETERMINE IF TIME TO STORE SOLUTION (OPTIONAL)
c.********
         if (KIS .ge. 1000) then
            TP2 = max(1.5D0, dble(KQN) * C2 ** (1001 - KIS)) * abs(TPS4)
 3295       if (TP2 .gt. abs(F(L+KQN))) then
               if (KQN .le. KQL) then
                  KQN = KQN - 1
                  if (KQN .gt. 1) go to 3295
                  KQN = 1
               end if
            end if
            KORD(I+3) = KQN
            if (I .eq. 1) LKQMAX = 0
            LKQMAX = max(KQN, LKQMAX)
            KQMAXI = LKQMAX
            if (KIS .eq. 1000) then
               if (I .eq. KEMAX) EMAX = dble(8 + KQN**2) * abs(EMAX)
               go to 3325
            end if
c++  Code for DUMP is active
         else
            if (E .eq. C0) go to 3310
            if (IOP9 .gt. 0) if ((E*dble(KIS-KQN+2)**(KQN+1)) - 1.D-2)
     1         3310, 3310, 3300
 3300       KIS = -1
c++  End
         end if
 3310    continue
c ********
c CORRECT
c ********
         do 3320 K = 1, KORDI
c++  Code for ~{p,x} is active
            Y(IY - K) = Y(IY - K) + G(KQL + 1, K) * TPP
c++  Code for {p,x} is inactive
Cc--D Next line special: P=>D, X=>Q
C            Y(IY - K) = Y(IY - K) + dble(G(KQL + 1, K)) * dble(TPP)
c++  END
 3320    continue
c END OF CORRECTING
 3325 continue
c++  Code for OUTPUT is active
      if (IOP10 .gt. 0) then
         if (I .eq. 1) then
            IDAT(1) = KSTEP
            IDAT(2) = LSC
            IDAT(3) = KSC
            IDAT(4) = IOP11
            FDAT(1) = TN
            FDAT(2) = HH
            FDAT(3) = EIMIN
            FDAT(4) = EAVE
            FDAT(5) = SIGMA(IOP11)
            FDAT(6) = ROBND
            MACT2(3) = NTE
c--D Next line special: P=>S, X=>D
            call DMESS(MACT1, MTXTAA, IDAT, FDAT)
         end if
         IDAT(1) = I
         IDAT(2) = KQL
         IDAT(3) = LINC
         FDAT(1) = E
         FDAT(2) = EI
         FDAT(3) = EPS
         FDAT(4) = F(I)
         FDAT(5) = TPS1
         FDAT(6) = TPS2
         FDAT(7) = TPS3
         FDAT(8) = TPS4
         FDAT(9) = RNOISE
         FDAT(10) = S
         FDAT(11) = BETA(KQD)
c--D Next line special: P=>S, X=>D
         call DMESS(MACT2, MTXTAB, IDAT, FDAT)
         if (I .eq. NTE) IOP10 = IOP10 - 1
      end if
c++  End
 3330    L = L + NUMDT
 3340    continue
      return
      end
c   End of DIVACR

      subroutine DIVAHC
c>> 1988-05-20 DIVAHC Krogh   Initial code.
c
c SUBROUTINE TO COMPUTE COEFFICIENTS REQUIRED FOR INTEGRATING
c ORDINARY DIFFERENTIAL EQUATIONS
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2, EEPT75, EOVEP2
      double precision OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
c                 K - 1 + 1 / K  is equivalent to max(1, K-1)
      double precision GG(MAXORD - 1 + 1/MAXORD), B(KDIM+MAXORD),
     1   W(KDIM+MAXORD)
      integer GOINT, K, N, J
      double precision C0, CP1, CRBQI, CP5, CP5625, C1, C1P125
      parameter (C0 = 0.D0)
      parameter (CP1 = .1D0)
      parameter (CRBQI = .421875D0)
      parameter (CP5 = .5D0)
      parameter (CP5625 = .5625D0)
      parameter (C1 = 1.D0)
      parameter (C1P125 = 1.125D0)
c++  Code for STIFF is inactive
c      INTEGER          GODIF
c++  End
      double precision TP1, TP2, HH, TEMP, TP
      equivalence (G(1, 1), HH)
c
      save GG, W
c
c  B(K)= 1/(K*(K+1))
c++ Of next 23 lines, only the first KDIM+MAXORD are active
      data B(1)  / 5.000000000000000000000000000000000000000D-1 /
      data B(2)  / 1.666666666666666666666666666666666666667D-1 /
      data B(3)  / 8.333333333333333333333333333333333333333D-2 /
      data B(4)  / 5.000000000000000000000000000000000000000D-2 /
      data B(5)  / 3.333333333333333333333333333333333333333D-2 /
      data B(6)  / 2.380952380952380952380952380952380952381D-2 /
      data B(7)  / 1.785714285714285714285714285714285714286D-2 /
      data B(8)  / 1.388888888888888888888888888888888888889D-2 /
      data B(9)  / 1.111111111111111111111111111111111111111D-2 /
      data B(10) / 9.090909090909090909090909090909090909091D-3 /
      data B(11) / 7.575757575757575757575757575757575757576D-3 /
      data B(12) / 6.410256410256410256410256410256410256410D-3 /
      data B(13) / 5.494505494505494505494505494505494505495D-3 /
      data B(14) / 4.761904761904761904761904761904761904762D-3 /
      data B(15) / 4.166666666666666666666666666666666666667D-3 /
      data B(16) / 3.676470588235294117647058823529411764706D-3 /
      data B(17) / 3.267973856209150326797385620915032679739D-3 /
      data B(18) / 2.923976608187134502923976608187134502924D-3 /
      data B(19) / 2.631578947368421052631578947368421052632D-3 /
      data B(20) / 2.380952380952380952380952380952380952381D-3 /
      data B(21) / 2.164502164502164502164502164502164502165D-3 /
      data B(22) / 1.976284584980237154150197628458498023715D-3 /
C     data B(23) / 1.811594202898550724637681159420289855072D-3 /
c
c ********
c START OF CODE
c ********
c     SET STEP NUMBER OF METHOD
c++  Code for STIFF is inactive
c      IOP11 = MIN(max(KQMAXI,KQMAXD) + 1), KDIM)
c++  Code for ~STIFF is active
      IOP11 = MIN(KQMAXI + 1, KDIM)
c++  End
c     TEST IF STEPSIZE WAS CHANGED
      if (KQICON .ge. 0) go to 3510
c ********
c STEPSIZE JUST CHANGED
c ********
c     SET CONSTANTS DEPENDING ON NEW STEPSIZE
      KQMXIL = KQMAXI
      TP1 = HH
      GG(1) = TP1 * TP1
      G(1, 2) = GG(1) * CP5
      if (MAXINT .le. 2) go to 3450
      do 3440 K = 3, MAXINT
         GG(K - 1) = G(1, K - 1) * TP1
         G(1, K) = GG(K - 1) / dble(K)
 3440    continue
c     SET CONSTANTS INDICATING STEP CHANGE
 3450 KQICON = 0
c++  Code for STIFF is inactive
c      KQDCON=0
c++  End
      KQMXIP = 1
      KSC = 1
      if (LSC .lt. 7) go to 3490
c     SPECIAL SET-UP OF CONSTANTS ON THE VERY FIRST STEP
      HINCC = C1P125
      LINCD = 6
      LINCQ = 12
      if (HINC .gt. C0) go to 3460
      LINCD = -2
      LINC = -2
      ROBND = C1
 3460 SIGMA(1) = 1.0
      BETA(1) = C1
      do 3470 N = 1, IOP11
c++  Code for STIFF is inactive
c      D(1,N)=C0
c++  End
         XI(N) = TP1
         ALPHA(N) = C1
         BETA(N + 1) = C1
         SIGMA(N + 1) = dble(N + 1) * SIGMA(N) * HINCC
 3470    continue
      TEMP = EEPS16
      RBQ(1) = C1
      RBQ(2) = CP1
      TP = CRBQI
c     **** IN THE LOOP BELOW RBQ(K) IS COMPUTED TO BE
c          APPROXIMATELY (3/4 ** ((K-1) ** 2 - 1) / 10
c          .5625 = (3/4) ** 2    TP = (3/4) ** (2*K -3)
      do 3480 K = 3, KDIM
         TEMP = TEMP + TEMP
         RBQ(K) = max(TEMP, RBQ(K - 1) * TP)
 3480    TP = TP * CP5625
      go to 3560
c     SET-UP AFTER THE FIRST STEP
 3490 TP2 = XI(1)
      XI(1) = TP1
      BETA(2) = TP1 / TP2
      K = 2
      if (HINCC .eq. HINC) go to 3540
      if ((LSC .ne. 0) .or. ((KSTEP-KSSTRT-KQMAXS) .lt. 10)) go to 3540
      HINCC = C1
      LINCD = 0
 3500 LINCD = LINCD + 1
      HINCC = HINCC * HINC
      if (HINCC .lt. 2.D0) go to 3500
      LINC = (LINC * (LINCD + LINCD)) / LINCQ
      LINCQ = LINCD + LINCD
      HINCC = HINC
      go to 3540
c END OF LOGIC FOR CASE WHEN STEPSIZE JUST CHANGED
c     TEST IF MAXIMUM INTEGRATION ORDER DID NOT INCREASE
 3510 if (KQMAXI .gt. KQMXIL) then
c ********
c INTEGRATION ORDER WAS INCREASED -- GET NEW V'S
c ********
         KQMXIL = KQMAXI
         KQMXIP = KQMXIL + MAXINT
         K = KQMXIP
         V(K) = B(K)
         if (KQICON .eq. 1) go to 3530
         if (KQICON .eq. K) KQICON = KQICON - 1
         do 3520 N = 2, KQICON
            K = K - 1
 3520       V(K) = V(K) - ALPHA(N) * V(K + 1)
c END OF GETTING NEW V'S
      else
         IOP11 = max(IOP11, KQMXIL+1)
      end if
 3530 if (IOP11 .le. KSC) go to 3560
c ********
c COMPUTE PARAMETERS WHICH ARE STILL CHANGING AS A RESULT OF
c A CHANGE IN THE STEPSIZE
c ********
      TP2 = XI(KSC)
c     UPDATE CONSTANT STEP COUNTER
      KSC = KSC + 1
      K = KSC
      BETA(K) = C1
 3540 continue
      TEMP = HINCC
c
c   LOOP TO COMPUTE NEW VALUES OF PARAMETERS
      do 3550 N = K, IOP11
         TP1 = TP2 + HH
         TP2 = XI(N)
         XI(N) = TP1
         ALPHA(N) = HH / TP1
         BETA(N + 1) = BETA(N) * (TP1 / TP2)
         TEMP = max(TEMP, dble(N) * (ALPHA(N) * HINCC))
         SIGMA(N) = SIGMA(N - 1) * TEMP
 3550    continue
      if (IOP11 .ne. KDIM) XI(IOP11 + 1) = TP2 + HH
c END OF CODE FOR COMPUTING PARAMETERS WHICH ARE STILL CHANGING
c
 3560 if (KQICON .ge. KQMXIP) go to 3690
c ********
c COMPUTE INTEGRATION COEFFICIENTS WHICH ARE STILL CHANGING
c ********
      KQMXIL = max(KQMAXI, KQMXIL)
      KQMXIP = KQMXIL + MAXINT
      J = KQMXIP - KQICON
      N = KQICON + 1
      KQICON = N
      if (N .ne. 1) go to 3580
c INITIALIZE V AND W
      do 3570 K = 1, J
         V(K) = B(K)
 3570    W(K) = V(K)
      go to 3600
c UPDATE V AND INITIALIZE W
 3580 if (N .eq. KDIM) go to 3690
      do 3590 K = 1, J
         V(K) = V(K) - ALPHA(N) * V(K + 1)
 3590    W(K) = V(K)
c SET TRANSFER FOR LOOP BELOW DEPENDING ON VALUE OF MAXINT
 3600 continue
c++  Code for MAXORD >= 2 is active
      if (MAXINT - 2) 3610, 3620, 3630
 3610 assign 3680 to GOINT
      go to 3680
 3620 assign 3670 to GOINT
      go to 3670
 3630 assign 3660 to GOINT
      go to 3660
c++  Code for MAXORD == 1 is inactive
c      go to 3610
c++  End
c
 3640 J = J - 1
c INNER LOOP FOR COMPUTING INTEGRATION COEFFICIENTS
      do 3650 K = 1, J
 3650    W(K) = W(K) - ALPHA(N) * W(K + 1)
c
c++  Code for MAXORD >= 2 is active
      go to GOINT, (3660, 3670, 3680)
c++  End
c     STORE INTEGRATION COEFFICIENTS
 3660 continue
c++  Code for MAXORD >= 3 is inactive
c     DO 3665 K=3,MAXINT
c3665 G(N+1,K)=GG(K-1)*W(K)
c++  End
 3670 continue
      G(N + 1, 2) = GG(1) * W(2)
c++  Code for MAXORD >= 2 is active
 3680 G(N + 1, 1) = HH * W(1)
c++  End
      GS(N + 1) = G(N + 1, 1) - G(N, 1)
      N = N + 1
      if (N .le. KQMXIL) go to 3640
c END OF COMPUTING INTEGRATION COEFFICIENTS
c
 3690 continue
c++  Code for STIFF is inactive
c      IF (KQDCON.GT.KQMAXD) GO TO 4662
cc.********
cc.COMPUTE DIFFERENTIATION COEFFICIENTS WHICH ARE STILL CHANGING
cc.********
cc.SET TRANSFER FOR LOOP BELOW, DEPENDING ON VALUE OF MAXDIF
c++  Code for STIFF & MAXORD >= 2 is inactive
c      IF (MAXDIF-2) 3693,3692,3691
c 3691 ASSIGN 3696 TO GODIF
c      GO TO 3694
c 3692 ASSIGN 3698 TO GODIF
c      GO TO 3694
c 3693 ASSIGN 3699 TO GODIF
c++  Code for STIFF is inactive
c 3694 KQDCON=KQDCON+1
cc.LOOP FOR COMPUTING DIFFERENTIATION COEFFICIENTS
c      DO 3699 N=KQDCON,KQMAXD
c      DS(N+1,2)=C1/XI(N)
c      D(N+1,1)=DS(N+1,2)+D(N,1)
c      DS(N+1,1)=DS(N+1,2)/D(N+1,1)
c++  Code for STIFF & MAXORD >= 2 is inactive
c      GO TO GODIF, (3696,3698,3699)
c 3696 CONTINUE
c++  Code for STIFF & MAXORD >= 3 is inactive
c      DO 3697 K=3,MAXDIF
c      DS(N+1,K)=D(N,K-2) * (K-1)/XI(N)
c 3697 D(N+1,K-1)=DS(N+1,K) + D(N,K-1)
c++  Code for STIFF is inactive
c 3698 CONTINUE
c++  Code for STIFF & MAXORD >= 2 is inactive
c      D(N+1,MAXDIF)=D(N,MAXDIF) + D(N,MAXDIF-1) * (MAXDIF)/XI(N)
c++  Code for STIFF is inactive
c 3699 CONTINUE
c++  End
c
c END OF COMPUTING DIFFERENTIATION COEFFICIENTS
      return
      end
c   End of DIVAHC

      subroutine DIVAIN(T, Y, F, KORD)
c>> 1988-01-14 DIVAIN Krogh   Initial code.
c
c  SUBROUTINE TO DO INTERPOLATION FOR VARIABLE ORDER INTEG. ROUTINE
c
      integer KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision T(*), Y(*)
      double precision F(*)
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
      save / DIVASC /
      integer I, ICI, IDT, INTERP, INTEG, INTEGZ, IY, IYI, IYN, IYNI, J,
     1    K, KQMXI, KQMXS, KQQ, L, N
      double precision C0, C1, C2
      parameter (C0 = 0.D0)
      parameter (C1 = 1.D0)
      parameter (C2 = 2.D0)
      double precision C(KDIM+MAXORD-1), ETA(KDIM)
      double precision GAMMA(KDIM)
      double precision TP1, HI
      double precision CSUM(KDIM+MAXORD-1)
c--D Next line special: P=>D, X=>Q
      double precision XP1
      logical LNOTM1
c
c              Stuff for processing error messages
      integer IDAT(1)
      double precision FDAT(6)
      integer MENTXT, MERET, MEEMES, METEXT
      parameter (MENTXT =23)
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      integer MACT(8)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVAIN$B
cAB Interpolating at T(1)=$F with $B
cAC TN=$F, T(2)=$F and H=$F.  T(1) must be in [$F, $F].$E
cAD internal variable LDT = $I.  Interpolation not allowed now.$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD
      parameter (LTXTAA=  1,LTXTAB=  9,LTXTAC= 41,LTXTAD= 94)
      character MTXTAA(1) * (154)
      data MTXTAA/'DIVAIN$BInterpolating at T(1)=$F with $BTN=$F, T(2)=$
     *F and H=$F.  T(1) must be in [$F, $F].$Einternal variable LDT = $I
     *.  Interpolation not allowed now.$E'/
c
c                      1 2 3 4       5 6       7      8
      data MACT / MEEMES,0,0,0, MENTXT,0, METEXT, MERET /
c
c++  Code for ARGM is inactive
c      ENTRY DIVAIE
c++  End
c ********
c START OF CODE -- CHECK ON STATE OF DIFFERENCE TABLE
c ********
      L = LDT
      if (L) 3710, 3730, 3780
 3710 if (L + 2) 4170, 3730, 3720
 3720 if (MAXINT .ge. 0) L = 1
      go to 3840
c ********
c UPDATE DIFFERENCE TABLE TO START OF NEXT STEP
c ********
 3730 K = NDTF
      do 3770 I = 1, NTE
         KQQ = KORD(I + 3)
         if (KQQ .le. 0) go to 3760
c EQUATION IS NOT STIFF
         TP1 = F(I) - F(K)
c LOOP TO DO UPDATING
         N = K + max(abs(KQQ), 2)
         do 3750 J = K, N
 3750       F(J) = F(J) + TP1
 3760    continue
         K = K + NUMDT
 3770    continue
      LDT = 1
      if (L .ne. 0) return
c END OF UPDATING DIFFERENCE TABLE
c ********
c INITIALIZE FOR COMPUTATION OF COEFFICIENTS
c ********
 3780 INTERP = 0
      assign 4066 to LGO
      HI = T(1) - TN
      GAMMA(1) = HI / XI(1)
      if (GAMMA(1)) 3790, 3800, 3810
 3790 if (GAMMA(1) .ge. -C1) go to 3820
      INTERP = 1
      if (abs(HI) - abs(T(2))) 3820, 3820, 4180
 3800 INTERP = 2 - KQMAXI
      go to 3820
 3810 if (GAMMA(1) .gt. C2) if (LDT - 2) 4180, 3820, 3820
 3820 KQMXI = KQMAXI + INTERP - 1
c++  Code for STIFF is inactive
c      KQMXS=max(KQMXI,KQMAXD)
c++  Code for ~STIFF is active
      KQMXS = KQMXI
c++  End
      do 3830 N = 2, KQMXS
 3830    GAMMA(N) = (HI + XI(N-1)) / XI(N)
 3840 LNOTM1 = L .ne. -1
      INTEG = MAXINT
      if (INTEG .le. 0) if (INTEG + MAXDIF) 4160, 3950, 3950
c ********
c COMPUTE INTEGRATION COEFFICIENTS
c ********
c     INITIAL SET-UP
c         COMPUTE INITIAL C VALUES
      do 3850 N = 1, INTEG
         C(N) = HI / dble(N)
 3850 continue
      I = INTEG + 1
      INTEG = INTEG + KQMXI
      do 3860 N = I, INTEG
         C(N) = C(N - 1) * (dble(N - MAXINT) / dble(N))
 3860    continue
c         COMPUTE ETA'S
      do 3870 N = 1, KQMXI
 3870    ETA(N) = HI / XI(N)
c         COMPUTE C(K)'S TO CORRESPOND TO G(K-MAXINT+1,MAXINT),
c         K=MAXINT, MAXINT+1,..., MAXINT+KQMXI-1
      I = INTEG
 3880 J = INTEG
      INTEG = J - 1
      if (INTEG .le. MAXINT) go to 3900
      do 3890 N = J, I
 3890    C(N) = ETA(N - INTEG) * C(N) + C(N - 1)
      go to 3880
 3900 do 3910 N = J, I
 3910    C(N) = ETA(N - INTEG) * C(N)
c         END OF COMPUTING  G(---,MAXINT)
      INTEGZ = 0
      go to 3940
c         COMPUTE C(K)-S TO CORRESPOND TO G(K-INTEG+1,INTEG),
c         K=INTEG+1,INTEG+2,..., INTEG+KQMXI
 3920 do 3930 N = 1, KQMXI
 3930    C(INTEG+N) = GAMMA(N)*C(INTEG+N-1) - ETA(N)*C(INTEG+N)
 3940 ICI = INTEG - 1
      go to 4020
c END OF COMPUTING INTEGRATION COEFFICIENTS
c ********
c COMPUTE COEFFICIENTS FOR INTERPOLATION
c ********
 3950 C(1) = C1
      ICI = 0
      do 3960 N = 1, KQMXS
 3960    C(N + 1) = GAMMA(N) * C(N)
      if (INTEG + 1) 3970, 3990, 4010
c END OF COMPUTING INTERPOLATION COEFFICIENTS
c
c     SET-UP TO COMPUTE DIFFERENTIATION COEFFICIENTS REQUIRED
c     IN ORDER TO GET COEFFICIENTS ACTUALLY USED
 3970 INTEG = 0
      ICI = 1
 3980 INTEG = INTEG - 1
      if (INTEG .eq. MAXINT) ICI = 0
c ********
c COMPUTE DIFFERENTIATION COEFFICIENTS
c ********
 3990 INTERP = max(INTERP, 0)
      TP1 = dble(-INTEG)
      C(1) = TP1 * C(1) / XI(-INTEG)
      J = KQMAXD + INTEG
      do 4000 N = 1, J
 4000    C(N + 1) = (TP1*C(N)) / XI(N - INTEG) + GAMMA(N - INTEG) * C(N)
c     C(N) NOW CORRESPONDS TO THE DIFFERENTIAL COEFFICIENT
c          D(N-INTEG,-INTEG)
 4010 INTEGZ = INTEG
      if (ICI .ne. 0) go to 3980
c END OF COMPUTING DIFFERENTIATION COEFFICIENTS
c ********
c BEGINNING OF LOOP TO DO
c         INTEGRATION       (INTEG.GT.0)
c         INTERPOLATION     (INTEG.EQ.0)
c         DIFFERENTIATION   (INTEG.LT.0)
c TO THE POINT INDICATED BY T.
c ********
c     SET UP INITIAL INDICES
 4020 if (NYNY .lt. 0) then
         IY = -NYNY
         IYNI = NYNY + ICI + 1
         if (LDT .eq. 2) then
            assign 4063 to LGO
            CSUM(ICI+1) = C(ICI+1)
            do 4025 J = ICI+2, INTEG+KQMXI
            CSUM(J) = CSUM(J-1) + C(J)
 4025       continue
         end if
      else
         IY = 1
         IYNI = NYNY + ICI - 1
      end if
      IDT = NDTF - INTEGZ
      do 4140 I = 1, NTE
         if (NKDKO .ne. 0) KORDI = KORD(NKDKO + I - 1)
         IY = IY + abs(KORDI)
         KQQ = KORD(I + 3)
c         GET INDEX OF HIGHEST ORDER DIFFERENCE TO BE USED
         K = max(abs(KQQ) + INTERP, 2)
         IYI = -INTEG
         if (KQQ) 4030, 4130, 4040
c EQUATION IS STIFF
 4030    continue
c++  Code for STIFF is inactive
c      JS=abs(KORD(NJSKO+I-1))-1
c      IYI=IYI-JS
c      IF(LNOTM1) IF (IYI) 4034,4032,4130
c      IF (KORDI.LT.0) IYI=IYI+1
c      IYI=IYI+MAXINT-abs(KORDI)
c      IF (IYI) 4034,4130,4130
cc.      IF EQUATION IS IMPLICIT DO NOT COMPUTE AN F
c 4032 IF (KORDI.LT.0) GO TO 4130
cc.      TEST IF INTEG TOO BIG FOR THIS EQUATION
c 4034 IF (abs(KORDI).LT.-IYI) GO TO 4130
c      IYI=IYI+IY
c      IYN=IYI+IYNI
cc. COMPUTE INNER PRODUCT FOR STIFF EQUATIONS
c      IF (INTEGZ.EQ.0) GO TO ???
cc.    DIFFERENTIATING
c      TP1 = C0
c      DO 4036 J = K+INTEGZ, 1, -1
c         TP1 = TP1 + C(J) * F(IDT+J-1)
c 4036 CONTINUE
cc.    TEST WHETHER TO STORE RESULT IN Y OR F
c      IF (IYI-IY) 4080, 4090, 4080
cc.    INTEGRATING OR INTERPOLATING
c      TP1 = C0
c      DO 4037 J = ICI + K, ICI + 2, -1
c         TP1 = TP1 + C(J) * F(IDT+J-ICI-1)
c 4037 CONTINUE
c      IF (INTEG.EQ.0) GO TO 4120
c      TP1=TP1 + C(ICI+1)*Y(IYN+1)
c++  End
         go to 4100
c END OF SPECIAL CODE FOR STIFF EQUATIONS
c
c EQUATION IS NOT STIFF
 4040    if (LNOTM1) if (IYI) 4050, 4060, 4130
         IYI = IYI + MAXINT - KORDI
         if (IYI .ge. 0) go to 4130
c       TEST IF INTEG TOO BIG FOR THIS EQUATION
 4050    if (KORDI .lt. -IYI) go to 4130
 4060    IYI = IYI + IY
         IYN = IYI + IYNI
c  COMPUTE INNER PRODUCT FOR EQUATION WHICH IS NOT STIFF
         XP1 = C0
         go to LGO, (4063, 4066)
 4063    if (KQQ .ne. KQMAXI) XP1 = CSUM(K+INTEGZ+ICI) *
     1      F(IDT+INTEGZ+NUMDT-1)
 4066    do 4070 J = K + INTEGZ + ICI, ICI + 1, -1
            XP1 = XP1 + C(J) * F(IDT - ICI - 1 + J)
 4070       continue
         if (INTEG) 4080, 4090, 4100
c STORE FINAL RESULT IN Y WHEN DIFFERENTIATING
 4080    continue
         Y(IYI) = XP1
         go to 4130
c STORE INTERPOLATED VALUE IN F (OR STIFF DIFFERENTIATION)
 4090    F(I) = XP1
         go to 4130
c PICK UP EXTRA STUFF TO ADD TO INNER PRODUCT WHEN INTEGRATING
 4100    K = ICI
         if (K .eq. 0) go to 4120
 4110    continue
         XP1 = C(K) * (XP1 + Y(IYN))
         IYN = IYN - 1
         K = K - 1
         if (K .ne. 0) go to 4110
c STORE FINAL RESULT IN Y WHEN INTEGRATING (OR STIFF INTERPOLATION)
 4120    Y(IYI) = XP1 + Y(IYN)
 4130    continue
         IDT = IDT + NUMDT
 4140    continue
c
      INTEG = INTEG - 1
      if (INTEG .ge. -MAXDIF) if (INTEG) 3990, 3950, 3920
 4160 return
c ********
c ERROR PROCESSING
c ********
 4170 MACT(2) = 68
      MACT(3) = 11
      MACT(6) = LTXTAD
      IDAT(1) = LDT
      go to 4190
 4180 MACT(2) = 28
      MACT(3) = 1
      MACT(6) = LTXTAC
      FDAT(2) = TN
      FDAT(3) = T(2)
      FDAT(4) = XI(1)
      FDAT(5) = TN - T(2)
      FDAT(6) = TN + C2 * XI(1)
      if (XI(1) .lt. 0) then
         FDAT(5) = FDAT(6)
         FDAT(6) = TN - T(2)
      end if
 4190 FDAT(1) = T(1)
c--D Next line special: P=>S, X=>D
      call DMESS(MACT, MTXTAA, IDAT, FDAT)
      if (MACT(2) .lt. 50) go to 3820
      return
      end
c   End of DIVAIN

      subroutine DIVAOP(IOPT, FOPT)
c>> 1987-12-07 DIVAOP Krogh   Initial code.
c
c  SUBROUTINE TO SET UP OPTIONS FOR DIFFERENTIAL EQUATION  PACKAGE -IVA
      double precision FOPT(*)
      integer IOPT(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2, EEPT75, EOVEP2
      double precision OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
c
      integer IOPTS(22), INCOP(22), IOPTC(22), I, IA, J, K, LIOPT
      double precision CMP75, C0, CP25, CP3, CP5, CP625, CP75, CP875,
     1   CP9, C1, C1P125, C2, C4, C10, C16
      parameter (CMP75 = (-.75D0))
      parameter (C0 = 0.D0)
      parameter (CP25 = .25D0)
      parameter (CP3 = .3D0)
      parameter (CP5 = .5D0)
      parameter (CP625 = .625D0)
      parameter (CP75 = .75D0)
      parameter (CP875 = .875D0)
      parameter (CP9 = .9D0)
      parameter (C1 = 1.D0)
      parameter (C1P125 = 1.125D0)
      parameter (C2 = 2.D0)
      parameter (C4 = 4.D0)
      parameter (C10 = 10.D0)
      parameter (C16 = 16.D0)
      double precision D1MACH
      equivalence (IOPTC(3), IOP3)
      save IOPTS, LIOPT
c
c                      Declarations for error message processing.
c
      integer MERET, MEEMES, METEXT, MEIVEC
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
      parameter (MEIVEC =57)
      integer MACT(7)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVAOP$B
cAB Error in IOPT() specifications: IOPT =$E
      integer LTXTAA,LTXTAB
      parameter (LTXTAA= 1,LTXTAB= 9)
      character MTXTAA(1) * (48)
      data MTXTAA/'DIVAOP$BError in IOPT() specifications: IOPT =$E'/
c                      1   2  3   4       5  6      7
      data MACT / MEEMES, 88, 24, 0, MEIVEC, 0, MERET /
c
      data IOPTS / 3*0, 500000, 12*0, 1, 5*0 /
c
c                  1  2    3  8  9 10   11   13 16   17 21 22
      data INCOP / 1, 3, 5*2, 1, 2, 3, 2*2, 3*1, 3, 4*2, 3, 2 /
c
c ********* START OF EXECUTABLE CODE ***********************
c
      K = 1
 4200 I = IOPT(K)
      IA = abs(I)
c 1 and 6 lines below need 20 changed if more options are added.
      if (IA .le. 20) if (I) 4220, 4520, 4280
      if (I .ne. 1111) go to 4490
      IOPT(2) = LIOPT
c
c     ****  INITIALIZE FOR STARTING A NEW INTEGRATION
      do 4210 J = 3, 20
 4210    IOPTC(J) = IOPTS(J)
      KSOUT = IOPTS(4)
      KMARK = 1 - IOPTS(1)
      KORDI = IOPTS(17)
      NKDKO = max(-KORDI, 0)
      IOPST = IOPTS(22)
      go to 4260
c
c     **** SET A NOMINAL VALUE
 4220 IOPTS(IA) = 0
      if (IA .eq. 12) go to 4420
      if (IA - 2) 4400, 4240, 4230
 4230 if (IA .eq. 4) IOPTS(4) = 500000
      go to 4390
c
c     **** SET ALL OPTIONS TO THEIR NOMINAL VALUES
 4240 IA = 1
      IOPTS(1) = 0
      do 4250 J = 3, 22
         IOPTS(J) = 0
         IOPTC(J) = 0
 4250    continue
      IOPTS(4) = 500000
      IOPTC(4) = IOPTS(4)
      IOPTS(17) = 1
 4260 NGTOT = IOPTS(7) + max(IOPTS(6), 0)
      if (IOPTS(12) .eq. 0) go to 4420
 4270 return
c
c     **** SET SPECIFIED OPTION
 4280 J = IOPT(K + 1)
      if (INCOP(IA) - 2) 4290, 4330, 4300
c     **** OPTION INVOLVES NO EXTRA PARAMETERS
 4290 IOPTS(IA) = 1
      if (IA - 2) 4400, 4400, 4390
c     **** TAKE CARE OF SECOND EXTRA PARAMETER
 4300 if (IA .ne. 10) go to 4310
      NOUTKO = IOPT(K + 2)
      if (NOUTKO) 4500, 4350, 4350
 4310 if (IA .ne. 16) go to 4320
      NTOLF = IOPT(K + 2)
      if (NTOLF) 4500, 4500, 4350
 4320 if (J .eq. 3) then
        if (KMARK .ne. 3) then
           if (XI(1)*(FOPT(IOPT(K+2)) - TMARK) .ge. C0) go to 4400
        end if
      end if
      TMARK = FOPT(IOPT(K+2))
      KMARK = J
      go to 4400
c     **** TAKE CARE OF FIRST EXTRA PARAMETER
 4330 continue
      if (IA .eq. 12) go to 4410
      if (IA .eq. 4) KSOUT = J
 4350 IOPTS(IA) = J
      if (abs(IA - 7) .gt. 1) go to 4360
c     **** SET SPECIAL PARAMETERS FOR GSTOP-S
      IGFLG = 0
      NGTOT = IOPTS(7) + max(IOPTS(6), 0)
c     **** TEST FOR ERROR
      if (J .gt. 500) go to 4500
 4360 if (J .gt. 0) go to 4390
      if ((IA .eq. 5) .or. (IA .eq. 17)) go to 4390
      if (J + 1) 4500, 4370, 4380
 4370 if (IA .eq. 7) go to 4500
 4380 if ((IA .eq. 4) .or. (IA .eq. 11) .or. (IA .ge. 16)) go to 4500
c     **** STORE SAVED VALUE IN COMMON
 4390 IOPTC(IA) = IOPTS(IA)
c
c     **** INCREMENT K TO GET NEXT OPTION
 4400 K = K + INCOP(IA)
      go to 4200
c
c ******* SET UP INFORMATION FOR CHANGING STEPSIZE *********
c
c     **** TEST IF VALUES ARE ALREADY SET
 4410 if (IOPTS(12) .gt. 0) go to 4430
c     **** SET NOMINAL VALUES FOR VARIABLES ONLY SET ONCE
 4420 EREP = CP3
c     **** SET NOMINAL VALUES FOR STEPSIZE CONTROL AND ENV. CONSTANTS
      EEPS2 = D1MACH(4)
      EEPS16 = C16 * EEPS2
      EEPS10 = CP625 * EEPS16
      EEPT75 = EEPS2 ** CP75
      EEPS2 = EEPS2 + EEPS2
      OVD10 = D1MACH(2)
      EROV10 = C10 / OVD10
      EOVEP2 = OVD10 * EEPS2
      OVTM75 = OVD10 ** CMP75
      OVD10 = OVD10 / C10
      HINC = C2
      HDEC = CP5
      HMIN = EROV10
      HMAX = OVD10
      if (I .ne. 12) go to 4470
 4430 IOPTS(12) = J
      if (J) 4450, 4470, 4460
c     **** SET UP TO GIVE USER COMPLETE STEPSIZE CONTROL
 4450 EREP = C1 / EROV10
      HINC = -C2
      IOP8 = 1
c## Recent code 12/16/94
      LINCD = -2
      LINC = -2
      ROBND = C1
c## End of recent code
      go to 4480
c     **** SET USER VALUES FOR STEPSIZE CONTROL
 4460 if (FOPT(J) .ne. C0) HINC = max(C1P125, min(FOPT(J),C4))
      if (FOPT(J + 1) .ne. C0) HDEC = min(CP875, max(FOPT(J + 1), CP25))
      if (FOPT(J + 2) .ne. C0) HMIN = FOPT(J + 2)
      if (FOPT(J + 3) .ne. C0) HMAX = FOPT(J + 3)
 4470 KQICON = -1
 4480 HMAXP9 = HMAX * CP9
      if (I - 1111) 4400, 4270, 4400
c
c ***************** ERROR  IN  IOPT ************************
c
 4490 IA = 1
 4500 KORD1I = 24
      MACT(6) = K + INCOP(IA) - 1
      call MESS(MACT, MTXTAA, IOPT)
      KORD2I = -4
c Usual return with no error is here.
 4520 LIOPT = K
      return
      end
c   End of DIVAOP

      subroutine DIVAPR(Y, YN, F, KORD)
c>> 1988-01-13 DIVAPR Krogh   Initial code.
c
c THIS SUBROUTINE
c   1. UPDATES THE DIFFERENCE TABLE FROM THE PREVIOUS STEP (IF NOT
c      DONE ALREADY).
c   2. PREDICTS WHAT THE VALUES OF THE DEPENDENT VARIABLES, Y, AND
c      THE DIFFERENCE TABLE, DT, WILL BE AT THE END OF THE CURRENT STEP.
c
c   Y = VECTOR OF PREDICTED VALUES COMPUTED BY THIS SUBROUTINE.
c   YN= VECTOR OF VALUES OF Y COMPUTED ON THE LAST STEP.
c   F = VECTOR OF DERIVATIVE VALUES.
c   DT= ARRAY CONTAINING DIFFERENCE TABLES.
c   KD= VECTOR GIVING ORDERS OF THE DIFFERENTIAL EQUATIONS (IF
c       EQUATIONS HAVE DIFFERENT ORDERS).
c   KQ= VECTOR OF INTEGRATION ORDERS.
c
      integer KORD(*)
c--D Next line special: P=>D, X=>Q
      double precision Y(*), YN(*)
      double precision F(*)
c
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 20, MAXORD = 2, MAXSTF = 1)
c--D Next line special: P=>D, X=>Q
      double precision TN
      double precision XI(KDIM)
c
c--D Next line special: P=>D, X=>Q
      double precision TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      double precision FDAT(11)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2
      double precision EEPT75, EOVEP2, OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / DIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /DIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
      double precision C0
      parameter (C0 = 0.D0)
c
      integer I, INTEG, INTEGS, J, K, KQQ, L, N
      double precision TEMP(KDIM)
      double precision TP1
c--D Next line special: P=>D, X=>Q
      double precision XP
      data INTEGS / -1 /
c
c++  Code for ARGM is inactive
c      RETURN
c      ENTRY DIVAPE
c++  End
c ********
c START OF CODE
c ********
      IY = 0
      L = NDTF - 1
      do 4680 I = 1, NTE
         INTEG = KORDI
         if (NKDKO .ne. 0) INTEG = KORD(NKDKO + I - 1)
         KQQ = KORD(I + 3)
         K = max(abs(KQQ), 2)
         if (KQQ) 4530, 4520, 4540
 4520    IY = IY + abs(INTEG)
         go to 4670
c ********
c EQUATION IS STIFF, OR IMPLICIT
c ********
 4530    continue
c++  Code for STIFF is inactive
c      KQQ=-KQQ
c      N=KQQ-1
c      JS=abs(KORD(NJSKO+I-1))-1
c      IMPLIC=INTEG
c      INTEG=abs(IMPLIC)-JS
cc.    SET INTEGS FOR STIFF EQUATIONS
c      INTEGS=0
c      IF (K-KSC) 160,160,140
cc.END OF SET-UP FOR STIFF EQUATIONS
c++  End
c ********
c EQUATION IS NOT STIFF
c ********
 4540    N = KQQ
         if (LDT .ne. 0) if (K - KSC) 4570, 4570, 4550
c     DIFFERENCE TABLE HAS NOT BEEN UPDATED
         TP1 = F(I) - F(L + 1)
         if (K - KSC) 4610, 4610, 4590
c END OF SET-UP FOR EQUATIONS WHICH ARE NOT STIFF
c ********
c GET PREDICTED DIFFERENCES FROM UPDATED DIFFERENCE TABLE
c ********
 4550    F(L + K + 1) = F(L + K + 1) * BETA(K + 1)
         TEMP(K) = F(L + K) * BETA(K)
         F(L + K) = TEMP(K)
c LOOP FOR MODIFIED DIVIDED DIFFERENCES
 4560    K = K - 1
         if (K .le. KSC) go to 4580
         TEMP(K) = F(L + K) * BETA(K)
         F(L + K) = TEMP(K) + F(L + K + 1)
         go to 4560
c CODE FOR BACKWARD DIFFERENCES
 4570    F(L + K + 1) = F(L + K + 1)
         TEMP(K) = F(L + K)
         K = K - 1
c
 4580    TEMP(K) = F(L + K)
         F(L + K) = TEMP(K) + F(L + K + 1)
         K = K - 1
         if (K .ne. 0) go to 4580
         go to 4630
c ********
c UPDATE DIFFERENCE TABLE AND GET PREDICTED DIFFERENCES
c ********
c CODE FOR MODIFIED DIVIDED DIFFERENCES
 4590    F(L + K + 1) = (F(L+K+1) + TP1) * BETA(K + 1)
         TEMP(K) = (F(L + K) + TP1) * BETA(K)
         F(L + K) = TEMP(K)
 4600    K = K - 1
         if (K .le. KSC) go to 4620
         TEMP(K) = (F(L + K) + TP1) * BETA(K)
         F(L + K) = TEMP(K) + F(L + K + 1)
         go to 4600
c CODE FOR BACKWARD DIFFERENCES
 4610    F(L + K + 1) = (F(L+K+1) + TP1)
         TEMP(K) = F(L + K) + TP1
         F(L + K) = TEMP(K)
         K = K - 1
c
 4620    TEMP(K) = F(L + K) + TP1
         F(L + K) = TEMP(K) + F(L + K + 1)
         K = K - 1
         if (K .ne. 0) go to 4620
c ********
c COMPUTE Y-S OBTAINED USING INTEGRATION
c ********
c     TEST IF NEXT Y TO BE OBTAINED BY INTERPOLATION
 4630    continue
c++  Code for STIFF is inactive
c      IF (INTEG.EQ.0) GO TO 4662
c++  End
         IY = IY + 1
c     FORM INNER PRODUCT
         XP = C0
         do 4650 J = INTEGS + N + 1, INTEGS + 2, -1
c++  Code for ~{p,x} is active
            XP = XP + G(J, INTEG) * TEMP(J)
c++  Code for {p,x} is inactive
Cc--D Next line special: P=>D, X=>Q
C            XP = XP + dble(G(J, INTEG)) * dble(TEMP(J))
c++  END
 4650    continue
         K = INTEG + INTEGS
         do 4660 J = K, 1, -1
c++  Code for ~{p,x} is active
            XP = XP + G(1, J) * YN(IY + J)
c++  Code for {p,x} is inactive
Cc--D Next line special: P=>D, X=>Q
C            XP = XP + dble(G(1, J)) * dble(YN(IY + J))
c++  END
 4660    continue
         Y(IY) = YN(IY) + XP
         INTEG = INTEG - 1
         if (K) 4670, 4670, 4630
c END OF COMPUTING Y-S OBTAINED BY INTEGRATION
c ********
c COMPUTE Y-S OBTAINED USING INTERPOLATION AND DIFFERENTIATION
c ********
c++  Code for STIFF is inactive
cc.    RESTORE INTEGS FOR EQUATIONS WHICH ARE NOT STIFF
c 4662 INTEGS=-1
c      IY=IY+1
cc.    COMPUTE Y USING INTERPOLATION
c      Y(IY)=YN(IY) + F(L+2)
c      IF (KQQ.EQ.1) Y(IY)=YN(IY)
c 4663 INTEG=INTEG+1
c      IF (INTEG.EQ.JS) IF (IMPLIC) 4680,4680,4664
cc.    COMPUTE INTEG-TH DERIVATIVE
c      XP = C0
c 4664 DO 4666 J = KQQ+1, INTEG+1, -1
c         XP = XP + D(J, INTEG) * TEMP(J)
c 4666 CONTINUE
c      IF (INTEG.EQ.JS) GO TO 4667
c      IY=IY+1
c      Y(IY)=XP
c      GO TO 4663
cc.STORE PREDICTED VALUE FOR F
c 4667 CONTINUE
c      F(L+NUMDT)=XP
c++  End
 4670    L = L + NUMDT
 4680    continue
      LDT = -3
      return
      end
