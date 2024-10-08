      subroutine SIVADB(LPRINT, TSPECS, Y, F, KORD, NTEXT)
c     .  Copyright (C) 1989, California Institute of Technology.
c     .  All rights reserved.  U. S. Government sponsorship under
c     .  NASA contract NAS7-918 is acknowledged.
C>> 1995-04-26 SIVADB Krogh Fixed print of V & G's for high order eqs.
C>> 1994-11-11 SIVADB Krogh Declared all vars.
c>> 1994-10-20 SIVADB Krogh Changes to use M77CON
c>> 1994-09-12 SIVADB Krogh Added CHGTYP code.
c>> 1994-03-07 SIVADB Krogh Allow larger order in single precision.
c>> 1993-05-03 SIVADB Krogh Additions for Conversion to C.
c>> 1993-04-14 SIVADB Krogh Changes for new MESS usage.
c>> 1992-04-08 SIVADB Krogh Unused labels 10 and 60 removed.
c>> 1992-03-10 SIVADB Krogh Fixed value for KDIM in single p. version.
c>> 1992-02-17 SIVADB Krogh Made tabs depend on # digits output.
c>> 1991-11-04 SIVADB Krogh Switched to use MESS, SMESS
c>> 1990-03-08 SIVADB Krogh Unused stiff vars. set to 0.
c>> 1989-07-21 SIVADB Krogh Code for integrating discontinuities
c>> 1988-06-07 SIVADB Krogh Dim. of IVC2 and DVC2 upped by 1 (old bug)
c>> 1987-12-07 SIVADB Krogh Initial code.
c
c--S replaces "?": ?IVADB, ?IVAEV, ?IVAMC, ?IVASC, ?MESS
c
c SUBROUTINE TO GIVE DEBUG PRINT FOR VARIABLE ORDER INTEGRATOR
c
c  LET ABS(LPRINT)=  10*N1 + N2     (N1,N2 DIGITS)
c    N1=1   DO NOT PRINT ANY VARIABLES EXTERNAL TO THE INTEGRATOR
c    N1=2   PRINT  TSPECS, CURRENT Y, PAST Y, CURRENT F,
c           ALL PERTINENT CONTENTS OF KORD, AND TOL.
c    N1=3   ABOVE + DIFFERENCE TABLES UP TO HIGHEST DIFFERENCE USED
c    N1=4   SAME AS N1=1 + ALL IN STORAGE ALLOCATED FOR DIFFERENCES
c
c    N2=1   DO NOT PRINT ANY VARIABLES INTERNAL TO THE INTEGRATOR
c    N2=2   PRINT ALL SCALAR VARIABLES IN INTERPOLATION COMMON BLOCK
c    N2=3   ABOVE + ALL SCALAR VARIABLES IN MAIN INTEG. COMMON BLOCK
c    N2=4   SAME AS N1=3 + ALL USED IN ARRAYS XI,BETA,ALPHA, FIRST
c           COLUMN OF G, GS,RBQ,SIGMA
c    N2=5   SAME AS N1=4 + ALL USED IN ARRAYS G,D,DS,V
c
      integer LPRINT, KORD(*)
      character NTEXT*(*)
      integer IVC1(12), IVC2(65), J, K, L, N1, N2
      real              DVC2(7), RVC2(8), EVC(8)
c--S Next line special: P=>D, X=>Q
      real             TSPECS(*), Y(*), TNEQ(1), DVC1(7)
      real             F(*)
c
c++S Default KDIM = 16
c++  Default KDIM = 20
c++  Default MAXORD = 2, MAXSTF = 1
c++  Default STIFF=.F.
      integer KDIM, MAXORD, MAXSTF
c++ Substitute for KDIM, MAXORD, MAXSTF below
      parameter (KDIM = 16, MAXORD = 2, MAXSTF = 1)
c--S Next line special: P=>D, X=>Q
      real             TN
      real             XI(KDIM)
c
c--S Next line special: P=>D, X=>Q
      real             TG(2), TGSTOP(2), TMARK, TMARKX, TOUT
      real             ALPHA(KDIM), BETA(KDIM+1)
      real              D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      real             V(KDIM+MAXORD)
      real             HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN
      real             FDAT(11)
c
      real             DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      real             SIGMA(KDIM), RBQ(KDIM), DNOISE
      real             EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      real             EEPS10, EEPS16, EROV10, EEPS2
      real             EEPT75, EOVEP2, OVTM75, OVD10
      common / SIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      save / SIVAEV /
      integer IOPST, KORDI, KQMAXD, KQMAXI, LDT, MAXDIF, MAXINT, NKDKO,
     1   NTE, NYNY, NDTF, NUMDT
      common / SIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      integer ICF,ICS,IGFLG,IGTYPE(2),IGSTOP(2),ILGREP,INGS,IOP3,IOP4,
     1   IOP5,IOP6,IOP7,IOP8,IOP9,IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,
     2   IOP16,IOP17,IOP18,IOP19,IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,
     3   KEMAX,KIS,KMARK,KORD1I,KORD2I,KPRED,KQDCON,KQICON,KQMAXS,
     4   KQMXDS,KQMXIL,KQMXIP,KQMXIS,KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,
     5   LINCD,LINCQ,LSC,MAXKQD,MAXKQI,METHOD,NE,NEPTOL,NG,NGTOT,
     6   NOISEQ,NOUTKO,NTOLF,NY,IDAT(6)
      common /SIVAMC/ TG,TGSTOP,TMARK,TMARKX,TOUT,HC,HDEC,HINC,HINCC,
     1   HMAX,HMAXP9,HMIN,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / SIVAMC / , / SIVASC /
      equivalence (IVC1(1), IOPST), (IVC2(1), ICF)
      equivalence (TNEQ, TN)
      equivalence (RVC2(1), DNOISE), (DVC1(1), TG), (DVC2(1), HC),
     1   (EVC(1), EEPS2)
c
c                      Declarations for error message processing.
      integer MEDDIG, NEDDIG, METDIG, METABS, MERET, METEXT, METXTF,
     1   METABL, MEIVEC, MEFVEC, MEFMAT
      parameter (MEDDIG =12)
      parameter (NEDDIG =-MEDDIG)
      parameter (METDIG =22)
      parameter (METABS =32)
      parameter (MERET  =51)
      parameter (METEXT =53)
      parameter (METXTF =54)
      parameter (METABL =55)
      parameter (MEIVEC =57)
      parameter (MEFVEC =61)
      parameter (MEFMAT =62)
      integer MACT0(3), MACT1(2), MACT2(7), MACT3(7), MACT4(8),
     1   MACT5(11), MACT6(3), MACT7(14), MACTFV(4)
      integer KPI, KPE
c                      wddtrr        wwddtrr
      parameter (KPI = 400301, KPE = 1305501)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA $NKORD:    $B
cAB Int. Ord.: $B
c   $
cAC D.E. Ord.: $B
c   $
cAD Meth.Type: $B
c   $
cAE Tolerance Groups: $B
cAF Tolerances: $B
c   $
cAG $NDifferences$B
cAH Eq. $#
cAI Ord. $#
c   $
cAJ $NTN=$F$E
c   $
cAK $NIOPST=$I$TKORDI=$I$TKQMAXD=$I$TKQMAXI=$I$TLDT=$I$T$C
c   MAXDIF=$I$TMAXINT=$I$TNKDKO=$I$TNTE=$I$TNYNY=$I$TNDTF=$I$C
c   $TNUMDT=$I$E
c   $
cAL $NICF=$I$TICS=$I$TIGFLG=$I$TIGTYPE(1)=$I$TIGTYPE(2)=$I$T$C
c   IGSTOP(1)=$I$TIGSTOP(2)=$I$TILGREP=$I$TINGS=$I$TIOP3=$I$T$C
c   IOP4=$I$TIOP5=$I$TIOP6=$I$TIOP7=$I$TIOP8=$I$TIOP9=$I$T$C
c   IOP10=$I$TIOP11=$I$TIOP12=$I$TIOP13=$I$TIOP14=$I$TIOP15=$I$T$C
c   IOP16=$I$TIOP17=$I$TIOP18=$I$TIOP19=$I$TIOP20=$I$TIOP21=$I$T$C
c   IOP22=$I$TIOP21S=$I$TITOLEP=$I$TIY=$I$TKEMAX=$I$TKIS=$I$T$C
c   KMARK=$I$TKORD1I=$I$TKORD2I=$I$TKPRED=$I$TKQDCON=$I$T$C
c   KQICON=$I$TKQMAXS=$I$TKQMXDS=$I$TKQMXIL=$I$TKQMXIP=$I$T$C
c   KQMXIS=$I$TKSC=$I$TKSOUT=$I$TKSSTRT=$I$TKSTEP=$I$TLEX=$I$T$C
c   LINC=$I$TLINCD=$I$TLINCQ=$I$TLSC=$I$TMAXKQD=$I$TMAXKQI=$I$T$C
c   METHOD=$I$TNE=$I$TNEPTOL=$I$TNG=$I$TNGTOT=$I$TNOISEQ=$I$T$C
c   NOUTKO=$I$TNTOLF=$I$TNY=$I$E
cAM $NDNOISE=$F$TEAVE=$F$TEIMAX=$F$TEIMIN=$F$TEMAX=$F$T$C
c   EREP=$F$TROBND=$F$E
c   $
cAN $NTG(1)=$F$TTG(2)=$F$TTGSTOP(1)=$F$TTGSTOP(2)=$F$C
c   $TTMARK=$F$TTMARKX=$F$TTOUT=$F$E
c   $
cAO HC=$F$THDEC=$F$THINC=$F$THINCC=$F$THMAX=$F$T$C
c   HMAXP9=$F$THMIN=$F$T$N$E
c   $
cAP K$HXI(K)$HBETA(K)$HALPHA(K)$HG(K,1)$HRBQ(K)$HSIGMA(K)$H
c   GS(K)$HV(K)$HG(K,2..MAXINT)$E
c   $
cAQ $NEEPS2=$F$TEEPT75=$F$TEOVEP2=$F$TOVTM75=$F$TOVD10=$F$T$C
c   EEPS10=$F$TEEPS16=$F$TEROV10=$F$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG,LTXTAH,
     * LTXTAI,LTXTAJ,LTXTAK,LTXTAL,LTXTAM,LTXTAN,LTXTAO,LTXTAP,LTXTAQ
      parameter (LTXTAA=  1,LTXTAB= 14,LTXTAC=  1,LTXTAD=  1,LTXTAE=  1,
     * LTXTAF= 21,LTXTAG=  1,LTXTAH= 16,LTXTAI= 22,LTXTAJ=  1,
     * LTXTAK=  1,LTXTAL=  1,LTXTAM=655,LTXTAN=  1,LTXTAO=  1,
     * LTXTAP=  1,LTXTAQ=  1)
      character MTXTAA(1) * (26)
      character MTXTAB(1) * (13)
      character MTXTAC(1) * (13)
      character MTXTAD(1) * (34)
      character MTXTAE(1) * (28)
      character MTXTAF(1) * (9)
      character MTXTAG(1) * (120)
      character MTXTAH(3) * (242)
      character MTXTAI(1) * (80)
      character MTXTAJ(1) * (68)
      character MTXTAK(1) * (84)
      character MTXTAL(1) * (88)
      data MTXTAA/'$NKORD:    $BInt. Ord.: $B'/
      data MTXTAB/'D.E. Ord.: $B'/
      data MTXTAC/'Meth.Type: $B'/
      data MTXTAD/'Tolerance Groups: $BTolerances: $B'/
      data MTXTAE/'$NDifferences$BEq. $#Ord. $#'/
      data MTXTAF/'$NTN=$F$E'/
      data MTXTAG/'$NIOPST=$I$TKORDI=$I$TKQMAXD=$I$TKQMAXI=$I$TLDT=$I$TM
     *AXDIF=$I$TMAXINT=$I$TNKDKO=$I$TNTE=$I$TNYNY=$I$TNDTF=$I$TNUMDT=$I$
     *E'/
      data MTXTAH/'$NICF=$I$TICS=$I$TIGFLG=$I$TIGTYPE(1)=$I$TIGTYPE(2)=$
     *I$TIGSTOP(1)=$I$TIGSTOP(2)=$I$TILGREP=$I$TINGS=$I$TIOP3=$I$TIOP4=$
     *I$TIOP5=$I$TIOP6=$I$TIOP7=$I$TIOP8=$I$TIOP9=$I$TIOP10=$I$TIOP11=$I
     *$TIOP12=$I$TIOP13=$I$TIOP14=$I$TIOP15=$I$TIOP16=$I$TIOP17','=$I$TI
     *OP18=$I$TIOP19=$I$TIOP20=$I$TIOP21=$I$TIOP22=$I$TIOP21S=$I$TITOLEP
     *=$I$TIY=$I$TKEMAX=$I$TKIS=$I$TKMARK=$I$TKORD1I=$I$TKORD2I=$I$TKPRE
     *D=$I$TKQDCON=$I$TKQICON=$I$TKQMAXS=$I$TKQMXDS=$I$TKQMXIL=$I$TKQMXI
     *P=$I$TKQMXIS=$I$TKSC=$I$TKSOUT=$I$TKSS','TRT=$I$TKSTEP=$I$TLEX=$I$
     *TLINC=$I$TLINCD=$I$TLINCQ=$I$TLSC=$I$TMAXKQD=$I$TMAXKQI=$I$TMETHOD
     *=$I$TNE=$I$TNEPTOL=$I$TNG=$I$TNGTOT=$I$TNOISEQ=$I$TNOUTKO=$I$TNTOL
     *F=$I$TNY=$I$E$NDNOISE=$F$TEAVE=$F$TEIMAX=$F$TEIMIN=$F$TEMAX=$F$TER
     *EP=$F$TROBND=$F$E  '/
      data MTXTAI/'$NTG(1)=$F$TTG(2)=$F$TTGSTOP(1)=$F$TTGSTOP(2)=$F$TTMA
     *RK=$F$TTMARKX=$F$TTOUT=$F$E'/
      data MTXTAJ/'HC=$F$THDEC=$F$THINC=$F$THINCC=$F$THMAX=$F$THMAXP9=$F
     *$THMIN=$F$T$N$E'/
      data MTXTAK/'K$HXI(K)$HBETA(K)$HALPHA(K)$HG(K,1)$HRBQ(K)$HSIGMA(K)
     *$HGS(K)$HV(K)$HG(K,2..MAXINT)$E'/
      data MTXTAL/'$NEEPS2=$F$TEEPT75=$F$TEOVEP2=$F$TOVTM75=$F$TOVD10=$F
     *$TEEPS10=$F$TEEPS16=$F$TEROV10=$F$E'/
c
      data MACT0 / METABS, 10, MERET /
      data MACT1 / METEXT, MERET /
c                       1       2  3       4       5  6      7
      data MACT2 / METEXT, MEIVEC, 3, METEXT, MEIVEC, 0, MERET /
c                       1       2  3       4       5  6      7
      data MACT3 / METEXT, MEIVEC, 0, METEXT, MEFVEC, 0, MERET /
c                       1       2  3  4  5       6       7
      data MACT4 / METEXT, MEFMAT, 0, 0, 0, LTXTAI, LTXTAH, MERET /
c                       1   2       3       4   5       6  7       8
      data MACT5 / METABS, 12, METEXT, METABS, 18, METDIG, 5, METEXT,
     1             METABS, 0, MERET /
c                       9 10     11
      data MACT6 / NEDDIG, 0, MERET /
c                         2 3 4   5 6 7 8 9  10  11  12 13 14
      data MACT7 / METABL,0,0,0,KPI,0,0,0,0,KPE,KPE,KPE, 0, 0 /
c                        1       2  3      4
      data MACTFV / METEXT, MEFVEC, 3, MERET /
c
c ********
c START OF CODE -- PRINT NTEXT AND SET INDEX FOR F
c ********
c++  Code for ~STIFF is active
      KQDCON = 0
      KQMXDS = 0
      MAXKQD = 0
      J = 0
      call MESSFT(MACT0, NTEXT)
c
      N1 = LPRINT / 10
      N2 = LPRINT - 10 * N1
      if (N1 .le. 1) go to 80
c ********
c PRINT ALL EXTERNAL VARIABLES EXCEPT FOR THE DIFFERENCES
c ********
      MACTFV(3) = max(IOP5, 4)
c--S Next line special: P=>D, X=>Q
      call SMESS(MACTFV, '$NTSPECS:$B', KORD, TSPECS)
      MACTFV(3) = NY
c--S Next line special: P=>D, X=>Q
      call SMESS(MACTFV, 'Y:$B', KORD, Y)
c--S Next line special: P=>D, X=>Q
      call SMESS(MACTFV, 'YN:$B', KORD, Y(NYNY))
      MACTFV(3) = NTE
c--S Next line special: P=>S, X=>D
      call SMESS(MACTFV, 'F:$B', KORD, F)
      MACT2(6) = NTE
      call MESS(MACT2, MTXTAA, KORD)
      if (NKDKO .gt. 0) call MESS(MACT2(4), MTXTAB, KORD(NKDKO))
      if (IOPST .gt. 0) call MESS(MACT2(4), MTXTAC, KORD(IOPST))
c WRITE TOL
      K = IOP16
   70 if (KORD(K) .lt. 0) K = K + 1
      K = K + 1
      if (KORD(K - 1) .lt. NTE) go to 70
      MACT3(3) = K - IOP16
      MACT3(6) = MACT3(3)
c--S Next line special: P=>S, X=>D
      call SMESS(MACT3, MTXTAD, KORD(IOP16), F(NTOLF))
      if (N1 .eq. 2) go to 80
c ********
c WRITE THE DIFFERENCE TABLES
c ********
      K = NUMDT
      if (N1 .eq. 3) K = KQMAXS
      MACT4(3) = NUMDT
      MACT4(4) = -K
      MACT4(5) = NTE
c--S Next line special: P=>S, X=>D
      call SMESS(MACT4, MTXTAE, KORD, F(NDTF))
c
   80 if (N2 .le. 1) return
c ********
c WRITE SCALARS IN COMMON
c ********
c--S Next line special: P=>D, X=>Q
      call SMESS(MACT1, MTXTAF, KORD, TNEQ)
c
c ===== COMMON 1  -- INTEGER
c
      call MESS(MACT1, MTXTAG, IVC1)
      if (N2 .eq. 2) return
      call MESS(MACT6, MTXTAA, IDAT)
      MACT5(10) = MACT6(2) + 14
c
c ===== COMMON 2  -- INTEGER AND FLOATING POINT
c
c--S Next line special: P=>S, X=>D
      call SMESS(MACT5, MTXTAH, IVC2, RVC2)
c--S Next line special: P=>D, X=>Q
      call SMESS(MACT1, MTXTAI, IVC2, DVC1)
c--S Next line special: P=>S, X=>D
      call SMESS(MACT1, MTXTAJ, IVC2, DVC2)
      if (N2 .eq. 3) return
c         wddtrr              wddtrr
      J = 101000 * MACT6(2) + 800501
      MACT7(2) = 1
      MACT7(3) = KQMAXS + 1
      MACT7(4) = 8
      do 90 K = 6, 9
         MACT7(K) = J
   90 continue
      if (N2 .gt. 0) then
         MACT7(4) = 8 + MAXINT
         MACT7(13) = J
         L = min(MAXINT, 4)
         MACT7(14) = J + L - 2
      end if
      do 100 K = 1, MACT7(3)
         FDAT(1) = XI(K)
         FDAT(2) = BETA(K)
         FDAT(3) = ALPHA(K)
         FDAT(4) = G(K, 1)
         FDAT(5) = RBQ(K)
         FDAT(6) = SIGMA(K)
         FDAT(7) = GS(K)
         if (N2 .ge. 4) then
            FDAT(8) = V(K)
            do 95 J = 2, L
               FDAT(7+J) = G(K, J)
   95       continue
         end if
c--S Next line special: P=>S, X=>D
         call SMESS(MACT7, MTXTAK, IDAT, FDAT)
  100 continue
c++  Code for STIFF is inactive
c     if (MAXDIF .le. 0) return
c        Need to define MACT8 and set values
cc--S Next line special: P=>S, X=>D
c     call SMESS(MACT8, 'D$B', IDAT, D)
cc--S Next line special: P=>S, X=>D
c     call SMESS(MACT8, 'DS$B', IDAT, DS)
c++  End
c
c--S Next line special: P=>S, X=>D
      call SMESS(MACT1, MTXTAL, IDAT, EVC)
      return
      end
