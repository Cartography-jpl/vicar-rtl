c$$   START( DIVADA, ROOT = 'DIVA', $ROOT$G)
c     .  Copyright (C) 1989, California Institute of Technology.
c     .  All rights reserved.  U. S. Government sponsorship under
c     .  NASA contract NAS7-918 is acknowledged.
c>> 1994-03-07 DIVADA  Krogh  Allow larger order in single precision.
c>> 1993-04-16 DIVADA  Krogh  Additions for Conversion to C.
c>> 1992-04-08 DIVADA  Krogh  Passed IDAT to MESS instead of IFLAG.
c>> 1992-04-08 DIVADA  Krogh  Unused labels 120 and 220 removed.
c>> 1992-03-10 DIVADA  Krogh  Fixed value for KDIM in single p. version.
c>> 1991-04-01 DIVADA  Krogh  Removed extra "." in character const.
c>> 1990-12-20 DIVADA  Krogh  Removed obsolete code.
c>> 1990-10-22 DIVADA  Krogh  Fixed TSPECS(1) not getting set on conv.
c>> 1990-09-28 DIVADA  Krogh  First "Library" version.
c>> 1989-07-06 DIVADA  Krogh  Initial code.  More needs adding.
c
c     -IVADA(TSPECS, Y, F, KORD, IFLAG, LEQ)
c
c  SUBROUTINE TO ADJUST DIFFERENCES NEAR A DISCONTINUITY.
c  FOR DIFFERENTIAL EQUATION INTEGRATOR -ODE (OR -IVA).
c ========== THIS PROGRAM HAS BEEN SPECIALIZED ==========
c
c
      subroutine DIVADA(TSPECS, Y, F, KORD, IFLAG, LEQ)
c
      integer KORD(*), IFLAG, LEQ(*)
      double precision TSPECS(*), F(*)
      double precision Y(*)
c
c ********************  Local variables  ****************
c
c C0     Parameter giving a floating point value of 0.
c C1     Parameter giving a floating point value of 1.
c CONV   .true. if errors suffieiently small for current F.
c CP5    Parameter giving a floating point value of 0.5.
c F      (formal) Array containing derivative values, difference tables,
c  and space for various options.
c HDIS   TD (point of discontinuity) - TN (base time for integrator).
c HH     The current integration stepsize.  (equivalenced into *IVAMC)
c KORD   (formal) Array used for integrator flags, integration orders,
c  and various options.
c KQQ    The current integration order.
c IDT    Index in the integrators difference tables.
c IFLAG  (formal) Value determines the action taken, see IFLAGA.
c IFLAGA Value of IFLAG on the last entry.
c    = 1  No discontinuities in Y or F at the point of discontinuity.
c    = 2  Discontinuities defined only for F at the discontinuity.
c    = 3  Discontinuities defined only for Y at the discontinuity.
c    = 4  Discontinuities defined for both F and Y at the discontinuity.
c    = 5  Just computed the first F in an iteration.
c    = 6  Computed F after the first of an iteration.
c    = 0  Called when convergence has already been signaled.
c IL     Used as index into LEQ.  abs(LEQ(IL)) is the index of the last
c  equation in the current group.
c IOP21  (*IVAMC) Value set on option 21, points to start of space in
c  F available for use in adjusting for the discontinuity.  This space
c  is used as follows.  Coefficients for integrating from TN to the
c  discontinuity are stored in F(IOP21) to F(IOP21+KDMXDA*KQMXDA-1).
c  The first KQMXDA coefficients are for integrating first order
c  equations, the next KQMXDA are for second order equations, etc.  The
c  remainder of the space is used for difference tables.  One must have
c  at least NTEDA spaces set aside.  At most NTEDA*KQMXDA spaces are
c  used.  Unless analysis are experiment shows that fewer spaces work,
c  the NTEDA*KQMXCA spaces should be set aside.
c IOP21S (*IVAMC) Space reserved in F for option 21, see IOP21.
c IY     index of last component of Y used for the current equation.
c J      temporary index.
c K      temporary index.
c KDMXDA maximum ODE order for equations being updated.
c KORDI  (*IVASC) the integration orders of the equations.
c KQMXDA maximum integration order for equations being updated.
c LDA    Index where saved differences are stored.
c LDA0   NIFF-1 before location of first saved difference.
c LEQ    (formal) Defines equation grouping for discontinuities.  If
c  discontinuity applies for all differential equations, set LEQ(1)=NEQ,
c  the number of differential equations in the system.  Otherwise set
c  abs(LEQ(k)) = index of last equation in the k-th group, with LEQ(k)
c  > 0 if the group is being corrected for a discontinuity, and < 0 if
c  the discontinuity corrections are not to be applied for the group.
c  The k-th group consists of equations with index abs(LEQ(k-1))+1, to
c  abs(LEQ(k)), k = 1, ...  (LEQ(0) is not specified by the user and is
c  assumed to be 0.)  Note that equations that don't have discontinui-
c  ties should be included in those processed if they are coupled
c  sufficiently strongly to equations that do have discontinuities.
c LNCF   Value of NCF at end of last iteration.
c LT     When computing integration coeffients, gives number of
c  coefficients being computed for the current order.
c M      temporary index.
c MA     = abs(LEQ(IL)).
c NCF    Index of function evaluation on current iteration.
c NCONV  number of times got small enough error on current iteration.
c NDIFF  number of differences that can be corrected.
c NDTF   (*IVASC) index of difference tables in F.
c NDTFDA index of correction differences in F.
c NKDKO  (*IVASC) if nonzero, points to location in KORD where
c integration orders for the different equations are stored.
c NTE    (*IVASC) Number of differential equations being integrated.
c NTEDA  number of equations being updated to smooth out the
c  discontinuity.
c NYNY   (*IVASC) index where base Y's start in Y.
c SUMHI  Absolute sum of the highest order correction differences.
c SUMLO  Absolute sum of the highest-1 and highest-2 correction diff.
c SUMLOM Initially 0, playing around with lots of choices now.
c TP     temporary floating point storage.
c TP1    temporary floating point storage.
c TP2    temporary floating point storage.
c TSPECS (formal) TSPECS(1)=current value of independent variable = T,
c  called time.  Remaining values of TSPECS are not used.
c XI     (*IVASC) XI(J) = TN - value of T j steps ago.
c Y      (formal) Array containing current values of dependent
c  variables, and values at the base time.
c
c$    parameter (KDIM = $KDIMDT$)
      parameter (KDIM = 20)
c$    parameter (MAXORD = $KMAXO$)
      parameter (MAXORD = 2)
c$    parameter (MAXSTF = $MAXKQD$)
      parameter (MAXSTF = 1)
      double precision TN, XI(KDIM)
c
      integer IGTYPE(2), IGSTOP(2)
      double precision ALPHA(KDIM), BETA(KDIM+1)
      double precision  D(MAXSTF+MAXORD,MAXORD), G(KDIM,MAXORD)
      double precision V(KDIM+MAXORD-2)
      double precision HC, HDEC, HINC, HINCC, HMAX, HMAXP9, HMIN, TG(2),
     2   TGSTOP(2), TMARK, TMARKX, TOUT
      double precision FDAT(11)
      integer IDAT(6)
c
      double precision DS(MAXSTF+MAXORD, MAXORD), GS(KDIM)
      double precision SIGMA(KDIM), RBQ(KDIM), DNOISE
      double precision EAVE, EIMAX, EIMIN, EMAX, EREP, ROBND, SNOISE
c
      logical CONV
      integer I, IDT, IFLAGA, J, K, KQQ, M, MA, NCONV
      double precision HH, HDIS, SUMHI, SUMLO, SUMLOM, TP, TP1,
     1   TP2, C0, C1, CP1, CP5
      double precision CDA(KDIM-1)
      parameter (C0 = 0.D0)
      parameter (C1 = 1.D0)
      parameter (CP1 = 0.1D0)
      parameter (CP5 = 0.5D0)
c.    SPECIFICATION OF ENVIRONMENTAL CONSTANTS.
      double precision EEPS10, EEPS16, EROV10, EEPS2
      double precision EEPT75, EOVEP2, OVTM75, OVD10
      common / DIVAEV / EEPS2, EEPT75, EOVEP2, OVTM75, OVD10, EEPS10,
     1   EEPS16, EROV10
      common / DIVASC / TN, XI, IOPST, KORDI, KQMAXD, KQMAXI, LDT,
     1   MAXDIF, MAXINT, NKDKO, NTE, NYNY, NDTF, NUMDT
c
      common /DIVAMC/ HC,HDEC,HINC,HINCC,HMAX,HMAXP9,HMIN,TG,TGSTOP,
     1   TMARK,TMARKX,TOUT,ALPHA,BETA,D,G,V,DS,GS,SIGMA,RBQ,DNOISE,
     2   EAVE,EIMAX,EIMIN,EMAX,EREP,ROBND,SNOISE,FDAT,ICF,ICS,IGFLG,
     3   IGTYPE,IGSTOP,ILGREP,INGS,IOP3,IOP4,IOP5,IOP6,IOP7,IOP8,IOP9,
     4   IOP10,IOP11,IOP12,IOP13,IOP14,IOP15,IOP16,IOP17,IOP18,IOP19,
     5   IOP20,IOP21,IOP22,IOP21S,ITOLEP,IY,KEMAX,KIS,KMARK,KORD1I,
     6   KORD2I,KPRED,KQDCON,KQICON,KQMAXS,KQMXDS,KQMXIL,KQMXIP,KQMXIS,
     7   KSC,KSOUT,KSSTRT,KSTEP,LEX,LINC,LINCD,LINCQ,LSC,MAXKQD,MAXKQI,
     8   METHOD,NE,NEPTOL,NG,NGTOT,NOISEQ,NOUTKO,NTOLF,NY,IDAT
      save / DIVAMC / , / DIVASC /
      equivalence (G(1, 1), HH), (KEXIT, IOP17)
      save HDIS, ITER, LDA0, NCF,
     1   NCONV, NDIFF, NDTFDA, NTEDA, SUMHI, SUMLO, SUMLOM
c
c                      Declarations for error message processing.
c     
      parameter (MERET  =51)
      parameter (MEEMES =52)
      integer MACT(5)
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA DIVADA$B
cAB IFLAG=$I, is not allowed on entry.$E
cAC Option 21 must be used initially.$E
      integer LTXTAA,LTXTAB,LTXTAC
      parameter (LTXTAA= 1,LTXTAB= 9,LTXTAC=45)
      character MTXTAA(1) * (79)
      data MTXTAA/'DIVADA$BIFLAG=$I, is not allowed on entry.$EOption 21
     * must be used initially.$E'/
      data MACT / MEEMES, 68, 0, 0, MERET /
c
c *********************** Start of Code ********************************
c
      IFLAGA = IFLAG
      if (IFLAGA .le. 4) then
         HDIS = TSPECS(1) - TN
         ITER = 1
         LNCF = 1
         SUMLOM = C0
         if (IFLAGA .gt. 1) then
            IL = 0
            MA = 0
            IY = 0
            NTEDA = 0
            KDMXDA = 0
            KQMXDA = 0
            IDT = NDTF
            do 30 I = 1, NTE
               if (I .gt. MA) then
                  IL = IL + 1
                  M = LEQ(IL)
                  if (M .lt. 0) then
                     MA = -M
                  else
                     MA = M
                     NTEDA = NTEDA + MA
                  end if
               end if
               if (NKDKO .ne. 0) then
                  KORDI = KORD(NKDKO + I - 1)
               end if
c No support for stiff equations here, assume KORDI > 0.
               IY = IY + KORDI
               if (M .gt. 0) then
                  KQMXDA = max(KQMXDA, KORD(I + 3))
                  KDMXDA = max(KDMXDA, KORDI)
                  J = KORDI
                  if (IFLAGA .ne. 3) then
                     TP = F(I)
                     F(IDT) = F(IDT) + TP
                  else
                     TP = C0
                  end if
                  Y(NYNY + IY - 1) = Y(NYNY + IY - 1) - HDIS * TP
                  if (IFLAGA .ne. 2) Y(NYNY+IY-1) = Y(NYNY+IY-1) + Y(IY)
                  if (J .ge. 2) then
                     do 20 K = 2, J
                        TP2 = K
                        TP1 = TP
                        do 10 KK = 1, K
                           TP1 = (-HDIS / TP2) * TP1
                           if (IFLAGA .gt. 2) TP1 = TP1 + Y(IY - KK + 1)
                           TP2 = TP2 - C1
   10                      continue
                        Y(NYNY + IY - K) = Y(NYNY + IY - K) + TP1
   20                   continue
                  end if
               end if
               IDT = IDT + NUMDT
   30          continue
         else if (IFLAG .lt. 0) then
            MACT(3) = 1
            MACT(4) = LTXTAB
            go to 280
         end if
c                  Check storage availability
         if (IOP21 .eq. 0) then
            MACT(3) = 2
            MACT(4) = LTXTAC
            go to 280
         end if
c Get coefficients to interpolate from TN to discontinuity.
         NDIFF = min(KQMXDA, IOP21S / (KDMXDA + NTEDA))
         if (NDIFF .lt. 1) go to 300
         LT = NDIFF + KDMXDA - 2
         K = IOP21 + KDMXDA - 1
         F(IOP21 + LT) = HDIS / float(LT + 1)
         do 70 J = IOP21 + LT - 1, IOP21, -1
            F(J) = HDIS / float(J - IOP21 + 1)
            F(J + 1) = (F(J) - F(J + 1)) * (HDIS / XI(1))
            do 40 I = 2, IOP21 + LT - J
               F(I+J) = ((XI(I-1)+HDIS)*F(I+J-1)-HDIS*F(I+J))/XI(I)
   40          continue
            if (J .le. K) then
               if (J .ne. IOP21) then
                  TP = HDIS
                  do 50 L = 2, J - IOP21
                     TP = TP * (HDIS / float(L))
   50                continue
                  L = IOP21 + NDIFF * (J - IOP21)
                  do 60 I = NDIFF - 1, 0, -1
                     F(I + L) = F(I + J) * TP
   60                continue
                  LT = LT - 1
               end if
            end if
   70       continue
         NDTFDA = IOP21 + NDIFF * KDMXDA
         TSPECS(1) = TN
         go to 230
      end if
c Update correction differences
c
      MA = 0
      IL = 0
      LDA = LDA0
      if (NCF .ne. 1) then
         TP1 = XI(NCF - 1)
         CDA(1) = -XI(1) / TP1
         do 100 K = 2, NCF - 1
            CDA(K) = XI(K) / (XI(K - 1) - TP1)
  100       continue
      end if
      IDT = NDTF
      CONV = .true.
      SUMHI = C0
      SUMLO = C0
      NCONV = NCONV + 1
      do 130 I = 1, NTE
         if (I .gt. MA) then
            IL = IL + 1
            M = LEQ(IL)
            MA = abs(M)
         end if
         if (M .gt. 0) then
            LDA = LDA + NDIFF
c                      J = integration order
            J = KORD(I + 3)
c                TP = -(function value for correction NCF-1 steps back)
            TP = (F(LDA + NDIFF) - F(I))
c                      Compute -(NCF-1 difference for correction)
            do 110 K = 1, NCF - 1
               TP = CDA(K) * (TP + F(LDA + K))
  110          continue
c                      Save difference just computed
            F(LDA + NCF) = -TP
            if (NCF .lt. J) then
               TP1 = (J - NCF) ** 2
               if (TP1 * abs(TP) .gt. abs(F(IDT+J)) + abs(F(IDT+J-
     1            1))) CONV = .false.
            end if
         end if
         continue
         IDT = IDT + NUMDT
  130    continue
      if (.not. CONV) NCONV = 0
      if (NCF .gt. 2) then
         SUMLOM = max(SUMLOM, SUMLO)
         if (SUMHI .gt. SUMLOM) then
            CONV = .true.
            LNCF = 0
         end if
      end if
      if (.not. CONV .or. (NCF .le. LNCF)) then
         if (NCF .lt. NDIFF) then
            NCF = NCF + 1
            TSPECS(1) = TN - XI(NCF - 1)
            IFLAG = 6
            go to 240
         end if
      end if
c Add difference corrections into the difference tables
      MA = 0
      IL = 0
      IY = NYNY - 1
      LDA = LDA0
      IDT = NDTF
      do 210 I = 1, NTE
         if (I .gt. MA) then
            IL = IL + 1
            M = LEQ(IL)
            MA = abs(M)
         end if
         if (NKDKO .ne. 0) KORDI = KORD(NKDKO + I - 1)
         IYL = IY
         IY = IY + KORDI
         L = IOP21 - 1
         if (M .gt. 0) then
            LDA = LDA + NDIFF
            KQQ = KORD(I + 3)
            TP = C0
            do 170 K = 1, min(KQQ + 1, NCF)
               F(IDT + K - 1) = F(IDT + K - 1) + F(LDA + K)
               TP = TP + F(L + K) * F(LDA + K)
  170          continue
            Y(IY) = Y(IY) - TP
            J1 = 0
  180       IY = IY - 1
            if (IY .gt. IYL) then
               J = IY - NYNY + 2
               Y(J) = TP
               TP = C0
               J1 = J1 + 1
               TP2 = J1
               do 190 K = J + J1 - 1, J, -1
                  TP = HDIS * (TP - Y(K)) / TP2
                  TP2 = TP2 - C1
  190             continue
               L = L + NDIFF
               do 200 K = 1, min(KQQ + 1, NCF)
                  TP = TP + F(L + K) * F(LDA + K)
  200             continue
               Y(IY) = Y(IY) - TP
               go to 180
            end if
            IY = IYL + KORDI
         end if
         IDT = IDT + NUMDT
  210    continue
c Check for convergence of outer loop
      TSPECS(1) = TN
      if (NCONV .gt. LNCF) go to 300
      ITER = ITER + 1
      LNCF = NCF
      SUMLOM = CP1 * SUMLOM
c Put some kind of check here for ITER too big and print error.
 
  230 IFLAG = 5
      NCF = 1
      NCONV = 1
 
  240 continue
      TSPECS(2) = XI(NDIFF)
      call DIVAIN(TSPECS(1), Y, F, KORD)
      TSPECS(2) = HH
c                    Save interpolated F's that are of interest
      MA = 0
      IL = 0
      LDA0 = NDTFDA - NDIFF - 1
      LDA = LDA0
      do 250 I = 1, NTE
         if (I .gt. MA) then
            IL = IL + 1
            M = LEQ(IL)
            MA = abs(M)
         end if
         if (M .gt. 0) then
            LDA = LDA + NDIFF
            F(LDA + NDIFF) = F(I)
         end if
  250    continue
      return
c      
  280 continue
      IDAT(1) = IFLAG
      call MESS(MACT, MTXTAA, IDAT)
  300 IFLAG = 0
      return
c$$   END OF DISCONTINUITY ADJUST SUBROUTINE -IVADA.
      end
