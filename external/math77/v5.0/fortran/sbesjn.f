      SUBROUTINE SBESJN(X,ALPHA,NUM,BJ)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1995-10-24 SBESJN Krogh  Removed blanks in numbers for C conversion.
C>> 1994-10-19 SBESJN Krogh  Changes to use M77CON
C>> 1994-04-19 SBESJN CLL  Edited to make DP & SP files similar.
C>> 1992-03-13 SBESJN FTK  Removed implicit statements.
C>> 1989-08-09 SBESJN CLL  More accurate HALFPI for Cray
C>> 1986-03-18 SBESJN Lawson  Initial code.                             00001200
c--S replaces "?": ?BESJN, ?GAMMA, ?BESPQ, ?ERV1
C
C     COMPUTE THE J-BESSEL FUNCTIONS OF X FOR ORDER ALPHA THROUGH
C     ALPHA + NUM - 1 in steps of one.
C     STORE THE RESULT INTO BJ(I),I=1,...,NUM.
C     Require X .ge. 0, ALPHA .ge. 0, NUM .ge. 1
C
c     Original code due to E. W. Ng and W. V. Snyder, JPL, 1973.
c     Modified by Ng and S. Singletary, 1974.
C     C.Lawson & S.Chan, JPL, 1984 Apr 05:                              00002200
c        Changed calling sequence.
c        Adapted to SFTRAN3 and Fortran 77.
c        Added code to avoid overflow during the recurrsion.
c        Added code to use Taylor series for small x.
c        March 23. Improved accuracy of backward recursion.
c        Changed to use P and Q instead of R and THETA
c                in the region of large X.
C     ------------------------------------------------------------------
C
      SAVE EPS, SMALL, XPQ, BIG, HICUT                                  00003200
c     ----------
c     Subprograms used:  LOG10, SQRT, COS, SIN
      EXTERNAL           SGAMMA, SBESPQ, R1MACH, ERMSG, SERV1, IERV1
c     ----------
      INTEGER I,I1,II,INT,J,K,M,MU,N20021,NMAX,NMIN,NPR001,NUM
      REAL             R1MACH, SGAMMA
      REAL             ALPHA,BESV,BESVM1,BESVP1,BIG,BJ(NUM),BJNU,BOT
      REAL             C11293,C16,C1P5,C59,CHI,CP6
      REAL             DR,EM,EMU,EPS,ETA,FAC,FAC2,FK,FKM1,FKP1,FV,FVM1
      REAL             FVP1,G,GNU,HALF,HALFPI,HALFX,HICUT               00004200
      REAL             ONE,P,Q,SCALE,SMALL,SUM,T1,T2,TENTH,TERM
      REAL             TERM1,TEST,TOP,TWO,TWODX,V,VPMU,X,XPQ,ZERO
      parameter(ZERO=0.E0, ONE=1.E0, TWO=2.E0, C16=16.E0)
      parameter( TENTH = 0.1E0, HALF = 0.5E0)
      parameter(HALFPI = 1.57079632679489661923132169163975144E0)
      parameter(C11293 = 1.1293E0, CP6 = 0.60206E0, C59 = 0.59E0)
      parameter(C1P5 = 1.5E0)
      data EPS / ZERO /
C     ------------------------------------------------------------------
C                                                                       00005300
c     Set environment parameters.  This should happen only
c     the first time this subroutine is called.
c
      IF( EPS .EQ. ZERO )THEN
         EPS = R1MACH(3)
         HICUT = ONE /(EPS*C16)
         XPQ = C11293 * (CP6 - LOG10(EPS)) - C59
         SMALL = C16 * R1MACH(1) / EPS
         BIG = R1MACH(2)/TWO
      END IF                                                            00006300
C
c     Test validity of input values.
c
      IF (.NOT.(X .LT. ZERO .OR. ALPHA .LT. ZERO .OR. NUM .LT. 1)) GO TO
     * 20005
c
c     >                                                      Error 1.
        CALL ERMSG('SBESJN',1,0,                                        
     *             'REQUIRE X.GE.0, ALPHA.GE.0, NUM.GE.1',',')
      ASSIGN 20006 TO NPR001
      GO TO 30001
20006   RETURN
C                                                                       00007500
c     Begin computation.
c
20005 NMIN = INT(ALPHA)
      V = ALPHA - REAL(NMIN)
      NMAX = NMIN + NUM - 1
C
      IF (.NOT.( X .LE. TENTH )) GO TO 20008
      GO TO 30002
20009 GO TO 20007
20008 IF (.NOT.( X .LT. MAX(REAL(NMAX+1), XPQ) )) GO TO 20010
      GO TO 30003                                                       00008500
20011 GO TO 20007
20010 IF (.NOT.( X .LT. HICUT )) GO TO 20012
      GO TO 30004
20013 GO TO 20007
c     >                                             Error 2.
20012    CALL ERMSG('SBESJN', 2, 0,                                     
     *   'Cannot obtain any accuracy when X exceeds HICUT.', ',')
         CALL SERV1('HICUT', HICUT, ',')
      ASSIGN 20014 TO NPR001
      GO TO 30001
C
20014 CONTINUE
20007 RETURN
C                                                                       00009700
C     ------------------------------------------------------------------
C
C     PROCEDURE( SMALL X )
C
30002 IF (.NOT.( X .EQ. ZERO )) GO TO 20016
c                                         Special for X .eq. 0.
      DO 20017 I = 1, NUM
            BJ(I) =  ZERO
20017 CONTINUE
         IF (ALPHA .EQ. 0) BJ(1) = ONE                                  00010700
      GO TO 20015
c                     Here use Taylor series for small x.
C
20016    GNU = ALPHA
         HALFX = HALF*X
         FAC2 = -HALFX*HALFX
         TERM1 = (HALFX**GNU) / SGAMMA(GNU + ONE)
      I =1
      N20021=NUM
      GO TO 20022
20020 I =I +1
20022 IF ((N20021-I ).LT.0) GO TO 20021
      GO TO 30005
20023       BJ(I) = BJNU                                                00011700
      IF( BJNU .EQ. ZERO )THEN
C
c     Here current result has underflowed to zero, so we will
c     set the rest of the results to zero also.
c
      DO 20026 J = I+1, NUM
                  BJ(J) = ZERO
20026 CONTINUE
      GO TO 20021
      END IF                                                            00012700
            GNU = GNU + ONE
            TERM1 = TERM1 * (HALFX / GNU)
      GO TO 20020
20021 CONTINUE
20015 GO TO 20009
C
C     ------------------------------------------------------------------
c
C     PROCEDURE( SERIES FOR SMALL X )
C                                                                       00013700
c     Sums the series for the Bessel fcn J sub GNU of X given
c     in Eq 9.1.10, page 360, of AMS 55.
c     Returns the value, BJNU.
c      1984 March 9, JPL, C. L. Lawson.
c
30005 IF(TERM1 .EQ. ZERO)THEN
         BJNU = ZERO
      GO TO 31005
      END IF
c                                                                       00014700
      SUM = ZERO
      TOP = FAC2
      BOT = GNU + ONE
      T1 = ONE
      T2 = BOT
      TERM = TOP/BOT
c
20031 IF( ABS(TERM) .GT. EPS )THEN
         SUM = SUM + TERM
         TOP = TOP * FAC2                                               00015700
         T1 = T1 + ONE
         T2 = T2 + ONE
         BOT = BOT * T1 * T2
         TERM = TOP/BOT
      GO TO 20031
      END IF
c
      BJNU = TERM1 + TERM1 * SUM
31005 GO TO 20023
c                                                                       00016600
C     ------------------------------------------------------------------
C
C     PROCEDURE( MIDDLE X )
C
C     J-TYPE BESSEL FUNCTIONS FOLLOW THE RECURRENCE RELATION
C     F(V-1,X)=(2*V/X)*F(V,X)-F(V+1,X).
C
30003 TWODX = TWO / X
      MU = MAX( INT(X)+1, NMAX)
      DR = TWODX * (V+REAL(MU))                                         00017600
      FKP1 = ONE
      FK =  ZERO
C
C     RECUR FORWARD UNTIL FKP1 IS GREATER THAN PRECISION OF ARITHMETIC.
C
20033 IF( EPS * ABS(FKP1) .LE. ONE )THEN
      MU = MU + 1
      DR = DR + TWODX
      FKM1 = FK
      FK = FKP1                                                         00018600
      FKP1 = DR * FK - FKM1
      GO TO 20033
      END IF
C
C     WE ARE NOW ASSURED THAT BACKWARD RECURRENCE FROM MU WILL YIELD
C     ACCURATE RESULTS.
C
C                                        GUARANTEE EVEN MU
      IF (MOD(MU,2) .NE. 0)  MU = MU + 1
      FVM1 = SMALL                                                      00019500
      FV = ZERO
      ETA = ONE
      SUM = FVM1
      M = MU / 2
      EM = REAL(M)
      EMU = REAL(MU)
      FAC = (V + EMU) * TWODX
C
c     Set TEST = largest value that can be multiplied by
c     FAC without risking overflow.  The present value of               00020500
c     FAC is the largest that will occur during the recursion.
c     TEST will be used to protect against overflow during
c     the recursion.
c
      TEST = BIG / MAX(ONE, FAC)
C
C                             Loop while MU .GT. ZERO
C
20035 CONTINUE
      FVP1 = FV                                                         00021500
      FV = FVM1
      IF (.NOT.( ABS(FV) .GT. TEST )) GO TO 20037
      GO TO 30006
20037 FVM1 = FAC * FV - FVP1
      MU = MU -1
      EMU = EMU - ONE
      FAC = (V + EMU) * TWODX
      IF (MU .GE. NMIN .AND. MU .LE. NMAX) BJ(MU-NMIN+1) = FVM1
      IF(MOD(MU,2) .EQ. 0)THEN
      IF(V .EQ. ZERO)THEN                                               00022400
          SUM = SUM + FVM1
      IF(MU .EQ. 0)THEN
            SCALE = ONE / SUM
      GO TO 20036
      END IF
          SUM = SUM + FVM1
      ELSE
      IF(MU .NE. 0)THEN
            VPMU = V + EMU
            ETA = ETA * (EM/(V + (EM-ONE))) * (VPMU / (VPMU + TWO))     00023400
            SUM = SUM + FVM1 * ETA
            EM = EM - ONE
      ELSE
c
c           Here MU = 0 and EM = 0NE.  Thus the expression for
c           updating ETA reduces to the following simpler
c           expression.
c
            ETA = ETA / (V + TWO)
            SUM = SUM + FVM1 * ETA                                      00024400
            SCALE = SGAMMA(V+ONE) / ETA * SUM * TWODX ** V
            SCALE = ONE / SCALE
      GO TO 20036
      END IF
      END IF
      END IF
      GO TO 20035
20036 CONTINUE
C
C     NORMALIZE BJ() TO GET VALUES OF J-BESSEL FUNCTION.                00025300
C
      DO 20046 I = 1, NUM
         BJ(I) = BJ(I) * SCALE
20046 CONTINUE
      GO TO 20011
C
C     ------------------------------------------------------------------
C
C     PROCEDURE( RESCALE )
C                                                                       00026300
30006 FV = FV / SUM
      FVP1 = FVP1 / SUM
      I1 = MAX( 1, MU - NMIN + 1 )
      DO 20049 II = I1, NUM
         BJ(II) = BJ(II) / SUM
20049 CONTINUE
      SUM = ONE
      GO TO 20037
C
C     ------------------------------------------------------------------00027300
C
C     PROCEDURE( LARGE X )
C
c     >     Here we have X .ge. XPQ, and V in [0.,1.).
c     The asymptotic series for
c     the auxiliary functions P and Q can be used.
c     From these we will compute J(V,X) and J(V+1,X) and
c     then recur forward.
c     Reference: NBS AMS 55 Eqs 9.2.5 & 9.2.6
c                                                                       00028300
30004 CALL SBESPQ (X,V,  P,Q)
      CHI = X - (V + HALF) * HALFPI
      BESV = SQRT(ONE / (HALFPI*X)) * (P*COS(CHI) - Q*SIN(CHI))
C
      IF( NMAX .GT. 0 )THEN
         CALL SBESPQ (X,V+ONE,   P,Q)
         CHI = X - (V + C1P5) * HALFPI
         BESVP1 = SQRT(ONE / (HALFPI*X)) * (P*COS(CHI) - Q*SIN(CHI))
      END IF
c                                                                       00029300
      TWODX = TWO / X
      GO TO 30007
20054 GO TO 20013
C     ------------------------------------------------------------------
c
C     PROCEDURE( FORWARD RECURSION )
c
c     >     Given BESV = J(V,X), BESVP1 = J(V+1,X), TWODX = 2/X,
c     NMIN, NUM, NMAX = NMIN + NUM -1,
c     X, ALPHA, and BIG.                                                00030300
c     Recur forward and store J(NMIN+V) thru J(NMAX+V) in
c     BJ(1) thru BJ(NUM).
c     >     There should be no overflow posibility in
c     this forward recursion since NMAX .le. X - 1, and
c     in this region the magnitude of the J function is
c     less than one.
c
30007 IF( NMIN .EQ. 0 )THEN
         BJ(1) = BESV
      IF( NMAX .GT. 0 )THEN                                             00031300
            BJ(2) = BESVP1
      END IF
      ELSEIF( NMIN .EQ. 1 )THEN
         BJ(1) = BESVP1
      END IF
c
      IF( NMAX .GT. 1 )THEN
         G = V * TWODX
c
c        Note:  In the following statement, 3-NMIN can be nonpositive.  00032300
c
      DO 20062 K = 3-NMIN, NUM
            BESVM1 = BESV
            BESV   = BESVP1
            G = G + TWODX
            BESVP1 = G * BESV - BESVM1
            IF( K .GE. 1)  BJ(K) = BESVP1
20062 CONTINUE
      END IF
      GO TO 20054                                                       00033300
C
C     ------------------------------------------------------------------
C
C     PROCEDURE( TRANSMIT X, ALPHA, AND NUM )
C
30001   CALL SERV1('X',X,',')
        CALL SERV1('ALPHA',ALPHA,',')
        CALL IERV1('NUM',NUM,'.')
      GO TO NPR001,(20006,20014)
C     ------------------------------------------------------------------00034300
      END        
