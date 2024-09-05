      SUBROUTINE SBESYN(X, ALPHA, NUM, BY)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-19 SBESYN Krogh  Changes to use M77CON
C>> 1994-04-19 SBESYN CLL  Edited to make DP & SP files similar.
C>> 1991-01-14 SBESYN CLL  Removed duplicate data statement.
C>> 1989-08-09 SBESYN CLL  More accurate constants for Cray
C>> 1986-03-18 SBESYN Lawson  Initial code.
c--S replaces "?": ?BESYN, ?BESPQ, ?ERV1, ?GAMMA, ?LGAMA                00001200
C
c     This subr computes the Y Bessel functions of X for
c     NUM orders from ALPHA through ALPHA + NUM - 1.
c     The results will be stored in BY(I), I = 1,...,NUM.
C
c     Require X .gt. 0.,    ALPHA .ge. 0.,    NUM .ge. 1.
C
c          The original subroutines SBYNU and BESJ/BESY were
c     designed and programmed by E. W. Ng and W. V. Snyder, JPL,
C     in 1973.  Modified by Ng and S. Singletary, JPL, 1974.            00002200
c
c     1984 April 16, JPL, C. Lawson and S. Chan.  Original subrs
c     combined into single subroutine.  Converted to SFTRAN3
c     and Fortran 77 for the JPL MATH77 library.
C
c     ------------------------------------------------------------------
c
c           As the machine precision, EPS, increases so does XPQ,
c     and thus so does the requirement for the storage dimension,
c     LDIMA.  Here are some values of -LOG10(EPS), XPQ, and LDIMA.      00003200
c
c     -LOG10(EPS)=  5     10     15     20     25     30
c            XPQ =  5.74  11.38  17.03  22.68  28.32  33.97
c          LDIMA = 17     33     49     63     79     95
c
c           Since LDIMA cannot be adjusted at run-time, we are
c     setting it to 95 to handle the case of CDC double precision
c     which is probably the largest precision system likely
c     to be encountered.  Note that the relative precision of results
c     from this subr cannot exceed about 16 or 17 decimal places        00004200
c     because of the limited accuracy of the polynomial coeffs used to
c     compute G0, and also the limited precision of the
c     subprograms referenced for gamma and loggamma.
c     ------------------------------------------------------------------
c     Subprograms used: TANH, LOG10, LOG, EXP, COS, SIN, SQRT
      EXTERNAL          R1MACH, SBESPQ, SERV1, ERMSG, ERMOR, IERV1
      EXTERNAL          SGAMMA, SLGAMA
c     ----------
      integer I, II, K, LDIMA, M, MU, MUSAVE, N20023, N20093, ND2
      integer NMAX, NMIN, NPR001, NPR008, NUM                           00005200
      parameter( LDIMA = 95, ND2 = 10)
      real             R1MACH, SGAMMA, SLGAMA
      real             AJ(LDIMA), ALPHA, ARG
      real             BGAM, BGAMSQ, BIG, BIGLOG, BY(NUM)
      real             C11293, C16, C1P5, C2BYPI, C59, CHI, CP6, CUTLOW
      real             D1, D2, D2SER(0:ND2), D2VAL, D3, DR
      real             EC, EM, EM1, EMU, EN1, EN2, EN3, EPS, ETA
      real             FAC, FK, FKM1, FKP1, FOUR, FV, FVM1, FVP1
      real             G, G0, G1, GMAX, GNU, HALF, HALFPI, HICUT
      real             LOGPI, LOGTWO, ONE, P, PI, PIV2, PSI1, PSIZ      00006200
      real             Q, Q2DXPV, SCALE, SMALL, SUM
      real             TEMP, TEST, THREE, THSJ, THVDX, TWO, TWODX
      real             V, V2, VPMU, X, XLOG, XPQ
      real             YLOG, YV, YVM1, YVP1, Z, ZERO
      logical FLAG, J1SMAL
      save EPS, HICUT, SMALL, XPQ, BIG, BIGLOG
c
      parameter( ZERO = 0.E0, ONE = 1.E0, TWO = 2.E0)
      parameter( HALF = 0.5E0, THREE = 3.0E0)
      parameter( C16 = 16.E0, FOUR = 4.0E0)                             00007200
      parameter(PI = 3.14159 26535 89793 23846 26433 83279 50288E0)
C
      parameter(HALFPI = 1.57079 63267 94896 61923 13216 91639 75144E0)
c          LOGPI = ln(pi)
      parameter(LOGPI = 1.14472 98858 49400 17414 34273 51353 05869E0)
c          C2BYPI = 2/pi
      parameter(C2BYPI = 0.63661 97723 67581 34307 55350 53490 05744E0)
c          EC = Euler's constant.
      parameter(EC = 0.57721 56649 01532 86060 65120 90082 40243 E0)
c          LOGTWO = Ln(2)                                               00008200
      parameter(LOGTWO = 0.69314 71805 59945 30941 72321 21458 17657E0)
      parameter(   C1P5 = 1.5E0 )
      parameter( C11293 = 1.1293E0)
      parameter(    CP6 = 0.60206E0)
      parameter(    C59 = 0.59E0)
      parameter( CUTLOW = 0.012E0)
      parameter(   THSJ = 0.12E0)
      DATA EPS / ZERO /
C
      DATA D2SER( 0) / +.36746 69051 96615 96151 85E+00/                00009200
      DATA D2SER( 1) / -.17829 03980 80726 98422 31E+01/
      DATA D2SER( 2) / +.94116 44685 12285 59084 27E+00/
      DATA D2SER( 3) / -.19588 65250 24874 78780 77E+01/
      DATA D2SER( 4) / +.15573 06621 10828 32944 75E+01/
      DATA D2SER( 5) / -.25210 51413 54681 20964 37E+01/
      DATA D2SER( 6) / +.21845 02685 63511 09141 45E+01/
      DATA D2SER( 7) / -.31400 67153 45267 44028 72E+01/
      DATA D2SER( 8) / +.28174 01038 92146 13611 58E+01/
      DATA D2SER( 9) / -.37721 98775 79967 00818 58E+01/
      DATA D2SER(10) / +.34527 80604 49258 45750 60E+01/                00010200
C
c     ------------------------------------------------------------------
C
c     Set environmental parameters.
c
      IF( EPS .EQ. ZERO)THEN
         EPS = R1MACH(3)
         HICUT = ONE / (EPS*C16)
         SMALL = C16 * R1MACH(1) / EPS
         XPQ = C11293 * (CP6 - LOG10( EPS )) - C59                      00011300
         BIG = R1MACH(2) / TWO
         BIGLOG = LOG(BIG)
      END IF
c     ------------------------------------------------------------------
c
c     Compute V, NMIN, and NMAX.
c
      NMIN = INT(ALPHA)
      V = ALPHA - REAL(NMIN)
      NMAX = NMIN + NUM -1                                              00012300
c     ------------------------------------------------------------------
c
c     Test validity of given argument values.
C
      IF (.NOT.( X .LT. ZERO  .OR.                                      
     *          ALPHA .LT. ZERO .OR. NUM .LT. 1)) GO TO 20005
C                                                Error 1.
        CALL ERMSG('SBESYN',1,0,                                        
     *             'Require X .gt. 0, ALPHA .ge. 0, NUM .ge. 1',',')
      ASSIGN 20006 TO NPR001
      GO TO 30001
20006   RETURN
c                                                                       00013600
c     ------------------------------------------------------------------
C
c     Branch on size of X.
c
20005 IF (.NOT.( X .EQ. ZERO )) GO TO 20008
c     >                                           Error 6.
      DO 20009 I = 1, NUM
            BY(I) = -BIG
20009 CONTINUE
         CALL ERMSG('SBESYN',6,0,                                       00014600
     *   'When X = 0., function value is -INFINITY.', ',')
      ASSIGN 20012 TO NPR001
      GO TO 30001
20012 GO TO 20007
20008 IF (.NOT.( X .LT. EPS )) GO TO 20013
      GO TO 30002
20014 GO TO 20007
20013    TWODX = TWO / X
      IF (.NOT.( X .LE. XPQ )) GO TO 20016
      GO TO 30003
20017 GO TO 30004
20018 GO TO 20015
20016 IF (.NOT.( X .LE. HICUT )) GO TO 20019
      GO TO 30005                                                       00015700
20020 GO TO 20015
c        >                                             Error 2.
20019       CALL ERMSG('SBESYN', 2, 0,                                  
     *      'Cannot obtain any accuracy when X exceeds HICUT.', ',')
            CALL SERV1('HICUT', HICUT, ',')
      ASSIGN 20021 TO NPR001
      GO TO 30001
20021 CONTINUE
20015 CONTINUE
C
20007 RETURN
c     ------------------------------------------------------------------
c                                                                       00016900
C     PROCEDURE( VERY SMALL X )
c
c     >     Use a single term expression for Y, valid
c     for X very close to zero.  Ref NBS AMS 55 Eqs 9.1.8 & 9.1.9.
c     For GNU = 0,   Y = (2/pi) * (EC + Ln(X/2)),  {EC = Euler's const.}
c     For GNU .gt. 0    Y =  -(1/pi) * Gamma(GNU) * (X/2)**(-GNU)
c
30002 XLOG = LOG( X )
      GNU = ALPHA
c                                                                       00017900
      I =1
      N20023=NUM
      GO TO 20024
20022 I =I +1
20024 IF ((N20023-I ).LT.0) GO TO 20023
      IF (.NOT.( GNU .EQ. ZERO )) GO TO 20026
            BY(I) = C2BYPI * (EC + XLOG - LOGTWO)
      GO TO 20025
20026       YLOG = SLGAMA(GNU) - GNU * (XLOG-LOGTWO) - LOGPI
      IF (.NOT.(YLOG .LT. BIGLOG)) GO TO 20028
               BY(I) = -EXP(YLOG)
      GO TO 20027
c     >                                         Error 5.
20028 DO 20029 II = I,NUM                                               00018900
                  BY(II) = -BIG
20029 CONTINUE
               CALL ERMSG('SBESYN',5,0,                                 
     *         'Results exceed overflow limit from BY(I) on.', ',')
               CALL IERV1('I', I, ',')
      ASSIGN 20032 TO NPR001
      GO TO 30001
20032 GO TO 20023
20027 CONTINUE
20025       GNU = GNU + ONE
      GO TO 20022
20023 GO TO 20014                                                       00020100
c     ------------------------------------------------------------------
C
C     PROCEDURE( COMPUTE J BY RECURSION )
C
C     J-TYPE BESSEL FUNCTIONS FOLLOW THE RECURRENCE RELATION
C     F(V-1,X)=(2*V/X)*F(V,X)-F(V+1,X).
C
30003 MU = INT(X) + 1
      DR = TWODX * (V+REAL(MU))
      FKP1 = ONE                                                        00021100
      FK =  ZERO
C
C     RECUR FORWARD UNTIL FKP1 IS GREATER THAN PRECISION OF ARITHMETIC.
C
20033 IF( EPS * ABS(FKP1) .LE. ONE )THEN
      MU = MU + 1
      DR = DR + TWODX
      FKM1 = FK
      FK = FKP1
      FKP1 = DR * FK - FKM1                                             00022100
      GO TO 20033
      END IF
C
C     WE ARE NOW ASSURED THAT BACKWARD RECURRENCE FROM MU WILL YIELD
C     ACCURATE RESULTS.
C
C                                        GUARANTEE EVEN MU
      IF (MOD(MU,2) .NE. 0)  MU = MU + 1
      MUSAVE = MU
c                                                                       00023000
c     >                                         Test for Error 3.
c
c     This error should never happen.  Large MU would be due to
c     large X.  But X is not larger than XPQ here.
c     See explanation at the beginning of this subroutine
c     of the relation of XPQ and LDIMA to the machine EPS.
c
      IF (.NOT.( MU + 1 .GT. LDIMA)) GO TO 20036
         CALL ERMSG('SBESYN', 3, 0,                                     
     *'Need larger dimension, LDIMA, to process given X.', ',')
         CALL ERMOR('Require LDIMA .ge. MU + 1', ',')                   00024100
         CALL IERV1('MU', MU, ',')
         CALL IERV1('LDIMA', LDIMA, ',')
      ASSIGN 20037 TO NPR001
      GO TO 30001
c                                                Error RETURN.
20037    RETURN
c
20036 FVM1 = SMALL
      AJ(MU+1) = FVM1
      FV = ZERO
      ETA = ONE                                                         00025200
      SUM = FVM1
      M = MU / 2
      EM = REAL(M)
      EMU = REAL(MU)
      FAC = (V + EMU) * TWODX
C
c     Set TEST = largest value that can be multiplied by
c     FAC without risking overflow.  The present value of
c     FAC is the largest that will occur during the recursion.
c     TEST will be used to protect against overflow during              00026200
c     the recursion.
c
      TEST = BIG / MAX(ONE, FAC)
C
C                             Loop while MU .GT. ZERO
C
20038 CONTINUE
      FVP1 = FV
      FV = FVM1
      IF (.NOT.( ABS(FV) .GT. TEST )) GO TO 20040                       00027200
      GO TO 30006
20040 FVM1 = FAC * FV - FVP1
      MU = MU -1
      EMU = EMU - ONE
      FAC = (V + EMU) * TWODX
         AJ(MU+1) = FVM1
      IF(MOD(MU,2) .EQ. 0)THEN
      IF(V .EQ. ZERO)THEN
          SUM = SUM + FVM1
      IF(MU .EQ. 0)THEN                                                 00028100
            SCALE = ONE / SUM
      GO TO 20039
      END IF
          SUM = SUM + FVM1
      ELSE
      IF(MU .NE. 0)THEN
            VPMU = V + EMU
            ETA = ETA * (EM/(V + (EM-ONE))) * (VPMU / (VPMU + TWO))
            SUM = SUM + FVM1 * ETA
            EM = EM - ONE                                               00029100
      ELSE
c
c           Here MU = 0 and EM = 0NE.  Thus the expression for
c           updating ETA reduces to the following simpler
c           expression.
c
            ETA = ETA / (V + TWO)
            SUM = SUM + FVM1 * ETA
            BGAM = SGAMMA(V+ONE)
            Q2DXPV = TWODX ** V                                         00030100
            SCALE = ( BGAM / ETA ) * SUM * Q2DXPV
            SCALE = ONE / SCALE
      GO TO 20039
      END IF
      END IF
      END IF
      GO TO 20038
20039 CONTINUE
C
C     NORMALIZE AJ() TO GET VALUES OF J-BESSEL FUNCTION.                00031000
C
      DO 20049 I = 1, MUSAVE+1
         AJ(I) = AJ(I) * SCALE
20049 CONTINUE
      MU = MUSAVE
      GO TO 20017
C
C     ------------------------------------------------------------------
C
C     PROCEDURE( RESCALE )                                              00032000
C
30006 FV = FV / SUM
      FVP1 = FVP1 / SUM
      DO 20052 II = MU+1, MUSAVE
         AJ(II) = AJ(II) / SUM
20052 CONTINUE
      SUM = ONE
      GO TO 20040
C
c     ------------------------------------------------------------------00033000
c
C     PROCEDURE( COMPUTE Y FROM J )
c
c          This proc computes Y Bessel functions, making use
c     of previously computed J Bessel functions and other
c     previously computed values, MU, BGAM, Q2DXPV, TWODX, V.
c
c     >     Here V is in the range [0.,1.).  The
c     quantities G0 and G1 depend on X and V and are unbounded
c     as V approaches 1.   Therefore we make                            00034000
c     the change of variables
c     >          V2 = V      if V .le. 0.5 and
c     >          V2 = 1 - V  if V .gt. 0.5
c     Then G0 and G1 are computed as functions of X and V2
c     with V2 in the range (-0.5, 0.5].
C
c     Compute G0 and G1.
c
30004 BGAMSQ = BGAM**2
      V2 = V                                                            00035000
      IF (.NOT.( V .EQ. ZERO )) GO TO 20056
         Z = EC - LOG(TWODX)
         G0 =  Z / HALFPI
         G1 = TWO / HALFPI
         BGAMSQ = ONE
         Q2DXPV = ONE
      GO TO 20055
20056 IF(V .GT. HALF)THEN
c
c           Use the transformed variable, V2 = V - 1.                   00036000
c           Make corresponding transformation of Q2DXPV & BGAMSQ.
c
            V2 = (V - HALF) - HALF
            Q2DXPV = Q2DXPV / TWODX
            BGAMSQ = BGAMSQ / V**2
      END IF
         PIV2 = PI * V2
c
c           Here V2 is in [-.5, .5].  Now test against CUTLOW = 0.012
c                                                                       00037000
      IF (.NOT.( ABS(V2) .LT. CUTLOW )) GO TO 20060
      GO TO 30007
20061 GO TO 20059
20060       G0 = (ONE / TAN(PIV2)) - Q2DXPV**2 * BGAMSQ / PIV2
            G1 = (Q2DXPV**2/HALFPI) * BGAMSQ * (TWO+V2) / (ONE-V2)
20059 CONTINUE
c ----------------------------------
c
C     COMPUTE YO FROM SUM(J'S) FORM
c                                                                       00038100
20055 EN3 = V2 + ONE
      EN2 = V2 + EN3
      EN1 = V2 + FOUR
      D1 = TWO
      D2 = D1 - V2
      D3 = D1 + V2
      FLAG = .FALSE.
c                                      THSJ = 0.12
      J1SMAL = ABS(AJ(1)) .LT. THSJ
      IF( J1SMAL  .OR. V2 .LT. ZERO)THEN                                00039100
         FLAG = .TRUE.
C
C        Y(V2+1,X) MUST ALSO BE COMPUTED BY A SUM
C
         THVDX = THREE * V2 / X
         PSIZ = -BGAMSQ * Q2DXPV**2 / (HALFPI*X)
         PSI1 = G0 - HALF * G1
      END IF
c
      IF(V2 .GE. ZERO)THEN                                              00040100
         M = 3
         YV = G0 * AJ(1)
      IF( J1SMAL )THEN
            YVP1 = PSIZ * AJ(1) + PSI1 * AJ(2)
      END IF
      ELSE
         Z = TWODX * V * AJ(1)-AJ(2)
         YV = G0 * Z
         M = 2
         YVP1 = PSIZ * Z + PSI1 * AJ(1)                                 00041100
      END IF
c
      DO 20068 I = M,MU,2
         YV = G1 * AJ(I) + YV
         G = G1
         G1 = -G1 * (EN1/D1) * (EN2/D2) * (EN3/D3)
         EN1 = EN1 + TWO
         EN2 = EN2 + ONE
         EN3 = EN3 + ONE
         D1 = D1 + ONE                                                  00042100
         D2 = ONE + D2
         D3 = D3 + TWO
      IF( FLAG )THEN
            YVP1 = YVP1 + THVDX*G*AJ(I) + HALF*(G-G1)*AJ(I+1)
      END IF
20068 CONTINUE
c
      IF(V2 .LT. ZERO)THEN
         Z = YVP1
         YVP1 = V * Z * TWODX - YV                                      00043100
         YV = Z
      ELSEIF( .NOT. J1SMAL )THEN
C
C           NOW COMPUTE Y(V+1)
C           WRONSKIAN PROVIDED NOT NEAR A ZERO OF J
C
            YVP1 = (YV*AJ(2)-ONE/(X*HALFPI)) / AJ(1)
      END IF
c
      ASSIGN 20076 TO NPR008                                            00044100
      GO TO 30008
20076 GO TO 20018
C
c     ------------------------------------------------------------------
C
C     PROCEDURE(COMPUTE G0 AND G1 FOR VERY SMALL V2)
C
c     Here we compute
c           G0 = (ONE / TAN(PIV2)) - Q2DXPV**2 * BGAMSQ / PIV2
c     by a formulation that retains accuracy for
c     V2 close to zero.                                                 00045100
c     >     The no. of coeffs from D2SER() used to compute D2VAL
c     could be fewer on lower precision computers, however this
c     computation is only done about 2.4% of the time so the
c     potential time saving would probably not be noticeable.
c
c     This method was derived by C. Lawson and W. V. Snyder,
c     JPL, 1984 Apr 15.
c
c     First compute EM1 = (2/X)**(2*V2) - 1
c                       = exp(2 * V2 * log(2/X)) - 1                    00046100
c
30007 ARG = TWO * V2 * LOG( TWODX )
      IF( ABS( ARG ) .LT. LOGTWO )THEN
         TEMP = TANH( HALF * ARG )
         EM1 = TWO * TEMP / (ONE - TEMP)
      ELSE
         EM1 = EXP( ARG ) - ONE
      END IF
c
c      Evaluate taylor series for                                       00047100
c      D2VAL = (PIV2 * cotan(PIV2) - BGAMSQ) / PIV2
c
      D2VAL = D2SER(ND2)
      DO 20079 I = ND2-1, 0, -1
         D2VAL = D2SER(I) + V2 * D2VAL
20079 CONTINUE
c
         G0 = D2VAL - BGAMSQ * (EM1 / PIV2)
         G1 = (Q2DXPV**2/HALFPI) * BGAMSQ * (TWO+V2) / (ONE-V2)
      GO TO 20061                                                       00048100
C
c     ------------------------------------------------------------------
C
C     PROCEDURE( LARGE X )
C
c     >     Here we have X .ge. XPQ, and V in [0.,1.).
c     The asymptotic series for
c     the auxiliary functions P and Q can be used.
c     From these we will compute Y(V,X) and Y(V+1,X) and
c     then recur forward.                                               00049100
c     Reference: NBS AMS 55 Eqs 9.2.5 & 9.2.6
c
30005 CALL SBESPQ (X,V,  P,Q)
      CHI = X - (V + HALF) * HALFPI
      YV = SQRT(ONE / (HALFPI*X)) * (P*SIN(CHI) + Q*COS(CHI))
C
      IF( NMAX .GT. 0 )THEN
         CALL SBESPQ (X,V+ONE,   P,Q)
         CHI = X - (V + C1P5) * HALFPI
         YVP1 = SQRT(ONE / (HALFPI*X)) * (P*SIN(CHI) + Q*COS(CHI))      00050100
      END IF
c
      ASSIGN 20084 TO NPR008
      GO TO 30008
20084 GO TO 20020
C     ------------------------------------------------------------------
c
C     PROCEDURE( FORWARD RECURSION )
c
c     >     Given YV = Y(V,X), YVP1 = Y(V+1,X), TWODX = 2/X,
c     NMIN, NUM, NMAX = NMIN + NUM -1,                                  00051100
c     X, ALPHA, and BIG.
c     Recur forward and store Y(NMIN+V) thru Y(NMAX+V) in
c     BY(1) thru BY(NUM).
c
30008 IF( NMIN .EQ. 0 )THEN
         BY(1) = YV
      IF( NMAX .GT. 0 )THEN
            BY(2) = YVP1
      END IF
      ELSEIF( NMIN .EQ. 1 )THEN                                         00052100
         BY(1) = YVP1
      END IF
c
      IF (.NOT.( NMAX .GT. 1 )) GO TO 20091
         G = V * TWODX
         GMAX = G + TWODX * REAL(NMAX-1)
         TEST = BIG / MAX(ONE, GMAX)
c
c        Note:  In the following statement, 3-NMIN can be nonpositive.
c                                                                       00053100
      K =3-NMIN
      N20093=NUM
      GO TO 20094
20092 K =K +1
20094 IF ((N20093-K ).LT.0) GO TO 20093
            YVM1 = YV
            YV   = YVP1
      IF (.NOT.(ABS(YV) .GT. TEST)) GO TO 20096
c
c              The recursion has reached the overflow limit.
c              Set remaining elts of BY() to a large negative value
c              and issue error message.
c
      DO 20097 II = MAX(K, 1),NUM                                       00054100
                  BY(II) = -BIG
20097 CONTINUE
C                                                   Error 4.
               CALL ERMSG('SBESYN',4,0,                                 
     *         'Results exceed overflow limit from BY(I) on.', ',')
               CALL IERV1('I', MAX(K,1), ',')
      ASSIGN 20100 TO NPR001
      GO TO 30001
20100 GO TO 31008
c
20096       G = G + TWODX
            YVP1 = G * YV - YVM1                                        00055300
            IF( K .GE. 1)  BY(K) = YVP1
      GO TO 20092
20093 CONTINUE
20091 CONTINUE
31008 GO TO NPR008,(20076,20084)
C     ------------------------------------------------------------------
C
C     PROCEDURE( TRANSMIT X, ALPHA, AND NUM )
C
30001   CALL SERV1('X',X,',')                                           00056200
        CALL SERV1('ALPHA',ALPHA,',')
        CALL IERV1('NUM',NUM,'.')
      GO TO NPR001,(20006,20012,20021,20032,20037,20100)
C     ------------------------------------------------------------------
      END        
