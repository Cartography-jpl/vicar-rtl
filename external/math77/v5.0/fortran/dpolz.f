      SUBROUTINE DPOLZ(A,NDEG,Z,H,IERR)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  All rights reserved.  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-10-19 DPOLZ  Krogh  Changes to use M77CON
C>> 1992-05-11 DPOLZ  CLL
C>> 1988-11-16        CLL More editing of spec stmts.
C>> 1988-06-07 DPOLZ  CLL Reordered spec stmts for ANSI standard.
C>> 1987-09-16 DPOLZ  Lawson  Initial code.
c--D replaces "?": ?POLZ,                                               00001000
C     ------------------------------------------------------------------
C
C     Given coefficients A(1),...,A(NDEG+1) this subr computes the
C     NDEG roots of the polynomial
C                 A(1)*X**NDEG + ... + A(NDEG+1)
C     storing the roots as complex numbers in the array Z( ).
C     Require NDEG .ge. 1 and A(1) .ne. 0.
C
C     ------------------------------------------------------------------
C-- Begin mask code changes                                             00002200
C
C     Argument Definitions
C     --------------------
C
C     A( )     (In) Contains the coefficients of a polynomial, high
C              order coefficient first with A(1).ne.0. The contents
C              of this array will not be modified by the subroutine.
C
C     NDEG     (In) Degree of the polynomial.
C                                                                       00003200
C     Z( )     (Out) Contains the polynomial roots stored as complex
C              numbers. The real and imaginary parts of the Jth roots
C              will be stored in Z(2*J-1) and Z(2*J) respectively.
C
C     H( )     (Scratch) Array of work space.
C
C     IERR     (Out) Error flag. Set by the subroutine to 0 on normal
C              termination. Set to -1 if A(1)=0. Set to -2 if NDEG
C              .le. 0. Set to  J > 0 if the iteration count limit
C              has been exceeded and roots 1 through J have not been    00004200
C              determined.
C
C     C.L.Lawson & S.Y.Chan, JPL, June 3,1986.
c     1992-05-11 CLL IERR was not being set when N = 0 or 1. Fixed this.
c                Added type stmts for all variables.
C-- End mask code changes
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION C43, C75, C95, HALF, ONE, ZERO
      PARAMETER(ZERO=0.D0, ONE=1.D0, C75=.75D0, HALF=.5D0,              00005200
     *          C43=-.4375D0, C95=.95D0)
      INTEGER I,J,K,L,M,N,NDEG, EN,LL,MM,NA,ITS,LOW,MP2,                
     *        ENM2,IERR,I1MACH
      DOUBLE PRECISION A(NDEG+1), H(NDEG,NDEG)
      DOUBLE PRECISION  P,Q,R,S,T,W,X,Y,ZZ,MACHEP,D1MACH
      DOUBLE PRECISION  C,F,G,B2,RADIX
c--D Next line special: S => C
      DOUBLE PRECISION Z(*)
      LOGICAL NOTLAS,CONV,FIRST
C
      SAVE FIRST, MACHEP, RADIX, B2
C                                                                       00006400
      DATA FIRST /.TRUE./
C     ------------------------------------------------------------------
C
      IF(FIRST)THEN
C
C     Set MACHEP = machine dependent parameter specifying the
C                  relative precision of floating point arithmetic.
C         RADIX  = machine dependent parameter specifying the base
C                  of the machine floating point representation.
C                                                                       00007500
        FIRST = .FALSE.
        MACHEP = D1MACH(4)
        RADIX = I1MACH(10)
        B2 = RADIX * RADIX
      END IF
C
      IERR = 0
C
      IF(NDEG .LE. 0)THEN
        IERR = -2                                                       00008500
        CALL ERMSG('DPOLZ',IERR,0,'NDEG .LE. 0','.')
        RETURN
      END IF
C
      IF(A(1) .EQ. ZERO)THEN
        IERR = -1
        CALL ERMSG('DPOLZ',IERR,0,'A(1) .EQ. ZERO','.')
        RETURN
      END IF
C                                                                       00009500
C     Build first row of companion matrix.
C
      DO 20008 I = 2,NDEG+1
        H(1,I-1) = -(A(I) / A(1))
20008 CONTINUE
C
C     Extract any exact zero roots and set N = degree of
C     remaining polynomial.
C
      N = NDEG                                                          00010500
      IERR = 0
C
      DO 20011 J = NDEG,1,-1
      IF (H(1,J) .NE. ZERO) GO TO 20012
c++  Code for {S} is inactive
C        Z(J) = ZERO
c++  Code for {D} is active
        Z(2*J-1) = ZERO
        Z(2*J) = ZERO
c++  End                                                                00011500
        N = N - 1
20011 CONTINUE
20012 CONTINUE
C
C     Special for N = 0 or 1.
C
      IF (N .EQ. 0) RETURN
      IF(N .EQ. 1)THEN
        Z(1) = H(1,1)
        RETURN                                                          00012400
      END IF
C
C     Build rows 2 thru N of the companion matrix.
C
      DO 20016 I = 2,N
      DO 20019 J = 1,N
          H(I,J) = ZERO
20019 CONTINUE
        H(I,I-1) = ONE
20016 CONTINUE                                                          00013400
C
      GO TO 30001
C
20022 GO TO 30002
C
20023 IF(IERR .NE. 0)THEN
        CALL ERMSG('DPOLZ',IERR,0,'Convergence failure','.')
      END IF
C
      RETURN                                                            00014400
C
C     ------------------------------------------------------------------
C
C     PROCEDURE (BALANCE THE MATRIX)
C
C     This procedure balances a companion matrix.
C
C     This proc is an adaption of the EISPACK subroutine BALANC to
C     the special case of a companion matrix. The EISPACK BALANCE
C     is a translation of the ALGOL procedure BALANCE, NUM. MATH.       00015400
C     13, 293-304(1969) by Parlett and Reinsch.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
C
C     ********** ITERATIVE LOOP FOR NORM REDUCTION **********
30001 GO TO 20028
20026 IF (CONV) GO TO 20027
20028   CONV = .TRUE.
      DO 20029 I = 1, N
C     Compute R = sum of magnitudes in row I skipping diagonal.
C             C = sum of magnitudes in col I skipping diagonal.
      IF(I .EQ. 1)THEN                                                  00016400
            R = ABS(H(1,2))
      DO 20034 J = 3,N
              R = R + ABS(H(1,J))
20034 CONTINUE
            C = ABS(H(2,1))
      ELSE
            R = ABS(H(I,I-1))
            C = ABS(H(1,I))
      IF(I .NE. N)THEN
              C = C + ABS(H(I+1,I))                                     00017400
      END IF
      END IF
C
C     Determine column scale factor, F.
C
          G = R / RADIX
          F = ONE
          S = C + R
20039 IF(C .LT. G)THEN
            F = F * RADIX                                               00018400
            C = C * B2
      GO TO 20039
      END IF
          G = R * RADIX
20041 IF(C .GE. G)THEN
            F = F / RADIX
            C = C / B2
      GO TO 20041
      END IF
C                                                                       00019200
C     Will the factor F have a significant effect ?
C
      IF((C + R) / F .LT. C95 * S)THEN
C
C           Yes, so do the scaling.
C
            G = ONE / F
            CONV = .FALSE.
C
C     Scale Row I                                                       00020200
C
      IF(I .EQ. 1)THEN
      DO 20047 J = 1,N
                H(1,J) = H(1,J)*G
20047 CONTINUE
      ELSE
              H(I,I-1) = H(I,I-1)*G
      END IF
C
C     Scale Column I                                                    00021200
C
            H(1,I) = H(1,I) * F
            IF (I .NE. N) H(I+1,I) = H(I+1,I) * F
C
      END IF
20029 CONTINUE
      GO TO 20026
C
20027 GO TO 20022
C                                                                       00022200
C     ------------------------------------------------------------------
C
C     PROCEDURE (QR EIGENVALUE ALGORITHM)
C
C     This proc is the EISPACK subroutine HQR that uses the QR
C     algorithm to compute all eigenvalues of an upper
C     Hessenberg matrix. Original ALGOL code was due to Martin,
C     Peters, and Wilkinson, Numer. Math., 14, 219-231(1970).
C
30002 LOW = 1                                                           00023200
      EN = N
      T = ZERO
C     ********** SEARCH FOR NEXT EIGENVALUES **********
   60 IF (EN .LT. LOW) GO TO 1001
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
C     ********** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW DO -- **********
   70 DO 80 LL = LOW, EN                                                00024200
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 100
         IF (ABS(H(L,L-1)) .LE. MACHEP * (ABS(H(L-1,L-1))               
     *      + ABS(H(L,L)))) GO TO 100
   80 CONTINUE
C     ********** FORM SHIFT **********
  100 X = H(EN,EN)
      IF (L .EQ. EN) GO TO 270
      Y = H(NA,NA)
      W = H(EN,NA) * H(NA,EN)
      IF (L .EQ. NA) GO TO 280                                          00025300
      IF (ITS .EQ. 30) GO TO 1000
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130
C     ********** FORM EXCEPTIONAL SHIFT **********
      T = T + X
C
      DO 120 I = LOW, EN
  120 H(I,I) = H(I,I) - X
C
      S = ABS(H(EN,NA)) + ABS(H(NA,ENM2))
      X = C75 * S                                                       00026300
      Y = X
      W = C43 * S * S
  130 ITS = ITS + 1
C     ********** LOOK FOR TWO CONSECUTIVE SMALL
C                SUB-DIAGONAL ELEMENTS.
C                FOR M=EN-2 STEP -1 UNTIL L DO -- **********
      DO 140 MM = L, ENM2
         M = ENM2 + L - MM
         ZZ = H(M,M)
         R = X - ZZ                                                     00027300
         S = Y - ZZ
         P = (R * S - W) / H(M+1,M) + H(M,M+1)
         Q = H(M+1,M+1) - ZZ - R - S
         R = H(M+2,M+1)
         S = ABS(P) + ABS(Q) + ABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF (M .EQ. L) GO TO 150
         IF (ABS(H(M,M-1)) * (ABS(Q) + ABS(R)) .LE. MACHEP * ABS(P)     00028300
     *    * (ABS(H(M-1,M-1)) + ABS(ZZ) + ABS(H(M+1,M+1)))) GO TO 150
  140 CONTINUE
C
  150 MP2 = M + 2
C
      DO 160 I = MP2, EN
         H(I,I-2) = ZERO
         IF (I .EQ. MP2) GO TO 160
         H(I,I-3) = ZERO
  160 CONTINUE
C     ********** DOUBLE QR STEP INVOLVING ROWS L TO EN AND              00029400
C                COLUMNS M TO EN **********
      DO 260 K = M, NA
         NOTLAS = K .NE. NA
         IF (K .EQ. M) GO TO 170
         P = H(K,K-1)
         Q = H(K+1,K-1)
         R = ZERO
         IF (NOTLAS) R = H(K+2,K-1)
         X = ABS(P) + ABS(Q) + ABS(R)
         IF (X .EQ. ZERO) GO TO 260                                     00030400
         P = P / X
         Q = Q / X
         R = R / X
  170    S = SIGN(SQRT(P*P+Q*Q+R*R),P)
         IF (K .EQ. M) GO TO 180
         H(K,K-1) = -S * X
         GO TO 190
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)
  190    P = P + S
         X = P / S                                                      00031400
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
C     ********** ROW MODIFICATION **********
         DO 210 J = K, EN
            P = H(K,J) + Q * H(K+1,J)
            IF (.NOT. NOTLAS) GO TO 200
            P = P + R * H(K+2,J)
            H(K+2,J) = H(K+2,J) - P * ZZ                                00032400
  200       H(K+1,J) = H(K+1,J) - P * Y
            H(K,J) = H(K,J) - P * X
  210    CONTINUE
C
         J = MIN0(EN,K+3)
C     ********** COLUMN MODIFICATION **********
         DO 230 I = L, J
            P = X * H(I,K) + Y * H(I,K+1)
            IF (.NOT. NOTLAS) GO TO 220
            P = P + ZZ * H(I,K+2)                                       00033400
            H(I,K+2) = H(I,K+2) - P * R
  220       H(I,K+1) = H(I,K+1) - P * Q
            H(I,K) = H(I,K) - P
  230    CONTINUE
C
  260 CONTINUE
C
      GO TO 70
C     ********** ONE ROOT FOUND **********
c++  Code for {S} is inactive                                           00034400
C  270 Z(EN) = CMPLX(X+T,ZERO)
c++  Code for {D} is active
  270 Z(2*EN-1) = X+T
      Z(2*EN) = ZERO
c++  End
      EN = NA
      GO TO 60
C     ********** TWO ROOTS FOUND **********
  280 P = (Y - X) * HALF
      Q = P * P + W                                                     00035400
      ZZ = SQRT(ABS(Q))
      X = X + T
      IF (Q .LT. ZERO) GO TO 320
C     ********** PAIR OF REALS **********
      ZZ = P + SIGN(ZZ,P)
c++  Code for {S} is inactive
C      Z(NA) = CMPLX(X+ZZ,ZERO)
C      Z(EN) = Z(NA)
C      IF (ZZ .NE. ZERO) Z(EN) = CMPLX(X-W/ZZ,ZERO)
c++  Code for {D} is active                                             00036400
      Z(2*NA-1) = X + ZZ
      Z(2*NA) = ZERO
      Z(2*EN-1) = Z(2*NA-1)
      Z(2*EN) = Z(2*NA)
      IF(ZZ .NE. ZERO)THEN
        Z(2*EN-1) = X - W/ZZ
        Z(2*EN) = ZERO
      END IF
c++  End
      GO TO 330                                                         00037400
C     ********** COMPLEX PAIR **********
c++  Code for {S} is inactive
C  320 Z(NA) = CMPLX(X+P,ZZ)
C      Z(EN) = CMPLX(X+P,-ZZ)
c++  Code for {D} is active
  320 Z(2*NA-1) = X + P
      Z(2*NA) = ZZ
      Z(2*EN-1) = X + P
      Z(2*EN) = -ZZ
c++  End                                                                00038400
  330 EN = ENM2
      GO TO 60
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = EN
 1001 CONTINUE
C
      GO TO 20023
C
C     ------------------------------------------------------------------00039400
C
      END                
