      SUBROUTINE CPOLZ(A,NDEG,Z,H,IERR)
C>> 1992-05-11 CLL
C>> 1988-11-16        CLL More editing of spec stmts.
C>> 1988-06-07 CPOLZ  CLL Reordered spec stmts for ANSI standard.
C>> 1987-10-30 CPOLZ  Lawson  Initial code.
C     ------------------------------------------------------------------
C
C     Given complex coefficients A(1),...,A(NDEG+1) this subr
C     computes the NDEG roots of the polynomial
C                 A(1)*X**NDEG + ... + A(NDEG+1)                        00001200
C     storing the roots as complex numbers in the array Z( ).
C     Require NDEG .ge. 1 and A(1) .ne. 0.
C
C     ------------------------------------------------------------------
C
C     Argument Definitions
C     --------------------
C
C     A( )     (In) Contains the complex coefficients of a polynomial,
C              high order coefficient first with A(1).ne.0. The contents00002200
C              of this array will not be modified by the subroutine.
C
C     NDEG     (In) Degree of the polynomial.
C
C     Z( )     (Out) Contains the polynomial roots stored as complex
C              numbers.
C
C     H( )     (Scratch) Array of work space.
C
C     IERR     (Out) Error flag. Set by the subroutine to 0 on normal   00003200
C              termination. Set to -1 if A(1)=0. Set to -2 if NDEG
C              .le. 0. Set to  J > 0 if the iteration count limit
C              has been exceeded and roots 1 through J have not been
C              determined.
C
C     C.L.Lawson & S.Y.Chan, JPL, June 3,1986.
c     1992-05-11 CLL IERR was not being set when N = 0 or 1. Fixed this.
c                Added type stmts for all variables.
C     ------------------------------------------------------------------
C                                                                       00004300
      real C95, ONE, ZERO
      PARAMETER(ZERO=0.E0, ONE=1.E0, C95=.95E0)
      INTEGER I, I1MACH, IERR, J, NDEG,N
      REAL B2, C,G,F, H(NDEG,NDEG,2)
      REAL R, RADIX, S
      COMPLEX A(NDEG+1),TEMP,Z(NDEG)
      LOGICAL CONV,FIRST
C
      SAVE FIRST,RADIX,B2
C                                                                       00005300
      DATA FIRST /.TRUE./
C     ------------------------------------------------------------------
      IF(FIRST)THEN
C
C     Set RADIX = machine dependent parameter specifying the base
C                 of the machine floating point representation.
C
        FIRST = .FALSE.
        RADIX = I1MACH(10)
        B2 = RADIX * RADIX                                              00006300
      END IF
C
      IF(NDEG .LE. 0)THEN
        IERR = -2
        CALL ERMSG('CPOLZ',IERR,0,'NDEG .LE. 0','.')
        RETURN
      END IF
C
      IF(A(1) .EQ. ZERO)THEN
        IERR = -1                                                       00007300
        CALL ERMSG('CPOLZ',IERR,0,'A(1) .EQ. ZERO','.')
        RETURN
      END IF
C
      N = NDEG
      IERR = 0
C
C     Build first row of companion matrix.
C
      DO 20008 I = 2,N+1                                                00008300
        TEMP = -(A(I)/A(1))
        H(1,I-1,1) = REAL(TEMP)
        H(1,I-1,2) = AIMAG(TEMP)
20008 CONTINUE
C
C     Extract any exact zero roots and set N = degree of
C     remaining polynomial.
C
      DO 20011 J = NDEG,1,-1
      IF (H(1,J,1).NE.ZERO .OR. H(1,J,2).NE.ZERO) GO TO 20012           00009300
        Z(J) = ZERO
        N = N - 1
20011 CONTINUE
C
C     Special for N = 0 or 1.
C
20012 IF (N .EQ. 0) RETURN
      IF(N .EQ. 1)THEN
        Z(1) = CMPLX(H(1,1,1),H(1,1,2))
        RETURN                                                          00010300
      END IF
C
C     Build rows 2 thru N of the companion matrix.
C
      DO 20016 I = 2,N
      DO 20019 J = 1,N
      IF(J .EQ. I-1)THEN
             H(I,J,1) = ONE
             H(I,J,2) = ZERO
      ELSE                                                              00011300
            H(I,J,1) = ZERO
            H(I,J,2) = ZERO
      END IF
20019 CONTINUE
20016 CONTINUE
C
      GO TO 30001
C
20024 CALL SCOMQR(NDEG,N,1,N,H(1,1,1),H(1,1,2),Z,IERR)
C                                                                       00012300
      IF(IERR .NE. 0)THEN
        CALL ERMSG('CPOLZ',IERR,0,'Convergence failure','.')
      END IF
C
      RETURN
C
C     ------------------------------------------------------------------
C     PROCEDURE (BALANCE THE MATRIX)
C
C     This procedure balances a complex companion matrix.               00013300
C
C     This proc is an adaption of the EISPACK subroutine BALANC to
C     the special case of a complex companion matrix. The EISPACK
C     BALANCE is a translation of the ALGOL procedure BALANCE,
C     NUM. MATH. 13, 293-304(1969) by Parlett and Reinsch.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
C
C     ********** ITERATIVE LOOP FOR NORM REDUCTION **********
30001 GO TO 20029
20027 IF (CONV) GO TO 20028                                             00014200
20029   CONV = .TRUE.
      DO 20030 I = 1, N
C     Compute R = sum of magnitudes in row I skipping diagonal.
C             C = sum of magnitudes in col I skipping diagonal.
      IF(I .EQ. 1)THEN
            R = ABS(H(1,2,1)) + ABS(H(1,2,2))
      DO 20035 J = 3,N
              R = R + ABS(H(1,J,1)) + ABS(H(1,J,2))
20035 CONTINUE
            C = ABS(H(2,1,1)) + ABS(H(2,1,2))                           00015200
      ELSE
            R = ABS(H(I,I-1,1)) + ABS(H(I,I-1,2))
            C = ABS(H(1,I,1)) + ABS(H(1,I,2))
      IF(I .NE. N)THEN
              C = C + ABS(H(I+1,I,1)) + ABS(H(I+1,I,2))
      END IF
      END IF
C
C     Determine column scale factor, F.
C                                                                       00016200
          G = R / RADIX
          F = ONE
          S = C + R
20040 IF(C .LT. G)THEN
            F = F * RADIX
            C = C * B2
      GO TO 20040
      END IF
          G = R * RADIX
20042 IF(C .GE. G)THEN                                                  00017100
            F = F / RADIX
            C = C / B2
      GO TO 20042
      END IF
C
C     Will the factor F have a significant effect ?
C
      IF((C + R) / F .LT. C95 * S)THEN
C
C           Yes, so do the scaling.                                     00018000
C
            G = ONE / F
            CONV = .FALSE.
C
C     Scale Row I
C
      IF(I .EQ. 1)THEN
      DO 20048 J = 1,N
                H(1,J,1) = H(1,J,1)*G
                H(1,J,2) = H(1,J,2)*G                                   00019000
20048 CONTINUE
      ELSE
              H(I,I-1,1) = H(I,I-1,1)*G
              H(I,I-1,2) = H(I,I-1,2)*G
      END IF
C
C     Scale Column I
C
            H(1,I,1) = H(1,I,1) * F
            H(1,I,2) = H(1,I,2) * F                                     00020000
      IF(I .NE. N)THEN
              H(I+1,I,1) = H(I+1,I,1) * F
              H(I+1,I,2) = H(I+1,I,2) * F
      END IF
C
      END IF
20030 CONTINUE
      GO TO 20027
C
20028 GO TO 20024                                                       00021000
C
C     ------------------------------------------------------------------
C
      END        
