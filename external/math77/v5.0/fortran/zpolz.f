      SUBROUTINE ZPOLZ(A,NDEG,Z,H,IERR)
C>> 1992-05-11 CLL
C>> 1989-10-20 CLL
C>> 1987-02-25 ZPOLZ  Lawson  Initial code.
C     ------------------------------------------------------------------
C
C     Given complex coefficients A(*,1),...,A(*,NDEG+1) this
C     subr computes the NDEG roots of the polynomial
C                 A(*,1)*X**NDEG + ... + A(*,NDEG+1)
C     storing the roots as complex numbers in the array Z( ).           00001200
C     Require NDEG .ge. 1 and A(*,1) .ne. 0.
C
C     ------------------------------------------------------------------
C
C     Argument Definitions
C     --------------------
C
C     A( )     (In) Contains the complex coefficients of a polynomial
C              high order coefficient first, with A(*,1).ne.0. The
C              real and imaginary parts of the Jth coefficient must     00002200
C              be provided in A(1,J) and A(2,J) respectively. The
C              contents of this array will not be modified by the
C              subroutine.
C
C     NDEG     (In) Degree of the polynomial.
C
C     Z( )     (Out) Contains the polynomial roots stored as complex
C              numbers. The real and imaginary parts of the Jth root
C              will be stored in Z(1,J) and Z(2,J) respectively.
C                                                                       00003200
C     H( )     (Scratch) Array of work space.
C
C     IERR     (Out) Error flag. Set by the subroutine to 0 on normal
C              termination. Set to -1 if A(1)=0. Set to -2 if NDEG
C              .le. 0. Set to  J > 0 if the iteration count limit
C              has been exceeded and roots 1 through J have not been
C              determined.
C
C     ------------------------------------------------------------------
C     C.L.Lawson & S.Y.Chan, JPL, June 3,1986.                          00004200
c     1989-10-20 CLL Moved integer declarations earlier to avoid
c     warning msg from Cray compiler.
c     Provided explicit declarations for all variables.  Previously
c     B2, C95, ONE, S, ZERO were not declared and thus were implicitly
c     single precision.  The omission of DP declarations for these
c     variables would not have given inaccurate results since ONE and
c     ZERO would be exact anyway, and the accuracy of B2, C95, and S are
c     not critical.
c     1992-05-11 CLL IERR was not being set when N = 0 or 1. Fixed this.
C     ------------------------------------------------------------------00005200
      INTEGER I, IERR, I1MACH, J, NDEG, N
      DOUBLE PRECISION A(2,NDEG+1), B2, C, C95, F, G, H(NDEG,NDEG,2)
      DOUBLE PRECISION ONE, R, RADIX, S, TEMP(2), Z(2,NDEG), ZERO
      LOGICAL CONV,FIRST
      PARAMETER(ZERO=0.D0, ONE=1.D0, C95=.95D0)
C
      SAVE FIRST,RADIX,B2
C
      DATA FIRST /.TRUE./
C     ------------------------------------------------------------------00006200
C
      IF(FIRST)THEN
C
C     Set RADIX = machine dependent parameter specifying the base
C                 of the machine floating point representation.
C
        FIRST = .FALSE.
        RADIX = I1MACH(10)
        B2 = RADIX * RADIX
      END IF                                                            00007300
C
      IF(NDEG .LE. 0)THEN
        IERR = -2
        CALL ERMSG('ZPOLZ',IERR,0,'NDEG .LE. 0','.')
        RETURN
      END IF
C
      IF(A(1,1) .EQ. ZERO .AND. A(2,1) .EQ. ZERO)THEN
        IERR = -1
        CALL ERMSG('ZPOLZ',IERR,0,'A(*,1) .EQ. ZERO','.')               00008300
        RETURN
      END IF
C
      N = NDEG
      IERR = 0
C
C     Build first row of companion matrix.
C     ZQUO computes the quotient of two DP complex numbers.
c
      DO 20008 I = 2,N+1                                                00009300
        CALL ZQUO(A(1,I),A(1,1),TEMP)
        H(1,I-1,1) = -TEMP(1)
        H(1,I-1,2) = -TEMP(2)
20008 CONTINUE
C
C     Extract any exact zero roots and set N = degree of
C     remaining polynomial.
C
      DO 20011 J = NDEG,1,-1
      IF (H(1,J,1).NE.ZERO .OR. H(1,J,2).NE.ZERO) GO TO 20012           00010300
        Z(1,J) = ZERO
        Z(2,J) = ZERO
        N = N - 1
20011 CONTINUE
C
C     Special for N = 0 or 1.
C
20012 IF (N .EQ. 0) RETURN
      IF(N .EQ. 1)THEN
        Z(1,1) = H(1,1,1)                                               00011300
        Z(2,1) = H(1,1,2)
        RETURN
      END IF
C
C     Build rows 2 thru N of the companion matrix.
C
      DO 20016 I = 2,N
      DO 20019 J = 1,N
      IF(J .EQ. I-1)THEN
             H(I,J,1) = ONE                                             00012300
             H(I,J,2) = ZERO
      ELSE
            H(I,J,1) = ZERO
            H(I,J,2) = ZERO
      END IF
20019 CONTINUE
20016 CONTINUE
C
      GO TO 30001
C                                                                       00013300
20024 CALL DCOMQR(NDEG,N,1,N,H(1,1,1),H(1,1,2),Z,IERR)
C
      IF(IERR .NE. 0)THEN
        CALL ERMSG('ZPOLZ',IERR,0,'Convergence failure','.')
      END IF
C
      RETURN
C
C     ------------------------------------------------------------------
C     PROCEDURE (BALANCE THE MATRIX)                                    00014300
C
C     This procedure balances a complex companion matrix.
C
C     This proc is an adaption of the EISPACK subroutine BALANC to
C     the special case of a complex companion matrix. The EISPACK
C     BALANCE is a translation of the ALGOL procedure BALANCE,
C     NUM. MATH. 13, 293-304(1969) by Parlett and Reinsch.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 315-326(1971).
C
C     ********** ITERATIVE LOOP FOR NORM REDUCTION **********           00015300
30001 GO TO 20029
20027 IF (CONV) GO TO 20028
20029   CONV = .TRUE.
      DO 20030 I = 1, N
C     Compute R = sum of magnitudes in row I skipping diagonal.
C             C = sum of magnitudes in col I skipping diagonal.
      IF(I .EQ. 1)THEN
            R = ABS(H(1,2,1)) + ABS(H(1,2,2))
      DO 20035 J = 3,N
              R = R + ABS(H(1,J,1)) + ABS(H(1,J,2))                     00016200
20035 CONTINUE
            C = ABS(H(2,1,1)) + ABS(H(2,1,2))
      ELSE
            R = ABS(H(I,I-1,1)) + ABS(H(I,I-1,2))
            C = ABS(H(1,I,1)) + ABS(H(1,I,2))
      IF(I .NE. N)THEN
              C = C + ABS(H(I+1,I,1)) + ABS(H(I+1,I,2))
      END IF
      END IF
C                                                                       00017200
C     Determine column scale factor, F.
C
          G = R / RADIX
          F = ONE
          S = C + R
20040 IF(C .LT. G)THEN
            F = F * RADIX
            C = C * B2
      GO TO 20040
      END IF                                                            00018100
          G = R * RADIX
20042 IF(C .GE. G)THEN
            F = F / RADIX
            C = C / B2
      GO TO 20042
      END IF
C
C     Will the factor F have a significant effect ?
C
      IF((C + R) / F .LT. C95 * S)THEN                                  00019000
C
C           Yes, so do the scaling.
C
            G = ONE / F
            CONV = .FALSE.
C
C     Scale Row I
C
      IF(I .EQ. 1)THEN
      DO 20048 J = 1,N                                                  00020000
                H(1,J,1) = H(1,J,1)*G
                H(1,J,2) = H(1,J,2)*G
20048 CONTINUE
      ELSE
              H(I,I-1,1) = H(I,I-1,1)*G
              H(I,I-1,2) = H(I,I-1,2)*G
      END IF
C
C     Scale Column I
C                                                                       00021000
            H(1,I,1) = H(1,I,1) * F
            H(1,I,2) = H(1,I,2) * F
      IF(I .NE. N)THEN
              H(I+1,I,1) = H(I+1,I,1) * F
              H(I+1,I,2) = H(I+1,I,2) * F
      END IF
C
      END IF
20030 CONTINUE
      GO TO 20027                                                       00022000
C
20028 GO TO 20024
C
C     ------------------------------------------------------------------
C
      END        
