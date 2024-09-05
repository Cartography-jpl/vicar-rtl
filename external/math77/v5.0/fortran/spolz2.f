      SUBROUTINE SPOLZ2(A,Z)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1994-11-11 CLL Typing all variables.
C>> 1987-02-25 SPOLZ2 Lawson  Initial code.
C
C     ------------------------------------------------------------------
C
C     Find the two roots of the quadratic polynomial
C               A(1)*X*X + A(2)*X + A(3)
C     Return the roots as the complex numbers Z(1) and Z(2).
C     Require A(1) .ne. 0.
C
C     Method:
C     Divide through by A(1). New polynomial is
C            X*X + P*X + Q
C     Let U = -P/2
C     Roots are U + SQRT(U*U-Q) and U - SQRT(U*U-Q).
C     Avoid computing U*U explicity if it might overflow or
C     underflow.
C     In case of real roots, U + Z and U - Z, first
C     compute  Z(1) = U + SIGN(U)*ABS(Z).
C     Then compute Z(2) = Q / Z(1).
C
C     C.L.Lawson & S.Chan, JPL, Mar 1986, Feb 1987.
C
C     ------------------------------------------------------------------
      REAL A(3), AQ, AU, C1, C2, D, R1MACH, F, P, Q, U
      real c16, half, one, v, w, zero
      PARAMETER(ZERO=0.E0, HALF=.5E0, ONE=1.E0, C16=16.E0 )
      COMPLEX Z(2), R1, R2
      LOGICAL FIRST
C
      SAVE FIRST, C1, C2
C
      DATA FIRST / .TRUE. /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF (FIRST) THEN
        FIRST = .FALSE.
C
C     R1MACH(1) is the underflow limit.
C
        C1 = SQRT(R1MACH(1)*C16)
C
C     R1MACH(2) is the overflow limit.
C
        C2 = SQRT(R1MACH(2)/C16)
      END IF
C
      IF (A(1) .EQ. ZERO) THEN
        CALL ERMSG('SPOLZ2',1,0,'A(1) .EQ. 0.','.')
        R1 = ZERO
        R2 = ZERO
        GO TO 99
      ELSE
        P = A(2) / A(1)
        Q = A(3) / A(1)
      END IF
C
      IF (Q .EQ. ZERO) THEN
        R1 = ZERO
        R2 = -P
        GO TO 99
      END IF
C
      IF (P .EQ. ZERO) THEN
        W = SQRT(ABS(Q))
        IF (Q .GT. ZERO) THEN
          R1 = CMPLX(ZERO,W)
          R2 = CMPLX(ZERO,-W)
        ELSE
          R1 = CMPLX(W,ZERO)
          R2 = CMPLX(-W,ZERO)
        END IF
        GO TO 99
      END IF
      U = -P * HALF
C
C     Compute D having the sign of U*U-Q
C         and F = SQRT(ABS(U*U-Q))
C
      AU = ABS(U)
      IF (AU .GT. C2) THEN
        D = ONE - (Q/U) / U
        F = AU * SQRT(ABS(D))
      ELSE IF (AU .LT. C1) THEN
        AQ = ABS(Q)
        D = U * (U/AQ) - SIGN(ONE,Q)
        F = SQRT(AQ) * SQRT(ABS(D))
      ELSE
        D = U*U - Q
        F = SQRT(ABS(D))
      END IF
C
      IF (D .EQ. ZERO) THEN
        R1 = CMPLX(U,ZERO)
        R2 = R1
      ELSE
        IF (D .GT. ZERO) THEN
          IF (U .GT. ZERO) THEN
            V = U + F
          ELSE
            V = U - F
          END IF
          R1 = CMPLX(V,ZERO)
          R2 = CMPLX(Q/V,ZERO)
        ELSE
          R1 = CMPLX(U,F)
          R2 = CMPLX(U,-F)
        END IF
      END IF
C
  99  CONTINUE
      Z(1) = R1
      Z(2) = R2
      RETURN
C
      END
