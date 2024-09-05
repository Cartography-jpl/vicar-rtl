      SUBROUTINE DPOLZ2(A,Z)
C     .  Copyright (C) 1989, California Institute of Technology.
C     .  U. S. Government sponsorship under
C     .  NASA contract NAS7-918 is acknowledged.
C>> 1992-03-13 DPOLZ2 FTK  Removed implicit statements.
C>> 1987-02-25 DPOLZ2 Lawson  Initial code.
C
C     ------------------------------------------------------------------
C
C     Find the two roots of the quadratic polynomial
C               A(1)*X*X + A(2)*X + A(3)
C     Return the roots as the complex numbers (Z(1), Z(2)) and
C     (Z(3), Z(4)).
C     Require A(1) .ne. 0.
C
C     Method:
C     Divide through by A(1). New polynomial is
C            X*X + P*X + Q
C     Let U = -P/2
C     Roots are U + SQRT(U*U-Q) and U - SQRT(U*U-Q).
C     Avoid computing U*U explicity if it might overflow or
C     underflow. In case of real roots, U + Z and U - Z, first
C     compute  Z(1) = U + SIGN(U)*ABS(Z).
C     Then compute Z(3) = Q / Z(1).
C
c     C. L. Lawson & S. Chiu, JPL, 1987 Feb 16.
C     ------------------------------------------------------------------
C
      DOUBLE PRECISION A(3),AQ,AU,C1,C16,C2,D,D1MACH,F,HALF,ONE,P,Q
      DOUBLE PRECISION R1,R2,R3,R4,U,V,W,Z(4),ZERO
      PARAMETER(ZERO=0.D0, HALF=.5D0, ONE=1.D0, C16=16.D0)
      LOGICAL FIRST
C
      SAVE FIRST, C1, C2
C
      DATA FIRST / .TRUE. /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF (FIRST) THEN
        FIRST = .FALSE.
C
C     D1MACH(1) is the underflow limit.
C
        C1 = SQRT(D1MACH(1)*C16)
C
C     D1MACH(2) is the overflow limit.
C
        C2 = SQRT(D1MACH(2)/C16)
      END IF
C
      IF (A(1) .EQ. ZERO) THEN
        CALL ERMSG('DPOLZ2',1,0,'A(1) .EQ. 0.','.')
        R1 = ZERO
        R2 = ZERO
        R3 = ZERO
        R4 = ZERO
        GO TO 99
      ELSE
        P = A(2) / A(1)
        Q = A(3) / A(1)
      END IF
C
      IF (Q .EQ. ZERO) THEN
        R1 = ZERO
        R2 = ZERO
        R3 = -P
        R4 = ZERO
        GO TO 99
      END IF
C
      IF (P .EQ. ZERO) THEN
        W = SQRT(ABS(Q))
        IF (Q .GT. ZERO) THEN
          R1 = ZERO
          R2 = W
          R3 = ZERO
          R4 = -W
        ELSE
          R1 = W
          R2 = ZERO
          R3 = -W
          R4 = ZERO
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
        D = U * (U/AQ) - SIGN(ONE, Q)
        F = SQRT(AQ) * SQRT(ABS(D))
      ELSE
        D = U*U - Q
        F = SQRT(ABS(D))
      END IF
C
      IF (D .EQ. ZERO) THEN
        R1 = U
        R2 = ZERO
        R3 = R1
        R4 = ZERO
      ELSE
        IF (D .GT. ZERO) THEN
          IF (U .GT. ZERO) THEN
            V = U + F
          ELSE
            V = U - F
          END IF
          R1 = V
          R2 = ZERO
          R3 = Q/V
          R4 = ZERO
        ELSE
          R1 = U
          R2 = F
          R3 = U
          R4 = -F
        END IF
      END IF
C
   99 CONTINUE
        Z(1) = R1
        Z(2) = R2
        Z(3) = R3
        Z(4) = R4
        RETURN
      END
