      SUBROUTINE DELPII (PHI, K2, ALPHA2, P, IERR)
C>>1994-10-20 DELPII Krogh  Changes to use M77CON
C>>1991-10-10 DELPII WV Snyder JPL Original code.
C ----------------------------------------------------------------------
C        COMPUTES REAL ELLIPTIC INTEGRAL OF THE THIRD KIND
C ----------------------------------------------------------------------
      DOUBLE PRECISION PHI, K2, ALPHA2, P
      INTEGER IERR
C        IERR = 0..3, AS FOR DRJVAL AND DRFVAL
C        IERR = 4 IF ABS(PHI) > PI/2.
C
C--D replaces "?": ?ELPII, ?ERM1, ?RFVAL, ?RJVAL
C --------------------
      DOUBLE PRECISION A, B, C, PIHALF, R, RF, S, S2
      PARAMETER (PIHALF = 1.5707 96326 79489 66192 31322D0)
C ----------------------------------------------------------------------
      IF (ABS(PHI) .GT. PIHALF) THEN
         IERR = 4
         CALL DERM1 ('DELPII',4,0,'ABS(PHI) > PI/2','PHI',PHI,'.')
         RETURN
      end if
C
      S = SIN(PHI)
      C = COS(PHI)
      A = C*C
      S2 = S*S
      B = 1.0 - K2*S2
      S2 = ALPHA2*S2
      R = 1.0 - S2
      C = MAX(A,B,R)
      A = C*A
      B = C*B
      R = C*R
C
      CALL DRJVAL (A, B, C, R, P, IERR)
      IF (IERR .EQ. 0) then
         CALL DRFVAL (A, B, C, RF, IERR)
         P = SQRT(C) * S * ((S2 * C * P) / 3.0 + RF)
      end if
      RETURN
C
      END
