      DOUBLE PRECISION FUNCTION DASUM(N,X,INCX)
C>> 1994-11-11 DASUM  Krogh  Declared all vars.
c>> 1994-10-20 DASUM  Krogh  Changes to use M77CON
c>> 1994-04-19 DASUM  Krogh  Minor -- Made diff. precision line up.
C>> 1985-08-02 DASUM  Lawson Initial code.
c--D replaces "?": ?ASUM
C
C     RETURNS SUM OF MAGNITUDES OF X.
C     DASUM = SUM FROM 0 TO N-1 OF ABS(X(1+I*INCX))
C
      INTEGER N, INCX, NS, I, M, MP1
      DOUBLE PRECISION X(*)
      DASUM = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      NS = N*INCX
          DO 10 I=1,NS,INCX
          DASUM = DASUM + ABS(X(I))
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 6.
C
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
         DASUM = DASUM + ABS(X(I))
   30 CONTINUE
      IF( N .LT. 6 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
         DASUM = DASUM + ABS(X(I)) + ABS(X(I + 1)) + ABS(X(I + 2))
     $   + ABS(X(I + 3)) + ABS(X(I + 4)) + ABS(X(I + 5))
   50 CONTINUE
      RETURN
      END
