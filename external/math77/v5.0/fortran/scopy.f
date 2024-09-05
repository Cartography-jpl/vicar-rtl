      SUBROUTINE SCOPY (N,X,INCX,Y,INCY)
C>> 1994-11-11 SCOPY  Krogh   Declared all vars.
C>> 1994-10-20 SCOPY  Krogh  Changes to use M77CON
C>> 1989-05-12 SCOPY  Snyder  Clean up a little for F77
C>> 1985-08-02 SCOPY  Lawson  Initial code.
c--S replaces "?": ?COPY
C
C     COPY X TO Y.
C     FOR I = 0 TO N-1, COPY X(LX+I*INCX) TO Y(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, IX, IY, I, M, NS
      REAL             X(*),Y(*)
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
C
C        CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        Y(IY) = X(IX)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 8.
C
   20 M = MOD(N,8)
      DO 30 I = 1,M
        Y(I) = X(I)
   30 CONTINUE
      DO 50 I = M+1,N,8
        Y(I)     = X(I)
        Y(I + 1) = X(I + 1)
        Y(I + 2) = X(I + 2)
        Y(I + 3) = X(I + 3)
        Y(I + 4) = X(I + 4)
        Y(I + 5) = X(I + 5)
        Y(I + 6) = X(I + 6)
        Y(I + 7) = X(I + 7)
   50 CONTINUE
      RETURN
C
C        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
C
   60 CONTINUE
      NS = N*INCX
      DO 70 I=1,NS,INCX
        Y(I) = X(I)
   70   CONTINUE
      RETURN
      END
