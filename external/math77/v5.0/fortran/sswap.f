      SUBROUTINE SSWAP(N,X,INCX,Y,INCY)
C>> 1994-11-11 SSWAP  Krogh   Declared all vars.
C>> 1994-10-20 SSWAP  Krogh  Changes to use M77CON
C>> 1985-08-02 SSWAP  Lawson  Initial code.
c--S replaces "?": ?SWAP
C
C     INTERCHANGE X and Y.
C     FOR I = 0 TO N-1, INTERCHANGE  X(LX+I*INCX) AND Y(LY+I*INCY),
C     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
C     DEFINED IN A SIMILAR WAY USING INCY.
C
      INTEGER N, INCX, INCY, IX, IY, I, M, MP1, NS
      REAL             X(*),Y(*),TEMP1,TEMP2,TEMP3
      IF(N.LE.0)RETURN
      IF(INCX.EQ.INCY) IF(INCX-1) 5,20,60
    5 CONTINUE
C
C       CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS.
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        TEMP1 = X(IX)
        X(IX) = Y(IY)
        Y(IY) = TEMP1
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 3.
C
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
        TEMP1 = X(I)
        X(I) = Y(I)
        Y(I) = TEMP1
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        TEMP1 = X(I)
        TEMP2 = X(I+1)
        TEMP3 = X(I+2)
        X(I) = Y(I)
        X(I+1) = Y(I+1)
        X(I+2) = Y(I+2)
        Y(I) = TEMP1
        Y(I+1) = TEMP2
        Y(I+2) = TEMP3
   50 CONTINUE
      RETURN
   60 CONTINUE
C
C     CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS.
C
      NS = N*INCX
        DO 70 I=1,NS,INCX
        TEMP1 = X(I)
        X(I) = Y(I)
        Y(I) = TEMP1
   70   CONTINUE
      RETURN
      END
